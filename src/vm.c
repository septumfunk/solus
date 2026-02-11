#include <stdio.h>
#include <stdlib.h>
#include <stdlib.h>
#include <string.h>
#include "solus/vm.h"
#include "sf/containers/buffer.h"
#include "sf/fs.h"
#include "sf/math.h"
#include "solus/val.h"
#include "solus/compiler.h"
#include "sf/str.h"
#include "std/std.h"

#define CALL_STACK_MAX 1024

solu_state *solu_state_new(void) {
    solu_dyn p = calloc(1, sizeof(solu_dalloc) + sizeof(solu_dobj));
    *(solu_dalloc *)p = (solu_dalloc){NULL, sizeof(solu_dobj), 1, SOLU_DOBJ, SOLU_DYN_GREEN};
    p = (char *)p + sizeof(solu_dalloc);
    *(solu_dobj *)p = solu_dobj_new();

    solu_state *s = malloc(sizeof(solu_state));
    *s = (solu_state){
        .stack = solu_valvec_new(),
        .files = solu_filenames_new(),
        .strcache = solu_strcache_new(),
        .global = {SOLU_TDYN, .dyn = p},
        .lb = 1<<20, .cb = 0, .nb = 0,
        .call_stack = 0,

        .collect = false,
        .alloc = NULL,
    };
    return s;
}

void solu_state_free(solu_state *state) {
    solu_valvec_free(&state->stack);
    solu_filenames_free(&state->files);
    solu_strcache_free(&state->strcache);
    solu_dclean(state->global);
    free(state);
}

void solu_usestd(solu_state *s) {
    solu_val solus = solu_dnew(s, SOLU_DOBJ);
    solu_dobj_strset(solus.dyn, "version", solu_dnstr(s, SOLU_VERSION));
    solu_dobj_strset(solus.dyn, "git", solu_dnstr(s, SOLU_GIT));
    solu_dobj_strset(s->global.dyn, "solus", solus);

    solu_mod_builtin(s);
    solu_mod_io(s);
    solu_mod_string(s);
    solu_mod_obj(s);
    solu_mod_math(s);
    solu_mod_gc(s);
}

solu_compile_ex solu_csrc(solu_state *state, char *src) {
    solu_compile_ex ex = solu_cproto(SF_STR_EMPTY, src, 0, NULL, 1, (solu_upvalue[]){
        (solu_upvalue){sf_lit("_g"), SOLU_UP_VAL, .value = state->global, .mut = false}
    });
    ex.ok.line_c = 1;
    for (char *c = src; *c != '\0'; ++c)
        if (*c == '\n') ++ex.ok.line_c;
    return ex;
}

solu_compile_ex solu_cfile(solu_state *state, char *path) {
    if (!sf_file_exists(sf_ref(path)))
        return solu_compile_ex_err((solu_compile_err){SOLU_ERRC_FILE_NOT_FOUND, 0, 0});
    sf_fsb_ex fsb = sf_file_buffer(sf_ref(path));
    if (!fsb.is_ok) {
        switch (fsb.err) {
            case SF_FILE_NOT_FOUND: return solu_compile_ex_err((solu_compile_err){SOLU_ERRC_FILE_NOT_FOUND, 0, 0}); break;
            case SF_OPEN_FAILURE:
            case SF_READ_FAILURE: return solu_compile_ex_err((solu_compile_err){SOLU_ERRC_FILE_UNREADABLE, 0, 0}); break;
        }
    }
    fsb.ok.flags = SF_BUFFER_GROW;
    sf_buffer_seek(&fsb.ok, SF_BUFFER_END, 0);
    sf_buffer_autoins(&fsb.ok, ""); // [\0]
    sf_buffer_seek(&fsb.ok, SF_BUFFER_START, 0);

    char *realpath = solu_realpath(path);
    if (!path) return solu_compile_ex_err((solu_compile_err){SOLU_ERRC_FILE_NOT_FOUND, 0, 0});

    solu_compile_ex ex = solu_cproto(sf_ref(realpath), (char *)fsb.ok.ptr, 0, NULL, 1, (solu_upvalue[]){
        (solu_upvalue){sf_lit("_g"), SOLU_UP_VAL, .value = state->global, .mut = false}
    });
    if (!ex.is_ok) return ex;
    free(realpath);
    ex.ok.line_c = 1;
    for (char *c = (char *)fsb.ok.ptr; *c != '\0'; ++c)
        if (*c == '\n') ++ex.ok.line_c;
    sf_buffer_clear(&fsb.ok);
    ex.ok.file_name = sf_str_cdup(path);
    return ex;
}

void solu_dpush(solu_state *s, solu_dalloc *ac) {
    if (!s->alloc) {
        s->alloc = ac;
        s->alloc_tail = ac;
    } else {
        s->alloc_tail->next = ac;
        s->alloc_tail = ac;
    }
    s->cb += ac->size;
    if (s->cb > s->nb)
        s->collect = true;
}

solu_val solu_dnew(solu_state *s, solu_dtype tt) {
    size_t size = 0;
    switch (tt) {
        case SOLU_DSTR: size = 0; break;
        case SOLU_DERR: size = 0; break;
        case SOLU_DOBJ: size = sizeof(solu_dobj); break;
        case SOLU_DFUN: size = sizeof(solu_fproto); break;
        case SOLU_DREF: size = sizeof(solu_val); break;

        case SOLU_DUSR:
        case SOLU_DCOUNT: return SOLU_NIL;
    }

    solu_dyn p = calloc(1, sizeof(solu_dalloc) + size);
    solu_dalloc *dh = p;
    *dh = (solu_dalloc){
        .next = NULL,
        .size = size,
        .thread = 1,
        .tt = tt,
        .mark = SOLU_DYN_WHITE,
    };
    p = (char *)p + sizeof(solu_dalloc);

    switch (tt) {
        case SOLU_DSTR:
        case SOLU_DERR: break;
        case SOLU_DOBJ: *(solu_dobj *)p = solu_dobj_new(); break;
        case SOLU_DFUN: *(solu_fproto *)p = solu_fproto_new(); break;
        case SOLU_DREF: *(solu_val *)p = SOLU_NIL; break;

        case SOLU_DUSR:
        case SOLU_DCOUNT: {
            free(dh);
            return SOLU_NIL;
        }
    }

    solu_dpush(s, dh);
    return (solu_val){ .tt = SOLU_TDYN, .dyn = p };
}

solu_val solu_dnusr(solu_state *s, size_t size, const char *name, void *value,
    solu_usrdel del, solu_usrtostring tostring, solu_usrmark mark) {
    solu_dyn p = calloc(1, sizeof(solu_dalloc) + size + sizeof(solu_usrwrap));
    solu_dalloc *dh = p;
    *dh = (solu_dalloc){
        .next = NULL,
        .size = size,
        .thread = 1,
        .tt = SOLU_DUSR,
        .mark = SOLU_DYN_WHITE,
    };
    p = (char *)p + sizeof(solu_dalloc);
    memcpy(p, value, size);
    *(solu_usrwrap *)((char *)p + size) = (solu_usrwrap){
        .name = sf_str_cdup(name),
        .del = del,
        .tostring = tostring,
        .mark = mark,
    };

    solu_dpush(s, dh);
    return (solu_val){ .tt = SOLU_TDYN, .dyn = p };
}

solu_val solu_dnstr(solu_state *s, const char *str) {
    size_t size = strlen(str) + 1;
    if (size <= SOLU_STRCACHE_MAX - 1) {
        solu_strcache_ex sex = solu_strcache_get(&s->strcache, sf_ref(str));
        if (sex.is_ok)
            return (solu_val){ .tt = SOLU_TDYN, .dyn = sex.ok + 1 };
    }

    solu_dyn p = calloc(1, sizeof(solu_dalloc) + size);
    solu_dalloc *dh = p;
    *dh = (solu_dalloc){
        .next = NULL,
        .size = size,
        .thread = 1,
        .tt = SOLU_DSTR,
        .mark = SOLU_DYN_WHITE,
    };
    p = (char *)p + sizeof(solu_dalloc);
    memcpy(p, str, size);
    solu_dpush(s, dh);
    if (size > 1 && size <= SOLU_STRCACHE_MAX - 1)
        solu_strcache_set(&s->strcache, sf_ref(p), dh);
    return (solu_val){ .tt = SOLU_TDYN, .dyn = p };
}

solu_val solu_dnerr(solu_state *s, const char *str) {
    size_t size = strlen(str) + 1;
    solu_dyn p = calloc(1, sizeof(solu_dalloc) + size);
    solu_dalloc *dh = p;
    *dh = (solu_dalloc){
        .next = NULL,
        .size = size,
        .thread = 1,
        .tt = SOLU_DERR,
        .mark = SOLU_DYN_WHITE,
    };
    p = (char *)p + sizeof(solu_dalloc);
    memcpy(p, str, size);
    solu_dpush(s, dh);
    return (solu_val){ .tt = SOLU_TDYN, .dyn = p };
}

char *solu_tostr(solu_state *s, solu_val val) {
    switch (val.tt) {
        case SOLU_TNIL: return _strdup("nil");
        case SOLU_TF64: return sf_str_fmt("%.10f", val.f64).c_str;
        case SOLU_TI64: return sf_str_fmt("%lld", val.i64).c_str;
        case SOLU_TBOOL: return _strdup(val.boolean ? "true" : "false");
        case SOLU_TDYN: {
            switch (solu_dheader(val)->tt) {
                case SOLU_DSTR:
                case SOLU_DERR:
                return _strdup(val.dyn); break;
                case SOLU_DOBJ: {
                    solu_val f = ((solu_dobj *)val.dyn)->metafuns[SOLU_META_STR];
                    if (solu_isdtype(f, SOLU_DFUN)) {
                        solu_call_ex ex = solu_call(s, f.dyn, NULL, 0);
                        if (ex.is_ok && solu_isdtype(ex.ok, SOLU_DSTR))
                            return _strdup(ex.ok.dyn);
                    }
                }
                case SOLU_DFUN: return sf_str_fmt("%p", val.dyn).c_str;
                case SOLU_DREF: return solu_tostr(s, *(solu_val *)val.dyn);

                case SOLU_DUSR: {
                    solu_usrwrap *w = solu_uheader(val);
                    return w->tostring ? w->tostring(val.dyn) : sf_str_fmt("%p", val.dyn).c_str;
                }
                case SOLU_DCOUNT: return NULL;
            }
        }
        default: return NULL;
    }
}

solu_val solu_dobj_get(solu_state *s, solu_dobj *obj, solu_val key) {
    if ((key.tt == SOLU_TI64 && key.i64 >= 0) || (key.tt == SOLU_TF64 && key.f64 >= 0)) {
        uint32_t nkey = (uint32_t)(key.tt == SOLU_TI64 ? key.i64 : (solu_i64)key.f64);
        if (obj->array.count == 0 || nkey > obj->array.count - 1)
            return SOLU_NIL;
        return solu_valvec_get(&obj->array, nkey);
    }
    char *nkey = solu_isdtype(key, SOLU_DSTR) ? key.dyn : solu_tostr(s, key);
    solu_valmap_ex ex = solu_valmap_get(&obj->map, sf_ref(nkey));
    if (!solu_isdtype(key, SOLU_DSTR))
        free(nkey);
    return ex.is_ok ? ex.ok : SOLU_NIL;
}

void solu_dobj_set(solu_state *s, solu_dobj *obj, solu_val key, solu_val val) {
    if (solu_isdtype(val, SOLU_DFUN)) {
        solu_fproto *fp = val.dyn;
        uint32_t self = UINT32_MAX;
        for (uint32_t i = 0; i < fp->up_c; ++i) {
            if (sf_str_eq(fp->upvals[i].name, sf_lit("self"))) {
                self = i;
                break;
            }
        }
        if (self != UINT32_MAX) {
            solu_upvalue op = fp->upvals[self];
            fp->upvals[self] = (solu_upvalue){ op.name, SOLU_UP_VAL, .value = (solu_val){SOLU_TDYN, .dyn = obj} };
        }
    }
    if ((key.tt == SOLU_TI64 && key.i64 >= 0) || (key.tt == SOLU_TF64 && key.f64 >= 0)) {
        uint32_t nkey = (uint32_t)(key.tt == SOLU_TI64 ? key.i64 : (solu_i64)key.f64);
        if (nkey == obj->array.count)
            solu_valvec_push(&obj->array, val);
        else if (nkey < obj->array.count)
            solu_valvec_set(&obj->array, nkey, val);
        else {
            while (obj->array.count < nkey)
                solu_valvec_push(&obj->array, SOLU_NIL);
            solu_valvec_push(&obj->array, val);
        }
        return;
    }
    char *nkey = solu_tostr(s, key);
    solu_valmap_set(&obj->map, sf_own(nkey), val);
}

static void obj_copy(void *dest, sf_str key, solu_val val) {
    solu_dobj_strset(dest, key.c_str, val);
}
solu_val solu_djoin(solu_state *s, solu_val obj1, solu_val obj2) {
    if (!solu_isdtype(obj1, SOLU_DOBJ))
        return SOLU_NIL;
    solu_val nobj = solu_dnew(s, SOLU_DOBJ);
    *(solu_dobj *)nobj.dyn = solu_dobj_new();
    solu_dappend(nobj, obj1);
    if (solu_isdtype(obj2, SOLU_DOBJ))
        solu_dappend(nobj, obj2);
    return nobj;
}
void solu_dappend(solu_val obj1, solu_val obj2) {
    solu_dobj *obj1p = obj1.dyn, *obj2p = obj2.dyn;
    solu_valmap_foreach(&obj2p->map, obj_copy, &obj1p->map);
    if (obj2p->array.count)
        solu_valvec_append(&obj1p->array, obj2p->array.data, obj2p->array.count);
}

/*
 * GC
*/
solu_val solu_dscopy(solu_state *state, solu_val val, bool kconst) {
    if (val.tt != SOLU_TDYN)
        return val; // This function only needs to copy dynamic constants

    solu_dalloc *ov = solu_dheader(val); (void)ov;
    solu_dalloc *ac = malloc(sizeof(solu_dalloc) + solu_dheader(val)->size);
    *ac = *(solu_dheader(val));
    ac->size = solu_dheader(val)->size;
    ac->mark = SOLU_DYN_WHITE;
    ac->next = NULL;
    solu_val nv = (solu_val){SOLU_TDYN, .dyn=(char*)ac + sizeof(solu_dalloc)};

    switch (solu_dheader(nv)->tt) {
        case SOLU_DOBJ:
            *(solu_dobj *)nv.dyn = solu_dobj_new();
            solu_dappend(nv, val);
            break;
        case SOLU_DSTR:
            memcpy(nv.dyn, val.dyn, ac->size);
            break;
        case SOLU_DFUN: {
            solu_fproto *fp = val.dyn, *nfp = nv.dyn;
            memcpy(nfp, fp, sizeof(solu_fproto));
            nfp->file_name = sf_str_dup(fp->file_name);
            nfp->constants = solu_valvec_new();
            nfp->code = malloc(sizeof(solu_instruction) * fp->code_c);
            nfp->dbg = malloc(sizeof(solu_dbg) * fp->code_c);

            // Deref Upvals
            nfp->upvals = malloc(sizeof(solu_upvalue) * nfp->up_c);
            for (uint32_t i = 0; i < nfp->up_c; ++i) {
                solu_upvalue upv = fp->upvals[i];
                if (kconst) {
                    upv.name = sf_str_dup(upv.name);
                    nfp->upvals[i] = upv;
                } else {
                    solu_val nv;
                    if (upv.tt == SOLU_UP_REF) {
                        uint32_t frame = state->rcmp ? state->frames.count - 1 - upv.frame : upv.frame;
                        solu_val cv = solu_rawget(state, upv.ref, frame);
                        if (cv.tt != SOLU_TDYN) {
                            nv = solu_dnew(state, SOLU_DREF);
                            solu_rawset(state, upv.ref, nv, frame);
                            *(solu_val *)nv.dyn = cv;
                        } else nv = cv;
                    } else nv = upv.value;

                    nfp->upvals[i] = (solu_upvalue){
                        sf_str_dup(upv.name),
                        SOLU_UP_VAL,
                        .value = nv,
                        .mut = upv.mut,
                    };
                }
            }

            memcpy(nfp->code, fp->code, sizeof(solu_instruction) * fp->code_c);
            memcpy(nfp->dbg, fp->dbg, sizeof(solu_dbg) * fp->code_c);
            for (solu_val *v = fp->constants.data; v < fp->constants.data + fp->constants.count; ++v)
                solu_valvec_push(&nfp->constants, solu_dscopy(state, *v, true));
            break;
        }
        default: return SOLU_NIL;
    }
    return nv;
}

solu_val solu_dcopy(solu_state *state, solu_val val) {
    if (val.tt == SOLU_TDYN) {
        val = solu_dscopy(state, val, false);
        solu_dpush(state, solu_dheader(val));
    }
    return val;
}

void solu_dmarkfun(solu_fproto *fp) {
    for (solu_upvalue *v = fp->upvals; v && v < fp->upvals + fp->up_c; ++v) {
        if (v->tt == SOLU_UP_VAL && v->value.tt == SOLU_TDYN)
            solu_dheader(v->value)->mark = SOLU_DYN_BLACK;
    }
}
static void solu_dmarkmember(void *ud, sf_str _k, solu_val member) {
    (void)_k; (void)ud;
    solu_dmark(member);
}
void solu_dmarkobj(solu_val obj) {
    solu_dobj *dobj = (solu_dobj *)obj.dyn;
    for (uint32_t i = 0; i < dobj->array.count; ++i)
        solu_dmark(dobj->array.data[i]);
    solu_valmap_foreach(obj.dyn, solu_dmarkmember, NULL);
    if (dobj->meta.tt == SOLU_TDYN)
        solu_dheader(dobj->meta)->mark = SOLU_DYN_BLACK;
}
void solu_dmarkref(solu_val r) {
    solu_val inner = solu_dval(r);
    while (inner.tt == SOLU_TDYN) {
        if (solu_dheader(inner)->mark == SOLU_DYN_BLACK)
            return;
        solu_dheader(inner)->mark = SOLU_DYN_BLACK;
        switch (solu_dtypeof(inner)) {
            case SOLU_DREF:
                inner = solu_dval(inner);
                break;
            default: solu_dmark(inner);
        }
    }
}
void solu_dmark(solu_val val) {
    if (val.tt != SOLU_TDYN) return;
    solu_dalloc *ac = solu_dheader(val);
    if (ac->mark == SOLU_DYN_BLACK) return;
    ac->mark = SOLU_DYN_BLACK;
    if (ac->tt == SOLU_DUSR) {
        solu_usrmark mark = solu_uheader(val)->mark;
        if (mark) mark(val.dyn);
    }
    if (ac->tt == SOLU_DOBJ)
        solu_dmarkobj(val);
    if (ac->tt == SOLU_DFUN)
        solu_dmarkfun((solu_fproto *)((char *)ac + sizeof(solu_dalloc)));
    if (ac->tt == SOLU_DREF)
        solu_dmarkref(val);
}

void solu_dcollect(solu_state *s) {
    s->lb = 0;
    for (solu_val *r = s->stack.data; r < s->stack.data + s->stack.count; ++r)
        solu_dmark(*r);
    solu_dmarkobj(s->global);

    solu_dalloc **ac = &s->alloc;
    solu_dalloc *last = NULL;
    while (*ac) {
        if ((*ac)->mark == SOLU_DYN_WHITE) {
            solu_dalloc *dead = *ac;
            *ac = dead->next;
            if (dead->tt == SOLU_DSTR && dead->size < SOLU_STRCACHE_MAX)
                solu_strcache_delete(&s->strcache, sf_ref((char *)(dead + 1)));
            solu_dclean((solu_val){SOLU_TDYN, .dyn = dead + 1});
            continue;
        }
        last = *ac;
        s->lb += (*ac)->size;
        (*ac)->mark = SOLU_DYN_WHITE;
        ac = &(*ac)->next;
    }
    s->nb = (size_t)((double)s->lb * SOLU_GCSTEP);
    s->cb = s->lb;
    s->alloc_tail = last;
}

solu_val solu_getk(solu_state *s, solu_fproto *proto, uint32_t index) {
    return solu_dcopy(s, *(proto->constants.data + index));
}

#define CAT(a, b) a##b
#define EXPAND(a) a
#define EXPAND_CAT(a, b) CAT(a, b)

//#define SOLU_DBG_NOCOMPUTE
#if (defined(__GNUC__) || defined(__clang__)) && !defined(SOLU_DBG_NOCOMPUTE)
#   define LABEL(name) [name] = &&EXPAND_CAT(name, _L)
#   define CASE(name) EXPAND_CAT(name, _L):

#   define COMPUTE_GOTOS
#   define PREDISPATCH() \
        if (pc >= proto->code_c) goto ret; /* EOF */\
        ins = proto->code[pc]; /* Read next instruction */\
        \
        if (bps && SOLU_DBG_LINE(proto->dbg[pc]) > proto->dbg_ll) { /* Debugger */\
            proto->dbg_ll = SOLU_DBG_LINE(proto->dbg[pc]); \
            if (bps[proto->dbg_ll - 1]) { /* If breakpoints, check if we should break */\
                proto->dbg_res = pc; \
                ++bpc; while (!*bpc) ++bpc; /* Advance to next BP */\
                return solu_call_ex_err((solu_call_err){SOLU_ERRV_BREAK, NULL, pc}); \
            } \
        } \
        proto->dbg_ll = SOLU_DBG_LINE(proto->dbg[pc]); \
        ++pc; \
        \
        if (s->collect) { \
            solu_dcollect(s); \
            s->collect = false; \
        }
#   define DISPATCH() PREDISPATCH(); goto *computed[solu_ins_op(ins)]; /* jump up jump up and get down */

#   pragma GCC diagnostic push
#   pragma GCC diagnostic ignored "-Wpedantic"
#else
#   define DISPATCH() continue;
#   define CASE(name) case EXPAND(name):
#endif

#define solu_callerr(en, fmt, ...) (solu_call_ex_err((solu_call_err){.tt=(en),.panic=sf_str_fmt((fmt), __VA_ARGS__).c_str, .pc=pc-1}))

solu_val solu_wrapcfun(solu_state *state, solu_cfunction fptr, uint32_t arg_c, uint32_t temp_c) {
    solu_val fun = solu_dnew(state, SOLU_DFUN);
    *(solu_fproto *)fun.dyn = solu_fproto_c(fptr, arg_c, temp_c);
    return fun;
}


#if defined(_WIN32)
#include <winsock2.h>
#else
#include <arpa/inet.h>
#endif

sf_buffer solu_fproto_serialize(solu_fproto *proto) {
    sf_buffer buf = sf_buffer_grow();

    sf_buffer_insert(&buf, "[SOLC]", 6);
    sf_buffer_insert(&buf, SOLU_VERSION, sizeof(SOLU_VERSION) - 1);
    sf_buffer_autoins(&buf, &(uint64_t){htonll(proto->file_name.len)});
    sf_buffer_insert(&buf, proto->file_name.c_str, proto->file_name.len);
    sf_buffer_autoins(&buf, &(uint16_t){htons(proto->code_c)});
    sf_buffer_autoins(&buf, &(uint16_t){htons(proto->line_c)});

    sf_buffer_autoins(&buf, &(uint32_t){htonl(proto->constants.count)});
    for (solu_val *k = proto->constants.data; k < proto->constants.data + proto->constants.count; ++k) {
        if (solu_isdtype(*k, SOLU_DFUN)) {
            sf_buffer_autoins(&buf, &(uint32_t){htonl(SOLU_TCOUNT)}); // fun
            sf_buffer bfun = solu_fproto_serialize(k->dyn);
            sf_buffer_insert(&buf, bfun.ptr, bfun.size);
            sf_buffer_clear(&bfun);
            continue;
        }
        sf_buffer_autoins(&buf, &(uint32_t){htonl((uint32_t)k->tt)});
        switch (k->tt) {
            case SOLU_TI64:
            case SOLU_TF64:
                sf_buffer_autoins(&buf, &(uint64_t){htonll((uint32_t)k->i64)});
                break;
            case SOLU_TBOOL:
                sf_buffer_autoins(&buf, &k->boolean);
                break;
            case SOLU_TDYN: { // str
                size_t s = solu_dheader(*k)->size - 1;
                if (s == 0) break;
                sf_buffer_autoins(&buf, &(uint64_t){htonll(s)});
                sf_buffer_insert(&buf, k->dyn, s);
                break;
            }
            default: break; // nil
        }
    }

    sf_buffer_autoins(&buf, &(uint32_t){htonl(proto->reg_c)});
    sf_buffer_autoins(&buf, &(uint32_t){htonl(proto->arg_c)});
    uint32_t upc = proto->up_c;
    if (upc > 0 && sf_str_eq(proto->upvals[0].name, sf_lit("_g")))
        --upc;
    sf_buffer_autoins(&buf, &(uint32_t){htonl(upc)});
    for (solu_upvalue *u = proto->upvals + (proto->up_c - upc); u < proto->upvals + proto->up_c; ++u) {
        sf_buffer_autoins(&buf, &(uint64_t){htonll(u->name.len)});
        sf_buffer_insert(&buf, u->name.c_str, u->name.len);
        sf_buffer_autoins(&buf, &(uint32_t){htonl(u->frame)});
        sf_buffer_autoins(&buf, &(uint32_t){htonl(u->ref)});
        sf_buffer_autoins(&buf, &u->mut);
    }

    for (uint16_t i = 0; i < proto->code_c; ++i)
        sf_buffer_autoins(&buf, &(uint32_t){htonl(proto->code[i])});
    for (uint16_t i = 0; i < proto->code_c; ++i)
        sf_buffer_autoins(&buf, &(uint32_t){htonl(proto->dbg[i])});

    return buf;
}

void solu_savefun(solu_fproto *proto, char *path) {
    if (proto->tt != SOLU_FPROTO_BC)
        return;
    sf_buffer buf = solu_fproto_serialize(proto);
    FILE *f = fopen(path, "wb");
    if (!f) return;
    fwrite(buf.ptr, 1, buf.size, f);
    fclose(f);
}

solu_load_ex _solu_loadfun(solu_state *s, sf_buffer *buf) {
    char *name = NULL;
    solu_valvec kvec = solu_valvec_new();
    solu_fproto proto = solu_fproto_new();
    solu_upvalue *upvals = NULL;

    if (memcmp(buf->head, "[SOLC]", 6) != 0) return solu_load_ex_err(SOLU_ERRV_CORRUPT);
    buf->head += 6;
    if (memcmp(buf->head, SOLU_VERSION, sizeof(SOLU_VERSION) - 1) != 0) return solu_load_ex_err(SOLU_ERRV_OLD_BC);
    buf->head += sizeof(SOLU_VERSION) - 1;

    uint64_t n_len;
    sf_buffer_ex ex = sf_buffer_autoread(buf, &n_len);
    if (!ex.is_ok) return solu_load_ex_err(SOLU_ERRV_CORRUPT);
    n_len = ntohll(n_len);
    if (n_len > 1024) return solu_load_ex_err(SOLU_ERRV_CORRUPT);

    if (n_len > 0) {
        name = malloc(n_len + 1);
        ex = sf_buffer_read(buf, name, n_len);
        if (!ex.is_ok) goto corrupt;
        name[n_len] = 0;
    }
    proto.file_name = name ? sf_own(name) : SF_STR_EMPTY;

    ex = sf_buffer_autoread(buf, &proto.code_c);
    if (!ex.is_ok) goto corrupt;
    proto.code_c = ntohs(proto.code_c);
    ex = sf_buffer_autoread(buf, &proto.line_c);
    if (!ex.is_ok) goto corrupt;
    proto.line_c = ntohs(proto.line_c);

    uint32_t kcount;
    ex = sf_buffer_autoread(buf, &kcount);
    if (!ex.is_ok) goto corrupt;
    kcount = ntohl(kcount);
    for (uint32_t k = 0; k < kcount; ++k) {
        uint32_t tt;
        ex = sf_buffer_autoread(buf, &tt);
        if (!ex.is_ok) goto corrupt;
        tt = ntohl(tt);

        solu_val val = {(solu_ptype)tt, .dyn = NULL};
        switch (tt) {
            case SOLU_TDYN: { // str
                uint64_t slen;
                ex = sf_buffer_autoread(buf, &slen);
                if (!ex.is_ok) goto corrupt;
                slen = ntohll(slen);

                char *temp = slen == 0 ? "" : malloc(slen + 1);
                if (!temp) goto corrupt;
                ex = sf_buffer_read(buf, temp, slen);
                if (!ex.is_ok) { if (slen) free(temp); goto corrupt; }
                temp[slen] = 0;

                solu_dyn p = calloc(1, sizeof(solu_dalloc) + slen + 1);
                solu_dalloc *dh = p;
                *dh = (solu_dalloc){
                    .next = NULL,
                    .size = slen + 1,
                    .thread = 1,
                    .tt = SOLU_DSTR,
                    .mark = SOLU_DYN_GREEN,
                };
                p = (char *)p + sizeof(solu_dalloc);
                memcpy(p, temp, slen + 1);
                val.dyn = p;
                if (slen) free(temp);
                break;
            }
            case SOLU_TI64:
            case SOLU_TF64:
                ex = sf_buffer_autoread(buf, &val.i64);
                if (!ex.is_ok) goto corrupt;
                val.i64 = (int64_t)ntohll((uint32_t)val.i64);
                break;
            case SOLU_TBOOL:
                ex = sf_buffer_autoread(buf, &val.boolean);
                if (!ex.is_ok) goto corrupt;
                break;
            case SOLU_TCOUNT: { // fun
                solu_load_ex lex = _solu_loadfun(s, buf);
                if (!lex.is_ok) {
                    if (name) free(name);
                    for (solu_val *k = kvec.data; k < kvec.data + kvec.count; ++k)
                        solu_dclean(*k);
                    solu_valvec_free(&kvec);
                    return lex;
                }
                solu_dyn p = calloc(1, sizeof(solu_dalloc) + sizeof(solu_fproto));
                solu_dalloc *dh = p;
                *dh = (solu_dalloc){
                    .next = NULL,
                    .size = sizeof(solu_fproto),
                    .thread = 1,
                    .tt = SOLU_DFUN,
                    .mark = SOLU_DYN_GREEN,
                };
                p = (char *)p + sizeof(solu_dalloc);
                val = (solu_val){SOLU_TDYN, .dyn = p};
                *(solu_fproto *)val.dyn = lex.ok;
                break;
            }
            default: break; // nil
        }
        solu_valvec_push(&kvec, val);
    }

    ex = sf_buffer_autoread(buf, &proto.reg_c);
    if (!ex.is_ok) goto corrupt;
    proto.reg_c = ntohl(proto.reg_c);
    ex = sf_buffer_autoread(buf, &proto.arg_c);
    if (!ex.is_ok) goto corrupt;
    proto.arg_c = ntohl(proto.arg_c);
    ex = sf_buffer_autoread(buf, &proto.up_c);
    if (!ex.is_ok) goto corrupt;
    proto.up_c = ntohl(proto.up_c) + 1;
    upvals = calloc(proto.up_c, sizeof(solu_upvalue));
    upvals[0] = (solu_upvalue){sf_lit("_g"), SOLU_UP_VAL, .value = s->global, .mut = false};
    for (uint32_t u = 1; u < proto.up_c; ++u) {
        uint64_t slen;
        ex = sf_buffer_autoread(buf, &slen);
        if (!ex.is_ok) goto corrupt;
        slen = ntohll(slen);
        if (slen == 0) goto corrupt;

        char *temp = malloc(slen + 1);
        if (!temp) goto corrupt;
        ex = sf_buffer_read(buf, temp, slen);
        if (!ex.is_ok) { free(temp); goto corrupt; }
        temp[slen] = 0;

        uint32_t frame, ref;
        bool mut;
        ex = sf_buffer_autoread(buf, &frame);
        if (!ex.is_ok) { free(temp); goto corrupt; }
        frame = ntohl(frame);
        ex = sf_buffer_autoread(buf, &ref);
        if (!ex.is_ok) { free(temp); goto corrupt; }
        ref = ntohl(ref);
        ex = sf_buffer_autoread(buf, &mut);
        if (!ex.is_ok) { free(temp); goto corrupt; }
        upvals[u] = (solu_upvalue){sf_own(temp), SOLU_UP_REF, .ref = ref, frame, };
    }

    proto.code = malloc(sizeof(uint32_t) * proto.code_c);
    proto.dbg = malloc(sizeof(uint32_t) * proto.code_c);

    for (uint16_t i = 0; i < proto.code_c; ++i) {
        ex = sf_buffer_autoread(buf, proto.code + i);
        proto.code[i] = ntohl(proto.code[i]);
        if (!ex.is_ok) goto corrupt;
    }
    for (uint16_t i = 0; i < proto.code_c; ++i) {
        ex = sf_buffer_autoread(buf, proto.dbg + i);
        proto.dbg[i] = ntohl(proto.dbg[i]);
        if (!ex.is_ok) goto corrupt;
    }

    proto.constants = kvec;
    proto.upvals = upvals;
    return solu_load_ex_ok(proto);
corrupt:
    if (name) free(name);
    for (solu_val *k = kvec.data; k < kvec.data + kvec.count; ++k)
        solu_dclean(*k);
    solu_valvec_free(&kvec);
    if (upvals) {
        for (uint32_t u = 0; u < proto.up_c; ++u)
            sf_str_free(upvals[u].name);
        free(upvals);
    }
    if (proto.code) {
        free(proto.code);
        free(proto.dbg);
    }
    return solu_load_ex_err(SOLU_ERRV_CORRUPT);
}

solu_load_ex solu_loadfun(solu_state *state, char *path) {
    sf_fsb_ex fsb = sf_file_buffer(sf_ref(path));
    if (!fsb.is_ok) return solu_load_ex_err(SOLU_ERRC_FILE_NOT_FOUND);
    return _solu_loadfun(state, &fsb.ok);
}

static sf_str solu_dirname(sf_str path) {
    const char *slash = strrchr(path.c_str, '/');
#ifdef _WIN32
    const char *bslash = strrchr(path.c_str, '\\');
    if (!slash || (bslash && bslash > slash))
        slash = bslash;
#endif
    if (!slash)
        return sf_str_cdup(".");
    size_t len = (size_t)(slash - path.c_str);
    if (len == 0)
        len = 1;
    char *out = malloc(len + 2);
    memcpy(out, path.c_str, len);
    out[len] = '/';
    out[len + 1] = '\0';
    return sf_own(out);
}

solu_call_ex solu_call_cfun(solu_state *state, solu_fproto *proto, const solu_val *args, uint32_t arg_c) {
    solu_pushframe(state, proto->reg_c);
    for (uint32_t i = 0; i < proto->arg_c && args && i < arg_c; ++i)
        solu_set(state, i, args[i]);

    solu_call_ex ex = proto->c_fun(state);
    solu_popframe(state);
    return ex;
}

solu_call_ex solu_call_bc(solu_state *s, solu_fproto *proto, const solu_val *args, uint32_t arg_c, bool *bps) {
    if (proto->tt == SOLU_FPROTO_BC && !sf_isempty(proto->file_name))
        solu_filenames_push(&s->files, sf_own(solu_realdir(proto->file_name.c_str)));
    #ifdef COMPUTE_GOTOS
    void *computed[] = {
        LABEL(SOLU_OP_LOAD),
        LABEL(SOLU_OP_MOVE),
        LABEL(SOLU_OP_RET),
        LABEL(SOLU_OP_JMP),
        LABEL(SOLU_OP_CALL),

        LABEL(SOLU_OP_ADD),
        LABEL(SOLU_OP_SUB),
        LABEL(SOLU_OP_MUL),
        LABEL(SOLU_OP_DIV),

        LABEL(SOLU_OP_NEG),
        LABEL(SOLU_OP_EQ),
        LABEL(SOLU_OP_LT),
        LABEL(SOLU_OP_LE),

        LABEL(SOLU_OP_SETU),
        LABEL(SOLU_OP_GETU),
        LABEL(SOLU_OP_REFU),

        LABEL(SOLU_OP_NEW),
        LABEL(SOLU_OP_SET),
        LABEL(SOLU_OP_GET),
        LABEL(SOLU_OP_PUSH),

        LABEL(SOLU_OP_SUPO),
        LABEL(SOLU_OP_GUPO),

        LABEL(SOLU_OP_UNKNOWN),
    };
    #endif

    solu_instruction ins;
    uint32_t pc = proto->dbg_res ? proto->dbg_res : 0;
    if (!proto->dbg_res) {
        solu_pushframe(s, proto->reg_c);
        for (uint32_t i = 0; i < proto->arg_c && args && i < arg_c; ++i)
            solu_set(s, i, args[i]);
    }
    proto->dbg_res = 0;
    solu_val return_val = SOLU_NIL;
    bool *bpc = bps;

    #ifdef COMPUTE_GOTOS
    DISPATCH();
    #else
    while (pc < proto->code_c) {
        PREDISPATCH();
        switch (solu_ins_op(ins)) {
    #endif
        CASE(SOLU_OP_LOAD) {
            solu_set(s, solu_iab_a(ins), solu_dcopy(s, solu_valvec_get(&proto->constants, solu_iab_b(ins))));
            DISPATCH();
        }
        CASE(SOLU_OP_MOVE) {
            solu_set(s, solu_iab_a(ins), solu_get(s, solu_iab_b(ins)));
            DISPATCH();
        }
        CASE(SOLU_OP_RET) {
            return_val = solu_get(s, (uint32_t)solu_ia_a(ins));
            goto ret;
        }
        CASE(SOLU_OP_JMP) {
            pc = (uint32_t)((int32_t)pc + solu_ia_a(ins));
            DISPATCH();
        }
        CASE(SOLU_OP_CALL) {
            uint32_t fun_r = solu_iabc_bx(ins);
            solu_val fun = solu_get(s, fun_r);
            if (solu_isdtype(fun, SOLU_DOBJ)) {
                solu_val call = ((solu_dobj *)fun.dyn)->metafuns[SOLU_META_CALL];
                if (solu_isdtype(call, SOLU_DFUN))
                    fun = call;
            }
            if (!solu_isdtype(fun, SOLU_DFUN)) {
                if (solu_isdtype(fun, SOLU_DERR))
                    return solu_callerr(SOLU_ERRV_TYPE_MISMATCH, "Attempted to call type %s: %s", solu_typename(fun).c_str, fun.dyn);
                return solu_callerr(SOLU_ERRV_TYPE_MISMATCH, "Attempted to call type %s", solu_typename(fun).c_str);
            }

            solu_fproto *f = fun.dyn;
            solu_call_ex fex;
            uint32_t argc = solu_iabc_cx(ins);
            if (argc > 0) {
                solu_val *argv = calloc(argc, sizeof(solu_val));
                for (uint32_t i = 0; i < argc && i < s->frames.data[s->frames.count - 1].size; ++i)
                    argv[i] = solu_get(s, fun_r + i + 1);
                fex = solu_call(s, f, argv, argc);
                free(argv);
            } else fex = solu_call(s, f, NULL, 0);

            if (!fex.is_ok) {
                fex.err.pc = pc - 1;
                return fex;
            }
            solu_set(s, solu_iabc_a(ins), fex.ok);
            DISPATCH();
        }

        CASE(SOLU_OP_ADD) {
            solu_val lhs = solu_iabc_bk(ins) ? solu_getk(s, proto, solu_iabc_bx(ins)) : solu_get(s, solu_iabc_bx(ins));
            solu_val rhs = solu_iabc_ck(ins) ? solu_getk(s, proto, solu_iabc_cx(ins)) : solu_get(s, solu_iabc_cx(ins));
            if (solu_isdtype(lhs, SOLU_DERR))
                return solu_callerr(SOLU_ERRV_PANIC, "%s", lhs.dyn);
            if (solu_isdtype(rhs, SOLU_DERR))
                return solu_callerr(SOLU_ERRV_PANIC, "%s", rhs.dyn);

            if (solu_isdtype(lhs, SOLU_DOBJ) && !solu_isdtype(rhs, SOLU_DOBJ)) {
                solu_valvec_push(&((solu_dobj *)lhs.dyn)->array, rhs);
                DISPATCH();
            }
            if (lhs.tt != rhs.tt && (lhs.tt == SOLU_TDYN || rhs.tt == SOLU_TDYN))
                return solu_callerr(SOLU_ERRV_TYPE_MISMATCH, "Implicit conversion %s into %s", solu_typename(rhs).c_str, solu_typename(lhs).c_str);
            if (lhs.tt != rhs.tt) {
                switch (lhs.tt) {
                    case SOLU_TI64: rhs = (solu_val){.tt = SOLU_TI64, .i64 = (solu_i64)rhs.f64}; break;
                    case SOLU_TF64: rhs = (solu_val){.tt = SOLU_TF64, .f64 = (solu_f64)rhs.i64}; break;
                    default: return solu_callerr(SOLU_ERRV_TYPE_MISMATCH, "Implicit conversion %s into %s", solu_typename(rhs).c_str, solu_typename(lhs).c_str);
                }
            }
            switch (lhs.tt) {
                case SOLU_TF64:
                    solu_set(s, solu_iabc_a(ins), (solu_val){.tt = SOLU_TF64, .f64 = lhs.f64 + rhs.f64});
                    break;
                case SOLU_TI64:
                    solu_set(s, solu_iabc_a(ins), (solu_val){.tt = SOLU_TI64, .i64 = lhs.i64 + rhs.i64});
                    break;
                case SOLU_TDYN: {
                    switch (solu_dtypeof(lhs)) {
                        case SOLU_DSTR: {
                            sf_str l =  sf_str_join(sf_ref(lhs.dyn), sf_ref(rhs.dyn));
                            solu_set(s, solu_iabc_a(ins), solu_dnstr(s, l.c_str));
                            sf_str_free(l);
                            break;
                        }
                        case SOLU_DOBJ: {
                            if (solu_iabc_a(ins) == solu_iabc_bx(ins))
                                solu_dappend(lhs, rhs);
                            else solu_set(s, solu_iabc_a(ins), solu_djoin(s, lhs, rhs));
                            break;
                        }
                        default:
                            return solu_callerr(SOLU_ERRV_TYPE_MISMATCH, "Type %s does not support operator '+'", solu_typename(lhs).c_str);
                    }
                    break;
                }
                default:
                    return solu_callerr(SOLU_ERRV_TYPE_MISMATCH, "Type %s does not support operator '+'", solu_typename(lhs).c_str);
            }
            DISPATCH();
        }
        CASE(SOLU_OP_SUB) {
            solu_val lhs = solu_iabc_bk(ins) ? solu_getk(s, proto, solu_iabc_bx(ins)) : solu_get(s, solu_iabc_bx(ins));
            solu_val rhs = solu_iabc_ck(ins) ? solu_getk(s, proto, solu_iabc_cx(ins)) : solu_get(s, solu_iabc_cx(ins));
            if (solu_isdtype(lhs, SOLU_DERR))
                return solu_callerr(SOLU_ERRV_PANIC, "%s", lhs.dyn);
            if (solu_isdtype(rhs, SOLU_DERR))
                return solu_callerr(SOLU_ERRV_PANIC, "%s", rhs.dyn);

            if (solu_isdtype(lhs, SOLU_DOBJ) && solu_isdtype(rhs, SOLU_DSTR) &&
                solu_iabc_a(ins) == solu_iabc_bx(ins)) {
                solu_valmap_delete(&((solu_dobj *)lhs.dyn)->map, sf_ref(rhs.dyn));
                DISPATCH();
            }

            if (lhs.tt != SOLU_TI64 && lhs.tt != SOLU_TF64)
                return solu_callerr(SOLU_ERRV_TYPE_MISMATCH, "Type %s does not support operator '-'", solu_typename(lhs).c_str);
            if (rhs.tt != SOLU_TI64 && rhs.tt != SOLU_TF64)
                return solu_callerr(SOLU_ERRV_TYPE_MISMATCH, "Type %s does not support operator '-'", solu_typename(rhs).c_str);
            if (lhs.tt != rhs.tt) {
                switch (lhs.tt) {
                    case SOLU_TI64: rhs = (solu_val){.tt = SOLU_TI64, .i64 = (solu_i64)rhs.f64}; break;
                    case SOLU_TF64: rhs = (solu_val){.tt = SOLU_TF64, .f64 = (solu_f64)rhs.i64}; break;
                    default: return solu_callerr(SOLU_ERRV_TYPE_MISMATCH, "Type %s does not support operator '-'", solu_typename(lhs).c_str);
                }
            }
            switch (lhs.tt) {
                case SOLU_TF64:
                    solu_set(s, solu_iabc_a(ins), (solu_val){.tt = SOLU_TF64, .f64 = lhs.f64 - rhs.f64});
                    break;
                case SOLU_TI64:
                    solu_set(s, solu_iabc_a(ins), (solu_val){.tt = SOLU_TI64, .i64 = lhs.i64 - rhs.i64});
                    break;
                default: return solu_callerr(SOLU_ERRV_TYPE_MISMATCH, "Type %s does not support operator '-'", solu_typename(lhs).c_str); break;
            }
            DISPATCH();
        }
        CASE(SOLU_OP_MUL) {
            solu_val lhs = solu_iabc_bk(ins) ? solu_getk(s, proto, solu_iabc_bx(ins)) : solu_get(s, solu_iabc_bx(ins));
            solu_val rhs = solu_iabc_ck(ins) ? solu_getk(s, proto, solu_iabc_cx(ins)) : solu_get(s, solu_iabc_cx(ins));
            if (solu_isdtype(lhs, SOLU_DERR))
                return solu_callerr(SOLU_ERRV_PANIC, "%s", lhs.dyn);
            if (solu_isdtype(rhs, SOLU_DERR))
                return solu_callerr(SOLU_ERRV_PANIC, "%s", rhs.dyn);

            if (lhs.tt != SOLU_TI64 && lhs.tt != SOLU_TF64)
                return solu_callerr(SOLU_ERRV_TYPE_MISMATCH, "Type %s does not support operator '*'", solu_typename(lhs).c_str);
            if (rhs.tt != SOLU_TI64 && rhs.tt != SOLU_TF64)
                return solu_callerr(SOLU_ERRV_TYPE_MISMATCH, "Type %s does not support operator '*'", solu_typename(rhs).c_str);
            if (lhs.tt != rhs.tt) {
                switch (lhs.tt) {
                    case SOLU_TI64: rhs = (solu_val){.tt = SOLU_TI64, .i64 = (solu_i64)rhs.f64}; break;
                    case SOLU_TF64: rhs = (solu_val){.tt = SOLU_TF64, .f64 = (solu_f64)rhs.i64}; break;
                    default: return solu_callerr(SOLU_ERRV_TYPE_MISMATCH, "Type %s does not support operator '*'", solu_typename(lhs).c_str);
                }
            }
            switch (lhs.tt) {
                case SOLU_TF64:
                    solu_set(s, solu_iabc_a(ins), (solu_val){.tt = SOLU_TF64, .f64 = lhs.f64 * rhs.f64});
                    break;
                case SOLU_TI64:
                    solu_set(s, solu_iabc_a(ins), (solu_val){.tt = SOLU_TI64, .i64 = lhs.i64 * rhs.i64});
                    break;
                default: return solu_callerr(SOLU_ERRV_TYPE_MISMATCH, "Type %s does not support operator '*'", NULL); break;
            }
            DISPATCH();
        }
        CASE(SOLU_OP_DIV) {
            solu_val lhs = solu_iabc_bk(ins) ? solu_getk(s, proto, solu_iabc_bx(ins)) : solu_get(s, solu_iabc_bx(ins));
            solu_val rhs = solu_iabc_ck(ins) ? solu_getk(s, proto, solu_iabc_cx(ins)) : solu_get(s, solu_iabc_cx(ins));
            if (solu_isdtype(lhs, SOLU_DERR))
                return solu_callerr(SOLU_ERRV_PANIC, "%s", lhs.dyn);
            if (solu_isdtype(rhs, SOLU_DERR))
                return solu_callerr(SOLU_ERRV_PANIC, "%s", rhs.dyn);

            if (lhs.tt != SOLU_TI64 && lhs.tt != SOLU_TF64)
                return solu_callerr(SOLU_ERRV_TYPE_MISMATCH, "Type %s does not support operator '/'", solu_typename(lhs).c_str);
            if (rhs.tt != SOLU_TI64 && rhs.tt != SOLU_TF64)
                return solu_callerr(SOLU_ERRV_TYPE_MISMATCH, "Type %s does not support operator '/'", solu_typename(rhs).c_str);
            if (lhs.tt != rhs.tt) {
                switch (lhs.tt) {
                    case SOLU_TI64: rhs = (solu_val){.tt = SOLU_TI64, .i64 = (solu_i64)rhs.f64}; break;
                    case SOLU_TF64: rhs = (solu_val){.tt = SOLU_TF64, .f64 = (solu_f64)rhs.i64}; break;
                    default: return solu_callerr(SOLU_ERRV_TYPE_MISMATCH, "Type %s does not support operator '/'", solu_typename(lhs).c_str);
                }
            }
            switch (lhs.tt) {
                case SOLU_TF64:
                    solu_set(s, solu_iabc_a(ins), (solu_val){.tt = SOLU_TF64, .f64 = lhs.f64 / rhs.f64});
                    break;
                case SOLU_TI64:
                    solu_set(s, solu_iabc_a(ins), (solu_val){.tt = SOLU_TI64, .i64 = lhs.i64 / rhs.i64});
                    break;
                default: return solu_callerr(SOLU_ERRV_TYPE_MISMATCH, "Type %s does not support operator '/'", solu_typename(lhs).c_str);
            }
            DISPATCH();
        }

        CASE(SOLU_OP_NEG) {
            solu_val in = solu_get(s, solu_iab_b(ins));
            switch (in.tt) {
                case SOLU_TI64: in.i64 = -in.i64; break;
                case SOLU_TF64: in.f64 = -in.f64; break;
                case SOLU_TBOOL: in.boolean = !in.boolean; break;
                default: return solu_callerr(SOLU_ERRV_TYPE_MISMATCH, "Type %s does not support prefix operator '-'", solu_typename(in).c_str);
            }
            solu_set(s, solu_iab_a(ins), in);
            DISPATCH();
        }
        CASE(SOLU_OP_EQ) {
            bool inv = solu_iabc_a(ins) != 0;
            solu_val lhs = solu_iabc_bk(ins) ? solu_getk(s, proto, solu_iabc_bx(ins)) : solu_get(s, solu_iabc_bx(ins));
            solu_val rhs = solu_iabc_ck(ins) ? solu_getk(s, proto, solu_iabc_cx(ins)) : solu_get(s, solu_iabc_cx(ins));
            if ((lhs.tt == SOLU_TNIL && rhs.tt == SOLU_TNIL)) {
                if (!inv) pc++;
                DISPATCH();
            }
            if (lhs.tt == SOLU_TBOOL && rhs.tt == SOLU_TDYN) {
                if (inv ? !lhs.boolean : lhs.boolean) pc++;
                DISPATCH();
            }
            if (lhs.tt == SOLU_TDYN && rhs.tt == SOLU_TBOOL) {
                if (inv ? !rhs.boolean : rhs.boolean) pc++;
                DISPATCH();
            }

            if (lhs.tt != rhs.tt) {
                if (lhs.tt == SOLU_TDYN || rhs.tt == SOLU_TDYN || lhs.tt == SOLU_TNIL || rhs.tt == SOLU_TNIL) {
                    if (inv) pc++;
                    DISPATCH();
                }
                switch (lhs.tt) {
                    case SOLU_TI64: rhs = (solu_val){.tt = SOLU_TI64, .i64 = rhs.tt == SOLU_TBOOL ? (lhs.boolean ? 1 : 0) : (solu_i64)rhs.f64}; break;
                    case SOLU_TF64: rhs = (solu_val){.tt = SOLU_TF64, .f64 = rhs.tt == SOLU_TBOOL ? (lhs.boolean ? 1 : 0) : (solu_f64)rhs.i64}; break;
                    case SOLU_TBOOL: rhs = (solu_val){.tt = SOLU_TBOOL, .boolean = rhs.tt == SOLU_TI64 ? rhs.i64 != 0 : rhs.f64 != 0};
                    default: return solu_callerr(SOLU_ERRV_TYPE_MISMATCH, "Unknown Type", NULL);
                }
            }

            bool e = false;
            switch (lhs.tt) {
                case SOLU_TI64: e = lhs.i64 == rhs.i64; break;
                case SOLU_TF64: e = lhs.f64 == rhs.f64; break;
                case SOLU_TBOOL: e = lhs.boolean == rhs.boolean; break;

                case SOLU_TDYN: {
                    solu_dalloc *h1 = solu_dheader(lhs);
                    solu_dalloc *h2 = solu_dheader(rhs);
                    if (h1->tt != h2->tt) {
                        e = false;
                        break;
                    }
                    switch (h1->tt) {
                        case SOLU_DSTR: e = lhs.dyn == rhs.dyn || strcmp(lhs.dyn, rhs.dyn) == 0; break;
                        case SOLU_DOBJ:
                        case SOLU_DFUN: e = lhs.dyn == rhs.dyn; break;
                        default: return solu_callerr(SOLU_ERRV_TYPE_MISMATCH, "Unknown Type", NULL);
                    }
                }
                default: break;
            }

            e = inv ? !e : e;
            if (e) pc++;
            DISPATCH();
        }
        CASE(SOLU_OP_LT) {
            bool inv = solu_iabc_a(ins) != 0;
            solu_val lhs = solu_iabc_bk(ins) ? solu_getk(s, proto, solu_iabc_bx(ins)) : solu_get(s, solu_iabc_bx(ins));
            solu_val rhs = solu_iabc_ck(ins) ? solu_getk(s, proto, solu_iabc_cx(ins)) : solu_get(s, solu_iabc_cx(ins));
            if (lhs.tt == SOLU_TDYN || rhs.tt == SOLU_TDYN || lhs.tt == SOLU_TNIL || rhs.tt == SOLU_TNIL ||
                lhs.tt == SOLU_TBOOL || rhs.tt == SOLU_TBOOL) {
                if (inv) pc++;
                DISPATCH();
            }
            if (lhs.tt != rhs.tt) {
                switch (lhs.tt) {
                    case SOLU_TI64: rhs = (solu_val){.tt = SOLU_TI64, .i64 = (solu_i64)rhs.f64}; break;
                    case SOLU_TF64: rhs = (solu_val){.tt = SOLU_TF64, .f64 = (solu_f64)rhs.i64}; break;
                    default: break;
                }
            }
            bool e = false;
            switch (lhs.tt) {
                case SOLU_TI64: e = lhs.i64 < rhs.i64; break;
                case SOLU_TF64: e = lhs.f64 < rhs.f64; break;
                default: break;
            }

            e = inv ? !e : e;
            if (e) pc++;
            DISPATCH();
        }
        CASE(SOLU_OP_LE) {bool inv = solu_iabc_a(ins) != 0;
            solu_val lhs = solu_iabc_bk(ins) ? solu_getk(s, proto, solu_iabc_bx(ins)) : solu_get(s, solu_iabc_bx(ins));
            solu_val rhs = solu_iabc_ck(ins) ? solu_getk(s, proto, solu_iabc_cx(ins)) : solu_get(s, solu_iabc_cx(ins));
            if (lhs.tt == SOLU_TDYN || rhs.tt == SOLU_TDYN || lhs.tt == SOLU_TNIL || rhs.tt == SOLU_TNIL ||
                lhs.tt == SOLU_TBOOL || rhs.tt == SOLU_TBOOL) {
                if (inv) pc++;
                DISPATCH();
            }

            if (lhs.tt != rhs.tt) {
                if (lhs.tt == SOLU_TDYN || rhs.tt == SOLU_TDYN || lhs.tt == SOLU_TNIL || rhs.tt == SOLU_TNIL) {
                    if (inv) pc++;
                    DISPATCH();
                }
                switch (lhs.tt) {
                    case SOLU_TI64: rhs = (solu_val){.tt = SOLU_TI64, .i64 = (solu_i64)rhs.f64}; break;
                    case SOLU_TF64: rhs = (solu_val){.tt = SOLU_TF64, .f64 = (solu_f64)rhs.i64}; break;
                    default: break;
                }
            }

            bool e = false;
            switch (lhs.tt) {
                case SOLU_TI64: e = lhs.i64 <= rhs.i64; break;
                case SOLU_TF64: e = lhs.f64 <= rhs.f64; break;
                default: break;
            }

            e = inv ? !e : e;
            if (e) pc++;
            DISPATCH();
        }

        CASE(SOLU_OP_SETU) {
            solu_val v = solu_get(s, solu_iab_b(ins));
            solu_upvalue *upv = proto->upvals + solu_iab_a(ins);
            if (upv->tt == SOLU_UP_VAL) {
                if (solu_isdtype(upv->value, SOLU_DREF)) {
                    *(solu_val *)upv->value.dyn = v;
                    DISPATCH();
                }
                upv->value = v;
            } else solu_rawset(s, upv->ref, v, upv->frame);
            DISPATCH();
        }
        CASE(SOLU_OP_GETU) {
            solu_upvalue *upv = proto->upvals + solu_iab_b(ins);
            if (upv->tt == SOLU_UP_VAL) {
                solu_set(s, solu_iab_a(ins), solu_isdtype(upv->value, SOLU_DREF) ?
                    *(solu_val *)upv->value.dyn : upv->value);
            } else solu_set(s, solu_iab_a(ins), solu_rawget(s, upv->ref, upv->frame));
            DISPATCH();
        }
        CASE(SOLU_OP_REFU) {
            solu_val v = solu_get(s, (uint32_t)solu_ia_a(ins));
            if (solu_isdtype(v, SOLU_DREF))
                DISPATCH();
            solu_val vref = solu_dnew(s, SOLU_DREF);
            *(solu_val *)vref.dyn = v;
            solu_set(s, (uint32_t)solu_ia_a(ins), vref);
            DISPATCH();
        }

        CASE(SOLU_OP_NEW) {
            solu_set(s, (uint32_t)solu_ia_a(ins), solu_dnew(s, SOLU_DOBJ));
            DISPATCH();
        }
        CASE(SOLU_OP_SET) {
            solu_val obj = solu_get(s, solu_iabc_a(ins));
            solu_val key = solu_iabc_bk(ins) ? solu_getk(s, proto, solu_iabc_bx(ins)) : solu_get(s, solu_iabc_bx(ins));
            solu_val val = solu_iabc_ck(ins) ? solu_getk(s, proto, solu_iabc_cx(ins)) : solu_get(s, solu_iabc_cx(ins));
            if (!solu_isdtype(obj, SOLU_DOBJ))
                return solu_callerr(SOLU_ERRV_TYPE_MISMATCH, "Attempted to index type %s", solu_typename(obj).c_str);
            solu_val set = ((solu_dobj *)obj.dyn)->metafuns[SOLU_META_SET];
            if (solu_isdtype(set, SOLU_DFUN)) {
                solu_call_ex ex = solu_call(s, set.dyn, (solu_val[]){key, val}, 2);
                if (!ex.is_ok) return ex;
                DISPATCH();
            }
            solu_dalloc *dh = solu_dheader(val); (void)dh;
            solu_dobj_set(s, obj.dyn, key, val);
            DISPATCH();
        }
        CASE(SOLU_OP_GET) {
            solu_val obj = solu_get(s, solu_iabc_bx(ins));
            solu_val key = solu_iabc_ck(ins) ? solu_getk(s, proto, solu_iabc_cx(ins)) : solu_get(s, solu_iabc_cx(ins));
            if (!solu_isdtype(obj, SOLU_DOBJ))
                return solu_callerr(SOLU_ERRV_TYPE_MISMATCH, "Attempted to index type %s", solu_typename(obj).c_str);
            solu_val get = ((solu_dobj *)obj.dyn)->metafuns[SOLU_META_GET];
            if (solu_isdtype(get, SOLU_DFUN)) {
                solu_call_ex ex = solu_call(s, get.dyn, (solu_val[]){key}, 1);
                if (!ex.is_ok) return ex;
                DISPATCH();
            }
            solu_set(s, solu_iabc_a(ins), solu_dobj_get(s, obj.dyn, key));
            DISPATCH();
        }
        CASE(SOLU_OP_PUSH) {
            solu_val obj = solu_get(s, solu_iab_a(ins));
            if (!solu_isdtype(obj, SOLU_DOBJ))
                return solu_callerr(SOLU_ERRV_TYPE_MISMATCH, "Attempted to push to type %s", solu_typename(obj).c_str);
            solu_valvec_push(&((solu_dobj *)obj.dyn)->array, solu_get(s, solu_iab_b(ins)));
            DISPATCH();
        }

        CASE(SOLU_OP_SUPO) {
            solu_upvalue *upv = proto->upvals + solu_iabc_a(ins);
            solu_val upo = upv->tt == SOLU_UP_VAL ? upv->value : solu_rawget(s, upv->ref, upv->frame);
            if (!solu_isdtype(upo, SOLU_DOBJ))
                return solu_callerr(SOLU_ERRV_CORRUPT, "Corrupt bytecode", NULL);
            solu_val kkey = solu_valvec_get(&proto->constants, solu_iabc_bx(ins));
            if (!solu_isdtype(kkey, SOLU_DSTR))
                return solu_callerr(SOLU_ERRV_CORRUPT, "Corrupt bytecode", NULL);
            solu_val val = solu_iabc_ck(ins) ? solu_getk(s, proto, solu_iabc_cx(ins)) : solu_get(s, solu_iabc_cx(ins));
            solu_dobj_strset(upo.dyn, kkey.dyn, val);
            DISPATCH();
        }
        CASE(SOLU_OP_GUPO) {
            solu_upvalue *upv = proto->upvals + solu_iabc_bx(ins);
            solu_val upo = upv->tt == SOLU_UP_VAL ? upv->value : solu_rawget(s, upv->ref, upv->frame);
            if (!solu_isdtype(upo, SOLU_DOBJ))
                return solu_callerr(SOLU_ERRV_CORRUPT, "Corrupt bytecode", NULL);
            solu_val kkey = solu_valvec_get(&proto->constants, solu_iabc_cx(ins));
            if (!solu_isdtype(kkey, SOLU_DSTR))
                return solu_callerr(SOLU_ERRV_CORRUPT, "Corrupt bytecode", NULL);
            solu_set(s, solu_iabc_a(ins), solu_dobj_strget(upo.dyn, kkey.dyn));
            DISPATCH();
        }

        CASE(SOLU_OP_UNKNOWN) { DISPATCH(); }
    #ifndef COMPUTE_GOTOS
        }
    }
    #endif

ret: {}
    proto->dbg_res = 0;
    proto->dbg_ll = 0;
    solu_popframe(s);
    if (proto->tt == SOLU_FPROTO_BC && proto->file_name.len > 0)
        sf_str_free(solu_filenames_pop(&s->files));
    return solu_call_ex_ok(return_val);
}

solu_call_ex solu_call(solu_state *state, solu_fproto *proto, const solu_val *args, uint32_t arg_c) {
    if (state->call_stack > CALL_STACK_MAX)
        return solu_panic("Stack Overflow");

    ++state->call_stack;
    if (proto->tt == SOLU_FPROTO_BC) {
        solu_call_ex ex = solu_call_bc(state, proto, args, arg_c, NULL);
        if (!ex.is_ok) {
            solu_popframe(state);
            sf_str_free(solu_filenames_pop(&state->files));
        }
        --state->call_stack;
        return ex;
    }
    solu_call_ex ex = solu_call_cfun(state, proto, args, arg_c);
    --state->call_stack;
    return ex;
}

solu_call_ex solu_dcall(solu_state *state, solu_fproto *proto, const solu_val *args, uint32_t arg_c, bool *bps) {
    if (state->call_stack > CALL_STACK_MAX)
        return solu_panic("Stack Overflow");

    ++state->call_stack;
    if (proto->tt == SOLU_FPROTO_BC) {
        solu_call_ex ex = solu_call_bc(state, proto, args, arg_c, bps);
        if (!ex.is_ok)
            sf_str_free(solu_filenames_pop(&state->files));
        --state->call_stack;
        return ex;
    }
    solu_call_ex ex = solu_call_cfun(state, proto, args, arg_c);
    --state->call_stack;
    return ex;
}

#if (defined(__GNUC__) || defined(__clang__)) && !defined(SOLU_DBG_NOCOMPUTE)
#pragma GCC diagnostic pop
#endif

solu_call_ex solu_err(solu_state *s, char *fmt, ...) {
    va_list arglist;

    va_start(arglist, fmt);
    const size_t size =
        (size_t)vsnprintf(NULL, 0, fmt, arglist);
    va_end(arglist);

    char *_fmt = calloc(1, size + 1);
    va_start(arglist, fmt);
    vsnprintf(_fmt, size + 1, fmt, arglist);
    va_end(arglist);

    solu_val err = solu_dnerr(s, _fmt);
    free(_fmt);

    return solu_call_ex_ok(err);
}

solu_call_ex solu_panic(char *fmt, ...) {
    va_list arglist;

    va_start(arglist, fmt);
    const size_t size =
        (size_t)vsnprintf(NULL, 0, fmt, arglist);
    va_end(arglist);

    char *_fmt = calloc(1, size + 1);
    va_start(arglist, fmt);
    vsnprintf(_fmt, size + 1, fmt, arglist);
    va_end(arglist);

    return solu_call_ex_err((solu_call_err){SOLU_ERRV_PANIC, _fmt, 0});
}
