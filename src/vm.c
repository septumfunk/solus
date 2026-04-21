#include <math.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdlib.h>
#include <string.h>
#include "solus/vm.h"
#include "sf/containers/buffer.h"
#include "sf/fs.h"
#include "solus/bytecode.h"
#include "solus/val.h"
#include "solus/compiler.h"
#include "sf/str.h"
#include "std/std.h"

#define CALL_STACK_MAX 1024

solu_state *solu_state_new(void) {
    solu_dyn p = calloc(1, sizeof(solu_dalloc) + sizeof(solu_dobj));
    *(solu_dalloc *)p = (solu_dalloc){NULL, sizeof(solu_dobj), 1, SOLU_DOBJ, SOLU_DYN_WHITE, true, SOLU_NIL, {SOLU_NIL}};
    p = (char *)p + sizeof(solu_dalloc);
    *(solu_dobj *)p = solu_dobj_new();

    solu_state *s = malloc(sizeof(solu_state));
    *s = (solu_state){
        .stack = solu_valvec_new(),
        .strcache = solu_strcache_new(),
        .frames = solu_frames_new(),
        .global = {SOLU_TDYN, .dyn = p},
        .lb = 1<<20, .cb = 0, .nb = 0,
        .call_stack = 0,
        .ccall = NULL,

        .collect = false,
        .alloc = NULL,
    };
    return s;
}

void solu_state_free(solu_state *state) {
    solu_valvec_free(&state->stack);
    solu_frames_free(&state->frames);
    solu_strcache_free(&state->strcache);
    solu_dclean(state->global);
    free(state);
}

void solu_usestd(solu_state *s) {
    solu_val solus = solu_dnew(s, SOLU_DOBJ);
    solu_dobj_strset(solus.dyn, "version", solu_dnstr(s, SOLU_VERSION));
    solu_dobj_strset(solus.dyn, "git", solu_dnstr(s, SOLU_GIT));
    solu_dobj_strset(s->global.dyn, "solus", solus);

    s->meta.string = solu_mod_string(s, true);
    s->meta.obj = solu_mod_obj(s, true);
    solu_dhold(s->meta.string);
    solu_dhold(s->meta.obj);

    solu_mod_builtin(s);
    solu_mod_string(s, false);
    solu_mod_obj(s, false);
    solu_mod_io(s);
    solu_mod_math(s);
    solu_mod_gc(s);
}

solu_compile_ex solu_csrc(solu_state *state, char *src) {
    solu_compile_ex ex = solu_cproto(SF_STR_EMPTY, src, 0, NULL, 1, (solu_upvalue[]){
        (solu_upvalue){sf_lit("global"), SOLU_UP_VAL, .value = state->global, .mut = false}
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
    if (!realpath) {
        sf_buffer_clear(&fsb.ok);
        return solu_compile_ex_err((solu_compile_err){SOLU_ERRC_FILE_NOT_FOUND, 0, 0});
    }

    solu_compile_ex ex = solu_cproto(sf_ref(realpath), (char *)fsb.ok.ptr, 0, NULL, 1, (solu_upvalue[]){
        (solu_upvalue){sf_lit("global"), SOLU_UP_VAL, .value = state->global, .mut = false}
    });
    free(realpath);
    if (!ex.is_ok) {
        sf_buffer_clear(&fsb.ok);
        return ex;
    }
    ex.ok.line_c = 1;
    for (char *c = (char *)fsb.ok.ptr; *c != '\0'; ++c)
        if (*c == '\n') ++ex.ok.line_c;
    sf_buffer_clear(&fsb.ok);
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
        .metadata = {[SOLU_META_EXTEND] = s->meta.base}
    };
    p = (char *)p + sizeof(solu_dalloc);

    switch (tt) {
        case SOLU_DSTR:
        case SOLU_DERR: break;
        case SOLU_DOBJ:
            *(solu_dobj *)p = solu_dobj_new();
            if (s->meta.obj.tt != SOLU_TNIL)
                dh->metadata[SOLU_META_EXTEND] = s->meta.obj;
            break;
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
    solu_usrdel del, solu_usrmark mark) {
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
        .metadata = {[SOLU_META_EXTEND] = s->meta.string}
    };
    p = (char *)p + sizeof(solu_dalloc);
    memcpy(p, str, size);
    solu_dpush(s, dh);
    if (size > 1 && size <= SOLU_STRCACHE_MAX - 1)
        solu_strcache_set(&s->strcache, sf_str_cdup(p), dh);
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
        .metadata = {[SOLU_META_EXTEND] = s->meta.base}
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
            solu_val f = solu_dheader(val)->metadata[SOLU_META_STR];
            if (f.tt != SOLU_TNIL) {
                solu_call_ex ex = solu_call(s, f.dyn, &val, 1);
                if (ex.is_ok && solu_isdtype(ex.ok, SOLU_DSTR))
                    return _strdup(ex.ok.dyn);
            }
            switch (solu_dheader(val)->tt) {
                case SOLU_DSTR:
                case SOLU_DERR:
                return _strdup(val.dyn); break;
                case SOLU_DOBJ:
                case SOLU_DUSR:
                case SOLU_DFUN: return sf_str_fmt("%p", val.dyn).c_str;
                case SOLU_DREF: return solu_tostr(s, *(solu_val *)val.dyn);
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
    solu_val ex = solu_dobj_strget(obj, nkey);
    if (!solu_isdtype(key, SOLU_DSTR))
        free(nkey);
    return ex;
}

void solu_dobj_set(solu_state *s, solu_dobj *obj, solu_val key, solu_val val) {
    if (solu_isdtype(val, SOLU_DFUN)) {
        solu_fproto *fp = val.dyn;
        if (fp->self) {
            solu_upvalue op = fp->upvals[1];
            fp->upvals[1] = (solu_upvalue){ op.name, SOLU_UP_VAL, .value = (solu_val){SOLU_TDYN, .dyn = obj} };
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

    solu_dalloc *ac = malloc(sizeof(solu_dalloc) + solu_dheader(val)->size);
    *ac = *(solu_dheader(val));
    ac->size = solu_dheader(val)->size;
    ac->mark = SOLU_DYN_WHITE;
    ac->held = false;
    ac->next = NULL;
    solu_val nv = (solu_val){SOLU_TDYN, .dyn=(char*)ac + sizeof(solu_dalloc)};

    switch (solu_dheader(nv)->tt) {
        case SOLU_DSTR:
            memcpy(nv.dyn, val.dyn, ac->size);
            ac->metadata[SOLU_META_EXTEND] = state->meta.string;
            break;
        case SOLU_DFUN: {
            solu_fproto *fp = val.dyn, *nfp = nv.dyn;
            memcpy(nfp, fp, sizeof(solu_fproto));
            nfp->file_name = sf_str_dup(fp->file_name);
            nfp->constants = solu_valvec_new();
            nfp->code = malloc(sizeof(solu_instruction) * fp->code_c);
            nfp->dbg = fp->dbg ? malloc(sizeof(solu_dbg) * fp->code_c) : NULL;
            nfp->self = fp->self;
            ac->metadata[SOLU_META_EXTEND] = state->meta.base;

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
                        solu_val cv = solu_valvec_get(&state->stack, state->frames.data[state->frames.count - 1].bottom_o + upv.ref);
                        if (cv.tt != SOLU_TDYN) {
                            nv = solu_dnew(state, SOLU_DREF);
                            solu_rawset(state, upv.ref, nv, state->frames.count - 1);
                            *(solu_val *)nv.dyn = cv;
                        } else nv = cv;
                    } else if (upv.tt == SOLU_UP_UPV) {
                        nv = state->ccall->upvals[upv.ref].value;
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
            if (fp->dbg)
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
        solu_dalloc *dh = solu_dheader(val);
        bool cache = false;

        if (dh->tt == SOLU_DSTR) {
            if (dh->size <= SOLU_STRCACHE_MAX - 1) {
                cache = true;
                solu_strcache_ex sex = solu_strcache_get(&state->strcache, sf_ref(val.dyn));
                if (sex.is_ok)
                    return (solu_val){ .tt = SOLU_TDYN, .dyn = sex.ok + 1 };
            }
        }

        val = solu_dscopy(state, val, false);
        if (cache) {
            dh = solu_dheader(val);
            solu_strcache_set(&state->strcache, sf_str_cdup(val.dyn), dh);
        }
        solu_dpush(state, solu_dheader(val));
    }
    return val;
}

void solu_dmarkfun(solu_fproto *fp) {
    for (solu_upvalue *v = fp->upvals; v && v < fp->upvals + fp->up_c; ++v) {
        if (v->tt == SOLU_UP_VAL && v->value.tt == SOLU_TDYN)
            solu_dmark(v->value);
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
    solu_valmap_foreach(&dobj->map, solu_dmarkmember, NULL);
}
void solu_dmarkref(solu_val r) {
    solu_val inner = solu_dval(r);
    while (inner.tt == SOLU_TDYN) {
        solu_dalloc *dc = solu_dheader(inner);
        if (dc->mark == SOLU_DYN_BLACK) return;
        switch (solu_dtypeof(inner)) {
            case SOLU_DREF:
                inner = solu_dval(inner);
                break;
            default: solu_dmark(inner);
        }
        dc->mark = SOLU_DYN_BLACK;
    }
}
void solu_dmark(solu_val val) {
    if (val.tt != SOLU_TDYN) return;
    solu_dalloc *ac = solu_dheader(val);
    if (ac->mark == SOLU_DYN_BLACK) return;
    ac->mark = SOLU_DYN_BLACK;

    for (int i = 0; i < SOLU_META_COUNT; ++i)
        if (ac->metadata[i].tt == SOLU_TDYN)
            solu_dmark(ac->metadata[i]);
    if (ac->meta.tt == SOLU_TDYN)
        solu_dmark(ac->meta);

    switch (ac->tt) {
        case SOLU_DUSR: {
            solu_usrwrap *uh = solu_uheader(val);
            if (uh->mark) uh->mark(val.dyn);
            break;
        }
        case SOLU_DOBJ: solu_dmarkobj(val); break;
        case SOLU_DFUN: solu_dmarkfun((solu_fproto *)((char *)ac + sizeof(solu_dalloc))); break;
        case SOLU_DREF: solu_dmarkref(val); break;
        default: break;
    }
}

void solu_dcollect(solu_state *s) {
    s->lb = 0;
    for (solu_val *r = s->stack.data; r < s->stack.data + s->stack.count; ++r)
        solu_dmark(*r);
    solu_dmarkobj(s->global);

    // Mark Greens
    for (solu_dalloc *a = s->alloc; a; a = a->next) {
        if (a->held)
            solu_dmark((solu_val){ SOLU_TDYN, .dyn = (a + 1) });
    }

    solu_dalloc **ac = &s->alloc;
    solu_dalloc *last = NULL;
    while (*ac) {
        if ((*ac)->mark == SOLU_DYN_WHITE && !(*ac)->held) {
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
        if (bps && proto->dbg && SOLU_DBG_LINE(proto->dbg[pc]) > proto->dbg_ll) { /* Debugger */\
            proto->dbg_ll = SOLU_DBG_LINE(proto->dbg[pc]); \
            if (bps[proto->dbg_ll - 1]) { /* If breakpoints, check if we should break */\
                proto->dbg_res = pc; \
                ++bpc; while (!*bpc) ++bpc; /* Advance to next BP */\
                return solu_call_ex_err((solu_call_err){SOLU_ERRV_BREAK, NULL, pc}); \
            } \
        } \
        if (proto->dbg) \
            proto->dbg_ll = SOLU_DBG_LINE(proto->dbg[pc]); \
        ++pc; \
        \
        if (s->collect) { \
            solu_dcollect(s); \
            s->collect = false; \
        }
#   define DISPATCH() do { PREDISPATCH(); goto *computed[solu_ins_op(ins)]; } while(0) /* jump up jump up and get down */

#   pragma GCC diagnostic push
#   pragma GCC diagnostic ignored "-Wpedantic"
#else
#   define DISPATCH() continue;
#   define CASE(name) case EXPAND(name):
#endif

#define solu_callerr(en, fmt, ...) (solu_call_ex_err((solu_call_err){.tt=(en),.panic=sf_str_fmt((fmt), __VA_ARGS__).c_str, .pc=pc-1}))

solu_val solu_wrapcfun(solu_state *state, solu_cfunction fptr, uint32_t arg_c, solu_val *captures, uint32_t cap_c) {
    solu_val fun = solu_dnew(state, SOLU_DFUN);
    *(solu_fproto *)fun.dyn = solu_fproto_c(fptr, arg_c, captures, cap_c);
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
    sf_buffer_autoins(&buf, &(uint8_t){proto->self});
    sf_buffer_autoins(&buf, &(uint8_t){proto->variadic});

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
                sf_buffer_autoins(&buf, &(uint64_t){htonll((uint64_t)k->i64)});
                break;
            case SOLU_TBOOL:
                sf_buffer_autoins(&buf, &k->boolean);
                break;
            case SOLU_TDYN: { // str
                size_t s = solu_dheader(*k)->size;
                if (s > 0) s = s - 1;
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
    if (upc > 0 && sf_str_eq(proto->upvals[0].name, sf_lit("global")))
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

    return buf;
}

void solu_savefun(solu_fproto *proto, char *path) {
    if (proto->tt != SOLU_FPROTO_BC)
        return;
    sf_buffer buf = solu_fproto_serialize(proto);
    FILE *f = fopen(path, "wb");
    if (!f) {
        sf_buffer_clear(&buf);
        return;
    }
    fwrite(buf.ptr, 1, buf.size, f);
    sf_buffer_clear(&buf);
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
    ex = sf_buffer_autoread(buf, &proto.self);
    if (!ex.is_ok) goto corrupt;
    ex = sf_buffer_autoread(buf, &proto.variadic);
    if (!ex.is_ok) goto corrupt;

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
                if (slen) temp[slen] = 0;

                solu_dyn p = calloc(1, sizeof(solu_dalloc) + slen + 1);
                solu_dalloc *dh = p;
                *dh = (solu_dalloc){
                    .next = NULL,
                    .size = slen + 1,
                    .thread = 1,
                    .tt = SOLU_DSTR,
                    .mark = SOLU_DYN_WHITE,
                    .held = true,
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
                val.i64 = (int64_t)ntohll((uint64_t)val.i64);
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
                    .mark = SOLU_DYN_WHITE,
                    .held = true,
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
    upvals[0] = (solu_upvalue){sf_lit("global"), SOLU_UP_VAL, .value = s->global, .mut = false};
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
    proto.dbg = NULL; // No debug info for compiled

    for (uint16_t i = 0; i < proto.code_c; ++i) {
        ex = sf_buffer_autoread(buf, proto.code + i);
        proto.code[i] = ntohl(proto.code[i]);
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
    solu_load_ex ex = _solu_loadfun(state, &fsb.ok);
    sf_buffer_clear(&fsb.ok);
    return ex;
}

static inline bool solu_truthy(solu_val val) {
    switch (val.tt) {
        case SOLU_TBOOL: return val.boolean;
        case SOLU_TI64: return val.i64 != 0;
        case SOLU_TF64: return !isnan(val.f64);
        case SOLU_TDYN: return val.dyn;
        default: return false;
    }
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
    #ifdef COMPUTE_GOTOS
    void *computed[] = {
        LABEL(SOLU_OP_LOAD),
        LABEL(SOLU_OP_MOVE),
        LABEL(SOLU_OP_RET),
        LABEL(SOLU_OP_JMP),
        LABEL(SOLU_OP_CALL),
        LABEL(SOLU_OP_MCALL),

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
        if (proto->variadic) {
            solu_val extra = solu_dnew(s, SOLU_DOBJ);
            solu_set(s, proto->arg_c, extra);
            for (uint32_t i = proto->arg_c; i < arg_c; ++i)
                solu_valvec_push(&((solu_dobj *)extra.dyn)->array, args[i]);
        }
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
            uint32_t var_r = UINT32_MAX;
            uint32_t fun_r = solu_iabc_bx(ins);
            solu_val var = SOLU_NIL;
            if (solu_iabc_bk(ins)) {
                var_r = fun_r;
                fun_r += 1;
                var = solu_get(s, var_r);
                if (!solu_isdtype(var, SOLU_DOBJ))
                    return solu_callerr(SOLU_ERRV_TYPE_MISMATCH, "Unfold operator expected obj, found %s", solu_typename(var).c_str);
            }

            solu_val of, fun = solu_get(s, fun_r);
            bool can_self = false;
            if (fun.tt == SOLU_TDYN) {
                solu_val call = solu_dheader(fun)->metadata[SOLU_META_CALL];
                if (call.tt != SOLU_TNIL) {
                    of = fun;
                    fun = call;
                    can_self = ((solu_fproto *)fun.dyn)->self;
                }
            }
            if (!solu_isdtype(fun, SOLU_DFUN)) {
                if (solu_isdtype(fun, SOLU_DERR))
                    return solu_callerr(SOLU_ERRV_TYPE_MISMATCH, "Attempted to call type %s: %s", solu_typename(fun).c_str, fun.dyn);
                return solu_callerr(SOLU_ERRV_TYPE_MISMATCH, "Attempted to call type %s", solu_typename(fun).c_str);
            }

            solu_fproto *f = fun.dyn;
            int i = -1;
            solu_val ov = SOLU_NIL;
            if (f->self)
                i = f->tt == SOLU_FPROTO_C ? 0 : 1;
            if (i >= 0) {
                if (can_self) {
                    ov = f->upvals[i].value;
                    f->upvals[i].value = of;
                } else f->upvals[i].value = SOLU_NIL;
            }

            uint32_t argc = solu_iabc_cx(ins);

            solu_call_ex fex;
            uint32_t extra = var_r != UINT32_MAX ? ((solu_dobj *)var.dyn)->array.count : 0;
            uint32_t passed = argc + extra;
            if (passed > 0) {
                solu_val local_argv[8];
                solu_val *argv = passed <= 8 ? local_argv : malloc(sizeof(solu_val) * passed);
                for (uint32_t i = 0; i < argc; ++i)
                    argv[i] = solu_get(s, fun_r + 1 + i);
                for (uint32_t i = argc; i < passed; ++i)
                    argv[i] = ((solu_dobj *)var.dyn)->array.data[i - argc];

                fex = solu_call(s, f, argv, passed);
                if (argv != local_argv)
                    free(argv);
            } else fex = solu_call(s, f, NULL, 0);

            if (i >= 0)
                f->upvals[i].value = ov;

            if (!fex.is_ok) {
                if (f->tt == SOLU_FPROTO_C)
                    fex.err.pc = pc - 1;
                s->ecall = f;
                return fex;
            }
            solu_set(s, solu_iabc_a(ins), fex.ok);
            DISPATCH();
        }
        CASE(SOLU_OP_MCALL) {
            uint32_t var_r = UINT32_MAX;
            uint32_t obj_r = solu_iabc_bx(ins), arg_r = obj_r + 2;
            solu_val var = SOLU_NIL;


            solu_val obj = solu_get(s, obj_r);
            solu_val key = solu_get(s, obj_r + 1);
            if (solu_iabc_bk(ins)) {
                var_r = obj_r + 1;
                var = key;
                arg_r = var_r + 2;
                key = solu_get(s, var_r + 1);
                if (!solu_isdtype(var, SOLU_DOBJ))
                    return solu_callerr(SOLU_ERRV_TYPE_MISMATCH, "Unfold operator expected obj, found %s", solu_typename(var).c_str);
            }

            solu_val get = SOLU_NIL;
            solu_val fun = SOLU_NIL;
            solu_dtype dt = solu_dtypeof(obj);
            solu_dalloc *da = solu_dheader(obj);
            bool extend = false;
            bool prim = dt == SOLU_DCOUNT;
            if (!prim) {
                extend = da->metadata[SOLU_META_EXTEND].tt != SOLU_TNIL;
                get = da->metadata[SOLU_META_GET];
                if (get.tt != SOLU_TNIL) {
                    solu_call_ex ex = solu_call(s, get.dyn, (solu_val[]){key}, 1);
                    if (!ex.is_ok) return ex;
                    fun = ex.ok;
                }
                solu_dalloc *fh = solu_dheader(fun);
                if (dt == SOLU_DOBJ && !(fh && (fh->tt == SOLU_DFUN || fh->metadata[SOLU_META_CALL].tt != SOLU_TNIL))) {
                    fun = solu_dobj_get(s, obj.dyn, key);
                    fh = solu_dheader(fun);
                }
                if (extend && !(fh && (fh->tt == SOLU_DFUN || fh->metadata[SOLU_META_CALL].tt != SOLU_TNIL))) {
                    fun = solu_dobj_get(s, da->metadata[SOLU_META_EXTEND].dyn, key);
                    fh = solu_dheader(fun);
                }
                if (fun.tt == SOLU_TDYN && fh->metadata[SOLU_META_CALL].tt != SOLU_TNIL)
                    fun = fh->metadata[SOLU_META_CALL];
            } else
                fun = solu_dobj_get(s, s->meta.prim.dyn, key);

            if (!solu_isdtype(fun, SOLU_DFUN)) {
                if (solu_isdtype(fun, SOLU_DERR))
                    return solu_callerr(SOLU_ERRV_TYPE_MISMATCH, "Attempted to call type %s: %s", solu_typename(fun).c_str, fun.dyn);
                if (solu_isdtype(key, SOLU_DSTR))
                    return solu_callerr(SOLU_ERRV_PANIC, "Member function '%s' not found", key.dyn);
                return solu_callerr(SOLU_ERRV_PANIC, "Member function not found", NULL);
            }

            solu_fproto *f = fun.dyn;
            int i = -1;
            solu_val ov = SOLU_NIL;
            if (f->self)
                i = f->tt == SOLU_FPROTO_C ? 0 : 1;
            if (i >= 0) {
                ov = f->upvals[i].value;
                f->upvals[i].value = obj;
            }

            solu_call_ex fex;
            uint32_t argc = solu_iabc_cx(ins) + prim;
            uint32_t extra = var_r != UINT32_MAX ? ((solu_dobj *)var.dyn)->array.count : 0;
            uint32_t passed = argc + extra;

            if (passed > 0) {
                solu_val local_argv[8];
                solu_val *argv = passed <= 8 ? local_argv : malloc(sizeof(solu_val) * passed);
                if (prim) argv[0] = obj;

                for (uint32_t j = prim; j < argc; ++j)
                    argv[j] = solu_get(s, arg_r + (j - prim));
                for (uint32_t j = argc; j < passed; ++j)
                    argv[j] = ((solu_dobj *)var.dyn)->array.data[j - argc];

                fex = solu_call(s, f, argv, passed);
                if (argv != local_argv) free(argv);
            } else fex = solu_call(s, f, NULL, 0);

            if (i >= 0)
                f->upvals[i].value = ov;

            if (!fex.is_ok) {
                s->ecall = f;
                return fex;
            }
            solu_set(s, solu_iabc_a(ins), fex.ok);
            DISPATCH();
        }

        CASE(SOLU_OP_ADD) {
            bool pleq = solu_iabc_bx(ins) == solu_iabc_a(ins);
            solu_val lhs = solu_iabc_bk(ins) ? solu_getk(s, proto, solu_iabc_bx(ins)) : solu_get(s, solu_iabc_bx(ins));
            solu_val rhs = solu_iabc_ck(ins) ? solu_getk(s, proto, solu_iabc_cx(ins)) : solu_get(s, solu_iabc_cx(ins));

            if (lhs.tt == SOLU_TDYN) {
                solu_dalloc *da = solu_dheader(lhs);
                if (da->metadata[SOLU_META_ADD].tt != SOLU_TNIL) {
                    solu_call_ex ex = solu_call(s, da->metadata[SOLU_META_ADD].dyn,
                        (solu_val[]){lhs, rhs},
                    2);
                    if (!ex.is_ok) return ex;
                    if (!solu_isdtype(ex.ok, SOLU_DERR))
                        solu_usemeta(ex.ok, da->meta.dyn);
                    solu_set(s, solu_iabc_a(ins), ex.ok);
                    DISPATCH();
                }
                goto skip_add;
            }
            if (rhs.tt == SOLU_TDYN) {
                skip_add:
                if (solu_isdtype(lhs, SOLU_DERR))
                    return solu_callerr(SOLU_ERRV_PANIC, "%s", lhs.dyn);
                if (solu_isdtype(rhs, SOLU_DERR))
                    return solu_callerr(SOLU_ERRV_PANIC, "%s", rhs.dyn);
                if (lhs.tt != rhs.tt) {
                    if (solu_isdtype(lhs, SOLU_DOBJ)) {
                        solu_valvec_push(&((solu_dobj *)lhs.dyn)->array, rhs);
                        DISPATCH();
                    }
                    if (solu_isdtype(lhs, SOLU_DSTR) && !solu_isdtype(rhs, SOLU_DSTR)) {
                        rhs = (solu_val){SOLU_TCOUNT, .dyn = solu_tostr(s, rhs)};
                        goto strcat;
                    } else if (solu_isdtype(rhs, SOLU_DSTR) && !solu_isdtype(lhs, SOLU_DSTR)) {
                        lhs = (solu_val){SOLU_TCOUNT, .dyn = solu_tostr(s, lhs)};
                        goto strcat;
                    } else return solu_callerr(SOLU_ERRV_TYPE_MISMATCH, "Implicit conversion %s into %s", solu_typename(rhs).c_str, solu_typename(lhs).c_str);
                }
            } else if (lhs.tt != rhs.tt) {
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
                    solu_dalloc *dh = solu_dheader(lhs);
                    switch (dh->tt) {
                        case SOLU_DSTR:
                        strcat: {
                            size_t lsize = lhs.tt == SOLU_TCOUNT ? strlen(lhs.dyn) : solu_dheader(lhs)->size - 1;
                            size_t rsize = rhs.tt == SOLU_TCOUNT ? strlen(rhs.dyn) : solu_dheader(rhs)->size - 1;
                            solu_dalloc *ac = malloc(sizeof(solu_dalloc) + lsize + rsize + 1);
                            *ac = (solu_dalloc) {
                                NULL, lsize+rsize+1,
                                1, SOLU_DSTR,
                                SOLU_DYN_WHITE,
                                false,
                                SOLU_NIL,
                                {[SOLU_META_EXTEND] = s->meta.string}
                            };
                            memcpy(ac + 1, lhs.dyn, lsize);
                            memcpy(((char *)(ac + 1)) + lsize, rhs.dyn, rsize);
                            ((char *)(ac+1))[lsize+rsize] = 0;
                            solu_dpush(s, ac);
                            if (rhs.tt == SOLU_TCOUNT) free(rhs.dyn);
                            if (lhs.tt == SOLU_TCOUNT) free(lhs.dyn);
                            solu_set(s, solu_iabc_a(ins), (solu_val){SOLU_TDYN, .dyn=ac+1});
                            break;
                        }
                        case SOLU_DOBJ: {
                            if (pleq) solu_dappend(lhs, rhs);
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

            if (lhs.tt == SOLU_TDYN) {
                solu_dalloc *da = solu_dheader(lhs);
                if (da->metadata[SOLU_META_SUB].tt != SOLU_TNIL) {
                    solu_call_ex ex = solu_call(s, da->metadata[SOLU_META_SUB].dyn,
                        (solu_val[]){lhs, rhs},
                    2);
                    if (!ex.is_ok) return ex;
                    if (!solu_isdtype(ex.ok, SOLU_DERR))
                        solu_usemeta(ex.ok, da->meta.dyn);
                    solu_set(s, solu_iabc_a(ins), ex.ok);
                    DISPATCH();
                }
                goto skip_sub;
            }
            if (rhs.tt == SOLU_TDYN) {
                skip_sub:
                if (solu_isdtype(lhs, SOLU_DERR))
                    return solu_callerr(SOLU_ERRV_PANIC, "%s", lhs.dyn);
                if (solu_isdtype(rhs, SOLU_DERR))
                    return solu_callerr(SOLU_ERRV_PANIC, "%s", rhs.dyn);
                if (solu_isdtype(lhs, SOLU_DOBJ) && solu_isdtype(rhs, SOLU_DSTR) &&
                    solu_iabc_a(ins) == solu_iabc_bx(ins)) {
                    solu_valmap_delete(&((solu_dobj *)lhs.dyn)->map, sf_ref(rhs.dyn));
                    DISPATCH();
                }
            }

            if (lhs.tt != SOLU_TI64 && lhs.tt != SOLU_TF64)
                return solu_callerr(SOLU_ERRV_TYPE_MISMATCH, "Type %s does not support operator '-'", solu_typename(lhs).c_str);
            if (rhs.tt != SOLU_TI64 && rhs.tt != SOLU_TF64)
                return solu_callerr(SOLU_ERRV_TYPE_MISMATCH, "Type %s does not support operator '-'", solu_typename(rhs).c_str);
            if (lhs.tt != rhs.tt)
                rhs = lhs.tt == SOLU_TI64 ?
                    (solu_val){.tt = SOLU_TI64, .i64 = (solu_i64)rhs.f64} :
                    (solu_val){.tt = SOLU_TF64, .f64 = (solu_f64)rhs.i64};
            solu_set(s, solu_iabc_a(ins), lhs.tt == SOLU_TI64 ?
                (solu_val){.tt = SOLU_TI64, .i64 = lhs.i64 - rhs.i64} :
                (solu_val){.tt = SOLU_TF64, .f64 = lhs.f64 - rhs.f64}
            );
            DISPATCH();
        }
        CASE(SOLU_OP_MUL) {
            solu_val lhs = solu_iabc_bk(ins) ? solu_getk(s, proto, solu_iabc_bx(ins)) : solu_get(s, solu_iabc_bx(ins));
            solu_val rhs = solu_iabc_ck(ins) ? solu_getk(s, proto, solu_iabc_cx(ins)) : solu_get(s, solu_iabc_cx(ins));
            if (lhs.tt == SOLU_TDYN) {
                solu_dalloc *da = solu_dheader(lhs);
                if (da->metadata[SOLU_META_MUL].tt != SOLU_TNIL) {
                    solu_call_ex ex = solu_call(s, da->metadata[SOLU_META_MUL].dyn,
                        (solu_val[]){lhs, rhs},
                    2);
                    if (!ex.is_ok) return ex;
                    if (!solu_isdtype(ex.ok, SOLU_DERR))
                        solu_usemeta(ex.ok, da->meta.dyn);
                    solu_set(s, solu_iabc_a(ins), ex.ok);
                    DISPATCH();
                }
            }
            if (rhs.tt == SOLU_TDYN) {
                if (solu_isdtype(lhs, SOLU_DERR))
                    return solu_callerr(SOLU_ERRV_PANIC, "%s", lhs.dyn);
                if (solu_isdtype(rhs, SOLU_DERR))
                    return solu_callerr(SOLU_ERRV_PANIC, "%s", rhs.dyn);
                return solu_callerr(SOLU_ERRV_TYPE_MISMATCH, "Type %s does not support operator '*'", solu_typename(rhs).c_str);
            }

            if (lhs.tt != SOLU_TI64 && lhs.tt != SOLU_TF64)
                return solu_callerr(SOLU_ERRV_TYPE_MISMATCH, "Type %s does not support operator '*'", solu_typename(lhs).c_str);
            if (rhs.tt != SOLU_TI64 && rhs.tt != SOLU_TF64)
                return solu_callerr(SOLU_ERRV_TYPE_MISMATCH, "Type %s does not support operator '*'", solu_typename(rhs).c_str);
            if (lhs.tt != rhs.tt)
                rhs = lhs.tt == SOLU_TI64 ?
                    (solu_val){.tt = SOLU_TI64, .i64 = (solu_i64)rhs.f64} :
                    (solu_val){.tt = SOLU_TF64, .f64 = (solu_f64)rhs.i64};
            solu_set(s, solu_iabc_a(ins), lhs.tt == SOLU_TI64 ?
                (solu_val){.tt = SOLU_TI64, .i64 = lhs.i64 * rhs.i64} :
                (solu_val){.tt = SOLU_TF64, .f64 = lhs.f64 * rhs.f64}
            );
            DISPATCH();
        }
        CASE(SOLU_OP_DIV) {
            solu_val lhs = solu_iabc_bk(ins) ? solu_getk(s, proto, solu_iabc_bx(ins)) : solu_get(s, solu_iabc_bx(ins));
            solu_val rhs = solu_iabc_ck(ins) ? solu_getk(s, proto, solu_iabc_cx(ins)) : solu_get(s, solu_iabc_cx(ins));
            if (lhs.tt == SOLU_TDYN) {
                solu_dalloc *da = solu_dheader(lhs);
                if (da->metadata[SOLU_META_DIV].tt != SOLU_TNIL) {
                    solu_call_ex ex = solu_call(s, da->metadata[SOLU_META_DIV].dyn,
                        (solu_val[]){lhs, rhs},
                    2);
                    if (!ex.is_ok) return ex;
                    if (!solu_isdtype(ex.ok, SOLU_DERR))
                        solu_usemeta(ex.ok, da->meta.dyn);
                    solu_set(s, solu_iabc_a(ins), ex.ok);
                    DISPATCH();
                }
            }
            if (rhs.tt == SOLU_TDYN) {
                if (solu_isdtype(lhs, SOLU_DERR))
                    return solu_callerr(SOLU_ERRV_PANIC, "%s", lhs.dyn);
                if (solu_isdtype(rhs, SOLU_DERR))
                    return solu_callerr(SOLU_ERRV_PANIC, "%s", rhs.dyn);
                return solu_callerr(SOLU_ERRV_TYPE_MISMATCH, "Type %s does not support operator '/'", solu_typename(lhs).c_str);
            }

            if (lhs.tt != SOLU_TI64 && lhs.tt != SOLU_TF64)
                return solu_callerr(SOLU_ERRV_TYPE_MISMATCH, "Type %s does not support operator '/'", solu_typename(lhs).c_str);
            if (rhs.tt != SOLU_TI64 && rhs.tt != SOLU_TF64)
                return solu_callerr(SOLU_ERRV_TYPE_MISMATCH, "Type %s does not support operator '/'", solu_typename(rhs).c_str);
            if (lhs.tt != rhs.tt)
                rhs = lhs.tt == SOLU_TI64 ?
                    (solu_val){.tt = SOLU_TI64, .i64 = (solu_i64)rhs.f64} :
                    (solu_val){.tt = SOLU_TF64, .f64 = (solu_f64)rhs.i64};
            solu_set(s, solu_iabc_a(ins), lhs.tt == SOLU_TI64 ?
                (solu_val){.tt = SOLU_TI64, .i64 = lhs.i64 / rhs.i64} :
                (solu_val){.tt = SOLU_TF64, .f64 = lhs.f64 / rhs.f64}
            );
            DISPATCH();
        }

        CASE(SOLU_OP_NEG) {
            solu_val in = solu_get(s, solu_iab_b(ins));
            switch (in.tt) {
                case SOLU_TI64: in.i64 = -in.i64; break;
                case SOLU_TF64: in.f64 = -in.f64; break;
                case SOLU_TBOOL: in.boolean = !in.boolean; break;
                case SOLU_TDYN: {
                    solu_dalloc *da = solu_dheader(in);
                    if (da->metadata[SOLU_META_NEG].tt != SOLU_TNIL) {
                        solu_call_ex ex = solu_call(s, da->metadata[SOLU_META_NEG].dyn, &in, 1);
                        if (!ex.is_ok) return ex;
                        if (!solu_isdtype(ex.ok, SOLU_DERR))
                        solu_usemeta(ex.ok, da->meta.dyn);
                        solu_set(s, solu_iabc_a(ins), ex.ok);
                        DISPATCH();
                    }
                }
                default: return solu_callerr(SOLU_ERRV_TYPE_MISMATCH, "Type %s does not support prefix operator '-/!'", solu_typename(in).c_str);
            }
            solu_set(s, solu_iab_a(ins), in);
            DISPATCH();
        }
        CASE(SOLU_OP_EQ) {
            bool inv = solu_iabc_a(ins) != 0;
            solu_val lhs = solu_iabc_bk(ins) ? solu_getk(s, proto, solu_iabc_bx(ins)) : solu_get(s, solu_iabc_bx(ins));
            solu_val rhs = solu_iabc_ck(ins) ? solu_getk(s, proto, solu_iabc_cx(ins)) : solu_get(s, solu_iabc_cx(ins));
            if (lhs.tt == SOLU_TDYN) {
                solu_dalloc *da = solu_dheader(lhs);
                if (da->metadata[SOLU_META_EQ].tt != SOLU_TNIL) {
                    solu_call_ex ex = solu_call(s, da->metadata[SOLU_META_EQ].dyn,
                        (solu_val[]){lhs, rhs},
                    2);
                    if (!ex.is_ok) return ex;
                    bool e = solu_truthy(ex.ok);
                    if (inv ? !e : e) pc++;
                    DISPATCH();
                }
            }

            if ((lhs.tt == SOLU_TNIL && rhs.tt == SOLU_TNIL)) {
                if (!inv) pc++;
                DISPATCH();
            }
            if (lhs.tt == SOLU_TBOOL && rhs.tt != SOLU_TNIL && rhs.tt != SOLU_TBOOL) {
                if (inv ? !lhs.boolean : lhs.boolean) pc++;
                DISPATCH();
            }
            if (lhs.tt != SOLU_TNIL && lhs.tt != SOLU_TBOOL && rhs.tt == SOLU_TBOOL) {
                if (inv ? !rhs.boolean : rhs.boolean) pc++;
                DISPATCH();
            }

            if (lhs.tt != rhs.tt) {
                if (lhs.tt == SOLU_TDYN || rhs.tt == SOLU_TDYN || lhs.tt == SOLU_TNIL || rhs.tt == SOLU_TNIL) {
                    if (inv) pc++;
                    DISPATCH();
                }
                switch (lhs.tt) {
                    case SOLU_TI64: rhs = (solu_val){.tt = SOLU_TI64, .i64 = rhs.tt == SOLU_TBOOL ? (rhs.boolean ? 1 : 0) : (solu_i64)rhs.f64}; break;
                    case SOLU_TF64: rhs = (solu_val){.tt = SOLU_TF64, .f64 = rhs.tt == SOLU_TBOOL ? (rhs.boolean ? 1 : 0) : (solu_f64)rhs.i64}; break;
                    case SOLU_TBOOL: rhs = (solu_val){.tt = SOLU_TBOOL, .boolean = rhs.tt == SOLU_TI64 ? rhs.i64 != 0 : rhs.f64 != 0}; break;
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
            if (!proto->upvals)
                return solu_panic("Corrupt Bytecode");
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
            if (!proto->upvals)
                return solu_panic("Corrupt Bytecode");
            solu_upvalue *upv = proto->upvals + solu_iab_b(ins);
            if (upv->tt == SOLU_UP_VAL) {
                solu_set(s, solu_iab_a(ins), solu_isdtype(upv->value, SOLU_DREF) ?
                    *(solu_val *)upv->value.dyn : upv->value);
            } else solu_set(s, solu_iab_a(ins), solu_rawget(s, upv->ref, upv->frame));
            DISPATCH();
        }
        CASE(SOLU_OP_REFU) {
            solu_val v = solu_valvec_get(&s->stack, s->frames.data[s->frames.count - 1].bottom_o + (uint32_t)solu_ia_a(ins));
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
            if (obj.tt != SOLU_TDYN)
                return solu_callerr(SOLU_ERRV_TYPE_MISMATCH, "Attempted to index type %s", solu_typename(obj).c_str);
            solu_val key = solu_iabc_bk(ins) ? solu_getk(s, proto, solu_iabc_bx(ins)) : solu_get(s, solu_iabc_bx(ins));
            solu_val val = solu_iabc_ck(ins) ? solu_getk(s, proto, solu_iabc_cx(ins)) : solu_get(s, solu_iabc_cx(ins));

            solu_dalloc *da = solu_dheader(obj);
            solu_val set = da->metadata[SOLU_META_SET];
            if (da->tt != SOLU_DOBJ && set.tt == SOLU_TNIL) {
                solu_val ext = da->metadata[SOLU_META_EXTEND];
                if (ext.tt != SOLU_TNIL) {
                    obj = ext;
                    da = solu_dheader(obj);
                    if (set.tt == SOLU_TNIL)
                        set = da->metadata[SOLU_META_SET];
                }
            }
            if (solu_isdtype(set, SOLU_DFUN)) {
                solu_call_ex ex = solu_call(s, set.dyn, (solu_val[]){obj, key, val}, 3);
                if (!ex.is_ok) return ex;
                DISPATCH();
            } else if (da->tt != SOLU_DOBJ)
                return solu_callerr(SOLU_ERRV_TYPE_MISMATCH, "Attempted to index type %s", solu_typename(obj).c_str);

            solu_dobj_set(s, obj.dyn, key, val);
            DISPATCH();
        }
        CASE(SOLU_OP_GET) {
            solu_val obj = solu_get(s, solu_iabc_bx(ins));
            if (obj.tt != SOLU_TDYN)
                return solu_callerr(SOLU_ERRV_TYPE_MISMATCH, "Attempted to index type %s", solu_typename(obj).c_str);
            solu_val key = solu_iabc_ck(ins) ? solu_getk(s, proto, solu_iabc_cx(ins)) : solu_get(s, solu_iabc_cx(ins));

            solu_val get = SOLU_NIL;
            solu_dalloc *da = solu_dheader(obj);
            if (da->tt == SOLU_DSTR) {
                solu_dalloc *da = solu_dheader(obj);
                if (key.tt != SOLU_TI64)
                    return solu_callerr(SOLU_ERRV_TYPE_MISMATCH, "Attempted to index string with type %s", solu_typename(key).c_str);
                if (key.i64 < 0 || key.i64 > (solu_i64)da->size - 2)
                    return solu_callerr(SOLU_ERRV_TYPE_MISMATCH, "Index %lld out of bounds (string length %llu)", key.i64, da->size - 1);
                char str[2] = { ((char *)obj.dyn)[key.i64], '\0' };
                solu_set(s, solu_iabc_a(ins), solu_dnstr(s, str));
                DISPATCH();
            } else {
                get = da->metadata[SOLU_META_GET];
                if (da->tt != SOLU_DOBJ) {
                    if (get.tt != SOLU_TNIL) {
                        solu_call_ex ex = solu_call(s, get.dyn, (solu_val[]){obj, key}, 2);
                        if (!ex.is_ok) return ex;
                        solu_set(s, solu_iabc_a(ins), ex.ok);
                        DISPATCH();
                    } else {
                        solu_val ext = da->metadata[SOLU_META_EXTEND];
                        if (ext.tt != SOLU_TNIL) {
                            obj = ext;
                            da = solu_dheader(obj);
                            if (get.tt == SOLU_TNIL)
                                get = da->metadata[SOLU_META_GET];
                        } else
                            return solu_callerr(SOLU_ERRV_TYPE_MISMATCH, "Attempted to index type %s", solu_typename(solu_get(s, solu_iabc_bx(ins))).c_str);
                    }
                }
            }
            if (get.tt != SOLU_TNIL) {
                solu_call_ex ex = solu_call(s, get.dyn, (solu_val[]){key}, 1);
                if (!ex.is_ok) return ex;
                solu_set(s, solu_iabc_a(ins), ex.ok);
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
            solu_val kkey = solu_valvec_get(&proto->constants, solu_iabc_bx(ins));
            solu_val val = solu_iabc_ck(ins) ? solu_getk(s, proto, solu_iabc_cx(ins)) : solu_get(s, solu_iabc_cx(ins));
            solu_upvalue *upv = proto->upvals + solu_iabc_a(ins);
            solu_val upo = upv->tt == SOLU_UP_VAL ? upv->value : solu_rawget(s, upv->ref, upv->frame);
            solu_dobj_strset(upo.dyn, kkey.dyn, val);
            DISPATCH();
        }
        CASE(SOLU_OP_GUPO) {
            solu_val kkey = solu_valvec_get(&proto->constants, solu_iabc_cx(ins));
            solu_upvalue *upv = proto->upvals + solu_iabc_bx(ins);
            solu_val upo = upv->tt == SOLU_UP_VAL ? upv->value : solu_rawget(s, upv->ref, upv->frame);
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
    return solu_call_ex_ok(return_val);
}

solu_call_ex solu_call(solu_state *state, solu_fproto *proto, const solu_val *args, uint32_t arg_c) {
    if (state->call_stack > CALL_STACK_MAX)
        return solu_panic("Stack Overflow");
    sf_str od = proto->file_name;
    if (proto->file_name.len)
        state->cwd = proto->file_name;

    state->ecall = NULL;
    ++state->call_stack;
    solu_fproto *ocall = state->ccall;
    state->ccall = proto;
    if (proto->tt == SOLU_FPROTO_BC) {
        solu_call_ex ex = solu_call_bc(state, proto, args, arg_c, NULL);
        if (!ex.is_ok) {
            if (!state->ecall)
                state->ecall = proto;
            solu_popframe(state);
        }
        --state->call_stack;
        state->ccall = ocall;
        state->cwd = od;
        return ex;
    }
    solu_call_ex ex = solu_call_cfun(state, proto, args, arg_c);
    if (!ex.is_ok && !state->ecall)
        state->ecall = proto;
    --state->call_stack;
    state->ccall = ocall;
    state->cwd = od;
    return ex;
}

solu_call_ex solu_dcall(solu_state *state, solu_fproto *proto, const solu_val *args, uint32_t arg_c, bool *bps) {
    if (state->call_stack > CALL_STACK_MAX)
        return solu_panic("Stack Overflow");

    ++state->call_stack;
    if (proto->tt == SOLU_FPROTO_BC) {
        solu_call_ex ex = solu_call_bc(state, proto, args, arg_c, bps);
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
