#include <stdio.h>
#include <stdlib.h>
#include <stdlib.h>
#include <string.h>
#include "solus/vm.h"
#include "sf/containers/buffer.h"
#include "sf/fs.h"
#include "solus/bytecode.h"
#include "solus/compiler.h"
#include "sf/str.h"
#include "std/std.h"

solu_state *solu_state_new(void) {
    solu_dyn p = calloc(1, sizeof(solu_dalloc) + sizeof(solu_dobj));
    *(solu_dalloc *)p = (solu_dalloc){NULL, sizeof(solu_dobj), SOLU_DOBJ, SOLU_DYN_GREEN};
    p = (char *)p + sizeof(solu_dalloc);
    *(solu_dobj *)p = solu_dobj_new();

    solu_state *s = malloc(sizeof(solu_state));
    *s = (solu_state){
        .stack = solu_valvec_new(),
        .files = solu_filenames_new(),
        .strcache = solu_strcache_new(),
        .global = {SOLU_TDYN, .dyn = p},
        .lb = 1<<20, .cb = 0,
    };
    solu_filenames_push(&s->files, sf_lit("./"));
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
    solu_dobj_set(solus.dyn, sf_lit("version"), solu_dnstr(s, SOLU_VERSION));
    solu_dobj_set(solus.dyn, sf_lit("git"), solu_dnstr(s, SOLU_GIT));
    solu_dobj_set(s->global.dyn, sf_lit("solus"), solus);

    solu_mod_builtin(s);
    solu_mod_io(s);
    solu_mod_string(s);
    solu_mod_obj(s);
    solu_mod_math(s);
    solu_mod_gc(s);
}

solu_compile_ex solu_csrc(solu_state *state, char *src) {
    solu_compile_ex ex = solu_cproto(src, 0, NULL, 1, (solu_upvalue[]){
        (solu_upvalue){sf_lit("_g"), SOLU_UP_VAL, .value = state->global}
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
    sf_buffer_autoins(&fsb.ok, ""); // [\0]

    solu_compile_ex ex = solu_csrc(state, (char *)fsb.ok.ptr);
    sf_buffer_clear(&fsb.ok);
    if (!ex.is_ok) return ex;
    ex.ok.file_name = sf_str_cdup(path);
    return ex;
}

void solu_dpush(solu_state *s, solu_dalloc *ac) {
    solu_dalloc *dd = s->alloc;
    if (dd == NULL) s->alloc = ac;
    else {
        while (dd->next) dd = dd->next;
        dd->next = ac;
    }
    s->cb += ac->size;
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

solu_val solu_dnewusr(solu_state *s, size_t size, const char *name, void *value, solu_usrdel del, solu_usrtostring tostring) {
    solu_dyn p = calloc(1, sizeof(solu_dalloc) + size);
    solu_dalloc *dh = p;
    *dh = (solu_dalloc){
        .next = NULL,
        .size = size + sizeof(solu_usrwrap),
        .tt = SOLU_DUSR,
        .mark = SOLU_DYN_WHITE,
    };
    p = (char *)p + sizeof(solu_dalloc);
    memcpy(p, value, size);
    *(solu_usrwrap *)((char *)p + size) = (solu_usrwrap){
        .name = sf_str_cdup(name),
        .del = del,
        .tostring = tostring,
    };

    solu_dpush(s, dh);
    return (solu_val){ .tt = SOLU_TDYN, .dyn = p };
}

solu_val solu_dnstr(solu_state *s, const char *str) {
    size_t size = strlen(str) + 1;
    if (size <= SOLU_STRCACHE_MAX - 1) {
        solu_strcache_ex sex = solu_strcache_get(&s->strcache, sf_ref(str));
        if (sex.is_ok)
            return (solu_val){ .tt = SOLU_TDYN, .dyn = sex.ok };
    }

    solu_dyn p = calloc(1, sizeof(solu_dalloc) + size);
    solu_dalloc *dh = p;
    *dh = (solu_dalloc){
        .next = NULL,
        .size = size,
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

char *solu_tostring(solu_val val) {
    switch (val.tt) {
        case SOLU_TNIL: return strdup("nil");
        case SOLU_TF64: return sf_str_fmt("%f", val.f64).c_str;
        case SOLU_TI64: return sf_str_fmt("%lld", val.i64).c_str;
        case SOLU_TBOOL: return strdup(val.boolean ? "true" : "false");
        case SOLU_TDYN: {
            switch (solu_dheader(val)->tt) {
                case SOLU_DSTR:
                case SOLU_DERR:
                return strdup(val.dyn); break;
                case SOLU_DOBJ:
                case SOLU_DFUN: return sf_str_fmt("%p", val.dyn).c_str;
                case SOLU_DREF: return solu_tostring(*(solu_val *)val.dyn);

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

solu_val solu_dscopy(solu_state *state, solu_val val, bool kconst) {
    if (val.tt != SOLU_TDYN)
        return val; // This function only needs to copy dynamic constants

    solu_dalloc *ac = malloc(sizeof(solu_dalloc) + solu_dheader(val)->size);
    *ac = *(solu_dheader(val));
    ac->size = solu_dheader(val)->size;
    ac->mark = SOLU_DYN_WHITE;
    ac->next = NULL;
    solu_val nv = (solu_val){SOLU_TDYN, .dyn=(char*)ac + sizeof(solu_dalloc)};

    switch (solu_dheader(nv)->tt) {
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
                        solu_val cv = solu_rawget(state, upv.ref, upv.frame);
                        if (cv.tt != SOLU_TDYN) {
                            nv = solu_dnew(state, SOLU_DREF);
                            solu_rawset(state, upv.ref, nv, upv.frame);
                            *(solu_val *)nv.dyn = cv;
                        } else nv = cv;
                    } else nv = upv.value;

                    nfp->upvals[i] = (solu_upvalue){
                        sf_str_dup(upv.name),
                        SOLU_UP_VAL,
                        .value = nv,
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

void solu_dmarkref(solu_val *r);
void solu_dcollect_obj(void *ud, sf_str _k, solu_val member) {
    (void)_k;
    if (member.tt != SOLU_TDYN || solu_dheader(member)->mark != SOLU_DYN_WHITE) return;
    solu_dheader(member)->mark = SOLU_DYN_BLACK;
    if (solu_dtypeof(member) == SOLU_DOBJ)
        solu_dobj_foreach(member.dyn, solu_dcollect_obj, ud);
    else if (solu_dtypeof(member) == SOLU_DFUN)
        solu_dmarkfun(member.dyn);
    else if (solu_dtypeof(member) == SOLU_DREF)
        solu_dmarkref(&member);
}

void solu_dmarkref(solu_val *r) {
    solu_val inner = solu_dval(*r);
    if (solu_dheader(inner)->mark == SOLU_DYN_BLACK)
        return;
    while (inner.tt == SOLU_TDYN) {
        solu_dheader(inner)->mark = SOLU_DYN_BLACK;
        switch (solu_dtypeof(inner)) {
            case SOLU_DOBJ:
                solu_dobj_foreach(inner.dyn, solu_dcollect_obj, NULL);
                break;
            case SOLU_DFUN:
                solu_dmarkfun(inner.dyn);
                break;
            case SOLU_DREF:
                inner = solu_dval(inner);
                break;
            default: break;
        }
    }
}

void solu_dcollect(solu_state *state) {
    state->lb = 0;
    for (solu_val *r = state->stack.data; r < state->stack.data + state->stack.count; ++r) {
        if (r->tt == SOLU_TDYN) {
            solu_dalloc *ac = solu_dheader(*r);
            ac->mark = SOLU_DYN_BLACK;

            if (ac->tt == SOLU_DOBJ)
                solu_dobj_foreach(r->dyn, solu_dcollect_obj, NULL);
            if (ac->tt == SOLU_DFUN)
                solu_dmarkfun((solu_fproto *)((char *)ac + sizeof(solu_dalloc)));
            if (ac->tt == SOLU_DREF)
                solu_dmarkref(r);
        }
    }
    solu_dobj_foreach(state->global.dyn, solu_dcollect_obj, NULL);

    solu_dalloc **ac = &state->alloc;
    while (*ac) {
        if ((*ac)->mark == SOLU_DYN_WHITE) {
            solu_dalloc *dead = *ac;
            *ac = dead->next;
            if (dead->tt == SOLU_DSTR)
                solu_strcache_delete(&state->strcache, sf_ref((char *)(dead + 1)));
            solu_dclean((solu_val){SOLU_TDYN, .dyn = dead + 1});
            continue;
        }
        state->lb += (*ac)->size;
        (*ac)->mark = SOLU_DYN_WHITE;
        ac = &(*ac)->next;
    }
}

#define CAT(a, b) a##b
#define EXPAND(a) a
#define EXPAND_CAT(a, b) CAT(a, b)

//#define SOLU_DBG_NOCOMPUTE

#if (defined(__GNUC__) || defined(__clang__)) && !defined(SOLU_DBG_NOCOMPUTE)
#   define LABEL(name) [name] = &&EXPAND_CAT(name, _L)
#   define CASE(name) EXPAND_CAT(name, _L):
#   define COMPUTE_GOTOS
#   define DISPATCH() do { \
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
        if (s->cb > (size_t)((double)s->lb * SOLU_GCSTEP)) /* Collect on GC threshold reached */\
            solu_dcollect(s); \
        goto *computed[solu_ins_op(ins)]; /* jump up jump up and get down */\
    } while (0)
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
        solu_filenames_push(&s->files, solu_dirname(proto->file_name));
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
        ins = proto->code[pc];
        if (bps && SOLU_DBG_LINE(proto->dbg[pc]) > proto->dbg_ll) {
            proto->dbg_ll = SOLU_DBG_LINE(proto->dbg[pc]);
            if (bps[proto->dbg_ll - 1]) {
                proto->dbg_res = pc;
                ++bpc; while (!*bpc) ++bpc;
                return solu_call_ex_err((solu_call_err){SOLU_ERRV_BREAK, SF_STR_EMPTY, pc});
            }
        }
        proto->dbg_ll = SOLU_DBG_LINE(proto->dbg[pc]);
        ++pc;
        if (s->cb > (size_t)((double)s->lb * SOLU_GCSTEP))
            solu_dcollect(s);
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
            solu_val fun = solu_get(s, solu_iabc_bx(ins));
            if (!solu_isdtype(fun, SOLU_DFUN)) {
                if (solu_isdtype(fun, SOLU_DERR))
                    return solu_callerr(SOLU_ERRV_TYPE_MISMATCH, "Attempted to call type %s: %s", solu_typename(fun).c_str, fun.dyn);
                return solu_callerr(SOLU_ERRV_TYPE_MISMATCH, "Attempted to call type %s", solu_typename(fun).c_str);
            }

            solu_fproto *f = fun.dyn;
            solu_call_ex fex;
            if (f->arg_c > 0) {
                solu_val *argv = calloc(f->arg_c, sizeof(solu_val));
                uint32_t argc = 0;
                for (; argc < f->arg_c && argc < s->frames.data[s->frames.count - 1].size; ++argc)
                    argv[argc] = solu_get(s, solu_iabc_cx(ins) + argc);
                fex = solu_call(s, f, argv, argc);
                free(argv);
            } else fex = solu_call(s, f, NULL, 0);
            if (!fex.is_ok) {
                fex.err.pc = pc - 1;
                return fex;
            }
            #ifdef SOLU_DBG_LOG
            sf_str ret = solu_tostring(fex.ok);
            printf("[RET] [Type: %s] %s\n", solu_typename(fex.ok).c_str, ret.c_str);
            sf_str_free(ret);
            #endif
            solu_set(s, solu_iabc_a(ins), fex.ok);
            DISPATCH();
        }

        CASE(SOLU_OP_ADD) {
            solu_val lhs = solu_iabc_bk(ins) ? solu_getk(proto, solu_iabc_bx(ins)) : solu_get(s, solu_iabc_bx(ins));
            solu_val rhs = solu_iabc_ck(ins) ? solu_getk(proto, solu_iabc_cx(ins)) : solu_get(s, solu_iabc_cx(ins));
            if (solu_isdtype(lhs, SOLU_DERR))
                return solu_callerr(SOLU_ERRV_PANIC, "%s", lhs.dyn);
            if (solu_isdtype(rhs, SOLU_DERR))
                return solu_callerr(SOLU_ERRV_PANIC, "%s", rhs.dyn);

            if (lhs.tt != rhs.tt) {
                if (lhs.tt == SOLU_TDYN || rhs.tt == SOLU_TDYN)
                    return solu_callerr(SOLU_ERRV_TYPE_MISMATCH, "Implicit conversion %s into %s", solu_typename(rhs).c_str, solu_typename(lhs).c_str);
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
                    if (!solu_isdtype(lhs, SOLU_DSTR))
                        return solu_callerr(SOLU_ERRV_TYPE_MISMATCH, "Type %s does not support operator '+'", solu_typename(lhs).c_str);
                    sf_str l =  sf_str_join(sf_ref(lhs.dyn), sf_ref(rhs.dyn));
                    solu_set(s, solu_iabc_a(ins), solu_dnstr(s, l.c_str));
                    sf_str_free(l);
                    break;
                }
                default: return solu_callerr(SOLU_ERRV_TYPE_MISMATCH, "Type %s does not support operator '+'", solu_typename(lhs).c_str);
            }
            DISPATCH();
        }
        CASE(SOLU_OP_SUB) {
            solu_val lhs = solu_iabc_bk(ins) ? solu_getk(proto, solu_iabc_bx(ins)) : solu_get(s, solu_iabc_bx(ins));
            solu_val rhs = solu_iabc_ck(ins) ? solu_getk(proto, solu_iabc_cx(ins)) : solu_get(s, solu_iabc_cx(ins));
            if (solu_isdtype(lhs, SOLU_DERR))
                return solu_callerr(SOLU_ERRV_PANIC, "%s", lhs.dyn);
            if (solu_isdtype(rhs, SOLU_DERR))
                return solu_callerr(SOLU_ERRV_PANIC, "%s", rhs.dyn);

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
            solu_val lhs = solu_iabc_bk(ins) ? solu_getk(proto, solu_iabc_bx(ins)) : solu_get(s, solu_iabc_bx(ins));
            solu_val rhs = solu_iabc_ck(ins) ? solu_getk(proto, solu_iabc_cx(ins)) : solu_get(s, solu_iabc_cx(ins));
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
            solu_val lhs = solu_iabc_bk(ins) ? solu_getk(proto, solu_iabc_bx(ins)) : solu_get(s, solu_iabc_bx(ins));
            solu_val rhs = solu_iabc_ck(ins) ? solu_getk(proto, solu_iabc_cx(ins)) : solu_get(s, solu_iabc_cx(ins));
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
            solu_val lhs = solu_iabc_bk(ins) ? solu_getk(proto, solu_iabc_bx(ins)) : solu_get(s, solu_iabc_bx(ins));
            solu_val rhs = solu_iabc_ck(ins) ? solu_getk(proto, solu_iabc_cx(ins)) : solu_get(s, solu_iabc_cx(ins));
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
            solu_val lhs = solu_iabc_bk(ins) ? solu_getk(proto, solu_iabc_bx(ins)) : solu_get(s, solu_iabc_bx(ins));
            solu_val rhs = solu_iabc_ck(ins) ? solu_getk(proto, solu_iabc_cx(ins)) : solu_get(s, solu_iabc_cx(ins));
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
            solu_val lhs = solu_iabc_bk(ins) ? solu_getk(proto, solu_iabc_bx(ins)) : solu_get(s, solu_iabc_bx(ins));
            solu_val rhs = solu_iabc_ck(ins) ? solu_getk(proto, solu_iabc_cx(ins)) : solu_get(s, solu_iabc_cx(ins));
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
            solu_val key = solu_iabc_bk(ins) ? solu_getk(proto, solu_iabc_bx(ins)) : solu_get(s, solu_iabc_bx(ins));
            solu_val val = solu_iabc_ck(ins) ? solu_getk(proto, solu_iabc_cx(ins)) : solu_get(s, solu_iabc_cx(ins));
            if (!solu_isdtype(obj, SOLU_DOBJ))
                return solu_callerr(SOLU_ERRV_TYPE_MISMATCH, "Attempted to index type %s", solu_typename(obj).c_str);
            if (!solu_isdtype(key, SOLU_DSTR))
                return solu_callerr(SOLU_ERRV_TYPE_MISMATCH, "Attempted to index type %s", solu_typename(key).c_str);
            solu_dobj_set((solu_dobj *)obj.dyn, sf_str_cdup(key.dyn), val);
            DISPATCH();
        }
        CASE(SOLU_OP_GET) {
            solu_val obj = solu_get(s, solu_iabc_bx(ins));
            solu_val key = solu_iabc_ck(ins) ? solu_getk(proto, solu_iabc_cx(ins)) : solu_get(s, solu_iabc_cx(ins));
            if (!solu_isdtype(obj, SOLU_DOBJ))
                return solu_callerr(SOLU_ERRV_TYPE_MISMATCH, "Attempted to index type %s", solu_typename(obj).c_str);
            if (!solu_isdtype(key, SOLU_DSTR))
                return solu_callerr(SOLU_ERRV_TYPE_MISMATCH, "Attempted to index object with type '%s", solu_typename(key).c_str);
            solu_dobj_ex ex = solu_dobj_get(obj.dyn, sf_ref(key.dyn));
            if (!ex.is_ok) {
                sf_str fm = sf_str_fmt("Member '%s' not found", key.dyn);
                solu_set(s, solu_iabc_a(ins), solu_dnerr(s, fm.c_str));
                sf_str_free(fm);
                DISPATCH();
            }
            solu_set(s, solu_iabc_a(ins), ex.ok);
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
            solu_val val = solu_get(s, solu_iabc_cx(ins));
            solu_dobj_set((solu_dobj *)upo.dyn, sf_str_cdup(kkey.dyn), val);
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

            solu_dalloc *dh = solu_dheader(kkey); (void)dh;
            solu_dobj_ex ex = solu_dobj_get((solu_dobj *)upo.dyn, sf_ref(kkey.dyn));
            if (!ex.is_ok) {
                sf_str e = sf_str_fmt("Member '%s' not found", kkey.dyn);
                solu_set(s, solu_iabc_a(ins), solu_dnerr(s, e.c_str));
                sf_str_free(e);
                DISPATCH();
            }
            solu_set(s, solu_iabc_a(ins), ex.ok);
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
    if (proto->tt == SOLU_FPROTO_BC && !sf_isempty(proto->file_name))
        sf_str_free(solu_filenames_pop(&s->files));
    return solu_call_ex_ok(return_val);
}

solu_call_ex solu_call(solu_state *state, solu_fproto *proto, const solu_val *args, uint32_t arg_c) {
    if (proto->tt == SOLU_FPROTO_BC)
        return solu_call_bc(state, proto, args, arg_c, NULL);
    return solu_call_cfun(state, proto, args, arg_c);
}

solu_call_ex solu_dcall(solu_state *state, solu_fproto *proto, const solu_val *args, uint32_t arg_c, bool *bps) {
    if (proto->tt == SOLU_FPROTO_BC)
        return solu_call_bc(state, proto, args, arg_c, bps);
    return solu_call_cfun(state, proto, args, arg_c);
}

#if (defined(__GNUC__) || defined(__clang__)) && !defined(SOLU_DBG_NOCOMPUTE)
#pragma GCC diagnostic pop
#endif
