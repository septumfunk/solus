#include <stdio.h>
#include <stdlib.h>
#include <stdlib.h>
#include <string.h>
#include "sol/vm.h"
#include "sf/containers/buffer.h"
#include "sf/fs.h"
#include "sol/bytecode.h"
#include "sol/solc.h"
#include "sf/str.h"

sol_state *sol_state_new(void) {
    sol_dyn p = calloc(1, sizeof(sol_dalloc) + sizeof(sol_dobj));
    *(sol_dalloc *)p = (sol_dalloc){NULL, sizeof(sol_dobj), SOL_DOBJ, SOL_DYN_GREEN};
    p = (char *)p + sizeof(sol_dalloc);
    *(sol_dobj *)p = sol_dobj_new();

    sol_state *s = malloc(sizeof(sol_state));
    *s = (sol_state){
        .stack = sol_valvec_new(),
        .files = sol_filenames_new(),
        .strcache = sol_strcache_new(),
        .global = {SOL_TDYN, .dyn = p},
        .lb = 1<<20, .cb = 0,
    };
    sol_filenames_push(&s->files, sf_lit("./"));
    return s;
}

void sol_state_free(sol_state *state) {
    sol_valvec_free(&state->stack);
    sol_filenames_free(&state->files);
    sol_strcache_free(&state->strcache);
    sol_dclean(state->global);
    free(state);
}

sol_compile_ex sol_csrc(sol_state *state, char *src) {
    sol_compile_ex ex = sol_cproto(src, 0, NULL, 1, (sol_upvalue[]){
        (sol_upvalue){sf_lit("_g"), SOL_UP_VAL, .value = state->global}
    });
    ex.ok.line_c = 1;
    for (char *c = src; *c != '\0'; ++c)
        if (*c == '\n') ++ex.ok.line_c;
    return ex;
}

sol_compile_ex sol_cfile(sol_state *state, char *path) {
    if (!sf_file_exists(sf_ref(path)))
        return sol_compile_ex_err((sol_compile_err){SOL_ERRC_FILE_NOT_FOUND, 0, 0});
    sf_fsb_ex fsb = sf_file_buffer(sf_ref(path));
    if (!fsb.is_ok) {
        switch (fsb.err) {
            case SF_FILE_NOT_FOUND: return sol_compile_ex_err((sol_compile_err){SOL_ERRC_FILE_NOT_FOUND, 0, 0}); break;
            case SF_OPEN_FAILURE:
            case SF_READ_FAILURE: return sol_compile_ex_err((sol_compile_err){SOL_ERRC_FILE_UNREADABLE, 0, 0}); break;
        }
    }
    fsb.ok.flags = SF_BUFFER_GROW;
    sf_buffer_autoins(&fsb.ok, ""); // [\0]

    sol_compile_ex ex = sol_csrc(state, (char *)fsb.ok.ptr);
    sf_buffer_clear(&fsb.ok);
    if (!ex.is_ok) return ex;
    ex.ok.file_name = sf_str_cdup(path);
    return ex;
}

sf_str sol_tostring(sol_val val) {
    switch (val.tt) {
        case SOL_TNIL: return sf_lit("nil");
        case SOL_TF64: return sf_str_fmt("%f", val.f64);
        case SOL_TI64: return sf_str_fmt("%lld", val.i64);
        case SOL_TBOOL: return sf_str_cdup(val.boolean ? "true" : "false");
        case SOL_TDYN: {
            switch (sol_dheader(val)->tt) {
                case SOL_DSTR:
                case SOL_DERR:
                return sf_str_cdup(val.dyn); break;
                case SOL_DOBJ:
                case SOL_DARRAY:
                case SOL_DFUN: return sf_str_fmt("%p", val.dyn);
                case SOL_DREF: return sol_tostring(*(sol_val *)val.dyn);

                case SOL_DUSR: {
                    sol_usrwrap *w = sol_uheader(val);
                    return w->tostring ? w->tostring(sol_uptr(val)) : sf_str_fmt("%p", val.dyn);
                }
                case SOL_DCOUNT: return SF_STR_EMPTY;
            }
        }
        default: return SF_STR_EMPTY;
    }
}

sf_str sol_stackdump(sol_state *state) {
    sf_str out = sf_str_cdup("====STACK DUMP====\n");
    for (uint32_t i = 0; i < state->stack.count; ++i) {
        sol_val val = sol_get(state, i);
        sf_str val_s = sol_tostring(val);
        sf_str line = sf_str_fmt(
            val.tt == SOL_TDYN && sol_dheader(val)->tt == SOL_DSTR ? "[%llu]: %s = '%s'\n" :
            "[%llu]: %s = %s\n", i, sol_typename(val).c_str, val_s.c_str
        );
        sf_str_append(&out, line);
        sf_str_free(val_s);
        sf_str_free(line);
    }
    sf_str_append(&out, sf_lit("=================="));
    return out;
}

void sol_dpush(sol_state *s, sol_dalloc *ac) {
    sol_dalloc *dd = s->alloc;
    if (dd == NULL) s->alloc = ac;
    else {
        while (dd->next) dd = dd->next;
        dd->next = ac;
    }
    s->cb += ac->size;
}

sol_val sol_dnew(sol_state *s, sol_dtype tt) {
    size_t size = 0;
    switch (tt) {
        case SOL_DSTR: size = 0; break;
        case SOL_DERR: size = 0; break;
        case SOL_DOBJ: size = sizeof(sol_dobj); break;
        case SOL_DARRAY: size = sizeof(sol_valvec); break;
        case SOL_DFUN: size = sizeof(sol_fproto); break;
        case SOL_DREF: size = sizeof(sol_val); break;

        case SOL_DUSR:
        case SOL_DCOUNT: return SOL_NIL;
    }

    sol_dyn p = calloc(1, sizeof(sol_dalloc) + size);
    sol_dalloc *dh = p;
    *dh = (sol_dalloc){
        .next = NULL,
        .size = size,
        .tt = tt,
        .mark = SOL_DYN_WHITE,
    };
    p = (char *)p + sizeof(sol_dalloc);

    switch (tt) {
        case SOL_DSTR:
        case SOL_DERR: break;
        case SOL_DOBJ: *(sol_dobj *)p = sol_dobj_new(); break;
        case SOL_DARRAY: *(sol_valvec *)p = sol_valvec_new(); break;
        case SOL_DFUN: *(sol_fproto *)p = sol_fproto_new(); break;
        case SOL_DREF: *(sol_val *)p = SOL_NIL; break;

        case SOL_DUSR:
        case SOL_DCOUNT: {
            free(dh);
            return SOL_NIL;
        }
    }

    sol_dpush(s, dh);
    return (sol_val){ .tt = SOL_TDYN, .dyn = p };
}

sol_val sol_dnstr(sol_state *s, const char *str) {
    size_t size = strlen(str) + 1;
    if (size <= SOL_STRCACHE_MAX - 1) {
        sol_strcache_ex sex = sol_strcache_get(&s->strcache, sf_ref(str));
        if (sex.is_ok)
            return (sol_val){ .tt = SOL_TDYN, .dyn = sex.ok };
    }

    sol_dyn p = calloc(1, sizeof(sol_dalloc) + size);
    sol_dalloc *dh = p;
    *dh = (sol_dalloc){
        .next = NULL,
        .size = size,
        .tt = SOL_DSTR,
        .mark = SOL_DYN_WHITE,
    };
    p = (char *)p + sizeof(sol_dalloc);
    memcpy(p, str, size);
    sol_dpush(s, dh);
    if (size > 1 && size <= SOL_STRCACHE_MAX - 1)
        sol_strcache_set(&s->strcache, sf_ref(p), dh);
    return (sol_val){ .tt = SOL_TDYN, .dyn = p };
}

sol_val sol_dscopy(sol_state *state, sol_val val, bool kconst) {
    if (val.tt != SOL_TDYN)
        return val; // This function only needs to copy dynamic constants

    sol_dalloc *ac = malloc(sizeof(sol_dalloc) + sol_dheader(val)->size);
    *ac = *(sol_dheader(val));
    ac->size = sol_dheader(val)->size;
    ac->mark = SOL_DYN_WHITE;
    ac->next = NULL;
    sol_val nv = (sol_val){SOL_TDYN, .dyn=(char*)ac + sizeof(sol_dalloc)};

    switch (sol_dheader(nv)->tt) {
        case SOL_DSTR:
            memcpy(nv.dyn, val.dyn, ac->size);
            break;
        case SOL_DFUN: {
            sol_fproto *fp = val.dyn, *nfp = nv.dyn;
            memcpy(nfp, fp, sizeof(sol_fproto));
            nfp->file_name = sf_str_dup(fp->file_name);
            nfp->constants = sol_valvec_new();
            nfp->code = malloc(sizeof(sol_instruction) * fp->code_c);
            nfp->dbg = malloc(sizeof(sol_dbg) * fp->code_c);

            // Deref Upvals
            nfp->upvals = malloc(sizeof(sol_upvalue) * nfp->up_c);
            for (uint32_t i = 0; i < nfp->up_c; ++i) {
                sol_upvalue upv = fp->upvals[i];
                if (kconst) {
                    upv.name = sf_str_dup(upv.name);
                    nfp->upvals[i] = upv;
                } else {
                    sol_val nv;
                    if (upv.tt == SOL_UP_REF) {
                        sol_val cv = sol_rawget(state, upv.ref, upv.frame);
                        if (cv.tt != SOL_TDYN) {
                            nv = sol_dnew(state, SOL_DREF);
                            sol_rawset(state, upv.ref, nv, upv.frame);
                            *(sol_val *)nv.dyn = cv;
                        } else nv = cv;
                    } else nv = upv.value;

                    nfp->upvals[i] = (sol_upvalue){
                        sf_str_dup(upv.name),
                        SOL_UP_VAL,
                        .value = nv,
                    };
                }
            }

            memcpy(nfp->code, fp->code, sizeof(sol_instruction) * fp->code_c);
            memcpy(nfp->dbg, fp->dbg, sizeof(sol_dbg) * fp->code_c);
            for (sol_val *v = fp->constants.data; v < fp->constants.data + fp->constants.count; ++v)
                sol_valvec_push(&nfp->constants, sol_dscopy(state, *v, true));
            break;
        }
        default: return SOL_NIL;
    }
    return nv;
}

sol_val sol_dcopy(sol_state *state, sol_val val) {
    if (val.tt == SOL_TDYN) {
        val = sol_dscopy(state, val, false);
        sol_dpush(state, sol_dheader(val));
    }
    return val;
}

void sol_dmarkfun(sol_fproto *fp) {
    for (sol_upvalue *v = fp->upvals; v && v < fp->upvals + fp->up_c; ++v) {
        if (v->tt == SOL_UP_VAL && v->value.tt == SOL_TDYN)
            sol_dheader(v->value)->mark = SOL_DYN_BLACK;
    }
}

void sol_dmarkref(sol_val *r);
void sol_dcollect_obj(void *ud, sf_str _k, sol_val member) {
    (void)_k;
    if (member.tt != SOL_TDYN || sol_dheader(member)->mark != SOL_DYN_WHITE) return;
    sol_dheader(member)->mark = SOL_DYN_BLACK;
    if (sol_dtypeof(member) == SOL_DOBJ)
        sol_dobj_foreach(member.dyn, sol_dcollect_obj, ud);
    else if (sol_dtypeof(member) == SOL_DFUN)
        sol_dmarkfun(member.dyn);
    else if (sol_dtypeof(member) == SOL_DREF)
        sol_dmarkref(&member);
}

void sol_dmarkref(sol_val *r) {
    sol_val inner = sol_dval(*r);
    if (sol_dheader(inner)->mark == SOL_DYN_BLACK)
        return;
    while (inner.tt == SOL_TDYN) {
        sol_dheader(inner)->mark = SOL_DYN_BLACK;
        switch (sol_dtypeof(inner)) {
            case SOL_DOBJ:
                sol_dobj_foreach(inner.dyn, sol_dcollect_obj, NULL);
                break;
            case SOL_DFUN:
                sol_dmarkfun(inner.dyn);
                break;
            case SOL_DREF:
                inner = sol_dval(inner);
                break;
            default: break;
        }
    }
}

void sol_dcollect(sol_state *state) {
    state->lb = 0;
    for (sol_val *r = state->stack.data; r < state->stack.data + state->stack.count; ++r) {
        if (r->tt == SOL_TDYN) {
            sol_dalloc *ac = sol_dheader(*r);
            ac->mark = SOL_DYN_BLACK;

            if (ac->tt == SOL_DOBJ)
                sol_dobj_foreach(r->dyn, sol_dcollect_obj, NULL);
            if (ac->tt == SOL_DFUN)
                sol_dmarkfun((sol_fproto *)((char *)ac + sizeof(sol_dalloc)));
            if (ac->tt == SOL_DREF)
                sol_dmarkref(r);
        }
    }
    sol_dobj_foreach(state->global.dyn, sol_dcollect_obj, NULL);

    sol_dalloc **ac = &state->alloc;
    while (*ac) {
        if ((*ac)->mark == SOL_DYN_WHITE) {
            sol_dalloc *dead = *ac;
            *ac = dead->next;
            if (dead->tt == SOL_DSTR)
                sol_strcache_delete(&state->strcache, sf_ref((char *)(dead + 1)));
            sol_dclean((sol_val){SOL_TDYN, .dyn = dead + 1});
            continue;
        }
        state->lb += (*ac)->size;
        (*ac)->mark = SOL_DYN_WHITE;
        ac = &(*ac)->next;
    }
}

#define CAT(a, b) a##b
#define EXPAND(a) a
#define EXPAND_CAT(a, b) CAT(a, b)

//#define SOL_DBG_NOCOMPUTE

#if (defined(__GNUC__) || defined(__clang__)) && !defined(SOL_DBG_NOCOMPUTE)
#   define LABEL(name) [name] = &&EXPAND_CAT(name, _L)
#   define CASE(name) EXPAND_CAT(name, _L):
#   define COMPUTE_GOTOS
#   define DISPATCH() do { \
        if (pc >= proto->code_c) goto ret; /* EOF */\
        ins = proto->code[pc]; /* Read next instruction */\
        \
        if (bps && SOL_DBG_LINE(proto->dbg[pc]) > proto->dbg_ll) { /* Debugger */\
            proto->dbg_ll = SOL_DBG_LINE(proto->dbg[pc]); \
            if (bps[proto->dbg_ll - 1]) { /* If breakpoints, check if we should break */\
                proto->dbg_res = pc; \
                ++bpc; while (!*bpc) ++bpc; /* Advance to next BP */\
                return sol_call_ex_err((sol_call_err){SOL_ERRV_BREAK, NULL, pc}); \
            } \
        } \
        proto->dbg_ll = SOL_DBG_LINE(proto->dbg[pc]); \
        ++pc; \
        \
        if (s->cb > (size_t)((double)s->lb * SOL_GCSTEP)) /* Collect on GC threshold reached */\
            sol_dcollect(s); \
        goto *computed[sol_ins_op(ins)]; /* jump up jump up and get down */\
    } while (0)
#   pragma GCC diagnostic push
#   pragma GCC diagnostic ignored "-Wpedantic"
#else
#   define DISPATCH() continue;
#   define CASE(name) case EXPAND(name):
#endif

#define sol_callerr(en, fmt, ...) (sol_call_ex_err((sol_call_err){.tt=(en),.panic=sf_str_fmt((fmt), __VA_ARGS__).c_str, .pc=pc-1}))

sol_val sol_wrapcfun(sol_state *state, sol_cfunction fptr, uint32_t arg_c, uint32_t temp_c) {
    sol_val fun = sol_dnew(state, SOL_DFUN);
    *(sol_fproto *)fun.dyn = sol_fproto_c(fptr, arg_c, temp_c);
    return fun;
}


static sf_str sol_dirname(sf_str path) {
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

sol_call_ex sol_call_cfun(sol_state *state, sol_fproto *proto, const sol_val *args, uint32_t arg_c) {
    sol_pushframe(state, proto->reg_c);
    for (uint32_t i = 0; i < proto->arg_c && args && i < arg_c; ++i)
        sol_set(state, i, args[i]);

    sol_call_ex ex = proto->c_fun(state);
    sol_popframe(state);
    return ex;
}

sol_call_ex sol_call_bc(sol_state *s, sol_fproto *proto, const sol_val *args, uint32_t arg_c, bool *bps) {
    if (proto->tt == SOL_FPROTO_BC && !sf_isempty(proto->file_name))
        sol_filenames_push(&s->files, sol_dirname(proto->file_name));
    #ifdef COMPUTE_GOTOS
    void *computed[] = {
        LABEL(SOL_OP_LOAD),
        LABEL(SOL_OP_MOVE),
        LABEL(SOL_OP_RET),
        LABEL(SOL_OP_JMP),
        LABEL(SOL_OP_CALL),

        LABEL(SOL_OP_ADD),
        LABEL(SOL_OP_SUB),
        LABEL(SOL_OP_MUL),
        LABEL(SOL_OP_DIV),

        LABEL(SOL_OP_NEG),
        LABEL(SOL_OP_EQ),
        LABEL(SOL_OP_LT),
        LABEL(SOL_OP_LE),

        LABEL(SOL_OP_SETU),
        LABEL(SOL_OP_GETU),
        LABEL(SOL_OP_REFU),

        LABEL(SOL_OP_NEW),
        LABEL(SOL_OP_SET),
        LABEL(SOL_OP_GET),

        LABEL(SOL_OP_SUPO),
        LABEL(SOL_OP_GUPO),

        LABEL(SOL_OP_UNKNOWN),
    };
    #endif

    sol_instruction ins;
    uint32_t pc = proto->dbg_res ? proto->dbg_res : proto->entry;
    if (!proto->dbg_res) {
        sol_pushframe(s, proto->reg_c);
        for (uint32_t i = 0; i < proto->arg_c && args && i < arg_c; ++i)
            sol_set(s, i, args[i]);
    }
    proto->dbg_res = 0;
    sol_val return_val = SOL_NIL;
    bool *bpc = bps;

    #ifdef COMPUTE_GOTOS
    DISPATCH();
    #else
    while (pc < proto->code_c) {
        ins = proto->code[pc];
        if (bps && SOL_DBG_LINE(proto->dbg[pc]) > proto->dbg_ll) {
            proto->dbg_ll = SOL_DBG_LINE(proto->dbg[pc]);
            if (bps[proto->dbg_ll - 1]) {
                proto->dbg_res = pc;
                ++bpc; while (!*bpc) ++bpc;
                return sol_call_ex_err((sol_call_err){SOL_ERRV_BREAK, SF_STR_EMPTY, pc});
            }
        }
        proto->dbg_ll = SOL_DBG_LINE(proto->dbg[pc]);
        ++pc;
        if (s->cb > (size_t)((double)s->lb * SOL_GCSTEP))
            sol_dcollect(s);
        switch (sol_ins_op(ins)) {
    #endif
        CASE(SOL_OP_LOAD) {
            sol_set(s, sol_iab_a(ins), sol_dcopy(s, sol_valvec_get(&proto->constants, sol_iab_b(ins))));
            DISPATCH();
        }
        CASE(SOL_OP_MOVE) {
            sol_set(s, sol_iab_a(ins), sol_get(s, sol_iab_b(ins)));
            DISPATCH();
        }
        CASE(SOL_OP_RET) {
            return_val = sol_get(s, (uint32_t)sol_ia_a(ins));
            goto ret;
        }
        CASE(SOL_OP_JMP) {
            pc = (uint32_t)((int32_t)pc + sol_ia_a(ins));
            DISPATCH();
        }
        CASE(SOL_OP_CALL) {
            sol_val fun = sol_get(s, sol_iabc_bx(ins));
            if (!sol_isdtype(fun, SOL_DFUN)) {
                if (sol_isdtype(fun, SOL_DERR))
                    return sol_callerr(SOL_ERRV_TYPE_MISMATCH, "Attempted to call type %s: %s", sol_typename(fun).c_str, fun.dyn);
                return sol_callerr(SOL_ERRV_TYPE_MISMATCH, "Attempted to call type %s", sol_typename(fun).c_str);
            }

            sol_fproto *f = fun.dyn;
            sol_call_ex fex;
            if (f->arg_c > 0) {
                sol_val *argv = calloc(f->arg_c, sizeof(sol_val));
                uint32_t argc = 0;
                for (; argc < f->arg_c && argc < s->frames.data[s->frames.count - 1].size; ++argc)
                    argv[argc] = sol_get(s, sol_iabc_cx(ins) + argc);
                fex = sol_call(s, f, argv, argc);
                free(argv);
            } else fex = sol_call(s, f, NULL, 0);
            if (!fex.is_ok) {
                fex.err.pc = pc - 1;
                return fex;
            }
            #ifdef SOL_DBG_LOG
            sf_str ret = sol_tostring(fex.ok);
            printf("[RET] [Type: %s] %s\n", sol_typename(fex.ok).c_str, ret.c_str);
            sf_str_free(ret);
            #endif
            sol_set(s, sol_iabc_a(ins), fex.ok);
            DISPATCH();
        }

        CASE(SOL_OP_ADD) {
            sol_val lhs = sol_iabc_bk(ins) ? sol_getk(proto, sol_iabc_bx(ins)) : sol_get(s, sol_iabc_bx(ins));
            sol_val rhs = sol_iabc_ck(ins) ? sol_getk(proto, sol_iabc_cx(ins)) : sol_get(s, sol_iabc_cx(ins));
            if (sol_isdtype(lhs, SOL_DERR))
                return sol_callerr(SOL_ERRV_PANIC, "%s", lhs.dyn);
            if (sol_isdtype(rhs, SOL_DERR))
                return sol_callerr(SOL_ERRV_PANIC, "%s", rhs.dyn);

            if (lhs.tt != rhs.tt) {
                if (lhs.tt == SOL_TDYN || rhs.tt == SOL_TDYN)
                    return sol_callerr(SOL_ERRV_TYPE_MISMATCH, "Implicit conversion %s into %s", sol_typename(rhs).c_str, sol_typename(lhs).c_str);
                switch (lhs.tt) {
                    case SOL_TI64: rhs = (sol_val){.tt = SOL_TI64, .i64 = (sol_i64)rhs.f64}; break;
                    case SOL_TF64: rhs = (sol_val){.tt = SOL_TF64, .f64 = (sol_f64)rhs.i64}; break;
                    default: return sol_callerr(SOL_ERRV_TYPE_MISMATCH, "Implicit conversion %s into %s", sol_typename(rhs).c_str, sol_typename(lhs).c_str);
                }
            }
            switch (lhs.tt) {
                case SOL_TF64:
                    sol_set(s, sol_iabc_a(ins), (sol_val){.tt = SOL_TF64, .f64 = lhs.f64 + rhs.f64});
                    break;
                case SOL_TI64:
                    sol_set(s, sol_iabc_a(ins), (sol_val){.tt = SOL_TI64, .i64 = lhs.i64 + rhs.i64});
                    break;
                case SOL_TDYN: {
                    if (!sol_isdtype(lhs, SOL_DSTR))
                        return sol_callerr(SOL_ERRV_TYPE_MISMATCH, "Type %s does not support operator '+'", sol_typename(lhs).c_str);
                    sf_str l =  sf_str_join(sf_ref(lhs.dyn), sf_ref(rhs.dyn));
                    sol_set(s, sol_iabc_a(ins), sol_dnstr(s, l.c_str));
                    sf_str_free(l);
                    break;
                }
                default: return sol_callerr(SOL_ERRV_TYPE_MISMATCH, "Type %s does not support operator '+'", sol_typename(lhs).c_str);
            }
            DISPATCH();
        }
        CASE(SOL_OP_SUB) {
            sol_val lhs = sol_iabc_bk(ins) ? sol_getk(proto, sol_iabc_bx(ins)) : sol_get(s, sol_iabc_bx(ins));
            sol_val rhs = sol_iabc_ck(ins) ? sol_getk(proto, sol_iabc_cx(ins)) : sol_get(s, sol_iabc_cx(ins));
            if (sol_isdtype(lhs, SOL_DERR))
                return sol_callerr(SOL_ERRV_PANIC, "%s", lhs.dyn);
            if (sol_isdtype(rhs, SOL_DERR))
                return sol_callerr(SOL_ERRV_PANIC, "%s", rhs.dyn);

            if (lhs.tt != SOL_TI64 && lhs.tt != SOL_TF64)
                return sol_callerr(SOL_ERRV_TYPE_MISMATCH, "Type %s does not support operator '-'", sol_typename(lhs).c_str);
            if (rhs.tt != SOL_TI64 && rhs.tt != SOL_TF64)
                return sol_callerr(SOL_ERRV_TYPE_MISMATCH, "Type %s does not support operator '-'", sol_typename(rhs).c_str);
            if (lhs.tt != rhs.tt) {
                switch (lhs.tt) {
                    case SOL_TI64: rhs = (sol_val){.tt = SOL_TI64, .i64 = (sol_i64)rhs.f64}; break;
                    case SOL_TF64: rhs = (sol_val){.tt = SOL_TF64, .f64 = (sol_f64)rhs.i64}; break;
                    default: return sol_callerr(SOL_ERRV_TYPE_MISMATCH, "Type %s does not support operator '-'", sol_typename(lhs).c_str);
                }
            }
            switch (lhs.tt) {
                case SOL_TF64:
                    sol_set(s, sol_iabc_a(ins), (sol_val){.tt = SOL_TF64, .f64 = lhs.f64 - rhs.f64});
                    break;
                case SOL_TI64:
                    sol_set(s, sol_iabc_a(ins), (sol_val){.tt = SOL_TI64, .i64 = lhs.i64 - rhs.i64});
                    break;
                default: return sol_callerr(SOL_ERRV_TYPE_MISMATCH, "Type %s does not support operator '-'", sol_typename(lhs).c_str); break;
            }
            DISPATCH();
        }
        CASE(SOL_OP_MUL) {
            sol_val lhs = sol_iabc_bk(ins) ? sol_getk(proto, sol_iabc_bx(ins)) : sol_get(s, sol_iabc_bx(ins));
            sol_val rhs = sol_iabc_ck(ins) ? sol_getk(proto, sol_iabc_cx(ins)) : sol_get(s, sol_iabc_cx(ins));
            if (sol_isdtype(lhs, SOL_DERR))
                return sol_callerr(SOL_ERRV_PANIC, "%s", lhs.dyn);
            if (sol_isdtype(rhs, SOL_DERR))
                return sol_callerr(SOL_ERRV_PANIC, "%s", rhs.dyn);

            if (lhs.tt != SOL_TI64 && lhs.tt != SOL_TF64)
                return sol_callerr(SOL_ERRV_TYPE_MISMATCH, "Type %s does not support operator '*'", sol_typename(lhs).c_str);
            if (rhs.tt != SOL_TI64 && rhs.tt != SOL_TF64)
                return sol_callerr(SOL_ERRV_TYPE_MISMATCH, "Type %s does not support operator '*'", sol_typename(rhs).c_str);
            if (lhs.tt != rhs.tt) {
                switch (lhs.tt) {
                    case SOL_TI64: rhs = (sol_val){.tt = SOL_TI64, .i64 = (sol_i64)rhs.f64}; break;
                    case SOL_TF64: rhs = (sol_val){.tt = SOL_TF64, .f64 = (sol_f64)rhs.i64}; break;
                    default: return sol_callerr(SOL_ERRV_TYPE_MISMATCH, "Type %s does not support operator '*'", sol_typename(lhs).c_str);
                }
            }
            switch (lhs.tt) {
                case SOL_TF64:
                    sol_set(s, sol_iabc_a(ins), (sol_val){.tt = SOL_TF64, .f64 = lhs.f64 * rhs.f64});
                    break;
                case SOL_TI64:
                    sol_set(s, sol_iabc_a(ins), (sol_val){.tt = SOL_TI64, .i64 = lhs.i64 * rhs.i64});
                    break;
                default: return sol_callerr(SOL_ERRV_TYPE_MISMATCH, "Type %s does not support operator '*'", NULL); break;
            }
            DISPATCH();
        }
        CASE(SOL_OP_DIV) {
            sol_val lhs = sol_iabc_bk(ins) ? sol_getk(proto, sol_iabc_bx(ins)) : sol_get(s, sol_iabc_bx(ins));
            sol_val rhs = sol_iabc_ck(ins) ? sol_getk(proto, sol_iabc_cx(ins)) : sol_get(s, sol_iabc_cx(ins));
            if (sol_isdtype(lhs, SOL_DERR))
                return sol_callerr(SOL_ERRV_PANIC, "%s", lhs.dyn);
            if (sol_isdtype(rhs, SOL_DERR))
                return sol_callerr(SOL_ERRV_PANIC, "%s", rhs.dyn);

            if (lhs.tt != SOL_TI64 && lhs.tt != SOL_TF64)
                return sol_callerr(SOL_ERRV_TYPE_MISMATCH, "Type %s does not support operator '/'", sol_typename(lhs).c_str);
            if (rhs.tt != SOL_TI64 && rhs.tt != SOL_TF64)
                return sol_callerr(SOL_ERRV_TYPE_MISMATCH, "Type %s does not support operator '/'", sol_typename(rhs).c_str);
            if (lhs.tt != rhs.tt) {
                switch (lhs.tt) {
                    case SOL_TI64: rhs = (sol_val){.tt = SOL_TI64, .i64 = (sol_i64)rhs.f64}; break;
                    case SOL_TF64: rhs = (sol_val){.tt = SOL_TF64, .f64 = (sol_f64)rhs.i64}; break;
                    default: return sol_callerr(SOL_ERRV_TYPE_MISMATCH, "Type %s does not support operator '/'", sol_typename(lhs).c_str);
                }
            }
            switch (lhs.tt) {
                case SOL_TF64:
                    sol_set(s, sol_iabc_a(ins), (sol_val){.tt = SOL_TF64, .f64 = lhs.f64 / rhs.f64});
                    break;
                case SOL_TI64:
                    sol_set(s, sol_iabc_a(ins), (sol_val){.tt = SOL_TI64, .i64 = lhs.i64 / rhs.i64});
                    break;
                default: return sol_callerr(SOL_ERRV_TYPE_MISMATCH, "Type %s does not support operator '/'", sol_typename(lhs).c_str);
            }
            DISPATCH();
        }

        CASE(SOL_OP_NEG) {
            sol_val in = sol_get(s, sol_iab_b(ins));
            switch (in.tt) {
                case SOL_TI64: in.i64 = -in.i64; break;
                case SOL_TF64: in.f64 = -in.f64; break;
                case SOL_TBOOL: in.boolean = !in.boolean; break;
                default: return sol_callerr(SOL_ERRV_TYPE_MISMATCH, "Type %s does not support prefix operator '-'", sol_typename(in).c_str);
            }
            sol_set(s, sol_iab_a(ins), in);
            DISPATCH();
        }
        CASE(SOL_OP_EQ) {
            bool inv = sol_iabc_a(ins) != 0;
            sol_val lhs = sol_iabc_bk(ins) ? sol_getk(proto, sol_iabc_bx(ins)) : sol_get(s, sol_iabc_bx(ins));
            sol_val rhs = sol_iabc_ck(ins) ? sol_getk(proto, sol_iabc_cx(ins)) : sol_get(s, sol_iabc_cx(ins));
            if ((lhs.tt == SOL_TNIL && rhs.tt == SOL_TNIL)) {
                if (!inv) pc++;
                DISPATCH();
            }
            if (lhs.tt == SOL_TBOOL && rhs.tt == SOL_TDYN) {
                if (inv ? !lhs.boolean : lhs.boolean) pc++;
                DISPATCH();
            }
            if (lhs.tt == SOL_TDYN && rhs.tt == SOL_TBOOL) {
                if (inv ? !rhs.boolean : rhs.boolean) pc++;
                DISPATCH();
            }

            if (lhs.tt != rhs.tt) {
                if (lhs.tt == SOL_TDYN || rhs.tt == SOL_TDYN || lhs.tt == SOL_TNIL || rhs.tt == SOL_TNIL) {
                    if (inv) pc++;
                    DISPATCH();
                }
                switch (lhs.tt) {
                    case SOL_TI64: rhs = (sol_val){.tt = SOL_TI64, .i64 = rhs.tt == SOL_TBOOL ? (lhs.boolean ? 1 : 0) : (sol_i64)rhs.f64}; break;
                    case SOL_TF64: rhs = (sol_val){.tt = SOL_TF64, .f64 = rhs.tt == SOL_TBOOL ? (lhs.boolean ? 1 : 0) : (sol_f64)rhs.i64}; break;
                    case SOL_TBOOL: rhs = (sol_val){.tt = SOL_TBOOL, .boolean = rhs.tt == SOL_TI64 ? rhs.i64 != 0 : rhs.f64 != 0};
                    default: return sol_callerr(SOL_ERRV_TYPE_MISMATCH, "Unknown Type", NULL);
                }
            }

            bool e = false;
            switch (lhs.tt) {
                case SOL_TI64: e = lhs.i64 == rhs.i64; break;
                case SOL_TF64: e = lhs.f64 == rhs.f64; break;
                case SOL_TBOOL: e = lhs.boolean == rhs.boolean; break;

                case SOL_TDYN: {
                    sol_dalloc *h1 = sol_dheader(lhs);
                    sol_dalloc *h2 = sol_dheader(rhs);
                    if (h1->tt != h2->tt) {
                        e = false;
                        break;
                    }
                    switch (h1->tt) {
                        case SOL_DSTR: e = lhs.dyn == rhs.dyn || strcmp(lhs.dyn, rhs.dyn) == 0; break;
                        case SOL_DOBJ:
                        case SOL_DARRAY:
                        case SOL_DFUN: e = lhs.dyn == rhs.dyn; break;
                        default: return sol_callerr(SOL_ERRV_TYPE_MISMATCH, "Unknown Type", NULL);
                    }
                }
                default: break;
            }

            e = inv ? !e : e;
            if (e) pc++;
            DISPATCH();
        }
        CASE(SOL_OP_LT) {
            bool inv = sol_iabc_a(ins) != 0;
            sol_val lhs = sol_iabc_bk(ins) ? sol_getk(proto, sol_iabc_bx(ins)) : sol_get(s, sol_iabc_bx(ins));
            sol_val rhs = sol_iabc_ck(ins) ? sol_getk(proto, sol_iabc_cx(ins)) : sol_get(s, sol_iabc_cx(ins));
            if (lhs.tt == SOL_TDYN || rhs.tt == SOL_TDYN || lhs.tt == SOL_TNIL || rhs.tt == SOL_TNIL ||
                lhs.tt == SOL_TBOOL || rhs.tt == SOL_TBOOL) {
                if (inv) pc++;
                DISPATCH();
            }
            if (lhs.tt != rhs.tt) {
                switch (lhs.tt) {
                    case SOL_TI64: rhs = (sol_val){.tt = SOL_TI64, .i64 = (sol_i64)rhs.f64}; break;
                    case SOL_TF64: rhs = (sol_val){.tt = SOL_TF64, .f64 = (sol_f64)rhs.i64}; break;
                    default: break;
                }
            }
            bool e = false;
            switch (lhs.tt) {
                case SOL_TI64: e = lhs.i64 < rhs.i64; break;
                case SOL_TF64: e = lhs.f64 < rhs.f64; break;
                default: break;
            }

            e = inv ? !e : e;
            if (e) pc++;
            DISPATCH();
        }
        CASE(SOL_OP_LE) {bool inv = sol_iabc_a(ins) != 0;
            sol_val lhs = sol_iabc_bk(ins) ? sol_getk(proto, sol_iabc_bx(ins)) : sol_get(s, sol_iabc_bx(ins));
            sol_val rhs = sol_iabc_ck(ins) ? sol_getk(proto, sol_iabc_cx(ins)) : sol_get(s, sol_iabc_cx(ins));
            if (lhs.tt == SOL_TDYN || rhs.tt == SOL_TDYN || lhs.tt == SOL_TNIL || rhs.tt == SOL_TNIL ||
                lhs.tt == SOL_TBOOL || rhs.tt == SOL_TBOOL) {
                if (inv) pc++;
                DISPATCH();
            }

            if (lhs.tt != rhs.tt) {
                if (lhs.tt == SOL_TDYN || rhs.tt == SOL_TDYN || lhs.tt == SOL_TNIL || rhs.tt == SOL_TNIL) {
                    if (inv) pc++;
                    DISPATCH();
                }
                switch (lhs.tt) {
                    case SOL_TI64: rhs = (sol_val){.tt = SOL_TI64, .i64 = (sol_i64)rhs.f64}; break;
                    case SOL_TF64: rhs = (sol_val){.tt = SOL_TF64, .f64 = (sol_f64)rhs.i64}; break;
                    default: break;
                }
            }

            bool e = false;
            switch (lhs.tt) {
                case SOL_TI64: e = lhs.i64 <= rhs.i64; break;
                case SOL_TF64: e = lhs.f64 <= rhs.f64; break;
                default: break;
            }

            e = inv ? !e : e;
            if (e) pc++;
            DISPATCH();
        }

        CASE(SOL_OP_SETU) {
            sol_val v = sol_get(s, sol_iab_b(ins));
            sol_upvalue *upv = proto->upvals + sol_iab_a(ins);
            if (upv->tt == SOL_UP_VAL) {
                if (sol_isdtype(upv->value, SOL_DREF)) {
                    *(sol_val *)upv->value.dyn = v;
                    DISPATCH();
                }
                upv->value = v;
            } else sol_rawset(s, upv->ref, v, upv->frame);
            DISPATCH();
        }
        CASE(SOL_OP_GETU) {
            sol_upvalue *upv = proto->upvals + sol_iab_b(ins);
            if (upv->tt == SOL_UP_VAL) {
                sol_set(s, sol_iab_a(ins), sol_isdtype(upv->value, SOL_DREF) ?
                    *(sol_val *)upv->value.dyn : upv->value);
            } else sol_set(s, sol_iab_a(ins), sol_rawget(s, upv->ref, upv->frame));
            DISPATCH();
        }
        CASE(SOL_OP_REFU) {
            sol_val v = sol_get(s, (uint32_t)sol_ia_a(ins));
            if (sol_isdtype(v, SOL_DREF))
                DISPATCH();
            sol_val vref = sol_dnew(s, SOL_DREF);
            *(sol_val *)vref.dyn = v;
            sol_set(s, (uint32_t)sol_ia_a(ins), vref);
            DISPATCH();
        }

        CASE(SOL_OP_NEW) {
            sol_set(s, (uint32_t)sol_ia_a(ins), sol_dnew(s, SOL_DOBJ));
            DISPATCH();
        }
        CASE(SOL_OP_SET) {
            sol_val obj = sol_get(s, sol_iabc_a(ins));
            sol_val key = sol_iabc_bk(ins) ? sol_getk(proto, sol_iabc_bx(ins)) : sol_get(s, sol_iabc_bx(ins));
            sol_val val = sol_iabc_ck(ins) ? sol_getk(proto, sol_iabc_cx(ins)) : sol_get(s, sol_iabc_cx(ins));
            if (!sol_isdtype(obj, SOL_DOBJ))
                return sol_callerr(SOL_ERRV_TYPE_MISMATCH, "Attempted to index type %s", sol_typename(obj).c_str);
            if (!sol_isdtype(key, SOL_DSTR))
                return sol_callerr(SOL_ERRV_TYPE_MISMATCH, "Attempted to index type %s", sol_typename(key).c_str);
            sol_dobj_set((sol_dobj *)obj.dyn, sf_str_cdup(key.dyn), val);
            DISPATCH();
        }
        CASE(SOL_OP_GET) {
            sol_val obj = sol_get(s, sol_iabc_bx(ins));
            sol_val key = sol_iabc_ck(ins) ? sol_getk(proto, sol_iabc_cx(ins)) : sol_get(s, sol_iabc_cx(ins));
            if (!sol_isdtype(obj, SOL_DOBJ))
                return sol_callerr(SOL_ERRV_TYPE_MISMATCH, "Attempted to index type %s", sol_typename(obj).c_str);
            if (!sol_isdtype(key, SOL_DSTR))
                return sol_callerr(SOL_ERRV_TYPE_MISMATCH, "Attempted to index object with type '%s", sol_typename(key).c_str);
            sol_dobj_ex ex = sol_dobj_get(obj.dyn, sf_ref(key.dyn));
            if (!ex.is_ok) {
                sf_str fm = sf_str_fmt("Member '%s' not found", key.dyn);
                sol_set(s, sol_iabc_a(ins), sol_dnerr(s, fm.c_str));
                sf_str_free(fm);
                DISPATCH();
            }
            sol_set(s, sol_iabc_a(ins), ex.ok);
            DISPATCH();
        }

        CASE(SOL_OP_SUPO) {
            sol_upvalue *upv = proto->upvals + sol_iabc_a(ins);
            sol_val upo = upv->tt == SOL_UP_VAL ? upv->value : sol_rawget(s, upv->ref, upv->frame);
            if (!sol_isdtype(upo, SOL_DOBJ))
                return sol_callerr(SOL_ERRV_CORRUPT, "Corrupt bytecode", NULL);
            sol_val kkey = sol_valvec_get(&proto->constants, sol_iabc_bx(ins));
            if (!sol_isdtype(kkey, SOL_DSTR))
                return sol_callerr(SOL_ERRV_CORRUPT, "Corrupt bytecode", NULL);
            sol_val val = sol_get(s, sol_iabc_cx(ins));
            sol_dobj_set((sol_dobj *)upo.dyn, sf_str_cdup(kkey.dyn), val);
            DISPATCH();
        }
        CASE(SOL_OP_GUPO) {
            sol_upvalue *upv = proto->upvals + sol_iabc_bx(ins);
            sol_val upo = upv->tt == SOL_UP_VAL ? upv->value : sol_rawget(s, upv->ref, upv->frame);
            if (!sol_isdtype(upo, SOL_DOBJ))
                return sol_callerr(SOL_ERRV_CORRUPT, "Corrupt bytecode", NULL);
            sol_val kkey = sol_valvec_get(&proto->constants, sol_iabc_cx(ins));
            if (!sol_isdtype(kkey, SOL_DSTR))
                return sol_callerr(SOL_ERRV_CORRUPT, "Corrupt bytecode", NULL);

            sol_dalloc *dh = sol_dheader(kkey); (void)dh;
            sol_dobj_ex ex = sol_dobj_get((sol_dobj *)upo.dyn, sf_ref(kkey.dyn));
            if (!ex.is_ok) {
                sf_str e = sf_str_fmt("Member '%s' not found", kkey.dyn);
                sol_set(s, sol_iabc_a(ins), sol_dnerr(s, e.c_str));
                sf_str_free(e);
                DISPATCH();
            }
            sol_set(s, sol_iabc_a(ins), ex.ok);
            DISPATCH();
        }

        CASE(SOL_OP_UNKNOWN) { DISPATCH(); }
    #ifndef COMPUTE_GOTOS
        }
    }
    #endif

ret: {}
    proto->dbg_res = 0;
    proto->dbg_ll = 0;
    sol_popframe(s);
    if (proto->tt == SOL_FPROTO_BC && !sf_isempty(proto->file_name))
        sf_str_free(sol_filenames_pop(&s->files));
    return sol_call_ex_ok(return_val);
}

sol_call_ex sol_call(sol_state *state, sol_fproto *proto, const sol_val *args, uint32_t arg_c) {
    if (proto->tt == SOL_FPROTO_BC)
        return sol_call_bc(state, proto, args, arg_c, NULL);
    return sol_call_cfun(state, proto, args, arg_c);
}

sol_call_ex sol_dcall(sol_state *state, sol_fproto *proto, const sol_val *args, uint32_t arg_c, bool *bps) {
    if (proto->tt == SOL_FPROTO_BC)
        return sol_call_bc(state, proto, args, arg_c, bps);
    return sol_call_cfun(state, proto, args, arg_c);
}

#if (defined(__GNUC__) || defined(__clang__)) && !defined(SOL_DBG_NOCOMPUTE)
#pragma GCC diagnostic pop
#endif
