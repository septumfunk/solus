#include <stdio.h>
#include <stdlib.h>
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
        .global = {SOL_TDYN, .dyn = p},
    };
    sol_filenames_push(&s->files, sf_lit("./"));
    return s;
}

void sol_state_free(sol_state *state) {
    sol_valvec_free(&state->stack);
    sol_filenames_free(&state->files);
    sol_dclean(state->global);
    free(state);
}

sol_compile_ex sol_csrc(sol_state *state, sf_str src) {
    sol_compile_ex ex = sol_cproto(src, 0, NULL, 1, (sol_upvalue[]){
        (sol_upvalue){sf_lit("_g"), SOL_UP_VAL, .value = state->global}
    });
    ex.ok.line_c = 1;
    for (char *c = src.c_str; *c != '\0'; ++c)
        if (*c == '\n') ++ex.ok.line_c;
    return ex;
}

sol_compile_ex sol_cfile(sol_state *state, sf_str path) {
    if (!sf_file_exists(path))
        return sol_compile_ex_err((sol_compile_err){SOL_ERRC_FILE_NOT_FOUND, 0, 0});
    sf_fsb_ex fsb = sf_file_buffer(path);
    if (!fsb.is_ok) {
        switch (fsb.err) {
            case SF_FILE_NOT_FOUND: return sol_compile_ex_err((sol_compile_err){SOL_ERRC_FILE_NOT_FOUND, 0, 0}); break;
            case SF_OPEN_FAILURE:
            case SF_READ_FAILURE: return sol_compile_ex_err((sol_compile_err){SOL_ERRC_FILE_UNREADABLE, 0, 0}); break;
        }
    }
    fsb.ok.flags = SF_BUFFER_GROW;
    sf_buffer_autoins(&fsb.ok, ""); // [\0]

    sol_compile_ex ex = sol_csrc(state, sf_ref((char *)fsb.ok.ptr));
    sf_buffer_clear(&fsb.ok);
    if (!ex.is_ok) return ex;
    ex.ok.file_name = sf_str_dup(path);
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
                return sf_str_dup(*(sf_str *)val.dyn); break;
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

sol_val sol_dnew(sol_state *s, sol_dtype tt) {
    size_t size;
    switch (tt) {
        case SOL_DSTR: size = sizeof(sf_str); break;
        case SOL_DERR: size = sizeof(sf_str); break;
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
        case SOL_DERR: *(sf_str *)p = SF_STR_EMPTY; break;
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

    sol_dalloc *dd = s->alloc;
    if (dd == NULL) s->alloc = dh;
    else {
        while (dd->next) dd = dd->next;
        dd->next = dh;
    }
    return (sol_val){ .tt = SOL_TDYN, .dyn = p };
}

sol_val sol_dcopy(sol_state *state, sol_val val) {
    if (val.tt == SOL_TDYN) {
        size_t size = sizeof(sol_dalloc) + sol_dheader(val)->size;
        sol_dalloc *ac = malloc(size);
        memcpy(ac, (char *)val.dyn - sizeof(sol_dalloc), size);
        sol_val nv = (sol_val){SOL_TDYN, .dyn=ac + 1};
        sol_dheader(nv)->mark = SOL_DYN_WHITE;

        switch (sol_dheader(nv)->tt) {
            case SOL_DSTR:
                *(sf_str *)nv.dyn = sf_str_dup(*(sf_str *)nv.dyn);
                break;
            case SOL_DFUN: {
                sol_fproto *fp = val.dyn, *nfp = nv.dyn;
                nfp->file_name = sf_str_dup(fp->file_name);
                nfp->constants = sol_valvec_new();
                nfp->code = malloc(sizeof(sol_instruction) * fp->code_c);
                nfp->dbg = malloc(sizeof(sol_dbg) * fp->code_c);
                nfp->upvals = malloc(sizeof(sol_upvalue) * nfp->up_c);
                memcpy(nfp->code, fp->code, sizeof(sol_instruction) * fp->code_c);
                memcpy(nfp->dbg, fp->dbg, sizeof(sol_dbg) * fp->code_c);
                memcpy(nfp->upvals, fp->upvals, sizeof(sol_upvalue) * nfp->up_c);
                for (sol_val *v = fp->constants.data; v < fp->constants.data + fp->constants.count; ++v)
                    sol_valvec_push(&nfp->constants, sol_dcopy(state, *v));
                break;
            }
            default: return SOL_NIL; // TODO: THIS
        }
        return nv;
    }
    return val;
}

void sol_dcollect_obj(void *ud, sf_str _k, sol_val member) {
    (void)_k;
    if (member.tt != SOL_TDYN || sol_dheader(member)->mark != SOL_DYN_WHITE) return;
    sol_dheader(member)->mark = SOL_DYN_BLACK;
    if (sol_dtypeof(member) == SOL_DOBJ)
        sol_dobj_foreach(member.dyn, sol_dcollect_obj, ud);
}

void sol_dcollect(sol_state *state) {
    for (sol_val *r = state->stack.data; r < state->stack.data + state->stack.count; ++r) {
        if (r->tt == SOL_TDYN) {
            sol_dheader(*r)->mark = SOL_DYN_BLACK;

            if (sol_dtypeof(*r) == SOL_DOBJ)
                sol_dobj_foreach(r->dyn, sol_dcollect_obj, NULL);

            if (sol_dtypeof(*r) == SOL_DREF) {
                sol_val inner = sol_dval(*r);
                while (inner.tt == SOL_TDYN) {
                    if (sol_dtypeof(inner) == SOL_DOBJ)
                        sol_dobj_foreach(inner.dyn, sol_dcollect_obj, NULL);
                    else if (sol_dtypeof(inner) == SOL_DREF)
                        inner = sol_dval(inner);
                    else {
                        sol_dheader(inner)->mark = SOL_DYN_BLACK;
                        break;
                    }
                }
            }
        }
    }
    sol_dobj_foreach(state->global.dyn, sol_dcollect_obj, NULL);

    sol_dalloc **ac = &state->alloc;
    while (*ac) {
        if ((*ac)->mark == SOL_DYN_WHITE) {
            sol_dalloc *dead = *ac;
            *ac = dead->next;
            sol_dclean((sol_val){SOL_TDYN, .dyn = dead + 1});
            continue;
        }
        (*ac)->mark = SOL_DYN_WHITE;
        ac = &(*ac)->next;
    }
}

void sol_log_op(sol_instruction ins) {
    switch (sol_op_info(sol_ins_op(ins))->type) {
        case SOL_INS_A: printf("[EXE] %s A:%d\n", sol_op_info(sol_ins_op(ins))->mnemonic, sol_ia_a(ins)); break;
        case SOL_INS_AB: printf("[EXE] %s A:%u B:%u\n", sol_op_info(sol_ins_op(ins))->mnemonic, sol_iab_a(ins), sol_iab_b(ins)); break;
        case SOL_INS_ABC: printf("[EXE] %s A:%d B:%u C:%u\n", sol_op_info(sol_ins_op(ins))->mnemonic, sol_iabc_a(ins), sol_iabc_b(ins), sol_iabc_c(ins)); break;
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
        if (pc >= proto->code_c) goto ret; \
        ins = proto->code[pc]; \
        if (bps && SOL_DBG_LINE(proto->dbg[pc]) > proto->dbg_ll) { \
            proto->dbg_ll = SOL_DBG_LINE(proto->dbg[pc]); \
            if (bps[proto->dbg_ll - 1]) { \
                proto->dbg_res = pc; \
                ++bpc; while (!*bpc) ++bpc; \
                return sol_call_ex_err((sol_call_err){SOL_ERRV_BREAK, SF_STR_EMPTY, pc}); \
            } \
        } \
        proto->dbg_ll = SOL_DBG_LINE(proto->dbg[pc]); \
        ++pc; \
        goto *computed[sol_ins_op(ins)]; \
    } while (0)
#   pragma GCC diagnostic push
#   pragma GCC diagnostic ignored "-Wpedantic"
#else
#   define DISPATCH() continue;
#   define CASE(name) case EXPAND(name):
#endif

#define sol_callerr(en, fmt, ...) (sol_call_ex_err((sol_call_err){.tt=(en),.panic=sf_str_fmt((fmt), __VA_ARGS__), .pc=pc-1}))

sol_val sol_wrapcfun(sol_state *state, sol_cfunction fptr, uint32_t arg_c, uint32_t temp_c) {
    sol_val fun = sol_dnew(state, SOL_DFUN);
    *(sol_fproto *)fun.dyn = sol_fproto_c(fptr, arg_c, temp_c);
    return fun;
}

#include <string.h>
#include <stdlib.h>

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

sol_call_ex sol_call_cfun(sol_state *state, sol_fproto *proto, const sol_val *args) {
    sol_pushframe(state, proto->reg_c);
    for (uint32_t i = 0; i < proto->arg_c && args; ++i)
        sol_set(state, i, args[i]);

    sol_call_ex ex = proto->c_fun(state);
    sol_popframe(state);
    return ex;
}

sol_call_ex sol_call_bc(sol_state *s, sol_fproto *proto, const sol_val *args, bool *bps) {
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

        LABEL(SOL_OP_EQ),
        LABEL(SOL_OP_LT),
        LABEL(SOL_OP_LE),

        LABEL(SOL_OP_SETU),
        LABEL(SOL_OP_GETU),
        LABEL(SOL_OP_REFU),

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
        for (uint32_t i = 0; i < proto->arg_c && args; ++i)
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
            DISPATCH();
        }
        CASE(SOL_OP_JMP) {
            pc = (uint32_t)((int32_t)pc + sol_ia_a(ins));
            DISPATCH();
        }
        CASE(SOL_OP_CALL) {
            sol_val fun = sol_get(s, sol_iabc_b(ins));
            if (!sol_isdtype(fun, SOL_DFUN))
                return sol_callerr(SOL_ERRV_TYPE_MISMATCH, "Expected fun at r[%d], found %s.", sol_iabc_b(ins), sol_typename(fun).c_str);

            sol_fproto f = *(sol_fproto *)fun.dyn;
            sol_call_ex fex;
            if (f.arg_c > 0) {
                sol_val *argv = calloc(f.arg_c, sizeof(sol_val));
                for (uint32_t i = 0; i < f.arg_c; ++i)
                    argv[i] = sol_get(s, sol_iabc_c(ins) + i);
                fex = sol_call(s, &f, argv);
                free(argv);
            } else fex = sol_call(s, &f, NULL);
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
            sol_val lhs = sol_get(s, sol_iabc_b(ins));
            sol_val rhs = sol_get(s, sol_iabc_c(ins));

            if (lhs.tt != rhs.tt) {
                if (lhs.tt == SOL_TDYN || rhs.tt == SOL_TDYN)
                    return sol_callerr(SOL_ERRV_TYPE_MISMATCH, "Cannot convert dynamic obj and primitive.", NULL);
                switch (lhs.tt) {
                    case SOL_TI64: rhs = (sol_val){.tt = SOL_TI64, .i64 = (sol_i64)rhs.f64}; break;
                    case SOL_TF64: rhs = (sol_val){.tt = SOL_TF64, .f64 = (sol_f64)rhs.i64}; break;
                    default: sol_callerr(SOL_ERRV_TYPE_MISMATCH, "Unknown Type", NULL);
                }
            }
            switch (lhs.tt) {
                case SOL_TNIL: return sol_callerr(SOL_ERRV_TYPE_MISMATCH, "Cannot perform arithmetic on nil.", NULL); break;
                case SOL_TF64:
                    sol_set(s, sol_iabc_a(ins), (sol_val){.tt = SOL_TF64, .f64 = lhs.f64 + rhs.f64});
                    break;
                case SOL_TI64:
                    sol_set(s, sol_iabc_a(ins), (sol_val){.tt = SOL_TI64, .i64 = lhs.i64 + rhs.i64});
                    break;
                case SOL_TDYN: {
                    if (!sol_isdtype(lhs, SOL_DSTR))
                        return sol_callerr(SOL_ERRV_TYPE_MISMATCH, "Cannot concatenate str with dynamic type.", NULL);
                    sol_set(s, sol_iabc_a(ins), sol_dnstr(s, sf_str_join(*(sf_str *)lhs.dyn, *(sf_str *)rhs.dyn)));
                    break;
                }
                default: break;
            }
            DISPATCH();
        }
        CASE(SOL_OP_SUB) {
            sol_val lhs = sol_get(s, sol_iabc_b(ins));
            sol_val rhs = sol_get(s, sol_iabc_c(ins));

            if (lhs.tt == SOL_TDYN || rhs.tt == SOL_TDYN)
                return sol_callerr(SOL_ERRV_TYPE_MISMATCH, "Cannot convert dynamic obj and primitive.", NULL);
            if (lhs.tt != rhs.tt) {
                switch (lhs.tt) {
                    case SOL_TI64: rhs = (sol_val){.tt = SOL_TI64, .i64 = (sol_i64)rhs.f64}; break;
                    case SOL_TF64: rhs = (sol_val){.tt = SOL_TF64, .f64 = (sol_f64)rhs.i64}; break;
                    default: return sol_callerr(SOL_ERRV_TYPE_MISMATCH, "Unknown Type", NULL);
                }
            }
            switch (lhs.tt) {
                case SOL_TF64:
                    sol_set(s, sol_iabc_a(ins), (sol_val){.tt = SOL_TF64, .f64 = lhs.f64 - rhs.f64});
                    break;
                case SOL_TI64:
                    sol_set(s, sol_iabc_a(ins), (sol_val){.tt = SOL_TI64, .i64 = lhs.i64 - rhs.i64});
                    break;
                default: return sol_callerr(SOL_ERRV_TYPE_MISMATCH, "Cannot perform arithmetic on nil.", NULL); break;
            }
            DISPATCH();
        }
        CASE(SOL_OP_MUL) {
            sol_val lhs = sol_get(s, sol_iabc_b(ins));
            sol_val rhs = sol_get(s, sol_iabc_c(ins));

            if (lhs.tt == SOL_TDYN || rhs.tt == SOL_TDYN)
                return sol_callerr(SOL_ERRV_TYPE_MISMATCH, "Cannot convert dynamic obj and primitive.", NULL);
            if (lhs.tt != rhs.tt) {
                switch (lhs.tt) {
                    case SOL_TI64: rhs = (sol_val){.tt = SOL_TI64, .i64 = (sol_i64)rhs.f64}; break;
                    case SOL_TF64: rhs = (sol_val){.tt = SOL_TF64, .f64 = (sol_f64)rhs.i64}; break;
                    default: return sol_callerr(SOL_ERRV_TYPE_MISMATCH, "Unknown Type", NULL);
                }
            }
            switch (lhs.tt) {
                case SOL_TF64:
                    sol_set(s, sol_iabc_a(ins), (sol_val){.tt = SOL_TF64, .f64 = lhs.f64 * rhs.f64});
                    break;
                case SOL_TI64:
                    sol_set(s, sol_iabc_a(ins), (sol_val){.tt = SOL_TI64, .i64 = lhs.i64 * rhs.i64});
                    break;
                default: return sol_callerr(SOL_ERRV_TYPE_MISMATCH, "Cannot perform arithmetic on nil.", NULL); break;
            }
            DISPATCH();
        }
        CASE(SOL_OP_DIV) {
            sol_val lhs = sol_get(s, sol_iabc_b(ins));
            sol_val rhs = sol_get(s, sol_iabc_c(ins));

            if (lhs.tt == SOL_TDYN || rhs.tt == SOL_TDYN)
                return sol_callerr(SOL_ERRV_TYPE_MISMATCH, "Cannot convert dynamic obj and primitive.", NULL);
            if (lhs.tt != rhs.tt) {
                switch (lhs.tt) {
                    case SOL_TI64: rhs = (sol_val){.tt = SOL_TI64, .i64 = (sol_i64)rhs.f64}; break;
                    case SOL_TF64: rhs = (sol_val){.tt = SOL_TF64, .f64 = (sol_f64)rhs.i64}; break;
                    default: return sol_callerr(SOL_ERRV_TYPE_MISMATCH, "Unknown Type", NULL);
                }
            }
            switch (lhs.tt) {
                case SOL_TF64:
                    sol_set(s, sol_iabc_a(ins), (sol_val){.tt = SOL_TF64, .f64 = lhs.f64 / rhs.f64});
                    break;
                case SOL_TI64:
                    sol_set(s, sol_iabc_a(ins), (sol_val){.tt = SOL_TI64, .i64 = lhs.i64 / rhs.i64});
                    break;
                default: return sol_callerr(SOL_ERRV_TYPE_MISMATCH, "Cannot perform arithmetic on nil.", NULL); break;
            }
            DISPATCH();
        }

        CASE(SOL_OP_EQ) {
            bool inv = sol_iabc_a(ins) != 0;
            sol_val lhs = sol_get(s, sol_iabc_b(ins));
            sol_val rhs = sol_get(s, sol_iabc_c(ins));
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
                    sol_dalloc *h2 = sol_dheader(lhs);
                    if (h1->tt != h2->tt) {
                        e = false;
                        break;
                    }
                    switch (h1->tt) {
                        case SOL_DSTR: e = sf_str_eq(*(sf_str *)lhs.dyn, *(sf_str *)rhs.dyn); break;
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
            sol_val lhs = sol_get(s, sol_iabc_b(ins));
            sol_val rhs = sol_get(s, sol_iabc_c(ins));
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
            sol_val lhs = sol_get(s, sol_iabc_b(ins));
            sol_val rhs = sol_get(s, sol_iabc_c(ins));
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

                case SOL_TDYN: {
                    sol_dalloc *h1 = sol_dheader(lhs);
                    sol_dalloc *h2 = sol_dheader(lhs);
                    if (h1->tt != h2->tt) {
                        e = false;
                        break;
                    }
                    switch (h1->tt) {
                        case SOL_DSTR: e = sf_str_cmp(*(sf_str *)lhs.dyn, *(sf_str *)rhs.dyn); break;
                        case SOL_DOBJ: e = lhs.dyn == rhs.dyn; break;
                        case SOL_DFUN: e = *(void **)lhs.dyn == *(void **)rhs.dyn; break;
                        default: return sol_callerr(SOL_ERRV_TYPE_MISMATCH, "Unknown Type", NULL);
                    }
                }
                default: break;
            }

            e = inv ? !e : e;
            if (e) pc++;
            DISPATCH();
        }

        CASE(SOL_OP_SETU) {
            sol_val v = sol_get(s, sol_iab_b(ins));
            sol_upvalue *upv = proto->upvals + sol_iab_a(ins);
            if (upv->tt == SOL_UP_VAL)
                upv->value = v;
            else sol_rawset(s, upv->ref, v, upv->frame);
            DISPATCH();
        }
        CASE(SOL_OP_GETU) {
            sol_upvalue *upv = proto->upvals + sol_iab_b(ins);
            if (upv->tt == SOL_UP_VAL)
                sol_set(s, sol_iab_a(ins), upv->value);
            else sol_set(s, sol_iab_a(ins), sol_rawget(s, upv->ref, upv->frame));
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

        CASE(SOL_OP_SET) {
            sol_val obj = sol_get(s, sol_iabc_a(ins));
            sol_val key = sol_get(s, sol_iabc_b(ins));
            sol_val val = sol_get(s, sol_iabc_c(ins));
            if (!sol_isdtype(obj, SOL_DOBJ))
                return sol_callerr(SOL_ERRV_TYPE_MISMATCH, "Expected obj at r[%d], found %s.", sol_iabc_a(ins), sol_typename(obj).c_str);
            if (!sol_isdtype(key, SOL_DSTR))
                return sol_callerr(SOL_ERRV_TYPE_MISMATCH, "Expected str at r[%d], found %s.", sol_iabc_b(ins), sol_typename(key).c_str);
            sol_dobj_set((sol_dobj *)obj.dyn, sf_str_dup(*(sf_str *)key.dyn), val);
            DISPATCH();
        }
        CASE(SOL_OP_GET) {
            sol_val obj = sol_get(s, sol_iabc_b(ins));
            sol_val key = sol_get(s, sol_iabc_c(ins));
            if (!sol_isdtype(obj, SOL_DOBJ))
                return sol_callerr(SOL_ERRV_TYPE_MISMATCH, "Expected obj at r[%d], found %s.", sol_iabc_b(ins), sol_typename(obj).c_str);
            if (!sol_isdtype(key, SOL_DSTR))
                return sol_callerr(SOL_ERRV_TYPE_MISMATCH, "Expected str at r[%d], found %s.", sol_iabc_c(ins), sol_typename(key).c_str);
            sol_dobj_ex ex = sol_dobj_get((sol_dobj *)obj.dyn, *(sf_str *)key.dyn);
            if (!ex.is_ok) {
                sol_set(s, sol_iabc_a(ins), sol_dnerr(s, sf_str_fmt("obj r[%d], does not contain member '%s'.", sol_iabc_b(ins), ((sf_str *)key.dyn)->c_str)));
                DISPATCH();
            }
            sol_set(s, sol_iabc_a(ins), ex.ok);
            DISPATCH();
        }

        CASE(SOL_OP_SUPO) {
            sol_upvalue *upv = proto->upvals + sol_iabc_a(ins);
            sol_val upo = upv->tt == SOL_UP_VAL ? upv->value : sol_rawget(s, upv->ref, upv->frame);
            if (!sol_isdtype(upo, SOL_DOBJ))
                return sol_callerr(SOL_ERRV_TYPE_MISMATCH, "Expected obj at u[%d], found %s.", sol_iabc_a(ins), sol_typename(upo).c_str);
            sol_val kkey = sol_valvec_get(&proto->constants, sol_iabc_b(ins));
            if (!sol_isdtype(kkey, SOL_DSTR))
                return sol_callerr(SOL_ERRV_TYPE_MISMATCH, "Expected str at k[%d], found %s.", sol_iabc_b(ins), sol_typename(kkey).c_str);
            sol_val val = sol_get(s, sol_iabc_c(ins));
            sol_dobj_set((sol_dobj *)upo.dyn, sf_str_dup(*(sf_str *)kkey.dyn), val);
            DISPATCH();
        }
        CASE(SOL_OP_GUPO) {
            sol_upvalue *upv = proto->upvals + sol_iabc_b(ins);
            sol_val upo = upv->tt == SOL_UP_VAL ? upv->value : sol_rawget(s, upv->ref, upv->frame);
            if (!sol_isdtype(upo, SOL_DOBJ))
                return sol_callerr(SOL_ERRV_TYPE_MISMATCH, "Expected obj at u[%d], found %s.", sol_iabc_b(ins), sol_typename(upo).c_str);
            sol_val kkey = sol_valvec_get(&proto->constants, sol_iabc_c(ins));
            if (!sol_isdtype(kkey, SOL_DSTR))
                return sol_callerr(SOL_ERRV_TYPE_MISMATCH, "Expected str at k[%d], found %s.", sol_iabc_c(ins), sol_typename(kkey).c_str);

            sol_dobj_ex ex = sol_dobj_get((sol_dobj *)upo.dyn, *(sf_str *)kkey.dyn);
            if (!ex.is_ok) {
                sol_set(s, sol_iabc_a(ins), sol_dnerr(s, sf_str_fmt("obj u[%d], does not contain member '%s'.", sol_iabc_b(ins), ((sf_str *)kkey.dyn)->c_str)));
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

sol_call_ex sol_call(sol_state *state, sol_fproto *proto, const sol_val *args) {
    if (proto->tt == SOL_FPROTO_BC)
        return sol_call_bc(state, proto, args, NULL);
    return sol_call_cfun(state, proto, args);
}

sol_call_ex sol_dcall(sol_state *state, sol_fproto *proto, const sol_val *args, bool *bps) {
    if (proto->tt == SOL_FPROTO_BC)
        return sol_call_bc(state, proto, args, bps);
    return sol_call_cfun(state, proto, args);
}

#if (defined(__GNUC__) || defined(__clang__)) && !defined(SOL_DBG_NOCOMPUTE)
#pragma GCC diagnostic pop
#endif
