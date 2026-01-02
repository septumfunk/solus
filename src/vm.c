#include <stdio.h>
#include "ctr/vm.h"
#include "ctr/bytecode.h"
#include "ctr/ctrc.h"
#include "sf/str.h"

ctr_state *ctr_state_new(void) {
    ctr_state *s = malloc(sizeof(ctr_state));
    *s = (ctr_state){
        .stack = ctr_valvec_new(),
        .global = ctr_dnew(CTR_DOBJ),
    };
    return s;
}

void ctr_state_free(ctr_state *state) {
    ctr_valvec_free(&state->stack);
    ctr_ddel(state->global);
    free(state);
}

ctr_compile_ex ctr_compile(ctr_state *state, sf_str src) {
    ctr_compile_ex ex = ctr_cproto(src, 0, NULL, 1, (ctr_upvalue[]){
        (ctr_upvalue){sf_lit("_g"), CTR_UP_VAL, .value = ctr_dref(state->global)}
    }, false);
    if (!ex.is_ok) return ex;
    return ctr_compile_ex_ok(ex.ok);
}

sf_str ctr_tostring(ctr_val val) {
    switch (val.tt) {
        case CTR_TNIL: return sf_lit("nil");
        case CTR_TF64: return sf_str_fmt("%f", val.f64);
        case CTR_TI64: return sf_str_fmt("%lld", val.i64);
        case CTR_TBOOL: return sf_str_cdup(val.boolean ? "true" : "false");
        case CTR_TDYN: {
            switch (ctr_header(val)->tt) {
                case CTR_DSTR:
                case CTR_DERR:
                return sf_str_dup(*(sf_str *)val.dyn); break;
                case CTR_DOBJ:
                case CTR_DARRAY:
                case CTR_DFUN: return sf_str_fmt("%p", val.dyn);
                case CTR_DREF: return ctr_tostring(*(ctr_val *)val.dyn);

                case CTR_DUSR: {
                    ctr_usrwrap *w = ctr_uheader(val);
                    return w->tostring ? w->tostring(ctr_uptr(val)) : sf_str_fmt("%p", val.dyn);
                }
                case CTR_DCOUNT: return SF_STR_EMPTY;
            }
        }
        case CTR_TCOUNT: return SF_STR_EMPTY;
    }
}

sf_str ctr_stackdump(ctr_state *state) {
    sf_str out = sf_str_cdup("====STACK DUMP====\n");
    for (uint32_t i = 0; i < state->stack.count; ++i) {
        ctr_val val = ctr_get(state, i);
        sf_str val_s = ctr_tostring(val);
        sf_str line = sf_str_fmt(
            val.tt == CTR_TDYN && ctr_header(val)->tt == CTR_DSTR ? "[%llu]: %s = '%s'\n" :
            "[%llu]: %s = %s\n", i, ctr_typename(val).c_str, val_s.c_str
        );
        sf_str_append(&out, line);
        sf_str_free(val_s);
        sf_str_free(line);
    }
    sf_str_append(&out, sf_lit("=================="));
    return out;
}


void ctr_log_op(ctr_instruction ins) {
    switch (ctr_op_info(ctr_ins_op(ins))->type) {
        case CTR_INS_A: printf("[EXE] %s A:%d\n", ctr_op_info(ctr_ins_op(ins))->mnemonic, ctr_ia_a(ins)); break;
        case CTR_INS_AB: printf("[EXE] %s A:%u B:%u\n", ctr_op_info(ctr_ins_op(ins))->mnemonic, ctr_iab_a(ins), ctr_iab_b(ins)); break;
        case CTR_INS_ABC: printf("[EXE] %s A:%d B:%u C:%u\n", ctr_op_info(ctr_ins_op(ins))->mnemonic, ctr_iabc_a(ins), ctr_iabc_b(ins), ctr_iabc_c(ins)); break;
    }
}

#define CAT(a, b) a##b
#define EXPAND(a) a
#define EXPACTR_ND_CAT(a, b) CAT(a, b)
#if defined(__GNUC__) || defined(__clang__)
#   define COMPUTE_GOTOS
#   ifdef CTR_DBG_LOG
#       define DISPATCH() do { if (pc >= proto->code_s) goto ret; ins = proto->code[pc++]; ctr_log_op(ins); goto *computed[ctr_ins_op(ins)]; } while(0)
#   else
#       define DISPATCH() do { if (pc >= proto->code_s) goto ret; ins = proto->code[pc++]; goto *computed[ctr_ins_op(ins)]; } while(0)
#   endif
#   define LABEL(name) [name] = &&EXPACTR_ND_CAT(name, _L)
#   define CASE(name) EXPACTR_ND_CAT(name, _L):
#   pragma GCC diagnostic push
#   pragma GCC diagnostic ignored "-Wpedantic"
#else
#   define DISPATCH() continue
#   define CASE(name) case EXPAND(name):
#endif

#define ctr_callerr(en, fmt, ...) (ctr_call_ex_err((ctr_call_err){.tt=(en),.panic=sf_str_fmt((fmt), __VA_ARGS__), .pc=pc-1}))

static inline uint32_t ctr_pushframe(ctr_state *state, uint32_t reg_c) {
    ctr_frames_push(&state->frames, (ctr_stackframe){
        state->frames.count == 0 ? 0 : state->frames.data[state->frames.count - 1].bottom_o + state->frames.data[state->frames.count - 1].size,
        reg_c,
    });
    return state->frames.count - 1;
}
static inline void ctr_popframe(ctr_state *state) { ctr_frames_pop(&state->frames); }

ctr_val ctr_wrapcfun(ctr_cfunction fptr, uint32_t arg_c, uint32_t temp_c) {
    ctr_val fun = ctr_dnew(CTR_DFUN);
    *(ctr_fproto *)fun.dyn = ctr_fproto_c(fptr, arg_c, temp_c);
    return fun;
}

ctr_call_ex ctr_call_cfun(ctr_state *state, ctr_fproto *proto, const ctr_val *args) {
    ctr_pushframe(state, proto->reg_c);
    for (uint32_t i = 0; i < proto->reg_c; ++i)
        ctr_valvec_push(&state->stack, CTR_NIL);
    for (uint32_t i = 0; i < proto->arg_c && args; ++i)
        ctr_set(state, i, args[i]);

    ctr_call_ex ex = proto->c_fun(state);

    for (uint32_t i = 0; i < proto->reg_c; ++i) {
        ctr_val v = ctr_valvec_pop(&state->stack);
        if (v.tt == CTR_TDYN)
            ctr_ddel(v);
    }

    ctr_popframe(state);
    return ex;
}

ctr_call_ex ctr_call_bc(ctr_state *state, ctr_fproto *proto, const ctr_val *args) {
    #ifdef COMPUTE_GOTOS
    void *computed[] = {
        LABEL(CTR_OP_LOAD),
        LABEL(CTR_OP_MOVE),
        LABEL(CTR_OP_RET),
        LABEL(CTR_OP_JMP),
        LABEL(CTR_OP_CALL),

        LABEL(CTR_OP_ADD),
        LABEL(CTR_OP_SUB),
        LABEL(CTR_OP_MUL),
        LABEL(CTR_OP_DIV),

        LABEL(CTR_OP_EQ),
        LABEL(CTR_OP_LT),
        LABEL(CTR_OP_LE),

        LABEL(CTR_OP_SETU),
        LABEL(CTR_OP_GETU),
        LABEL(CTR_OP_REFU),

        LABEL(CTR_OP_SET),
        LABEL(CTR_OP_GET),

        LABEL(CTR_OP_SUPO),
        LABEL(CTR_OP_GUPO),

        LABEL(CTR_OP_UNKNOWN),
    };
    #endif

    ctr_instruction ins;
    uint32_t pc = proto->entry;
    ctr_val return_val = CTR_NIL;

    uint32_t t_reg = proto->arg_c + proto->reg_c;
    ctr_pushframe(state, t_reg);
    for (uint32_t i = 0; i < t_reg; ++i)
        ctr_valvec_push(&state->stack, CTR_NIL);
    for (uint32_t i = 0; i < t_reg && args; ++i)
        ctr_set(state, i, args[i]);

    #ifdef COMPUTE_GOTOS
    DISPATCH();
    #else
    while (pc < proto->code_s) {
        ins = proto->code[pc];
        switch (ctr_ins_op(ins)) {
    #endif
        CASE(CTR_OP_LOAD) {
            ctr_set(state, ctr_iab_a(ins), ctr_dref(ctr_valvec_get(&proto->constants, ctr_iab_b(ins))));
            DISPATCH();
        }
        CASE(CTR_OP_MOVE) {
            ctr_set(state, ctr_iab_a(ins), ctr_dref(ctr_get(state, ctr_iab_b(ins))));
            DISPATCH();
        }
        CASE(CTR_OP_RET) {
            return_val = ctr_dref(ctr_get(state, (uint32_t)ctr_ia_a(ins)));
            goto ret;
            DISPATCH();
        }
        CASE(CTR_OP_JMP) {
            pc = (uint32_t)((int32_t)pc + ctr_ia_a(ins));
            DISPATCH();
        }
        CASE(CTR_OP_CALL) {
            ctr_val fun = ctr_get(state, ctr_iabc_b(ins));
            if (!ctr_isdtype(fun, CTR_DFUN))
                return ctr_callerr(CTR_ERRV_TYPE_MISMATCH, "Expected fun at r[%d], found %s.", ctr_iabc_b(ins), ctr_typename(fun).c_str);

            ctr_fproto f = *(ctr_fproto *)fun.dyn;
            ctr_call_ex fex;
            if (f.arg_c > 0) {
                ctr_val args[f.arg_c];
                for (uint32_t i = 0; i < f.arg_c; ++i)
                    args[i] = ctr_dref(ctr_get(state, ctr_iabc_c(ins) + i));
                fex = ctr_call(state, &f, args);
            } else fex = ctr_call(state, &f, NULL);
            if (!fex.is_ok) {
                fex.err.pc = pc - 1;
                return fex;
            }
            #ifdef CTR_DBG_LOG
            sf_str ret = ctr_tostring(fex.ok);
            printf("[RET] [Type: %s] %s\n", ctr_typename(fex.ok).c_str, ret.c_str);
            sf_str_free(ret);
            #endif
            ctr_set(state, ctr_iabc_a(ins), fex.ok);
            DISPATCH();
        }

        CASE(CTR_OP_ADD) {
            ctr_val lhs = ctr_get(state, ctr_iabc_b(ins));
            ctr_val rhs = ctr_get(state, ctr_iabc_c(ins));

            if (lhs.tt != rhs.tt) {
                if (lhs.tt == CTR_TDYN || rhs.tt == CTR_TDYN)
                    return ctr_callerr(CTR_ERRV_TYPE_MISMATCH, "Cannot convert dynamic obj and primitive.", NULL);
                switch (lhs.tt) {
                    case CTR_TI64: rhs = (ctr_val){.tt = CTR_TI64, .i64 = (ctr_i64)rhs.f64}; break;
                    case CTR_TF64: rhs = (ctr_val){.tt = CTR_TF64, .f64 = (ctr_f64)rhs.i64}; break;
                    default: ctr_callerr(CTR_ERRV_TYPE_MISMATCH, "Unknown Type", NULL);
                }
            }
            switch (lhs.tt) {
                case CTR_TNIL: return ctr_callerr(CTR_ERRV_TYPE_MISMATCH, "Cannot perform arithmetic on nil.", NULL); break;
                case CTR_TF64:
                    ctr_set(state, ctr_iabc_a(ins), (ctr_val){.tt = CTR_TF64, .f64 = lhs.f64 + rhs.f64});
                    break;
                case CTR_TI64:
                    ctr_set(state, ctr_iabc_a(ins), (ctr_val){.tt = CTR_TI64, .i64 = lhs.i64 + rhs.i64});
                    break;
                case CTR_TDYN: {
                    if (!ctr_isdtype(lhs, CTR_DSTR))
                        return ctr_callerr(CTR_ERRV_TYPE_MISMATCH, "Cannot concatenate str with dynamic type.", NULL);
                    ctr_set(state, ctr_iabc_a(ins), ctr_dnewstr(sf_str_join(*(sf_str *)lhs.dyn, *(sf_str *)rhs.dyn)));
                    break;
                }
                default: break;
            }
            DISPATCH();
        }
        CASE(CTR_OP_SUB) {
            ctr_val lhs = ctr_get(state, ctr_iabc_b(ins));
            ctr_val rhs = ctr_get(state, ctr_iabc_c(ins));

            if (lhs.tt == CTR_TDYN || rhs.tt == CTR_TDYN)
                return ctr_callerr(CTR_ERRV_TYPE_MISMATCH, "Cannot convert dynamic obj and primitive.", NULL);
            if (lhs.tt != rhs.tt) {
                switch (lhs.tt) {
                    case CTR_TI64: rhs = (ctr_val){.tt = CTR_TI64, .i64 = (ctr_i64)rhs.f64}; break;
                    case CTR_TF64: rhs = (ctr_val){.tt = CTR_TF64, .f64 = (ctr_f64)rhs.i64}; break;
                    default: return ctr_callerr(CTR_ERRV_TYPE_MISMATCH, "Unknown Type", NULL);
                }
            }
            switch (lhs.tt) {
                case CTR_TF64:
                    ctr_set(state, ctr_iabc_a(ins), (ctr_val){.tt = CTR_TF64, .f64 = lhs.f64 - rhs.f64});
                    break;
                case CTR_TI64:
                    ctr_set(state, ctr_iabc_a(ins), (ctr_val){.tt = CTR_TI64, .i64 = lhs.i64 - rhs.i64});
                    break;
                default: return ctr_callerr(CTR_ERRV_TYPE_MISMATCH, "Cannot perform arithmetic on nil.", NULL); break;
            }
            DISPATCH();
        }
        CASE(CTR_OP_MUL) {
            ctr_val lhs = ctr_get(state, ctr_iabc_b(ins));
            ctr_val rhs = ctr_get(state, ctr_iabc_c(ins));

            if (lhs.tt == CTR_TDYN || rhs.tt == CTR_TDYN)
                return ctr_callerr(CTR_ERRV_TYPE_MISMATCH, "Cannot convert dynamic obj and primitive.", NULL);
            if (lhs.tt != rhs.tt) {
                switch (lhs.tt) {
                    case CTR_TI64: rhs = (ctr_val){.tt = CTR_TI64, .i64 = (ctr_i64)rhs.f64}; break;
                    case CTR_TF64: rhs = (ctr_val){.tt = CTR_TF64, .f64 = (ctr_f64)rhs.i64}; break;
                    default: return ctr_callerr(CTR_ERRV_TYPE_MISMATCH, "Unknown Type", NULL);
                }
            }
            switch (lhs.tt) {
                case CTR_TF64:
                    ctr_set(state, ctr_iabc_a(ins), (ctr_val){.tt = CTR_TF64, .f64 = lhs.f64 * rhs.f64});
                    break;
                case CTR_TI64:
                    ctr_set(state, ctr_iabc_a(ins), (ctr_val){.tt = CTR_TI64, .i64 = lhs.i64 * rhs.i64});
                    break;
                default: return ctr_callerr(CTR_ERRV_TYPE_MISMATCH, "Cannot perform arithmetic on nil.", NULL); break;
            }
            DISPATCH();
        }
        CASE(CTR_OP_DIV) {
            ctr_val lhs = ctr_get(state, ctr_iabc_b(ins));
            ctr_val rhs = ctr_get(state, ctr_iabc_c(ins));

            if (lhs.tt == CTR_TDYN || rhs.tt == CTR_TDYN)
                return ctr_callerr(CTR_ERRV_TYPE_MISMATCH, "Cannot convert dynamic obj and primitive.", NULL);
            if (lhs.tt != rhs.tt) {
                switch (lhs.tt) {
                    case CTR_TI64: rhs = (ctr_val){.tt = CTR_TI64, .i64 = (ctr_i64)rhs.f64}; break;
                    case CTR_TF64: rhs = (ctr_val){.tt = CTR_TF64, .f64 = (ctr_f64)rhs.i64}; break;
                    default: return ctr_callerr(CTR_ERRV_TYPE_MISMATCH, "Unknown Type", NULL);
                }
            }
            switch (lhs.tt) {
                case CTR_TF64:
                    ctr_set(state, ctr_iabc_a(ins), (ctr_val){.tt = CTR_TF64, .f64 = lhs.f64 / rhs.f64});
                    break;
                case CTR_TI64:
                    ctr_set(state, ctr_iabc_a(ins), (ctr_val){.tt = CTR_TI64, .i64 = lhs.i64 / rhs.i64});
                    break;
                default: return ctr_callerr(CTR_ERRV_TYPE_MISMATCH, "Cannot perform arithmetic on nil.", NULL); break;
            }
            DISPATCH();
        }

        CASE(CTR_OP_EQ) {
            bool inv = ctr_iabc_a(ins) != 0;
            ctr_val lhs = ctr_get(state, ctr_iabc_b(ins));
            ctr_val rhs = ctr_get(state, ctr_iabc_c(ins));
            if ((lhs.tt == CTR_TNIL && rhs.tt == CTR_TNIL)) {
                if (!inv) pc++;
                DISPATCH();
            }
            if (lhs.tt == CTR_TBOOL && rhs.tt == CTR_TDYN) {
                if (inv ? !lhs.boolean : lhs.boolean) pc++;
                DISPATCH();
            }
            if (lhs.tt == CTR_TDYN && rhs.tt == CTR_TBOOL) {
                if (inv ? !rhs.boolean : rhs.boolean) pc++;
                DISPATCH();
            }

            if (lhs.tt != rhs.tt) {
                if (lhs.tt == CTR_TDYN || rhs.tt == CTR_TDYN || lhs.tt == CTR_TNIL || rhs.tt == CTR_TNIL) {
                    if (inv) pc++;
                    DISPATCH();
                }
                switch (lhs.tt) {
                    case CTR_TI64: rhs = (ctr_val){.tt = CTR_TI64, .i64 = rhs.tt == CTR_TBOOL ? (lhs.boolean ? 1 : 0) : (ctr_i64)rhs.f64}; break;
                    case CTR_TF64: rhs = (ctr_val){.tt = CTR_TF64, .f64 = rhs.tt == CTR_TBOOL ? (lhs.boolean ? 1 : 0) : (ctr_f64)rhs.i64}; break;
                    case CTR_TBOOL: rhs = (ctr_val){.tt = CTR_TBOOL, .boolean = rhs.tt == CTR_TI64 ? rhs.i64 != 0 : rhs.f64 != 0};
                    default: return ctr_callerr(CTR_ERRV_TYPE_MISMATCH, "Unknown Type", NULL);
                }
            }

            bool e = false;
            switch (lhs.tt) {
                case CTR_TI64: e = lhs.i64 == rhs.i64; break;
                case CTR_TF64: e = lhs.f64 == rhs.f64; break;
                case CTR_TBOOL: e = lhs.boolean == rhs.boolean; break;

                case CTR_TDYN: {
                    ctr_dheader *h1 = ctr_header(lhs);
                    ctr_dheader *h2 = ctr_header(lhs);
                    if (h1->tt != h2->tt) {
                        e = false;
                        break;
                    }
                    switch (h1->tt) {
                        case CTR_DSTR: e = sf_str_eq(*(sf_str *)lhs.dyn, *(sf_str *)rhs.dyn); break;
                        case CTR_DOBJ:
                        case CTR_DARRAY:
                        case CTR_DFUN: e = lhs.dyn == rhs.dyn; break;
                        default: return ctr_callerr(CTR_ERRV_TYPE_MISMATCH, "Unknown Type", NULL);
                    }
                }
                default: break;
            }

            e = inv ? !e : e;
            if (e) pc++;
            DISPATCH();
        }
        CASE(CTR_OP_LT) {
            bool inv = ctr_iabc_a(ins) != 0;
            ctr_val lhs = ctr_get(state, ctr_iabc_b(ins));
            ctr_val rhs = ctr_get(state, ctr_iabc_c(ins));
            if (lhs.tt == CTR_TDYN || rhs.tt == CTR_TDYN || lhs.tt == CTR_TNIL || rhs.tt == CTR_TNIL ||
                lhs.tt == CTR_TBOOL || rhs.tt == CTR_TBOOL) {
                if (inv) pc++;
                DISPATCH();
            }
            if (lhs.tt != rhs.tt) {
                switch (lhs.tt) {
                    case CTR_TI64: rhs = (ctr_val){.tt = CTR_TI64, .i64 = (ctr_i64)rhs.f64}; break;
                    case CTR_TF64: rhs = (ctr_val){.tt = CTR_TF64, .f64 = (ctr_f64)rhs.i64}; break;
                    default: break;
                }
            }
            bool e = false;
            switch (lhs.tt) {
                case CTR_TI64: e = lhs.i64 < rhs.i64; break;
                case CTR_TF64: e = lhs.f64 < rhs.f64; break;
                default: break;
            }

            e = inv ? !e : e;
            if (e) pc++;
            DISPATCH();
        }
        CASE(CTR_OP_LE) {bool inv = ctr_iabc_a(ins) != 0;
            ctr_val lhs = ctr_get(state, ctr_iabc_b(ins));
            ctr_val rhs = ctr_get(state, ctr_iabc_c(ins));
            if (lhs.tt == CTR_TDYN || rhs.tt == CTR_TDYN || lhs.tt == CTR_TNIL || rhs.tt == CTR_TNIL ||
                lhs.tt == CTR_TBOOL || rhs.tt == CTR_TBOOL) {
                if (inv) pc++;
                DISPATCH();
            }

            if (lhs.tt != rhs.tt) {
                if (lhs.tt == CTR_TDYN || rhs.tt == CTR_TDYN || lhs.tt == CTR_TNIL || rhs.tt == CTR_TNIL) {
                    if (inv) pc++;
                    DISPATCH();
                }
                switch (lhs.tt) {
                    case CTR_TI64: rhs = (ctr_val){.tt = CTR_TI64, .i64 = (ctr_i64)rhs.f64}; break;
                    case CTR_TF64: rhs = (ctr_val){.tt = CTR_TF64, .f64 = (ctr_f64)rhs.i64}; break;
                    default: break;
                }
            }

            bool e = false;
            switch (lhs.tt) {
                case CTR_TI64: e = lhs.i64 <= rhs.i64; break;
                case CTR_TF64: e = lhs.f64 <= rhs.f64; break;

                case CTR_TDYN: {
                    ctr_dheader *h1 = ctr_header(lhs);
                    ctr_dheader *h2 = ctr_header(lhs);
                    if (h1->tt != h2->tt) {
                        e = false;
                        break;
                    }
                    switch (h1->tt) {
                        case CTR_DSTR: e = sf_str_cmp(*(sf_str *)lhs.dyn, *(sf_str *)rhs.dyn); break;
                        case CTR_DOBJ: e = lhs.dyn == rhs.dyn; break;
                        case CTR_DFUN: e = *(void **)lhs.dyn == *(void **)rhs.dyn; break;
                        default: return ctr_callerr(CTR_ERRV_TYPE_MISMATCH, "Unknown Type", NULL);
                    }
                }
                default: break;
            }

            e = inv ? !e : e;
            if (e) pc++;
            DISPATCH();
        }

        CASE(CTR_OP_SETU) {
            ctr_val v = ctr_get(state, ctr_iab_b(ins));
            ctr_upvalue *upv = proto->upvals + ctr_iab_a(ins);
            if (upv->tt == CTR_UP_VAL)
                upv->value = ctr_dref(v);
            else ctr_rawset(state, upv->ref, ctr_dref(v), upv->frame);
            DISPATCH();
        }
        CASE(CTR_OP_GETU) {
            ctr_upvalue *upv = proto->upvals + ctr_iab_b(ins);
            if (upv->tt == CTR_UP_VAL)
                ctr_set(state, ctr_iab_a(ins), ctr_dref(upv->value));
            else ctr_set(state, ctr_iab_a(ins), ctr_dref(ctr_rawget(state, upv->ref, upv->frame)));
            DISPATCH();
        }
        CASE(CTR_OP_REFU) {
            ctr_val v = ctr_get(state, (uint32_t)ctr_ia_a(ins));
            if (ctr_isdtype(v, CTR_DREF)) {
                ctr_dref(v);
                DISPATCH();
            }
            ctr_val vref = ctr_dnew(CTR_DREF);
            *(ctr_val *)vref.dyn = ctr_dref(v);
            ctr_set(state, (uint32_t)ctr_ia_a(ins), vref);
            DISPATCH();
        }

        CASE(CTR_OP_SET) {
            ctr_val obj = ctr_get(state, ctr_iabc_a(ins));
            ctr_val key = ctr_get(state, ctr_iabc_b(ins));
            ctr_val val = ctr_get(state, ctr_iabc_c(ins));
            if (!ctr_isdtype(obj, CTR_DOBJ))
                return ctr_callerr(CTR_ERRV_TYPE_MISMATCH, "Expected obj at r[%d], found %s.", ctr_iabc_a(ins), ctr_typename(obj).c_str);
            if (!ctr_isdtype(key, CTR_DSTR))
                return ctr_callerr(CTR_ERRV_TYPE_MISMATCH, "Expected str at r[%d], found %s.", ctr_iabc_b(ins), ctr_typename(key).c_str);
            ctr_dobj_ex ex = ctr_dobj_get((ctr_dobj *)obj.dyn, *(sf_str *)key.dyn);
            if (ex.is_ok && ex.ok.tt == CTR_TDYN)
                ctr_ddel(ex.ok);
            ctr_dobj_set((ctr_dobj *)obj.dyn, sf_str_dup(*(sf_str *)key.dyn), ctr_dref(val));
            DISPATCH();
        }
        CASE(CTR_OP_GET) {
            ctr_val obj = ctr_get(state, ctr_iabc_b(ins));
            ctr_val key = ctr_get(state, ctr_iabc_c(ins));
            if (!ctr_isdtype(obj, CTR_DOBJ))
                return ctr_callerr(CTR_ERRV_TYPE_MISMATCH, "Expected obj at r[%d], found %s.", ctr_iabc_b(ins), ctr_typename(obj).c_str);
            if (!ctr_isdtype(key, CTR_DSTR))
                return ctr_callerr(CTR_ERRV_TYPE_MISMATCH, "Expected str at r[%d], found %s.", ctr_iabc_c(ins), ctr_typename(key).c_str);
            ctr_dobj_ex ex = ctr_dobj_get((ctr_dobj *)obj.dyn, *(sf_str *)key.dyn);
            if (!ex.is_ok) {
                ctr_set(state, ctr_iabc_a(ins), ctr_dnewerr(sf_str_fmt("obj r[%d], does not contain member '%s'.", ctr_iabc_b(ins), ((sf_str *)key.dyn)->c_str)));
                DISPATCH();
            }
            ctr_set(state, ctr_iabc_a(ins), ctr_dref(ex.ok));
            DISPATCH();
        }

        CASE(CTR_OP_SUPO) {
            ctr_upvalue *upv = proto->upvals + ctr_iabc_a(ins);
            ctr_val upo = upv->tt == CTR_UP_VAL ? upv->value : ctr_rawget(state, upv->ref, upv->frame);
            if (!ctr_isdtype(upo, CTR_DOBJ))
                return ctr_callerr(CTR_ERRV_TYPE_MISMATCH, "Expected obj at u[%d], found %s.", ctr_iabc_a(ins), ctr_typename(upo).c_str);
            ctr_val kkey = ctr_valvec_get(&proto->constants, ctr_iabc_b(ins));
            if (!ctr_isdtype(kkey, CTR_DSTR))
                return ctr_callerr(CTR_ERRV_TYPE_MISMATCH, "Expected str at k[%d], found %s.", ctr_iabc_b(ins), ctr_typename(kkey).c_str);
            ctr_val val = ctr_get(state, ctr_iabc_c(ins));

            ctr_dobj_ex ex = ctr_dobj_get((ctr_dobj *)upo.dyn, *(sf_str *)kkey.dyn);
            if (ex.is_ok && ex.ok.tt == CTR_TDYN)
                ctr_ddel(ex.ok);
            ctr_dobj_set((ctr_dobj *)upo.dyn, sf_str_dup(*(sf_str *)kkey.dyn), ctr_dref(val));
            DISPATCH();
        }
        CASE(CTR_OP_GUPO) {
            ctr_upvalue *upv = proto->upvals + ctr_iabc_b(ins);
            ctr_val upo = upv->tt == CTR_UP_VAL ? upv->value : ctr_rawget(state, upv->ref, upv->frame);
            if (!ctr_isdtype(upo, CTR_DOBJ))
                return ctr_callerr(CTR_ERRV_TYPE_MISMATCH, "Expected obj at u[%d], found %s.", ctr_iabc_b(ins), ctr_typename(upo).c_str);
            ctr_val kkey = ctr_valvec_get(&proto->constants, ctr_iabc_c(ins));
            if (!ctr_isdtype(kkey, CTR_DSTR))
                return ctr_callerr(CTR_ERRV_TYPE_MISMATCH, "Expected str at k[%d], found %s.", ctr_iabc_c(ins), ctr_typename(kkey).c_str);

            ctr_dobj_ex ex = ctr_dobj_get((ctr_dobj *)upo.dyn, *(sf_str *)kkey.dyn);
            if (!ex.is_ok) {
                ctr_set(state, ctr_iabc_a(ins), ctr_dnewerr(sf_str_fmt("obj u[%d], does not contain member '%s'.", ctr_iabc_b(ins), ((sf_str *)kkey.dyn)->c_str)));
                DISPATCH();
            }
            ctr_set(state, ctr_iabc_a(ins), ctr_dref(ex.ok));
            DISPATCH();
        }

        CASE(CTR_OP_UNKNOWN) { DISPATCH(); }
    #ifndef COMPUTE_GOTOS
        }
    }
    #endif

ret: {}
    for (uint32_t i = 0; i < proto->reg_c; ++i) {
        ctr_val v = ctr_valvec_pop(&state->stack);
        if (v.tt == CTR_TDYN)
            ctr_ddel(v);
    }
    ctr_popframe(state);
    return ctr_call_ex_ok(return_val);
}

ctr_call_ex ctr_call(ctr_state *state, ctr_fproto *proto, const ctr_val *args) {
    if (proto->tt == CTR_FPROTO_BC)
        return ctr_call_bc(state, proto, args);
    return ctr_call_cfun(state, proto, args);
}

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic pop
#endif
