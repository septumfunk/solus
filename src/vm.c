#include <stdio.h>
#include "ctr/vm.h"
#include "ctr/bytecode.h"
#include "ctr/ctrc.h"
#include "sf/str.h"

ctr_state *ctr_state_new(void) {
    ctr_state *s = malloc(sizeof(ctr_state));
    *s = (ctr_state){
        .stack = ctr_valvec_new(),
        .protos = ctr_protovec_new(),
        .global = ctr_dnew(CTR_DOBJ),
    };
    return s;
}

void ctr_state_free(ctr_state *state) {
    ctr_valvec_free(&state->stack);
    ctr_protovec_free(&state->protos);
    ctr_ddel(state->global);
    free(state);
}

ctr_compile_ex ctr_compile(ctr_state *state, sf_str src) {
    ctr_compile_ex ex = ctr_cproto(src, 0, NULL, 1, (ctr_upvalue[]){
        (ctr_upvalue){sf_lit("_g"), CTR_UP_VAL, .inner.value = ctr_dref(state->global)}
    });
    if (!ex.is_ok) return ex;
    return ctr_compile_ex_ok(ex.value.ok);
}

sf_str ctr_tostring(ctr_val val) {
    switch (val.tt) {
        case CTR_TNIL: return sf_lit("nil");
        case CTR_TF64: return sf_str_fmt("%f", val.val.f64);
        case CTR_TI64: return sf_str_fmt("%lld", val.val.i64);
        case CTR_TDYN: {
            switch (ctr_header(val)->tt) {
                case CTR_DSTR: return sf_str_dup(*(sf_str *)val.val.dyn); break;
                case CTR_DOBJ:
                case CTR_DFUN: return sf_str_fmt("%p", val.val.dyn);
                default: return SF_STR_EMPTY;
            }
        }
        default: return SF_STR_EMPTY;
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
#   ifdef CTR_DEBUG_LOG
#       define DISPATCH() do { if (pc >= proto->code_s) goto ret; ins = proto->code.bc[pc++]; ctr_log_op(ins); goto *computed[ctr_ins_op(ins)]; } while(0)
#   else
#       define DISPATCH() do { if (pc >= proto->code_s) goto ret; ins = proto->code.bc[pc++]; goto *computed[ctr_ins_op(ins)]; } while(0)
#   endif
#   define LABEL(name) [name] = &&EXPACTR_ND_CAT(name, _L)
#   define CASE(name) EXPACTR_ND_CAT(name, _L):
#   pragma GCC diagnostic push
#   pragma GCC diagnostic ignored "-Wpedantic"
#else
#   define DISPATCH() continue
#   define CASE(name) case EXPAND(name):
#endif

#define call_err(en, fmt, ...) (ctr_call_ex_err((ctr_call_err){.tt=(en),.string=sf_str_fmt((fmt), __VA_ARGS__), .pc=pc-1}))

static inline uint32_t ctr_pushframe(ctr_state *state, uint32_t reg_c) {
    state->frames = realloc(state->frames, ++state->frame_c * sizeof(ctr_stackframe));
    state->frames[state->frame_c - 1] = (ctr_stackframe){
        state->frame_c == 1 ? 0 : state->frames[state->frame_c - 2].bottom_o + state->frames[state->frame_c - 2].size,
        reg_c,
    };
    return state->frame_c - 1;
}

static inline void ctr_popframe(ctr_state *state) {
    state->frames = realloc(state->frames, --state->frame_c * sizeof(ctr_stackframe));
}

ctr_val ctr_wrapcfun(ctr_cfunction fptr, uint32_t arg_c, uint32_t temp_c) {
    ctr_val fun = ctr_dnew(CTR_DFUN);
    *(ctr_proto *)fun.val.dyn = ctr_proto_cfun(fptr, arg_c, temp_c);
    return fun;
}

ctr_call_ex ctr_call_cfun(ctr_state *state, ctr_proto *proto, const ctr_val *args) {
    ctr_pushframe(state, proto->reg_c);
    for (uint32_t i = 0; i < proto->reg_c; ++i)
        ctr_valvec_push(&state->stack, CTR_NIL);
    for (uint32_t i = 0; i < proto->arg_c && args; ++i)
        ctr_set(state, i, args[i]);

    ctr_call_ex ex = proto->code.c_fun(state);

    for (uint32_t i = 0; i < proto->reg_c; ++i) {
        ctr_val v = ctr_valvec_pop(&state->stack);
        if (v.tt == CTR_TDYN)
            ctr_ddel(v);
    }

    ctr_popframe(state);
    return ex;
}

ctr_call_ex ctr_call_bc(ctr_state *state, ctr_proto *proto, const ctr_val *args) {
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

        LABEL(CTR_OP_UP_SET),
        LABEL(CTR_OP_UP_GET),
        LABEL(CTR_OP_UP_REF),

        LABEL(CTR_OP_OBJ_NEW),
        LABEL(CTR_OP_OBJ_SET),
        LABEL(CTR_OP_OBJ_GET),

        LABEL(CTR_OP_STR_FROM),
        LABEL(CTR_OP_STR_ECHO),

        LABEL(CTR_OP_DBG_DUMP),

        LABEL(CTR_OP_UNKNOWN),
    };
    #endif

    ctr_instruction ins;
    uint32_t pc = proto->entry;
    ctr_val return_val = CTR_NIL;

    ctr_pushframe(state, proto->reg_c);
    for (uint32_t i = 0; i < proto->reg_c; ++i)
        ctr_valvec_push(&state->stack, CTR_NIL);
    for (uint32_t i = 0; i < proto->arg_c && args; ++i)
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
            if (fun.tt != CTR_TDYN || ctr_header(fun)->tt != CTR_DFUN)
                return call_err(CTR_ERRV_TYPE_MISMATCH, "Expected fun at [%d], found %s.", ctr_iabc_b(ins), ctr_typename(fun).c_str);

            ctr_proto f = *(ctr_proto *)fun.val.dyn;
            ctr_call_ex fex;
            if (f.arg_c > 0) {
                ctr_val args[f.arg_c];
                for (uint32_t i = 0; i < f.arg_c; ++i)
                    args[i] = ctr_dref(ctr_get(state, ctr_iabc_c(ins) + i));
                fex = ctr_call(state, &f, args);
            } else fex = ctr_call(state, &f, NULL);
            if (!fex.is_ok)
                return fex;
            #ifdef CTR_DEBUG_LOG
            sf_str ret = ctr_tostring(fex.value.ok);
            printf("[RET] [Type: %s] %s\n", ctr_typename(fex.value.ok).c_str, ret.c_str);
            sf_str_free(ret);
            #endif
            ctr_set(state, ctr_iabc_a(ins), fex.value.ok);
            DISPATCH();
        }

        CASE(CTR_OP_ADD) {
            ctr_val lhs = ctr_get(state, ctr_iabc_b(ins));
            ctr_val rhs = ctr_get(state, ctr_iabc_c(ins));

            if (lhs.tt != rhs.tt) {
                if (lhs.tt == CTR_TDYN || rhs.tt == CTR_TDYN)
                    return call_err(CTR_ERRV_TYPE_MISMATCH, "Cannot convert dynamic object and primitive.", NULL);
                switch (lhs.tt) {
                    case CTR_TI64: rhs = (ctr_val){.tt = CTR_TI64, .val.i64 = (ctr_i64)rhs.val.f64}; break;
                    case CTR_TF64: rhs = (ctr_val){.tt = CTR_TF64, .val.f64 = (ctr_f64)rhs.val.i64}; break;
                    default: call_err(CTR_ERRV_TYPE_MISMATCH, "Unknown Type", NULL);
                }
            }
            switch (lhs.tt) {
                case CTR_TNIL: return call_err(CTR_ERRV_TYPE_MISMATCH, "Cannot perform arithmetic on nil.", NULL); break;
                case CTR_TF64:
                    ctr_set(state, ctr_iabc_a(ins), (ctr_val){.tt = CTR_TF64, .val.f64 = lhs.val.f64 + rhs.val.f64});
                    break;
                case CTR_TI64:
                    ctr_set(state, ctr_iabc_a(ins), (ctr_val){.tt = CTR_TI64, .val.i64 = lhs.val.i64 + rhs.val.i64});
                    break;
                case CTR_TDYN: {
                    ctr_dheader *dh1 = ctr_header(lhs), *dh2 = ctr_header(rhs); (void)dh1; (void)dh2;
                    if (ctr_header(lhs)->tt != CTR_DSTR || ctr_header(rhs)->tt != CTR_DSTR)
                        return call_err(CTR_ERRV_TYPE_MISMATCH, "Cannot concatenate str with dynamic type.", NULL);
                    ctr_val str = ctr_dnew(CTR_DSTR);
                    *(sf_str *)str.val.dyn = sf_str_join(*(sf_str *)lhs.val.dyn, *(sf_str *)rhs.val.dyn);
                    ctr_set(state, ctr_iabc_a(ins), str);
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
                return call_err(CTR_ERRV_TYPE_MISMATCH, "Cannot convert dynamic object and primitive.", NULL);
            if (lhs.tt != rhs.tt) {
                switch (lhs.tt) {
                    case CTR_TI64: rhs = (ctr_val){.tt = CTR_TI64, .val.i64 = (ctr_i64)rhs.val.f64}; break;
                    case CTR_TF64: rhs = (ctr_val){.tt = CTR_TF64, .val.f64 = (ctr_f64)rhs.val.i64}; break;
                    default: return call_err(CTR_ERRV_TYPE_MISMATCH, "Unknown Type", NULL);
                }
            }
            switch (lhs.tt) {
                case CTR_TF64:
                    ctr_set(state, ctr_iabc_a(ins), (ctr_val){.tt = CTR_TF64, .val.f64 = lhs.val.f64 - rhs.val.f64});
                    break;
                case CTR_TI64:
                    ctr_set(state, ctr_iabc_a(ins), (ctr_val){.tt = CTR_TI64, .val.i64 = lhs.val.i64 - rhs.val.i64});
                    break;
                default: return call_err(CTR_ERRV_TYPE_MISMATCH, "Cannot perform arithmetic on nil.", NULL); break;
            }
            DISPATCH();
        }
        CASE(CTR_OP_MUL) {
            ctr_val lhs = ctr_get(state, ctr_iabc_b(ins));
            ctr_val rhs = ctr_get(state, ctr_iabc_c(ins));

            if (lhs.tt == CTR_TDYN || rhs.tt == CTR_TDYN)
                return call_err(CTR_ERRV_TYPE_MISMATCH, "Cannot convert dynamic object and primitive.", NULL);
            if (lhs.tt != rhs.tt) {
                switch (lhs.tt) {
                    case CTR_TI64: rhs = (ctr_val){.tt = CTR_TI64, .val.i64 = (ctr_i64)rhs.val.f64}; break;
                    case CTR_TF64: rhs = (ctr_val){.tt = CTR_TF64, .val.f64 = (ctr_f64)rhs.val.i64}; break;
                    default: return call_err(CTR_ERRV_TYPE_MISMATCH, "Unknown Type", NULL);
                }
            }
            switch (lhs.tt) {
                case CTR_TF64:
                    ctr_set(state, ctr_iabc_a(ins), (ctr_val){.tt = CTR_TF64, .val.f64 = lhs.val.f64 * rhs.val.f64});
                    break;
                case CTR_TI64:
                    ctr_set(state, ctr_iabc_a(ins), (ctr_val){.tt = CTR_TI64, .val.i64 = lhs.val.i64 * rhs.val.i64});
                    break;
                default: return call_err(CTR_ERRV_TYPE_MISMATCH, "Cannot perform arithmetic on nil.", NULL); break;
            }
            DISPATCH();
        }
        CASE(CTR_OP_DIV) {
            ctr_val lhs = ctr_get(state, ctr_iabc_b(ins));
            ctr_val rhs = ctr_get(state, ctr_iabc_c(ins));

            if (lhs.tt == CTR_TDYN || rhs.tt == CTR_TDYN)
                return call_err(CTR_ERRV_TYPE_MISMATCH, "Cannot convert dynamic object and primitive.", NULL);
            if (lhs.tt != rhs.tt) {
                switch (lhs.tt) {
                    case CTR_TI64: rhs = (ctr_val){.tt = CTR_TI64, .val.i64 = (ctr_i64)rhs.val.f64}; break;
                    case CTR_TF64: rhs = (ctr_val){.tt = CTR_TF64, .val.f64 = (ctr_f64)rhs.val.i64}; break;
                    default: return call_err(CTR_ERRV_TYPE_MISMATCH, "Unknown Type", NULL);
                }
            }
            switch (lhs.tt) {
                case CTR_TF64:
                    ctr_set(state, ctr_iabc_a(ins), (ctr_val){.tt = CTR_TF64, .val.f64 = lhs.val.f64 / rhs.val.f64});
                    break;
                case CTR_TI64:
                    ctr_set(state, ctr_iabc_a(ins), (ctr_val){.tt = CTR_TI64, .val.i64 = lhs.val.i64 / rhs.val.i64});
                    break;
                default: return call_err(CTR_ERRV_TYPE_MISMATCH, "Cannot perform arithmetic on nil.", NULL); break;
            }
            DISPATCH();
        }

        CASE(CTR_OP_EQ) {
            bool inv = ctr_iabc_a(ins) != 0;
            ctr_val lhs = ctr_get(state, ctr_iabc_b(ins));
            ctr_val rhs = ctr_get(state, ctr_iabc_c(ins));
            if (lhs.tt == CTR_TNIL && rhs.tt == CTR_TNIL) {
                if (!inv) pc++;
                DISPATCH();
            }

            if (lhs.tt != rhs.tt) {
                if (lhs.tt == CTR_TDYN || rhs.tt == CTR_TDYN || lhs.tt == CTR_TNIL || rhs.tt == CTR_TNIL) {
                    if (inv) pc++;
                    DISPATCH();
                }
                switch (lhs.tt) {
                    case CTR_TI64: rhs = (ctr_val){.tt = CTR_TI64, .val.i64 = (ctr_i64)rhs.val.f64}; break;
                    case CTR_TF64: rhs = (ctr_val){.tt = CTR_TF64, .val.f64 = (ctr_f64)rhs.val.i64}; break;
                    default: return call_err(CTR_ERRV_TYPE_MISMATCH, "Unknown Type", NULL);
                }
            }

            bool e = false;
            switch (lhs.tt) {
                case CTR_TI64: e = lhs.val.i64 == rhs.val.i64; break;
                case CTR_TF64: e = lhs.val.f64 == rhs.val.f64; break;

                case CTR_TDYN: {
                    ctr_dheader *h1 = ctr_header(lhs);
                    ctr_dheader *h2 = ctr_header(lhs);
                    if (h1->tt != h2->tt) {
                        e = false;
                        break;
                    }
                    switch (h1->tt) {
                        case CTR_DSTR: e = sf_str_eq(*(sf_str *)lhs.val.dyn, *(sf_str *)rhs.val.dyn); break;
                        case CTR_DOBJ: e = lhs.val.dyn == rhs.val.dyn; break;
                        case CTR_DFUN: e = *(void **)lhs.val.dyn == *(void **)rhs.val.dyn; break;
                        default: return call_err(CTR_ERRV_TYPE_MISMATCH, "Unknown Type", NULL);
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
            if (lhs.tt == CTR_TDYN || rhs.tt == CTR_TDYN || lhs.tt == CTR_TNIL || rhs.tt == CTR_TNIL) {
                if (inv) pc++;
                DISPATCH();
            }
            if (lhs.tt != rhs.tt) {
                switch (lhs.tt) {
                    case CTR_TI64: rhs = (ctr_val){.tt = CTR_TI64, .val.i64 = (ctr_i64)rhs.val.f64}; break;
                    case CTR_TF64: rhs = (ctr_val){.tt = CTR_TF64, .val.f64 = (ctr_f64)rhs.val.i64}; break;
                    default: break;
                }
            }
            bool e = false;
            switch (lhs.tt) {
                case CTR_TI64: e = lhs.val.i64 < rhs.val.i64; break;
                case CTR_TF64: e = lhs.val.f64 < rhs.val.f64; break;
                default: break;
            }

            e = inv ? !e : e;
            if (e) pc++;
            DISPATCH();
        }
        CASE(CTR_OP_LE) {bool inv = ctr_iabc_a(ins) != 0;
            ctr_val lhs = ctr_get(state, ctr_iabc_b(ins));
            ctr_val rhs = ctr_get(state, ctr_iabc_c(ins));
            if (lhs.tt == CTR_TNIL && rhs.tt == CTR_TNIL) {
                if (!inv) pc++;
                DISPATCH();
            }

            if (lhs.tt != rhs.tt) {
                if (lhs.tt == CTR_TDYN || rhs.tt == CTR_TDYN || lhs.tt == CTR_TNIL || rhs.tt == CTR_TNIL) {
                    if (inv) pc++;
                    DISPATCH();
                }
                switch (lhs.tt) {
                    case CTR_TI64: rhs = (ctr_val){.tt = CTR_TI64, .val.i64 = (ctr_i64)rhs.val.f64}; break;
                    case CTR_TF64: rhs = (ctr_val){.tt = CTR_TF64, .val.f64 = (ctr_f64)rhs.val.i64}; break;
                    default: break;
                }
            }

            bool e = false;
            switch (lhs.tt) {
                case CTR_TI64: e = lhs.val.i64 <= rhs.val.i64; break;
                case CTR_TF64: e = lhs.val.f64 <= rhs.val.f64; break;

                case CTR_TDYN: {
                    ctr_dheader *h1 = ctr_header(lhs);
                    ctr_dheader *h2 = ctr_header(lhs);
                    if (h1->tt != h2->tt) {
                        e = false;
                        break;
                    }
                    switch (h1->tt) {
                        case CTR_DSTR: e = sf_str_cmp(*(sf_str *)lhs.val.dyn, *(sf_str *)rhs.val.dyn); break;
                        case CTR_DOBJ: e = lhs.val.dyn == rhs.val.dyn; break;
                        case CTR_DFUN: e = *(void **)lhs.val.dyn == *(void **)rhs.val.dyn; break;
                        default: return call_err(CTR_ERRV_TYPE_MISMATCH, "Unknown Type", NULL);
                    }
                }
                default: break;
            }

            e = inv ? !e : e;
            if (e) pc++;
            DISPATCH();
        }

        CASE(CTR_OP_UP_SET) {
            ctr_val v = ctr_get(state, ctr_iab_b(ins));
            ctr_upvalue *upv = proto->upvals + ctr_iab_a(ins);
            if (upv->tt == CTR_UP_VAL)
                upv->inner.value = ctr_dref(v);
            else ctr_rawset(state, upv->inner.ref, ctr_dref(v), (uint32_t)((int64_t)(state->frame_c - 1) + upv->frame_o));
            DISPATCH();
        }
        CASE(CTR_OP_UP_GET) {
            ctr_upvalue *upv = proto->upvals + ctr_iab_b(ins);
            if (upv->tt == CTR_UP_VAL)
                ctr_set(state, ctr_iab_a(ins), ctr_dref(upv->inner.value));
            else ctr_set(state, ctr_iab_a(ins), ctr_dref(ctr_rawget(state, upv->inner.ref, (uint32_t)((int64_t)(state->frame_c - 1) + upv->frame_o))));
            DISPATCH();
        }
        CASE(CTR_OP_UP_REF) {
            ctr_val v = ctr_get(state, (uint32_t)ctr_ia_a(ins));
            if (v.tt == CTR_TDYN && ctr_header(v)->tt == CTR_DVAL) {
                ctr_dref(v);
                DISPATCH();
            }
            ctr_val vref = ctr_dnew(CTR_DVAL);
            *(ctr_val *)vref.val.dyn = v;
            ctr_set(state, (uint32_t)ctr_ia_a(ins), vref);
            DISPATCH();
        }

        CASE(CTR_OP_OBJ_NEW) {
            ctr_val v = ctr_dnew(CTR_DOBJ);
            ctr_set(state, (uint32_t)ctr_ia_a(ins), v);
            DISPATCH();
        }
        CASE(CTR_OP_OBJ_SET) {
            ctr_val obj = ctr_get(state, ctr_iabc_a(ins));
            ctr_val key = ctr_get(state, ctr_iabc_b(ins));
            ctr_val val = ctr_get(state, ctr_iabc_c(ins));
            if (obj.tt != CTR_TDYN || ctr_header(obj)->tt != CTR_DOBJ)
                return call_err(CTR_ERRV_TYPE_MISMATCH, "Expected object at [%d], found %s.", ctr_iabc_a(ins), ctr_typename(obj).c_str);
            if (key.tt != CTR_TDYN || ctr_header(key)->tt != CTR_DSTR)
                return call_err(CTR_ERRV_TYPE_MISMATCH, "Expected str at [%d], found %s.", ctr_iabc_b(ins), ctr_typename(key).c_str);
            ctr_dobj_ex ex = ctr_dobj_get((ctr_dobj *)obj.val.dyn, *(sf_str *)key.val.dyn);
            if (ex.is_ok && ex.value.ok.tt == CTR_TDYN)
                ctr_ddel(ex.value.ok);
            ctr_dobj_set((ctr_dobj *)obj.val.dyn, sf_str_dup(*(sf_str *)key.val.dyn), ctr_dref(val));
            DISPATCH();
        }
        CASE(CTR_OP_OBJ_GET) {
            ctr_val obj = ctr_get(state, ctr_iabc_b(ins));
            ctr_val key = ctr_get(state, ctr_iabc_c(ins));
            if (obj.tt != CTR_TDYN || ctr_header(obj)->tt != CTR_DOBJ)
                return call_err(CTR_ERRV_TYPE_MISMATCH, "Expected object at [%d], found %s.", ctr_iabc_b(ins), ctr_typename(obj).c_str);
            if (key.tt != CTR_TDYN || ctr_header(key)->tt != CTR_DSTR)
                return call_err(CTR_ERRV_TYPE_MISMATCH, "Expected str at [%d], found %s.", ctr_iabc_c(ins), ctr_typename(key).c_str);
            ctr_dobj_ex ex = ctr_dobj_get((ctr_dobj *)obj.val.dyn, *(sf_str *)key.val.dyn);
            if (!ex.is_ok)
                return call_err(CTR_ERRV_TYPE_MISMATCH, "Object [%d], does not contain member '%s'.", ctr_iabc_a(ins), ((sf_str *)key.val.dyn)->c_str);
            ctr_set(state, ctr_iabc_a(ins), ctr_dref(ex.value.ok));
            DISPATCH();
        }

        CASE(CTR_OP_STR_FROM) {
            ctr_val in = ctr_get(state, ctr_iab_b(ins));
            ctr_val str = ctr_dnew(CTR_DSTR);
            *(sf_str *)str.val.dyn = ctr_tostring(in);
            ctr_set(state, ctr_iab_a(ins), str);
            DISPATCH();
        }
        CASE(CTR_OP_STR_ECHO) {
            ctr_val val = ctr_get(state, (uint32_t)ctr_ia_a(ins));
            if (val.tt != CTR_TDYN || ctr_header(val)->tt != CTR_DSTR)
                return call_err(CTR_ERRV_TYPE_MISMATCH, "Expected str at [%d], found %s.", ctr_iabc_a(ins), ctr_typename(val));
            printf("%s", ((sf_str *)val.val.dyn)->c_str);
            DISPATCH();
        }

        CASE(CTR_OP_DBG_DUMP) {
            ctr_stackdump(state);
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
    state->frames = realloc(state->frames, --state->frame_c * sizeof(ctr_stackframe));
    return ctr_call_ex_ok(return_val);
}

ctr_call_ex ctr_call(ctr_state *state, ctr_proto *proto, const ctr_val *args) {
    if (proto->tt == CTR_PROTO_BC)
        return ctr_call_bc(state, proto, args);
    return ctr_call_cfun(state, proto, args);
}

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic pop
#endif
