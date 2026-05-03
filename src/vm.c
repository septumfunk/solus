#include "solus/api.h"

/// Push a stack frame to the VM
uint32_t solu_pushframe(solu_state *state, uint32_t reg_c) {
    solu_frames_push(&state->frames, (solu_stackframe){
        state->frames.count == 0 ? 0 : state->frames.data[state->frames.count - 1].bottom_o + state->frames.data[state->frames.count - 1].size,
        reg_c,
        false,
    });
    for (uint32_t i = 0; i < reg_c; ++i)
        solu_valvec_push(&state->stack, SOLU_NIL);
    return state->frames.count - 1;
}
/// Pop the top stack frame from the VM
void solu_popframe(solu_state *state) {
    solu_stackframe f = solu_frames_pop(&state->frames);
    for (uint32_t i = 0; i < f.size; ++i)
        solu_valvec_pop(&state->stack);
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
        if (s->pc >= proto->code_c) goto ret; /* EOF */\
        ins = proto->code[s->pc]; /* Read next instruction */\
        \
        ++s->pc; \
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

#define solu_callerr(en, fmt, ...) (solu_call_ex_err((solu_call_err){.tt=(en),.panic=sf_str_fmt((fmt), __VA_ARGS__).c_str, .trace=&s->trace, .pc=s->pc-1}))

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

solu_call_ex solu_call_bc(solu_state *s, solu_fproto *proto, const solu_val *args, uint32_t arg_c) {
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

    s->pc = 0;
    solu_instruction ins;
    solu_pushframe(s, proto->reg_c);
    for (uint32_t i = 0; i < proto->arg_c && args && i < arg_c; ++i)
        solu_set(s, i, args[i]);
    if (proto->variadic) {
        solu_val extra = solu_dnew(s, SOLU_DOBJ);
        solu_set(s, proto->arg_c, extra);
        for (uint32_t i = proto->arg_c; i < arg_c; ++i)
            solu_valvec_push(&((solu_dobj *)extra.dyn)->array, args[i]);
    }

    solu_val return_val = SOLU_NIL;

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
            s->pc = (uint32_t)((int32_t)s->pc + solu_ia_a(ins));
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

            if (!fex.is_ok)
                return fex;

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
                    solu_call_ex ex = solu_call(s, get.dyn, (solu_val[]){obj, key}, 2);
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

            if (!fex.is_ok)
                return fex;

            solu_set(s, solu_iabc_a(ins), fex.ok);
            DISPATCH();
        }

        CASE(SOLU_OP_ADD) {
            uint32_t a = solu_iabc_a(ins), b = solu_iabc_bx(ins); (void)a;(void)b;
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
                    solu_set(s, solu_iabc_a(ins), (solu_val){.tt = SOLU_TI64,
                        .i64 = (solu_i64)((uint64_t)lhs.i64 + (uint64_t)rhs.i64)
                    });
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
                (solu_val){.tt = SOLU_TI64, .i64 = (solu_i64)((uint64_t)lhs.i64 - (uint64_t)rhs.i64)} :
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
                (solu_val){.tt = SOLU_TI64, .i64 = (solu_i64)((uint64_t)lhs.i64 * (uint64_t)rhs.i64)} :
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
            if (lhs.tt == SOLU_TI64) {
                if (rhs.i64 == 0)
                    return solu_callerr(SOLU_ERRV_PANIC, "Division by zero", NULL);
                solu_set(s, solu_iabc_a(ins), (solu_val){
                    .tt = SOLU_TI64,
                    .i64 = rhs.i64 == -1
                        ? (solu_i64)(0ULL - (uint64_t)lhs.i64)
                        : lhs.i64 / rhs.i64
                });
                DISPATCH();
            }
            solu_set(s, solu_iabc_a(ins), (solu_val){.tt = SOLU_TF64, .f64 = lhs.f64 / rhs.f64});
            DISPATCH();
        }

        CASE(SOLU_OP_NEG) {
            solu_val in = solu_get(s, solu_iab_b(ins));
            switch (in.tt) {
                case SOLU_TI64:
                    in.i64 = (solu_i64)(0ULL - (uint64_t)in.i64);
                    break;
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
                    __attribute__((fallthrough));
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
                    if (inv ? !e : e) ++s->pc;
                    DISPATCH();
                }
            }

            if ((lhs.tt == SOLU_TNIL && rhs.tt == SOLU_TNIL)) {
                if (!inv) ++s->pc;
                DISPATCH();
            }
            if (lhs.tt == SOLU_TBOOL && rhs.tt != SOLU_TNIL && rhs.tt != SOLU_TBOOL) {
                if (inv ? !lhs.boolean : lhs.boolean) ++s->pc;
                DISPATCH();
            }
            if (lhs.tt != SOLU_TNIL && lhs.tt != SOLU_TBOOL && rhs.tt == SOLU_TBOOL) {
                if (inv ? !rhs.boolean : rhs.boolean) ++s->pc;
                DISPATCH();
            }

            if (lhs.tt != rhs.tt) {
                if (lhs.tt == SOLU_TDYN || rhs.tt == SOLU_TDYN || lhs.tt == SOLU_TNIL || rhs.tt == SOLU_TNIL) {
                    if (inv) ++s->pc;
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
                        default: e = lhs.dyn == rhs.dyn; break;
                    }
                }
                default: break;
            }

            e = inv ? !e : e;
            if (e) ++s->pc;
            DISPATCH();
        }
        CASE(SOLU_OP_LT) {
            bool inv = solu_iabc_a(ins) != 0;
            solu_val lhs = solu_iabc_bk(ins) ? solu_getk(s, proto, solu_iabc_bx(ins)) : solu_get(s, solu_iabc_bx(ins));
            solu_val rhs = solu_iabc_ck(ins) ? solu_getk(s, proto, solu_iabc_cx(ins)) : solu_get(s, solu_iabc_cx(ins));
            if (lhs.tt == SOLU_TDYN || rhs.tt == SOLU_TDYN || lhs.tt == SOLU_TNIL || rhs.tt == SOLU_TNIL ||
                lhs.tt == SOLU_TBOOL || rhs.tt == SOLU_TBOOL) {
                if (inv) ++s->pc;
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
            if (e) ++s->pc;
            DISPATCH();
        }
        CASE(SOLU_OP_LE) {bool inv = solu_iabc_a(ins) != 0;
            solu_val lhs = solu_iabc_bk(ins) ? solu_getk(s, proto, solu_iabc_bx(ins)) : solu_get(s, solu_iabc_bx(ins));
            solu_val rhs = solu_iabc_ck(ins) ? solu_getk(s, proto, solu_iabc_cx(ins)) : solu_get(s, solu_iabc_cx(ins));
            if (lhs.tt == SOLU_TDYN || rhs.tt == SOLU_TDYN || lhs.tt == SOLU_TNIL || rhs.tt == SOLU_TNIL ||
                lhs.tt == SOLU_TBOOL || rhs.tt == SOLU_TBOOL) {
                if (inv) ++s->pc;
                DISPATCH();
            }

            if (lhs.tt != rhs.tt) {
                if (lhs.tt == SOLU_TDYN || rhs.tt == SOLU_TDYN || lhs.tt == SOLU_TNIL || rhs.tt == SOLU_TNIL) {
                    if (inv) ++s->pc;
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
            if (e) ++s->pc;
            DISPATCH();
        }

        CASE(SOLU_OP_SETU) {
            if (!proto->upvals)
                return solu_callerr(SOLU_ERRV_CORRUPT, "Corrupt Bytecode", NULL);
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
                return solu_callerr(SOLU_ERRV_CORRUPT, "Corrupt Bytecode", NULL);
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
                solu_call_ex ex = solu_call(s, get.dyn, (solu_val[]){obj, key}, 2);
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
    solu_popframe(s);
    return solu_call_ex_ok(return_val);
}

#if (defined(__GNUC__) || defined(__clang__)) && !defined(SOLU_DBG_NOCOMPUTE)
#pragma GCC diagnostic pop
#endif
