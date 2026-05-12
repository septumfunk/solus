#include "solus/compiler.h"
#include "solus/syntax.h"

static solu_val _solu_push_tprim(solu_dalloc *alloc, sf_str name, solu_tinfo info) {
    while (alloc && alloc->next)
        alloc = alloc->next;
    if (!alloc) return SOLU_NIL;
    solu_dalloc *da = malloc(sizeof(solu_dalloc) + sizeof(solu_tinfo));
    alloc->next = da;

    *da = (solu_dalloc) {
        NULL,
        sizeof(solu_tinfo),
        SOLU_DUSR,
        SOLU_DYN_WHITE,
        true, SOLU_NIL, {SOLU_NIL}
    };

    solu_tinfo *ti = (solu_tinfo *)(da + 1);
    *ti = info;
    ti->name = solu_cstr(alloc, name);
    return (solu_val){SOLU_TDYN, .dyn=ti};
}

static inline solu_val solu_push_tprim(solu_dalloc *alloc, sf_str name, solu_opcode cast) {
    return _solu_push_tprim(alloc, name, (solu_tinfo){ .tt = SOLU_TI_PRIM, .cast = cast });
}

static inline solu_val solu_push_tfun(solu_dalloc *alloc, sf_str name, solu_type *args, uint32_t arg_c, solu_type return_t, bool variadic) {
    solu_tinfo ti = {
        .tt = SOLU_TI_FUN,
        .fun = {args, arg_c, return_t, variadic}
    };
    return _solu_push_tprim(alloc, name, ti);
}

solu_val solu_push_tdef(solu_dalloc *alloc, sf_str name, bool complex) {
    solu_tinfo ti = {
        .tt = SOLU_TI_DEF,
        .complex = complex,
        .def = solu_def_new(),
    };
    return _solu_push_tprim(alloc, name, ti);
}

solu_type solu_typeof(solu_compiler *c, solu_node *node);
/// Used to resolve a type NAME, not the type of a variable
solu_type solu_tres(solu_compiler *c, solu_node *type) {
    if (type->tt == SOLU_ND_TYPE) {
        solu_valmap_ex ex = solu_valmap_get(&c->tenv->types, sf_ref(type->n_type.name.dyn));
        if (!ex.is_ok) return solu_type_err();

        bool err = type->n_type.err;
        if (ex.ok.dyn == c->ptypes[SOLU_CERR].dyn) err = true;
        return solu_type_ok(ex.ok, type->n_type.nil, err);
    }
    if (type->tt == SOLU_ND_SIG) {
        sf_str sig = sf_str_fmt("(");
        uint32_t fixed_arg_c = type->n_sig.arg_c - type->n_sig.variadic;
        solu_type *types = fixed_arg_c ?
            malloc(fixed_arg_c * sizeof(solu_type)) : NULL;
        for (uint32_t i = 0; i < fixed_arg_c; ++i) {
            solu_type t = solu_tres(c, type->n_sig.args[i]);
            if (!t.ok) {
                free(types);
                return solu_type_err();
            }
            types[i] = t;

            sf_str base = sf_ref(((solu_tinfo *)t.base.dyn)->name.dyn);
            if (t.nilable && t.base.dyn != c->ptypes[SOLU_CNIL].dyn) {
                sf_str tmp = sf_str_fmt("%s?", base.c_str);
                sf_str_free(base);
                base = tmp;
            }
            if (t.fallible && t.base.dyn != c->ptypes[SOLU_CERR].dyn) {
                sf_str tmp = sf_str_fmt("%s!", base.c_str);
                sf_str_free(base);
                base = tmp;
            }
            sf_str_append(&sig, base);
            sf_str_free(base);
            if (i != fixed_arg_c - 1)
                sf_str_append(&sig, sf_lit(", "));
        }
        sf_str_append(&sig, sf_lit(")"));

        solu_type return_t = (solu_type){c->ptypes[SOLU_CANY], true, true, true};
        if (type->n_sig.return_t != NULL) {
            return_t = solu_tres(c, type->n_sig.return_t);
            if (!return_t.ok) { free(types); return solu_type_err(); }
        }
        if (return_t.base.dyn != c->ptypes[SOLU_CANY].dyn) {
            sf_str_append(&sig, sf_lit(" -> "));
            sf_str base = sf_ref(((solu_tinfo *)return_t.base.dyn)->name.dyn);
            if (return_t.nilable) {
                sf_str tmp = sf_str_fmt("%s?", base.c_str);
                sf_str_free(base);
                base = tmp;
            }
            if (return_t.fallible) {
                sf_str tmp = sf_str_fmt("%s!", base.c_str);
                sf_str_free(base);
                base = tmp;
            }
            sf_str_append(&sig, base);
            sf_str_free(base);
        }

        solu_valmap_ex ex = solu_valmap_get(&c->tenv->types, sig);
        if (!ex.is_ok) {
            solu_val t = solu_push_tfun(c->alloc, sig, types, type->n_sig.arg_c, return_t, type->n_sig.variadic);
            solu_valmap_set(&c->tenv->types, sig, t);
            return solu_type_ok(t, type->n_sig.nil, type->n_sig.err);
        }
        sf_str_free(sig);
        return solu_type_ok(ex.ok, type->n_sig.nil, type->n_sig.err);
    }
    return solu_type_err();
}

static solu_val solu_postfix_key(solu_node *node) {
    if (node->tt == SOLU_ND_LITERAL && node->n_literal.tt == SOLU_TDYN)
        return node->n_literal;
    if (node->tt == SOLU_ND_IDENTIFIER)
        return node->n_identifier;
    return SOLU_NIL;
}

solu_type solu_typeof(solu_compiler *c, solu_node *node) {
    switch (node->tt) {
        case SOLU_ND_CAST: {
            solu_valmap_ex ex = solu_valmap_get(&c->tenv->types, sf_ref(node->n_cast.type.dyn));
            if (!ex.is_ok) return solu_type_err();
            return solu_type_ok(ex.ok, false, false);
        }
        case SOLU_ND_POSTFIX: {
            if (node->n_postfix.op == TK_PERIOD || node->n_postfix.op == TK_LEFT_BRACKET) {
                solu_type type = solu_typeof(c, node->n_postfix.expr);
                if (!type.ok) return solu_type_err();
                solu_val base = type.base;
                if (base.dyn == c->ptypes[SOLU_CANY].dyn) return solu_type_ok(c->ptypes[SOLU_CANY], true, false);

                // Complex
                solu_tinfo *ti = base.dyn;
                if (ti->tt != SOLU_TI_DEF)
                    return solu_type_ok(c->ptypes[SOLU_CANY], true, false);

                solu_val key = solu_postfix_key(node->n_postfix.postfix);
                if (key.tt != SOLU_TDYN)
                    return solu_type_ok(c->ptypes[SOLU_CANY], true, false);

                solu_def_ex ex = solu_def_get(&ti->def, sf_ref(key.dyn));
                if (!ex.is_ok) return solu_type_err();
                return ex.ok;
            } else { // ?
                solu_type typeof = solu_typeof(c, node->n_postfix.expr);
                if (!typeof.ok) return solu_type_err();
                return solu_type_ok(typeof.base, false, false);
            }
        }
        case SOLU_ND_BINARY: {
            if (node->n_binary.op == TK_AS) { // Reinterpret
                char *tname = node->n_binary.right->tt == SOLU_ND_LITERAL ? "nil" : node->n_binary.right->n_identifier.dyn;
                solu_valmap_ex ex = solu_valmap_get(&c->tenv->types, sf_ref(tname));
                if (!ex.is_ok) return solu_type_err();
                return solu_type_ok(ex.ok, true, false);
            }
            if (solu_niscondition(node))
                return solu_type_ok(c->ptypes[SOLU_CBOOL], false, false);

            solu_type to = solu_typeof(c, node->n_binary.left);
            if (!to.ok) return solu_type_err();
            solu_type tr = solu_typeof(c, node->n_binary.right);
            if (!tr.ok) return solu_type_err();

            if (to.base.dyn == c->ptypes[SOLU_CI64].dyn // f64 promotion
                && tr.base.dyn == c->ptypes[SOLU_CF64].dyn) {
                to = solu_type_ok(c->ptypes[SOLU_CF64], to.nilable || tr.nilable, to.fallible || tr.fallible);
            }
            return to;
        }
        case SOLU_ND_LITERAL: {
            switch (node->n_literal.tt) {
                case SOLU_TCOUNT: return solu_type_ok(c->ptypes[SOLU_CANY], false, false);
                case SOLU_TNIL:   return solu_type_ok(c->ptypes[SOLU_CNIL], true, false);
                case SOLU_TI64:   return solu_type_ok(c->ptypes[SOLU_CI64], false, false);
                case SOLU_TF64:   return solu_type_ok(c->ptypes[SOLU_CF64], false, false);
                case SOLU_TBOOL:  return solu_type_ok(c->ptypes[SOLU_CBOOL], false, false);
                case SOLU_TDYN:   return solu_type_ok(c->ptypes[SOLU_CSTR], false, false);
            }
        }
        case SOLU_ND_OBJ: return solu_type_ok(c->ptypes[SOLU_COBJ], false, false);
        case SOLU_ND_IDENTIFIER: {
            solu_local loc;
            if (solu_lexists(c, node->n_identifier.dyn, &loc)) {
                return loc.type;
            }
            solu_def_ex ex = solu_def_get(&c->tenv->global->def, sf_ref(node->n_identifier.dyn));
            if (ex.is_ok) return ex.ok;
            return solu_type_ok(c->ptypes[SOLU_CANY], false, false);
        }
        case SOLU_ND_CALL: {
            solu_type type = solu_typeof(c, node->n_call.identifier);
            if (!type.ok) return solu_type_err();
            if (type.base.dyn == c->ptypes[SOLU_CANY].dyn)
                return type;

            solu_tinfo *t = type.base.dyn;
            if (t->tt != SOLU_TI_FUN) // TODO: handle this better
                return solu_type_err();

            return t->fun.return_t;
        }
        case SOLU_ND_RETURN: return solu_typeof(c, node->n_return.expr);
        case SOLU_ND_FUN:
        case SOLU_ND_ASM: return solu_tres(c, node->n_fun.sig);
        case SOLU_ND_TYPEOF: return solu_type_ok(c->ptypes[SOLU_CSTR], false, false);

        default: return solu_type_ok(c->ptypes[SOLU_CANY], false, false);
    }
}

sf_str solu_stypename(solu_compiler *c, solu_type t) {
    solu_tinfo *ti = t.base.dyn;
    sf_str base = sf_ref(ti->name.dyn);
    if (t.nilable && t.base.dyn != c->ptypes[SOLU_CNIL].dyn) {
        sf_str _t = sf_str_fmt(ti->tt == SOLU_TI_FUN ? "(%s)?" : "%s?", base.c_str);
        sf_str_free(base);
        base = _t;
    }
    if (t.fallible && t.base.dyn != c->ptypes[SOLU_CERR].dyn) {
        sf_str _t = sf_str_fmt(ti->tt == SOLU_TI_FUN ? (t.nilable ? "%s!" : "(%s)!") : "%s!", base.c_str);
        sf_str_free(base);
        base = _t;
    }
    return base;
}

bool solu_tassignable(solu_compiler *c, solu_type dst, solu_type src) {
    if (!dst.ok || !src.ok)
        return false;
    bool base_ok =
        dst.base.dyn == c->ptypes[SOLU_CANY].dyn ||
        dst.base.dyn == src.base.dyn ||
        (dst.nilable && src.base.dyn == c->ptypes[SOLU_CNIL].dyn) ||
        (dst.fallible && src.base.dyn == c->ptypes[SOLU_CERR].dyn);
    if (!base_ok)
        return false;
    if (src.nilable && !dst.nilable)
        return false;
    if (src.fallible && !dst.fallible)
        return false;
    return true;
}
