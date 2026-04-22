#include "solus/val.h"
#include <math.h>
#include <string.h>

void _valmap_foreach(void *_u, sf_str k, solu_val _v) { (void)_u;(void)_v; sf_str_free(k); }
void _solu_valmap_cleanup(solu_valmap *map) {
    solu_valmap_foreach(map, _valmap_foreach, NULL);
}

void _strcache_foreach(void *_u, sf_str k, solu_dalloc *_v) { (void)_u;(void)_v; sf_str_free(k); }
void _solu_strcache_cleanup(solu_strcache *obj) {
    solu_strcache_foreach(obj, _strcache_foreach, NULL);
}

solu_dobj solu_dobj_new(void) {
    return (solu_dobj){
        solu_valmap_new(),
        solu_valvec_new(),
    };
}
void solu_dobj_free(solu_dobj *obj) {
    solu_valmap_free(&obj->map);
    solu_valvec_free(&obj->array);
}
solu_val solu_dobj_strget(solu_dobj *obj, char *key) {
    solu_valmap_ex ex = solu_valmap_get(&obj->map, sf_ref(key));
    solu_dalloc *da = (solu_dalloc *)obj - 1;
    if (!ex.is_ok && da->metadata[SOLU_META_EXTEND].tt != SOLU_TNIL) {
        ex.is_ok = true;
        ex.ok = solu_dobj_strget(da->metadata[SOLU_META_EXTEND].dyn, key);
    }
    return ex.is_ok ? ex.ok : SOLU_NIL;
}
void solu_dobj_strset(solu_dobj *obj, char *key, solu_val val) {
    solu_valmap_set(&obj->map, sf_str_cdup(key), val);
}

void solu_usemeta(solu_val obj, solu_dobj *meta) {
    if (obj.tt != SOLU_TDYN) return;
    solu_dalloc *da = solu_dheader(obj);
    solu_val oe = da->metadata[SOLU_META_EXTEND];
    memset(&da->metadata, 0, SOLU_META_COUNT * sizeof(solu_val));
    da->meta = (solu_val){SOLU_TDYN, .dyn=meta};

    da->metadata[SOLU_META_GET] = solu_dobj_strget(meta, "_get");
    da->metadata[SOLU_META_GET] = solu_isdtype(da->metadata[SOLU_META_GET], SOLU_DFUN) ? da->metadata[SOLU_META_GET] : SOLU_NIL;

    da->metadata[SOLU_META_SET] = solu_dobj_strget(meta, "_set");
    da->metadata[SOLU_META_SET] = solu_isdtype(da->metadata[SOLU_META_SET], SOLU_DFUN) ? da->metadata[SOLU_META_SET] : SOLU_NIL;

    da->metadata[SOLU_META_CALL] = solu_dobj_strget(meta, "_call");
    da->metadata[SOLU_META_CALL] = solu_isdtype(da->metadata[SOLU_META_CALL], SOLU_DFUN) ? da->metadata[SOLU_META_CALL] : SOLU_NIL;

    da->metadata[SOLU_META_STR] = solu_dobj_strget(meta, "_str");
    da->metadata[SOLU_META_STR] = solu_isdtype(da->metadata[SOLU_META_STR], SOLU_DFUN) ? da->metadata[SOLU_META_STR] : SOLU_NIL;

    da->metadata[SOLU_META_EXTEND] = solu_dobj_strget(meta, "_extend");
    da->metadata[SOLU_META_EXTEND] = solu_isdtype(da->metadata[SOLU_META_EXTEND], SOLU_DOBJ) ? da->metadata[SOLU_META_EXTEND] : oe;

    da->metadata[SOLU_META_ADD] = solu_dobj_strget(meta, "_add");
    da->metadata[SOLU_META_ADD] = solu_isdtype(da->metadata[SOLU_META_ADD], SOLU_DFUN) ? da->metadata[SOLU_META_ADD] : SOLU_NIL;
    da->metadata[SOLU_META_SUB] = solu_dobj_strget(meta, "_sub");
    da->metadata[SOLU_META_SUB] = solu_isdtype(da->metadata[SOLU_META_SUB], SOLU_DFUN) ? da->metadata[SOLU_META_SUB] : SOLU_NIL;
    da->metadata[SOLU_META_MUL] = solu_dobj_strget(meta, "_mul");
    da->metadata[SOLU_META_MUL] = solu_isdtype(da->metadata[SOLU_META_MUL], SOLU_DFUN) ? da->metadata[SOLU_META_MUL] : SOLU_NIL;
    da->metadata[SOLU_META_DIV] = solu_dobj_strget(meta, "_div");
    da->metadata[SOLU_META_DIV] = solu_isdtype(da->metadata[SOLU_META_DIV], SOLU_DFUN) ? da->metadata[SOLU_META_DIV] : SOLU_NIL;
    da->metadata[SOLU_META_EQ] = solu_dobj_strget(meta, "_eq");
    da->metadata[SOLU_META_EQ] = solu_isdtype(da->metadata[SOLU_META_EQ], SOLU_DFUN) ? da->metadata[SOLU_META_EQ] : SOLU_NIL;
    da->metadata[SOLU_META_NEG] = solu_dobj_strget(meta, "_neg");
    da->metadata[SOLU_META_NEG] = solu_isdtype(da->metadata[SOLU_META_NEG], SOLU_DFUN) ? da->metadata[SOLU_META_NEG] : SOLU_NIL;
}

solu_fproto solu_fproto_new(void) {
    return (solu_fproto){
        .tt = SOLU_FPROTO_BC,
        .code = NULL,
        .dbg = NULL,
        .code_c = 0,
        .reg_c = 0,
        .arg_c = 0,
        .dbg_res = 0, .dbg_ll = 0,
        .file_name = SF_STR_EMPTY,
        .constants = solu_valvec_new(),
        .upvals = NULL,
    };
}

solu_fproto solu_fproto_c(solu_cfunction c_fun, uint32_t arg_c, solu_val *captures, uint32_t cap_c) {
    solu_upvalue *upc = cap_c ? malloc(sizeof(solu_upvalue) * cap_c) : NULL;
    for (uint32_t i = 0; i < cap_c; ++i)
        upc[i] = (solu_upvalue){
            sf_lit("C"),
            SOLU_UP_VAL,
            .value = captures[i],
        };
    return (solu_fproto){
        .tt = SOLU_FPROTO_C,
        .c_fun = c_fun,
        .reg_c = arg_c,
        .arg_c = arg_c,
        .constants = solu_valvec_new(),
        .upvals = upc,
        .up_c = cap_c,
    };
}

void solu_fproto_free(solu_fproto *proto) {
    sf_str_free(proto->file_name);
    if (proto->tt == SOLU_FPROTO_BC && proto->code) {
        free(proto->code);
        if (proto->dbg) free(proto->dbg);
    }
    proto->code = NULL;
    proto->dbg = NULL;
    proto->c_fun = NULL;
    for (solu_val *v = proto->constants.data; v && v < proto->constants.data + proto->constants.count; ++v)
        solu_dclean(*v);
    solu_valvec_free(&proto->constants);
    if (proto->upvals) {
        for (uint32_t i = 0; i < proto->up_c; ++i)
            sf_str_free(proto->upvals[i].name);
        free(proto->upvals);
    }
    proto->up_c = 0;
    proto->reg_c = 0;
}

void solu_dclean(solu_val val) {
    solu_dalloc *dh = solu_dheader(val);
    if (!dh) return;
    switch (dh->tt) {
        case SOLU_DSTR:
        case SOLU_DERR: break;
        case SOLU_DOBJ: solu_dobj_free(val.dyn); break;
        case SOLU_DFUN: solu_fproto_free((solu_fproto *)val.dyn); break;
        case SOLU_DUSR: {
            solu_usrwrap *uh = solu_uheader(val);
            if (uh->del) uh->del(val.dyn);
        }
        default: break;
    }
    free(dh);
}

bool solu_truthy(solu_val val) {
    switch (val.tt) {
        case SOLU_TBOOL: return val.boolean;
        case SOLU_TI64: return val.i64 != 0;
        case SOLU_TF64: return !isnan(val.f64);
        case SOLU_TDYN: return val.dyn;
        default: return false;
    }
}

bool solu_strict_eq(solu_val lhs, solu_val rhs) {
    if (lhs.tt != rhs.tt) return false;
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
    return e;
}

sf_str solu_dasmf(solu_fproto *p) {
    sf_str final = SF_STR_EMPTY;
    for (uint32_t pc = 0; pc < p->code_c; ++pc) {
        sf_str bc = solu_dasmi(p->code[pc]);
        uint16_t line = SOLU_DBG_LINE(p->dbg[pc]), column = SOLU_DBG_COL(p->dbg[pc]);
        sf_str f = sf_str_fmt("%.2u:%-6.2u%s\n", line, column, bc.c_str);
        sf_str_free(bc);
        if (sf_isempty(final))
            final = f;
        else {
            sf_str_append(&final, f);
            sf_str_free(f);
        }
    }
    return final;
}

const char *SOLU_TYPE_NAMES[(size_t)SOLU_TCOUNT + (size_t)SOLU_DCOUNT] = {
    "nil",
    "f64",
    "i64",
    "bool",
    "dyn",

    "str",
    "err",
    "obj",
    "fun",
    "ref",

    "usr",
};
