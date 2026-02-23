#include "solus/val.h"
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
        SOLU_NIL,
        .metafuns = {
            [SOLU_META_GET] = SOLU_NIL,
            [SOLU_META_SET] = SOLU_NIL,
            [SOLU_META_CALL] = SOLU_NIL,
            [SOLU_META_STR] = SOLU_NIL,
        }
    };
}
void solu_dobj_free(solu_dobj *obj) {
    solu_valmap_free(&obj->map);
    solu_valvec_free(&obj->array);
}
solu_val solu_dobj_strget(solu_dobj *obj, char *key) {
    solu_valmap_ex ex = solu_valmap_get(&obj->map, sf_ref(key));
    return ex.is_ok ? ex.ok : SOLU_NIL;
}
void solu_dobj_strset(solu_dobj *obj, char *key, solu_val val) {
    solu_valmap_set(&obj->map, sf_str_cdup(key), val);
}

solu_fproto solu_fproto_new(void) {
    return (solu_fproto){
        .tt = SOLU_FPROTO_BC,
        .code = NULL,
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
    if (proto->tt == SOLU_FPROTO_BC && proto->code) {
        free(proto->code);
        if (proto->dbg) free(proto->dbg);
    }
    proto->code = NULL;
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
        default: break;
    }
    free(dh);
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
