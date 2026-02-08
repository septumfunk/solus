#include "solus/bytecode.h"
#include "sf/containers/buffer.h"
#include "sf/str.h"
#include <stdlib.h>

void _valmap_foreach(void *_u, sf_str k, solu_val _v) { (void)_u;(void)_v; sf_str_free(k); }
void _solu_valmap_cleanup(solu_valmap *map) {
    solu_valmap_foreach(map, _valmap_foreach, NULL);
}

void _strcache_foreach(void *_u, sf_str k, solu_dalloc *_v) { (void)_u;(void)_v; sf_str_free(k); }
void _solu_strcache_cleanup(solu_strcache *obj) {
    solu_strcache_foreach(obj, _strcache_foreach, NULL);
}

char *solu_tostring(solu_val val) {
    switch (val.tt) {
        case SOLU_TNIL: return _strdup("nil");
        case SOLU_TF64: return sf_str_fmt("%.10f", val.f64).c_str;
        case SOLU_TI64: return sf_str_fmt("%lld", val.i64).c_str;
        case SOLU_TBOOL: return _strdup(val.boolean ? "true" : "false");
        case SOLU_TDYN: {
            switch (solu_dheader(val)->tt) {
                case SOLU_DSTR:
                case SOLU_DERR:
                return _strdup(val.dyn); break;
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

solu_dobj solu_dobj_new(void) {
    return (solu_dobj){
        solu_valmap_new(),
        solu_valvec_new(),
        SOLU_NIL,
        .metafuns = {
            [SOLU_META_GET] = false,
            [SOLU_META_SET] = false,
        }
    };
}
void solu_dobj_free(solu_dobj *obj) {
    solu_valmap_free(&obj->map);
    solu_valvec_free(&obj->array);
}
solu_val solu_dobj_get(solu_dobj *obj, solu_val key) {
    if ((key.tt == SOLU_TI64 && key.i64 >= 0) || (key.tt == SOLU_TF64 && key.f64 >= 0)) {
        uint32_t nkey = (uint32_t)(key.tt == SOLU_TI64 ? key.i64 : (solu_i64)key.f64);
        if (obj->array.count == 0 || nkey > obj->array.count - 1)
            return SOLU_NIL;
        return solu_valvec_get(&obj->array, nkey);
    }
    char *nkey = solu_isdtype(key, SOLU_DSTR) ? key.dyn : solu_tostring(key);
    solu_valmap_ex ex = solu_valmap_get(&obj->map, sf_ref(nkey));
    if (!solu_isdtype(key, SOLU_DSTR))
        free(nkey);
    return ex.is_ok ? ex.ok : SOLU_NIL;
}
void solu_dobj_set(solu_dobj *obj, solu_val key, solu_val val) {
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
    char *nkey = solu_tostring(key);
    solu_valmap_set(&obj->map, sf_own(nkey), val);
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

solu_fproto solu_fproto_c(solu_cfunction c_fun, uint32_t arg_c, uint32_t temp_c) {
    return (solu_fproto){
        .tt = SOLU_FPROTO_C,
        .c_fun = c_fun,
        .reg_c = arg_c + temp_c,
        .arg_c = arg_c,
        .constants = solu_valvec_new(),
        .upvals = NULL,
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

char *solu_realdir(const char *rp) {
    if (!rp) return NULL;

    char out[4096];
    size_t len = strlen(rp);
    if (len == 0)
        return _strdup(".");
    if (len >= sizeof(out))
        return NULL;
    memcpy(out, rp, len + 1);

    while (len > 0 && (out[len - 1] == '/' || out[len - 1] == '\\')) {
        if (len == 1 && (out[0] == '/' || out[0] == '\\'))
            break;
    #ifdef _WIN32
        if (len == 3 && out[1] == ':' &&
            (out[2] == '/' || out[2] == '\\'))
            break;
    #endif
        out[--len] = '\0';
    }

    char *last_slash = NULL;
    for (char *p = out; *p; p++)
        if (*p == '/' || *p == '\\')
            last_slash = p;
    if (!last_slash)
        return _strdup(".");
    if (last_slash == out) {
        out[1] = '\0';
        return _strdup(out);
    }

#ifdef _WIN32
    if (last_slash == out + 2 && out[1] == ':') {
        out[3] = out[2];
        out[2] = '\0';
        return _strdup(out);
    }
#endif

    *last_slash = '\0';
    return _strdup(out);
}

static sf_str solu_try_realpath(const char *_cwd, sf_str p) {
    sf_str rp = sf_own(solu_realpath(p.c_str));
    if (rp.c_str) return rp;

    if (_cwd) {
        sf_str cwd = sf_str_cdup(_cwd);
        sf_str_append(&cwd,
            #if defined(_WIN32) || defined(_WIN64)
            sf_lit("\\")
            #else
            sf_lit("/")
            #endif
        );
        sf_str_append(&cwd, p);
        rp = sf_own(solu_realpath(cwd.c_str));
        sf_str_free(cwd);
    }

    return rp;
}

char *solu_findfile(const char *cwd, const char *rel_path) {
    if (!rel_path || !*rel_path) return NULL;

    size_t len = strlen(rel_path);
    int has_ext = (len >= 5 && memcmp(rel_path + len - 5, ".solu",  5) == 0) ||
        (len >= 5 && memcmp(rel_path + len - 5, ".solc",  5) == 0) ||
        (len >= 6 && memcmp(rel_path + len - 6, ".solus",  6) == 0);

    sf_str base = sf_str_cdup(rel_path);

    sf_str rp0 = solu_try_realpath(cwd, base);
    if (rp0.c_str) { sf_str_free(base); return rp0.c_str; }
    if (!has_ext) {
        sf_str p1 = sf_str_dup(base);
        sf_str_append(&p1, sf_lit(".solu"));

        sf_str rp1 = solu_try_realpath(cwd, p1);
        sf_str_free(p1);
        if (rp1.c_str) { sf_str_free(base); return rp1.c_str; }

        sf_str p2 = sf_str_dup(base);
        sf_str_append(&p2, sf_lit(".solus"));

        sf_str rp2 = solu_try_realpath(cwd, p2);
        sf_str_free(p2);
        if (rp2.c_str) { sf_str_free(base); return rp2.c_str; }

        sf_str p3 = sf_str_dup(base);
        sf_str_append(&p3, sf_lit(".solc"));

        sf_str rp3 = solu_try_realpath(cwd, p3);
        sf_str_free(p3);
        if (rp3.c_str) { sf_str_free(base); return rp3.c_str; }
    }

    sf_str_free(base);
    return NULL;
}

sf_str solu_dasmi(solu_instruction ins) {
    const char *op = solu_op_info(solu_ins_op(ins))->mnemonic;
    switch (solu_op_info(solu_ins_op(ins))->type) {
        default:
        case SOLU_INS_A: return sf_str_fmt("%-7s%-8d", op, solu_ia_a(ins));
        case SOLU_INS_AB: return sf_str_fmt("%-7s%-4u%-4u",  op, solu_iab_a(ins), solu_iab_b(ins)); break;
        case SOLU_INS_ABC: return sf_str_fmt("%-7s%-4u%-4u%-4u",  op, solu_iabc_a(ins), solu_iabc_bx(ins), solu_iabc_cx(ins)); break;
    }
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

const char *SOLU_ERR_STRINGS[SOLU_ERR_COUNT] = {
#define X(prefix, name, string) string,
#include "solus/error.def"
#undef X
};

const solu_inssig SOLU_OP_INFO[SOLU_OP_COUNT] = {
    [SOLU_OP_LOAD] = {
        .opcode = SOLU_OP_LOAD,
        .mnemonic = "LOAD",
        .type = SOLU_INS_AB,
    },
    [SOLU_OP_MOVE] = {
        .opcode = SOLU_OP_MOVE,
        .mnemonic = "MOVE",
        .type = SOLU_INS_AB,
    },
    [SOLU_OP_RET] = {
        .opcode = SOLU_OP_RET,
        .mnemonic = "RET",
        .type = SOLU_INS_A,
    },
    [SOLU_OP_JMP] = {
        .opcode = SOLU_OP_JMP,
        .mnemonic = "JMP",
        .type = SOLU_INS_A,
    },
    [SOLU_OP_CALL] = {
        .opcode = SOLU_OP_CALL,
        .mnemonic = "CALL",
        .type = SOLU_INS_ABC,
    },

    [SOLU_OP_ADD] = {
        .opcode = SOLU_OP_ADD,
        .mnemonic = "ADD",
        .type = SOLU_INS_ABC,
    },
    [SOLU_OP_SUB] = {
        .opcode = SOLU_OP_SUB,
        .mnemonic = "SUB",
        .type = SOLU_INS_ABC,
    },
    [SOLU_OP_DIV] = {
        .opcode = SOLU_OP_DIV,
        .mnemonic = "DIV",
        .type = SOLU_INS_ABC,
    },
    [SOLU_OP_MUL] = {
        .opcode = SOLU_OP_MUL,
        .mnemonic = "MUL",
        .type = SOLU_INS_ABC,
    },

    [SOLU_OP_NEG] = {
        .opcode = SOLU_OP_NEG,
        .mnemonic = "NEG",
        .type = SOLU_INS_AB,
    },
    [SOLU_OP_EQ] = {
        .opcode = SOLU_OP_EQ,
        .mnemonic = "EQ",
        .type = SOLU_INS_ABC,
    },
    [SOLU_OP_LT] = {
        .opcode = SOLU_OP_LT,
        .mnemonic = "LT",
        .type = SOLU_INS_ABC,
    },
    [SOLU_OP_LE] = {
        .opcode = SOLU_OP_LE,
        .mnemonic = "LE",
        .type = SOLU_INS_ABC,
    },

    [SOLU_OP_SETU] = {
        .opcode = SOLU_OP_SETU,
        .mnemonic = "SETU",
        .type = SOLU_INS_AB,
    },
    [SOLU_OP_GETU] = {
        .opcode = SOLU_OP_GETU,
        .mnemonic = "GETU",
        .type = SOLU_INS_AB,
    },
    [SOLU_OP_REFU] = {
        .opcode = SOLU_OP_REFU,
        .mnemonic = "REFU",
        .type = SOLU_INS_A,
    },

    [SOLU_OP_NEW] = {
        .opcode = SOLU_OP_NEW,
        .mnemonic = "NEW",
        .type = SOLU_INS_A,
    },
    [SOLU_OP_SET] = {
        .opcode = SOLU_OP_SET,
        .mnemonic = "SET",
        .type = SOLU_INS_ABC,
    },
    [SOLU_OP_GET] = {
        .opcode = SOLU_OP_GET,
        .mnemonic = "GET",
        .type = SOLU_INS_ABC,
    },
    [SOLU_OP_PUSH] = {
        .opcode = SOLU_OP_PUSH,
        .mnemonic = "PUSH",
        .type = SOLU_INS_AB,
    },

    [SOLU_OP_SUPO] = {
        .opcode = SOLU_OP_SUPO,
        .mnemonic = "SUPO",
        .type = SOLU_INS_ABC,
    },
    [SOLU_OP_GUPO] = {
        .opcode = SOLU_OP_GUPO,
        .mnemonic = "GUPO",
        .type = SOLU_INS_ABC,
    },

    [SOLU_OP_UNKNOWN] = {
        .opcode = SOLU_OP_UNKNOWN,
        .mnemonic = "???",
    }
};

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
