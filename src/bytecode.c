#include "solus/bytecode.h"
#include "sf/str.h"
#include <stdlib.h>

void _dobj_foreach(void *_u, sf_str k, solu_val _v) { (void)_u;(void)_v; sf_str_free(k); }
void _solu_dobj_cleanup(solu_dobj *obj) {
    solu_dobj_foreach(obj, _dobj_foreach, NULL);
}

void _strcache_foreach(void *_u, sf_str k, solu_dalloc *_v) { (void)_u;(void)_v; sf_str_free(k); }
void _solu_strcache_cleanup(solu_strcache *obj) {
    solu_strcache_foreach(obj, _strcache_foreach, NULL);
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
