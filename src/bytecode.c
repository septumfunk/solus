#include "sol/bytecode.h"
#include "sf/str.h"
#include <stdlib.h>

#define MAP_NAME sol_pp
#define MAP_K sf_str
#define MAP_V sf_str
#define EQUAL_FN sf_str_eq
#define HASH_FN sf_str_hash
#include <sf/containers/map.h>
#define MAP_NAME sol_cmap
#define MAP_K sf_str
#define MAP_V sol_i64
#define EQUAL_FN sf_str_eq
#define HASH_FN sf_str_hash
#include <sf/containers/map.h>
#define MAP_NAME sol_opmap
#define MAP_K sf_str
#define MAP_V const sol_inssig *
#define EQUAL_FN sf_str_eq
#define HASH_FN sf_str_hash
#include <sf/containers/map.h>

void _dobj_foreach(void *_u, sf_str k, sol_val _v) { (void)_u;(void)_v; sf_str_free(k); }
void _sol_dobj_cleanup(sol_dobj *obj) {
    sol_dobj_foreach(obj, _dobj_foreach, NULL);
}

sol_fproto sol_fproto_new(void) {
    return (sol_fproto){
        .tt = SOL_FPROTO_BC,
        .code = NULL,
        .code_c = 0,
        .reg_c = 0,
        .arg_c = 0,
        .entry = 0,
        .dbg_res = 0, .dbg_ll = 0,
        .file_name = SF_STR_EMPTY,
        .constants = sol_valvec_new(),
        .upvals = NULL,
    };
}

sol_fproto sol_fproto_c(sol_cfunction c_fun, uint32_t arg_c, uint32_t temp_c) {
    return (sol_fproto){
        .tt = SOL_FPROTO_C,
        .c_fun = c_fun,
        .reg_c = arg_c + temp_c,
        .arg_c = arg_c,
        .entry = 0,
        .constants = sol_valvec_new(),
        .upvals = NULL,
    };
}

void sol_fproto_free(sol_fproto *proto) {
    if (proto->tt == SOL_FPROTO_BC && proto->code) {
        free(proto->code);
        if (proto->dbg) free(proto->dbg);
    }
    proto->code = NULL;
    proto->c_fun = NULL;
    for (sol_val *v = proto->constants.data; v && v < proto->constants.data + proto->constants.count; ++v)
        sol_dclean(*v);
    sol_valvec_free(&proto->constants);
    if (proto->upvals) {
        for (uint32_t i = 0; i < proto->up_c; ++i)
            sf_str_free(proto->upvals[i].name);
        free(proto->upvals);
    }
    proto->up_c = 0;
    proto->reg_c = 0;
}

void sol_dclean(sol_val val) {
    sol_dalloc *dh = sol_dheader(val);
    if (!dh) return;
    switch (dh->tt) {
        case SOL_DSTR:
        case SOL_DERR: sf_str_free(*(sf_str *)val.dyn); break;
        case SOL_DOBJ: sol_dobj_free(val.dyn); break;
        case SOL_DARRAY: sol_valvec_free(val.dyn); break;
        case SOL_DFUN: sol_fproto_free((sol_fproto *)val.dyn); break;
        default: break;
    }
    free(dh);
}

const char *SOL_ERR_STRINGS[SOL_ERR_COUNT] = {
#define X(prefix, name, string) string,
#include "sol/error.def"
#undef X
};

const sol_inssig SOL_OP_INFO[SOL_OP_COUNT] = {
    [SOL_OP_LOAD] = {
        .opcode = SOL_OP_LOAD,
        .mnemonic = "LOAD",
        .type = SOL_INS_AB,
    },
    [SOL_OP_MOVE] = {
        .opcode = SOL_OP_MOVE,
        .mnemonic = "MOVE",
        .type = SOL_INS_AB,
    },
    [SOL_OP_RET] = {
        .opcode = SOL_OP_RET,
        .mnemonic = "RET",
        .type = SOL_INS_A,
    },
    [SOL_OP_JMP] = {
        .opcode = SOL_OP_JMP,
        .mnemonic = "JMP",
        .type = SOL_INS_A,
    },
    [SOL_OP_CALL] = {
        .opcode = SOL_OP_CALL,
        .mnemonic = "CALL",
        .type = SOL_INS_ABC,
    },

    [SOL_OP_ADD] = {
        .opcode = SOL_OP_ADD,
        .mnemonic = "ADD",
        .type = SOL_INS_ABC,
    },
    [SOL_OP_SUB] = {
        .opcode = SOL_OP_SUB,
        .mnemonic = "SUB",
        .type = SOL_INS_ABC,
    },
    [SOL_OP_DIV] = {
        .opcode = SOL_OP_DIV,
        .mnemonic = "DIV",
        .type = SOL_INS_ABC,
    },
    [SOL_OP_MUL] = {
        .opcode = SOL_OP_MUL,
        .mnemonic = "MUL",
        .type = SOL_INS_ABC,
    },

    [SOL_OP_EQ] = {
        .opcode = SOL_OP_EQ,
        .mnemonic = "EQ",
        .type = SOL_INS_ABC,
    },
    [SOL_OP_LT] = {
        .opcode = SOL_OP_LT,
        .mnemonic = "LT",
        .type = SOL_INS_ABC,
    },
    [SOL_OP_LE] = {
        .opcode = SOL_OP_LE,
        .mnemonic = "LE",
        .type = SOL_INS_ABC,
    },

    [SOL_OP_SETU] = {
        .opcode = SOL_OP_SETU,
        .mnemonic = "SETU",
        .type = SOL_INS_AB,
    },
    [SOL_OP_GETU] = {
        .opcode = SOL_OP_GETU,
        .mnemonic = "GETU",
        .type = SOL_INS_AB,
    },
    [SOL_OP_REFU] = {
        .opcode = SOL_OP_REFU,
        .mnemonic = "REFU",
        .type = SOL_INS_A,
    },

    [SOL_OP_SET] = {
        .opcode = SOL_OP_SET,
        .mnemonic = "SET",
        .type = SOL_INS_ABC,
    },
    [SOL_OP_GET] = {
        .opcode = SOL_OP_GET,
        .mnemonic = "GET",
        .type = SOL_INS_ABC,
    },

    [SOL_OP_SUPO] = {
        .opcode = SOL_OP_SUPO,
        .mnemonic = "SUPO",
        .type = SOL_INS_ABC,
    },
    [SOL_OP_GUPO] = {
        .opcode = SOL_OP_GUPO,
        .mnemonic = "GUPO",
        .type = SOL_INS_ABC,
    },

    [SOL_OP_UNKNOWN] = {
        .opcode = SOL_OP_UNKNOWN,
        .mnemonic = "???",
    }
};

const char *SOL_TYPE_NAMES[(size_t)SOL_TCOUNT + (size_t)SOL_DCOUNT] = {
    "nil",
    "f64",
    "i64",
    "bool",
    "dyn",

    "str",
    "err",
    "obj",
    "array",
    "fun",
    "ref",

    "usr",
};

sf_str sol_dasmi(sol_instruction ins) {
    const char *op = sol_op_info(sol_ins_op(ins))->mnemonic;
    switch (sol_op_info(sol_ins_op(ins))->type) {
        default:
        case SOL_INS_A: return sf_str_fmt("%-7s%-8d", op, sol_ia_a(ins));
        case SOL_INS_AB: return sf_str_fmt("%-7s%-4u%-4u",  op, sol_iab_a(ins), sol_iab_b(ins)); break;
        case SOL_INS_ABC: return sf_str_fmt("%-7s%-4u%-4u%-4u",  op, sol_iabc_a(ins), sol_iabc_b(ins), sol_iabc_c(ins)); break;
    }
}

sf_str sol_dasmp(sol_fproto *p) {
    sf_str final = SF_STR_EMPTY;
    for (uint32_t pc = 0; pc < p->code_c; ++pc) {
        sf_str bc = sol_dasmi(p->code[pc]);
        uint16_t line = SOL_DBG_LINE(p->dbg[pc]), column = SOL_DBG_COL(p->dbg[pc]);
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
