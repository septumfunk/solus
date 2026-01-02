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
        .code_s = 0,
        .reg_c = 0,
        .arg_c = 0,
        .entry = 0,
        .constants = sol_valvec_new(),
        .upvals = NULL,
    };
}

sol_fproto sol_fproto_c(sol_cfunction c_fun, uint32_t arg_c, uint32_t temp_c) {
    return (sol_fproto){
        .tt = SOL_FPROTO_C,
        .c_fun = c_fun,
        .code_s = 0,
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
    sol_valvec_free(&proto->constants);
    if (proto->upvals) {
        for (uint32_t i = 0; i < proto->up_c; ++i)
            sf_str_free(proto->upvals[i].name);
        free(proto->upvals);
    }
    proto->up_c = 0;
    proto->reg_c = 0;
}


sol_val sol_dnew(sol_dtype tt) {
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

    sol_dyn p = calloc(1, sizeof(sol_dheader) + size);
    *(sol_dheader *)p = (sol_dheader){
        .size = size,
        .is_const = false,
        .tt = tt,
        .rc = 1,
    };
    p = (char *)p + sizeof(sol_dheader);

    switch (tt) {
        case SOL_DSTR:
        case SOL_DERR: *(sf_str *)p = SF_STR_EMPTY; break;
        case SOL_DOBJ: *(sol_dobj *)p = sol_dobj_new(); break;
        case SOL_DARRAY: *(sol_valvec *)p = sol_valvec_new(); break;
        case SOL_DFUN: *(sol_fproto *)p = sol_fproto_new(); break;
        case SOL_DREF: *(sol_val *)p = SOL_NIL; break;

        case SOL_DUSR:
        case SOL_DCOUNT: return SOL_NIL;
    }
    return (sol_val){ .tt = SOL_TDYN, .dyn = p };
}

sol_val sol_dnewusr(size_t size, sf_str name, void *value, sol_usrdel del, sol_usrtostring tostring) {
    sol_dyn p = calloc(1, sizeof(sol_dheader) + sizeof(sol_usrwrap) + size);

    *(sol_dheader *)p = (sol_dheader){
        .size = size + sizeof(sol_usrwrap),
        .is_const = false,
        .tt = SOL_DUSR,
        .rc = 1,
    };
    p = (char *)p + sizeof(sol_dheader);

    sol_usrwrap *w = p;
    *w = (sol_usrwrap){
        name,
        del,
        tostring,
    };
    p = (char *)p + sizeof(sol_usrwrap);

    memcpy(p, value, size);
    return (sol_val){ .tt = SOL_TDYN, .dyn = w };
}

void sol_ddel(sol_val val) {
    sol_dheader *dh = sol_header(val);
    if (dh && !dh->is_const && --dh->rc == 0) {
        switch (dh->tt) {
            case SOL_DSTR:
            case SOL_DERR: sf_str_free(*(sf_str *)val.dyn); break;
            case SOL_DOBJ: sol_dobj_free(val.dyn); break;
            case SOL_DARRAY: sol_valvec_free(val.dyn); break;
            case SOL_DREF: sol_ddel(*(sol_val *)val.dyn); break;
            case SOL_DFUN: sol_fproto_free((sol_fproto *)val.dyn); break;

            case SOL_DUSR: {
                sol_usrwrap *w = sol_uheader(val);
                sf_str_free(w->name);
                if (w->del)
                    w->del(sol_uptr(val));
                break;
            }
            case SOL_DCOUNT: break;
        }
        free(dh);
    }
}

const sf_str SOL_ERR_STRINGS[SOL_ERR_COUNT] = {
#define X(prefix, name, string) sf_lit(string),
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

const sf_str SOL_TYPE_NAMES[(size_t)SOL_TCOUNT + (size_t)SOL_DCOUNT] = {
    sf_lit("nil"),
    sf_lit("f64"),
    sf_lit("i64"),
    sf_lit("bool"),
    sf_lit("dyn"),

    sf_lit("str"),
    sf_lit("err"),
    sf_lit("obj"),
    sf_lit("array"),
    sf_lit("fun"),
    sf_lit("ref"),

    sf_lit("usr"),
};
