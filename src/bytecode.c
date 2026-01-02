#include "ctr/bytecode.h"
#include "sf/gfx/window.h"
#include "sf/str.h"
#include <stdlib.h>

#define MAP_NAME ctr_pp
#define MAP_K sf_str
#define MAP_V sf_str
#define EQUAL_FN sf_str_eq
#define HASH_FN sf_str_hash
#include <sf/containers/map.h>
#define MAP_NAME ctr_cmap
#define MAP_K sf_str
#define MAP_V ctr_i64
#define EQUAL_FN sf_str_eq
#define HASH_FN sf_str_hash
#include <sf/containers/map.h>
#define MAP_NAME ctr_opmap
#define MAP_K sf_str
#define MAP_V const ctr_inssig *
#define EQUAL_FN sf_str_eq
#define HASH_FN sf_str_hash
#include <sf/containers/map.h>

void _dobj_foreach(void *_u, sf_str k, ctr_val _v) { (void)_u;(void)_v; sf_str_free(k); }
void _ctr_dobj_cleanup(ctr_dobj *obj) {
    ctr_dobj_foreach(obj, _dobj_foreach, NULL);
}

ctr_fproto ctr_fproto_new(void) {
    return (ctr_fproto){
        .tt = CTR_FPROTO_BC,
        .code = NULL,
        .code_s = 0,
        .reg_c = 0,
        .arg_c = 0,
        .entry = 0,
        .constants = ctr_valvec_new(),
        .upvals = NULL,
    };
}

ctr_fproto ctr_fproto_c(ctr_cfunction c_fun, uint32_t arg_c, uint32_t temp_c) {
    return (ctr_fproto){
        .tt = CTR_FPROTO_C,
        .c_fun = c_fun,
        .code_s = 0,
        .reg_c = arg_c + temp_c,
        .arg_c = arg_c,
        .entry = 0,
        .constants = ctr_valvec_new(),
        .upvals = NULL,
    };
}

void ctr_fproto_free(ctr_fproto *proto) {
    if (proto->tt == CTR_FPROTO_BC && proto->code) {
        free(proto->code);
        if (proto->dbg) free(proto->dbg);
    }
    proto->code = NULL;
    proto->c_fun = NULL;
    ctr_valvec_free(&proto->constants);
    if (proto->upvals) {
        for (uint32_t i = 0; i < proto->up_c; ++i)
            sf_str_free(proto->upvals[i].name);
        free(proto->upvals);
    }
    proto->up_c = 0;
    proto->reg_c = 0;
}


ctr_val ctr_dnew(ctr_dtype tt) {
    size_t size;
    switch (tt) {
        case CTR_DSTR: size = sizeof(sf_str); break;
        case CTR_DERR: size = sizeof(sf_str); break;
        case CTR_DOBJ: size = sizeof(ctr_dobj); break;
        case CTR_DARRAY: size = sizeof(ctr_valvec); break;
        case CTR_DFUN: size = sizeof(ctr_fproto); break;
        case CTR_DREF: size = sizeof(ctr_val); break;

        case CTR_DUSR:
        case CTR_DCOUNT: return CTR_NIL;
    }

    ctr_dyn p = calloc(1, sizeof(ctr_dheader) + size);
    *(ctr_dheader *)p = (ctr_dheader){
        .size = size,
        .is_const = false,
        .tt = tt,
        .rc = 1,
    };
    p = (char *)p + sizeof(ctr_dheader);

    switch (tt) {
        case CTR_DSTR:
        case CTR_DERR: *(sf_str *)p = SF_STR_EMPTY; break;
        case CTR_DOBJ: *(ctr_dobj *)p = ctr_dobj_new(); break;
        case CTR_DARRAY: *(ctr_valvec *)p = ctr_valvec_new(); break;
        case CTR_DFUN: *(ctr_fproto *)p = ctr_fproto_new(); break;
        case CTR_DREF: *(ctr_val *)p = CTR_NIL; break;

        case CTR_DUSR:
        case CTR_DCOUNT: return CTR_NIL;
    }
    return (ctr_val){ .tt = CTR_TDYN, .dyn = p };
}

ctr_val ctr_dnewusr(size_t size, sf_str name, void *value, ctr_usrdel del, ctr_usrtostring tostring) {
    ctr_dyn p = calloc(1, sizeof(ctr_dheader) + sizeof(ctr_usrwrap) + size);

    *(ctr_dheader *)p = (ctr_dheader){
        .size = size + sizeof(ctr_usrwrap),
        .is_const = false,
        .tt = CTR_DUSR,
        .rc = 1,
    };
    p = (char *)p + sizeof(ctr_dheader);

    ctr_usrwrap *w = p;
    *w = (ctr_usrwrap){
        name,
        del,
        tostring,
    };
    p = (char *)p + sizeof(ctr_usrwrap);

    memcpy(p, value, size);
    return (ctr_val){ .tt = CTR_TDYN, .dyn = w };
}

void ctr_ddel(ctr_val val) {
    ctr_dheader *dh = ctr_header(val);
    if (dh && !dh->is_const && --dh->rc == 0) {
        switch (dh->tt) {
            case CTR_DSTR:
            case CTR_DERR: sf_str_free(*(sf_str *)val.dyn); break;
            case CTR_DOBJ: ctr_dobj_free(val.dyn); break;
            case CTR_DARRAY: ctr_valvec_free(val.dyn); break;
            case CTR_DREF: ctr_ddel(*(ctr_val *)val.dyn); break;
            case CTR_DFUN: ctr_fproto_free((ctr_fproto *)val.dyn); break;

            case CTR_DUSR: {
                ctr_usrwrap *w = ctr_uheader(val);
                sf_str_free(w->name);
                sf_window *ww = ctr_uptr(val); (void)ww;
                if (w->del)
                    w->del(ctr_uptr(val));
                break;
            }
            case CTR_DCOUNT: break;
        }
        free(dh);
    }
}

const sf_str CTR_ERR_STRINGS[CTR_ERR_COUNT] = {
#define X(prefix, name, string) sf_lit(string),
#include "ctr/error.def"
#undef X
};

const ctr_inssig CTR_OP_INFO[CTR_OP_COUNT] = {
    [CTR_OP_LOAD] = {
        .opcode = CTR_OP_LOAD,
        .mnemonic = "LOAD",
        .type = CTR_INS_AB,
    },
    [CTR_OP_MOVE] = {
        .opcode = CTR_OP_MOVE,
        .mnemonic = "MOVE",
        .type = CTR_INS_AB,
    },
    [CTR_OP_RET] = {
        .opcode = CTR_OP_RET,
        .mnemonic = "RET",
        .type = CTR_INS_A,
    },
    [CTR_OP_JMP] = {
        .opcode = CTR_OP_JMP,
        .mnemonic = "JMP",
        .type = CTR_INS_A,
    },
    [CTR_OP_CALL] = {
        .opcode = CTR_OP_CALL,
        .mnemonic = "CALL",
        .type = CTR_INS_ABC,
    },

    [CTR_OP_ADD] = {
        .opcode = CTR_OP_ADD,
        .mnemonic = "ADD",
        .type = CTR_INS_ABC,
    },
    [CTR_OP_SUB] = {
        .opcode = CTR_OP_SUB,
        .mnemonic = "SUB",
        .type = CTR_INS_ABC,
    },
    [CTR_OP_DIV] = {
        .opcode = CTR_OP_DIV,
        .mnemonic = "DIV",
        .type = CTR_INS_ABC,
    },
    [CTR_OP_MUL] = {
        .opcode = CTR_OP_MUL,
        .mnemonic = "MUL",
        .type = CTR_INS_ABC,
    },

    [CTR_OP_EQ] = {
        .opcode = CTR_OP_EQ,
        .mnemonic = "EQ",
        .type = CTR_INS_ABC,
    },
    [CTR_OP_LT] = {
        .opcode = CTR_OP_LT,
        .mnemonic = "LT",
        .type = CTR_INS_ABC,
    },
    [CTR_OP_LE] = {
        .opcode = CTR_OP_LE,
        .mnemonic = "LE",
        .type = CTR_INS_ABC,
    },

    [CTR_OP_SETU] = {
        .opcode = CTR_OP_SETU,
        .mnemonic = "SETU",
        .type = CTR_INS_AB,
    },
    [CTR_OP_GETU] = {
        .opcode = CTR_OP_GETU,
        .mnemonic = "GETU",
        .type = CTR_INS_AB,
    },
    [CTR_OP_REFU] = {
        .opcode = CTR_OP_REFU,
        .mnemonic = "REFU",
        .type = CTR_INS_A,
    },

    [CTR_OP_SET] = {
        .opcode = CTR_OP_SET,
        .mnemonic = "SET",
        .type = CTR_INS_ABC,
    },
    [CTR_OP_GET] = {
        .opcode = CTR_OP_GET,
        .mnemonic = "GET",
        .type = CTR_INS_ABC,
    },

    [CTR_OP_SUPO] = {
        .opcode = CTR_OP_SUPO,
        .mnemonic = "SUPO",
        .type = CTR_INS_ABC,
    },
    [CTR_OP_GUPO] = {
        .opcode = CTR_OP_GUPO,
        .mnemonic = "GUPO",
        .type = CTR_INS_ABC,
    },

    [CTR_OP_UNKNOWN] = {
        .opcode = CTR_OP_UNKNOWN,
        .mnemonic = "???",
    }
};

const sf_str CTR_TYPE_NAMES[(size_t)CTR_TCOUNT + (size_t)CTR_DCOUNT] = {
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
