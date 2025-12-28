#ifndef BYTECODE_H
#define BYTECODE_H

#include "sf/str.h"
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

// crash team racing
#define CTR_BYTECODE_HEADER { 'C', 'T', 'R' }
// The great switch for debug output
//#define CTR_DEBUG_LOG

typedef enum {
    CTR_INS_A, // A: i26 (jmp)
    CTR_INS_AB, // A: u8, B: u18 (load)
    CTR_INS_ABC, // A: u8, B: u9, C: u9
} ctr_instype;
typedef uint32_t ctr_operand;

typedef enum {
    CTR_OP_LOAD,
    CTR_OP_MOVE,
    CTR_OP_RET,
    CTR_OP_JMP,
    CTR_OP_CALL,

    CTR_OP_ADD,
    CTR_OP_SUB,
    CTR_OP_MUL,
    CTR_OP_DIV,

    CTR_OP_EQ,
    CTR_OP_LT,
    CTR_OP_LE,

    CTR_OP_UP_SET,
    CTR_OP_UP_GET,
    CTR_OP_UP_REF,

    CTR_OP_OBJ_NEW,
    CTR_OP_OBJ_SET,
    CTR_OP_OBJ_GET,

    CTR_OP_STR_FROM,
    CTR_OP_STR_ECHO,

    CTR_OP_DBG_DUMP,

    CTR_OP_UNKNOWN,
    CTR_OP_COUNT,
} ctr_opcode;
typedef uint32_t ctr_instruction;


#define MASKI(n) ((1U<<(n))-1U)
#define MAXARG_A ((1 << 25) - 1)
#define ctr_ins_op(i) ((i >> 26U) & MASKI(6U))

#define ctr_ins_a_ec(a) ((uint32_t)((a) + MAXARG_A))
#define ctr_ins_a_dc(a)  ((int32_t)((a) & MASKI(26U)) - MAXARG_A)

#define ctr_ins_a(op, as) \
  (((uint32_t)(op) & MASKI(6U)) << 26U | (ctr_ins_a_ec(as) & MASKI(26U)))
#define ctr_ia_a(i) (ctr_ins_a_dc(i))

#define ctr_ins_ab(op, a, b) \
  ((op & MASKI(6U)) << 26U) | ((a & MASKI(8U)) << 18U) | ((b & MASKI(18U)))
#define ctr_iab_a(i) ((i >> 18U) & MASKI(8U))
#define ctr_iab_b(i) ((i) & MASKI(18U))

#define ctr_ins_abc(op, a, b, c) \
  ((op & MASKI(6U)) << 26U) | ((a & MASKI(8U)) << 18U) | ((b & MASKI(9U)) << 9U) | ((c & MASKI(9U)))
#define ctr_iabc_a(i) ((i >> 18U) & MASKI(8U))
#define ctr_iabc_b(i) ((i >> 9U) & MASKI(9U))
#define ctr_iabc_c(i) ((i) & MASKI(9U))


typedef struct {
    ctr_opcode opcode;
    const char *mnemonic;
    ctr_instype type;
} ctr_inssig;
extern const ctr_inssig CTR_OP_INFO[CTR_OP_COUNT];
#define ctr_op_info(op) (&(CTR_OP_INFO[(op)]))

typedef enum {
    CTR_TNIL,
    CTR_TF64,
    CTR_TI64,
    CTR_TDYN,

    CTR_TCOUNT,
} ctr_type;
typedef double ctr_f64;
typedef int64_t ctr_i64;
typedef void *ctr_dyn;

typedef enum {
    CTR_DSTR,
    CTR_DOBJ,
    CTR_DFUN,
    CTR_DVAL,
    CTR_DCOUNT,
} ctr_dtype;
/// Dynamic allocation header including size, type, and gc info.
typedef struct {
    size_t size;
    bool is_const;
    ctr_dtype tt;
    uint32_t rc;
} ctr_dheader;

typedef struct {
    union ctr_innerval {
        ctr_f64 f64;
        ctr_i64 i64;
        ctr_dyn dyn;
    } val;
    ctr_type tt;
} ctr_val;
#define CTR_NIL (ctr_val){.tt = CTR_TNIL}
#define VEC_NAME ctr_valvec
#define VEC_T ctr_val
#define SIZE_T uint32_t
#include <sf/containers/vec.h>

typedef struct {
    sf_str name;
    enum {
        CTR_UP_VAL,
        CTR_UP_REF,
    } tt;
    union {
        ctr_val value;
        uint32_t ref;
    } inner;
    int32_t frame_o;
} ctr_upvalue;

struct ctr_state;
struct ctr_call_ex;
typedef struct ctr_call_ex (*ctr_cfunction)(struct ctr_state *);

/// Function prototype. This is the main unit of bytecode
/// for the language, and the result of compilation.
typedef struct {
    enum {
        CTR_PROTO_BC,
        CTR_PROTO_CFUN,
    } tt;
    union {
        ctr_instruction *bc;
        ctr_cfunction c_fun;
    } code;
    uint32_t code_s, reg_c, arg_c, up_c, entry;
    ctr_valvec constants;
    ctr_upvalue *upvals;
} ctr_proto;
#define VEC_NAME ctr_protovec
#define VEC_T ctr_proto *
#include <sf/containers/vec.h>
EXPORT ctr_proto ctr_proto_new(void);
EXPORT ctr_proto ctr_proto_cfun(ctr_cfunction c_fun, uint32_t arg_c, uint32_t temp_c);
EXPORT void ctr_proto_free(ctr_proto *proto);

typedef sf_str ctr_dstr;

struct ctr_dobj;
void _ctr_dobj_cleanup(struct ctr_dobj *obj);
#define MAP_NAME ctr_dobj
#define MAP_K sf_str
#define MAP_V ctr_val
#define EQUAL_FN(s1, s2) (sf_str_eq(s1, s2))
#define HASH_FN(s) (sf_str_hash(s))
#define CLEANUP_FN _ctr_dobj_cleanup
#include <sf/containers/map.h>
typedef ctr_proto *ctr_dfun;

/// Allocates a dynamic object, a heap allocated object with an information header.
EXPORT ctr_val ctr_dnew(ctr_dtype tt);
static inline ctr_dheader *ctr_header(ctr_val val) { return val.tt == CTR_TDYN ? (ctr_dheader *)((char *)val.val.dyn - sizeof(ctr_dheader)) : NULL; }
/// Make a new reference to a dynamic object.
static inline ctr_val ctr_dref(ctr_val val) {
    ctr_dheader *dh = ctr_header(val);
    if (dh && !dh->is_const) ++ctr_header(val)->rc;
    return val;
}
/// Get an inner value reference if it is a reference.
static inline ctr_val ctr_dval(ctr_val val) {
    ctr_dheader *dh = ctr_header(val);
    if (dh && dh->tt == CTR_DVAL)
        return *(ctr_val *)val.val.dyn;
    return val;
}
/// Delete a reference to a dynamic object.
static inline void ctr_ddel(ctr_val val) {
    ctr_dheader *dh = ctr_header(val);
    if (dh && !dh->is_const && --dh->rc == 0) {
        if (dh->tt == CTR_DSTR && val.val.dyn)
            sf_str_free(*(sf_str *)val.val.dyn);
        if (dh->tt == CTR_DOBJ && val.val.dyn)
            ctr_dobj_free((ctr_dobj *)val.val.dyn);
        if (dh->tt == CTR_DVAL && val.val.dyn)
            ctr_ddel(*(ctr_val *)val.val.dyn);
        if (dh->tt == CTR_DFUN && val.val.dyn)
            ctr_proto_free((ctr_proto *)val.val.dyn);
        free(dh);
    }
}

extern const sf_str CTR_TYPE_NAMES[(size_t)CTR_TCOUNT + (size_t)CTR_DCOUNT];
static inline sf_str ctr_typename(ctr_val val) { return val.tt == CTR_TDYN ? CTR_TYPE_NAMES[(int)CTR_TDYN + 1 + ctr_header(val)->tt] : CTR_TYPE_NAMES[val.tt]; }


typedef struct {
    enum {
        CTR_ERR_UNEXPECTED_EOF,
        CTR_ERR_UNKNOWN_OP,
        CTR_ERR_UNKNOWN_TYPE,
        CTR_ERR_MISSING_ARG,
        CTR_ERR_STRING_FORMAT,
    } type;
    sf_str string;
} ctr_asm_err;
#define EXPECTED_NAME ctr_asm_ex
#define EXPECTED_O ctr_proto
#define EXPECTED_E ctr_asm_err
#include <sf/containers/expected.h>
/// Compile a proto asm file (.csm)
EXPORT ctr_asm_ex ctr_assemble(const sf_str code);

#endif // BYTECODE_H
