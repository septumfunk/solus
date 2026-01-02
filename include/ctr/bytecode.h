#ifndef BYTECODE_H
#define BYTECODE_H

#include "sf/str.h"
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

// Define CTR_DBG_LOG to get debug output from the compiler and vm
#define CTR_DBG_LOG true

#ifdef CTR_DBG_LOG
#define ctr_log(lit) printf(lit"\n")
#define ctr_logf(lit, ...) printf(lit"\n", __VA_ARGS__)
#else
#define ctr_log(lit)
#define ctr_logf(lit, ...)
#endif

typedef enum {
    CTR_INS_A, // A: i26 (jmp)
    CTR_INS_AB, // A: u8, B: u18 (load)
    CTR_INS_ABC, // A: u8, B: u9, C: u9 (most)
} ctr_instype;

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

    CTR_OP_SETU,
    CTR_OP_GETU,
    CTR_OP_REFU,

    CTR_OP_SET,
    CTR_OP_GET,

    CTR_OP_UNKNOWN,
    CTR_OP_COUNT,
} ctr_opcode;
typedef uint32_t ctr_instruction;


typedef enum {
#define X(prefix, name, string) CTR_ERR##prefix##_##name,
#include "error.def"
#undef X
    CTR_ERR_COUNT
} ctr_error;
extern const sf_str CTR_ERR_STRINGS[CTR_ERR_COUNT];
#define ctr_err_string(err) (CTR_ERR_STRINGS[(err)])


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


#define CTR_DBG_LINE_BITS 16
#define CTR_DBG_COL_BITS  16

#define CTR_DBG_COL_MASK  ((1u << CTR_DBG_COL_BITS) - 1u)
#define CTR_DBG_LINE_MASK ((1u << CTR_DBG_LINE_BITS) - 1u)

#define CTR_DBG_ENCODE(line, col) \
    (((uint32_t)(line) & CTR_DBG_LINE_MASK) << CTR_DBG_COL_BITS | \
     ((uint32_t)(col)  & CTR_DBG_COL_MASK) )

#define CTR_DBG_LINE(loc) (((loc) >> CTR_DBG_COL_BITS) & CTR_DBG_LINE_MASK)
#define CTR_DBG_COL(loc)  ((loc) & CTR_DBG_COL_MASK)

typedef uint32_t ctr_dbg;


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
    CTR_TBOOL,
    CTR_TDYN,

    CTR_TCOUNT,
} ctr_ptype;
typedef double ctr_f64;
typedef int64_t ctr_i64;
typedef bool ctr_bool;
typedef void *ctr_dyn;

typedef enum {
    CTR_DSTR,
    CTR_DERR,
    CTR_DOBJ,
    CTR_DARRAY,
    CTR_DFUN,
    CTR_DREF,

    CTR_DCOUNT,
} ctr_dtype;
extern const sf_str CTR_TYPE_NAMES[(size_t)CTR_TCOUNT + (size_t)CTR_DCOUNT];

/// Dynamic allocation header including size, type, and gc info
typedef struct {
    size_t size;
    ctr_dtype tt;
    bool is_const;
    uint32_t rc;
} ctr_dheader;

typedef struct {
    ctr_ptype tt;
    union {
        ctr_f64 f64;
        ctr_i64 i64;
        ctr_bool boolean;
        ctr_dyn dyn;
    };
} ctr_val;
#define CTR_NIL (ctr_val){.tt = CTR_TNIL}
#define CTR_TRUE (ctr_val){.tt = CTR_TBOOL, .boolean = true}
#define CTR_FALSE (ctr_val){.tt = CTR_TBOOL, .boolean = false}
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
    };
    int32_t frame_o;
} ctr_upvalue;

struct ctr_state;
struct ctr_call_ex;
typedef struct ctr_call_ex (*ctr_cfunction)(struct ctr_state *);

/// Function prototype. This is the main unit of bytecode
/// for the language, and the result of compilation
typedef struct {
    enum {
        CTR_FPROTO_BC, // bytecode
        CTR_FPROTO_C, // c function
    } tt;
    union {
        struct {
            ctr_instruction *code;
            ctr_dbg *dbg;
        };
        ctr_cfunction c_fun;
    };
    uint32_t code_s, reg_c, arg_c, up_c, entry;
    ctr_valvec constants;
    ctr_upvalue *upvals;
} ctr_fproto;
EXPORT ctr_fproto ctr_fproto_new(void);
EXPORT ctr_fproto ctr_fproto_c(ctr_cfunction c_fun, uint32_t arg_c, uint32_t temp_c);
EXPORT void ctr_fproto_free(ctr_fproto *proto);

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
typedef ctr_fproto *ctr_dfun;

/// Allocates a dynamic object, a heap allocated object with an information header
EXPORT ctr_val ctr_dnew(ctr_dtype tt);
/// Shorthand for using ctr_dnew and assigning a string value.
/// This function takes ownership of the string passed, so make a copy if needed
static inline ctr_val ctr_dnewstr(sf_str str) {
    ctr_val strv = ctr_dnew(CTR_DSTR);
    *(sf_str *)strv.dyn = str;
    return strv;
}
/// Shorthand for using ctr_dnew and assigning a string value.
/// This function takes ownership of the string passed, so make a copy if needed
static inline ctr_val ctr_dnewerr(sf_str str) {
    ctr_val strv = ctr_dnew(CTR_DERR);
    *(sf_str *)strv.dyn = str;
    return strv;
}
/// Delete a reference to a dynamic object (decrement ref counter)
EXPORT void ctr_ddel(ctr_val val);
/// Return the header pointer of a dynamic value (or NULL)
static inline ctr_dheader *ctr_header(ctr_val val) {
    return val.tt == CTR_TDYN ? (ctr_dheader *)((char *)val.dyn - sizeof(ctr_dheader)) : NULL;
}
/// Returns whether a value is of the provided dynamic type
static inline bool ctr_isdtype(ctr_val value, ctr_dtype dtype) {
    return value.tt == CTR_TDYN && ctr_header(value)->tt == dtype;
}
/// Make a new reference to a dynamic object (increment ref counter)
static inline ctr_val ctr_dref(ctr_val val) {
    ctr_dheader *dh = ctr_header(val);
    if (dh && !dh->is_const) ++ctr_header(val)->rc;
    return val;
}
/// Get an inner value reference if it is a reference
static inline ctr_val ctr_dval(ctr_val val) {
    ctr_dheader *dh = ctr_header(val);
    if (dh && dh->tt == CTR_DREF)
        return *(ctr_val *)val.dyn;
    return val;
}
/// Returns a (static) string denoting the type of a value
static inline sf_str ctr_typename(ctr_val val) {
    
    return val.tt == CTR_TDYN ? CTR_TYPE_NAMES[(int)CTR_TDYN + 1 + ctr_header(val)->tt] : CTR_TYPE_NAMES[val.tt];
}

#endif // BYTECODE_H
