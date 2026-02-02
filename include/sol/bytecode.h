#ifndef BYTECODE_H
#define BYTECODE_H

#include "sf/str.h"
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/// Bytecode version
#define SOL_VERSION "0.6"
/// Git repository, hosted on GitHub for now
#define SOL_GIT "https://github.com/septumfunk/solus"

/// Bytecode instruction type
typedef enum {
    SOL_INS_A, // A: i26 (jmp)
    SOL_INS_AB, // A: u8, B: u18 (load)
    SOL_INS_ABC, // A: u8, B: u9, C: u9 (most)
} sol_instype;
/// Bytecode Operations
typedef enum {
    SOL_OP_LOAD,
    SOL_OP_MOVE,
    SOL_OP_RET,
    SOL_OP_JMP,
    SOL_OP_CALL,

    SOL_OP_ADD,
    SOL_OP_SUB,
    SOL_OP_MUL,
    SOL_OP_DIV,

    SOL_OP_NEG,
    SOL_OP_EQ,
    SOL_OP_LT,
    SOL_OP_LE,

    SOL_OP_SETU,
    SOL_OP_GETU,
    SOL_OP_REFU,

    SOL_OP_NEW,
    SOL_OP_SET,
    SOL_OP_GET,

    SOL_OP_SUPO,
    SOL_OP_GUPO,

    SOL_OP_UNKNOWN,
    SOL_OP_COUNT,
} sol_opcode;
/// All instructions are packed into a 32 bit uint
typedef uint32_t sol_instruction;
/// Loading errors from error.def
typedef enum {
#define X(prefix, name, string) SOL_ERR##prefix##_##name,
#include "error.def"
#undef X
    SOL_ERR_COUNT
} sol_error;
extern const char *SOL_ERR_STRINGS[SOL_ERR_COUNT];
#define sol_err_string(err) (sf_ref(SOL_ERR_STRINGS[(err)]))

/// Masking functions
#define MASKI(n) ((1U << (n)) - 1U)
#define MAXARG_A ((1 << 25) - 1)

/// Store int26 as uint26
#define sol_ins_a_ec(a) ((uint32_t)((a) + MAXARG_A))
#define sol_ins_a_dc(a)  ((int32_t)((a) & MASKI(26U)) - MAXARG_A)

/// Pack type A instruction
#define sol_ins_a(op, as) \
  (((uint32_t)(op) & MASKI(6U)) << 26U | (sol_ins_a_ec(as) & MASKI(26U)))
#define sol_ia_a(i) (sol_ins_a_dc(i)) /// Retrieve operand A

/// Pack type AB instruction
#define sol_ins_ab(op, a, b) \
  ((op & MASKI(6U)) << 26U) | ((a & MASKI(8U)) << 18U) | ((b & MASKI(18U)))
#define sol_iab_a(i) ((i >> 18U) & MASKI(8U)) /// Retrieve operand A
#define sol_iab_b(i) ((i) & MASKI(18U)) /// Retrieve operand B

/// ABC allows B and C to be const
#define RKBIT 0x100 // MSB
#define RKIDX(x) ((x) & 0xFF) // lower 8 bits
#define sol_const(x) (RKBIT | RKIDX(x)) // constant
#define sol_reg(x) RKIDX(x)            // register
/// Pack type ABC instruction
#define sol_ins_abc(op, a, b, c) \
  ((op & MASKI(6U)) << 26U) | ((a & MASKI(8U)) << 18U) | ((b & MASKI(9U)) << 9U) | ((c & MASKI(9U)))
#define sol_iabc_a(i) ((i >> 18U) & MASKI(8U)) /// Retrieve operand A
#define sol_iabc_bx(i) ((i >> 9U) & 0xFF)  /// Retrieve operand B
#define sol_iabc_cx(i) ((i) & 0xFF) /// Retreieve operand C
#define sol_iabc_bk(i) (((i) >> 17U) & 1) // Const flag
#define sol_iabc_ck(i) (((i) >> 8U) & 1)  // Const flag

/// Retrieve opcode
#define sol_ins_op(i) ((i >> 26U) & MASKI(6U))

#define SOL_DBG_LINE_BITS 16U // uint16_t
#define SOL_DBG_COL_BITS  16U // uint16_t

/// Pack debug data
#define SOL_DBG_COL_MASK  ((1u << SOL_DBG_COL_BITS) - 1u)
#define SOL_DBG_LINE_MASK ((1u << SOL_DBG_LINE_BITS) - 1u)
#define SOL_DBG_ENCODE(line, col) \
    (((uint32_t)(line) & SOL_DBG_LINE_MASK) << SOL_DBG_COL_BITS | \
     ((uint32_t)(col)  & SOL_DBG_COL_MASK) )

/// Retrieve debug line
#define SOL_DBG_LINE(loc) (((loc) >> SOL_DBG_COL_BITS) & SOL_DBG_LINE_MASK)
/// Retrieve debug column
#define SOL_DBG_COL(loc)  ((loc) & SOL_DBG_COL_MASK)

typedef uint32_t sol_dbg;

/// Instruction signatures, defines how an opcode's operands should look
typedef struct {
    sol_opcode opcode;
    const char *mnemonic;
    sol_instype type;
} sol_inssig;
extern const sol_inssig SOL_OP_INFO[SOL_OP_COUNT];
#define sol_op_info(op) (&(SOL_OP_INFO[(op)]))

/// Primitive types stored on the stack
typedef enum {
    SOL_TNIL,
    SOL_TF64,
    SOL_TI64,
    SOL_TBOOL,
    SOL_TDYN,

    SOL_TCOUNT,
} sol_ptype;
typedef double sol_f64; // float
typedef int64_t sol_i64; // integer
typedef bool sol_bool; // boolean
typedef void *sol_dyn; // dynamic
/// Dynamic types stored on the heap.
/// The d prefix refers to dynamic
typedef enum {
    SOL_DSTR, // string
    SOL_DERR, // error
    SOL_DOBJ, // object
    SOL_DFUN, // function
    SOL_DREF, // managed primitive

    SOL_DUSR, // usertype

    SOL_DCOUNT,
} sol_dtype;
/// Type names table
extern const char *SOL_TYPE_NAMES[(size_t)SOL_TCOUNT + (size_t)SOL_DCOUNT];
/// GC State
typedef enum {
    SOL_DYN_WHITE, /// Not yet marked, will be swept if it's not
    SOL_DYN_BLACK, /// Marked valid
    SOL_DYN_GREEN, /// Reference held by C
} sol_dstate;
/// GC header
typedef struct sol_dalloc {
    struct sol_dalloc *next;
    size_t size;
    sol_dtype tt;
    sol_dstate mark;
} sol_dalloc;

/// A primitive value, which may be a (dyn) reference to a GC/heap managed dynamic value
typedef struct {
    sol_ptype tt; // tt = Type Tag
    union {
        sol_f64 f64;
        sol_i64 i64;
        sol_bool boolean;
        sol_dyn dyn;
    };
} sol_val;
#define SOL_NIL (sol_val){.tt = SOL_TNIL}
#define SOL_TRUE (sol_val){.tt = SOL_TBOOL, .boolean = true}
#define SOL_FALSE (sol_val){.tt = SOL_TBOOL, .boolean = false}
#define VEC_NAME sol_valvec
#define VEC_T sol_val
#define VSIZE_T uint32_t
#include <sf/containers/vec.h>
/// The captures of a fun, all UP_REF upvals are converted to VAL when loaded from the const table
typedef struct {
    sf_str name;
    enum {
        SOL_UP_VAL,
        SOL_UP_REF,
    } tt;
    union {
        sol_val value;
        uint32_t ref;
    };
    uint32_t frame;
} sol_upvalue;

struct sol_state;
struct sol_call_ex;
/// The signature of a solus C API function
typedef struct sol_call_ex (*sol_cfunction)(struct sol_state *);

/// Function prototype. This is the main unit of bytecode for the language, and the result of compilation
typedef struct {
    enum {
        SOL_FPROTO_BC, // bytecode
        SOL_FPROTO_C, // c function
    } tt;
    union {
        struct {
            uint16_t line_c, code_c; // lines/instruction count
            uint32_t dbg_res, dbg_ll; // debug resume/line
            sf_str file_name;
            sol_instruction *code; // bytecode
            sol_dbg *dbg; // debug info
        };
        sol_cfunction c_fun;
    };
    uint32_t reg_c, arg_c, up_c; // registers, args, upvals,
    sol_valvec constants;
    sol_upvalue *upvals;
} sol_fproto;
EXPORT sol_fproto sol_fproto_new(void);
EXPORT sol_fproto sol_fproto_c(sol_cfunction c_fun, uint32_t arg_c, uint32_t temp_c);
EXPORT void sol_fproto_free(sol_fproto *proto);

// dstr
typedef sf_str sol_dstr;

// dobj
struct sol_dobj;
void _sol_dobj_cleanup(struct sol_dobj *obj);
#define MAP_NAME sol_dobj
#define MAP_K sf_str
#define MAP_V sol_val
#define EQUAL_FN(s1, s2) (sf_str_eq(s1, s2))
#define HASH_FN(s) (sf_str_hash(s))
#define CLEANUP_FN _sol_dobj_cleanup
#define KCLEANUP sf_str_free
#include <sf/containers/map.h>
typedef sol_fproto *sol_dfun;

/// Strings 40 characters or less are cached
#define SOL_STRCACHE_MAX 40
struct sol_strcache;
void _sol_strcache_cleanup(struct sol_strcache *self);
#define MAP_NAME sol_strcache
#define MAP_K sf_str
#define MAP_V sol_dalloc *
#define EQUAL_FN(s1, s2) (sf_str_eq(s1, s2))
#define HASH_FN(s) (sf_str_hash(s))
#define CLEANUP_FN _sol_strcache_cleanup
#define KCLEANUP sf_str_free
#include <sf/containers/map.h>

typedef void (*sol_usrdel)(void *);
typedef char *(*sol_usrtostring)(void *);
typedef struct {
    sf_str name;
    sol_usrdel del;
    sol_usrtostring tostring;
} sol_usrwrap;

/// Cleanup functions for dynamic types
void sol_dclean(sol_val val);
/// Convenience function to get the sol_dalloc of a dyn value
static inline sol_dalloc *sol_dheader(sol_val val) {
    if (val.tt != SOL_TDYN)
        return NULL;
    return (sol_dalloc *)((char *)val.dyn - sizeof(sol_dalloc));
}
/// Convenience function to get the sol_dtype of a dyn value
static inline sol_dtype sol_dtypeof(sol_val val) {
    if (val.tt != SOL_TDYN)
        return SOL_DCOUNT;
    return sol_dheader(val)->tt;
}

/// Returns whether a value is of the provided dynamic type
static inline bool sol_isdtype(sol_val value, sol_dtype dtype) {
    return value.tt == SOL_TDYN && sol_dheader(value)->tt == dtype;
}

/// Returns whether two dstrs equal
static inline bool sol_streq(sol_val str1, sol_val str2) {
    size_t s1 = sol_dheader(str1)->size - 1, s2 = sol_dheader(str2)->size - 1;
    if (s1 != s2) return false;
    return memcmp(str1.dyn, str2.dyn, s1) == 0;
}

/// Get an inner value reference if it is a reference
static inline sol_val sol_dval(sol_val val) {
    sol_dalloc *dh = sol_dheader(val);
    if (dh && dh->tt == SOL_DREF)
        return *(sol_val *)val.dyn;
    return val;
}

/// Gets the usrwrap header of a usrtype object
static inline sol_usrwrap *sol_uheader(sol_val val) {
    if (!sol_isdtype(val, SOL_DUSR))
        return NULL;
    return (sol_usrwrap *)((char *)val.dyn + sol_dheader(val)->size);
}

/// Returns a (static) string denoting the type of a value
static inline sf_str sol_typename(sol_val val) {
    if (sol_isdtype(val, SOL_DUSR))
        return sol_uheader(val)->name;
    sol_dalloc *dh = sol_dheader(val);(void)dh;
    return sf_lit(val.tt == SOL_TDYN ? SOL_TYPE_NAMES[(int)SOL_TDYN + 1 + sol_dheader(val)->tt] : SOL_TYPE_NAMES[val.tt]);
}
/// Returns whether a usrtype object is of the specified type
static inline bool sol_isutype(sol_val val, sf_str name) { return sf_str_eq(name, sol_typename(val)); }

#if defined(_WIN32) || defined(_WIN64)
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
/// Complicated cross platform stuff to get time in seconds
static inline double sol_timesec(void) {
    FILETIME ft;
    ULARGE_INTEGER uli;
    GetSystemTimeAsFileTime(&ft);
    uli.LowPart  = ft.dwLowDateTime;
    uli.HighPart = ft.dwHighDateTime;
    return (double)(uli.QuadPart - 116444736000000000ULL) / 10000000.0;
}
#else
#include <time.h>
#include <sys/time.h>
/// Complicated cross platform stuff to get time in seconds
static inline double sol_timesec(void) {
#if defined(CLOCK_REALTIME)
    struct timespec ts;
    clock_gettime(CLOCK_REALTIME, &ts);
    return (double)ts.tv_sec + (double)ts.tv_nsec / 1e9;
#else
    struct timeval tv;
    gettimeofday(&tv, NULL);
    return (double)tv.tv_sec + (double)tv.tv_usec / 1e6;
#endif
}
#endif

/// Disassemble instruction
EXPORT sf_str sol_dasmi(sol_instruction ins);
/// Disassemble fun
EXPORT sf_str sol_dasmf(sol_fproto *proto);

#endif // BYTECODE_H
