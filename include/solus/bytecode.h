#ifndef BYTECODE_H
#define BYTECODE_H

#include "sf/containers/buffer.h"
#include "sf/str.h"
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef _WIN32
#define _strdup strdup
#endif

/// Bytecode version
#define SOLU_VERSION "0.7.2"
/// Git repository, hosted on GitHub for now
#define SOLU_GIT "https://github.com/solus-lang/solus"

/// Bytecode instruction type
typedef enum {
    SOLU_INS_A, // A: i26 (jmp)
    SOLU_INS_AB, // A: u8, B: u18 (load)
    SOLU_INS_ABC, // A: u8, B: u9, C: u9 (most)
} solu_instype;
/// Bytecode Operations
typedef enum {
    SOLU_OP_LOAD,
    SOLU_OP_MOVE,
    SOLU_OP_RET,
    SOLU_OP_JMP,
    SOLU_OP_CALL,

    SOLU_OP_ADD,
    SOLU_OP_SUB,
    SOLU_OP_MUL,
    SOLU_OP_DIV,

    SOLU_OP_NEG,
    SOLU_OP_EQ,
    SOLU_OP_LT,
    SOLU_OP_LE,

    SOLU_OP_SETU,
    SOLU_OP_GETU,
    SOLU_OP_REFU,

    SOLU_OP_NEW,
    SOLU_OP_SET,
    SOLU_OP_GET,

    SOLU_OP_SUPO,
    SOLU_OP_GUPO,

    SOLU_OP_UNKNOWN,
    SOLU_OP_COUNT,
} solu_opcode;
/// All instructions are packed into a 32 bit uint
typedef uint32_t solu_instruction;
/// Loading errors from error.def
typedef enum {
#define X(prefix, name, string) SOLU_ERR##prefix##_##name,
#include "error.def"
#undef X
    SOLU_ERR_COUNT
} solu_error;
extern const char *SOLU_ERR_STRINGS[SOLU_ERR_COUNT];
#define solu_err_string(err) (sf_ref(SOLU_ERR_STRINGS[(err)]))

/// Masking functions
#define MASKI(n) ((1U << (n)) - 1U)
#define MAXARG_A ((1 << 25) - 1)

/// Store int26 as uint26
#define solu_ins_a_ec(a) ((uint32_t)((a) + MAXARG_A))
#define solu_ins_a_dc(a)  ((int32_t)((a) & MASKI(26U)) - MAXARG_A)

/// Pack type A instruction
#define solu_ins_a(op, as) \
  (((uint32_t)(op) & MASKI(6U)) << 26U | (solu_ins_a_ec(as) & MASKI(26U)))
#define solu_ia_a(i) (solu_ins_a_dc(i)) /// Retrieve operand A

/// Pack type AB instruction
#define solu_ins_ab(op, a, b) \
  ((op & MASKI(6U)) << 26U) | ((a & MASKI(8U)) << 18U) | ((b & MASKI(18U)))
#define solu_iab_a(i) ((i >> 18U) & MASKI(8U)) /// Retrieve operand A
#define solu_iab_b(i) ((i) & MASKI(18U)) /// Retrieve operand B

/// ABC allows B and C to be const
#define RKBIT 0x100 // MSB
#define RKIDX(x) ((x) & 0xFF) // lower 8 bits
#define solu_const(x) (RKBIT | RKIDX(x)) // constant
#define solu_reg(x) RKIDX(x)            // register
/// Pack type ABC instruction
#define solu_ins_abc(op, a, b, c) \
  ((op & MASKI(6U)) << 26U) | ((a & MASKI(8U)) << 18U) | ((b & MASKI(9U)) << 9U) | ((c & MASKI(9U)))
#define solu_iabc_a(i) ((i >> 18U) & MASKI(8U)) /// Retrieve operand A
#define solu_iabc_bx(i) ((i >> 9U) & 0xFF)  /// Retrieve operand B
#define solu_iabc_cx(i) ((i) & 0xFF) /// Retreieve operand C
#define solu_iabc_bk(i) (((i) >> 17U) & 1) // Const flag
#define solu_iabc_ck(i) (((i) >> 8U) & 1)  // Const flag

/// Retrieve opcode
#define solu_ins_op(i) ((i >> 26U) & MASKI(6U))

#define SOLU_DBG_LINE_BITS 16U // uint16_t
#define SOLU_DBG_COL_BITS  16U // uint16_t

/// Pack debug data
#define SOLU_DBG_COL_MASK  ((1u << SOLU_DBG_COL_BITS) - 1u)
#define SOLU_DBG_LINE_MASK ((1u << SOLU_DBG_LINE_BITS) - 1u)
#define SOLU_DBG_ENCODE(line, col) \
    (((uint32_t)(line) & SOLU_DBG_LINE_MASK) << SOLU_DBG_COL_BITS | \
     ((uint32_t)(col)  & SOLU_DBG_COL_MASK) )

/// Retrieve debug line
#define SOLU_DBG_LINE(loc) (((loc) >> SOLU_DBG_COL_BITS) & SOLU_DBG_LINE_MASK)
/// Retrieve debug column
#define SOLU_DBG_COL(loc)  ((loc) & SOLU_DBG_COL_MASK)

typedef uint32_t solu_dbg;

/// Instruction signatures, defines how an opcode's operands should look
typedef struct {
    solu_opcode opcode;
    const char *mnemonic;
    solu_instype type;
} solu_inssig;
extern const solu_inssig SOLU_OP_INFO[SOLU_OP_COUNT];
#define solu_op_info(op) (&(SOLU_OP_INFO[(op)]))

/// Primitive types stored on the stack
typedef enum {
    SOLU_TNIL,
    SOLU_TF64,
    SOLU_TI64,
    SOLU_TBOOL,
    SOLU_TDYN,

    SOLU_TCOUNT,
} solu_ptype;
typedef double solu_f64; // float
typedef int64_t solu_i64; // integer
typedef bool solu_bool; // boolean
typedef void *solu_dyn; // dynamic
/// Dynamic types stored on the heap.
/// The d prefix refers to dynamic
typedef enum {
    SOLU_DSTR, // string
    SOLU_DERR, // error
    SOLU_DOBJ, // object
    SOLU_DFUN, // function
    SOLU_DREF, // managed primitive

    SOLU_DUSR, // usertype

    SOLU_DCOUNT,
} solu_dtype;
/// Type names table
extern const char *SOLU_TYPE_NAMES[(size_t)SOLU_TCOUNT + (size_t)SOLU_DCOUNT];
/// GC State
typedef enum {
    SOLU_DYN_WHITE, /// Not yet marked, will be swept if it's not
    SOLU_DYN_BLACK, /// Marked valid
    SOLU_DYN_SHARED, /// Thread shared
    SOLU_DYN_GREEN, /// Reference held by C
} solu_dstate;
/// GC header
typedef struct solu_dalloc {
    struct solu_dalloc *next;
    size_t size, thread;
    solu_dtype tt;
    solu_dstate mark;
} solu_dalloc;

/// A primitive value, which may be a (dyn) reference to a GC/heap managed dynamic value
typedef struct {
    solu_ptype tt; // tt = Type Tag
    union {
        solu_f64 f64;
        solu_i64 i64;
        solu_bool boolean;
        solu_dyn dyn;
    };
} solu_val;
#define SOLU_NIL (solu_val){.tt = SOLU_TNIL}
#define SOLU_TRUE (solu_val){.tt = SOLU_TBOOL, .boolean = true}
#define SOLU_FALSE (solu_val){.tt = SOLU_TBOOL, .boolean = false}
#define VEC_NAME solu_valvec
#define VEC_T solu_val
#define VSIZE_T uint32_t
#include <sf/containers/vec.h>
/// The captures of a fun, all UP_REF upvals are converted to VAL when loaded from the const table
typedef struct {
    sf_str name;
    enum {
        SOLU_UP_VAL,
        SOLU_UP_REF,
    } tt;
    union {
        solu_val value;
        uint32_t ref;
    };
    uint32_t frame;
    bool mut;
} solu_upvalue;

struct solu_state;
struct solu_call_ex;
/// The signature of a soluus C API function
typedef struct solu_call_ex (*solu_cfunction)(struct solu_state *);

/// Function prototype. This is the main unit of bytecode for the language, and the result of compilation
typedef struct {
    enum {
        SOLU_FPROTO_BC, // bytecode
        SOLU_FPROTO_C, // c function
    } tt;
    union {
        struct {
            uint16_t code_c, line_c; // lines/instruction count
            uint32_t dbg_res, dbg_ll; // debug resume/line
            sf_str file_name;
            solu_instruction *code; // bytecode
            solu_dbg *dbg; // debug info
        };
        solu_cfunction c_fun;
    };
    uint32_t reg_c, arg_c, up_c; // registers, args, upvals,
    solu_valvec constants;
    solu_upvalue *upvals;
} solu_fproto;
EXPORT solu_fproto solu_fproto_new(void);
EXPORT solu_fproto solu_fproto_c(solu_cfunction c_fun, uint32_t arg_c, uint32_t temp_c);
EXPORT void solu_fproto_free(solu_fproto *proto);

// dstr
typedef sf_str solu_dstr;

// dobj
struct solu_dobj;
void _solu_dobj_cleanup(struct solu_dobj *obj);
#define MAP_NAME solu_dobj
#define MAP_K sf_str
#define MAP_V solu_val
#define EQUAL_FN(s1, s2) (sf_str_eq(s1, s2))
#define HASH_FN(s) (sf_str_hash(s))
#define CLEANUP_FN _solu_dobj_cleanup
#define KCLEANUP sf_str_free
#include <sf/containers/map.h>
typedef solu_fproto *solu_dfun;

/// Strings 40 characters or less are cached
#define SOLU_STRCACHE_MAX 40
struct solu_strcache;
void _solu_strcache_cleanup(struct solu_strcache *self);
#define MAP_NAME solu_strcache
#define MAP_K sf_str
#define MAP_V solu_dalloc *
#define EQUAL_FN(s1, s2) (sf_str_eq(s1, s2))
#define HASH_FN(s) (sf_str_hash(s))
#define CLEANUP_FN _solu_strcache_cleanup
#define KCLEANUP sf_str_free
#include <sf/containers/map.h>

typedef void (*solu_usrdel)(void *);
typedef char *(*solu_usrtostring)(void *);
typedef struct {
    sf_str name;
    solu_usrdel del;
    solu_usrtostring tostring;
} solu_usrwrap;

/// Cleanup functions for dynamic types
void solu_dclean(solu_val val);
/// Convenience function to get the solu_dalloc of a dyn value
static inline solu_dalloc *solu_dheader(solu_val val) {
    if (val.tt != SOLU_TDYN)
        return NULL;
    return (solu_dalloc *)((char *)val.dyn - sizeof(solu_dalloc));
}
/// Convenience function to get the solu_dtype of a dyn value
static inline solu_dtype solu_dtypeof(solu_val val) {
    if (val.tt != SOLU_TDYN)
        return SOLU_DCOUNT;
    return solu_dheader(val)->tt;
}

/// Returns whether a value is of the provided dynamic type
static inline bool solu_isdtype(solu_val value, solu_dtype dtype) {
    return value.tt == SOLU_TDYN && solu_dheader(value)->tt == dtype;
}

/// Returns whether two dstrs equal
static inline bool solu_streq(solu_val str1, solu_val str2) {
    size_t s1 = solu_dheader(str1)->size - 1, s2 = solu_dheader(str2)->size - 1;
    if (s1 != s2) return false;
    return memcmp(str1.dyn, str2.dyn, s1) == 0;
}

/// Get an inner value reference if it is a reference
static inline solu_val solu_dval(solu_val val) {
    solu_dalloc *dh = solu_dheader(val);
    if (dh && dh->tt == SOLU_DREF)
        return *(solu_val *)val.dyn;
    return val;
}

/// Gets the usrwrap header of a usrtype object
static inline solu_usrwrap *solu_uheader(solu_val val) {
    if (!solu_isdtype(val, SOLU_DUSR))
        return NULL;
    return (solu_usrwrap *)((char *)val.dyn + solu_dheader(val)->size);
}

/// Returns a (static) string denoting the type of a value
static inline sf_str solu_typename(solu_val val) {
    if (solu_isdtype(val, SOLU_DUSR))
        return solu_uheader(val)->name;
    solu_dalloc *dh = solu_dheader(val);(void)dh;
    return sf_lit(val.tt == SOLU_TDYN ? SOLU_TYPE_NAMES[(int)SOLU_TDYN + 1 + solu_dheader(val)->tt] : SOLU_TYPE_NAMES[val.tt]);
}
/// Returns whether a usrtype object is of the specified type
static inline bool solu_isutype(solu_val val, sf_str name) { return sf_str_eq(name, solu_typename(val)); }

#if defined(_WIN32) || defined(_WIN64)
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
/// Complicated cross platform stuff to get time in seconds
static inline double solu_timesec(void) {
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
static inline double solu_timesec(void) {
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

/// Canonize path
#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
static inline char *solu_realpath(const char *path) {
    char buf[_MAX_PATH];
    if (!_fullpath(buf, path, _MAX_PATH))
        return NULL;
    if (!sf_file_exists(sf_ref(path)))
        return NULL;
    return _strdup(buf);
}
#else
static inline char *solu_realpath(const char *path) {
    return realpath(path, NULL);
}
#endif
/// Get dir of canonized path
char *solu_realdir(const char *rp);
char *solu_findfile(const char *cwd, const char *rel_path);

/// Disassemble instruction
EXPORT sf_str solu_dasmi(solu_instruction ins);
/// Disassemble fun
EXPORT sf_str solu_dasmf(solu_fproto *proto);

#endif // BYTECODE_H
