#ifndef VAL_H
#define VAL_H

#include "bytecode.h"

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
} solu_dstate;

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
/// GC header
typedef enum {
    SOLU_META_GET,
    SOLU_META_SET,
    SOLU_META_CALL,
    SOLU_META_STR,
    SOLU_META_EXTEND,

    SOLU_META_ADD,
    SOLU_META_SUB,
    SOLU_META_MUL,
    SOLU_META_DIV,
    SOLU_META_EQ,
    SOLU_META_NEG,

    SOLU_META_COUNT,
} solu_metafun;
typedef struct solu_dalloc {
    struct solu_dalloc *next;
    size_t size, thread;
    solu_dtype tt;
    solu_dstate mark;
    bool held;
    solu_val meta;
    solu_val metadata[SOLU_META_COUNT];
} solu_dalloc;

#define SOLU_NIL (solu_val){.tt = SOLU_TNIL}
#define SOLU_TRUE (solu_val){.tt = SOLU_TBOOL, .boolean = true}
#define SOLU_FALSE (solu_val){.tt = SOLU_TBOOL, .boolean = false}
#define VEC_NAME solu_valvec
#define VEC_T solu_val
#define VSIZE_T uint32_t
#define VSIZE_MAX UINT32_MAX
#include <sf/containers/vec.h>
/// The captures of a fun, all UP_REF upvals are converted to VAL when loaded from the const table
typedef struct {
    sf_str name;
    enum {
        SOLU_UP_VAL,
        SOLU_UP_REF,
        SOLU_UP_UPV,
    } tt;
    union {
        solu_val value;
        uint32_t ref;
    };
    uint32_t frame;
    bool mut;
    solu_val type;
} solu_upvalue;

struct solu_state;
struct solu_call_ex;
/// The signature of a solus C API function
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
            sf_str file_name;
            solu_instruction *code; // bytecode
            solu_dbg *dbg; // debug info
        };
        solu_cfunction c_fun;
    };
    uint32_t reg_c, arg_c, up_c; // registers, args, upvals,
    solu_valvec constants;
    solu_upvalue *upvals;
    bool self, variadic;
} solu_fproto;
EXPORT solu_fproto solu_fproto_new(void);
EXPORT solu_fproto solu_fproto_c(solu_cfunction c_fun, uint32_t arg_c, solu_val *captures, uint32_t cap_c);
EXPORT void solu_fproto_free(solu_fproto *proto);

// dstr
typedef sf_str solu_dstr;

// dobj
struct solu_valmap;
void _solu_valmap_cleanup(struct solu_valmap *obj);
#define MAP_NAME solu_valmap
#define MAP_K sf_str
#define MAP_V solu_val
#define EQUAL_FN(s1, s2) (sf_str_eq(s1, s2))
#define HASH_FN(s) (sf_str_hash(s))
#define CLEANUP_FN _solu_valmap_cleanup
#define KCLEANUP sf_str_free
#include <sf/containers/map.h>

typedef struct {
    solu_valmap map;
    solu_valvec array;
} solu_dobj;
EXPORT solu_dobj solu_dobj_new(void);
EXPORT void solu_dobj_free(solu_dobj *obj);
EXPORT solu_val solu_dobj_strget(solu_dobj *obj, char *key);
/// You do NOT need to pass an owned string
EXPORT void solu_dobj_strset(solu_dobj *obj, char *key, solu_val val);
EXPORT void solu_usemeta(solu_val obj, solu_dobj *meta);
// fun
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
typedef void (*solu_usrmark)(void *);
typedef struct {
    sf_str name;
    solu_usrdel del;
    solu_usrmark mark;
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

/// Convenience function for checking arrays
static inline bool solu_isarr(solu_val value, uint32_t minimum) {
    return solu_isdtype(value, SOLU_DOBJ) && ((solu_dobj *)value.dyn)->array.count >= minimum;
}
static inline bool solu_arrptype(solu_val value, solu_ptype ptype, uint32_t minimum) {
    if (!solu_isarr(value, minimum)) return false;
    solu_dobj *arr = (solu_dobj *)value.dyn;
    if (arr->array.count < minimum) return false;

    bool t = true;
    for (uint32_t i = 0; i < minimum; ++i)
        if (arr->array.data[i].tt != ptype)
            t = false;
    return t;
}

/// Returns whether two dstrs equal
static inline bool solu_streq(solu_val str1, solu_val str2) {
    solu_dalloc *da1 = solu_dheader(str1), *da2 = solu_dheader(str2);
    if (da1->tt != SOLU_DSTR || da2->tt != SOLU_DSTR) return false;
    size_t s1 = da1->size - 1, s2 = da2->size - 1;
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

/// Returns the boolean value of any given type
EXPORT bool solu_truthy(solu_val value);
/// Returns whether values are equal without metadata calls or casting
EXPORT bool solu_strict_eq(solu_val lhs, solu_val rhs);

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

/// Disassemble fun
EXPORT sf_str solu_dasmf(solu_fproto *proto);

typedef struct {
    char *path;
    uint16_t line, column;
    solu_fproto *p;
} solu_tracedata;
typedef struct solu_trace solu_trace;
void _solu_trace_cleanup(solu_trace *st);
#define VEC_NAME solu_trace
#define VEC_T solu_tracedata
#define VSIZE_T uint32_t
#define VSIZE_MAX UINT32_MAX
#define CLEANUP_FN _solu_trace_cleanup
#include <sf/containers/vec.h>

#ifndef _WIN32
    #define TUI_CLR  "\x1b[0m"
    #define TUI_ERR    "\x1b[1;31m"  // bright red
    #define TUI_INFO   "\x1b[0;90m"  // gray
    #define TUI_UL  "\x1b[4m"
    #define TUI_BLD "\x1b[1m"
    #define TUI_ITL "\x1b[3m"
#else
    #define TUI_CLR  ""
    #define TUI_ERR    ""
    #define TUI_INFO   ""
    #define TUI_UL  ""
    #define TUI_BLD ""
    #define TUI_ITL ""
#endif

EXPORT solu_trace solu_trace_clone(solu_trace *st);
EXPORT char *solu_trace_print(solu_trace *st, uint32_t max, uint8_t lookback, uint8_t lookahead);
EXPORT void highlight_line(sf_str *out, sf_str src, uint16_t line, uint16_t column, uint8_t lookback, uint8_t lookahead);

#endif // VAL_H
