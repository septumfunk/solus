#ifndef TYPES_H
#define TYPES_H

#include "val.h"

typedef struct {
    solu_val base;
    bool nilable, fallible, ok;
} solu_type;
#define solu_type_err() (solu_type){.ok=false}
#define solu_type_ok(val, nil, err) (solu_type){(val), (nil), (err), true}

static inline void _def_cleanup_fe(void *_u, sf_str key, solu_type _v) {
    (void)_u; (void)_v;
    sf_str_free(key);
}
typedef struct solu_def solu_def;
static void _def_cleanup(solu_def *def);
#define MAP_NAME solu_def
#define MAP_K sf_str
#define MAP_V solu_type
#define HASH_FN sf_str_hash
#define KCLEANUP sf_str_free
#define CLEANUP_FN _def_cleanup
#define EQUAL_FN sf_str_eq
#include <sf/containers/map.h>
static void _def_cleanup(solu_def *def) {
    solu_def_foreach(def, _def_cleanup_fe, NULL);
}

typedef struct {
    solu_val name;
    enum solu_titype {
        SOLU_TI_PRIM,
        SOLU_TI_FUN,
        SOLU_TI_DEF,
    } tt;
    bool complex;
    union {
        solu_opcode cast;
        struct {
            solu_type *args;
            uint32_t arg_c;
            solu_type return_t;
            bool variadic;
        } fun;
        solu_def def;
    };
} solu_tinfo;

typedef enum {
    SOLU_CANY,
    SOLU_CNIL,
    SOLU_CF64,
    SOLU_CI64,
    SOLU_CBOOL,
    SOLU_CSTR,
    SOLU_COBJ,
    SOLU_CERR,
    SOLU_CCOUNT,
} solu_pinfo;

typedef struct {
    solu_tinfo *global;
    solu_valmap types;
    solu_val ptypes[SOLU_CCOUNT];
} solu_typeenv;

#define EXPECTED_NAME solu_type_ex
#define EXPECTED_O solu_val
#define EXPECTED_E solu_error
#include <sf/containers/expected.h>

#endif // TYPES_H
