#ifndef SOLUC_H
#define SOLUC_H

#include "val.h"
#include "types.h"
#include "syntax.h"

typedef struct { uint16_t idx; solu_tokentype tt; } solu_control;
#define VEC_NAME solu_controls
#define VEC_T solu_control
#define VSIZE_T uint32_t
#define VSIZE_MAX UINT32_MAX
#include <sf/containers/vec.h>

/// A simple representation of a local variable (or upvalue)
typedef struct {
    uint32_t reg, scope;
    bool upval, mut;
    uint32_t frame;
    solu_type type;
} solu_local;

struct solu_scope;
void _solu_scope_cleanup(struct solu_scope *);
#define MAP_NAME solu_scope
#define MAP_K sf_str
#define MAP_V solu_local
#define EQUAL_FN sf_str_eq
#define HASH_FN sf_str_hash
#define KCLEANUP sf_str_free
#define CLEANUP_FN _solu_scope_cleanup
#include <sf/containers/map.h>

struct solu_scopes;
void _solu_scopes_cleanup(struct solu_scopes *);
#define VEC_NAME solu_scopes
#define VEC_T solu_scope
#define VSIZE_T uint32_t
#define VSIZE_MAX UINT32_MAX
#define CLEANUP_FN _solu_scopes_cleanup
#include <sf/containers/vec.h>

/// Temporary compilation info that's shared between all compiler functions
typedef struct {
    sf_str path;
    solu_fproto proto;
    solu_ast ast;
    solu_scopes scopes;
    uint32_t temps, max_reg, frame;
    solu_dalloc *alloc;

    uint32_t obj_r;
    solu_valmap fields;
    bool inloop;
    solu_controls controls;
    solu_ctrace *ct;
    solu_typeenv *tenv;
    solu_val *ptypes;
    solu_tinfo *def;
} solu_compiler;

/// Helper function for the compiler to allocate a new str
solu_val solu_cstr(solu_dalloc *alloc, sf_str str);
/// Loads a local from the current scope if it exists
bool solu_lexists(solu_compiler *c, char *name, solu_local *loc);

// Types

solu_type solu_typeof(solu_compiler *c, solu_node *node);
solu_type solu_tres(solu_compiler *c, solu_node *type);
bool solu_tassignable(solu_compiler *c, solu_type dst, solu_type src);
solu_val solu_push_tdef(solu_dalloc *alloc, sf_str name, bool complex);
sf_str solu_stypename(solu_compiler *c, solu_type t);

#define EXPECTED_NAME solu_compile_ex
#define EXPECTED_O solu_fproto
#define EXPECTED_E solu_ctrace *
#include <sf/containers/expected.h>
/// Compile a solu_proto from source code
solu_compile_ex solu_cproto(solu_ctrace *ct, sf_str path, char *src, uint32_t arg_c, solu_val *args, uint32_t up_c, solu_upvalue *upvals, solu_typeenv *tenv);

#endif // SOLUC_H
