#ifndef VM_H
#define VM_H

#include "sf/str.h"
#include "val.h"
#include "compiler.h"
#include <stdarg.h>

/// GC will collect after cb = lb * SOLU_GCSTEP
#define SOLU_GCSTEP 1.5

/// Represents a function's frame, or compiler reserved registers, on the stack
typedef struct {
    uint32_t bottom_o;
    uint32_t size;
    bool self;
} solu_stackframe;
/// Stack frame stack (i know that sounds confusing)
#define VEC_NAME solu_frames
#define VEC_T solu_stackframe
#define VSIZE_T uint32_t
#define VSIZE_MAX UINT32_MAX
#include <sf/containers/vec.h>
/// File name stack
typedef struct solu_filenames solu_filenames;
void _solu_filenames_cleanup(solu_filenames *);
#define VEC_NAME solu_filenames
#define VEC_T sf_str
#define VSIZE_T uint32_t
#define VSIZE_MAX UINT32_MAX
#define CLEANUP_FN _solu_filenames_cleanup
#include <sf/containers/vec.h>

/// The main global state for the VM, responsible for the stack and any globals/caching
typedef struct solu_state {
    solu_valvec stack; // registers/stack
    solu_frames frames; // stack frames
    solu_val global; // _g
    uint32_t call_stack;

    bool collect;
    solu_dalloc *alloc, *alloc_tail; // gc allocations
    solu_strcache strcache; // short string cache
    size_t lb, cb, nb; // last bytes, current bytes, next bytes
    solu_fproto *ccall, *ecall; // the proto being called currently
    uint32_t cpc; // current program counter at call
    sf_str cwd;

    struct {
        solu_val prim;
        solu_val base;
        solu_val obj;
        solu_val string;
    } meta;

    bool rcmp; // Special flag for compiled files to reset the frame count
} solu_state;

/// Create a new solus VM state
EXPORT solu_state *solu_state_new(void);
/// Clean up a solus VM's state
EXPORT void solu_state_free(solu_state *state);

/// Load the solus standard library into the global namespace.
/// The standard library is implemented in C functions
EXPORT void solu_usestd(solu_state *state);
/// Compile solus source code.
/// Returns a fun or an err
EXPORT solu_compile_ex solu_csrc(solu_state *state, char *src);
/// Compile a solus source code file.
/// Returns a fun or an err
EXPORT solu_compile_ex solu_cfile(solu_state *state, char *path);

/// Construct a new dynamic type
EXPORT solu_val solu_dnew(solu_state *state, solu_dtype type);
/// Constructs a dynamic usertype object, a dynamic type with extra user info.
/// User types are managed by the GC so make sure you use solu_dhold if you don't want them to be!
EXPORT solu_val solu_dnusr(solu_state *state, size_t size, const char *name, void *value,
    solu_usrdel del, solu_usrmark mark);
/// Shorthand for using solu_dnew and assigning a string value.
EXPORT solu_val solu_dnstr(solu_state *state, const char *str);
/// Shorthand for using solu_dnew and assigning a string value.
EXPORT solu_val solu_dnerr(solu_state *state, const char *str);

/// Converts a value to a string.
/// You are responsible for freeing this string
EXPORT char *solu_tostr(solu_state *state, solu_val value);

/// Automatically converts a key to lookup an object's member
EXPORT solu_val solu_dobj_get(solu_state *state, solu_dobj *obj, solu_val key);
/// Automatically converts a key to set an object's member
EXPORT void solu_dobj_set(solu_state *state, solu_dobj *obj, solu_val key, solu_val val);
/// Join two objects into a single object.
/// Passing SOLU_NIL for obj2 will just copy obj1
EXPORT solu_val solu_djoin(solu_state *s, solu_val obj1, solu_val obj2);
/// Add all fields from obj2 into obj1
EXPORT void solu_dappend(solu_val obj1, solu_val obj2);

/// Mark a dynamic value as reachable.
/// Use this if your userdata "owns" any values
EXPORT void solu_dmark(solu_val val);
/// Mark and sweep garbage collection
EXPORT void solu_dcollect(solu_state *state);
/// Manually push an allocation to the gc.
/// Not recommended for users :)
void solu_dpush(solu_state *s, solu_dalloc *ac);
/// Hold a reference to the a dyn value for the C API.
/// This marks the object as green, meaning collection is skipped
static inline void solu_dhold(solu_val val) {
    if (val.tt != SOLU_TDYN) return;
    solu_dheader(val)->held = true;
}
/// Release a reference held to a dyn value in the C API
static inline void solu_drelease(solu_val val) {
    if (val.tt != SOLU_TDYN) return;
    solu_dheader(val)->held = false;
}


/// Get the value of a register from a specific stack frame
static inline solu_val solu_rawget(solu_state *state, uint32_t index, uint32_t frame) {
    solu_val val = solu_valvec_get(&state->stack, state->frames.data[frame].bottom_o + index - (state->frames.data[frame].self ? 1 : 0));
    if (solu_isdtype(val, SOLU_DREF))
        return *(solu_val *)val.dyn;
    return val;
}
/// Get the value of a register from the current stack frame.
/// In the C API this can be used to get function arguments (0, 1, 2...)
static inline solu_val solu_get(solu_state *state, uint32_t index) { return solu_rawget(state, index, state->frames.count - 1); }
/// Get the value of a constant from the current fun
EXPORT solu_val solu_getk(solu_state *state, solu_fproto *proto, uint32_t index);

/// Set the value of a register in a specific stack frame
static inline void solu_rawset(solu_state *state, uint32_t index, solu_val val, uint32_t frame) {
    uint32_t i = state->frames.data[frame].bottom_o + index - (state->frames.data[frame].self ? 1 : 0);
    solu_val old = solu_valvec_get(&state->stack, i);
    if (solu_isdtype(old, SOLU_DREF)) {
        *(solu_val *)old.dyn = val;
        return;
    }
    solu_valvec_set(&state->stack, i, val);
}
/// Set the value of a register in the current stack frame
static inline void solu_set(solu_state *state, uint32_t index, solu_val val) {
    solu_rawset(state, index, val, state->frames.count - 1);
}
/// Get a global value by name. Returns nil if it's not found
static inline solu_val solu_getg(solu_state *state, char *name) {
    return solu_dobj_strget(state->global.dyn, name);
}
/// Set a global value by name
static inline void solu_setg(solu_state *state, char *name, solu_val value) {
    solu_dobj_strset(state->global.dyn, name, value);
}
/// Push a stack frame to the VM
static inline uint32_t solu_pushframe(solu_state *state, uint32_t reg_c) {
    solu_frames_push(&state->frames, (solu_stackframe){
        state->frames.count == 0 ? 0 : state->frames.data[state->frames.count - 1].bottom_o + state->frames.data[state->frames.count - 1].size,
        reg_c,
        false,
    });
    for (uint32_t i = 0; i < reg_c; ++i)
        solu_valvec_push(&state->stack, SOLU_NIL);
    return state->frames.count - 1;
}
/// Pop the top stack frame from the VM
static inline void solu_popframe(solu_state *state) {
    solu_stackframe f = solu_frames_pop(&state->frames);
    for (uint32_t i = 0; i < f.size; ++i)
        solu_valvec_pop(&state->stack);
}

#define EXPECTED_NAME solu_load_ex
#define EXPECTED_O solu_fproto
#define EXPECTED_E solu_error
#include <sf/containers/expected.h>

/// Wrap a c function into a fun and insert it into a dynamic val
EXPORT solu_val solu_wrapcfun(solu_state *state, solu_cfunction fptr, uint32_t arg_c, solu_val *captures, uint32_t cap_c);
static inline solu_val solu_capturec(solu_state *state, uint32_t index) {
    if (!state->ccall || index > state->ccall->up_c - 1 || state->ccall->upvals[index].tt != SOLU_UP_VAL)
        return SOLU_NIL;
    return state->ccall->upvals[index].value;
}
static inline solu_val solu_selfc(solu_state *state) {
    if (state->ccall->up_c && sf_str_eq(state->ccall->upvals[0].name, sf_lit("self"))) {
        state->frames.data[state->frames.count - 1].self = true;
        return solu_capturec(state, 0);
    }
    return solu_get(state, 0);
}
/// Wrap a c function into a member fun (has the 'self' upvalue)
static inline solu_val solu_wrapmfun(solu_state *state, solu_cfunction fptr, uint32_t arg_c, solu_val *captures, uint32_t cap_c) {
    solu_val fun = solu_wrapcfun(state, fptr, arg_c, captures, cap_c);
    solu_fproto *f = fun.dyn;
    f->self = true;
    solu_upvalue *u = f->upvals;
    f->upvals = malloc(sizeof(solu_upvalue) * ++f->up_c);
    if (u) {
        memcpy(f->upvals + 1, u, sizeof(solu_upvalue) * (f->up_c - 1));
        free(u);
    }
    f->upvals[0] = (solu_upvalue){sf_lit("self"), SOLU_UP_VAL, .value = SOLU_NIL};
    return fun;
}
EXPORT void solu_savefun(solu_fproto *proto, char *path);
EXPORT solu_load_ex solu_loadfun(solu_state *state, char *path);

typedef struct {
    solu_error tt;
    char *panic;
    size_t pc;
} solu_call_err;
#define EXPECTED_NAME solu_call_ex
#define EXPECTED_O solu_val
#define EXPECTED_E solu_call_err
#include <sf/containers/expected.h>
/// Call a fun proto.
/// If you have a solu_val that refers to a fun type, you can use val.dyn for the arg `proto`.
/// Returns a value on success, or panic on failure
EXPORT solu_call_ex solu_call(solu_state *state, solu_fproto *proto, const solu_val *args, uint32_t arg_c);
/// Call a fun proto with debug breakpoints.
/// If you have a solu_val that refers to a fun type, you can use val.dyn for the arg `proto`.
/// Returns a value on success, or panic on failure
EXPORT solu_call_ex solu_dcall(solu_state *state, solu_fproto *proto, const solu_val *args, uint32_t arg_c, bool *bps);

/// Convenience function for returning ok in API functions
static inline solu_call_ex solu_ok(solu_val return_val) {
    return solu_call_ex_ok(return_val);
}
/// Convenience function for returning err in API functions
EXPORT solu_call_ex solu_err(solu_state *state, char *fmt, ...);
/// Convenience function for returning panic in API functions
EXPORT solu_call_ex solu_panic(solu_state *state, char *fmt, ...);
/// Convenience function for cleaning up a panic
static inline void solu_panic_cleanup(solu_call_ex panic) {
    if (panic.err.panic) free(panic.err.panic);
}

#endif // VM_H
