#ifndef VM_H
#define VM_H

#include "bytecode.h"
#include "compiler.h"
#include <_stdlib.h>
#include <stdarg.h>

/// Represents a function's frame, or compiler reserved registers, on the stack
typedef struct {
    uint32_t bottom_o;
    uint32_t size;
} solu_stackframe;
/// Stack frame stack (i know that sounds confusing)
#define VEC_NAME solu_frames
#define VEC_T solu_stackframe
#define VSIZE_T uint32_t
#include <sf/containers/vec.h>
/// File name stack
#define VEC_NAME solu_filenames
#define VEC_T sf_str
#define VSIZE_T uint32_t
#include <sf/containers/vec.h>

/// GC will collect after cb = lb * SOLU_GCSTEP
#define SOLU_GCSTEP 1.5

/// The main global state for the VM, responsible for the stack and any globals/caching
typedef struct solu_state {
    solu_valvec stack; // registers/stack
    solu_frames frames; // stack frames
    solu_filenames files; // filename stack
    solu_val global; // _g

    bool collect;
    solu_dalloc *alloc, *alloc_tail; // gc allocations
    solu_strcache strcache; // short string cache
    size_t lb, cb, nb; // last bytes, current bytes, next bytes

    bool rcmp; // Special flag for compiled files to reset the frame count
} solu_state;
/// Create a new soluus VM state
EXPORT solu_state *solu_state_new(void);
/// Clean up a soluus VM's state
EXPORT void solu_state_free(solu_state *state);

/// Load the soluus standard library into the global namespace.
/// The standard library is implemented in C functions
EXPORT void solu_usestd(solu_state *state);
/// Compile soluus source code.
/// Returns a fun proto or an error
EXPORT solu_compile_ex solu_csrc(solu_state *state, char *src);
/// Compile a soluus source code file.
/// Returns a fun proto or an error
EXPORT solu_compile_ex solu_cfile(solu_state *state, char *path);

/// Manually push an allocation to the gc.
/// Not recommended for users :)
void solu_dpush(solu_state *s, solu_dalloc *ac);
/// Construct a new dynamic type
EXPORT solu_val solu_dnew(solu_state *state, solu_dtype type);
/// Constructs a dynamic usertype object, a dynamic type with extra user info.
/// User types are managed by the GC so make sure you use solu_dhold if you don't want them to be!
EXPORT solu_val solu_dnusr(solu_state *state, size_t size, const char *name, void *value, solu_usrdel del, solu_usrtostring tostring);
/// Shorthand for using solu_dnew and assigning a string value.
solu_val solu_dnstr(solu_state *state, const char *str);
/// Shorthand for using solu_dnew and assigning a string value.
static inline solu_val solu_dnerr(solu_state *state, const char *str) {
    solu_val err = solu_dnstr(state, str);
    solu_dheader(err)->tt = SOLU_DERR;
    return err;
}

/// Hold a reference to the a dyn value for the C API.
/// This marks the object as green, meaning collection is skipped
static inline void solu_dhold(solu_val val) {
    if (val.tt != SOLU_TDYN) return;
    solu_dheader(val)->mark = SOLU_DYN_GREEN;
}
/// Release a reference held to a dyn value in the C API
static inline void solu_drelease(solu_val val) {
    if (val.tt != SOLU_TDYN || solu_dheader(val)->mark != SOLU_DYN_GREEN) return;
    solu_dheader(val)->mark = SOLU_DYN_WHITE;
}

/// Mark and Sweep garbage collection
EXPORT void solu_dcollect(solu_state *state);

/// Converts a value to a string.
/// You are responsible for freeing this string
EXPORT char *solu_tostring(solu_val val);

/// Get the value of a register from a specific stack frame
static inline solu_val solu_rawget(solu_state *state, uint32_t index, uint32_t frame) {
    solu_val val = solu_valvec_get(&state->stack, state->frames.data[frame].bottom_o + index);
    if (solu_isdtype(val, SOLU_DREF))
        return *(solu_val *)val.dyn;
    return val;
}
/// Get the value of a register from the current stack frame.
/// In the C API this can be used to get function arguments (0, 1, 2...)
static inline solu_val solu_get(solu_state *state, uint32_t index) { return solu_rawget(state, index, state->frames.count - 1); }
/// Get the value of a constant from the current fun
static inline solu_val solu_getk(solu_fproto *proto, uint32_t index) { return *(proto->constants.data + index); }

/// Set the value of a register in a specific stack frame
static inline void solu_rawset(solu_state *state, uint32_t index, solu_val val, uint32_t frame) {
    solu_valvec_set(&state->stack, state->frames.data[frame].bottom_o + index, val);
}
/// Set the value of a register in the current stack frame
static inline void solu_set(solu_state *state, uint32_t index, solu_val val) {
    solu_rawset(state, index, val, state->frames.count - 1);
}
/// Get a global value by name. Returns nil if it's not found
static inline solu_val solu_getg(solu_state *state, sf_str name) {
    solu_dobj_ex ex = solu_dobj_get((solu_dobj *)state->global.dyn, name);
    if (!ex.is_ok) return SOLU_NIL;
    return ex.ok;
}
/// Set a global value by name
static inline void solu_setg(solu_state *state, char *name, solu_val value) {
    solu_dobj_set((solu_dobj *)state->global.dyn, sf_str_cdup(name), value);
}
/// Push a stack frame to the VM
static inline uint32_t solu_pushframe(solu_state *state, uint32_t reg_c) {
    solu_frames_push(&state->frames, (solu_stackframe){
        state->frames.count == 0 ? 0 : state->frames.data[state->frames.count - 1].bottom_o + state->frames.data[state->frames.count - 1].size,
        reg_c,
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
EXPORT solu_val solu_wrapcfun(solu_state *state, solu_cfunction fptr, uint32_t arg_c, uint32_t temp_c);
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
EXPORT solu_call_ex solu_panic(char *fmt, ...);

#endif // VM_H
