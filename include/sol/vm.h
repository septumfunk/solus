#ifndef VM_H
#define VM_H

#include "bytecode.h"
#include "solc.h"

/// Represents a function's frame, or reserved registers, on the stack
typedef struct {
    uint32_t bottom_o;
    uint32_t size;
} sol_stackframe;
#define VEC_NAME sol_frames
#define VEC_T sol_stackframe
#define VSIZE_T uint32_t
#include <sf/containers/vec.h>

#define VEC_NAME sol_filenames
#define VEC_T sf_str
#define VSIZE_T uint32_t
#include <sf/containers/vec.h>

/// The main global state for the VM, responsible for the stack and any globals/caching
typedef struct sol_state {
    sol_valvec stack;
    sol_frames frames;
    sol_filenames files;
    sol_val global;
    bool dbg;

    sol_dalloc *alloc;
} sol_state;
EXPORT sol_state *sol_state_new(void);
EXPORT void sol_state_free(sol_state *state);

/// Include the standard library defined in std.c into the global namespace
EXPORT void sol_usestd(struct sol_state *state);
EXPORT sol_compile_ex sol_csrc(sol_state *state, sf_str src);
EXPORT sol_compile_ex sol_cfile(sol_state *state, sf_str path);

static inline sf_str sol_cwd(sol_state *state) {
    return sf_str_dup(*(state->files.data + (state->files.count - 1)));
}

/// Converts a value to an owned string.
EXPORT sf_str sol_tostring(sol_val val);
/// Dumps the current stack frame to an owned string.
EXPORT sf_str sol_stackdump(sol_state *state);

/// Create a new dynamic value
EXPORT sol_val sol_dnew(sol_state *state, sol_dtype type);
EXPORT sol_val sol_dcopy(sol_state *state, sol_val val);
EXPORT void sol_dcollect(sol_state *state);
/// Shorthand for using sol_dnew and assigning a string value.
/// This function takes ownership of the string passed, so make a copy if needed
static inline sol_val sol_dnstr(sol_state *state, sf_str str) {
    sol_val strv = sol_dnew(state, SOL_DSTR);
    *(sf_str *)strv.dyn = str;
    return strv;
}
/// Shorthand for using sol_dnew and assigning a string value.
/// This function takes ownership of the string passed, so make a copy if needed
static inline sol_val sol_dnerr(sol_state *state, sf_str str) {
    sol_val strv = sol_dnew(state, SOL_DERR);
    *(sf_str *)strv.dyn = str;
    return strv;
}
/// Hold a reference to the a dyn value for the C API.
/// This marks the object as green, meaning collection is skipped
static inline void sol_dhold(sol_val val) {
    if (val.tt != SOL_TDYN) return;
    sol_dheader(val)->mark = SOL_DYN_GREEN;
}
/// Release a reference held to a dyn value in the C API
static inline void sol_drelease(sol_val val) {
    if (val.tt != SOL_TDYN || sol_dheader(val)->mark != SOL_DYN_GREEN) return;
    sol_dheader(val)->mark = SOL_DYN_WHITE;
}

/// Get the value of a register from a specific stack frame
static inline sol_val sol_rawget(sol_state *state, uint32_t index, uint32_t frame) {
    sol_val val = sol_valvec_get(&state->stack, state->frames.data[frame].bottom_o + index);
    if (sol_isdtype(val, SOL_DREF))
        return *(sol_val *)val.dyn;
    return val;
}
/// Get the value of a register from the current stack frame
static inline sol_val sol_get(sol_state *state, uint32_t index) { return sol_rawget(state, index, state->frames.count - 1); }
/// Set the value of a register in a specific stack frame.
static inline void sol_rawset(sol_state *state, uint32_t index, sol_val val, uint32_t frame) {
    sol_valvec_set(&state->stack, state->frames.data[frame].bottom_o + index, val);
}
/// Set the value of a register in the current stack frame.
static inline void sol_set(sol_state *state, uint32_t index, sol_val val) {
    sol_rawset(state, index, val, state->frames.count - 1);
}
/// Get a global value by name. Returns nil if it's not found
static inline sol_val sol_getg(sol_state *state, sf_str name) {
    sol_dobj_ex ex = sol_dobj_get((sol_dobj *)state->global.dyn, name);
    if (!ex.is_ok) return SOL_NIL;
    return ex.ok;
}
/// Set a global value by name
static inline void sol_setg(sol_state *state, sf_str name, sol_val value) {
    sol_dobj_set((sol_dobj *)state->global.dyn, sf_str_dup(name), value);
}
static inline uint32_t sol_pushframe(sol_state *state, uint32_t reg_c) {
    sol_frames_push(&state->frames, (sol_stackframe){
        state->frames.count == 0 ? 0 : state->frames.data[state->frames.count - 1].bottom_o + state->frames.data[state->frames.count - 1].size,
        reg_c,
    });
    for (uint32_t i = 0; i < reg_c; ++i)
        sol_valvec_push(&state->stack, SOL_NIL);
    return state->frames.count - 1;
}
static inline void sol_popframe(sol_state *state) {
    sol_stackframe f = sol_frames_pop(&state->frames);
    for (uint32_t i = 0; i < f.size; ++i)
        sol_valvec_pop(&state->stack);
}

/// Wrap a c function into a fun and insert it into a dynamic val
EXPORT sol_val sol_wrapcfun(sol_state *state, sol_cfunction fptr, uint32_t arg_c, uint32_t temp_c);

typedef struct {
    sol_error tt;
    sf_str panic;
    size_t pc;
} sol_call_err;
#define EXPECTED_NAME sol_call_ex
#define EXPECTED_O sol_val
#define EXPECTED_E sol_call_err
#include <sf/containers/expected.h>
EXPORT sol_call_ex sol_call(sol_state *state, sol_fproto *proto, const sol_val *args);
EXPORT sol_call_ex sol_dcall(sol_state *state, sol_fproto *proto, const sol_val *args, bool *bps);

#endif // VM_H
