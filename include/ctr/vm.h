#ifndef VM_H
#define VM_H

#include "bytecode.h"
#include "ctrc.h"

/// Represents a function's frame, or reserved registers, on the stack
typedef struct {
    uint32_t bottom_o;
    uint32_t size;
} ctr_stackframe;
#define VEC_NAME ctr_frames
#define VEC_T ctr_stackframe
#include <sf/containers/vec.h>

/// The main global state for the VM, responsible for the stack and any globals/caching
typedef struct ctr_state {
    ctr_valvec stack;
    ctr_frames frames;
    ctr_val global;
} ctr_state;
EXPORT ctr_state *ctr_state_new(void);
EXPORT void ctr_state_free(ctr_state *state);

/// Include the standard library defined in std.c into the global namespace
EXPORT void ctr_usestd(struct ctr_state *state);
EXPORT ctr_compile_ex ctr_compile(ctr_state *state, sf_str src);

/// Converts a value to an owned string.
EXPORT sf_str ctr_tostring(ctr_val val);
/// Dumps the current stack frame to an owned string.
EXPORT sf_str ctr_stackdump(ctr_state *state);

/// Get the value of a register from a specific stack frame
static inline ctr_val ctr_rawget(ctr_state *state, uint32_t index, uint32_t frame) {
    ctr_val val = ctr_valvec_get(&state->stack, state->frames.data[frame].bottom_o + index);
    if (ctr_isdtype(val, CTR_DREF))
        return *(ctr_val *)val.dyn;
    return val;
}
/// Get the value of a register from the current stack frame
static inline ctr_val ctr_get(ctr_state *state, uint32_t index) { return ctr_rawget(state, index, state->frames.count - 1); }
/// Set the value of a register in a specific stack frame.
/// Note: this may free resources, if it replaces a dynamic value
static inline void ctr_rawset(ctr_state *state, uint32_t index, ctr_val val, uint32_t frame) {
    ctr_val old = ctr_rawget(state, index, frame);
    if (old.tt == CTR_TDYN) {
        if (ctr_header(old)->tt == CTR_DREF) {
            *(ctr_val *)old.dyn = val;
            return;
        }
        ctr_ddel(old);
    }
    ctr_valvec_set(&state->stack, state->frames.data[frame].bottom_o + index, val);
}
/// Set the value of a register in the current stack frame.
/// Note: this may free resources, if it replaces a dynamic value
static inline void ctr_set(ctr_state *state, uint32_t index, ctr_val val) {
    ctr_rawset(state, index, val, state->frames.count - 1);
}
/// Get a global value by name. Returns nil if it's not found
static inline ctr_val ctr_getg(ctr_state *state, sf_str name) {
    ctr_dobj_ex ex = ctr_dobj_get((ctr_dobj *)state->global.dyn, name);
    if (!ex.is_ok) return CTR_NIL;
    return ex.ok;
}
/// Set a global value by name
static inline void ctr_setg(ctr_state *state, sf_str name, ctr_val value) {
    ctr_dobj_set((ctr_dobj *)state->global.dyn, sf_str_dup(name), value);
}

/// Wrap a c function into a fun and insert it into a dynamic val
EXPORT ctr_val ctr_wrapcfun(ctr_cfunction fptr, uint32_t arg_c, uint32_t temp_c);

typedef struct {
    ctr_error tt;
    sf_str panic;
    size_t pc;
} ctr_call_err;
#define EXPECTED_NAME ctr_call_ex
#define EXPECTED_O ctr_val
#define EXPECTED_E ctr_call_err
#include <sf/containers/expected.h>
EXPORT ctr_call_ex ctr_call(ctr_state *state, ctr_fproto *proto, const ctr_val *args);

#endif // VM_H
