#ifndef VM_H
#define VM_H

#include "bytecode.h"
#include "ctrc.h"

typedef struct {
    uint32_t bottom_o;
    uint32_t size;
} ctr_stackframe;

typedef struct ctr_state {
    ctr_valvec stack;
    ctr_protovec protos;
    ctr_stackframe *frames;
    uint32_t frame_c;
    ctr_val global;
} ctr_state;

EXPORT ctr_state *ctr_state_new(void);
EXPORT void ctr_state_free(ctr_state *state);

EXPORT ctr_compile_ex ctr_compile(ctr_state *state, sf_str src);

EXPORT sf_str ctr_tostring(ctr_val val);
EXPORT sf_str ctr_stackdump(ctr_state *state);

static inline ctr_val ctr_rawget(ctr_state *state, uint32_t index, uint32_t frame) {
    ctr_val val = ctr_valvec_get(&state->stack, state->frames[frame].bottom_o + index);
    if (val.tt == CTR_TDYN && ctr_header(val)->tt == CTR_DVAL)
        return *(ctr_val *)val.val.dyn;
    return val;
}
static inline ctr_val ctr_get(ctr_state *state, uint32_t index) { return ctr_rawget(state, index, state->frame_c - 1); }
static inline void ctr_rawset(ctr_state *state, uint32_t index, ctr_val val, uint32_t frame) {
    ctr_val old = ctr_rawget(state, index, frame);
    if (old.tt == CTR_TDYN) {
        if (ctr_header(old)->tt == CTR_DVAL) {
            *(ctr_val *)old.val.dyn = val;
            return;
        }
        ctr_ddel(old);
    }
    ctr_valvec_set(&state->stack, state->frames[frame].bottom_o + index, val);
}
static inline void ctr_set(ctr_state *state, uint32_t index, ctr_val val) {
    ctr_rawset(state, index, val, state->frame_c - 1);
}

static inline ctr_val ctr_getg(ctr_state *state, sf_str name) {
    ctr_dobj_ex ex = ctr_dobj_get((ctr_dobj *)state->global.val.dyn, name);
    if (!ex.is_ok) return CTR_NIL;
    return ex.value.ok;
}
static inline void ctr_setg(ctr_state *state, sf_str name, ctr_val value) {
    ctr_dobj_set((ctr_dobj *)state->global.val.dyn, sf_str_dup(name), value);
}

EXPORT ctr_val ctr_wrapcfun(ctr_cfunction fptr, uint32_t arg_c, uint32_t temp_c);

typedef struct {
    enum ctr_call_errt {
        CTR_ERRV_UNKNOWN_OP,
        CTR_ERRV_OOB_ACCESS,
        CTR_ERRV_TYPE_MISMATCH,
        CTR_ERRV_OBJ_NO_MEMBER,
        CTR_ERRV_RUNTIME_ERR,
    } tt;
    sf_str string;
    size_t pc;
} ctr_call_err;
#define EXPECTED_NAME ctr_call_ex
#define EXPECTED_O ctr_val
#define EXPECTED_E ctr_call_err
#include <sf/containers/expected.h>
EXPORT ctr_call_ex ctr_call(ctr_state *state, ctr_proto *proto, const ctr_val *args);

#endif // VM_H
