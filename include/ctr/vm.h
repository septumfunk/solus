#ifndef VM_H
#define VM_H

#include "bytecode.h"

typedef struct {
    ctr_valvec stack;
    ctr_protovec protos;
    size_t stack_o;
} ctr_state;

EXPORT ctr_state *ctr_state_new(void);
EXPORT void ctr_state_free(ctr_state *state);

EXPORT sf_str ctr_tostring(ctr_val val);
EXPORT sf_str ctr_stackdump(ctr_state *state);

static inline ctr_val ctr_get(ctr_state *state, size_t index) {
    return ctr_valvec_get(&state->stack, state->stack_o + index);
}
static inline void ctr_set(ctr_state *state, size_t index, ctr_val val) {
    ctr_val old = ctr_get(state, index);
    if (old.tt == CTR_TDYN)
        ctr_ddel(old);
    ctr_valvec_set(&state->stack, state->stack_o + index, val);
}


typedef struct {
    enum {
        CTR_CALL_UNKNOWN_OP,
        CTR_CALL_OOB_ACCESS,
        CTR_CALL_TYPE_MISMATCH,
    } type;
    sf_str string;
    size_t pc;
} ctr_call_err;
#define EXPECTED_NAME ctr_call_ex
#define EXPECTED_O ctr_val
#define EXPECTED_E ctr_call_err
#include <sf/containers/expected.h>
EXPORT ctr_call_ex ctr_call(ctr_state *state, ctr_proto *proto, const ctr_val *args);

#endif // VM_H
