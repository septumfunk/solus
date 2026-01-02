#ifndef SOLC_H
#define SOLC_H

#include <stddef.h>
#include "bytecode.h"

typedef struct {
    sol_error tt;
    uint16_t line, column;
} sol_compile_err;

#define EXPECTED_NAME sol_compile_ex
#define EXPECTED_O sol_fproto
#define EXPECTED_E sol_compile_err
#include <sf/containers/expected.h>
/// Compile a sol_proto from source code
EXPORT sol_compile_ex sol_cproto(sf_str src, uint32_t arg_c, sol_val *args, uint32_t up_c, sol_upvalue *upvals, bool echo);

#endif // SOLC_H
