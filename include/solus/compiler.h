#ifndef SOLUC_H
#define SOLUC_H

#include <stddef.h>
#include "bytecode.h"

typedef struct {
    solu_error tt;
    uint16_t line, column;
} solu_compile_err;

#define EXPECTED_NAME solu_compile_ex
#define EXPECTED_O solu_fproto
#define EXPECTED_E solu_compile_err
#include <sf/containers/expected.h>
/// Compile a solu_proto from source code
EXPORT solu_compile_ex solu_cproto(sf_str path, char *src, uint32_t arg_c, solu_val *args, uint32_t up_c, solu_upvalue *upvals);

#endif // SOLUC_H
