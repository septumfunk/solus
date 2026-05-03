#ifndef SOLUC_H
#define SOLUC_H

#include "val.h"
#include "syntax.h"

#define EXPECTED_NAME solu_compile_ex
#define EXPECTED_O solu_fproto
#define EXPECTED_E solu_ctrace *
#include <sf/containers/expected.h>
/// Compile a solu_proto from source code
EXPORT solu_compile_ex solu_cproto(sf_str path, char *src, uint32_t arg_c, solu_val *args, uint32_t up_c, solu_upvalue *upvals);

#endif // SOLUC_H
