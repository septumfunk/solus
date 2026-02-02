#ifndef STD_H
#define STD_H

#include "sol/vm.h"
#include <sf/fs.h>

#define sol_serr(T, str) \
    sol_call_ex_err((sol_call_err){T, \
        str, \
    0})
#define sol_serrf(T, fmt, ...) \
    sol_call_ex_err((sol_call_err){T, \
        sf_str_fmt(fmt, __VA_ARGS__).c_str, \
    0})
#define expect_type(T, val) do { \
    if (val.tt != T) \
        return sol_serrf(SOL_ERRV_TYPE_MISMATCH, "'%s' expected %s, found %s", #val, SOL_TYPE_NAMES[T], sol_typename(val).c_str); \
} while (0);
#define expect_dtype(T, val) do { \
    if (!sol_isdtype(val, T)) \
        return sol_serrf(SOL_ERRV_TYPE_MISMATCH, "'%s' expected %s, found %s", #val, SOL_TYPE_NAMES[(int)SOL_TDYN + 1 + T], sol_typename(val).c_str); \
} while (0);

EXPORT void sol_mod_builtin(sol_state *s);
EXPORT void sol_mod_io(sol_state *s);
EXPORT void sol_mod_string(sol_state *s);
EXPORT void sol_mod_obj(sol_state *s);
EXPORT void sol_mod_math(sol_state *s);
EXPORT void sol_mod_gc(sol_state *s);

#endif // STD_H
