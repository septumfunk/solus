#ifndef STD_H
#define STD_H

#include "solus/vm.h"
#include <sf/fs.h>

#define solu_serr(T, str) \
    solu_call_ex_err((solu_call_err){T, \
        str, \
    0})
#define solu_serrf(T, fmt, ...) \
    solu_call_ex_err((solu_call_err){T, \
        sf_str_fmt(fmt, __VA_ARGS__).c_str, \
    0})
#define expect_type(T, val) do { \
    if (val.tt != T) \
        return solu_serrf(SOLU_ERRV_TYPE_MISMATCH, "'%s' expected %s, found %s", #val, SOLU_TYPE_NAMES[T], solu_typename(val).c_str); \
} while (0);
#define expect_dtype(T, val) do { \
    if (!solu_isdtype(val, T)) \
        return solu_serrf(SOLU_ERRV_TYPE_MISMATCH, "'%s' expected %s, found %s", #val, SOLU_TYPE_NAMES[(int)SOLU_TDYN + 1 + T], solu_typename(val).c_str); \
} while (0);

EXPORT void solu_mod_builtin(solu_state *s);
EXPORT void solu_mod_io(solu_state *s);
EXPORT void solu_mod_string(solu_state *s);
EXPORT void solu_mod_obj(solu_state *s);
EXPORT void solu_mod_math(solu_state *s);
EXPORT void solu_mod_gc(solu_state *s);

#endif // STD_H
