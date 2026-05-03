#ifndef STD_H
#define STD_H

#include "solus/api.h"

#define expect_type(T, val) do { \
    if (val.tt != T) \
        return solu_panic(s, "'%s' expected %s, found %s", #val, SOLU_TYPE_NAMES[T], solu_typename(val).c_str); \
} while (0);
#define expect_dtype(T, val) do { \
    if (!solu_isdtype(val, T)) \
        return solu_panic(s, "'%s' expected %s, found %s", #val, SOLU_TYPE_NAMES[(int)SOLU_TDYN + 1 + T], solu_typename(val).c_str); \
} while (0);

solu_call_ex builtin_then(solu_state *s);
solu_call_ex builtin_type(solu_state *s);
solu_call_ex builtin_str(solu_state *s);
solu_call_ex builtin_unwrap(solu_state *s);
solu_call_ex builtin_or_else(solu_state *s);

solu_call_ex string_join(solu_state *s);

EXPORT void solu_mod_builtin(solu_state *s);
EXPORT solu_val solu_mod_io(solu_state *s);
EXPORT solu_val solu_mod_string(solu_state *s, bool meta);
EXPORT solu_val solu_mod_obj(solu_state *s, bool meta);
EXPORT solu_val solu_mod_math(solu_state *s);
EXPORT solu_val solu_mod_gc(solu_state *s);

#endif // STD_H
