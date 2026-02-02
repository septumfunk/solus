#include "std.h"

static sol_call_ex math_mini(sol_state *s) {
    sol_val a = sol_get(s, 0);
    expect_type(SOL_TI64, a);
    sol_val b = sol_get(s, 0);
    expect_type(SOL_TI64, b);
    return sol_call_ex_ok((sol_val){SOL_TI64, .i64 = min(a.i64, b.i64)});
}
static sol_call_ex math_maxi(sol_state *s) {
    sol_val a = sol_get(s, 0);
    expect_type(SOL_TI64, a);
    sol_val b = sol_get(s, 0);
    expect_type(SOL_TI64, b);
    return sol_call_ex_ok((sol_val){SOL_TI64, .i64 = max(a.i64, b.i64)});
}
static sol_call_ex math_minf(sol_state *s) {
    sol_val a = sol_get(s, 0);
    expect_type(SOL_TF64, a);
    sol_val b = sol_get(s, 0);
    expect_type(SOL_TF64, b);
    return sol_call_ex_ok((sol_val){SOL_TF64, .f64 = min(a.f64, b.f64)});
}
static sol_call_ex math_randi(sol_state *s) {
    sol_val min_v = sol_get(s, 0);
    sol_val max_v = sol_get(s, 1);
    sol_i64 min, max;
    if (min_v.tt != SOL_TI64) {
        if (min_v.tt == SOL_TF64) min = min_v.i64;
        else {
            return sol_call_ex_err((sol_call_err){SOL_ERRV_TYPE_MISMATCH,
                sf_str_fmt("'min' expected i64, found %s", sol_typename(min_v).c_str).c_str,
            0});
        }
    } else min = min_v.i64;
    if (max_v.tt != SOL_TI64) {
        if (max_v.tt == SOL_TF64) max = max_v.i64;
        else {
            return sol_call_ex_err((sol_call_err){SOL_ERRV_TYPE_MISMATCH,
                sf_str_fmt("'max' expected i64, found %s", sol_typename(max_v).c_str).c_str,
            0});
        }
    } else max = max_v.i64;

    if (min > max) { int64_t tmp = min; min = max; max = tmp; }
    uint64_t range = (uint64_t)(max - min) + 1;
#if defined(_WIN32)
    uint64_t r = ((uint64_t)rand() << 48) | ((uint64_t)rand() << 32) |
                 ((uint64_t)rand() << 16) | (uint64_t)rand();
#else
    uint64_t r = (uint64_t)rand();
#endif
    return sol_call_ex_ok((sol_val){ .tt = SOL_TI64, .i64 = (int64_t)(r % range) + min });
}
static sol_call_ex math_randf(sol_state *s) {
    sol_val min_v = sol_get(s, 0);
    sol_val max_v = sol_get(s, 1);
    double min, max;

    if (min_v.tt == SOL_TF64) min = min_v.f64;
    else if (min_v.tt == SOL_TI64) min = (double)min_v.i64;
    else {
        return sol_call_ex_err((sol_call_err){SOL_ERRV_TYPE_MISMATCH,
            sf_str_fmt("'min' expected f64, found %s", sol_typename(min_v).c_str).c_str,
        0});
    }

    if (max_v.tt == SOL_TF64) max = max_v.f64;
    else if (max_v.tt == SOL_TI64) max = (double)max_v.i64;
    else {
        return sol_call_ex_err((sol_call_err){SOL_ERRV_TYPE_MISMATCH,
            sf_str_fmt("'max' expected f64, found %s", sol_typename(max_v).c_str).c_str,
        0});
    }

    if (min > max) { double tmp = min; min = max; max = tmp; }

    double frac = (double)rand() / (double)RAND_MAX; // [0, 1]
    double val = min + frac * (max - min);

    return sol_call_ex_ok((sol_val){ .tt = SOL_TF64, .f64 = val });
}

void sol_mod_math(sol_state *s) {
    sol_val math = sol_dnew(s, SOL_DOBJ);
    sol_dobj_set(math.dyn, sf_lit("randi"), sol_wrapcfun(s, math_randi, 2, 0));
    sol_dobj_set(math.dyn, sf_lit("randf"), sol_wrapcfun(s, math_randf, 2, 0));
    sol_dobj_set(s->global.dyn, sf_lit("math"), math);
    srand((unsigned)time(NULL));
}
