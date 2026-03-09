#include "std.h"
#include <time.h>

static solu_call_ex math_mini(solu_state *s) {
    solu_val a = solu_get(s, 0);
    expect_type(SOLU_TI64, a);
    solu_val b = solu_get(s, 1);
    expect_type(SOLU_TI64, b);
    return solu_ok((solu_val){SOLU_TI64, .i64 = min(a.i64, b.i64)});
}
static solu_call_ex math_maxi(solu_state *s) {
    solu_val a = solu_get(s, 0);
    expect_type(SOLU_TI64, a);
    solu_val b = solu_get(s, 1);
    expect_type(SOLU_TI64, b);
    return solu_ok((solu_val){SOLU_TI64, .i64 = max(a.i64, b.i64)});
}
static solu_call_ex math_minf(solu_state *s) {
    solu_val a = solu_get(s, 0);
    expect_type(SOLU_TF64, a);
    solu_val b = solu_get(s, 1);
    expect_type(SOLU_TF64, b);
    return solu_ok((solu_val){SOLU_TF64, .f64 = min(a.f64, b.f64)});
}
static solu_call_ex math_maxf(solu_state *s) {
    solu_val a = solu_get(s, 0);
    expect_type(SOLU_TF64, a);
    solu_val b = solu_get(s, 1);
    expect_type(SOLU_TF64, b);
    return solu_ok((solu_val){SOLU_TF64, .f64 = max(a.f64, b.f64)});
}
static solu_call_ex math_randi(solu_state *s) {
    solu_val min_v = solu_get(s, 0);
    solu_val max_v = solu_get(s, 1);
    solu_i64 min, max;
    if (min_v.tt != SOLU_TI64) {
        if (min_v.tt == SOLU_TF64) min = min_v.i64;
        else {
            return solu_panic("'min' expected i64, found %s", solu_typename(min_v).c_str);
        }
    } else min = min_v.i64;
    if (max_v.tt != SOLU_TI64) {
        if (max_v.tt == SOLU_TF64) max = max_v.i64;
        else {
            return solu_panic("'max' expected i64, found %s", solu_typename(max_v).c_str);
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
    return solu_ok((solu_val){ .tt = SOLU_TI64, .i64 = (int64_t)(r % range) + min });
}
static solu_call_ex math_randf(solu_state *s) {
    solu_val min_v = solu_get(s, 0);
    solu_val max_v = solu_get(s, 1);
    double min, max;

    if (min_v.tt == SOLU_TF64) min = min_v.f64;
    else if (min_v.tt == SOLU_TI64) min = (double)min_v.i64;
    else {
        return solu_panic("'min' expected f64, found %s", solu_typename(min_v).c_str);
    }

    if (max_v.tt == SOLU_TF64) max = max_v.f64;
    else if (max_v.tt == SOLU_TI64) max = (double)max_v.i64;
    else {
        return solu_panic("'max' expected f64, found %s", solu_typename(max_v).c_str);
    }

    if (min > max) { double tmp = min; min = max; max = tmp; }

    double frac = (double)rand() / (double)RAND_MAX; // [0, 1]
    double val = min + frac * (max - min);

    return solu_ok((solu_val){ .tt = SOLU_TF64, .f64 = val });
}

solu_val solu_mod_math(solu_state *s) {
    solu_val math = solu_dnew(s, SOLU_DOBJ);
    solu_dobj_strset(math.dyn, "mini", solu_wrapcfun(s, math_mini, 2, NULL, 0));
    solu_dobj_strset(math.dyn, "maxi", solu_wrapcfun(s, math_maxi, 2, NULL, 0));
    solu_dobj_strset(math.dyn, "minf", solu_wrapcfun(s, math_minf, 2, NULL, 0));
    solu_dobj_strset(math.dyn, "maxf", solu_wrapcfun(s, math_maxf, 2, NULL, 0));
    solu_dobj_strset(math.dyn, "randi", solu_wrapcfun(s, math_randi, 2, NULL, 0));
    solu_dobj_strset(math.dyn, "randf", solu_wrapcfun(s, math_randf, 2, NULL, 0));
    solu_dobj_strset(s->global.dyn, "math", math);
    srand((unsigned)time(NULL));
    return math;
}
