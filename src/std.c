#include "ctr/std.h"
#include "ctr/bytecode.h"
#include "ctr/vm.h"
#include <stdint.h>
#include <stdlib.h>
#include <time.h>

#if defined(_WIN32) || defined(_WIN64)
#include <windows.h>
double ctr_timesec(void) {
    FILETIME ft;
    ULARGE_INTEGER uli;
    GetSystemTimeAsFileTime(&ft);
    uli.LowPart  = ft.dwLowDateTime;
    uli.HighPart = ft.dwHighDateTime;
    return (double)(uli.QuadPart - 116444736000000000ULL) / 10000000.0;
}
#else
#include <time.h>
#include <sys/time.h>
double ctr_timesec(void) {
#if defined(CLOCK_REALTIME)
    struct timespec ts;
    clock_gettime(CLOCK_REALTIME, &ts);
    return (double)ts.tv_sec + (double)ts.tv_nsec / 1e9;
#else
    struct timeval tv;
    gettimeofday(&tv, NULL);
    return (double)tv.tv_sec + (double)tv.tv_usec / 1e6;
#endif
}
#endif

void ctr_usestd(struct ctr_state *state) {
    ctr_dobj *_g = state->global.val.dyn;
    ctr_dobj_set(_g, sf_lit("print"), ctr_wrapcfun(ctr_std_print, 1, 0));
    ctr_dobj_set(_g, sf_lit("println"), ctr_wrapcfun(ctr_std_println, 1, 0));
    ctr_dobj_set(_g, sf_lit("time"), ctr_wrapcfun(ctr_std_time, 0, 0));
    ctr_dobj_set(_g, sf_lit("error"), ctr_wrapcfun(ctr_std_error, 1, 0));
    ctr_dobj_set(_g, sf_lit("string"), ctr_wrapcfun(ctr_std_string, 1, 0));

    ctr_dobj_set(_g, sf_lit("new"), ctr_wrapcfun(ctr_std_new, 0, 0));
    ctr_dobj_set(_g, sf_lit("set"), ctr_wrapcfun(ctr_std_set, 3, 0));
    ctr_dobj_set(_g, sf_lit("get"), ctr_wrapcfun(ctr_std_get, 2, 0));

    ctr_dobj_set(_g, sf_lit("randi"), ctr_wrapcfun(ctr_std_randi, 2, 0));
    ctr_dobj_set(_g, sf_lit("randf"), ctr_wrapcfun(ctr_std_randf, 2, 0));

    srand((unsigned)time(NULL));
}

ctr_call_ex ctr_std_print(ctr_state *s) {
    ctr_val to_print = ctr_get(s, 0);
    sf_str val = ctr_tostring(to_print);
    printf("%s", val.c_str);
    sf_str_free(val);
    return ctr_call_ex_ok(CTR_NIL);
}
ctr_call_ex ctr_std_println(ctr_state *s) {
    ctr_val to_print = ctr_get(s, 0);
    sf_str val = ctr_tostring(to_print);
    printf("%s\n", val.c_str);
    sf_str_free(val);
    return ctr_call_ex_ok(CTR_NIL);
}
ctr_call_ex ctr_std_time(ctr_state *s) {
    (void)s;
    return ctr_call_ex_ok((ctr_val){.tt = CTR_TF64, .val.f64 = ctr_timesec()});
}
ctr_call_ex ctr_std_error(ctr_state *s) {
    ctr_val string = ctr_get(s, 0);
    return ctr_call_ex_err((ctr_call_err){CTR_ERRV_RUNTIME_ERR, sf_str_dup(*(sf_str *)string.val.dyn), 0});
}
ctr_call_ex ctr_std_string(ctr_state *s) {
    ctr_val var = ctr_get(s, 0);
    ctr_val str = ctr_dnew(CTR_DSTR);
    *(sf_str *)str.val.dyn = ctr_tostring(var);
    return ctr_call_ex_ok(str);
}

ctr_call_ex ctr_std_new(ctr_state *s) {
    (void)s;
    return ctr_call_ex_ok(ctr_dnew(CTR_DOBJ));
}
ctr_call_ex ctr_std_set(ctr_state *s) {
    ctr_val obj = ctr_get(s, 0);
    ctr_val key = ctr_get(s, 1);
    ctr_val val = ctr_get(s, 2);

    if (obj.tt != CTR_TDYN || ctr_header(obj)->tt != CTR_DOBJ)
        return ctr_call_ex_err((ctr_call_err){CTR_ERRV_TYPE_MISMATCH,
            sf_str_fmt("Arg 'obj' expected obj, found '%s'", ctr_typename(obj).c_str),
        0});

    sf_str kstr;
    if (key.tt != CTR_TDYN || ctr_header(obj)->tt != CTR_DSTR)
        kstr = ctr_tostring(key);
    else kstr = sf_str_dup(*(sf_str *)key.val.dyn);
    ctr_dobj_set(obj.val.dyn, kstr, ctr_dref(val));
    return ctr_call_ex_ok(CTR_NIL);
}
ctr_call_ex ctr_std_get(ctr_state *s) {
    ctr_val obj = ctr_get(s, 0);
    ctr_val key = ctr_get(s, 1);

    if (obj.tt != CTR_TDYN || ctr_header(obj)->tt != CTR_DOBJ)
        return ctr_call_ex_err((ctr_call_err){CTR_ERRV_TYPE_MISMATCH,
            sf_str_fmt("Arg 'obj' expected obj, found '%s'", ctr_typename(obj).c_str),
        0});

    sf_str kstr;
    if (key.tt != CTR_TDYN || ctr_header(obj)->tt != CTR_DSTR)
        kstr = ctr_tostring(key);
    else kstr = sf_str_dup(*(sf_str *)key.val.dyn);

    ctr_dobj_ex ex = ctr_dobj_get(obj.val.dyn, kstr);
    if (!ex.is_ok) {
        sf_str estr = sf_str_fmt("Object does not contain member '%s'", kstr.c_str);
        sf_str_free(kstr);
        return ctr_call_ex_err((ctr_call_err){CTR_ERRV_OBJ_NO_MEMBER, estr, 0});
    }

    return ctr_call_ex_ok(ctr_dref(ex.value.ok));
}

ctr_call_ex ctr_std_randi(ctr_state *s) {
    ctr_val min_v = ctr_get(s, 0);
    ctr_val max_v = ctr_get(s, 1);
    ctr_i64 min, max;
    if (min_v.tt != CTR_TI64) {
        if (min_v.tt == CTR_TF64) min = min_v.val.i64;
        else {
            return ctr_call_ex_err((ctr_call_err){CTR_ERRV_TYPE_MISMATCH,
                sf_str_fmt("Arg 'min' expected i64, found '%s'", ctr_typename(min_v).c_str),
            0});
        }
    } else min = min_v.val.i64;
    if (max_v.tt != CTR_TI64) {
        if (max_v.tt == CTR_TF64) max = max_v.val.i64;
        else {
            return ctr_call_ex_err((ctr_call_err){CTR_ERRV_TYPE_MISMATCH,
                sf_str_fmt("Arg 'max' expected i64, found '%s'", ctr_typename(max_v).c_str),
            0});
        }
    } else max = max_v.val.i64;

    if (min > max) { int64_t tmp = min; min = max; max = tmp; }
    uint64_t range = (uint64_t)(max - min) + 1;
#if defined(_WIN32)
    uint64_t r = ((uint64_t)rand() << 48) | ((uint64_t)rand() << 32) |
                 ((uint64_t)rand() << 16) | rand();
#else
    uint64_t r = (uint64_t)rand();
#endif
    return ctr_call_ex_ok((ctr_val){ .tt = CTR_TI64, .val.i64 = (int64_t)(r % range) + min });
}
ctr_call_ex ctr_std_randf(ctr_state *s) {
    ctr_val min_v = ctr_get(s, 0);
    ctr_val max_v = ctr_get(s, 1);
    double min, max;

    if (min_v.tt == CTR_TF64) min = min_v.val.f64;
    else if (min_v.tt == CTR_TI64) min = (double)min_v.val.i64;
    else {
        return ctr_call_ex_err((ctr_call_err){CTR_ERRV_TYPE_MISMATCH,
            sf_str_fmt("Arg 'min' expected f64, found '%s'", ctr_typename(min_v).c_str),
        0});
    }

    if (max_v.tt == CTR_TF64) max = max_v.val.f64;
    else if (max_v.tt == CTR_TI64) max = (double)max_v.val.i64;
    else {
        return ctr_call_ex_err((ctr_call_err){CTR_ERRV_TYPE_MISMATCH,
            sf_str_fmt("Arg 'max' expected f64, found '%s'", ctr_typename(max_v).c_str),
        0});
    }

    if (min > max) { double tmp = min; min = max; max = tmp; }

    double frac = (double)rand() / (double)RAND_MAX; // [0, 1]
    double val = min + frac * (max - min);

    return ctr_call_ex_ok((ctr_val){ .tt = CTR_TF64, .val.f64 = val });
}
