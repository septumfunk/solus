#include "sol/bytecode.h"
#include "sol/vm.h"
#include "sf/math.h"
#include "sf/str.h"
#include <sf/fs.h>
#include <stdint.h>
#include <stdlib.h>
#include <time.h>

#if defined(_WIN32) || defined(_WIN64)
#include <windows.h>
double sol_timesec(void) {
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
double sol_timesec(void) {
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

sol_call_ex sol_std_print(sol_state *s) {
    sol_val to_print = sol_get(s, 0);
    sf_str val = sol_tostring(to_print);
    printf("%s", val.c_str);
    sf_str_free(val);
    return sol_call_ex_ok(SOL_NIL);
}
sol_call_ex sol_std_println(sol_state *s) {
    sol_val to_print = sol_get(s, 0);
    sf_str val = sol_tostring(to_print);
    printf("%s\n", val.c_str);
    sf_str_free(val);
    return sol_call_ex_ok(SOL_NIL);
}
sol_call_ex sol_std_time(sol_state *s) {
    (void)s;
    return sol_call_ex_ok((sol_val){.tt = SOL_TF64, .f64 = sol_timesec()});
}
sol_call_ex sol_std_string(sol_state *s) {
    sol_val var = sol_get(s, 0);
    sol_val str = sol_dnew(s, SOL_DSTR);
    *(sf_str *)str.dyn = sol_tostring(var);
    return sol_call_ex_ok(str);
}
sol_call_ex sol_std_eval(sol_state *s) {
    sol_val src = sol_get(s, 0);
    if (!sol_isdtype(src, SOL_DSTR))
        return sol_call_ex_err((sol_call_err){SOL_ERRV_TYPE_MISMATCH,
            sf_str_fmt("Arg 'src' expected str, found '%s'", sol_typename(src).c_str),
        0});
    sol_compile_ex cm_ex = sol_csrc(s, *(sf_str *)src.dyn);
    if (!cm_ex.is_ok)
        return sol_call_ex_ok(sol_dnerr(s, sf_str_dup(sol_err_string(cm_ex.err.tt))));
    sol_call_ex cl_ex = sol_call(s, &cm_ex.ok, NULL);
    sol_fproto_free(&cm_ex.ok);
    if (!cl_ex.is_ok)
            return sol_call_ex_ok(sol_dnerr(s, sf_str_dup(cl_ex.err.tt == SOL_ERRV_PANIC ?
                cl_ex.err.panic :
                sol_err_string(cm_ex.err.tt))
            ));
    return cl_ex;
}
sol_call_ex sol_std_import(sol_state *s) {
    sol_val path = sol_get(s, 0);
    if (!sol_isdtype(path, SOL_DSTR))
        return sol_call_ex_err((sol_call_err){SOL_ERRV_TYPE_MISMATCH,
            sf_str_fmt("Arg 'path' expected str, found '%s'", sol_typename(path).c_str),
        0});

    sf_str cwd = sol_cwd(s);

    sf_str p = sf_str_fmt("%s%s", cwd.c_str, ((sf_str *)path.dyn)->c_str);
    if (!sf_file_exists(p)) {
        sf_str p2 = sf_str_fmt("%s.sol", p.c_str);
        sf_str_free(p);
        p = p2;
    }
    if (!sf_file_exists(p)) {
        sf_str p2 = sf_str_fmt("File '%s' not found", p.c_str);
        sf_str_free(p);
        return sol_call_ex_ok(sol_dnerr(s, p2));
    }

    sol_compile_ex cm_ex = sol_cfile(s, p);
    if (!cm_ex.is_ok)
        return sol_call_ex_ok(sol_dnerr(s, sf_str_dup(sol_err_string(cm_ex.err.tt))));
    sol_call_ex cl_ex = sol_call(s, &cm_ex.ok, NULL);
    sol_fproto_free(&cm_ex.ok);
    if (!cl_ex.is_ok)
            return sol_call_ex_ok(sol_dnerr(s, sf_str_dup(cl_ex.err.tt == SOL_ERRV_PANIC ?
                cl_ex.err.panic :
                sol_err_string(cm_ex.err.tt))
            ));
    return cl_ex;
}
sol_call_ex sol_std_collect(sol_state *s) {
    sol_dcollect(s);
    return sol_call_ex_ok(SOL_NIL);
}

sol_call_ex sol_std_new(sol_state *s) {
    (void)s;
    return sol_call_ex_ok(sol_dnew(s, SOL_DOBJ));
}
sol_call_ex sol_std_set(sol_state *s) {
    sol_val obj = sol_get(s, 0);
    sol_val key = sol_get(s, 1);
    sol_val val = sol_get(s, 2);

    if (!sol_isdtype(obj, SOL_DOBJ))
        return sol_call_ex_err((sol_call_err){SOL_ERRV_TYPE_MISMATCH,
            sf_str_fmt("Arg 'obj' expected obj, found '%s'", sol_typename(obj).c_str),
        0});

    sf_str kstr;
    if (!sol_isdtype(key, SOL_DSTR))
        kstr = sol_tostring(key);
    else kstr = sf_str_dup(*(sf_str *)key.dyn);
    sol_dobj_set(obj.dyn, kstr, val);
    return sol_call_ex_ok(SOL_NIL);
}
sol_call_ex sol_std_get(sol_state *s) {
    sol_val obj = sol_get(s, 0);
    sol_val key = sol_get(s, 1);

    if (!sol_isdtype(obj, SOL_DOBJ))
        return sol_call_ex_err((sol_call_err){SOL_ERRV_TYPE_MISMATCH,
            sf_str_fmt("Arg 'obj' expected obj, found '%s'", sol_typename(obj).c_str),
        0});

    sf_str kstr;
    if (!sol_isdtype(key, SOL_DSTR))
        kstr = sol_tostring(key);
    else kstr = sf_str_dup(*(sf_str *)key.dyn);

    sol_dobj_ex ex = sol_dobj_get(obj.dyn, kstr);
    if (!ex.is_ok) {
        sf_str estr = sf_str_fmt("Object does not contain member '%s'", kstr.c_str);
        sf_str_free(kstr);
        return sol_call_ex_err((sol_call_err){SOL_ERRV_MEMBER_NOT_FOUND, estr, 0});
    }

    return sol_call_ex_ok(ex.ok);
}

sol_call_ex sol_std_err(sol_state *s) {
    sol_val str = sol_get(s, 0);
    sol_val err = sol_dnew(s, SOL_DERR);
    *(sf_str *)err.dyn = sf_str_dup(*(sf_str *)str.dyn);
    return sol_call_ex_ok(err);
}
sol_call_ex sol_std_panic(sol_state *s) {
    sol_val err = sol_get(s, 0);
    if (!sol_isdtype(err, SOL_DERR))
        return sol_call_ex_ok(err);
    return sol_call_ex_err((sol_call_err){SOL_ERRV_PANIC, sf_str_dup(*(sf_str *)err.dyn), 0});
}
sol_call_ex sol_std_assert(sol_state *s) {
    sol_val con = sol_get(s, 0);
    if (con.tt != SOL_TBOOL)
        return sol_call_ex_err((sol_call_err){SOL_ERRV_TYPE_MISMATCH,
            sf_str_fmt("Arg 'con' expected bool, found '%s'", sol_typename(con).c_str),
        0});
    return con.boolean ? sol_call_ex_ok(SOL_NIL) : sol_call_ex_err((sol_call_err){SOL_ERRV_ASSERT, SF_STR_EMPTY, 0});
}
sol_call_ex sol_std_type(sol_state *s) {
    sol_val val = sol_get(s, 0);
    sol_val str = sol_dnew(s, SOL_DSTR);
    *(sf_str *)str.dyn = sol_typename(val);
    return sol_call_ex_ok(str);
}

sol_call_ex sol_std_randi(sol_state *s) {
    sol_val min_v = sol_get(s, 0);
    sol_val max_v = sol_get(s, 1);
    sol_i64 min, max;
    if (min_v.tt != SOL_TI64) {
        if (min_v.tt == SOL_TF64) min = min_v.i64;
        else {
            return sol_call_ex_err((sol_call_err){SOL_ERRV_TYPE_MISMATCH,
                sf_str_fmt("Arg 'min' expected i64, found '%s'", sol_typename(min_v).c_str),
            0});
        }
    } else min = min_v.i64;
    if (max_v.tt != SOL_TI64) {
        if (max_v.tt == SOL_TF64) max = max_v.i64;
        else {
            return sol_call_ex_err((sol_call_err){SOL_ERRV_TYPE_MISMATCH,
                sf_str_fmt("Arg 'max' expected i64, found '%s'", sol_typename(max_v).c_str),
            0});
        }
    } else max = max_v.i64;

    if (min > max) { int64_t tmp = min; min = max; max = tmp; }
    uint64_t range = (uint64_t)(max - min) + 1;
#if defined(_WIN32)
    uint64_t r = ((uint64_t)rand() << 48) | ((uint64_t)rand() << 32) |
                 ((uint64_t)rand() << 16) | rand();
#else
    uint64_t r = (uint64_t)rand();
#endif
    return sol_call_ex_ok((sol_val){ .tt = SOL_TI64, .i64 = (int64_t)(r % range) + min });
}
sol_call_ex sol_std_randf(sol_state *s) {
    sol_val min_v = sol_get(s, 0);
    sol_val max_v = sol_get(s, 1);
    double min, max;

    if (min_v.tt == SOL_TF64) min = min_v.f64;
    else if (min_v.tt == SOL_TI64) min = (double)min_v.i64;
    else {
        return sol_call_ex_err((sol_call_err){SOL_ERRV_TYPE_MISMATCH,
            sf_str_fmt("Arg 'min' expected f64, found '%s'", sol_typename(min_v).c_str),
        0});
    }

    if (max_v.tt == SOL_TF64) max = max_v.f64;
    else if (max_v.tt == SOL_TI64) max = (double)max_v.i64;
    else {
        return sol_call_ex_err((sol_call_err){SOL_ERRV_TYPE_MISMATCH,
            sf_str_fmt("Arg 'max' expected f64, found '%s'", sol_typename(max_v).c_str),
        0});
    }

    if (min > max) { double tmp = min; min = max; max = tmp; }

    double frac = (double)rand() / (double)RAND_MAX; // [0, 1]
    double val = min + frac * (max - min);

    return sol_call_ex_ok((sol_val){ .tt = SOL_TF64, .f64 = val });
}
sol_call_ex sol_std_floor(sol_state *s) {
    sol_val f64 = sol_get(s, 0);
    return sol_call_ex_ok((sol_val){ .tt = SOL_TI64, .i64 = (sol_i64)f64.f64 });
}

sol_call_ex sol_std_fread(sol_state *s) {
    sol_val path = sol_get(s, 0);
    if (!sol_isdtype(path, SOL_DSTR))
        return sol_call_ex_err((sol_call_err){SOL_ERRV_TYPE_MISMATCH,
            sf_str_fmt("Arg 'path' expected str, found '%s'", sol_typename(path).c_str),
        0});
    sf_str p = *(sf_str *)path.dyn;
    if (!sf_file_exists(p))
        return sol_call_ex_ok(sol_dnerr(s, sf_str_fmt("File '%s' not found", p.c_str)));
    sf_fsb_ex fsb = sf_file_buffer(p);
    if (!fsb.is_ok) {
        sf_str errs;
        switch (fsb.err) {
            case SF_FILE_NOT_FOUND: errs = sf_str_fmt("File '%s' not found", p.c_str); break;
            case SF_OPEN_FAILURE: errs = sf_str_fmt("File '%s' failed to open", p.c_str); break;
            case SF_READ_FAILURE: errs = sf_str_fmt("File '%s' failed to read", p.c_str); break;
        }
        return sol_call_ex_ok(sol_dnerr(s, sf_str_dup(errs)));
    }
    fsb.ok.flags = SF_BUFFER_GROW;
    sf_buffer_autoins(&fsb.ok, ""); // [\0]

    sol_val str = sol_dnew(s, SOL_DSTR);
    *(sf_str *)str.dyn = sf_own((char *)fsb.ok.ptr);
    return sol_call_ex_ok(str);
}
sol_call_ex sol_std_fwrite(sol_state *s) {
    sol_val path = sol_get(s, 0);
    sol_val str = sol_get(s, 0);
    if (!sol_isdtype(path, SOL_DSTR))
        return sol_call_ex_err((sol_call_err){SOL_ERRV_TYPE_MISMATCH,
            sf_str_fmt("Arg 'path' expected str, found '%s'", sol_typename(path).c_str),
        0});
    if (!sol_isdtype(path, SOL_DSTR))
        return sol_call_ex_err((sol_call_err){SOL_ERRV_TYPE_MISMATCH,
            sf_str_fmt("Arg 'path' expected str, found '%s'", sol_typename(path).c_str),
        0});
    sf_str p = *(sf_str *)path.dyn;
    sf_str cont = *(sf_str *)str.dyn;

    FILE *f = fopen(p.c_str, "w");
    if (!f) return sol_call_ex_ok(sol_dnerr(s, sf_str_fmt("File '%s' failed to open", p.c_str)));;
    fwrite(cont.c_str, 1, cont.len, f);
    fclose(f);

    return sol_call_ex_ok(SOL_NIL);
}

void sol_usestd(sol_state *state) {
    sol_val io = sol_dnew(state, SOL_DOBJ);
    sol_dobj_set(io.dyn, sf_lit("print"), sol_wrapcfun(state, sol_std_print, 1, 0));
    sol_dobj_set(io.dyn, sf_lit("println"), sol_wrapcfun(state, sol_std_println, 1, 0));
    sol_dobj_set(io.dyn, sf_lit("time"), sol_wrapcfun(state, sol_std_time, 0, 0));
    sol_dobj_set(io.dyn, sf_lit("fread"), sol_wrapcfun(state, sol_std_fread, 2, 0));
    sol_dobj_set(io.dyn, sf_lit("fwrite"), sol_wrapcfun(state, sol_std_fread, 2, 0));

    sol_val obj = sol_dnew(state, SOL_DOBJ);
    sol_dobj_set(obj.dyn, sf_lit("new"), sol_wrapcfun(state, sol_std_new, 0, 0));
    sol_dobj_set(obj.dyn, sf_lit("set"), sol_wrapcfun(state, sol_std_set, 3, 0));
    sol_dobj_set(obj.dyn, sf_lit("get"), sol_wrapcfun(state, sol_std_get, 2, 0));

    sol_val math = sol_dnew(state, SOL_DOBJ);
    sol_dobj_set(math.dyn, sf_lit("randi"), sol_wrapcfun(state, sol_std_randi, 2, 0));
    sol_dobj_set(math.dyn, sf_lit("randf"), sol_wrapcfun(state, sol_std_randf, 2, 0));
    sol_dobj_set(math.dyn, sf_lit("floor"), sol_wrapcfun(state, sol_std_floor, 1, 0));

    sol_dobj *_g = state->global.dyn;
    sol_dobj_set(_g, sf_lit("string"), sol_wrapcfun(state, sol_std_string, 1, 0));
    sol_dobj_set(_g, sf_lit("err"), sol_wrapcfun(state, sol_std_err, 1, 0));
    sol_dobj_set(_g, sf_lit("panic"), sol_wrapcfun(state, sol_std_panic, 1, 0));
    sol_dobj_set(_g, sf_lit("assert"), sol_wrapcfun(state, sol_std_assert, 1, 0));
    sol_dobj_set(_g, sf_lit("type"), sol_wrapcfun(state, sol_std_type, 1, 0));
    sol_dobj_set(_g, sf_lit("eval"), sol_wrapcfun(state, sol_std_eval, 1, 0));
    sol_dobj_set(_g, sf_lit("import"), sol_wrapcfun(state, sol_std_import, 1, 0));
    sol_dobj_set(_g, sf_lit("collect"), sol_wrapcfun(state, sol_std_collect, 0, 0));

    sol_dobj_set(_g, sf_lit("io"), io);
    sol_dobj_set(_g, sf_lit("obj"), obj);
    sol_dobj_set(_g, sf_lit("math"), math);

    srand((unsigned)time(NULL));
}
