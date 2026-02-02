#include "std.h"

static sol_call_ex builtin_import(sol_state *s) {
    sol_val path = sol_get(s, 0);
    expect_dtype(SOL_DSTR, path);
    sf_str cwd = sol_cwd(s);

    sf_str p = sf_str_fmt("%s%s", cwd.c_str, path.dyn);
    if (!sf_file_exists(p)) {
        sf_str p2 = sf_str_fmt("%s.sol", p.c_str);
        sf_str_free(p);
        p = p2;
    }
    if (!sf_file_exists(p)) {
        sf_str p2 = sf_str_fmt("File '%s' not found", p.c_str);
        sf_str_free(p);
        return sol_call_ex_ok(sol_dnerr(s, p2.c_str));
    }

    sol_compile_ex cm_ex = sol_cfile(s, p.c_str);
    if (!cm_ex.is_ok)
        return sol_call_ex_ok(sol_dnerr(s, sol_err_string(cm_ex.err.tt).c_str));
    sol_call_ex cl_ex = sol_call(s, &cm_ex.ok, NULL, 0);
    sol_fproto_free(&cm_ex.ok);
    if (!cl_ex.is_ok)
            return sol_call_ex_ok(sol_dnerr(s, cl_ex.err.panic
            ));
    return cl_ex;
}
static sol_call_ex builtin_require(sol_state *s) {
    sol_call_ex import = builtin_import(s);
    if (!import.is_ok) return import;
    if (sol_isdtype(import.ok, SOL_DERR))
        return sol_call_ex_err((sol_call_err){
            SOL_ERRV_PANIC, strdup(import.ok.dyn), 0
        });
    return import;
}
static sol_call_ex builtin_eval(sol_state *s) {
    sol_val src = sol_get(s, 0);
    expect_dtype(SOL_DSTR, src);

    sol_compile_ex cm_ex = sol_csrc(s, src.dyn);
    if (!cm_ex.is_ok)
        return sol_call_ex_ok(sol_dnerr(s, sol_err_string(cm_ex.err.tt).c_str));
    sol_call_ex cl_ex = sol_call(s, &cm_ex.ok, NULL, 0);
    sol_fproto_free(&cm_ex.ok);
    if (!cl_ex.is_ok)
        return sol_call_ex_ok(sol_dnerr(s, cl_ex.err.tt == SOL_ERRV_PANIC ?
            cl_ex.err.panic :
            sol_err_string(cm_ex.err.tt).c_str
        ));
    return cl_ex;
}
static sol_call_ex builtin_err(sol_state *s) {
    sol_val str = sol_get(s, 0);
    expect_dtype(SOL_DSTR, str);
    return sol_call_ex_ok(sol_dnstr(s, str.dyn));
}
static sol_call_ex builtin_panic(sol_state *s) {
    sol_val err = sol_get(s, 0);
    expect_dtype(SOL_DSTR, err);
    return sol_call_ex_err((sol_call_err){SOL_ERRV_PANIC, strdup(err.dyn), 0});
}
static sol_call_ex builtin_catch(sol_state *s) {
    sol_val try = sol_get(s, 0);
    expect_dtype(SOL_DFUN, try);
    sol_call_ex try_ex = sol_call(s, try.dyn, NULL, 0);
    if (!try_ex.is_ok) {
        sol_popframe(s); // Frame remains after panic!
        return sol_call_ex_ok(sol_dnerr(s, try_ex.err.panic));
    }
    return sol_call_ex_ok(try_ex.ok);
}
static sol_call_ex builtin_attempt(sol_state *s) {
    sol_val try = sol_get(s, 0);
    expect_dtype(SOL_DFUN, try);
    sol_val handler = sol_get(s, 1);
    expect_dtype(SOL_DFUN, handler);
    sol_call_ex try_ex = sol_call(s, try.dyn, NULL, 0);
    if (!try_ex.is_ok) {
        sol_popframe(s); // Frame remains after panic!
        sol_val err = sol_dnerr(s, try_ex.err.panic);
        sol_call_ex hand_ex = sol_call(s, handler.dyn, (sol_val[]){err}, 1);
        if (!hand_ex.is_ok) return hand_ex;
        return sol_call_ex_ok(hand_ex.ok);
    }
    if (sol_isdtype(try_ex.ok, SOL_DERR)) {
        sol_call_ex hand_ex = sol_call(s, handler.dyn, (sol_val[]){try_ex.ok}, 1);
        if (!hand_ex.is_ok) return hand_ex;
        return sol_call_ex_ok(hand_ex.ok);
    }
    return sol_call_ex_ok(try_ex.ok);
}
static sol_call_ex builtin_unwrap(sol_state *s) {
    sol_val val = sol_get(s, 0);
    if (!sol_isdtype(val, SOL_DERR))
        return sol_call_ex_ok(val);
    return sol_call_ex_err((sol_call_err){SOL_ERRV_PANIC, strdup(val.dyn), 0});
}
static sol_call_ex builtin_unwrap_or(sol_state *s) {
    sol_val val = sol_get(s, 0);
    if (!sol_isdtype(val, SOL_DERR))
        return sol_call_ex_ok(val);
    return sol_call_ex_ok(sol_get(s, 1));
}
static sol_call_ex builtin_assert(sol_state *s) {
    sol_val con = sol_get(s, 0);
    expect_type(SOL_TBOOL, con);
    return con.boolean ? sol_call_ex_ok(SOL_NIL) : sol_call_ex_err((sol_call_err){SOL_ERRV_PANIC, "Assertion failed", 0});
}
static sol_call_ex builtin_type(sol_state *s) {
    return sol_call_ex_ok(sol_dnstr(s, sol_typename(sol_get(s, 0)).c_str));
}

static sol_call_ex builtin_str(sol_state *s) {
    sf_str e = sol_tostring(sol_get(s, 0));
    sol_call_ex ex = sol_call_ex_ok(sol_dnstr(s, e.c_str));
    sf_str_free(e);
    return ex;
}
static sol_call_ex builtin_i64(sol_state *s) {
    sol_val f64 = sol_get(s, 0);
    expect_type(SOL_TF64, f64);
    return sol_call_ex_ok((sol_val){SOL_TI64, .i64 = (sol_i64)f64.f64});
}
static sol_call_ex builtin_f64(sol_state *s) {
    sol_val i64 = sol_get(s, 0);
    expect_type(SOL_TI64, i64);
    return sol_call_ex_ok((sol_val){SOL_TF64, .f64 = (sol_f64)i64.i64});
}

void sol_mod_builtin(sol_state *s) {
    sol_dobj *_g = s->global.dyn;
    sol_dobj_set(_g, sf_lit("str"), sol_wrapcfun(s, builtin_str, 1, 0));
    sol_dobj_set(_g, sf_lit("err"), sol_wrapcfun(s, builtin_err, 1, 0));
    sol_dobj_set(_g, sf_lit("panic"), sol_wrapcfun(s, builtin_panic, 1, 0));
    sol_dobj_set(_g, sf_lit("attempt"), sol_wrapcfun(s, builtin_attempt, 2, 0));
    sol_dobj_set(_g, sf_lit("catch"), sol_wrapcfun(s, builtin_catch, 1, 0));
    sol_dobj_set(_g, sf_lit("unwrap"), sol_wrapcfun(s, builtin_unwrap, 1, 0));
    sol_dobj_set(_g, sf_lit("unwrap_or"), sol_wrapcfun(s, builtin_unwrap_or, 2, 0));
    sol_dobj_set(_g, sf_lit("assert"), sol_wrapcfun(s, builtin_assert, 1, 0));
    sol_dobj_set(_g, sf_lit("type"), sol_wrapcfun(s, builtin_type, 1, 0));
    sol_dobj_set(_g, sf_lit("eval"), sol_wrapcfun(s, builtin_eval, 1, 0));
    sol_dobj_set(_g, sf_lit("import"), sol_wrapcfun(s, builtin_import, 1, 0));
    sol_dobj_set(_g, sf_lit("require"), sol_wrapcfun(s, builtin_require, 1, 0));
}
