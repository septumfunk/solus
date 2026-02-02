#include "std.h"

static inline sf_str solu_cwd(solu_state *state) {
    return sf_str_dup(*(state->files.data + (state->files.count - 1)));
}

static int has_suffix_solu(const char *s) {
    size_t n = strlen(s);
    return (n >= 5 && memcmp(s + (n - 5), ".solu", 5) == 0) ||
           (n >= 6 && memcmp(s + (n - 6), ".solus", 6) == 0);
}

static solu_call_ex builtin_import(solu_state *s) {
    solu_val path = solu_get(s, 0);
    expect_dtype(SOLU_DSTR, path);
    sf_str cwd = solu_cwd(s);

    sf_str p = sf_str_fmt("%s%s", cwd.c_str, path.dyn);
    if (!sf_file_exists(p) && !has_suffix_solu(p.c_str)) {
        sf_str p2 = sf_str_fmt("%s.solu", p.c_str);
        if (sf_file_exists(p2)) {
            sf_str_free(p);
            p = p2;
        } else {
            sf_str_free(p2);
            p2 = sf_str_fmt("%s.solus", p.c_str);
            p = p2;
        }
    }

    if (!sf_file_exists(p)) {
        sf_str p2 = sf_str_fmt("File '%s' not found", p.c_str);
        sf_str_free(p);
        return solu_ok(solu_dnerr(s, p2.c_str));
    }

    solu_compile_ex cm_ex = solu_cfile(s, p.c_str);
    if (!cm_ex.is_ok)
        return solu_ok(solu_dnerr(s, solu_err_string(cm_ex.err.tt).c_str));
    solu_call_ex cl_ex = solu_call(s, &cm_ex.ok, NULL, 0);
    solu_fproto_free(&cm_ex.ok);
    if (!cl_ex.is_ok)
            return solu_ok(solu_dnerr(s, cl_ex.err.panic
            ));
    return cl_ex;
}
static solu_call_ex builtin_require(solu_state *s) {
    solu_call_ex import = builtin_import(s);
    if (!import.is_ok) return import;
    if (solu_isdtype(import.ok, SOLU_DERR))
        return solu_call_ex_err((solu_call_err){
            SOLU_ERRV_PANIC, _strdup(import.ok.dyn), 0
        });
    return import;
}
static solu_call_ex builtin_eval(solu_state *s) {
    solu_val src = solu_get(s, 0);
    expect_dtype(SOLU_DSTR, src);

    solu_compile_ex cm_ex = solu_csrc(s, src.dyn);
    if (!cm_ex.is_ok)
        return solu_ok(solu_dnerr(s, solu_err_string(cm_ex.err.tt).c_str));
    solu_call_ex cl_ex = solu_call(s, &cm_ex.ok, NULL, 0);
    solu_fproto_free(&cm_ex.ok);
    if (!cl_ex.is_ok)
        return solu_ok(solu_dnerr(s, cl_ex.err.tt == SOLU_ERRV_PANIC ?
            cl_ex.err.panic :
            solu_err_string(cm_ex.err.tt).c_str
        ));
    return cl_ex;
}
static solu_call_ex builtin_err(solu_state *s) {
    solu_val str = solu_get(s, 0);
    expect_dtype(SOLU_DSTR, str);
    return solu_ok(solu_dnstr(s, str.dyn));
}
static solu_call_ex builtin_panic(solu_state *s) {
    solu_val err = solu_get(s, 0);
    expect_dtype(SOLU_DSTR, err);
    return solu_call_ex_err((solu_call_err){SOLU_ERRV_PANIC, _strdup(err.dyn), 0});
}
static solu_call_ex builtin_catch(solu_state *s) {
    solu_val try = solu_get(s, 0);
    expect_dtype(SOLU_DFUN, try);
    solu_call_ex try_ex = solu_call(s, try.dyn, NULL, 0);
    if (!try_ex.is_ok) {
        solu_popframe(s); // Frame remains after panic!
        return solu_ok(solu_dnerr(s, try_ex.err.panic));
    }
    return solu_ok(try_ex.ok);
}
static solu_call_ex builtin_attempt(solu_state *s) {
    solu_val try = solu_get(s, 0);
    expect_dtype(SOLU_DFUN, try);
    solu_val handler = solu_get(s, 1);
    expect_dtype(SOLU_DFUN, handler);
    solu_call_ex try_ex = solu_call(s, try.dyn, NULL, 0);
    if (!try_ex.is_ok) {
        solu_popframe(s); // Frame remains after panic!
        solu_val err = solu_dnerr(s, try_ex.err.panic);
        solu_call_ex hand_ex = solu_call(s, handler.dyn, (solu_val[]){err}, 1);
        if (!hand_ex.is_ok) return hand_ex;
        return solu_ok(hand_ex.ok);
    }
    if (solu_isdtype(try_ex.ok, SOLU_DERR)) {
        solu_call_ex hand_ex = solu_call(s, handler.dyn, (solu_val[]){try_ex.ok}, 1);
        if (!hand_ex.is_ok) return hand_ex;
        return solu_ok(hand_ex.ok);
    }
    return solu_ok(try_ex.ok);
}
static solu_call_ex builtin_unwrap(solu_state *s) {
    solu_val val = solu_get(s, 0);
    if (!solu_isdtype(val, SOLU_DERR))
        return solu_ok(val);
    return solu_call_ex_err((solu_call_err){SOLU_ERRV_PANIC, _strdup(val.dyn), 0});
}
static solu_call_ex builtin_unwrap_or(solu_state *s) {
    solu_val val = solu_get(s, 0);
    if (!solu_isdtype(val, SOLU_DERR))
        return solu_ok(val);
    return solu_ok(solu_get(s, 1));
}
static solu_call_ex builtin_assert(solu_state *s) {
    solu_val con = solu_get(s, 0);
    expect_type(SOLU_TBOOL, con);
    return con.boolean ? solu_ok(SOLU_NIL) : solu_call_ex_err((solu_call_err){SOLU_ERRV_PANIC, "Assertion failed", 0});
}
static solu_call_ex builtin_type(solu_state *s) {
    return solu_ok(solu_dnstr(s, solu_typename(solu_get(s, 0)).c_str));
}

static solu_call_ex builtin_str(solu_state *s) {
    char *e = solu_tostring(solu_get(s, 0));
    solu_call_ex ex = solu_ok(solu_dnstr(s, e));
    free(e);
    return ex;
}
static solu_call_ex builtin_i64(solu_state *s) {
    solu_val f64 = solu_get(s, 0);
    expect_type(SOLU_TF64, f64);
    return solu_ok((solu_val){SOLU_TI64, .i64 = (solu_i64)f64.f64});
}
static solu_call_ex builtin_f64(solu_state *s) {
    solu_val i64 = solu_get(s, 0);
    expect_type(SOLU_TI64, i64);
    return solu_ok((solu_val){SOLU_TF64, .f64 = (solu_f64)i64.i64});
}

void solu_mod_builtin(solu_state *s) {
    solu_dobj *_g = s->global.dyn;
    solu_dobj_set(_g, sf_lit("str"), solu_wrapcfun(s, builtin_str, 1, 0));
    solu_dobj_set(_g, sf_lit("err"), solu_wrapcfun(s, builtin_err, 1, 0));
    solu_dobj_set(_g, sf_lit("panic"), solu_wrapcfun(s, builtin_panic, 1, 0));
    solu_dobj_set(_g, sf_lit("attempt"), solu_wrapcfun(s, builtin_attempt, 2, 0));
    solu_dobj_set(_g, sf_lit("catch"), solu_wrapcfun(s, builtin_catch, 1, 0));
    solu_dobj_set(_g, sf_lit("unwrap"), solu_wrapcfun(s, builtin_unwrap, 1, 0));
    solu_dobj_set(_g, sf_lit("unwrap_or"), solu_wrapcfun(s, builtin_unwrap_or, 2, 0));
    solu_dobj_set(_g, sf_lit("assert"), solu_wrapcfun(s, builtin_assert, 1, 0));
    solu_dobj_set(_g, sf_lit("type"), solu_wrapcfun(s, builtin_type, 1, 0));
    solu_dobj_set(_g, sf_lit("eval"), solu_wrapcfun(s, builtin_eval, 1, 0));
    solu_dobj_set(_g, sf_lit("import"), solu_wrapcfun(s, builtin_import, 1, 0));
    solu_dobj_set(_g, sf_lit("require"), solu_wrapcfun(s, builtin_require, 1, 0));
}
