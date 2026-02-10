#include "sf/containers/buffer.h"
#include "solus/val.h"
#include "solus/vm.h"
#include "std.h"
#include <stdlib.h>
#include <string.h>

static inline sf_str solu_cwd(solu_state *state) {
    if (state->files.count == 0) return sf_lit("./");
    return sf_own((state->files.data + (state->files.count - 1))->c_str);
}

static solu_call_ex builtin_import(solu_state *s) {
    solu_val path = solu_get(s, 0);
    expect_dtype(SOLU_DSTR, path);

    char *rpath = solu_findfile(solu_cwd(s).c_str, path.dyn);
    if (!rpath) {
        sf_str p2 = sf_str_fmt("File '%s' not found", path.dyn);
        return solu_ok(solu_dnerr(s, p2.c_str));
    }

    solu_call_ex cl_ex;
    if (memcmp(rpath + strlen(rpath) - 4, "solc", 4) == 0) {
        solu_load_ex ld_ex = solu_loadfun(s, rpath);
        free(rpath);
        if (!ld_ex.is_ok) return solu_err(s, "%s", solu_err_string(ld_ex.err));
        s->rcmp = true;
        cl_ex = solu_call(s, &ld_ex.ok, NULL, 0);
        s->rcmp = false;
        solu_fproto_free(&ld_ex.ok);
    } else {
        solu_compile_ex cm_ex = solu_cfile(s, rpath);
        free(rpath);
        if (!cm_ex.is_ok)
            return solu_ok(solu_dnerr(s, solu_err_string(cm_ex.err.tt)));
        cl_ex = solu_call(s, &cm_ex.ok, NULL, 0);
        solu_fproto_free(&cm_ex.ok);
    }

    if (!cl_ex.is_ok)
        return solu_ok(solu_dnerr(s, cl_ex.err.panic));
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
        return solu_ok(solu_dnerr(s, solu_err_string(cm_ex.err.tt)));
    solu_call_ex cl_ex = solu_call(s, &cm_ex.ok, NULL, 0);
    solu_fproto_free(&cm_ex.ok);
    if (!cl_ex.is_ok) {
        return solu_ok(solu_dnerr(s, cl_ex.err.panic ?
            cl_ex.err.panic :
            solu_err_string(cm_ex.err.tt)
        ));
    }
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
    if (!try_ex.is_ok)
        return solu_ok(solu_dnerr(s, try_ex.err.panic));
    return solu_ok(try_ex.ok);
}
static solu_call_ex builtin_attempt(solu_state *s) {
    solu_val try = solu_get(s, 0);
    expect_dtype(SOLU_DFUN, try);
    solu_val handler = solu_get(s, 1);
    expect_dtype(SOLU_DFUN, handler);
    solu_call_ex try_ex = solu_call(s, try.dyn, NULL, 0);
    if (!try_ex.is_ok) {
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
    char *e = solu_tostr(s, solu_get(s, 0));
    solu_call_ex ex = solu_ok(solu_dnstr(s, e));
    solu_dalloc *dh = solu_dheader(ex.ok); (void)dh;
    free(e);
    return ex;
}
static solu_call_ex builtin_i64(solu_state *s) {
    solu_val conv = solu_get(s, 0);
    if (conv.tt != SOLU_TF64 && !solu_isdtype(conv, SOLU_DSTR))
        return solu_panic("'%s' expected f64|str, found %s", solu_typename(conv).c_str);
    return conv.tt == SOLU_TF64 ?
        solu_ok((solu_val){SOLU_TI64, .i64 = (solu_i64)conv.f64}) :
        solu_ok((solu_val){SOLU_TI64, .i64 = (solu_i64)strtoll(conv.dyn, NULL, 10)});
}
static solu_call_ex builtin_f64(solu_state *s) {
    solu_val conv = solu_get(s, 0);
    if (conv.tt != SOLU_TI64 && !solu_isdtype(conv, SOLU_DSTR))
        return solu_panic("'%s' expected i64|str, found %s", solu_typename(conv).c_str);
    return conv.tt == SOLU_TI64 ?
        solu_ok((solu_val){SOLU_TF64, .f64 = (solu_f64)conv.i64}) :
        solu_ok((solu_val){SOLU_TF64, .f64 = (solu_f64)strtod(conv.dyn, NULL)});
}

void solu_mod_builtin(solu_state *s) {
    solu_dobj *_g = s->global.dyn;
    solu_dobj_strset(_g, "err", solu_wrapcfun(s, builtin_err, 1, 0));
    solu_dobj_strset(_g, "panic", solu_wrapcfun(s, builtin_panic, 1, 0));
    solu_dobj_strset(_g, "attempt", solu_wrapcfun(s, builtin_attempt, 2, 0));
    solu_dobj_strset(_g, "catch", solu_wrapcfun(s, builtin_catch, 1, 0));
    solu_dobj_strset(_g, "unwrap", solu_wrapcfun(s, builtin_unwrap, 1, 0));
    solu_dobj_strset(_g, "unwrap_or", solu_wrapcfun(s, builtin_unwrap_or, 2, 0));
    solu_dobj_strset(_g, "assert", solu_wrapcfun(s, builtin_assert, 1, 0));
    solu_dobj_strset(_g, "type", solu_wrapcfun(s, builtin_type, 1, 0));
    solu_dobj_strset(_g, "eval", solu_wrapcfun(s, builtin_eval, 1, 0));
    solu_dobj_strset(_g, "import", solu_wrapcfun(s, builtin_import, 1, 0));
    solu_dobj_strset(_g, "require", solu_wrapcfun(s, builtin_require, 1, 0));

    solu_dobj_strset(_g, "str", solu_wrapcfun(s, builtin_str, 1, 0));
    solu_dobj_strset(_g, "i64", solu_wrapcfun(s, builtin_i64, 1, 0));
    solu_dobj_strset(_g, "f64", solu_wrapcfun(s, builtin_f64, 1, 0));
}
