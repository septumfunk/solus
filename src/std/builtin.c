#include "sf/containers/buffer.h"
#include "solus/bytecode.h"
#include "solus/val.h"
#include "solus/vm.h"
#include "std.h"
#include <stdlib.h>
#include <string.h>

static inline sf_str solu_cwd(solu_state *s) {
    return sf_own(solu_realdir(solu_realpath(s->cwd.len ? s->cwd.c_str : ".")));
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
    return solu_ok(solu_dnerr(s, str.dyn));
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
solu_call_ex builtin_unwrap(solu_state *s) {
    solu_val val = solu_selfc(s);
    if (!solu_isdtype(val, SOLU_DERR))
        return solu_ok(val);
    return solu_call_ex_err((solu_call_err){SOLU_ERRV_PANIC, _strdup(val.dyn), 0});
}
solu_call_ex builtin_or_else(solu_state *s) {
    solu_val val = solu_selfc(s);
    if (!solu_isdtype(val, SOLU_DERR))
        return solu_ok(val);
    return solu_ok(solu_get(s, 1));
}
static solu_call_ex builtin_assert(solu_state *s) {
    solu_val con = solu_get(s, 0);
    expect_type(SOLU_TBOOL, con);
    return con.boolean ? solu_ok(SOLU_NIL) : solu_panic("Assertion failed", 0);
}

solu_call_ex builtin_then(solu_state *s) {
    solu_val self = solu_selfc(s);
    solu_val handler = solu_get(s, 1);
    expect_dtype(SOLU_DFUN, handler);
    return solu_call(s, handler.dyn, (solu_val[]){self}, 1);
}
solu_call_ex builtin_type(solu_state *s) {
    return solu_ok(solu_dnstr(s, solu_typename(solu_selfc(s)).c_str));
}
solu_call_ex builtin_str(solu_state *s) {
    char *e = solu_tostr(s, solu_selfc(s));
    solu_call_ex ex = solu_ok(solu_dnstr(s, e));
    solu_dalloc *dh = solu_dheader(ex.ok); (void)dh;
    free(e);
    return ex;
}
static solu_call_ex builtin_i64(solu_state *s) {
    solu_val conv = solu_selfc(s);
    switch (conv.tt) {
        case SOLU_TI64: return solu_ok(conv);
        case SOLU_TBOOL: return solu_ok((solu_val){SOLU_TI64, .i64 = conv.boolean ? 1 : 0});
        case SOLU_TF64: return solu_ok((solu_val){SOLU_TI64, .i64 = (solu_i64)conv.f64});
        case SOLU_TDYN: return solu_ok((solu_val){SOLU_TI64, .i64 = (solu_i64)strtoll(conv.dyn, NULL, 10)});
        default: return solu_panic("'%s' expected f64|str, found %s", solu_typename(conv).c_str);
    }
}
static solu_call_ex builtin_f64(solu_state *s) {
    solu_val conv = solu_selfc(s);
    switch (conv.tt) {
        case SOLU_TF64: return solu_ok(conv);
        case SOLU_TBOOL: return solu_ok((solu_val){SOLU_TF64, .f64 = conv.boolean ? 1 : 0});
        case SOLU_TI64: return solu_ok((solu_val){SOLU_TF64, .f64 = (solu_f64)conv.i64});
        case SOLU_TDYN: return solu_ok((solu_val){SOLU_TF64, .f64 = (solu_f64)strtof(conv.dyn, NULL)});
        default: return solu_panic("'%s' expected i64|str, found %s", solu_typename(conv).c_str);
    }
}

void solu_mod_builtin(solu_state *s) {
    solu_dobj *_g = s->global.dyn;
    solu_dobj_strset(_g, "err", solu_wrapcfun(s, builtin_err, 1, NULL, 0));
    solu_dobj_strset(_g, "panic", solu_wrapcfun(s, builtin_panic, 1, NULL, 0));
    solu_dobj_strset(_g, "attempt", solu_wrapcfun(s, builtin_attempt, 2, NULL, 0));
    solu_dobj_strset(_g, "catch", solu_wrapcfun(s, builtin_catch, 1, NULL, 0));
    solu_dobj_strset(_g, "assert", solu_wrapcfun(s, builtin_assert, 1, NULL, 0));
    solu_dobj_strset(_g, "eval", solu_wrapcfun(s, builtin_eval, 1, NULL, 0));
    solu_dobj_strset(_g, "import", solu_wrapcfun(s, builtin_import, 1, NULL, 0));
    solu_dobj_strset(_g, "require", solu_wrapcfun(s, builtin_require, 1, NULL, 0));

    solu_val then = solu_wrapcfun(s, builtin_then, 2, NULL, 0);
    solu_val type = solu_wrapcfun(s, builtin_type, 1, NULL, 0);
    solu_val str = solu_wrapcfun(s, builtin_str, 1, NULL, 0);
    solu_val i64 = solu_wrapcfun(s, builtin_i64, 1, NULL, 0);
    solu_val f64 = solu_wrapcfun(s, builtin_f64, 1, NULL, 0);
    solu_val unwrap = solu_wrapcfun(s, builtin_unwrap, 1, NULL, 0);
    solu_val or_else = solu_wrapcfun(s, builtin_or_else, 2, NULL, 0);

    solu_dobj_strset(_g, "then", then);
    solu_dobj_strset(_g, "type", type);
    solu_dobj_strset(_g, "str", str);
    solu_dobj_strset(_g, "i64", i64);
    solu_dobj_strset(_g, "f64", f64);
    solu_dobj_strset(_g, "unwrap", unwrap);
    solu_dobj_strset(_g, "or_else", or_else);

    solu_drelease(s->meta.base);
    s->meta.base = solu_dnew(s, SOLU_DOBJ);
    solu_dhold(s->meta.base);
    solu_dobj_strset(s->meta.base.dyn, "then", then);
    solu_dobj_strset(s->meta.base.dyn, "type", type);
    solu_dobj_strset(s->meta.base.dyn, "str", str);
    solu_dobj_strset(s->meta.base.dyn, "i64", i64);
    solu_dobj_strset(s->meta.base.dyn, "f64", f64);
    solu_dobj_strset(s->meta.base.dyn, "unwrap", unwrap);
    solu_dobj_strset(s->meta.base.dyn, "or_else", or_else);
}
