#include "solus/api.h"
#include "std.h"
#include <errno.h>

/*
 * import(path: str) -> any|err
 * Loads a solus file and then executes it. If the file is not found at
 * first and the .solu file extension is not provided, it will attempt
 * to find path + ".solu", and path + ".solus".
 */
static solu_call_ex builtin_import(solu_state *s) {
    solu_val path = solu_get(s, 0);
    expect_dtype(SOLU_DSTR, path);

    // Locate file
    char *rpath = solu_findfile(s, path.dyn);
    if (!rpath) {
        sf_str p2 = sf_str_fmt("File '%s' not found", path.dyn);
        solu_val e = solu_dnerr(s, p2.c_str);
        sf_str_free(p2);
        return solu_ok(e);
    }

    solu_call_ex cl_ex;
    if (memcmp(rpath + strlen(rpath) - 4, "solc", 4) == 0) {
        // Bytecode
        solu_load_ex ld_ex = solu_loadfun(s, rpath);
        free(rpath);
        if (!ld_ex.is_ok) return solu_err(s, "%s", solu_err_string(ld_ex.err));
        s->rcmp = true;
        cl_ex = solu_call(s, &ld_ex.ok, NULL, 0);
        s->rcmp = false;
        solu_fproto_free(&ld_ex.ok);
    } else {
        // Source code
        solu_compile_ex cm_ex = solu_cfile(s, rpath);
        if (!cm_ex.is_ok) {
            char *trace = solu_ctrace_print(rpath, cm_ex.err, 15, 2, 1);
            free(rpath);
            return solu_err(s, "%s", trace ? "Unknown compile error" : trace);
        }
        free(rpath);
        cl_ex = solu_call(s, &cm_ex.ok, NULL, 0);
        solu_fproto_free(&cm_ex.ok);
    }

    if (!cl_ex.is_ok) {
        // Compile failure
        solu_val e = solu_dnerr(s, cl_ex.err.panic);
        return solu_ok(e);
    }
    return cl_ex;
}

/*
 * require(path: str) -> any|panic
 * Loads a solus file and then executes it, behaving identically to import
 * besides the fact that it panics instead of returning an error.
 */
static solu_call_ex builtin_require(solu_state *s) {
    solu_call_ex import = builtin_import(s);
    if (!import.is_ok) return import;
    // Raise err into panic
    if (solu_isdtype(import.ok, SOLU_DERR))
        return solu_panic(s, import.ok.dyn);
    return import;
}

/*
 * eval(src: str) -> any|err
 * Compiles source code from a string into a fun, note that this is NOT sandboxed
 * and is NOT safe to accept use input with. Treat eval very carefully.
 */
static solu_call_ex builtin_eval(solu_state *s) {
    solu_val src = solu_get(s, 0);
    expect_dtype(SOLU_DSTR, src);

    solu_compile_ex cm_ex = solu_csrc(s, src.dyn);
    if (!cm_ex.is_ok) {
        char *trace = solu_ctrace_print("Source", cm_ex.err, 15, 2, 1);
        solu_call_ex e = solu_err(s, "%s", trace ? "Unknown compile error" : trace);
        if (trace) free(trace);
        return e;
    }
    solu_call_ex cl_ex = solu_call(s, &cm_ex.ok, NULL, 0);
    solu_fproto_free(&cm_ex.ok);

    if (!cl_ex.is_ok) {
        char *trace = solu_trace_print(cl_ex.err.trace, 15, 2, 1);
        solu_call_ex e = solu_err(s, trace ? "error: %s\n%s" : "error: %s", cl_ex.err.panic, trace);
        if (trace) free(trace);
        return e;
    }
    return cl_ex;
}

/*
 * panic(err: str) -> panic
 * Creates a runtime panic, solus’ equivalent to an exception pretty much.
 * This will exit the program if it is not DIRECTLY caught.
 */
static solu_call_ex builtin_panic(solu_state *s) {
    solu_val err = solu_get(s, 0);
    expect_dtype(SOLU_DSTR, err);
    return solu_panic(s, err.dyn);
}

/*
 * catch(try: fun) -> any|err
 * Catches any panics thrown within the provided try block’s scope of execution.
 * Think of this as a try/catch that returns a result instead of passing it.
 */
static solu_call_ex builtin_catch(solu_state *s) {
    solu_val try = solu_get(s, 0);
    expect_dtype(SOLU_DFUN, try);
    solu_call_ex try_ex = solu_call(s, try.dyn, NULL, 0);
    if (!try_ex.is_ok)
        return solu_ok(solu_dnerr(s, try_ex.err.panic));
    return solu_ok(try_ex.ok);
}

/*
 * attempt(try: fun, handler: fun) -> any
 * Similar to catch except it uses a proper handler fun, mimicking try/catch behavior
 * in other languages. If `try` fails or panics, `handler` will be passed the err.
 */
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

/*
 * unwrap(val: any) -> any|panic
 * Unwraps a variable that is ambiguously value or error, returning either
 * the value or panicking on err
 */
solu_call_ex builtin_unwrap(solu_state *s) {
    solu_val val = solu_selfc(s);
    if (!solu_isdtype(val, SOLU_DERR))
        return solu_ok(val);
    return solu_panic(s, val.dyn);
}

/*
 * or_else(valid: any, invalid: any) -> any
 * Returns valid if it is a valid value, or returns invalid if valid is err
 */
solu_call_ex builtin_or_else(solu_state *s) {
    solu_val val = solu_selfc(s);
    if (!solu_isdtype(val, SOLU_DERR))
        return solu_ok(val);
    return solu_ok(solu_get(s, 1));
}

/*
 * then(self: any, handler: fun) -> any
 * Perform an operation on a value using a handler function.
 * Useful for function chaining
 */
solu_call_ex builtin_then(solu_state *s) {
    solu_val self = solu_selfc(s);
    solu_val handler = solu_get(s, 1);
    expect_dtype(SOLU_DFUN, handler);
    return solu_call(s, handler.dyn, (solu_val[]){self}, 1);
}

/*
 * assert(con: bool) -> nil|panic
 * Throws a panic if the provided condition is false
 */
static solu_call_ex builtin_assert(solu_state *s) {
    solu_val con = solu_get(s, 0);
    expect_type(SOLU_TBOOL, con);
    return con.boolean ? solu_ok(SOLU_NIL) : solu_panic(s, "Assertion failed");
}

/*
 * type(val: any) -> str
 * Returns the type of a value represented as a string
 */
solu_call_ex builtin_type(solu_state *s) {
    return solu_ok(solu_dnstr(s, solu_typename(solu_selfc(s)).c_str));
}

/*
 * str(val: any)
 * Converts a value of any type into a string representation. Note that this function
 * does not stringify objects, but rather gives a string representation of the pointer.
 */
solu_call_ex builtin_str(solu_state *s) {
    char *e = solu_tostr(s, solu_selfc(s));
    solu_call_ex ex = solu_ok(solu_dnstr(s, e));
    solu_dalloc *dh = solu_dheader(ex.ok); (void)dh;
    free(e);
    return ex;
}

/*
 * err(str: str) -> err
 * Constructs an err type from a str. You can return this to
 * represent failure conditions.
 */
static solu_call_ex builtin_err(solu_state *s) {
    solu_val str = solu_get(s, 0);
    expect_dtype(SOLU_DSTR, str);
    return solu_ok(solu_dnerr(s, str.dyn));
}

/*
 * i64(conv: f64|str)
 * Converts an f64 value into the i64 equivalent value. This will result in
 * loss of floating point (decimal) precision.
 */
static solu_call_ex builtin_i64(solu_state *s) {
    solu_val conv = solu_selfc(s);
    switch (conv.tt) {
        case SOLU_TI64: return solu_ok(conv);
        case SOLU_TBOOL: return solu_ok((solu_val){SOLU_TI64, .i64 = conv.boolean ? 1 : 0});
        case SOLU_TF64: return solu_ok((solu_val){SOLU_TI64, .i64 = (solu_i64)conv.f64});
        case SOLU_TDYN: {
            if (!solu_isdtype(conv, SOLU_DSTR))
                return solu_panic(s, "'%s' expected f64|str|bool, found %s", solu_typename(conv).c_str);
            errno = 0;
            char *end;
            solu_val out = {SOLU_TI64, .i64 = (solu_i64)strtoll(conv.dyn, &end, 10)};
            if (end == conv.dyn)
                return solu_err(s, "no value found for conversion");
            if (errno == ERANGE)
                return solu_err(s, "value out of range for type i64");
            return solu_ok(out);
        }
        default: return solu_panic(s, "'%s' expected f64|str|bool, found %s", solu_typename(conv).c_str);
    }
}

/*
 * f64(conv: i64|str) -> f64
 * Converts an i64 value into the f64 equivalent value. Not all values i64
 * represents can be represented by f64 accurately.
 */
static solu_call_ex builtin_f64(solu_state *s) {
    solu_val conv = solu_selfc(s);
    switch (conv.tt) {
        case SOLU_TF64: return solu_ok(conv);
        case SOLU_TBOOL: return solu_ok((solu_val){SOLU_TF64, .f64 = conv.boolean ? 1 : 0});
        case SOLU_TI64: return solu_ok((solu_val){SOLU_TF64, .f64 = (solu_f64)conv.i64});
        case SOLU_TDYN: {
            if (!solu_isdtype(conv, SOLU_DSTR))
                return solu_panic(s, "'%s' expected i64|str|bool, found %s", solu_typename(conv).c_str);
            errno = 0;
            char *end;
            solu_val out = {SOLU_TF64, .f64 = (solu_f64)strtof(conv.dyn, &end)};
            if (end == conv.dyn)
                return solu_err(s, "no value found for conversion");
            if (errno == ERANGE)
                return solu_err(s, "value out of range for type f64");
            return solu_ok(out);
        }
        default: return solu_panic(s, "'%s' expected i64|str|bool, found %s", solu_typename(conv).c_str);
    }
}


void solu_mod_builtin(solu_state *s) {
    solu_dobj *_g = s->global.dyn;

    // Modules
    solu_dobj_strset(_g, "import", solu_wrapcfun(s, builtin_import, 1, NULL, 0));
    solu_dobj_strset(_g, "require", solu_wrapcfun(s, builtin_require, 1, NULL, 0));
    solu_dobj_strset(_g, "eval", solu_wrapcfun(s, builtin_eval, 1, NULL, 0));

    // Error handling
    solu_dobj_strset(_g, "panic", solu_wrapcfun(s, builtin_panic, 1, NULL, 0));
    solu_dobj_strset(_g, "catch", solu_wrapcfun(s, builtin_catch, 1, NULL, 0));
    solu_dobj_strset(_g, "attempt", solu_wrapcfun(s, builtin_attempt, 2, NULL, 0));
    solu_dobj_strset(_g, "unwrap", solu_wrapcfun(s, builtin_unwrap, 1, NULL, 0));
    solu_dobj_strset(_g, "or_else", solu_wrapcfun(s, builtin_or_else, 2, NULL, 0));
    solu_dobj_strset(_g, "then", solu_wrapcfun(s, builtin_then, 2, NULL, 0));
    solu_dobj_strset(_g, "assert", solu_wrapcfun(s, builtin_assert, 1, NULL, 0));

    // Casting
    solu_dobj_strset(_g, "type", solu_wrapcfun(s, builtin_type, 1, NULL, 0));
    solu_dobj_strset(_g, "str", solu_wrapcfun(s, builtin_str, 1, NULL, 0));
    solu_dobj_strset(_g, "err", solu_wrapcfun(s, builtin_err, 1, NULL, 0));
    solu_dobj_strset(_g, "i64", solu_wrapcfun(s, builtin_i64, 1, NULL, 0));
    solu_dobj_strset(_g, "f64", solu_wrapcfun(s, builtin_f64, 1, NULL, 0));

    solu_drelease(s->meta.base);
    solu_drelease(s->meta.prim);
    s->meta.base = solu_dnew(s, SOLU_DOBJ);
    s->meta.prim = solu_dnew(s, SOLU_DOBJ);
    solu_dhold(s->meta.base);
    solu_dhold(s->meta.prim);

    // Builtin functions that extend as methods to all types
    for (int i = 0; i < 2; ++i) {
        solu_val (*fun)(solu_state *, solu_cfunction, uint32_t, solu_val *, uint32_t) = i ?
            solu_wrapmfun : solu_wrapcfun;
        solu_dobj_strset((i ? s->meta.base : s->meta.prim).dyn, "unwrap", fun(s, builtin_unwrap, 1, NULL, 0));
        solu_dobj_strset((i ? s->meta.base : s->meta.prim).dyn, "or_else", fun(s, builtin_or_else, 2, NULL, 0));
        solu_dobj_strset((i ? s->meta.base : s->meta.prim).dyn, "then", fun(s, builtin_then, 2, NULL, 0));
        solu_dobj_strset((i ? s->meta.base : s->meta.prim).dyn, "type", fun(s, builtin_type, 1, NULL, 0));
        solu_dobj_strset((i ? s->meta.base : s->meta.prim).dyn, "str", fun(s, builtin_str, 1, NULL, 0));
        solu_dobj_strset((i ? s->meta.base : s->meta.prim).dyn, "i64", fun(s, builtin_i64, 1, NULL, 0));
        solu_dobj_strset((i ? s->meta.base : s->meta.prim).dyn, "f64", fun(s, builtin_f64, 1, NULL, 0));
    }
}
