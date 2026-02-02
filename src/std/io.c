#include "std.h"

static sol_call_ex io_print(sol_state *s) {
    sol_val to_print = sol_get(s, 0);
    sf_str val = sol_tostring(to_print);
    printf("%s", val.c_str);
    sf_str_free(val);
    return sol_call_ex_ok(SOL_NIL);
}
static sol_call_ex io_println(sol_state *s) {
    sol_val to_print = sol_get(s, 0);
    sf_str val = sol_tostring(to_print);
    printf("%s\n", val.c_str);
    sf_str_free(val);
    return sol_call_ex_ok(SOL_NIL);
}
static sol_call_ex io_time(sol_state *s) {
    (void)s;
    return sol_call_ex_ok((sol_val){.tt = SOL_TF64, .f64 = sol_timesec()});
}
static sol_call_ex io_fread(sol_state *s) {
    sol_val path = sol_get(s, 0);
    expect_dtype(SOL_DSTR, path);

    sf_str p = sf_ref(path.dyn);
    if (!sf_file_exists(p)) {
        sf_str e = sf_str_fmt("File '%s' not found", p.c_str);
        sol_call_ex ex = sol_call_ex_ok(sol_dnerr(s, e.c_str));
        sf_str_free(e);
        return ex;
    }
    sf_fsb_ex fsb = sf_file_buffer(p);
    if (!fsb.is_ok) {
        sf_str errs = SF_STR_EMPTY;
        switch (fsb.err) {
            case SF_FILE_NOT_FOUND: errs = sf_str_fmt("File '%s' not found", p.c_str); break;
            case SF_OPEN_FAILURE: errs = sf_str_fmt("File '%s' failed to open", p.c_str); break;
            case SF_READ_FAILURE: errs = sf_str_fmt("File '%s' failed to read", p.c_str); break;
        }
        sol_call_ex ex = sol_call_ex_ok(sol_dnerr(s, errs.c_str));
        sf_str_free(errs);
        return ex;
    }
    fsb.ok.flags = SF_BUFFER_GROW;
    sf_buffer_autoins(&fsb.ok, ""); // [\0]

    return sol_call_ex_ok(sol_dnstr(s, (char *)fsb.ok.ptr));
}
static sol_call_ex io_fwrite(sol_state *s) {
    sol_val path = sol_get(s, 0);
    expect_dtype(SOL_DSTR, path);
    sol_val content = sol_get(s, 1);
    expect_dtype(SOL_DSTR, content);

    sf_str p = sf_ref(path.dyn);
    sf_str cont = sf_ref(content.dyn);

    FILE *f = fopen(p.c_str, "w");
    if (!f) {
        sf_str e = sf_str_fmt("File '%s' failed to open", p.c_str);
        sol_call_ex ex = sol_call_ex_ok(sol_dnerr(s, e.c_str));
        sf_str_free(e);
        return ex;
    }
    fwrite(cont.c_str, 1, cont.len, f);
    fclose(f);

    return sol_call_ex_ok(SOL_NIL);
}

void sol_mod_io(sol_state *s) {
    sol_val io = sol_dnew(s, SOL_DOBJ);
    sol_dobj_set(io.dyn, sf_lit("print"), sol_wrapcfun(s, io_print, 1, 0));
    sol_dobj_set(io.dyn, sf_lit("println"), sol_wrapcfun(s, io_println, 1, 0));
    sol_dobj_set(io.dyn, sf_lit("time"), sol_wrapcfun(s, io_time, 0, 0));
    sol_dobj_set(io.dyn, sf_lit("fread"), sol_wrapcfun(s, io_fread, 1, 0));
    sol_dobj_set(io.dyn, sf_lit("fwrite"), sol_wrapcfun(s, io_fwrite, 2, 0));
    sol_dobj_set(s->global.dyn, sf_lit("io"), io);
}
