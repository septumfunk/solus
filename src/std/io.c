#include "std.h"

static solu_call_ex io_print(solu_state *s) {
    solu_val to_print = solu_get(s, 0);
    char *val = solu_tostring(to_print);
    printf("%s", val);
    free(val);
    return solu_call_ex_ok(SOLU_NIL);
}
static solu_call_ex io_println(solu_state *s) {
    solu_val to_print = solu_get(s, 0);
    char *val = solu_tostring(to_print);
    printf("%s\n", val);
    free(val);
    return solu_call_ex_ok(SOLU_NIL);
}
static solu_call_ex io_time(solu_state *s) {
    (void)s;
    return solu_call_ex_ok((solu_val){.tt = SOLU_TF64, .f64 = solu_timesec()});
}
static solu_call_ex io_fread(solu_state *s) {
    solu_val path = solu_get(s, 0);
    expect_dtype(SOLU_DSTR, path);

    sf_str p = sf_ref(path.dyn);
    if (!sf_file_exists(p)) {
        sf_str e = sf_str_fmt("File '%s' not found", p.c_str);
        solu_call_ex ex = solu_call_ex_ok(solu_dnerr(s, e.c_str));
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
        solu_call_ex ex = solu_call_ex_ok(solu_dnerr(s, errs.c_str));
        sf_str_free(errs);
        return ex;
    }
    fsb.ok.flags = SF_BUFFER_GROW;
    sf_buffer_autoins(&fsb.ok, ""); // [\0]

    return solu_call_ex_ok(solu_dnstr(s, (char *)fsb.ok.ptr));
}
static solu_call_ex io_fwrite(solu_state *s) {
    solu_val path = solu_get(s, 0);
    expect_dtype(SOLU_DSTR, path);
    solu_val content = solu_get(s, 1);
    expect_dtype(SOLU_DSTR, content);

    sf_str p = sf_ref(path.dyn);
    sf_str cont = sf_ref(content.dyn);

    FILE *f = fopen(p.c_str, "w");
    if (!f) {
        sf_str e = sf_str_fmt("File '%s' failed to open", p.c_str);
        solu_call_ex ex = solu_call_ex_ok(solu_dnerr(s, e.c_str));
        sf_str_free(e);
        return ex;
    }
    fwrite(cont.c_str, 1, cont.len, f);
    fclose(f);

    return solu_call_ex_ok(SOLU_NIL);
}

void solu_mod_io(solu_state *s) {
    solu_val io = solu_dnew(s, SOLU_DOBJ);
    solu_dobj_set(io.dyn, sf_lit("print"), solu_wrapcfun(s, io_print, 1, 0));
    solu_dobj_set(io.dyn, sf_lit("println"), solu_wrapcfun(s, io_println, 1, 0));
    solu_dobj_set(io.dyn, sf_lit("time"), solu_wrapcfun(s, io_time, 0, 0));
    solu_dobj_set(io.dyn, sf_lit("fread"), solu_wrapcfun(s, io_fread, 1, 0));
    solu_dobj_set(io.dyn, sf_lit("fwrite"), solu_wrapcfun(s, io_fwrite, 2, 0));
    solu_dobj_set(s->global.dyn, sf_lit("io"), io);
}
