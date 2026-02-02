#include "std.h"

static sol_call_ex string_len(sol_state *s) {
    sol_val str = sol_get(s, 0);
    expect_dtype(SOL_DSTR, str);
    return sol_call_ex_ok((sol_val){.tt = SOL_TI64, .i64 = (sol_i64)(sol_dheader(str)->size - 1)});
}
static sol_call_ex string_sub(sol_state *s) {
    sol_val str = sol_get(s, 0);
    expect_dtype(SOL_DSTR, str);
    sol_val start = sol_get(s, 1);
    expect_type(SOL_TI64, start);
    sol_val end = sol_get(s, 2);
    expect_type(SOL_TI64, end);
    if (end.i64 < start.i64)
        return sol_serr(SOL_ERRV_PANIC, strdup("end cannot be before start"));

    sf_str *sstr = str.dyn;
    sol_i64 len = (sol_i64)sstr->len;
    if (len == 0)
        return sol_call_ex_ok(sol_dnew(s, SOL_DSTR));
    start.i64 = max(0, min(start.i64, len > 0 ? len - 1 : 0));
    end.i64 = max(0, min(end.i64, len > 0 ? len - 1 : 0));

    size_t slen = (size_t)(end.i64 - start.i64 + 1);
    char *buf = malloc(slen + 1);
    memcpy(buf, sstr->c_str + start.i64, slen - 1);
    buf[slen] = 0;
    sol_val nstr = sol_dnstr(s, buf);
    free(buf);
    return sol_call_ex_ok(nstr);
}
static sol_call_ex string_repeat(sol_state *s) {
    sol_val str = sol_get(s, 0);
    expect_dtype(SOL_DSTR, str);
    sol_val count = sol_get(s, 1);
    expect_type(SOL_TI64, count);

    size_t size = sol_dheader(str)->size;
    size_t strlen = (size - 1) * (size_t)count.i64 + 1;
    sol_dyn p = calloc(1, sizeof(sol_dalloc) + strlen);
    sol_dalloc *dh = p;
    *dh = (sol_dalloc){
        .next = NULL,
        .size = strlen,
        .tt = SOL_DSTR,
        .mark = SOL_DYN_WHITE,
    };
    p = (char *)p + sizeof(sol_dalloc);
    for (sol_i64 i = 0; i < count.i64; ++i) {
        memcpy(p, str.dyn, size - 1);
        p = (char *)p + size - 1;
    }
    sol_dpush(s, dh);
    return sol_call_ex_ok((sol_val){SOL_TDYN, .dyn = dh + 1});
}

void sol_mod_string(sol_state *s) {
    sol_val string = sol_dnew(s, SOL_DOBJ);
    sol_dobj_set(string.dyn, sf_lit("len"), sol_wrapcfun(s, string_len, 1, 0));
    sol_dobj_set(string.dyn, sf_lit("sub"), sol_wrapcfun(s, string_sub, 3, 0));
    sol_dobj_set(string.dyn, sf_lit("repeat"), sol_wrapcfun(s, string_repeat, 2, 0));
    sol_dobj_set(s->global.dyn, sf_lit("string"), string);
}
