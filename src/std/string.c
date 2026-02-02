#include "std.h"

static solu_call_ex string_len(solu_state *s) {
    solu_val str = solu_get(s, 0);
    expect_dtype(SOLU_DSTR, str);
    return solu_call_ex_ok((solu_val){.tt = SOLU_TI64, .i64 = (solu_i64)(solu_dheader(str)->size - 1)});
}
static solu_call_ex string_sub(solu_state *s) {
    solu_val str = solu_get(s, 0);
    expect_dtype(SOLU_DSTR, str);
    solu_val start = solu_get(s, 1);
    expect_type(SOLU_TI64, start);
    solu_val end = solu_get(s, 2);
    expect_type(SOLU_TI64, end);
    if (end.i64 < start.i64)
        return solu_serr(SOLU_ERRV_PANIC, _strdup("end cannot be before start"));

    sf_str *sstr = str.dyn;
    solu_i64 len = (solu_i64)sstr->len;
    if (len == 0)
        return solu_call_ex_ok(solu_dnew(s, SOLU_DSTR));
    start.i64 = max(0, min(start.i64, len > 0 ? len - 1 : 0));
    end.i64 = max(0, min(end.i64, len > 0 ? len - 1 : 0));

    size_t slen = (size_t)(end.i64 - start.i64 + 1);
    char *buf = malloc(slen + 1);
    memcpy(buf, sstr->c_str + start.i64, slen - 1);
    buf[slen] = 0;
    solu_val nstr = solu_dnstr(s, buf);
    free(buf);
    return solu_call_ex_ok(nstr);
}
static solu_call_ex string_repeat(solu_state *s) {
    solu_val str = solu_get(s, 0);
    expect_dtype(SOLU_DSTR, str);
    solu_val count = solu_get(s, 1);
    expect_type(SOLU_TI64, count);

    size_t size = solu_dheader(str)->size;
    size_t strlen = (size - 1) * (size_t)count.i64 + 1;
    solu_dyn p = calloc(1, sizeof(solu_dalloc) + strlen);
    solu_dalloc *dh = p;
    *dh = (solu_dalloc){
        .next = NULL,
        .size = strlen,
        .tt = SOLU_DSTR,
        .mark = SOLU_DYN_WHITE,
    };
    p = (char *)p + sizeof(solu_dalloc);
    for (solu_i64 i = 0; i < count.i64; ++i) {
        memcpy(p, str.dyn, size - 1);
        p = (char *)p + size - 1;
    }
    solu_dpush(s, dh);
    return solu_call_ex_ok((solu_val){SOLU_TDYN, .dyn = dh + 1});
}

void solu_mod_string(solu_state *s) {
    solu_val string = solu_dnew(s, SOLU_DOBJ);
    solu_dobj_set(string.dyn, sf_lit("len"), solu_wrapcfun(s, string_len, 1, 0));
    solu_dobj_set(string.dyn, sf_lit("sub"), solu_wrapcfun(s, string_sub, 3, 0));
    solu_dobj_set(string.dyn, sf_lit("repeat"), solu_wrapcfun(s, string_repeat, 2, 0));
    solu_dobj_set(s->global.dyn, sf_lit("string"), string);
}
