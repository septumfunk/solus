#include "sf/str.h"
#include "solus/val.h"
#include "solus/vm.h"
#include "std.h"
#include <ctype.h>
#include <string.h>

static inline solu_i64 clamp_i64(solu_i64 v, solu_i64 lo, solu_i64 hi) {
    if (v < lo) return lo;
    if (v > hi) return hi;
    return v;
}

static solu_call_ex string_len(solu_state *s) {
    solu_val str = solu_selfc(s);
    expect_dtype(SOLU_DSTR, str);
    return solu_ok((solu_val){.tt = SOLU_TI64, .i64 = (solu_i64)(solu_dheader(str)->size - 1)});
}
static solu_call_ex string_sub(solu_state *s) {
    solu_val str = solu_selfc(s);
    expect_dtype(SOLU_DSTR, str);

    solu_val start = solu_get(s, 1);
    expect_type(SOLU_TI64, start);

    solu_val end = solu_get(s, 2);
    expect_type(SOLU_TI64, end);

    char *sstr = (char *)str.dyn;
    solu_i64 len = (solu_i64)solu_dheader(str)->size - 1;

    start.i64 = clamp_i64(start.i64, 0, len);
    end.i64 = clamp_i64(end.i64, 0, len);

    if (end.i64 < start.i64)
        return solu_panic(_strdup("end cannot be before start"));

    size_t slen = (size_t)(end.i64 - start.i64);
    char *buf = (char *)malloc(slen + 1);
    memcpy(buf, sstr + start.i64, slen);
    buf[slen] = '\0';
    solu_val nstr = solu_dnstr(s, buf);
    free(buf);

    return solu_ok(nstr);
}
static solu_call_ex string_repeat(solu_state *s) {
    solu_val str = solu_selfc(s);
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
        .thread = 1,
        .tt = SOLU_DSTR,
        .mark = SOLU_DYN_WHITE,
    };
    p = (char *)p + sizeof(solu_dalloc);
    for (solu_i64 i = 0; i < count.i64; ++i) {
        memcpy(p, str.dyn, size - 1);
        p = (char *)p + size - 1;
    }
    solu_dpush(s, dh);
    return solu_ok((solu_val){SOLU_TDYN, .dyn = dh + 1});
}
solu_call_ex string_join(solu_state *s) {
    solu_val strings = solu_get(s, 0);
    expect_dtype(SOLU_DOBJ, strings);
    solu_dobj *dobj = strings.dyn;
    sf_str final = sf_str_cdup("");
    for (solu_val *v = dobj->array.data; v < dobj->array.data + dobj->array.count; ++v) {
        if (solu_isdtype(*v, SOLU_DSTR))
            sf_str_append(&final, sf_ref(v->dyn));
    }
    solu_val str = solu_dnstr(s, final.c_str);
    sf_str_free(final);
    return solu_ok(str);
}
static solu_call_ex string_split(solu_state *s) {
    solu_val string = solu_selfc(s);
    expect_dtype(SOLU_DSTR, string);
    solu_val delim = solu_get(s, 1);
    expect_dtype(SOLU_DSTR, delim);

    char *src = string.dyn;
    size_t len = strlen(src);
    char *d = delim.dyn;
    size_t dlen = strlen(d);

    solu_val out = solu_dnew(s, SOLU_DOBJ);
    solu_dobj *o = out.dyn;

    if (len == 0) {
        solu_val empty = solu_dnstr(s, "");
        solu_valvec_push(&o->array, empty);
        return solu_ok(out);
    }
    if (dlen == 0) {
        solu_val whole = solu_dnstr(s, src);
        solu_valvec_push(&o->array, whole);
        return solu_ok(out);
    }

    const char *p = src;
    const char *end = src + len;
    const char *start = p;
    while (p <= end - (ptrdiff_t)dlen) {
        if (memcmp(p, d, dlen) == 0) {
            size_t tok_len = (size_t)(p - start);
            char *buf = malloc(tok_len + 1);
            memcpy(buf, start, tok_len);
            buf[tok_len] = 0;

            solu_val part = solu_dnstr(s, buf);
            free(buf);
            solu_valvec_push(&o->array, part);

            p += dlen;
            start = p;
            continue;
        }
        ++p;
    }

    size_t tok_len = (size_t)(end - start);
    char *buf = malloc(tok_len + 1);
    memcpy(buf, start, tok_len);
    buf[tok_len] = 0;

    solu_val part = solu_dnstr(s, buf);
    free(buf);
    solu_valvec_push(&o->array, part);

    return solu_ok(out);
}
static solu_call_ex string_ord(solu_state *s) {
    solu_val str = solu_selfc(s);
    expect_dtype(SOLU_DSTR, str);
    if (solu_dheader(str)->size - 1 < 1)
        return solu_panic("Empty string");
    return solu_ok((solu_val){SOLU_TI64, .i64=((char *)str.dyn)[0]});
}
static solu_call_ex string_upper(solu_state *s) {
    solu_val str = solu_selfc(s);
    expect_dtype(SOLU_DSTR, str);

    solu_dalloc *da = solu_dheader(str);
    char *up = malloc(da->size);
    assert(up && "Out of memory");
    for (size_t i = 0; i < da->size - 1; ++i)
        up[i] = (char)toupper(((char *)str.dyn)[i]);
    up[da->size - 1] = 0;

    str = solu_dnstr(s, up);
    free(up);
    return solu_ok(str);
}
static solu_call_ex string_lower(solu_state *s) {
    solu_val str = solu_selfc(s);
    expect_dtype(SOLU_DSTR, str);

    solu_dalloc *da = solu_dheader(str);
    char *up = malloc(da->size);
    assert(up && "Out of memory");
    for (size_t i = 0; i < da->size - 1; ++i)
        up[i] = (char)tolower(((char *)str.dyn)[i]);
    up[da->size - 1] = 0;

    str = solu_dnstr(s, up);
    free(up);
    return solu_ok(str);
}

solu_val solu_mod_string(solu_state *s, bool meta) {
    solu_val (*fun)(solu_state *, solu_cfunction, uint32_t, solu_val *, uint32_t) = meta ?
        solu_wrapmfun : solu_wrapcfun;

    solu_val string = solu_dnew(s, SOLU_DOBJ);
    solu_dobj_strset(string.dyn, "len", fun(s, string_len, 1, NULL, 0));
    solu_dobj_strset(string.dyn, "sub", fun(s, string_sub, 3, NULL, 0));
    solu_dobj_strset(string.dyn, "repeat", fun(s, string_repeat, 2, NULL, 0));
    solu_dobj_strset(string.dyn, "split", fun(s, string_split, 2, NULL, 0));
    solu_dobj_strset(string.dyn, "ord", fun(s, string_ord, 1, NULL, 0));
    solu_dobj_strset(string.dyn, "upper", fun(s, string_upper, 2, NULL, 0));
    solu_dobj_strset(string.dyn, "lower", fun(s, string_lower, 2, NULL, 0));

    // Builtins that extend to string
    if (meta) {
        solu_dobj_strset(string.dyn, "then", fun(s, builtin_then, 2, NULL, 0));
        solu_dobj_strset(string.dyn, "type", fun(s, builtin_type, 1, NULL, 0));
        solu_dobj_strset(string.dyn, "str", fun(s, builtin_str, 1, NULL, 0));
        solu_dobj_strset(string.dyn, "unwrap", fun(s, builtin_unwrap, 0, NULL, 0));
        solu_dobj_strset(string.dyn, "or_else", fun(s, builtin_or_else, 1, NULL, 0));
    } else { // Some don't make sense as member functions
        solu_dobj_strset(string.dyn, "join", fun(s, string_join, 1, NULL, 0));
        solu_dobj_strset(s->global.dyn, "string", string);
    }
    return string;
}
