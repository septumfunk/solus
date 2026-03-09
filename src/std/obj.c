#include "solus/val.h"
#include "solus/vm.h"
#include "std.h"
#include <setjmp.h>

static solu_call_ex obj_new(solu_state *s) {
    return solu_ok(solu_dnew(s, SOLU_DOBJ));
}
static solu_call_ex obj_set(solu_state *s) {
    solu_val obj = solu_selfc(s);
    expect_dtype(SOLU_DOBJ, obj);
    solu_dobj_set(s, obj.dyn, solu_get(s, 1), solu_get(s, 2));
    return solu_ok(SOLU_NIL);
}
static solu_call_ex obj_get(solu_state *s) {
    solu_val obj = solu_get(s, 0);
    if (!solu_isdtype(obj, SOLU_DOBJ) && s->ccall->up_c)
        obj = solu_capturec(s, 0);
    expect_dtype(SOLU_DOBJ, obj);
    return solu_ok(solu_dobj_get(s, obj.dyn, solu_get(s, 1)));
}

static solu_call_ex obj_usemeta(solu_state *s) {
    solu_val obj = solu_selfc(s);
    expect_dtype(SOLU_DOBJ, obj);
    solu_val meta = solu_get(s, 1);
    solu_dobj *objp = obj.dyn;
    if (meta.tt == SOLU_TNIL) {
        memset(&objp->metafuns, 0, SOLU_META_COUNT * sizeof(solu_val));
        objp->meta = meta;
        return solu_ok(SOLU_NIL);
    }
    expect_dtype(SOLU_DOBJ, meta);

    solu_dobj *metap = meta.dyn;
    solu_usemeta(objp, metap);

    return solu_ok(SOLU_NIL);
}
static solu_call_ex obj_meta(solu_state *s) {
    solu_val obj = solu_selfc(s);
    expect_dtype(SOLU_DOBJ, obj);
    return solu_ok(((solu_dobj *)obj.dyn)->meta);
}

typedef struct {
    solu_state *s;
    sf_str *out;
    bool pretty, commas;
    uint32_t id;
} _solu_stringify_args;
static void _stringify_fe(void *u, sf_str key, solu_val val);
static sf_str _stringify(solu_state *s, solu_dobj *obj, bool pretty, bool commas, uint32_t id) {
    if (obj->map.pair_count == 0) return sf_lit("{}");
    sf_str out = sf_str_cdup(pretty ? "{\n" : "{ ");
    solu_valmap_foreach(&obj->map, _stringify_fe, &(_solu_stringify_args){s, &out, pretty, commas, id});

    if (pretty && id) {
        size_t s = sizeof(char) * (id-1) * 2;
        char *idt = malloc(s + 1);
        memset(idt, ' ', sizeof(char) * (id-1) * 2);
        sf_str_append(&out, sf_ref(idt));
        free(idt);
    }
    sf_str_append(&out, sf_lit("}"));
    return out;
}
static void _stringify_fe(void *u, sf_str key, solu_val val) {
    _solu_stringify_args *args = u;
    if (args->pretty && args->id) {
        size_t s = sizeof(char) * args->id * 2;
        char *id = malloc(s + 1);
        memset(id, ' ', sizeof(char) * args->id * 2);
        sf_str_append(args->out, sf_ref(id));
        free(id);
    }
    sf_str_append(args->out, key);
    sf_str_append(args->out, sf_lit(" = "));
    switch (val.tt) {
        case SOLU_TDYN: if (solu_isdtype(val, SOLU_DOBJ)) {
            sf_str_append(args->out, _stringify(args->s, val.dyn, args->pretty, args->commas, args->id + 1));
            break;
        }
        default: {
            char *s = solu_tostr(args->s, val);
            if (solu_isdtype(val, SOLU_DSTR)) {
                sf_str s2 = sf_str_fmt("\"%s\"", s);
                free(s);
                s = s2.c_str;
            }
            sf_str_append(args->out, sf_ref(s));
            free(s);
            break;
        }
    }
    sf_str ec = sf_lit(" ");
    if (args->pretty && args->commas)
        ec = sf_lit(",\n");
    else if (args->commas)
        ec = sf_lit(",");
    else if (args->pretty)
        ec = sf_lit("\n");
    sf_str_append(args->out, ec);
}
solu_call_ex obj_stringify(solu_state *s) {
    solu_val obj = solu_selfc(s);
    expect_dtype(SOLU_DOBJ, obj);
    solu_val pretty = solu_get(s, 1);
    solu_val commas = solu_get(s, 2);

    sf_str e = _stringify(
        s,
        obj.dyn,
        pretty.tt == SOLU_TBOOL ? pretty.boolean : true,
        commas.tt == SOLU_TBOOL ? commas.boolean : false,
        1
    );
    solu_call_ex ex = solu_ok(solu_dnstr(s, e.c_str));
    sf_str_free(e);
    return ex;
}
typedef struct {
    solu_state *s;
    solu_fproto *p;
    solu_call_ex ex;
    jmp_buf *b;
} _obj_fe_args;
static void _obj_fe(void *u, sf_str key, solu_val val) {
    _obj_fe_args *args = u;
    args->ex = solu_call(args->s, args->p, (solu_val []){
        solu_dnstr(args->s, key.c_str),
        val
    }, 2);
    if (!args->ex.is_ok)
        longjmp(*args->b, 1);
}
static solu_call_ex obj_pairs(solu_state *s) {
    solu_val obj = solu_selfc(s);
    expect_dtype(SOLU_DOBJ, obj);
    solu_val callback = solu_get(s, 1);
    expect_dtype(SOLU_DFUN, callback);

    jmp_buf ctx;
    _obj_fe_args args = {s, callback.dyn, solu_ok(SOLU_NIL), &ctx};
    if (setjmp(ctx) == 0)
        solu_valmap_foreach(&((solu_dobj *)obj.dyn)->map, _obj_fe, &args);
    else return args.ex;

    return solu_ok(SOLU_NIL);
}
static solu_call_ex obj_foreach(solu_state *s) {
    solu_val array = solu_selfc(s);
    expect_dtype(SOLU_DOBJ, array);
    solu_val callback = solu_get(s, 1);
    expect_dtype(SOLU_DFUN, callback);

    solu_dobj *o = array.dyn;
    for (uint32_t i = 0; i < o->array.count; ++i) {
        solu_call_ex ex = solu_call(s, callback.dyn,
            (solu_val[]){o->array.data[i], (solu_val){SOLU_TI64, .i64 = (solu_i64)i}},
        2);
        if (!ex.is_ok) return ex;
    }
    return solu_ok(SOLU_NIL);
}
static solu_call_ex obj_range(solu_state *s) {
    solu_val array = solu_selfc(s);
    expect_dtype(SOLU_DOBJ, array);
    solu_val start = solu_get(s, 1);
    expect_type(SOLU_TI64, start);
    solu_val end = solu_get(s, 2);
    expect_type(SOLU_TI64, end);

    solu_dobj *o = array.dyn;
    solu_val new = solu_dnew(s, SOLU_DOBJ);
    for (solu_i64 i = 0; i < o->array.count; ++i)
        if (i >= start.i64 && i <= end.i64)
            solu_valvec_push(&((solu_dobj *)new.dyn)->array, o->array.data[i]);
    return solu_ok(SOLU_NIL);
}
static solu_call_ex obj_members(solu_state *s) {
    solu_val obj = solu_selfc(s);
    expect_dtype(SOLU_DOBJ, obj);
    return solu_ok((solu_val){SOLU_TI64, .i64 = (solu_i64)((solu_dobj *)obj.dyn)->map.pair_count});
}
static solu_call_ex obj_len(solu_state *s) {
    solu_val obj = solu_selfc(s);
    expect_dtype(SOLU_DOBJ, obj);
    return solu_ok((solu_val){SOLU_TI64, .i64 = (solu_i64)((solu_dobj *)obj.dyn)->array.count});
}

typedef struct {
    solu_state *s;
    solu_val fun;
} solu_template;
static solu_call_ex template_tostring(solu_state *s) {
    solu_template *temp = solu_capturec(s, 0).dyn;
    solu_call_ex ex = solu_call(temp->s, temp->fun.dyn, NULL, 0);
    if (!ex.is_ok) {
        sf_str fmt = sf_str_fmt("<%s>", ex.err.panic ? ex.err.panic : solu_err_string(ex.err.tt));
        solu_panic_cleanup(ex);
        solu_val o = solu_dnstr(s, fmt.c_str);
        sf_str_free(fmt);
        return solu_ok(o);
    }
    char *ss = solu_tostr(temp->s, ex.ok);
    solu_val o = solu_dnstr(s, ss);
    free(ss);
    return solu_ok(o);
}
static void template_mark(void *_temp) {
    solu_template *temp = _temp;
    solu_dheader(temp->fun)->mark = SOLU_DYN_BLACK;
    solu_dmark(temp->fun);
}

static solu_call_ex obj_template(solu_state *s) {
    solu_val serialize = solu_get(s, 0);
    expect_dtype(SOLU_DFUN, serialize);
    solu_val ud = solu_dnusr(s, sizeof(solu_template), "template", &(solu_template){
        s, serialize,
    }, NULL, template_mark);
    solu_uheader(ud)->metafuns[SOLU_META_STR] = solu_wrapcfun(s, template_tostring, 0, &ud, 1);
    return solu_ok(ud);
}

solu_val solu_mod_obj(solu_state *s) {
    solu_val obj = solu_dnew(s, SOLU_DOBJ);
    solu_dobj_strset(obj.dyn, "new", solu_wrapmfun(s, obj_new, 0, NULL, 0));
    solu_dobj_strset(obj.dyn, "set", solu_wrapmfun(s, obj_set, 3, NULL, 0));
    solu_dobj_strset(obj.dyn, "get", solu_wrapmfun(s, obj_get, 2, NULL, 0));
    solu_dobj_strset(obj.dyn, "usemeta", solu_wrapmfun(s, obj_usemeta, 2, NULL, 0));
    solu_dobj_strset(obj.dyn, "meta", solu_wrapmfun(s, obj_meta, 1, NULL, 0));
    solu_dobj_strset(obj.dyn, "stringify", solu_wrapmfun(s, obj_stringify, 3, NULL, 0));
    solu_dobj_strset(obj.dyn, "pairs", solu_wrapmfun(s, obj_pairs, 2, NULL, 0));
    solu_dobj_strset(obj.dyn, "foreach", solu_wrapmfun(s, obj_foreach, 2, NULL, 0));
    solu_dobj_strset(obj.dyn, "range", solu_wrapmfun(s, obj_range, 3, NULL, 0));

    solu_dobj_strset(obj.dyn, "members", solu_wrapmfun(s, obj_members, 1, NULL, 0));
    solu_dobj_strset(obj.dyn, "len", solu_wrapmfun(s, obj_len, 1, NULL, 0));

    solu_dobj_strset(obj.dyn, "template", solu_wrapmfun(s, obj_template, 1, NULL, 0));

    solu_dobj_strset(s->global.dyn, "obj", obj);
    return obj;
}
