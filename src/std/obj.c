#include "solus/bytecode.h"
#include "std.h"
#include <_stdio.h>
#include <setjmp.h>

static solu_call_ex obj_new(solu_state *s) {
    return solu_call_ex_ok(solu_dnew(s, SOLU_DOBJ));
}
static solu_call_ex obj_set(solu_state *s) {
    solu_val obj = solu_get(s, 0);
    expect_dtype(SOLU_DOBJ, obj);
    solu_val key = solu_get(s, 1);
    solu_val val = solu_get(s, 2);

    char *kstr;
    if (!solu_isdtype(key, SOLU_DSTR))
        kstr = solu_tostring(key);
    else kstr = _strdup(key.dyn);
    solu_dobj_set(obj.dyn, sf_own(kstr), val);
    return solu_call_ex_ok(SOLU_NIL);
}
static solu_call_ex obj_get(solu_state *s) {
    solu_val obj = solu_get(s, 0);
    expect_dtype(SOLU_DOBJ, obj);
    solu_val key = solu_get(s, 1);

    char *kstr;
    if (!solu_isdtype(key, SOLU_DSTR))
        kstr = solu_tostring(key);
    else kstr = key.dyn;

    solu_dobj_ex ex = solu_dobj_get(obj.dyn, sf_ref(kstr));
    if (!ex.is_ok) {
        sf_str estr = sf_str_fmt("Object does not contain member '%s'", kstr);
        free(kstr);
        return solu_call_ex_err((solu_call_err){SOLU_ERRV_MEMBER_NOT_FOUND, estr.c_str, 0});
    }
    free(kstr);

    return solu_call_ex_ok(ex.ok);
}

typedef struct {
    sf_str *out;
    bool pretty, commas;
    uint32_t id;
} _solu_stringify_args;
static void _stringify_fe(void *u, sf_str key, solu_val val);
static sf_str _stringify(solu_dobj *obj, bool pretty, bool commas, uint32_t id) {
    if (obj->pair_count == 0) return sf_lit("{}");
    sf_str out = sf_str_cdup(pretty ? "{\n" : "{ ");
    solu_dobj_foreach(obj, _stringify_fe, &(_solu_stringify_args){&out, pretty, commas, id});

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
            sf_str_append(args->out, _stringify(val.dyn, args->pretty, args->commas, args->id + 1));
            break;
        }
        default: {
            char *s = solu_tostring(val);
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
    solu_val obj = solu_get(s, 0);
    expect_dtype(SOLU_DOBJ, obj);
    solu_val pretty = solu_get(s, 1);
    solu_val commas = solu_get(s, 2);

    sf_str e = _stringify(
        obj.dyn,
        pretty.tt == SOLU_TBOOL ? pretty.boolean : true,
        commas.tt == SOLU_TBOOL ? commas.boolean : false,
        1
    );
    solu_call_ex ex = solu_call_ex_ok(solu_dnstr(s, e.c_str));
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
static solu_call_ex obj_foreach(solu_state *s) {
    solu_val self = solu_get(s, 0);
    expect_dtype(SOLU_DOBJ, self);
    solu_val callback = solu_get(s, 1);
    expect_dtype(SOLU_DFUN, callback);

    jmp_buf ctx;
    _obj_fe_args args = {s, callback.dyn, solu_call_ex_ok(SOLU_NIL), &ctx};
    if (setjmp(ctx) == 0)
        solu_dobj_foreach(self.dyn, _obj_fe, &args);
    else return args.ex;

    return solu_call_ex_ok(SOLU_NIL);
}

void solu_mod_obj(solu_state *s) {
    solu_val obj = solu_dnew(s, SOLU_DOBJ);
    solu_dobj_set(obj.dyn, sf_lit("new"), solu_wrapcfun(s, obj_new, 0, 0));
    solu_dobj_set(obj.dyn, sf_lit("set"), solu_wrapcfun(s, obj_set, 3, 0));
    solu_dobj_set(obj.dyn, sf_lit("get"), solu_wrapcfun(s, obj_get, 2, 0));
    solu_dobj_set(obj.dyn, sf_lit("stringify"), solu_wrapcfun(s, obj_stringify, 3, 0));
    solu_dobj_set(obj.dyn, sf_lit("foreach"), solu_wrapcfun(s, obj_foreach, 2, 0));
    solu_dobj_set(s->global.dyn, sf_lit("obj"), obj);
}
