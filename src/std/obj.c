#include "sol/bytecode.h"
#include "std.h"
#include <_stdio.h>
#include <setjmp.h>

static sol_call_ex obj_new(sol_state *s) {
    return sol_call_ex_ok(sol_dnew(s, SOL_DOBJ));
}
static sol_call_ex obj_set(sol_state *s) {
    sol_val obj = sol_get(s, 0);
    expect_dtype(SOL_DOBJ, obj);
    sol_val key = sol_get(s, 1);
    sol_val val = sol_get(s, 2);

    char *kstr;
    if (!sol_isdtype(key, SOL_DSTR))
        kstr = sol_tostring(key);
    else kstr = strdup(key.dyn);
    sol_dobj_set(obj.dyn, sf_own(kstr), val);
    return sol_call_ex_ok(SOL_NIL);
}
static sol_call_ex obj_get(sol_state *s) {
    sol_val obj = sol_get(s, 0);
    expect_dtype(SOL_DOBJ, obj);
    sol_val key = sol_get(s, 1);

    char *kstr;
    if (!sol_isdtype(key, SOL_DSTR))
        kstr = sol_tostring(key);
    else kstr = key.dyn;

    sol_dobj_ex ex = sol_dobj_get(obj.dyn, sf_ref(kstr));
    if (!ex.is_ok) {
        sf_str estr = sf_str_fmt("Object does not contain member '%s'", kstr);
        free(kstr);
        return sol_call_ex_err((sol_call_err){SOL_ERRV_MEMBER_NOT_FOUND, estr.c_str, 0});
    }
    free(kstr);

    return sol_call_ex_ok(ex.ok);
}

typedef struct {
    sf_str *out;
    bool pretty, commas;
    uint32_t id;
} _sol_stringify_args;
static void _stringify_fe(void *u, sf_str key, sol_val val);
static sf_str _stringify(sol_dobj *obj, bool pretty, bool commas, uint32_t id) {
    if (obj->pair_count == 0) return sf_lit("{}");
    sf_str out = sf_str_cdup(pretty ? "{\n" : "{ ");
    sol_dobj_foreach(obj, _stringify_fe, &(_sol_stringify_args){&out, pretty, commas, id});

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
static void _stringify_fe(void *u, sf_str key, sol_val val) {
    _sol_stringify_args *args = u;
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
        case SOL_TDYN: if (sol_isdtype(val, SOL_DOBJ)) {
            sf_str_append(args->out, _stringify(val.dyn, args->pretty, args->commas, args->id + 1));
            break;
        }
        default: {
            char *s = sol_tostring(val);
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
sol_call_ex obj_stringify(sol_state *s) {
    sol_val obj = sol_get(s, 0);
    expect_dtype(SOL_DOBJ, obj);
    sol_val pretty = sol_get(s, 1);
    sol_val commas = sol_get(s, 2);

    sf_str e = _stringify(
        obj.dyn,
        pretty.tt == SOL_TBOOL ? pretty.boolean : true,
        commas.tt == SOL_TBOOL ? commas.boolean : false,
        1
    );
    sol_call_ex ex = sol_call_ex_ok(sol_dnstr(s, e.c_str));
    sf_str_free(e);
    return ex;
}
typedef struct {
    sol_state *s;
    sol_fproto *p;
    sol_call_ex ex;
    jmp_buf *b;
} _obj_fe_args;
static void _obj_fe(void *u, sf_str key, sol_val val) {
    _obj_fe_args *args = u;
    args->ex = sol_call(args->s, args->p, (sol_val []){
        sol_dnstr(args->s, key.c_str),
        val
    }, 2);
    if (!args->ex.is_ok)
        longjmp(*args->b, 1);
}
static sol_call_ex obj_foreach(sol_state *s) {
    sol_val self = sol_get(s, 0);
    expect_dtype(SOL_DOBJ, self);
    sol_val callback = sol_get(s, 1);
    expect_dtype(SOL_DFUN, callback);

    jmp_buf ctx;
    _obj_fe_args args = {s, callback.dyn, sol_call_ex_ok(SOL_NIL), &ctx};
    if (setjmp(ctx) == 0)
        sol_dobj_foreach(self.dyn, _obj_fe, &args);
    else return args.ex;

    return sol_call_ex_ok(SOL_NIL);
}

void sol_mod_obj(sol_state *s) {
    sol_val obj = sol_dnew(s, SOL_DOBJ);
    sol_dobj_set(obj.dyn, sf_lit("new"), sol_wrapcfun(s, obj_new, 0, 0));
    sol_dobj_set(obj.dyn, sf_lit("set"), sol_wrapcfun(s, obj_set, 3, 0));
    sol_dobj_set(obj.dyn, sf_lit("get"), sol_wrapcfun(s, obj_get, 2, 0));
    sol_dobj_set(obj.dyn, sf_lit("stringify"), sol_wrapcfun(s, obj_stringify, 3, 0));
    sol_dobj_set(obj.dyn, sf_lit("foreach"), sol_wrapcfun(s, obj_foreach, 2, 0));
    sol_dobj_set(s->global.dyn, sf_lit("obj"), obj);
}
