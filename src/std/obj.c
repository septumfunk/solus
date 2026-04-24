#include "sf/str.h"
#include "solus/val.h"
#include "solus/vm.h"
#include "std.h"
#include <setjmp.h>

/*
 * obj.usemeta(self: obj, meta: obj)
 * Set the metaobj of an object. This defines custom behavior on the object.
 * Note that if you update the metafuns on the metaobj, you must use it again.
 */
static solu_call_ex obj_usemeta(solu_state *s) {
    solu_val obj = solu_selfc(s);
    expect_type(SOLU_TDYN, obj);
    solu_val meta = solu_get(s, 1);
    solu_dalloc *objp = solu_dheader(obj);
    if (meta.tt == SOLU_TNIL) {
        // Remove meta
        memset(&objp->metadata, 0, SOLU_META_COUNT * sizeof(solu_val));
        objp->meta = meta;
        return solu_ok(SOLU_NIL);
    }
    expect_dtype(SOLU_DOBJ, meta);

    solu_dobj *metap = meta.dyn;
    solu_usemeta(obj, metap);

    return solu_ok(obj);
}

/*
 * obj.meta(self: obj) -> obj|nil
 * Get the metadata of an object
 */
static solu_call_ex obj_meta(solu_state *s) {
    solu_val obj = solu_selfc(s);
    expect_type(SOLU_TDYN, obj);
    return solu_ok(solu_dheader(obj)->meta);
}

typedef struct {
    solu_state *s;
    sf_str *out;
    bool pretty, commas;
    uint32_t id;
} _solu_stringify_args;
static void _stringify_fe(void *u, sf_str key, solu_val val);
static sf_str _stringify(solu_state *s, solu_dobj *obj, bool pretty, bool commas, uint32_t id) {
    if (obj->map.pair_count == 0 && obj->array.count == 0) return sf_lit("{}");
    sf_str out = sf_str_cdup(pretty ? "{\n" : "{ ");
    solu_valmap_foreach(&obj->map, _stringify_fe, &(_solu_stringify_args){s, &out, pretty, commas, id});

    if (obj->array.count) {
        // Serialize array
        if (pretty) {
            // Indent
            size_t size = sizeof(char) * id * 2;
            char *idt = malloc(size + 1);
            memset(idt, ' ', sizeof(char) * id * 2);
            idt[size] = 0;

            sf_str_append(&out, sf_ref(idt));
            free(idt);
        }
        for (uint32_t i = 0; i < obj->array.count; ++i) {
            solu_val val = obj->array.data[i];
            switch (val.tt) {
                case SOLU_TDYN: if (solu_isdtype(val, SOLU_DOBJ)) {
                    // {x = 2, y = 2, 1, 2, 3}
                    sf_str_append(&out, _stringify(s, val.dyn, false, true, id + 1));
                    break;
                }
                default: {
                    char *str = solu_tostr(s, val);
                    if (solu_isdtype(val, SOLU_DSTR)) {
                        sf_str s2 = sf_str_fmt("\"%s\"", str);
                        free(str);
                        str = s2.c_str;
                    }
                    sf_str_append(&out, sf_ref(str));
                    free(str);
                    break;
                }
            }
            sf_str_append(&out, i == obj->array.count - 1 ?
                (pretty ? sf_lit("\n") : sf_lit(" ")) :
                sf_lit(", "));
        }
    }

    if (pretty && id) {
        // Indent
        size_t s = sizeof(char) * (id-1) * 2;
        char *idt = malloc(s + 1);
        memset(idt, ' ', s);
        idt[s] = 0;

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
        memset(id, ' ', s);
        id[s] = 0;

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

    // End chars
    sf_str ec = sf_lit(" ");
    if (args->pretty && args->commas)
        ec = sf_lit(",\n");
    else if (args->commas)
        ec = sf_lit(", ");
    else if (args->pretty)
        ec = sf_lit("\n");

    sf_str_append(args->out, ec);
}

/*
 * obj.stringify(self: obj)
 * Converts an object into a string representation
 */
static solu_call_ex obj_stringify(solu_state *s) {
    solu_val obj = solu_selfc(s);
    expect_dtype(SOLU_DOBJ, obj);
    solu_val pretty = solu_get(s, 1);
    solu_val commas = solu_get(s, 2);

    sf_str e = _stringify(
        s,
        obj.dyn,
        solu_truthy(pretty),
        solu_truthy(commas),
        1
    );
    solu_call_ex ex = solu_ok(solu_dnstr(s, e.c_str));
    sf_str_free(e);
    return ex;
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

/*
 * obj.template(fmt: fun) -> usr:template
 * Creates a special member that will call a function on stringification
 */
static solu_call_ex obj_template(solu_state *s) {
    solu_val serialize = solu_get(s, 0);
    expect_dtype(SOLU_DFUN, serialize);
    solu_val ud = solu_dnusr(s, sizeof(solu_template), "template", &(solu_template){
        s, serialize,
    }, NULL, template_mark);
    solu_dheader(ud)->metadata[SOLU_META_STR] = solu_wrapcfun(s, template_tostring, 0, &ud, 1);
    return solu_ok(ud);
}


// Map

/*
 * obj.set(self: obj, key: str, val: any)
 * Sets the object’s member at the specified key.
 */
static solu_call_ex obj_set(solu_state *s) {
    solu_val obj = solu_selfc(s);
    solu_val key = solu_get(s, 1);
    solu_val value = solu_get(s, 2);
    solu_dalloc *dh = solu_dheader(obj);
    if (dh && dh->metadata[SOLU_META_SET].tt != SOLU_TNIL) {
        solu_call_ex ex = solu_call(s, dh->metadata[SOLU_META_SET].dyn,
            (solu_val[]){obj, key, value}, 3);
        if (!ex.is_ok) return ex;
        return solu_ok(ex.ok);
    }
    expect_dtype(SOLU_DOBJ, obj);
    solu_dobj_set(s, obj.dyn, key, value);
    return solu_ok(SOLU_NIL);
}

/*
 * obj.setr(self: obj, key: str, value: any)
 * Sets the object’s "raw" member at the specified key, bypassing `_set` metadata
 */
static solu_call_ex obj_setr(solu_state *s) {
    solu_val obj = solu_selfc(s);
    expect_dtype(SOLU_DOBJ, obj);
    solu_dobj_set(s, obj.dyn, solu_get(s, 1), solu_get(s, 2));
    return solu_ok(SOLU_NIL);
}

/*
 * obj.get(self: obj, key: str) -> any
 * Gets the object’s member at the specified key. Useful for non identifier
 * friendly member keys.
 */
static solu_call_ex obj_get(solu_state *s) {
    solu_val obj = solu_selfc(s);
    solu_val key = solu_get(s, 1);
    solu_dalloc *dh = solu_dheader(obj);
    if (dh && dh->metadata[SOLU_META_GET].tt != SOLU_TNIL) {
        solu_call_ex ex = solu_call(s, dh->metadata[SOLU_META_GET].dyn,
            (solu_val[]){obj, key}, 2);
        if (!ex.is_ok) return ex;
        return solu_ok(ex.ok);
    }
    expect_dtype(SOLU_DOBJ, obj);
    return solu_ok(solu_dobj_get(s, obj.dyn, solu_get(s, 1)));
}

/*
 * obj.getr(obj: object, key: str) -> any
 * Gets the object’s "raw" member at the specified key, bypassing `_get` metadata
 */
static solu_call_ex obj_getr(solu_state *s) {
    solu_val obj = solu_selfc(s);
    expect_dtype(SOLU_DOBJ, obj);
    return solu_ok(solu_dobj_get(s, obj.dyn, solu_get(s, 1)));
}

/*
 * obj.members(obj: obj) -> i64
 * Returns the total amount of named members stored in the map component of the object.
 */
static solu_call_ex obj_members(solu_state *s) {
    solu_val obj = solu_selfc(s);
    expect_dtype(SOLU_DOBJ, obj);
    return solu_ok((solu_val){SOLU_TI64, .i64 = (solu_i64)((solu_dobj *)obj.dyn)->map.pair_count});
}

typedef struct {
    solu_state *s;
    solu_fproto *p;
    solu_call_ex ex;
    jmp_buf *b;
    solu_val match;
} _obj_fe_args;
static void _obj_fe(void *u, sf_str key, solu_val val) {
    _obj_fe_args *args = u;
    // Callback
    args->ex = solu_call(args->s, args->p, (solu_val []){
        solu_dnstr(args->s, key.c_str),
        val
    }, 2);
    if (!args->ex.is_ok)
        longjmp(*args->b, 1); // Panic behavior
    else args->match = val;
}

/*
 * obj.pairs(self: obj, callback: fun)
 * Runs a callback passing each key/value pair in the object’s members component
 */
static solu_call_ex obj_pairs(solu_state *s) {
    solu_val obj = solu_selfc(s);
    expect_dtype(SOLU_DOBJ, obj);
    solu_val callback = solu_get(s, 1);
    expect_dtype(SOLU_DFUN, callback);

    jmp_buf ctx;
    _obj_fe_args args = {s, callback.dyn, solu_ok(SOLU_NIL), &ctx, SOLU_NIL};
    if (setjmp(ctx) == 0)
        solu_valmap_foreach(&((solu_dobj *)obj.dyn)->map, _obj_fe, &args);
    else return args.ex; // Panic

    return solu_ok(SOLU_NIL);
}

/*
 * obj.find(self: obj, callback: fun) -> any
 * Returns *the first* member in the object with which the callback returns
 * true or a truthy value.
 */
static solu_call_ex obj_find(solu_state *s) {
    solu_val obj = solu_selfc(s);
    expect_dtype(SOLU_DOBJ, obj);
    solu_val callback = solu_get(s, 1);
    expect_dtype(SOLU_DFUN, callback);

    jmp_buf ctx;
    _obj_fe_args args = {s, callback.dyn, solu_ok(SOLU_NIL), &ctx, SOLU_NIL};
    if (setjmp(ctx) == 0) {
        solu_valmap_foreach(&((solu_dobj *)obj.dyn)->map, _obj_fe, &args);
        if (solu_truthy(args.match))
            return solu_ok(args.match);
    } else return args.ex; // Panic

    return solu_ok(SOLU_NIL);
}

/*
 * obj.match(self: obj, callback: fun) -> obj
 * Returns *all* members in the object with which the callback returns true
 * or a truthy value.
 */
static solu_call_ex obj_match(solu_state *s) {
    solu_val obj = solu_selfc(s);
    expect_dtype(SOLU_DOBJ, obj);
    solu_val callback = solu_get(s, 1);
    expect_dtype(SOLU_DFUN, callback);

    solu_val out = solu_dnew(s, SOLU_DOBJ);
    solu_dhold(out);

    jmp_buf ctx;
    _obj_fe_args args = {s, callback.dyn, solu_ok(SOLU_NIL), &ctx, SOLU_NIL};
    if (setjmp(ctx) == 0) {
        solu_valmap_foreach(&((solu_dobj *)obj.dyn)->map, _obj_fe, &args);
        if (solu_truthy(args.match)) {
            solu_valvec_push(&((solu_dobj *)out.dyn)->array, args.match);
            args.match = SOLU_NIL;
        }
    } else return args.ex; // Panic

    solu_drelease(out);
    return solu_ok(out);
}

/*
 * obj.has(self: obj, key: str) -> bool
 * Returns whether the object has been assigned a member with the specified key name.
 * Members must be explicitly deleted with the delete `obj -= 'key'` operator.
 */
static solu_call_ex obj_has(solu_state *s) {
    solu_val obj = solu_selfc(s);
    expect_dtype(SOLU_DOBJ, obj);
    solu_val key = solu_get(s, 1);
    expect_dtype(SOLU_DSTR, key);

    return solu_ok((solu_val){
        SOLU_TBOOL,
        .boolean = solu_valmap_get(&((solu_dobj *)obj.dyn)->map, sf_ref(key.dyn)).is_ok
    });
}

// Array

/*
 * obj.push(self: obj, val: any)
 * Pushes or inserts a value at the very *end* of an array
 */
static solu_call_ex obj_push(solu_state *s) {
    solu_val obj = solu_selfc(s);
    expect_dtype(SOLU_DOBJ, obj);
    solu_dobj *d = obj.dyn;
    solu_valvec_push(&d->array, solu_get(s, 1));
    return solu_ok(SOLU_NIL);
}

/*
 * obj.push_back(self: obj, val: any)
 * Pushes or inserts a value at the very *back* of an array
 */
static solu_call_ex obj_push_back(solu_state *s) {
    solu_val obj = solu_selfc(s);
    expect_dtype(SOLU_DOBJ, obj);
    solu_dobj *d = obj.dyn;
    solu_valvec_insert(&d->array, 0, solu_get(s, 1));
    return solu_ok(SOLU_NIL);
}

/*
 * obj.pop(self: obj) -> any
 * Pops off a value from the very *end* of an array, removing and returning it.
 * Returns `nil` if the array is empty.
 */
static solu_call_ex obj_pop(solu_state *s) {
    solu_val obj = solu_selfc(s);
    expect_dtype(SOLU_DOBJ, obj);
    solu_dobj *d = obj.dyn;
    if (d->array.count == 0)
        return solu_ok(SOLU_NIL);
    return solu_ok(solu_valvec_pop(&d->array));
}

/*
 * obj.pop_back(self: obj) -> any
 * Pops off a value from the very *back* of an array, removing and returning it.
 * Returns `nil` if the array is empty.
 */
static solu_call_ex obj_pop_back(solu_state *s) {
    solu_val obj = solu_selfc(s);
    expect_dtype(SOLU_DOBJ, obj);
    solu_dobj *d = obj.dyn;
    if (d->array.count == 0)
        return solu_ok(SOLU_NIL);
    solu_val o = d->array.data[0];
    solu_valvec_delete(&d->array, 0);
    return solu_ok(o);
}

/*
 * obj.remove(self: obj, ) -> any|err
 * Removes and returns a value from the array at a specific index, acting like pop.
 * You can combine this with `obj.where` to match a specific value.
 * Fails if index is out of bounds.
 */
static solu_call_ex obj_remove(solu_state *s) {
    solu_val obj = solu_selfc(s);
    expect_dtype(SOLU_DOBJ, obj);
    solu_dobj *d = obj.dyn;
    solu_val index = solu_get(s, 1);
    expect_type(SOLU_TI64, index);
    if (d->array.count == 0 || index.i64 < 0 || index.i64 > d->array.count)
        return solu_err(s, "index %lld out-of-bounds (size: %u)", index.i64, d->array.count);

    solu_val o = d->array.data[index.i64];
    solu_valvec_delete(&d->array, (uint32_t)index.i64);
    return solu_ok(o);
}

/*
 * obj.foreach(self: obj, callback: fun)
 * Runs a callback passing each value in the object’s array component
 */
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

/*
 * obj.reverse(self: obj) -> obj
 * Returns an array containing the same values as the original array but reversed.
 */
static solu_call_ex obj_reverse(solu_state *s) {
    solu_val array = solu_selfc(s);
    expect_dtype(SOLU_DOBJ, array);

    solu_val out = solu_dnew(s, SOLU_DOBJ);
    solu_dobj *o = array.dyn, *o_out = out.dyn;
    for (uint32_t i = 0; i < o->array.count; ++i)
        solu_valvec_push(&o_out->array, o->array.data[o->array.count - 1 - i]);
    return solu_ok(out);
}

/*
 * obj.len(obj: obj) -> i64
 * Returns the total length of the array component of the object
 */
static solu_call_ex obj_len(solu_state *s) {
    solu_val obj = solu_selfc(s);
    expect_dtype(SOLU_DOBJ, obj);
    return solu_ok((solu_val){SOLU_TI64, .i64 = (solu_i64)((solu_dobj *)obj.dyn)->array.count});
}

/*
 * obj.range(self: obj, start: i64, end: i64) -> obj
 * Returns an inclusive range of values from an object's array component
 */
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
    return solu_ok(new);
}

/*
 * obj.where(self: obj, callback: fun) -> any
 * Returns *the first* value in the array with which the callback returns true or a truthy value.
 */
static solu_call_ex obj_where(solu_state *s) {
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
        if (solu_truthy(ex.ok))
            return solu_ok(o->array.data[i]);
    }
    return solu_ok(SOLU_NIL);
}

/*
 * obj.all(self: obj, callback: fun) -> obj
 * Returns *all* values in the array with which the callback returns true or a truthy value.
 */
static solu_call_ex obj_all(solu_state *s) {
    solu_val array = solu_selfc(s);
    expect_dtype(SOLU_DOBJ, array);
    solu_val callback = solu_get(s, 1);
    expect_dtype(SOLU_DFUN, callback);

    solu_val out = solu_dnew(s, SOLU_DOBJ);
    solu_dhold(out);

    solu_dobj *o = array.dyn;
    for (uint32_t i = 0; i < o->array.count; ++i) {
        solu_call_ex ex = solu_call(s, callback.dyn,
            (solu_val[]){o->array.data[i], (solu_val){SOLU_TI64, .i64 = (solu_i64)i}},
        2);
        if (!ex.is_ok) return ex;
        if (solu_truthy(ex.ok))
            solu_valvec_push(&((solu_dobj *)out.dyn)->array, o->array.data[i]);
    }

    solu_drelease(out);
    return solu_ok(out);
}

/*
 * obj.map(self: obj, callback: fun) -> obj
 * Builds a map using all values in the array, performing a transformation
 * on each element via callback.
 */
static solu_call_ex obj_map(solu_state *s) {
    solu_val array = solu_selfc(s);
    expect_dtype(SOLU_DOBJ, array);
    solu_val callback = solu_get(s, 1);
    expect_dtype(SOLU_DFUN, callback);

    solu_val out = solu_dnew(s, SOLU_DOBJ);
    solu_dhold(out);

    solu_dobj *o = array.dyn;
    for (uint32_t i = 0; i < o->array.count; ++i) {
        solu_call_ex ex = solu_call(s, callback.dyn,
            (solu_val[]){o->array.data[i], (solu_val){SOLU_TI64, .i64 = (solu_i64)i}},
        2);
        if (!ex.is_ok) return ex;
        solu_valvec_push(&((solu_dobj *)out.dyn)->array, ex.ok);
    }

    solu_drelease(out);
    return solu_ok(out);
}

/*
 * obj.contains(self: obj, match: any) -> bool
 * Returns whether the array contains the specified value. This function
 * does not currently interact with metadata.
 */
static solu_call_ex obj_contains(solu_state *s) {
    solu_val array = solu_selfc(s);
    expect_dtype(SOLU_DOBJ, array);
    solu_val match = solu_get(s, 1);

    solu_dobj *o = array.dyn;
    for (uint32_t i = 0; i < o->array.count; ++i)
        if (solu_strict_eq(o->array.data[i], match))
            return solu_ok(SOLU_TRUE);
    return solu_ok(SOLU_FALSE);
}


solu_val solu_mod_obj(solu_state *s, bool meta) {
    solu_val (*fun)(solu_state *, solu_cfunction, uint32_t, solu_val *, uint32_t) = meta ?
        solu_wrapmfun : solu_wrapcfun;
    solu_val obj = solu_dnew(s, SOLU_DOBJ);

    // General
    solu_dobj_strset(obj.dyn, "usemeta", fun(s, obj_usemeta, 2, NULL, 0));
    solu_dobj_strset(obj.dyn, "meta", fun(s, obj_meta, 1, NULL, 0));
    solu_dobj_strset(obj.dyn, "stringify", fun(s, obj_stringify, 3, NULL, 0));
    solu_dobj_strset(obj.dyn, "template", fun(s, obj_template, 1, NULL, 0));

    // Map
    solu_dobj_strset(obj.dyn, "set", fun(s, obj_set, 3, NULL, 0));
    solu_dobj_strset(obj.dyn, "setr", fun(s, obj_setr, 3, NULL, 0));
    solu_dobj_strset(obj.dyn, "get", fun(s, obj_get, 2, NULL, 0));
    solu_dobj_strset(obj.dyn, "getr", fun(s, obj_getr, 2, NULL, 0));
    solu_dobj_strset(obj.dyn, "members", fun(s, obj_members, 1, NULL, 0));
    solu_dobj_strset(obj.dyn, "pairs", fun(s, obj_pairs, 2, NULL, 0));
    solu_dobj_strset(obj.dyn, "find", fun(s, obj_find, 2, NULL, 0));
    solu_dobj_strset(obj.dyn, "match", fun(s, obj_match, 2, NULL, 0));
    solu_dobj_strset(obj.dyn, "has", fun(s, obj_has, 2, NULL, 0));

    // Array
    solu_dobj_strset(obj.dyn, "push", fun(s, obj_push, 2, NULL, 0));
    solu_dobj_strset(obj.dyn, "push_back", fun(s, obj_push_back, 2, NULL, 0));
    solu_dobj_strset(obj.dyn, "pop", fun(s, obj_pop, 1, NULL, 0));
    solu_dobj_strset(obj.dyn, "pop_back", fun(s, obj_pop_back, 1, NULL, 0));
    solu_dobj_strset(obj.dyn, "len", fun(s, obj_len, 1, NULL, 0));
    solu_dobj_strset(obj.dyn, "remove", fun(s, obj_remove, 1, NULL, 0));
    solu_dobj_strset(obj.dyn, "foreach", fun(s, obj_foreach, 2, NULL, 0));
    solu_dobj_strset(obj.dyn, "reverse", fun(s, obj_reverse, 1, NULL, 0));
    solu_dobj_strset(obj.dyn, "range", fun(s, obj_range, 3, NULL, 0));
    solu_dobj_strset(obj.dyn, "where", fun(s, obj_where, 2, NULL, 0));
    solu_dobj_strset(obj.dyn, "all", fun(s, obj_all, 2, NULL, 0));
    solu_dobj_strset(obj.dyn, "map", fun(s, obj_map, 2, NULL, 0));
    solu_dobj_strset(obj.dyn, "contains", fun(s, obj_contains, 2, NULL, 0));

    // Others that extend to obj
    if (meta) {
        solu_dobj_strset(obj.dyn, "then", fun(s, builtin_then, 2, NULL, 0));
        solu_dobj_strset(obj.dyn, "type", fun(s, builtin_type, 1, NULL, 0));
        solu_dobj_strset(obj.dyn, "str", fun(s, builtin_str, 1, NULL, 0));
        solu_dobj_strset(obj.dyn, "unwrap", fun(s, builtin_unwrap, 1, NULL, 0));
        solu_dobj_strset(obj.dyn, "or_else", fun(s, builtin_or_else, 1, NULL, 0));

        solu_dobj_strset(obj.dyn, "join", fun(s, string_join, 2, NULL, 0));
    } else solu_dobj_strset(s->global.dyn, "obj", obj);
    return obj;
}
