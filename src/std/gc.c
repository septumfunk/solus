#include "std.h"

static sol_call_ex gc_collect(sol_state *s) {
    sol_dcollect(s);
    return sol_call_ex_ok(SOL_NIL);
}

void sol_mod_gc(sol_state *s) {
    sol_val gc = sol_dnew(s, SOL_DOBJ);
    sol_dobj_set(gc.dyn, sf_lit("collect"), sol_wrapcfun(s, gc_collect, 0, 0));
    sol_dobj_set(s->global.dyn, sf_lit("gc"), gc);
}
