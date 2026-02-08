#include "std.h"

static solu_call_ex gc_collect(solu_state *s) {
    solu_dcollect(s);
    return solu_ok(SOLU_NIL);
}

void solu_mod_gc(solu_state *s) {
    solu_val gc = solu_dnew(s, SOLU_DOBJ);
    solu_dobj_strset(gc.dyn, "collect", solu_wrapcfun(s, gc_collect, 0, 0));
    solu_dobj_strset(s->global.dyn, "gc", gc);
}
