#include "solus/api.h"
#include "std.h"

solu_error solu_register_types(solu_state *s) {
    solu_type_ex ex = solu_type_prim(s, "str", (solu_tinfo){SOLU_NIL, SOLU_TI_DEF, .def={0}});
    if (!ex.is_ok) return ex.err;
    ex = solu_type_prim(s, "Global", (solu_tinfo){solu_dnstr(s, "Global"), SOLU_TI_DEF, .def=solu_def_new(), .complex=true});
    if (!ex.is_ok) return ex.err;
    s->typeenv.global = ex.ok.dyn;

    ex = solu_type_def(s, "any", (solu_complex_member[]){
        {.name = "then",     .type = "((any) -> any) -> any"},
        {.name = "type",     .type = "() -> str"},
        {.name = "or_else", .type = "(any) -> any"},
        {.name = "unwrap", .type = "() -> any"},
    }, 4, false);
    if (!ex.is_ok) return ex.err;
    s->typeenv.ptypes[SOLU_CANY] = ex.ok;

    ex = solu_type_def(s, "nil", (solu_complex_member[]){
        {.name = "then",     .type = "((nil) -> any) -> any"},
        {.name = "type",     .type = "() -> str"},
        {.name = "or_else", .type = "(nil) -> nil"},
        {.name = "unwrap", .type = "() -> nil"},
    }, 4, false);
    if (!ex.is_ok) return ex.err;
    s->typeenv.ptypes[SOLU_CNIL] = ex.ok;
    ex = solu_type_def(s, "f64", (solu_complex_member[]){
        {.name = "then",     .type = "((f64?!) -> any) -> any"},
        {.name = "type",     .type = "() -> str"},
        {.name = "or_else", .type = "(f64) -> f64"},
        {.name = "unwrap", .type = "() -> f64"},
    }, 4, false);
    if (!ex.is_ok) return ex.err;
    s->typeenv.ptypes[SOLU_CF64] = ex.ok;
    ex = solu_type_def(s, "i64", (solu_complex_member[]){
        {.name = "then",     .type = "((i64?!) -> any) -> any"},
        {.name = "type",     .type = "() -> str"},
        {.name = "or_else", .type = "(i64) -> i64"},
        {.name = "unwrap", .type = "() -> i64"},
    }, 4, false);
    if (!ex.is_ok) return ex.err;
    s->typeenv.ptypes[SOLU_CI64] = ex.ok;
    ex = solu_type_def(s, "bool", (solu_complex_member[]){
        {.name = "then",     .type = "((bool?!) -> any) -> any"},
        {.name = "type",     .type = "() -> str"},
        {.name = "or_else", .type = "(bool) -> bool"},
        {.name = "unwrap", .type = "() -> bool"},
    }, 4, false);
    if (!ex.is_ok) return ex.err;
    s->typeenv.ptypes[SOLU_CBOOL] = ex.ok;
    ex = solu_type_prim(s, "err", (solu_tinfo){solu_dnstr(s, "err"), .cast = SOLU_OP_UNKNOWN});
    if (!ex.is_ok) return ex.err;
    s->typeenv.ptypes[SOLU_CERR] = ex.ok;

    ex = solu_type_def(s, "str", (solu_complex_member[]){
        {.name = "len",      .type = "() -> i64"},
        {.name = "sub",      .type = "(i64, i64) -> str"},
        {.name = "reverse",  .type = "() -> str"},
        {.name = "repeat",   .type = "(i64) -> str"},
        {.name = "split",    .type = "(str) -> str"},
        {.name = "ord",      .type = "() -> i64"},
        {.name = "upper",    .type = "() -> str"},
        {.name = "lower",    .type = "() -> str"},


        {.name = "then",     .type = "((str?!) -> any) -> any"},
        {.name = "type",     .type = "() -> str"},
        {.name = "unwrap",     .type = "() -> str"},
        {.name = "or_else", .type = "(str) -> str"},
    }, 12, false);
    if (!ex.is_ok) return ex.err;
    s->typeenv.ptypes[SOLU_CSTR] = ex.ok;

    ex = solu_type_def(s, "obj", (solu_complex_member[]){
        {.name = "usemeta",      .type = "(obj) -> obj"},
        {.name = "meta",      .type = "() -> obj"},
        {.name = "stringify",  .type = "() -> str"},

        {.name = "set",   .type = "(str, any) -> nil"},
        {.name = "setr",    .type = "(str, any) -> nil"},
        {.name = "get",      .type = "(str) -> any"},
        {.name = "getr",    .type = "(str) -> any"},
        {.name = "members",    .type = "() -> i64"},
        {.name = "pairs",    .type = "((str, any)) -> nil"},
        {.name = "find",    .type = "((str, any) -> bool) -> any"},
        {.name = "match",    .type = "((str, any) -> bool) -> obj"},
        {.name = "has",    .type = "(str) -> bool"},

        {.name = "push",   .type = "(any) -> nil"},
        {.name = "push_back",   .type = "(any) -> nil"},
        {.name = "pop",   .type = "() -> any"},
        {.name = "pop_back",   .type = "() -> any"},
        {.name = "members",    .type = "() -> i64"},
        {.name = "remove",   .type = "(i64) -> any"},
        {.name = "foreach",    .type = "((any)) -> nil"},
        {.name = "reverse",    .type = "() -> obj"},
        {.name = "range",    .type = "(i64, i64) -> obj"},
        {.name = "where",    .type = "((any) -> bool) -> any"},
        {.name = "all",    .type = "((any) -> bool) -> obj"},
        {.name = "map",    .type = "((any) -> any) -> obj"},
        {.name = "contains",    .type = "(any) -> bool"},

        {.name = "then",     .type = "((obj?!) -> any) -> any"},
        {.name = "type",     .type = "() -> str"},
        {.name = "unwrap",     .type = "() -> obj"},
        {.name = "or_else", .type = "(obj) -> obj"},
    }, 29, false);
    if (!ex.is_ok) return ex.err;
    s->typeenv.ptypes[SOLU_COBJ] = ex.ok;

    ex = solu_type_global(s, "import", "(str) -> any!", false, false);
    if (!ex.is_ok) return ex.err;
    ex = solu_type_global(s, "require", "(str) -> any", false, false);
    if (!ex.is_ok) return ex.err;
    ex = solu_type_global(s, "eval", "(str) -> any!", false, false);
    if (!ex.is_ok) return ex.err;
    ex = solu_type_global(s, "panic", "(str) -> nil", false, false);
    if (!ex.is_ok) return ex.err;
    ex = solu_type_global(s, "catch", "(any) -> any!", false, false);
    if (!ex.is_ok) return ex.err;
    ex = solu_type_global(s, "attempt", "(() -> any!, (any!) -> any) -> any", false, false);
    if (!ex.is_ok) return ex.err;
    ex = solu_type_global(s, "unwrap", "(any?!) -> any", false, false);
    if (!ex.is_ok) return ex.err;
    ex = solu_type_global(s, "assert", "(bool) -> nil", false, false);
    if (!ex.is_ok) return ex.err;
    ex = solu_type_global(s, "type", "(any) -> str", false, false);
    if (!ex.is_ok) return ex.err;
    ex = solu_type_global(s, "err", "(str) -> err", false, false);
    if (!ex.is_ok) return ex.err;

    // Modules
    ex = solu_type_def(s, "IO", (solu_complex_member[]){
        {.name = "print", .type = "(any) -> nil"},
        {.name = "println", .type = "(any) -> nil"},
        {.name = "time", .type = "() -> f64"},
        {.name = "fread", .type = "(str) -> str!"},
        {.name = "fwrite", .type = "(str, str) -> nil!"},
        {.name = "compile", .type = "(str, str) -> nil!"},
        {.name = "input", .type = "(str) -> str!"},
    }, 7, true);
    ex = solu_type_global(s, "io", "IO", false, false);
    if (!ex.is_ok) return ex.err;

    return SOLU_ERRC_NONE;
}
