#include "solus/api.h"
#include "sf/containers/buffer.h"
#include "solus/bytecode.h"
#include "solus/compat.h"
#include "sf/str.h"
#include "solus/syntax.h"
#include "solus/val.h"
#include "std/std.h"
#include <sf/fs.h>
#include <stdarg.h>

#define CALL_STACK_MAX 1024

static bool solu_usestd(solu_state *s) {
    solu_val solus = solu_dnew(s, SOLU_DOBJ);
    solu_dobj_strset(solus.dyn, "version", solu_dnstr(s, SOLU_VERSION));
    solu_dobj_strset(solus.dyn, "git", solu_dnstr(s, SOLU_GIT));
    solu_dobj_strset(s->global.dyn, "solus", solus);

    solu_mod_builtin(s);
    solu_error ex = solu_register_types(s);
    if (ex != SOLU_ERRC_NONE) {
        fprintf(stderr, TUI_ERR "init err: %s\n" TUI_CLR, (char *)solu_err_string(ex));
        return false;
    }
    s->meta.string = solu_mod_string(s);
    if (solu_isdtype(s->meta.string, SOLU_DERR)) {
        fprintf(stderr, TUI_ERR "init err: %s\n" TUI_CLR, (char *)s->meta.string.dyn);
        return false;
    }
    s->meta.obj = solu_mod_obj(s);
    if (solu_isdtype(s->meta.obj, SOLU_DERR)) {
        fprintf(stderr, TUI_ERR "init err: %s\n" TUI_CLR, (char *)s->meta.obj.dyn);
        return false;
    }
    solu_dhold(s->meta.string);
    solu_dhold(s->meta.obj);

    solu_mod_io(s);
    solu_mod_math(s);
    solu_mod_gc(s);

    return true;
}

solu_state *solu_state_new(void) {
    solu_dyn p = calloc(1, sizeof(solu_dalloc) + sizeof(solu_dobj));
    *(solu_dalloc *)p = (solu_dalloc){NULL, sizeof(solu_dobj), SOLU_DOBJ, SOLU_DYN_WHITE, true, SOLU_NIL, {SOLU_NIL}};
    p = (char *)p + sizeof(solu_dalloc);
    *(solu_dobj *)p = solu_dobj_new();

    solu_state *s = malloc(sizeof(solu_state));
    *s = (solu_state){
        .stack = solu_valvec_new(),
        .strcache = solu_strcache_new(),
        .frames = solu_frames_new(),
        .global = {SOLU_TDYN, .dyn = p},
        .lb = 1<<20, .cb = 0, .nb = 0,
        .call_stack = 0,
        .ccall = NULL,

        .import_paths = solu_valvec_new(),
        .trace = solu_trace_new(),
        .ctrace = solu_ctrace_new(),

        .collect = false,
        .alloc = NULL,

        .typeenv = {
            NULL,
            solu_valmap_new(),
            {SOLU_NIL}
        }
    };

    if (!solu_usestd(s)) {
        free(s);
        return NULL;
    }
    return s;
}

void solu_state_free(solu_state *state) {
    solu_valvec_free(&state->stack);
    solu_valvec_free(&state->import_paths);
    solu_trace_free(&state->trace);
    solu_ctrace_free(&state->ctrace);
    solu_frames_free(&state->frames);
    solu_strcache_free(&state->strcache);
    solu_dclean(state->global);
    free(state);
}

// Functions/Files

solu_compile_ex solu_csrc(solu_state *state, char *src) {
    solu_compile_ex ex = solu_cproto(&state->ctrace, sf_lit("Source Code"), src, 0, NULL, 1, (solu_upvalue[]){
        (solu_upvalue){
                sf_lit("global"), SOLU_UP_VAL, .value = state->global, .mut = false,
                .type = (solu_val){SOLU_TDYN, .dyn=state->typeenv.global},
            }
    }, &state->typeenv);
    ex.ok.line_c = 1;
    for (char *c = src; *c != '\0'; ++c)
        if (*c == '\n') ++ex.ok.line_c;
    return ex;
}

solu_compile_ex solu_cfile(solu_state *state, char *path) {
    if (state->ctrace.count)
        solu_ctrace_free(&state->ctrace);
    sf_fsb_ex fsb = sf_file_buffer(sf_ref(path));
    if (!fsb.is_ok) {
        solu_ctrace_push(&state->ctrace, (solu_compiledata){
            SOLU_ERRC_FILE_NOT_FOUND, 0, 0,
            sf_str_fmt("Cannot open file %s", path).c_str
        });
        return solu_compile_ex_err(&state->ctrace);
    }
    fsb.ok.flags = SF_BUFFER_GROW;
    sf_buffer_seek(&fsb.ok, SF_BUFFER_END, 0);
    sf_buffer_autoins(&fsb.ok, ""); // [\0]
    sf_buffer_seek(&fsb.ok, SF_BUFFER_START, 0);

    char *realpath = solu_realpath(path);
    if (!realpath) {
        solu_ctrace_push(&state->ctrace, (solu_compiledata){
            SOLU_ERRC_FILE_NOT_FOUND, 0, 0,
            sf_str_fmt("Cannot locate file %s", path).c_str
        });
        return solu_compile_ex_err(&state->ctrace);
    }

    solu_compile_ex ex = solu_cproto(&state->ctrace, sf_ref(realpath), (char *)fsb.ok.ptr, 0, NULL, 1, (solu_upvalue[]){
        (solu_upvalue){
            sf_lit("global"), SOLU_UP_VAL, .value = state->global, .mut = false,
            .type = (solu_val){SOLU_TDYN, .dyn=state->typeenv.global}
        }
    }, &state->typeenv);
    free(realpath);
    if (!ex.is_ok) {
        sf_buffer_clear(&fsb.ok);
        return ex;
    }
    ex.ok.line_c = 1;
    for (char *c = (char *)fsb.ok.ptr; *c != '\0'; ++c)
        if (*c == '\n') ++ex.ok.line_c;
    sf_buffer_clear(&fsb.ok);
    return ex;
}

static char *solu_check_file_candidate(const char *base, bool has_ext) {
    sf_str rp0 = sf_str_cdup(base);
    if (sf_file_exists(rp0)) return rp0.c_str;

    if (!has_ext) {
        sf_str rp1 = sf_str_fmt("%s.solu", rp0.c_str);
        if (sf_file_exists(rp1)) {
            sf_str_free(rp0);
            return rp1.c_str;
        }
        sf_str_free(rp1);
        sf_str rp2 = sf_str_fmt("%s.solc", rp0.c_str);
        if (sf_file_exists(rp2)) {
            sf_str_free(rp0);
            return rp2.c_str;
        }
        sf_str_free(rp2);
        sf_str rp3 = sf_str_fmt("%s.solus", rp0.c_str);
        if (sf_file_exists(rp3)) {
            sf_str_free(rp0);
            return rp3.c_str;
        }
        sf_str_free(rp3);
    }
    sf_str_free(rp0);
    return NULL;
}

char *solu_findfile(solu_state *s, char *name) {
    if (!s || !name || !*name) return NULL;
    bool is_static =
        strchr(name, ':') != NULL ||
        name[0] == '/';

    size_t len = strlen(name);
    bool has_ext =
        (len >= 5 && memcmp(name + len - 5, ".solu",  5) == 0) ||
        (len >= 5 && memcmp(name + len - 5, ".solc",  5) == 0) ||
        (len >= 6 && memcmp(name + len - 6, ".solus", 6) == 0);
    if (is_static)
        return solu_check_file_candidate(name, has_ext);

    for (uint32_t i = 0; i < s->import_paths.count; ++i) {
        char *path = s->import_paths.data[i].dyn;
        if (!path) continue;

        sf_str base;
        size_t plen = strlen(path);
        if (plen && (path[plen - 1] == '/' || path[plen - 1] == '\\'))
            base = sf_str_fmt("%s%s", path, name);
        else
            base = sf_str_fmt("%s/%s", path, name);

        char *found = solu_check_file_candidate(base.c_str, has_ext);
        sf_str_free(base);
        if (found) return found;
    }

    return NULL;
}

void solu_addpath(solu_state *s, char *realpath) {
    solu_val str = solu_dnstr(s, realpath);
    solu_dhold(str);
    solu_valvec_push(&s->import_paths, str);
}

solu_val solu_wrapcfun(solu_state *state, solu_cfunction fptr, uint32_t arg_c, solu_val *captures, uint32_t cap_c) {
    solu_val fun = solu_dnew(state, SOLU_DFUN);
    *(solu_fproto *)fun.dyn = solu_fproto_c(fptr, arg_c, captures, cap_c);
    return fun;
}

solu_val solu_wrapmfun(solu_state *state, solu_cfunction fptr, uint32_t arg_c, solu_val *captures, uint32_t cap_c) {
    solu_val fun = solu_wrapcfun(state, fptr, arg_c, captures, cap_c);
    solu_fproto *f = fun.dyn;
    f->self = true;
    solu_upvalue *u = f->upvals;
    f->upvals = malloc(sizeof(solu_upvalue) * ++f->up_c);
    if (u) {
        memcpy(f->upvals + 1, u, sizeof(solu_upvalue) * (f->up_c - 1));
        free(u);
    }
    f->upvals[0] = (solu_upvalue){sf_lit("self"), SOLU_UP_VAL, .value = SOLU_NIL};
    return fun;
}

sf_buffer solu_fproto_serialize(solu_fproto *proto) {
    sf_buffer buf = sf_buffer_grow();

    sf_buffer_insert(&buf, "[SOLC]", 6);
    sf_buffer_insert(&buf, SOLU_VERSION, sizeof(SOLU_VERSION) - 1);
    sf_buffer_autoins(&buf, &(uint64_t){htonll(proto->file_name.len)});
    sf_buffer_insert(&buf, proto->file_name.c_str, proto->file_name.len);
    sf_buffer_autoins(&buf, &(uint16_t){htons(proto->code_c)});
    sf_buffer_autoins(&buf, &(uint16_t){htons(proto->line_c)});
    sf_buffer_autoins(&buf, &(uint8_t){proto->self});
    sf_buffer_autoins(&buf, &(uint8_t){proto->variadic});

    sf_buffer_autoins(&buf, &(uint32_t){htonl(proto->constants.count)});
    for (solu_val *k = proto->constants.data; k < proto->constants.data + proto->constants.count; ++k) {
        if (solu_isdtype(*k, SOLU_DFUN)) {
            sf_buffer_autoins(&buf, &(uint32_t){htonl(SOLU_TCOUNT)}); // fun
            sf_buffer bfun = solu_fproto_serialize(k->dyn);
            sf_buffer_insert(&buf, bfun.ptr, bfun.size);
            sf_buffer_clear(&bfun);
            continue;
        }
        sf_buffer_autoins(&buf, &(uint32_t){htonl((uint32_t)k->tt)});
        switch (k->tt) {
            case SOLU_TI64:
            case SOLU_TF64:
                sf_buffer_autoins(&buf, &(uint64_t){htonll((uint64_t)k->i64)});
                break;
            case SOLU_TBOOL:
                sf_buffer_autoins(&buf, &k->boolean);
                break;
            case SOLU_TDYN: { // str
                size_t s = solu_dheader(*k)->size;
                if (s > 0) s = s - 1;
                sf_buffer_autoins(&buf, &(uint64_t){htonll(s)});
                sf_buffer_insert(&buf, k->dyn, s);
                break;
            }
            default: break; // nil
        }
    }

    sf_buffer_autoins(&buf, &(uint32_t){htonl(proto->reg_c)});
    sf_buffer_autoins(&buf, &(uint32_t){htonl(proto->arg_c)});
    uint32_t upc = proto->up_c;
    if (upc > 0 && sf_str_eq(proto->upvals[0].name, sf_lit("global")))
        --upc;
    sf_buffer_autoins(&buf, &(uint32_t){htonl(upc)});
    for (solu_upvalue *u = proto->upvals + (proto->up_c - upc); u < proto->upvals + proto->up_c; ++u) {
        sf_buffer_autoins(&buf, &(uint64_t){htonll(u->name.len)});
        sf_buffer_insert(&buf, u->name.c_str, u->name.len);
        sf_buffer_autoins(&buf, &(uint32_t){htonl(u->frame)});
        sf_buffer_autoins(&buf, &(uint32_t){htonl(u->ref)});
        sf_buffer_autoins(&buf, &u->mut);
    }

    for (uint16_t i = 0; i < proto->code_c; ++i)
        sf_buffer_autoins(&buf, &(uint32_t){htonl(proto->code[i])});

    return buf;
}

void solu_savefun(solu_fproto *proto, char *path) {
    if (proto->tt != SOLU_FPROTO_BC)
        return;
    sf_buffer buf = solu_fproto_serialize(proto);
    FILE *f = fopen(path, "wb");
    if (!f) {
        sf_buffer_clear(&buf);
        return;
    }
    fwrite(buf.ptr, 1, buf.size, f);
    sf_buffer_clear(&buf);
    fclose(f);
}

solu_load_ex _solu_loadfun(solu_state *s, sf_buffer *buf) {
    char *name = NULL;
    solu_valvec kvec = solu_valvec_new();
    solu_fproto proto = solu_fproto_new();
    solu_upvalue *upvals = NULL;

    if (memcmp(buf->head, "[SOLC]", 6) != 0) return solu_load_ex_err(SOLU_ERRV_CORRUPT);
    buf->head += 6;
    if (memcmp(buf->head, SOLU_VERSION, sizeof(SOLU_VERSION) - 1) != 0) return solu_load_ex_err(SOLU_ERRV_OLD_BC);
    buf->head += sizeof(SOLU_VERSION) - 1;

    uint64_t n_len;
    sf_buffer_ex ex = sf_buffer_autoread(buf, &n_len);
    if (!ex.is_ok) return solu_load_ex_err(SOLU_ERRV_CORRUPT);
    n_len = ntohll(n_len);
    if (n_len > 1024) return solu_load_ex_err(SOLU_ERRV_CORRUPT);

    if (n_len > 0) {
        name = malloc((size_t)n_len + 1);
        ex = sf_buffer_read(buf, name, (size_t)n_len);
        if (!ex.is_ok) goto corrupt;
        name[n_len] = 0;
    }
    proto.file_name = name ? sf_own(name) : SF_STR_EMPTY;

    ex = sf_buffer_autoread(buf, &proto.code_c);
    if (!ex.is_ok) goto corrupt;
    proto.code_c = ntohs(proto.code_c);
    ex = sf_buffer_autoread(buf, &proto.line_c);
    if (!ex.is_ok) goto corrupt;
    proto.line_c = ntohs(proto.line_c);
    ex = sf_buffer_autoread(buf, &proto.self);
    if (!ex.is_ok) goto corrupt;
    ex = sf_buffer_autoread(buf, &proto.variadic);
    if (!ex.is_ok) goto corrupt;

    uint32_t kcount;
    ex = sf_buffer_autoread(buf, &kcount);
    if (!ex.is_ok) goto corrupt;
    kcount = ntohl(kcount);
    for (uint32_t k = 0; k < kcount; ++k) {
        uint32_t tt;
        ex = sf_buffer_autoread(buf, &tt);
        if (!ex.is_ok) goto corrupt;
        tt = ntohl(tt);

        solu_val val = {(solu_ptype)tt, .dyn = NULL};
        switch (tt) {
            case SOLU_TDYN: { // str
                uint64_t slen;
                ex = sf_buffer_autoread(buf, &slen);
                if (!ex.is_ok) goto corrupt;
                slen = ntohll(slen);

                char *temp = slen == 0 ? "" : malloc((size_t)slen + 1);
                if (!temp) goto corrupt;
                ex = sf_buffer_read(buf, temp, (size_t)slen);
                if (!ex.is_ok) { if (slen) free(temp); goto corrupt; }
                if (slen) temp[slen] = 0;

                solu_dyn p = calloc(1, sizeof(solu_dalloc) + (size_t)slen + 1);
                solu_dalloc *dh = p;
                *dh = (solu_dalloc){
                    .next = NULL,
                    .size = (size_t)slen + 1,
                    .tt = SOLU_DSTR,
                    .mark = SOLU_DYN_WHITE,
                    .held = true,
                };
                p = (char *)p + sizeof(solu_dalloc);
                memcpy(p, temp, (size_t)slen + 1);
                val.dyn = p;
                if (slen) free(temp);
                break;
            }
            case SOLU_TI64:
            case SOLU_TF64:
                ex = sf_buffer_autoread(buf, &val.i64);
                if (!ex.is_ok) goto corrupt;
                val.i64 = (int64_t)ntohll((uint64_t)val.i64);
                break;
            case SOLU_TBOOL:
                ex = sf_buffer_autoread(buf, &val.boolean);
                if (!ex.is_ok) goto corrupt;
                break;
            case SOLU_TCOUNT: { // fun
                solu_load_ex lex = _solu_loadfun(s, buf);
                if (!lex.is_ok) {
                    if (name) free(name);
                    for (solu_val *k = kvec.data; k < kvec.data + kvec.count; ++k)
                        solu_dclean(*k);
                    solu_valvec_free(&kvec);
                    return lex;
                }
                solu_dyn p = calloc(1, sizeof(solu_dalloc) + sizeof(solu_fproto));
                solu_dalloc *dh = p;
                *dh = (solu_dalloc){
                    .next = NULL,
                    .size = sizeof(solu_fproto),
                    .tt = SOLU_DFUN,
                    .mark = SOLU_DYN_WHITE,
                    .held = true,
                };
                p = (char *)p + sizeof(solu_dalloc);
                val = (solu_val){SOLU_TDYN, .dyn = p};
                *(solu_fproto *)val.dyn = lex.ok;
                break;
            }
            default: break; // nil
        }
        solu_valvec_push(&kvec, val);
    }

    ex = sf_buffer_autoread(buf, &proto.reg_c);
    if (!ex.is_ok) goto corrupt;
    proto.reg_c = ntohl(proto.reg_c);
    ex = sf_buffer_autoread(buf, &proto.arg_c);
    if (!ex.is_ok) goto corrupt;
    proto.arg_c = ntohl(proto.arg_c);
    ex = sf_buffer_autoread(buf, &proto.up_c);
    if (!ex.is_ok) goto corrupt;
    proto.up_c = ntohl(proto.up_c) + 1;
    upvals = calloc(proto.up_c, sizeof(solu_upvalue));
    upvals[0] = (solu_upvalue) {
        sf_lit("global"), SOLU_UP_VAL, .value = s->global, .mut = false,
        .type = (solu_val){SOLU_TDYN, .dyn=s->typeenv.global},
    };
    for (uint32_t u = 1; u < proto.up_c; ++u) {
        uint64_t slen;
        ex = sf_buffer_autoread(buf, &slen);
        if (!ex.is_ok) goto corrupt;
        slen = ntohll(slen);
        if (slen == 0) goto corrupt;

        char *temp = malloc((size_t)slen + 1);
        if (!temp) goto corrupt;
        ex = sf_buffer_read(buf, temp, (size_t)slen);
        if (!ex.is_ok) { free(temp); goto corrupt; }
        temp[slen] = 0;

        uint32_t frame, ref;
        bool mut;
        ex = sf_buffer_autoread(buf, &frame);
        if (!ex.is_ok) { free(temp); goto corrupt; }
        frame = ntohl(frame);
        ex = sf_buffer_autoread(buf, &ref);
        if (!ex.is_ok) { free(temp); goto corrupt; }
        ref = ntohl(ref);
        ex = sf_buffer_autoread(buf, &mut);
        if (!ex.is_ok) { free(temp); goto corrupt; }
        upvals[u] = (solu_upvalue){sf_own(temp), SOLU_UP_REF, .ref = ref, frame, };
    }

    proto.code = malloc(sizeof(uint32_t) * proto.code_c);
    proto.dbg = NULL; // No debug info for compiled

    for (uint16_t i = 0; i < proto.code_c; ++i) {
        ex = sf_buffer_autoread(buf, proto.code + i);
        proto.code[i] = ntohl(proto.code[i]);
        if (!ex.is_ok) goto corrupt;
    }

    proto.constants = kvec;
    proto.upvals = upvals;
    return solu_load_ex_ok(proto);
corrupt:
    if (name) free(name);
    for (solu_val *k = kvec.data; k < kvec.data + kvec.count; ++k)
        solu_dclean(*k);
    solu_valvec_free(&kvec);
    if (upvals) {
        for (uint32_t u = 0; u < proto.up_c; ++u)
            sf_str_free(upvals[u].name);
        free(upvals);
    }
    if (proto.code) {
        free(proto.code);
        free(proto.dbg);
    }
    return solu_load_ex_err(SOLU_ERRV_CORRUPT);
}

solu_load_ex solu_loadfun(solu_state *state, char *path) {
    sf_fsb_ex fsb = sf_file_buffer(sf_ref(path));
    if (!fsb.is_ok) return solu_load_ex_err(SOLU_ERRC_FILE_NOT_FOUND);
    solu_load_ex ex = _solu_loadfun(state, &fsb.ok);
    sf_buffer_clear(&fsb.ok);
    return ex;
}

static inline void report_stack(solu_state *state, solu_fproto *proto, char *fn, uint32_t pc) {
    uint16_t line = 0, column = 0;
    if (proto->dbg) {
        // Location of last call site or error
        line = SOLU_DBG_LINE(proto->dbg[pc]);
        column = SOLU_DBG_COL(proto->dbg[pc]);
    }
    solu_trace_push(&state->trace, (solu_tracedata){
        fn, line, column, proto
    });
}

solu_call_ex solu_call(solu_state *state, solu_fproto *proto, const solu_val *args, uint32_t arg_c) {
    if (state->call_stack > CALL_STACK_MAX)
        return solu_panic(state, "Stack Overflow");

    // Reset from panic
    if (!state->ccall) {
        if (state->trace.data)
            solu_trace_free(&state->trace);
        if (state->panic) {
            free(state->panic);
            state->panic = NULL;
        }
    }

    bool fd = false;
    char *fn;
    if (proto->file_name.len) {
        fn = strdup(proto->file_name.c_str);
        char *rd = solu_realdir(proto->file_name.c_str);
        if (rd) {
            solu_val dn = solu_dnstr(state, rd);
            free(rd);
            solu_dhold(dn);
            solu_valvec_insert(&state->import_paths, 0, dn);
            fd = true;
        } else fn = strdup("Unknown");
    } else fn = strdup("C Function");

    // Backup
    solu_fproto *ocall = state->ccall;
    uint32_t opc = state->pc;

    ++state->call_stack;
    state->ccall = proto;
    if (proto->tt == SOLU_FPROTO_BC) {
        solu_call_ex ex = solu_call_bc(state, proto, args, arg_c);
        if (!ex.is_ok) {
            report_stack(state, proto, fn, state->pc - 1);
            solu_popframe(state);
        } else free(fn);

        // Restore
        state->ccall = ocall;
        state->pc = opc;
        --state->call_stack;

        if (fd) {
            solu_drelease(state->import_paths.data[0]);
            solu_valvec_delete(&state->import_paths, 0);
        }
        return ex;
    }

    solu_call_ex ex = solu_call_cfun(state, proto, args, arg_c);
    if (!ex.is_ok)
        report_stack(state, proto, fn, state->pc - 1);
    else free(fn);

    // Restore
    state->ccall = ocall;
    state->pc = opc;
    --state->call_stack;

    if (fd) {
        solu_drelease(state->import_paths.data[0]);
        solu_valvec_delete(&state->import_paths, 0);
    }
    return ex;
}

// Dynamic

void solu_dpush(solu_state *s, solu_dalloc *ac) {
    if (!s->alloc) {
        s->alloc = ac;
        s->alloc_tail = ac;
    } else {
        s->alloc_tail->next = ac;
        s->alloc_tail = ac;
    }
    s->cb += ac->size;
    if (s->cb > s->nb)
        s->collect = true;
}

solu_val solu_dnew(solu_state *s, solu_dtype tt) {
    size_t size = 0;
    switch (tt) {
        case SOLU_DSTR: size = 0; break;
        case SOLU_DERR: size = 0; break;
        case SOLU_DOBJ: size = sizeof(solu_dobj); break;
        case SOLU_DFUN: size = sizeof(solu_fproto); break;
        case SOLU_DREF: size = sizeof(solu_val); break;

        case SOLU_DUSR:
        case SOLU_DCOUNT: return SOLU_NIL;
    }

    solu_dyn p = calloc(1, sizeof(solu_dalloc) + size);
    solu_dalloc *dh = p;
    *dh = (solu_dalloc){
        .next = NULL,
        .size = size,
        .tt = tt,
        .mark = SOLU_DYN_WHITE,
        .metadata = {[SOLU_META_EXTEND] = s->meta.base}
    };
    p = (char *)p + sizeof(solu_dalloc);

    switch (tt) {
        case SOLU_DSTR:
        case SOLU_DERR: break;
        case SOLU_DOBJ:
            *(solu_dobj *)p = solu_dobj_new();
            if (s->meta.obj.tt != SOLU_TNIL)
                dh->metadata[SOLU_META_EXTEND] = s->meta.obj;
            break;
        case SOLU_DFUN: *(solu_fproto *)p = solu_fproto_new(); break;
        case SOLU_DREF: *(solu_val *)p = SOLU_NIL; break;

        case SOLU_DUSR:
        case SOLU_DCOUNT: {
            free(dh);
            return SOLU_NIL;
        }
    }

    solu_dpush(s, dh);
    return (solu_val){ .tt = SOLU_TDYN, .dyn = p };
}

solu_val solu_dnusr(solu_state *s, size_t size, const char *name, void *value,
    solu_usrdel del, solu_usrmark mark) {
    solu_dyn p = calloc(1, sizeof(solu_dalloc) + size + sizeof(solu_usrwrap));
    solu_dalloc *dh = p;
    *dh = (solu_dalloc){
        .next = NULL,
        .size = size,
        .tt = SOLU_DUSR,
        .mark = SOLU_DYN_WHITE,
    };
    p = (char *)p + sizeof(solu_dalloc);
    memcpy(p, value, size);
    *(solu_usrwrap *)((char *)p + size) = (solu_usrwrap){
        .name = sf_str_cdup(name),
        .del = del,
        .mark = mark,
    };

    solu_dpush(s, dh);
    return (solu_val){ .tt = SOLU_TDYN, .dyn = p };
}

solu_val solu_dnstr(solu_state *s, const char *str) {
    size_t size = strlen(str) + 1;
    if (size <= SOLU_STRCACHE_MAX - 1) {
        solu_strcache_ex sex = solu_strcache_get(&s->strcache, sf_ref(str));
        if (sex.is_ok)
            return (solu_val){ .tt = SOLU_TDYN, .dyn = sex.ok + 1 };
    }

    solu_dyn p = calloc(1, sizeof(solu_dalloc) + size);
    solu_dalloc *dh = p;
    *dh = (solu_dalloc){
        .next = NULL,
        .size = size,
        .tt = SOLU_DSTR,
        .mark = SOLU_DYN_WHITE,
        .metadata = {[SOLU_META_EXTEND] = s->meta.string}
    };
    p = (char *)p + sizeof(solu_dalloc);
    memcpy(p, str, size);
    solu_dpush(s, dh);
    if (size > 1 && size <= SOLU_STRCACHE_MAX - 1)
        solu_strcache_set(&s->strcache, sf_str_cdup(p), dh);
    return (solu_val){ .tt = SOLU_TDYN, .dyn = p };
}

solu_val solu_dnerr(solu_state *s, const char *str) {
    size_t size = strlen(str) + 1;
    solu_dyn p = calloc(1, sizeof(solu_dalloc) + size);
    solu_dalloc *dh = p;
    *dh = (solu_dalloc){
        .next = NULL,
        .size = size,
        .tt = SOLU_DERR,
        .mark = SOLU_DYN_WHITE,
        .metadata = {[SOLU_META_EXTEND] = s->meta.base}
    };
    p = (char *)p + sizeof(solu_dalloc);
    memcpy(p, str, size);
    solu_dpush(s, dh);
    return (solu_val){ .tt = SOLU_TDYN, .dyn = p };
}

char *solu_tostr(solu_state *s, solu_val val) {
    switch (val.tt) {
        case SOLU_TNIL: return strdup("nil");
        case SOLU_TF64: return sf_str_fmt("%g", val.f64).c_str;
        case SOLU_TI64: return sf_str_fmt("%lld", val.i64).c_str;
        case SOLU_TBOOL: return strdup(val.boolean ? "true" : "false");
        case SOLU_TDYN: {
            solu_val f = solu_dheader(val)->metadata[SOLU_META_STR];
            if (f.tt != SOLU_TNIL) {
                solu_call_ex ex = solu_call(s, f.dyn, &val, 1);
                if (ex.is_ok && solu_isdtype(ex.ok, SOLU_DSTR))
                    return strdup(ex.ok.dyn);
            }
            switch (solu_dheader(val)->tt) {
                case SOLU_DSTR:
                case SOLU_DERR:
                return strdup(val.dyn); break;
                case SOLU_DOBJ:
                case SOLU_DUSR:
                case SOLU_DFUN: return sf_str_fmt("%p", val.dyn).c_str;
                case SOLU_DREF: return solu_tostr(s, *(solu_val *)val.dyn);
                case SOLU_DCOUNT: return NULL;
            }
            return NULL;
        }
        default: return NULL;
    }
}

solu_val solu_dobj_get(solu_state *s, solu_dobj *obj, solu_val key) {
    if ((key.tt == SOLU_TI64 && key.i64 >= 0) || (key.tt == SOLU_TF64 && key.f64 >= 0)) {
        uint32_t nkey = (uint32_t)(key.tt == SOLU_TI64 ? key.i64 : (solu_i64)key.f64);
        if (obj->array.count == 0 || nkey > obj->array.count - 1)
            return SOLU_NIL;
        return solu_valvec_get(&obj->array, nkey);
    }
    char *nkey = solu_isdtype(key, SOLU_DSTR) ? key.dyn : solu_tostr(s, key);
    solu_val ex = solu_dobj_strget(obj, nkey);
    if (!solu_isdtype(key, SOLU_DSTR))
        free(nkey);
    return ex;
}

// Object

void solu_dobj_set(solu_state *s, solu_dobj *obj, solu_val key, solu_val val) {
    if (solu_isdtype(val, SOLU_DFUN)) {
        solu_fproto *fp = val.dyn;
        if (fp->self) {
            solu_upvalue op = fp->upvals[1];
            fp->upvals[1] = (solu_upvalue){ op.name, SOLU_UP_VAL, .value = (solu_val){SOLU_TDYN, .dyn = obj} };
        }
    }
    if ((key.tt == SOLU_TI64 && key.i64 >= 0) || (key.tt == SOLU_TF64 && key.f64 >= 0)) {
        uint32_t nkey = (uint32_t)(key.tt == SOLU_TI64 ? key.i64 : (solu_i64)key.f64);
        if (nkey == obj->array.count)
            solu_valvec_push(&obj->array, val);
        else if (nkey < obj->array.count)
            solu_valvec_set(&obj->array, nkey, val);
        else {
            while (obj->array.count < nkey)
                solu_valvec_push(&obj->array, SOLU_NIL);
            solu_valvec_push(&obj->array, val);
        }
        return;
    }
    char *nkey = solu_tostr(s, key);
    solu_valmap_set(&obj->map, sf_own(nkey), val);
}

static void obj_copy(void *dest, sf_str key, solu_val val) {
    solu_dobj_strset(dest, key.c_str, val);
}
solu_val solu_djoin(solu_state *s, solu_val obj1, solu_val obj2) {
    if (!solu_isdtype(obj1, SOLU_DOBJ))
        return SOLU_NIL;
    solu_val nobj = solu_dnew(s, SOLU_DOBJ);
    solu_dappend(nobj, obj1);
    if (solu_isdtype(obj2, SOLU_DOBJ))
        solu_dappend(nobj, obj2);
    return nobj;
}
void solu_dappend(solu_val obj1, solu_val obj2) {
    solu_dobj *obj1p = obj1.dyn, *obj2p = obj2.dyn;
    solu_valmap_foreach(&obj2p->map, obj_copy, &obj1p->map);
    if (obj2p->array.count)
        solu_valvec_append(&obj1p->array, obj2p->array.data, obj2p->array.count);
}

// GC

solu_val solu_dscopy(solu_state *state, solu_val val, bool kconst) {
    if (val.tt != SOLU_TDYN)
        return val; // This function only needs to copy dynamic constants

    solu_dalloc *ac = malloc(sizeof(solu_dalloc) + solu_dheader(val)->size);
    *ac = *(solu_dheader(val));
    ac->size = solu_dheader(val)->size;
    ac->mark = SOLU_DYN_WHITE;
    ac->held = false;
    ac->next = NULL;
    solu_val nv = (solu_val){SOLU_TDYN, .dyn=(char*)ac + sizeof(solu_dalloc)};

    switch (solu_dheader(nv)->tt) {
        case SOLU_DSTR:
            memcpy(nv.dyn, val.dyn, ac->size);
            ac->metadata[SOLU_META_EXTEND] = state->meta.string;
            break;
        case SOLU_DFUN: {
            solu_fproto *fp = val.dyn, *nfp = nv.dyn;
            memcpy(nfp, fp, sizeof(solu_fproto));
            nfp->file_name = sf_str_dup(fp->file_name);
            nfp->constants = solu_valvec_new();
            nfp->code = malloc(sizeof(solu_instruction) * fp->code_c);
            nfp->dbg = fp->dbg ? malloc(sizeof(solu_dbg) * fp->code_c) : NULL;
            nfp->self = fp->self;
            ac->metadata[SOLU_META_EXTEND] = state->meta.base;

            // Deref Upvals
            nfp->upvals = malloc(sizeof(solu_upvalue) * nfp->up_c);
            for (uint32_t i = 0; i < nfp->up_c; ++i) {
                solu_upvalue upv = fp->upvals[i];
                if (kconst) {
                    upv.name = sf_str_dup(upv.name);
                    nfp->upvals[i] = upv;
                } else {
                    solu_val nv;
                    if (upv.tt == SOLU_UP_REF) {
                        solu_val cv = solu_valvec_get(&state->stack, state->frames.data[state->frames.count - 1].bottom_o + upv.ref);
                        if (cv.tt != SOLU_TDYN) {
                            nv = solu_dnew(state, SOLU_DREF);
                            solu_rawset(state, upv.ref, nv, state->frames.count - 1);
                            *(solu_val *)nv.dyn = cv;
                        } else nv = cv;
                    } else if (upv.tt == SOLU_UP_UPV) {
                        nv = state->ccall->upvals[upv.ref].value;
                    } else nv = upv.value;

                    nfp->upvals[i] = (solu_upvalue){
                        sf_str_dup(upv.name),
                        SOLU_UP_VAL,
                        .value = nv,
                        .mut = upv.mut,
                    };
                }
            }

            memcpy(nfp->code, fp->code, sizeof(solu_instruction) * fp->code_c);
            if (fp->dbg)
                memcpy(nfp->dbg, fp->dbg, sizeof(solu_dbg) * fp->code_c);
            for (solu_val *v = fp->constants.data; v < fp->constants.data + fp->constants.count; ++v)
                solu_valvec_push(&nfp->constants, solu_dscopy(state, *v, true));
            break;
        }
        default: return SOLU_NIL;
    }
    return nv;
}

solu_val solu_dcopy(solu_state *state, solu_val val) {
    if (val.tt == SOLU_TDYN) {
        solu_dalloc *dh = solu_dheader(val);
        bool cache = false;

        if (dh->tt == SOLU_DSTR) {
            if (dh->size <= SOLU_STRCACHE_MAX - 1) {
                cache = true;
                solu_strcache_ex sex = solu_strcache_get(&state->strcache, sf_ref(val.dyn));
                if (sex.is_ok)
                    return (solu_val){ .tt = SOLU_TDYN, .dyn = sex.ok + 1 };
            }
        }

        val = solu_dscopy(state, val, false);
        if (cache) {
            dh = solu_dheader(val);
            solu_strcache_set(&state->strcache, sf_str_cdup(val.dyn), dh);
        }
        solu_dpush(state, solu_dheader(val));
    }
    return val;
}

void solu_dmarkfun(solu_fproto *fp) {
    for (solu_upvalue *v = fp->upvals; v && v < fp->upvals + fp->up_c; ++v) {
        if (v->tt == SOLU_UP_VAL && v->value.tt == SOLU_TDYN)
            solu_dmark(v->value);
    }
}
static void solu_dmarkmember(void *ud, sf_str _k, solu_val member) {
    (void)_k; (void)ud;
    solu_dmark(member);
}
void solu_dmarkobj(solu_val obj) {
    solu_dobj *dobj = (solu_dobj *)obj.dyn;
    for (uint32_t i = 0; i < dobj->array.count; ++i)
        solu_dmark(dobj->array.data[i]);
    solu_valmap_foreach(&dobj->map, solu_dmarkmember, NULL);
}
void solu_dmarkref(solu_val r) {
    solu_val inner = solu_dval(r);
    while (inner.tt == SOLU_TDYN) {
        solu_dalloc *dc = solu_dheader(inner);
        if (dc->mark == SOLU_DYN_BLACK) return;
        switch (solu_dtypeof(inner)) {
            case SOLU_DREF:
                inner = solu_dval(inner);
                break;
            default: solu_dmark(inner);
        }
        dc->mark = SOLU_DYN_BLACK;
    }
}
void solu_dmark(solu_val val) {
    if (val.tt != SOLU_TDYN) return;
    solu_dalloc *ac = solu_dheader(val);
    if (ac->mark == SOLU_DYN_BLACK) return;
    ac->mark = SOLU_DYN_BLACK;

    for (int i = 0; i < SOLU_META_COUNT; ++i)
        if (ac->metadata[i].tt == SOLU_TDYN)
            solu_dmark(ac->metadata[i]);
    if (ac->meta.tt == SOLU_TDYN)
        solu_dmark(ac->meta);

    switch (ac->tt) {
        case SOLU_DUSR: {
            solu_usrwrap *uh = solu_uheader(val);
            if (uh->mark) uh->mark(val.dyn);
            break;
        }
        case SOLU_DOBJ: solu_dmarkobj(val); break;
        case SOLU_DFUN: solu_dmarkfun((solu_fproto *)((char *)ac + sizeof(solu_dalloc))); break;
        case SOLU_DREF: solu_dmarkref(val); break;
        default: break;
    }
}

void solu_dcollect(solu_state *s) {
    s->lb = 0;
    for (solu_val *r = s->stack.data; r < s->stack.data + s->stack.count; ++r)
        solu_dmark(*r);
    solu_dmarkobj(s->global);

    // Mark Greens
    for (solu_dalloc *a = s->alloc; a; a = a->next) {
        if (a->held)
            solu_dmark((solu_val){ SOLU_TDYN, .dyn = (a + 1) });
    }

    solu_dalloc **ac = &s->alloc;
    solu_dalloc *last = NULL;
    while (*ac) {
        if ((*ac)->mark == SOLU_DYN_WHITE && !(*ac)->held) {
            solu_dalloc *dead = *ac;
            *ac = dead->next;
            if (dead->tt == SOLU_DSTR && dead->size < SOLU_STRCACHE_MAX)
                solu_strcache_delete(&s->strcache, sf_ref((char *)(dead + 1)));
            solu_dclean((solu_val){SOLU_TDYN, .dyn = dead + 1});
            continue;
        }
        last = *ac;
        s->lb += (*ac)->size;
        (*ac)->mark = SOLU_DYN_WHITE;
        ac = &(*ac)->next;
    }
    s->nb = (size_t)((double)s->lb * SOLU_GCSTEP);
    s->cb = s->lb;
    s->alloc_tail = last;
}

// Status

solu_call_ex solu_err(solu_state *s, char *fmt, ...) {
    va_list arglist;

    va_start(arglist, fmt);
    const size_t size =
        (size_t)vsnprintf(NULL, 0, fmt, arglist);
    va_end(arglist);

    char *_fmt = calloc(1, size + 1);
    va_start(arglist, fmt);
    vsnprintf(_fmt, size + 1, fmt, arglist);
    va_end(arglist);

    solu_val err = solu_dnerr(s, _fmt);
    free(_fmt);

    return solu_call_ex_ok(err);
}

solu_call_ex solu_panic(solu_state *s, char *fmt, ...) {
    va_list arglist;

    va_start(arglist, fmt);
    const size_t size =
        (size_t)vsnprintf(NULL, 0, fmt, arglist);
    va_end(arglist);

    char *_fmt = calloc(1, size + 1);
    va_start(arglist, fmt);
    vsnprintf(_fmt, size + 1, fmt, arglist);
    va_end(arglist);

    return solu_call_ex_err((solu_call_err){SOLU_ERRV_PANIC, _fmt, &s->trace, s->pc});
}

solu_type_ex solu_type_prim(solu_state *s, char *name, solu_tinfo ti) {
    solu_dalloc *alloc = s->alloc;
    while (alloc && alloc->next)
        alloc = alloc->next;
    solu_dalloc *da = malloc(sizeof(solu_dalloc) + sizeof(solu_tinfo));
    if (alloc)
        alloc->next = da;
    else s->alloc = da;
    *da = (solu_dalloc) {
        NULL,
        sizeof(solu_tinfo),
        SOLU_DUSR,
        SOLU_DYN_WHITE,
        true, SOLU_NIL, {SOLU_NIL}
    };

    solu_tinfo *tip = (solu_tinfo *)(da + 1);
    *tip = ti;

    solu_val val = {SOLU_TDYN, .dyn=tip};
    solu_valmap_set(&s->typeenv.types, sf_str_cdup(name), val);

    solu_dhold(val);
    return solu_type_ex_ok(val);
}

static char *solu_skip_ws(char *p) {
    while (*p == ' ') ++p;
    return p;
}

static bool solu_parse_type_atom(char **ptk, char out[256], bool *nil, bool *err) {
    char *tk = solu_skip_ws(*ptk);
    int depth = 0;
    int i = 0;

    while (*tk && i < 255) {
        if (*tk == '(') {
            depth++;
            out[i++] = *tk++;
            continue;
        }
        if (*tk == ')') {
            if (depth == 0)
                break;
            depth--;
            out[i++] = *tk++;
            continue;
        }
        if (*tk == ',' && depth == 0)
            break;
        if ((*tk == '?' || *tk == '!') && depth == 0)
            break;
        out[i++] = *tk++;
    }

    out[i] = '\0';

    *nil = false;
    *err = false;

    if (*tk == '?') {
        *nil = true;
        ++tk;
    }
    if (strcmp(out, "err") == 0 || *tk == '!') {
        *err = true;
        ++tk;
    }

    *ptk = tk;
    return out[0] != '\0';
}

solu_type_ex solu_type_fun(solu_state *s, char *signature) {
    char *tk = signature;
    if (*tk != '(')
        return solu_type_ex_err(SOLU_ERRP_EXPECTED_LPAREN);
    ++tk;

    sf_str canonical = sf_str_cdup("(");
    solu_tinfo ti = {
        SOLU_NIL,
        SOLU_TI_FUN,
        .fun = {
            .args = NULL,
            .arg_c = 0,
            .return_t = {solu_valmap_get(&s->typeenv.types, sf_lit("any")).ok, true, true, true},
        }
    };
    while (*tk != ')' && *tk) {
        if (*tk == ' ') { ++tk; continue; }

        char t[256] = {0};
        int l = 1;
        bool nil = false, err = false;
        if (!solu_parse_type_atom(&tk, t, &nil, &err)) {
            if (ti.fun.arg_c) free(ti.fun.args);
            sf_str_free(canonical);
            return solu_type_ex_err(SOLU_ERRP_EXPECTED_TYPE);
        }
        if ((*tk != ',' && *tk != ')') || l > 1) {
            if (ti.fun.arg_c) free(ti.fun.args);
            sf_str_free(canonical);
            return solu_type_ex_err(SOLU_ERRP_EXPECTED_RPAREN);
        }
        if (*tk == ',') ++tk;

        solu_val ok = SOLU_NIL;
        if (*t == '(') {
            solu_type_ex ex = solu_type_fun(s, t);
            if (!ex.is_ok) {
                if (ti.fun.arg_c) free(ti.fun.args);
                sf_str_free(canonical);
                return ex;
            }
            ok = ex.ok;
        } else {
            solu_valmap_ex ex = solu_valmap_get(&s->typeenv.types, sf_ref(t));
            if (!ex.is_ok) {
                if (ti.fun.arg_c) free(ti.fun.args);
                sf_str_free(canonical);
                return solu_type_ex_err(SOLU_ERRC_UNDEFINED_TYPE);
            }
            ok = ex.ok;
        }
        ti.fun.args = realloc(ti.fun.args, sizeof(solu_type) * ++ti.fun.arg_c);
        ti.fun.args[ti.fun.arg_c - 1] = (solu_type){ok, nil, err, true};

        if (ti.fun.arg_c > 1)
            sf_str_append(&canonical, sf_lit(", "));
        sf_str_append(&canonical, sf_ref(t));
        if (nil) sf_str_append(&canonical, sf_lit("?"));
        if (err) sf_str_append(&canonical, sf_lit("!"));
    }
    if (*tk != ')') {
        if (ti.fun.arg_c) free(ti.fun.args);
        sf_str_free(canonical);
        return solu_type_ex_err(SOLU_ERRP_EXPECTED_RPAREN);
    }
    sf_str_append(&canonical, sf_lit(")"));
    ++tk;
    while (*tk == ' ') ++tk;
    if (*tk == '-' && *(tk+1) == '>') {
        tk += 2;

        char t[256] = {0};
        bool nil = false, err = false;
        if (!solu_parse_type_atom(&tk, t, &nil, &err)) {
            if (ti.fun.arg_c) free(ti.fun.args);
            sf_str_free(canonical);
            return solu_type_ex_err(SOLU_ERRP_EXPECTED_TYPE);
        }

        char *ws = t;
        while (*ws == ' ') ++ws;

        solu_val ok = SOLU_NIL;
        if (*ws == '(') {
            solu_type_ex rex = solu_type_fun(s, ws);
            if (!rex.is_ok) {
                if (ti.fun.arg_c) free(ti.fun.args);
                sf_str_free(canonical);
                return rex;
            }
            ok = rex.ok;
        } else {
            solu_valmap_ex ex = solu_valmap_get(&s->typeenv.types, sf_ref(ws));
            if (!ex.is_ok) {
                if (ti.fun.arg_c) free(ti.fun.args);
                sf_str_free(canonical);
                return solu_type_ex_err(SOLU_ERRC_UNDEFINED_TYPE);
            }
            ok = ex.ok;
        }
        ti.fun.return_t = (solu_type){ok, nil, err, true};

        sf_str_append(&canonical, sf_lit(" -> "));
        sf_str_append(&canonical, sf_ref(ws));
        if (nil) sf_str_append(&canonical, sf_lit("?"));
        if (err) sf_str_append(&canonical, sf_lit("!"));
    }
    ti.name = solu_dnstr(s, canonical.c_str);
    solu_dhold(ti.name);

    solu_valmap_ex exists = solu_valmap_get(&s->typeenv.types, canonical);
    sf_str_free(canonical);
    if (exists.is_ok) {
        if (ti.fun.arg_c) free(ti.fun.args);
        return solu_type_ex_ok(exists.ok);
    }
    return solu_type_prim(s, ti.name.dyn, ti);
}

static void solu_type_delete(solu_state *s, solu_val type) {
    solu_val name = ((solu_tinfo *)type.dyn)->name;
    solu_valmap_delete(&s->typeenv.types, sf_ref(name.dyn));
    solu_drelease(name);
    solu_drelease(type);
}

solu_type_ex solu_type_def(solu_state *s, char *name, solu_complex_member *members, uint32_t mem_c, bool complex) {
    solu_val nval = solu_dnstr(s, name);
    solu_val _temp;
    solu_valmap_ex exists = solu_valmap_get(&s->typeenv.types, sf_ref(name));
    if (exists.is_ok) _temp = exists.ok;
    else {
        solu_type_ex ex = solu_type_prim(s, name, (solu_tinfo){nval, SOLU_TI_DEF, .def={0}});
        if (!ex.is_ok) return ex;
        _temp = ex.ok;
    }

    solu_tinfo ti = {
        nval,
        SOLU_TI_DEF,
        .complex = complex,
        .def = solu_def_new(),
    };
    for (uint32_t i = 0; i < mem_c; ++i) {
        solu_complex_member m = members[i];
        char *tk = m.type;
        while (*tk == ' ') ++tk;
        solu_val type = SOLU_NIL;
        if (*tk == '(') {
            solu_type_ex ex = solu_type_fun(s, m.type);
            if (!ex.is_ok) {
                solu_type_delete(s, _temp);
                solu_def_free(&ti.def);
                return ex;
            }
            type = ex.ok;
        } else {
            solu_valmap_ex exists = solu_valmap_get(&s->typeenv.types, sf_ref(m.type));
            if (!exists.is_ok) {
                solu_type_delete(s, _temp);
                solu_def_free(&ti.def);
                return solu_type_ex_err(SOLU_ERRC_UNDEFINED_TYPE);
            }
            type = exists.ok;
        }
        solu_def_set(&ti.def, sf_str_cdup(m.name), (solu_type){type, m.nil, m.err, true});
    }
    solu_dhold(ti.name);

    memcpy(_temp.dyn, &ti, sizeof(solu_tinfo));
    return solu_type_ex_ok(_temp);
}

solu_type_ex solu_type_global(solu_state *s, char *global, char *type, bool nil, bool err) {
    char *tk = type;
    while (*tk == ' ') ++tk;
    solu_val _type = SOLU_NIL;
    if (*tk == '(') {
        solu_type_ex ex = solu_type_fun(s, type);
        if (!ex.is_ok) return ex;
        _type = ex.ok;
    } else {
        solu_valmap_ex exists = solu_valmap_get(&s->typeenv.types, sf_ref(type));
        if (!exists.is_ok)
            return solu_type_ex_err(SOLU_ERRC_UNDEFINED_TYPE);
        _type = exists.ok;
    }

    solu_def_set(&s->typeenv.global->def, sf_str_cdup(global), (solu_type){_type, nil, err, true});
    return solu_type_ex_ok(_type);
}
