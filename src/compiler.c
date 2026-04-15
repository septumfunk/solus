#include "solus/compiler.h"
#include "solus/bytecode.h"
#include "solus/val.h"
#include "solus/syntax.h"
#include "sf/str.h"
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef _WIN32
#define TUI_UL  "\x1b[4m"
#define TUI_BLD "\x1b[1m"
#define TUI_ERR "\x1b[1;31m"
#define TUI_CLR "\x1b[0m"
#else
#define TUI_UL  ""
#define TUI_BLD ""
#define TUI_ERR ""
#define TUI_CLR ""
#endif

/// A simple representation of a local variable (or upvalue)
typedef struct {
    uint32_t reg, scope;
    bool upval, mut;
    uint32_t frame;
} solu_local;

struct solu_scope;
static void _solu_scope_cleanup(struct solu_scope *);
#define MAP_NAME solu_scope
#define MAP_K sf_str
#define MAP_V solu_local
#define EQUAL_FN sf_str_eq
#define HASH_FN sf_str_hash
#define KCLEANUP sf_str_free
#define CLEANUP_FN _solu_scope_cleanup
#include <sf/containers/map.h>
static void _solu_scope_fe(void *ud, sf_str key, solu_local loc) {
    (void)ud; (void)loc;
    sf_str_free(key);
}
static void _solu_scope_cleanup(struct solu_scope *s) {
    solu_scope_foreach(s, _solu_scope_fe, NULL);
}

struct solu_scopes;
static void _solu_scopes_cleanup(struct solu_scopes *);
#define VEC_NAME solu_scopes
#define VEC_T solu_scope
#define VSIZE_T uint32_t
#define VSIZE_MAX UINT32_MAX
#define CLEANUP_FN _solu_scopes_cleanup
#include <sf/containers/vec.h>
static void _solu_scopes_cleanup(struct solu_scopes *v) {
    for (size_t i = 0; i < v->count; ++i)
        solu_scope_free(v->data + i);
}

typedef struct { uint16_t idx; solu_tokentype tt; } solu_control;
#define VEC_NAME solu_controls
#define VEC_T solu_control
#define VSIZE_T uint32_t
#define VSIZE_MAX UINT32_MAX
#include <sf/containers/vec.h>

/// Temporary compilation info that's shared between all compiler functions
typedef struct {
    solu_fproto proto;
    solu_ast ast;
    solu_scopes scopes;
    uint32_t temps, max_reg, frame;
    solu_dalloc *alloc;

    uint32_t obj_r;
    solu_valmap fields;
    bool inloop;
    solu_controls controls;
} solu_compiler;

#define OP_W 10
/// Add an instruction to the proto.
/// Optionally logs every instruction compiled (see SOLU_DBG_LOG)
static inline void solu_cemitraw(solu_compiler *c, solu_instruction ins, uint16_t line, uint16_t column) {
    c->proto.code = realloc(c->proto.code, ++c->proto.code_c * sizeof(solu_instruction));
    c->proto.code[c->proto.code_c - 1] = ins;
    c->proto.dbg = realloc(c->proto.dbg, c->proto.code_c * sizeof(solu_dbg));
    c->proto.dbg[c->proto.code_c - 1] = SOLU_DBG_ENCODE(line, column);
}
#define solu_cemit(c, ins) solu_cemitraw(c, ins, node->line, node->column)

/// Match *_EQUALS to the instruction
static inline solu_opcode solu_eq_op(solu_tokentype tt) {
    switch (tt) {
        case TK_PLUS_EQUAL: return SOLU_OP_ADD;
        case TK_MINUS_EQUAL: return SOLU_OP_SUB;
        case TK_STAR_EQUAL: return SOLU_OP_MUL;
        case TK_SLASH_EQUAL: return SOLU_OP_DIV;
        default: return SOLU_OP_ADD;
    }
}

/// Reserve local variable register
static inline uint32_t solu_rtemp(solu_compiler *c) {
    ++c->temps;
    c->max_reg = c->temps > c->max_reg ? c->temps : c->max_reg;
    return c->temps - 1;
}
#define solu_rlocal solu_rtemp
/// Find whether a local exists, and output the local if it does
static inline bool solu_lexists(solu_compiler *c, char *name, solu_local *loc) {
    for (solu_scope *s = c->scopes.data + c->scopes.count - 1; s != c->scopes.data - 1; --s) {
        solu_scope_ex sc_ex = solu_scope_get(s, sf_ref(name));
        if (sc_ex.is_ok) {
            *loc = sc_ex.ok;
            return true;
        }
    }
    if (strcmp(name, "self") == 0 && c->obj_r != UINT32_MAX) {
        *loc = (solu_local){c->obj_r, c->scopes.count - 1, false, false, c->frame};
        return true;
    }
    return false;
}
/// Clear temporary register(s)
static inline void solu_ctemps(solu_compiler *c, uint32_t count) {
    if (count > c->temps) {
        fprintf(stderr, "TEMP UNDERFLOW: ctemps(%u) but temps=%u\n", count, c->temps);
        abort();
    }
    c->temps -= count;
}
#define solu_clocals solu_ctemps
/// Find whether a constant exists, and output the index if it does
bool solu_kfind(solu_compiler *c, solu_val con, uint32_t *idx) {
    if (con.tt == SOLU_TBOOL) {
        *idx = con.boolean;
        return true;
    }
    for (uint32_t i = 0; i < c->proto.constants.count; ++i) {
        solu_val v = c->proto.constants.data[i];
        if (v.tt != con.tt) continue;
        switch (v.tt) {
            case SOLU_TNIL: *idx = i; return true;
            case SOLU_TF64: if (v.f64 == con.f64) { *idx = i; return true; } else continue;
            case SOLU_TI64: if (v.i64 == con.i64) { *idx = i; return true; } else continue;
            case SOLU_TDYN: {
                if (solu_isdtype(v, SOLU_DSTR) && solu_isdtype(con, SOLU_DSTR))
                    if (solu_streq(v, con)) { *idx = i; return true; }
                    else continue;
                else if (v.dyn == con.dyn) { *idx = i; return true; }
                else continue;
            }
            default: continue;
        }
    }
    return false;
}
/// Add a constant to the proto
static uint32_t solu_kadd(solu_compiler *c, solu_val con) {
    if (con.tt == SOLU_TDYN) {
        size_t size = sizeof(solu_dalloc) + solu_dheader(con)->size;
        solu_dalloc *ac = malloc(size);
        memcpy(ac, (char *)con.dyn - sizeof(solu_dalloc), size);
        con = (solu_val){SOLU_TDYN, .dyn=ac + 1};
        solu_dheader(con)->held = true;
    }
    solu_valvec_push(&c->proto.constants, con);
    return c->proto.constants.count - 1;
}

/// Shorthand macro for returning a solu_cnode_ex_err
#define solu_cerr(type) solu_cnode_ex_err((solu_compile_err){(type), node->line, node->column})


#define EXPECTED_NAME solu_cnode_ex
#define EXPECTED_E solu_compile_err
#include <sf/containers/expected.h>
solu_cnode_ex solu_cnode(solu_compiler *c, solu_node *node, uint32_t t_reg);

/// Compile a fun from a block and info
solu_compile_ex solu_cfun(sf_str file_name, uint32_t frame, solu_dalloc *alloc, solu_node *ast, uint32_t arg_c, solu_val *args, uint32_t up_c, solu_upvalue *upvals) {
    solu_compiler c = {
        .proto = solu_fproto_new(),
        .ast = ast,
        .scopes = solu_scopes_new(),
        .temps = arg_c,
        .max_reg = arg_c,
        .alloc = alloc,
        .obj_r = UINT_MAX,
        .frame = frame,

        .controls = solu_controls_new(),
    };
    c.proto.arg_c = arg_c;
    c.proto.file_name = file_name;
    solu_scopes_push(&c.scopes, solu_scope_new());
    for (uint32_t i = 0; i < arg_c; ++i)
        solu_scope_set(c.scopes.data + c.scopes.count - 1, sf_str_cdup(args[i].dyn), (solu_local){i, 0, false, true, 0});
    for (uint32_t i = 0; i < up_c; ++i)
        solu_scope_set(c.scopes.data + c.scopes.count - 1, sf_str_dup(upvals[i].name), (solu_local){i, 0, true, upvals[i].mut, upvals[i].frame});

    solu_kadd(&c, (solu_val){.tt = SOLU_TBOOL, .boolean = false});
    solu_kadd(&c, (solu_val){.tt = SOLU_TBOOL, .boolean = true});

    c.proto.upvals = malloc(sizeof(solu_upvalue) * up_c);
    memcpy(c.proto.upvals, upvals, sizeof(solu_upvalue) * up_c);
    c.proto.up_c = up_c;

    solu_cnode_ex e = solu_cnode(&c, c.ast, UINT32_MAX);
    c.proto.reg_c = c.max_reg;

    solu_scopes_free(&c.scopes);
    solu_controls_free(&c.controls);
    return e.is_ok ? solu_compile_ex_ok(c.proto) : solu_compile_ex_err(e.err);
}

static solu_cnode_ex solu_cmembers(solu_compiler *c, solu_node *node, uint32_t t_reg) {
    uint32_t s_index = 0;
    uint32_t it = solu_rtemp(c);
    uint32_t obj_r = c->obj_r;
    c->obj_r = t_reg;
    for (uint32_t i = 0; i < node->n_obj.mem_c; ++i) {
        solu_node *nd = node->n_obj.members[i];
        uint32_t key_i;
        solu_cnode_ex right;
        if (nd->tt != SOLU_ND_BINARY || nd->n_binary.op != TK_EQUAL) {
            solu_val idx = (solu_val){SOLU_TI64, .i64 = (solu_i64)s_index++};
            if (!solu_kfind(c, idx, &key_i))
                key_i = solu_kadd(c, idx);
            right = solu_cnode(c, nd, it);
            if (!right.is_ok) return right;
            solu_cemit(c, solu_ins_ab(SOLU_OP_PUSH, t_reg, it));
        } else {
            if (!solu_kfind(c, nd->n_binary.left->n_identifier, &key_i))
                key_i = solu_kadd(c, nd->n_binary.left->n_identifier);
            right = solu_cnode(c, nd->n_binary.right, it);
            if (!right.is_ok) return right;
            solu_cemit(c, solu_ins_abc(SOLU_OP_SET, t_reg, solu_const(key_i), solu_reg(it)));
        }
    }
    c->obj_r = obj_r;
    solu_ctemps(c, 1);
    return solu_cnode_ex_ok();
}

static inline int32_t jmp_ofs(uint32_t from, uint32_t to) {
    return (int32_t)to - (int32_t)(from + 1);
}

static inline void solu_evalcond(solu_compiler *c, solu_node *node, uint32_t t_reg) {
    if (node->tt == SOLU_ND_BINARY && solu_niscondition(node)) {
        solu_cemit(c, solu_ins_a(SOLU_OP_JMP, 2));
        solu_cemit(c, solu_ins_ab(SOLU_OP_LOAD, t_reg, 1));
        solu_cemit(c, solu_ins_a(SOLU_OP_JMP, 1));
        solu_cemit(c, solu_ins_ab(SOLU_OP_LOAD, t_reg, 0));
    }
}

/// Compile a single node into bytecode.
/// Passing UINT32_MAX as t_reg acts as a discard
solu_cnode_ex solu_cnode(solu_compiler *c, solu_node *node, uint32_t t_reg) {
    switch (node->tt) {
        // statements
        case SOLU_ND_LOCAL: {
            solu_scope_ex exists = solu_scope_get(c->scopes.data + c->scopes.count - 1, sf_ref(node->n_local.name.dyn));
            if (exists.is_ok)
                return solu_cerr(SOLU_ERRC_REDEFINED_LOCAL);
            uint32_t rhs = solu_rlocal(c);
            solu_scope_set(c->scopes.data + c->scopes.count - 1, sf_str_cdup(node->n_local.name.dyn), (solu_local){
                rhs, c->scopes.count - 1, false, node->n_local.mut, 0
            });
            return solu_cnode(c, node->n_local.value, rhs);
        }
        case SOLU_ND_IF: {
            uint32_t s = 0;
            solu_node *cond = node->n_if.condition;
            if (cond->tt == SOLU_ND_UNARY && cond->n_unary.op == TK_BANG) {
                s = 1;
                cond = cond->n_unary.right;
            }

            if (cond->tt == SOLU_ND_IDENTIFIER) {
                solu_local loc;
                if (!solu_lexists(c, cond->n_identifier.dyn, &loc)) { // Global
                    uint32_t name_i;
                    if (!solu_kfind(c, cond->n_identifier, &name_i))
                        name_i = solu_kadd(c, cond->n_identifier);

                    uint32_t id_r = solu_rtemp(c);
                    solu_cemit(c, solu_ins_abc(SOLU_OP_GUPO, id_r, solu_reg(0), solu_const(name_i)));
                    solu_cemit(c, solu_ins_abc(SOLU_OP_EQ, s, solu_reg(id_r), solu_const(1)));
                    solu_ctemps(c, 1);
                } else { // Local
                    if (loc.upval) { // Reserve temp for upval
                        uint32_t up = solu_rtemp(c);
                        solu_cemit(c, solu_ins_ab(SOLU_OP_GETU, up, loc.reg));
                        solu_cemit(c, solu_ins_abc(SOLU_OP_EQ, s, solu_reg(up), solu_const(1)));
                        solu_ctemps(c, 1);
                    } else
                        solu_cemit(c, solu_ins_abc(SOLU_OP_EQ, s, solu_reg(loc.reg), solu_const(1)));
                }
            } else {
                uint32_t cr = solu_rtemp(c);
                solu_cnode_ex ex = solu_cnode(c, cond, cr);
                if (!ex.is_ok) return ex;
                solu_cemit(c, solu_ins_abc(SOLU_OP_EQ, s, solu_reg(cr), solu_const(1)));
                solu_ctemps(c, 1);
            }

            uint32_t jmp_false = c->proto.code_c;
            solu_cemit(c, solu_ins_a(SOLU_OP_JMP, 0));

            // then
            solu_cnode_ex ex = solu_cnode(c, node->n_if.then_node, UINT32_MAX);
            if (!ex.is_ok) return ex;

            if (node->n_if.else_node) {
                uint32_t jmp_end = c->proto.code_c;
                solu_cemit(c, solu_ins_a(SOLU_OP_JMP, 0));

                uint32_t else_pc = c->proto.code_c;
                ex = solu_cnode(c, node->n_if.else_node, UINT32_MAX);
                if (!ex.is_ok) return ex;

                uint32_t end_pc = c->proto.code_c;
                c->proto.code[jmp_false] = solu_ins_a(SOLU_OP_JMP, jmp_ofs(jmp_false, else_pc));
                c->proto.code[jmp_end]   = solu_ins_a(SOLU_OP_JMP, jmp_ofs(jmp_end, end_pc));
            } else {
                uint32_t end_pc = c->proto.code_c;
                c->proto.code[jmp_false] = solu_ins_a(SOLU_OP_JMP, jmp_ofs(jmp_false, end_pc));
            }

            return solu_cnode_ex_ok();
        }
        case SOLU_ND_WHILE: {
            uint32_t jmp_cond = c->proto.code_c;
            uint32_t s = 0;
            solu_node *cond = node->n_while.condition;
            if (cond->tt == SOLU_ND_UNARY && cond->n_unary.op == TK_BANG) {
                s = 1;
                cond = cond->n_unary.right;
            }

            if (cond->tt == SOLU_ND_IDENTIFIER) {
                solu_local loc;
                if (!solu_lexists(c, cond->n_identifier.dyn, &loc)) { // Global
                    uint32_t name_i;
                    if (!solu_kfind(c, cond->n_identifier, &name_i))
                        name_i = solu_kadd(c, cond->n_identifier);

                    uint32_t id_r = solu_rtemp(c);
                    solu_cemit(c, solu_ins_abc(SOLU_OP_GUPO, id_r, solu_reg(0), solu_const(name_i)));
                    solu_cemit(c, solu_ins_abc(SOLU_OP_EQ, s, solu_reg(id_r), solu_const(1)));
                    solu_ctemps(c, 1);
                } else { // Local
                    if (loc.upval) { // Reserve temp for upval
                        uint32_t up = solu_rtemp(c);
                        solu_cemit(c, solu_ins_ab(SOLU_OP_GETU, up, loc.reg));
                        solu_cemit(c, solu_ins_abc(SOLU_OP_EQ, s, solu_reg(up), solu_const(1)));
                        solu_ctemps(c, 1);
                    } else
                        solu_cemit(c, solu_ins_abc(SOLU_OP_EQ, s, solu_reg(loc.reg), solu_const(1)));
                }
            } else {
                uint32_t cr = solu_rtemp(c);
                solu_cnode_ex ex = solu_cnode(c, cond, cr);
                if (!ex.is_ok) return ex;
                solu_cemit(c, solu_ins_abc(SOLU_OP_EQ, s, solu_reg(cr), solu_const(1)));
                solu_ctemps(c, 1);
            }

            uint32_t jmp_break = c->proto.code_c;
            solu_cemit(c, solu_ins_a(SOLU_OP_JMP, 0));

            // Do
            c->inloop = true;
            solu_cnode_ex ex = solu_cnode(c, node->n_while.stmt, UINT32_MAX);
            c->inloop = false;
            if (!ex.is_ok) return ex;

            solu_cemit(c, solu_ins_a(SOLU_OP_JMP, jmp_ofs(c->proto.code_c, jmp_cond)));
            uint32_t break_c = c->proto.code_c; // The end
            c->proto.code[jmp_break] = solu_ins_a(SOLU_OP_JMP, jmp_ofs(jmp_break, break_c));
            c->proto.dbg[jmp_break] = SOLU_DBG_ENCODE(node->n_while.condition->line, node->n_while.condition->column);
            while (c->controls.count) {
                solu_control patch = solu_controls_pop(&c->controls);
                c->proto.code[patch.idx] = solu_ins_a(SOLU_OP_JMP, patch.tt == TK_BREAK ? jmp_ofs(patch.idx, break_c) : jmp_ofs(patch.idx, jmp_cond));
            }

            return solu_cnode_ex_ok();
        }
        case SOLU_ND_FOR: {
            solu_scopes_push(&c->scopes, solu_scope_new());

            solu_cnode_ex pre = solu_cnode(c, node->n_for.pre, UINT32_MAX);
            if (!pre.is_ok) return pre;

            uint32_t jmp_cond = c->proto.code_c;
            uint32_t s = 0;
            solu_node *cond = node->n_for.condition;
            if (cond->tt == SOLU_ND_UNARY && cond->n_unary.op == TK_BANG) {
                s = 1;
                cond = cond->n_unary.right;
            }
            if (cond->tt == SOLU_ND_IDENTIFIER) {
                solu_local loc;
                if (!solu_lexists(c, cond->n_identifier.dyn, &loc)) { // Global
                    uint32_t name_i;
                    if (!solu_kfind(c, cond->n_identifier, &name_i))
                        name_i = solu_kadd(c, cond->n_identifier);

                    uint32_t id_r = solu_rtemp(c);
                    solu_cemit(c, solu_ins_abc(SOLU_OP_GUPO, id_r, solu_reg(0), solu_const(name_i)));
                    solu_cemit(c, solu_ins_abc(SOLU_OP_EQ, s, solu_reg(id_r), solu_const(1)));
                    solu_ctemps(c, 1);
                } else { // Local
                    if (loc.upval) { // Reserve temp for upval
                        uint32_t up = solu_rtemp(c);
                        solu_cemit(c, solu_ins_ab(SOLU_OP_GETU, up, loc.reg));
                        solu_cemit(c, solu_ins_abc(SOLU_OP_EQ, s, solu_reg(up), solu_const(1)));
                        solu_ctemps(c, 1);
                    } else
                        solu_cemit(c, solu_ins_abc(SOLU_OP_EQ, s, solu_reg(loc.reg), solu_const(1)));
                }
            } else {
                uint32_t cr = solu_rtemp(c);
                solu_cnode_ex ex = solu_cnode(c, cond, cr);
                if (!ex.is_ok) return ex;
                solu_cemit(c, solu_ins_abc(SOLU_OP_EQ, s, solu_reg(cr), solu_const(1)));
                solu_ctemps(c, 1);
            }

            uint32_t jmp_break = c->proto.code_c;
            solu_cemit(c, solu_ins_a(SOLU_OP_JMP, 0));

            // Do
            c->inloop = true;
            solu_cnode_ex ex = solu_cnode(c, node->n_for.body, UINT32_MAX);
            c->inloop = false;
            if (!ex.is_ok) return ex;

            uint32_t post_c = c->proto.code_c;
            solu_cnode_ex post = solu_cnode(c, node->n_for.post, UINT32_MAX);
            if (!post.is_ok) return post;

            solu_cemit(c, solu_ins_a(SOLU_OP_JMP, jmp_ofs(c->proto.code_c, jmp_cond)));
            uint32_t break_c = c->proto.code_c; // The end
            c->proto.code[jmp_break] = solu_ins_a(SOLU_OP_JMP, jmp_ofs(jmp_break, break_c));
            c->proto.dbg[jmp_break] = SOLU_DBG_ENCODE(node->n_while.condition->line, node->n_while.condition->column);
            while (c->controls.count) {
                solu_control patch = solu_controls_pop(&c->controls);
                c->proto.code[patch.idx] = solu_ins_a(SOLU_OP_JMP, patch.tt == TK_BREAK ? jmp_ofs(patch.idx, break_c) : jmp_ofs(patch.idx, post_c));
            }

            solu_scope sc = solu_scopes_pop(&c->scopes);
            solu_clocals(c, (uint32_t)sc.pair_count);
            solu_scope_free(&sc);

            return solu_cnode_ex_ok();
        }
        case SOLU_ND_INS: {
            solu_i64 opa[3];
            for (int i = 0; i < 3; ++i) {
                solu_val v = node->n_ins.opa[i];
                if ((node->n_ins.op == SOLU_OP_LOAD && i == 1) ||
                    (node->n_ins.op == SOLU_OP_SUPO && i == 1) ||
                    (i == 2 && v.tt != SOLU_TI64)) {
                    uint32_t pos;
                    if (!solu_kfind(c, node->n_literal, &pos))
                        pos = solu_kadd(c, v);
                    opa[i] = pos;
                    continue;
                }
                if (v.tt == SOLU_TDYN) { // Arg/Upval
                    solu_local loc;
                    if (!solu_lexists(c, v.dyn, &loc))
                        return solu_cerr(SOLU_ERRC_UNKNOWN_LOCAL);
                    opa[i] = loc.reg;
                    continue;
                }
                opa[i] = v.i64;
            }

            switch (solu_op_info(node->n_ins.op)->type) {
                case SOLU_INS_A:   solu_cemit(c, solu_ins_a(node->n_ins.op, opa[0])); break;
                case SOLU_INS_AB:  solu_cemit(c, solu_ins_ab((uint32_t)node->n_ins.op, (uint32_t)opa[0], (uint32_t)opa[1])); break;
                case SOLU_INS_ABC: solu_cemit(c, solu_ins_abc((uint32_t)node->n_ins.op, (uint32_t)opa[0], solu_reg((uint32_t)opa[1]),
                    node->n_ins.opa[2].tt == SOLU_TI64 ? solu_reg((uint32_t)opa[2]) : solu_const((uint32_t)opa[2]))
                ); break;
            }
            return solu_cnode_ex_ok();
        }
        case SOLU_ND_RETURN: {
            if (node->n_return.implicit && t_reg != UINT_MAX) {
                solu_cnode_ex ex = solu_cnode(c, node->n_return.expr, t_reg);
                return ex;
            }
            uint32_t r = solu_rtemp(c);
            solu_cnode_ex ex = solu_cnode(c, node->n_return.expr, r);
            if (!ex.is_ok) return ex;
            solu_cemit(c, solu_ins_a(SOLU_OP_RET, r));
            solu_ctemps(c, 1);
            return solu_cnode_ex_ok();
        }
        case SOLU_ND_LCONTROL: {
            if (!c->inloop) return solu_cerr(SOLU_ERRC_UNEXPECTED_CONTINUE);
            solu_controls_push(&c->controls, (solu_control){.idx = c->proto.code_c, .tt = node->n_lcontrol});
            solu_cemit(c, solu_ins_a(SOLU_OP_JMP, 0));
            return solu_cnode_ex_ok();
        }
        // operators
        case SOLU_ND_UNARY: {
            uint32_t right = solu_rtemp(c);
            solu_cnode_ex right_ex = solu_cnode(c, node->n_unary.right, right);
            if (!right_ex.is_ok) return right_ex;

            switch(node->n_unary.op) {
                case TK_MINUS:
                    solu_cemit(c, solu_ins_ab(SOLU_OP_NEG, t_reg, right));
                    break;
                case TK_BANG: {
                    solu_cemit(c, solu_ins_abc(SOLU_OP_EQ, 1, solu_reg(right), solu_const(1)));
                    solu_cemit(c, solu_ins_a(SOLU_OP_JMP, 2));
                    solu_cemit(c, solu_ins_ab(SOLU_OP_LOAD, t_reg, 1));
                    solu_cemit(c, solu_ins_a(SOLU_OP_JMP, 1));
                    solu_cemit(c, solu_ins_ab(SOLU_OP_LOAD, t_reg, 0));
                    break;
                }
                default:
                    return solu_cerr(SOLU_ERRC_UNKNOWN);
            }
            solu_ctemps(c, 1);
            return solu_cnode_ex_ok();
        }
        case SOLU_ND_BINARY: {
            bool ll = false, rl = false;
            bool lt = false, rt = false;
            uint32_t left = UINT32_MAX, right = UINT32_MAX;
            bool set = t_reg == UINT32_MAX;

            if (node->n_binary.op != TK_AND && node->n_binary.op != TK_OR) {
                if (node->n_binary.op != TK_EQUAL && node->n_binary.op != TK_PLUS_EQUAL && node->n_binary.op != TK_MINUS_EQUAL) {
                    if (node->n_binary.left->tt == SOLU_ND_LITERAL && node->n_binary.op != TK_AND && node->n_binary.op != TK_OR) {
                        if (!solu_kfind(c, node->n_binary.left->n_literal, &left))
                            left = solu_kadd(c, node->n_binary.left->n_literal);
                        ll = true;
                    } else {
                        left = solu_rtemp(c);
                        lt = true;
                        solu_cnode_ex left_ex = solu_cnode(c, node->n_binary.left, left);
                        if (!left_ex.is_ok) return left_ex;
                    }
                }
                if (node->n_binary.op != TK_AND && node->n_binary.op != TK_OR) {
                    if (node->n_binary.right->tt == SOLU_ND_LITERAL) {
                        if (!solu_kfind(c, node->n_binary.right->n_literal, &right))
                            right = solu_kadd(c, node->n_binary.right->n_literal);
                        rl = true;
                    } else {
                        right = solu_rtemp(c);
                        rt = true;
                        solu_cnode_ex right_ex = solu_cnode(c, node->n_binary.right, right);
                        if (!right_ex.is_ok) return right_ex;
                    }
                }
            }

            uint32_t ot = UINT32_MAX;
            switch (node->n_binary.op) {
            case TK_AND:
            case TK_OR: {
                solu_cnode_ex ex = solu_cnode(c, node->n_binary.left, t_reg);
                if (!ex.is_ok) return ex;

                solu_cemit(c, solu_ins_abc(SOLU_OP_EQ, (node->n_binary.op != TK_AND),
                    solu_reg(t_reg), solu_const(1)));
                uint32_t lhs_j = c->proto.code_c;
                solu_cemit(c, solu_ins_a(SOLU_OP_JMP, 0));

                ex = solu_cnode(c, node->n_binary.right, t_reg);
                if (!ex.is_ok) return ex;

                c->proto.code[lhs_j] =  solu_ins_a(SOLU_OP_JMP, jmp_ofs(lhs_j, c->proto.code_c));
                set = true;
                break;
            }

            case TK_PLUS_EQUAL: case TK_MINUS_EQUAL:
            case TK_STAR_EQUAL: case TK_SLASH_EQUAL:
            case TK_EQUAL: {
                if (node->n_binary.left->tt == SOLU_ND_IDENTIFIER) {
                    solu_local loc;
                    if (strcmp(node->n_binary.left->n_identifier.dyn, "self") == 0)
                        return solu_cerr(SOLU_ERRC_REASSIGNED_SELF);
                    if (solu_lexists(c, node->n_binary.left->n_identifier.dyn, &loc)) {
                        if (!loc.mut)
                            return solu_cerr(SOLU_ERRC_REASSIGNED_VAL);
                        if (loc.upval) {
                            if (node->n_binary.op == TK_EQUAL) {
                                if (rl) {
                                    ot = solu_rtemp(c);
                                    solu_cemit(c, solu_ins_ab(SOLU_OP_LOAD, ot, right));
                                }
                                solu_cemit(c, solu_ins_ab(SOLU_OP_SETU, loc.reg, rl ? ot : right));
                                if (t_reg != UINT32_MAX)
                                    solu_cemit(c, solu_ins_ab(SOLU_OP_MOVE, t_reg, rl ? ot : right));
                            } else if (node->n_binary.op == TK_PLUS_EQUAL && node->n_binary.right->tt == SOLU_ND_OBJ) {
                                ot = solu_rtemp(c);
                                solu_cemit(c, solu_ins_ab(SOLU_OP_GETU, ot, loc.reg));
                                solu_cnode_ex ex = solu_cmembers(c, node->n_binary.right, ot);
                                if (!ex.is_ok) return ex;
                                if (t_reg != UINT32_MAX)
                                    solu_cemit(c, solu_ins_ab(SOLU_OP_MOVE, t_reg, ot));
                            } else {
                                ot = solu_rtemp(c);
                                solu_cemit(c, solu_ins_ab(SOLU_OP_GETU, ot, loc.reg));
                                if (t_reg != UINT32_MAX && node->n_binary.flip)
                                    solu_cemit(c, solu_ins_ab(SOLU_OP_MOVE, t_reg, ot));
                                solu_cemit(c, solu_ins_abc((unsigned int)solu_eq_op(node->n_binary.op), ot, solu_reg(ot), rl ? solu_const(right) : solu_reg(right)));
                                solu_cemit(c, solu_ins_ab(SOLU_OP_SETU, loc.reg, ot));
                                if (t_reg != UINT32_MAX && !node->n_binary.flip)
                                    solu_cemit(c, solu_ins_ab(SOLU_OP_MOVE, t_reg, ot));
                            }
                        } else {
                            if (t_reg != UINT32_MAX && node->n_binary.flip)
                                solu_cemit(c, solu_ins_ab(SOLU_OP_MOVE, t_reg, loc.reg));
                            if (node->n_binary.op == TK_PLUS_EQUAL && node->n_binary.right->tt == SOLU_ND_OBJ) {
                                solu_cnode_ex ex = solu_cmembers(c, node->n_binary.right, loc.reg);
                                if (!ex.is_ok) return ex;
                            } else if (node->n_binary.op != TK_EQUAL) {
                                solu_cemit(c, solu_ins_abc((unsigned int)solu_eq_op(node->n_binary.op), loc.reg,
                                solu_reg(loc.reg), rl ? solu_const(right) : solu_reg(right)));
                            } else solu_cemit(c, solu_ins_ab(rl ? SOLU_OP_LOAD : SOLU_OP_MOVE, loc.reg, right));
                            if (t_reg != UINT32_MAX && !node->n_binary.flip)
                                solu_cemit(c, solu_ins_ab(SOLU_OP_MOVE, t_reg, loc.reg));
                        }
                    } else {
                        uint32_t up = solu_reg(0);
                        uint32_t name_i;
                        if (!solu_kfind(c, node->n_binary.left->n_identifier, &name_i))
                            name_i = solu_kadd(c, node->n_binary.left->n_identifier);
                        if (node->n_binary.op != TK_EQUAL) {
                            ot = solu_rtemp(c);
                            solu_cemit(c, solu_ins_abc(SOLU_OP_GUPO, ot, up, solu_const(name_i)));
                            if (t_reg != UINT32_MAX && node->n_binary.flip)
                                solu_cemit(c, solu_ins_ab(SOLU_OP_MOVE, t_reg, ot));
                            solu_cemit(c, solu_ins_abc((unsigned int)solu_eq_op(node->n_binary.op), ot, solu_reg(ot), rl ? solu_const(right) : solu_reg(right)));
                        }
                        solu_cemit(c, solu_ins_abc(SOLU_OP_SUPO, 0, solu_const(name_i), ot == UINT32_MAX ?
                            (rl ? solu_const(right) : solu_reg(right)) : solu_reg(ot)));
                        if (t_reg != UINT32_MAX && !node->n_binary.flip)
                            solu_cemit(c, ot == UINT32_MAX ? solu_ins_abc(SOLU_OP_GUPO, t_reg, up, solu_const(name_i))
                                : solu_ins_ab(SOLU_OP_MOVE, t_reg, ot));
                    }
                } else if (node->n_binary.left->tt == SOLU_ND_POSTFIX) {
                    uint32_t obj = solu_rtemp(c);
                    solu_cnode_ex ex = solu_cnode(c, node->n_binary.left->n_postfix.expr, obj);
                    uint32_t key_r;
                    if (!ex.is_ok) return ex;

                    bool ac = false;
                    if (node->n_binary.left->n_postfix.postfix->tt == SOLU_ND_LITERAL) {
                        uint32_t name_i;
                        if (!solu_kfind(c, node->n_binary.left->n_postfix.postfix->n_identifier, &name_i))
                            name_i = solu_kadd(c, node->n_binary.left->n_postfix.postfix->n_identifier);
                        key_r = solu_const(name_i);
                    } else {
                        uint32_t eval = solu_rtemp(c);
                        solu_cnode_ex ex = solu_cnode(c, node->n_binary.left->n_postfix.postfix, eval);
                        if (!ex.is_ok) return ex;
                        key_r = solu_reg(eval);
                        ac = true;
                    }

                    if (node->n_binary.op == TK_PLUS_EQUAL && node->n_binary.right->tt == SOLU_ND_OBJ) {
                        ot = solu_rtemp(c);
                        solu_instruction ins = solu_ins_abc(SOLU_OP_GET, ot, solu_reg(obj), key_r);
                        solu_cemit(c, ins);
                        solu_cnode_ex ex = solu_cmembers(c, node->n_binary.right, ot);
                        if (!ex.is_ok) return ex;
                        solu_ctemps(c, ac ? 1 : 2);
                        if (t_reg != UINT32_MAX)
                            solu_cemit(c, solu_ins_ab(SOLU_OP_MOVE, t_reg, ot));
                    } else {
                        if (node->n_binary.op != TK_EQUAL) {
                            ot = solu_rtemp(c);
                            solu_cemit(c, solu_ins_abc(SOLU_OP_GET, ot, solu_reg(obj), key_r));
                            if (t_reg != UINT32_MAX && node->n_binary.flip)
                                solu_cemit(c, solu_ins_ab(SOLU_OP_MOVE, t_reg, ot));
                            solu_cemit(c, solu_ins_abc((unsigned int)solu_eq_op(node->n_binary.op), ot, solu_reg(ot), rl ? solu_const(right) : solu_reg(right)));
                        }
                        solu_instruction ins = solu_ins_abc(SOLU_OP_SET, obj, key_r, ot == UINT32_MAX ?
                            (rl ? solu_const(right) : solu_reg(right)) : solu_reg(ot));
                        solu_cemit(c, ins);
                        if (t_reg != UINT32_MAX && !node->n_binary.flip)
                            solu_cemit(c, ot == UINT32_MAX ? solu_ins_abc(SOLU_OP_GET, t_reg, solu_reg(obj), key_r) :
                                solu_ins_ab(SOLU_OP_MOVE, t_reg, ot));
                        solu_ctemps(c, solu_iabc_bk(ins) ? 1 : 2);
                    }
                } else return solu_cerr(SOLU_ERRC_INVALID_ASSIGN);

                if (ot != UINT32_MAX) solu_ctemps(c, 1);
                if (rt) solu_ctemps(c, 1);
                if (lt) solu_ctemps(c, 1);
                return solu_cnode_ex_ok();
            }

            case TK_PLUS: solu_cemit(c, solu_ins_abc(SOLU_OP_ADD, t_reg, ll ? solu_const(left) : solu_reg(left),
                rl ? solu_const(right) : solu_reg(right))); set = true; break;
            case TK_MINUS: solu_cemit(c, solu_ins_abc(SOLU_OP_SUB, t_reg, ll ? solu_const(left) : solu_reg(left),
                rl ? solu_const(right) : solu_reg(right))); set = true;  break;
            case TK_ASTERISK: solu_cemit(c, solu_ins_abc(SOLU_OP_MUL, t_reg, ll ? solu_const(left) : solu_reg(left),
                rl ? solu_const(right) : solu_reg(right))); set = true;  break;
            case TK_SLASH: solu_cemit(c, solu_ins_abc(SOLU_OP_DIV, t_reg, ll ? solu_const(left) : solu_reg(left),
                rl ? solu_const(right) : solu_reg(right))); set = true; break;

            case TK_DOUBLE_EQUAL: solu_cemit(c, solu_ins_abc(SOLU_OP_EQ, 0, ll ? solu_const(left) : solu_reg(left),
                rl ? solu_const(right) : solu_reg(right))); break;
            case TK_LESS: solu_cemit(c, solu_ins_abc(SOLU_OP_LT, 0, ll ? solu_const(left) : solu_reg(left),
                rl ? solu_const(right) : solu_reg(right))); break;
            case TK_LESS_EQUAL: solu_cemit(c, solu_ins_abc(SOLU_OP_LE, 0, ll ? solu_const(left) : solu_reg(left),
                rl ? solu_const(right) : solu_reg(right))); break;

            case TK_NOT_EQUAL: solu_cemit(c, solu_ins_abc(SOLU_OP_EQ, 1, ll ? solu_const(left) : solu_reg(left),
                rl ? solu_const(right) : solu_reg(right))); break;
            case TK_GREATER: solu_cemit(c, solu_ins_abc(SOLU_OP_LE, 1, ll ? solu_const(left) : solu_reg(left),
                rl ? solu_const(right) : solu_reg(right))); break;
            case TK_GREATER_EQUAL: solu_cemit(c, solu_ins_abc(SOLU_OP_LT, 1, ll ? solu_const(left) : solu_reg(left),
                rl ? solu_const(right) : solu_reg(right))); break;

            default:
                return solu_cerr(SOLU_ERRC_UNKNOWN_OPERATION);
            }

            if (t_reg == UINT32_MAX)
                return solu_cerr(SOLU_ERRC_UNUSED_EVALUATION);
            if (!set)
                solu_evalcond(c, node, t_reg);

            if (rt) solu_ctemps(c, 1);
            if (lt) solu_ctemps(c, 1);
            return solu_cnode_ex_ok();
        }

        case SOLU_ND_POSTFIX: {
            if (t_reg == UINT32_MAX)
                return solu_cerr(SOLU_ERRC_UNUSED_EVALUATION);
            uint32_t lhs;
            bool rl = false;
            solu_local loc;
            if (node->n_postfix.expr->tt == SOLU_ND_IDENTIFIER &&
                solu_lexists(c, node->n_postfix.expr->n_identifier.dyn, &loc) &&
                !loc.upval) {
                lhs = loc.reg;
            } else {
                rl = true;
                lhs = solu_rtemp(c);
                solu_cnode_ex lex = solu_cnode(c, node->n_postfix.expr, lhs);
                if (!lex.is_ok) return lex;
            }

            if (node->n_postfix.postfix->tt == SOLU_ND_LITERAL) {
                uint32_t name_i;
                if (!solu_kfind(c, node->n_postfix.postfix->n_identifier, &name_i))
                    name_i = solu_kadd(c, node->n_postfix.postfix->n_identifier);
                solu_cemit(c, solu_ins_abc(SOLU_OP_GET, t_reg, solu_reg(lhs), solu_const(name_i)));
            } else {
                uint32_t eval = solu_rtemp(c);
                solu_cnode_ex ex = solu_cnode(c, node->n_postfix.postfix, eval);
                if (!ex.is_ok) return ex;
                solu_cemit(c, solu_ins_abc(SOLU_OP_GET, t_reg, solu_reg(lhs), solu_reg(eval)));
                solu_ctemps(c, 1);
            }
            if (rl) solu_ctemps(c, 1);

            return solu_cnode_ex_ok();
        }
        case SOLU_ND_CALL: {
            if (node->n_call.identifier->tt == SOLU_ND_POSTFIX) {
                solu_node *pf = node->n_call.identifier;
                uint32_t lhs = solu_rtemp(c);
                solu_cnode_ex lex = solu_cnode(c, pf->n_postfix.expr, lhs);
                if (!lex.is_ok) return lex;

                uint32_t rhs = solu_rtemp(c);
                solu_cnode_ex rex = solu_cnode(c, pf->n_postfix.postfix, rhs);
                if (!rex.is_ok) return rex;

                for (uint32_t i = 0; i < node->n_call.arg_c; ++i)
                    solu_rtemp(c);
                for (uint32_t i = 0; i < node->n_call.arg_c; ++i) {
                    uint32_t r = rhs + 1 + i;
                    solu_cnode_ex ex = solu_cnode(c, node->n_call.args[i], r);
                    if (!ex.is_ok) return ex;
                }

                solu_cemit(c, solu_ins_abc(SOLU_OP_MCALL, t_reg == UINT32_MAX ? solu_rtemp(c) : t_reg, solu_reg(lhs), solu_reg(node->n_call.arg_c)));
                solu_ctemps(c, node->n_call.arg_c + 2);
            } else {
                uint32_t f_reg = solu_rtemp(c);
                solu_cnode_ex lex = solu_cnode(c, node->n_call.identifier, f_reg);
                if (!lex.is_ok) return lex;

                for (uint32_t i = 0; i < node->n_call.arg_c; ++i)
                    solu_rtemp(c);
                for (uint32_t i = 0; i < node->n_call.arg_c; ++i) {
                    uint32_t r = f_reg + 1 + i;
                    solu_cnode_ex ex = solu_cnode(c, node->n_call.args[i], r);
                    if (!ex.is_ok) return ex;
                }

                solu_cemit(c, solu_ins_abc(SOLU_OP_CALL, t_reg == UINT32_MAX ? solu_rtemp(c) : t_reg, solu_reg(f_reg), solu_reg(node->n_call.arg_c)));
                solu_ctemps(c, t_reg == UINT32_MAX ? node->n_call.arg_c + 2 : node->n_call.arg_c + 1);
            }
            return solu_cnode_ex_ok();
        }
        // literals
        case SOLU_ND_IDENTIFIER: {
            if (t_reg == UINT32_MAX)
                return solu_cerr(SOLU_ERRC_UNUSED_EVALUATION);

            solu_local loc;
            if (!solu_lexists(c, node->n_identifier.dyn, &loc)) { // Global
                uint32_t name_i;
                if (!solu_kfind(c, node->n_identifier, &name_i))
                    name_i = solu_kadd(c, node->n_identifier);
                solu_cemit(c, solu_ins_abc(SOLU_OP_GUPO, t_reg, solu_reg(0), solu_const(name_i))); // state->global
            } else // Local/Upval
                solu_cemit(c, solu_ins_ab(loc.upval ? SOLU_OP_GETU : SOLU_OP_MOVE, t_reg, loc.reg));

            return solu_cnode_ex_ok();
        }
        case SOLU_ND_LITERAL: {
            if (t_reg == UINT32_MAX)
                return solu_cerr(SOLU_ERRC_UNUSED_EVALUATION);

            uint32_t pos;
            if (!solu_kfind(c, node->n_literal, &pos))
                pos = solu_kadd(c, node->n_literal);
            solu_cemit(c, solu_ins_ab(SOLU_OP_LOAD, t_reg, pos));
            return solu_cnode_ex_ok();
        }
        case SOLU_ND_OBJ: {
            if (t_reg == UINT32_MAX)
                return solu_cerr(SOLU_ERRC_UNUSED_EVALUATION);
            solu_cemit(c, solu_ins_a(SOLU_OP_NEW, t_reg));
            return solu_cmembers(c, node, t_reg);
        }
        // functions
        case SOLU_ND_BLOCK: {
            solu_scopes_push(&c->scopes, solu_scope_new());
            bool val = false;
            for (size_t i = 0; i < node->n_block.count; ++i) {
                solu_node *nd = node->n_block.stmts[i];
                solu_cnode_ex ex;
                if (nd->tt == SOLU_ND_RETURN && nd->n_return.implicit && t_reg != UINT_MAX) {
                    val = true;
                    ex = solu_cnode(c, nd->n_return.expr, t_reg);
                } else if (nd->tt == SOLU_ND_IF && t_reg != UINT_MAX) {
                    val = true;
                    ex = solu_cnode(c, nd, t_reg);
                } else ex = solu_cnode(c, nd, UINT_MAX);
                if (!ex.is_ok) return ex;
            }
            if (t_reg != UINT_MAX && !val) {
                uint32_t nil;
                if (!solu_kfind(c, SOLU_NIL, &nil))
                    nil = solu_kadd(c, SOLU_NIL);
                solu_cemit(c, solu_ins_ab(SOLU_OP_LOAD, t_reg, nil));
            }
            solu_scope s = solu_scopes_pop(&c->scopes);
            solu_clocals(c, (uint32_t)s.pair_count);
            solu_scope_free(&s);
            return solu_cnode_ex_ok();
        }
        case SOLU_ND_FUN:
        case SOLU_ND_ASM: {
            uint32_t r_asm = 0;
            if (node->tt == SOLU_ND_ASM) {
                r_asm = (uint32_t)node->n_asm.temps;
                node = node->n_asm.n_fun;
            }
            // Propagate global
            solu_upvalue *upvals;
            uint32_t ofs = 0;
            if (c->proto.upvals && sf_str_eq(c->proto.upvals[0].name, sf_lit("global"))) {
                solu_upvalue upv = c->proto.upvals[0];
                upvals = malloc((1 + node->n_fun.cap_c) * sizeof(solu_upvalue));
                upvals[0] = (solu_upvalue){
                    sf_islit(upv.name) ? upv.name : sf_str_dup(upv.name),
                    upv.tt,
                    .mut = upv.mut,
                };
                if (upv.tt == SOLU_UP_REF || upv.tt == SOLU_UP_UPV)
                    upvals[0].ref = upv.ref;
                else upvals[0].value = upv.value;
                ofs = 1;
            } else upvals = malloc(node->n_fun.cap_c * sizeof(solu_upvalue));

            bool self = false;
            for (uint32_t i = 0; i < node->n_fun.cap_c; ++i) {
                solu_val *cap = node->n_fun.captures + i;
                char *name = cap->dyn;
                // Self capture (reserved name)
                solu_local loc;
                if (c->obj_r != UINT_MAX && strcmp(name, "self") == 0) {
                    self = true;
                    upvals[ofs + i] = (solu_upvalue){sf_lit("self"), .tt = SOLU_UP_REF, .ref = c->obj_r, .frame = c->frame, .mut = false};
                } else {
                    if (!solu_lexists(c, name, &loc) && strcmp(name, "self") != 0)
                        return solu_cerr(SOLU_ERRC_UNKNOWN_LOCAL);
                    if (loc.upval)
                        upvals[ofs + i] = (solu_upvalue){sf_str_cdup(name), .tt = SOLU_UP_UPV, .ref = loc.reg, .frame = loc.frame, .mut = loc.mut };
                    else {
                        solu_cemit(c, solu_ins_a(SOLU_OP_REFU, loc.reg));
                        upvals[ofs + i] = (solu_upvalue){sf_str_cdup(name), .tt = SOLU_UP_REF, .ref = loc.reg, .frame = c->frame, .mut = loc.mut };
                    }
                }
            }

            solu_compile_ex ex = solu_cfun(
                sf_str_dup(c->proto.file_name),
                c->frame + 1,
                c->alloc,
                node->n_fun.stmt,
                node->n_fun.arg_c, node->n_fun.args,
                ofs + node->n_fun.cap_c, upvals
            );
            if (upvals) free(upvals);

            if (!ex.is_ok) return solu_cnode_ex_err(ex.err);
            ex.ok.self = self;
            if (r_asm != 0) ex.ok.reg_c += r_asm;
            solu_dyn p = calloc(1, sizeof(solu_dalloc) + sizeof(solu_fproto));
            solu_dalloc *dh = p, *dd = c->alloc;
            *dh = (solu_dalloc){
                .next = NULL,
                .size = sizeof(solu_fproto),
                .thread = 1,
                .tt = SOLU_DFUN,
                .mark = SOLU_DYN_WHITE,
                .held = true,
            };
            if (dd == NULL) c->alloc = dh;
            else {
                while (dd->next) dd = dd->next;
                dd->next = dh;
            }
            solu_val fun = (solu_val){ .tt = SOLU_TDYN, .dyn = (char *)p + sizeof(solu_dalloc) };
            *(solu_fproto *)fun.dyn = ex.ok;

            solu_kadd(c, fun);
            if (node->n_fun.include) {
                uint32_t r = solu_rtemp(c);
                solu_cemit(c, solu_ins_ab(SOLU_OP_LOAD, r, c->proto.constants.count - 1));
                solu_cemit(c, solu_ins_abc(SOLU_OP_CALL, t_reg, r, 0)); // Includes never have args
                solu_ctemps(c, 1);
            } else solu_cemit(c, solu_ins_ab(SOLU_OP_LOAD, t_reg, c->proto.constants.count - 1));
            return solu_cnode_ex_ok();
        }
        default: return solu_cerr(SOLU_ERRC_UNKNOWN);
    }
}

solu_compile_ex solu_cproto(sf_str path, char *src, uint32_t arg_c, solu_val *args, uint32_t up_c, solu_upvalue *upvals) {
    if (memcmp(src, "[SOLC]", 6) == 0)
        return solu_compile_ex_err((solu_compile_err){SOLU_ERRP_EXPECTED_SOURCE, 0, 0});
    solu_scan_ex scan_ex = solu_scan(sf_ref(src));
    if (!scan_ex.is_ok)
        return solu_compile_ex_err((solu_compile_err){scan_ex.err.tt, scan_ex.err.line, scan_ex.err.column});
    solu_parse_ex par_ex = solu_parse(path, scan_ex);
    solu_tokenvec_free(&scan_ex.ok.tv);
    if (!par_ex.is_ok) {
        if (scan_ex.is_ok)
            for (solu_dalloc *ac = scan_ex.ok.alloc; ac; ) {
                solu_dalloc *next = ac->next;
                free(ac);
                ac = next;
            }
        return solu_compile_ex_err((solu_compile_err){
            .tt = par_ex.err.tt,
            .line = par_ex.err.token.line,
            .column = par_ex.err.token.column,
        });
    }

    solu_compile_ex ex = solu_cfun(sf_str_dup(path), 0, scan_ex.ok.alloc, par_ex.ok, arg_c, args, up_c, upvals);
    solu_node_free(par_ex.ok);

    for (solu_dalloc *ac = scan_ex.ok.alloc; ac;) {
        solu_dalloc *next = ac->next;
        free(ac);
        ac = next;
    }
    return ex;
}
