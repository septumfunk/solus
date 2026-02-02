#include "solus/compiler.h"
#include "solus/bytecode.h"
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
    bool upval;
    uint32_t frame;
} solu_local;

struct solu_scope;

#define MAP_NAME solu_scope
#define MAP_K sf_str
#define MAP_V solu_local
#define EQUAL_FN sf_str_eq
#define HASH_FN sf_str_hash
#define KCLEANUP sf_str_free
#include <sf/containers/map.h>

struct solu_scopes;
void _solu_scopes_cleanup(struct solu_scopes *);
#define VEC_NAME solu_scopes
#define VEC_T solu_scope
#define VSIZE_T uint32_t
#define CLEANUP_FN _solu_scopes_cleanup
#include <sf/containers/vec.h>
void _solu_scopes_cleanup(struct solu_scopes *v) {
    for (size_t i = 0; i < v->count; ++i)
        solu_scope_free(v->data + i);
}

/// Temporary compilation info that's shared between all compiler functions
typedef struct {
    solu_fproto proto;
    solu_ast ast;
    solu_scopes scopes;
    uint32_t locals, max_locals, temps, max_temps, frame;
    solu_dalloc *alloc;

    uint32_t obj_r;
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

/// Reserve local variable register
static inline uint32_t solu_rlocal(solu_compiler *c) {
    ++c->locals;
    c->max_locals = c->locals > c->max_locals ? c->locals : c->max_locals;
    return c->locals - 1 + c->temps;
}
/// Clear local variable register(s)
static inline void solu_clocals(solu_compiler *c, uint32_t count) {
    c->locals -= count;
}
/// Find whether a local exists, and output the local if it does
static inline bool solu_lexists(solu_compiler *c, char *name, solu_local *loc) {
    for (solu_scope *s = c->scopes.data + c->scopes.count - 1; s != c->scopes.data - 1; --s) {
        solu_scope_ex sc_ex = solu_scope_get(s, sf_ref(name));
        if (sc_ex.is_ok) {
            *loc = sc_ex.ok;
            return true;
        }
    }
    return false;
}
/// Reserve temporary register
static inline uint32_t solu_rtemp(solu_compiler *c) {
    ++c->temps;
    c->max_temps = c->temps > c->max_temps ? c->temps : c->max_temps;
    return c->locals + c->temps - 1;
}
/// Clear temporary register(s)
static inline void solu_ctemps(solu_compiler *c, uint32_t count) {
    c->temps -= count;
}
/// Find whether a constant exists, and output the index if it does
bool solu_kfind(solu_compiler *c, solu_val con, uint32_t *idx) {
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
        solu_dheader(con)->mark = SOLU_DYN_GREEN;
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
solu_compile_ex solu_cfun(uint32_t frame, solu_dalloc *alloc, solu_node *ast, uint32_t arg_c, solu_val *args, uint32_t up_c, solu_upvalue *upvals) {
    solu_compiler c = {
        .proto = solu_fproto_new(),
        .ast = ast,
        .scopes = solu_scopes_new(),
        .locals = arg_c,
        .max_locals = arg_c,
        .temps = 0, .max_temps = 0,
        .alloc = alloc,
        .obj_r = UINT_MAX,
        .frame = frame,
    };
    c.proto.arg_c = arg_c;
    solu_scopes_push(&c.scopes, solu_scope_new());
    for (uint32_t i = 0; i < arg_c; ++i)
        solu_scope_set(c.scopes.data + c.scopes.count - 1, sf_str_cdup(args[i].dyn), (solu_local){i, 0, false, 0});
    for (uint32_t i = 0; i < up_c; ++i)
        solu_scope_set(c.scopes.data + c.scopes.count - 1, upvals[i].name, (solu_local){i, 0, true, upvals[i].frame});

    solu_kadd(&c, (solu_val){.tt = SOLU_TBOOL, .boolean = false});
    solu_kadd(&c, (solu_val){.tt = SOLU_TBOOL, .boolean = true});

    c.proto.upvals = malloc(sizeof(solu_upvalue) * up_c);
    memcpy(c.proto.upvals, upvals, sizeof(solu_upvalue) * up_c);
    c.proto.up_c = up_c;

    solu_cnode_ex e = solu_cnode(&c, c.ast, UINT32_MAX);
    c.proto.reg_c = c.max_locals + c.max_temps;

    solu_scopes_free(&c.scopes);
    return e.is_ok ? solu_compile_ex_ok(c.proto) : solu_compile_ex_err(e.err);
}

/// Compile a single node into bytecode.
/// Passing UINT32_MAX as t_reg acts as a discard
solu_cnode_ex solu_cnode(solu_compiler *c, solu_node *node, uint32_t t_reg) {
    switch (node->tt) {
        // statements
        case SOLU_ND_LET: {
            solu_scope_ex exists = solu_scope_get(c->scopes.data + c->scopes.count - 1, sf_ref(node->n_let.name.dyn));
            if (exists.is_ok)
                return solu_cerr(SOLU_ERRC_REDEFINED_LOCAL);
            uint32_t rhs = solu_rlocal(c);
            solu_cnode_ex rv_ex = solu_cnode(c, node->n_let.value, rhs);
            if (!rv_ex.is_ok) return rv_ex;

            solu_scope_set(c->scopes.data + c->scopes.count - 1, sf_str_cdup(node->n_let.name.dyn), (solu_local){rhs, c->scopes.count - 1, false, 0});

            if (node->n_let.value->tt == SOLU_ND_BINARY && solu_niscondition(node->n_let.value)) { // Conditions
                solu_cemit(c, solu_ins_ab(SOLU_OP_LOAD, rhs, 0));
                solu_cemit(c, solu_ins_ab(SOLU_OP_LOAD, rhs, 1));
            }
            return solu_cnode_ex_ok();
        }
        case SOLU_ND_IF: {
            uint32_t s = 0;
            solu_node *cond = node->n_if.condition;
            if (cond->tt == SOLU_ND_UNARY) {
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
                if (cond->tt == SOLU_ND_CALL || cond->tt == SOLU_ND_LITERAL)
                    solu_cemit(c, solu_ins_abc(SOLU_OP_EQ, s, solu_reg(cr), solu_const(1)));
                if (!ex.is_ok) return ex;
                solu_ctemps(c, 1);
            }

            uint32_t jmp_false = c->proto.code_c;
            solu_cemit(c, solu_ins_a(SOLU_OP_JMP, 0));

            // Then
            solu_cnode_ex ex = solu_cnode(c, node->n_if.then_node, t_reg);
            if (!ex.is_ok) return ex;
            uint32_t ofs = c->proto.code_c - (jmp_false + 1);

            if (node->n_if.else_node) {
                uint32_t jmp_end = c->proto.code_c;
                solu_cemit(c, solu_ins_a(SOLU_OP_JMP, 0));
                ex = solu_cnode(c, node->n_if.else_node, t_reg);
                if (!ex.is_ok) return ex;
                // Patch jump
                c->proto.code[jmp_false] = solu_ins_a(SOLU_OP_JMP, ofs + 1);
                c->proto.dbg[jmp_false] = SOLU_DBG_ENCODE(node->line, node->column);
                c->proto.code[jmp_end] = solu_ins_a(SOLU_OP_JMP, c->proto.code_c - (jmp_end + 1));
                c->proto.dbg[jmp_end] = SOLU_DBG_ENCODE(node->n_if.else_node->line, node->n_if.else_node->column);
            } else {
                c->proto.code[jmp_false] = solu_ins_a(SOLU_OP_JMP, ofs);
                c->proto.dbg[jmp_false] = SOLU_DBG_ENCODE(node->line, node->column);
            }

            return solu_cnode_ex_ok();
        }
        case SOLU_ND_WHILE: {
            uint32_t jmp_cond = c->proto.code_c - 1;
            uint32_t s = 0;
            solu_node *cond = node->n_while.condition;
            if (cond->tt == SOLU_ND_UNARY) {
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
                if (cond->tt == SOLU_ND_CALL || cond->tt == SOLU_ND_LITERAL) {
                    solu_cemit(c, solu_ins_abc(SOLU_OP_EQ, s, solu_reg(cr), solu_const(1)));
                    solu_ctemps(c, 1);
                }
                if (!ex.is_ok) return ex;
            }

            uint32_t jmp_break = c->proto.code_c;
            solu_cemit(c, solu_ins_a(SOLU_OP_JMP, 0));

            // Do
            solu_cnode_ex ex = solu_cnode(c, node->n_while.stmt, UINT32_MAX);
            if (!ex.is_ok) return ex;
            c->proto.code[jmp_break] = solu_ins_a(SOLU_OP_JMP, c->proto.code_c - jmp_break);
            c->proto.dbg[jmp_break] = SOLU_DBG_ENCODE(node->n_while.condition->line, node->n_while.condition->column);
            solu_cemit(c, solu_ins_a(SOLU_OP_JMP, jmp_cond - c->proto.code_c));

            return solu_cnode_ex_ok();
        }
        case SOLU_ND_INS: {
            solu_i64 opa[3];
            for (int i = 0; i < 3; ++i) {
                solu_val v = node->n_ins.opa[i];
                if ((node->n_ins.op == SOLU_OP_LOAD && i == 1) ||
                    (node->n_ins.op == SOLU_OP_SUPO && i == 1) ||
                    (node->n_ins.op == SOLU_OP_GUPO && i == 2)) {
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
                case SOLU_INS_ABC: solu_cemit(c, solu_ins_abc((uint32_t)node->n_ins.op, (uint32_t)opa[0], solu_reg((uint32_t)opa[1]), solu_reg((uint32_t)opa[2]))); break;
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
                    solu_cemit(c, solu_ins_abc(SOLU_OP_EQ, 0, solu_reg(right), solu_const(1)));
                    solu_cemit(c, solu_ins_ab(SOLU_OP_LOAD, t_reg, 1));
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
            uint32_t left, right;
            if (node->n_binary.op != TK_EQUAL && node->n_binary.op != TK_PLUS_EQUAL && node->n_binary.op != TK_MINUS_EQUAL) {
                if (node->n_binary.left->tt == SOLU_ND_LITERAL) {
                    if (!solu_kfind(c, node->n_binary.left->n_literal, &left))
                        left = solu_kadd(c, node->n_binary.left->n_literal);
                    ll = true;
                } else {
                    left = solu_rtemp(c);
                    solu_cnode_ex left_ex = solu_cnode(c, node->n_binary.left, left);
                    if (!left_ex.is_ok) return left_ex;
                }
            }
            if (node->n_binary.right->tt == SOLU_ND_LITERAL) {
                if (!solu_kfind(c, node->n_binary.right->n_literal, &right))
                    right = solu_kadd(c, node->n_binary.right->n_literal);
                rl = true;
            } else {
                right = solu_rtemp(c);
                solu_cnode_ex right_ex = solu_cnode(c, node->n_binary.right, right);
                if (!right_ex.is_ok) return right_ex;
            }

            uint32_t ot = UINT32_MAX;
            switch (node->n_binary.op) {
                case TK_PLUS_EQUAL:
                case TK_MINUS_EQUAL:
                case TK_EQUAL: {
                    if (node->n_binary.left->tt == SOLU_ND_IDENTIFIER) {
                        solu_local loc;
                        if (solu_lexists(c, node->n_binary.left->n_identifier.dyn, &loc)) { // Local/Upval
                            if (loc.upval) {
                                if (node->n_binary.op == TK_EQUAL)
                                    solu_cemit(c, solu_ins_ab(SOLU_OP_SETU, loc.reg, rl ? solu_const(right) : solu_reg(right)));
                                else {
                                    ot = solu_rtemp(c);
                                    solu_cemit(c, solu_ins_ab(SOLU_OP_GETU, ot, loc.reg));
                                    solu_cemit(c, solu_ins_abc(node->n_binary.op == TK_PLUS_EQUAL ? SOLU_OP_ADD : SOLU_OP_SUB, ot, solu_reg(ot), rl ? solu_const(right) : solu_reg(right)));
                                    solu_cemit(c, solu_ins_ab(SOLU_OP_SETU, loc.reg, ot));
                                }
                            } else {
                                if (node->n_binary.op != TK_EQUAL)
                                    solu_cemit(c, solu_ins_abc(node->n_binary.op == TK_PLUS_EQUAL ? SOLU_OP_ADD : SOLU_OP_SUB, loc.upval ? ot : loc.reg,
                                        solu_reg(loc.upval ? ot : loc.reg), rl ? solu_const(right) : solu_reg(right)));
                                else
                                    solu_cemit(c, solu_ins_ab(rl ? SOLU_OP_LOAD : SOLU_OP_MOVE, loc.reg, right));
                            }
                        } else { // Global
                            uint32_t name_i;
                            if (!solu_kfind(c, node->n_binary.left->n_identifier, &name_i))
                                name_i = solu_kadd(c, node->n_binary.left->n_identifier);
                            if (node->n_binary.op != TK_EQUAL) {
                                ot = solu_rtemp(c);
                                solu_cemit(c, solu_ins_abc(SOLU_OP_GUPO, ot, solu_reg(0), solu_const(name_i)));
                                solu_cemit(c, solu_ins_abc(node->n_binary.op == TK_PLUS_EQUAL ? SOLU_OP_ADD : SOLU_OP_SUB, ot, solu_reg(ot), rl ? solu_const(right) : solu_reg(right)));
                            }
                            solu_cemit(c, solu_ins_abc(SOLU_OP_SUPO, 0, solu_const(name_i), ot == UINT32_MAX ?
                                (rl ? solu_const(right) : solu_reg(right)) : solu_reg(ot))); // state->global
                        }
                    } else if (node->n_binary.left->tt == SOLU_ND_MEMBER) { // Member Assign
                        uint32_t obj = solu_rtemp(c);
                        solu_cnode_ex ex = solu_cnode(c, node->n_binary.left->n_postfix.expr, obj);
                        if (!ex.is_ok) return ex;

                        uint32_t name_i;
                        sf_str *v = node->n_binary.left->n_postfix.postfix.dyn; (void)v;
                        if (!solu_kfind(c, node->n_binary.left->n_postfix.postfix, &name_i))
                            name_i = solu_kadd(c, node->n_binary.left->n_postfix.postfix);

                        if (node->n_binary.op != TK_EQUAL) {
                            ot = solu_rtemp(c);
                            solu_cemit(c, solu_ins_abc(SOLU_OP_GET, ot, solu_reg(obj), solu_const(name_i)));
                            solu_cemit(c, solu_ins_abc(node->n_binary.op == TK_PLUS_EQUAL ? SOLU_OP_ADD : SOLU_OP_SUB, ot, solu_reg(ot), rl ? solu_const(right) : solu_reg(right)));
                        }
                        solu_cemit(c, solu_ins_abc(SOLU_OP_SET, obj, solu_const(name_i), ot == UINT32_MAX ?
                                (rl ? solu_const(right) : solu_reg(right)) : solu_reg(ot)));
                        solu_ctemps(c, 1);
                    } else return solu_cerr(SOLU_ERRC_INVALID_ASSIGN);

                    if (ot != UINT32_MAX) solu_ctemps(c, 1);
                    if (t_reg != UINT32_MAX)
                        return solu_cerr(SOLU_ERRC_EXPECTED_EXPRESSION);
                    return solu_cnode_ex_ok();
                }

                case TK_PLUS: solu_cemit(c, solu_ins_abc(SOLU_OP_ADD, t_reg, ll ? solu_const(left) : solu_reg(left),
                    rl ? solu_const(right) : solu_reg(right))); break;
                case TK_MINUS: solu_cemit(c, solu_ins_abc(SOLU_OP_SUB, t_reg, ll ? solu_const(left) : solu_reg(left),
                    rl ? solu_const(right) : solu_reg(right))); break;
                case TK_ASTERISK: solu_cemit(c, solu_ins_abc(SOLU_OP_MUL, t_reg, ll ? solu_const(left) : solu_reg(left),
                    rl ? solu_const(right) : solu_reg(right))); break;
                case TK_SLASH: solu_cemit(c, solu_ins_abc(SOLU_OP_DIV, t_reg, ll ? solu_const(left) : solu_reg(left),
                    rl ? solu_const(right) : solu_reg(right))); break;

                case TK_DOUBLE_EQUAL: solu_cemit(c, solu_ins_abc(SOLU_OP_EQ, 0, ll ? solu_const(left) : solu_reg(left),
                    rl ? solu_const(right) : solu_reg(right))); break;
                case TK_LESS: solu_cemit(c, solu_ins_abc(SOLU_OP_LT, 0, ll ? solu_const(left) : solu_reg(left),
                    rl ? solu_const(right) : solu_reg(right))); break;
                case TK_LESS_EQUAL: solu_cemit(c, solu_ins_abc(SOLU_OP_LE, 0, ll ? solu_const(left) : solu_reg(left),
                    rl ? solu_const(right) : solu_reg(right))); break;

                case TK_NOT_EQUAL: solu_cemit(c, solu_ins_abc(SOLU_OP_EQ, 1, ll ? solu_const(left) : solu_reg(left),
                    rl ? solu_const(right) : solu_reg(right))); break;
                case TK_GREATER: solu_cemit(c, solu_ins_abc(SOLU_OP_LT, 1, ll ? solu_const(left) : solu_reg(left),
                    rl ? solu_const(right) : solu_reg(right))); break;
                case TK_GREATER_EQUAL: solu_cemit(c, solu_ins_abc(SOLU_OP_LE, 1, ll ? solu_const(left) : solu_reg(left),
                    rl ? solu_const(right) : solu_reg(right))); break;

                default:
                    return solu_cerr(SOLU_ERRC_UNKNOWN_OPERATION);
            }

            if (!ll) solu_ctemps(c, 1);
            if (!rl) solu_ctemps(c, 1);
            return solu_cnode_ex_ok();
        }
        case SOLU_ND_MEMBER: {
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

            uint32_t name_i;
            if (!solu_kfind(c, node->n_postfix.postfix, &name_i))
                name_i = solu_kadd(c, node->n_postfix.postfix);

            solu_cemit(c, solu_ins_abc(SOLU_OP_GET, t_reg, solu_reg(lhs), solu_const(name_i)));
            if (rl) solu_ctemps(c, 1);

            return solu_cnode_ex_ok();
        }
        case SOLU_ND_CALL: {
            uint32_t arg_rs = UINT32_MAX;  // Args temps start
            for (size_t i = 0; i < node->n_call.arg_c; ++i) {
                uint32_t r = solu_rtemp(c);
                solu_cnode_ex ex = solu_cnode(c, node->n_call.args[i], r);
                if (node->n_call.args[i]->tt == SOLU_ND_BINARY && solu_niscondition(node->n_call.args[i])) { // Conditions
                    solu_cemit(c, solu_ins_ab(SOLU_OP_LOAD, r, 0));
                    solu_cemit(c, solu_ins_ab(SOLU_OP_LOAD, r, 1));
                }

                if (!ex.is_ok) return ex;
                if (arg_rs == UINT32_MAX) arg_rs = r;
            }

            uint32_t f_reg = solu_rtemp(c);
            solu_cnode_ex lex = solu_cnode(c, node->n_call.identifier, f_reg);
            if (!lex.is_ok) return lex;
            solu_cemit(c, solu_ins_abc(SOLU_OP_CALL, t_reg == UINT32_MAX ? solu_rtemp(c) : t_reg, solu_reg(f_reg), solu_reg(arg_rs == UINT32_MAX ? 0 : arg_rs)));

            solu_ctemps(c, t_reg == UINT32_MAX ? node->n_call.arg_c + 2 : node->n_call.arg_c + 1);
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
            solu_cemit(c, solu_ins_a(SOLU_OP_NEW, t_reg));
            uint32_t nt = solu_rtemp(c), it = solu_rtemp(c);
            uint32_t obj_r = c->obj_r;
            c->obj_r = t_reg;
            for (uint32_t i = 0; i < node->n_obj.mem_c; ++i) {
                solu_node *nd = node->n_obj.members[i];
                uint32_t name_i;
                if (!solu_kfind(c, nd->n_binary.left->n_identifier, &name_i))
                    name_i = solu_kadd(c, nd->n_binary.left->n_identifier);
                solu_cnode_ex right = solu_cnode(c, nd->n_binary.right, it);
                if (!right.is_ok) return right;
                solu_cemit(c, solu_ins_ab(SOLU_OP_LOAD, nt, name_i));
                solu_cemit(c, solu_ins_abc(SOLU_OP_SET, t_reg, solu_reg(nt), solu_reg(it)));
            }
            c->obj_r = obj_r;
            solu_ctemps(c, 2);
            return solu_cnode_ex_ok();
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
            // Shared upvals
            solu_upvalue *upvals = malloc((c->proto.up_c + node->n_fun.cap_c) * sizeof(solu_upvalue));
            memcpy(upvals, c->proto.upvals, c->proto.up_c * sizeof(solu_upvalue));

            for (uint32_t i = 0; i < node->n_fun.cap_c; ++i) {
                solu_val *cap = node->n_fun.captures + i;
                char *name = cap->dyn;
                // Self capture (reserved name)
                solu_local loc;
                if (c->obj_r != UINT_MAX && strcmp(name, "self") == 0)
                    upvals[c->proto.up_c + i] = (solu_upvalue){sf_str_cdup(name), .tt = SOLU_UP_REF, .ref = c->obj_r, .frame = c->frame};
                else {
                    if (!solu_lexists(c, name, &loc))
                        return solu_cerr(SOLU_ERRC_UNKNOWN_LOCAL);
                    if (loc.upval)
                        upvals[c->proto.up_c + i] = (solu_upvalue){sf_str_cdup(name), .tt = SOLU_UP_REF, .ref = loc.reg, .frame = loc.frame};
                    else {
                        solu_cemit(c, solu_ins_a(SOLU_OP_REFU, loc.reg));
                        upvals[c->proto.up_c + i] = (solu_upvalue){sf_str_cdup(name), .tt = SOLU_UP_REF, .ref = loc.reg, .frame = c->frame};
                    }
                }
            }

            solu_compile_ex ex = solu_cfun(
                c->frame + 1,
                c->alloc,
                node->n_fun.block,
                node->n_fun.arg_c, node->n_fun.args,
                c->proto.up_c + node->n_fun.cap_c, upvals
            );
            free(upvals);

            if (!ex.is_ok) return solu_cnode_ex_err(ex.err);
            if (r_asm != 0) ex.ok.reg_c += r_asm;
            solu_dyn p = calloc(1, sizeof(solu_dalloc) + sizeof(solu_fproto));
            solu_dalloc *dh = p, *dd = c->alloc;
            *dh = (solu_dalloc){
                .next = NULL,
                .size = sizeof(solu_fproto),
                .tt = SOLU_DFUN,
                .mark = SOLU_DYN_GREEN,
            };
            if (dd == NULL) c->alloc = dh;
            else {
                while (dd->next) dd = dd->next;
                dd->next = dh;
            }
            solu_val fun = (solu_val){ .tt = SOLU_TDYN, .dyn = (char *)p + sizeof(solu_dalloc) };
            *(solu_fproto *)fun.dyn = ex.ok;

            solu_kadd(c, fun);
            solu_cemit(c, solu_ins_ab(SOLU_OP_LOAD, t_reg, c->proto.constants.count - 1));
            return solu_cnode_ex_ok();
        }
        default: return solu_cerr(SOLU_ERRC_UNKNOWN);
    }
}

solu_compile_ex solu_cproto(char *src, uint32_t arg_c, solu_val *args, uint32_t up_c, solu_upvalue *upvals) {
    solu_scan_ex scan_ex = solu_scan(sf_ref(src));
    if (!scan_ex.is_ok)
        return solu_compile_ex_err((solu_compile_err){
            .tt = scan_ex.err.tt,
            .line = scan_ex.err.line,
            .column = scan_ex.err.column,
        });
    solu_parse_ex par_ex = solu_parse(&scan_ex.ok.tv);
    if (!par_ex.is_ok)
        return solu_compile_ex_err((solu_compile_err){
            .tt = par_ex.err.tt,
            .line = par_ex.err.line,
            .column = par_ex.err.column,
        });

    solu_compile_ex ex = solu_cfun(0, scan_ex.ok.alloc, par_ex.ok, arg_c, args, up_c, upvals);
    solu_node_free(par_ex.ok);

    for (solu_dalloc *ac = scan_ex.ok.alloc; ac; ) {
        solu_dalloc *next = ac->next;
        free(ac);
        ac = next;
    }
    return ex;
}
