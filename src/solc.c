#include "sol/solc.h"
#include "sol/bytecode.h"
#include "sol/syntax.h"
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
} sol_local;

struct sol_scope;

#define MAP_NAME sol_scope
#define MAP_K sf_str
#define MAP_V sol_local
#define EQUAL_FN sf_str_eq
#define HASH_FN sf_str_hash
#include <sf/containers/map.h>

struct sol_scopes;
void _sol_scopes_cleanup(struct sol_scopes *);
#define VEC_NAME sol_scopes
#define VEC_T sol_scope
#define VSIZE_T uint32_t
#define CLEANUP_FN _sol_scopes_cleanup
#include <sf/containers/vec.h>
void _sol_scopes_cleanup(struct sol_scopes *v) {
    for (size_t i = 0; i < v->count; ++i)
        sol_scope_free(v->data + i);
}

/// Temporary compilation info that's shared between all compiler functions
typedef struct {
    sol_fproto proto;
    sol_ast ast;
    sol_scopes scopes;
    uint32_t locals, max_locals, temps, max_temps, frame;
    sol_dalloc *alloc;
} sol_compiler;

#define OP_W 10
/// Add an instruction to the proto.
/// Optionally logs every instruction compiled (see SOL_DBG_LOG)
static inline void sol_cemitraw(sol_compiler *c, sol_instruction ins, uint16_t line, uint16_t column) {
    c->proto.code = realloc(c->proto.code, ++c->proto.code_c * sizeof(sol_instruction));
    c->proto.code[c->proto.code_c - 1] = ins;
    c->proto.dbg = realloc(c->proto.dbg, c->proto.code_c * sizeof(sol_dbg));
    c->proto.dbg[c->proto.code_c - 1] = SOL_DBG_ENCODE(line, column);
}
#define sol_cemit(c, ins) sol_cemitraw(c, ins, node->line, node->column)

/// Reserve local variable register
static inline uint32_t sol_rlocal(sol_compiler *c) {
    ++c->locals;
    c->max_locals = c->locals > c->max_locals ? c->locals : c->max_locals;
    return c->locals - 1;
}
/// Clear local variable register(s)
static inline void sol_clocals(sol_compiler *c, uint32_t count) {
    c->locals -= count;
}
/// Find whether a local exists, and output the local if it does
static inline bool sol_lexists(sol_compiler *c, sf_str name, sol_local *loc) {
    for (sol_scope *s = c->scopes.data + c->scopes.count - 1; s != c->scopes.data - 1; --s) {
        sol_scope_ex sc_ex = sol_scope_get(s, name);
        if (sc_ex.is_ok) {
            *loc = sc_ex.ok;
            return true;
        }
    }
    return false;
}
/// Reserve temporary register
static inline uint32_t sol_rtemp(sol_compiler *c) {
    ++c->temps;
    c->max_temps = c->temps > c->max_temps ? c->temps : c->max_temps;
    return c->locals + c->temps - 1;
}
/// Clear temporary register(s)
static inline void sol_ctemps(sol_compiler *c, uint32_t count) {
    c->temps -= count;
}
/// Find whether a constant exists, and output the index if it does
bool sol_kfind(sol_compiler *c, sol_val con, uint32_t *idx) {
    for (uint32_t i = 0; i < c->proto.constants.count; ++i) {
        sol_val v = c->proto.constants.data[i];
        if (v.tt != con.tt) continue;
        switch (v.tt) {
            case SOL_TNIL: *idx = i; return true;
            case SOL_TF64: if (v.f64 == con.f64) { *idx = i; return true; } else continue;
            case SOL_TI64: if (v.i64 == con.i64) { *idx = i; return true; } else continue;
            case SOL_TDYN: {
                if (sol_isdtype(v, SOL_DSTR) && sol_isdtype(con, SOL_DSTR))
                    if (sf_str_eq(*(sf_str *)v.dyn, *(sf_str *)con.dyn)) { *idx = i; return true; }
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
static void sol_kadd(sol_compiler *c, sol_val con) {
    if (con.tt == SOL_TDYN) {
        size_t size = sizeof(sol_dalloc) + sol_dheader(con)->size;
        sol_dalloc *ac = malloc(size);
        memcpy(ac, (char *)con.dyn - sizeof(sol_dalloc), size);
        con = (sol_val){SOL_TDYN, .dyn=ac + 1};
        sol_dheader(con)->mark = SOL_DYN_GREEN;
        if (sol_dheader(con)->tt == SOL_DSTR)
            *(sf_str *)con.dyn = sf_str_dup(*(sf_str *)con.dyn);
    }
    sol_valvec_push(&c->proto.constants, con);
}

/// Shorthand macro for returning a sol_cnode_ex_err
#define sol_cerr(type) sol_cnode_ex_err((sol_compile_err){(type), node->line, node->column})


#define EXPECTED_NAME sol_cnode_ex
#define EXPECTED_E sol_compile_err
#include <sf/containers/expected.h>
sol_cnode_ex sol_cnode(sol_compiler *c, sol_node *node, uint32_t t_reg);

/// Compile a fun from a block and info
sol_compile_ex sol_cfun(sol_dalloc *alloc, sol_node *ast, uint32_t arg_c, sol_val *args, uint32_t up_c, sol_upvalue *upvals) {
    sol_compiler c = {
        .proto = sol_fproto_new(),
        .ast = ast,
        .scopes = sol_scopes_new(),
        .locals = arg_c,
        .max_locals = arg_c,
        .temps = 0, .max_temps = 0,
        .alloc = alloc,
    };
    c.proto.arg_c = arg_c;
    sol_scopes_push(&c.scopes, sol_scope_new());
    for (uint32_t i = 0; i < arg_c; ++i)
        sol_scope_set(c.scopes.data + c.scopes.count - 1, *(sf_str *)args[i].dyn, (sol_local){i, 0, false, 0});
    for (uint32_t i = 0; i < up_c; ++i)
        sol_scope_set(c.scopes.data + c.scopes.count - 1, upvals[i].name, (sol_local){i, 0, true, upvals[i].frame});

    sol_kadd(&c, (sol_val){.tt = SOL_TBOOL, .boolean = false});
    sol_kadd(&c, (sol_val){.tt = SOL_TBOOL, .boolean = true});

    c.proto.upvals = malloc(sizeof(sol_upvalue) * up_c);
    memcpy(c.proto.upvals, upvals, sizeof(sol_upvalue) * up_c);
    c.proto.up_c = up_c;

    sol_cnode_ex e = sol_cnode(&c, c.ast, UINT32_MAX);
    c.proto.reg_c = c.max_locals + c.max_temps;

    sol_scopes_free(&c.scopes);
    return e.is_ok ? sol_compile_ex_ok(c.proto) : sol_compile_ex_err(e.err);
}

/// Compile a single node into bytecode.
/// Passing UINT32_MAX as t_reg acts as a discard
sol_cnode_ex sol_cnode(sol_compiler *c, sol_node *node, uint32_t t_reg) {
    switch (node->tt) {
        case SOL_ND_MEMBER: {
            uint32_t lhs = sol_rtemp(c);
            sol_cnode_ex lex = sol_cnode(c, node->n_postfix.expr, lhs);
            if (!lex.is_ok) return lex;

            uint32_t name_i;
            if (!sol_kfind(c, node->n_postfix.postfix, &name_i)) {
                sol_kadd(c, node->n_postfix.postfix);
                name_i = c->proto.constants.count - 1;
            }

            uint32_t name = sol_rtemp(c);
            sol_cemit(c, sol_ins_ab(SOL_OP_LOAD, name, name_i));
            sol_cemit(c, sol_ins_abc(SOL_OP_GET, t_reg, lhs, name));
            sol_ctemps(c, 2);

            return sol_cnode_ex_ok();
        }
        case SOL_ND_BLOCK: {
            sol_scopes_push(&c->scopes, sol_scope_new());
            for (size_t i = 0; i < node->n_block.count; ++i) {
                sol_cnode_ex ex = sol_cnode(c, node->n_block.stmts[i], UINT32_MAX);
                if (!ex.is_ok) return ex;
            }
            sol_scope s = sol_scopes_pop(&c->scopes);
            sol_clocals(c, (uint32_t)s.pair_count);
            sol_scope_free(&s);
            return sol_cnode_ex_ok();
        }

        case SOL_ND_IDENTIFIER: {
            if (t_reg == UINT32_MAX)
                return sol_cerr(SOL_ERRC_UNUSED_EVALUATION);

            sol_local loc;
            if (!sol_lexists(c, *(sf_str *)node->n_identifier.dyn, &loc)) { // Global
                uint32_t name_i;
                if (!sol_kfind(c, node->n_identifier, &name_i)) {
                    sol_kadd(c, node->n_identifier);
                    name_i = c->proto.constants.count - 1;
                }
                sol_cemit(c, sol_ins_abc(SOL_OP_GUPO, t_reg, 0, name_i)); // state->global
            } else // Local/Upval
                sol_cemit(c, sol_ins_ab(loc.upval ? SOL_OP_GETU : SOL_OP_MOVE, t_reg, loc.reg));

            return sol_cnode_ex_ok();
        }
        case SOL_ND_LITERAL: {
            if (t_reg == UINT32_MAX)
                return sol_cerr(SOL_ERRC_UNUSED_EVALUATION);

            uint32_t pos;
            if (!sol_kfind(c, node->n_literal, &pos)) {
                sol_kadd(c, node->n_literal);
                pos = c->proto.constants.count - 1;
            }
            sol_cemit(c, sol_ins_ab(SOL_OP_LOAD, t_reg, pos));
            return sol_cnode_ex_ok();
        }

        case SOL_ND_LET: {
            sol_scope_ex exists = sol_scope_get(c->scopes.data + c->scopes.count - 1, *(sf_str *)node->n_let.name.dyn);
            if (exists.is_ok)
                return sol_cerr(SOL_ERRC_REDEFINED_LOCAL);
            uint32_t rhs = sol_rlocal(c);
            sol_scope_set(c->scopes.data + c->scopes.count - 1, *(sf_str *)node->n_let.name.dyn, (sol_local){rhs, c->scopes.count - 1, false, 0});

            sol_cnode_ex rv_ex = sol_cnode(c, node->n_let.value, rhs);
            if (!rv_ex.is_ok) return rv_ex;

            if (node->n_let.value->tt == SOL_ND_BINARY && sol_niscondition(node->n_let.value)) { // Conditions
                sol_cemit(c, sol_ins_ab(SOL_OP_LOAD, rhs, 0));
                sol_cemit(c, sol_ins_ab(SOL_OP_LOAD, rhs, 1));
            }
            return sol_cnode_ex_ok();
        }

        case SOL_ND_BINARY: {
            uint32_t left = sol_rtemp(c), right = sol_rtemp(c);
            if (node->n_binary.op != TK_EQUAL) {
                sol_cnode_ex left_ex = sol_cnode(c, node->n_binary.left, left);
                if (!left_ex.is_ok)
                    return left_ex;
            }

            sol_cnode_ex right_ex = sol_cnode(c, node->n_binary.right, right);
            if (!right_ex.is_ok) return right_ex;

            switch (node->n_binary.op) {
                case TK_EQUAL: {
                    if (node->n_binary.left->tt == SOL_ND_IDENTIFIER) {
                        sol_local loc;
                        if (sol_lexists(c, *(sf_str *)node->n_binary.left->n_identifier.dyn, &loc)) // Local/Upval
                            sol_cemit(c, loc.upval ? sol_ins_ab(SOL_OP_SETU, loc.reg, right) : sol_ins_ab(SOL_OP_MOVE, loc.reg, right));
                        else { // Global
                            uint32_t name_i;
                            if (!sol_kfind(c, node->n_binary.left->n_identifier, &name_i)) {
                                sol_kadd(c, node->n_binary.left->n_identifier);
                                name_i = c->proto.constants.count - 1;
                            }
                            sol_cemit(c, sol_ins_abc(SOL_OP_SUPO, 0, name_i, right)); // state->global
                        }
                    } else if (node->n_binary.left->tt == SOL_ND_MEMBER) { // Member Assign
                        uint32_t obj = sol_rtemp(c), name = sol_rtemp(c);
                        sol_cnode_ex ex = sol_cnode(c, node->n_binary.left->n_postfix.expr, obj);
                        if (!ex.is_ok) return ex;

                        uint32_t name_i;
                        sf_str *v = node->n_binary.left->n_postfix.postfix.dyn; (void)v;
                        if (!sol_kfind(c, node->n_binary.left->n_postfix.postfix, &name_i)) {
                            sol_kadd(c, node->n_binary.left->n_postfix.postfix);
                            name_i = c->proto.constants.count - 1;
                        }

                        sol_cemit(c, sol_ins_ab(SOL_OP_LOAD, name, name_i));
                        sol_cemit(c, sol_ins_abc(SOL_OP_SET, obj, name, right));
                        sol_ctemps(c, 2);
                    } else return sol_cerr(SOL_ERRC_INVALID_ASSIGN);
                    sol_ctemps(c, 2);
                    return sol_cnode_ex_ok();
                }

                case TK_PLUS: sol_cemit(c, sol_ins_abc(SOL_OP_ADD, t_reg, left, right)); break;
                case TK_MINUS: sol_cemit(c, sol_ins_abc(SOL_OP_SUB, t_reg, left, right)); break;
                case TK_ASTERISK: sol_cemit(c, sol_ins_abc(SOL_OP_MUL, t_reg, left, right)); break;
                case TK_SLASH: sol_cemit(c, sol_ins_abc(SOL_OP_DIV, t_reg, left, right)); break;

                case TK_DOUBLE_EQUAL: sol_cemit(c, sol_ins_abc(SOL_OP_EQ, 0, left, right)); break;
                case TK_LESS: sol_cemit(c, sol_ins_abc(SOL_OP_LT, 0, left, right)); break;
                case TK_LESS_EQUAL: sol_cemit(c, sol_ins_abc(SOL_OP_LE, 0, left, right)); break;

                case TK_NOT_EQUAL: sol_cemit(c, sol_ins_abc(SOL_OP_EQ, 1, left, right)); break;
                case TK_GREATER: sol_cemit(c, sol_ins_abc(SOL_OP_LT, 1, left, right)); break;
                case TK_GREATER_EQUAL: sol_cemit(c, sol_ins_abc(SOL_OP_LE, 1, left, right)); break;

                default:
                    return sol_cerr(SOL_ERRC_UNKNOWN_OPERATION);
            }

            sol_ctemps(c, 2);
            return sol_cnode_ex_ok();
        }
        case SOL_ND_CALL: {
            uint32_t arg_rs = UINT32_MAX;  // Args temps start
            for (size_t i = 0; i < node->n_call.arg_c; ++i) {
                uint32_t r = sol_rtemp(c);
                sol_cnode_ex ex = sol_cnode(c, node->n_call.args[i], r);
                if (node->n_call.args[i]->tt == SOL_ND_BINARY && sol_niscondition(node->n_call.args[i])) { // Conditions
                    sol_cemit(c, sol_ins_ab(SOL_OP_LOAD, r, 0));
                    sol_cemit(c, sol_ins_ab(SOL_OP_LOAD, r, 1));
                }

                if (!ex.is_ok) return ex;
                if (arg_rs == UINT32_MAX) arg_rs = r;
            }

            uint32_t f_reg = sol_rtemp(c);
            sol_cnode_ex lex = sol_cnode(c, node->n_call.identifier, f_reg);
            if (!lex.is_ok) return lex;
            sol_cemit(c, sol_ins_abc(SOL_OP_CALL, t_reg == UINT32_MAX ? sol_rtemp(c) : t_reg, f_reg, arg_rs == UINT32_MAX ? 0 : arg_rs));

            sol_ctemps(c, t_reg == UINT32_MAX ? node->n_call.arg_c + 2 : node->n_call.arg_c + 1);
            return sol_cnode_ex_ok();
        }
        case SOL_ND_FUN:
        case SOL_ND_ASM: {
            uint32_t r_asm = 0;
            if (node->tt == SOL_ND_ASM) {
                r_asm = (uint32_t)node->n_asm.temps;
                node = node->n_asm.n_fun;
            }
            // Shared upvals
            sol_upvalue *upvals = malloc((c->proto.up_c + node->n_fun.cap_c) * sizeof(sol_upvalue));
            memcpy(upvals, c->proto.upvals, c->proto.up_c * sizeof(sol_upvalue));

            for (uint32_t i = 0; i < node->n_fun.cap_c; ++i) {
                sol_val *cap = node->n_fun.captures + i;
                sf_str name = *(sf_str *)cap->dyn;
                sol_local loc;
                if (!sol_lexists(c, name, &loc))
                    return sol_cerr(SOL_ERRC_UNKNOWN_LOCAL);
                if (loc.upval)
                    upvals[c->proto.up_c + i] = (sol_upvalue){sf_str_dup(name), SOL_UP_REF, .ref = loc.reg, .frame = loc.frame};
                else {
                    sol_cemit(c, sol_ins_a(SOL_OP_REFU, loc.reg));
                    upvals[c->proto.up_c + i] = (sol_upvalue){sf_str_dup(name), SOL_UP_REF, .ref = loc.reg, .frame = c->frame};
                }
            }

            ++c->frame;
            sol_compile_ex ex = sol_cfun(
                c->alloc,
                node->n_fun.block,
                node->n_fun.arg_c, node->n_fun.args,
                c->proto.up_c + node->n_fun.cap_c, upvals
            );
            free(upvals);
            --c->frame;

            if (!ex.is_ok) return sol_cnode_ex_err(ex.err);
            if (r_asm != 0) ex.ok.reg_c += r_asm;
            sol_dyn p = calloc(1, sizeof(sol_dalloc) + sizeof(sol_fproto));
            sol_dalloc *dh = p, *dd = c->alloc;
            *dh = (sol_dalloc){
                .next = NULL,
                .size = sizeof(sol_fproto),
                .tt = SOL_DFUN,
                .mark = SOL_DYN_GREEN,
            };
            if (dd == NULL) c->alloc = dh;
            else {
                while (dd->next) dd = dd->next;
                dd->next = dh;
            }
            sol_val fun = (sol_val){ .tt = SOL_TDYN, .dyn = (char *)p + sizeof(sol_dalloc) };
            *(sol_fproto *)fun.dyn = ex.ok;

            sol_kadd(c, fun);
            sol_cemit(c, sol_ins_ab(SOL_OP_LOAD, t_reg, c->proto.constants.count - 1));
            return sol_cnode_ex_ok();
        }
        case SOL_ND_INS: {
            sol_i64 opa[3];
            for (int i = 0; i < 3; ++i) {
                sol_val v = node->n_ins.opa[i];
                if ((node->n_ins.op == SOL_OP_LOAD && i == 1) ||
                    (node->n_ins.op == SOL_OP_SUPO && i == 1) ||
                    (node->n_ins.op == SOL_OP_GUPO && i == 2)) {
                    uint32_t pos;
                    if (!sol_kfind(c, node->n_literal, &pos)) {
                        sol_kadd(c, v);
                        pos = c->proto.constants.count - 1;
                    }
                    opa[i] = pos;
                    continue;
                }
                if (v.tt == SOL_TDYN) { // Arg/Upval
                    sol_local loc;
                    if (!sol_lexists(c, *(sf_str *)v.dyn, &loc))
                        return sol_cerr(SOL_ERRC_UNKNOWN_LOCAL);
                    opa[i] = loc.reg;
                    continue;
                }
                opa[i] = v.i64;
            }

            switch (sol_op_info(node->n_ins.op)->type) {
                case SOL_INS_A:   sol_cemit(c, sol_ins_a(node->n_ins.op, opa[0])); break;
                case SOL_INS_AB:  sol_cemit(c, sol_ins_ab((uint32_t)node->n_ins.op, (uint32_t)opa[0], (uint32_t)opa[1])); break;
                case SOL_INS_ABC: sol_cemit(c, sol_ins_abc((uint32_t)node->n_ins.op, (uint32_t)opa[0], (uint32_t)opa[1], (uint32_t)opa[2])); break;
            }
            return sol_cnode_ex_ok();
        }

        case SOL_ND_IF: {
            uint32_t cr = sol_rtemp(c);
            if (node->n_if.condition->tt == SOL_ND_IDENTIFIER) {
                sol_local loc;
                if (!sol_lexists(c, *(sf_str *)node->n_if.condition->n_identifier.dyn, &loc)) { // Global
                    uint32_t name_i;
                    if (!sol_kfind(c, node->n_if.condition->n_identifier, &name_i)) {
                        sol_kadd(c, node->n_if.condition->n_identifier);
                        name_i = c->proto.constants.count - 1;
                    }

                    uint32_t id_r = sol_rtemp(c);
                    sol_cemit(c, sol_ins_abc(SOL_OP_GUPO, id_r, 0, name_i));
                    sol_cemit(c, sol_ins_abc(SOL_OP_EQ, 0, id_r, cr));
                    sol_ctemps(c, 1);
                } else { // Local
                    sol_cemit(c, sol_ins_ab(SOL_OP_LOAD, cr, 1));
                    if (loc.upval) { // Reserve temp for upval
                        uint32_t up = sol_rtemp(c);
                        sol_cemit(c, sol_ins_ab(SOL_OP_GETU, up, loc.reg));
                        sol_cemit(c, sol_ins_abc(SOL_OP_EQ, 0, up, cr));
                        sol_ctemps(c, 1);
                    } else
                        sol_cemit(c, sol_ins_abc(SOL_OP_EQ, 0, loc.reg, cr));
                }
            } else {
                sol_cnode_ex ex = sol_cnode(c, node->n_if.condition, cr);
                if (node->n_if.condition->tt == SOL_ND_CALL) {
                    uint32_t ttemp = sol_rtemp(c);
                    sol_cemit(c, sol_ins_ab(SOL_OP_LOAD, ttemp, 1));
                    sol_cemit(c, sol_ins_abc(SOL_OP_EQ, 0, cr, ttemp));
                    sol_ctemps(c, 1);
                }
                if (!ex.is_ok) return ex;
            }

            uint32_t jmp_false = c->proto.code_c;
            sol_cemit(c, sol_ins_a(SOL_OP_JMP, 0));

            // Then
            sol_cnode_ex ex = sol_cnode(c, node->n_if.then_node, UINT32_MAX);
            if (!ex.is_ok) return ex;
            uint32_t ofs = c->proto.code_c - (jmp_false + 1);

            if (node->n_if.else_node) {
                uint32_t jmp_end = c->proto.code_c;
                sol_cemit(c, sol_ins_a(SOL_OP_JMP, 0));
                ex = sol_cnode(c, node->n_if.else_node, UINT32_MAX);
                if (!ex.is_ok) return ex;
                // Patch jump
                c->proto.code[jmp_false] = sol_ins_a(SOL_OP_JMP, ofs + 1);
                c->proto.code[jmp_end] = sol_ins_a(SOL_OP_JMP, c->proto.code_c - (jmp_end + 1));
            } else c->proto.code[jmp_false] = sol_ins_a(SOL_OP_JMP, ofs);

            sol_ctemps(c, 1);
            return sol_cnode_ex_ok();
        }
        case SOL_ND_WHILE: {
            uint32_t jmp_cond = c->proto.code_c - 1;
            uint32_t cr = sol_rtemp(c);
            if (node->n_while.condition->tt == SOL_ND_IDENTIFIER) {
                sol_local loc;
                if (!sol_lexists(c, *(sf_str *)node->n_while.condition->n_identifier.dyn, &loc)) { // Global
                    uint32_t name_i;
                    if (!sol_kfind(c, node->n_while.condition->n_identifier, &name_i)) {
                        sol_kadd(c, node->n_while.condition->n_identifier);
                        name_i = c->proto.constants.count - 1;
                    }

                    uint32_t id_r = sol_rtemp(c);
                    sol_cemit(c, sol_ins_abc(SOL_OP_GUPO, id_r, 0, name_i));
                    sol_cemit(c, sol_ins_abc(SOL_OP_EQ, 0, id_r, cr));
                    sol_ctemps(c, 1);
                } else { // Local
                    sol_cemit(c, sol_ins_ab(SOL_OP_LOAD, cr, 1));
                    if (loc.upval) { // Reserve temp for upval
                        uint32_t up = sol_rtemp(c);
                        sol_cemit(c, sol_ins_ab(SOL_OP_GETU, up, loc.reg));
                        sol_cemit(c, sol_ins_abc(SOL_OP_EQ, 0, up, cr));
                        sol_ctemps(c, 1);
                    } else
                        sol_cemit(c, sol_ins_abc(SOL_OP_EQ, 0, loc.reg, cr));
                }
            } else {
                sol_cnode_ex ex = sol_cnode(c, node->n_while.condition, cr);
                if (node->n_while.condition->tt == SOL_ND_CALL) {
                    uint32_t ttemp = sol_rtemp(c);
                    sol_cemit(c, sol_ins_ab(SOL_OP_LOAD, ttemp, 1));
                    sol_cemit(c, sol_ins_abc(SOL_OP_EQ, 0, cr, ttemp));
                    sol_ctemps(c, 1);
                }
                if (!ex.is_ok) return ex;
            }

            uint32_t jmp_break = c->proto.code_c;
            sol_cemit(c, sol_ins_a(SOL_OP_JMP, 0));

            // Do
            sol_cnode_ex ex = sol_cnode(c, node->n_while.block, UINT32_MAX);
            if (!ex.is_ok) return ex;
            c->proto.code[jmp_break] = sol_ins_a(SOL_OP_JMP, c->proto.code_c - jmp_break);
            sol_cemit(c, sol_ins_a(SOL_OP_JMP, jmp_cond - c->proto.code_c));

            sol_ctemps(c, 1);
            return sol_cnode_ex_ok();
        }
        case SOL_ND_RETURN: {
            uint32_t r = sol_rtemp(c);
            sol_cnode_ex ex = sol_cnode(c, node->n_return, r);
            if (!ex.is_ok) return ex;
            sol_cemit(c, sol_ins_a(SOL_OP_RET, r));
            return sol_cnode_ex_ok();
        }

        default: return sol_cerr(SOL_ERRC_UNKNOWN);
    }
}

sol_compile_ex sol_cproto(sf_str src, uint32_t arg_c, sol_val *args, uint32_t up_c, sol_upvalue *upvals) {
    sol_scan_ex scan_ex = sol_scan(src);
    if (!scan_ex.is_ok)
        return sol_compile_ex_err((sol_compile_err){
            .tt = scan_ex.err.tt,
            .line = scan_ex.err.line,
            .column = scan_ex.err.column,
        });
    sol_parse_ex par_ex = sol_parse(&scan_ex.ok.tv);
    if (!par_ex.is_ok)
        return sol_compile_ex_err((sol_compile_err){
            .tt = par_ex.err.tt,
            .line = par_ex.err.line,
            .column = par_ex.err.column,
        });

    sol_compile_ex ex = sol_cfun(scan_ex.ok.alloc, par_ex.ok, arg_c, args, up_c, upvals);
    sol_node_free(par_ex.ok);

    for (sol_dalloc *ac = scan_ex.ok.alloc; ac; ) {
        sol_dalloc *next = ac->next;
        if (ac->tt == SOL_DSTR) {
            sf_str *str = (sf_str *)((char *)ac + sizeof(sol_dalloc));
            sf_str_free(*str);
        }
        free(ac);
        ac = next;
    }
    return ex;
}
