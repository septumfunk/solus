#include "ctr/ctrc.h"
#include "ctr/bytecode.h"
#include "ctr/syntax.h"
#include "sf/str.h"
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define TUI_UL  "\x1b[4m"
#define TUI_BLD "\x1b[1m"
#define TUI_CLR "\x1b[0m"

/// A simple representation of a local variable (or upvalue)
typedef struct {
    uint32_t reg, scope;
    bool upval;
    uint32_t frame;
} ctr_local;

struct ctr_scope;
void _ctr_scope_fe(void *_, sf_str key, ctr_local _v) { (void)_v; sf_str_free(key); }
void _ctr_scope_cleanup(struct ctr_scope *);

#define MAP_NAME ctr_scope
#define MAP_K sf_str
#define MAP_V ctr_local
#define EQUAL_FN sf_str_eq
#define HASH_FN sf_str_hash
#define CLEANUP_FN _ctr_scope_cleanup
#include <sf/containers/map.h>
void _ctr_scope_cleanup(ctr_scope *map) { ctr_scope_foreach(map, _ctr_scope_fe, NULL); }

struct ctr_scopes;
void _ctr_scopes_cleanup(struct ctr_scopes *);
#define VEC_NAME ctr_scopes
#define VEC_T ctr_scope
#define CLEANUP_FN _ctr_scopes_cleanup
#include <sf/containers/vec.h>
void _ctr_scopes_cleanup(struct ctr_scopes *v) {
    for (size_t i = 0; i < v->count; ++i)
        ctr_scope_free(v->data + i);
}

/// Temporary compilation info that's shared between all compiler functions
typedef struct {
    ctr_fproto proto;
    ctr_ast ast;
    ctr_scopes scopes;
    uint32_t locals, max_locals, temps, max_temps, frame;
    bool echo;
} ctr_compiler;

#define OP_W 10
/// Add an instruction to the proto.
/// Optionally logs every instruction compiled (see CTR_DBG_LOG)
static inline void ctr_cemitraw(ctr_compiler *c, ctr_instruction ins, uint16_t line, uint16_t column) {
    if (c->echo) {
        const char *op = ctr_op_info(ctr_ins_op(ins))->mnemonic;
        switch (ctr_op_info(ctr_ins_op(ins))->type) {
            case CTR_INS_A: printf("[ %.2u:%-6.2u%-7s%-8d    ]\n", line, column, op, ctr_ia_a(ins)); break;
            case CTR_INS_AB: printf("[ %.2u:%-6.2u%-7s%-4u%-4u    ]\n", line, column,  op, ctr_iab_a(ins), ctr_iab_b(ins)); break;
            case CTR_INS_ABC: printf("[ %.2u:%-6.2u%-7s%-4u%-4u%-4u]\n", line, column,  op, ctr_iabc_a(ins), ctr_iabc_b(ins), ctr_iabc_c(ins)); break;
        }
    }
    c->proto.code = realloc(c->proto.code, ++c->proto.code_s * sizeof(ctr_instruction));
    c->proto.code[c->proto.code_s - 1] = ins;
    c->proto.dbg = realloc(c->proto.dbg, c->proto.code_s * sizeof(ctr_dbg));
    c->proto.dbg[c->proto.code_s - 1] = CTR_DBG_ENCODE(line, column);
}
#define ctr_cemit(c, ins) ctr_cemitraw(c, ins, node->line, node->column)

/// Reserve local variable register
static inline uint32_t ctr_rlocal(ctr_compiler *c) {
    ++c->locals;
    c->max_locals = c->locals > c->max_locals ? c->locals : c->max_locals;
    return c->locals - 1;
}
/// Clear local variable register(s)
static inline void ctr_clocals(ctr_compiler *c, uint32_t count) {
    c->locals -= count;
}
/// Find whether a local exists, and output the local if it does
static inline bool ctr_lexists(ctr_compiler *c, sf_str name, ctr_local *loc) {
    for (ctr_scope *s = c->scopes.data + c->scopes.count - 1; s != c->scopes.data - 1; --s) {
        ctr_scope_ex sc_ex = ctr_scope_get(s, name);
        if (sc_ex.is_ok) {
            *loc = sc_ex.ok;
            return true;
        }
    }
    return false;
}
/// Reserve temporary register
static inline uint32_t ctr_rtemp(ctr_compiler *c) {
    ++c->temps;
    c->max_temps = c->temps > c->max_temps ? c->temps : c->max_temps;
    return c->locals + c->temps - 1;
}
/// Clear temporary register(s)
static inline void ctr_ctemps(ctr_compiler *c, uint32_t count) {
    c->temps -= count;
}
/// Find whether a constant exists, and output the index if it does
bool ctr_kfind(ctr_compiler *c, ctr_val con, uint32_t *idx) {
    for (uint32_t i = 0; i < c->proto.constants.count; ++i) {
        ctr_val v = c->proto.constants.data[i];
        if (v.tt != con.tt) continue;
        switch (v.tt) {
            case CTR_TNIL: *idx = i; return true;
            case CTR_TF64: if (v.f64 == con.f64) { *idx = i; return true; } else continue;
            case CTR_TI64: if (v.i64 == con.i64) { *idx = i; return true; } else continue;
            case CTR_TDYN: {
                if (ctr_isdtype(v, CTR_DSTR) && ctr_isdtype(con, CTR_DSTR))
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
void ctr_kadd(ctr_compiler *c, ctr_val con) {
    if (con.tt == CTR_TDYN)
        ctr_header(con)->is_const = true;
    ctr_valvec_push(&c->proto.constants, con);
}

void ctr_clog(ctr_compiler *c, sf_str str) {
    if (c->echo) printf("%s\n", str.c_str);
}

/// Shorthand macro for returning a ctr_cnode_ex_err
#define ctr_cerr(type) ctr_cnode_ex_err((ctr_compile_err){(type), node->line, node->column})


#define EXPECTED_NAME ctr_cnode_ex
#define EXPECTED_E ctr_compile_err
#include <sf/containers/expected.h>
ctr_cnode_ex ctr_cnode(ctr_compiler *c, ctr_node *node, uint32_t t_reg);

/// Compile a fun from a block and info
ctr_compile_ex ctr_cfun(ctr_node *ast, uint32_t arg_c, ctr_val *args, uint32_t up_c, ctr_upvalue *upvals, bool echo) {
    ctr_compiler c = {
        .proto = ctr_fproto_new(),
        .ast = ast,
        .scopes = ctr_scopes_new(),
        .locals = arg_c,
        .temps = 0, .max_temps = 0,
        .echo = echo,
    };
    c.proto.arg_c = arg_c;
    ctr_scopes_push(&c.scopes, ctr_scope_new());
    for (uint32_t i = 0; i < arg_c; ++i)
        ctr_scope_set(c.scopes.data + c.scopes.count - 1, sf_str_dup(*(sf_str *)args[i].dyn), (ctr_local){i, 0, false, 0});
    for (uint32_t i = 0; i < up_c; ++i)
        ctr_scope_set(c.scopes.data + c.scopes.count - 1, sf_str_dup(upvals[i].name), (ctr_local){i, 0, true, upvals[i].frame});

    ctr_kadd(&c, (ctr_val){.tt = CTR_TBOOL, .boolean = false});
    ctr_kadd(&c, (ctr_val){.tt = CTR_TBOOL, .boolean = true});

    c.proto.upvals = malloc(sizeof(ctr_upvalue) * up_c);
    memcpy(c.proto.upvals, upvals, sizeof(ctr_upvalue) * up_c);
    c.proto.up_c = up_c;

    ctr_cnode_ex e = ctr_cnode(&c, c.ast, UINT32_MAX);
    c.proto.reg_c = c.max_locals + c.max_temps;

    ctr_scopes_free(&c.scopes);
    return e.is_ok ? ctr_compile_ex_ok(c.proto) : ctr_compile_ex_err(e.err);
}

/// Compile a single node into bytecode.
/// Passing UINT32_MAX as t_reg acts as a discard
ctr_cnode_ex ctr_cnode(ctr_compiler *c, ctr_node *node, uint32_t t_reg) {
    switch (node->tt) {
        case CTR_ND_MEMBER: {
            uint32_t lhs = ctr_rtemp(c);
            ctr_cnode_ex lex = ctr_cnode(c, node->n_postfix.expr, lhs);
            if (!lex.is_ok) return lex;

            uint32_t name_i;
            if (!ctr_kfind(c, node->n_postfix.postfix, &name_i)) {
                ctr_kadd(c, ctr_dref(node->n_postfix.postfix));
                name_i = c->proto.constants.count - 1;
            }

            uint32_t name = ctr_rtemp(c);
            ctr_cemit(c, ctr_ins_ab(CTR_OP_LOAD, name, name_i));
            ctr_cemit(c, ctr_ins_abc(CTR_OP_GET, t_reg, lhs, name));
            ctr_ctemps(c, 2);

            return ctr_cnode_ex_ok();
        }
        case CTR_ND_BLOCK: {
            ctr_scopes_push(&c->scopes, ctr_scope_new());
            for (size_t i = 0; i < node->n_block.count; ++i) {
                ctr_cnode_ex ex = ctr_cnode(c, node->n_block.stmts[i], UINT32_MAX);
                if (!ex.is_ok) return ex;
            }
            ctr_scope s = ctr_scopes_pop(&c->scopes);
            ctr_clocals(c, (uint32_t)s.pair_count);
            ctr_scope_free(&s);
            return ctr_cnode_ex_ok();
        }

        case CTR_ND_IDENTIFIER: {
            if (t_reg == UINT32_MAX)
                return ctr_cerr(CTR_ERRC_UNUSED_EVALUATION);

            ctr_local loc;
            if (!ctr_lexists(c, *(sf_str *)node->n_identifier.dyn, &loc)) { // Global
                uint32_t name_i;
                if (!ctr_kfind(c, node->n_identifier, &name_i)) {
                    ctr_kadd(c, ctr_dref(node->n_identifier));
                    name_i = c->proto.constants.count - 1;
                }
                ctr_cemit(c, ctr_ins_abc(CTR_OP_GUPO, t_reg, 0, name_i)); // state->global
            } else // Local/Upval
                ctr_cemit(c, ctr_ins_ab(loc.upval ? CTR_OP_GETU : CTR_OP_MOVE, t_reg, loc.reg));

            return ctr_cnode_ex_ok();
        }
        case CTR_ND_LITERAL: {
            if (t_reg == UINT32_MAX)
                return ctr_cerr(CTR_ERRC_UNUSED_EVALUATION);

            uint32_t pos;
            if (!ctr_kfind(c, node->n_literal, &pos)) {
                ctr_kadd(c, ctr_dref(node->n_literal));
                pos = c->proto.constants.count - 1;
            }
            ctr_cemit(c, ctr_ins_ab(CTR_OP_LOAD, t_reg, pos));
            return ctr_cnode_ex_ok();
        }

        case CTR_ND_LET: {
            ctr_scope_ex exists = ctr_scope_get(c->scopes.data + c->scopes.count - 1, *(sf_str *)node->n_let.name.dyn);
            if (exists.is_ok)
                return ctr_cerr(CTR_ERRC_REDEFINED_LOCAL);
            uint32_t rhs = ctr_rlocal(c);
            ctr_scope_set(c->scopes.data + c->scopes.count - 1, sf_str_dup(*(sf_str *)node->n_let.name.dyn), (ctr_local){rhs, c->scopes.count - 1, false, 0});

            ctr_cnode_ex rv_ex = ctr_cnode(c, node->n_let.value, rhs);
            if (!rv_ex.is_ok) return rv_ex;

            if (node->n_let.value->tt == CTR_ND_BINARY && ctr_niscondition(node->n_let.value)) { // Conditions
                ctr_cemit(c, ctr_ins_ab(CTR_OP_LOAD, rhs, 0));
                ctr_cemit(c, ctr_ins_ab(CTR_OP_LOAD, rhs, 1));
            }
            return ctr_cnode_ex_ok();
        }

        case CTR_ND_BINARY: {
            uint32_t left = ctr_rtemp(c), right = ctr_rtemp(c);
            if (node->n_binary.op != TK_EQUAL) {
                ctr_cnode_ex left_ex = ctr_cnode(c, node->n_binary.left, left);
                if (!left_ex.is_ok)
                    return left_ex;
            }

            ctr_cnode_ex right_ex = ctr_cnode(c, node->n_binary.right, right);
            if (!right_ex.is_ok) return right_ex;

            switch (node->n_binary.op) {
                case TK_EQUAL: {
                    if (node->n_binary.left->tt == CTR_ND_IDENTIFIER) {
                        ctr_local loc;
                        if (ctr_lexists(c, *(sf_str *)node->n_binary.left->n_identifier.dyn, &loc)) // Local/Upval
                            ctr_cemit(c, loc.upval ? ctr_ins_ab(CTR_OP_SETU, loc.reg, right) : ctr_ins_ab(CTR_OP_MOVE, loc.reg, right));
                        else { // Global
                            uint32_t name_i;
                            if (!ctr_kfind(c, node->n_binary.left->n_identifier, &name_i)) {
                                ctr_kadd(c, ctr_dref(node->n_binary.left->n_identifier));
                                name_i = c->proto.constants.count - 1;
                            }
                            ctr_cemit(c, ctr_ins_abc(CTR_OP_SUPO, 0, name_i, right)); // state->global
                        }
                    } else if (node->n_binary.left->tt == CTR_ND_MEMBER) { // Member Assign
                        uint32_t obj = ctr_rtemp(c), name = ctr_rtemp(c);
                        ctr_cnode_ex ex = ctr_cnode(c, node->n_binary.left->n_postfix.expr, obj);
                        if (!ex.is_ok) return ex;

                        uint32_t name_i;
                        sf_str *v = node->n_binary.left->n_postfix.postfix.dyn; (void)v;
                        if (!ctr_kfind(c, node->n_binary.left->n_postfix.postfix, &name_i)) {
                            ctr_kadd(c, ctr_dref(node->n_binary.left->n_postfix.postfix));
                            name_i = c->proto.constants.count - 1;
                        }

                        ctr_cemit(c, ctr_ins_ab(CTR_OP_LOAD, name, name_i));
                        ctr_cemit(c, ctr_ins_abc(CTR_OP_SET, obj, name, right));
                        ctr_ctemps(c, 2);
                    } else return ctr_cerr(CTR_ERRC_INVALID_ASSIGN);
                    ctr_ctemps(c, 2);
                    return ctr_cnode_ex_ok();
                }

                case TK_PLUS: ctr_cemit(c, ctr_ins_abc(CTR_OP_ADD, t_reg, left, right)); break;
                case TK_MINUS: ctr_cemit(c, ctr_ins_abc(CTR_OP_SUB, t_reg, left, right)); break;
                case TK_ASTERISK: ctr_cemit(c, ctr_ins_abc(CTR_OP_MUL, t_reg, left, right)); break;
                case TK_SLASH: ctr_cemit(c, ctr_ins_abc(CTR_OP_DIV, t_reg, left, right)); break;

                case TK_DOUBLE_EQUAL: ctr_cemit(c, ctr_ins_abc(CTR_OP_EQ, 0, left, right)); break;
                case TK_LESS: ctr_cemit(c, ctr_ins_abc(CTR_OP_LT, 0, left, right)); break;
                case TK_LESS_EQUAL: ctr_cemit(c, ctr_ins_abc(CTR_OP_LE, 0, left, right)); break;

                case TK_NOT_EQUAL: ctr_cemit(c, ctr_ins_abc(CTR_OP_EQ, 1, left, right)); break;
                case TK_GREATER: ctr_cemit(c, ctr_ins_abc(CTR_OP_LT, 1, left, right)); break;
                case TK_GREATER_EQUAL: ctr_cemit(c, ctr_ins_abc(CTR_OP_LE, 1, left, right)); break;

                default:
                    return ctr_cerr(CTR_ERRC_UNKNOWN_OPERATION);
            }

            ctr_ctemps(c, 2);
            return ctr_cnode_ex_ok();
        }
        case CTR_ND_CALL: {
            uint32_t arg_rs = UINT32_MAX;  // Args temps start
            for (size_t i = 0; i < node->n_call.arg_c; ++i) {
                uint32_t r = ctr_rtemp(c);
                ctr_cnode_ex ex = ctr_cnode(c, node->n_call.args[i], r);
                if (node->n_call.args[i]->tt == CTR_ND_BINARY && ctr_niscondition(node->n_call.args[i])) { // Conditions
                    ctr_cemit(c, ctr_ins_ab(CTR_OP_LOAD, r, 0));
                    ctr_cemit(c, ctr_ins_ab(CTR_OP_LOAD, r, 1));
                }

                if (!ex.is_ok) return ex;
                if (arg_rs == UINT32_MAX) arg_rs = r;
            }

            uint32_t f_reg = ctr_rtemp(c);
            ctr_cnode_ex lex = ctr_cnode(c, node->n_call.identifier, f_reg);
            if (!lex.is_ok) return lex;
            ctr_cemit(c, ctr_ins_abc(CTR_OP_CALL, t_reg == UINT32_MAX ? ctr_rtemp(c) : t_reg, f_reg, arg_rs == UINT32_MAX ? 0 : arg_rs));

            ctr_ctemps(c, t_reg == UINT32_MAX ? node->n_call.arg_c + 2 : node->n_call.arg_c + 1);
            return ctr_cnode_ex_ok();
        }
        case CTR_ND_FUN:
        case CTR_ND_ASM: {
            uint32_t r_asm = 0;
            if (node->tt == CTR_ND_ASM) {
                r_asm = (uint32_t)node->n_asm.temps;
                node = node->n_asm.n_fun;
            }
            // Shared upvals
            ctr_upvalue *upvals = malloc((c->proto.up_c + node->n_fun.cap_c) * sizeof(ctr_upvalue));
            memcpy(upvals, c->proto.upvals, c->proto.up_c * sizeof(ctr_upvalue));

            for (uint32_t i = 0; i < node->n_fun.cap_c; ++i) {
                ctr_val *cap = node->n_fun.captures + i;
                sf_str name = *(sf_str *)cap->dyn;
                ctr_local loc;
                if (!ctr_lexists(c, name, &loc))
                    return ctr_cerr(CTR_ERRC_UNKNOWN_LOCAL);
                if (loc.upval)
                    upvals[c->proto.up_c + i] = (ctr_upvalue){name, CTR_UP_REF, .ref = loc.reg, .frame = loc.frame};
                else {
                    ctr_cemit(c, ctr_ins_a(CTR_OP_REFU, loc.reg));
                    upvals[c->proto.up_c + i] = (ctr_upvalue){name, CTR_UP_REF, .ref = loc.reg, .frame = c->frame};
                }
            }

            ++c->frame;
            ctr_clog(c, sf_lit(TUI_BLD "[=========  BGN FUN  =========]" TUI_CLR));
            ctr_compile_ex ex = ctr_cfun(
                node->n_fun.block,
                node->n_fun.arg_c, node->n_fun.args,
                c->proto.up_c + node->n_fun.cap_c, upvals,
                c->echo
            );
            ctr_clog(c, sf_lit(TUI_BLD "[=========  END FUN  =========]" TUI_CLR));
            free(upvals);
            --c->frame;

            if (!ex.is_ok) return ctr_cnode_ex_err(ex.err);
            if (r_asm != 0) ex.ok.reg_c += r_asm;
            ctr_val fun = ctr_dnew(CTR_DFUN);
            *(ctr_fproto *)fun.dyn = ex.ok;

            ctr_kadd(c, fun);
            ctr_cemit(c, ctr_ins_ab(CTR_OP_LOAD, t_reg, c->proto.constants.count - 1));
            return ctr_cnode_ex_ok();
        }
        case CTR_ND_INS: {
            ctr_i64 opa[3];
            for (int i = 0; i < 3; ++i) {
                ctr_val v = node->n_ins.opa[i];
                if ((node->n_ins.op == CTR_OP_LOAD && i == 1) ||
                    (node->n_ins.op == CTR_OP_SUPO && i == 1) ||
                    (node->n_ins.op == CTR_OP_GUPO && i == 2)) {
                    uint32_t pos;
                    if (!ctr_kfind(c, node->n_literal, &pos)) {
                        ctr_kadd(c, ctr_dref(v));
                        pos = c->proto.constants.count - 1;
                    }
                    opa[i] = pos;
                    continue;
                }
                if (v.tt == CTR_TDYN) { // Arg/Upval
                    ctr_local loc;
                    if (!ctr_lexists(c, *(sf_str *)v.dyn, &loc))
                        return ctr_cerr(CTR_ERRC_UNKNOWN_LOCAL);
                    opa[i] = loc.reg;
                    continue;
                }
                opa[i] = v.i64;
            }

            switch (ctr_op_info(node->n_ins.op)->type) {
                case CTR_INS_A:   ctr_cemit(c, ctr_ins_a(node->n_ins.op, opa[0])); break;
                case CTR_INS_AB:  ctr_cemit(c, ctr_ins_ab(node->n_ins.op, (uint32_t)opa[0], (uint32_t)opa[1])); break;
                case CTR_INS_ABC: ctr_cemit(c, ctr_ins_abc(node->n_ins.op, (uint32_t)opa[0], (uint32_t)opa[1], (uint32_t)opa[2])); break;
            }
            return ctr_cnode_ex_ok();
        }

        case CTR_ND_IF: {
            uint32_t cr = ctr_rtemp(c);
            if (node->n_if.condition->tt == CTR_ND_IDENTIFIER) {
                ctr_local loc;
                if (!ctr_lexists(c, *(sf_str *)node->n_if.condition->n_identifier.dyn, &loc)) { // Global
                    uint32_t name_i;
                    if (!ctr_kfind(c, node->n_if.condition->n_identifier, &name_i)) {
                        ctr_kadd(c, ctr_dref(node->n_if.condition->n_identifier));
                        name_i = c->proto.constants.count - 1;
                    }

                    uint32_t id_r = ctr_rtemp(c);
                    ctr_cemit(c, ctr_ins_abc(CTR_OP_GUPO, id_r, 0, name_i));
                    ctr_cemit(c, ctr_ins_abc(CTR_OP_EQ, 0, id_r, cr));
                    ctr_ctemps(c, 1);
                } else { // Local
                    ctr_cemit(c, ctr_ins_ab(CTR_OP_LOAD, cr, 1));
                    if (loc.upval) { // Reserve temp for upval
                        uint32_t up = ctr_rtemp(c);
                        ctr_cemit(c, ctr_ins_ab(CTR_OP_GETU, up, loc.reg));
                        ctr_cemit(c, ctr_ins_abc(CTR_OP_EQ, 0, up, cr));
                        ctr_ctemps(c, 1);
                    } else
                        ctr_cemit(c, ctr_ins_abc(CTR_OP_EQ, 0, loc.reg, cr));
                }
            } else {
                ctr_cnode_ex ex = ctr_cnode(c, node->n_if.condition, cr);
                if (node->n_if.condition->tt == CTR_ND_CALL) {
                    uint32_t ttemp = ctr_rtemp(c);
                    ctr_cemit(c, ctr_ins_ab(CTR_OP_LOAD, ttemp, 1));
                    ctr_cemit(c, ctr_ins_abc(CTR_OP_EQ, 0, cr, ttemp));
                    ctr_ctemps(c, 1);
                }
                if (!ex.is_ok) return ex;
            }

            uint32_t jmp_false = c->proto.code_s;
            ctr_cemit(c, ctr_ins_a(CTR_OP_JMP, 0));

            // Then
            ctr_cnode_ex ex = ctr_cnode(c, node->n_if.then_node, UINT32_MAX);
            if (!ex.is_ok) return ex;
            uint32_t ofs = c->proto.code_s - (jmp_false + 1);

            if (node->n_if.else_node) {
                uint32_t jmp_end = c->proto.code_s;
                ctr_cemit(c, ctr_ins_a(CTR_OP_JMP, 0));
                ex = ctr_cnode(c, node->n_if.else_node, UINT32_MAX);
                if (!ex.is_ok) return ex;
                // Patch jump
                c->proto.code[jmp_false] = ctr_ins_a(CTR_OP_JMP, ofs + 1);
                c->proto.code[jmp_end] = ctr_ins_a(CTR_OP_JMP, c->proto.code_s - (jmp_end + 1));
            } else c->proto.code[jmp_false] = ctr_ins_a(CTR_OP_JMP, ofs);

            ctr_ctemps(c, 1);
            return ctr_cnode_ex_ok();
        }
        case CTR_ND_WHILE: {
            uint32_t jmp_cond = c->proto.code_s - 1;
            uint32_t cr = ctr_rtemp(c);
            if (node->n_while.condition->tt == CTR_ND_IDENTIFIER) {
                ctr_local loc;
                if (!ctr_lexists(c, *(sf_str *)node->n_while.condition->n_identifier.dyn, &loc)) { // Global
                    uint32_t name_i;
                    if (!ctr_kfind(c, node->n_while.condition->n_identifier, &name_i)) {
                        ctr_kadd(c, ctr_dref(node->n_while.condition->n_identifier));
                        name_i = c->proto.constants.count - 1;
                    }

                    uint32_t id_r = ctr_rtemp(c);
                    ctr_cemit(c, ctr_ins_abc(CTR_OP_GUPO, id_r, 0, name_i));
                    ctr_cemit(c, ctr_ins_abc(CTR_OP_EQ, 0, id_r, cr));
                    ctr_ctemps(c, 1);
                } else { // Local
                    ctr_cemit(c, ctr_ins_ab(CTR_OP_LOAD, cr, 1));
                    if (loc.upval) { // Reserve temp for upval
                        uint32_t up = ctr_rtemp(c);
                        ctr_cemit(c, ctr_ins_ab(CTR_OP_GETU, up, loc.reg));
                        ctr_cemit(c, ctr_ins_abc(CTR_OP_EQ, 0, up, cr));
                        ctr_ctemps(c, 1);
                    } else
                        ctr_cemit(c, ctr_ins_abc(CTR_OP_EQ, 0, loc.reg, cr));
                }
            } else {
                ctr_cnode_ex ex = ctr_cnode(c, node->n_while.condition, cr);
                if (node->n_while.condition->tt == CTR_ND_CALL) {
                    uint32_t ttemp = ctr_rtemp(c);
                    ctr_cemit(c, ctr_ins_ab(CTR_OP_LOAD, ttemp, 1));
                    ctr_cemit(c, ctr_ins_abc(CTR_OP_EQ, 0, cr, ttemp));
                    ctr_ctemps(c, 1);
                }
                if (!ex.is_ok) return ex;
            }

            uint32_t jmp_break = c->proto.code_s;
            ctr_cemit(c, ctr_ins_a(CTR_OP_JMP, 0));

            // Do
            ctr_cnode_ex ex = ctr_cnode(c, node->n_while.block, UINT32_MAX);
            if (!ex.is_ok) return ex;
            c->proto.code[jmp_break] = ctr_ins_a(CTR_OP_JMP, c->proto.code_s - jmp_break);
            ctr_cemit(c, ctr_ins_a(CTR_OP_JMP, jmp_cond - c->proto.code_s));

            ctr_ctemps(c, 1);
            return ctr_cnode_ex_ok();
        }
        case CTR_ND_RETURN: {
            uint32_t r = ctr_rtemp(c);
            ctr_cnode_ex ex = ctr_cnode(c, node->n_return, r);
            if (!ex.is_ok) return ex;
            ctr_cemit(c, ctr_ins_a(CTR_OP_RET, r));
            return ctr_cnode_ex_ok();
        }

        default: return ctr_cerr(CTR_ERRC_UNKNOWN);
    }
}

ctr_compile_ex ctr_cproto(sf_str src, uint32_t arg_c, ctr_val *args, uint32_t up_c, ctr_upvalue *upvals, bool echo) {
    ctr_scan_ex scan_ex = ctr_scan(src);
    if (!scan_ex.is_ok)
        return ctr_compile_ex_err((ctr_compile_err){
            .tt = scan_ex.err.tt,
            .line = scan_ex.err.line,
            .column = scan_ex.err.column,
        });
    ctr_parse_ex par_ex = ctr_parse(&scan_ex.ok);
    if (!par_ex.is_ok)
        return ctr_compile_ex_err((ctr_compile_err){
            .tt = par_ex.err.tt,
            .line = par_ex.err.line,
            .column = par_ex.err.column,
        });

    ctr_compile_ex ex = ctr_cfun(par_ex.ok, arg_c, args, up_c, upvals, echo);
    ctr_node_free(par_ex.ok);
    return ex;
}
