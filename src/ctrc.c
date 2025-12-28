#include "ctr/ctrc.h"
#include "ctr/bytecode.h"
#include "ctr/syntax.h"
#include "sf/str.h"
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    uint32_t reg;
    bool upval;
    int32_t frame_o;
} ctr_local;

struct ctr_localmap;
void _ctr_localmap_fe(void *_, sf_str key, ctr_local _v) { (void)_v; sf_str_free(key); }
void _ctr_localmap_cleanup(struct ctr_localmap *);
#define MAP_NAME ctr_localmap
#define MAP_K sf_str
#define MAP_V ctr_local
#define EQUAL_FN sf_str_eq
#define HASH_FN sf_str_hash
#define CLEANUP_FN _ctr_localmap_cleanup
#include <sf/containers/map.h>
void _ctr_localmap_cleanup(ctr_localmap *map) { ctr_localmap_foreach(map, _ctr_localmap_fe, NULL); }

typedef struct {
    ctr_proto proto;
    ctr_ast ast;
    ctr_localmap lmap;
    uint32_t locals, temps, max_temps;
} ctr_compiler;

#define EXPECTED_NAME ctr_cnode_ex
#define EXPECTED_E ctr_compile_err
#include <sf/containers/expected.h>

static inline void ctr_cemit(ctr_compiler *c, ctr_instruction ins) {
    #ifdef CTR_DEBUG_LOG
    switch (ctr_op_info(ctr_ins_op(ins))->type) {
        case CTR_INS_A: printf("[COM] %s A:%d\n", ctr_op_info(ctr_ins_op(ins))->mnemonic, ctr_ia_a(ins)); break;
        case CTR_INS_AB: printf("[COM] %s A:%u B:%u\n", ctr_op_info(ctr_ins_op(ins))->mnemonic, ctr_iab_a(ins), ctr_iab_b(ins)); break;
        case CTR_INS_ABC: printf("[COM] %s A:%d B:%u C:%u\n", ctr_op_info(ctr_ins_op(ins))->mnemonic, ctr_iabc_a(ins), ctr_iabc_b(ins), ctr_iabc_c(ins)); break;
    }
    #endif
    c->proto.code.bc = realloc(c->proto.code.bc, ++c->proto.code_s * sizeof(ctr_instruction));
    c->proto.code.bc[c->proto.code_s - 1] = ins;
}

static inline uint32_t ctr_rlocal(ctr_compiler *c) { return c->locals++; }

static inline uint32_t ctr_rtemp(ctr_compiler *c) {
    ++c->temps;
    c->max_temps = c->temps > c->max_temps ? c->temps : c->max_temps;
    return c->locals + c->temps - 1;
}
static inline void ctr_ctemps(ctr_compiler *c, uint32_t count) { c->temps -= count; }

bool ctr_kfind(ctr_compiler *c, ctr_val con, uint32_t *idx) {
    for (uint32_t i = 0; i < c->proto.constants.count; ++i) {
        ctr_val v = c->proto.constants.data[i];
        if (v.tt != con.tt) continue;
        switch (v.tt) {
            case CTR_TNIL: *idx = i; return true;
            case CTR_TF64: if (v.val.f64 == con.val.f64) { *idx = i; return true; } else continue;
            case CTR_TI64: if (v.val.i64 == con.val.i64) { *idx = i; return true; } else continue;
            case CTR_TDYN: if (sf_str_eq(*(sf_str *)v.val.dyn, *(sf_str *)con.val.dyn)) { *idx = i; return true; } else continue;
            default: continue;
        }
    }
    return false;
}
void ctr_kadd(ctr_compiler *c, ctr_val con) {
    if (con.tt == CTR_TDYN)
        ctr_header(con)->is_const = true;
    ctr_valvec_push(&c->proto.constants, con);
}

ctr_cnode_ex ctr_cnode(ctr_compiler *c, ctr_node *node, uint32_t t_reg);

ctr_compile_ex ctr_cfun(ctr_node *ast, uint32_t arg_c, ctr_val *args, uint32_t up_c, ctr_upvalue *upvals) {
    ctr_compiler c = {
        .proto = ctr_proto_new(),
        .ast = ast,
        .lmap = ctr_localmap_new(),
        .locals = arg_c,
        .temps = 0, .max_temps = 0,
    };
    c.proto.arg_c = arg_c;
    for (uint32_t i = 0; i < arg_c; ++i)
        ctr_localmap_set(&c.lmap, sf_str_dup(*(sf_str *)args[i].val.dyn), (ctr_local){i, false, 0});
    for (uint32_t i = 0; i < up_c; ++i)
        ctr_localmap_set(&c.lmap, sf_str_dup(upvals[i].name), (ctr_local){i, true, upvals[i].frame_o});

    ctr_kadd(&c, (ctr_val){.tt = CTR_TI64, .val.i64 = 0});
    ctr_kadd(&c, (ctr_val){.tt = CTR_TI64, .val.i64 = 1});

    c.proto.upvals = malloc(sizeof(ctr_upvalue) * up_c);
    memcpy(c.proto.upvals, upvals, sizeof(ctr_upvalue) * up_c);
    c.proto.up_c = up_c;

    ctr_cnode_ex e = ctr_cnode(&c, c.ast, UINT_MAX);
    c.proto.reg_c = c.locals + c.max_temps;

    ctr_localmap_free(&c.lmap);
    ctr_node_free(c.ast);
    return e.is_ok ? ctr_compile_ex_ok(c.proto) : ctr_compile_ex_err(e.value.err);
}

/// Passing UINT_MAX as t_reg acts as a discard.
ctr_cnode_ex ctr_cnode(ctr_compiler *c, ctr_node *node, uint32_t t_reg) {
    switch (node->tt) {
        case CTR_ND_BINARY: {
            if (t_reg == UINT_MAX)
                return ctr_cnode_ex_err((ctr_compile_err){CTR_ERRC_UNUSED_EVALUATION, {0}, node->line, node->column});

            uint32_t left = ctr_rtemp(c), right = ctr_rtemp(c);
            ctr_cnode_ex left_ex = ctr_cnode(c, node->inner.binary.left, left);
            if (!left_ex.is_ok)
                return left_ex;

            ctr_cnode_ex right_ex = ctr_cnode(c, node->inner.binary.right, right);
            if (!right_ex.is_ok) return right_ex;

            switch (node->inner.binary.tt) {
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
                    return ctr_cnode_ex_err((ctr_compile_err){CTR_ERRC_UNKNOWN_OPERATION, {0}, node->line, node->column});
            }

            ctr_ctemps(c, 2);
            return ctr_cnode_ex_ok();
        }
        case CTR_ND_IDENTIFIER: {
            if (t_reg == UINT_MAX)
                return ctr_cnode_ex_err((ctr_compile_err){CTR_ERRC_UNUSED_EVALUATION, {0}, node->line, node->column});
            ctr_localmap_ex lex = ctr_localmap_get(&c->lmap, *(sf_str *)node->inner.identifier.val.dyn);
            if (!lex.is_ok) {
                uint32_t name_i;
                if (!ctr_kfind(c, node->inner.stmt_assign.name, &name_i)) {
                    ctr_kadd(c, ctr_dref(node->inner.stmt_assign.name));
                    name_i = c->proto.constants.count - 1;
                }
                uint32_t gt = ctr_rtemp(c), name = ctr_rtemp(c);
                ctr_cemit(c, ctr_ins_ab(CTR_OP_UP_GET, gt, 0)); // state->global
                ctr_cemit(c, ctr_ins_ab(CTR_OP_LOAD, name, name_i));
                ctr_cemit(c, ctr_ins_abc(CTR_OP_OBJ_GET, t_reg, gt, name));
                ctr_ctemps(c, 2);
            } else ctr_cemit(c, ctr_ins_ab(lex.value.ok.upval ? CTR_OP_UP_GET : CTR_OP_MOVE, t_reg, lex.value.ok.reg));
            return ctr_cnode_ex_ok();
        }
        case CTR_ND_LITERAL: {
            if (t_reg == UINT_MAX)
                return ctr_cnode_ex_err((ctr_compile_err){CTR_ERRC_UNUSED_EVALUATION, {0}, node->line, node->column});
            uint32_t pos;
            if (!ctr_kfind(c, node->inner.literal, &pos)) {
                ctr_kadd(c, ctr_dref(node->inner.literal));
                pos = c->proto.constants.count - 1;
            }
            ctr_cemit(c, ctr_ins_ab(CTR_OP_LOAD, t_reg, pos));
            return ctr_cnode_ex_ok();
        }

        case CTR_ND_LET: {
            ctr_localmap_ex exists = ctr_localmap_get(&c->lmap, *(sf_str *)node->inner.stmt_let.name.val.dyn);
            uint32_t r = exists.is_ok ? exists.value.ok.reg : c->locals++;
            ctr_cnode_ex rv_ex = ctr_cnode(c, node->inner.stmt_let.value, r);
            if (!rv_ex.is_ok) return rv_ex;
            if (!exists.is_ok)
                ctr_localmap_set(&c->lmap, sf_str_dup(*(sf_str *)node->inner.stmt_let.name.val.dyn), (ctr_local){r, false, 0});
            if (ctr_niscondition(node->inner.stmt_let.value)) { // Conditions
                ctr_cemit(c, ctr_ins_ab(CTR_OP_LOAD, r, 0));
                ctr_cemit(c, ctr_ins_ab(CTR_OP_LOAD, r, 1));
            }
            return ctr_cnode_ex_ok();
        }
        case CTR_ND_ASSIGN: {
            uint32_t rhs = ctr_rtemp(c);
            ctr_cnode_ex rv_ex = ctr_cnode(c, node->inner.stmt_let.value, rhs);
            if (!rv_ex.is_ok) return rv_ex;
            ctr_localmap_ex lex = ctr_localmap_get(&c->lmap, *(sf_str *)node->inner.stmt_assign.name.val.dyn);
            if (!lex.is_ok) {
                uint32_t name_i;
                if (!ctr_kfind(c, node->inner.stmt_assign.name, &name_i)) {
                    ctr_kadd(c, ctr_dref(node->inner.stmt_assign.name));
                    name_i = c->proto.constants.count - 1;
                }
                uint32_t gt = ctr_rtemp(c), name = ctr_rtemp(c);
                ctr_cemit(c, ctr_ins_ab(CTR_OP_UP_GET, gt, 0)); // state->global
                ctr_cemit(c, ctr_ins_ab(CTR_OP_LOAD, name, name_i));
                ctr_cemit(c, ctr_ins_abc(CTR_OP_OBJ_SET, gt, name, rhs));
                ctr_ctemps(c, 2);
            } else ctr_cemit(c, ctr_ins_ab(lex.value.ok.upval ? CTR_OP_UP_SET : CTR_OP_MOVE, lex.value.ok.reg, rhs));
            if (ctr_niscondition(node->inner.stmt_let.value)) { // Conditions
                ctr_cemit(c, ctr_ins_ab(lex.value.ok.upval ? CTR_OP_UP_SET : CTR_OP_MOVE, lex.value.ok.reg, 0));
                ctr_cemit(c, ctr_ins_ab(lex.value.ok.upval ? CTR_OP_UP_SET : CTR_OP_MOVE, lex.value.ok.reg, 1));
            }
            ctr_ctemps(c, 1); // temp
            return ctr_cnode_ex_ok();
        }
        case CTR_ND_CALL: {
            uint32_t arg_rs = UINT32_MAX;
            for (size_t i = 0; i < node->inner.stmt_call.arg_c; ++i) {
                uint32_t r = ctr_rtemp(c);
                ctr_cnode_ex ex = ctr_cnode(c, node->inner.stmt_call.args[i], r);
                if (!ex.is_ok) return ex;
                if (arg_rs == UINT32_MAX) arg_rs = r; // first
            }

            ctr_localmap_ex lex = ctr_localmap_get(&c->lmap, *(sf_str *)node->inner.stmt_call.name.val.dyn);
            if (!lex.is_ok) {
                uint32_t name_i;
                if (!ctr_kfind(c, node->inner.stmt_assign.name, &name_i)) {
                    ctr_kadd(c, ctr_dref(node->inner.stmt_assign.name));
                    name_i = c->proto.constants.count - 1;
                }
                uint32_t gt = ctr_rtemp(c), name = ctr_rtemp(c), fun_r = ctr_rtemp(c);
                ctr_cemit(c, ctr_ins_ab(CTR_OP_UP_GET, gt, 0)); // state->global
                ctr_cemit(c, ctr_ins_ab(CTR_OP_LOAD, name, name_i));
                ctr_cemit(c, ctr_ins_abc(CTR_OP_OBJ_GET, fun_r, gt, name));
                ctr_cemit(c, ctr_ins_abc(CTR_OP_CALL, t_reg == UINT32_MAX ? ctr_rtemp(c) : t_reg, fun_r, arg_rs == UINT32_MAX ? 0 : arg_rs));
                ctr_ctemps(c, 3);
            } else {
                uint32_t fun_r = lex.value.ok.reg;
                if (lex.value.ok.upval) {
                    fun_r = ctr_rtemp(c);
                    ctr_cemit(c, ctr_ins_ab(CTR_OP_UP_GET, fun_r, lex.value.ok.reg));
                }
                ctr_cemit(c, ctr_ins_abc(CTR_OP_CALL, t_reg == UINT32_MAX ? ctr_rtemp(c) : t_reg, fun_r, arg_rs == UINT32_MAX ? 0 : arg_rs));
                if (lex.value.ok.upval) ctr_ctemps(c, 1);
            }
            if (t_reg == UINT32_MAX) ctr_ctemps(c, 1);
            ctr_ctemps(c, node->inner.stmt_call.arg_c);
            return ctr_cnode_ex_ok();
        }
        case CTR_ND_IF: {
            uint32_t cr = ctr_rtemp(c);
            ctr_cnode_ex reg = ctr_cnode(c, node->inner.stmt_if.condition, cr);
            if (!reg.is_ok) return reg;

            uint32_t jmp_false = c->proto.code_s;
            ctr_cemit(c, ctr_ins_a(CTR_OP_JMP, 0));

            // Then
            reg = ctr_cnode(c, node->inner.stmt_if.then_node, UINT_MAX);
            uint32_t ofs = c->proto.code_s - (jmp_false + 1);

            if (node->inner.stmt_if.else_node) {
                uint32_t jmp_end = c->proto.code_s;
                ctr_cemit(c, ctr_ins_a(CTR_OP_JMP, 0));
                reg = ctr_cnode(c, node->inner.stmt_if.else_node, UINT_MAX);
                c->proto.code.bc[jmp_false] = ctr_ins_a(CTR_OP_JMP, ofs + 1);
                c->proto.code.bc[jmp_end] = ctr_ins_a(CTR_OP_JMP, c->proto.code_s - (jmp_end + 1));
            } else c->proto.code.bc[jmp_false] = ctr_ins_a(CTR_OP_JMP, ofs);

            ctr_ctemps(c, 1);
            return ctr_cnode_ex_ok();
        }
        case CTR_ND_BLOCK:
            for (size_t i = 0; i < node->inner.block.count; ++i) {
                ctr_cnode_ex ex = ctr_cnode(c, node->inner.block.stmts[i], UINT_MAX);
                if (!ex.is_ok) return ex;
            }
            return ctr_cnode_ex_ok();
        case CTR_ND_FUN: {
            ctr_upvalue upvals[c->proto.up_c + node->inner.fun.cap_c];
            memcpy(upvals, c->proto.upvals, c->proto.up_c * sizeof(ctr_upvalue));
            for (uint32_t i = 0; i < node->inner.fun.cap_c; ++i) {
                ctr_val *cap = node->inner.fun.captures + i;
                sf_str name = *(sf_str *)cap->val.dyn;
                ctr_localmap_ex lex = ctr_localmap_get(&c->lmap, name); // here
                if (!lex.is_ok)
                    return ctr_cnode_ex_err((ctr_compile_err){CTR_ERRC_UNKNOWN_LOCAL, {0}, node->line, node->column});
                if (lex.value.ok.upval)
                    upvals[c->proto.up_c + i] = (ctr_upvalue){name, CTR_UP_REF, .inner.ref = lex.value.ok.reg, lex.value.ok.frame_o - 1};
                else {
                    ctr_cemit(c, ctr_ins_a(CTR_OP_UP_REF, lex.value.ok.reg));
                    upvals[c->proto.up_c + i] = (ctr_upvalue){name, CTR_UP_REF, .inner.ref = lex.value.ok.reg, .frame_o = -1};
                }
            }

            #ifdef CTR_EMIT_LOG
            printf("[COM] == FUN ==\n");
            #endif
            ctr_compile_ex ex = ctr_cfun(
                node->inner.fun.block,
                node->inner.fun.arg_c, node->inner.fun.args,
                c->proto.up_c + node->inner.fun.cap_c, upvals
            );
            #ifdef CTR_EMIT_LOG
            printf("[COM] == END ==\n");
            #endif

            if (!ex.is_ok) return ctr_cnode_ex_err(ex.value.err);
            ctr_val fun = ctr_dnew(CTR_DFUN);
            *(ctr_proto *)fun.val.dyn = ex.value.ok;

            ctr_kadd(c, fun);
            ctr_cemit(c, ctr_ins_ab(CTR_OP_LOAD, t_reg, c->proto.constants.count - 1));
            return ctr_cnode_ex_ok();
        }
        case CTR_ND_WHILE: {
            uint32_t cr = ctr_rtemp(c);
            uint32_t jmp_cond = c->proto.code_s - 1;
            ctr_cnode_ex reg = ctr_cnode(c, node->inner.stmt_while.condition, cr);
            if (!reg.is_ok) return reg;

            uint32_t jmp_break = c->proto.code_s;
            ctr_cemit(c, ctr_ins_a(CTR_OP_JMP, 0));

            // Do
            reg = ctr_cnode(c, node->inner.stmt_while.block, UINT_MAX);
            c->proto.code.bc[jmp_break] = ctr_ins_a(CTR_OP_JMP, c->proto.code_s - jmp_break);
            ctr_cemit(c, ctr_ins_a(CTR_OP_JMP, jmp_cond - c->proto.code_s));

            ctr_ctemps(c, 1);
            return ctr_cnode_ex_ok();
        }
        case CTR_ND_RETURN: {
            uint32_t r = ctr_rtemp(c);
            ctr_cnode_ex ex = ctr_cnode(c, node->inner.stmt_ret, r);
            if (!ex.is_ok) return ex;
            ctr_cemit(c, ctr_ins_a(CTR_OP_RET, r));
            return ctr_cnode_ex_ok();
        }
        default: return ctr_cnode_ex_err((ctr_compile_err){CTR_ERRC_UNKNOWN, {0}, node->line, node->column});
    }
}

ctr_compile_ex ctr_cproto(sf_str src, uint32_t arg_c, ctr_val *args, uint32_t up_c, ctr_upvalue *upvals) {
    ctr_scan_ex scan_ex = ctr_scan(src);
    if (!scan_ex.is_ok)
        return ctr_compile_ex_err((ctr_compile_err){
            .tt = CTR_ERRC_SCAN_ERR,
            .inner.scan_err = scan_ex.value.err.tt,
            .line = scan_ex.value.err.line,
            .column = scan_ex.value.err.column,
        });
    ctr_parse_ex par_ex = ctr_parse(&scan_ex.value.ok);
    if (!par_ex.is_ok)
        return ctr_compile_ex_err((ctr_compile_err){
            .tt = CTR_ERRC_PARSE_ERR,
            .inner.parse_err = par_ex.value.err.tt,
            .line = par_ex.value.err.line,
            .column = par_ex.value.err.column,
        });
    return ctr_cfun(par_ex.value.ok, arg_c, args, up_c, upvals);
}
