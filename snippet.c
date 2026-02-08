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