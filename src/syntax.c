#include "sol/syntax.h"
#include "sol/bytecode.h"
#include "sf/str.h"
#include <stdint.h>
#include <stdlib.h>

void _keywords_foreach(void *_u, sf_str k, sol_tokentype _v) { (void)_u;(void)_v; sf_str_free(k); }
void _sol_keywords_cleanup(sol_keywords *map) {
    sol_keywords_foreach(map, _keywords_foreach, NULL);
}

typedef struct {
    sf_str src;
    sol_token current;
    size_t cc;
    sol_keywords keywords;
    sol_dalloc *alloc;
} sol_scanner;

static sol_val sol_scan_str(sol_scanner *s, const sf_str str) {
    sol_dyn p = calloc(1, sizeof(sol_dalloc) + sizeof(sf_str));
    sol_dalloc *dh = p;
    *dh = (sol_dalloc){
        .next = NULL,
        .size = sizeof(sf_str),
        .tt = SOL_DSTR,
        .mark = SOL_DYN_WHITE,
    };
    p = (char *)p + sizeof(sol_dalloc);
    *(sf_str *)p = sf_str_dup(str);
    sol_dalloc *dd = s->alloc;
    if (dd == NULL) s->alloc = dh;
    else {
        while (dd->next) dd = dd->next;
        dd->next = dh;
    }
    return (sol_val){ .tt = SOL_TDYN, .dyn = p };
}

#define sol_scancase(_c, _tt) case _c: s.current.tt = _tt; break
static inline bool sol_scanpeek(sol_scanner *s, char match) {
    if (s->cc + 1 >= s->src.len || s->src.c_str[s->cc+1] != match)
        return false;
    ++s->cc;
    ++s->current.column;
    return true;
}

static inline bool sol_isnumber(char c) { return c >= '0' && c <= '9'; }
static inline bool sol_isalphan(char c) { return (c >= 'A' && c <= 'Z')|| (c >= 'a' && c <= 'z') || c == '_' || sol_isnumber(c); }

sol_token sol_scanstr(sol_scanner *s) {
    bool terminated = false;
    uint16_t len = 0;
    for (size_t cc = s->cc + 1; cc < s->src.len; ++cc) {
        char c = s->src.c_str[cc];
        if (c == '"') {
            terminated = true;
            break;
        }
        ++len;
    }
    if (!terminated)
        return (sol_token){TK_NIL, SOL_NIL, 0, 0};

    char *str = calloc(1, len + 1);
    memcpy(str, s->src.c_str + s->cc + 1, len);

    s->cc += len + 1;
    s->current.column += len + 1;

    return (sol_token){
        TK_STRING,
        sol_scan_str(s, sf_own(str)),
        s->current.line,
        s->current.column,
    };
}

sol_token sol_scannum(sol_scanner *s) {
    bool is_number = false;
    uint16_t len = 1;
    for (size_t cc = s->cc + 1; cc < s->src.len; ++cc) {
        if (s->src.c_str[cc] == '.') {
            if (is_number)
                return (sol_token){TK_NIL, SOL_NIL, 0, 0};
            is_number = true;
        } else if (!sol_isnumber(s->src.c_str[cc]))
            break;
        ++len;
    }

    char *str = calloc(len + 1, sizeof(char));
    memcpy(str, s->src.c_str + s->cc, len);
    s->cc += len - 1;
    s->current.column += len - 1;

    sol_token tok;
    if (is_number)
        tok = (sol_token) {
            .tt = TK_NUMBER,
            .value = (sol_val){.f64 = atof(str), .tt = SOL_TF64},
            .line = s->current.line,
            .column = s->current.column,
        };
    else
        tok = (sol_token) {
            .tt = TK_INTEGER,
            .value = (sol_val){.i64 = atoll(str), .tt = SOL_TI64},
            .line = s->current.line,
            .column = s->current.column,
        };
    free(str);
    return tok;
}

sol_token sol_scanidentifier(sol_scanner *s) {
    uint16_t len = 1;
    for (size_t cc = s->cc + 1; cc < s->src.len; ++cc) {
        if (!sol_isalphan(s->src.c_str[cc]))
            break;
        ++len;
    }

    char *str = calloc(len + 1, sizeof(char));
    memcpy(str, s->src.c_str + s->cc, len);

    s->cc += len - 1;
    s->current.column += len - 1;

    sol_keywords_ex ex = sol_keywords_get(&s->keywords, sf_ref(str));
    if (ex.is_ok) {
        sol_val value = SOL_NIL;
        if (ex.ok == TK_TRUE)
            value = SOL_TRUE;
        if (ex.ok == TK_FALSE)
            value = SOL_FALSE;
        if (ex.ok == TK_OPCODE) {
            for (sol_opcode o = 0; o < SOL_OP_COUNT; ++o)
                value = sf_str_eq(sf_lit(sol_op_info(o)->mnemonic), sf_ref(str)) ?
                    (sol_val){.tt = SOL_TI64, .i64 = o} : value;
        }
        free(str);
        return (sol_token) {
            .tt = ex.ok,
            .value = value,
            .line = s->current.line,
            .column = s->current.column,
        };
    } else {
        sf_str ds = sf_str_cdup(str);
        free(str);
        return (sol_token){
            .tt = TK_IDENTIFIER,
            .value = sol_scan_str(s, ds),
            .line = s->current.line,
            .column = s->current.column,
        };
    }
}

sol_scan_ex sol_scan(sf_str src) {
    sol_tokenvec tks = sol_tokenvec_new();
    sol_scanner s = {
        .src = src,
        .current = {TK_EOF, SOL_NIL, 1, 1},
        .cc = 0,
        .keywords = sol_keywords_new(),
    };
    sol_error eval = SOL_ERRP_UNEXPECTED_TOKEN;

    for (sol_opcode o = 0; o < SOL_OP_COUNT; ++o)
        sol_keywords_set(&s.keywords, sf_ref(sol_op_info(o)->mnemonic), TK_OPCODE);

    sol_keywords_set(&s.keywords, sf_lit("asm"), TK_ASM);
    sol_keywords_set(&s.keywords, sf_lit("and"), TK_AND);
    sol_keywords_set(&s.keywords, sf_lit("or"), TK_OR);
    sol_keywords_set(&s.keywords, sf_lit("return"), TK_RETURN);
    sol_keywords_set(&s.keywords, sf_lit("if"), TK_IF);
    sol_keywords_set(&s.keywords, sf_lit("else"), TK_ELSE);
    sol_keywords_set(&s.keywords, sf_lit("nil"), TK_NIL);
    sol_keywords_set(&s.keywords, sf_lit("let"), TK_LET);
    sol_keywords_set(&s.keywords, sf_lit("for"), TK_FOR);
    sol_keywords_set(&s.keywords, sf_lit("while"), TK_WHILE);
    sol_keywords_set(&s.keywords, sf_lit("true"), TK_TRUE);
    sol_keywords_set(&s.keywords, sf_lit("false"), TK_FALSE);

    sol_tokenvec_push(&tks, (sol_token){TK_SOF, SOL_NIL, s.current.line, s.current.column});
    for (; s.cc < src.len; ++s.cc) {
        s.current = (sol_token){TK_EOF, SOL_NIL, s.current.line, s.current.column};
        char c = src.c_str[s.cc];
        size_t pcc = s.cc;
        switch (c) {
            sol_scancase('(', TK_LEFT_PAREN);
            sol_scancase(')', TK_RIGHT_PAREN);
            sol_scancase('{', TK_LEFT_BRACE);
            sol_scancase('}', TK_RIGHT_BRACE);
            sol_scancase('[', TK_LEFT_BRACKET);
            sol_scancase(']', TK_RIGHT_BRACKET);
            sol_scancase(',', TK_COMMA);
            sol_scancase('.', TK_PERIOD);
            sol_scancase('+', TK_PLUS);
            sol_scancase(';', TK_SEMICOLON);
            sol_scancase('*', TK_ASTERISK);
            sol_scancase('!', sol_scanpeek(&s, '=') ? TK_NOT_EQUAL : TK_BANG);
            sol_scancase('<', sol_scanpeek(&s, '=') ? TK_LESS_EQUAL : TK_LESS);
            sol_scancase('>', sol_scanpeek(&s, '=') ? TK_GREATER_EQUAL : TK_GREATER);
            sol_scancase('=', sol_scanpeek(&s, '=') ? TK_DOUBLE_EQUAL : TK_EQUAL);

            case '&': {
                if (sol_scanpeek(&s, '&')) { s.current.tt = TK_AND; break; }
                goto err;
            }
            case '|': {
                if (sol_scanpeek(&s, '|')) { s.current.tt = TK_OR; break; }
                goto err;
            }
            case '/': {
                if (sol_scanpeek(&s, '/')) {
                    for (; s.cc < src.len && src.c_str[s.cc] != '\n'; ++s.cc){};
                    continue;
                }
                s.current.tt = TK_SLASH;
                break;
            }

            case '\n': {
                ++s.current.line;
                s.current.column = 0;
                continue;
            }
            case ' ': ++s.current.column; continue;
            case '\r': case '\t': continue;
            case '"': {
                s.current = sol_scanstr(&s);
                if (s.current.tt != TK_STRING) {
                    eval = SOL_ERRP_UNTERMINATED_STR;
                    goto err;
                }
                sol_tokenvec_push(&tks, s.current);
                continue;
            }

            default:
                if (c == '-' && tks.count > 0 && (
                    (tks.data + tks.count - 1)->tt == TK_NUMBER || (tks.data + tks.count - 1)->tt == TK_INTEGER ||
                    (tks.data + tks.count - 1)->tt == TK_IDENTIFIER || (tks.data + tks.count - 1)->tt == TK_STRING ||
                    (tks.data + tks.count - 1)->tt == TK_TRUE || (tks.data + tks.count - 1)->tt == TK_FALSE ||
                    (tks.data + tks.count - 1)->tt == TK_NIL || (tks.data + tks.count - 1)->tt == TK_RIGHT_PAREN
                )) {
                    s.current.tt = TK_MINUS;
                    sol_tokenvec_push(&tks, s.current);
                    continue;
                }
                if (sol_isnumber(c) || (c == '-' && sol_isnumber(s.src.c_str[s.cc + 1]))) { // Number
                    s.current = sol_scannum(&s);
                    if (s.current.tt != TK_NUMBER && s.current.tt != TK_INTEGER) {
                        eval = SOL_ERRP_NUMBER_FORMAT;
                        goto err;
                    }
                    sol_tokenvec_push(&tks, s.current);
                    continue;
                } else if (sol_isalphan(c)) { // Identifier
                    s.current = sol_scanidentifier(&s);
                    sol_tokenvec_push(&tks, s.current);
                    continue;
                }
            err: {
                sol_tokenvec_free(&tks);
                sol_keywords_free(&s.keywords);
                size_t tk_len = s.cc - pcc + 1;
                for (size_t cc2 = s.cc; cc2 < s.src.len; ++cc2) {
                    char ws = s.src.c_str[cc2];
                    if (ws == ' ' || ws == '\n' || ws == '\r' || ws == '\n')
                        break;
                    ++tk_len;
                }
                char *str = calloc(1, tk_len + 1);
                memcpy(str, s.src.c_str + pcc, tk_len);
                return sol_scan_ex_err((sol_scan_err){eval, sf_own(str), s.current.line, s.current.column});
            }
        }
        sol_tokenvec_push(&tks, s.current);
    }

    sol_keywords_free(&s.keywords);
    sol_tokenvec_push(&tks, (sol_token){TK_EOF, SOL_NIL, s.current.line, s.current.column});
    return sol_scan_ex_ok((sol_scan_ok){tks, s.alloc});
}

typedef struct {
    sol_token *tok;
    bool asm;
} sol_parser;

void sol_node_free(sol_node *tree) {
    switch (tree->tt) {
        case SOL_ND_MEMBER:
            sol_node_free(tree->n_postfix.expr);
            break;
        case SOL_ND_BINARY:
            sol_node_free(tree->n_binary.left);
            sol_node_free(tree->n_binary.right);
            break;
        case SOL_ND_RETURN:
            sol_node_free(tree->n_return);
            break;
        case SOL_ND_IDENTIFIER:
        case SOL_ND_LITERAL:
            break;
        case SOL_ND_LET:
            sol_node_free(tree->n_let.value);
            break;
        case SOL_ND_ASSIGN:
            sol_node_free(tree->n_assign.expr);
            sol_node_free(tree->n_assign.value);
            break;
        case SOL_ND_CALL:
            sol_node_free(tree->n_call.identifier);
            if (tree->n_call.args) {
                for (size_t i = 0; i < tree->n_call.arg_c; ++i)
                    sol_node_free(tree->n_call.args[i]);
                free(tree->n_call.args);
            }
            break;
        case SOL_ND_IF:
            sol_node_free(tree->n_if.condition);
            sol_node_free(tree->n_if.then_node);
            if (tree->n_if.else_node)
                sol_node_free(tree->n_if.else_node);
            break;
        case SOL_ND_BLOCK:
            if (tree->n_block.stmts) {
                for (uint32_t i = 0; i < tree->n_block.count; ++i)
                    sol_node_free(tree->n_block.stmts[i]);
                free(tree->n_block.stmts);
            }
            break;
        case SOL_ND_FUN:
            free(tree->n_fun.captures);
            free(tree->n_fun.args);
            if (tree->n_fun.block)
                sol_node_free(tree->n_fun.block);
            break;
        case SOL_ND_ASM:
            sol_node_free(tree->n_asm.n_fun);
            break;
        case SOL_ND_INS:
            break;
        case SOL_ND_WHILE:
            sol_node_free(tree->n_while.condition);
            sol_node_free(tree->n_while.block);
            break;
    }
    free(tree);
}

size_t sol_precedence(sol_tokentype tt) {
    switch (tt) {
        case TK_EQUAL: return 0;
        case TK_OR: return 1;
        case TK_AND: return 2;
        case TK_DOUBLE_EQUAL:
        case TK_NOT_EQUAL: return 3;
        case TK_LESS: case TK_LESS_EQUAL:
        case TK_GREATER: case TK_GREATER_EQUAL: return 4;
        case TK_PLUS: case TK_MINUS: return 5;
        case TK_ASTERISK: case TK_SLASH: return 6;
        default: return SIZE_MAX;
    }
}
bool sol_niscondition(sol_node *node) {
    if (node->tt == SOL_ND_IDENTIFIER ||
        node->tt == SOL_ND_CALL ||
       (node->tt == SOL_ND_LITERAL && node->n_literal.tt == SOL_TBOOL))
        return true;
    if (node->tt != SOL_ND_BINARY)
        return false;
    switch (node->n_binary.op) {
        case TK_OR: case TK_AND: case TK_DOUBLE_EQUAL: case TK_NOT_EQUAL:
        case TK_LESS: case TK_LESS_EQUAL: case TK_GREATER: case TK_GREATER_EQUAL:
            return true;
        default: return false;
    }
}

static inline bool sol_parpeek(sol_parser *p, sol_tokentype match) {
    if (p->tok->tt != match)
        return false;
    ++p->tok;
    return true;
}

// Convenience err macro
#define sol_perr(type) sol_parse_ex_err((sol_parse_err){(type), p->tok->line, p->tok->column})

sol_parse_ex sol_pprimary(sol_parser *p);
sol_parse_ex sol_pexpr(sol_parser *p, size_t prec);
sol_parse_ex sol_pif(sol_parser *p);
sol_parse_ex sol_plet(sol_parser *p);
sol_parse_ex sol_ppostfix(sol_parser *p);
sol_parse_ex sol_pblock(sol_parser *p);
sol_parse_ex sol_pfun(sol_parser *p);
sol_parse_ex sol_pasm(sol_parser *p);
sol_parse_ex sol_pins(sol_parser *p);
sol_parse_ex sol_pobj(sol_parser *p);
sol_parse_ex sol_pwhile(sol_parser *p);
sol_parse_ex sol_preturn(sol_parser *p);
sol_parse_ex sol_pstmt(sol_parser *p);

sol_parse_ex sol_pprimary(sol_parser *p) {
    switch (p->tok->tt) {
        case TK_INTEGER: case TK_NUMBER: case TK_STRING: case TK_TRUE: case TK_FALSE: case TK_NIL: {
            sol_node *n = malloc(sizeof(sol_node));
            *n = (sol_node){
                p->tok->tt == TK_IDENTIFIER ? SOL_ND_IDENTIFIER : SOL_ND_LITERAL,
                p->tok->line, p->tok->column,
                .n_literal = p->tok->value,
            };
            ++p->tok;
            return sol_parse_ex_ok(n);
        }
        case TK_LEFT_BRACKET: return sol_pfun(p);
        case TK_ASM: return sol_pasm(p);
        case TK_LEFT_BRACE: return sol_pobj(p);
        case TK_IDENTIFIER: {
            sol_node *n = malloc(sizeof(sol_node));
            *n = (sol_node){
                SOL_ND_IDENTIFIER,
                p->tok->line, p->tok->column,
                .n_identifier = p->tok->value,
            };
            ++p->tok;
            return sol_parse_ex_ok(n);
        }
        case TK_LEFT_PAREN: {
            ++p->tok;
            sol_parse_ex ex = sol_pexpr(p, 0);
            if (!ex.is_ok) return ex;
            if (p->tok->tt != TK_RIGHT_PAREN)
                return sol_perr(SOL_ERRP_EXPECTED_RPAREN);
            ++p->tok;
            return sol_parse_ex_ok(ex.ok);
        }
        default:
            return sol_perr(SOL_ERRP_EXPECTED_EXPRESSION);
    }
}

sol_parse_ex sol_pexpr(sol_parser *p, size_t prec) {
    sol_parse_ex ex = sol_ppostfix(p);
    if (!ex.is_ok) return ex;

    sol_node *left = ex.ok;
    while (true) {
        sol_token *op = p->tok;
        size_t op_prec = sol_precedence(op->tt);
        if (op_prec == SIZE_MAX || op_prec < prec)
            break;
        ++p->tok;

        ex = sol_pexpr(p, (op->tt == TK_EQUAL) ? op_prec : op_prec + 1);
        if (!ex.is_ok) {
            sol_node_free(left);
            return ex;
        }
        sol_node *bin = malloc(sizeof(sol_node));
        *bin = (sol_node){
            .tt = SOL_ND_BINARY,
            .line = op->line, .column = op->column,
            .n_binary = {
                .op = op->tt,
                .left = left,
                .right = ex.ok,
            },
        };
        left = bin;
    }
    return sol_parse_ex_ok(left);
}

sol_parse_ex sol_pif(sol_parser *p) {
    sol_token *tk_if = p->tok++;
    sol_parse_ex cex = sol_pexpr(p, 0);
    if (!cex.is_ok) return cex;
    if (!sol_niscondition(cex.ok)) {
        uint16_t line = cex.ok->line;
        uint16_t column = cex.ok->column;
        sol_node_free(cex.ok);
        return sol_parse_ex_err((sol_parse_err){SOL_ERRP_EXPECTED_CONDITION, line, column});
    }

    if (p->tok->tt != TK_LEFT_BRACE) {
        sol_node_free(cex.ok);
        return sol_perr(SOL_ERRP_EXPECTED_BLOCK);
    }
    sol_parse_ex tex = sol_pblock(p);
    if (!tex.is_ok) {
        sol_node_free(cex.ok);
        return tex;
    }
    sol_parse_ex eex = (sol_parse_ex){.is_ok = false};
    if (p->tok->tt == TK_ELSE) {
        ++p->tok;
        if (p->tok->tt != TK_LEFT_BRACE) {
            sol_node_free(cex.ok);
            sol_node_free(tex.ok);
            return sol_perr(SOL_ERRP_EXPECTED_BLOCK);
        }
        eex = sol_pblock(p);
        if (!eex.is_ok) {
            sol_node_free(cex.ok);
            sol_node_free(tex.ok);
            return eex;
        }
    }

    sol_node *n_if = malloc(sizeof(sol_node));
    *n_if = (sol_node){
        .tt = SOL_ND_IF,
        .line = tk_if->line, .column = tk_if->column,
        .n_if = {
            .condition = cex.ok,
            .then_node = tex.ok,
            .else_node = eex.is_ok ? eex.ok : NULL,
        },
    };
    return sol_parse_ex_ok(n_if);
}

sol_parse_ex sol_plet(sol_parser *p) {
    sol_token *let = p->tok++;

    if (p->tok->tt != TK_IDENTIFIER)
        return sol_perr(SOL_ERRP_EXPECTED_IDENTIFIER);
    sol_token *name = p->tok++;

    if (p->tok->tt != TK_EQUAL)
        return sol_perr(SOL_ERRP_EXPECTED_EQUAL);
    ++p->tok;

    sol_parse_ex vex = sol_pexpr(p, 0);
    if (!vex.is_ok) return vex;
    if (p->tok->tt != TK_SEMICOLON) {
        --p->tok;
        sol_node_free(vex.ok);
        return sol_perr(SOL_ERRP_EXPECTED_SEMICOLON);
    }
    ++p->tok;

    sol_node *n_let = malloc(sizeof(sol_node));
    *n_let = (sol_node){
        .tt = SOL_ND_LET,
        .line = let->line, .column = let->column,
        .n_let = {
            .name = name->value,
            .value = vex.ok,
        }
    };
    return sol_parse_ex_ok(n_let);
}

sol_parse_ex sol_ppostfix(sol_parser *p) {
    uint16_t line = p->tok->line, column = p->tok->column;
    sol_parse_ex ex = sol_pprimary(p);
    if (!ex.is_ok) return ex;
    sol_node *node = ex.ok;

    while (true) {
        if (p->tok->tt == TK_LEFT_PAREN) {
            sol_node *call = malloc(sizeof(sol_node));
            *call = (sol_node){
                .tt = SOL_ND_CALL,
                .line = line,
                .column = column,
                .n_call = {
                    .identifier = node,
                    .args = NULL,
                    .arg_c = 0,
                }
            };
            ++p->tok;

            while (p->tok->tt != TK_RIGHT_PAREN && p->tok->tt != TK_EOF) {
                sol_parse_ex arg = sol_pexpr(p, 0);
                if (!arg.is_ok) {
                    sol_node_free(call);
                    return arg;
                }
                call->n_call.args = realloc(call->n_call.args, sizeof(sol_node*) * (++call->n_call.arg_c));
                call->n_call.args[call->n_call.arg_c - 1] = arg.ok;
                if (p->tok->tt == TK_COMMA)
                    ++p->tok;
                else break;
            }
            if (p->tok->tt != TK_RIGHT_PAREN)
                return sol_perr(SOL_ERRP_UNTERMINATED_ARGS);

            ++p->tok;
            node = call;
            continue;
        }

        if (p->tok->tt == TK_PERIOD) {
            ++p->tok;
            if (p->tok->tt != TK_IDENTIFIER)
                return sol_perr(SOL_ERRP_EXPECTED_IDENTIFIER);

            sol_node *member = malloc(sizeof(sol_node));
            *member = (sol_node){
                .tt = SOL_ND_MEMBER,
                .line = line,
                .column = column,
                .n_postfix = {
                    .expr = node,
                    .postfix = p->tok->value,
                }
            };
            ++p->tok;
            node = member;
            continue;
        }
        break;
    }

    return sol_parse_ex_ok(node);
}

sol_parse_ex sol_pblock(sol_parser *p) {
    sol_node *n_block = malloc(sizeof(sol_node));
    *n_block = (sol_node){
        .tt = SOL_ND_BLOCK,
        .line = p->tok->line, .column = p->tok->column,
        .n_block = {
            .stmts = NULL,
            .count = 0,
        },
    };
    sol_tokentype st = p->tok->tt;
    ++p->tok;
    while (p->tok->tt != TK_RIGHT_BRACE && p->tok->tt != TK_EOF) {
        sol_parse_ex sex = sol_pstmt(p); // HHAHAHAHHAHHAHAHHAH
        if (!sex.is_ok) {
            sol_node_free(n_block);
            return sex;
        }
        n_block->n_block.stmts = realloc(n_block->n_block.stmts, ++n_block->n_block.count * sizeof(sol_node *));
        n_block->n_block.stmts[n_block->n_block.count - 1] = sex.ok;
    }

    if (st == TK_LEFT_BRACE && p->tok->tt != TK_RIGHT_BRACE) {
        sol_node_free(n_block);
        return sol_perr(SOL_ERRP_EXPECTED_RBRACE);
    }
    ++p->tok;

    return sol_parse_ex_ok(n_block);
}

sol_parse_ex sol_pfun(sol_parser *p) {
    ++p->tok; // Consume [
    sol_node *n_fun = malloc(sizeof(sol_node));
    *n_fun = (sol_node){
        .tt = SOL_ND_FUN,
        .line = p->tok->line, .column = p->tok->column,
        .n_fun = {
            .captures = NULL,
            .cap_c = 0,
            .args = NULL,
            .arg_c = 0,
        },
    };

    while (p->tok->tt != TK_RIGHT_BRACKET && p->tok->tt != TK_EOF) {
        if (p->tok->tt != TK_IDENTIFIER) {
            sol_node_free(n_fun);
            return sol_perr(SOL_ERRP_EXPECTED_IDENTIFIER);
        }
        n_fun->n_fun.captures = realloc(n_fun->n_fun.captures, (++n_fun->n_fun.cap_c) * sizeof(sol_val));
        n_fun->n_fun.captures[n_fun->n_fun.cap_c - 1] = p->tok->value;
        ++p->tok;

        if (p->tok->tt != TK_COMMA && p->tok->tt != TK_RIGHT_BRACKET) {
            sol_node_free(n_fun);
            return sol_perr(SOL_ERRP_UNTERMINATED_CAPTURES);
        }
        if (p->tok->tt == TK_COMMA) ++p->tok;
    }
    ++p->tok;

    if (p->tok->tt != TK_LEFT_PAREN) {
        sol_node_free(n_fun);
        return sol_perr(SOL_ERRP_EXPECTED_ARGS);
    }
    ++p->tok;
    while (p->tok->tt != TK_RIGHT_PAREN && p->tok->tt != TK_EOF) {
        if (p->tok->tt != TK_IDENTIFIER) {
            sol_node_free(n_fun);
            return sol_perr(SOL_ERRP_EXPECTED_IDENTIFIER);
        }
        n_fun->n_fun.args = realloc(n_fun->n_fun.args, ++n_fun->n_fun.arg_c * sizeof(sol_val));
        n_fun->n_fun.args[n_fun->n_fun.arg_c - 1] = p->tok->value;
        ++p->tok;

        if (p->tok->tt != TK_COMMA && p->tok->tt != TK_RIGHT_PAREN) {
            sol_node_free(n_fun);
            return sol_perr(SOL_ERRP_UNTERMINATED_ARGS);
        }
        if (p->tok->tt == TK_COMMA) ++p->tok;
    }
    ++p->tok;

    if (p->tok->tt != TK_LEFT_BRACE) {
        sol_node_free(n_fun);
        return sol_perr(SOL_ERRP_EXPECTED_BLOCK);
    }
    sol_parse_ex bex = sol_pblock(p);
    if (!bex.is_ok) {
        sol_node_free(n_fun);
        return bex;
    }
    n_fun->n_fun.block = bex.ok;

    return sol_parse_ex_ok(n_fun);
}

sol_parse_ex sol_pasm(sol_parser *p) {
    ++p->tok; // Consume asm
    if (p->tok->tt != TK_LEFT_PAREN)
        return sol_perr(SOL_ERRP_EXPECTED_LPAREN);
    ++p->tok;
    if (p->tok->tt != TK_INTEGER)
        return sol_perr(SOL_ERRP_EXPECTED_INTEGER);
    uint32_t temps = (uint32_t)p->tok->value.i64;
    ++p->tok;
    if (p->tok->tt != TK_RIGHT_PAREN)
        return sol_perr(SOL_ERRP_EXPECTED_RPAREN);
    ++p->tok;

    p->asm = true;
    sol_parse_ex fex = sol_pfun(p);
    p->asm = false;
    if (!fex.is_ok) return fex;

    sol_node *n_asm = malloc(sizeof(sol_node));
    *n_asm = (sol_node){
        .tt = SOL_ND_ASM,
        .line = p->tok->line, .column = p->tok->column,
        .n_asm = {
            .temps = temps,
            .n_fun = fex.ok,
        },
    };
    return sol_parse_ex_ok(n_asm);
}

sol_parse_ex sol_pins(sol_parser *p) {
    sol_opcode op = (sol_opcode)p->tok->value.i64;
    ++p->tok;
    sol_val opa[3] = {SOL_NIL, SOL_NIL, SOL_NIL};
    for (int i = 0; i < (int)(sol_op_info(op)->type) + 1; ++i) {
        sol_parse_ex vex = sol_pprimary(p);
        if (!vex.is_ok) return vex;
        switch (vex.ok->tt) {
            case SOL_ND_IDENTIFIER: opa[i] = vex.ok->n_identifier; break;
            case SOL_ND_LITERAL: {
                if (vex.ok->n_literal.tt != SOL_TI64 && !((op == SOL_OP_LOAD && i == 1) ||
                    (op == SOL_OP_SUPO && i == 1) ||
                    (op == SOL_OP_GUPO && i == 2))) {
                    sol_node_free(vex.ok);
                    return sol_perr(SOL_ERRP_EXPECTED_INTEGER);
                }
                opa[i] = vex.ok->n_literal;
                break;
            }
            default: {
                sol_node_free(vex.ok);
                return sol_perr(SOL_ERRP_EXPECTED_IDENTIFIER);
            }
        }
    }
    if (p->tok->tt != TK_SEMICOLON)
        return sol_perr(SOL_ERRP_EXPECTED_SEMICOLON);
    ++p->tok;

    sol_node *n_ins = malloc(sizeof(sol_node));
    *n_ins = (sol_node){
        .tt = SOL_ND_INS,
        .line = p->tok->line, .column = p->tok->column,
        .n_ins = {
            .op = op,
            .opa = {opa[0], opa[1], opa[2]},
        },
    };
    return sol_parse_ex_ok(n_ins);
}

sol_parse_ex sol_pobj(sol_parser *p) {
    ++p->tok; // Consume {
    sol_node *n_obj = malloc(sizeof(sol_node));
    *n_obj = (sol_node){
        .tt = SOL_ND_FUN,
        .line = p->tok->line, .column = p->tok->column,
        .n_obj = {
            .members = NULL,
        },
    };

    return sol_parse_ex_ok(n_obj);
}

sol_parse_ex sol_pwhile(sol_parser *p) {
    ++p->tok;
    sol_node *n_while = malloc(sizeof(sol_node));
    *n_while = (sol_node){
        .tt = SOL_ND_WHILE,
        .line = p->tok->line, .column = p->tok->column,
        .n_while = {
            .condition = NULL,
            .block = NULL,
        },
    };

    sol_parse_ex ex = sol_pexpr(p, 0);
    if (!ex.is_ok) {
        sol_node_free(n_while);
        return ex;
    }

    n_while->n_while.condition = ex.ok;
    if (ex.ok->tt != SOL_ND_BINARY || !sol_niscondition(ex.ok)) {
        uint16_t line = ex.ok->line;
        uint16_t column = ex.ok->column;
        return sol_parse_ex_err((sol_parse_err){SOL_ERRP_EXPECTED_CONDITION, line, column});
    }

    if (p->tok->tt != TK_LEFT_BRACE) {
        sol_node_free(n_while);
        return sol_perr(SOL_ERRP_EXPECTED_BLOCK);
    }

    ex = sol_pblock(p);
    if (!ex.is_ok) {
        sol_node_free(n_while);
        return ex;
    }
    n_while->n_while.block = ex.ok;

    return sol_parse_ex_ok(n_while);
}

sol_parse_ex sol_preturn(sol_parser *p) {
    uint16_t line = p->tok->line, column = p->tok->column;
    ++p->tok;
    sol_parse_ex expr = sol_pexpr(p, 0);
    if (!expr.is_ok) return expr;
    if (p->tok->tt != TK_SEMICOLON) {
        --p->tok;
        sol_node_free(expr.ok);
        return sol_perr(SOL_ERRP_EXPECTED_SEMICOLON);
    }
    ++p->tok;

    sol_node *n_return = malloc(sizeof(sol_node));
    *n_return = (sol_node){
        .tt = SOL_ND_RETURN,
        .line = line, .column = column,
        .n_return = expr.ok,
    };
    return sol_parse_ex_ok(n_return);
}

sol_parse_ex sol_pstmt(sol_parser *p) {
    if (p->asm && p->tok->tt != TK_OPCODE)
        return sol_perr(SOL_ERRP_EXPECTED_ASM);
    switch (p->tok->tt) {
        case TK_OPCODE: {
            if (!p->asm) return sol_perr(SOL_ERRP_UNEXPECTED_ASM);
            return sol_pins(p);
        }
        case TK_IF: return sol_pif(p);
        case TK_LET: return sol_plet(p);
        case TK_WHILE: return sol_pwhile(p);
        case TK_LEFT_BRACE: case TK_SOF: return sol_pblock(p);
        case TK_RETURN: return sol_preturn(p);
        case TK_IDENTIFIER: {
            sol_parse_ex id = sol_pexpr(p, 0);
            if (!id.is_ok) return id;
            if (p->tok->tt != TK_SEMICOLON) {
                --p->tok;
                return sol_perr(SOL_ERRP_EXPECTED_SEMICOLON);
            }
            ++p->tok;
            return id;
        }
        default: return sol_perr(SOL_ERRP_EXPECTED_STMT);
    };
}

sol_parse_ex sol_parse(sol_tokenvec *tokens) {
    if (tokens->count == 0)
        return sol_parse_ex_err((sol_parse_err){SOL_ERRP_NO_TOKENS, 0, 0});
    sol_parser p = { tokens->data, false };
    return sol_pstmt(&p);
}
