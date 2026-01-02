#include "ctr/syntax.h"
#include "ctr/bytecode.h"
#include "sf/str.h"
#include <stdint.h>
#include <stdlib.h>

void _ctr_tokenvec_cleanup(ctr_tokenvec *vec) {
    for (ctr_token *t = vec->data; t && t < vec->data + vec->count; ++t) {
        if (t->tt == TK_STRING || t->tt == TK_IDENTIFIER)
            ctr_ddel(t->value);
    }
}

void _keywords_foreach(void *_u, sf_str k, ctr_tokentype _v) { (void)_u;(void)_v; sf_str_free(k); }
void _ctr_keywords_cleanup(ctr_keywords *map) {
    ctr_keywords_foreach(map, _keywords_foreach, NULL);
}

typedef struct {
    sf_str src;
    ctr_token current;
    size_t cc;
    ctr_keywords keywords;
} ctr_scanner;

#define ctr_scancase(_c, _tt) case _c: s.current.tt = _tt; break
static inline bool ctr_scanpeek(ctr_scanner *s, char match) {
    if (s->cc + 1 >= s->src.len || s->src.c_str[s->cc+1] != match)
        return false;
    ++s->cc;
    ++s->current.len;
    return true;
}

static inline bool ctr_isnumber(char c) { return c >= '0' && c <= '9'; }
static inline bool ctr_isalphan(char c) { return (c >= 'A' && c <= 'Z')|| (c >= 'a' && c <= 'z') || c == '_' || ctr_isnumber(c); }

ctr_token ctr_scanstr(ctr_scanner *s) {
    bool terminated = false;
    size_t len = 0;
    for (size_t cc = s->cc + 1; cc < s->src.len; ++cc) {
        char c = s->src.c_str[cc];
        if (c == '"') {
            terminated = true;
            break;
        }
        ++len;
    }
    if (!terminated)
        return (ctr_token){TK_NIL, CTR_NIL, 0, 0, 0};

    char *str = calloc(1, len + 1);
    memcpy(str, s->src.c_str + s->cc + 1, len);

    s->cc += len + 1;

    return (ctr_token){
        TK_STRING,
        ctr_dnewstr(sf_own(str)),
        s->current.line,
        s->current.column,
        .len = (uint16_t)len + 2,
    };
}

ctr_token ctr_scannum(ctr_scanner *s) {
    bool is_number = false;
    size_t len = 1;
    for (size_t cc = s->cc + 1; cc < s->src.len; ++cc) {
        if (s->src.c_str[cc] == '.') {
            if (is_number)
                return (ctr_token){TK_NIL, CTR_NIL, 0, 0, 0};
            is_number = true;
        } else if (!ctr_isnumber(s->src.c_str[cc]))
            break;
        ++len;
    }

    char str[len + 1];
    memset(str, 0, sizeof(str));
    memcpy(str, s->src.c_str + s->cc, len);
    s->cc += len - 1;

    ctr_token tok;
    if (is_number)
        tok = (ctr_token) {
            .tt = TK_NUMBER,
            .value = (ctr_val){.f64 = atof(str), .tt = CTR_TF64},
            .line = s->current.line,
            .column = s->current.column,
            .len = (uint16_t)len,
        };
    else
        tok = (ctr_token) {
            .tt = TK_INTEGER,
            .value = (ctr_val){.i64 = atoll(str), .tt = CTR_TI64},
            .line = s->current.line,
            .column = s->current.column,
            .len = (uint16_t)len,
        };
    return tok;
}

ctr_token ctr_scanidentifier(ctr_scanner *s) {
    uint16_t len = 1;
    for (size_t cc = s->cc + 1; cc < s->src.len; ++cc) {
        if (!ctr_isalphan(s->src.c_str[cc]))
            break;
        ++len;
    }

    char str[len + 1];
    memset(str, 0, sizeof(str));
    memcpy(str, s->src.c_str + s->cc, len);

    s->cc += len - 1;

    ctr_keywords_ex ex = ctr_keywords_get(&s->keywords, sf_ref(str));
    if (ex.is_ok) {
        ctr_val value = CTR_NIL;
        if (ex.ok == TK_TRUE)
            value = CTR_TRUE;
        if (ex.ok == TK_FALSE)
            value = CTR_FALSE;
        return (ctr_token) {
            .tt = ex.ok,
            .value = value,
            .line = s->current.line,
            .column = s->current.column,
            .len = len - 1,
        };
    } else {
        char *s2 = calloc(1, len + 1);
        memcpy(s2, str, len);
        return (ctr_token){
            .tt = TK_IDENTIFIER,
            .value = ctr_dnewstr(sf_own(s2)),
            .line = s->current.line,
            .column = s->current.column,
            .len = len - 1,
        };
    }
}

ctr_scan_ex ctr_scan(sf_str src) {
    ctr_tokenvec tks = ctr_tokenvec_new();
    ctr_scanner s = {
        .src = src,
        .current = {TK_EOF, CTR_NIL, 1, 1, 0},
        .cc = 0,
        .keywords = ctr_keywords_new(),
    };
    ctr_error eval = CTR_ERRP_UNEXPECTED_TOKEN;

    ctr_keywords_set(&s.keywords, sf_lit("and"), TK_AND);
    ctr_keywords_set(&s.keywords, sf_lit("or"), TK_OR);
    ctr_keywords_set(&s.keywords, sf_lit("return"), TK_RETURN);
    ctr_keywords_set(&s.keywords, sf_lit("if"), TK_IF);
    ctr_keywords_set(&s.keywords, sf_lit("else"), TK_ELSE);
    ctr_keywords_set(&s.keywords, sf_lit("nil"), TK_NIL);
    ctr_keywords_set(&s.keywords, sf_lit("let"), TK_LET);
    ctr_keywords_set(&s.keywords, sf_lit("for"), TK_FOR);
    ctr_keywords_set(&s.keywords, sf_lit("while"), TK_WHILE);
    ctr_keywords_set(&s.keywords, sf_lit("true"), TK_TRUE);
    ctr_keywords_set(&s.keywords, sf_lit("false"), TK_FALSE);

    ctr_tokenvec_push(&tks, (ctr_token){TK_SOF, CTR_NIL, s.current.line, s.current.column, 1});
    for (; s.cc < src.len; ++s.cc) {
        s.current = (ctr_token){TK_EOF, CTR_NIL, s.current.line, s.current.column, 1};
        char c = src.c_str[s.cc];
        size_t pcc = s.cc;
        switch (c) {
            ctr_scancase('(', TK_LEFT_PAREN);
            ctr_scancase(')', TK_RIGHT_PAREN);
            ctr_scancase('{', TK_LEFT_BRACE);
            ctr_scancase('}', TK_RIGHT_BRACE);
            ctr_scancase('[', TK_LEFT_BRACKET);
            ctr_scancase(']', TK_RIGHT_BRACKET);
            ctr_scancase(',', TK_COMMA);
            ctr_scancase('.', TK_PERIOD);
            ctr_scancase('+', TK_PLUS);
            ctr_scancase(';', TK_SEMICOLON);
            ctr_scancase('*', TK_ASTERISK);
            ctr_scancase('!', ctr_scanpeek(&s, '=') ? TK_NOT_EQUAL : TK_BANG);
            ctr_scancase('<', ctr_scanpeek(&s, '=') ? TK_LESS_EQUAL : TK_LESS);
            ctr_scancase('>', ctr_scanpeek(&s, '=') ? TK_GREATER_EQUAL : TK_GREATER);
            ctr_scancase('=', ctr_scanpeek(&s, '=') ? TK_DOUBLE_EQUAL : TK_EQUAL);

            case '&': {
                if (ctr_scanpeek(&s, '&')) { s.current.len = 2; s.current.tt = TK_AND; break; }
                goto err;
            }
            case '|': {
                if (ctr_scanpeek(&s, '|')) { s.current.len = 2; s.current.tt = TK_OR; break; }
                goto err;
            }
            case '/': {
                if (ctr_scanpeek(&s, '/')) {
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
                s.current = ctr_scanstr(&s);
                if (s.current.tt != TK_STRING) {
                    eval = CTR_ERRP_UNTERMINATED_STR;
                    goto err;
                }
                ctr_tokenvec_push(&tks, s.current);
                s.current.column += s.current.len;
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
                    ctr_tokenvec_push(&tks, s.current);
                    s.current.column += s.current.len;
                    continue;
                }
                if (ctr_isnumber(c) || (c == '-' && ctr_isnumber(s.src.c_str[s.cc + 1]))) { // Number
                    s.current = ctr_scannum(&s);
                    if (s.current.tt != TK_NUMBER && s.current.tt != TK_INTEGER) {
                        eval = CTR_ERRP_NUMBER_FORMAT;
                        goto err;
                    }
                    ctr_tokenvec_push(&tks, s.current);
                    s.current.column += s.current.len;
                    continue;
                } else if (ctr_isalphan(c)) { // Identifier
                    s.current = ctr_scanidentifier(&s);
                    ctr_tokenvec_push(&tks, s.current);
                    s.current.column += s.current.len;
                    continue;
                }
            err: {
                ctr_tokenvec_free(&tks);
                ctr_keywords_free(&s.keywords);
                size_t tk_len = s.cc - pcc + 1;
                for (size_t cc2 = s.cc; cc2 < s.src.len; ++cc2) {
                    char c = s.src.c_str[cc2];
                    if (c == ' ' || c == '\n' || c == '\r' || c == '\n')
                        break;
                    ++tk_len;
                }
                char *str = calloc(1, tk_len + 1);
                memcpy(str, s.src.c_str + pcc, tk_len);
                return ctr_scan_ex_err((ctr_scan_err){eval, sf_own(str), s.current.line, s.current.column});
            }
        }
        s.current.column += s.current.len;
        ctr_tokenvec_push(&tks, s.current);
    }

    ctr_keywords_free(&s.keywords);
    ctr_tokenvec_push(&tks, (ctr_token){TK_EOF, CTR_NIL, s.current.line, s.current.column, 1});
    return ctr_scan_ex_ok(tks);
}

typedef struct { ctr_token *tok; } ctr_parser;

void ctr_node_free(ctr_node *tree) {
    switch (tree->tt) {
        case CTR_ND_MEMBER:
            ctr_node_free(tree->n_postfix.expr);
            ctr_ddel(tree->n_postfix.postfix);
            break;
        case CTR_ND_BINARY:
            ctr_node_free(tree->n_binary.left);
            ctr_node_free(tree->n_binary.right);
            break;
        case CTR_ND_RETURN:
            ctr_node_free(tree->n_return);
            break;
        case CTR_ND_IDENTIFIER:
        case CTR_ND_LITERAL:
            ctr_ddel(tree->n_identifier);
            break;
        case CTR_ND_LET:
            ctr_ddel(tree->n_let.name);
            ctr_node_free(tree->n_let.value);
            break;
        case CTR_ND_ASSIGN:
            ctr_node_free(tree->n_assign.expr);
            ctr_node_free(tree->n_assign.value);
            break;
        case CTR_ND_CALL:
            ctr_node_free(tree->n_call.identifier);
            if (tree->n_call.args) {
                for (size_t i = 0; i < tree->n_call.arg_c; ++i)
                    ctr_node_free(tree->n_call.args[i]);
                free(tree->n_call.args);
            }
            break;
        case CTR_ND_IF:
            ctr_node_free(tree->n_if.condition);
            ctr_node_free(tree->n_if.then_node);
            if (tree->n_if.else_node)
                ctr_node_free(tree->n_if.else_node);
            break;
        case CTR_ND_BLOCK:
            if (tree->n_block.stmts) {
                for (uint32_t i = 0; i < tree->n_block.count; ++i)
                    ctr_node_free(tree->n_block.stmts[i]);
                free(tree->n_block.stmts);
            }
            break;
        case CTR_ND_FUN:
            if (tree->n_fun.captures) {
                for (uint32_t i = 0; i < tree->n_fun.cap_c; ++i)
                    ctr_ddel(tree->n_fun.captures[i]);
                free(tree->n_fun.captures);
            }
            if (tree->n_fun.args) {
                for (uint32_t i = 0; i < tree->n_fun.arg_c; ++i)
                    ctr_ddel(tree->n_fun.args[i]);
                free(tree->n_fun.args);
            }
            if (tree->n_fun.block)
                ctr_node_free(tree->n_fun.block);
            break;
        case CTR_ND_WHILE:
            ctr_node_free(tree->n_while.condition);
            ctr_node_free(tree->n_while.block);
            break;
    }
    free(tree);
}

size_t ctr_precedence(ctr_tokentype tt) {
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
bool ctr_niscondition(ctr_node *node) {
    if (node->tt == CTR_ND_IDENTIFIER ||
        node->tt == CTR_ND_CALL ||
       (node->tt == CTR_ND_LITERAL && node->n_literal.tt == CTR_TBOOL))
        return true;
    if (node->tt != CTR_ND_BINARY)
        return false;
    switch (node->n_binary.op) {
        case TK_OR: case TK_AND: case TK_DOUBLE_EQUAL: case TK_NOT_EQUAL:
        case TK_LESS: case TK_LESS_EQUAL: case TK_GREATER: case TK_GREATER_EQUAL:
            return true;
        default: return false;
    }
}

static inline bool ctr_parpeek(ctr_parser *p, ctr_tokentype match) {
    if (p->tok->tt != match)
        return false;
    ++p->tok;
    return true;
}

// Convenience err macro
#define ctr_perr(type) ctr_parse_ex_err((ctr_parse_err){(type), p->tok->line, p->tok->column})

ctr_parse_ex ctr_pprimary(ctr_parser *p);
ctr_parse_ex ctr_pexpr(ctr_parser *p, size_t prec);
ctr_parse_ex ctr_pif(ctr_parser *p);
ctr_parse_ex ctr_plet(ctr_parser *p);
ctr_parse_ex ctr_ppostfix(ctr_parser *p);
ctr_parse_ex ctr_pblock(ctr_parser *p);
ctr_parse_ex ctr_pfun(ctr_parser *p);
ctr_parse_ex ctr_pobj(ctr_parser *p);
ctr_parse_ex ctr_pwhile(ctr_parser *p);
ctr_parse_ex ctr_preturn(ctr_parser *p);
ctr_parse_ex ctr_pstmt(ctr_parser *p);

ctr_parse_ex ctr_pprimary(ctr_parser *p) {
    switch (p->tok->tt) {
        case TK_INTEGER: case TK_NUMBER: case TK_STRING: case TK_TRUE: case TK_FALSE: case TK_NIL: {
            ctr_node *n = malloc(sizeof(ctr_node));
            *n = (ctr_node){
                p->tok->tt == TK_IDENTIFIER ? CTR_ND_IDENTIFIER : CTR_ND_LITERAL,
                p->tok->line, p->tok->column,
                .n_literal = ctr_dref(p->tok->value),
            };
            ++p->tok;
            return ctr_parse_ex_ok(n);
        }
        case TK_LEFT_BRACKET: return ctr_pfun(p);
        case TK_LEFT_BRACE: return ctr_pobj(p);
        case TK_IDENTIFIER: {
            ctr_node *n = malloc(sizeof(ctr_node));
            *n = (ctr_node){
                p->tok->tt == TK_IDENTIFIER ? CTR_ND_IDENTIFIER : CTR_ND_LITERAL,
                p->tok->line, p->tok->column,
                .n_identifier = ctr_dref(p->tok->value),
            };
            ++p->tok;
            return ctr_parse_ex_ok(n);
        }
        case TK_LEFT_PAREN: {
            ++p->tok;
            ctr_parse_ex ex = ctr_pexpr(p, 0);
            if (!ex.is_ok) return ex;
            if (p->tok->tt != TK_RIGHT_PAREN)
                return ctr_perr(CTR_ERRP_EXPECTED_RPAREN);
            ++p->tok;
            return ctr_parse_ex_ok(ex.ok);
        }
        default:
            return ctr_perr(CTR_ERRP_EXPECTED_EXPRESSION);
    }
}

ctr_parse_ex ctr_pexpr(ctr_parser *p, size_t prec) {
    ctr_parse_ex ex = ctr_ppostfix(p);
    if (!ex.is_ok) return ex;

    ctr_node *left = ex.ok;
    while (true) {
        ctr_token *op = p->tok;
        size_t op_prec = ctr_precedence(op->tt);
        if (op_prec == SIZE_MAX || op_prec < prec)
            break;
        ++p->tok;

        ex = ctr_pexpr(p, (op->tt == TK_EQUAL) ? op_prec : op_prec + 1);
        if (!ex.is_ok) {
            ctr_node_free(left);
            return ex;
        }
        ctr_node *bin = malloc(sizeof(ctr_node));
        *bin = (ctr_node){
            .tt = CTR_ND_BINARY,
            .line = op->line, .column = op->column,
            .n_binary = {
                .op = op->tt,
                .left = left,
                .right = ex.ok,
            },
        };
        left = bin;
    }
    return ctr_parse_ex_ok(left);
}

ctr_parse_ex ctr_pif(ctr_parser *p) {
    ctr_token *tk_if = p->tok++;
    ctr_parse_ex cex = ctr_pexpr(p, 0);
    if (!cex.is_ok) return cex;
    if (!ctr_niscondition(cex.ok)) {
        uint16_t line = cex.ok->line;
        uint16_t column = cex.ok->column;
        ctr_node_free(cex.ok);
        return ctr_parse_ex_err((ctr_parse_err){CTR_ERRP_EXPECTED_CONDITION, line, column});
    }

    if (p->tok->tt != TK_LEFT_BRACE) {
        ctr_node_free(cex.ok);
        return ctr_perr(CTR_ERRP_EXPECTED_BLOCK);
    }
    ctr_parse_ex tex = ctr_pblock(p);
    if (!tex.is_ok) {
        ctr_node_free(cex.ok);
        return tex;
    }
    ctr_parse_ex eex = (ctr_parse_ex){.is_ok = false};
    if (p->tok->tt == TK_ELSE) {
        ++p->tok;
        if (p->tok->tt != TK_LEFT_BRACE) {
            ctr_node_free(cex.ok);
            ctr_node_free(tex.ok);
            return ctr_perr(CTR_ERRP_EXPECTED_BLOCK);
        }
        eex = ctr_pblock(p);
        if (!eex.is_ok) {
            ctr_node_free(cex.ok);
            ctr_node_free(tex.ok);
            return eex;
        }
    }

    ctr_node *n_if = malloc(sizeof(ctr_node));
    *n_if = (ctr_node){
        .tt = CTR_ND_IF,
        .line = tk_if->line, .column = tk_if->column,
        .n_if = {
            .condition = cex.ok,
            .then_node = tex.ok,
            .else_node = eex.is_ok ? eex.ok : NULL,
        },
    };
    return ctr_parse_ex_ok(n_if);
}

ctr_parse_ex ctr_plet(ctr_parser *p) {
    ctr_token *let = p->tok++;

    if (p->tok->tt != TK_IDENTIFIER)
        return ctr_perr(CTR_ERRP_EXPECTED_IDENTIFIER);
    ctr_token *name = p->tok++;

    if (p->tok->tt != TK_EQUAL)
        return ctr_perr(CTR_ERRP_EXPECTED_EQUAL);
    ++p->tok;

    ctr_parse_ex vex = ctr_pexpr(p, 0);
    if (!vex.is_ok) return vex;
    if (p->tok->tt != TK_SEMICOLON) {
        --p->tok;
        ctr_node_free(vex.ok);
        return ctr_perr(CTR_ERRP_EXPECTED_SEMICOLON);
    }
    ++p->tok;

    ctr_node *n_let = malloc(sizeof(ctr_node));
    *n_let = (ctr_node){
        .tt = CTR_ND_LET,
        .line = let->line, .column = let->column,
        .n_let = {
            .name = ctr_dref(name->value),
            .value = vex.ok,
        }
    };
    return ctr_parse_ex_ok(n_let);
}

ctr_parse_ex ctr_ppostfix(ctr_parser *p) {
    ctr_parse_ex ex = ctr_pprimary(p);
    if (!ex.is_ok) return ex;
    ctr_node *node = ex.ok;

    while (true) {
        if (p->tok->tt == TK_LEFT_PAREN) {
            ctr_node *call = malloc(sizeof(ctr_node));
            *call = (ctr_node){
                .tt = CTR_ND_CALL,
                .line = p->tok->line,
                .column = p->tok->column,
                .n_call = {
                    .identifier = node,
                    .args = NULL,
                    .arg_c = 0,
                }
            };
            ++p->tok;

            while (p->tok->tt != TK_RIGHT_PAREN && p->tok->tt != TK_EOF) {
                ctr_parse_ex arg = ctr_pexpr(p, 0);
                if (!arg.is_ok) {
                    ctr_node_free(call);
                    return arg;
                }
                call->n_call.args = realloc(call->n_call.args, sizeof(ctr_node*) * (++call->n_call.arg_c));
                call->n_call.args[call->n_call.arg_c - 1] = arg.ok;
                if (p->tok->tt == TK_COMMA)
                    ++p->tok;
                else break;
            }
            if (p->tok->tt != TK_RIGHT_PAREN)
                return ctr_perr(CTR_ERRP_UNTERMINATED_ARGS);

            ++p->tok;
            node = call;
            continue;
        }

        if (p->tok->tt == TK_PERIOD) {
            ++p->tok;
            if (p->tok->tt != TK_IDENTIFIER)
                return ctr_perr(CTR_ERRP_EXPECTED_IDENTIFIER);

            ctr_node *member = malloc(sizeof(ctr_node));
            *member = (ctr_node){
                .tt = CTR_ND_MEMBER,
                .line = p->tok->line,
                .column = p->tok->column,
                .n_postfix = {
                    .expr = node,
                    .postfix = ctr_dref(p->tok->value),
                }
            };
            ++p->tok;
            node = member;
            continue;
        }
        break;
    }

    return ctr_parse_ex_ok(node);
}

ctr_parse_ex ctr_pblock(ctr_parser *p) {
    ctr_node *n_block = malloc(sizeof(ctr_node));
    *n_block = (ctr_node){
        .tt = CTR_ND_BLOCK,
        .line = p->tok->line, .column = p->tok->column,
        .n_block = {
            .stmts = NULL,
            .count = 0,
        },
    };
    ctr_tokentype st = p->tok->tt;
    ++p->tok;
    while (p->tok->tt != TK_RIGHT_BRACE && p->tok->tt != TK_EOF) {
        ctr_parse_ex sex = ctr_pstmt(p); // HHAHAHAHHAHHAHAHHAH
        if (!sex.is_ok) {
            ctr_node_free(n_block);
            return sex;
        }
        n_block->n_block.stmts = realloc(n_block->n_block.stmts, ++n_block->n_block.count * sizeof(ctr_node *));
        n_block->n_block.stmts[n_block->n_block.count - 1] = sex.ok;
    }

    if (st == TK_LEFT_BRACE && p->tok->tt != TK_RIGHT_BRACE) {
        ctr_node_free(n_block);
        return ctr_perr(CTR_ERRP_EXPECTED_RBRACE);
    }
    ++p->tok;

    return ctr_parse_ex_ok(n_block);
}

ctr_parse_ex ctr_pfun(ctr_parser *p) {
    ++p->tok; // Consume [
    ctr_node *n_fun = malloc(sizeof(ctr_node));
    *n_fun = (ctr_node){
        .tt = CTR_ND_FUN,
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
            ctr_node_free(n_fun);
            return ctr_perr(CTR_ERRP_EXPECTED_IDENTIFIER);
        }
        n_fun->n_fun.captures = realloc(n_fun->n_fun.captures, (++n_fun->n_fun.cap_c) * sizeof(ctr_val));
        n_fun->n_fun.captures[n_fun->n_fun.cap_c - 1] = ctr_dref(p->tok->value);
        ++p->tok;

        if (p->tok->tt != TK_COMMA && p->tok->tt != TK_RIGHT_BRACKET) {
            ctr_node_free(n_fun);
            return ctr_perr(CTR_ERRP_UNTERMINATED_CAPTURES);
        }
        if (p->tok->tt == TK_COMMA) ++p->tok;
    }
    ++p->tok;

    if (p->tok->tt != TK_LEFT_PAREN) {
        ctr_node_free(n_fun);
        return ctr_perr(CTR_ERRP_EXPECTED_ARGS);
    }
    ++p->tok;
    while (p->tok->tt != TK_RIGHT_PAREN && p->tok->tt != TK_EOF) {
        if (p->tok->tt != TK_IDENTIFIER) {
            ctr_node_free(n_fun);
            return ctr_perr(CTR_ERRP_EXPECTED_IDENTIFIER);
        }
        n_fun->n_fun.args = realloc(n_fun->n_fun.args, ++n_fun->n_fun.arg_c * sizeof(ctr_val));
        n_fun->n_fun.args[n_fun->n_fun.arg_c - 1] = ctr_dref(p->tok->value);
        ++p->tok;

        if (p->tok->tt != TK_COMMA && p->tok->tt != TK_RIGHT_PAREN) {
            ctr_node_free(n_fun);
            return ctr_perr(CTR_ERRP_UNTERMINATED_ARGS);
        }
        if (p->tok->tt == TK_COMMA) ++p->tok;
    }
    ++p->tok;

    if (p->tok->tt != TK_LEFT_BRACE) {
        ctr_node_free(n_fun);
        return ctr_perr(CTR_ERRP_EXPECTED_BLOCK);
    }
    ctr_parse_ex bex = ctr_pblock(p);
    if (!bex.is_ok) {
        ctr_node_free(n_fun);
        return bex;
    }
    n_fun->n_fun.block = bex.ok;

    return ctr_parse_ex_ok(n_fun);
}

ctr_parse_ex ctr_pobj(ctr_parser *p) {
    ++p->tok; // Consume {
    ctr_node *n_obj = malloc(sizeof(ctr_node));
    *n_obj = (ctr_node){
        .tt = CTR_ND_FUN,
        .line = p->tok->line, .column = p->tok->column,
        .n_obj = {
            .members = NULL,
        },
    };

    return ctr_parse_ex_ok(n_obj);
}

ctr_parse_ex ctr_pwhile(ctr_parser *p) {
    ++p->tok;
    ctr_node *n_while = malloc(sizeof(ctr_node));
    *n_while = (ctr_node){
        .tt = CTR_ND_WHILE,
        .line = p->tok->line, .column = p->tok->column,
        .n_while = {
            .condition = NULL,
            .block = NULL,
        },
    };

    ctr_parse_ex ex = ctr_pexpr(p, 0);
    if (!ex.is_ok) {
        ctr_node_free(n_while);
        return ex;
    }

    n_while->n_while.condition = ex.ok;
    if (ex.ok->tt != CTR_ND_BINARY || !ctr_niscondition(ex.ok)) {
        uint16_t line = ex.ok->line;
        uint16_t column = ex.ok->column;
        ctr_parse_ex_err((ctr_parse_err){CTR_ERRP_EXPECTED_CONDITION, line, column});
    }

    if (p->tok->tt != TK_LEFT_BRACE) {
        ctr_node_free(n_while);
        return ctr_perr(CTR_ERRP_EXPECTED_BLOCK);
    }

    ex = ctr_pblock(p);
    if (!ex.is_ok) {
        ctr_node_free(n_while);
        return ex;
    }
    n_while->n_while.block = ex.ok;

    return ctr_parse_ex_ok(n_while);
}

ctr_parse_ex ctr_preturn(ctr_parser *p) {
    ++p->tok;
    ctr_parse_ex expr = ctr_pexpr(p, 0);
    if (!expr.is_ok) return expr;
    if (p->tok->tt != TK_SEMICOLON) {
        --p->tok;
        ctr_node_free(expr.ok);
        return ctr_perr(CTR_ERRP_EXPECTED_SEMICOLON);
    }
    ++p->tok;

    ctr_node *n_return = malloc(sizeof(ctr_node));
    *n_return = (ctr_node){
        .tt = CTR_ND_RETURN,
        .line = (p->tok - 1)->line, .column = (p->tok - 1)->column,
        .n_return = expr.ok,
    };
    return ctr_parse_ex_ok(n_return);
}

ctr_parse_ex ctr_pstmt(ctr_parser *p) {
    switch (p->tok->tt) {
        case TK_IF: return ctr_pif(p);
        case TK_LET: return ctr_plet(p);
        case TK_WHILE: return ctr_pwhile(p);
        case TK_LEFT_BRACE: case TK_SOF: return ctr_pblock(p);
        case TK_RETURN: return ctr_preturn(p);
        case TK_IDENTIFIER: {
            ctr_parse_ex id = ctr_pexpr(p, 0);
            if (!id.is_ok) return id;
            if (p->tok->tt != TK_SEMICOLON) {
                --p->tok;
                return ctr_perr(CTR_ERRP_EXPECTED_SEMICOLON);
            }
            ++p->tok;
            return id;
        }
        default: return ctr_perr(CTR_ERRP_EXPECTED_STMT);
    };
}

ctr_parse_ex ctr_parse(ctr_tokenvec *tokens) {
    if (tokens->count == 0)
        return ctr_parse_ex_err((ctr_parse_err){CTR_ERRP_NO_TOKENS, 0, 0});
    ctr_parser p = { tokens->data };
    return ctr_pstmt(&p);
}
