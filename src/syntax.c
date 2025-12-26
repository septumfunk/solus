#include "ctr/syntax.h"
#include "ctr/bytecode.h"
#include "sf/str.h"
#include <stdlib.h>

void _ctr_tokenvec_cleanup(ctr_tokenvec *vec) {
    for (ctr_token *t = vec->data; t && t < vec->top + 1; ++t) {
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
    ++s->current.column;
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
        return (ctr_token){TK_NIL, CTR_NIL, 0, 0};

    char *str = calloc(1, len + 1);
    memcpy(str, s->src.c_str + s->cc + 1, len);
    s->cc += len + 1;
    s->current.column += len + 1;
    ctr_token tok = {
        TK_STRING,
        ctr_dnew(CTR_DSTR),
        s->current.line,
        s->current.column,
    };
    *(sf_str *)tok.value.val.dyn = sf_own(str);

    return tok;
}

ctr_token ctr_scannum(ctr_scanner *s) {
    bool is_number = false;
    size_t len = 1;
    for (size_t cc = s->cc + 1; cc < s->src.len; ++cc) {
        if (s->src.c_str[cc] == '.') {
            if (is_number)
                return (ctr_token){TK_NIL, CTR_NIL, 0, 0};
            is_number = true;
        } else if (!ctr_isnumber(s->src.c_str[cc]))
            break;
        ++len;
    }

    char str[len + 1];
    memset(str, 0, sizeof(str));
    memcpy(str, s->src.c_str + s->cc, len);
    s->cc += len - 1;
    s->current.column += len;

    ctr_token tok;
    if (is_number)
        tok = (ctr_token) {
            .tt = TK_NUMBER,
            .value = (ctr_val){.val.f64 = atof(str), .tt = CTR_TF64},
            .line = s->current.line,
            .column = s->current.column,
        };
    else
        tok = (ctr_token) {
            .tt = TK_INTEGER,
            .value = (ctr_val){.val.i64 = atoll(str), .tt = CTR_TI64},
            .line = s->current.line,
            .column = s->current.column,
        };
    return tok;
}

ctr_token ctr_scanidentifier(ctr_scanner *s) {
    size_t len = 1;
    for (size_t cc = s->cc + 1; cc < s->src.len; ++cc) {
        if (!ctr_isalphan(s->src.c_str[cc]))
            break;
        ++len;
    }

    char str[len + 1];
    memset(str, 0, sizeof(str));
    memcpy(str, s->src.c_str + s->cc, len);

    s->cc += len - 1;
    s->current.column += len - 1;

    ctr_keywords_ex ex = ctr_keywords_get(&s->keywords, sf_ref(str));
    if (ex.is_ok) return (ctr_token) {
            .tt = ex.value.ok,
            .value = CTR_NIL,
            .line = s->current.line,
            .column = s->current.line,
        };
    else {
        char *s2 = calloc(1, len + 1);
        memcpy(s2, str, len);
        ctr_token tok = {
            TK_IDENTIFIER,
            ctr_dnew(CTR_DSTR),
            s->current.line,
            s->current.column,
        };
        *(sf_str *)tok.value.val.dyn = sf_own(s2);
        return tok;
    }
}

ctr_scan_ex ctr_scan(sf_str src) {
    ctr_tokenvec tks = ctr_tokenvec_new();
    ctr_scanner s = {
        .src = src,
        .current = {TK_EOF, CTR_NIL, 1, 1},
        .cc = 0,
        .keywords = ctr_keywords_new(),
    };
    enum ctr_scan_errt eval = CTR_ERR_UNEXPECTED_TOKEN;

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

    ctr_tokenvec_push(&tks, (ctr_token){TK_SOF, CTR_NIL, s.current.line, s.current.column});
    for (; s.cc < src.len; ++s.cc, ++s.current.column) {
        s.current = (ctr_token){TK_EOF, CTR_NIL, s.current.line, s.current.column};
        char c = src.c_str[s.cc];
        size_t pcc = s.cc;
        switch (c) {
            ctr_scancase('(', TK_LEFT_PAREN);
            ctr_scancase(')', TK_RIGHT_PAREN);
            ctr_scancase('{', TK_LEFT_BRACE);
            ctr_scancase('}', TK_RIGHT_BRACE);
            ctr_scancase(',', TK_COMMA);
            ctr_scancase('.', TK_PERIOD);
            ctr_scancase('-', TK_MINUS);
            ctr_scancase('+', TK_PLUS);
            ctr_scancase(';', TK_SEMICOLON);
            ctr_scancase('*', TK_ASTERISK);
            ctr_scancase('!', ctr_scanpeek(&s, '=') ? TK_NOT_EQUAL : TK_BANG);
            ctr_scancase('<', ctr_scanpeek(&s, '=') ? TK_LESS_EQUAL : TK_LESS);
            ctr_scancase('>', ctr_scanpeek(&s, '=') ? TK_GREATER_EQUAL : TK_GREATER);

            case '=': {
                if (ctr_scanpeek(&s, '=')) { s.current.tt = TK_DOUBLE_EQUAL; break; }
                if (ctr_scanpeek(&s, '>')) { s.current.tt = TK_FAT_ARROW; break; }
                s.current.tt = TK_EQUAL;
                break;
            }
            case '&': {
                if (ctr_scanpeek(&s, '&')) { s.current.tt = TK_AND; break; }
                goto err;
            }
            case '|': {
                if (ctr_scanpeek(&s, '|')) { s.current.tt = TK_OR; break; }
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
            case ' ': case '\r': case '\t': continue;
            case '"': {
                s.current = ctr_scanstr(&s);
                if (s.current.tt != TK_STRING) {
                    eval = CTR_ERR_UNTERMINATED_STR;
                    goto err;
                }
                ctr_tokenvec_push(&tks, s.current);
                continue;
            }

            default:
                if (ctr_isnumber(c)) { // Number
                    s.current = ctr_scannum(&s);
                    if (s.current.tt != TK_NUMBER && s.current.tt != TK_INTEGER) {
                        eval = CTR_ERR_NUMBER_FORMAT;
                        goto err;
                    }
                    ctr_tokenvec_push(&tks, s.current);
                    continue;
                } else if (ctr_isalphan(c)) { // Identifier
                    s.current = ctr_scanidentifier(&s);
                    ctr_tokenvec_push(&tks, s.current);
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
        ctr_tokenvec_push(&tks, s.current);
    }

    ctr_keywords_free(&s.keywords);
    ctr_tokenvec_push(&tks, (ctr_token){TK_EOF, CTR_NIL, s.current.line, s.current.column});
    return ctr_scan_ex_ok(tks);
}

typedef struct { ctr_token *tok; } ctr_parser;

void ctr_node_free(ctr_node *tree) {
    switch (tree->tt) {
        case ND_BINARY:
            ctr_node_free(tree->statement.binary.left);
            ctr_node_free(tree->statement.binary.right);
            break;
        case ND_UNARY:
            ctr_node_free(tree->statement.unary.expr);
            break;
        case ND_RETURN:
            ctr_node_free(tree->statement.stmt_ret.expr);
            break;
        case ND_IDENTIFIER:
        case ND_LITERAL:
            ctr_ddel(tree->statement.identifier);
            break;
        case ND_LET:
            ctr_ddel(tree->statement.stmt_let.name);
            ctr_node_free(tree->statement.stmt_let.value);
            break;
        case ND_ASSIGN:
            ctr_ddel(tree->statement.stmt_assign.name);
            ctr_node_free(tree->statement.stmt_assign.value);
            break;
        case ND_CALL:
            ctr_ddel(tree->statement.stmt_call.name);
            if (tree->statement.stmt_call.args) {
                for (size_t i = 0; i < tree->statement.stmt_call.arg_c; ++i)
                    ctr_node_free(tree->statement.stmt_call.args[i]);
                free(tree->statement.stmt_call.args);
            }
            break;
        case ND_IF:
            ctr_node_free(tree->statement.stmt_if.condition);
            ctr_node_free(tree->statement.stmt_if.then_node);
            if (tree->statement.stmt_if.else_node)
                ctr_node_free(tree->statement.stmt_if.else_node);
            break;
        case ND_BLOCK:
            if (tree->statement.block.stmts) {
                for (size_t i = 0; i < tree->statement.block.count; ++i)
                    ctr_node_free(tree->statement.block.stmts[i]);
                free(tree->statement.block.stmts);
            }
            break;
    }
    free(tree);
}

size_t ctr_precedence(ctr_tokentype tt) {
    switch (tt) {
        case TK_FAT_ARROW: return 1;
        case TK_OR: return 2;
        case TK_AND: return 3;
        case TK_DOUBLE_EQUAL:
        case TK_NOT_EQUAL: return 4;
        case TK_LESS: case TK_LESS_EQUAL:
        case TK_GREATER: case TK_GREATER_EQUAL: return 5;
        case TK_PLUS: case TK_MINUS: return 6;
        case TK_ASTERISK: case TK_SLASH: return 7;
        default: return SIZE_MAX;
    }
}
bool ctr_lassoc(ctr_tokentype tt) {
    switch (tt) {
        case TK_FAT_ARROW: return false;
        default: return true;
    }
}
bool ctr_iscondition(ctr_tokentype tt) {
    switch (tt) {
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

ctr_parse_ex ctr_parprim(ctr_parser *p);
ctr_parse_ex ctr_parbin(ctr_parser *p, size_t prec);
ctr_parse_ex ctr_parif(ctr_parser *p);
ctr_parse_ex ctr_parlet(ctr_parser *p);
ctr_parse_ex ctr_parassign(ctr_parser *p);
ctr_parse_ex ctr_parcall(ctr_parser *p);
ctr_parse_ex ctr_parblock(ctr_parser *p);
ctr_parse_ex ctr_parreturn(ctr_parser *p);
ctr_parse_ex ctr_parstmt(ctr_parser *p);

ctr_parse_ex ctr_parprim(ctr_parser *p) {
    switch (p->tok->tt) {
        case TK_INTEGER: case TK_NUMBER: case TK_STRING: case TK_TRUE: case TK_FALSE: case TK_NIL: {
            ctr_node *n = malloc(sizeof(ctr_node));
            *n = (ctr_node){
                p->tok->tt == TK_IDENTIFIER ? ND_IDENTIFIER : ND_LITERAL,
                p->tok->line, p->tok->column,
                .statement = (union ctr_statement){.literal = ctr_dref(p->tok->value)},
            };
            ++p->tok;
            return ctr_parse_ex_ok(n);
        }
        case TK_IDENTIFIER: {
            if ((p->tok + 1)->tt == TK_LEFT_PAREN) { // Call
                ctr_parse_ex cex = ctr_parcall(p);
                if (!cex.is_ok) return cex;
                return ctr_parse_ex_ok(cex.value.ok);
            } else { // Identifier
                ctr_node *n = malloc(sizeof(ctr_node));
                *n = (ctr_node){
                    p->tok->tt == TK_IDENTIFIER ? ND_IDENTIFIER : ND_LITERAL,
                    p->tok->line, p->tok->column,
                    .statement = (union ctr_statement){.identifier = ctr_dref(p->tok->value)},
                };
                ++p->tok;
                return ctr_parse_ex_ok(n);
            }
            break;
        }
        case TK_LEFT_PAREN: {
            ++p->tok;
            ctr_parse_ex ex = ctr_parbin(p, 0);
            if (!ex.is_ok) return ex;
            if (p->tok->tt != TK_RIGHT_PAREN)
                return ctr_parse_ex_err((ctr_parse_err){CTR_ERR_EXPECTED_RPAREN, p->tok->line, p->tok->column});
            ++p->tok;
            return ctr_parse_ex_ok(ex.value.ok);
        }
        default:
            return ctr_parse_ex_err((ctr_parse_err){CTR_ERR_EXPECTED_EXPRESSION, p->tok->line, p->tok->column});
    }
}

ctr_parse_ex ctr_parbin(ctr_parser *p, size_t prec) {
    ctr_parse_ex ex = ctr_parprim(p);
    if (!ex.is_ok) return ex;

    ctr_node *left = ex.value.ok;
    while (true) {
        ctr_token *op = p->tok;
        size_t op_prec = ctr_precedence(op->tt);
        if (op_prec == SIZE_MAX || op_prec < prec)
            break;
        ++p->tok;

        ex = op->tt == TK_FAT_ARROW ?
            ctr_parblock(p) :
            ctr_parbin(p, ctr_lassoc(op->tt) ? op_prec + 1 : op_prec);
        if (!ex.is_ok) {
            ctr_node_free(left);
            return ex;
        }
        ctr_node *bin = malloc(sizeof(ctr_node));
        *bin = (ctr_node){
            .tt = ND_BINARY,
            .line = op->line, .column = op->column,
            .statement.binary = {
                .tt = op->tt,
                .left = left,
                .right = ex.value.ok,
            },
        };
        left = bin;
    }
    return ctr_parse_ex_ok(left);
}

ctr_parse_ex ctr_parif(ctr_parser *p) {
    ctr_token *tk_if = p->tok++;
    ctr_parse_ex cex = ctr_parbin(p, 0);
    if (!cex.is_ok) return cex;
    if (cex.value.ok->tt != ND_BINARY || !ctr_iscondition(cex.value.ok->statement.binary.tt)) {
        size_t line = cex.value.ok->line;
        size_t column = cex.value.ok->column;
        ctr_node_free(cex.value.ok);
        return ctr_parse_ex_err((ctr_parse_err){CTR_ERR_EXPECTED_CONDITION, line, column});
    }

    if (p->tok->tt != TK_LEFT_BRACE) {
        ctr_node_free(cex.value.ok);
        return ctr_parse_ex_err((ctr_parse_err){CTR_ERR_EXPECTED_BLOCK, p->tok->line, p->tok->column});
    }
    ctr_parse_ex tex = ctr_parblock(p);
    if (!tex.is_ok) {
        ctr_node_free(cex.value.ok);
        return tex;
    }
    ctr_parse_ex eex = (ctr_parse_ex){.is_ok = false};
    if (p->tok->tt == TK_ELSE) {
        ++p->tok;
        if (p->tok->tt != TK_LEFT_BRACE) {
            ctr_node_free(cex.value.ok);
            ctr_node_free(tex.value.ok);
            return ctr_parse_ex_err((ctr_parse_err){CTR_ERR_EXPECTED_BLOCK, p->tok->line, p->tok->column});
        }
        eex = ctr_parblock(p);
        if (!eex.is_ok) {
            ctr_node_free(cex.value.ok);
            ctr_node_free(tex.value.ok);
            return eex;
        }
    }

    ctr_node *n_if = malloc(sizeof(ctr_node));
    *n_if = (ctr_node){
        .tt = ND_IF,
        .line = tk_if->line, .column = tk_if->column,
        .statement.stmt_if = {
            .condition = cex.value.ok,
            .then_node = tex.value.ok,
            .else_node = eex.is_ok ? eex.value.ok : NULL,
        },
    };
    return ctr_parse_ex_ok(n_if);
}

ctr_parse_ex ctr_parlet(ctr_parser *p) {
    ctr_token *let = p->tok++;

    if (p->tok->tt != TK_IDENTIFIER)
        return ctr_parse_ex_err((ctr_parse_err){CTR_ERR_EXPECTED_IDENTIFIER, p->tok->line, p->tok->column});
    ctr_token *name = p->tok++;

    if (p->tok->tt != TK_EQUAL)
        return ctr_parse_ex_err((ctr_parse_err){CTR_ERR_EXPECTED_EQUAL, p->tok->line, p->tok->column});
    ++p->tok;

    ctr_parse_ex vex = ctr_parbin(p, 0);
    if (!vex.is_ok) return vex;
    if (p->tok->tt != TK_SEMICOLON) {
        ctr_node_free(vex.value.ok);
        return ctr_parse_ex_err((ctr_parse_err){CTR_ERR_EXPECTED_SEMICOLON, p->tok->line, p->tok->column});
    }
    ++p->tok;

    ctr_node *n_let = malloc(sizeof(ctr_node));
    *n_let = (ctr_node){
        .tt = ND_LET,
        .line = let->line, .column = let->column,
        .statement.stmt_let = {
            .name = ctr_dref(name->value),
            .value = vex.value.ok,
        }
    };
    return ctr_parse_ex_ok(n_let);
}

ctr_parse_ex ctr_parassign(ctr_parser *p) {
    if (p->tok->tt != TK_IDENTIFIER)
        return ctr_parse_ex_err((ctr_parse_err){CTR_ERR_EXPECTED_IDENTIFIER, p->tok->line, p->tok->column});
    ctr_token *name = p->tok++;

    if (p->tok->tt != TK_EQUAL)
        return ctr_parse_ex_err((ctr_parse_err){CTR_ERR_EXPECTED_EQUAL, p->tok->line, p->tok->column});
    ++p->tok;

    ctr_parse_ex vex = ctr_parbin(p, 0);
    if (!vex.is_ok) return vex;
    if (p->tok->tt != TK_SEMICOLON) {
        ctr_node_free(vex.value.ok);
        return ctr_parse_ex_err((ctr_parse_err){CTR_ERR_EXPECTED_SEMICOLON, p->tok->line, p->tok->column});
    }
    ++p->tok;

    ctr_node *n_assign = malloc(sizeof(ctr_node));
    *n_assign = (ctr_node){
        .tt = ND_ASSIGN,
        .line = name->line, .column = name->column,
        .statement.stmt_assign = {
            .name = ctr_dref(name->value),
            .value = vex.value.ok,
        }
    };
    return ctr_parse_ex_ok(n_assign);
}

ctr_parse_ex ctr_parcall(ctr_parser *p) {
    if (p->tok->tt != TK_IDENTIFIER)
        return ctr_parse_ex_err((ctr_parse_err){CTR_ERR_EXPECTED_IDENTIFIER, p->tok->line, p->tok->column});
    ctr_token *name = p->tok++;

    if (p->tok->tt != TK_LEFT_PAREN)
        return ctr_parse_ex_err((ctr_parse_err){CTR_ERR_EXPECTED_LPAREN, p->tok->line, p->tok->column});
    ++p->tok;

    ctr_node *n_call = malloc(sizeof(ctr_node));
    *n_call = (ctr_node){
        .tt = ND_CALL,
        .line = name->line, .column = name->column,
        .statement.stmt_call = {
            .name = ctr_dref(name->value),
            .args = NULL,
            .arg_c = 0,
        },
    };

    while (p->tok->tt != TK_RIGHT_PAREN && p->tok->tt != TK_EOF) {
        ctr_parse_ex arg = ctr_parbin(p, 0);
        if (!arg.is_ok) {
            ctr_node_free(n_call);
            return arg;
        }
        n_call->statement.stmt_call.args = realloc(n_call->statement.stmt_call.args, ++n_call->statement.stmt_call.arg_c * sizeof(ctr_node *));
        n_call->statement.stmt_call.args[n_call->statement.stmt_call.arg_c - 1] = arg.value.ok;
        if (p->tok->tt != TK_COMMA && p->tok->tt != TK_RIGHT_PAREN) {
            ctr_node_free(n_call);
            return ctr_parse_ex_err((ctr_parse_err){CTR_ERR_UNTERMINATED_ARGS, p->tok->line, p->tok->column});
        }
    }
    ++p->tok;

    if (p->tok->tt != TK_SEMICOLON) {
        ctr_node_free(n_call);
        return ctr_parse_ex_err((ctr_parse_err){CTR_ERR_EXPECTED_SEMICOLON, p->tok->line, p->tok->column});
    }
    ++p->tok;

    return ctr_parse_ex_ok(n_call);
}

ctr_parse_ex ctr_parblock(ctr_parser *p) {
    ctr_node *n_block = malloc(sizeof(ctr_node));
    *n_block = (ctr_node){
        .tt = ND_BLOCK,
        .line = p->tok->line, .column = p->tok->column,
        .statement.block = {
            .stmts = NULL,
            .count = 0,
        },
    };
    ctr_tokentype st = p->tok->tt;
    ++p->tok;
    while (p->tok->tt != TK_RIGHT_BRACE && p->tok->tt != TK_EOF) {
        ctr_parse_ex sex = ctr_parstmt(p); // HHAHAHAHHAHHAHAHHAH
        if (!sex.is_ok) {
            ctr_node_free(n_block);
            return sex;
        }
        n_block->statement.block.stmts = realloc(n_block->statement.block.stmts, ++n_block->statement.block.count * sizeof(ctr_node *));
        n_block->statement.block.stmts[n_block->statement.block.count - 1] = sex.value.ok;
    }

    if (st == TK_LEFT_BRACE && p->tok->tt != TK_RIGHT_BRACE) {
        ctr_node_free(n_block);
        return ctr_parse_ex_err((ctr_parse_err){CTR_ERR_EXPECTED_RBRACE, p->tok->line, p->tok->column});
    }
    ++p->tok;

    return ctr_parse_ex_ok(n_block);
}

ctr_parse_ex ctr_parreturn(ctr_parser *p) {
    ++p->tok;
    ctr_parse_ex expr = ctr_parbin(p, 0);
    if (!expr.is_ok) return expr;
    if (p->tok->tt != TK_SEMICOLON) {
        ctr_node_free(expr.value.ok);
        return ctr_parse_ex_err((ctr_parse_err){CTR_ERR_EXPECTED_SEMICOLON, p->tok->line, p->tok->column});
    }
    ++p->tok;

    ctr_node *n_ret = malloc(sizeof(ctr_node));
    *n_ret = (ctr_node){
        .tt = ND_RETURN,
        .line = (p->tok - 1)->line, .column = (p->tok - 1)->column,
        .statement.unary = {
            .tt = TK_RETURN,
            .expr = expr.value.ok,
        },
    };
    return ctr_parse_ex_ok(n_ret);
}

ctr_parse_ex ctr_parstmt(ctr_parser *p) {
    switch (p->tok->tt) {
        case TK_IF: return ctr_parif(p);
        case TK_LET: return ctr_parlet(p);
        case TK_LEFT_BRACE: case TK_SOF: return ctr_parblock(p);
        case TK_RETURN: return ctr_parreturn(p);
        case TK_IDENTIFIER:
            switch ((p->tok + 1)->tt) {
                case TK_EQUAL: return ctr_parassign(p);
                default: return ctr_parbin(p, 0);
            }
        default: return ctr_parse_ex_err((ctr_parse_err){CTR_ERR_EXPECTED_STMT, p->tok->line, p->tok->column});
    };
}

ctr_parse_ex ctr_parse(ctr_tokenvec *tokens) {
    if (tokens->count == 0)
        return ctr_parse_ex_err((ctr_parse_err){CTR_ERR_NO_TOKENS, 0, 0});
    ctr_parser p = { tokens->data };
    return ctr_parstmt(&p);
}
