#include "solus/syntax.h"
#include "sf/fs.h"
#include "solus/bytecode.h"
#include "solus/val.h"
#include "sf/str.h"
#include <math.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define VEC_NAME solu_visited
#define VEC_T sf_str
#define VSIZE_T uint32_t
#define VSIZE_MAX UINT32_MAX
#include <sf/containers/vec.h>

struct solu_includecache;
static void _solu_includecache_cleanup(struct solu_includecache *self);
#define MAP_NAME solu_includecache
#define MAP_K sf_str
#define MAP_V solu_node *
#define EQUAL_FN(s1, s2) (sf_str_eq(s1, s2))
#define HASH_FN(s) (sf_str_hash(s))
#define CLEANUP_FN _solu_includecache_cleanup
#define KCLEANUP sf_str_free
#include <sf/containers/map.h>
static void _ic_fe(void *_ud, sf_str key, solu_node *v) { sf_str_free(key); (void)_ud; (void)v; }
static void _solu_includecache_cleanup(solu_includecache *self) {
    solu_includecache_foreach(self, _ic_fe, NULL);
}

void _keywords_foreach(void *_u, sf_str k, solu_tokentype _v) { (void)_u;(void)_v; sf_str_free(k); }
void _solu_keywords_cleanup(solu_keywords *map) {
    solu_keywords_foreach(map, _keywords_foreach, NULL);
}

typedef struct {
    solu_dalloc *alloc;
    solu_includecache includes;
    solu_visited visited;
} solu_pshared;

typedef struct {
    sf_str src;
    solu_token current;
    size_t cc;
    solu_keywords keywords;
    solu_dalloc *alloc;
} solu_scanner;

// Scan string
static solu_val solu_scan_str(solu_scanner *s, const sf_str str) {
    solu_dyn p = calloc(1, sizeof(solu_dalloc) + str.len + 1);
    solu_dalloc *dh = p;
    *dh = (solu_dalloc){
        .next = NULL,
        .size = str.len + 1,
        .thread = 1,
        .tt = SOLU_DSTR,
        .mark = SOLU_DYN_WHITE,
    };
    p = (char *)p + sizeof(solu_dalloc);
    memcpy(p, str.c_str, str.len);
    solu_dalloc *dd = s->alloc;
    if (dd == NULL) s->alloc = dh;
    else {
        while (dd->next) dd = dd->next;
        dd->next = dh;
    }
    return (solu_val){ .tt = SOLU_TDYN, .dyn = p };
}
// Shortcut for simple char cases
#define solu_scancase(_c, _tt) case _c: s.current.tt = _tt; break
// Peek the next char
static inline bool solu_scanpeek(solu_scanner *s, char match) {
    if (s->cc + 1 >= s->src.len || s->src.c_str[s->cc+1] != match)
        return false;
    ++s->cc;
    ++s->current.column;
    return true;
}

static inline bool solu_isnumber(char c) { return c >= '0' && c <= '9'; }
static inline bool solu_isalphan(char c) { return (c >= 'A' && c <= 'Z')|| (c >= 'a' && c <= 'z') || c == '_' || solu_isnumber(c); }

solu_token solu_scanstr(solu_scanner *s, char quote, bool fmt) {
    size_t cc = s->cc + 1;
    size_t cap = 16;
    size_t len = 0;
    int bdepth = 0;
    char *buf = malloc(cap);

    for (; cc < s->src.len; ++cc) {
        char c = s->src.c_str[cc];
        if (c == quote && bdepth == 0) break;
        if (c == '{' && fmt) ++bdepth;
        if (c == '}' && fmt) --bdepth;

        if (c == '\\') {
            if (++cc >= s->src.len)
                goto error;
            switch (s->src.c_str[cc]) {
                case 'n': c = '\n'; break;
                case 't': c = '\t'; break;
                case 'r': c = '\r'; break;
                case '"': c = '"'; break;
                case '\'': c = '\''; break;
                case '\\': c = '\\'; break;
                default:
                    goto error;
            }
        }
        if (len + 1 >= cap) {
            cap *= 2;
            buf = realloc(buf, cap);
        }
        buf[len++] = c;
    }

    if (cc >= s->src.len || s->src.c_str[cc] != quote || bdepth != 0)
        goto error;
    buf[len] = 0;
    uint16_t c = s->current.column;
    s->current.column += (uint16_t)(cc - s->cc + 1U);
    s->cc = cc;
    solu_token tk = (solu_token){
        TK_STRING,
        solu_scan_str(s, sf_own(buf)),
        s->current.line,
        c,
    };
    free(buf);
    return tk;
error:
    free(buf);
    return (solu_token){TK_NIL, SOLU_NIL, 0, 0};
}

solu_token solu_scannum(solu_scanner *s) {
    bool is_number = false;
    uint16_t len = 0;
    for (size_t cc = s->cc; cc < s->src.len; ++cc) {

        if (s->src.c_str[cc] == '.') {
            if (is_number)
                break;
            if (cc + 1 < s->src.len && solu_isnumber(s->src.c_str[cc + 1])) {
                is_number = true;
                ++len;
                continue;
            }
            break;
        } else if (s->src.c_str[cc] == 'f') {
            is_number = true;
            break;
        } else if (!solu_isnumber(s->src.c_str[cc]))
            break;
        ++len;
    }

    char *str = calloc(len + 1, sizeof(char));
    memcpy(str, s->src.c_str + s->cc, len);
    s->cc += s->src.c_str[s->cc + len] == 'f' ? len : len - 1;
    uint16_t column = s->current.column;
    s->current.column += len;

    solu_token tok;
    if (is_number)
        tok = (solu_token) {
            .tt = TK_NUMBER,
            .value = (solu_val){.f64 = atof(str), .tt = SOLU_TF64},
            .line = s->current.line,
            .column = column,
        };
    else
        tok = (solu_token) {
            .tt = TK_INTEGER,
            .value = (solu_val){.i64 = atoll(str), .tt = SOLU_TI64},
            .line = s->current.line,
            .column = column,
        };
    free(str);
    return tok;
}

solu_token solu_scanidentifier(solu_scanner *s) {
    uint16_t column = s->current.column;
    uint16_t len = 1;
    for (size_t cc = s->cc + 1; cc < s->src.len; ++cc) {
        if (!solu_isalphan(s->src.c_str[cc]))
            break;
        ++len;
    }

    char *str = calloc(len + 1, sizeof(char));
    memcpy(str, s->src.c_str + s->cc, len);

    s->cc += len - 1;
    s->current.column += len;

    solu_keywords_ex ex = solu_keywords_get(&s->keywords, sf_ref(str));
    if (ex.is_ok) {
        solu_val value = SOLU_NIL;
        if (ex.ok == TK_TRUE)
            value = SOLU_TRUE;
        if (ex.ok == TK_FALSE)
            value = SOLU_FALSE;
        if (ex.ok == TK_OPCODE) {
            for (solu_opcode o = 0; o < SOLU_OP_COUNT; ++o)
                value = sf_str_eq(sf_lit(solu_op_info(o)->mnemonic), sf_ref(str)) ?
                    (solu_val){.tt = SOLU_TI64, .i64 = o} : value;
        }
        free(str);
        return (solu_token) {
            .tt = ex.ok,
            .value = value,
            .line = s->current.line,
            .column = column,
        };
    } else {
        solu_val val = solu_scan_str(s, sf_ref(str));
        free(str);
        return (solu_token){
            .tt = TK_IDENTIFIER,
            .value = val,
            .line = s->current.line,
            .column = column,
        };
    }
}

solu_scan_ex solu_scan(sf_str src) {
    solu_tokenvec tks = solu_tokenvec_new();
    solu_scanner s = {
        .src = src,
        .current = {TK_EOF, SOLU_NIL, 1, 1},
        .cc = 0,
        .keywords = solu_keywords_new(),
    };
    solu_error eval = SOLU_ERRP_UNEXPECTED_TOKEN;

    for (solu_opcode o = 0; o < SOLU_OP_COUNT; ++o)
        solu_keywords_set(&s.keywords, sf_ref(solu_op_info(o)->mnemonic), TK_OPCODE);

    // statements
    solu_keywords_set(&s.keywords, sf_lit("val"), TK_VAL);
    solu_keywords_set(&s.keywords, sf_lit("var"), TK_VAR);
    solu_keywords_set(&s.keywords, sf_lit("do"), TK_DO);
    solu_keywords_set(&s.keywords, sf_lit("if"), TK_IF);
    solu_keywords_set(&s.keywords, sf_lit("else"), TK_ELSE);
    solu_keywords_set(&s.keywords, sf_lit("for"), TK_FOR);
    solu_keywords_set(&s.keywords, sf_lit("while"), TK_WHILE);
    solu_keywords_set(&s.keywords, sf_lit("include"), TK_INCLUDE);
    solu_keywords_set(&s.keywords, sf_lit("break"), TK_BREAK);
    solu_keywords_set(&s.keywords, sf_lit("continue"), TK_CONTINUE);
    solu_keywords_set(&s.keywords, sf_lit("return"), TK_RETURN);
    // operators
    solu_keywords_set(&s.keywords, sf_lit("and"), TK_AND);
    solu_keywords_set(&s.keywords, sf_lit("or"), TK_OR);
    // literals
    solu_keywords_set(&s.keywords, sf_lit("nil"), TK_NIL);
    solu_keywords_set(&s.keywords, sf_lit("nan"), TK_NAN);
    solu_keywords_set(&s.keywords, sf_lit("inf"), TK_INF);
    solu_keywords_set(&s.keywords, sf_lit("true"), TK_TRUE);
    solu_keywords_set(&s.keywords, sf_lit("false"), TK_FALSE);

    solu_keywords_set(&s.keywords, sf_lit("asm"), TK_ASM);


    solu_tokenvec_push(&tks, (solu_token){TK_SOF, SOLU_NIL, s.current.line, s.current.column});
    for (; s.cc < src.len; ++s.cc) {
        s.current = (solu_token){TK_EOF, SOLU_NIL, s.current.line, s.current.column};
        char c = src.c_str[s.cc];
        size_t pcc = s.cc;
        bool fstr = false;
        switch (c) {
            solu_scancase('(', TK_LEFT_PAREN);
            solu_scancase(')', TK_RIGHT_PAREN);
            solu_scancase('{', TK_LEFT_BRACE);
            solu_scancase('}', TK_RIGHT_BRACE);
            solu_scancase('[', TK_LEFT_BRACKET);
            solu_scancase(']', TK_RIGHT_BRACKET);
            solu_scancase(',', TK_COMMA);
            solu_scancase(':', TK_COLON);
            solu_scancase(';', TK_SEMICOLON);

            solu_scancase('*', solu_scanpeek(&s, '=') ? TK_STAR_EQUAL : TK_ASTERISK);
            solu_scancase('/', solu_scanpeek(&s, '=') ? TK_SLASH_EQUAL : TK_SLASH);
            solu_scancase('+', solu_scanpeek(&s, '=') ? TK_PLUS_EQUAL :
                (solu_scanpeek(&s, '+') ? TK_INCREMENT : TK_PLUS));
            solu_scancase('-', solu_scanpeek(&s, '=') ? TK_MINUS_EQUAL :
                (solu_scanpeek(&s, '-') ? TK_DECREMENT : TK_MINUS));

            solu_scancase('!', solu_scanpeek(&s, '=') ? TK_NOT_EQUAL :
                (solu_scanpeek(&s, '!') ? TK_NEG : TK_BANG));
            solu_scancase('<', solu_scanpeek(&s, '=') ? TK_LESS_EQUAL : TK_LESS);
            solu_scancase('>', solu_scanpeek(&s, '=') ? TK_GREATER_EQUAL : TK_GREATER);
            solu_scancase('=', solu_scanpeek(&s, '=') ? TK_DOUBLE_EQUAL : TK_EQUAL);

            case '&': {
                if (solu_scanpeek(&s, '&')) { s.current.tt = TK_AND; break; }
                goto err;
            }
            case '|': {
                if (solu_scanpeek(&s, '|')) { s.current.tt = TK_OR; break; }
                goto err;
            }
            case '#': {
                if (solu_scanpeek(&s, '#')) {
                    for (; s.cc < src.len; ++s.cc) {
                        if (src.c_str[s.cc] == '#' && solu_scanpeek(&s, '#')) {
                            s.current.column += 2;
                            break;
                        }
                    };
                    if (src.c_str[s.cc] != '#') {
                        eval = SOLU_ERRP_UNTERMINATED_COMMENT;
                        goto err;
                    }
                    continue;
                } else  {
                    for (; s.cc < src.len && src.c_str[s.cc + 1] != '\n'; ++s.cc) {};
                    continue;
                }
                goto err;
            }

            case '\n': {
                ++s.current.line;
                s.current.column = 1;
                continue;
            }

            case '\t': s.current.column += 4; continue;
            case ' ': ++s.current.column; continue;
            case '\r': continue;

            case '$':
                fstr = true;
                c = src.c_str[++s.cc];
                if (c != '"' && c != '\'') {
                    eval = SOLU_ERRP_EXPECTED_STR;
                    ++s.current.column;
                    goto err;
                }
                __attribute__((fallthrough));
            case '"':
            case '\'': {
                solu_token tk = solu_scanstr(&s, c, fstr);
                if (tk.tt != TK_STRING) {
                    eval = SOLU_ERRP_UNTERMINATED_STR;
                    s.current = tk;
                    goto err;
                }
                solu_tokenvec_push(&tks, fstr ?
                    (solu_token){TK_FSTRING, tk.value, tk.line, tk.column} :
                    tk);
                continue;
            }

            case '.':
                if (src.c_str[s.cc + 1] == '.' && src.c_str[s.cc + 1] == '.') {
                    s.current.tt = TK_ELIPSES;
                    s.cc += 2;
                    break;
                }
                if (!solu_isnumber(src.c_str[s.cc + 1])) {
                    s.current.tt = TK_PERIOD;
                    break;
                }
                __attribute__((fallthrough));
            default:
                if (solu_isnumber(c) || c == '.') { // Number
                    solu_token tk = solu_scannum(&s);
                    if (tk.tt != TK_NUMBER && tk.tt != TK_INTEGER) {
                        eval = SOLU_ERRP_NUMBER_FORMAT;
                        goto err;
                    }
                    solu_tokenvec_push(&tks, tk);
                    continue;
                } else if (solu_isalphan(c)) { // Identifier
                    solu_tokenvec_push(&tks, solu_scanidentifier(&s));
                    continue;
                }
            err: {
                solu_tokenvec_free(&tks);
                solu_keywords_free(&s.keywords);
                size_t tk_len = s.cc - pcc + 1;
                for (size_t cc2 = s.cc; cc2 < s.src.len; ++cc2) {
                    char ws = s.src.c_str[cc2];
                    if (ws == ' ' || ws == '\n' || ws == '\r' || ws == '\n')
                        break;
                    ++tk_len;
                }
                for (solu_dalloc *ac = s.alloc; ac; ) {
                    solu_dalloc *next = ac->next;
                    free(ac);
                    ac = next;
                }
                char *str = calloc(1, tk_len + 1);
                memcpy(str, s.src.c_str + pcc, tk_len);
                return solu_scan_ex_err((solu_scan_err){eval, sf_own(str), s.current.line, s.current.column});
            }
        }
        solu_tokenvec_push(&tks, s.current);
        ++s.current.column;
    }

    solu_keywords_free(&s.keywords);
    solu_tokenvec_push(&tks, (solu_token){TK_EOF, SOLU_NIL, s.current.line, s.current.column});
    return solu_scan_ex_ok((solu_scan_ok){tks, s.alloc});
}

typedef struct {
    sf_str path;
    solu_token *tok;
    bool asm;
    solu_pshared *shared;
} solu_parser;

void solu_node_free(solu_node *tree) {
    if (!tree) return;
    switch (tree->tt) {
        // statements
        case SOLU_ND_LOCAL:
            for (uint16_t i = 0; i < tree->n_local.entry_c; ++i)
                solu_node_free(tree->n_local.entries[i].value);
            free(tree->n_local.entries);
            break;
        case SOLU_ND_IF:
            solu_node_free(tree->n_if.condition);
            solu_node_free(tree->n_if.then_node);
            if (tree->n_if.else_node)
                solu_node_free(tree->n_if.else_node);
            break;
        case SOLU_ND_FOR:
            solu_node_free(tree->n_for.pre);
            solu_node_free(tree->n_for.condition);
            solu_node_free(tree->n_for.post);
            solu_node_free(tree->n_for.body);
            break;
        case SOLU_ND_WHILE:
            solu_node_free(tree->n_while.condition);
            solu_node_free(tree->n_while.stmt);
            break;
        case SOLU_ND_INS:
            break;
        case SOLU_ND_RETURN:
            solu_node_free(tree->n_return.expr);
            break;
        case SOLU_ND_LCONTROL:
            break;
        // operators
        case SOLU_ND_UNARY:
            solu_node_free(tree->n_unary.right);
            break;
        case SOLU_ND_BINARY:
            solu_node_free(tree->n_binary.left);
            solu_node_free(tree->n_binary.right);
            break;
        case SOLU_ND_POSTFIX:
            solu_node_free(tree->n_postfix.expr);
            solu_node_free(tree->n_postfix.postfix);
            break;
        case SOLU_ND_CALL:
            solu_node_free(tree->n_call.identifier);
            if (tree->n_call.args) {
                for (size_t i = 0; i < tree->n_call.arg_c; ++i)
                    solu_node_free(tree->n_call.args[i]);
                free(tree->n_call.args);
            }
            break;
        // literals
        case SOLU_ND_IDENTIFIER:
        case SOLU_ND_LITERAL:
            break;
        case SOLU_ND_OBJ:
            for (uint32_t i = 0; i < tree->n_obj.mem_c; ++i)
                solu_node_free(tree->n_obj.members[i]);
            free(tree->n_obj.members);
            break;
        // functions
        case SOLU_ND_BLOCK:
            if (tree->n_block.stmts) {
                for (uint32_t i = 0; i < tree->n_block.count; ++i)
                    solu_node_free(tree->n_block.stmts[i]);
                free(tree->n_block.stmts);
            }
            break;
        case SOLU_ND_FUN:
            free(tree->n_fun.captures);
            free(tree->n_fun.args);
            if (tree->n_fun.stmt)
                solu_node_free(tree->n_fun.stmt);
            break;
        case SOLU_ND_ASM:
            solu_node_free(tree->n_asm.n_fun);
            break;
    }
    free(tree);
}

size_t solu_precedence(solu_tokentype tt) {
    switch (tt) {
        case TK_EQUAL:
        case TK_PLUS_EQUAL: case TK_MINUS_EQUAL:
        case TK_STAR_EQUAL: case TK_SLASH_EQUAL:
            return 0;
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
bool solu_niscondition(solu_node *node) {
    if (node->tt == SOLU_ND_IDENTIFIER ||
        node->tt == SOLU_ND_CALL ||
        (node->tt == SOLU_ND_UNARY && (node->n_unary.op == TK_BANG || node->n_unary.op == TK_INCREMENT || node->n_unary.op == TK_DECREMENT)) ||
       (node->tt == SOLU_ND_LITERAL && node->n_literal.tt == SOLU_TBOOL))
        return true;
    if (node->tt != SOLU_ND_BINARY)
        return false;
    switch (node->n_binary.op) {
        case TK_OR: case TK_AND: case TK_DOUBLE_EQUAL: case TK_NOT_EQUAL:
        case TK_LESS: case TK_LESS_EQUAL: case TK_GREATER: case TK_GREATER_EQUAL:
            return true;
        default: return false;
    }
}

static inline bool solu_parpeek(solu_parser *p, solu_tokentype match) {
    if (p->tok->tt != match)
        return false;
    ++p->tok;
    return true;
}

// Convenience err macro
#define solu_perr(type, tok) solu_parse_ex_err((solu_parse_err){(type), *(tok)})

static solu_val solu_pstr(solu_parser *p, char *str) {
    size_t len = 0, ilen = strlen(str);
    for (size_t i = 0; i < ilen; ++i) {
        if (i != ilen - 1 && ((str[i] == '{' && str[i + 1] == '{') || (str[i] == '}' && str[i + 1] == '}')))
            ++i;
        ++len;
    }
    solu_dalloc *dc = calloc(1, sizeof(solu_dalloc) + len + 1);
    *dc = (solu_dalloc){
        NULL,
        len + 1,
        1,
        SOLU_DSTR,
        SOLU_DYN_WHITE,
        true,
        SOLU_NIL,
        {SOLU_NIL}
    };
    solu_dalloc *dd = p->shared->alloc;
    if (dd == NULL) p->shared->alloc = dc;
    else {
        while (dd->next) dd = dd->next;
        dd->next = dc;
    }
    ++dc;

    for (size_t i = 0, j = 0; i < ilen; ++i, ++j) {
        if (i != ilen - 1 && ((str[i] == '{' && str[i + 1] == '{') || (str[i] == '}' && str[i + 1] == '}'))) {
            ((char *)dc)[j] = str[i];
            ++i;
            continue;
        }
        ((char *)dc)[j] = str[i];
    }
    return (solu_val){SOLU_TDYN, .dyn = dc};
}

solu_parse_ex solu_pprimary(solu_parser *p);
solu_parse_ex solu_punary(solu_parser *p);
solu_parse_ex solu_pexpr(solu_parser *p, size_t prec);
solu_parse_ex solu_pif(solu_parser *p);
solu_parse_ex solu_plocal(solu_parser *p);
solu_parse_ex solu_ppostfix(solu_parser *p);
solu_parse_ex solu_pblock(solu_parser *p);
solu_parse_ex solu_pfun(solu_parser *p);
solu_parse_ex solu_pasm(solu_parser *p);
solu_parse_ex solu_pins(solu_parser *p);
solu_parse_ex solu_pobj(solu_parser *p);
solu_parse_ex solu_pwhile(solu_parser *p);
solu_parse_ex solu_pfor(solu_parser *p);
solu_parse_ex solu_preturn(solu_parser *p);
solu_parse_ex solu_pinclude(solu_parser *p);
solu_parse_ex solu_pstmt(solu_parser *p);

static char *pfstr_fend(char *p, char *end) {
    int brace_depth = 0;
    while (p < end) {
        switch (*p) {
            case '\'':
            case '"': {
                char quote = *p++;
                while (p < end) {
                    if (*p == '\\' && p + 1 < end) {
                        p += 2;
                        continue;
                    }
                    if (*p == quote) {
                        ++p;
                        break;
                    }
                    ++p;
                }
                continue;
            }
            case '{':
                ++brace_depth;
                ++p;
                continue;
            case '}':
                if (brace_depth == 0)
                    return p;
                --brace_depth;
                ++p;
                continue;
            default:
                ++p;
                continue;
        }
    }
    return NULL;
}

solu_node *solu_pfstr_app(solu_parser *p, solu_node *left, char *pp, char *ep) {
    char op = *pp;
    *pp = '\0';
    solu_node *n_lit = malloc(sizeof(solu_node));
    *n_lit = (solu_node){
        SOLU_ND_LITERAL,
        p->tok->line, p->tok->column,
        .n_literal = solu_pstr(p, ep),
    };
    *pp = op;
    if (left) {
        solu_node *n_binary = malloc(sizeof(solu_node));
        *n_binary = (solu_node){
            SOLU_ND_BINARY,
            p->tok->line, p->tok->column,
            .n_binary = {
                .left = left,
                .op = TK_PLUS,
                .right = n_lit,
            }
        };
        return n_binary;
    }
    return n_lit;
}
solu_parse_ex solu_pprimary(solu_parser *p) {
    switch (p->tok->tt) {
        case TK_NAN:
        case TK_INF:
            p->tok->value = (solu_val){SOLU_TF64, .f64=p->tok->tt == TK_NAN ? NAN : INFINITY};
            __attribute__((fallthrough));
        case TK_INTEGER: case TK_NUMBER: case TK_STRING:
        case TK_TRUE: case TK_FALSE: case TK_NIL: {
            solu_node *n = malloc(sizeof(solu_node));
            *n = (solu_node){
                p->tok->tt == TK_IDENTIFIER ? SOLU_ND_IDENTIFIER : SOLU_ND_LITERAL,
                p->tok->line, p->tok->column,
                .n_literal = p->tok->value,
            };
            ++p->tok;
            return solu_parse_ex_ok(n);
        }
        case TK_FSTRING: {
            solu_node *left = malloc(sizeof(solu_node));
            *left = (solu_node){
                SOLU_ND_LITERAL,
                p->tok->line, p->tok->column,
                .n_literal = solu_pstr(p, ""), // Dummy
            };

            char *str = p->tok->value.dyn, *pp = str, *ep = str;
            size_t len = strlen(str);
            while (pp != str + len) {
                if (*pp == '{') {
                    if (pp < str + len - 1 && *(pp+1) == '{') {
                        pp += 2;
                        continue;
                    }
                    if (ep != pp) {
                        if (ep == str) solu_node_free(left);
                        left = solu_pfstr_app(p, ep == str ? NULL : left, pp, ep);
                    } else if (left->tt == SOLU_ND_LITERAL && left->n_literal.dyn == p->tok->value.dyn) {
                        *pp = '\0';
                        solu_node_free(left);
                        left = malloc(sizeof(solu_node));
                        *left = (solu_node){
                            SOLU_ND_LITERAL,
                            p->tok->line, p->tok->column,
                            .n_literal = solu_pstr(p, str),
                        };
                    }

                    ++pp;
                    ep = pfstr_fend(pp, str + len);
                    if (!ep) {
                        solu_node_free(left);
                        return solu_perr(SOLU_ERRP_UNTERMINATED_FMT, p->tok);
                    }
                    *ep = '\0';
                    solu_scan_ex ex = solu_scan(sf_ref(pp));
                    *ep = '}';
                    if (!ex.is_ok) {
                        solu_node_free(left);
                        return solu_parse_ex_err((solu_parse_err){
                            ex.err.tt,
                            (solu_token){TK_FSTRING, SOLU_NIL, ex.err.line, ex.err.column}
                        });
                    }

                    solu_token *ot = p->tok;
                    p->tok = ex.ok.tv.data + 1; // Skip SOF
                    // append alloc
                    solu_dalloc *ac = p->shared->alloc;
                    while (ac->next) ac = ac->next;
                    ac->next = ex.ok.alloc;

                    solu_parse_ex ex2 = solu_pexpr(p, 0);
                    p->tok = ot;
                    solu_tokenvec_free(&ex.ok.tv);
                    if (!ex2.is_ok) {
                        ex2.err.token = *ot;
                        solu_node_free(left);
                        return ex2;
                    }
                    ex2.ok->line = ot->line;
                    ex2.ok->column = ot->column;

                    solu_node *n_binary = malloc(sizeof(solu_node));
                    *n_binary = (solu_node){
                        SOLU_ND_BINARY,
                        p->tok->line, p->tok->column,
                        .n_binary = {
                            .left = left,
                            .op = TK_PLUS,
                            .right = ex2.ok,
                        }
                    };
                    left = n_binary;
                    pp = ep++;
                }
                ++pp;
            }
            if (ep != pp)
                left = solu_pfstr_app(p, left, pp, ep);
            ++p->tok;
            return solu_parse_ex_ok(left);
        }
        case TK_INCLUDE: return solu_pinclude(p);
        case TK_DO:
            ++p->tok;
            if (p->tok->tt != TK_LEFT_BRACE)
                return solu_perr(SOLU_ERRP_EXPECTED_BLOCK, p->tok);
            return solu_pblock(p);

        case TK_BANG: case TK_NEG:
        case TK_MINUS:
        case TK_INCREMENT:
        case TK_DECREMENT:
            return solu_punary(p);

        case TK_LEFT_BRACKET: return solu_pfun(p);
        case TK_ASM: return solu_pasm(p);
        case TK_LEFT_BRACE: return solu_pobj(p);
        case TK_IDENTIFIER: {
            solu_node *n = malloc(sizeof(solu_node));
            *n = (solu_node){
                SOLU_ND_IDENTIFIER,
                p->tok->line, p->tok->column,
                .n_identifier = p->tok->value,
            };
            ++p->tok;
            return solu_parse_ex_ok(n);
        }
        case TK_LEFT_PAREN: {
            ++p->tok;
            solu_parse_ex ex = solu_pexpr(p, 0);
            if (!ex.is_ok) return ex;
            if (p->tok->tt != TK_RIGHT_PAREN)
                return solu_perr(SOLU_ERRP_EXPECTED_RPAREN, p->tok);
            ++p->tok;
            return solu_parse_ex_ok(ex.ok);
        }
        case TK_SEMICOLON:
            return solu_perr(SOLU_ERRP_UNEXPECTED_SEMICOLON, p->tok);
        default:
            return solu_perr(SOLU_ERRP_EXPECTED_EXPRESSION, p->tok);
    }
}

solu_parse_ex solu_punary(solu_parser *p) {
    solu_tokentype tt = (p->tok++)->tt;
    if (tt == TK_INCREMENT || tt == TK_DECREMENT) {
        solu_parse_ex expr = solu_ppostfix(p);
        if (!expr.is_ok) return expr;
        if (expr.ok->tt != SOLU_ND_IDENTIFIER
        &&  expr.ok->tt != SOLU_ND_POSTFIX) {
            solu_node_free(expr.ok);
            return solu_perr(SOLU_ERRP_EXPECTED_IDENTIFIER, p->tok);
        }
        solu_node *n_binary = malloc(sizeof(solu_node));
        solu_node *n_literal = malloc(sizeof(solu_node));
        *n_literal = (solu_node){ SOLU_ND_LITERAL, (p->tok-1)->line, (p->tok-1)->column, .n_literal = (solu_val){SOLU_TI64, .i64 = 1}};
        *n_binary = (solu_node){
            SOLU_ND_BINARY,
            (p->tok-1)->line, (p->tok-1)->column,
            .n_binary = {
                .op = tt == TK_INCREMENT ? TK_PLUS_EQUAL : TK_MINUS_EQUAL,
                .left = expr.ok,
                .right = n_literal,
            }
        };
        return solu_parse_ex_ok(n_binary);
    }
    if (tt == TK_NEG) {
        solu_token *tk = p->tok;
        solu_parse_ex expr = solu_ppostfix(p);
        if (!expr.is_ok) return expr;
        if (expr.ok->tt != SOLU_ND_IDENTIFIER
        &&  expr.ok->tt != SOLU_ND_POSTFIX) {
            solu_node_free(expr.ok);
            return solu_perr(SOLU_ERRP_EXPECTED_IDENTIFIER, p->tok);
        }
        p->tok = tk;
        solu_node *pfc = solu_ppostfix(p).ok;

        solu_node *n_binary = malloc(sizeof(solu_node));
        solu_node *n_unary = malloc(sizeof(solu_node));
        *n_unary = (solu_node){
            SOLU_ND_UNARY,
            (p->tok-1)->line, (p->tok-1)->column,
            .n_unary = { TK_BANG, pfc }
        };
        *n_binary = (solu_node){
            SOLU_ND_BINARY,
            (p->tok-1)->line, (p->tok-1)->column,
            .n_binary = {
                .op = TK_EQUAL,
                .left = expr.ok,
                .right = n_unary,
            }
        };
        return solu_parse_ex_ok(n_binary);
    }

    solu_parse_ex expr = solu_ppostfix(p);
    if (!expr.is_ok) return expr;

    solu_node *n_unary = malloc(sizeof(solu_node));
    *n_unary = (solu_node){
        SOLU_ND_UNARY,
        (p->tok-1)->line, (p->tok-1)->column,
        .n_unary = { tt, expr.ok }
    };
    return solu_parse_ex_ok(n_unary);
}

solu_parse_ex solu_pexpr(solu_parser *p, size_t prec) {
    solu_parse_ex ex = solu_ppostfix(p);
    if (!ex.is_ok) return ex;

    solu_node *left = ex.ok;
    while (true) {
        solu_token *op = p->tok;
        size_t op_prec = solu_precedence(op->tt);
        if (op_prec == SIZE_MAX || op_prec < prec)
            break;
        ++p->tok;

        ex = solu_pexpr(p, (op->tt == TK_EQUAL) ? op_prec : op_prec + 1);
        if (!ex.is_ok) {
            solu_node_free(left);
            return ex;
        }

        solu_node *bin = malloc(sizeof(solu_node));
        *bin = (solu_node){
            .tt = SOLU_ND_BINARY,
            .line = op->line, .column = op->column,
            .n_binary = {
                .op = op->tt,
                .left = left,
                .right = ex.ok,
            },
        };
        left = bin;
    }
    return solu_parse_ex_ok(left);
}

solu_parse_ex solu_pif(solu_parser *p) {
    solu_token *tk_if = p->tok++;
    solu_parse_ex cex = solu_pexpr(p, 0);
    if (!cex.is_ok) return cex;

    solu_parse_ex tex = p->tok->tt == TK_LEFT_BRACE ? solu_pblock(p) : solu_pstmt(p);
    if (!tex.is_ok) {
        solu_node_free(cex.ok);
        return tex;
    }
    solu_parse_ex eex = (solu_parse_ex){.is_ok = false};
    if (p->tok->tt == TK_ELSE) {
        ++p->tok;

        eex = p->tok->tt == TK_LEFT_BRACE ? solu_pblock(p) : solu_pstmt(p);
        if (!eex.is_ok) {
            solu_node_free(cex.ok);
            solu_node_free(tex.ok);
            return eex;
        }
    }

    solu_node *n_if = malloc(sizeof(solu_node));
    *n_if = (solu_node){
        .tt = SOLU_ND_IF,
        .line = tk_if->line, .column = tk_if->column,
        .n_if = {
            .condition = cex.ok,
            .then_node = tex.ok,
            .else_node = eex.is_ok ? eex.ok : NULL,
        },
    };
    return solu_parse_ex_ok(n_if);
}

solu_parse_ex solu_plocal(solu_parser *p) {
    solu_token *lv = p->tok++;

    struct solu_name *names = NULL;
    uint16_t name_c = 0;
    while (p->tok->tt == TK_IDENTIFIER) {
        if (name_c >= 255) {
            free(names);
            return solu_perr(SOLU_ERRP_MAX_NAMES, p->tok);
        }
        solu_token *name = p->tok++;
        struct solu_name n = {name->value, NULL};
        if (p->tok->tt == TK_EQUAL) {
            ++p->tok;
            solu_parse_ex vex = solu_pexpr(p, 0);
            if (!vex.is_ok) {
                if (names) free(names);
                return vex;
            }
            n.value = vex.ok;
            if (p->tok->tt != TK_SEMICOLON && p->tok->tt != TK_COMMA) {
                if (names) free(names);
                return solu_perr(SOLU_ERRP_EXPECTED_LIST_TERM, p->tok - 1);
            }
            names = realloc(names, ++name_c * sizeof(struct solu_name));
            names[name_c - 1] = n;
        } else if (lv->tt == TK_VAL) {
            if (names) free(names);
            return solu_perr(SOLU_ERRP_EXPECTED_EQUAL, p->tok);
        } else if (p->tok->tt == TK_SEMICOLON || p->tok->tt == TK_COMMA) {
            solu_node *n_nil = malloc(sizeof(solu_node));
            *n_nil = (solu_node){SOLU_ND_LITERAL, lv->line, lv->column, .n_literal=SOLU_NIL};
            n.value = n_nil;
            names = realloc(names, ++name_c * sizeof(struct solu_name));
            names[name_c - 1] = n;
        }
        if (p->tok->tt == TK_SEMICOLON) break;
        ++p->tok;
    }
    if (!names) return solu_perr(SOLU_ERRP_EXPECTED_NAME, p->tok);
    if (p->tok->tt != TK_SEMICOLON) {
        free(names);
        return solu_perr(SOLU_ERRP_EXPECTED_SEMICOLON, p->tok);
    }
    ++p->tok;

    solu_node *n_local = malloc(sizeof(solu_node));
    *n_local = (solu_node){
        .tt = SOLU_ND_LOCAL,
        .line = lv->line, .column = lv->column,
        .n_local = {
            .entries = names,
            .entry_c = name_c,
            .mut = lv->tt == TK_VAR
        }
    };
    return solu_parse_ex_ok(n_local);
}

solu_parse_ex solu_ppostfix(solu_parser *p) {
    uint16_t line = p->tok->line, column = p->tok->column;
    solu_parse_ex ex = solu_pprimary(p);
    if (!ex.is_ok) return ex;
    solu_node *node = ex.ok;

    while (true) {
        switch (p->tok->tt) {
            case TK_LEFT_PAREN: {
                solu_node *call = malloc(sizeof(solu_node));
                *call = (solu_node){
                    .tt = SOLU_ND_CALL,
                    .line = line,
                    .column = column,
                    .n_call = {
                        .identifier = node,
                        .args = NULL,
                        .arg_c = 0,
                    }
                };
                ++p->tok;

                bool variadic = false;
                while (p->tok->tt != TK_RIGHT_PAREN && p->tok->tt != TK_EOF) {
                    if (p->tok->tt == TK_ELIPSES) {
                        solu_token *el = p->tok++;
                        if (p->tok->tt != TK_IDENTIFIER) {
                            solu_node_free(call);
                            return solu_perr(SOLU_ERRP_EXPECTED_IDENTIFIER, p->tok);
                        }
                        variadic = true;
                        solu_node *arg = malloc(sizeof(solu_node));
                        *arg = (solu_node){
                            SOLU_ND_IDENTIFIER,
                            el->line, el->column,
                            .n_identifier = p->tok->value,
                        };
                        ++p->tok;

                        call->n_call.args = realloc(call->n_call.args, sizeof(solu_node*) * (++call->n_call.arg_c));
                        call->n_call.args[call->n_call.arg_c - 1] = arg;
                        if (p->tok->tt == TK_COMMA) {
                            solu_node_free(call);
                            return solu_perr(SOLU_ERRP_VARIADIC_LAST, p->tok);
                        }
                        break;
                    }
                    solu_parse_ex arg = solu_pexpr(p, 0);
                    if (!arg.is_ok) {
                        solu_node_free(call);
                        return arg;
                    }
                    call->n_call.args = realloc(call->n_call.args, sizeof(solu_node*) * (++call->n_call.arg_c));
                    call->n_call.args[call->n_call.arg_c - 1] = arg.ok;
                    if (p->tok->tt == TK_COMMA)
                        ++p->tok;
                    else break;
                }
                if (p->tok->tt != TK_RIGHT_PAREN) {
                    solu_node_free(call);
                    return solu_perr(SOLU_ERRP_MALFORMED_CALL, p->tok);
                }
                call->n_call.variadic = variadic;

                ++p->tok;
                node = call;
                continue;
            }
            case TK_PERIOD: {
                ++p->tok;
                if (p->tok->tt != TK_IDENTIFIER)
                    return solu_perr(SOLU_ERRP_EXPECTED_IDENTIFIER, p->tok);

                solu_node *member = malloc(sizeof(solu_node));
                solu_node *ident = malloc(sizeof(solu_node));
                // It's literal so it gets treated as ["..."]
                *ident = (solu_node){SOLU_ND_LITERAL, p->tok->line, p->tok->column, .n_identifier = p->tok->value};
                *member = (solu_node){
                    .tt = SOLU_ND_POSTFIX,
                    .line = line,
                    .column = column,
                    .n_postfix = {
                        .op = TK_PERIOD,
                        .expr = node,
                        .postfix = ident,
                    }
                };
                ++p->tok;
                node = member;
                continue;
            }
            case TK_LEFT_BRACKET: {
                ++p->tok;
                solu_parse_ex ex = solu_pexpr(p, 0);
                if (!ex.is_ok) {
                    solu_node_free(node);
                    return ex;
                }
                solu_node *member = malloc(sizeof(solu_node));
                *member = (solu_node){
                    .tt = SOLU_ND_POSTFIX,
                    .line = line,
                    .column = column,
                    .n_postfix = {
                        .op = TK_LEFT_BRACKET,
                        .expr = node,
                        .postfix = ex.ok,
                    }
                };
                node = member;
                if (p->tok->tt != TK_RIGHT_BRACKET) {
                    solu_node_free(node);
                    solu_node_free(member);
                    return solu_perr(SOLU_ERRP_EXPECTED_RBRACKET, p->tok);
                }
                ++p->tok;
                continue;
            }
            case TK_INCREMENT:
            case TK_DECREMENT: {
                if (node->tt != SOLU_ND_IDENTIFIER && node->tt != SOLU_ND_POSTFIX) {
                    solu_node_free(node);
                    return solu_perr(SOLU_ERRP_EXPECTED_IDENTIFIER, p->tok - 1);
                }
                solu_node *post = malloc(sizeof(solu_node));
                solu_node *lit = malloc(sizeof(solu_node));
                *lit = (solu_node){SOLU_ND_LITERAL, line, column, .n_literal=(solu_val){SOLU_TI64, .i64=1}};
                *post = (solu_node){
                    .tt = SOLU_ND_BINARY,
                    .line = line,
                    .column = column,
                    .n_binary = {
                        p->tok->tt == TK_INCREMENT ? TK_PLUS_EQUAL : TK_MINUS_EQUAL,
                        node, lit,
                        true
                    }
                };
                ++p->tok;
                node = post;
                continue;
            }
            default: break;
        }
        break;
    }
    return solu_parse_ex_ok(node);
}

solu_parse_ex solu_pblock(solu_parser *p) {
    solu_node *n_block = malloc(sizeof(solu_node));
    *n_block = (solu_node){
        .tt = SOLU_ND_BLOCK,
        .line = p->tok->line, .column = p->tok->column,
        .n_block = {
            .stmts = NULL,
            .count = 0,
        },
    };
    solu_token *st = p->tok;
    ++p->tok;
    while (p->tok->tt != TK_RIGHT_BRACE && p->tok->tt != TK_EOF) {
        solu_parse_ex sex = solu_pstmt(p); // HHAHAHAHHAHHAHAHHAH
        if (!sex.is_ok) {
            solu_node_free(n_block);
            return sex;
        }
        if (sex.ok->tt == SOLU_ND_RETURN && p->tok->tt != TK_RIGHT_BRACE && p->tok->tt != TK_EOF) {
            solu_node_free(n_block);
            solu_error e = sex.ok->n_return.implicit ? SOLU_ERRP_UNEXPECTED_IDENTIFIER : SOLU_ERRP_UNREACHABLE_CODE;
            if (p->tok->tt == TK_SEMICOLON) {
                st = p->tok;
                e = SOLU_ERRP_UNEXPECTED_SEMICOLON;
            } else st = p->tok;
            return solu_perr(e, st);
        }
        n_block->n_block.stmts = realloc(n_block->n_block.stmts, ++n_block->n_block.count * sizeof(solu_node *));
        n_block->n_block.stmts[n_block->n_block.count - 1] = sex.ok;
    }

    if (st->tt == TK_LEFT_BRACE && p->tok->tt != TK_RIGHT_BRACE) {
        solu_node_free(n_block);
        return solu_perr(SOLU_ERRP_EXPECTED_RBRACKET, p->tok);
    }
    ++p->tok;

    return solu_parse_ex_ok(n_block);
}

solu_parse_ex solu_pfun(solu_parser *p) {
    ++p->tok; // Consume [
    solu_node *n_fun = malloc(sizeof(solu_node));
    *n_fun = (solu_node){
        .tt = SOLU_ND_FUN,
        .line = p->tok->line, .column = p->tok->column,
        .n_fun = {
            .captures = NULL,
            .cap_c = 0,
            .args = NULL,
            .arg_c = 0,
        },
    };

    bool self = false;
    while (p->tok->tt != TK_RIGHT_BRACKET && p->tok->tt != TK_EOF) {
        if (p->tok->tt != TK_IDENTIFIER) {
            solu_node_free(n_fun);
            return solu_perr(SOLU_ERRP_EXPECTED_IDENTIFIER, p->tok);
        }
        for (uint32_t i = 0; i < n_fun->n_fun.cap_c; ++i)
            if (strcmp(p->tok->value.dyn, n_fun->n_fun.captures[i].dyn) == 0) {
                solu_node_free(n_fun);
                return solu_perr(SOLU_ERRP_DUPLICATE_CAPTURE, p->tok);
            }
        if (strcmp(p->tok->value.dyn, "global") == 0) {
            solu_node_free(n_fun);
            return solu_perr(SOLU_ERRP_DUPLICATE_CAPTURE, p->tok);
        }
        if (strcmp(p->tok->value.dyn, "self") == 0) { // Self Check!
            if (n_fun->n_fun.cap_c > 0) {
                solu_node_free(n_fun);
                return solu_perr(SOLU_ERRP_SELF_FIRST, p->tok);
            }
            self = true;
        }

        n_fun->n_fun.captures = realloc(n_fun->n_fun.captures, (++n_fun->n_fun.cap_c) * sizeof(solu_val));
        n_fun->n_fun.captures[n_fun->n_fun.cap_c - 1] = p->tok->value;
        ++p->tok;

        if (p->tok->tt != TK_COMMA && p->tok->tt != TK_RIGHT_BRACKET) {
            solu_node_free(n_fun);
            return solu_perr(SOLU_ERRP_UNTERMINATED_CAPTURES, p->tok);
        }
        if (p->tok->tt == TK_COMMA) ++p->tok;
    }
    ++p->tok;

    if (p->tok->tt != TK_LEFT_PAREN) {
        solu_node_free(n_fun);
        return solu_perr(SOLU_ERRP_EXPECTED_ARGS, p->tok);
    }
    ++p->tok;

    bool variadic = false;
    while (p->tok->tt != TK_RIGHT_PAREN && p->tok->tt != TK_EOF && !variadic) {
        if (p->tok->tt == TK_ELIPSES) {
            ++p->tok;
            variadic = true;
        }
        if (p->tok->tt != TK_IDENTIFIER) {
            solu_node_free(n_fun);
            return solu_perr(SOLU_ERRP_EXPECTED_IDENTIFIER, p->tok);
        }
        if (self && strcmp(p->tok->value.dyn, "self") == 0) {
            solu_node_free(n_fun);
            return solu_perr(SOLU_ERRP_DUPLICATE_SELF, p->tok);
        }
        n_fun->n_fun.args = realloc(n_fun->n_fun.args, ++n_fun->n_fun.arg_c * sizeof(solu_val));
        n_fun->n_fun.args[n_fun->n_fun.arg_c - 1] = p->tok->value;
        ++p->tok;

        if (p->tok->tt != TK_COMMA && p->tok->tt != TK_RIGHT_PAREN) {
            solu_node_free(n_fun);
            return solu_perr(SOLU_ERRP_UNTERMINATED_ARGS, p->tok);
        }
        if (p->tok->tt == TK_COMMA) {
            if (variadic) {
                solu_node_free(n_fun);
                return solu_perr(SOLU_ERRP_VARIADIC_LAST, p->tok);
            }
            ++p->tok;
        }
    }
    ++p->tok;
    n_fun->n_fun.variadic = variadic;

    if (p->tok->tt == TK_LEFT_BRACE) {
        solu_parse_ex bex = solu_pblock(p);
        if (!bex.is_ok) {
            solu_node_free(n_fun);
            return bex;
        }
        n_fun->n_fun.stmt = bex.ok;
    } else {
        solu_parse_ex id = solu_pexpr(p, 0);
        if (!id.is_ok) return id;
        n_fun->n_fun.stmt = malloc(sizeof(solu_node));
        *n_fun->n_fun.stmt = (solu_node){
            .tt = SOLU_ND_RETURN,
            .line = id.ok->line, .column = id.ok->column,
            .n_return = { id.ok, true },
        };
    }
    return solu_parse_ex_ok(n_fun);
}

solu_parse_ex solu_pasm(solu_parser *p) {
    ++p->tok; // Consume asm
    if (p->tok->tt != TK_LEFT_PAREN)
        return solu_perr(SOLU_ERRP_EXPECTED_LPAREN, p->tok);
    ++p->tok;
    if (p->tok->tt == TK_MINUS && (p->tok+1)->tt == TK_INTEGER)
        return solu_perr(SOLU_ERRP_NEGATIVE_REGISTERS, p->tok);

    solu_parse_ex count = solu_pprimary(p);
    if (!count.is_ok) return count;
    if (count.ok->tt != SOLU_ND_LITERAL || count.ok->n_literal.tt != SOLU_TI64)
        return solu_perr(SOLU_ERRP_EXPECTED_INTEGER, p->tok);
    uint32_t regs = (uint32_t)count.ok->n_literal.i64;

    if (p->tok->tt != TK_RIGHT_PAREN)
        return solu_perr(SOLU_ERRP_EXPECTED_RPAREN, p->tok);
    ++p->tok;

    p->asm = true;
    solu_parse_ex fex = solu_pfun(p);
    p->asm = false;
    if (!fex.is_ok) return fex;

    solu_node *n_asm = malloc(sizeof(solu_node));
    *n_asm = (solu_node){
        .tt = SOLU_ND_ASM,
        .line = p->tok->line, .column = p->tok->column,
        .n_asm = {
            .temps = regs,
            .n_fun = fex.ok,
        },
    };
    return solu_parse_ex_ok(n_asm);
}

solu_parse_ex solu_pins(solu_parser *p) {
    solu_opcode op = (solu_opcode)p->tok->value.i64;
    ++p->tok;
    solu_val opa[3] = {SOLU_NIL, SOLU_NIL, SOLU_NIL};
    for (int i = 0; i < (int)(solu_op_info(op)->type) + 1; ++i) {
        solu_token *bt = p->tok;
        solu_parse_ex vex = solu_pexpr(p, 0);
        if (!vex.is_ok) return vex;
        solu_node *nl = vex.ok;
        bool neg = false;
        if (vex.ok->tt == SOLU_ND_UNARY) {
            if (vex.ok->n_unary.op != TK_MINUS)
                return solu_perr(SOLU_ERRP_EXPECTED_MINUS, bt);
            if (vex.ok->n_unary.right->tt != SOLU_ND_LITERAL ||
                vex.ok->n_unary.right->n_literal.tt != SOLU_TI64)
                return solu_perr(SOLU_ERRP_EXPECTED_INTEGER, bt);
            nl = vex.ok->n_unary.right;
            vex.ok->n_unary.right = NULL;
            solu_node_free(vex.ok);
            neg = true;
        }
        switch (nl->tt) {
            case SOLU_ND_IDENTIFIER: opa[i] = nl->n_identifier; break;
            case SOLU_ND_LITERAL: {
                if (nl->n_literal.tt != SOLU_TI64 && !((op == SOLU_OP_LOAD && i == 1) ||
                    (op == SOLU_OP_SUPO && i == 1) ||
                    (solu_op_info(op)->type == SOLU_INS_ABC && i == 2))) {
                    solu_node_free(vex.ok);
                    return solu_perr(SOLU_ERRP_EXPECTED_INTEGER, bt);
                }
                opa[i] = neg ?
                    (solu_val){nl->n_literal.tt, .i64 = -nl->n_literal.i64} :
                    nl->n_literal;
                break;
            }
            default: {
                solu_node_free(vex.ok);
                return solu_perr(SOLU_ERRP_EXPECTED_IDENTIFIER, bt);
            }
        }
    }
    if (p->tok->tt != TK_SEMICOLON)
        return solu_perr(SOLU_ERRP_EXPECTED_SEMICOLON, p->tok - 1);
    ++p->tok;

    solu_node *n_ins = malloc(sizeof(solu_node));
    *n_ins = (solu_node){
        .tt = SOLU_ND_INS,
        .line = p->tok->line, .column = p->tok->column,
        .n_ins = {
            .op = op,
            .opa = {opa[0], opa[1], opa[2]},
        },
    };
    return solu_parse_ex_ok(n_ins);
}

solu_parse_ex solu_pobj(solu_parser *p) {
    ++p->tok;
    solu_node *n_obj = malloc(sizeof(solu_node));
    *n_obj = (solu_node){
        .tt = SOLU_ND_OBJ,
        .line = p->tok->line, .column = p->tok->column,
        .n_obj = {
            .members = NULL,
            .mem_c = 0,
        },
    };

    while (p->tok->tt != TK_RIGHT_BRACE) {
        solu_token *name = p->tok;
        solu_node *nn = NULL;
        if (p->tok->tt == TK_LEFT_BRACKET) {
            ++p->tok;
            if (p->tok->tt == TK_RIGHT_BRACKET && (p->tok+1)->tt == TK_LEFT_PAREN) {
                p->tok = name;
                goto fun_exc; // Catch our mistake if we mistook a function for index specifier
            }
            solu_parse_ex ex = solu_pexpr(p, 0);
            if (!ex.is_ok) {
                solu_node_free(n_obj);
                return ex;
            }
            nn = ex.ok;
            if (p->tok->tt == TK_COMMA || (p->tok->tt == TK_RIGHT_BRACKET && (p->tok+1)->tt == TK_LEFT_PAREN)) {
                p->tok = name;
                solu_node_free(nn);
                goto fun_exc; // Catch our mistake if we mistook a function for index specifier
            }
            if (p->tok->tt != TK_RIGHT_BRACKET) {
                solu_node_free(n_obj);
                solu_node_free(nn);
                return solu_perr(SOLU_ERRP_EXPECTED_RBRACKET, p->tok);
            }
        } else {
            if ((p->tok + 1)->tt != TK_EQUAL) fun_exc: {
                solu_parse_ex array_ex = solu_pexpr(p, 0);
                if (!array_ex.is_ok) {
                    solu_node_free(n_obj);
                    return array_ex;
                }
                if (p->tok->tt != TK_RIGHT_BRACE && p->tok->tt != TK_COMMA) {
                    solu_node_free(n_obj);
                    return solu_perr(SOLU_ERRP_EXPECTED_COMMA, p->tok);
                }
                if (p->tok->tt == TK_COMMA) ++p->tok;
                n_obj->n_obj.members = realloc(n_obj->n_obj.members, ++n_obj->n_obj.mem_c * sizeof(solu_node *));
                n_obj->n_obj.members[n_obj->n_obj.mem_c - 1] = array_ex.ok;
                continue;
            }
            if (name->tt != TK_IDENTIFIER) {
                solu_node_free(n_obj);
                return solu_perr(SOLU_ERRP_EXPECTED_IDENTIFIER, p->tok);
            }
        }
        if (name->tt == TK_SEMICOLON) {
            solu_node_free(n_obj);
            return solu_perr(SOLU_ERRP_UNEXPECTED_SEMICOLON_OBJ, p->tok);
        }
        p->tok += 2; // consume '='

        solu_parse_ex right = solu_pexpr(p, 0);
        if (!right.is_ok) {
            solu_node_free(n_obj);
            return right;
        }

        if (!nn) {
            nn = malloc(sizeof(solu_node));
            *nn = (solu_node){SOLU_ND_IDENTIFIER, name->line, name->column, .n_identifier = name->value};
        }
        solu_node *member = malloc(sizeof(solu_node));
        *member = (solu_node){
            SOLU_ND_BINARY,
            name->line, name->column,
            .n_binary = {TK_EQUAL, nn, right.ok, false},
        };
        n_obj->n_obj.members = realloc(n_obj->n_obj.members, ++n_obj->n_obj.mem_c * sizeof(solu_node *));
        n_obj->n_obj.members[n_obj->n_obj.mem_c - 1] = member;
        if (p->tok->tt == TK_COMMA) ++p->tok;
    }
    ++p->tok; // }
    return solu_parse_ex_ok(n_obj);
}

solu_parse_ex solu_pwhile(solu_parser *p) {
    solu_token *st = p->tok;
    ++p->tok;

    solu_parse_ex cond = solu_pexpr(p, 0);
    if (!cond.is_ok)
        return cond;

    solu_parse_ex stmt = p->tok->tt == TK_LEFT_BRACE ? solu_pblock(p) : solu_pstmt(p);;
    if (!stmt.is_ok) {
        solu_node_free(cond.ok);
        return stmt;
    }

    solu_node *n_while = malloc(sizeof(solu_node));
    *n_while = (solu_node){
        .tt = SOLU_ND_WHILE,
        .line = st->line, .column = st->column,
        .n_while = {
            .condition = cond.ok,
            .stmt = stmt.ok,
        },
    };

    return solu_parse_ex_ok(n_while);
}

solu_parse_ex solu_pfor(solu_parser *p) {
    solu_token *st = p->tok;
    ++p->tok;
    if (p->tok->tt == TK_LEFT_PAREN)
        ++p->tok;

    solu_token *pt = p->tok;
    solu_parse_ex pre = solu_pstmt(p);
    if (!pre.is_ok) return pre;
    if (pre.ok->tt == SOLU_ND_RETURN && pre.ok->n_return.implicit)
        return solu_perr(SOLU_ERRP_EXPECTED_SEMICOLON, pt);

    solu_parse_ex cond = solu_pexpr(p, 0);
    if (!cond.is_ok) {
        solu_node_free(pre.ok);
        return cond;
    }
    if (p->tok->tt != TK_SEMICOLON) {
        solu_node_free(pre.ok);
        solu_node_free(cond.ok);
        return solu_perr(SOLU_ERRP_EXPECTED_SEMICOLON, p->tok - 1);
    }
    ++p->tok;

    solu_parse_ex post = solu_pexpr(p, 0);
    if (!post.is_ok) return post;
    if (p->tok->tt == TK_RIGHT_PAREN)
        ++p->tok;

    solu_parse_ex stmt = p->tok->tt == TK_LEFT_BRACE ? solu_pblock(p) : solu_pstmt(p);
    if (!stmt.is_ok) {
        solu_node_free(pre.ok);
        solu_node_free(cond.ok);
        return stmt;
    }

    solu_node *n_for = malloc(sizeof(solu_node));
    *n_for = (solu_node){
        .tt = SOLU_ND_FOR,
        .line = st->line, .column = st->column,
        .n_for = {
            .pre = pre.ok,
            .condition = cond.ok,
            .post = post.ok,
            .body = stmt.ok,
        },
    };

    return solu_parse_ex_ok(n_for);
}

solu_parse_ex solu_preturn(solu_parser *p) {
    solu_token *tk = p->tok++;
    if (p->tok->tt == TK_SEMICOLON) {
        ++p->tok;
        solu_node *nd = malloc(sizeof(solu_node));
        solu_node *val = malloc(sizeof(solu_node));
        *val = (solu_node){
            SOLU_ND_LITERAL,
            .line = tk->line, .column = tk->column,
            .n_literal = SOLU_NIL,
        };
        *nd = (solu_node){
            SOLU_ND_RETURN,
            .line = tk->line, .column = tk->column,
            .n_return = { val, false }
        };
        return solu_parse_ex_ok(nd);
    }
    solu_parse_ex val = solu_pexpr(p, 0);
    if (!val.is_ok) return val;
    if (p->tok->tt != TK_SEMICOLON) {
        solu_node_free(val.ok);
        return solu_perr(SOLU_ERRP_EXPECTED_SEMICOLON, p->tok - 1);
    }
    ++p->tok;

    solu_node *nd = malloc(sizeof(solu_node));
    *nd = (solu_node){
        SOLU_ND_RETURN,
        .line = tk->line, .column = tk->column,
        .n_return = { val.ok, false }
    };
    return solu_parse_ex_ok(nd);
}

static inline void free_alloc(solu_dalloc *alloc) {
    for (solu_dalloc *ac = alloc; ac; ) {
        solu_dalloc *next = ac->next;
        free(ac);
        ac = next;
    }
}

solu_parse_ex _solu_parse(sf_str path, solu_tokenvec *tokens, solu_pshared *shared);
solu_parse_ex solu_pinclude(solu_parser *p) {
    solu_token *st = p->tok;
    ++p->tok;
    if (p->tok->tt != TK_LEFT_PAREN)
        return solu_perr(SOLU_ERRP_EXPECTED_LPAREN, p->tok);
    ++p->tok;
    if (p->tok->tt != TK_STRING)
        return solu_perr(SOLU_ERRP_EXPECTED_LITERAL, p->tok);

    char *path = p->tok->value.dyn;
    char *cwd = solu_realdir(p->path.c_str);
    if (!cwd) return solu_perr(SOLU_ERRP_INCLUDE_NOT_FOUND, st);
    sf_str realpath = sf_own(solu_realpath(path));
    free(cwd);
    if (!realpath.c_str) return solu_perr(SOLU_ERRP_INCLUDE_NOT_FOUND, st);
    ++p->tok;

    if (p->tok->tt != TK_RIGHT_PAREN)
        return solu_perr(SOLU_ERRP_EXPECTED_RPAREN, p->tok);
    ++p->tok;
    sf_fsb_ex fsb = sf_file_buffer(sf_ref(realpath.c_str));
    if (!fsb.is_ok)
        return solu_perr(SOLU_ERRP_INCLUDE_NOT_FOUND, st);
    solu_scan_ex res = solu_scan(sf_ref((char *)fsb.ok.ptr));
    if (!res.is_ok) return solu_perr(res.err.tt, st);

    solu_visited_push(&p->shared->visited, p->path);
    for (sf_str *s = p->shared->visited.data; s < p->shared->visited.data + p->shared->visited.count; ++s) {
        if (sf_str_eq(*s, realpath)) {
            solu_visited_pop(&p->shared->visited);
            sf_str_free(realpath);
            return solu_perr(SOLU_ERRP_CIRCULAR_INCLUDE, st);
        }
    }
    solu_parse_ex pres = _solu_parse(realpath, &res.ok.tv, p->shared);
    solu_visited_pop(&p->shared->visited);
    sf_str_free(realpath);

    if (!pres.is_ok) {
        free_alloc(res.ok.alloc);
        return pres;
    }
    solu_dalloc *ac = p->shared->alloc;
    while (ac->next) ac = ac->next;
    ac->next = res.ok.alloc;

    pres.ok->line = st->line;
    pres.ok->column = st->column;
    solu_node *n_include = malloc(sizeof(solu_node));
    *n_include = (solu_node){
        SOLU_ND_FUN,
        st->line, st->column,
        .n_fun = {
            .stmt = pres.ok,
            .include = true,
        }
    };
    return solu_parse_ex_ok(n_include);
}

solu_parse_ex solu_pstmt(solu_parser *p) {
    if (p->asm && p->tok->tt != TK_OPCODE)
        return solu_perr(SOLU_ERRP_EXPECTED_ASM, p->tok);

    switch (p->tok->tt) {
        case TK_OPCODE: {
            if (!p->asm) return solu_perr(SOLU_ERRP_UNEXPECTED_ASM, p->tok);
            return solu_pins(p);
        }
        case TK_IF: return solu_pif(p);
        case TK_VAL:
        case TK_VAR: return solu_plocal(p);
        case TK_WHILE: return solu_pwhile(p);
        case TK_FOR: return solu_pfor(p);

        case TK_BREAK:
        case TK_CONTINUE: {
            solu_node *nd = malloc(sizeof(solu_node));
            *nd = (solu_node){ .tt = SOLU_ND_LCONTROL, p->tok->line, p->tok->column, .n_lcontrol = p->tok->tt };
            ++p->tok;
            if (p->tok->tt != TK_SEMICOLON) {
                solu_node_free(nd);
                return solu_perr(SOLU_ERRP_EXPECTED_SEMICOLON, p->tok);
            }
            ++p->tok;
            return solu_parse_ex_ok(nd);
        }
        case TK_RETURN: return solu_preturn(p);

        case TK_DO:
            ++p->tok;
            __attribute__((fallthrough));
        case TK_SOF: return solu_pblock(p);

        default: {
            solu_parse_ex id = solu_pexpr(p, 0);
            if (!id.is_ok) return id;
            if (p->tok->tt != TK_SEMICOLON) {
                solu_node *n_return = malloc(sizeof(solu_node));
                *n_return = (solu_node){
                    .tt = SOLU_ND_RETURN,
                    .line = id.ok->line, .column = id.ok->column,
                    .n_return = { id.ok, true },
                };
                return solu_parse_ex_ok(n_return);
            }
            ++p->tok;
            return id;
        }
    };
}

solu_parse_ex _solu_parse(sf_str path, solu_tokenvec *tokens, solu_pshared *shared) {
    if (tokens->count == 0)
        return solu_parse_ex_err((solu_parse_err){.tt = SOLU_ERRP_NO_TOKENS});
    solu_parser p = { path, tokens->data, false, shared };
    if (!p.shared)
        return solu_parse_ex_err((solu_parse_err){.tt = SOLU_ERRP_EXPECTED_SHARED});

    solu_parse_ex ex = solu_pstmt(&p);
    if (!shared) {
        solu_includecache_free(&p.shared->includes);
        solu_visited_free(&p.shared->visited);
        free(p.shared);
    }
    return ex;
}
solu_parse_ex solu_parse(sf_str path, solu_scan_ex scan_ex) {
    if (!scan_ex.is_ok) return solu_parse_ex_err((solu_parse_err){
        scan_ex.err.tt, (solu_token){.line = scan_ex.err.line, .column = scan_ex.err.column}
    });

    solu_pshared shared = {scan_ex.ok.alloc, solu_includecache_new(), solu_visited_new()};
    solu_parse_ex e = _solu_parse(path, &scan_ex.ok.tv, &shared);

    solu_includecache_free(&shared.includes);
    solu_visited_free(&shared.visited);
    return e;
}
