#ifndef SYNTAX_H
#define SYNTAX_H

#include <stdint.h>
#include "bytecode.h"

typedef enum {
    TK_SOF,
    TK_LEFT_PAREN, TK_RIGHT_PAREN, TK_LEFT_BRACE, TK_RIGHT_BRACE,
    TK_LEFT_BRACKET, TK_RIGHT_BRACKET,
    TK_COMMA, TK_PERIOD, TK_MINUS, TK_PLUS, TK_SEMICOLON, TK_SLASH, TK_ASTERISK,

    TK_BANG, TK_NOT_EQUAL,
    TK_EQUAL, TK_DOUBLE_EQUAL,
    TK_GREATER, TK_GREATER_EQUAL,
    TK_LESS, TK_LESS_EQUAL,

    TK_IDENTIFIER, TK_STRING, TK_NUMBER, TK_INTEGER,

    TK_AND, TK_ELSE, TK_TRUE, TK_FALSE, TK_FUN, TK_FOR, TK_IF, TK_NIL, TK_OR,
    TK_RETURN, TK_LET, TK_WHILE,

    TK_EOF
} ctr_tokentype;

typedef struct {
    ctr_tokentype tt;
    ctr_val value;
    uint32_t line, column;
} ctr_token;

typedef struct {
    enum ctr_scan_errt {
        CTR_ERRS_UNEXPECTED_TOKEN,
        CTR_ERRS_UNTERMINATED_STR,
        CTR_ERRS_NUMBER_FORMAT,
    } tt;
    sf_str token;
    uint32_t line, column;
} ctr_scan_err;

struct ctr_tokenvec;
void _ctr_tokenvec_cleanup(struct ctr_tokenvec *vec);
#define VEC_NAME ctr_tokenvec
#define VEC_T ctr_token
#define CLEANUP_FN _ctr_tokenvec_cleanup
#include <sf/containers/vec.h>

struct ctr_keywords;
void _ctr_keywords_cleanup(struct ctr_keywords *vec);
#define MAP_NAME ctr_keywords
#define MAP_K sf_str
#define MAP_V ctr_tokentype
#define EQUAL_FN(s1, s2) (sf_str_eq(s1, s2))
#define HASH_FN(s) (sf_str_hash(s))
#define CLEANUP_FN _ctr_keywords_cleanup
#include <sf/containers/map.h>

#define EXPECTED_NAME ctr_scan_ex
#define EXPECTED_O ctr_tokenvec
#define EXPECTED_E ctr_scan_err
#include <sf/containers/expected.h>
EXPORT ctr_scan_ex ctr_scan(sf_str src);

typedef enum {
    CTR_ND_BINARY,
    CTR_ND_IDENTIFIER,
    CTR_ND_LITERAL,
    CTR_ND_LET,
    CTR_ND_ASSIGN,
    CTR_ND_CALL,
    CTR_ND_IF,
    CTR_ND_BLOCK,
    CTR_ND_FUN,
    CTR_ND_WHILE,
    CTR_ND_RETURN,
} ctr_nodetype;

typedef struct ctr_node {
    ctr_nodetype tt;
    size_t line, column;
    union ctr_ninner {
        ctr_val literal, identifier;
        struct ctr_node *stmt_ret;
        struct {
            ctr_tokentype tt;
            struct ctr_node *left;
            struct ctr_node *right;
        } binary;
        struct {
            ctr_val name;
            struct ctr_node *value;
        } stmt_let, stmt_assign;
        struct {
            struct ctr_node *condition;
            struct ctr_node *then_node;
            struct ctr_node *else_node;
        } stmt_if;
        struct {
            ctr_val name;
            struct ctr_node **args;
            uint32_t arg_c;
        } stmt_call;
        struct {
            struct ctr_node *condition;
            struct ctr_node *block;
        } stmt_while;
        struct ctr_block {
            struct ctr_node **stmts;
            uint32_t count;
        } block;
        struct {
            ctr_val *captures;
            uint32_t cap_c;
            ctr_val *args;
            uint32_t arg_c;
            struct ctr_node *block;
        } fun;
    } inner;
} ctr_node;

EXPORT bool ctr_niscondition(ctr_node *node);
EXPORT void ctr_node_free(ctr_node *tree);

typedef struct {
    enum ctr_parse_errt {
        CTR_ERRP_NO_TOKENS,
        CTR_ERRP_EXPECTED_EXPRESSION,
        CTR_ERRP_EXPECTED_IDENTIFIER,
        CTR_ERRP_EXPECTED_LPAREN,
        CTR_ERRP_EXPECTED_RPAREN,
        CTR_ERRP_EXPECTED_RBRACE,
        CTR_ERRP_EXPECTED_EQUAL,
        CTR_ERRP_EXPECTED_SEMICOLON,
        CTR_ERRP_EXPECTED_CONDITION,
        CTR_ERRP_EXPECTED_BLOCK,
        CTR_ERRP_EXPECTED_STMT,
        CTR_ERRP_EXPECTED_ARGS,
        CTR_ERRP_UNTERMINATED_ARGS,
        CTR_ERRP_UNTERMINATED_CAPTURES,
    } tt;
    size_t line, column;
} ctr_parse_err;

typedef ctr_node *ctr_ast;

#define EXPECTED_NAME ctr_parse_ex
#define EXPECTED_O ctr_ast
#define EXPECTED_E ctr_parse_err
#include <sf/containers/expected.h>
EXPORT ctr_parse_ex ctr_parse(ctr_tokenvec *tokens);

#endif // SYNTAX_H
