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

    TK_ASM, TK_OPCODE,

    TK_IDENTIFIER, TK_STRING, TK_NUMBER, TK_INTEGER,

    TK_AND, TK_ELSE, TK_TRUE, TK_FALSE, TK_FUN, TK_FOR, TK_IF, TK_NIL, TK_OR,
    TK_RETURN, TK_LET, TK_WHILE,

    TK_EOF
} ctr_tokentype;

typedef struct {
    ctr_tokentype tt;
    ctr_val value;
    uint16_t line, column, len;
} ctr_token;

typedef struct {
    ctr_error tt;
    sf_str token;
    uint16_t line, column;
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

/// Node types that the parser is capable of producing
typedef enum {
    CTR_ND_BLOCK,

    CTR_ND_IDENTIFIER,
    CTR_ND_MEMBER,
    CTR_ND_LITERAL,

    CTR_ND_LET,
    CTR_ND_ASSIGN,

    CTR_ND_BINARY,
    CTR_ND_CALL,
    CTR_ND_FUN,

    CTR_ND_ASM,
    CTR_ND_INS,

    CTR_ND_IF,
    CTR_ND_WHILE,
    CTR_ND_RETURN,
} ctr_nodetype;

/// A node in the AST (Abstract Syntax Tree) that the parser exports.
/// The compiler walks these to make the bytecode :)
typedef struct ctr_node {
    ctr_nodetype tt;
    uint16_t line, column;
    union {
        ctr_val n_literal, n_identifier;
        struct ctr_node *n_return;
        struct { // e.p
            struct ctr_node *expr;
            ctr_val postfix;
        } n_postfix;
        struct { // l <op> r
            ctr_tokentype op;
            struct ctr_node *left;
            struct ctr_node *right;
        } n_binary;
        struct { // <let> n = v; // { n = v }
            ctr_val name;
            struct ctr_node *value;
        } n_let, n_member;
        struct {
            struct ctr_node *expr;
            struct ctr_node *value;
        } n_assign;
        struct { // if c {t} else {e}
            struct ctr_node *condition;
            struct ctr_node *then_node;
            struct ctr_node *else_node;
        } n_if;
        struct { // x(a)
            struct ctr_node *identifier;
            struct ctr_node **args;
            uint32_t arg_c;
        } n_call;
        struct { // while c {b}
            struct ctr_node *condition;
            struct ctr_node *block;
        } n_while;
        struct ctr_block { // {s}
            struct ctr_node **stmts;
            uint32_t count;
        } n_block;

        struct { // [c](a) {b}
            ctr_val *captures;
            uint32_t cap_c;
            ctr_val *args;
            uint32_t arg_c;
            struct ctr_node *block;
        } n_fun;

        struct {
            ctr_i64 temps;
            struct ctr_node *n_fun;
        } n_asm;
        struct {
            ctr_opcode op;
            ctr_val opa[3];
        } n_ins;

        struct { // { n_member, }
            struct ctr_node **members;
        } n_obj;
    };
} ctr_node;

/// Returns whether an expression can evaluate to a bool or not
EXPORT bool ctr_niscondition(ctr_node *node);
/// Walk through an AST and free all of its nodes
EXPORT void ctr_node_free(ctr_node *root);

/// A possible result of parsing, describes what went wrong and where
typedef struct {
    ctr_error tt;
    uint16_t line, column;
} ctr_parse_err;
/// An optional type alias for the root node of an AST
typedef ctr_node *ctr_ast;

#define EXPECTED_NAME ctr_parse_ex
#define EXPECTED_O ctr_ast
#define EXPECTED_E ctr_parse_err
#include <sf/containers/expected.h>
EXPORT ctr_parse_ex ctr_parse(ctr_tokenvec *tokens);

#endif // SYNTAX_H
