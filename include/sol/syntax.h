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
} sol_tokentype;

typedef struct {
    sol_tokentype tt;
    sol_val value;
    uint16_t line, column;
} sol_token;

typedef struct {
    sol_error tt;
    sf_str token;
    uint16_t line, column;
} sol_scan_err;

struct sol_tokenvec;
#define VEC_NAME sol_tokenvec
#define VEC_T sol_token
#define VSIZE_T uint32_t
#include <sf/containers/vec.h>

struct sol_keywords;
void _sol_keywords_cleanup(struct sol_keywords *vec);
#define MAP_NAME sol_keywords
#define MAP_K sf_str
#define MAP_V sol_tokentype
#define EQUAL_FN(s1, s2) (sf_str_eq(s1, s2))
#define HASH_FN(s) (sf_str_hash(s))
#define CLEANUP_FN _sol_keywords_cleanup
#include <sf/containers/map.h>

typedef struct {
    sol_tokenvec tv;
    sol_dalloc *alloc;
} sol_scan_ok;
#define EXPECTED_NAME sol_scan_ex
#define EXPECTED_O sol_scan_ok
#define EXPECTED_E sol_scan_err
#include <sf/containers/expected.h>
EXPORT sol_scan_ex sol_scan(sf_str src);

/// Node types that the parser is capable of producing
typedef enum {
    SOL_ND_BLOCK,

    SOL_ND_IDENTIFIER,
    SOL_ND_MEMBER,
    SOL_ND_LITERAL,

    SOL_ND_LET,
    SOL_ND_ASSIGN,

    SOL_ND_BINARY,
    SOL_ND_CALL,
    SOL_ND_FUN,

    SOL_ND_ASM,
    SOL_ND_INS,

    SOL_ND_IF,
    SOL_ND_WHILE,
    SOL_ND_RETURN,
} sol_nodetype;

/// A node in the AST (Abstract Syntax Tree) that the parser exports.
/// The compiler walks these to make the bytecode :)
typedef struct sol_node {
    sol_nodetype tt;
    uint16_t line, column;
    union {
        sol_val n_literal, n_identifier;
        struct sol_node *n_return;
        struct { // e.p
            struct sol_node *expr;
            sol_val postfix;
        } n_postfix;
        struct { // l <op> r
            sol_tokentype op;
            struct sol_node *left;
            struct sol_node *right;
        } n_binary;
        struct { // <let> n = v; // { n = v }
            sol_val name;
            struct sol_node *value;
        } n_let, n_member;
        struct {
            struct sol_node *expr;
            struct sol_node *value;
        } n_assign;
        struct { // if c {t} else {e}
            struct sol_node *condition;
            struct sol_node *then_node;
            struct sol_node *else_node;
        } n_if;
        struct { // x(a)
            struct sol_node *identifier;
            struct sol_node **args;
            uint32_t arg_c;
        } n_call;
        struct { // while c {b}
            struct sol_node *condition;
            struct sol_node *block;
        } n_while;
        struct sol_block { // {s}
            struct sol_node **stmts;
            uint32_t count;
        } n_block;

        struct { // [c](a) {b}
            sol_val *captures;
            uint32_t cap_c;
            sol_val *args;
            uint32_t arg_c;
            struct sol_node *block;
        } n_fun;

        struct {
            sol_i64 temps;
            struct sol_node *n_fun;
        } n_asm;
        struct {
            sol_opcode op;
            sol_val opa[3];
        } n_ins;

        struct { // { n_member, }
            struct sol_node **members;
        } n_obj;
    };
} sol_node;

/// Returns whether an expression can evaluate to a bool or not
EXPORT bool sol_niscondition(sol_node *node);
/// Walk through an AST and free all of its nodes
EXPORT void sol_node_free(sol_node *root);

/// A possible result of parsing, describes what went wrong and where
typedef struct {
    sol_error tt;
    uint16_t line, column;
} sol_parse_err;
/// An optional type alias for the root node of an AST
typedef sol_node *sol_ast;

#define EXPECTED_NAME sol_parse_ex
#define EXPECTED_O sol_ast
#define EXPECTED_E sol_parse_err
#include <sf/containers/expected.h>
EXPORT sol_parse_ex sol_parse(sol_tokenvec *tokens);

#endif // SYNTAX_H
