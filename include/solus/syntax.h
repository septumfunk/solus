#ifndef SYNTAX_H
#define SYNTAX_H

#include <stdint.h>
#include "bytecode.h"

/// Token type, or character
typedef enum {
    // Statements
    TK_VAL, TK_VAR, TK_DO, TK_IF, TK_ELSE, TK_FOR, TK_WHILE, TK_RETURN, TK_INCLUDE,
    TK_BREAK,
    // Operators
    TK_PLUS, TK_MINUS, TK_BANG, TK_INCREMENT, TK_DECREMENT, TK_ASTERISK, TK_SLASH, TK_EQUAL, TK_PLUS_EQUAL,
    TK_NOT_EQUAL, TK_MINUS_EQUAL, TK_DOUBLE_EQUAL, TK_GREATER, TK_GREATER_EQUAL,
    TK_LESS, TK_LESS_EQUAL, TK_AND, TK_OR,
    // Assembly
    TK_ASM, TK_OPCODE,
    // Identifier/Literals
    TK_IDENTIFIER, TK_STRING, TK_NUMBER, TK_INTEGER, TK_NIL, TK_TRUE, TK_FALSE,
    // Misc
    TK_LEFT_PAREN, TK_RIGHT_PAREN, TK_LEFT_BRACE, TK_RIGHT_BRACE,
    TK_LEFT_BRACKET, TK_RIGHT_BRACKET, TK_COMMA, TK_PERIOD,
    TK_SEMICOLON, TK_COLON, TK_SOF, TK_EOF
} solu_tokentype;

/// Token info
typedef struct {
    solu_tokentype tt;
    solu_val value;
    uint16_t line, column;
} solu_token;

typedef struct {
    solu_error tt;
    sf_str token;
    uint16_t line, column;
} solu_scan_err;

struct solu_tokenvec;
#define VEC_NAME solu_tokenvec
#define VEC_T solu_token
#define VSIZE_T uint32_t
#include <sf/containers/vec.h>

/// Maps keyword strings to tokentype
struct solu_keywords;
void _solu_keywords_cleanup(struct solu_keywords *vec);
#define MAP_NAME solu_keywords
#define MAP_K sf_str
#define MAP_V solu_tokentype
#define EQUAL_FN(s1, s2) (sf_str_eq(s1, s2))
#define HASH_FN(s) (sf_str_hash(s))
#define CLEANUP_FN _solu_keywords_cleanup
#define KCLEANUP sf_str_free
#include <sf/containers/map.h>

typedef struct {
    solu_tokenvec tv;
    solu_dalloc *alloc;
} solu_scan_ok;
#define EXPECTED_NAME solu_scan_ex
#define EXPECTED_O solu_scan_ok
#define EXPECTED_E solu_scan_err
#include <sf/containers/expected.h>
EXPORT solu_scan_ex solu_scan(sf_str src);

/// Node types that the parser is capable of producing
typedef enum {
    // Statements
    SOLU_ND_LOCAL, SOLU_ND_IF, SOLU_ND_FOR, SOLU_ND_WHILE, SOLU_ND_INS, SOLU_ND_RETURN,
    SOLU_ND_BREAK,
    // Operators
    SOLU_ND_UNARY, SOLU_ND_BINARY, SOLU_ND_MEMBER, SOLU_ND_CALL,
    // Literals
    SOLU_ND_IDENTIFIER, SOLU_ND_LITERAL, SOLU_ND_OBJ,
    // Functions
    SOLU_ND_BLOCK, SOLU_ND_FUN, SOLU_ND_ASM,
} solu_nodetype;

/// A node in the AST (Abstract Syntax Tree) that the parser exports.
/// The compiler walks these to make the bytecode :)
typedef struct solu_node {
    solu_nodetype tt;
    uint16_t line, column;
    union {
        solu_val n_literal, n_identifier;
        struct {
            struct solu_node *expr;
            bool implicit;
        } n_return;
        struct { // e.p
            struct solu_node *expr;
            solu_val postfix;
        } n_postfix;
        struct { // <op> r
            solu_tokentype op;
            struct solu_node *right;
        } n_unary;
        struct { // l <op> r
            solu_tokentype op;
            struct solu_node *left;
            struct solu_node *right;
        } n_binary;
        struct { // <let> n = v; // { n = v }
            solu_val name;
            struct solu_node *value;
            bool mut;
        } n_local;
        struct {
            struct solu_node *expr;
            struct solu_node *value;
        } n_assign;
        struct { // if c {t} else {e}
            struct solu_node *condition;
            struct solu_node *then_node;
            struct solu_node *else_node;
        } n_if;
        struct { // x(a)
            struct solu_node *identifier;
            struct solu_node **args;
            uint32_t arg_c;
        } n_call;
        struct {
            struct solu_node *pre, *condition, *post;
            struct solu_node *body;
        } n_for;
        struct { // while c {b}
            struct solu_node *condition;
            struct solu_node *stmt;
        } n_while;
        struct solu_block { // {s}
            struct solu_node **stmts;
            uint32_t count;
        } n_block;

        struct { // [c](a) {b}
            solu_val *captures;
            uint32_t cap_c;
            solu_val *args;
            uint32_t arg_c;
            struct solu_node *block;
            bool include;
        } n_fun;

        struct {
            solu_i64 temps;
            struct solu_node *n_fun;
        } n_asm;
        struct {
            solu_opcode op;
            solu_val opa[3];
        } n_ins;

        struct { // { n_binary, }
            struct solu_node **members;
            uint32_t mem_c;
        } n_obj;
    };
} solu_node;

/// Returns whether an expression can evaluate to a bool or not
EXPORT bool solu_niscondition(solu_node *node);
/// Walk through an AST and free all of its nodes
EXPORT void solu_node_free(solu_node *root);

/// A possible result of parsing, describes what went wrong and where
typedef struct {
    solu_error tt;
    uint16_t line, column;
} solu_parse_err;
/// An optional type alias for the root node of an AST
typedef solu_node *solu_ast;

#define EXPECTED_NAME solu_parse_ex
#define EXPECTED_O solu_ast
#define EXPECTED_E solu_parse_err
#include <sf/containers/expected.h>
EXPORT solu_parse_ex solu_parse(sf_str path, solu_scan_ex scan_ex);

#endif // SYNTAX_H
