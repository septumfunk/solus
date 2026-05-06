#ifndef SYNTAX_H
#define SYNTAX_H

#include "val.h"

typedef struct {
    solu_error tt;
    uint16_t line, column;
} solu_compiledata;

#define VEC_NAME solu_ctrace
#define VEC_T solu_compiledata
#define VSIZE_T uint32_t
#define VSIZE_MAX UINT32_MAX
#include <sf/containers/vec.h>

char *solu_ctrace_print(char *path, solu_ctrace *ct, uint32_t max, uint8_t lookback, uint8_t lookahead);

/// Token type, or character
typedef enum {
    // Statements
    TK_VAL, TK_VAR, TK_DO, TK_IF, TK_ELSE, TK_FOR, TK_WHILE, TK_INCLUDE, TK_TYPEOF,
    TK_BREAK, TK_CONTINUE, TK_RETURN,
    // Operators
    TK_PLUS, TK_MINUS, TK_BANG, TK_NEG, TK_INCREMENT, TK_DECREMENT, TK_ASTERISK, TK_SLASH,
    TK_EQUAL, TK_PLUS_EQUAL, TK_MINUS_EQUAL, TK_STAR_EQUAL, TK_SLASH_EQUAL, TK_COL_EQUAL,
    TK_NOT_EQUAL, TK_DOUBLE_EQUAL, TK_GREATER, TK_GREATER_EQUAL,
    TK_LESS, TK_LESS_EQUAL, TK_AND, TK_OR, TK_ELIPSES,
    // Assembly
    TK_ASM, TK_OPCODE,
    // Identifier/Literals
    TK_IDENTIFIER, TK_STRING, TK_FSTRING, TK_NUMBER, TK_INTEGER,
    TK_NIL, TK_NAN, TK_INF, TK_TRUE, TK_FALSE,
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

struct solu_tokenvec;
#define VEC_NAME solu_tokenvec
#define VEC_T solu_token
#define VSIZE_T uint32_t
#define VSIZE_MAX UINT32_MAX
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
#define EXPECTED_E solu_ctrace *
#include <sf/containers/expected.h>
solu_scan_ex solu_scan(sf_str src, solu_ctrace *ct);

/// Node types that the parser is capable of producing
typedef enum {
    // Statements
    SOLU_ND_LOCAL, SOLU_ND_IF, SOLU_ND_FOR, SOLU_ND_WHILE, SOLU_ND_INS, SOLU_ND_RETURN,
    SOLU_ND_LCONTROL,
    // Operators
    SOLU_ND_UNARY, SOLU_ND_BINARY, SOLU_ND_POSTFIX, SOLU_ND_CALL, SOLU_ND_CAST, SOLU_ND_TYPEOF,
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
        struct { // <let> n = v; // { n = v }
            struct solu_name {
                solu_val name, type;
                struct solu_node *value;
                bool infer;
            } *entries;
            uint16_t entry_c;
            bool mut;
        } n_local;
        struct { // if c {t} else {e}
            struct solu_node *condition;
            struct solu_node *then_node;
            struct solu_node *else_node;
        } n_if;
        struct {
            struct solu_node *pre, *condition, *post;
            struct solu_node *body;
        } n_for;
        struct { // while c {b}
            struct solu_node *condition;
            struct solu_node *stmt;
        } n_while;
        struct {
            solu_opcode op;
            solu_val opa[3];
        } n_ins;
        struct {
            struct solu_node *expr;
            bool implicit;
        } n_return;
        solu_tokentype n_lcontrol;

        struct { // <op> r
            solu_tokentype op;
            struct solu_node *right;
        } n_unary;
        struct { // l <op> r
            solu_tokentype op;
            struct solu_node *left;
            struct solu_node *right;
            bool flip;
        } n_binary;
        struct { // e.p
            solu_tokentype op;
            struct solu_node *expr;
            struct solu_node *postfix;
        } n_postfix;
        struct { // x(a)
            struct solu_node *identifier;
            struct solu_node **args;
            uint32_t arg_c;
            bool variadic;
        } n_call;
        struct {
            solu_val type;
            struct solu_node *expr;
        } n_cast;
        struct {
            struct solu_node *expr;
        } n_typeof;

        solu_val n_identifier, n_literal;
        struct { // { n_binary, }
            struct solu_node **members;
            uint32_t mem_c;
        } n_obj;

        struct solu_block { // {s}
            struct solu_node **stmts;
            uint32_t count;
        } n_block;
        struct {
            solu_val *captures;
            uint32_t cap_c;
            solu_val *args;
            uint32_t arg_c;
            struct solu_node *stmt;
            bool include, variadic;
        } n_fun;
        struct {
            solu_i64 temps;
            struct solu_node *n_fun;
        } n_asm;
    };
} solu_node;

/// Returns whether an expression can evaluate to a bool or not
EXPORT bool solu_niscondition(solu_node *node);
/// Walk through an AST and free all of its nodes
EXPORT void solu_node_free(solu_node *root);

/// An optional type alias for the root node of an AST
typedef solu_node *solu_ast;

#define EXPECTED_NAME solu_parse_ex
#define EXPECTED_O solu_ast
#define EXPECTED_E solu_ctrace *
#include <sf/containers/expected.h>
solu_parse_ex solu_parse(sf_str path, solu_scan_ex scan_ex, solu_ctrace *ct);



#endif // SYNTAX_H
