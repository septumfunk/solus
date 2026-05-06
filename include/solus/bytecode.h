#ifndef BYTECODE_H
#define BYTECODE_H

#include <sf/str.h>

/// Bytecode version
#define SOLU_VERSION "0.10.2"
/// Git repository, hosted on GitHub for now
#define SOLU_GIT "https://github.com/solus-lang/solus"

/// Bytecode instruction type
typedef enum {
    SOLU_INS_A, // A: i26 (jmp)
    SOLU_INS_AB, // A: u8, B: u18 (load)
    SOLU_INS_ABC, // A: u8, B: u9, C: u9 (most)
} solu_instype;
/// Bytecode Operations
typedef enum {
    SOLU_OP_LOAD,
    SOLU_OP_MOVE,
    SOLU_OP_RET,
    SOLU_OP_JMP,
    SOLU_OP_CALL,
    SOLU_OP_MCALL,

    SOLU_OP_ADD,
    SOLU_OP_SUB,
    SOLU_OP_MUL,
    SOLU_OP_DIV,

    SOLU_OP_NEG,
    SOLU_OP_EQ,
    SOLU_OP_LT,
    SOLU_OP_LE,

    SOLU_OP_SETU,
    SOLU_OP_GETU,
    SOLU_OP_REFU,

    SOLU_OP_NEW,
    SOLU_OP_SET,
    SOLU_OP_GET,
    SOLU_OP_PUSH,

    SOLU_OP_SUPO,
    SOLU_OP_GUPO,

    SOLU_OP_CI64,
    SOLU_OP_CF64,
    SOLU_OP_CBOOL,
    SOLU_OP_CSTR,

    SOLU_OP_UNKNOWN,
    SOLU_OP_COUNT,
} solu_opcode;
/// All instructions are packed into a 32 bit uint
typedef uint32_t solu_instruction;
/// Loading errors from error.def
typedef enum {
#define X(prefix, name, string) SOLU_ERR##prefix##_##name,
#include "error.def"
#undef X
    SOLU_ERR_COUNT
} solu_error;
extern const char *SOLU_ERR_STRINGS[SOLU_ERR_COUNT];
#define solu_err_string(err) (SOLU_ERR_STRINGS[(err)])

/// Masking functions
#define MASKI(n) ((1U << (n)) - 1U)
#define MAXARG_A ((1 << 25) - 1)

/// Store int26 as uint26
#define solu_ins_a_ec(a) ((uint32_t)((a) + MAXARG_A))
#define solu_ins_a_dc(a)  ((int32_t)((a) & MASKI(26U)) - MAXARG_A)

/// Pack type A instruction
#define solu_ins_a(op, as) \
  (((uint32_t)(op) & MASKI(6U)) << 26U | (solu_ins_a_ec(as) & MASKI(26U)))
#define solu_ia_a(i) (solu_ins_a_dc(i)) /// Retrieve operand A

/// Pack type AB instruction
#define solu_ins_ab(op, a, b) \
  (((op) & MASKI(6U)) << 26U) | (((a) & MASKI(8U)) << 18U) | (((b) & MASKI(18U)))
#define solu_iab_a(i) ((i >> 18U) & MASKI(8U)) /// Retrieve operand A
#define solu_iab_b(i) ((i) & MASKI(18U)) /// Retrieve operand B

/// ABC allows B and C to be const
#define RKBIT 0x100 // MSB
#define RKIDX(x) ((x) & 0xFF) // lower 8 bits
#define solu_const(x) (RKBIT | RKIDX(x)) // constant
#define solu_reg(x) RKIDX(x)            // register
/// Pack type ABC instruction
#define solu_ins_abc(op, a, b, c) \
  ((op & MASKI(6U)) << 26U) | ((a & MASKI(8U)) << 18U) | ((b & MASKI(9U)) << 9U) | ((c & MASKI(9U)))
#define solu_iabc_a(i) ((i >> 18U) & MASKI(8U)) /// Retrieve operand A
#define solu_iabc_bx(i) ((i >> 9U) & 0xFF)  /// Retrieve operand B
#define solu_iabc_cx(i) ((i) & 0xFF) /// Retreieve operand C
#define solu_iabc_bk(i) (((i) >> 17U) & 1) // Const flag
#define solu_iabc_ck(i) (((i) >> 8U) & 1)  // Const flag

/// Retrieve opcode
#define solu_ins_op(i) ((i >> 26U) & MASKI(6U))

#define SOLU_DBG_LINE_BITS 16U // uint16_t
#define SOLU_DBG_COL_BITS  16U // uint16_t

/// Pack debug data
#define SOLU_DBG_COL_MASK  ((1u << SOLU_DBG_COL_BITS) - 1u)
#define SOLU_DBG_LINE_MASK ((1u << SOLU_DBG_LINE_BITS) - 1u)
#define SOLU_DBG_ENCODE(line, col) \
    (((uint32_t)(line) & SOLU_DBG_LINE_MASK) << SOLU_DBG_COL_BITS | \
     ((uint32_t)(col)  & SOLU_DBG_COL_MASK) )

/// Retrieve debug line
#define SOLU_DBG_LINE(loc) ((uint16_t)((loc) >> SOLU_DBG_COL_BITS) & (uint16_t)SOLU_DBG_LINE_MASK)
/// Retrieve debug column
#define SOLU_DBG_COL(loc)  ((loc) & SOLU_DBG_COL_MASK)

typedef uint32_t solu_dbg;

/// Instruction signatures, defines how an opcode's operands should look
typedef struct {
    solu_opcode opcode;
    const char *mnemonic;
    solu_instype type;
} solu_inssig;
extern const solu_inssig SOLU_OP_INFO[SOLU_OP_COUNT];
#define solu_op_info(op) (&(SOLU_OP_INFO[(op)]))

EXPORT double solu_timesec(void);
EXPORT char *solu_realpath(const char *path);

/// Get dir of canonized path
EXPORT char *solu_realdir(const char *rp);

/// Disassemble instruction
EXPORT sf_str solu_dasmi(solu_instruction ins);

#endif // BYTECODE_H
