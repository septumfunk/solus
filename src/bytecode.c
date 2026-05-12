#include "solus/bytecode.h"
#include "solus/compat.h"
#include <sf/str.h>
#include <stdio.h>

char *solu_realdir(const char *rp) {
    if (!rp) return NULL;

    char out[4096];
    size_t len = strlen(rp);
    if (len == 0)
        return strdup(".");
    if (len >= sizeof(out))
        return NULL;
    memcpy(out, rp, len + 1);

    while (len > 0 && (out[len - 1] == '/' || out[len - 1] == '\\')) {
        if (len == 1 && (out[0] == '/' || out[0] == '\\'))
            break;
    #ifdef _WIN32
        if (len == 3 && out[1] == ':' &&
            (out[2] == '/' || out[2] == '\\'))
            break;
    #endif
        out[--len] = '\0';
    }

    char *last_slash = NULL;
    for (char *p = out; *p; p++)
        if (*p == '/' || *p == '\\')
            last_slash = p;
    if (!last_slash)
        return strdup(".");
    if (last_slash == out) {
        out[1] = '\0';
        return strdup(out);
    }

#ifdef _WIN32
    if (last_slash == out + 2 && out[1] == ':') {
        out[3] = out[2];
        out[2] = '\0';
        return strdup(out);
    }
#endif

    *last_slash = '\0';
    return strdup(out);
}

sf_str solu_dasmi(solu_instruction ins) {
    const char *op = solu_op_info(solu_ins_op(ins))->mnemonic;
    switch (solu_op_info(solu_ins_op(ins))->type) {
        default:
        case SOLU_INS_A: return sf_str_fmt("%-7s%-8d", op, solu_ia_a(ins));
        case SOLU_INS_AB: return sf_str_fmt("%-7s%-4u%-4u",  op, solu_iab_a(ins), solu_iab_b(ins)); break;
        case SOLU_INS_ABC: return sf_str_fmt("%-7s%-4u%-4u%-4u",  op, solu_iabc_a(ins), solu_iabc_bx(ins), solu_iabc_cx(ins)); break;
    }
}

const char *SOLU_ERR_STRINGS[SOLU_ERR_COUNT] = {
#define X(prefix, name, string) string,
#include "solus/error.def"
#undef X
};

const solu_inssig SOLU_OP_INFO[SOLU_OP_COUNT] = {
    [SOLU_OP_LOAD] = {
        .opcode = SOLU_OP_LOAD,
        .mnemonic = "LOAD",
        .type = SOLU_INS_AB,
    },
    [SOLU_OP_MOVE] = {
        .opcode = SOLU_OP_MOVE,
        .mnemonic = "MOVE",
        .type = SOLU_INS_AB,
    },
    [SOLU_OP_RET] = {
        .opcode = SOLU_OP_RET,
        .mnemonic = "RET",
        .type = SOLU_INS_A,
    },
    [SOLU_OP_JMP] = {
        .opcode = SOLU_OP_JMP,
        .mnemonic = "JMP",
        .type = SOLU_INS_A,
    },
    [SOLU_OP_CALL] = {
        .opcode = SOLU_OP_CALL,
        .mnemonic = "CALL",
        .type = SOLU_INS_ABC,
    },
    [SOLU_OP_MCALL] = {
        .opcode = SOLU_OP_MCALL,
        .mnemonic = "MCALL",
        .type = SOLU_INS_ABC,
    },

    [SOLU_OP_ADD] = {
        .opcode = SOLU_OP_ADD,
        .mnemonic = "ADD",
        .type = SOLU_INS_ABC,
    },
    [SOLU_OP_SUB] = {
        .opcode = SOLU_OP_SUB,
        .mnemonic = "SUB",
        .type = SOLU_INS_ABC,
    },
    [SOLU_OP_DIV] = {
        .opcode = SOLU_OP_DIV,
        .mnemonic = "DIV",
        .type = SOLU_INS_ABC,
    },
    [SOLU_OP_MUL] = {
        .opcode = SOLU_OP_MUL,
        .mnemonic = "MUL",
        .type = SOLU_INS_ABC,
    },

    [SOLU_OP_NEG] = {
        .opcode = SOLU_OP_NEG,
        .mnemonic = "NEG",
        .type = SOLU_INS_AB,
    },
    [SOLU_OP_EQ] = {
        .opcode = SOLU_OP_EQ,
        .mnemonic = "EQ",
        .type = SOLU_INS_ABC,
    },
    [SOLU_OP_LT] = {
        .opcode = SOLU_OP_LT,
        .mnemonic = "LT",
        .type = SOLU_INS_ABC,
    },
    [SOLU_OP_LE] = {
        .opcode = SOLU_OP_LE,
        .mnemonic = "LE",
        .type = SOLU_INS_ABC,
    },

    [SOLU_OP_SETU] = {
        .opcode = SOLU_OP_SETU,
        .mnemonic = "SETU",
        .type = SOLU_INS_AB,
    },
    [SOLU_OP_GETU] = {
        .opcode = SOLU_OP_GETU,
        .mnemonic = "GETU",
        .type = SOLU_INS_AB,
    },
    [SOLU_OP_REFU] = {
        .opcode = SOLU_OP_REFU,
        .mnemonic = "REFU",
        .type = SOLU_INS_A,
    },

    [SOLU_OP_NEW] = {
        .opcode = SOLU_OP_NEW,
        .mnemonic = "NEW",
        .type = SOLU_INS_A,
    },
    [SOLU_OP_SET] = {
        .opcode = SOLU_OP_SET,
        .mnemonic = "SET",
        .type = SOLU_INS_ABC,
    },
    [SOLU_OP_GET] = {
        .opcode = SOLU_OP_GET,
        .mnemonic = "GET",
        .type = SOLU_INS_ABC,
    },
    [SOLU_OP_PUSH] = {
        .opcode = SOLU_OP_PUSH,
        .mnemonic = "PUSH",
        .type = SOLU_INS_AB,
    },

    [SOLU_OP_SUPO] = {
        .opcode = SOLU_OP_SUPO,
        .mnemonic = "SUPO",
        .type = SOLU_INS_ABC,
    },
    [SOLU_OP_GUPO] = {
        .opcode = SOLU_OP_GUPO,
        .mnemonic = "GUPO",
        .type = SOLU_INS_ABC,
    },

    [SOLU_OP_CI64] = {
        .opcode = SOLU_OP_CI64,
        .mnemonic = "CI64",
        .type = SOLU_INS_AB,
    },
    [SOLU_OP_CF64] = {
        .opcode = SOLU_OP_CF64,
        .mnemonic = "CF64",
        .type = SOLU_INS_AB,
    },
    [SOLU_OP_CBOOL] = {
        .opcode = SOLU_OP_CBOOL,
        .mnemonic = "CBOOL",
        .type = SOLU_INS_AB,
    },
    [SOLU_OP_CSTR] = {
        .opcode = SOLU_OP_CSTR,
        .mnemonic = "CSTR",
        .type = SOLU_INS_AB,
    },
    [SOLU_OP_TRY] = {
        .opcode = SOLU_OP_TRY,
        .mnemonic = "TRY",
        .type = SOLU_INS_AB,
    },

    [SOLU_OP_UNKNOWN] = {
        .opcode = SOLU_OP_UNKNOWN,
        .mnemonic = "???",
    }
};

#if defined(_WIN32) || defined(_WIN64)
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
/// Complicated cross platform stuff to get time in seconds
double solu_timesec(void) {
    FILETIME ft;
    ULARGE_INTEGER uli;
    GetSystemTimeAsFileTime(&ft);
    uli.LowPart  = ft.dwLowDateTime;
    uli.HighPart = ft.dwHighDateTime;
    return (double)(uli.QuadPart - 116444736000000000ULL) / 10000000.0;
}
#else
#include <time.h>
#include <sys/time.h>
/// Complicated cross platform stuff to get time in seconds
double solu_timesec(void) {
#if defined(CLOCK_REALTIME)
    struct timespec ts;
    clock_gettime(CLOCK_REALTIME, &ts);
    return (double)ts.tv_sec + (double)ts.tv_nsec / 1e9;
#else
    struct timeval tv;
    gettimeofday(&tv, NULL);
    return (double)tv.tv_sec + (double)tv.tv_usec / 1e6;
#endif
}
#endif

/// Canonize path
#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
char *solu_realpath(const char *base_file, const char *path) {
    char buf[_MAX_PATH];
    if (!_fullpath(buf, path, _MAX_PATH))
        return NULL;
    if (!sf_file_exists(sf_ref(path)))
        return NULL;
    return strdup(buf);
}
#elif defined(POSIX_COMPAT)
char *solu_realpath(const char *path) {
    return strdup(path);
}
#else
char *solu_realpath(const char *path) {
    return realpath(path, NULL);
}
#endif
