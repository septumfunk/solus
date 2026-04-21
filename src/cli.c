#include "sf/containers/buffer.h"
#include "solus/bytecode.h"
#include "solus/val.h"
#include "solus/compiler.h"
#include "solus/vm.h"
#include <sf/str.h>
#include <sf/fs.h>
#include <stdio.h>
#include <string.h>

#ifndef _WIN32
#define TUI_UL  "\x1b[4m"
#define TUI_BLD "\x1b[1m"
#define TUI_ERR "\x1b[1;31m"
#define TUI_CLR "\x1b[0m"
#else
#define TUI_UL  ""
#define TUI_BLD ""
#define TUI_ERR ""
#define TUI_CLR ""
#endif

#ifdef _WIN32
//TODO: pdcurses
#else
#include <ncurses.h>
#endif

typedef enum {
    CLI_RUN,
    CLI_TEST,
    CLI_COMPILE,
} cli_mode;

static void cli_print_line(sf_str src, uint16_t line) {
    if (!src.c_str || src.len == 0 || line == 0)
        return;

    const char *line_start = src.c_str;
    uint16_t ln = 1;
    for (const char *p = src.c_str; p < src.c_str + src.len && ln < line; ++p) {
        if (*p == '\n') {
            ++ln;
            line_start = p + 1;
        }
    }
    if (ln != line || line_start >= src.c_str + src.len) return;

    const char *line_end = line_start;
    while (line_end < src.c_str + src.len && *line_end != '\n' && *line_end != '\0')
        ++line_end;
    if (line_end > line_start && line_end[-1] == '\r')
        --line_end;
    fprintf(stderr, "%4u | %.*s\n", line, (int)(line_end - line_start), line_start);
}

void cli_highlight_line(sf_str src, sf_str err, uint16_t line, uint16_t column, uint8_t lookback, uint8_t lookahead) {
    for (uint16_t i = line <= lookback ? 1 : line - lookback + 1; i < line + 1; ++i)
        cli_print_line(src, i);
    if (err.c_str == solu_err_string(SOLU_ERRP_EXPECTED_SEMICOLON))
        ++column;

    int prefix = snprintf(NULL, 0, "%4u | ", line);
    int caret = prefix + column - 1;

    char *pointer = malloc((size_t)caret + 2);
    memset(pointer, '~', (size_t)caret);
    pointer[caret] = '^';
    pointer[caret + 1] = '\0';

    fprintf(stderr, TUI_ERR "%s %s\n" TUI_CLR, pointer, err.c_str);
    for (uint16_t i = line + 1; i < line + lookahead + 1; ++i)
        cli_print_line(src, i);

    free(pointer);
}

sf_str cli_load_file(char *name) {
    sf_str f = sf_own(name);
    if (!sf_file_exists(f)) {
        fprintf(stderr, TUI_ERR "error: file '%s' not found.\n", name);
        return SF_STR_EMPTY;
    }
    sf_fsb_ex fsb = sf_file_buffer(f);
    if (!fsb.is_ok) {
        switch (fsb.err) {
            case SF_FILE_NOT_FOUND: fprintf(stderr, TUI_ERR "error: file '%s' not found\n" TUI_CLR, name); break;
            case SF_OPEN_FAILURE: fprintf(stderr, TUI_ERR "error: file '%s' failed to open\n" TUI_CLR, name); break;
            case SF_READ_FAILURE: fprintf(stderr, TUI_ERR "error: file '%s' failed to read\n" TUI_CLR, name); break;
        }
        return SF_STR_EMPTY;
    }
    fsb.ok.flags = SF_BUFFER_GROW;
    sf_buffer_seek(&fsb.ok, SF_BUFFER_END, 0);
    sf_buffer_autoins(&fsb.ok, ""); // [\0]
    sf_buffer_seek(&fsb.ok, SF_BUFFER_START, 0);
    return sf_own((char *)fsb.ok.ptr);
}

int cli_run(char *path, sf_str src) {
    solu_state *s = solu_state_new();
    solu_usestd(s);

    sf_fsb_ex fsb = sf_file_buffer(sf_ref(path));
    if (!fsb.is_ok) {
        fprintf(stderr, TUI_ERR "error: File '%s' not found\n" TUI_CLR, path);
        solu_state_free(s);
        return -1;
    }
    bool is_solc = memcmp(fsb.ok.ptr, "[SOLC]", 6) == 0;
    solu_fproto fb;
    if (is_solc) {
        solu_load_ex lex = solu_loadfun(s, path);
        if (!lex.is_ok) {
            fprintf(stderr, TUI_ERR "error: %s\n" TUI_CLR, solu_err_string(lex.err));
            solu_state_free(s);
            return -1;
        }
        fb = lex.ok;
    } else {
        solu_compile_ex comp_ex = solu_cfile(s, path);
        if (!comp_ex.is_ok) {
            if (comp_ex.err.line) {
                fprintf(stderr, TUI_ERR "error: %s:%u:%u\n" TUI_CLR, path, comp_ex.err.line, comp_ex.err.column);
                cli_highlight_line(src, sf_ref(solu_err_string(comp_ex.err.tt)), comp_ex.err.line, comp_ex.err.column, 2, 2);
            } else fprintf(stderr, TUI_ERR "error: %s\n" TUI_CLR, solu_err_string(comp_ex.err.tt));            solu_state_free(s);
            return -1;
        }
        fb = comp_ex.ok;
    }
    sf_buffer_clear(&fsb.ok);

    solu_call_ex call_ex = solu_call(s, &fb, NULL, 0);
    if (!call_ex.is_ok) {
        uint16_t line = 0, col = 0;
        if (fb.dbg) {
            line = SOLU_DBG_LINE(fb.dbg[call_ex.err.pc]);
            col = SOLU_DBG_COL(fb.dbg[call_ex.err.pc]);
            fprintf(stderr, TUI_ERR "error: %s:%u:%u\n" TUI_CLR, path, line, col);
        } else fprintf(stderr, TUI_ERR "error: %s\n" TUI_CLR, path);


        if (call_ex.err.panic) {
            sf_str full = sf_str_fmt("%s: %s", solu_err_string(call_ex.err.tt), call_ex.err.panic);
            if (line)
                cli_highlight_line(src, full, line, col, 2, 2);
            else
                fprintf(stderr, TUI_ERR TUI_BLD "%s\n" TUI_CLR, full.c_str);
            free(call_ex.err.panic);
            sf_str_free(full);
        } else if (line)
            cli_highlight_line(src, sf_ref(solu_err_string(call_ex.err.tt)), line, col, 2, 2);
        else
            fprintf(stderr, TUI_ERR TUI_BLD "%s\n" TUI_CLR, solu_err_string(call_ex.err.tt));
        return -1;
    }

    char *ret = solu_tostr(s, call_ex.ok);
    printf(solu_isdtype(call_ex.ok, SOLU_DSTR) ? TUI_BLD "Returned: (%s) '%s'\n" : TUI_BLD "Returned: (%s) %s\n",
        solu_typename(call_ex.ok).c_str, ret);

    free(ret);
    solu_fproto_free(&fb);
    solu_state_free(s);
    return 0;
}

int cli_compile(char *path, sf_str src) {
    solu_state *s = solu_state_new();
    solu_usestd(s);
    solu_compile_ex comp_ex = solu_cfile(s, path);
    if (!comp_ex.is_ok) {
        if (comp_ex.err.line) {
            fprintf(stderr, TUI_ERR "error: %s:%u:%u\n" TUI_CLR, path, comp_ex.err.line, comp_ex.err.column);
            cli_highlight_line(src, sf_ref(solu_err_string(comp_ex.err.tt)), comp_ex.err.line, comp_ex.err.column, 2, 2);
        } else fprintf(stderr, TUI_ERR "error: %s\n" TUI_CLR, solu_err_string(comp_ex.err.tt));
        solu_state_free(s);
        return -1;
    }

    sf_str pstr = sf_own(solu_realpath(path));
    if (!pstr.c_str) {
        fprintf(stderr, TUI_ERR "error: Unknown\n" TUI_CLR);
        solu_fproto_free(&comp_ex.ok);
        solu_state_free(s);
        return -1;
    }
    char *end = pstr.c_str + pstr.len;
    if (pstr.len >= 5 && strcmp(end - 5, ".solu") == 0) {
        memcpy(pstr.c_str + pstr.len - 5, ".solc", 5);
    } else if (pstr.len >= 6 && strcmp(end - 6, ".solus") == 0) {
        memcpy(pstr.c_str + pstr.len - 6, ".solc\0", 6);
    }

    solu_savefun(&comp_ex.ok, pstr.c_str);
    solu_fproto_free(&comp_ex.ok);
    solu_state_free(s);
    printf(TUI_BLD "Compiled file '%s' successfully.\n", pstr.c_str);
    sf_str_free(pstr);
    return 0;
}

int cli_tf(char *path, sf_str src) {
    printf(TUI_BLD TUI_UL "Test '%s'\n" TUI_CLR, path);
    double start = solu_timesec();
    int ret = cli_run(path, src);
    printf( ret == 0 ? (TUI_BLD "Success: %fs\n" TUI_CLR) : (TUI_BLD "Failure: %fs\n" TUI_CLR), solu_timesec() - start);
    return ret;
}

static int has_suffix_solu(const char *s) {
    size_t n = strlen(s);
    return (n >= 5 && memcmp(s + (n - 5), ".solu", 5) == 0) ||
           (n >= 6 && memcmp(s + (n - 6), ".solus", 6) == 0);
}
#if defined(_WIN32) || defined(_WIN64)
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
int cli_test(char *dirpath) {
    size_t len = strlen(dirpath);
    while (dirpath[len - 1] == '\\' || dirpath[len - 1] == '/') {
        dirpath[len - 1] = '\0';
        len -= 1;
    }

    char pattern[MAX_PATH];
    snprintf(pattern, sizeof(pattern), "%s\\*", dirpath);

    WIN32_FIND_DATAA fd;
    HANDLE h = FindFirstFileA(pattern, &fd);
    if (h == INVALID_HANDLE_VALUE) {
        fprintf(stderr, "FindFirstFileA failed for '%s'\n", pattern);
        return 1;
    }

    int printed_any = 0;
    do {
        if (fd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
            continue;
        if (!has_suffix_solu(fd.cFileName))
            continue;
        char full[MAX_PATH];
        snprintf(full, sizeof(full), "%s/%s", dirpath, fd.cFileName);
        if (printed_any++) printf("\n");

        sf_fsb_ex fsb = sf_file_buffer(sf_ref(full));
        if (!fsb.is_ok) {
            fprintf(stderr, TUI_ERR TUI_UL "Test %s failed to open!\n" TUI_CLR, full);
            continue;
        }
        cli_tf(full, (sf_str){(char *)fsb.ok.ptr, fsb.ok.size - 1, SF_STR_NONE});
        sf_buffer_clear(&fsb.ok);

    } while (FindNextFileA(h, &fd));

    FindClose(h);
    return 0;
}
#else
#include <dirent.h>
int cli_test(char *dirpath) {
    DIR *dir = opendir(dirpath);
    if (!dir) {
        perror("opendir");
        return 1;
    }

    struct dirent *ent;
    int printed_any = 0;
    while ((ent = readdir(dir)) != NULL) {
        if (strcmp(ent->d_name, ".") == 0 || strcmp(ent->d_name, "..") == 0)
            continue;
        if (!has_suffix_solu(ent->d_name))
            continue;

        char full[1024];
        snprintf(full, sizeof(full), "%s%s", dirpath, ent->d_name);
        if (printed_any++) printf("\n");

        sf_fsb_ex fsb = sf_file_buffer(sf_ref(full));
        if (!fsb.is_ok) {
            fprintf(stderr, TUI_ERR TUI_UL "Test %s failed to open!\n" TUI_CLR, full);
            continue;
        }
        sf_buffer_seek(&fsb.ok, SF_BUFFER_END, 0);
        cli_tf(full, (sf_str){(char *)fsb.ok.ptr, fsb.ok.size - 1, SF_STR_NONE});
        sf_buffer_clear(&fsb.ok);
    }

    closedir(dir);
    return 0;
}
#endif

int main(int argc, char **argv) {
    if (argc == 1) {
        char *fp = sf_str_join(sf_ref(solu_realdir(argv[0])), sf_lit("/bundle.solc")).c_str;
        if (sf_file_exists(sf_ref(fp))) {
            sf_str src = cli_load_file(fp);
            free(fp);
            if (sf_isempty(src) || src.len == 0)
                return 1;
            return cli_run("bundle.solc", src);
        }
        free(fp);
        printf("Usage: %s [run|compile|test] <file>\n", argv[0]);
        return 1;
    }

    cli_mode mode;
    if (!strcmp(argv[1], "run")) {
        if (argc == 2) {
            printf("Usage: %s run <file>\n", argv[0]);
            return 1;
        }
        mode = CLI_RUN;
    } else if (!strcmp(argv[1], "test")) {
        if (argc == 2) {
            printf("Usage: %s test <dir>\n", argv[0]);
            return 1;
        }
        mode = CLI_TEST;
    } else if (!strcmp(argv[1], "compile")) {
        if (argc == 2) {
            printf("Usage: %s compile <entry>\n", argv[0]);
            return 1;
        }
        mode = CLI_COMPILE;
    } else {
        printf("Unknown option '%s'.\nUsage: %s [run|test] <file|dir>\n", argv[1], argv[0]);
        return 1;
    }

    if (mode == CLI_TEST)
        return cli_test(argv[2]);

    sf_str src = cli_load_file(argv[2]);
    if (sf_isempty(src) || src.len == 0)
        return 1;

    int ret = 0;
    switch (mode) {
        case CLI_RUN: ret = cli_run(argv[2], src); break;
        case CLI_COMPILE: ret = cli_compile(argv[2], src); break;
        default: ret = -1; break;
    }
    sf_str_free(src);
    return ret;
}
