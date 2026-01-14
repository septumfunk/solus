#include "sol/bytecode.h"
#include "sol/solc.h"
#include "sol/vm.h"
#include "sol/cli.h"
#include <sf/str.h>
#include <sf/fs.h>
#include <stdio.h>
#include <string.h>

#ifdef _WIN32
//TODO: pdcurses
#else
#include <ncurses.h>
#endif

typedef enum {
    CLI_RUN,
    CLI_DBG,
} cli_mode;

void cli_highlight_line(sf_str src, sf_str err, uint16_t line, uint16_t column) {
    char *c = src.c_str, *cc = c;
    uint16_t ln = 1;
    while (true) {
        if (*c == '\n')
            ++ln;
        if (ln == line + 1 || *c == '\0') {
            cc = c;
            *c = '\0';
            while ((c == src.c_str || *(c-1) != '\n') && c != src.c_str)
                --c;
            break;
        } else ++c;
    }
    fprintf(stderr, /* "%d | " */ "%s\n", /* line, */ c);
    *cc = '\n';

    char *pointer = malloc(column);
    memset(pointer, '~', column);
    pointer[sizeof(pointer) - 1] = '\0';
    pointer[sizeof(pointer) - 2] = '^';

    if (err.c_str == sol_err_string(SOL_ERRP_EXPECTED_SEMICOLON).c_str)
        fprintf(stderr, TUI_ERR "%s\n" TUI_CLR, err.c_str);
    else fprintf(stderr, TUI_ERR "%s %s\n" TUI_CLR, pointer, err.c_str);
    free(pointer);
}

sf_str cli_load_file(char *name) {
    sf_str f = sf_lit(name);
    if (!sf_file_exists(f)) {
        fprintf(stderr, TUI_ERR "error: file '%s' not found.", name);
        return SF_STR_EMPTY;
    }
    sf_fsb_ex fsb = sf_file_buffer(f);
    if (!fsb.is_ok) {
        switch (fsb.err) {
            case SF_FILE_NOT_FOUND: fprintf(stderr, TUI_ERR "error: file '%s' not found" TUI_CLR, name); break;
            case SF_OPEN_FAILURE: fprintf(stderr, TUI_ERR "error: file '%s' failed to open" TUI_CLR, name); break;
            case SF_READ_FAILURE: fprintf(stderr, TUI_ERR "error: file '%s' failed to read" TUI_CLR, name); break;
        }
        return SF_STR_EMPTY;
    }
    fsb.ok.flags = SF_BUFFER_GROW;
    sf_buffer_autoins(&fsb.ok, ""); // [\0]
    return sf_own((char *)fsb.ok.ptr);
}

int cli_run(char *path, sf_str src) {
    sol_state *s = sol_state_new();
    sol_usestd(s);
    sol_compile_ex comp_ex = sol_cfile(s, sf_ref(path));
    if (!comp_ex.is_ok) {
        fprintf(stderr, TUI_ERR "error: %s:%u:%u\n" TUI_CLR, path, comp_ex.err.line, comp_ex.err.column);
        cli_highlight_line(src, sol_err_string(comp_ex.err.tt), comp_ex.err.line, comp_ex.err.column);
        sol_state_free(s);
        return -1;
    }

    sol_fproto *fun = &comp_ex.ok;
    sol_call_ex call_ex = sol_call(s, fun, NULL);
    if (!call_ex.is_ok) {
        uint16_t line = SOL_DBG_LINE(fun->dbg[call_ex.err.pc]), col = SOL_DBG_COL(fun->dbg[call_ex.err.pc]);
        fprintf(stderr, TUI_ERR "error: %s:%u:%u\n" TUI_CLR, path, line, col);

        if (!sf_isempty(call_ex.err.panic)) {
            sf_str full = sf_str_fmt("%s: %s", sol_err_string(call_ex.err.tt).c_str, call_ex.err.panic.c_str);
            cli_highlight_line(src, full, line, col);
            sf_str_free(call_ex.err.panic);
            sf_str_free(full);
        } else
            cli_highlight_line(src, sol_err_string(call_ex.err.tt), line, col);
        return -1;
    }

    sf_str ret = sol_tostring(call_ex.ok);
    printf(sol_isdtype(call_ex.ok, SOL_DSTR) ? TUI_BLD "Returned: (%s) '%s'\n" : TUI_BLD "Returned: (%s) %s\n",
        sol_typename(call_ex.ok).c_str, ret.c_str);

    sf_str_free(ret);
    sol_fproto_free(fun);
    sol_state_free(s);
    return 0;
}

int main(int argc, char **argv) {
    if (argc == 1) {
        printf("Usage: %s [run|dbg] <file>\n", argv[0]);
        return 1;
    }

    cli_mode mode;
    if (!strcmp(argv[1], "run")) {
        if (argc == 2) {
            printf("Usage: %s run <file>\n", argv[0]);
            return 1;
        }
        mode = CLI_RUN;
    } else if (!strcmp(argv[1], "dbg")) {
        if (argc == 2) {
            printf("Usage: %s dbg <file>\n", argv[0]);
            return 1;
        }
        mode = CLI_DBG;
    } else {
        printf("Unknown option '%s'.\nUsage: %s [run|dbg] <file>\n", argv[1], argv[0]);
        return 1;
    }

    sf_str src = cli_load_file(argv[2]);
    if (sf_isempty(src))
        return 1;

    int ret = 0;
    switch (mode) {
        case CLI_RUN: ret = cli_run(argv[2], src); break;
        case CLI_DBG: ret = sol_cli_cbg(argv[2], src); break;
    }
    sf_str_free(src);
    return ret;
}
