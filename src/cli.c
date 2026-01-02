#include "ctr/bytecode.h"
#include "ctr/ctrc.h"
#include "ctr/vm.h"
#include <sf/str.h>
#include <sf/fs.h>
#include <stdio.h>
#include <string.h>

#define TUI_UL  "\x1b[4m"
#define TUI_BLD "\x1b[1m"
#define TUI_ERR "\x1b[1;31m"
#define TUI_CLR "\x1b[0m"

typedef enum {
    CLI_RUN,
    CLI_DBG,
    CLI_COMP,
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
            while (*(c-1) != '\n' && c != src.c_str)
                --c;
            break;
        } else ++c;
    }
    fprintf(stderr, /* "%d | " */ "%s\n", /* line, */ c);
    *cc = '\n';

    char pointer[column];
    memset(pointer, '~', sizeof(pointer));
    pointer[sizeof(pointer) - 1] = '\0';
    pointer[sizeof(pointer) - 2] = '^';

    if (err.c_str == ctr_err_string(CTR_ERRP_EXPECTED_SEMICOLON).c_str)
        fprintf(stderr, TUI_ERR "%s\n" TUI_CLR, err.c_str);
    else fprintf(stderr, TUI_ERR "%s %s\n" TUI_CLR, pointer, err.c_str);
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
    ctr_state *s = ctr_state_new();
    ctr_usestd(s);
    ctr_compile_ex comp_ex = ctr_compile(s, src);
    if (!comp_ex.is_ok) {
        fprintf(stderr, TUI_ERR "error: %s:%u:%u\n" TUI_CLR, path, comp_ex.err.line, comp_ex.err.column);
        cli_highlight_line(src, ctr_err_string(comp_ex.err.tt), comp_ex.err.line, comp_ex.err.column);
        ctr_state_free(s);
        return -1;
    }

    ctr_fproto *fun = &comp_ex.ok;
    ctr_call_ex call_ex = ctr_call(s, fun, NULL);
    if (!call_ex.is_ok) {
        uint16_t line = CTR_DBG_LINE(fun->dbg[call_ex.err.pc]), col = CTR_DBG_COL(fun->dbg[call_ex.err.pc]);
        fprintf(stderr, TUI_ERR "error: %s:%u:%u\n" TUI_CLR, path, line, col);

        if (!sf_isempty(call_ex.err.panic)) {
            sf_str full = sf_str_fmt("%s: %s", ctr_err_string(call_ex.err.tt).c_str, call_ex.err.panic.c_str);
            cli_highlight_line(src, full, line, col);
            sf_str_free(call_ex.err.panic);
            sf_str_free(full);
        } else
            cli_highlight_line(src, ctr_err_string(call_ex.err.tt), line, col);
        return -1;
    }

    sf_str ret = ctr_tostring(call_ex.ok);
    printf(ctr_isdtype(call_ex.ok, CTR_DSTR) ? TUI_BLD "Returned: (%s) '%s'\n" : TUI_BLD "Returned: (%s) %s\n",
        ctr_typename(call_ex.ok).c_str, ret.c_str);

    sf_str_free(ret);
    ctr_ddel(call_ex.ok);
    ctr_fproto_free(fun);
    ctr_state_free(s);
    return 0;
}

int cli_dbg(char *path, sf_str src) {
    (void)path; (void)src;
    fprintf(stderr, TUI_ERR "Unimplemented\n" TUI_CLR);
    return 0;
}

int cli_comp(char *path, sf_str src) {
    ctr_state *s = ctr_state_new();
    ctr_usestd(s);

    printf(TUI_UL TUI_BLD "[ LN:COL = OP === A = B = C = ]\n" TUI_CLR);
    ctr_compile_ex comp_ex = ctr_cproto(src, 0, NULL, 1, (ctr_upvalue[]){
        (ctr_upvalue){sf_lit("_g"), CTR_UP_VAL, .value = ctr_dref(s->global)}
    }, true);

    if (!comp_ex.is_ok) {
        fprintf(stderr, TUI_ERR "error: %s:%u:%u\n" TUI_CLR, path, comp_ex.err.line, comp_ex.err.column);
        cli_highlight_line(src, ctr_err_string(comp_ex.err.tt), comp_ex.err.line, comp_ex.err.column);
        ctr_state_free(s);
        return -1;
    }
    ctr_state_free(s);
    return 0;
}


int main(int argc, char **argv) {
    if (argc == 1) {
        printf("Usage: %s [run|dbg|comp] <file>\n", argv[0]);
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
    } else if (!strcmp(argv[1], "comp")) {
        if (argc == 2) {
            printf("Usage: %s comp <file>\n", argv[0]);
            return 1;
        }
        mode = CLI_COMP;
    } else {
        printf("Unknown option '%s'.\nUsage: %s [run|debug|comp] <file>\n", argv[1], argv[0]);
        return 1;
    }

    sf_str src = cli_load_file(argv[2]);
    if (sf_isempty(src))
        return 1;

    int ret;
    switch (mode) {
        case CLI_RUN: ret = cli_run(argv[2], src); break;
        case CLI_DBG: ret = cli_dbg(argv[2], src); break;
        case CLI_COMP: ret = cli_comp(argv[2], src); break;
    }
    sf_str_free(src);
    return ret;
}
