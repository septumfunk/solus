#include "sf/math.h"
#include "solus/val.h"
#include "solus/vm.h"
#include "cli/cli.h"
#include <limits.h>
#include <ctype.h>

#include <sf/str.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
    #include <curses.h>
#else
    #include <ncurses.h>
#endif

#define CMD_MAX 256
#define DBGOUT_MAX 8192

typedef enum {
    SOLU_DBG_ASM,
    SOLU_DBG_STACK,
    SOLU_DBG_OUT,
} solu_dbgpane;

typedef struct {
    sf_str src;
    solu_state *s;
    solu_fproto proto;
    WINDOW *src_w, *asm_w, *cmd_w;
    char cmd[CMD_MAX];
    sf_str err;
    bool *bp, e, cap_cur, stack_frame;
    int line_o, cmd_len, cur, src_h, _break;

    solu_dbgpane pane;
    sf_str dbgout; char *path;
} solu_debugger;
static solu_debugger dbg;

static void solu_writeout(sf_str s) {
    sf_str_append(&dbg.dbgout, s);
    if (dbg.dbgout.len > DBGOUT_MAX) {
        char *nstr = malloc(DBGOUT_MAX);
        memcpy(nstr, dbg.dbgout.c_str + (dbg.dbgout.len - DBGOUT_MAX), DBGOUT_MAX);
        sf_str_free(dbg.dbgout);
        dbg.dbgout.c_str = nstr;
        dbg.dbgout.len = DBGOUT_MAX;
    }
}

/// Hijack!
solu_call_ex solu_dbgprint(solu_state *s) {
    solu_val to_print = solu_get(s, 0);
    char *val = solu_tostr(s, to_print);
    solu_writeout(sf_ref(val));
    free(val);
    return solu_call_ex_ok(SOLU_NIL);
}
solu_call_ex solu_dbgprintln(solu_state *s) {
    solu_val to_print = solu_get(s, 0);
    sf_str val = sf_own(solu_tostr(s, to_print));
    sf_str_append(&val, sf_lit("\n"));
    solu_writeout(val);
    sf_str_free(val);
    return solu_call_ex_ok(SOLU_NIL);
}

static inline void solu_cmdc(void) {
    dbg.e = false;
    memset(dbg.cmd, 0, CMD_MAX);
    dbg.cmd_len = 0;
}

static inline void solu_cmderr(sf_str s) {
    memset(dbg.cmd, 0, CMD_MAX);
    memcpy(dbg.cmd, s.c_str, s.len);
    dbg.cmd_len = (int)s.len;
    dbg.e = true;
}

static void solu_drawstack(void);
static int solu_rdcmd(void) {
    if (sf_str_eq(sf_ref(dbg.cmd), sf_lit("q")))
        return -1;

    if (sf_str_eq(sf_ref(dbg.cmd), sf_lit("stack"))) {
        if (!dbg._break) {
            solu_cmderr(sf_lit("Must be running to show stack"));
            return 0;
        }
        dbg.pane = SOLU_DBG_STACK;
        solu_cmdc();
        return 0;
    }
    if (sf_str_eq(sf_ref(dbg.cmd), sf_lit("asm"))) {
        dbg.pane = SOLU_DBG_ASM;
        solu_cmdc();
        return 0;
    }
    if (sf_str_eq(sf_ref(dbg.cmd), sf_lit("out"))) {
        dbg.pane = SOLU_DBG_OUT;
        solu_cmdc();
        return 0;
    }

    if (sf_str_eq(sf_ref(dbg.cmd), sf_lit("b")) && dbg.bp) {
        dbg.bp[dbg.cur - 1] = !dbg.bp[dbg.cur - 1];
        solu_cmdc();
        return 0;
    }

    bool rs = sf_str_eq(sf_ref(dbg.cmd), sf_lit("rs"));
    bool n = sf_str_eq(sf_ref(dbg.cmd), sf_lit("n"));
    if ((rs || sf_str_eq(sf_ref(dbg.cmd), sf_lit("s"))) && dbg._break) { // Unset resume state
        dbg.proto.dbg_res = 0;
        dbg.proto.dbg_ll = 0;
        dbg._break = 0;
        if (!rs) {
            solu_cmdc();
            return 0;
        }
    }
    if (rs || n || sf_str_eq(sf_ref(dbg.cmd), sf_lit("r"))) {
        bool *o = NULL;
        if (n) {
            o = dbg.bp;
            dbg.bp = malloc(dbg.proto.line_c * sizeof(bool));
            memset(dbg.bp, 1, dbg.proto.line_c * sizeof(bool));
        }

        solu_call_ex e = solu_dcall(dbg.s, &dbg.proto, NULL, 0, dbg.bp);
        if (e.is_ok) {
            sf_str ret = sf_own(solu_tostr(dbg.s, e.ok));
            sf_str p = sf_str_fmt(solu_isdtype(e.ok, SOLU_DSTR) ?
                "return: %s | '%s'\n" : "return: %s | %s\n",
                solu_typename(e.ok).c_str, ret.c_str
            );
            dbg._break = 0;
            dbg.pane = SOLU_DBG_OUT;
            solu_writeout(p);
            sf_str_free(p);
        } else if (e.err.tt == SOLU_ERRV_BREAK) {
            dbg.cur = SOLU_DBG_LINE(dbg.proto.dbg[e.err.pc]);
            dbg.line_o = dbg.cur;
            dbg._break = dbg.cur;
            dbg.pane = SOLU_DBG_STACK;
            solu_cmderr(sf_lit("BREAK"));
        } else {
            uint16_t line = SOLU_DBG_LINE(dbg.proto.dbg[e.err.pc]), column = SOLU_DBG_COL(dbg.proto.dbg[e.err.pc]);
            sf_str p = sf_str_fmt(e.err.tt == SOLU_ERRV_PANIC ? "panic: %s:%u:%u %s\n" : "error: %s:%u:%u %s\n", dbg.path, line, column,
                e.err.panic ? e.err.panic : solu_err_string(e.err.tt)
            );
            solu_writeout(p);
            dbg.pane = SOLU_DBG_OUT;
            sf_str_free(p);
        }
        if (n) {
            free(dbg.bp);
            dbg.bp = o;
        }
        solu_cmdc();
        return 0;
    }
    if (sf_str_eq(sf_ref(dbg.cmd), sf_lit("cm"))) {
        dbg.cap_cur = !dbg.cap_cur;
        solu_cmderr(sf_ref(dbg.cap_cur ? "Cursor cap ON" : "Cursor cap OFF"));
        return 0;
    }
    if (sf_str_eq(sf_ref(dbg.cmd), sf_lit("sm"))) {
        dbg.stack_frame = !dbg.stack_frame;
        solu_cmderr(sf_ref(dbg.cap_cur ? "Stack frame ON" : "Stack frame OFF"));
        return 0;
    }

    if (dbg.cmd[0] == '$') {
        if (dbg.cmd_len == 1) {
            solu_cmderr(sf_lit("usage: $<code>"));
            return 0;
        }
        solu_writeout(sf_ref(dbg.cmd));
        solu_writeout(sf_lit("\n"));

        dbg.pane = SOLU_DBG_OUT;
        solu_compile_ex comp_ex = solu_csrc(dbg.s, dbg.cmd + 1);
        if (!comp_ex.is_ok) {
            sf_str e = sf_str_fmt("error: %s\n", solu_err_string(comp_ex.err.tt));
            solu_cmderr(e);
            sf_str_free(e);
            return 0;
        }
        solu_call_ex call_ex = solu_call(dbg.s, &comp_ex.ok, NULL, 0);
        if (!call_ex.is_ok) {
            sf_str e = sf_str_fmt(call_ex.err.tt == SOLU_ERRV_PANIC ? "panic: %s\n" : "error: %s\n",
                call_ex.err.tt == SOLU_ERRV_PANIC ? call_ex.err.panic : solu_err_string(call_ex.err.tt)
            );
            solu_cmderr(e);
            sf_str_free(e);
        } else {
            sf_str ret = sf_own(solu_tostr(dbg.s, call_ex.ok));
            sf_str p = sf_str_fmt(solu_isdtype(call_ex.ok, SOLU_DSTR) ?
                "return: %s | '%s'\n" : "return: %s | %s\n",
                solu_typename(call_ex.ok).c_str, ret.c_str
            );
            solu_cmderr(p);
            sf_str_free(p);
        }
        solu_fproto_free(&comp_ex.ok);
        solu_cmdc();
        return 0;
    }

    sf_str f = sf_str_fmt("Unknown Command: %s", dbg.cmd);
    solu_cmderr(f);
    sf_str_free(f);
    return 0;
}

#define fmt  (dbg.cur == dbg.proto.line_c ?  : "o%4u  %.*s\n") : \
                                            ( ? " %4u > %.*s\n" : " %4u | %.*s\n")

static void solu_drawsrc(void) {
    werase(dbg.src_w);
    wmove(dbg.src_w, 1, 0);
    uint16_t line = 1;
    char *cs, *c = dbg.src.c_str, *end = dbg.src.c_str + dbg.src.len;
    cs = c;
    while (c < end) {
        if (*c == '\n') {
            if (line > dbg.line_o - 1)
                mvwprintw(dbg.src_w, line - (dbg.line_o - 1), 2, "%c%4u %c %.*s\n",
                    dbg.bp && dbg.bp[line - 1] ? (dbg._break == line ? '#' : 'o') : ' ',
                    line,
                    dbg.cur == line ? '>' : '|',
                    (int)(c - cs), cs
                );
            cs = c + 1;
            line++;
        }
        c++;
    }
    if (cs < end && line > dbg.line_o - 1)
        mvwprintw(dbg.src_w, line - (dbg.line_o - 1), 2, "%c%4u %c %.*s\n",
            dbg.bp && dbg.bp[line - 1] ? (dbg._break == line ? '#' : 'o') : ' ',
            line,
            dbg.cur == line ? '>' : '|',
            (int)(c - cs), cs
        );

    if (!dbg.bp)
        dbg.bp = calloc((size_t)dbg.proto.line_c, sizeof(bool));
    dbg.proto.line_c = line;

    box(dbg.src_w, 0, 0);
    mvwprintw(dbg.src_w, 0, 2, "src");
    wrefresh(dbg.src_w);
}

static void solu_drawasm(void) {
    werase(dbg.asm_w);

    solu_dbg *db = dbg.proto.dbg,
            *end = dbg.proto.dbg + dbg.proto.code_c;
    if (!db) {
        mvwprintw(dbg.asm_w, 1, 1, "Assembly unavailable.");
        box(dbg.asm_w, 0, 0);
        mvwprintw(dbg.asm_w, 0, 2, "asm");
        wrefresh(dbg.asm_w);
        return;
    }

    solu_dbg *cur_db = db;  // start with first entry
    for (solu_dbg *it = db; it < end; ++it) {
        int line = (int)SOLU_DBG_LINE(*it);
        if (line <= dbg.cur) {
            if ((int)SOLU_DBG_LINE(*cur_db) < line)
                cur_db = it;
        } else
            break;
    }

    int y = 1;
    for (solu_dbg *it = cur_db; it < end && ((int)SOLU_DBG_LINE(*it) <= dbg.cur || !dbg.cap_cur); ++it, ++y) {
        solu_instruction ins = dbg.proto.code[it - dbg.proto.dbg];
        const char *op = solu_op_info(solu_ins_op(ins))->mnemonic;
        uint16_t line = SOLU_DBG_LINE(*it), column = SOLU_DBG_COL(*it);
        switch (solu_op_info(solu_ins_op(ins))->type) {
            case SOLU_INS_A: mvwprintw(dbg.asm_w, y, 1, "%4u:%-3u %-7s %-8d", line, column, op, solu_ia_a(ins)); break;
            case SOLU_INS_AB: mvwprintw(dbg.asm_w, y, 1, "%4u:%-3u %-7s %-4u %-4u", line, column, op, solu_iab_a(ins), solu_iab_b(ins)); break;
            case SOLU_INS_ABC: mvwprintw(dbg.asm_w, y, 1, "%4u:%-3u %-7s %-4u %-4u %-4u", line, column, op, solu_iabc_a(ins), solu_iabc_bx(ins), solu_iabc_cx(ins)); break;
        }
    }

    box(dbg.asm_w, 0, 0);
    mvwprintw(dbg.asm_w, 0, 2, "asm");
    wrefresh(dbg.asm_w);
}

static void solu_drawstack(void) {
    werase(dbg.asm_w);

    uint32_t s_reg = dbg.stack_frame ? (dbg.s->frames.data + dbg.s->frames.count - 1)->bottom_o : 0;
    int y = 1;
    for (uint32_t r = s_reg; r < dbg.s->stack.count; ++r) {
        solu_val v = solu_valvec_get(&dbg.s->stack, r);
        sf_str type = solu_typename(v);
        sf_str val = sf_own(solu_tostr(dbg.s, v));
        mvwprintw(dbg.asm_w, y, 1, "  [%03u]: %-4s | %s", r, type.c_str, val.c_str);
        sf_str_free(val);
        ++y;
    }

    box(dbg.asm_w, 0, 0);
    mvwprintw(dbg.asm_w, 0, 2, "stack");
    wrefresh(dbg.asm_w);
}

static void solu_drawout(void) {
    werase(dbg.asm_w);
    if (!sf_isempty(dbg.dbgout)) {
        char *nc = dbg.dbgout.c_str + dbg.dbgout.len - 1;
        char *ll = NULL;
        int y = dbg.src_h;
        while (nc && nc != dbg.dbgout.c_str) {
            if (y == 0) break;
            --nc;
            if (*nc == '\n' || nc == dbg.dbgout.c_str) {
                if (ll) *ll = '\0';
                mvwprintw(dbg.asm_w, y--, 2, "%s\n", nc == dbg.dbgout.c_str ? nc : nc + 1);
                if (ll) *ll = '\n';
                ll = nc;
            }
        }
    }

    box(dbg.asm_w, 0, 0);
    mvwprintw(dbg.asm_w, 0, 2, "out");
    wrefresh(dbg.asm_w);
}

static void solu_drawcmd(void) {
    nodelay(dbg.cmd_w, FALSE);
    keypad(dbg.cmd_w, true);

    int ch = 0;
    int prev_lines = 0, prev_cols = 0;
    getmaxyx(stdscr, prev_lines, prev_cols);
    while (1) {
        int new_lines, new_cols;
        getmaxyx(stdscr, new_lines, new_cols);
        if (new_lines != prev_lines || new_cols != prev_cols) {
            resize_term(new_lines, new_cols);
            clear();

            wresize(dbg.src_w, new_lines - 3, new_cols / 2);
            mvwin(dbg.src_w, 0, 0);
            wresize(dbg.asm_w, new_lines - 3, new_cols - new_cols / 2);
            mvwin(dbg.asm_w, 0, new_cols / 2);
            wresize(dbg.cmd_w, 3, new_cols);
            mvwin(dbg.cmd_w, new_lines - 3, 0);

            touchwin(dbg.src_w);
            touchwin(dbg.asm_w);
            touchwin(dbg.cmd_w);

            dbg.src_h = new_lines - 5;
            prev_lines = new_lines;
            prev_cols = new_cols;
        }

        werase(dbg.cmd_w);
        box(dbg.cmd_w, 0, 0);

        solu_drawsrc();
        switch (dbg.pane) {
            case SOLU_DBG_ASM:   solu_drawasm(); break;
            case SOLU_DBG_STACK: solu_drawstack(); break;
            case SOLU_DBG_OUT:   solu_drawout(); break;
        }

        mvwprintw(dbg.cmd_w, 0, 2, "cmd");
        mvwprintw(dbg.cmd_w, 1, 2, "> %.*s", dbg.cmd_len, dbg.cmd);
        wmove(dbg.cmd_w, 1, 4 + dbg.cmd_len);
        wrefresh(dbg.cmd_w);

        ch = wgetch(dbg.cmd_w);
        if (dbg.e)
            solu_cmdc();

        if (ch == '\n') {
            if (dbg.cmd_len == 0)
                continue;
            dbg.cmd[dbg.cmd_len] = '\0';
            if (solu_rdcmd() == -1)
                break;
        } else if (ch == KEY_BACKSPACE || ch == 127) {
            if (dbg.cmd_len > 0) dbg.cmd_len--;
        } else if (ch == KEY_UP) {
            if (dbg.cur > 1) dbg.cur -= 1;
            if (dbg.cur < dbg.line_o)
                --dbg.line_o;
        } else if (ch == KEY_DOWN) {
            if (dbg.cur < dbg.proto.line_c) dbg.cur += 1;
            if (dbg.cur - (dbg.line_o - 1) > dbg.src_h)
                ++dbg.line_o;
        } else if (isprint(ch) && dbg.cmd_len < CMD_MAX - 1) {
            dbg.cmd[dbg.cmd_len++] = (char)ch;
        }
    }
}

int solu_cli_cbg(char *path, sf_str src) {
#ifndef _WIN32
    (void)path;

    // Compile
    solu_state *s = solu_state_new();
    solu_usestd(s);

    solu_val io = solu_dobj_strget(s->global.dyn, "io");
    if (io.tt == SOLU_TNIL)
        return -1;
    solu_dobj_strset(io.dyn, "print", solu_wrapcfun(s, solu_dbgprint, 1, NULL, 0));
    solu_dobj_strset(io.dyn, "println", solu_wrapcfun(s, solu_dbgprintln, 1, NULL, 0));

    solu_compile_ex comp_ex = solu_cfile(s, path);
    if (!comp_ex.is_ok) {
        fprintf(stderr, TUI_ERR "error: %s:%u:%u\n" TUI_CLR, path, comp_ex.err.line, comp_ex.err.column);
        cli_highlight_line(src, sf_ref(solu_err_string(comp_ex.err.tt)), comp_ex.err.line, comp_ex.err.column);
        solu_state_free(s);
        return -1;
    }

    initscr();
    cbreak();
    noecho();
    keypad(stdscr, TRUE);

    int line, col, w;
    getmaxyx(stdscr, line, col);
    w = col / 2;
    dbg = (solu_debugger){
        .src = src, .proto = comp_ex.ok,
        .s = s,
        .err = SF_STR_EMPTY,
        .src_w = newwin(line - 3, w, 0, 0),
        .asm_w = newwin(line - 3, col - w, 0, w),
        .cmd_w = newwin(3, col, line - 3, 0),
        .cap_cur = true, .e = false, .stack_frame = true, ._break = false,
        .line_o = 1, .cmd_len = 0, .cur = 1,
        .src_h = line - 5,

        .dbgout = SF_STR_EMPTY,
        .path = path,
    };
    if (!dbg.src_w || !dbg.asm_w || !dbg.cmd_w) return -1;

    solu_drawcmd();

    delwin(dbg.src_w);
    delwin(dbg.asm_w);
    delwin(dbg.cmd_w);
    free(dbg.bp);
    solu_fproto_free(&dbg.proto);
    solu_state_free(s);
    endwin();
    if (!sf_isempty(dbg.err)) {
        fprintf(stderr, "%s\n", dbg.err.c_str);
        sf_str_free(dbg.err);
    }
    return 0;
#else
    (void)path; (void)src;
    fprintf(stderr, TUI_ERR "Unimplemented\n" TUI_CLR);
    return 0;
#endif
}
