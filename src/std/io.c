#include "sf/fs.h"
#include "solus/vm.h"
#include "std.h"

/*
 * io.print(val: any)
 * Prints the str representation of any value to the console
 */
static solu_call_ex io_print(solu_state *s) {
    solu_val to_print = solu_get(s, 0);
    char *val = solu_tostr(s, to_print);
    printf("%s", val);
    free(val);
    return solu_ok(SOLU_NIL);
}

/*
 * io.println(val: any)
 * Prints the str representation of any value to the console, followed by
 * a newline character and flush
 */
static solu_call_ex io_println(solu_state *s) {
    solu_val to_print = solu_get(s, 0);
    char *val = solu_tostr(s, to_print);
    printf("%s\n", val);
    free(val);
    return solu_ok(SOLU_NIL);
}

/*
 * io.time() -> f64
 * Returns the time in seconds since the UNIX epoch
 */
static solu_call_ex io_time(solu_state *s) {
    (void)s;
    return solu_ok((solu_val){.tt = SOLU_TF64, .f64 = solu_timesec()});
}

/*
 * io.fread(path: str) -> str|err
 * Attempts to read a file from the specified path
 */
static solu_call_ex io_fread(solu_state *s) {
    solu_val path = solu_get(s, 0);
    expect_dtype(SOLU_DSTR, path);

    sf_str p = sf_ref(path.dyn);
    if (!sf_file_exists(p)) {
        sf_str e = sf_str_fmt("File '%s' not found", p.c_str);
        solu_call_ex ex = solu_ok(solu_dnerr(s, e.c_str));
        sf_str_free(e);
        return ex;
    }
    sf_fsb_ex fsb = sf_file_buffer(p);
    if (!fsb.is_ok) {
        sf_str errs = SF_STR_EMPTY;
        switch (fsb.err) {
            case SF_FILE_NOT_FOUND: errs = sf_str_fmt("File '%s' not found", p.c_str); break;
            case SF_OPEN_FAILURE: errs = sf_str_fmt("File '%s' failed to open", p.c_str); break;
            case SF_READ_FAILURE: errs = sf_str_fmt("File '%s' failed to read", p.c_str); break;
        }
        solu_call_ex ex = solu_ok(solu_dnerr(s, errs.c_str));
        sf_str_free(errs);
        return ex;
    }
    fsb.ok.flags = SF_BUFFER_GROW;
    sf_buffer_seek(&fsb.ok, SF_BUFFER_END, 0);
    sf_buffer_autoins(&fsb.ok, ""); // [\0]
    sf_buffer_seek(&fsb.ok, SF_BUFFER_START, 0);

    return solu_ok(solu_dnstr(s, (char *)fsb.ok.ptr));
}

/*
 * io.fwrite(path: str, content: str) -> nil|err
 * Writes the provided string to a file at the specified path
 */
static solu_call_ex io_fwrite(solu_state *s) {
    solu_val path = solu_get(s, 0);
    expect_dtype(SOLU_DSTR, path);
    solu_val content = solu_get(s, 1);
    expect_dtype(SOLU_DSTR, content);

    sf_str p = sf_ref(path.dyn);
    sf_str cont = sf_ref(content.dyn);

    FILE *f = fopen(p.c_str, "w");
    if (!f) {
        sf_str e = sf_str_fmt("File '%s' failed to open", p.c_str);
        solu_call_ex ex = solu_ok(solu_dnerr(s, e.c_str));
        sf_str_free(e);
        return ex;
    }
    fwrite(cont.c_str, 1, cont.len, f);
    fclose(f);

    return solu_ok(SOLU_NIL);
}

/*
 * io.compile(src: str, path: str) -> nil|err
 * Attempts to compile provided source into a function and then save
 * it to a file at the provided path
 */
static solu_call_ex io_compile(solu_state *s) {
    solu_val src = solu_get(s, 0);
    expect_dtype(SOLU_DSTR, src);
    solu_val path = solu_get(s, 1);
    expect_dtype(SOLU_DSTR, path);

    solu_compile_ex comp_ex = solu_csrc(s, src.dyn);
    if (!comp_ex.is_ok)
        return solu_call_ex_err((solu_call_err){
            .tt = comp_ex.err.tt,
        });
    solu_savefun(&comp_ex.ok, path.dyn);
    if (!sf_file_exists(sf_lit(path.dyn)))
        return solu_err(s, "Failed to write compiled file.");

    return solu_ok(SOLU_NIL);
}

#if defined(_WIN32) && !defined(HAVE_GETLINE)
#if defined(_MSC_VER)
  #include <BaseTsd.h>
  typedef SSIZE_T ssize_t;
#else
  typedef long ssize_t;
#endif
/// Windows getline impl
static ssize_t getline(char **lineptr, size_t *n, FILE *stream) {
    if (lineptr == NULL || n == NULL || stream == NULL) {
        errno = EINVAL;
        return -1;
    }
    if (*lineptr == NULL || *n == 0) {
        *n = 128;
        *lineptr = (char*)malloc(*n);
        if (*lineptr == NULL) {
            errno = ENOMEM;
            return -1;
        }
    }
    size_t len = 0;
    for (;;) {
        int ch = fgetc(stream);
        if (ch == '\n') break;
        if (ch == EOF) {
            if (ferror(stream)) {
                return -1;
            }
            if (len == 0) return -1;
            break;
        }
        if (len + 1 >= *n) {
            size_t newcap = (*n < 1024) ? (*n * 2) : (*n + 1024);
            char *tmp = (char*)realloc(*lineptr, newcap);
            if (tmp == NULL) {
                errno = ENOMEM;
                return -1;
            }
            *lineptr = tmp;
            *n = newcap;
        }
        (*lineptr)[len++] = (char)ch;
    }
    (*lineptr)[len] = '\0';
    return (ssize_t)len;
}
#endif

/*
 * io.input(prefix: str) -> str
 * Get input from the user in a command line, printing a prefix before hand
 */
static solu_call_ex io_input(solu_state *s) {
    solu_val prefix = solu_get(s, 0);
    expect_dtype(SOLU_DSTR, prefix);

    char *line = NULL;
    size_t cap = 0;
    printf("%s", (char *)prefix.dyn);
    ssize_t n = getline(&line, &cap, stdin);
    if (n == -1)
        return solu_ok(solu_dnerr(s, "User canceled input"));

    line[n - 1] = 0;
    solu_val str = solu_dnstr(s, line);
    free(line);

    return solu_ok(str);
}

solu_val solu_mod_io(solu_state *s) {
    solu_val io = solu_dnew(s, SOLU_DOBJ);

    solu_dobj_strset(io.dyn, "print", solu_wrapcfun(s, io_print, 1, NULL, 0));
    solu_dobj_strset(io.dyn, "println", solu_wrapcfun(s, io_println, 1, NULL, 0));
    solu_dobj_strset(io.dyn, "time", solu_wrapcfun(s, io_time, 0, NULL, 0));
    solu_dobj_strset(io.dyn, "fread", solu_wrapcfun(s, io_fread, 1, NULL, 0));
    solu_dobj_strset(io.dyn, "fwrite", solu_wrapcfun(s, io_fwrite, 2, NULL, 0));
    solu_dobj_strset(io.dyn, "compile", solu_wrapcfun(s, io_compile, 2, NULL, 0));
    solu_dobj_strset(io.dyn, "input", solu_wrapcfun(s, io_input, 1, NULL, 0));

    solu_dobj_strset(s->global.dyn, "io", io);
    return io;
}
