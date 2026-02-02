#ifndef CLI_H
#define CLI_H

#include <sf/str.h>

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

int sol_cli_cbg(char *path, sf_str src);
void cli_highlight_line(sf_str src, sf_str err, uint16_t line, uint16_t column);

#endif // CLI_H
