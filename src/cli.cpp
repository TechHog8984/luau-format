#include <cstdio>

#include "cli.hpp"

void displayHelp(const char* filename) {
    printf("luau-format by techhog\n"
        "usage: %s inputfile [options]\n\n"
        "options:\n"
        "  --output=file - output file; default is stdout\n"
        "  --nosolve - disable AstSimplifier\n"
        "  --nosimplify - alias for --nosolve\n"
        "  --minify - minify instead of beautify\n"
        "  --lua_calls - solve lua calls such as math.max(1, 4)\n"
        "\n"
        "  --sep_stat=[sep]\n"
        "  --sep_block=[sep]\n"
        "\n"
        "  --luraph - all Luraph options plus --lua_calls\n"
        "  --solve_record_table - solves Luraph's function table\n"
        "  --solve_list_table - solves Luraph's number table\n"
        "  --lph_control_flow - solves Luraph's control flow\n"
    , filename);
}