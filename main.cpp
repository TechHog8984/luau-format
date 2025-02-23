#include <cstdio>
#include <cstring>
#include <fstream>

#include "cli.hpp"
#include "formatter.hpp"

#include "Luau/ToString.h"

int tryFormatContents(Allocator& allocator, AstFormatter::FormatOptions format_options, std::string contents, FILE* output_file) {
    AstNameTable names(allocator);

    ParseResult parse_result = Parser::parse(contents.c_str(), contents.size(), names, allocator);
    auto parse_errors = parse_result.errors;
    if (parse_errors.empty()) {
        auto result = AstFormatter::formatRoot(
            parse_result.root,
            allocator,
            format_options
        );

        fprintf(output_file, "%s", result.formatted.c_str());
        if (output_file == stdout)
            puts("");

        auto errors = result.errors;
        if (!errors.empty()) {
            auto error_count = errors.size();
            fprintf(stderr, "ERROR: there were %zu errors when formatting:\n", error_count);
            for (size_t i = 0; i < error_count; i++)
                fprintf(stderr, "  %zu - %s\n", i, errors[i].c_str());
        }
        auto warnings = result.warnings;
        if (!warnings.empty()) {
            auto warning_count = warnings.size();
            fprintf(stderr, "WARNING: there were %zu warnings when formatting:\n", warning_count);
            for (size_t i = 0; i < warning_count; i++)
                fprintf(stderr, "  %zu - %s\n", i, warnings[i].c_str());
        }
        return !result.success;
    }

    fprintf(stderr, "ERROR: failed to parse code\n");
    for (const Luau::ParseError& error : parse_errors)
        fprintf(stderr, "   %s - %s\n", Luau::toString(error.getLocation()).c_str(), error.getMessage().c_str());
    fprintf(stderr, "\n");

    return 1;
}

int main(int argc, char** argv) {
    if (argc < 2) {
        if (argc == 1) displayHelp(argv[0]);
        else displayHelp();
        return 1;
    }

    AstFormatter::FormatOptions::OutputType output_type = AstFormatter::FormatOptions::Beautified;

    bool no_simplify = false;
    bool lua_calls = false;

    bool solve_record_table = false;
    bool solve_list_table = false;
    bool lph_control_flow = false;

    const char* input_path = argv[1];
    const char* output_path = nullptr;

    for (unsigned i = 2; i < (unsigned) argc; i++) {
        const char* arg = argv[i];
        if (strncmp(arg, "--output", 8) == 0) {
            if (strlen(arg) == 8 || arg[8] != '=') {
                fprintf(stderr, "ERROR: --output expects an equals sign\n");
                return 1;
            } else if (strlen(arg) < 10) {
                fprintf(stderr, "ERROR: --output expects a file path after the equals sign\n");
                return 1;
            }
            arg += 9;
            output_path = arg;
        } else if (strcmp(arg, "--nosolve") == 0 || strcmp(arg, "--nosimplify") == 0) {
            no_simplify = true;
        } else if (strcmp(arg, "--minify") == 0) {
            output_type = AstFormatter::FormatOptions::Minified;
        } else if (strcmp(arg, "--lua_calls") == 0) {
            lua_calls = true;

        } else if (strcmp(arg, "--luraph") == 0) {
            solve_record_table = true;
            solve_list_table = true;
            lph_control_flow = true;
            lua_calls = true;
        } else if (strcmp(arg, "--solve_record_table") == 0) {
            solve_record_table = true;
        } else if (strcmp(arg, "--solve_list_table") == 0) {
            solve_list_table = true;
        } else if (strcmp(arg, "--lph_control_flow") == 0) {
            lph_control_flow = true;
        } else {
            fprintf(stderr, "ERROR: unrecognized option '%s'\n", arg);
            return 1;
        }
    }

    int ret = 1;

    FILE* output_file = stdout;
    if (output_path) {
        output_file = fopen(output_path, "w");
        if (!output_file) {
            fprintf(stderr, "ERROR: failed to open output file '%s'\n", output_path);
            goto RET;
        }
    }

    {

    Luau::Allocator allocator;

    std::fstream input_file(input_path);

    if (input_file) {
        std::string input_contents;
        std::string buffer;
        while (std::getline(input_file, buffer)) {
            input_contents.append(buffer);
            input_contents += '\n';
        }

        AstFormatter::FormatOptions format_options(
            output_type,
            !no_simplify, lua_calls,
            solve_record_table, solve_list_table, lph_control_flow
        );
        ret = tryFormatContents(
            allocator,
            format_options,
            input_contents,
            output_file
        );
    } else {
        fprintf(stderr, "ERROR: failed to open input file '%s'\n", input_path);
        ret = 1;
    }

    input_file.close();

    }

RET:
    if (output_path)
        fclose(output_file);

    return ret;
}