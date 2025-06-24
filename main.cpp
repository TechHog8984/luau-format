#include <cstdio>
#include <cstring>
#include <fstream>

#include "cli.hpp"
#include "formatter.hpp"

#include "Luau/ToString.h"

using namespace LuauFormat;

int tryFormatContents(Allocator& allocator, AstFormatter::FormatOptions format_options, std::string contents, FILE* output_file) {
    AstNameTable names(allocator);

    ParseResult parse_result = Parser::parse(contents.c_str(), contents.size(), names, allocator);
    auto& parse_errors = parse_result.errors;
    if (parse_errors.empty()) {
        auto result = AstFormatter::formatRoot(
            parse_result.root,
            allocator,
            format_options
        );

        fprintf(output_file, "%s", result.formatted.c_str());
        if (output_file == stdout)
            puts("");

        auto& errors = result.errors;
        if (!errors.empty()) {
            auto error_count = errors.size();
            fprintf(stderr, "ERROR: there were %zu errors when formatting:\n", error_count);
            for (size_t i = 0; i < error_count; i++)
                fprintf(stderr, "  %zu - %s\n", i, errors[i].c_str());
        }
        auto& warnings = result.warnings;
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

int handleRecordOption(const char* option, const char*& arg, bool can_be_empty = false) {
    size_t option_length = strlen(option);

    if (strncmp(arg, option, option_length) != 0)
        return 1;

    if (strlen(arg) == option_length || arg[option_length] != '=') {
        fprintf(stderr, "ERROR: %s expects an equals sign\n", option);
        return 1;
    } else if (!can_be_empty && strlen(arg) < option_length + 2) {
        fprintf(stderr, "ERROR: %s expects a value after the equals sign\n", option);
        return 1;
    }

    arg += option_length + 1;
    return 0;
}
std::string parseSeparator(const char*& sep) {
    std::string result;

    for (size_t i = 0; i < strlen(sep); i++) {
        char ch = sep[i];
        if (ch == '\\') {
            i++;
            if (i >= strlen(sep))
                break;
            ch = sep[i];

            switch (ch) {
                case 'a':
                    ch = '\a';
                    break;
                case 'b':
                    ch = '\b';
                    break;
                case 'f':
                    ch = '\f';
                    break;
                case 'n':
                    ch = '\n';
                    break;
                case 'r':
                    ch = '\r';
                    break;
                case 't':
                    ch = '\t';
                    break;
                case 'v':
                    ch = '\v';
                    break;
                case '\\':
                    ch = '\\';
                    break;
                default:
                    result.push_back('\\');
                    break;
            }
        }
        result.push_back(ch);
    }

    return result;
}

int main(int argc, char** argv) {
    if (argc < 2) {
        if (argc == 1) displayHelp(argv[0]);
        else displayHelp();
        return 1;
    }

    AstFormatter::FormatOptions::OutputType output_type = AstFormatter::FormatOptions::Beautified;

    bool no_simplify = false;
    bool optimizations = false;
    bool lua_calls = false;
    bool assume_globals = false;

    const char* sep_stat = nullptr;
    const char* sep_block = nullptr;

    bool solve_record_table = false;
    bool solve_list_table = false;
    bool lph_control_flow = false;

    const char* input_path = argv[1];
    const char* output_path = nullptr;

    for (unsigned i = 2; i < (unsigned) argc; i++) {
        const char* arg = argv[i];
        if (!handleRecordOption("--output", arg)) {
            output_path = arg;
        } else if (strcmp(arg, "--nosolve") == 0 || strcmp(arg, "--nosimplify") == 0) {
            no_simplify = true;
        } else if (strcmp(arg, "--minify") == 0) {
            output_type = AstFormatter::FormatOptions::Minified;
        } else if (strcmp(arg, "--lua_calls") == 0) {
            lua_calls = true;
        } else if (strcmp(arg, "--optimize") == 0) {
            optimizations = true;
        } else if (strcmp(arg, "--assume_globals") == 0) {
            assume_globals = true;

        } else if (!handleRecordOption("--sep_stat", arg, true)) {
            sep_stat = arg;
        } else if (!handleRecordOption("--sep_block", arg, true)) {
            sep_block = arg;

        } else if (strcmp(arg, "--luraph") == 0) {
            solve_record_table = true;
            solve_list_table = true;
            lph_control_flow = true;
            optimizations = true;
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

    if (sep_stat) {
        std::string temp = parseSeparator(sep_stat);
        sep_stat = static_cast<const char*>(malloc(temp.length() + 1));
        strcpy(const_cast<char*>(sep_stat), temp.c_str());
    }
    if (sep_block) {
        std::string temp = parseSeparator(sep_block);
        sep_block = static_cast<const char*>(malloc(temp.length() + 1));
        strcpy(const_cast<char*>(sep_block), temp.c_str());
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
            !no_simplify, optimizations, lua_calls, assume_globals,
            solve_record_table, solve_list_table, lph_control_flow,
            sep_stat, sep_block
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

    if (sep_stat)
        free((void*) sep_stat);
    if (sep_block)
        free((void*) sep_block);

    return ret;
}
