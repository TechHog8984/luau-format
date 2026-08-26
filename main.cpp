#include <cstdio>
#include <cstring>
#include <fstream>

#include "cli.hpp"
#include "formatter.hpp"

#include "Luau/ToString.h"

using namespace LuauFormat;

int tryFormatContents(Allocator& allocator, AstFormatter::FormatOptions format_options, std::string contents, std::string& output) {
    AstNameTable names(allocator);

    ParseResult parse_result = Parser::parse(contents.c_str(), contents.size(), names, allocator);
    auto& parse_errors = parse_result.errors;
    if (parse_errors.empty()) {
        auto result = AstFormatter::formatRoot(
            parse_result.root,
            names,
            allocator,
            format_options
        );

        output = std::move(result.formatted);

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
        exit(1);
    } else if (!can_be_empty && strlen(arg) < option_length + 2) {
        fprintf(stderr, "ERROR: %s expects a value after the equals sign\n", option);
        exit(1);
    }

    arg += option_length + 1;
    return 0;
}
std::string parseSeparator(const char* sep) {
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
    bool no_render_unicode = false;

    bool has_sep_stat = false;
    bool has_sep_block = false;
    std::string sep_stat;
    std::string sep_block;

    bool solve_record_table = false;
    bool solve_list_table = false;

    int input_path_argc = 0;
    const char* output_path = nullptr;

    std::string input_contents;

    for (unsigned i = 1; i < (unsigned) argc; i++) {
        const char* arg = argv[i];
        if (arg[0] == '-') {
            if (!handleRecordOption("--code", arg))
                input_contents.assign(arg);
            else if (!handleRecordOption("--output", arg))
                output_path = arg;
            else if (strcmp(arg, "--nosolve") == 0 || strcmp(arg, "--nosimplify") == 0)
                no_simplify = true;
            else if (strcmp(arg, "--minify") == 0)
                output_type = AstFormatter::FormatOptions::Minified;
            else if (strcmp(arg, "--lua_calls") == 0)
                lua_calls = true;
            else if (strcmp(arg, "--optimize") == 0)
                optimizations = true;
            else if (strcmp(arg, "--assume_globals") == 0)
                assume_globals = true;
            else if (strcmp(arg, "--no_render_unicode") == 0)
                no_render_unicode = true;

            else if (!handleRecordOption("--sep_stat", arg, true)) {
                sep_stat = arg;
                has_sep_stat = true;
            } else if (!handleRecordOption("--sep_block", arg, true)) {
                sep_block = arg;
                has_sep_block = true;

            } else if (strcmp(arg, "--luraph") == 0) {
                solve_record_table = true;
                solve_list_table = true;
                optimizations = true;
                lua_calls = true;
            } else if (strcmp(arg, "--solve_record_table") == 0)
                solve_record_table = true;
            else if (strcmp(arg, "--solve_list_table") == 0)
                solve_list_table = true;
            else
                goto INVALID_ARG;
        } else if (input_path_argc)
            goto INVALID_ARG;
        else
            input_path_argc = i;

        continue;

        INVALID_ARG:
        fprintf(stderr, "ERROR: unrecognized option '%s'; run with no arguments for help (or use --help)\n", arg);
        exit(1);
    }

    if (has_sep_stat)
        sep_stat = parseSeparator(sep_stat.c_str());
    if (has_sep_block)
        sep_block = parseSeparator(sep_block.c_str());

    std::string output;

    if (input_contents.empty()) {
        if (input_path_argc) {
            const char* input_path = argv[input_path_argc];
            std::fstream input_file(input_path);

            if (input_file) {
                std::string buffer;
                while (std::getline(input_file, buffer)) {
                    input_contents.append(buffer);
                    input_contents += '\n';
                }
                input_file.close();
            } else {
                fprintf(stderr, "ERROR: failed to open input file '%s'\n", input_path);
                exit(1);
            }
        } else {
            fprintf(stderr, "ERROR: you must pass either an input file or --code=...\n");
            exit(1);
        }
    }

    AstFormatter::FormatOptions format_options(
        output_type,
        !no_simplify, optimizations, lua_calls, assume_globals,
        !no_render_unicode,
        solve_record_table, solve_list_table,
        has_sep_stat ? sep_stat.c_str() : nullptr, has_sep_block ? sep_block.c_str() : nullptr
    );
    Luau::Allocator allocator{};
    int ret = tryFormatContents(
        allocator,
        format_options,
        input_contents,
        output
    );

    if (!ret) {
        if (output_path) {
            std::ofstream output_file(output_path);
            if (!output_file) {
                fprintf(stderr, "ERROR: failed to open output file '%s'\n", output_path);
                exit(1);
            }
            output_file << output;
        } else
            printf("%.*s\n", (int) output.size(), output.c_str());
    }

    return ret;
}
