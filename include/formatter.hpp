#pragma once

#include <cstddef>
#include <unordered_map>

#include "Luau/Lexer.h"
#include "simplifier.hpp"

#include "Luau/Ast.h"
#include "Luau/Parser.h"

using namespace Luau;

namespace LuauFormat {

template<typename T>
AstArray<T> copy(Allocator& allocator, const T* data, size_t size)
{
    AstArray<T> result;

    result.data = size ? static_cast<T*>(allocator.allocate(sizeof(T) * size)) : nullptr;
    result.size = size;

    // This is equivalent to std::uninitialized_copy, but without the exception guarantee
    // since our types don't have destructors
    for (size_t i = 0; i < size; ++i)
        new (result.data + i) T(data[i]);

    return result;
}

template<typename T>
AstArray<T> copy(Allocator& allocator, const TempVector<T>& data)
{
    return copy(allocator, data.empty() ? nullptr : &data[0], data.size());
}

template<typename T>
AstArray<T> copy(Allocator& allocator, std::initializer_list<T> data)
{
    return copy(allocator, data.size() == 0 ? nullptr : data.begin(), data.size());
}

AstStatBlock* allocateBlockFromSingleStat(Allocator& allocator, AstStat* single_stat);
AstStatBlock* combineBlocks(Allocator& allocator, std::vector<AstStatBlock*> block_list);

class AstSimplifier;
class SimplifyResult;
class RecordTableReplaceVisitor;
class ListTableReplaceVisitor;

class AstFormatter {
    AstNameTable& name_table;
    Allocator& allocator;
    AstSimplifier& simplifier;
    std::vector<std::string> errors;
    std::vector<std::string> warnings;
    std::string current;

    void appendChar(std::string& result, char ch);
    void appendStr(std::string& result, std::string str);
    void erase(std::string& result, size_t pos, size_t n);
    void eraseOne(std::string& result, size_t pos);
    void insertEnd(std::string& result, const char* begin, const char* end);

    struct separators_t {
        const char* stat = "UNSET SEPARATOR stat"; bool stat_has_semicolon = false;
        const char* block = "UNSET SEPARATOR block";
        const char* optional_space = "UNSET SEPARATOR optional_space";
        const char* optional_newline = "UNSET SEPARATOR optional_newline";
        const char* post_keyword_expr_open = "UNSET SEPARATOR post_keyword_expr_open";
        const char* post_keyword_expr_close = "UNSET SEPARATOR post_keyword_expr_close";
        const char* optional_post_keyword_expr_close = "UNSET SEPARATOR optional_post_keyword_expr_close";
        const char* expr_list = "UNSET SEPARATOR expr_list";
        const char* table_item = "UNSET SEPARATOR table_item";
        const char* equals = "UNSET SEPARATOR equals";
    } separators;

    void setSeparatorStat(const char* sep);

    bool indents_active = true;
    size_t indent_count = 0;
    bool skip_indent = false;

    void appendIndents(std::string& result);
    void incrementIndent();
    void decrementIndent();

    bool newline_allowed = true;
    void appendComment(std::string& result, const char* comment, bool force = false);

    void ensureExactlyOne(std::string& result, char ch);

    AstStat* current_stat = nullptr;
public:
    RecordTableReplaceVisitor* record_table_replace_visitor = nullptr;
    ListTableReplaceVisitor* list_table_replace_visitor = nullptr;
    void reportError(std::string message) {
        errors.push_back(message);
    }
    void reportWarning(std::string message) {
        warnings.push_back(message);
    }

    class FormatOptions {
    public:
        enum OutputType {
            Beautified,
            Minified,
            Uglified
        } output_type = Beautified;
        bool simplify_expressions;
        bool optimizations;
        bool lua_calls;
        bool assume_globals;

        bool record_table_replace;
        bool list_table_replace;

        const char* separator_stat;
        const char* separator_block;

        FormatOptions(
            OutputType output_type = Beautified, bool simplify_expressions = true,
            bool optimizations = false, bool lua_calls = false, bool assume_globals = false,
            bool record_table_replace = false, bool list_table_replace = false,
            const char* separator_stat = nullptr, const char* separator_block = nullptr
        );
    };

    class FormatResult {
    public:
        bool success = false;
        std::string formatted = "-- failed to format: uninitialized FormatResult";
        std::vector<std::string> errors;
        std::vector<std::string> warnings;
    };

    // NOTE: don't forget to reflect changes to copyNodeTag
    struct NodeTag {
        bool dont_format = false;
        bool no_do_end = false;
        bool inside_table_list = false;
        bool inside_tuple = false;
        bool skip_indent = false;
        bool skip_last_stat_separator = false;
        AstStat* stat_replacement = nullptr;
    };

    static void copyNodeTag(AstNode* recipient, AstNode* reference);

private:
    FormatOptions options;
    static std::unordered_map<AstNode*, NodeTag> node_tag_map;

    std::optional<std::string> formatNode(AstNode* node);
    std::optional<std::string> formatNode(AstLocal* local);

    size_t appendOptionalSemicolon(std::string& current, std::string& result, NodeTag& main_tag);

    bool canSimplifyRepeatBody(AstStatRepeat* main_stat, SimplifyResult& condition_simplified);

    std::optional<std::string> formatExpr(AstExpr* expr);
    std::optional<std::string> formatStat(AstStat* stat);
    std::optional<std::string> formatType(AstType* type);
public:
    static NodeTag& getNodeTag(AstNode* node);

    AstFormatter(AstNameTable& name_table, Allocator& allocator, AstSimplifier& simplifier, FormatOptions options);
    ~AstFormatter();

    static FormatResult formatRoot(AstStatBlock* root, AstNameTable& name_table, Allocator& allocator, AstSimplifier& simplifier, FormatOptions options);
    static FormatResult formatRoot(AstStatBlock* root, AstNameTable& name_table, Allocator& allocator, FormatOptions options);

    FormatResult formatRoot(AstStatBlock* root, bool dont_make_visitors = false);
};

}; // namespace LuauFormat
