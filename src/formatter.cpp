#include "formatter.hpp"
#include "simplifier.hpp"

#include "Luau/Ast.h"

std::unordered_map<AstNode*, AstFormatter::NodeTag> AstFormatter::node_tag_map = {};

AstStatBlock* allocateBlockFromSingleStat(Allocator& allocator, AstStat* single_stat) {
    auto body = std::vector<AstStat*>();
    body.push_back(single_stat);

    return allocator.alloc<AstStatBlock>(Location(), copy(allocator, body.data(), body.size()));
}
AstStatBlock* combineBlocks(Allocator& allocator, std::vector<AstStatBlock*> block_list) {
    auto body = std::vector<AstStat*>();

    for (size_t i = 0; i < block_list.size(); i++) {
        auto& block_body = block_list[i]->body;
        for (size_t j = 0; j < block_body.size; j++)
            body.push_back(block_body.data[j]);
    }

    return allocator.alloc<AstStatBlock>(Location(), copy(allocator, body.data(), body.size()));
}

void AstFormatter::copyNodeTag(AstNode* recipient, AstNode* reference) {
    auto& recipient_tag = getNodeTag(recipient);
    auto& reference_tag = getNodeTag(reference);

    recipient_tag.dont_format = reference_tag.dont_format;
    recipient_tag.no_do_end = reference_tag.no_do_end;
    recipient_tag.inside_table_list = reference_tag.inside_table_list;
    recipient_tag.inside_tuple = reference_tag.inside_tuple;
    recipient_tag.skip_indent = reference_tag.skip_indent;
    recipient_tag.skip_last_stat_separator = reference_tag.skip_last_stat_separator;
    recipient_tag.stat_replacement = reference_tag.stat_replacement;
}

AstFormatter::FormatOptions::FormatOptions(OutputType output_type, bool simplify_expressions,
    bool optimizations, bool lua_calls, bool record_table_replace, bool list_table_replace,
    bool lph_control_flow,
    const char* separator_stat, const char* separator_block) :
    output_type(output_type), simplify_expressions(simplify_expressions), optimizations(optimizations), lua_calls(lua_calls), record_table_replace(record_table_replace),
    list_table_replace(list_table_replace), lph_control_flow(lph_control_flow), separator_stat(separator_stat), separator_block(separator_block) {}

AstFormatter::AstFormatter(Allocator& allocator, AstSimplifier& simplifier, FormatOptions options) :
    allocator(allocator), simplifier(simplifier), options(options)
{
    switch (options.output_type) {
        case FormatOptions::Beautified:
            separators.stat = "\n";
            separators.block = separators.stat;
            separators.optional_space = " ";
            separators.optional_newline = "\n";
            separators.post_keyword_expr_open = separators.optional_space;
            separators.post_keyword_expr_close = separators.post_keyword_expr_open;
            separators.optional_post_keyword_expr_close = "";
            separators.expr_list = ", ";
            separators.table_item = ",\n";
            separators.equals = " = ";
            break;
        case FormatOptions::Minified:
        case FormatOptions::Uglified:
            indents_active = false;
            newline_allowed = false;

            separators.stat = ";";
            separators.block = " ";
            separators.optional_space = "";
            separators.optional_newline = "";
            separators.post_keyword_expr_open = "(";
            separators.post_keyword_expr_close = ")";
            separators.optional_post_keyword_expr_close = separators.post_keyword_expr_close;
            separators.expr_list = ",";
            separators.table_item = separators.expr_list;
            separators.equals = "=";
            break;
    }

    setSeparatorStat(options.separator_stat ? options.separator_stat : separators.stat);

    if (options.separator_block)
        separators.block = options.separator_block;
}

void AstFormatter::setSeparatorStat(const char* sep) {
    separators.stat = sep;
    separators.stat_has_semicolon = strstr(sep, ";") != NULL;

    if (!separators.stat_has_semicolon && strlen(sep) > 1) {
        fputs("[ERROR] separators.stat needs to be one character (or have a semicolon) for appendOptionalSemicolon\n", stderr);
        exit(1);
    }
}

#define maybeDeleteVisitor(visitor) { \
    if (visitor) { \
        delete visitor; \
        visitor = nullptr; \
        simplifier.visitor = nullptr; \
    } \
}

AstFormatter::~AstFormatter() {
    maybeDeleteVisitor(record_table_replace_visitor)
    maybeDeleteVisitor(list_table_replace_visitor)
    maybeDeleteVisitor(lph_control_flow_visitor)
}

// static
AstFormatter::FormatResult AstFormatter::formatRoot(AstStatBlock* root, Allocator& allocator, AstSimplifier& simplifier, FormatOptions options) {
    bool disable_mismatch = options.simplify_expressions == simplifier.disabled;
    bool lua_calls_mismatch = options.lua_calls == simplifier.simplify_lua_calls;
    AstFormatter formatter(allocator, simplifier, options);

    auto result = formatter.formatRoot(root);
    if (disable_mismatch)
        result.warnings.insert(result.warnings.begin(), "options.simplify_expressions was not the opposite of simplifier.disabled");
    if (lua_calls_mismatch)
        result.warnings.insert(result.warnings.begin(), "options.lua_calls did not match simplifier.simplify_lua_calls");

    return result;
}
// static
AstFormatter::FormatResult AstFormatter::formatRoot(AstStatBlock* root, Allocator& allocator, FormatOptions options) {
    AstSimplifier simplifier(allocator, true, !options.simplify_expressions, options.lua_calls);
    AstFormatter formatter(allocator, simplifier, options);

    return formatter.formatRoot(root);
}

#define tagOneTrue(node, property) getNodeTag(node).property = true;
#define tagAllTrue(array, property) { \
    for (size_t i = 0; i < array.size; i++) \
        tagOneTrue(array.data[i], property) \
}

AstFormatter::FormatResult AstFormatter::formatRoot(AstStatBlock* root, bool dont_make_visitors) {
    if (!dont_make_visitors) {
        #define createVisitor(check, visitor, construction) { \
            if (check) { \
                visitor = construction; \
                root->visit(visitor); \
                if (!visitor->success) { \
                    reportWarning("visitor " #visitor " failed!"); \
                    maybeDeleteVisitor(visitor) \
                } \
                simplifier.visitor = visitor; \
            } \
        }

        createVisitor(options.record_table_replace, record_table_replace_visitor, new RecordTableReplaceVisitor(simplifier))
        createVisitor(options.list_table_replace, list_table_replace_visitor, new ListTableReplaceVisitor(simplifier))
        createVisitor(options.lph_control_flow, lph_control_flow_visitor, new LPHControlFlowVisitor(allocator, simplifier))

        #undef createVisitor
        #undef maybeDeleteVisitor
    }

    FormatResult result;

    tagOneTrue(root, no_do_end)
    auto root_formatted = formatStat(root);
    result.formatted = root_formatted ? root_formatted.value() : "-- failed to format";

    result.success = errors.empty();
    result.errors = errors;
    result.warnings = warnings;

    return result;
}

void AstFormatter::appendChar(std::string& result, char ch) {
    current += ch;
    result += ch;
}
void AstFormatter::appendStr(std::string& result, std::string str) {
    current.append(str);
    result.append(str);
}
void AstFormatter::erase(std::string& result, size_t pos, size_t n) {
    // TODO: better solution for erasing current; we can't just supply the same pos
    // also, this HEAVILY slow down the program
    // if (pos < current.size())
    //     current.erase(pos, n);
    result.erase(pos, n);
}
void AstFormatter::eraseOne(std::string& result, size_t pos) {
    erase(result, pos, 1);
}
void AstFormatter::insertEnd(std::string& result, const char* begin, const char* end) {
    current.insert(current.end(), begin, end);
    result.insert(result.end(), begin, end);
}

#define appendNode(node, name) { \
    auto format_result = formatNode(node); \
    if (!format_result) { \
        reportError(std::string("failed to append ").append(name)); \
        return std::nullopt; \
    } \
    auto value = format_result.value(); \
    appendStr(result, value); \
}
#define appendBlockNode(node, name, is_first) { \
    auto format_result = formatNode(node); \
    if (!format_result) { \
        reportError(std::string("failed to append ").append(name)); \
        return std::nullopt; \
    } \
    auto value = format_result.value(); \
    if (is_first && !value.empty() && value.at(0) == ';') { \
        current.erase(current.size() - value.size(), 1); \
        value.erase(0, 1); \
    } \
    if (!is_first && node->asStat()) \
        appendOptionalSemicolon(result, value); \
    appendStr(result, value); \
}

#define appendArray(array, name, sep) { \
    auto array_size = array.size; \
    if (array_size > 0) { \
        for (size_t i = 0; i < array_size; i++) { \
            appendNode(array.data[i], std::string(name).append(".data[").append(convertNumber(i)) += ']') \
            appendStr(result, sep); \
        } \
        erase(result, result.size() - strlen(sep), strlen(sep)); \
    } \
}

void AstFormatter::appendIndents(std::string& result) {
    if (skip_indent) {
        skip_indent = false;
        return;
    }
    if (!indent_count)
        return;

    size_t to_reserve = result.size() + indent_count * 4;
    result.reserve(to_reserve);
    current.reserve(to_reserve);
    for (size_t i = 0; i < indent_count; i++)
        appendStr(result, "    ");
}
void AstFormatter::incrementIndent() {
    if (!indents_active)
        return;

    indent_count++;
}
void AstFormatter::decrementIndent() {
    if (!indents_active)
        return;

    if (indent_count)
        indent_count--;
}

void AstFormatter::appendComment(std::string& result, const char* comment, bool force) {
    if (force)
        appendStr(result, std::string("--[=[ ").append(comment).append(" ]=]"));
    else if (newline_allowed)
        appendStr(result, std::string("-- ").append(comment));
}

#define appendFunctionArgs(real_args, vararg, name) { \
    appendChar(result, '('); \
    auto temp_args = std::vector<AstExpr*>(); \
    temp_args.reserve(real_args.size); \
    for (size_t i = 0; i < real_args.size; i++) \
        temp_args.push_back(allocator.alloc<AstExprLocal>(Location(), real_args.data[i], false)); \
    if (vararg) \
        temp_args.push_back(allocator.alloc<AstExprVarargs>(Location())); \
\
    auto new_args = copy(allocator, temp_args.data(), temp_args.size()); \
\
    appendArray(new_args, name, separators.expr_list) \
    appendChar(result, ')'); \
}

#define appendFunctionBody(func, name) { \
    appendFunctionArgs(func->args, func->vararg, name "->args"); \
    appendStr(result, separators.block); \
\
    incrementIndent(); \
    tagOneTrue(func->body, no_do_end) \
        appendNode(func->body, name "->body") \
    decrementIndent(); \
\
    appendStr(result, separators.block); \
    appendIndents(result); \
    appendStr(result, "end"); \
}

std::optional<std::string> AstFormatter::formatNode(AstNode* node) {
    if (auto expr = node->asExpr())
        return formatExpr(expr);
    else if (auto stat = node->asStat())
        return formatStat(stat);
    else if (auto type = node->asType())
        return formatType(type);

    reportError(std::string("failed formatNode: unhandled node type with class index ").append(std::to_string(node->classIndex)));
    return std::nullopt;
}
std::optional<std::string> AstFormatter::formatNode(AstLocal* local) {
    return local->name.value;
}

static const char* expr_unary_op_strings[] = {
    "not ",
    "-",
    "#"
};
static const char* expr_binary_op_strings[] = {
    "+",
    "-",
    "*",
    "/",
    "//",
    "%",
    "^",
    "..",
    "~=",
    "==",
    "<",
    "<=",
    ">",
    ">=",
    // when these start with a space, it means spaces on either side are required
    " and",
    " or"
};

size_t rewindPastFirstSpace(std::string& string, bool include_newlines = false) {
    size_t i = string.size();
    if (i > 0)
        for (i--;i >= 0;i--)
            if (string.at(i) != ' ' && (include_newlines || string.at(i) != '\n'))
                return i;

    return std::string::npos;
}
size_t AstFormatter::appendOptionalSemicolon(std::string& current, std::string& result) {
    if (separators.stat_has_semicolon)
        return std::string::npos;

    if (!result.empty() && result.at(0) == '(') {
        size_t pos = result.size();
        appendChar(current, ';');
        return pos;
    }

    return std::string::npos;
}

AstFormatter::NodeTag& AstFormatter::getNodeTag(AstNode* node) {
    if (node_tag_map.find(node) == node_tag_map.end())
        node_tag_map.emplace(node, NodeTag{});

    return node_tag_map.at(node);
}

class RepeatSimplifierChecker : public AstVisitor {
    AstStatRepeat* main_stat;
public:
    bool found = false;
    RepeatSimplifierChecker(AstStatRepeat* main_stat) : main_stat(main_stat) {}

    bool visit(AstStatBreak*) override {
        found = true;
        return false;
    }
    bool visit(AstStatContinue*) override {
        found = true;
        return false;
    }

    bool visit(AstStatBlock*) override {
        return true;
    }
    bool visit(AstStatIf*) override {
        return true;
    }
    bool visit(AstStatWhile*) override {
        return true;
    }
    bool visit(AstStatRepeat* stat_repeat) override {
        return stat_repeat == main_stat;
    }
    bool visit(AstStatReturn*) override {
        return false;
    }
    bool visit(AstStatExpr*) override {
        return false;
    }
    bool visit(AstStatLocal*) override {
        return false;
    }
    bool visit(AstStatFor*) override {
        return false;
    }
    bool visit(AstStatForIn*) override {
        return false;
    }
    bool visit(AstStatAssign*) override {
        return false;
    }
    bool visit(AstStatCompoundAssign*) override {
        return false;
    }
    bool visit(AstStatFunction*) override {
        return false;
    }
    bool visit(AstStatLocalFunction*) override {
        return false;
    }
    bool visit(AstStatTypeAlias*) override {
        return false;
    }
    bool visit(AstStatDeclareFunction*) override {
        return false;
    }
    bool visit(AstStatDeclareGlobal*) override {
        return false;
    }
    bool visit(AstStatDeclareClass*) override {
        return false;
    }
    bool visit(AstStatError*) override {
        return false;
    }
};

bool AstFormatter::canSimplifyRepeatBody(AstStatRepeat* main_stat, SimplifyResult& condition_simplified) {
    if (simplifier.disabled)
        return false;

    if (condition_simplified.type != SimplifyResult::Bool)
        return false;

    if (!condition_simplified.bool_value)
        return false;

    RepeatSimplifierChecker* visitor = new RepeatSimplifierChecker(main_stat);
    main_stat->visit(visitor);
    bool found = visitor->found;

    delete visitor;
    visitor = nullptr;

    return !found;
}

std::optional<std::string> AstFormatter::formatExpr(AstExpr* main_expr) {
    std::string result;
    auto main_expr_simplified = simplifier.simplify(main_expr);
    main_expr = main_expr_simplified.toExpr();

    if (main_expr_simplified.group) {
        appendChar(result, '(');
    }

    if (auto main_expr_as_group = main_expr->as<AstExprGroup>()) {
        // appendChar(result, '(');
        appendNode(main_expr_as_group->expr, "expr_group->expr")
        // appendChar(result, ')');
    } else if (main_expr->is<AstExprConstantNil>()) {
        appendStr(result, "nil");
    } else if (auto main_expr_as_constant_bool = main_expr->as<AstExprConstantBool>()) {
        appendStr(result, main_expr_as_constant_bool->value ? "true" : "false");
    } else if (auto main_expr_as_constant_number = main_expr->as<AstExprConstantNumber>()) {
        appendStr(result, convertNumber(main_expr_as_constant_number->value));
    } else if (auto main_expr_as_constant_string = main_expr->as<AstExprConstantString>()) {
        appendStr(result, fixString(main_expr_as_constant_string->value));
    } else if (auto main_expr_as_local = main_expr->as<AstExprLocal>()) {
        appendStr(result, main_expr_as_local->local->name.value);
    } else if (auto main_expr_as_global = main_expr->as<AstExprGlobal>()) {
        appendStr(result, main_expr_as_global->name.value);
    } else if (main_expr->is<AstExprVarargs>()) {
        appendStr(result, "...");
    } else if (auto main_expr_as_call = main_expr->as<AstExprCall>()) {
        /*
            if (!separators.stat_is_semicolon && getRootExpr(main_expr_as_call->func, false)->is<AstExprFunction>())
                appendChar(result, ';');
        */

        appendNode(main_expr_as_call->func, "call->func")
        appendChar(result, '(');
        tagAllTrue(main_expr_as_call->args, inside_tuple);
        appendArray(main_expr_as_call->args, "call->args", separators.expr_list)
        appendChar(result, ')');
    } else if (auto main_expr_as_index_name = main_expr->as<AstExprIndexName>()) {
        appendNode(main_expr_as_index_name->expr, "index_name->expr")
        appendChar(result, main_expr_as_index_name->op);
        appendStr(result, main_expr_as_index_name->index.value);
    } else if (auto main_expr_as_index_expr = main_expr->as<AstExprIndexExpr>()) {
        appendNode(main_expr_as_index_expr->expr, "index_expr->expr")
        appendChar(result, '[');
        appendNode(main_expr_as_index_expr->index, "index_expr->index")
        appendChar(result, ']');
    } else if (auto main_expr_as_function = main_expr->as<AstExprFunction>()) {
        appendStr(result, "function");

        appendFunctionBody(main_expr_as_function, "function")
    } else if (auto main_expr_as_table = main_expr->as<AstExprTable>()) {
        appendChar(result, '{');

        auto& items = main_expr_as_table->items;
        auto items_size = items.size;
        if (items_size > 0) {
            incrementIndent();
                appendStr(result, separators.optional_newline);
                for (size_t i = 0; i < items_size; i++) {
                    appendIndents(result);
                    auto& item = main_expr_as_table->items.data[i];
                    switch (item.kind) {
                        case AstExprTable::Item::List:
                            break;
                        case AstExprTable::Item::Record:
                            appendStr(result, std::string(item.key->as<AstExprConstantString>()->value.data)
                                .append(separators.equals));
                            break;
                        case AstExprTable::Item::General:
                            appendChar(result, '[');
                            appendNode(item.key, std::string("table->items.data[").append(convertNumber(i)).append("].key"))
                            appendChar(result, ']');
                            appendStr(result, separators.equals);
                            break;
                    }
                    tagOneTrue(item.value, inside_table_list)
                    appendNode(item.value, std::string("table->items.data[").append(convertNumber(i)).append("].value"))
                    appendStr(result, separators.table_item);
                }
                erase(result, result.size() - strlen(separators.table_item), strlen(separators.table_item));
            decrementIndent();
            appendStr(result, separators.optional_newline);
            appendIndents(result);
        }

        appendChar(result, '}');
    } else if (auto main_expr_as_unary = main_expr->as<AstExprUnary>()) {
        appendStr(result, expr_unary_op_strings[main_expr_as_unary->op]);
        appendNode(main_expr_as_unary->expr, "unary->expr")
    } else if (auto main_expr_as_binary = main_expr->as<AstExprBinary>()) {
        appendNode(main_expr_as_binary->left, "binary->left")

        const char* open_sep = separators.optional_space;
        const char* close_sep = separators.optional_space;
        auto string = expr_binary_op_strings[main_expr_as_binary->op];
        if (string[0] == ' ') {
            open_sep = "";
            close_sep = " ";
        }

        appendStr(result, std::string(open_sep).append(string).append(close_sep));

        appendNode(main_expr_as_binary->right, "binary->right")
    } else if (auto main_expr_as_if_else = main_expr->as<AstExprIfElse>()) {
        appendStr(result, std::string("if").append(separators.post_keyword_expr_open));
        appendNode(main_expr_as_if_else->condition, "if_else->condition")
        appendStr(result, std::string(separators.post_keyword_expr_close).append("then")
            .append(separators.post_keyword_expr_open));

        appendNode(main_expr_as_if_else->trueExpr, "if_else->trueExpr")
        appendStr(result, std::string(separators.post_keyword_expr_close).append("else")
            .append(separators.post_keyword_expr_open));

        appendNode(main_expr_as_if_else->falseExpr, "if_else->falseExpr")
        if (strcmp(separators.post_keyword_expr_close, " ") != 0)
            appendStr(result, separators.post_keyword_expr_close);
    } else if (auto main_expr_as_interp_string = main_expr->as<AstExprInterpString>()) {
        appendChar(result, '`');

        auto& string_list = main_expr_as_interp_string->strings;
        auto& expr_list = main_expr_as_interp_string->expressions;
        auto expr_list_size = expr_list.size;

        if (expr_list_size > 0) {
            auto& string = string_list.data[0];
            insertEnd(result, string.begin(), string.end());
            for (size_t i = 0; i < expr_list_size; i++) {
                appendChar(result, '{');
                appendNode(expr_list.data[i], std::string("interp_string->expressions.data[").append(convertNumber(i)) += ']')
                appendChar(result, '}');
                auto& string = string_list.data[i + 1];
                insertEnd(result, string.begin(), string.end());
            }
        }

        appendChar(result, '`');
    } else {
        auto message = std::string("failed formatExpr: unhandled expr with class index ").append(std::to_string(main_expr->classIndex));
        reportWarning(message);
        appendComment(result, message.c_str(), true);
    }

    if (main_expr_simplified.group)
        appendChar(result, ')');

    return result;
}
std::optional<std::string> AstFormatter::formatStat(AstStat* main_stat) {
    current_stat = main_stat;

    std::string result;
    auto& main_tag = getNodeTag(main_stat);

    if (main_tag.dont_format) {
        current_stat = nullptr;
        appendComment(result, "emitted stat");
        return result;
    }

    if (main_tag.stat_replacement) {
        if (main_tag.stat_replacement == main_stat) {
            fprintf(stderr, "cyclic node tag stat_replacement\n");
            exit(1);
        }
        current_stat = nullptr;
        return formatStat(main_tag.stat_replacement);
    }

    // TODO: maybe just `skip_indent = main_tag.skip_indent` which will set skip_indent to desired state potentially getting around state management issues
    if (main_tag.skip_indent)
        skip_indent = true;

    if (auto main_stat_as_block = main_stat->as<AstStatBlock>()) {
        bool do_end = !main_tag.no_do_end;
        if (do_end) {
            appendStr(result, std::string("do").append(separators.block));
            incrementIndent();
        }

        auto& body = main_stat_as_block->body;
        auto body_size = body.size;
        if (body_size < 1) {
            appendIndents(result);
            appendComment(result, "empty block");
        } else {
            bool is_first = true;
            const size_t last = body_size - 1;
            for (size_t i = 0; i < body_size; i++) {
                auto stat = body.data[i];
                if (getNodeTag(stat).dont_format)
                    continue;
                appendIndents(result);
                appendBlockNode(stat, std::string("stat_block->body.data[").append(convertNumber(i) += ']'), is_first)
                if (!(main_tag.skip_last_stat_separator && i == last))
                    appendStr(result, separators.stat);
                is_first = false;
            }
            // !is_first lets us know if we actually appended any stats (versus all being tagged with dont_format)
            if (!is_first && !main_tag.skip_last_stat_separator && separators.stat[strlen(separators.stat) - 1] == '\n')
                erase(result, result.size() - 1, 1);
        }

        if (do_end) {
            decrementIndent();
            appendStr(result, separators.block);
            appendIndents(result);
            appendStr(result, "end");
        }
    } else if (auto main_stat_as_if = main_stat->as<AstStatIf>()) {
        auto thenbody = main_stat_as_if->thenbody;
        auto elsebody = main_stat_as_if->elsebody;

        auto condition = main_stat_as_if->condition;

        auto condition_is_truthy = simplifier.simplify(condition).isTruthy();
        if (options.optimizations && condition_is_truthy != 2) {
            if (condition_is_truthy) {
                skip_indent = true;
                tagOneTrue(thenbody, no_do_end)
                appendNode(thenbody, "optimized stat_if->thenbody")
            } else if (elsebody) {
                skip_indent = true;
                tagOneTrue(elsebody, no_do_end)
                appendNode(elsebody, "optimized stat_if->elsebody")
            } else
                appendComment(result, "optimized-out if statement"); // TODO: this exists because there's an indent; find a better fix
            goto IF_BRANCH_OMIT_END;
        }

        if (options.optimizations && elsebody && thenbody->body.size == 0) {
            if (auto condition_as_binary = simplifier.simplifyToExpr(condition)->as<AstExprBinary>()) {
                auto inversed = inverseBinaryOp(condition_as_binary->op);
                if (inversed) {
                    auto new_condition = allocator.alloc<AstExprBinary>(condition_as_binary->location, inversed.value(), condition_as_binary->left, condition_as_binary->right);
                    condition = new_condition;
                    tagOneTrue(elsebody, no_do_end);
                    tagOneTrue(elsebody, skip_indent);
                    thenbody = allocateBlockFromSingleStat(allocator, elsebody);
                    tagOneTrue(thenbody, no_do_end);
                    elsebody = nullptr;
                }
            }
        }

        appendStr(result, std::string("if").append(separators.post_keyword_expr_open));
        appendNode(condition, "stat_if->condition")
        appendStr(result, std::string(separators.post_keyword_expr_close).append("then")
            .append(separators.block));

        incrementIndent();
        tagOneTrue(thenbody, no_do_end)
            appendNode(thenbody, "stat_if->thenbody")
        decrementIndent();

        appendStr(result, separators.block);
        appendIndents(result);

        if (elsebody) {
            appendStr(result, "else");

            if (auto else_body_as_if = elsebody->as<AstStatIf>()) {
                appendNode(else_body_as_if, "stat_if->elsebody");
                goto IF_BRANCH_OMIT_END;
            } else {
                appendStr(result, separators.block);

                incrementIndent();
                tagOneTrue(elsebody, no_do_end)
                    appendNode(elsebody, "stat_if->elsebody")
                decrementIndent();

                appendStr(result, separators.block);
                appendIndents(result);
            }
        }

        appendStr(result, "end");
    IF_BRANCH_OMIT_END: ;
    } else if (auto main_stat_as_while = main_stat->as<AstStatWhile>()) {
        appendStr(result, std::string("while").append(separators.post_keyword_expr_open));
        appendNode(main_stat_as_while->condition, "stat_while->condition")
        appendStr(result, std::string(separators.post_keyword_expr_close).append("do")
            .append(separators.block));

        incrementIndent();
        tagOneTrue(main_stat_as_while->body, no_do_end)
            appendNode(main_stat_as_while->body, "stat_while->body")
        decrementIndent();

        appendStr(result, separators.block);
        appendIndents(result);
        appendStr(result, "end");
    } else if (auto main_stat_as_repeat = main_stat->as<AstStatRepeat>()) {
        auto repeat_body = main_stat_as_repeat->body;
        auto repeat_condition = main_stat_as_repeat->condition;
        auto repeat_condition_simplified = simplifier.simplify(repeat_condition);
        if (canSimplifyRepeatBody(main_stat_as_repeat, repeat_condition_simplified)) {
            skip_indent = true; // below AstStatBlock pushes indent for first stat
            tagOneTrue(repeat_body, no_do_end)
            tagOneTrue(repeat_body, skip_last_stat_separator)
            appendNode(repeat_body, "stat_repeat simplified body");
        } else {
            appendStr(result, std::string("repeat").append(separators.block));

            incrementIndent();
            tagOneTrue(repeat_body, no_do_end)
                appendNode(repeat_body, "stat_repeat->body")
            decrementIndent();

            appendStr(result, separators.block);
            appendIndents(result);

            appendStr(result, std::string("until").append(separators.post_keyword_expr_open));
            appendNode(repeat_condition, "stat_repeat->condition")
            appendStr(result, separators.optional_post_keyword_expr_close);
        }
    } else if (main_stat->is<AstStatBreak>()) {
        appendStr(result, "break");
    } else if (main_stat->is<AstStatContinue>()) {
        appendStr(result, "continue");
    } else if (auto main_stat_as_return = main_stat->as<AstStatReturn>()) {
        auto& list = main_stat_as_return->list;
        appendStr(result, "return");

        if (list.size > 0) {
            appendChar(result, ' ');
            appendArray(main_stat_as_return->list, "stat_return->list", separators.expr_list)
        }
    } else if (auto main_stat_as_expr = main_stat->as<AstStatExpr>()) {
        appendNode(main_stat_as_expr->expr, "expr->expr")
    } else if (auto main_stat_as_local = main_stat->as<AstStatLocal>()) {
        appendStr(result, "local ");

        auto& value_list = main_stat_as_local->values;

        appendArray(main_stat_as_local->vars, "stat_local-vars", separators.expr_list)
        if (value_list.size > 0) {
            appendStr(result, separators.equals);
            appendArray(value_list, "stat_local->values", separators.expr_list)
        }
    } else if (auto main_stat_as_for = main_stat->as<AstStatFor>()) {
        appendStr(result, "for ");
        appendNode(main_stat_as_for->var, "for->var")
        appendStr(result, separators.equals);
        appendNode(main_stat_as_for->from, "for->from")
        appendStr(result, separators.expr_list);
        appendNode(main_stat_as_for->to, "for->to")
        auto step = main_stat_as_for->step;
        if (step) {
            appendStr(result, separators.expr_list);
            appendNode(step, "for->step")
        }

        appendStr(result, std::string(" do").append(separators.block));

        incrementIndent();
        tagOneTrue(main_stat_as_for->body, no_do_end)
            appendNode(main_stat_as_for->body, "for->body")
        decrementIndent();

        appendStr(result, separators.block);
        appendIndents(result);
        appendStr(result, "end");
    } else if (auto main_stat_as_for_in = main_stat->as<AstStatForIn>()) {
        appendStr(result, "for ");
        appendArray(main_stat_as_for_in->vars, "for_in->vars", separators.expr_list)
        appendStr(result, " in ");
        appendArray(main_stat_as_for_in->values, "for_in->values", separators.expr_list)
        appendStr(result, std::string(" do").append(separators.block));

        incrementIndent();
        tagOneTrue(main_stat_as_for_in->body, no_do_end)
            appendNode(main_stat_as_for_in->body, "for_in->body")
        decrementIndent();

        appendStr(result, separators.block);
        appendIndents(result);
        appendStr(result, "end");
    } else if (auto main_stat_as_assign = main_stat->as<AstStatAssign>()) {
        appendArray(main_stat_as_assign->vars, "assign->vars", separators.expr_list)
        appendStr(result, separators.equals);
        appendArray(main_stat_as_assign->values, "assign->values", separators.expr_list)
    } else if (auto main_stat_as_compound_assign = main_stat->as<AstStatCompoundAssign>()) {
        appendNode(main_stat_as_compound_assign->var, "compount_assign->var")
        appendStr(result, std::string(separators.optional_space).append(expr_binary_op_strings[main_stat_as_compound_assign->op]));
        appendChar(result, '=');
        appendStr(result, separators.optional_space);
        appendNode(main_stat_as_compound_assign->value, "compount_assign->value")
    } else if (auto main_stat_as_function = main_stat->as<AstStatFunction>()) {
        auto name = main_stat_as_function->name;
        auto func = main_stat_as_function->func;
        auto name_as_index_name = name->as<AstExprIndexName>();
        bool is_namecall = name_as_index_name == nullptr ? false : name_as_index_name->op == ':';
        if (is_namecall) {
            appendStr(result, "function ");
            appendNode(name, "function->name")
            appendFunctionBody(func, "function->func")
        } else {
            appendNode(name, "function->name")
            appendStr(result, separators.equals);
            appendNode(func, "function->func")
        }
    } else if (auto main_stat_as_local_function = main_stat->as<AstStatLocalFunction>()) {
        appendStr(result, std::string("local function ")
            .append(main_stat_as_local_function->name->name.value));
        appendFunctionBody(main_stat_as_local_function->func, "local_function->func")
    } else {
        auto message = std::string("failed formatStat: unhandled stat with class index ").append(std::to_string(main_stat->classIndex));
        reportWarning(message);
        appendComment(result, message.c_str(), true);
    }

    // note: everything you do here must be replicated in `if main_tag.stat_replacement` branch at the beginning of this function
    current_stat = nullptr;

    return result;
}
std::optional<std::string> AstFormatter::formatType(AstType* main_type) {
    std::string result;

    // TODO: format types
    /*
    if (auto main_type_as_reference = main_type->as<AstTypeReference>()) {
        auto& prefix = main_type_as_reference->prefix;
        if (prefix)
            appendStr(result, prefix->value);
            appendChar(result, '.');

        appendStr(result, main_type_as_reference->name.value);

        auto& parameter_list = main_type_as_reference->parameters;
        auto parameter_list_size = parameter_list.size;
        if (parameter_list_size > 0 || main_type_as_reference->hasParameterList) {
            appendChar(result, '<');
            for (size_t i = 0; i < parameter_list_size; i++) {
                // TODO: this separator might not work with spaces
                appendChar(result, ',');
                auto& param = parameter_list.data[i];
                appendNode(param.type ? param.type->asType() : param.typePack->asType(), std::string("reference->params.data[").append(convertNumber(i)) += ']')
            }
            appendChar(result, '>');
        }
    } else */ {
        auto message = std::string("failed formatType: unhandled type with class index ").append(std::to_string(main_type->classIndex));
        reportWarning(message);
        appendComment(result, message.c_str(), true);
    }

    return result;
}

#undef appendExpr
