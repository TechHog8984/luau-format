#include <cmath>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <optional>
#include <unordered_map>

#include "simplifier.hpp"
#include "Luau/Lexer.h"
#include "formatter.hpp"

#include "Luau/Ast.h"
#include "lua.h"
#include "lualib.h"

namespace LuauFormat {

#define safeSetExpr(value) { \
    auto& _expr = value; \
    if (_expr->is<AstExprFunction>() || _expr->is<AstExprVarargs>()) \
        return expr; \
    expr = _expr; \
}
AstExpr* getRootExpr(AstExpr* expr, bool safe) {
    if (safe) {
        while (true) {
            if (auto expr_group = expr->as<AstExprGroup>()) {
                safeSetExpr(expr_group->expr)
                continue;
            } else if (auto expr_type_assertion = expr->as<AstExprTypeAssertion>()) {
                safeSetExpr(expr_type_assertion->expr);
                continue;
            }
            break;
        }
    } else {
        AstExprGroup* expr_group;
        while ((expr_group = expr->as<AstExprGroup>()))
            expr = expr_group->expr;
    }

    return expr;
}
#undef safeSetExpr

/*
    0 -> falsy
    1 -> truthy
    2 -> can't be determined
*/
uint8_t isExpressionTruthy(AstExpr* expr, bool allow_globals) {
    expr = getRootExpr(expr, false);

    if (expr->is<AstExprConstantNil>())
        return 0;
    else if (auto expr_constant_bool = expr->as<AstExprConstantBool>())
        return expr_constant_bool->value;
    else if (expr->is<AstExprConstantNumber>() || expr->is<AstExprConstantString>() || expr->is<AstExprFunction>() || expr->is<AstExprTable>() || expr->is<AstExprInterpString>())
        return 1;
    else if (auto expr_index_name = expr->as<AstExprIndexName>()) {
        if (allow_globals) {
            auto global = getRootExpr(expr_index_name->expr, false)->as<AstExprGlobal>();
            if (!global)
                return 0;

            const char* global_name = global->name.value;
            const char* index = expr_index_name->index.value;

            if (strcmp(global_name, "bit32") == 0)
                return strcmp(index, "arshift") == 0
                    || strcmp(index, "band") == 0
                    || strcmp(index, "bnot") == 0
                    || strcmp(index, "bnot") == 0
                    || strcmp(index, "bor") == 0
                    || strcmp(index, "btest") == 0
                    || strcmp(index, "bxor") == 0
                    || strcmp(index, "byteswap") == 0
                    || strcmp(index, "countlz") == 0
                    || strcmp(index, "countrz") == 0
                    || strcmp(index, "extract") == 0
                    || strcmp(index, "replace") == 0
                    || strcmp(index, "lrotate") == 0
                    || strcmp(index, "lshift") == 0
                    || strcmp(index, "rrotate") == 0
                    || strcmp(index, "rshift") == 0;
            else if (strcmp(global_name, "buffer") == 0)
                return strcmp(index, "create") == 0
                    || strcmp(index, "fromstring") == 0
                    || strcmp(index, "tostring") == 0
                    || strcmp(index, "len") == 0
                    || strcmp(index, "readbits") == 0
                    || strcmp(index, "readi8") == 0
                    || strcmp(index, "readu8") == 0
                    || strcmp(index, "readi16") == 0
                    || strcmp(index, "readu16") == 0
                    || strcmp(index, "readi32") == 0
                    || strcmp(index, "readu32") == 0
                    || strcmp(index, "readf32") == 0
                    || strcmp(index, "readf64") == 0
                    || strcmp(index, "writebits") == 0
                    || strcmp(index, "writei8") == 0
                    || strcmp(index, "writeu8") == 0
                    || strcmp(index, "writei16") == 0
                    || strcmp(index, "writeu16") == 0
                    || strcmp(index, "writei32") == 0
                    || strcmp(index, "writeu32") == 0
                    || strcmp(index, "writef32") == 0
                    || strcmp(index, "writef64") == 0
                    || strcmp(index, "readstring") == 0
                    || strcmp(index, "writestring") == 0
                    || strcmp(index, "copy") == 0
                    || strcmp(index, "fill") == 0;
            else if (strcmp(global_name, "coroutine") == 0)
                return strcmp(index, "close") == 0
                    || strcmp(index, "create") == 0
                    || strcmp(index, "isyieldable") == 0
                    || strcmp(index, "resume") == 0
                    || strcmp(index, "running") == 0
                    || strcmp(index, "status") == 0
                    || strcmp(index, "wrap") == 0
                    || strcmp(index, "yield") == 0;
            else if (strcmp(global_name, "debug") == 0)
                return strcmp(index, "traceback") == 0
                    || strcmp(index, "info") == 0;
                // TODO: roblox setting
                    // || strcmp(index, "profilebegin") == 0
                    // || strcmp(index, "profileend") == 0
                    // || strcmp(index, "getmemorycategory") == 0
                    // || strcmp(index, "setmemorycategory") == 0
                    // || strcmp(index, "resetmemorycategory") == 0
                    // || strcmp(index, "dumpcodesize") == 0;
            else if (strcmp(global_name, "math") == 0)
                return strcmp(index, "abs") == 0
                    || strcmp(index, "acos") == 0
                    || strcmp(index, "asin") == 0
                    || strcmp(index, "atan") == 0
                    || strcmp(index, "atan2") == 0
                    || strcmp(index, "ceil") == 0
                    || strcmp(index, "clamp") == 0
                    || strcmp(index, "cos") == 0
                    || strcmp(index, "cosh") == 0
                    || strcmp(index, "deg") == 0
                    || strcmp(index, "exp") == 0
                    || strcmp(index, "floor") == 0
                    || strcmp(index, "fmod") == 0
                    || strcmp(index, "frexp") == 0
                    || strcmp(index, "ldexp") == 0
                    || strcmp(index, "lerp") == 0
                    || strcmp(index, "log") == 0
                    || strcmp(index, "log10") == 0
                    || strcmp(index, "map") == 0
                    || strcmp(index, "max") == 0
                    || strcmp(index, "min") == 0
                    || strcmp(index, "modf") == 0
                    || strcmp(index, "noise") == 0
                    || strcmp(index, "pow") == 0
                    || strcmp(index, "rad") == 0
                    || strcmp(index, "random") == 0
                    || strcmp(index, "randomseed") == 0
                    || strcmp(index, "round") == 0
                    || strcmp(index, "sign") == 0
                    || strcmp(index, "sin") == 0
                    || strcmp(index, "sinh") == 0
                    || strcmp(index, "sqrt") == 0
                    || strcmp(index, "tan") == 0
                    || strcmp(index, "tanh") == 0

                    || strcmp(index, "huge") == 0
                    || strcmp(index, "pi") == 0;
            else if (strcmp(global_name, "os") == 0)
                return strcmp(index, "clock") == 0
                    || strcmp(index, "date") == 0
                    || strcmp(index, "difftime") == 0
                    || strcmp(index, "time");
            else if (strcmp(global_name, "string") == 0)
                return strcmp(index, "byte") == 0
                    || strcmp(index, "char") == 0
                    || strcmp(index, "find") == 0
                    || strcmp(index, "format") == 0
                    || strcmp(index, "gmatch") == 0
                    || strcmp(index, "gsub") == 0
                    || strcmp(index, "len") == 0
                    || strcmp(index, "lower") == 0
                    || strcmp(index, "match") == 0
                    || strcmp(index, "pack") == 0
                    || strcmp(index, "packsize") == 0
                    || strcmp(index, "rep") == 0
                    || strcmp(index, "reverse") == 0
                    || strcmp(index, "split") == 0
                    || strcmp(index, "sub") == 0
                    || strcmp(index, "unpack") == 0
                    || strcmp(index, "upper") == 0;
            else if (strcmp(global_name, "table") == 0)
                return strcmp(index, "clear") == 0
                    || strcmp(index, "clone") == 0
                    || strcmp(index, "concat") == 0
                    || strcmp(index, "create") == 0
                    || strcmp(index, "find") == 0
                    || strcmp(index, "freeze") == 0
                    || strcmp(index, "insert") == 0
                    || strcmp(index, "isfrozen") == 0
                    || strcmp(index, "maxn") == 0
                    || strcmp(index, "move") == 0
                    || strcmp(index, "pack") == 0
                    || strcmp(index, "remove") == 0
                    || strcmp(index, "sort") == 0
                    || strcmp(index, "unpack") == 0;
            // TODO: roblox setting
            // else if (strcmp(global_name, "task") == 0)
            //     return strcmp(index, "spawn") == 0
            //         || strcmp(index, "defer") == 0
            //         || strcmp(index, "delay") == 0
            //         || strcmp(index, "desynchronize") == 0
            //         || strcmp(index, "synchronize") == 0
            //         || strcmp(index, "wait") == 0
            //         || strcmp(index, "cancel") == 0;
            else if (strcmp(global_name, "utf8") == 0)
                return strcmp(index, "char") == 0
                    || strcmp(index, "codes") == 0
                    || strcmp(index, "codepoint") == 0
                    || strcmp(index, "len") == 0
                    || strcmp(index, "offset") == 0
                    || strcmp(index, "graphemes") == 0
                    || strcmp(index, "nfcnormalize") == 0
                    || strcmp(index, "nfdnormalize") == 0;
        }
    } else if (auto global_expr = expr->as<AstExprGlobal>()) {
        if (allow_globals) {
            const char* global_name = global_expr->name.value;

            if (strcmp(global_name, "assert") == 0
                || strcmp(global_name, "error") == 0
                || strcmp(global_name, "gcinfo") == 0
                || strcmp(global_name, "getmetatable") == 0
                || strcmp(global_name, "ipairs") == 0
                || strcmp(global_name, "loadstring") == 0
                || strcmp(global_name, "newproxy") == 0
                || strcmp(global_name, "next") == 0
                || strcmp(global_name, "pairs") == 0
                || strcmp(global_name, "pcall") == 0
                || strcmp(global_name, "print") == 0
                || strcmp(global_name, "rawequal") == 0
                || strcmp(global_name, "rawget") == 0
                || strcmp(global_name, "rawlen") == 0
                || strcmp(global_name, "rawset") == 0
                || strcmp(global_name, "require") == 0
                || strcmp(global_name, "select") == 0
                || strcmp(global_name, "setmetatable") == 0
                || strcmp(global_name, "tonumber") == 0
                || strcmp(global_name, "tostring") == 0
                || strcmp(global_name, "type") == 0
                || strcmp(global_name, "unpack") == 0
                || strcmp(global_name, "xpcall") == 0

                || strcmp(global_name, "_G") == 0
                || strcmp(global_name, "_VERSION") == 0
            )
                return 1;
        }
    }

    return 2;
}

std::string convertNumber(double value) {
    double decimal, integer;

    decimal = modf(value, &integer);

    std::string result;

    if (decimal == 0) {
        result = std::to_string(integer);
    } else {
        char str[500];
        sprintf(str, "%.15f", value);

        result = str;
    };

    if (result == "inf")
        result = "math.huge";
    else if (result == "nan" or result == "-nan")
        result = "(0/0)";
    else {
        while (result.length() > 2 && result[result.length() - 1] == '0')
            result.erase(result.length() - 1, 1);

        if (result[result.length() - 1] == '.')
            result.erase(result.length() - 1, 1);
    };

    return result;
};
std::string fixString(AstArray<char> value) {
    std::string result = "\"";

    for (size_t i = 0; i < value.size; i++) {
        auto& ch = value.data[i];
        if (ch > 31 && ch < 127 && ch != '"' && ch != '\\')
            result.append(std::string{ch});
        else {
            switch (ch) {
                case 7:
                    result.append("\\a");
                    break;
                case 8:
                    result.append("\\b");
                    break;
                case 9:
                    result.append("\\t");
                    break;
                case 10:
                    result.append("\\n");
                    break;
                case 11:
                    result.append("\\v");
                    break;
                case 12:
                    result.append("\\f");
                    break;
                case 13:
                    result.append("\\r");
                    break;

                case '"':
                    result.append("\\\"");
                    break;
                case '\\':
                    result.append("\\\\");
                    break;

                default:
                    result.append("\\");

                    unsigned char n = (unsigned char) ch;
                    if (n < 10)
                        result.append("00");
                    else if (n < 100)
                        result.append("0");

                    result.append(std::to_string((unsigned char)ch));
            };
        };
    };

    result.append("\"");

    return result;
};

bool equalsCharArray(const AstArray<char> left, const AstArray<char> right) {
    if (left.size != right.size)
        return false;

    for (unsigned i = 0; i < left.size; i++)
        if (left.data[i] != right.data[i])
            return false;

    return true;
}

int charArrayCmp(const AstArray<char> left, const AstArray<char> right) {
    const char* l = left.data;
    const char* r = right.data;

    // always safe to read one character because even empty strings are nul terminated
    if (*l != *r)
        return uint8_t(*l) - uint8_t(*r);

    size_t size_left = left.size;
    size_t size_right = right.size;
    size_t size_min = size_left < size_right ? size_left : size_right;

    int res = memcmp(l, r, size_min);
    if (res != 0)
        return res;

    return size_left == size_right ? 0 : size_left < size_right ? -1 : 1;
}


uint8_t binaryCompareBool_ne(bool left, bool right) {
    return left != right;
}
uint8_t binaryCompareNumber_ne(double left, double right) {
    return left != right;
}
uint8_t binaryCompareString_ne(const AstArray<char> left, const AstArray<char> right) {
    return !equalsCharArray(left, right);
}

uint8_t binaryCompareBool_eq(bool left, bool right) {
    return left == right;
}
uint8_t binaryCompareNumber_eq(double left, double right) {
    return left == right;
}
uint8_t binaryCompareString_eq(const AstArray<char> left, const AstArray<char> right) {
    return equalsCharArray(left, right);
}

uint8_t binaryCompareBool_lt(bool left, bool right) {
    return 2;
}
uint8_t binaryCompareNumber_lt(double left, double right) {
    return left < right;
}
uint8_t binaryCompareString_lt(const AstArray<char> left, const AstArray<char> right) {
    return charArrayCmp(left, right) < 0;
}

uint8_t binaryCompareBool_le(bool left, bool right) {
    return 2;
}
uint8_t binaryCompareNumber_le(double left, double right) {
    return left <= right;
}
uint8_t binaryCompareString_le(const AstArray<char> left, const AstArray<char> right) {
    return charArrayCmp(left, right) <= 0;
}

uint8_t binaryCompareBool_gt(bool left, bool right) {
    return 2;
}
uint8_t binaryCompareNumber_gt(double left, double right) {
    return left > right;
}
uint8_t binaryCompareString_gt(const AstArray<char> left, const AstArray<char> right) {
    return charArrayCmp(left, right) > 0;
}

uint8_t binaryCompareBool_ge(bool left, bool right) {
    return 2;
}
uint8_t binaryCompareNumber_ge(double left, double right) {
    return left >= right;
}
uint8_t binaryCompareString_ge(const AstArray<char> left, const AstArray<char> right) {
    return charArrayCmp(left, right) >= 0;
}

std::optional<SimpleAssign> getSimpleAssign(AstStat* stat, bool can_have_no_values) {
    if (auto stat_as_assign = stat->as<AstStatAssign>()) {
        auto& var_list = stat_as_assign->vars;
        auto& value_list = stat_as_assign->values;

        bool has_value = false;
        if (var_list.size != 1)
            return std::nullopt;
        if (can_have_no_values) {
            if (value_list.size > 1)
                return std::nullopt;
        } else {
            if (value_list.size != 1)
                return std::nullopt;
            has_value = true;
        }

        auto var = var_list.data[0]->as<AstExprLocal>();
        if (!var)
            return std::nullopt;

        auto& local = var->local;

        return SimpleAssign{
            .var = local->name.value,
            .var_local = local,
            .value = has_value ? value_list.data[0] : nullptr
        };
    } else if (auto stat_as_local = stat->as<AstStatLocal>()) {
        auto& var_list = stat_as_local->vars;
        auto& value_list = stat_as_local->values;

        bool has_value = false;
        if (var_list.size != 1)
            return std::nullopt;
        if (can_have_no_values) {
            if (value_list.size > 1)
                return std::nullopt;
        } else {
            if (value_list.size != 1)
                return std::nullopt;
            has_value = true;
        }

        auto& local = var_list.data[0];

        return SimpleAssign{
            .var = local->name.value,
            .var_local = local,
            .value = has_value ? value_list.data[0] : nullptr
        };
    }

    return std::nullopt;
}

std::optional<Condition> Condition::tryCreateCondition(AstSimplifier& simplifier, AstExprBinary* expr_binary) {
    if (!expr_binary)
        return std::nullopt;

    Condition result;
    result.op = expr_binary->op;
    result.flipped = false;

    if (!(result.op == AstExprBinary::CompareNe or result.op == AstExprBinary::CompareEq or result.op == AstExprBinary::CompareLt
        or result.op == AstExprBinary::CompareLe or result.op == AstExprBinary::CompareGt or result.op == AstExprBinary::CompareGe
    ))
        return std::nullopt;

    auto left = simplifier.simplify(expr_binary->left);
    auto right = simplifier.simplify(expr_binary->right);

    auto left_as_number = left.asNumber();
    auto right_as_number = right.asNumber();

    if (left_as_number && right_as_number)
        return std::nullopt;

    AstExpr* expr_to_check = right.toExpr();
    if (left_as_number) {
        result.number_is_left = true;
        result.number = left_as_number.value();
    } else if (right_as_number) {
        result.number_is_left = false;
        result.number = right_as_number.value();
        expr_to_check = left.toExpr();
    } else {
        return std::nullopt;
    }

    auto expr_local = expr_to_check->as<AstExprLocal>();
    if (!expr_local)
        return std::nullopt;

    result.local = expr_local->local;

    return result;
}
std::optional<Condition> Condition::tryCreateCondition(AstSimplifier& simplifier, AstExprBinary* expr_binary, const char* expected_local) {
    auto condition = tryCreateCondition(simplifier, expr_binary);
    if (!condition)
        return std::nullopt;

    if (strcmp(condition->local->name.value, expected_local) != 0)
        return std::nullopt;

    return condition;
}
std::optional<Condition> Condition::tryCreateCondition(AstSimplifier& simplifier, AstExprBinary* expr_binary, AstLocal* expected_local) {
    auto condition = tryCreateCondition(simplifier, expr_binary);
    if (!condition)
        return std::nullopt;

    if (condition->local != expected_local)
        return std::nullopt;

    return condition;
}

bool Condition::passes(double input) {
    double left = number_is_left ? number : input;
    double right = number_is_left ? input : number;

    bool result = false;

    switch (op) {
        case AstExprBinary::CompareNe:
            result = left != right;
            break;
        case AstExprBinary::CompareEq:
            result = left == right;
            break;
        case AstExprBinary::CompareLt:
            result = left < right;
            break;
        case AstExprBinary::CompareLe:
            result = left <= right;
            break;
        case AstExprBinary::CompareGt:
            result = left > right;
            break;
        case AstExprBinary::CompareGe:
            result = left >= right;
            break;
        default: {
            fprintf(stderr, "ERROR passes: unhandled op %d\n", op);
            exit(1);
        }
    }

    if (flipped)
        result = !result;

    return result;
}

RecordTableReplaceVisitor::RecordTableReplaceVisitor(AstSimplifier& simplifier) : simplifier(simplifier) {}

bool RecordTableReplaceVisitor::visit(AstStatBlock* root_block) {
    auto root_block_body = root_block->body;
    if (root_block_body.size < 1)
        goto RET;

    {

    auto last_stat_as_return = root_block_body.data[root_block_body.size - 1]->as<AstStatReturn>();
    if (!last_stat_as_return)
        goto RET;

    auto last_stat_as_return_list = last_stat_as_return->list;
    if (last_stat_as_return_list.size != 1)
        goto RET;

    auto outer_call = getRootExpr(last_stat_as_return_list.data[0])->as<AstExprCall>();
    if (!outer_call)
        goto RET;

    auto inner_call = getRootExpr(outer_call->func)->as<AstExprCall>();
    if (!inner_call)
        goto RET;

    auto return_func_as_function = getRootExpr(inner_call->func)->as<AstExprFunction>();
    if (!return_func_as_function)
        goto RET;

    auto call_parameters = inner_call->args;
    if (call_parameters.size < 1)
        goto RET;

    key_to_expr_map.clear();
    size_t table_index_in_parameters = -1;
    for (size_t i = 0; i < call_parameters.size; i++) {
        auto expr_as_table = getRootExpr(call_parameters.data[i])->as<AstExprTable>();
        if (!expr_as_table)
            continue;
        auto items = expr_as_table->items;
        bool passes = items.size > 0;
        for (size_t j = 0; j < items.size; j++) {
            auto& item = items.data[j];
            if (item.kind != Luau::AstExprTable::Item::Record) {
                passes = false;
                break;
            }
            key_to_expr_map.emplace(item.key->as<AstExprConstantString>()->value.data, item.value);
        }
        if (passes) {
            table_index_in_parameters = i;
            break;
        }
    }

    if (table_index_in_parameters == (size_t) -1)
        goto RET;

    auto function_args = return_func_as_function->args;
    if (function_args.size <= table_index_in_parameters)
        goto RET;

    arg_local = function_args.data[table_index_in_parameters];

    success = true;

    }

RET:
    return false;
}

ListTableReplaceVisitor::ListTableReplaceVisitor(AstSimplifier& simplifier) : simplifier(simplifier) {}

bool ListTableReplaceVisitor::visit(AstStatBlock* root_block) {
    auto root_block_body = root_block->body;
    if (root_block_body.size < 1)
        goto RET;

    {

    auto last_stat_as_return = root_block_body.data[root_block_body.size - 1]->as<AstStatReturn>();
    if (!last_stat_as_return)
        goto RET;

    auto last_stat_as_return_list = last_stat_as_return->list;
    if (last_stat_as_return_list.size != 1)
        goto RET;

    auto outer_call = getRootExpr(last_stat_as_return_list.data[0])->as<AstExprCall>();
    if (!outer_call)
        goto RET;

    auto inner_call = getRootExpr(outer_call->func)->as<AstExprCall>();
    if (!inner_call)
        goto RET;

    auto return_func_as_function = getRootExpr(inner_call->func)->as<AstExprFunction>();
    if (!return_func_as_function)
        goto RET;

    auto call_parameters = inner_call->args;
    if (call_parameters.size < 1)
        goto RET;

    index_to_expr_map.clear();
    size_t table_index_in_parameters = -1;
    for (size_t i = 0; i < call_parameters.size; i++) {
        auto expr_as_table = getRootExpr(call_parameters.data[i])->as<AstExprTable>();
        if (!expr_as_table)
            continue;
        auto items = expr_as_table->items;
        bool passes = items.size > 0;
        for (size_t ti = 0; ti < items.size; ti++) {
            auto item = &items.data[ti];
            if (item->kind != Luau::AstExprTable::Item::List) {
                passes = false;
                break;
            }
            index_to_expr_map.emplace(ti + 1, item->value);
        }
        if (passes) {
            table_index_in_parameters = i;
            break;
        }
    }

    if (table_index_in_parameters == (size_t) -1)
        goto RET;

    auto function_args = return_func_as_function->args;
    if (function_args.size <= table_index_in_parameters)
        goto RET;

    arg_local = function_args.data[table_index_in_parameters];
    success = true;

    }

RET:
    return false;
}


class LPHControlFlowSolver {
    Allocator& allocator;
    AstSimplifier& simplifier;

    std::unordered_map<double, AstStatBlock*> cfposition_to_block_map;
    std::vector<double> cfposition_set_order;

    void tryHandleSet_cftable(AstStat* stat);
    void tryHandleSet_cfposition(AstStat* stat);
    int handleIfBlock(AstStatBlock* main_block);
    int handleIf(AstStatIf* main_if, bool passes);
public:
    std::unordered_map<double, double>& cftable;
    double cfposition;
    AstLocal* cftable_local;
    AstLocal* cfposition_local;

    LPHControlFlowSolver(Allocator& allocator, AstSimplifier& simplifier, std::unordered_map<double, double>& cftable, double cfposition, AstLocal* cftable_local, AstLocal* cfposition_local)
        : allocator(allocator), simplifier(simplifier), cftable(cftable), cfposition(cfposition), cftable_local(cftable_local), cfposition_local(cfposition_local)
    {
        cfposition_set_order.push_back(cfposition);
    }

    void handleLoopBody(AstStat* main_loop_stat, AstArray<AstStat*> main_loop_body) {
        if (main_loop_body.size < 1)
            return;

        auto first_stat_as_if = main_loop_body.data[0]->as<AstStatIf>();
        if (!first_stat_as_if)
            return;

        auto condition = Condition::tryCreateCondition(simplifier, first_stat_as_if->condition->as<AstExprBinary>(), cfposition_local);
        if (!condition)
            return;

        while (!handleIf(first_stat_as_if, condition.value().passes(cfposition)))
            ;

        std::vector<AstStat*> generated_block_list;
        generated_block_list.reserve(cfposition_to_block_map.size()); // not accurate
        for (size_t i = 0; i < cfposition_set_order.size(); i++) {
            double& value = cfposition_set_order.at(i);
            if (cfposition_to_block_map.find(value) == cfposition_to_block_map.end()) {
                fprintf(stderr, "no block found with cfposition %.f", value);
                return;
            }

            auto stat = cfposition_to_block_map.at(value);

            for (size_t j = 0; j < stat->body.size; j++) {
                auto inner_stat = stat->body.data[j];
                if (inner_stat->is<AstStatBreak>() || inner_stat->is<AstStatContinue>())
                    continue;
                generated_block_list.push_back(inner_stat);
            }
        }
        AstStatBlock* generated_block = allocator.alloc<AstStatBlock>(Location(), copy(allocator, generated_block_list.data(), generated_block_list.size()));;

        AstFormatter::getNodeTag(main_loop_stat).stat_replacement = generated_block;
        auto& generated_block_tag = AstFormatter::getNodeTag(generated_block);
        generated_block_tag.no_do_end = true;
        generated_block_tag.skip_indent = true;
    }
};

// typedef struct {
//     AstArray<AstStat*> body;
// } InfiniteLoop;
// std::optional<InfiniteLoop> getInfiniteLoop(AstSimplifier& simplifier, AstStat* main_stat) {
//     std::optional<InfiniteLoop> result = std::nullopt;

//     if (auto main_stat_as_while = main_stat->as<AstStatWhile>()) {
//         if (simplifier.simplify(main_stat_as_while->condition).isTruthy() != 1)
//             goto RET;
//         result = std::make_optional(InfiniteLoop{ .body = main_stat_as_while->body->body });
//     } else if (auto main_stat_as_repeat = main_stat->as<AstStatRepeat>()) {
//         if (simplifier.simplify(main_stat_as_repeat->condition).isTruthy() != 0)
//             goto RET;
//         result = std::make_optional(InfiniteLoop{ .body = main_stat_as_repeat->body->body });
//     }

// RET:
//     return result;
// }

SimplifyResult::SimplifyResult(AstSimplifier* simplifier, AstExprConstantNil* nil_value, bool group) : simplifier(simplifier), type(Nil), nil_value(nil_value), group(group) {}
SimplifyResult::SimplifyResult(AstSimplifier* simplifier, bool bool_value, bool group) : simplifier(simplifier), type(Bool), bool_value(bool_value), group(group) {}
SimplifyResult::SimplifyResult(AstSimplifier* simplifier, double number_value, bool group) : simplifier(simplifier), type(Number), number_value(number_value), group(group) {}
SimplifyResult::SimplifyResult(AstSimplifier* simplifier, AstArray<char> string_value, bool group) : simplifier(simplifier), type(String), string_value(string_value), group(group) {}
SimplifyResult::SimplifyResult(AstSimplifier* simplifier, AstExpr* other_value, bool group) : simplifier(simplifier), type(Other), other_value(other_value), group(group) {}

AstExpr* SimplifyResult::toExpr() {
    switch (type) {
        case Nil:
            return nil_value;
        case Bool:
            return simplifier->getAllocator().alloc<AstExprConstantBool>(Location(), bool_value);
        case Number:
            return simplifier->getAllocator().alloc<AstExprConstantNumber>(Location(), number_value);
        case String:
            return simplifier->getAllocator().alloc<AstExprConstantString>(Location(), string_value);
        case Other:
            return other_value;
    }
    // TODO: think of a better solution
    return nullptr;
}
std::optional<double> SimplifyResult::asNumber() {
    switch (type) {
        case Number:
            return number_value;
        case String: {
            // TODO: check behavior with zero bytes
            char* end;
            errno = 0;
            int result = strtod(string_value.data, &end);
            if (errno == ERANGE || *string_value.data == '\0' || *end != '\0')
                return std::nullopt;
            return result;
        }
        default:
            return std::nullopt;
    }
}
std::optional<AstArray<char>> SimplifyResult::asString() {
    if (type == String)
        return string_value;

    auto result = AstArray<char>();
    result.data = nullptr;
    result.size = 0;
    switch (type) {
        case Number: {
            std::string str = convertNumber(number_value);
            const size_t len = str.size();
            char* allocated = static_cast<char*>(simplifier->getAllocator().allocate(len));
            memcpy(allocated, str.data(), len);
            result.data = allocated;
            result.size = len;
            return result;
        }
        default:
            return std::nullopt;
    }
}
uint8_t SimplifyResult::isTruthy() {
    return isExpressionTruthy(toExpr(), simplifier->assume_globals);
}

AstSimplifier::AstSimplifier(Allocator& allocator, bool safe, bool disabled, bool simplify_lua_calls, bool assume_globals) :
    allocator(allocator), safe(safe), disabled(disabled), simplify_lua_calls(simplify_lua_calls), assume_globals(assume_globals) {}

Allocator& AstSimplifier::getAllocator() {
    return allocator;
}

std::optional<std::vector<AstExpr*>> AstSimplifier::getDeterminableList(std::vector<AstExpr*> list) {
    size_t list_size = list.size();
    std::vector<AstExpr*> determinable_list;

    for (size_t i = 0; i < list_size; i++) {
        AstExpr* value = simplifyToExpr(list[i]);

        if (isExpressionTruthy(value, assume_globals) < 2) // if a value can be determined as truthy, we consider it "determinable"
            determinable_list.push_back(value);
        else if (auto value_call = value->as<AstExprCall>()) {
            if (auto func = getRootExpr(value_call->func, false)->as<AstExprFunction>()) {
                auto& body = func->body->body;
                // we need to ensure that there is only one return
                // this could easily be improved using a visitor that looks for return stats
                if (body.size != 1)
                    return std::nullopt;

                auto return_stat = body.data[0]->as<AstStatReturn>();
                if (!return_stat)
                    return std::nullopt;

                std::vector<AstExpr*> sub_list;

                // bool vararg = func->args.size == 0 && func->vararg;
                auto& return_list = return_stat->list;
                auto& return_count = return_list.size;
                if (return_count == 0) {
                    determinable_list.push_back(allocator.alloc<AstExprConstantNil>(Location(Position(0, 0), 0)));
                    continue;
                }

                if (i + 1 == list_size) {
                    for (unsigned index = 0; index < return_count; index++)
                        sub_list.push_back(return_list.data[index]);

                    if (getRootExpr(sub_list.back())->is<AstExprVarargs>(), false) {
                        sub_list.pop_back();
                        for (size_t j = 0; j < value_call->args.size; j++) {
                            auto& arg = value_call->args.data[j];
                            sub_list.push_back(arg);
                        }
                    }

                    auto sub_determinable_list = getDeterminableList(sub_list);
                    if (sub_determinable_list.has_value()) {
                        determinable_list.insert(std::end(determinable_list), std::begin(sub_determinable_list.value()), std::end(sub_determinable_list.value()));
                    } else
                        return std::nullopt;
                } else
                    determinable_list.push_back(return_list.data[0]);
            } else
                return std::nullopt;
        } else
            return std::nullopt;
    }

    return determinable_list;
}
std::optional<std::vector<AstExpr*>> AstSimplifier::getDeterminableList(AstArray<AstExpr*> list) {
    std::vector<AstExpr*> vector(list.data, list.data + list.size);
    return getDeterminableList(vector);
}

std::optional<size_t> AstSimplifier::getTableSize(AstExprTable* table) {
    std::vector<AstExpr*> list;
    auto& items = table->items;

    if (items.size == 0)
        return 0;

    for (unsigned index = 0; index < items.size; index++) {
        auto& item = items.data[index];
        if (item.kind != AstExprTable::Item::List)
            return std::nullopt;

        list.push_back(item.value);
    }

    auto determinable_list = getDeterminableList(list);
    if (!determinable_list.has_value())
        return std::nullopt;

    /*
    ** Try to find a boundary in table `t'. A `boundary' is an integer index
    ** such that t[i] is non-nil and t[i+1] is nil (and 0 if t[1] is nil).
    */

    int j = determinable_list->size();

    if (j > 0) {
        auto expr = simplifyToExpr(determinable_list->at(j - 1));

        if (expr->is<AstExprConstantNil>()) {
            #define solveAndCheckNil(oldexpr) expr = simplifyToExpr(oldexpr); \
                bool is_nil = expr->is<AstExprConstantNil>();

            AstExpr** base = determinable_list->data();
            int rest = j;
            while (int half = rest >> 1) {
                solveAndCheckNil(base[half])
                base = is_nil ? base : base + half;
                rest -= half;
            }

            solveAndCheckNil(*base);

            int boundary = !is_nil + int(base - determinable_list->data());
            return boundary;

            #undef solveAndCheckNil
        }
    }

    return j;
}

enum LuaCallArgs {
    InvalidFunction,
    Number,
    String
};
LuaCallArgs getLuaCallArgs(const char* global, const char* global_index = "") {
    if (strcmp(global, "math") == 0 || strcmp(global, "bit32") == 0)
        return Number;

    if (strcmp(global, "string") == 0 && strcmp(global_index, "unpack") == 0)
        return String;

    return InvalidFunction;
}

std::optional<double> AstSimplifier::callLuaFunction(AstArray<AstExpr*> args, const char* global, const char* global_index, simplifyHook hook, void* hook_data) {
    std::optional<double> result = std::nullopt;

    auto lua_call_args = getLuaCallArgs(global, global_index);
    if (lua_call_args == InvalidFunction)
        return result;

    lua_State* L = luaL_newstate();
    luaopen_base(L);
    luaopen_string(L);
    luaopen_math(L);
    luaopen_bit32(L);
    lua_pop(L, lua_gettop(L));

    lua_getglobal(L, "print"); // error handler

    // push target function
    lua_getglobal(L, global);
    if (global_index)
        lua_getfield(L, -1, global_index);

    for (size_t i = 0; i < args.size; i++) {
        auto arg = args.data[i];
        SimplifyResult arg_simplified = simplify(arg, hook, hook_data);
        auto arg_as_number = arg_simplified.asNumber();
        auto arg_as_string = arg_simplified.asString();
        switch (lua_call_args) {
            case InvalidFunction:
                break;
            case Number:
                if (!arg_as_number)
                    goto RET;
                lua_pushnumber(L, arg_as_number.value());
                break;
            case String:
                if (!arg_as_string)
                    goto RET;
                lua_pushlstring(L, arg_as_string->data, arg_as_string->size);
                break;
        }
    }

    {

    int r = lua_pcall(L, args.size, 1, 1);

    if (r == 0) {
        result = lua_tonumber(L, -1);
        lua_pop(L, 1);
    }

    }

RET:
    lua_pop(L, lua_gettop(L));
    lua_close(L);

    return result;
}

std::optional<SimplifyResult> AstSimplifier::tryReplaceLuaCall(AstExprCall* expr_call, bool group, simplifyHook hook, void* hook_data) {
    if (!simplify_lua_calls)
        goto RET;

    {

    auto func_expr_as_index_name = simplifyToExpr(expr_call->func)->as<AstExprIndexName>();
    if (!func_expr_as_index_name)
        goto RET;

    auto expr_as_global = getRootExpr(func_expr_as_index_name->expr)->as<AstExprGlobal>();
    if (!expr_as_global)
        goto RET;

    auto global = expr_as_global->name.value;
    auto index = func_expr_as_index_name->index.value;

    auto result = callLuaFunction(expr_call->args, global, index, hook, hook_data);
    if (!result)
        goto RET;

    return SimplifyResult(this, result.value(), group);

    }

RET:
    return std::nullopt;
}

std::optional<SimplifyResult> AstSimplifier::tryReplaceRecordTableIndex(AstExprIndexName* expr_index_name, bool group, simplifyHook hook, void* hook_data) {
    if (!record_table_replace_visitor)
        goto RET;

    {

    auto expr_as_local = getRootExpr(expr_index_name->expr)->as<AstExprLocal>();
    if (!(expr_as_local && expr_as_local->local == record_table_replace_visitor->arg_local))
        goto RET;

    if (record_table_replace_visitor->key_to_expr_map.find(expr_index_name->index.value) == record_table_replace_visitor->key_to_expr_map.end())
        goto RET;

    return simplify(record_table_replace_visitor->key_to_expr_map.at(expr_index_name->index.value), hook, hook_data);

    }
RET:
    return std::nullopt;
}

std::optional<SimplifyResult> AstSimplifier::tryReplaceListTableIndex(AstExprIndexExpr* expr_index_expr, bool group, simplifyHook hook, void* hook_data) {
    if (!list_table_replace_visitor)
        goto RET;

    {

    auto expr_as_local = getRootExpr(expr_index_expr->expr)->as<AstExprLocal>();
    auto index_as_number_optional = simplify(expr_index_expr->index, hook, hook_data).asNumber();
    if (!(expr_as_local && expr_as_local->local == list_table_replace_visitor->arg_local && index_as_number_optional))
        goto RET;

    double index_as_number = index_as_number_optional.value();
    if (index_as_number < 1 || index_as_number > list_table_replace_visitor->index_to_expr_map.size())
        goto RET;

    return simplify(list_table_replace_visitor->index_to_expr_map.at(index_as_number), hook, hook_data);

    }

RET:
    return std::nullopt;
}

std::optional<SimplifyResult> AstSimplifier::tryReplaceSimpleAnonymousFunctionCall(AstExprCall* expr_call, bool group, simplifyHook hook, void* hook_data) {
    auto func = simplify(getRootExpr(expr_call->func, false), hook, hook_data).toExpr()->as<AstExprFunction>();
    if (!func)
        goto RET;

    {

    auto func_body = func->body->body;
    if (func_body.size == 1) {
        if (auto stat_as_return = func_body.data[0]->as<AstStatReturn>()) {
            auto determinable_list = getDeterminableList(stat_as_return->list);
            if (!determinable_list)
                goto RET;

            size_t count = determinable_list->size();
            if (count == 0)
                return simplify(allocator.alloc<AstExprConstantNil>(Location()));

            if (count != 1)
                goto RET;

            return simplify(determinable_list.value()[0], hook, hook_data);
        }
    }

    }
RET:
    return std::nullopt;
}

std::optional<AstExprBinary::Op> inverseBinaryOp(AstExprBinary::Op op) {
    switch (op) {
        case AstExprBinary::CompareNe:
            return AstExprBinary::CompareEq;
        case AstExprBinary::CompareEq:
            return AstExprBinary::CompareNe;
        case AstExprBinary::CompareLt:
            return AstExprBinary::CompareGe;
        case AstExprBinary::CompareLe:
            return AstExprBinary::CompareGt;
        case AstExprBinary::CompareGt:
            return AstExprBinary::CompareLe;
        case AstExprBinary::CompareGe:
            return AstExprBinary::CompareLt;
        default:
            break;
    }
    return std::nullopt;
}

SimplifyResult AstSimplifier::simplify(AstExpr* expr, simplifyHook hook, void* hook_data) {
    // TODO: maybe this should be a pointer passed to getrootexpr that will become true if it encounters a group, instead of just checking if the outermost expr is group
    bool group = expr->is<AstExprGroup>();

    if (disabled)
        goto RET;

    expr = getRootExpr(expr, safe);

    if (expr->is<AstExprGlobal>() || expr->is<AstExprLocal>())
        group = false;

    if (hook) {
        auto result = hook(this, allocator, expr, group, hook_data);
        if (result)
            return result.value();
    }

    #define try(func, expr) {\
        auto result = func(expr, group, hook, hook_data); \
        if (result) \
            return result.value(); \
    }

    if (auto expr_constant_nil = expr->as<AstExprConstantNil>()) {
        return SimplifyResult(this, expr_constant_nil, group);
    } else if (auto expr_constant_bool = expr->as<AstExprConstantBool>()) {
        return SimplifyResult(this, expr_constant_bool->value, group);
    } else if (auto expr_constant_number = expr->as<AstExprConstantNumber>()) {
        return SimplifyResult(this, expr_constant_number->value, group);
    } else if (auto expr_constant_string = expr->as<AstExprConstantString>()) {
        return SimplifyResult(this, expr_constant_string->value, group);
    } else if (auto expr_unary = expr->as<AstExprUnary>()) {
        auto expr_simplified = simplify(expr_unary->expr, hook, hook_data);
        switch (expr_unary->op) {
            case AstExprUnary::Not: {
                switch (expr_simplified.type) {
                    case SimplifyResult::Nil:
                        return SimplifyResult(this, true, group);
                    case SimplifyResult::Bool:
                        return SimplifyResult(this, !expr_simplified.bool_value, group);
                    case SimplifyResult::Number:
                    case SimplifyResult::String:
                        return SimplifyResult(this, false, group);
                    case SimplifyResult::Other: {
                        auto is_truthy = isExpressionTruthy(expr_simplified.other_value, assume_globals);
                        if (is_truthy < 2)
                            return SimplifyResult(this, !is_truthy, group);
                        else if (auto expr_binary = expr_simplified.toExpr()->as<AstExprBinary>()) {
                            auto new_op = inverseBinaryOp(expr_binary->op);
                            if (!new_op)
                                goto UNARY_OTHER_CASE_EXIT;
                            return SimplifyResult(this, allocator.alloc<AstExprBinary>(expr_binary->location, new_op.value(), expr_binary->left, expr_binary->right));
                        }
                    UNARY_OTHER_CASE_EXIT:
                        break;
                    }
                }
                break;
            }
            case AstExprUnary::Minus:
                if (expr_simplified.type == SimplifyResult::Number)
                    return SimplifyResult(this, -expr_simplified.number_value, group);
                break;
            case AstExprUnary::Len:
                switch (expr_simplified.type) {
                    case SimplifyResult::String:
                        return SimplifyResult(this, (double) expr_simplified.string_value.size, group);
                    case SimplifyResult::Other:
                        if (auto expr_table = expr_simplified.other_value->as<AstExprTable>()) {
                            auto size = getTableSize(expr_table);
                            if (size.has_value())
                                return SimplifyResult(this, (double) size.value(), group);
                        }
                        break;
                    default:
                        break;
                }
                break;
        }
    } else if (auto expr_binary = expr->as<AstExprBinary>()) {
        auto left_simplified = simplify(expr_binary->left, hook, hook_data);
        auto right_simplified = simplify(expr_binary->right, hook, hook_data);

        auto left_type = left_simplified.type;
        auto right_type = right_simplified.type;

        double left_number = 0;
        double right_number = 0;
        bool left_number_valid = false;
        bool right_number_valid = false;

        {
            auto optional = left_simplified.asNumber();
            if (optional.has_value()) {
                left_number = optional.value();
                left_number_valid = true;
            }
            optional = right_simplified.asNumber();
            if (optional.has_value()) {
                right_number = optional.value();
                right_number_valid = true;
            }
        }

        AstArray<char> left_string{};
        AstArray<char> right_string{};
        bool left_string_valid = false;
        bool right_string_valid = false;

        {
            auto optional = left_simplified.asString();
            if (optional.has_value()) {
                left_string = optional.value();
                left_string_valid = true;
            }
            optional = right_simplified.asString();
            if (optional.has_value()) {
                right_string = optional.value();
                right_string_valid = true;
            }
        }

        const bool both_number_valid = left_number_valid && right_number_valid;
        const bool both_string_valid = left_string_valid && right_string_valid;

        // const bool left_is_other = left_type == SimplifyResult::Other;
        // const bool right_is_other = right_type == SimplifyResult::Other;

        const bool both_have_same_type = left_type == right_type;

        // const bool can_compare_equality = (!left_is_other && !right_is_other);
        const bool can_compare_equality = isExpressionTruthy(left_simplified.toExpr(), assume_globals) < 2 && isExpressionTruthy(right_simplified.toExpr(), assume_globals) < 2;

        #define genericNumberCase(op, value) case AstExprBinary::op: \
            if (both_number_valid) \
                return SimplifyResult(this, value, group); \
            break;
        #define simpleMathCase(op, operation) genericNumberCase(op, left_number operation right_number)

        auto& binary_op = expr_binary->op;
        switch (binary_op) {
            simpleMathCase(Add, +)
            simpleMathCase(Sub, -)
            simpleMathCase(Mul, *)
            simpleMathCase(Div, /)

            genericNumberCase(FloorDiv, floor(left_number / right_number))
            genericNumberCase(Mod, left_number - floor(left_number / right_number) * right_number)
            genericNumberCase(Pow, pow(left_number, right_number))

            case AstExprBinary::Concat:
                if (both_string_valid) {
                    auto vector = std::vector<char>();
                    vector.reserve(left_string.size + right_string.size);

                    vector.insert(vector.end(), left_string.begin(), left_string.end());
                    vector.insert(vector.end(), right_string.begin(), right_string.end());

                    return SimplifyResult(this, copy(allocator, vector.data(), vector.size()), group);
                }
                break;

            case AstExprBinary::CompareNe:
            case AstExprBinary::CompareEq:
            case AstExprBinary::CompareLt:
            case AstExprBinary::CompareLe:
            case AstExprBinary::CompareGt:
            case AstExprBinary::CompareGe: {
                uint8_t equality = 2; // 2 means don't return anything; otherwise it should be 0 for false and 1 for true

                if (both_have_same_type) {
                    switch (left_type) {
                        case SimplifyResult::Nil:
                            if (binary_op == Luau::AstExprBinary::CompareNe)
                                equality = false;
                            else if (binary_op == AstExprBinary::CompareEq)
                                equality = true;
                            break;
                        case SimplifyResult::Bool:
                            equality = binary_compare_bool_list[binary_op - AstExprBinary::CompareNe](left_simplified.bool_value, right_simplified.bool_value);
                            break;
                        case SimplifyResult::Number:
                            equality = binary_compare_number_list[binary_op - AstExprBinary::CompareNe](left_simplified.number_value, right_simplified.number_value);
                            break;
                        case SimplifyResult::String:
                            equality = binary_compare_string_list[binary_op - AstExprBinary::CompareNe](left_simplified.string_value, right_simplified.string_value);
                            break;
                        default:
                            break;
                    }
                } else if (can_compare_equality) {
                    if (binary_op == Luau::AstExprBinary::CompareNe)
                        equality = true;
                    else if (binary_op == AstExprBinary::CompareEq)
                        equality = false;
                }

                if (equality < 2) {
                    return SimplifyResult(this, (bool) equality, group);
                }
                break;
            }

            case AstExprBinary::And:
                if (can_compare_equality) {
                    int left_truthy = isExpressionTruthy(left_simplified.toExpr(), assume_globals);
                    if (left_truthy < 2)
                        return left_truthy ? right_simplified : left_simplified;
                }
                break;
            case AstExprBinary::Or:
                if (can_compare_equality) {
                    int left_truthy = isExpressionTruthy(left_simplified.toExpr(), assume_globals);
                    if (left_truthy < 2)
                        return left_truthy ? left_simplified : right_simplified;
                }
                break;

            case AstExprBinary::Op__Count:
                break;
        }

        #undef simpleMathCase
        #undef genericNumberCase
    } else if (auto expr_call = expr->as<AstExprCall>()) {
        try(tryReplaceLuaCall, expr_call)
        try(tryReplaceSimpleAnonymousFunctionCall, expr_call)
    } else if (auto expr_index_name = expr->as<AstExprIndexName>()) {
        try(tryReplaceRecordTableIndex, expr_index_name)
    } else if (auto expr_index_expr = expr->as<AstExprIndexExpr>()) {
        try(tryReplaceListTableIndex, expr_index_expr)
    }

    #undef try

RET:
    return SimplifyResult(this, expr, group);
}

AstExpr* AstSimplifier::simplifyToExpr(AstExpr* expr, simplifyHook hook, void* hook_data) {
    return simplify(expr, hook, hook_data).toExpr();
}

}; // namespace LuauFormat
