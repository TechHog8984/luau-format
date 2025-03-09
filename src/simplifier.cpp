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
uint8_t isExpressionTruthy(AstExpr* expr) {
    expr = getRootExpr(expr, false);

    if (expr->is<AstExprConstantNil>())
        return 0;
    else if (auto expr_constant_bool = expr->as<AstExprConstantBool>())
        return expr_constant_bool->value;
    else if (expr->is<AstExprConstantNumber>() || expr->is<AstExprConstantString>() || expr->is<AstExprFunction>() || expr->is<AstExprTable>() || expr->is<AstExprInterpString>())
        return 1;

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

        return SimpleAssign{
            .var = var->local->name.value,
            .var_local = var->local,
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

        return SimpleAssign{
            .var = var_list.data[0]->name.value,
            .var_local = nullptr,
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
            result = left >= right;
            break;
        case AstExprBinary::CompareGe:
            result = left > right;
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

LPHControlFlowVisitor::LPHControlFlowVisitor(Allocator& allocator, AstSimplifier& simplifier) :
    allocator(allocator), simplifier(simplifier) {}

std::optional<SimplifyResult> lphControlFlowSimplifyHook(AstSimplifier* simplifier, Allocator& allocator, AstExpr* root_expr, bool group, void* data);
std::optional<SimplifyResult> lphControlFlowSimplifyHook(AstSimplifier* simplifier, Allocator& allocator, AstExpr* root_expr, bool group, void* data) {
    if (!data)
        goto RET;

    {

    auto visitor = static_cast<LPHControlFlowSolver*>(data);
    if (!visitor)
        goto RET;

    if (auto root_expr_as_index_expr = root_expr->as<AstExprIndexExpr>()) {
        auto expr_as_local = getRootExpr(root_expr_as_index_expr->expr)->as<AstExprLocal>();
        if (!expr_as_local)
            goto RET;

        if (expr_as_local->local != visitor->cftable_local)
            goto RET;

        auto index_as_number_optional = simplifier->simplify(root_expr_as_index_expr->index).asNumber();
        if (!index_as_number_optional)
            goto RET;

        double index_as_number = index_as_number_optional.value();

        if (visitor->cftable.find(index_as_number) == visitor->cftable.end())
            goto RET;

        // printf("solved cftable[%.f]: %.f\n", index_as_number, visitor->cftable.at(index_as_number));

        return SimplifyResult(simplifier, visitor->cftable.at(index_as_number), group);
    } else if (auto expr_as_binary = root_expr->as<AstExprBinary>()) {
        AstExpr* left = simplifier->simplifyToExpr(expr_as_binary->left, lphControlFlowSimplifyHook, visitor);
        AstExpr* right = simplifier->simplifyToExpr(expr_as_binary->right, lphControlFlowSimplifyHook, visitor);

        bool updated = false;

        auto left_as_local = left->as<AstExprLocal>();
        if (left_as_local && left_as_local->local == visitor->cfposition_local) {
            updated = true;
            left = allocator.alloc<AstExprConstantNumber>(Location(), visitor->cfposition);
        }

        auto right_as_local = right->as<AstExprLocal>();
        if (right_as_local && right_as_local->local == visitor->cfposition_local) {
            updated = true;
            right = allocator.alloc<AstExprConstantNumber>(Location(), visitor->cfposition);
        }

        if (updated)
            return simplifier->simplify(allocator.alloc<AstExprBinary>(Location(), expr_as_binary->op, left, right), lphControlFlowSimplifyHook, visitor);
    }

    }

RET:
    return std::nullopt;
}

void LPHControlFlowSolver::tryHandleSet_cftable(AstStat* stat) {
    auto stat_as_assign = stat->as<AstStatAssign>();
    if (!stat_as_assign)
        return;

    auto& var_list = stat_as_assign->vars;
    auto& value_list = stat_as_assign->values;

    if (var_list.size != 1 || value_list.size != 1)
        return;

    auto var_as_index_expr = getRootExpr(var_list.data[0])->as<AstExprIndexExpr>();
    if (!var_as_index_expr)
        return;

    auto expr_as_local = getRootExpr(var_as_index_expr->expr)->as<AstExprLocal>();
    if (!expr_as_local)
        return;

    if (expr_as_local->local != cftable_local)
        return;

    auto index_as_number = simplifier.simplify(var_as_index_expr->index).asNumber();
    if (!index_as_number)
        return;

    auto value_as_number = simplifier.simplify(value_list.data[0], lphControlFlowSimplifyHook, this).asNumber();
    if (!value_as_number) {
        auto value_as_local = getRootExpr(value_list.data[0])->as<AstExprLocal>();
        if (!value_as_local)
            return;
        if (value_as_local->local != cfposition_local)
            return;
        value_as_number.emplace(cfposition);
    }

    cftable.emplace(index_as_number.value(), value_as_number.value());
}
void LPHControlFlowSolver::tryHandleSet_cfposition(AstStat* stat) {
    auto stat_simple_assignment = getSimpleAssign(stat);
    if (!stat_simple_assignment)
        return;

    if (!stat_simple_assignment->var_local || stat_simple_assignment->var_local != cfposition_local)
        return;

    auto value_as_number = simplifier.simplify(stat_simple_assignment->value, lphControlFlowSimplifyHook, this).asNumber();
    if (!value_as_number)
        return;

    cfposition = value_as_number.value();
    cfposition_set_order.push_back(cfposition);
}

int LPHControlFlowSolver::handleIfBlock(AstStatBlock* main_block) {
    auto& root_block_body = main_block->body;
    if (root_block_body.size == 1) {
        auto inner_if = root_block_body.data[0]->as<AstStatIf>();
        if (!inner_if) goto FALLTHROUGH;

        auto inner_condition = Condition::tryCreateCondition(simplifier, inner_if->condition->as<AstExprBinary>(), cfposition_local);
        if (!inner_condition) goto FALLTHROUGH;

        return handleIf(inner_if, inner_condition.value().passes(cfposition));
    }

FALLTHROUGH:
    // return mapIndexToOp(main_block);

    auto& main_block_body = main_block->body;

    AstStatIf* main_block_target_stat_as_if = nullptr;
    size_t main_block_target_stat_as_if_index = -1;
    for (size_t i = main_block_body.size; i > 0;) {
        if (auto stat_if = main_block_body.data[--i]->as<AstStatIf>()) {
            main_block_target_stat_as_if = stat_if;
            main_block_target_stat_as_if_index = i;
            break;
        }
    }

    double current_cfposition = cfposition;

    if (cfposition_to_block_map.find(current_cfposition) != cfposition_to_block_map.end())
        return 1;

    if (!main_block_target_stat_as_if)
        goto FALLTHROUGH2;

    if (!main_block_target_stat_as_if->elsebody)
        goto FALLTHROUGH2;

    {

    auto main_block_target_stat_as_if_else = main_block_target_stat_as_if->elsebody->as<AstStatBlock>();
    if (!main_block_target_stat_as_if_else)
        goto FALLTHROUGH2;

    auto condition_as_unary = getRootExpr(main_block_target_stat_as_if->condition)->as<AstExprUnary>();
    if (!condition_as_unary)
        goto FALLTHROUGH2;

    bool double_unary = false;
    AstExpr* condition_root = nullptr;

    {

    condition_root = getRootExpr(condition_as_unary->expr);
    if (auto condition_root_as_unary = condition_root->as<AstExprUnary>()) {
        double_unary = true;
        condition_root = condition_root_as_unary->expr;
    }

    }

    auto condition_root_as_index_expr = getRootExpr(condition_root)->as<AstExprIndexExpr>();
    if (!condition_root_as_index_expr)
        goto FALLTHROUGH2;

    auto condition_index_as_local = getRootExpr(condition_root_as_index_expr->expr)->as<AstExprLocal>();
    if (!condition_index_as_local)
        goto FALLTHROUGH2;

    if (condition_index_as_local->local != cftable_local)
        goto FALLTHROUGH2;

    auto& target_branch_body = double_unary ? main_block_target_stat_as_if_else->body : main_block_target_stat_as_if->thenbody->body;

    for (size_t i = 0; i < target_branch_body.size; i++) {
        auto stat = target_branch_body.data[i];
        tryHandleSet_cfposition(stat);
        tryHandleSet_cftable(stat);
    }

    if (main_block_target_stat_as_if_index) {
        std::vector<AstStat*> new_main_block_list;
        new_main_block_list.reserve(main_block_target_stat_as_if_index);

        for (size_t i = 0; i < main_block_target_stat_as_if_index; i++) {
            new_main_block_list.push_back(main_block_body.data[i]);
        }

        main_block = allocator.alloc<AstStatBlock>(Location(), copy(allocator, new_main_block_list.data(), new_main_block_list.size()));
    } else {
        AstArray<AstStat*> ast_array;
        ast_array.data = nullptr;
        ast_array.size = 0;
        main_block = allocator.alloc<AstStatBlock>(Location(), ast_array);
    }

    }

FALLTHROUGH2:

    cfposition_to_block_map.emplace(current_cfposition, main_block);
    return 0;
}

int LPHControlFlowSolver::handleIf(AstStatIf* main_if, bool passes) {
    if (passes)
        return handleIfBlock(main_if->thenbody);

    auto else_body = main_if->elsebody;
    if (!else_body)
        // return setErrorMessage("[handleIf] condition didn't pass, but there was no else body");
        return 1;

    AstStatBlock* else_body_as_block = else_body->as<AstStatBlock>();
    if (auto else_body_as_if = else_body->as<AstStatIf>()) {
        auto condition_as_binary = else_body_as_if->condition->as<AstExprBinary>();
        if (!condition_as_binary)
            goto ELSEIF_FALLTHROUGH;

        {

        auto inner_condition = Condition::tryCreateCondition(simplifier, condition_as_binary, cfposition_local);

        if (!inner_condition) {
            // auto left_condition = Condition::tryCreateCondition(
            //     simplifier,
            //     condition_as_binary->left->as<AstExprBinary>(),
            //     cfposition_local
            // );
            // auto right_condition = Condition::tryCreateCondition(
            //     simplifier,
            //     condition_as_binary->right->as<AstExprBinary>(),
            //     cfposition_local
            // );

            // if (!left_condition || !right_condition)
            //     goto ELSEIF_FALLTHROUGH;

            // return handlfi
            // TODO: check left to be binary with left/right of valid condition and left/right of cfposition_local
            goto ELSEIF_FALLTHROUGH;
        }

        if (!inner_condition) {
            goto ELSEIF_FALLTHROUGH;
        }

        return handleIf(else_body_as_if, inner_condition.value().passes(cfposition));

        }

    ELSEIF_FALLTHROUGH:
        else_body_as_block = allocateBlockFromSingleStat(allocator, else_body_as_if);
        goto ELSE_FALLTHROUGH;
    } else if (!else_body_as_block)
        // return setErrorMessage("[handleIf] Luau error: if else body was neither block nor if");
        return 1;

ELSE_FALLTHROUGH:
    return handleIfBlock(else_body_as_block);
}

typedef struct {
    AstArray<AstStat*> body;
} InfiniteLoop;
std::optional<InfiniteLoop> getInfiniteLoop(AstSimplifier& simplifier, AstStat* main_stat) {
    std::optional<InfiniteLoop> result = std::nullopt;

    if (auto main_stat_as_while = main_stat->as<AstStatWhile>()) {
        if (simplifier.simplify(main_stat_as_while->condition).isTruthy() != 1)
            goto RET;
        result = std::make_optional(InfiniteLoop{ .body = main_stat_as_while->body->body });
    } else if (auto main_stat_as_repeat = main_stat->as<AstStatRepeat>()) {
        if (simplifier.simplify(main_stat_as_repeat->condition).isTruthy() != 0)
            goto RET;
        result = std::make_optional(InfiniteLoop{ .body = main_stat_as_repeat->body->body });
    }

RET:
    return result;
}

bool LPHControlFlowVisitor::visit(AstStatBlock* root_block) {
    auto& root_block_body = root_block->body;
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

    auto root_func = getRootExpr(inner_call->func)->as<AstExprFunction>();
    if (!root_func)
        goto RET;

    auto& root_func_body = root_func->body->body;
    if (root_func_body.size < 3)
        goto RET;

    auto first_stat_simple_assign = getSimpleAssign(root_func_body.data[0]);
    if (!first_stat_simple_assign)
        goto RET;

    auto cftable_expr = getRootExpr(first_stat_simple_assign->value)->as<AstExprTable>();
    if (!cftable_expr)
        goto RET;

    if (cftable_expr->items.size != 0)
        goto RET;

    AstLocal* cftable_local = first_stat_simple_assign->var_local;

    auto second_stat_as_local = root_func_body.data[1]->as<AstStatLocal>();
    if (!second_stat_as_local)
        goto RET;

    auto& second_stat_var_list = second_stat_as_local->vars;
    auto& second_stat_value_list = second_stat_as_local->values;

    double cfposition = -1;
    AstLocal* cfposition_local = nullptr;

    for (size_t i = 0; i < second_stat_var_list.size; i++) {
        if (second_stat_value_list.size <= i)
            goto RET;

        auto value_as_number = simplifier.simplify(second_stat_value_list.data[i]).asNumber();
        if (!value_as_number)
            continue;

        cfposition_local = second_stat_var_list.data[i];
        cfposition = value_as_number.value();
        break;
    }

    if (!cfposition_local)
        goto RET;

    std::unordered_map<double, double> cftable;

    for (size_t i = 2; i < root_func_body.size - 1; i++) {
        auto first_stat = root_func_body.data[i];
        auto second_stat = root_func_body.data[i + 1];

        auto first_simple_assign = getSimpleAssign(first_stat);
        if (!first_simple_assign) {
            if (i == 2) {
                second_stat = first_stat;
                goto AFTER_FIRST_VERIFICATION;
            }
            continue;
        }

        if (first_simple_assign->var_local != cfposition_local)
            continue;

        {

        auto first_simple_assign_as_number = simplifier.simplify(first_simple_assign->value).asNumber();
        if (!first_simple_assign_as_number)
            continue;

        cfposition = first_simple_assign_as_number.value();

        }

    AFTER_FIRST_VERIFICATION:

        auto second_stat_loop = getInfiniteLoop(simplifier, second_stat);
        if (!second_stat_loop)
            continue;

        LPHControlFlowSolver inner_visitor(allocator, simplifier, cftable, cfposition, cftable_local, cfposition_local);
        inner_visitor.handleLoopBody(second_stat, second_stat_loop->body);
        i++;
    }

    // TODO: have a loop through root body; find valid while / repeats that have an assign to cfposition (we will use that new cf position and pass to constructor)

    success = true;

    }

RET:
    return false;
}

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
            char* allocated = static_cast<char*>(simplifier->getAllocator().allocate(str.size()));
            strcpy(allocated, str.data());
            result.data = allocated;
            result.size = str.size();
            return result;
        }
        default:
            return std::nullopt;
    }
}
uint8_t SimplifyResult::isTruthy() {
    return isExpressionTruthy(toExpr());
}

AstSimplifier::AstSimplifier(Allocator& allocator, bool safe, bool disabled, bool simplify_lua_calls) :
    allocator(allocator), safe(safe), disabled(disabled), simplify_lua_calls(simplify_lua_calls) {}

Allocator& AstSimplifier::getAllocator() {
    return allocator;
}

std::optional<std::vector<AstExpr*>> AstSimplifier::getDeterminableList(std::vector<AstExpr*> list) {
    size_t list_size = list.size();
    std::vector<AstExpr*> determinable_list;

    for (size_t i = 0; i < list_size; i++) {
        AstExpr* value = simplifyToExpr(list.at(i));

        if (isExpressionTruthy(value) < 2) // if a value can be determined as truthy, we consider it "determinable"
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
            }
        } else
            return std::nullopt;
    }

    return determinable_list;
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
                        auto is_truthy = isExpressionTruthy(expr_simplified.other_value);
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

        const bool left_is_other = left_type == SimplifyResult::Other;
        const bool right_is_other = right_type == SimplifyResult::Other;

        const bool both_have_same_type = left_type == right_type;

        const bool can_compare_equality = (!left_is_other && !right_is_other);

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
                    int left_truthy = isExpressionTruthy(left_simplified.toExpr());
                    if (left_truthy < 2)
                        return left_truthy ? right_simplified : left_simplified;
                }
                break;
            case AstExprBinary::Or:
                if (can_compare_equality) {
                    int left_truthy = isExpressionTruthy(left_simplified.toExpr());
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
