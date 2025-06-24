#pragma once

#include <cstdint>
#include <unordered_map>

#include "Luau/Ast.h"
#include "Luau/Lexer.h"
#include "formatter.hpp"

using namespace Luau;

namespace LuauFormat {

AstExpr* getRootExpr(AstExpr* expr, bool safe = false);
uint8_t isExpressionTruthy(AstExpr* expr, bool allow_globals = false);
std::string convertNumber(double value);
std::string fixString(AstArray<char> value);
bool equalsCharArray(const AstArray<char> left, const AstArray<char> right);
int charArrayCmp(const AstArray<char> left, const AstArray<char> right);

// for binaryCompare functions, 2 means don't return anything; otherwise 0 means false and 1 means true

uint8_t binaryCompareBool_ne(bool left, bool right);
uint8_t binaryCompareNumber_ne(double left, double right);
uint8_t binaryCompareString_ne(const AstArray<char> left, const AstArray<char> right);

uint8_t binaryCompareBool_eq(bool left, bool right);
uint8_t binaryCompareNumber_eq(double left, double right);
uint8_t binaryCompareString_eq(const AstArray<char> left, const AstArray<char> right);

uint8_t binaryCompareBool_lt(bool left, bool right);
uint8_t binaryCompareNumber_lt(double left, double right);
uint8_t binaryCompareString_lt(const AstArray<char> left, const AstArray<char> right);

uint8_t binaryCompareBool_le(bool left, bool right);
uint8_t binaryCompareNumber_le(double left, double right);
uint8_t binaryCompareString_le(const AstArray<char> left, const AstArray<char> right);

uint8_t binaryCompareBool_gt(bool left, bool right);
uint8_t binaryCompareNumber_gt(double left, double right);
uint8_t binaryCompareString_gt(const AstArray<char> left, const AstArray<char> right);

uint8_t binaryCompareBool_ge(bool left, bool right);
uint8_t binaryCompareNumber_ge(double left, double right);
uint8_t binaryCompareString_ge(const AstArray<char> left, const AstArray<char> right);

typedef uint8_t (*binaryCompareBool)(bool, bool);
typedef uint8_t (*binaryCompareNumber)(double, double);
typedef uint8_t (*binaryCompareString)(AstArray<char>, AstArray<char>);

typedef struct {
    const char* var;
    AstLocal* var_local;
    AstExpr* value;
} SimpleAssign;

std::optional<SimpleAssign> getSimpleAssign(AstStat* stat, bool can_have_no_values = false);

class AstSimplifier;
class AstFormatter;

class Condition {
    AstExprBinary::Op op;
    AstLocal* local;
    double number;
    bool number_is_left;
    bool flipped;
public:
    static std::optional<Condition> tryCreateCondition(AstSimplifier& simplifier, AstExprBinary* expr_binary);
    static std::optional<Condition> tryCreateCondition(AstSimplifier& simplifier, AstExprBinary* expr_binary, const char* expected_local);
    static std::optional<Condition> tryCreateCondition(AstSimplifier& simplifier, AstExprBinary* expr_binary, AstLocal* expected_local);

    AstLocal* getLocal() {
        return local;
    }
    double getNumber() {
        return number;
    }

    // TODO: uint8_t behaviour similar to equality?
    bool passes(double input);

    Condition flip() {
        Condition result = *this;

        result.flipped = !flipped;

        return result;
    }
};

class RecordTableReplaceVisitor : public AstVisitor {
    AstSimplifier& simplifier;
public:
    bool success = false;

    std::unordered_map<std::string, AstExpr*> key_to_expr_map;
    AstLocal* arg_local = nullptr;

    RecordTableReplaceVisitor(AstSimplifier& simplifier);

    bool visit(AstStatBlock* root_block) override;
};
class ListTableReplaceVisitor : public AstVisitor {
    AstSimplifier& simplifier;
public:
    bool success = false;

    std::unordered_map<size_t, AstExpr*> index_to_expr_map;
    AstLocal* arg_local = nullptr;

    ListTableReplaceVisitor(AstSimplifier& simplifier);

    bool visit(AstStatBlock* root_block) override;
};

std::optional<AstExprBinary::Op> inverseBinaryOp(AstExprBinary::Op op);

class SimplifyResult {
    AstSimplifier* simplifier;
public:
    // NOTE: when adding a new type, remember that simplifying a Binary NE or EQ takes this into account
    enum Type {
        // simple types
        Nil,
        Bool,
        Number,
        String,
        // other type
        Other
    } type;
    AstExprConstantNil* nil_value;
    bool bool_value;
    double number_value;
    AstArray<char> string_value;
    AstExpr* other_value;

    bool group;

    SimplifyResult(AstSimplifier* simplifier, AstExprConstantNil* nil_value, bool group = false);
    SimplifyResult(AstSimplifier* simplifier, bool bool_value, bool group = false);
    SimplifyResult(AstSimplifier* simplifier, double number_value, bool group = false);
    SimplifyResult(AstSimplifier* simplifier, AstArray<char> string_value, bool group = false);
    SimplifyResult(AstSimplifier* simplifier, AstExpr* other_value, bool group = false);

    AstExpr* toExpr();
    std::optional<double> asNumber();
    std::optional<AstArray<char>> asString();
    uint8_t isTruthy();
};

class AstSimplifier {
    Allocator& allocator;
    bool safe;

    std::optional<std::vector<AstExpr*>> getDeterminableList(std::vector<AstExpr*> list);
    std::optional<std::vector<AstExpr*>> getDeterminableList(AstArray<AstExpr*> list);
    std::optional<size_t> getTableSize(AstExprTable* table);

    binaryCompareBool binary_compare_bool_list[6] = { binaryCompareBool_ne, binaryCompareBool_eq, binaryCompareBool_lt, binaryCompareBool_le, binaryCompareBool_gt, binaryCompareBool_ge };
    binaryCompareNumber binary_compare_number_list[6] = { binaryCompareNumber_ne, binaryCompareNumber_eq, binaryCompareNumber_lt, binaryCompareNumber_le, binaryCompareNumber_gt, binaryCompareNumber_ge };
    binaryCompareString binary_compare_string_list[6] = { binaryCompareString_ne, binaryCompareString_eq, binaryCompareString_lt, binaryCompareString_le, binaryCompareString_gt, binaryCompareString_ge };

public:
    bool disabled;
    bool simplify_lua_calls;
    bool assume_globals;

    RecordTableReplaceVisitor* record_table_replace_visitor = nullptr;
    ListTableReplaceVisitor* list_table_replace_visitor = nullptr;

    AstSimplifier(Allocator& allocator, bool safe = true, bool disabled = false, bool simplify_lua_calls = false, bool assume_globals = false);

    typedef std::optional<SimplifyResult> (*simplifyHook)(AstSimplifier*, Allocator&, AstExpr*, bool, void*);

    SimplifyResult simplify(AstExpr* expr, simplifyHook hook = nullptr, void* hook_data = nullptr);
    AstExpr* simplifyToExpr(AstExpr* expr, simplifyHook hook = nullptr, void* hook_data = nullptr);

    Allocator& getAllocator();

private:
    std::optional<double> callLuaFunction(AstArray<AstExpr*> args, const char* global, const char* global_index = nullptr, simplifyHook hook = nullptr, void* hook_data = nullptr);
    std::optional<SimplifyResult> tryReplaceLuaCall(AstExprCall* expr_call, bool group, simplifyHook hook = nullptr, void* hook_data = nullptr);

    std::optional<SimplifyResult> tryReplaceRecordTableIndex(AstExprIndexName* expr_index_name, bool group, simplifyHook hook = nullptr, void* hook_data = nullptr);
    std::optional<SimplifyResult> tryReplaceListTableIndex(AstExprIndexExpr* expr_index_expr, bool group, simplifyHook hook = nullptr, void* hook_data = nullptr);
    std::optional<SimplifyResult> tryReplaceSimpleAnonymousFunctionCall(AstExprCall* expr_call, bool group, simplifyHook hook = nullptr, void* hook_data = nullptr);
};

}; // namespace LuauFormat
