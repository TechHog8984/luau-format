#include <cstdio>

#include "test.hpp"
#include "formatter.hpp"

AstArray<char> cstringToAstCharArray(const char* cstring) {
    AstArray<char> string;
    string.data = const_cast<char*>(cstring);
    string.size = strlen(cstring);
    return string;
}

template<typename T>
AstArray<T> createEmptyAstArray() {
    AstArray<T> ast_array;
    ast_array.data = nullptr;
    ast_array.size = 0;
    return ast_array;
}

#define runTest(test) failed_count += test(state)
int main() {
    const auto location = Location();
    const auto position = location.begin; // 0, 0

    Allocator allocator;

    auto foo_zero_bytes_bar_vector = std::vector<char>() = { 'f', 'o', 'o', '\0', '\0', 'b', 'a', 'r' };
    auto foo_zero_bytes_bar_char_array = copy(allocator, foo_zero_bytes_bar_vector.data(), foo_zero_bytes_bar_vector.size());

    AstExprLocal* expr_local = allocator.alloc<AstExprLocal>(location, allocator.alloc<AstLocal>(AstName("local_foo"), location, nullptr, 0, 0, nullptr), false);
    AstExprGlobal* expr_global = allocator.alloc<AstExprGlobal>(location, AstName("global_bar"));

    AstExprFunction* expr_function = allocator.alloc<AstExprFunction>(
        location,
        createEmptyAstArray<AstAttr*>(),
        createEmptyAstArray<AstGenericType>(),
        createEmptyAstArray<AstGenericTypePack>(),
        allocator.alloc<AstLocal>(AstName("func"), location, nullptr, 0, 0, nullptr),
        createEmptyAstArray<AstLocal*>(),
        false,
        location,
        allocator.alloc<AstStatBlock>(location, createEmptyAstArray<AstStat*>(), true),
        0,
        AstName("func")
    );
    AstExprUnary* expr_unary = allocator.alloc<AstExprUnary>(location, AstExprUnary::Not, expr_local);

    auto expr_array_vector = std::vector<AstExpr*>() = { expr_local, expr_global, };
    auto ast_array_expr = copy(allocator, expr_array_vector.data(), expr_array_vector.size());

    auto string_array_vector = std::vector<AstArray<char>>() = { cstringToAstCharArray("foo"), cstringToAstCharArray("bar") };
    auto ast_array_string = copy(allocator, string_array_vector.data(), string_array_vector.size());

    TestState state{
        .allocator = allocator, .current_test = nullptr, .test_count = 0, .passed_count = 0, .context = "",

        .expr_constant_nil = allocator.alloc<AstExprConstantNil>(location),

        .expr_constant_bool_true = allocator.alloc<AstExprConstantBool>(location, true),
        .expr_constant_bool_false = allocator.alloc<AstExprConstantBool>(location, false),

        .expr_constant_number_100 = allocator.alloc<AstExprConstantNumber>(location, 100),
        .expr_constant_number_negative_100 = allocator.alloc<AstExprConstantNumber>(location, -100),

        .expr_constant_string_foo = allocator.alloc<AstExprConstantString>(location, cstringToAstCharArray("foo")),
        .expr_constant_string_bar = allocator.alloc<AstExprConstantString>(location, cstringToAstCharArray("bar")),
        .expr_constant_string_foo_zero_bytes_bar = allocator.alloc<AstExprConstantString>(location, foo_zero_bytes_bar_char_array),
        .expr_constant_string_100 = allocator.alloc<AstExprConstantString>(location, cstringToAstCharArray("100")),

        .expr_local = expr_local,
        .expr_global = expr_global,

        .expr_varargs = allocator.alloc<AstExprVarargs>(location),

        .expr_call = allocator.alloc<AstExprCall>(location, expr_function, ast_array_expr, false, location),

        .expr_index_name = allocator.alloc<AstExprIndexName>(location, expr_local, AstName("index_foo"), location, position, '.'),
        .expr_index_expr = allocator.alloc<AstExprIndexExpr>(location, expr_local, expr_global),

        .expr_function = expr_function,

        .expr_table = allocator.alloc<AstExprTable>(location, createEmptyAstArray<AstExprTable::Item>()),

        .expr_unary = expr_unary,
        .expr_binary = allocator.alloc<AstExprBinary>(location, AstExprBinary::And, expr_local, expr_global),

        .expr_if_else = allocator.alloc<AstExprIfElse>(location, expr_unary, true, expr_global, true, expr_local),

        .expr_interp_string = allocator.alloc<AstExprInterpString>(location, ast_array_string, ast_array_expr),
    };

    uint8_t failed_count = 0;

    // run tests
    puts("RUNNING TESTS");
    runTest(testSimplifier);

    // display information
    auto test_count = state.test_count;
    auto passed_count = state.passed_count;

    if (failed_count + passed_count != test_count)
        fprintf(stderr, "the amount of failed tests (%d) + passed tests (%d) does not match total amount of tests (%d)\n", failed_count, passed_count, test_count);

    if (failed_count) {
        fprintf(stderr, "FAILED %d/%d TESTS\n", failed_count, test_count);
        return 1;
    }

    if (passed_count != test_count)
        fprintf(stderr, "no tests failed but the amount of passed tests (%d) does not match the total amount of tests (%d)\n", passed_count, test_count);

    printf("PASSED %d/%d TESTS\n", passed_count, test_count);

    return 0;
}