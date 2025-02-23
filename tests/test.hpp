#include "Luau/Lexer.h"

using namespace Luau;

/* TODO: each test should have a designated code snippet along with an expected result value
    example: snippet("true == true", "true")
    then for each snippet we employ Luau code such as
    ```lua
    assert((CODE_SNIPPET) == EXPECTED_RESULT_VALUE)
    ```
    then we will run before and after format and ensure no errors occur

    we also could have a lua state where take Luau code such as
    ```lua
    return CODE_SNIPPET
    ```
    then run it, get return value, do the same after format, and finally compare
*/

// defining tests
#define startTest(name) auto test_##name = [&state, &failed, &allocator, &simplifier, &location]{ state.test_count++; state.current_test = #name;
#define endTest(name) state.passed_count++; \
    printf("passed '%s'\n", state.current_test); \
    }; \
    test_##name(); \
    clearContext();

// for use in test bodies
#define __fail_header(message) { failed++; \
    fprintf(stderr, "[test failed]: " message "\n", state.context);
#ifdef FAILTESTS
#define fail(message) __fail_header(message) }
#define expect(condition, message) if (!!!!condition) fail(message)
#else
#define fail(message) __fail_header(message) \
    return; \
}
#define expect(condition, message) if (!!!condition) fail(message)
#endif
#define equals(left, right) (left == right)

// context
#define setContext(desired_context) state.context = desired_context;
#define clearContext() state.context = "";

AstArray<char> cstringToAstCharArray(const char* cstring);

typedef struct {
    Allocator& allocator;
    const char* current_test;
    uint8_t test_count;
    uint8_t passed_count;
    const char* context;

    AstExpr* expr_constant_nil;

    AstExpr* expr_constant_bool_true;
    AstExpr* expr_constant_bool_false;

    AstExpr* expr_constant_number_100;
    AstExpr* expr_constant_number_negative_100;

    AstExpr* expr_constant_string_foo;
    AstExpr* expr_constant_string_bar;
    AstExpr* expr_constant_string_foo_zero_bytes_bar;
    AstExpr* expr_constant_string_100;

    AstExpr* expr_local;
    AstExpr* expr_global;

    AstExpr* expr_varargs;

    AstExpr* expr_call;

    AstExpr* expr_index_name;
    AstExpr* expr_index_expr;

    AstExpr* expr_function;

    AstExpr* expr_table;

    AstExpr* expr_unary;
    AstExpr* expr_binary;

    AstExpr* expr_if_else;

    AstExpr* expr_interp_string;
} TestState;

uint8_t testSimplifier(TestState& state);