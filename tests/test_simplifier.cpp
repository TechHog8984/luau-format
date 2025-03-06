#include "Luau/Ast.h"
#include "formatter.hpp"
#include "test.hpp"
#include "simplifier.hpp"

uint8_t testSimplifier(TestState& state) {
    uint8_t failed = 0;

    Allocator& allocator = state.allocator;
    AstSimplifier simplifier(allocator, false);

    const Location location = Location();

    startTest(getRootExpr) {
        AstExprGroup* expr_group = allocator.alloc<AstExprGroup>(location,
            allocator.alloc<AstExprGroup>(location, state.expr_constant_bool_false)
        );
        auto root_expr = getRootExpr(expr_group);
        auto root_as_bool_expr = root_expr->as<AstExprConstantBool>();
        setContext("getRootExpr")
        expect(root_as_bool_expr, "%s did not return an expression with the same type as the root expression")
        expect(equals(root_as_bool_expr, state.expr_constant_bool_false), "%s did not return the root expression")
        expect(equals(root_as_bool_expr->value, false), "%s did not return the root expression")
    }
    endTest(getRootExpr)

    startTest(isExpressionTruthy) {
        setContext("constant nil")
        expect(equals(isExpressionTruthy(state.expr_constant_nil), 0), "isExpressionTruthy on a %s expression did not return 0")

        setContext("constant bool (true)")
        expect(equals(isExpressionTruthy(state.expr_constant_bool_true), 1), "isExpressionTruthy on a %s expression did not return 1")
        setContext("constant bool (false)")
        expect(equals(isExpressionTruthy(state.expr_constant_bool_false), 0), "isExpressionTruthy on a %s expression did not return 0")

        setContext("constant string ('foo')")
        expect(equals(isExpressionTruthy(state.expr_constant_string_foo), 1), "isExpressionTruthy on a %s expression did not return 1")
        setContext("function")
        expect(equals(isExpressionTruthy(state.expr_function), 1), "isExpressionTruthy on a %s expression did not return 1")
        setContext("table")
        expect(equals(isExpressionTruthy(state.expr_table), 1), "isExpressionTruthy on a %s expression did not return 1")
        setContext("interp string")
        expect(equals(isExpressionTruthy(state.expr_interp_string), 1), "isExpressionTruthy on an %s expression did not return 1")

        setContext("local")
        expect(equals(isExpressionTruthy(state.expr_local), 2), "isExpressionTruthy on a %s expression did not return 2")
        setContext("global")
        expect(equals(isExpressionTruthy(state.expr_global), 2), "isExpressionTruthy on a %s expression did not return 2")
        setContext("varargs")
        expect(equals(isExpressionTruthy(state.expr_varargs), 2), "isExpressionTruthy on a %s expression did not return 2")
        setContext("call")
        expect(equals(isExpressionTruthy(state.expr_call), 2), "isExpressionTruthy on a %s expression did not return 2")
        setContext("index_name")
        expect(equals(isExpressionTruthy(state.expr_index_name), 2), "isExpressionTruthy on an %s expression did not return 2")
        setContext("index_expr")
        expect(equals(isExpressionTruthy(state.expr_index_expr), 2), "isExpressionTruthy on an %s expression did not return 2")
        setContext("unary")
        expect(equals(isExpressionTruthy(state.expr_unary), 2), "isExpressionTruthy on a %s expression did not return 2")
        setContext("binary")
        expect(equals(isExpressionTruthy(state.expr_binary), 2), "isExpressionTruthy on a %s expression did not return 2")
        setContext("if-else")
        expect(equals(isExpressionTruthy(state.expr_if_else), 2), "isExpressionTruthy on an %s expression did not return 2")
    }
    endTest(isExpressionTruthy)

    startTest(simplify_constant_nil) {
        auto simplified = simplifier.simplify(state.expr_constant_nil);
        setContext("simplify on a constant nil")
        expect(equals(simplified.type, SimplifyResult::Nil), "%s did not return a SimplifyResult of type Nil")
    }
    endTest(simplify_constant_nil)

    startTest(simplify_constant_number) {
        auto simplified = simplifier.simplify(state.expr_constant_number_100);
        setContext("simplify on a constant number (100)")
        expect(equals(simplified.type, SimplifyResult::Number), "%s did not return a SimplifyResult of type Number")
        expect(equals(simplified.number_value, 100), "%s did not return a SimplifyResult with the same value")

        simplified = simplifier.simplify(state.expr_constant_number_negative_100);
        setContext("simplify on a constant number (-100)")
        expect(equals(simplified.type, SimplifyResult::Number), "%s did not return a SimplifyResult of type Number")
        expect(equals(simplified.number_value, -100), "%s did not return a SimplifyResult with the same value")
    }
    endTest(simplify_constant_number)

    startTest(simplify_constant_bool) {
        auto simplified = simplifier.simplify(state.expr_constant_bool_true);
        setContext("simplify on a constant bool (true)")
        expect(equals(simplified.type, SimplifyResult::Bool), "%s did not return a SimplifyResult of type Bool")
        expect(equals(simplified.bool_value, true), "%s did not return a SimplifyResult with the same value (true)")

        simplified = simplifier.simplify(state.expr_constant_bool_false);
        setContext("simplify on a constant bool (false)")
        expect(equals(simplified.type, SimplifyResult::Bool), "%s did not return a SimplifyResult of type Bool")
        expect(equals(simplified.bool_value, false), "%s did not return a SimplifyResult with the same value (false)")
    }
    endTest(simplify_constant_bool)

    #define testUnaryNot(expr, expr_text, result_type, result_type_member, expected_value, expected_value_text) { \
        AstExpr* expr_unary = allocator.alloc<AstExprUnary>(location, AstExprUnary::Not, expr); \
        auto simplified = simplifier.simplify(expr_unary); \
        setContext("simplify on a " expr_text) \
        expect(equals(simplified.type, SimplifyResult::result_type), "%s did not return a SimplifyResult of type " #result_type) \
        expect(equals(simplified.result_type_member, expected_value), "%s did not return a SimplifyResult with " expected_value_text) \
    }

    startTest(simplify_unary_not_nil) {
        testUnaryNot(state.expr_constant_nil, "expr constant nil", Bool, bool_value, true, "value true")
    }
    endTest(simplify_unary_not_nil)

    startTest(simplify_unary_not_bool) {
        testUnaryNot(state.expr_constant_bool_true, "unary not with expr constant bool (true)", Bool, bool_value, false, "the opposite value (false)")

        testUnaryNot(state.expr_constant_bool_false, "unary not with expr constant bool (false)", Bool, bool_value, true, "the opposite value (true)")

        testUnaryNot(allocator.alloc<AstExprUnary>(location, AstExprUnary::Not, state.expr_constant_bool_false), "nested unary not with expr constant bool (false)", Bool, bool_value, false, "the same value (false)")
    }
    endTest(simplify_unary_not_bool)

    startTest(simplify_unary_not_number) {
        testUnaryNot(state.expr_constant_number_100, "unary not with expr constant number (100)", Bool, bool_value, false, "value false")

        // testing a negative is likely redundant, but we'll keep it
        testUnaryNot(state.expr_constant_number_negative_100, "unary not with expr constant number (-100)", Bool, bool_value, false, "value false")
    }
    endTest(simplify_unary_not_number)

    startTest(simplify_unary_not_other) {
        // determinables
        testUnaryNot(state.expr_constant_string_foo, "unary not with constant string ('foo') expression", Bool, bool_value, false, "value false")
        testUnaryNot(state.expr_function, "unary not with function expression", Bool, bool_value, false, "value false")
        testUnaryNot(state.expr_table, "unary not with table expression", Bool, bool_value, false, "value false")
        testUnaryNot(state.expr_interp_string, "unary not with interp string expression", Bool, bool_value, false, "value false")

        // non-determinables
        testUnaryNot(state.expr_local, "unary not with a local expression", Other, other_value, expr_unary, "the same expression passed")
        testUnaryNot(state.expr_global, "unary not with a global expression", Other, other_value, expr_unary, "the same expression passed")
        testUnaryNot(state.expr_varargs, "unary not with a varargs expression", Other, other_value, expr_unary, "the same expression passed")
        testUnaryNot(state.expr_call, "unary not with a call expression", Other, other_value, expr_unary, "the same expression passed")
        testUnaryNot(state.expr_index_name, "unary not with an index_name expression", Other, other_value, expr_unary, "the same expression passed")
        testUnaryNot(state.expr_index_expr, "unary not with an index_expr expression", Other, other_value, expr_unary, "the same expression passed")
        testUnaryNot(state.expr_unary, "unary not with a unary expression", Other, other_value, expr_unary, "the same expression passed")
        testUnaryNot(state.expr_binary, "unary not with a binary expression", Other, other_value, expr_unary, "the same expression passed")
        testUnaryNot(state.expr_if_else, "unary not with an if-else expression", Other, other_value, expr_unary, "the same expression passed")
    }
    endTest(simplify_unary_not_other)

    #undef testUnaryNot

    startTest(simplify_unary_minus) {
        auto simplified = simplifier.simplify(allocator.alloc<AstExprUnary>(
            location, AstExprUnary::Minus,
            state.expr_constant_number_100
        ));
        setContext("simplify on a unary minus with constant number (100)")
        expect(equals(simplified.type, SimplifyResult::Number), "%s did not return a SimplifyResult of type Number")
        expect(equals(simplified.number_value, -100), "%s did not return a SimplifyResult with the negated value (-100)")

        simplified = simplifier.simplify(allocator.alloc<AstExprUnary>(
            location, AstExprUnary::Minus,
            state.expr_constant_number_negative_100
        ));
        setContext("simplify on a unary minus with constant number (-100)")
        expect(equals(simplified.type, SimplifyResult::Number), "%s did not return a SimplifyResult of type Number")
        expect(equals(simplified.number_value, 100), "%s did not return a SimplifyResult with the negated value (100)")
    }
    endTest(simplify_unary_minus)

    startTest(simplify_unary_len) {
        // normal string
        auto simplified = simplifier.simplify(allocator.alloc<AstExprUnary>(
            location, AstExprUnary::Len,
            state.expr_constant_string_foo
        ));
        setContext("simplify on a unary len with constant string ('foo')")
        expect(equals(simplified.type, SimplifyResult::Number), "%s did not return a SimplifyResult of type Number")
        expect(equals(simplified.number_value, 3), "%s did not return a SimplifyResult with value 3")

        // string with zero bytes
        simplified = simplifier.simplify(allocator.alloc<AstExprUnary>(location, AstExprUnary::Len, state.expr_constant_string_foo_zero_bytes_bar));
        setContext("simplify on a unary len with constant string ('foo\\0\\0bar')")
        expect(equals(simplified.type, SimplifyResult::Number), "%s did not return a SimplifyResult of type Number")
        expect(equals(simplified.number_value, 8), "%s did not return a SimplifyResult with value 8")

        // normal table
        auto items = std::vector<AstExprTable::Item>() = {
            { .kind = AstExprTable::Item::Kind::List, .value = state.expr_constant_number_100 },
            { .kind = AstExprTable::Item::Kind::List, .value = state.expr_constant_number_100 },
            { .kind = AstExprTable::Item::Kind::List, .value = state.expr_constant_number_100 },
        };
        simplified = simplifier.simplify(allocator.alloc<AstExprUnary>(
            location, AstExprUnary::Len,
            allocator.alloc<AstExprTable>(location, copy(allocator, items.data(), items.size()))
        ));
        setContext("simplify on a unary len with normal table ({100,100,100})")
        expect(equals(simplified.type, SimplifyResult::Number), "%s did not return a SimplifyResult of type Number")
        expect(equals(simplified.number_value, 3), "%s did not return a SimplifyResult with value 3")

        // table with undeterminable
        items = std::vector<AstExprTable::Item>() = {
            { .kind = AstExprTable::Item::Kind::List, .value = state.expr_constant_number_100 },
            { .kind = AstExprTable::Item::Kind::List, .value = state.expr_local },
            { .kind = AstExprTable::Item::Kind::List, .value = state.expr_constant_number_100 },
        };
        auto expr = allocator.alloc<AstExprUnary>(
            location, AstExprUnary::Len,
            allocator.alloc<AstExprTable>(location, copy(allocator, items.data(), items.size())
        ));
        simplified = simplifier.simplify(expr);
        setContext("simplify on a unary len with table with undeterminables ({100,local_foo,100})")
        expect(equals(simplified.type, SimplifyResult::Other), "%s did not return a SimplifyResult of type Other")
        expect(equals(simplified.other_value, expr), "%s did not return a SimplifyResult with the same expression passed")

        // table with nils forcing binary search
        items = std::vector<AstExprTable::Item>() = {
            { .kind = AstExprTable::Item::Kind::List, .value = state.expr_constant_number_100 },
            { .kind = AstExprTable::Item::Kind::List, .value = state.expr_constant_nil },
            { .kind = AstExprTable::Item::Kind::List, .value = state.expr_constant_nil },
            { .kind = AstExprTable::Item::Kind::List, .value = state.expr_constant_number_100 },
            { .kind = AstExprTable::Item::Kind::List, .value = state.expr_constant_nil },
        };
        simplified = simplifier.simplify(allocator.alloc<AstExprUnary>(
            location, AstExprUnary::Len,
            allocator.alloc<AstExprTable>(location, copy(allocator, items.data(), items.size()))
        ));
        setContext("simplify on a unary len with table with nils ({100,nil,nil,100,nil})")
        expect(equals(simplified.type, SimplifyResult::Number), "%s did not return a SimplifyResult of type Number")
        expect(equals(simplified.number_value, 1), "%s did not return a SimplifyResult with value 1")
    }
    endTest(simplify_unary_len)

    startTest(simplify_binary_add) {
        // 100 + 100
        auto simplified = simplifier.simplify(allocator.alloc<AstExprBinary>(
            location, AstExprBinary::Add,
            state.expr_constant_number_100,
            state.expr_constant_number_100
        ));
        setContext("simplify on a binary add with constant number (100) and constant number (100)")
        expect(equals(simplified.type, SimplifyResult::Number), "%s did not return a SimplifyResult of type Number")
        expect(equals(simplified.number_value, 200), "%s did not return a SimplifyResult with value 200")

        // "100" + 100
        simplified = simplifier.simplify(allocator.alloc<AstExprBinary>(
            location, AstExprBinary::Add,
            state.expr_constant_string_100,
            state.expr_constant_number_100
        ));
        setContext("simplify on a binary add with constant string ('100') and constant number (100)")
        expect(equals(simplified.type, SimplifyResult::Number), "%s did not return a SimplifyResult of type Number")
        expect(equals(simplified.number_value, 200), "%s did not return a SimplifyResult with value 200")

        // "100" + "100"
        simplified = simplifier.simplify(allocator.alloc<AstExprBinary>(
            location, AstExprBinary::Add,
            state.expr_constant_string_100,
            state.expr_constant_string_100
        ));
        setContext("simplify on a binary add with constant string ('100') and constant string ('100')")
        expect(equals(simplified.type, SimplifyResult::Number), "%s did not return a SimplifyResult of type Number")
        expect(equals(simplified.number_value, 200), "%s did not return a SimplifyResult with value 200")
    }
    endTest(simplify_binary_add)
    startTest(simplify_binary_sub) {
        // -100 - 100
        auto simplified = simplifier.simplify(allocator.alloc<AstExprBinary>(
            location, AstExprBinary::Sub,
            state.expr_constant_number_negative_100,
            state.expr_constant_number_100
        ));
        setContext("simplify on a binary sub with constant number (-100) and constant number (100)")
        expect(equals(simplified.type, SimplifyResult::Number), "%s did not return a SimplifyResult of type Number")
        expect(equals(simplified.number_value, -200), "%s did not return a SimplifyResult with value -200")

        // "100" - -100
        simplified = simplifier.simplify(allocator.alloc<AstExprBinary>(
            location, AstExprBinary::Sub,
            state.expr_constant_string_100,
            state.expr_constant_number_negative_100
        ));
        setContext("simplify on a binary sub with constant string ('100') and constant number (-100)")
        expect(equals(simplified.type, SimplifyResult::Number), "%s did not return a SimplifyResult of type Number")
        expect(equals(simplified.number_value, 200), "%s did not return a SimplifyResult with value 200")

        // "100" - "100"
        simplified = simplifier.simplify(allocator.alloc<AstExprBinary>(
            location, AstExprBinary::Sub,
            state.expr_constant_string_100,
            state.expr_constant_string_100
        ));
        setContext("simplify on a binary sub with constant string ('100') and constant string ('100')")
        expect(equals(simplified.type, SimplifyResult::Number), "%s did not return a SimplifyResult of type Number")
        expect(equals(simplified.number_value, 0), "%s did not return a SimplifyResult with value 0")
    }
    endTest(simplify_binary_sub)
    startTest(simplify_binary_mul) {
        // 100 * 100
        auto simplified = simplifier.simplify(allocator.alloc<AstExprBinary>(
            location, AstExprBinary::Mul,
            state.expr_constant_number_100,
            state.expr_constant_number_100
        ));
        setContext("simplify on a binary mul with constant number (100) and constant number (100)")
        expect(equals(simplified.type, SimplifyResult::Number), "%s did not return a SimplifyResult of type Number")
        expect(equals(simplified.number_value, 10000), "%s did not return a SimplifyResult with value 10000")

        // "100" * -100
        simplified = simplifier.simplify(allocator.alloc<AstExprBinary>(
            location, AstExprBinary::Mul,
            state.expr_constant_string_100,
            state.expr_constant_number_negative_100
        ));
        setContext("simplify on a binary mul with constant string ('100') and constant number (-100)")
        expect(equals(simplified.type, SimplifyResult::Number), "%s did not return a SimplifyResult of type Number")
        expect(equals(simplified.number_value, -10000), "%s did not return a SimplifyResult with value -10000")

        // "100" * "100"
        simplified = simplifier.simplify(allocator.alloc<AstExprBinary>(
            location, AstExprBinary::Mul,
            state.expr_constant_string_100,
            state.expr_constant_string_100
        ));
        setContext("simplify on a binary mul with constant string ('100') and constant string ('100')")
        expect(equals(simplified.type, SimplifyResult::Number), "%s did not return a SimplifyResult of type Number")
        expect(equals(simplified.number_value, 10000), "%s did not return a SimplifyResult with value 10000")
    }
    endTest(simplify_binary_mul)
    startTest(simplify_binary_div) {
        // 100 / 100
        auto simplified = simplifier.simplify(allocator.alloc<AstExprBinary>(
            location, AstExprBinary::Div,
            state.expr_constant_number_100,
            state.expr_constant_number_100
        ));
        setContext("simplify on a binary div with constant number (100) and constant number (100)")
        expect(equals(simplified.type, SimplifyResult::Number), "%s did not return a SimplifyResult of type Number")
        expect(equals(simplified.number_value, 1), "%s did not return a SimplifyResult with value 1")

        // "100" / -100
        simplified = simplifier.simplify(allocator.alloc<AstExprBinary>(
            location, AstExprBinary::Div,
            state.expr_constant_string_100,
            state.expr_constant_number_negative_100
        ));
        setContext("simplify on a binary div with constant string ('100') and constant number (-100)")
        expect(equals(simplified.type, SimplifyResult::Number), "%s did not return a SimplifyResult of type Number")
        expect(equals(simplified.number_value, -1), "%s did not return a SimplifyResult with value -1")

        // "100" / "100"
        simplified = simplifier.simplify(allocator.alloc<AstExprBinary>(
            location, AstExprBinary::Div,
            state.expr_constant_string_100,
            state.expr_constant_string_100
        ));
        setContext("simplify on a binary div with constant string ('100') and constant string ('100')")
        expect(equals(simplified.type, SimplifyResult::Number), "%s did not return a SimplifyResult of type Number")
        expect(equals(simplified.number_value, 1), "%s did not return a SimplifyResult with value 1")
    }
    endTest(simplify_binary_div)
    startTest(simplify_binary_floordiv) {
        // 100 // 100
        auto simplified = simplifier.simplify(allocator.alloc<AstExprBinary>(
            location, AstExprBinary::FloorDiv,
            state.expr_constant_number_100,
            state.expr_constant_number_100
        ));
        setContext("simplify on a binary floordiv with constant number (100) and constant number (100)")
        expect(equals(simplified.type, SimplifyResult::Number), "%s did not return a SimplifyResult of type Number")
        expect(equals(simplified.number_value, 1), "%s did not return a SimplifyResult with value 1")

        // "100" // -100
        simplified = simplifier.simplify(allocator.alloc<AstExprBinary>(
            location, AstExprBinary::FloorDiv,
            state.expr_constant_string_100,
            state.expr_constant_number_negative_100
        ));
        setContext("simplify on a binary floordiv with constant string ('100') and constant number (-100)")
        expect(equals(simplified.type, SimplifyResult::Number), "%s did not return a SimplifyResult of type Number")
        expect(equals(simplified.number_value, -1), "%s did not return a SimplifyResult with value -1")

        // "100" // "100"
        simplified = simplifier.simplify(allocator.alloc<AstExprBinary>(
            location, AstExprBinary::FloorDiv,
            state.expr_constant_string_100,
            state.expr_constant_string_100
        ));
        setContext("simplify on a binary floordiv with constant string ('100') and constant string ('100')")
        expect(equals(simplified.type, SimplifyResult::Number), "%s did not return a SimplifyResult of type Number")
        expect(equals(simplified.number_value, 1), "%s did not return a SimplifyResult with value 1")
    }
    endTest(simplify_binary_floordiv)
    startTest(simplify_binary_mod) {
        // 100 % 100
        auto simplified = simplifier.simplify(allocator.alloc<AstExprBinary>(
            location, AstExprBinary::Mod,
            state.expr_constant_number_100,
            state.expr_constant_number_100
        ));
        setContext("simplify on a binary mod with constant number (100) and constant number (100)")
        expect(equals(simplified.type, SimplifyResult::Number), "%s did not return a SimplifyResult of type Number")
        expect(equals(simplified.number_value, 0), "%s did not return a SimplifyResult with value 0")

        // "100" % -100
        simplified = simplifier.simplify(allocator.alloc<AstExprBinary>(
            location, AstExprBinary::Mod,
            state.expr_constant_string_100,
            state.expr_constant_number_negative_100
        ));
        setContext("simplify on a binary mod with constant string ('100') and constant number (-100)")
        expect(equals(simplified.type, SimplifyResult::Number), "%s did not return a SimplifyResult of type Number")
        expect(equals(simplified.number_value, 0), "%s did not return a SimplifyResult with value 0")

        // "100" % "100"
        simplified = simplifier.simplify(allocator.alloc<AstExprBinary>(
            location, AstExprBinary::Mod,
            state.expr_constant_string_100,
            state.expr_constant_string_100
        ));
        setContext("simplify on a binary mod with constant string ('100') and constant string ('100')")
        expect(equals(simplified.type, SimplifyResult::Number), "%s did not return a SimplifyResult of type Number")
        expect(equals(simplified.number_value, 0), "%s did not return a SimplifyResult with value 0")
    }
    endTest(simplify_binary_mod)
    startTest(simplify_binary_pow) {
        // 100 ^ 100
        auto simplified = simplifier.simplify(allocator.alloc<AstExprBinary>(
            location, AstExprBinary::Pow,
            state.expr_constant_number_100,
            state.expr_constant_number_100
        ));
        setContext("simplify on a binary pow with constant number (100) and constant number (100)")
        expect(equals(simplified.type, SimplifyResult::Number), "%s did not return a SimplifyResult of type Number")
        expect(equals(simplified.number_value, 1e+200), "%s did not return a SimplifyResult with value 1e+200")

        // "100" ^ -100
        simplified = simplifier.simplify(allocator.alloc<AstExprBinary>(
            location, AstExprBinary::Pow,
            state.expr_constant_string_100,
            state.expr_constant_number_negative_100
        ));
        setContext("simplify on a binary pow with constant string ('100') and constant number (-100)")
        expect(equals(simplified.type, SimplifyResult::Number), "%s did not return a SimplifyResult of type Number")
        expect(equals(simplified.number_value, 1e-200), "%s did not return a SimplifyResult with value 1e-200")

        // "100" ^ "100"
        simplified = simplifier.simplify(allocator.alloc<AstExprBinary>(
            location, AstExprBinary::Pow,
            state.expr_constant_string_100,
            state.expr_constant_string_100
        ));
        setContext("simplify on a binary pow with constant string ('100') and constant string ('100')")
        expect(equals(simplified.type, SimplifyResult::Number), "%s did not return a SimplifyResult of type Number")
        expect(equals(simplified.number_value, 1e+200), "%s did not return a SimplifyResult with value 1e+200")
    }
    endTest(simplify_binary_pow)

    startTest(simplify_binary_concat) {
        // "foo" .. "bar"
        auto simplified = simplifier.simplify(allocator.alloc<AstExprBinary>(
            location, AstExprBinary::Concat,
            state.expr_constant_string_foo,
            state.expr_constant_string_bar
        ));
        setContext("simplify on a binary concat with constant string ('foo') and constant string ('bar')")
        expect(equals(simplified.type, SimplifyResult::String), "%s did not return a SimplifyResult of type String")
        expect(equalsCharArray(simplified.string_value, cstringToAstCharArray("foobar")), "%s did not return a SimplifyResult with value 'foobar'")

        // "foo" .. 100
        simplified = simplifier.simplify(allocator.alloc<AstExprBinary>(
            location, AstExprBinary::Concat,
            state.expr_constant_string_foo,
            state.expr_constant_number_100
        ));
        setContext("simplify on a binary concat with constant string ('foo') and constant number (100)")
        expect(equals(simplified.type, SimplifyResult::String), "%s did not return a SimplifyResult of type String")
        expect(equalsCharArray(simplified.string_value, cstringToAstCharArray("foo100")), "%s did not return a SimplifyResult with value 'foo100'")

        // 100 .. "bar"
        simplified = simplifier.simplify(allocator.alloc<AstExprBinary>(
            location, AstExprBinary::Concat,
            state.expr_constant_number_100,
            state.expr_constant_string_bar
        ));
        setContext("simplify on a binary concat with constant number (100) and constant string ('bar')")
        expect(equals(simplified.type, SimplifyResult::String), "%s did not return a SimplifyResult of type String")
        expect(equalsCharArray(simplified.string_value, cstringToAstCharArray("100bar")), "%s did not return a SimplifyResult with value '100bar'")
    }
    endTest(simplify_binary_concat)

    startTest(simplify_binary_compareeq_other) {
        auto expr_binary = allocator.alloc<AstExprBinary>(
            location, AstExprBinary::CompareNe,
            state.expr_call,
            state.expr_call
        );
        auto simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary ~= with two calls")
        expect(equals(simplified.type, SimplifyResult::Other), "%s did not return a SimplifyResult of type Other")
        expect(equals(simplified.other_value, expr_binary), "%s did not return a SimplifyResult with the same expression passed")

        expr_binary = allocator.alloc<AstExprBinary>(
            location, AstExprBinary::CompareEq,
            state.expr_call,
            state.expr_call
        );
        simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary == with two calls")
        expect(equals(simplified.type, SimplifyResult::Other), "%s did not return a SimplifyResult of type Other")
        expect(equals(simplified.other_value, expr_binary), "%s did not return a SimplifyResult with the same expression passed")
    }
    endTest(simplify_binary_compareeq_other)

    startTest(simplify_binary_compareeq_simple) {
        // nil ~= nil
        auto expr_binary = allocator.alloc<AstExprBinary>(
            location, AstExprBinary::CompareNe,
            state.expr_constant_nil,
            state.expr_constant_nil
        );
        auto simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary ~= with two constant nils")
        expect(equals(simplified.type, SimplifyResult::Bool), "%s did not return a SimplifyResult of type Boolean")
        expect(equals(simplified.bool_value, false), "%s did not return a SimplifyResult with value false")

        // nil == nil
        expr_binary = allocator.alloc<AstExprBinary>(
            location, AstExprBinary::CompareEq,
            state.expr_constant_nil,
            state.expr_constant_nil
        );
        simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary == with two constant nils")
        expect(equals(simplified.type, SimplifyResult::Bool), "%s did not return a SimplifyResult of type Boolean")
        expect(equals(simplified.bool_value, true), "%s did not return a SimplifyResult with value true")

        // "foo" ~= nil
        expr_binary = allocator.alloc<AstExprBinary>(
            location, AstExprBinary::CompareNe,
            state.expr_constant_string_foo,
            state.expr_constant_nil
        );
        simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary ~= with a constant string ('foo') and a constant nil")
        expect(equals(simplified.type, SimplifyResult::Bool), "%s did not return a SimplifyResult of type Boolean")
        expect(equals(simplified.bool_value, true), "%s did not return a SimplifyResult with value true")

        // "foo" == nil
        expr_binary = allocator.alloc<AstExprBinary>(
            location, AstExprBinary::CompareEq,
            state.expr_constant_string_foo,
            state.expr_constant_nil
        );
        simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary == with a constant string ('foo') and a constant nil")
        expect(equals(simplified.type, SimplifyResult::Bool), "%s did not return a SimplifyResult of type Boolean")
        expect(equals(simplified.bool_value, false), "%s did not return a SimplifyResult with value false")

        // true ~= true
        expr_binary = allocator.alloc<AstExprBinary>(
            location, AstExprBinary::CompareNe,
            state.expr_constant_bool_true,
            state.expr_constant_bool_true
        );
        simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary ~= with two constant bools (true)")
        expect(equals(simplified.type, SimplifyResult::Bool), "%s did not return a SimplifyResult of type Boolean")
        expect(equals(simplified.bool_value, false), "%s did not return a SimplifyResult with value false")

        // true == true
        expr_binary = allocator.alloc<AstExprBinary>(
            location, AstExprBinary::CompareEq,
            state.expr_constant_bool_true,
            state.expr_constant_bool_true
        );
        simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary == with two constant bools (true)")
        expect(equals(simplified.type, SimplifyResult::Bool), "%s did not return a SimplifyResult of type Boolean")
        expect(equals(simplified.bool_value, true), "%s did not return a SimplifyResult with value true")

        // false ~= true
        expr_binary = allocator.alloc<AstExprBinary>(
            location, AstExprBinary::CompareNe,
            state.expr_constant_bool_false,
            state.expr_constant_bool_true
        );
        simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary ~= with a constant bool (false) and a constant bool (true)")
        expect(equals(simplified.type, SimplifyResult::Bool), "%s did not return a SimplifyResult of type Boolean")
        expect(equals(simplified.bool_value, true), "%s did not return a SimplifyResult with value true")

        // false == true
        expr_binary = allocator.alloc<AstExprBinary>(
            location, AstExprBinary::CompareEq,
            state.expr_constant_bool_false,
            state.expr_constant_bool_true
        );
        simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary == with a constant bool (false) and a constant bool (true)")
        expect(equals(simplified.type, SimplifyResult::Bool), "%s did not return a SimplifyResult of type Boolean")
        expect(equals(simplified.bool_value, false), "%s did not return a SimplifyResult with value false")

        // "foo" ~= "foo"
        expr_binary = allocator.alloc<AstExprBinary>(
            location, AstExprBinary::CompareNe,
            state.expr_constant_string_foo,
            state.expr_constant_string_foo
        );
        simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary ~= with two constant string ('foo')s")
        expect(equals(simplified.type, SimplifyResult::Bool), "%s did not return a SimplifyResult of type Boolean")
        expect(equals(simplified.bool_value, false), "%s did not return a SimplifyResult with value false")

        // "foo" == "foo"
        expr_binary = allocator.alloc<AstExprBinary>(
            location, AstExprBinary::CompareEq,
            state.expr_constant_string_foo,
            state.expr_constant_string_foo
        );
        simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary == with two constant string ('foo')s")
        expect(equals(simplified.type, SimplifyResult::Bool), "%s did not return a SimplifyResult of type Boolean")
        expect(equals(simplified.bool_value, true), "%s did not return a SimplifyResult with value true")

        // "foo" ~= "bar"
        expr_binary = allocator.alloc<AstExprBinary>(
            location, AstExprBinary::CompareNe,
            state.expr_constant_string_foo,
            state.expr_constant_string_bar
        );
        simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary ~= with a constant string ('foo') and a constant string ('bar')")
        expect(equals(simplified.type, SimplifyResult::Bool), "%s did not return a SimplifyResult of type Boolean")
        expect(equals(simplified.bool_value, true), "%s did not return a SimplifyResult with value true")

        // "foo" == "bar"
        expr_binary = allocator.alloc<AstExprBinary>(
            location, AstExprBinary::CompareEq,
            state.expr_constant_string_foo,
            state.expr_constant_string_bar
        );
        simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary == with a constant string ('foo') and a constant string ('bar')")
        expect(equals(simplified.type, SimplifyResult::Bool), "%s did not return a SimplifyResult of type Boolean")
        expect(equals(simplified.bool_value, false), "%s did not return a SimplifyResult with value false")

        // 100 ~= 100
        expr_binary = allocator.alloc<AstExprBinary>(
            location, AstExprBinary::CompareNe,
            state.expr_constant_number_100,
            state.expr_constant_number_100
        );
        simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary ~= with two constant numbers (100)")
        expect(equals(simplified.type, SimplifyResult::Bool), "%s did not return a SimplifyResult of type Boolean")
        expect(equals(simplified.bool_value, false), "%s did not return a SimplifyResult with value false")

        // 100 == 100
        expr_binary = allocator.alloc<AstExprBinary>(
            location, AstExprBinary::CompareEq,
            state.expr_constant_number_100,
            state.expr_constant_number_100
        );
        simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary == with two constant numbers (100)")
        expect(equals(simplified.type, SimplifyResult::Bool), "%s did not return a SimplifyResult of type Boolean")
        expect(equals(simplified.bool_value, true), "%s did not return a SimplifyResult with value true")

        // 100 ~= -100
        expr_binary = allocator.alloc<AstExprBinary>(
            location, AstExprBinary::CompareNe,
            state.expr_constant_number_100,
            state.expr_constant_number_negative_100
        );
        simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary ~= with a constant number (100) and constant number (-100)")
        expect(equals(simplified.type, SimplifyResult::Bool), "%s did not return a SimplifyResult of type Boolean")
        expect(equals(simplified.bool_value, true), "%s did not return a SimplifyResult with value true")

        // 100 == -100
        expr_binary = allocator.alloc<AstExprBinary>(
            location, AstExprBinary::CompareEq,
            state.expr_constant_number_100,
            state.expr_constant_number_negative_100
        );
        simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary == with a constant number (100) and constant number (-100)")
        expect(equals(simplified.type, SimplifyResult::Bool), "%s did not return a SimplifyResult of type Boolean")
        expect(equals(simplified.bool_value, false), "%s did not return a SimplifyResult with value false")
    }
    endTest(simplify_binary_compareeq_simple)

    startTest(simplify_binary_compareltge_simple) {
        // nil < nil
        auto expr_binary = allocator.alloc<AstExprBinary>(
            location, AstExprBinary::CompareLt,
            state.expr_constant_nil,
            state.expr_constant_nil
        );
        auto simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary < with two constant nils")
        expect(equals(simplified.type, SimplifyResult::Other), "%s did not return a SimplifyResult of type Other")
        expect(equals(simplified.other_value, expr_binary), "%s did not return a SimplifyResult with the same expression passed")

        // nil >= nil
        expr_binary = allocator.alloc<AstExprBinary>(
            location, AstExprBinary::CompareGe,
            state.expr_constant_nil,
            state.expr_constant_nil
        );
        simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary >= with two constant nils")
        expect(equals(simplified.type, SimplifyResult::Other), "%s did not return a SimplifyResult of type Other")
        expect(equals(simplified.other_value, expr_binary), "%s did not return a SimplifyResult with the same expression passed")

        // "foo" < nil
        expr_binary = allocator.alloc<AstExprBinary>(
            location, AstExprBinary::CompareLt,
            state.expr_constant_string_foo,
            state.expr_constant_nil
        );
        simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary < with a constant string ('foo') and a constant nil")
        expect(equals(simplified.type, SimplifyResult::Other), "%s did not return a SimplifyResult of type Other")
        expect(equals(simplified.other_value, expr_binary), "%s did not return a SimplifyResult with the same expression passed")

        // "foo" >= nil
        expr_binary = allocator.alloc<AstExprBinary>(
            location, AstExprBinary::CompareGe,
            state.expr_constant_string_foo,
            state.expr_constant_nil
        );
        simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary >= with a constant string ('foo') and a constant nil")
        expect(equals(simplified.type, SimplifyResult::Other), "%s did not return a SimplifyResult of type Other")
        expect(equals(simplified.other_value, expr_binary), "%s did not return a SimplifyResult with the same expression passed")

        // true < true
        expr_binary = allocator.alloc<AstExprBinary>(
            location, AstExprBinary::CompareLt,
            state.expr_constant_bool_true,
            state.expr_constant_bool_true
        );
        simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary < with two constant bools (true)")
        expect(equals(simplified.type, SimplifyResult::Other), "%s did not return a SimplifyResult of type Other")
        expect(equals(simplified.other_value, expr_binary), "%s did not return a SimplifyResult with the same expression passed")

        // true >= true
        expr_binary = allocator.alloc<AstExprBinary>(
            location, AstExprBinary::CompareGe,
            state.expr_constant_bool_true,
            state.expr_constant_bool_true
        );
        simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary >= with two constant bools (true)")
        expect(equals(simplified.type, SimplifyResult::Other), "%s did not return a SimplifyResult of type Other")
        expect(equals(simplified.other_value, expr_binary), "%s did not return a SimplifyResult with the same expression passed")

        // false < true
        expr_binary = allocator.alloc<AstExprBinary>(
            location, AstExprBinary::CompareLt,
            state.expr_constant_bool_false,
            state.expr_constant_bool_true
        );
        simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary < with a constant bool (false) and a constant bool (true)")
        expect(equals(simplified.type, SimplifyResult::Other), "%s did not return a SimplifyResult of type Other")
        expect(equals(simplified.other_value, expr_binary), "%s did not return a SimplifyResult with the same expression passed")

        // false >= true
        expr_binary = allocator.alloc<AstExprBinary>(
            location, AstExprBinary::CompareGe,
            state.expr_constant_bool_false,
            state.expr_constant_bool_true
        );
        simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary >= with a constant bool (false) and a constant bool (true)")
        expect(equals(simplified.type, SimplifyResult::Other), "%s did not return a SimplifyResult of type Other")
        expect(equals(simplified.other_value, expr_binary), "%s did not return a SimplifyResult with the same expression passed")

        // "foo" < "foo"
        expr_binary = allocator.alloc<AstExprBinary>(
            location, AstExprBinary::CompareLt,
            state.expr_constant_string_foo,
            state.expr_constant_string_foo
        );
        simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary < with two constant string ('foo')s")
        expect(equals(simplified.type, SimplifyResult::Bool), "%s did not return a SimplifyResult of type Boolean")
        expect(equals(simplified.bool_value, false), "%s did not return a SimplifyResult with value false")

        // "foo" >= "foo"
        expr_binary = allocator.alloc<AstExprBinary>(
            location, AstExprBinary::CompareGe,
            state.expr_constant_string_foo,
            state.expr_constant_string_foo
        );
        simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary >= with two constant string ('foo')s")
        expect(equals(simplified.type, SimplifyResult::Bool), "%s did not return a SimplifyResult of type Boolean")
        expect(equals(simplified.bool_value, true), "%s did not return a SimplifyResult with value true")

        // "foo" < "bar"
        expr_binary = allocator.alloc<AstExprBinary>(
            location, AstExprBinary::CompareLt,
            state.expr_constant_string_foo,
            state.expr_constant_string_bar
        );
        simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary < with a constant string ('foo') and a constant string ('bar')")
        expect(equals(simplified.type, SimplifyResult::Bool), "%s did not return a SimplifyResult of type Boolean")
        expect(equals(simplified.bool_value, false), "%s did not return a SimplifyResult with value false")

        // "foo" >= "bar"
        expr_binary = allocator.alloc<AstExprBinary>(
            location, AstExprBinary::CompareGe,
            state.expr_constant_string_foo,
            state.expr_constant_string_bar
        );
        simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary >= with a constant string ('foo') and a constant string ('bar')")
        expect(equals(simplified.type, SimplifyResult::Bool), "%s did not return a SimplifyResult of type Boolean")
        expect(equals(simplified.bool_value, true), "%s did not return a SimplifyResult with value true")

        // 100 < 100
        expr_binary = allocator.alloc<AstExprBinary>(
            location, AstExprBinary::CompareLt,
            state.expr_constant_number_100,
            state.expr_constant_number_100
        );
        simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary < with two constant numbers (100)")
        expect(equals(simplified.type, SimplifyResult::Bool), "%s did not return a SimplifyResult of type Boolean")
        expect(equals(simplified.bool_value, false), "%s did not return a SimplifyResult with value false")

        // 100 >= 100
        expr_binary = allocator.alloc<AstExprBinary>(
            location, AstExprBinary::CompareGe,
            state.expr_constant_number_100,
            state.expr_constant_number_100
        );
        simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary >= with two constant numbers (100)")
        expect(equals(simplified.type, SimplifyResult::Bool), "%s did not return a SimplifyResult of type Boolean")
        expect(equals(simplified.bool_value, true), "%s did not return a SimplifyResult with value true")

        // 100 < -100
        expr_binary = allocator.alloc<AstExprBinary>(
            location, AstExprBinary::CompareLt,
            state.expr_constant_number_100,
            state.expr_constant_number_negative_100
        );
        simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary < with a constant number (100) and constant number (-100)")
        expect(equals(simplified.type, SimplifyResult::Bool), "%s did not return a SimplifyResult of type Boolean")
        expect(equals(simplified.bool_value, false), "%s did not return a SimplifyResult with value false")

        // 100 >= -100
        expr_binary = allocator.alloc<AstExprBinary>(
            location, AstExprBinary::CompareGe,
            state.expr_constant_number_100,
            state.expr_constant_number_negative_100
        );
        simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary >= with a constant number (100) and constant number (-100)")
        expect(equals(simplified.type, SimplifyResult::Bool), "%s did not return a SimplifyResult of type Boolean")
        expect(equals(simplified.bool_value, true), "%s did not return a SimplifyResult with value true")
    }
    endTest(simplify_binary_compareltge_simple)

    startTest(simplify_binary_comparelegt_simple) {
        // nil <= nil
        auto expr_binary = allocator.alloc<AstExprBinary>(
            location, AstExprBinary::CompareLe,
            state.expr_constant_nil,
            state.expr_constant_nil
        );
        auto simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary <= with two constant nils")
        expect(equals(simplified.type, SimplifyResult::Other), "%s did not return a SimplifyResult of type Other")
        expect(equals(simplified.other_value, expr_binary), "%s did not return a SimplifyResult with the same expression passed")

        // nil > nil
        expr_binary = allocator.alloc<AstExprBinary>(
            location, AstExprBinary::CompareGt,
            state.expr_constant_nil,
            state.expr_constant_nil
        );
        simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary > with two constant nils")
        expect(equals(simplified.type, SimplifyResult::Other), "%s did not return a SimplifyResult of type Other")
        expect(equals(simplified.other_value, expr_binary), "%s did not return a SimplifyResult with the same expression passed")

        // "foo" <= nil
        expr_binary = allocator.alloc<AstExprBinary>(
            location, AstExprBinary::CompareLe,
            state.expr_constant_string_foo,
            state.expr_constant_nil
        );
        simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary <= with a constant string ('foo') and a constant nil")
        expect(equals(simplified.type, SimplifyResult::Other), "%s did not return a SimplifyResult of type Other")
        expect(equals(simplified.other_value, expr_binary), "%s did not return a SimplifyResult with the same expression passed")

        // "foo" > nil
        expr_binary = allocator.alloc<AstExprBinary>(
            location, AstExprBinary::CompareGt,
            state.expr_constant_string_foo,
            state.expr_constant_nil
        );
        simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary > with a constant string ('foo') and a constant nil")
        expect(equals(simplified.type, SimplifyResult::Other), "%s did not return a SimplifyResult of type Other")
        expect(equals(simplified.other_value, expr_binary), "%s did not return a SimplifyResult with the same expression passed")

        // true <= true
        expr_binary = allocator.alloc<AstExprBinary>(
            location, AstExprBinary::CompareLe,
            state.expr_constant_bool_true,
            state.expr_constant_bool_true
        );
        simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary <= with two constant bools (true)")
        expect(equals(simplified.type, SimplifyResult::Other), "%s did not return a SimplifyResult of type Other")
        expect(equals(simplified.other_value, expr_binary), "%s did not return a SimplifyResult with the same expression passed")

        // true > true
        expr_binary = allocator.alloc<AstExprBinary>(
            location, AstExprBinary::CompareGt,
            state.expr_constant_bool_true,
            state.expr_constant_bool_true
        );
        simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary > with two constant bools (true)")
        expect(equals(simplified.type, SimplifyResult::Other), "%s did not return a SimplifyResult of type Other")
        expect(equals(simplified.other_value, expr_binary), "%s did not return a SimplifyResult with the same expression passed")

        // false <= true
        expr_binary = allocator.alloc<AstExprBinary>(
            location, AstExprBinary::CompareLe,
            state.expr_constant_bool_false,
            state.expr_constant_bool_true
        );
        simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary <= with a constant bool (false) and a constant bool (true)")
        expect(equals(simplified.type, SimplifyResult::Other), "%s did not return a SimplifyResult of type Other")
        expect(equals(simplified.other_value, expr_binary), "%s did not return a SimplifyResult with the same expression passed")

        // false > true
        expr_binary = allocator.alloc<AstExprBinary>(
            location, AstExprBinary::CompareGt,
            state.expr_constant_bool_false,
            state.expr_constant_bool_true
        );
        simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary > with a constant bool (false) and a constant bool (true)")
        expect(equals(simplified.type, SimplifyResult::Other), "%s did not return a SimplifyResult of type Other")
        expect(equals(simplified.other_value, expr_binary), "%s did not return a SimplifyResult with the same expression passed")

        // "foo" <= "foo"
        expr_binary = allocator.alloc<AstExprBinary>(
            location, AstExprBinary::CompareLe,
            state.expr_constant_string_foo,
            state.expr_constant_string_foo
        );
        simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary <= with two constant string ('foo')s")
        expect(equals(simplified.type, SimplifyResult::Bool), "%s did not return a SimplifyResult of type Boolean")
        expect(equals(simplified.bool_value, true), "%s did not return a SimplifyResult with value true")

        // "foo" > "foo"
        expr_binary = allocator.alloc<AstExprBinary>(
            location, AstExprBinary::CompareGt,
            state.expr_constant_string_foo,
            state.expr_constant_string_foo
        );
        simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary > with two constant string ('foo')s")
        expect(equals(simplified.type, SimplifyResult::Bool), "%s did not return a SimplifyResult of type Boolean")
        expect(equals(simplified.bool_value, false), "%s did not return a SimplifyResult with value false")

        // "foo" <= "bar"
        expr_binary = allocator.alloc<AstExprBinary>(
            location, AstExprBinary::CompareLe,
            state.expr_constant_string_foo,
            state.expr_constant_string_bar
        );
        simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary <= with a constant string ('foo') and a constant string ('bar')")
        expect(equals(simplified.type, SimplifyResult::Bool), "%s did not return a SimplifyResult of type Boolean")
        expect(equals(simplified.bool_value, false), "%s did not return a SimplifyResult with value false")

        // "foo" > "bar"
        expr_binary = allocator.alloc<AstExprBinary>(
            location, AstExprBinary::CompareGt,
            state.expr_constant_string_foo,
            state.expr_constant_string_bar
        );
        simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary > with a constant string ('foo') and a constant string ('bar')")
        expect(equals(simplified.type, SimplifyResult::Bool), "%s did not return a SimplifyResult of type Boolean")
        expect(equals(simplified.bool_value, true), "%s did not return a SimplifyResult with value true")

        // 100 <= 100
        expr_binary = allocator.alloc<AstExprBinary>(
            location, AstExprBinary::CompareLe,
            state.expr_constant_number_100,
            state.expr_constant_number_100
        );
        simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary <= with two constant numbers (100)")
        expect(equals(simplified.type, SimplifyResult::Bool), "%s did not return a SimplifyResult of type Boolean")
        expect(equals(simplified.bool_value, true), "%s did not return a SimplifyResult with value true")

        // 100 > 100
        expr_binary = allocator.alloc<AstExprBinary>(
            location, AstExprBinary::CompareGt,
            state.expr_constant_number_100,
            state.expr_constant_number_100
        );
        simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary > with two constant numbers (100)")
        expect(equals(simplified.type, SimplifyResult::Bool), "%s did not return a SimplifyResult of type Boolean")
        expect(equals(simplified.bool_value, false), "%s did not return a SimplifyResult with value false")

        // 100 <= -100
        expr_binary = allocator.alloc<AstExprBinary>(
            location, AstExprBinary::CompareLe,
            state.expr_constant_number_100,
            state.expr_constant_number_negative_100
        );
        simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary <= with a constant number (100) and constant number (-100)")
        expect(equals(simplified.type, SimplifyResult::Bool), "%s did not return a SimplifyResult of type Boolean")
        expect(equals(simplified.bool_value, false), "%s did not return a SimplifyResult with value false")

        // 100 > -100
        expr_binary = allocator.alloc<AstExprBinary>(
            location, AstExprBinary::CompareGt,
            state.expr_constant_number_100,
            state.expr_constant_number_negative_100
        );
        simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary > with a constant number (100) and constant number (-100)")
        expect(equals(simplified.type, SimplifyResult::Bool), "%s did not return a SimplifyResult of type Boolean")
        expect(equals(simplified.bool_value, true), "%s did not return a SimplifyResult with value true")
    }
    endTest(simplify_binary_comparelegt_simple)

    startTest(simplify_binary_and) {
        // local_foo and true
        auto expr_binary = allocator.alloc<AstExprBinary>(location, AstExprBinary::And,
            state.expr_local,
            state.expr_constant_bool_true
        );
        auto simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary and with a local expr (foo_local) and constant bool (true)")
        expect(equals(simplified.type, SimplifyResult::Other), "%s did not return a SimplifyResult of type Other")
        expect(equals(simplified.other_value, expr_binary), "%s did not return a SimplifyResult with the same expression passed")

        // nil and true
        expr_binary = allocator.alloc<AstExprBinary>(location, AstExprBinary::And,
            state.expr_constant_nil,
            state.expr_constant_bool_true
        );
        simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary and with a constant nil and constant bool (true)")
        expect(equals(simplified.type, SimplifyResult::Nil), "%s did not return a SimplifyResult of type Nil")

        // true and true
        expr_binary = allocator.alloc<AstExprBinary>(location, AstExprBinary::And,
            state.expr_constant_bool_true,
            state.expr_constant_bool_true
        );
        simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary and with a constant bool (true) and constant bool (true)")
        expect(equals(simplified.type, SimplifyResult::Bool), "%s did not return a SimplifyResult of type Bool")
        expect(equals(simplified.bool_value, true), "%s did not return a SimplifyResult with value true")

        // true and 100
        expr_binary = allocator.alloc<AstExprBinary>(location, AstExprBinary::And,
            state.expr_constant_bool_true,
            state.expr_constant_number_100
        );
        simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary and with a constant bool (true) and constant number (100)")
        expect(equals(simplified.type, SimplifyResult::Number), "%s did not return a SimplifyResult of type Number")
        expect(equals(simplified.number_value, 100), "%s did not return a SimplifyResult with value 100")

        // false and false
        expr_binary = allocator.alloc<AstExprBinary>(location, AstExprBinary::And,
            state.expr_constant_bool_false,
            state.expr_constant_bool_false
        );
        simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary and with a constant bool (false) and constant bool (false)")
        expect(equals(simplified.type, SimplifyResult::Bool), "%s did not return a SimplifyResult of type Bool")
        expect(equals(simplified.bool_value, false), "%s did not return a SimplifyResult with value false")

        // false and true
        expr_binary = allocator.alloc<AstExprBinary>(location, AstExprBinary::And,
            state.expr_constant_bool_false,
            state.expr_constant_bool_true
        );
        simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary and with a constant bool (false) and constant bool (true)")
        expect(equals(simplified.type, SimplifyResult::Bool), "%s did not return a SimplifyResult of type Bool")
        expect(equals(simplified.bool_value, false), "%s did not return a SimplifyResult with value false")

        // true and false
        expr_binary = allocator.alloc<AstExprBinary>(location, AstExprBinary::And,
            state.expr_constant_bool_true,
            state.expr_constant_bool_false
        );
        simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary and with a constant bool (true) and constant bool (false)")
        expect(equals(simplified.type, SimplifyResult::Bool), "%s did not return a SimplifyResult of type Bool")
        expect(equals(simplified.bool_value, false), "%s did not return a SimplifyResult with value false")
    }
    endTest(simplify_binary_and)

    startTest(simplify_binary_or) {
        // local_foo or true
        auto expr_binary = allocator.alloc<AstExprBinary>(location, AstExprBinary::Or,
            state.expr_local,
            state.expr_constant_bool_true
        );
        auto simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary or with a local expr (foo_local) and constant bool (true)")
        expect(equals(simplified.type, SimplifyResult::Other), "%s did not return a SimplifyResult of type Other")
        expect(equals(simplified.other_value, expr_binary), "%s did not return a SimplifyResult with the same expression passed")

        // nil or true
        expr_binary = allocator.alloc<AstExprBinary>(location, AstExprBinary::Or,
            state.expr_constant_nil,
            state.expr_constant_bool_true
        );
        simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary or with a constant nil and constant bool (true)")
        expect(equals(simplified.type, SimplifyResult::Bool), "%s did not return a SimplifyResult of type Bool")
        expect(equals(simplified.bool_value, true), "%s did not return a SimplifyResult with value true")

        // true or true
        expr_binary = allocator.alloc<AstExprBinary>(location, AstExprBinary::Or,
            state.expr_constant_bool_true,
            state.expr_constant_bool_true
        );
        simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary or with a constant bool (true) and constant bool (true)")
        expect(equals(simplified.type, SimplifyResult::Bool), "%s did not return a SimplifyResult of type Bool")
        expect(equals(simplified.bool_value, true), "%s did not return a SimplifyResult with value true")

        // false or false
        expr_binary = allocator.alloc<AstExprBinary>(location, AstExprBinary::Or,
            state.expr_constant_bool_false,
            state.expr_constant_bool_false
        );
        simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary or with a constant bool (false) and constant bool (false)")
        expect(equals(simplified.type, SimplifyResult::Bool), "%s did not return a SimplifyResult of type Bool")
        expect(equals(simplified.bool_value, false), "%s did not return a SimplifyResult with value false")

        // false or -100
        expr_binary = allocator.alloc<AstExprBinary>(location, AstExprBinary::Or,
            state.expr_constant_bool_false,
            state.expr_constant_number_negative_100
        );
        simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary or with a constant bool (false) and constant number (-100)")
        expect(equals(simplified.type, SimplifyResult::Number), "%s did not return a SimplifyResult of type Number")
        expect(equals(simplified.number_value, -100), "%s did not return a SimplifyResult with value -100")

        // false or true
        expr_binary = allocator.alloc<AstExprBinary>(location, AstExprBinary::Or,
            state.expr_constant_bool_false,
            state.expr_constant_bool_true
        );
        simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary or with a constant bool (false) and constant bool (true)")
        expect(equals(simplified.type, SimplifyResult::Bool), "%s did not return a SimplifyResult of type Bool")
        expect(equals(simplified.bool_value, true), "%s did not return a SimplifyResult with value true")

        // true or false
        expr_binary = allocator.alloc<AstExprBinary>(location, AstExprBinary::Or,
            state.expr_constant_bool_true,
            state.expr_constant_bool_false
        );
        simplified = simplifier.simplify(expr_binary);
        setContext("simplify on a binary or with a constant bool (true) and constant bool (false)")
        expect(equals(simplified.type, SimplifyResult::Bool), "%s did not return a SimplifyResult of type Bool")
        expect(equals(simplified.bool_value, true), "%s did not return a SimplifyResult with value true")
    }
    endTest(simplify_binary_or)

    return failed;
}