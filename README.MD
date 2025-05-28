# luau-format

`luau-format` is a Luau code formatter, capable of simplifying a wide range of expressions. Additionally, it can also minify code.

## usage
```sh
./luau-format input.lua [options]
```
After running, the formatted code will be in stdout, or the output file passed through --output=[file]. Any errors/warnings will be in stderr.
Run `luau-format` without any arguments to get a help message.

Or try [luau-format-gui](https://github.com/TechHog8984/luau-format-gui)!

# NOTE: lph_control_flow does not work and it was not supposed to be made public

## download
If you do not wish to build `luau-format` through the below instructions, you can find binaries (windows & linux x86_64) in the [latest release](https://github.com/TechHog8984/luau-format/releases/latest)

## build
`luau-format` can be built through a bash script:
```sh
./build.sh [platform] [options]
```
The available target platforms are: `linux`, `windows`. The default platform is `linux`.

## testing
`luau-format` can be tested through its test program. You can build the test program through the bash script mentioned above like so:
```sh
./build.sh [platform] --test
```
Run the output program (named something like `test-luau-format`) for an output like this:
```
RUNNING TESTS
passed 'getRootExpr'
...
passed 'simplify_unary_not_number'
PASSED 5/5 TESTS
```

## features

`luau-format` comes with a smart expression simplifier called `AstSimplifier`. `AstSimplifier` is capable of simplifying a wide variety of expression types:
* nested expression groups[^1]
* unary not (example: `not nil` -> true; `not not {}` -> true; `not some_local` -> `not some_local`)
* unary minus[^1]
* unary len (example: `#"foo"` -> 3; `#{1,2,3,4}` -> 4; `#{nil,1,nil,1}` -> 4; `#{nil,1,nil}` -> 2)
* binary math operations such as addition and subtraction (example: `4 + 10` -> 14; `"100" - 3` -> 97)
* binary concat (example: `"foo" .. "bar"` -> "foobar"; `100 .. "bar"` -> "100bar")
* binary comparisons such as == and >= (example: `4 == 4` -> true; `10 < 3` -> false; `nil == nil` -> true; `true == false` -> false)
* binary and (example: `false and true` -> false; `123 and true` -> true; `true and 123` -> 123)
* binary or (example: `true or false` -> true; `false or false` -> false; `false or 321` -> 321)
* unary not on a binary (example: `not (a == 4)` -> `a ~= 4`; `not (a > 4)` -> `a <= 4`)
* repeat [body] until true (example: `repeat a += 1 until 1 == 1` -> `a += 1`; implemented in formatter, but only in use when simplifier is enabled)
* more...

[^1]: <small> not necessarily noticable unless working with the AST (for example, `-100` goes from `ExprUnary(Minus, ExprConstantNumber(100))` to `ExprConstantNumber(-100)`) and therefore there is a lack of examples </small>

## TODO
* file with all sorts of ast/expr combinations. beautify twicefold and check contents + output. this will ensure the output can be parsed. (refer to test.hpp for more information)
* tests for `AstFormatter` & complete simplifier tests
* the current simplifying&formatting setup does not handle ExprGroups appropriately (sometimes parenthesis are needed)
* explore if the lexer will allow us to preserve whitespace. if so, use for certain characters (like single/double newlines)
* all functions declared should belong to a class (make 'extra' functions static members of its file's class)
* formatRoot should have an argument to explicitly omit `do` & `end`
* simplify explicitly setting a certain randomseed and then calling math.random (be under an option!! this is dangerous)
* complete feature list
