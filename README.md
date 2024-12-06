# Compile-Time-SLR(1)-Parser

This is a SLR(1) Parser writen in cpp20.

Amazingly, it can build syntax tree and even visit syntax tree at compile time! 

## How to use?

1. Add `include` path to your project.
2. `#include <ctslrp.hpp>`.
3. Create an enum for your grammar.
4. Use `ctslrp::SyntaxRuleGenerator<SymbolEnum>` to start generating your grammars.
5. Compile the syntax table you generated.
6. Call `parse(std::string_view)` and enjoy the result!

An example as follows:
```cpp
static inline void test_parser() {
    using namespace ctslrp::literals;
    enum class Symbol { ADD, MUL, NUM };
    ctslrp::SyntaxRuleGenerator<Symbol> gen;
    constexpr auto addexp = gen.decl<Symbol::ADD>();
    constexpr auto mulexp = gen.decl<Symbol::MUL>();
    constexpr auto number = gen.decl<Symbol::NUM>();
    constexpr auto table = (
        // Syntex Table Begin
        addexp // AddExp -> MulExp
            .define<Symbol::MUL>()
            .bind<int>([](int x) { return x; }),
        addexp // AddExp -> AddExp + MulExp
            .define<Symbol::ADD, "+", Symbol::MUL>()
            .bind<int>([](int x, auto &&, int y) { return x + y; }),
        mulexp // MulExp -> Num
            .define<Symbol::NUM>()
            .bind<int>([](int x) { return x; }),
        mulexp // MulExp -> MulExp * Num
            .define<Symbol::MUL, "*", Symbol::NUM>()
            .bind<int>([](int x, auto &&, int y) { return x * y; }),
        number // Number -> r"[0-9]*"
            .define<"[0-9]*"_r>()
            .bind<int>([](ctslrp::Token token) { return atoi(token.value); })
        // Syntex Table End
    );
    constexpr auto parser = table.compile<Symbol::ADD>();
    static_assert(parser.parse("1+2*3+4") == 11);
    static_assert(parser.parse("9999*9999") == 99980001);
}
```

## APIs

+ `ctslrp::SyntaxRuleGenerator<T>`: the class of generator
+ `generator.decl<...>()`: declare a symbol in your grammar
+ `generator.decl<...>().define<...>()`: define a rule for that symbol
+ `generator.decl<...>().define<...>().bind<T>(Func &&func)`: bind a type and a visitor function
+ `operator,(...)`: use COMMA to concat the rules your defined
+ `table.compile<symbol>()`: compile the rule table, `symbol` is where your grammar started
+ `parser.parse(std::string_view input)`: parse the input and return the results
