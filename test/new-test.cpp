#include "ctslrp.hpp"
#include "slr/gen_action.hpp"
#include <iostream>

constexpr int atoi(std::string_view sv) {
    int result = 0;
    for (char c : sv) {
        result = result * 10 + c - '0';
    }
    return result;
}

static inline void test_parser() {
    using namespace ctslrp::literals;
    enum class Symbol { ADD, MUL, NUM };
    ctslrp::SyntaxRuleGenerator<Symbol> gen;
    constexpr auto addexp = gen.decl(Symbol::ADD);
    constexpr auto mulexp = gen.decl(Symbol::MUL);
    constexpr auto number = gen.decl(Symbol::NUM);
    // constexpr auto rule1 = addexp.define(Symbol::ADD, "+", Symbol::MUL);
    // constexpr auto rule2 = number.define("[0-9]*"_r);
    constexpr auto table = gen(
        // Syntex Table Begin
        addexp // AddExp -> MulExp
            .define(Symbol::MUL)
            .bind<int>([](int x) { return x; }),
        addexp // AddExp -> AddExp + MulExp
            .define(Symbol::ADD, "+", Symbol::MUL)
            .bind<int>([](int x, auto &&, int y) { return x + y; }),
        mulexp // MulExp -> Num
            .define(Symbol::NUM)
            .bind<int>([](int x) { return x; }),
        mulexp // MulExp -> MulExp * Num
            .define(Symbol::MUL, "*", Symbol::NUM)
            .bind<int>([](int x, auto &&, int y) { return x * y; }),
        number // Number -> r"[0-9]*"
            .define("[0-9]*"_r)
            .bind<int>([](auto token) { return atoi(token.value); })
        // Syntex Table End
    );
    constexpr auto result = gen.template compile<table.get_terminals_count()>(table);
    std::cout << result.second << std::endl;
    ctslrp::details::FirstSetHelper<result.second>::print_result(std::cout);
    ctslrp::details::FollowSetHelper<result.second>::print_result(std::cout);
    ctslrp::details::ClosureHelper<result.second>::print_result(std::cout);
    ctslrp::details::ItemSetCollectionHelper<result.second>::print_result(std::cout);
    // constexpr auto parser = table.compile<Symbol::ADD>();
    // static_assert(parser.parse("1+2*3+4") == 11);
    // static_assert(parser.parse("9999*9999") == 99980001);
}

int main() {
    test_parser();
    return 0;
}
