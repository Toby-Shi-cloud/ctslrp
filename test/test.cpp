#include "ctslrp.hpp"
#include <iostream>
#include <string_view>
#include <type_traits>

static inline void test_type_map() {
    using namespace ctslrp::details;
    auto map = TypeMapEntry<int, char>{} + TypeMapEntry<float, double>{} +
               TypeMapEntry<char, int>{} + TypeMapEntry<double, float>{};
    using Map = decltype(map);
    static_assert(std::is_same_v<Map::ValueOf<int>, char>);
    static_assert(std::is_same_v<Map::ValueOf<char>, int>);
    static_assert(std::is_same_v<Map::ValueOf<double>, float>);
    static_assert(std::is_same_v<Map::ValueOf<float>, double>);
}

static inline void test_regex() {
    using namespace ctre;
    using namespace ctslrp::literals;
    using ctslrp::details::to_regex_by_value_t;
    using pat1 = to_regex_by_value_t<"+"_raw>;
    using pat2 = to_regex_by_value_t<"int"_k>;
    using pat3 = to_regex_by_value_t<"[0-9]+"_r>;
    static_assert(std::is_same_v<pat1, regular_expression<string<'+'>>>);
    static_assert(
        std::is_same_v<
            pat2, regular_expression<sequence<
                      boundary<set<char_range<'A', 'Z'>, char_range<'a', 'z'>,
                                   char_range<'0', '9'>, character<'_'>>>,
                      string<'i', 'n', 't'>,
                      boundary<set<char_range<'A', 'Z'>, char_range<'a', 'z'>,
                                   char_range<'0', '9'>, character<'_'>>>>>>);
    static_assert(
        std::is_same_v<pat3,
                       ctre::regular_expression<ctre::repeat<
                           1, 0, ctre::set<ctre::char_range<'0', '9'>>>>>);
    static_assert(pat1::search("1+2") == "+");
    static_assert(pat2::search("int a") == "int");
    static_assert(!pat2::search("int3"));
    static_assert(pat3::search("999x") == "999");
}

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

static inline void test_parser_runtime() {
    using namespace ctslrp::literals;
    enum class Symbol { ADD, MUL, PRI, NUM };
    ctslrp::SyntaxRuleGenerator<Symbol> gen;
    constexpr auto addexp = gen.decl<Symbol::ADD>();
    constexpr auto mulexp = gen.decl<Symbol::MUL>();
    constexpr auto priexp = gen.decl<Symbol::PRI>();
    constexpr auto number = gen.decl<Symbol::NUM>();
    constexpr auto table = (
        // Syntex Table Begin
        addexp // AddExp -> MulExp
            .define<Symbol::MUL>()
            .bind<double>([](double x) { return x; }),
        addexp // AddExp -> AddExp + MulExp
            .define<Symbol::ADD, "+", Symbol::MUL>()
            .bind<double>([](double x, auto &&, double y) { return x + y; }),
        addexp // AddExp -> AddExp - MulExp
            .define<Symbol::ADD, "-", Symbol::MUL>()
            .bind<double>([](double x, auto &&, double y) { return x - y; }),
        mulexp // MulExp -> Num
            .define<Symbol::PRI>()
            .bind<double>([](double x) { return x; }),
        mulexp // MulExp -> MulExp * PriExp
            .define<Symbol::MUL, "*", Symbol::PRI>()
            .bind<double>([](double x, auto &&, double y) { return x * y; }),
        mulexp // MulExp -> MulExp / PriExp
            .define<Symbol::MUL, "/", Symbol::PRI>()
            .bind<double>([](double x, auto &&, double y) { return x / y; }),
        priexp // PriExp -> Num
            .define<Symbol::NUM>()
            .bind<double>([](double x) { return x; }),
        priexp // PriExp -> ( AddExp )
            .define<"(", Symbol::ADD, ")">()
            .bind<double>([](auto &&, double y, auto &&) { return y; }),
        number // Number -> r"[1-9][0-9]*"
            .define<"[0-9]*"_r>()
            .bind<double>([](auto &&token) { return std::atoi(token.value.data()); }),
        number // Number -> r"[0-9]*.[0-9]*"
            .define<"[0-9]*\\.[0-9]*"_r>()
            .bind<double>([](auto &&token) { return std::atof(token.value.data()); })
        // Syntex Table End
    );
    constexpr auto parser = table.compile<Symbol::ADD>();
    std::cout << parser.parse("(1-0)*.3+4.1") << std::endl;         // 4.4
    std::cout << parser.parse("(999*99)/(99*(99-9))") << std::endl; // 11.1
}

int main() {
    test_type_map();
    test_regex();
    test_parser();
    test_parser_runtime();
    std::cout << "All tests passed!" << std::endl;
    return 0;
}
