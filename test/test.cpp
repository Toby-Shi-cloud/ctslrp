#include "regex.hpp"
#include "syntax_rule.hpp"
#include "type_map.hpp"
#include "utils.hpp"
#include <iostream>
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

static inline void test_syntax() {
    using namespace ctslrp::literals;
    using namespace ctslrp::details;
    enum class Symbol { ADD, MUL, NUM };
    SyntaxRuleGenerator<Symbol> gen;
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
        mulexp // MulExp -> MulExp *
            .define<Symbol::MUL, "*", Symbol::NUM>()
            .bind<int>([](int x, auto &&, int y) { return x * y; }),
        number // Number -> r"[1-9][0-9]*"
            .define<"[1-9][0-9]*"_r>()
            .bind<int>([](int x) { return x; })
        // Syntex Table End
    );
    constexpr auto compiled = table.compile<Symbol::ADD>();
    constexpr auto lexer = std::get<0>(compiled);
    constexpr auto tokens = lexer.tokenize<"1+2*3">();
    for (const auto &token : tokens) {
        std::cout << token << std::endl;
    }
    constexpr auto lr_table = std::get<1>(compiled);
    using lr_table_t = decltype(lr_table);
    std::cout << lr_table;
    using first_set = lr_table_t::FirstSet;
    std::cout << "FirstSet = " << first_set{};
    using follow_set = lr_table_t::FollowSet;
    std::cout << "FollowSet = " << follow_set{};
    using item_set_collection = lr_table_t::ItemSetCollection;
    std::cout << item_set_collection{}; 
}

int main() {
    test_type_map();
    test_regex();
    test_syntax();
    return 0;
}
