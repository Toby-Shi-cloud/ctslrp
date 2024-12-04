#include "regex.hpp"
#include "type_map.hpp"
#include "utils.hpp"
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

int main() {
    test_type_map();
    test_regex();
}
