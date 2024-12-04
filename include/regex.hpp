#pragma once

#include "ctre.hpp"
#include "utils.hpp"
#include <type_traits>
#include <utility>

namespace ctslrp::details::regex {
namespace ctre = ::ctre;
namespace ctll = ::ctll;

/// cast ctslrp::details::string_literal to ctll::fixed_string
template <size_t N>
constexpr ctll::fixed_string<N> literal_cast(const string_literal<N> &str) {
    return ctll::fixed_string<N>(ctll::construct_from_pointer, str.data());
}

/// cast std::integer_sequence<T, ...> to ctre::string<...>
template <typename T, T... str>
constexpr auto literal_cast(std::integer_sequence<T, str...>) {
    return ctre::string<str...>{};
}

/// cast ctslrp::details::string_literal to ctre::string<...>
template <string_literal str> constexpr auto literal_cast() {
    auto seq = sequence_cast<str>();
    return literal_cast(seq);
};

template <string_literal str>
using literal_cast_t = decltype(literal_cast<str>());

/// to_regex: convert a type to a regex (aka ctre::regular_expression<RE>).
/// warning: to_regex will remove flags from the regex.
template <typename T> struct to_regex {
    static_assert(sizeof(T) == 0, "to_regex_traits: unsupported type");
};

template <typename RE, typename... Args>
struct to_regex<ctre::regular_expression<RE, Args...>> {
    using type = ctre::regular_expression<RE>;
};

template <typename RE, typename... Args,
          ctre::regular_expression<RE, Args...> re>
struct to_regex<value_wrapper<re>> {
    using type = ctre::regular_expression<RE>;
};

template <string_literal raw> struct to_regex<value_wrapper<raw>> {
    using type = ctre::regular_expression<literal_cast_t<raw>>;
};

template <typename T> using to_regex_t = typename to_regex<T>::type;
template <auto value>
using to_regex_by_value_t = to_regex_t<value_wrapper<value>>;

/// to determine if a type is a regex
template <typename T> struct is_regex : std::false_type {};
template <typename RE, typename... Args>
struct is_regex<ctre::regular_expression<RE, Args...>> : std::true_type {};
template <typename T> inline constexpr bool is_regex_v = is_regex<T>::value;
} // namespace ctslrp::details::regex

namespace ctslrp::details {
using regex::to_regex;
using regex::to_regex_by_value_t;
using regex::to_regex_t;
} // namespace ctslrp::details

namespace ctslrp {
inline namespace literals {
/// define as keywords
template <details::string_literal str> constexpr auto operator""_k() {
    static_assert(ctre::match<"\\w+">(str.sv()), "Invalid keyword");
    using details::regex::literal_cast;
    constexpr auto pat = "\\b"_raw + str + "\\b"_raw;
    using re = ctre::regex_builder<literal_cast(pat)>::type;
    return ctre::regular_expression<re>{};
}
/// define as regex
template <details::string_literal str> constexpr auto operator""_r() {
    using details::regex::literal_cast;
    using re = ctre::regex_builder<literal_cast(str)>::type;
    return ctre::regular_expression<re>{};
}
} // namespace literals
} // namespace ctslrp
