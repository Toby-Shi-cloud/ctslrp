#pragma once

#include <algorithm>
#include <array>
#include <cstddef>
#include <functional>
#include <numeric>
#include <ostream>
#include <string_view>
#include <type_traits>
#include <utility>

namespace ctslrp::details {
constexpr std::string constexpr_to_string(size_t n) {
    std::string str;
    while (n) {
        str.push_back('0' + n % 10);
        n /= 10;
    }
    std::reverse(str.begin(), str.end());
    return str;
}

/// a value wrapper of any type (convert constexpr value to type)
template <auto v> struct value_wrapper {
    using type = decltype(v);
    static constexpr type value = v;
};

/// a sequence of any type
template <auto... values> struct any_sequence {
    static constexpr auto size = sizeof...(values);

    template <typename T> constexpr static auto as_array() noexcept {
        return std::array<T, size>{values...};
    }

    friend std::ostream &operator<<(std::ostream &os, any_sequence) {
        os << "any_sequence: ";
        ((os << values << ", "), ...);
        return os;
    }
};

template <auto... I, auto... J>
constexpr auto operator+(any_sequence<I...>, any_sequence<J...>) {
    return any_sequence<I..., J...>{};
}

/// a sequence of fixed type
template <typename T, T... values> struct typed_sequence {
    using type = T;
    static constexpr auto size = sizeof...(values);

    constexpr typed_sequence() noexcept = default;

    template <typename U>
    constexpr typed_sequence(std::integer_sequence<U, values...>) noexcept {}

    constexpr operator any_sequence<values...>() const noexcept { return {}; }

    template <T... others>
    friend constexpr auto operator+(typed_sequence<T, values...>,
                                    typed_sequence<T, others...>) noexcept {
        return typed_sequence<T, values..., others...>{};
    }

    constexpr static auto as_array() noexcept {
        return std::array<T, size>{values...};
    }

    friend std::ostream &operator<<(std::ostream &os, typed_sequence) {
        os << "typed_sequence: ";
        ((os << values << ", "), ...);
        return os;
    }
};
template <typename Int, Int... vs>
typed_sequence(std::integer_sequence<Int, vs...>) -> typed_sequence<Int, vs...>;

namespace seq {
using ::ctslrp::details::any_sequence;
using ::ctslrp::details::typed_sequence;
template <size_t... Is> using index_sequence = typed_sequence<size_t, Is...>;

template <std::array arr, size_t... I>
constexpr auto array_as_sequence_impl(std::index_sequence<I...>) {
    return typed_sequence<std::decay_t<decltype(arr[0])>, arr[I]...>{};
}

template <std::array arr, size_t size = arr.size()>
constexpr auto array_as_sequence() {
    constexpr auto index_seq = std::make_index_sequence<size>();
    return array_as_sequence_impl<arr>(index_seq);
}

template <auto func = std::plus<>{}, typename T, T v, T... vs>
constexpr auto reduce(typed_sequence<T, v, vs...>) noexcept {
    constexpr auto array = std::array{vs...};
    return std::reduce(std::begin(array), std::end(array), v, func);
}

template <typename Func, typename T, T v, T... vs>
constexpr auto reduce(Func &&func, typed_sequence<T, v, vs...>) noexcept {
    constexpr auto array = std::array{vs...};
    return std::reduce(std::begin(array), std::end(array), v, func);
}

template <auto func = std::plus<>{}, auto value>
constexpr auto reduce(any_sequence<value>) noexcept {
    return value;
}

template <auto func = std::plus<>{}, auto v1, auto v2, auto... vs>
constexpr auto reduce(any_sequence<v1, v2, vs...>) noexcept {
    return reduce<func>(any_sequence<std::invoke(func, v1, v2), vs...>{});
}

template <auto func, typename T, T... vs>
constexpr auto map(typed_sequence<T, vs...>) noexcept {
    return typed_sequence<std::invoke_result_t<decltype(func), T>,
                          std::invoke(func, vs)...>{};
}

template <auto func, auto... vs>
constexpr auto map(any_sequence<vs...>) noexcept {
    return any_sequence<std::invoke(func, vs)...>{};
}

template <typename Func, typename T, T... vs>
constexpr auto map(Func &&func, typed_sequence<T, vs...>) noexcept {
    return std::array{std::invoke(func, vs)...};
}

template <typename Func, auto... vs>
constexpr auto map(Func &&func, any_sequence<vs...>) noexcept {
    return std::array{std::invoke(func, vs)...};
}

template <typename Func, typename... Ts>
constexpr auto for_each(Func &&func, Ts &&...vs) noexcept {
    (std::invoke(std::forward<Func>(func), vs), ...);
}

template <typename Func, typename T, T... vs>
constexpr auto for_each(Func &&func, typed_sequence<T, vs...>) noexcept {
    (std::invoke(std::forward<Func>(func), vs), ...);
}

template <typename Func, auto... vs>
constexpr auto for_each(Func &&func, any_sequence<vs...>) noexcept {
    (std::invoke(std::forward<Func>(func), vs), ...);
}

template <template <auto> typename Wrapper, typename T, auto... vs>
constexpr auto wrap(typed_sequence<T, vs...>) noexcept {
    return any_sequence<Wrapper<vs>{}...>{};
}

template <template <auto> typename Wrapper, auto... vs>
constexpr auto wrap(any_sequence<vs...>) noexcept {
    return any_sequence<Wrapper<vs>{}...>{};
}

template <auto func, typename T, auto... vs>
constexpr auto wrapped_map(typed_sequence<T, vs...>) noexcept {
    return map<func>(any_sequence<value_wrapper<vs>{}...>{});
}

template <auto func, auto... vs>
constexpr auto wrapped_map(any_sequence<vs...>) noexcept {
    return map<func>(any_sequence<value_wrapper<vs>{}...>{});
}

template <typename Func, typename T, auto... vs>
constexpr auto wrapped_map(Func &&func, typed_sequence<T, vs...>) noexcept {
    return map(func, any_sequence<value_wrapper<vs>{}...>{});
}

template <typename Func, auto... vs>
constexpr auto wrapped_map(Func &&func, any_sequence<vs...>) noexcept {
    return map(func, any_sequence<value_wrapper<vs>{}...>{});
}

template <typename T, size_t N>
constexpr auto sorted(std::array<T, N> arr) noexcept {
    std::sort(arr.begin(), arr.end());
    return arr;
}

template <typename T, T... vs>
constexpr auto sorted(typed_sequence<T, vs...>) noexcept {
    constexpr auto arr = typed_sequence<T, vs...>::as_array();
    return array_as_sequence<sorted(arr)>();
}
} // namespace seq

/// a string literal with fixed size
template <size_t N> struct string_literal {
    char m_data[N + 1];
    constexpr string_literal() : m_data{} {}
    constexpr string_literal(char c) {
        m_data[0] = c;
        m_data[1] = '\0';
    }
    constexpr string_literal(const char (&str)[N + 1]) {
        std::copy_n(str, N, m_data);
        m_data[N] = '\0';
    }
    constexpr string_literal(std::string_view str) {
        size_t len = std::min(N, str.size());
        std::copy_n(str.data(), len, m_data);
        for (; len < N + 1; ++len)
            m_data[len] = 0;
    }

    static constexpr size_t npos = -1;
    constexpr const char *data() const noexcept { return m_data; }
    constexpr std::string_view sv() const noexcept { return {m_data, N}; }
    static constexpr size_t size() noexcept { return N; }

    constexpr operator std::string_view() const noexcept { return sv(); }
    constexpr char operator[](size_t i) const noexcept { return m_data[i]; }

    constexpr size_t find(char ch, size_t pos = 0) const noexcept {
        for (size_t i = pos; i < N; ++i)
            if (m_data[i] == ch) return i;
        return npos;
    }

    template <size_t pos, size_t length = npos>
    constexpr auto substr() const noexcept {
        constexpr size_t real_length = std::min(N - pos, length);
        string_literal<real_length> result;
        std::copy_n(m_data + pos, real_length, result.m_data);
        return result;
    }

    template <size_t M>
    constexpr string_literal<N + M>
    operator+(const string_literal<M> &rhs) const noexcept {
        string_literal<N + M> result;
        std::copy_n(m_data, N, result.m_data);
        std::copy_n(rhs.m_data, M, result.m_data + N);
        return result;
    }

    friend std::ostream &operator<<(std::ostream &os,
                                    const string_literal &str) {
        return os << str.m_data;
    }
};
template <size_t N> string_literal(const char (&)[N]) -> string_literal<N - 1>;
string_literal(char) -> string_literal<1>;

template <string_literal str> struct string_as_sequence_helpers {
    template <size_t... Is>
    constexpr static auto char_at(std::index_sequence<Is...>) {
        return std::integer_sequence<char, str[Is]...>{};
    }

    constexpr static auto find_space_index(std::index_sequence<>) {
        return std::index_sequence<str.size()>();
    }

    template <size_t I, size_t... Is>
    constexpr static auto find_space_index(std::index_sequence<I, Is...>) {
        if constexpr (str[I] == ' ') {
            return std::index_sequence<I>{} +
                   find_space_index(std::index_sequence<Is...>());
        } else {
            return find_space_index(std::index_sequence<Is...>());
        }
    }

    template <size_t pos, size_t nxt, size_t... rest>
    constexpr static auto split_by(std::index_sequence<pos, nxt, rest...>) {
        constexpr auto head = str.template substr<pos, nxt - pos>();
        if constexpr (sizeof...(rest) == 0)
            return any_sequence<head>{};
        else
            return any_sequence<head>{} +
                   split_by(std::index_sequence<nxt + 1, rest...>());
    }
};

template <string_literal str> constexpr auto sequence_cast() {
    auto index_seq = std::make_index_sequence<str.size()>();
    return string_as_sequence_helpers<str>::char_at(index_seq);
}

template <string_literal str> constexpr auto string_as_sequence() {
    using helper = string_as_sequence_helpers<str>;
    auto index_seq = std::make_index_sequence<str.size()>();
    auto space_idx = helper::find_space_index(index_seq);
    return helper::split_by(std::index_sequence<0>() + space_idx);
}

template <typename T> constexpr auto typename_of() {
    constexpr std::string_view name = __PRETTY_FUNCTION__;
    constexpr auto start = name.find("T = ");
    constexpr auto end = name.rfind("]");
    constexpr auto tpname = name.substr(start + 4, end - start - 4);
    return string_literal<tpname.size()>{tpname};
}

template <typename T> constexpr auto typename_of(T) { return typename_of<T>(); }

template <auto value> constexpr auto valuename_of() {
    static_assert(std::is_enum_v<std::decay_t<decltype(value)>>,
                  "value must be an enum");
    constexpr std::string_view name = __PRETTY_FUNCTION__;
    constexpr auto start = name.find("value = ");
    constexpr auto end = name.rfind("]", start);
    constexpr auto tpname = name.substr(start + 8, end - start - 8);
    constexpr auto base = tpname.rfind("::");
    if constexpr (base != std::string_view::npos)
        return string_literal<tpname.size() - base - 2>{
            tpname.substr(base + 2)};
    else
        return string_literal<tpname.size()>{tpname};
}

/// A magic type to represent any non-type-template parameter
template <typename T> struct any_value {
    using type = T;
    type value;
    constexpr any_value(T v) : value{v} {}
    constexpr any_value(const char (&str)[sizeof(T)]) : value{str} {}
};
template <size_t N>
any_value(const char (&str)[N]) -> any_value<string_literal<N - 1>>;
template <typename T> any_value(T) -> any_value<T>;

template <auto msg> struct CompileError {
#if __cpp_static_assert >= 202306L
    static_assert(false, msg);
#elif defined(__clang__)
    using split_msg = decltype(string_as_sequence<msg>());
    static_assert(sizeof(split_msg) == 0);
#else
    struct _undef; // not defined to trigger error
    static_assert(_undef{});
#endif
};
} // namespace ctslrp::details

namespace ctslrp {
inline namespace literals {
template <details::string_literal str> constexpr auto operator""_raw() {
    return str;
}
} // namespace literals
} // namespace ctslrp
