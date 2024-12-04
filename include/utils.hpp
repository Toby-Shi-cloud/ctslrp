#pragma once

#include <algorithm>
#include <ostream>
#include <string_view>
#include <type_traits>
#include <utility>

namespace ctslrp::details {
/// a value wrapper of any type (convert constexpr value to type)
template <auto v> struct value_wrapper {
    static constexpr auto value = v;
};

/// a sequence of any type
template <auto... Ss> struct any_sequence {
    static constexpr auto size = sizeof...(Ss);
};

template <auto... I, auto... J>
constexpr auto operator+(any_sequence<I...>, any_sequence<J...>) {
    return any_sequence<I..., J...>{};
}

template <typename T, size_t... I, size_t... J>
constexpr auto operator+(std::integer_sequence<T, I...>,
                         std::integer_sequence<T, J...>) {
    return std::integer_sequence<T, I..., J...>{};
}

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
        std::copy_n(str.data(), N, m_data);
        m_data[N] = '\0';
    }

    static constexpr size_t npos = -1;
    constexpr const char *data() const { return m_data; }
    constexpr std::string_view sv() const { return {m_data, N}; }
    static constexpr size_t size() { return N; }

    constexpr char operator[](size_t i) const { return m_data[i]; }

    constexpr size_t find(char ch, size_t pos = 0) const {
        for (size_t i = pos; i < N; ++i)
            if (m_data[i] == ch) return i;
        return npos;
    }

    template <size_t pos, size_t length = npos> constexpr auto substr() const {
        constexpr size_t real_length = std::min(N - pos, length);
        string_literal<real_length> result;
        std::copy_n(m_data + pos, real_length, result.m_data);
        return result;
    }

    template <size_t M>
    constexpr string_literal<N + M>
    operator+(const string_literal<M> &rhs) const {
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
