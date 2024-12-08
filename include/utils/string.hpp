#pragma once

#include <algorithm>
#include <ctre.hpp>
#include <string_view>

namespace ctslrp::utils {
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
    constexpr explicit string_literal(std::string_view str) {
        size_t len = std::min(N, str.size());
        std::copy_n(str.data(), len, m_data);
        for (; len < N + 1; ++len)
            m_data[len] = 0;
    }

    constexpr const char *data() const noexcept { return m_data; }
    static constexpr size_t size() noexcept { return N; }

    constexpr std::string_view sv() const noexcept { return {m_data, N}; }
    constexpr operator std::string_view() const noexcept { return sv(); }

    /// cast ctslrp::utils::string_literal to ctll::fixed_string
    constexpr operator ctll::fixed_string<N>() const noexcept {
        return ctll::fixed_string<N>(ctll::construct_from_pointer, data());
    }

    constexpr char operator[](size_t i) const noexcept { return m_data[i]; }
};
template <size_t N> string_literal(const char (&)[N]) -> string_literal<N - 1>;
string_literal(char) -> string_literal<1>;
} // namespace ctslrp::utils

namespace ctll {
template <size_t N> fixed_string(ctslrp::utils::string_literal<N>) -> fixed_string<N>;
}
