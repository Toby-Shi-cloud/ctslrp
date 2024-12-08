#pragma once

#include "utils/string.hpp"
#include <ctre.hpp>
#include <string_view>

namespace ctslrp::details {
constexpr size_t const_hash(const char *input) noexcept {
    return *input ? static_cast<size_t>(*input) + 33 * const_hash(input + 1) : 5381;
}

struct TokenWrapper {
    using match_method_t = std::string_view (*)(std::string_view);

    std::string_view name;
    match_method_t match_method;

    constexpr TokenWrapper() noexcept = default;
    constexpr TokenWrapper(const char *name) noexcept : name(name), match_method(nullptr) {}
    constexpr TokenWrapper(std::string_view name) noexcept : name(name), match_method(nullptr) {}
    constexpr TokenWrapper(std::string_view name, match_method_t match) noexcept
        : name(name), match_method(match) {}

    constexpr auto match(std::string_view sv) const noexcept {
        return match_method ? match_method(sv) : sv.starts_with(name) ? name : "";
    }

    constexpr bool operator==(const TokenWrapper &other) const noexcept {
        return name == other.name && match_method == other.match_method;
    }
    constexpr auto operator<=>(const TokenWrapper &other) const noexcept {
        return name <=> other.name;
    }
};
} // namespace ctslrp::details

namespace std {
template <> struct hash<ctslrp::details::TokenWrapper> {
    constexpr size_t operator()(const ctslrp::details::TokenWrapper &token) const noexcept {
        return ctslrp::details::const_hash(token.name.data());
    }
};
} // namespace std

namespace ctslrp::literals {
template <ctslrp::utils::string_literal input> constexpr auto operator""_r() {
    constexpr auto lambda = [](std::string_view sv) -> std::string_view {
        auto match = ctre::starts_with<input>(sv);
        if (match) return match.to_view();
        return "";
    };
    return details::TokenWrapper{input.sv(), lambda};
}
} // namespace ctslrp::literals

namespace ctslrp::details {
using namespace ctslrp::literals;
};

namespace ctslrp::utils {
using namespace ctslrp::literals;
};
