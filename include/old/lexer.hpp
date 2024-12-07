#pragma once

#include "regex.hpp"
#include "utils.hpp"
#include <algorithm>
#include <array>
#include <ostream>
#include <string>
#include <string_view>
#include <tuple>
#include <utility>
#include <vector>

namespace ctslrp::details {
struct Token {
    static constexpr size_t npos = -1;
    static constexpr size_t error = -2;
    size_t token_id = npos;
    std::string_view value = "";
    size_t line = 0, column = 0;
    constexpr std::string to_string() const noexcept {
        return std::string(value) + " at " + constexpr_to_string(line) + ":" +
               constexpr_to_string(column);
    }
    friend std::ostream &operator<<(std::ostream &os, const Token &token) {
        return os << "Token(" << token.token_id << "): " << token.value
                  << " at " << token.line << ":" << token.column;
    }
};

template <typename Skip, typename... Re> class Lexer {
    static_assert(is_regex_v<Skip>, "Skip must be regex");
    static_assert((is_regex_v<Re> && ...), "All types must be regex");

    template <size_t idx>
    using RegexExp = std::tuple_element_t<idx, std::tuple<Re...>>;

    template <size_t idx = 0>
    constexpr static Token match(std::string_view str) noexcept {
        if constexpr (idx == sizeof...(Re))
            return Token{Token::error, str.substr(0, 1)};
        else {
            auto matched = RegexExp<idx>::starts_with(str);
            Token rest = match<idx + 1>(str);
            if (!matched || matched.size() < rest.value.size()) return rest;
            return Token{idx, matched.to_view()};
        }
    }

    constexpr static std::string_view skip(std::string_view str) noexcept {
        auto match = Skip::starts_with(str);
        return match ? match.to_view() : "";
    }

    constexpr static std::pair<size_t, size_t>
    get_line_column(std::string_view str) noexcept {
        size_t line = std::count(str.begin(), str.end(), '\n') + 1;
        size_t column = str.size() - str.find_last_of('\n');
        return {line, column};
    }

 public:
    constexpr Lexer() noexcept = default;
    constexpr Lexer(Skip, std::tuple<Re...>) noexcept {}
    constexpr Lexer(Skip, Re...) noexcept {}

    /// Get next token
    constexpr static Token next(std::string_view input, size_t &pos) noexcept {
        pos += skip(input.substr(pos)).size();
        auto [line, column] = get_line_column(input.substr(0, pos));
        if (pos >= input.size()) return Token{Token::npos, "EOF", line, column};
        Token token = match(input.substr(pos));
        token.line = line;
        token.column = column;
        pos += token.value.size();
        return token;
    }

    /// Get tokens size in input, -1 if error.
    /// note: this function is usually not used unless you want to know the size
    /// in compile time.
    constexpr static size_t tokens_size(std::string_view input) noexcept {
        size_t pos = 0;
        size_t size = 0;
        while (pos < input.size()) {
            auto token = next(input, pos);
            if (token.token_id == Token::npos) break;
            size++;
        }
        if (pos < input.size()) return Token::npos;
        return size;
    }

    /// Tokenize input string in compile time.
    template <string_literal input> constexpr static auto tokenize() noexcept {
        static_assert(tokens_size(input) != Token::npos, "Invalid input");
        std::array<Token, tokens_size(input)> tokens;
        size_t pos = 0;
        size_t idx = 0;
        while (pos < input.size()) {
            auto token = next(input, pos);
            if (token.token_id == Token::npos) break;
            tokens[idx++] = token;
        }
        return tokens;
    }

    /// Tokenize input string in runtime.
    static auto tokenize(std::string_view input) noexcept {
        std::vector<Token> tokens;
        size_t pos = 0;
        while (pos < input.size()) {
            auto token = next(input, pos);
            if (token.token_id == Token::npos) break;
            tokens.push_back(token);
        }
        return tokens;
    }
};
template <typename Skip, typename... Re>
Lexer(Skip, std::tuple<Re...>) -> Lexer<Skip, Re...>;
template <typename Skip, typename... Re>
Lexer(Skip, Re...) -> Lexer<Skip, Re...>;

} // namespace ctslrp::details
