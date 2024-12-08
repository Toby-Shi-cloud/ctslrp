#pragma once

#include "gen/regex.hpp"
#include "slr/symbol.hpp"
#include "utils/adaptor.hpp"
#include <ostream>
#include <string>
#include <string_view>
#include <vector>

namespace ctslrp::details {
struct Token {
    Terminal token_id = literals::error;
    std::string_view value = "";
    size_t line = 0, column = 0;
    std::string to_string() const noexcept {
        return std::string(value) + " at " + std::to_string(line) + ":" + std::to_string(column);
    }
    friend std::ostream &operator<<(std::ostream &os, const Token &token) {
        return os << "Token(" << token.token_id << "): " << token.value << " at " << token.line
                  << ":" << token.column;
    }
};

template <size_t N> class Lexer {
    utils::wrapped_array<TokenWrapper, N> tokens;
    TokenWrapper::match_method_t skip_token;

    constexpr static auto default_skip_token = "\\s"_r.match_method;

    class LexerInternal {
        const Lexer *lex;
        size_t line = 1, column = 1;

        constexpr void process_position(std::string_view match) noexcept {
            for (char c : match) {
                if (c == '\n') {
                    line++;
                    column = 1;
                } else {
                    column++;
                }
            }
        }

        constexpr void skip(std::string_view &input) noexcept {
            auto match = lex->skip_token(input);
            process_position(match);
            input.remove_prefix(match.size());
        }

     public:
        constexpr LexerInternal(const Lexer *lex) noexcept : lex(lex) {}

        /// Get next token
        constexpr Token next(std::string_view &input) noexcept {
            skip(input);
            if (input.empty()) return {literals::sharp, "<end-of-file>", line, column};
            Token result{literals::error, "", line, column};
            for (auto &token : lex->tokens) {
                auto current = token.match(input);
                if (current.size() > result.value.size()) {
                    result.token_id = Terminal{size_t(&token - &lex->tokens[0])};
                    result.value = current;
                }
            }
            if (result.token_id == literals::error) {
                result.value = input.substr(0, 1);
            }
            process_position(result.value);
            input.remove_prefix(result.value.size());
            return result;
        }
    };

 public:
    constexpr Lexer(const std::vector<TokenWrapper> &tokens,
                    TokenWrapper::match_method_t skip_token = default_skip_token) noexcept
        : tokens(tokens), skip_token(skip_token) {}

    constexpr auto get_lexer() const noexcept { return LexerInternal{this}; }

    constexpr auto tokenize(std::string_view input) const noexcept {
        auto lexer = get_lexer();
        std::vector<Token> tokens;
        while (input.size() > 0) {
            auto token = lexer.next(input);
            tokens.push_back(token);
            if (token.token_id == literals::error) break;
            if (token.token_id == literals::sharp) break;
        }
        return tokens;
    }
};
} // namespace ctslrp::details
