#pragma once

#include "lexer.hpp"
#include "slr_action.hpp"
#include <algorithm>
#include <array>
#include <cstddef>
#include <functional>
#include <ostream>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace ctslrp::details {
template <typename T> struct ParserResult {
    enum { ERROR, RESULT } type;
    union {
        T result;
        Token error;
    };

    constexpr ParserResult(T result) noexcept
        : type(RESULT), result(std::move(result)) {}
    constexpr ParserResult(Token error) noexcept
        : type(ERROR), error(std::move(error)) {}

    constexpr bool has_result() const noexcept { return type == RESULT; }
    constexpr bool has_error() const noexcept { return type == ERROR; }

    constexpr explicit operator bool() const noexcept { return has_result(); }
    constexpr bool operator!() const noexcept { return has_error(); }

    constexpr bool operator==(const T &other) const noexcept {
        return has_result() && result == other;
    }
    constexpr bool operator!=(const T &other) const noexcept {
        return !(*this == other);
    }

    constexpr std::string to_error_string() const noexcept {
        return has_error() ? "error: " + error.to_string() : "";
    }

    friend std::ostream &operator<<(std::ostream &os,
                                    const ParserResult &result) {
        if (result)
            return os << "ParserResult: " << result.result;
        else
            return os << "ParserResult: " << result.to_error_string();
    }
};

/// The SLR(1) Parser.
/// template parameters:
/// * T: the type of the token value. (usually std::variant)
/// * lexer: the lexer.
/// * rule_reduce_to: the symbol id that the rule reduce to.
/// * rule_sizes: the length of syntax rules.
/// * table: the SLR(1) action/goto table.
/// * Func: the reduce function type.
template <typename Result, typename Data, Lexer lexer,
          std::array rule_reduce_to, std::array rule_sizes,
          LRActionGotoTable table, typename BindedRuleTable>
class Parser {
    BindedRuleTable binded_rule_table;

    template <size_t... Is>
    static constexpr auto
    get_reduce_functions(std::index_sequence<Is...>) noexcept {
        return std::array{&BindedRuleTable::template wrapper_function<Is>...};
    }
    static constexpr auto reduce_functions =
        get_reduce_functions(std::make_index_sequence<rule_sizes.size()>());

    struct Symbol {
        bool is_terminal = true;
        size_t id = Token::npos;

        constexpr bool operator==(Symbol other) const noexcept {
            return is_terminal == other.is_terminal && id == other.id;
        }
        constexpr bool operator!=(Symbol other) const noexcept {
            return is_terminal != other.is_terminal || id != other.id;
        }
    };

    struct StackItem {
        size_t state;
        Symbol symbol;
        Data data;
    };

    constexpr __attribute__((always_inline)) ParserResult<Result>
    parse_impl(std::string_view input) const {
        std::vector<StackItem> stack{{0, {}, {}}};
        size_t state = 0, pos = 0;
        Token token = lexer.next(input, pos);
        while (true) {
            if (token.token_id == Token::error) return token;
            const auto &action = table.actions[state][token.token_id];
            switch (action.type) {
            case Action::ERROR: {
                return token;
            }
            case Action::SHIFT: {
                stack.emplace_back(action.idx, Symbol{true, token.token_id},
                                   token);
                state = action.idx;
                token = lexer.next(input, pos);
                break;
            }
            case Action::REDUCE: {
                auto rule_idx = action.idx;
                auto rule_size = rule_sizes[rule_idx];
                auto symbol_id = rule_reduce_to[rule_idx];
                std::vector<Data> args;
                args.reserve(rule_size);
                for (size_t i = 0; i < rule_size; i++) {
                    args.push_back(std::move(stack.back().data));
                    stack.pop_back();
                }
                auto result = std::invoke(reduce_functions[rule_idx],
                                          binded_rule_table, std::move(args));
                state = stack.back().state;
                state = table.gotos[state][symbol_id].idx;
                stack.emplace_back(state, Symbol{false, symbol_id},
                                   std::move(result));
                if (state == Goto::error)
                    throw std::runtime_error("Goto error");
                break;
            }
            case Action::ACCEPT: {
                return get<Result>(std::move(stack.back().data));
            }
            }
        }
    }

 public:
    constexpr Parser(BindedRuleTable binded_rule_table) noexcept
        : binded_rule_table(std::move(binded_rule_table)) {}

    constexpr auto parse(std::string_view input) const {
        return parse_impl(input);
    }

    constexpr static auto get_slr_table() noexcept { return table; }
};
} // namespace ctslrp::details
