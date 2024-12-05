#pragma once

#include "lexer.hpp"
#include "slr_action.hpp"
#include "utils.hpp"
#include <algorithm>
#include <array>
#include <cstddef>
#include <functional>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

namespace ctslrp::details {
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

    constexpr __attribute__((always_inline))
    std::variant<Result, string_literal<255>>
    parse_impl(std::string_view input) const noexcept {
        std::vector<StackItem> stack{{0, {}, {}}};
        size_t state = 0, pos = 0;
        Token token = lexer.next(input, pos);
        while (true) {
            const auto &action = table.actions[state][token.token_id];
            switch (action.type) {
            case Action::ERROR: {
                auto error_msg = "unexpected token: " + token.to_string();
                return string_literal<255>(error_msg);
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
                    return string_literal<255>("unexpected goto error");
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
        auto result = parse_impl(input);
        if (std::holds_alternative<Result>(result)) {
            return std::get<Result>(std::move(result));
        } else {
            const char *msg = std::get<string_literal<255>>(result).data();
            throw std::runtime_error(msg);
        }
    }

    constexpr static auto get_slr_table() noexcept { return table; }
};
} // namespace ctslrp::details
