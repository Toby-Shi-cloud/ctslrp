#pragma once

#include "gen/errors.hpp"
#include "gen/regex.hpp"
#include "slr/gen_action.hpp"
#include "slr/lexer.hpp"
#include "slr/symbol.hpp"
#include <__algorithm/ranges_max.h>
#include <__ranges/transform_view.h>
#include <algorithm>
#include <tuple>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

namespace ctslrp::details {
using Statement = std::variant<NonTerminal, TokenWrapper>;
template <size_t> struct DefinedRule;
template <typename, typename, size_t> struct BindedRule;

template <size_t N> struct DefinedRule {
    NonTerminal symbol;
    std::array<Statement, N> statements;
    template <typename, typename, size_t> friend class BindedRule;

    constexpr static size_t size() noexcept { return N; }

    constexpr auto get_token_used() const noexcept {
        std::vector<TokenWrapper> result;
        for (const auto &stmt : statements) {
            if (auto *token = std::get_if<TokenWrapper>(&stmt)) {
                result.push_back(*token);
            }
        }
        return result;
    }

    constexpr auto get_symbol_used() const noexcept {
        std::vector<NonTerminal> result;
        for (const auto &stmt : statements) {
            if (auto *symbol = std::get_if<NonTerminal>(&stmt)) {
                result.push_back(*symbol);
            }
        }
        return result;
    }

    template <typename T, typename Func> constexpr auto bind(Func &&func) const noexcept {
        return BindedRule<T, Func, N>{*this, std::forward<Func>(func)};
    }
};

template <typename T, typename Func, size_t N> struct BindedRule {
    using BindedType = T;
    DefinedRule<N> rule;
    Func func;

    constexpr static size_t size() noexcept { return N; }
    constexpr auto &get_statements() const noexcept { return rule.statements; }
    constexpr auto get_defined_symbol() const noexcept { return rule.symbol; }
    constexpr auto get_token_used() const noexcept { return rule.get_token_used(); }
    constexpr auto get_symbol_used() const noexcept { return rule.get_symbol_used(); }
};

template <typename T>
concept same_as_rule = requires(T t) {
    typename T::BindedType;
    std::same_as<decltype(t.rule), DefinedRule<T::size()>>;
    { t.func };
};

template <same_as_rule... Rules> struct BindedRuleTable {
    std::tuple<Rules...> rules;
    constexpr static size_t size() noexcept { return sizeof...(Rules); }
    constexpr BindedRuleTable(const Rules &...rules) noexcept : rules(rules...) {}
    constexpr BindedRuleTable(const std::tuple<Rules...> &rules) noexcept : rules(rules) {}

    template <same_as_rule... Rest>
    constexpr auto operator+(const BindedRuleTable<Rest...> &rhs) const noexcept {
        return BindedRuleTable(std::tuple_cat(rules, rhs.rules));
    }

 private:
    constexpr auto symbol_defined() const noexcept {
        auto symbols_set = std::vector<NonTerminal>{};
        std::apply(
            [&](const auto &...rule) { (symbols_set.push_back(rule.get_defined_symbol()), ...); },
            rules);
        std::ranges::sort(symbols_set);
        symbols_set.erase(std::unique(symbols_set.begin(), symbols_set.end()), symbols_set.end());
        return symbols_set;
    }

    constexpr void check_defined() const {
        std::apply(
            [symbols = symbol_defined()](const auto &...rule) {
                (std::ranges::for_each(rule.get_symbol_used(),
                                       [&](const auto &symbol) {
                                           auto it = std::lower_bound(symbols.begin(),
                                                                      symbols.end(), symbol);
                                           if (it != symbols.end() && *it == symbol) return;
                                           throw undefined_symbol_error(symbol.id);
                                       }),
                 ...);
            },
            rules);
    }

    constexpr size_t max_symbol_used() const noexcept {
        size_t max = 0;
        std::apply(
            [&](const auto &...rule) {
                std::vector<NonTerminal> temp;
                ((rule.get_symbol_used().swap(temp),
                  temp.empty() || (max = std::max(max, std::ranges::max(temp).id))),
                 ...);
            },
            rules);
        return max;
    }

    constexpr static size_t max_rule_length() noexcept {
        size_t max = 0;
        ((max = std::max(max, Rules::size())), ...);
        return max;
    }

    constexpr auto get_all_terminals() const noexcept {
        auto terminals = std::vector<TokenWrapper>{};
        std::apply(
            [&](const auto &...rule) {
                (std::ranges::for_each(rule.get_token_used(),
                                       [&](const auto &tk) { terminals.push_back(tk); }),
                 ...);
            },
            rules);
        std::ranges::sort(terminals);
        terminals.erase(std::unique(terminals.begin(), terminals.end()), terminals.end());
        return terminals;
    }

    constexpr auto make_table(NonTerminal start_symbol) const noexcept {
        constexpr auto M = max_rule_length();
        const auto terminals = get_all_terminals();
        auto index_of = [&](const TokenWrapper &tk) -> size_t {
            return std::lower_bound(terminals.begin(), terminals.end(), tk) - terminals.begin();
        };
        auto transform = std::views::transform([&](const Statement &s) -> SymbolT {
            if (auto t = std::get_if<NonTerminal>(&s)) return *t;
            return Terminal{index_of(std::get<TokenWrapper>(s))};
        });
        auto extended_symbol = NonTerminal{max_symbol_used() + 1};
        auto extended_grammar = std::vector<SymbolT>{start_symbol};
        auto extended_rule = SimplifiedRule<M>{extended_symbol, extended_grammar};
        return std::apply(
            [&](const auto &...rule) {
                return SimplifiedRuleTable<size() + 1, M>{
                    extended_symbol,
                    {extended_rule, SimplifiedRule<M>{rule.get_defined_symbol(),
                                                      transform(rule.get_statements())}...}};
            },
            rules);
    }

 public:
    constexpr auto get_terminals_count() const noexcept { return get_all_terminals().size(); }

    template <size_t N> constexpr auto compile(NonTerminal start_symbol) const {
        check_defined();
        auto lexer = Lexer<N>{get_all_terminals()};
        auto table = make_table(start_symbol);
        return std::make_pair(lexer, table);
    }
};
} // namespace ctslrp::details

namespace ctslrp {
template <typename SymbolEnum> class SyntaxRuleGenerator {
    static_assert(std::is_enum_v<SymbolEnum>, "SymbolEnum must be an enum class");

    constexpr static auto cast_statement(SymbolEnum value) noexcept {
        return details::NonTerminal{static_cast<size_t>(value)};
    }

    constexpr static auto
    cast_statement(std::convertible_to<details::TokenWrapper> auto value) noexcept {
        return details::TokenWrapper{value};
    }

    struct DeclaredSymbol {
        SymbolEnum symbol;

        /// Define a rule for the symbol
        constexpr auto define(auto... args) const noexcept {
            using DefinedRule = details::DefinedRule<sizeof...(args)>;
            return DefinedRule{cast_statement(symbol), {cast_statement(args)...}};
        }
    };

 public:
    /// Declare a symbol
    constexpr static auto decl(SymbolEnum symbol) noexcept { return DeclaredSymbol{symbol}; }

    /// Generate a table from rules
    constexpr auto operator()(details::same_as_rule auto &&...rules) const noexcept {
        return details::BindedRuleTable{std::forward<decltype(rules)>(rules)...};
    }

    /// Compile a table to a parser
    template <size_t N, typename BindedRuleTable>
    constexpr auto compile(const BindedRuleTable &table,
                           SymbolEnum start_symbol = static_cast<SymbolEnum>(0)) const {
        return table.template compile<N>(details::NonTerminal{static_cast<size_t>(start_symbol)});
    }
};
} // namespace ctslrp
