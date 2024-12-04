#pragma once

#include "utils.hpp"
#include <cstddef>
#include <ostream>
#include <tuple>
#include <type_traits>

namespace ctslrp::details::slr_gen {
struct Terminal {
    size_t id;
    constexpr bool operator==(Terminal other) const noexcept {
        return id == other.id;
    }
    constexpr bool operator!=(Terminal other) const noexcept {
        return id != other.id;
    }
    friend std::ostream &operator<<(std::ostream &os, Terminal t) {
        return os << "T" << t.id;
    }
};
struct NonTerminal {
    size_t id;
    constexpr bool operator==(NonTerminal other) const noexcept {
        return id == other.id;
    }
    constexpr bool operator!=(NonTerminal other) const noexcept {
        return id != other.id;
    }
    friend std::ostream &operator<<(std::ostream &os, NonTerminal t) {
        return os << "S" << t.id;
    }
};
constexpr bool operator==(Terminal, NonTerminal) noexcept { return false; }
constexpr bool operator==(NonTerminal, Terminal) noexcept { return false; }
constexpr bool operator!=(Terminal, NonTerminal) noexcept { return true; }
constexpr bool operator!=(NonTerminal, Terminal) noexcept { return true; }
} // namespace ctslrp::details::slr_gen

/// forward declaration
namespace ctslrp::details::slr_gen {
/// a simplified version of syntax rule
template <NonTerminal symbol, auto... exps> struct SimplifiedRule;
/// a simplified version of syntax rule table
template <typename... Rules> struct SimplifiedRuleTable;
/// The LR(0) Item
template <typename Rule, size_t idx> struct LRItem;
/// The LR(0) Item Set
template <typename... Items> struct LRItemSet;
} // namespace ctslrp::details::slr_gen

// type traits
namespace ctslrp::details::slr_gen {
/// check if the type is SimplifiedRule
template <typename T> struct is_simplified_rule : std::false_type {};
template <NonTerminal symbol, auto... statements>
struct is_simplified_rule<SimplifiedRule<symbol, statements...>>
    : std::true_type {};
template <typename T>
constexpr bool is_simplified_rule_v = is_simplified_rule<T>::value;
/// check if the type is LRItem
template <typename T> struct is_lr_item : std::false_type {};
template <typename Rule, size_t idx>
struct is_lr_item<LRItem<Rule, idx>> : std::true_type {};
template <typename T> constexpr bool is_lr_item_v = is_lr_item<T>::value;
} // namespace ctslrp::details::slr_gen

namespace ctslrp::details::slr_gen {
template <NonTerminal symbol, auto... exps> struct SimplifiedRule {
    static_assert(((std::is_same_v<decltype(exps), Terminal> ||
                    std::is_same_v<decltype(exps), NonTerminal>)&&...),
                  "statements must be Terminal or NonTerminal");

    static constexpr NonTerminal non_terminal = symbol;
    static constexpr size_t statement_size = sizeof...(exps);
    static constexpr auto statements = std::make_tuple(exps...);

    constexpr SimplifiedRule() noexcept = default;
    template <auto... values>
    constexpr explicit SimplifiedRule(any_sequence<values...>) noexcept {}

    friend std::ostream &operator<<(std::ostream &os, SimplifiedRule rule) {
        os << rule.non_terminal << " ->";
        ((os << ' ' << exps), ...);
        return os;
    }
};
template <NonTerminal symbol, auto... statements>
SimplifiedRule(any_sequence<symbol, statements...>)
    -> SimplifiedRule<symbol, statements...>;

template <typename... Rules> struct SimplifiedRuleTable {
    static_assert((is_simplified_rule_v<Rules> && ...),
                  "Rules must be SimplifiedRule");

    template <size_t idx>
    using rule_t = std::tuple_element_t<idx, std::tuple<Rules...>>;
    static constexpr size_t rule_size = sizeof...(Rules);

    constexpr SimplifiedRuleTable() noexcept = default;

    static constexpr size_t max_symbol_id() noexcept {
        size_t max_id = 0;
        ((max_id = std::max(max_id, Rules::non_terminal.id)), ...);
        return max_id;
    }

    template <NonTerminal symbol>
    static constexpr auto get_rules_of() noexcept {
        return std::tuple_cat(
            std::conditional_t<symbol == Rules::non_terminal, std::tuple<Rules>,
                               std::tuple<>>{}...);
    }

    template <NonTerminal symbol>
    static constexpr auto get_items_of() noexcept {
        return std::tuple_cat(std::conditional_t<symbol == Rules::non_terminal,
                                                 std::tuple<LRItem<Rules, 0>>,
                                                 std::tuple<>>{}...);
    }

    static constexpr NonTerminal extended_grammar =
        NonTerminal{max_symbol_id() + 1};

    template <typename Set> static constexpr auto Closure() noexcept {
        return Set{};
    }

    template <typename Set, typename... items, typename... rest>
    static constexpr auto Closure(std::tuple<items...>, rest...) noexcept {
        return Closure<Set>(items{}..., rest{}...);
    }

    template <typename Set, typename item, typename... rest,
              typename = std::enable_if_t<is_lr_item_v<item>>>
    static constexpr auto Closure(item, rest...) noexcept {
        if constexpr (Set::template contains<item>()) {
            return Closure<Set>(rest{}...);
        } else {
            using S = decltype(Set{} + item{});
            if constexpr (std::is_same_v<decltype(item::next_symbol()),
                                         NonTerminal>) {
                return Closure<S>(rest{}...,
                                  get_items_of<item::next_symbol()>());
            } else {
                return Closure<S>(rest{}...);
            }
        }
    }

    template <NonTerminal grammar>
    static constexpr auto start_closure() noexcept {
        using extended_item =
            LRItem<SimplifiedRule<extended_grammar, grammar>, 0>;
        return Closure<LRItemSet<>>(extended_item{});
    }

    template <auto symbol, typename ItemSet>
    static constexpr auto Goto(ItemSet) noexcept {
        return Closure<LRItemSet<>>(ItemSet::template next<symbol>());
    }

    friend std::ostream &operator<<(std::ostream &os,
                                    SimplifiedRuleTable table) {
        os << "SimplifiedRuleTable:\n";
        ((os << Rules{} << '\n'), ...);
        return os;
    }
};

template <typename Rule, size_t idx> struct LRItem {
    static_assert(is_simplified_rule_v<Rule>, "Rule must be SimplifiedRule");
    static_assert(idx <= Rule::statement_size, "idx out of range");

    using rule_t = Rule;
    static constexpr size_t index = idx;
    static constexpr auto symbol = Rule::non_terminal;

    /// get the next symbol
    static constexpr auto next_symbol() noexcept {
        if constexpr (index == Rule::statement_size) {
            return /* void */;
        } else {
            return std::get<index>(Rule::statements);
        }
    }

    /// determine if the symbol is matched
    template <auto symbol> static constexpr bool match_next() noexcept {
        if constexpr (index == Rule::statement_size) {
            return false;
        } else {
            return symbol == std::get<index>(Rule::statements);
        }
    }

    /// get the next item if the symbol is matched
    template <auto forward> static constexpr auto next() noexcept {
        if constexpr (index == Rule::statement_size) {
            return std::tuple<>{};
        } else if constexpr (forward == std::get<index>(Rule::statements)) {
            return std::tuple<LRItem<Rule, index + 1>>{};
        } else {
            return std::tuple<>{};
        }
    }

    template <size_t output_idx = 0>
    friend std::ostream &operator<<(std::ostream &os, LRItem item) {
        if constexpr (output_idx == 0) {
            os << symbol << " ->";
        }
        if constexpr (output_idx == index) {
            os << " .";
        }
        if constexpr (output_idx < Rule::statement_size) {
            os << ' ' << std::get<output_idx>(Rule::statements);
            operator<< <output_idx + 1>(os, item);
        }
        return os;
    }
};

template <typename... Items> struct LRItemSet {
    static_assert((is_lr_item_v<Items> && ...), "Items must be LRItem");
    static constexpr size_t item_size = sizeof...(Items);

    template <auto symbol> static constexpr auto next() noexcept {
        return std::tuple_cat((Items::template next<symbol>())...);
    }

    template <typename Item> static constexpr bool contains() noexcept {
        return ((std::is_same_v<Items, Item>) || ...);
    }

    template <typename Item> constexpr auto operator+(Item) const noexcept {
        static_assert(!contains<Item>(), "Item already exists");
        return LRItemSet<Items..., Item>{};
    }

    friend std::ostream &operator<<(std::ostream &os, LRItemSet set) {
        os << "LRItemSet:\n";
        ((os << Items{} << '\n'), ...);
        return os;
    }
};
} // namespace ctslrp::details::slr_gen
