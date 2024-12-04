#pragma once

#include "type_map.hpp"
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
        if (t.id == size_t(-1)) return os << "ε";
        if (t.id == size_t(-2)) return os << "#";
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
static constexpr Terminal epsilon = Terminal{static_cast<size_t>(-1)};
static constexpr Terminal sharp = Terminal{static_cast<size_t>(-2)};
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
template <NonTerminal start_symbol, typename... Rules>
struct SimplifiedRuleTable;
/// The LR(0) Item
template <typename Rule, size_t idx> struct LRItem;
/// The LR(0) Item Set
template <typename... Items> struct LRItemSet;
/// The Terminal Set (First/Follow Set)
template <Terminal... ts> struct TerminalSet;
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
    template <size_t idx>
    using statement_type = std::tuple_element_t<idx, decltype(statements)>;

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

template <NonTerminal start_symbol, typename... Rules>
struct SimplifiedRuleTable {
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
    using extended_rule = SimplifiedRule<extended_grammar, start_symbol>;
    using extended_item = LRItem<extended_rule, 0>;

    struct ClosureHelper {
        template <typename Set> static constexpr auto calc() noexcept {
            return Set{};
        }

        template <typename Set, typename... items, typename... rest>
        static constexpr auto calc(std::tuple<items...>, rest...) noexcept {
            return calc<Set>(items{}..., rest{}...);
        }

        template <typename Set, typename item, typename... rest,
                  typename = std::enable_if_t<is_lr_item_v<item>>>
        static constexpr auto calc(item, rest...) noexcept {
            if constexpr (Set::template contains<item>()) {
                return calc<Set>(rest{}...);
            } else {
                using S = decltype(Set{} + item{});
                if constexpr (std::is_same_v<decltype(item::next_symbol()),
                                             NonTerminal>) {
                    return calc<S>(rest{}...,
                                   get_items_of<item::next_symbol()>());
                } else {
                    return calc<S>(rest{}...);
                }
            }
        }
    };
    template <typename... Items>
    using Closure =
        decltype(ClosureHelper::template calc<LRItemSet<>>(Items{}...));
    template <typename ItemSet, auto symbol>
    using Goto = Closure<decltype(ItemSet::template next<symbol>())>;
    using StartClosure = Closure<extended_item>;

    struct FirstSetHelper {
        template <typename TMap, typename Rule>
        static constexpr auto calc_value() noexcept {
            /// key of this rule's non-terminal
            using key = value_wrapper<Rule::non_terminal>;
            /// current first set
            using current = TMap::template ValueOrDefault<key, TerminalSet<>>;
            if constexpr (Rule::statement_size == 0) {
                return current::template add<epsilon>();
            } else {
                constexpr auto first = std::get<0>(Rule::statements);
                if constexpr (std::is_same_v<std::decay_t<decltype(first)>,
                                             Terminal>) {
                    return current::template add<first>();
                } else {
                    using that = value_wrapper<first>;
                    using set =
                        TMap::template ValueOrDefault<that, TerminalSet<>>;
                    return current{} + set{};
                }
            }
        }

        template <typename TMap, size_t idx = 0>
        static constexpr auto calc_impl() noexcept {
            if constexpr (idx == rule_size) {
                using key = value_wrapper<extended_grammar>;
                using ret = decltype(calc_value<TMap, extended_rule>());
                using new_map = decltype(TMap::template set<key, ret>());
                return new_map{};
            } else {
                using key = value_wrapper<rule_t<idx>::non_terminal>;
                using ret = decltype(calc_value<TMap, rule_t<idx>>());
                using new_map = decltype(TMap::template set<key, ret>());
                return calc_impl<new_map, idx + 1>();
            }
        }

        template <typename TMap = TypeMap<>, size_t depth = 0>
        static constexpr auto calc() noexcept {
            static_assert(depth < 5, "FirstSet calculation too deep");
            using ret = decltype(calc_impl<TMap>());
            if constexpr (std::is_same_v<TMap, ret>) {
                return ret{};
            } else {
                return calc<ret, depth + 1>();
            }
        }
    };
    using FirstSet = decltype(FirstSetHelper::calc());

    struct FollowSetHelper {
        template <typename TMap, typename Rule,
                  size_t idx = Rule::statement_size,
                  typename first_set = TerminalSet<>, bool nullable = true>
        static constexpr auto calc_value() noexcept {
            if constexpr (idx == 0) {
                return TMap{};
            } else {
                constexpr auto symbol = std::get<idx - 1>(Rule::statements);
                using symbol_t = std::decay_t<decltype(symbol)>;
                if constexpr (std::is_same_v<symbol_t, Terminal>) {
                    return calc_value<TMap, Rule, idx - 1, TerminalSet<symbol>,
                                      false>();
                } else {
                    using key = value_wrapper<symbol>;
                    using key_first = FirstSet::template ValueOf<key>;
                    using current =
                        TMap::template ValueOrDefault<key, TerminalSet<>>;
                    using new_set = decltype(current{} + first_set{});
                    constexpr bool next_nullable = key_first::contains(epsilon);
                    using next_first = std::conditional_t<
                        next_nullable,
                        decltype(decltype(first_set{} + key_first{})::
                                     template remove<epsilon>()),
                        key_first>;
                    if constexpr (nullable) {
                        using follow_of_rule = TMap::template ValueOrDefault<
                            value_wrapper<Rule::non_terminal>, TerminalSet<>>;
                        using new_new_set =
                            decltype(new_set{} + follow_of_rule{});
                        return calc_value<
                            decltype(TMap::template set<key, new_new_set>()),
                            Rule, idx - 1, next_first, next_nullable>();
                    } else {
                        return calc_value<
                            decltype(TMap::template set<key, new_set>()), Rule,
                            idx - 1, next_first, next_nullable>();
                    }
                }
            }
        }

        template <typename TMap, size_t idx = 0>
        static constexpr auto calc_impl() noexcept {
            if constexpr (idx == rule_size) {
                using key = value_wrapper<extended_grammar>;
                using new_map = decltype(calc_value<TMap, extended_rule>());
                return new_map{};
            } else {
                using key = value_wrapper<rule_t<idx>::non_terminal>;
                using new_map = decltype(calc_value<TMap, rule_t<idx>>());
                return calc_impl<new_map, idx + 1>();
            }
        }

        template <typename TMap = TypeMap<TypeMapEntry<
                      value_wrapper<extended_grammar>, TerminalSet<sharp>>>,
                  size_t depth = 0>
        static constexpr auto calc() noexcept {
            static_assert(depth < 5, "FollowSet calculation too deep");
            using ret = decltype(calc_impl<TMap>());
            if constexpr (std::is_same_v<TMap, ret>) {
                return ret{};
            } else {
                return calc<ret, depth + 1>();
            }
        }
    };
    using FollowSet = decltype(FollowSetHelper::calc());

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

template <Terminal... ts> struct TerminalSet {
    static constexpr size_t size() noexcept { return sizeof...(ts); }
    static constexpr bool contains(Terminal t) noexcept {
        return ((t == ts) || ...);
    }
    template <Terminal t> constexpr static auto remove() noexcept {
        if constexpr (!contains(t)) {
            return TerminalSet<ts...>{};
        } else {
            return remove_impl<t, ts...>();
        }
    }
    template <Terminal t> constexpr static auto add() noexcept {
        if constexpr (contains(t)) {
            return TerminalSet<ts...>{};
        } else {
            return TerminalSet<ts..., t>{};
        }
    }
    constexpr auto operator+(TerminalSet<>) const noexcept { return *this; }
    template <Terminal o, Terminal... os>
    constexpr auto operator+(TerminalSet<o, os...>) const noexcept {
        return add<o>() + TerminalSet<os...>{};
    }

 private:
    template <Terminal> constexpr static auto remove_impl() noexcept {
        return TerminalSet<>{};
    }
    template <Terminal value, Terminal item, Terminal... rest>
    constexpr static auto remove_impl() noexcept {
        if constexpr (value == item) {
            return TerminalSet<rest...>{};
        } else {
            return TerminalSet<item>{} + remove_impl<value, rest...>();
        }
    }
};

} // namespace ctslrp::details::slr_gen
