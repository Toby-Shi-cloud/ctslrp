#pragma once

#include "slr_action.hpp"
#include "type_map.hpp"
#include "utils.hpp"
#include <cstddef>
#include <optional>
#include <ostream>
#include <tuple>
#include <type_traits>
#include <variant>

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
        if (t.id == size_t(-1)) return os << "#";
        if (t.id == size_t(-2)) return os << "ε";
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
static constexpr Terminal sharp = Terminal{static_cast<size_t>(-1)};
static constexpr Terminal epsilon = Terminal{static_cast<size_t>(-2)};
constexpr bool operator==(Terminal, NonTerminal) noexcept { return false; }
constexpr bool operator==(NonTerminal, Terminal) noexcept { return false; }
constexpr bool operator!=(Terminal, NonTerminal) noexcept { return true; }
constexpr bool operator!=(NonTerminal, Terminal) noexcept { return true; }

template <typename T>
constexpr auto operator&(const std::optional<T> &x,
                         const std::optional<T> &y) noexcept {
    if (x.has_value()) return y;
    return std::nullopt;
}
template <typename T>
constexpr auto operator|(const std::optional<T> &x,
                         const std::optional<T> &y) noexcept {
    if (!x.has_value()) return y;
    return x;
}
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
/// The LR(0) Item Set Collection
template <typename... Sets> struct LRItemSetCollection;
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
/// check if the type is LRItemSet
template <typename T> struct is_lr_item_set : std::false_type {};
template <typename... Items>
struct is_lr_item_set<LRItemSet<Items...>> : std::true_type {};
template <typename T>
constexpr bool is_lr_item_set_v = is_lr_item_set<T>::value;
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

    static constexpr size_t max_token_id() noexcept {
        size_t max_id = 0;
        ((max_id = std::max(
              max_id, std::is_same_v<decltype(exps), Terminal> ? exps.id : 0)),
         ...);
        return max_id;
    }

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

    static constexpr size_t max_token_id() noexcept {
        size_t max_id = 0;
        ((max_id = std::max(max_id, Rules::max_token_id())), ...);
        return max_id;
    }

    template <NonTerminal symbol>
    static constexpr auto get_rules_of() noexcept {
        static_assert(((symbol == Rules::non_terminal) || ...),
                      "undefined symbols...");
        return std::tuple_cat(
            std::conditional_t<symbol == Rules::non_terminal, std::tuple<Rules>,
                               std::tuple<>>{}...);
    }

    template <NonTerminal symbol>
    static constexpr auto get_items_of() noexcept {
        static_assert(((symbol == Rules::non_terminal) || ...),
                      "undefined symbols...");
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

        template <typename Set = LRItemSet<>, typename... items,
                  typename... rest>
        static constexpr auto calc(std::tuple<items...>, rest...) noexcept {
            return calc<Set>(items{}..., rest{}...);
        }

        template <typename Set = LRItemSet<>, typename item, typename... rest,
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
    using Closure = decltype(ClosureHelper::template calc(Items{}...));
    template <typename ItemSet, auto symbol>
    using Goto = decltype(ClosureHelper::template calc(
        ItemSet::template next<symbol>()));
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

    struct ItemSetCollectionHelper {
        template <typename Collection, typename set, typename... rest>
        static constexpr auto calc_impl(any_sequence<>) noexcept {
            return calc<Collection, rest...>();
        }

        template <typename Collection, typename set, typename... rest,
                  auto symbol, auto... others>
        static constexpr auto
        calc_impl(any_sequence<symbol, others...>) noexcept {
            return calc_impl<Collection, set, rest..., Goto<set, symbol>>(
                any_sequence<others...>{});
        }

        template <typename Collection> static constexpr auto calc() noexcept {
            return Collection{};
        }

        template <typename Collection, typename set, typename... rest>
        static constexpr auto calc() noexcept {
            constexpr auto symbols = set::next_symbols();
            using new_collection = decltype(Collection{} + set{});
            return calc_impl<new_collection, set, rest...>(symbols);
        }
    };
    using ItemSetCollection =
        decltype(ItemSetCollectionHelper::template calc<LRItemSetCollection<>,
                                                        StartClosure>());

    struct IndexOfHelper {
        template <typename rule> static constexpr auto index_of_impl() {
            static_assert(sizeof(rule) == 0, "rule not found!");
        }

        template <typename rule, typename cur, typename... rest>
        static constexpr auto index_of_impl() {
            if constexpr (std::is_same_v<rule, cur>) {
                return 0;
            } else {
                return 1 + index_of_impl<rule, rest...>();
            }
        }
    };

    template <typename rule> static constexpr auto index_of() noexcept {
        return IndexOfHelper::template index_of_impl<rule, Rules...,
                                                     extended_rule>();
    }

    static constexpr size_t state_size = ItemSetCollection::size();
    static constexpr size_t token_size = max_token_id() + 1;
    static constexpr size_t symbol_size = max_symbol_id() + 2;
    using ActionTable = details::ActionTable<state_size, token_size>;
    using GotoTable = details::GotoTable<state_size, symbol_size>;
    using LRActionGotoTable =
        details::LRActionGotoTable<state_size, token_size, symbol_size>;

    struct GenerateActionGotoTableHelper {
        template <size_t idx>
        static constexpr std::optional<Conflict>
        gen_impl_nxt(LRActionGotoTable &, any_sequence<>) noexcept {
            return std::nullopt;
        }
        template <size_t idx, NonTerminal A, auto... rest>
        static constexpr std::optional<Conflict>
        gen_impl_nxt(LRActionGotoTable &table,
                     any_sequence<A, rest...>) noexcept {
            using ItemSet = ItemSetCollection::template set_at<idx>;
            constexpr auto j =
                ItemSetCollection::template index_of<Goto<ItemSet, A>>();
            table.gotos[idx][A.id].idx = j;
            return gen_impl_nxt<idx>(table, any_sequence<rest...>{});
        }
        template <size_t idx, Terminal a, auto... rest>
        static constexpr std::optional<Conflict>
        gen_impl_nxt(LRActionGotoTable &table,
                     any_sequence<a, rest...>) noexcept {
            using ItemSet = ItemSetCollection::template set_at<idx>;
            constexpr auto j =
                ItemSetCollection::template index_of<Goto<ItemSet, a>>();
            if (table.actions[idx][a.id].type == Action::REDUCE) {
                return Conflict{Conflict::S_R,
                                idx,
                                a.id,
                                {Action::SHIFT, j},
                                table.actions[idx][a.id]};
            }
            table.actions[idx][a.id] = {Action::SHIFT, j};
            return gen_impl_nxt<idx>(table, any_sequence<rest...>{});
        }

        template <size_t i, size_t val, Terminal... follows>
        static constexpr std::optional<Conflict>
        gen_impl_reduce_set_follows(LRActionGotoTable &table,
                                    TerminalSet<follows...>) noexcept {
            return (
                (table.actions[i][follows.id].type != Action::ERROR
                     ? std::optional(Conflict{
                           table.actions[i][follows.id].type == Action::SHIFT
                               ? Conflict::S_R
                               : Conflict::R_R,
                           i,
                           follows.id,
                           table.actions[i][follows.id],
                           {Action::REDUCE, val}})
                     : (table.actions[i][follows.id] = {Action::REDUCE, val},
                        std::nullopt)) |
                ...);
        }

        template <size_t idx>
        static constexpr std::optional<Conflict>
        gen_impl_reduce(LRActionGotoTable &, std::tuple<>) noexcept {
            return std::nullopt;
        }
        template <size_t idx, typename rule, typename... rest>
        static constexpr std::optional<Conflict>
        gen_impl_reduce(LRActionGotoTable &table,
                        std::tuple<rule, rest...>) noexcept {
            constexpr auto val = index_of<rule>();
            using follow_set =
                FollowSet::template ValueOf<value_wrapper<rule::non_terminal>>;
            auto conflict =
                gen_impl_reduce_set_follows<idx, val>(table, follow_set{});
            if (conflict) return conflict;
            return gen_impl_reduce<idx>(table, std::tuple<rest...>{});
        }

        template <size_t idx>
        static constexpr std::optional<Conflict>
        gen_impl(LRActionGotoTable &table) noexcept {
            if constexpr (idx == state_size) {
                return std::nullopt;
            } else {
                using ItemSet = ItemSetCollection::template set_at<idx>;
                constexpr auto next_symbols = ItemSet::next_symbols();
                auto conflict = gen_impl_nxt<idx>(table, next_symbols);
                if (conflict) return conflict;
                constexpr auto reduce_seq = ItemSet::reduce_seq();
                conflict = gen_impl_reduce<idx>(table, reduce_seq);
                if (conflict) return conflict;
                if constexpr (ItemSet::template contains<
                                  LRItem<extended_rule, 1>>()) {
                    table.actions[idx][-1] = Action{Action::ACCEPT};
                }
                return gen_impl<idx + 1>(table);
            }
        }

        static constexpr std::variant<Conflict, LRActionGotoTable>
        gen_constexpr() noexcept {
            LRActionGotoTable table{};
            auto conflict = gen_impl<0>(table);
            if (conflict) {
                return *conflict;
            } else {
                return table;
            }
        }

        static constexpr auto gen() noexcept {
            constexpr auto result = gen_constexpr();
            static_assert(std::holds_alternative<LRActionGotoTable>(result),
                          "Conflict detected!");
            return std::get<LRActionGotoTable>(result);
        }
    };

    static constexpr auto lr_action_goto_table =
        GenerateActionGotoTableHelper::gen();

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
    static constexpr bool reducable = index == Rule::statement_size;

    /// return Rule if idx == size
    static constexpr auto reduce_seq() noexcept {
        if constexpr (reducable) {
            return std::tuple<Rule>{};
        } else {
            return std::tuple<>{};
        }
    }

    /// get the next symbol
    static constexpr auto next_symbol() noexcept {
        if constexpr (index == Rule::statement_size) {
            return /* void */;
        } else {
            return std::get<index>(Rule::statements);
        }
    }

    /// get the next symbol as sequence
    static constexpr auto next_symbol_as_seq() noexcept {
        if constexpr (index == Rule::statement_size) {
            return any_sequence<>{};
        } else {
            return any_sequence<std::get<index>(Rule::statements)>{};
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

    /// get the next item if the symbol is matched
    template <auto symbol> static constexpr auto next() noexcept {
        return std::tuple_cat(Items::template next<symbol>()...);
    }

    /// get the next symbol (as any_sequence)
    static constexpr auto next_symbols() noexcept {
        return (Items::next_symbol_as_seq() + ...);
    }

    /// get all could reduced rules
    static constexpr auto reduce_seq() noexcept {
        return std::tuple_cat(Items::reduce_seq()...);
    }

    template <typename Item> static constexpr bool contains() noexcept {
        return ((std::is_same_v<Items, Item>) || ...);
    }

    template <typename... Others>
    constexpr bool operator==(LRItemSet<Others...>) const noexcept {
        return (contains<Others>() && ...) &&
               (LRItemSet<Others...>::template contains<Items>() && ...);
    }

    template <typename... Others>
    constexpr bool operator!=(LRItemSet<Others...> o) const noexcept {
        return !(*this == o);
    }

    template <typename Item> constexpr auto operator+(Item) const noexcept {
        static_assert(!contains<Item>(), "Item already exists");
        return LRItemSet<Items..., Item>{};
    }

    friend std::ostream &operator<<(std::ostream &os, LRItemSet set) {
        os << "LRItemSet:\n";
        ((os << "  " << Items{} << '\n'), ...);
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

template <typename... Sets> struct LRItemSetCollection {
    static_assert((is_lr_item_set_v<Sets> && ...), "Sets should be LRItemSet");

    static constexpr size_t size() noexcept { return sizeof...(Sets); }
    template <typename set> static constexpr bool contains() noexcept {
        return ((set{} == Sets{}) || ...);
    }

    template <size_t idx>
    using set_at = std::tuple_element_t<idx, std::tuple<Sets...>>;

    template <typename set> static constexpr auto index_of() {
        return index_of_impl<set, Sets...>();
    }

    template <typename set> constexpr auto operator+(set) const noexcept {
        if constexpr (contains<set>()) {
            return LRItemSetCollection<Sets...>{};
        } else {
            return LRItemSetCollection<Sets..., set>{};
        }
    }

    template <size_t idx = 0>
    friend std::ostream &operator<<(std::ostream &os,
                                    LRItemSetCollection) noexcept {
        if constexpr (idx == size()) {
            return os;
        } else {
            os << "closure(" << idx << ") = " << set_at<idx>();
            return operator<< <idx + 1>(os, LRItemSetCollection{});
        }
    }

 private:
    template <typename set> static constexpr auto index_of_impl() {
        static_assert(sizeof(set) == 0, "set not found!");
    }

    template <typename set, typename cur, typename... rest>
    static constexpr auto index_of_impl() {
        if constexpr (set{} == cur{}) {
            return 0;
        } else {
            return 1 + index_of_impl<set, rest...>();
        }
    }
};

} // namespace ctslrp::details::slr_gen
