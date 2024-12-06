#pragma once

#include "slr_action.hpp"
#include "type_map.hpp"
#include "utils.hpp"
#include <algorithm>
#include <array>
#include <iostream>
#include <optional>
#include <ostream>
#include <tuple>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

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

struct SymbolT {
    enum { TERMINAL, NONTERMINAL, ERROR } type;
    size_t id;
    constexpr SymbolT() : type(ERROR), id(0) {}
    constexpr SymbolT(Terminal t) : type(TERMINAL), id(t.id) {}
    constexpr SymbolT(NonTerminal t) : type(NONTERMINAL), id(t.id) {}
    constexpr bool operator==(const SymbolT &) const noexcept = default;
    constexpr auto operator<=>(const SymbolT &) const noexcept = default;
    friend std::ostream &operator<<(std::ostream &os, const SymbolT &symbol) {
        if (symbol.type == SymbolT::TERMINAL) {
            return os << Terminal{symbol.id};
        } else if (symbol.type == SymbolT::NONTERMINAL) {
            return os << NonTerminal{symbol.id};
        } else {
            return os << "ERROR";
        }
    }
};

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

template <typename... T> struct overload : T... {
    using T::operator()...;
};
template <typename... T> overload(T...) -> overload<T...>;
} // namespace ctslrp::details::slr_gen

/// forward declaration
namespace ctslrp::details::slr_gen {
/// a simplified version of syntax rule
template <NonTerminal symbol, auto... exps> struct SimplifiedRule;
/// a simplified version of syntax rule table
template <NonTerminal start_symbol, typename... Rules>
struct SimplifiedRuleTable;
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

    static constexpr size_t max_statement_size() noexcept {
        size_t max_size = 1;
        ((max_size = std::max(max_size, Rules::statement_size)), ...);
        return max_size;
    }

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

    static constexpr NonTerminal extended_grammar =
        NonTerminal{max_symbol_id() + 1};
    using extended_rule = SimplifiedRule<extended_grammar, start_symbol>;

    template <size_t idx>
    using rule_t =
        std::tuple_element_t<idx, std::tuple<Rules..., extended_rule>>;

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

    struct LRItem {
        size_t rule_idx;
        size_t dot_idx;

        constexpr bool operator==(const LRItem &other) const noexcept = default;
        constexpr auto
        operator<=>(const LRItem &other) const noexcept = default;

        friend std::ostream &operator<<(std::ostream &os, const LRItem &item);
    };

    template <LRItem item> struct LRItemHelper {
        using Rule = rule_t<item.rule_idx>;
        static constexpr size_t index = item.dot_idx;
        static constexpr auto symbol = Rule::non_terminal;
        static constexpr bool reducable = index == Rule::statement_size;

        /// return RuleId if idx == size
        static constexpr auto reduce_seq() noexcept {
            if constexpr (reducable) {
                return seq::index_sequence<index_of<Rule>()>{};
            } else {
                return seq::index_sequence<>{};
            }
        }

        /// get the next symbol
        static constexpr SymbolT next_symbol() noexcept {
            if constexpr (index >= Rule::statement_size) {
                return {};
            } else {
                return std::get<index>(Rule::statements);
            }
        }

        static constexpr bool next_symbol_is_nonterminal() noexcept {
            return std::is_same_v<decltype(next_symbol()), NonTerminal>;
        }

        /// get the next symbol as sequence
        static constexpr auto next_symbol_as_seq() noexcept {
            if constexpr (index >= Rule::statement_size) {
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
        template <auto symbol> static constexpr auto next() noexcept {
            if constexpr (match_next<symbol>()) {
                return typed_sequence<LRItem,
                                      LRItem{item.rule_idx, index + 1}>{};
            } else {
                return typed_sequence<LRItem>{};
            }
        }

        template <size_t output_idx = 0>
        friend std::ostream &operator<<(std::ostream &os, LRItemHelper) {
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

    static constexpr auto non_tenminals =
        std::array{Rules::non_terminal..., extended_grammar};

    static constexpr auto next_symbol_of = seq::wrapped_map(
        [](auto rule_idx) {
            return seq::wrapped_map(
                [](auto dot_idx) {
                    return LRItemHelper<LRItem{
                        decltype(rule_idx)::value,
                        decltype(dot_idx)::value}>::next_symbol();
                },
                typed_sequence{
                    std::make_index_sequence<max_statement_size() + 1>()});
        },
        typed_sequence{std::make_index_sequence<rule_size + 1>()});

    friend std::ostream &operator<<(std::ostream &os, const LRItem &item) {
        os << non_tenminals[item.rule_idx] << " ->";
        for (size_t i = 0;; i++) {
            if (i == item.dot_idx) os << " .";
            auto sym = next_symbol_of[item.rule_idx][i];
            if (sym.type == SymbolT::ERROR) break;
            os << ' ' << sym;
        }
        return os;
    }

    template <size_t N> struct LRItemSet {
        std::array<LRItem, N> data;
        size_t actual_size;
        static constexpr size_t size = N;

        constexpr auto begin() const noexcept { return data.begin(); }
        constexpr auto begin() noexcept { return data.begin(); }
        constexpr auto end() const noexcept {
            return data.begin() + actual_size;
        }
        constexpr auto end() noexcept { return data.begin() + actual_size; }

        constexpr LRItemSet() noexcept = default;

        constexpr LRItemSet(std::array<LRItem, N> array) noexcept
            : data{seq::sorted(array)}, actual_size(N){};

        template <size_t M>
        constexpr LRItemSet(const LRItemSet<M> &other) noexcept
            : data(), actual_size(M) {
            static_assert(M <= N, "overflow!");
            std::copy_n(other.data.begin(), M, data.begin());
        };

        constexpr LRItemSet(const std::vector<LRItem> &vec) noexcept
            : data(), actual_size(vec.size()) {
            std::copy_n(vec.begin(), actual_size, data.begin());
            std::sort(data.begin(), data.begin() + actual_size);
        }

        template <size_t M>
        constexpr bool operator==(const LRItemSet<M> &other) const noexcept {
            if (other.actual_size != actual_size) return false;
            return std::equal(data.begin(), data.begin() + actual_size,
                              other.data.begin());
        }

        constexpr bool contains(LRItem item) const noexcept {
            return std::find(begin(), end(), item) != end();
        }

        constexpr auto next_symbols() const noexcept {
            std::vector<SymbolT> result;
            for (auto &item : *this) {
                auto next_symbol = next_symbol_of[item.rule_idx][item.dot_idx];
                if (next_symbol.type != SymbolT::ERROR)
                    result.push_back(next_symbol);
            }
            std::sort(result.begin(), result.end());
            auto new_size =
                std::unique(result.begin(), result.end()) - result.begin();
            result.resize(new_size);
            return result;
        }

        constexpr auto next_items(const SymbolT &symbol) const noexcept {
            std::vector<LRItem> result;
            if (symbol.type == SymbolT::ERROR) return result;
            for (auto &item : *this) {
                auto next_symbol = next_symbol_of[item.rule_idx][item.dot_idx];
                if (next_symbol == symbol) {
                    result.push_back(LRItem{item.rule_idx, item.dot_idx + 1});
                }
            }
            return result;
        }

        friend std::ostream &operator<<(std::ostream &os,
                                        const LRItemSet &set) {
            os << "LRItemSet:\n";
            for (auto &item : set) {
                os << "  " << item << '\n';
            }
            return os;
        }
    };
    template <size_t N> LRItemSet(std::array<LRItem, N>) -> LRItemSet<N>;

    template <LRItemSet set> struct LRItemSetHelper {
        constexpr static auto items =
            seq::array_as_sequence<set.data, set.actual_size>();
        constexpr static auto helpers = seq::wrap<LRItemHelper>(items);

        /// get the next item if the symbol is matched
        template <auto symbol> static constexpr auto next() noexcept {
            return seq::reduce(
                seq::map<[](auto x) { return x.template next<symbol>(); }>(
                    helpers));
        }

        /// get the next symbol (as any_sequence)
        static constexpr auto next_symbols() noexcept {
            return seq::reduce(
                seq::map<[](auto x) { return x.next_symbol_as_seq(); }>(
                    helpers));
        }

        /// get all could reduced rules
        static constexpr auto reduce_seq() noexcept {
            return seq::reduce(
                seq::map<[](auto x) { return x.reduce_seq(); }>(helpers));
        }

        static constexpr bool contains(LRItem item) noexcept {
            return set.contains(item);
        }

        friend std::ostream &operator<<(std::ostream &os, LRItemSetHelper) {
            os << "LRItemSet:\n";
            seq::for_each([&os](auto x) { os << x << '\n'; }, helpers);
            return os;
        }
    };

    static constexpr auto extended_item = LRItem{sizeof...(Rules), 0};

    struct ClosureHelper {
        template <size_t rule_idx, size_t dot_idx>
        static constexpr std::vector<LRItem> items_of_impl() noexcept {
            static_assert(rule_idx <= rule_size, "rule_idx out of range");
            constexpr auto sym = next_symbol_of[rule_idx][dot_idx];
            if constexpr (sym.type == SymbolT::NONTERMINAL) {
                constexpr NonTerminal symbol = {sym.id};
                static_assert(((symbol == Rules::non_terminal) || ...),
                              "undefined symbols...");
                std::vector<LRItem> result;
                ((symbol == Rules::non_terminal
                      ? result.push_back(LRItem{index_of<Rules>(), 0})
                      : (void)0),
                 ...);
                return result;
            } else {
                return {};
            }
        }

        static constexpr auto items_of = seq::wrapped_map(
            [](auto rule_idx) {
                return seq::wrapped_map(
                    [](auto dot_idx) {
                        return &ClosureHelper::items_of_impl<
                            decltype(rule_idx)::value,
                            decltype(dot_idx)::value>;
                    },
                    typed_sequence{
                        std::make_index_sequence<max_statement_size() + 1>()});
            },
            typed_sequence{std::make_index_sequence<rule_size + 1>()});

        static constexpr auto
        calc_impl(std::vector<LRItem> start_vec) noexcept {
            std::vector<LRItem> result;
            auto &stack = start_vec;
            while (!stack.empty()) {
                auto item = stack.back();
                stack.pop_back();
                if (std::find(result.begin(), result.end(), item) !=
                    result.end())
                    continue;
                result.push_back(item);
                auto next_items = items_of[item.rule_idx][item.dot_idx]();
                stack.insert(stack.end(), next_items.begin(), next_items.end());
            }
            return result;
        }

        template <bool eval, LRItem... items>
        static constexpr auto
        calc_impl(typed_sequence<LRItem, items...>) noexcept {
            auto result = calc_impl(std::vector{items...});
            if constexpr (eval) {
                constexpr auto size = calc_impl<false, items...>({});
                return LRItemSet<size>{result};
            } else {
                return result.size();
            }
        }

        template <LRItem... items>
        static constexpr auto
        calc(typed_sequence<LRItem, items...> seq) noexcept {
            return calc_impl<true>(seq);
        }
    };
    template <typename ItemSeq>
    constexpr static auto Closure = ClosureHelper::template calc(ItemSeq{});
    template <LRItemSet set, auto symbol>
    constexpr static auto Goto = ClosureHelper::template calc(
        LRItemSetHelper<set>::template next<symbol>());
    constexpr static auto StartClosure =
        Closure<typed_sequence<LRItem, extended_item>>;

    template <size_t N, size_t EleN> struct LRItemSetCollection {
        std::array<LRItemSet<EleN>, N> data;
        static constexpr size_t size() noexcept { return N; }

        constexpr LRItemSetCollection(std::array<LRItemSet<EleN>, N> array)
            : data(array) {}

        constexpr explicit LRItemSetCollection(
            std::vector<LRItemSet<EleN>> vec) {
            std::copy(vec.begin(), vec.end(), data.begin());
        }

        template <size_t M>
        constexpr bool contains(const LRItemSet<M> &set) const noexcept {
            return std::find(data.begin(), data.end(), set) != data.end();
        }

        constexpr auto &operator[](size_t idx) noexcept { return data[idx]; }
        constexpr const auto &operator[](size_t idx) const noexcept {
            return data[idx];
        }

        template <size_t M>
        constexpr auto index_of(const LRItemSet<M> &set) const noexcept {
            return std::find(data.begin(), data.end(), set) - data.begin();
        }

        friend std::ostream &operator<<(std::ostream &os,
                                        const LRItemSetCollection &collection) {
            for (size_t i = 0; i < N; i++) {
                os << i << "$" << collection[i] << '\n';
            }
            return os;
        }
    };
    template <size_t N, size_t EleN>
    LRItemSetCollection(std::array<LRItemSet<EleN>, N>)
        -> LRItemSetCollection<N, EleN>;

    struct ItemSetCollectionHelper {
        template <size_t EleN, LRItemSet start_set>
        static constexpr std::vector<LRItemSet<EleN>> calc_try() noexcept {
            std::vector<LRItemSet<EleN>> collection;
            std::vector<LRItemSet<EleN>> stack{start_set};
            while (!stack.empty()) {
                auto set = stack.back();
                stack.pop_back();
                if (std::find(collection.begin(), collection.end(), set) !=
                    collection.end())
                    continue;
                collection.push_back(set);
                for (auto &symbol : set.next_symbols()) {
                    auto next_items = set.next_items(symbol);
                    auto next_set = ClosureHelper::calc_impl(next_items);
                    if (next_set.size() > EleN) return {};
                    stack.emplace_back(next_set);
                }
            }
            return collection;
        }

        template <size_t EleN, LRItemSet start_set>
        static constexpr size_t calc_try_size() noexcept {
            return calc_try<EleN, start_set>().size();
        }

        template <size_t EleN, LRItemSet start_set>
        static constexpr std::pair<size_t, size_t> get_sizes() noexcept {
            constexpr auto size = calc_try_size<EleN, start_set>();
            if constexpr (size > 0) {
                return std::make_pair(size, EleN);
            } else {
                return get_sizes<EleN << 1, start_set>();
            }
        }

        template <LRItemSet start_set> static constexpr auto calc() noexcept {
            constexpr auto sizes = get_sizes<32, start_set>();
            auto collection = calc_try<sizes.second, start_set>();
            return LRItemSetCollection<sizes.first, sizes.second>{collection};
        }
    };
    constexpr static auto ItemSetCollection =
        ItemSetCollectionHelper::template calc<StartClosure>();

    static constexpr size_t state_size = ItemSetCollection.size();
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
            constexpr auto ItemSet = ItemSetCollection[idx];
            constexpr auto j =
                ItemSetCollection.template index_of(Goto<ItemSet, A>);
            table.gotos[idx][A.id].idx = j;
            return gen_impl_nxt<idx>(table, any_sequence<rest...>{});
        }
        template <size_t idx, Terminal a, auto... rest>
        static constexpr std::optional<Conflict>
        gen_impl_nxt(LRActionGotoTable &table,
                     any_sequence<a, rest...>) noexcept {
            constexpr auto ItemSet = ItemSetCollection[idx];
            constexpr auto j =
                ItemSetCollection.template index_of(Goto<ItemSet, a>);
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
        gen_impl_reduce(LRActionGotoTable &, seq::index_sequence<>) noexcept {
            return std::nullopt;
        }
        template <size_t idx, size_t rule_id, size_t... rest>
        static constexpr std::optional<Conflict>
        gen_impl_reduce(LRActionGotoTable &table,
                        seq::index_sequence<rule_id, rest...>) noexcept {
            using rule = rule_t<rule_id>;
            using follow_set =
                FollowSet::template ValueOf<value_wrapper<rule::non_terminal>>;
            auto conflict =
                gen_impl_reduce_set_follows<idx, rule_id>(table, follow_set{});
            if (conflict) return conflict;
            return gen_impl_reduce<idx>(table, seq::index_sequence<rest...>{});
        }

        template <size_t idx>
        static constexpr std::optional<Conflict>
        gen_impl(LRActionGotoTable &table) noexcept {
            if constexpr (idx == state_size) {
                return std::nullopt;
            } else {
                constexpr auto ItemSet = ItemSetCollection[idx];
                constexpr auto next_symbols =
                    LRItemSetHelper<ItemSet>::next_symbols();
                auto conflict = gen_impl_nxt<idx>(table, next_symbols);
                if (conflict) return conflict;
                constexpr auto reduce_seq =
                    LRItemSetHelper<ItemSet>::reduce_seq();
                conflict = gen_impl_reduce<idx>(table, reduce_seq);
                if (conflict) return conflict;
                if constexpr (ItemSet.contains({rule_size, 1})) {
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
