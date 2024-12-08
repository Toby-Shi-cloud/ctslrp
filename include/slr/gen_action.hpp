#pragma once

#include "gen/errors.hpp"
#include "slr/symbol.hpp"
#include "utils/adaptor.hpp"
#include "utils/sequence.hpp"
#include <algorithm>
#include <array>
#include <cassert>
#include <ostream>

namespace ctslrp::details {
template <size_t N> struct SimplifiedRule {
    NonTerminal symbol;
    utils::wrapped_array<SymbolT, N> statements;

    constexpr size_t size() const noexcept { return statements.size(); }
    constexpr static size_t capacity() noexcept { return N; }

    friend std::ostream &operator<<(std::ostream &os, const SimplifiedRule &rule) {
        os << rule.symbol << " -> ";
        for (const auto &stmt : rule.statements) {
            os << stmt << ' ';
        }
        return os;
    }
};

template <size_t N, size_t M> struct SimplifiedRuleTable {
    NonTerminal extended_symbol;
    std::array<SimplifiedRule<M>, N> rules;

    friend std::ostream &operator<<(std::ostream &os, const SimplifiedRuleTable &table) {
        for (const auto &rule : table.rules) {
            os << rule << '\n';
        }
        return os;
    }
};

template <size_t N> using TerminalSet = utils::sorded_array<Terminal, N>;

template <size_t _R_N> struct LRItem {
    const SimplifiedRule<_R_N> *rule = nullptr;
    size_t dot_idx = 0;

    constexpr LRItem() noexcept = default;
    constexpr LRItem(const SimplifiedRule<_R_N> *rule, size_t dot_idx) noexcept
        : rule(rule), dot_idx(dot_idx) {}

    constexpr auto next_symbol() const noexcept {
        if (dot_idx >= rule->size()) return SymbolT{};
        return rule->statements[dot_idx];
    }

    constexpr auto to_next() const noexcept {
        assert(dot_idx < rule->size());
        return LRItem{rule, dot_idx + 1};
    }

    constexpr auto operator<=>(const LRItem &other) const noexcept = default;
    friend std::ostream &operator<<(std::ostream &os, const LRItem &item) {
        os << item.rule->symbol << " ->";
        for (size_t i = 0; i < item.rule->size(); i++) {
            if (i == item.dot_idx) os << " .";
            os << ' ' << item.rule->statements[i];
        }
        if (item.dot_idx == item.rule->size()) os << " .";
        return os;
    }
};

template <size_t N, size_t _R_N> struct LRItemSet : utils::sorded_array<LRItem<_R_N>, N> {
    using sorded_array = utils::sorded_array<LRItem<_R_N>, N>;
    using LRItem = LRItem<_R_N>;

    constexpr auto next_symbols() const noexcept {
        std::vector<SymbolT> result;
        for (auto &item : *this) {
            auto next_symbol = item.next_symbol();
            if (next_symbol.type != SymbolT::ERROR) result.push_back(next_symbol);
        }
        std::sort(result.begin(), result.end());
        auto new_size = std::unique(result.begin(), result.end()) - result.begin();
        result.resize(new_size);
        return result;
    }

    constexpr auto next_items(const SymbolT &symbol) const noexcept {
        std::vector<LRItem> result;
        if (symbol.type == SymbolT::ERROR) return result;
        for (auto &item : *this) {
            auto next_symbol = item.next_symbol();
            if (next_symbol == symbol) {
                result.push_back(LRItem{item.rule, item.dot_idx + 1});
            }
        }
        return result;
    }

    friend std::ostream &operator<<(std::ostream &os, const LRItemSet &set) {
        os << "LRItemSet:\n";
        for (auto &item : set) {
            os << "  " << item << '\n';
        }
        return os;
    }
};

template <size_t N, size_t EleN, size_t _R_N>
struct LRItemSetCollection : utils::wrapped_array<LRItemSet<EleN, _R_N>, N> {
    using wrapped_array = utils::wrapped_array<LRItemSet<EleN, _R_N>, N>;
    using wrapped_array::wrapped_array;

    friend std::ostream &operator<<(std::ostream &os, const LRItemSetCollection &collection) {
        for (size_t i = 0; i < N; i++) {
            os << i << "$" << collection[i] << '\n';
        }
        return os;
    }
};
} // namespace ctslrp::details

namespace ctslrp::details {
template <SimplifiedRuleTable table> struct FirstSetHelper {
    constexpr static auto calc_impl() noexcept {
        std::array<std::vector<Terminal>, table.extended_symbol.id + 1> first_set;
        bool changed = true;
        while (changed) {
            changed = false;
            for (size_t i = 0; i < table.rules.size(); i++) {
                auto symbol = table.rules[i].symbol;
                auto first = LRItem{&table.rules[i], 0}.next_symbol();
                auto &set = first_set[symbol.id];
                auto ori_size = set.size();
                if (first.type == SymbolT::TERMINAL) {
                    set.push_back(Terminal{first.id});
                } else if (first.type == SymbolT::NONTERMINAL) {
                    auto &first_of_rule = first_set[first.id];
                    set.insert(set.end(), first_of_rule.begin(), first_of_rule.end());
                } else {
                    set.push_back(literals::epsilon);
                }
                std::sort(set.begin(), set.end());
                auto new_size = std::unique(set.begin(), set.end()) - set.begin();
                set.resize(new_size);
                if (new_size != ori_size) changed = true;
            }
        }
        return first_set;
    }

    constexpr static auto get_size() noexcept {
        using T = std::vector<Terminal>;
        auto first_set = calc_impl();
        return std::ranges::max(first_set | std::views::transform(&T::size));
    }

    constexpr static auto calc() noexcept {
        constexpr auto size = get_size();
        auto first_set = calc_impl();
        std::array<TerminalSet<size>, first_set.size()> result;
        for (size_t i = 0; i < first_set.size(); i++) {
            std::copy(first_set[i].begin(), first_set[i].end(), result[i].begin());
            result[i].actual_size = first_set[i].size();
        }
        return result;
    }

    constexpr static auto result = calc();

    static std::ostream &print_result(std::ostream &os) {
        os << "FirstSet:\n";
        for (size_t i = 0; i < result.size(); i++) {
            os << "  " << NonTerminal{i} << ": " << result[i] << '\n';
        }
        return os;
    }
};

template <SimplifiedRuleTable table> struct FollowSetHelper {
    constexpr static auto calc_impl() noexcept {
        std::array<std::vector<Terminal>, table.extended_symbol.id + 1> follow_set;
        follow_set[table.extended_symbol.id].push_back(literals::sharp);
        bool changed = true;
        while (changed) {
            changed = false;
            for (size_t i = 0; i < table.rules.size(); i++) {
                /// A -> αBβ
                bool could_epsilon = true; // first(β) contains ε
                // follow(β) += follow(A)
                auto current_follow = follow_set[table.rules[i].symbol.id];
                for (ssize_t j = table.rules[i].size() - 1; j >= 0; j--) {
                    auto sym = LRItem{&table.rules[i], size_t(j)}.next_symbol(); // B
                    if (sym.type != SymbolT::NONTERMINAL) continue;
                    auto &set = follow_set[sym.id];
                    auto ori_size = set.size();
                    auto nxt = LRItem{&table.rules[i], size_t(j + 1)}.next_symbol();
                    if (nxt.type == SymbolT::NONTERMINAL) {
                        // first(β)
                        auto &first_of_rule = FirstSetHelper<table>::result[nxt.id];
                        could_epsilon &= first_of_rule.contains(literals::epsilon);
                        for (auto &t : first_of_rule) {
                            if (t != literals::epsilon) {
                                // follow(B) += first(β) - ε
                                set.push_back(t);
                                current_follow.push_back(t);
                            }
                        }
                    } else if (nxt.type == SymbolT::TERMINAL) {
                        set.push_back(Terminal{nxt.id}); // first(β)
                        could_epsilon = false;
                    }
                    if (could_epsilon) {
                        // if first(β) contains ε,
                        // follow(B) += follow(β)
                        for (auto &t : current_follow)
                            set.push_back(t);
                    }
                    std::sort(set.begin(), set.end());
                    auto new_size = std::unique(set.begin(), set.end()) - set.begin();
                    set.resize(new_size);
                    if (new_size != ori_size) changed = true;
                }
            }
        }
        return follow_set;
    }

    constexpr static auto get_size() noexcept {
        using T = std::vector<Terminal>;
        auto follow_set = calc_impl();
        return std::ranges::max(follow_set | std::views::transform(&T::size));
    }

    constexpr static auto calc() noexcept {
        constexpr auto size = get_size();
        auto follew_set = calc_impl();
        std::array<TerminalSet<size>, follew_set.size()> result;
        for (size_t i = 0; i < follew_set.size(); i++) {
            std::copy(follew_set[i].begin(), follew_set[i].end(), result[i].begin());
            result[i].actual_size = follew_set[i].size();
        }
        return result;
    }

    constexpr static auto result = calc();

    static std::ostream &print_result(std::ostream &os) {
        os << "FollowSet:\n";
        for (size_t i = 0; i < result.size(); i++) {
            os << "  " << NonTerminal{i} << ": " << result[i] << '\n';
        }
        return os;
    }
};

template <SimplifiedRuleTable table> struct ClosureHelper {
    constexpr static size_t _R_N = table.rules[0].capacity();
    using LRItem = LRItem<_R_N>;

    static constexpr std::vector<LRItem> items_of(LRItem item) noexcept {
        auto sym = item.next_symbol();
        if (sym.type == SymbolT::NONTERMINAL) {
            NonTerminal symbol = {sym.id};
            return table.rules | std::views::filter([symbol](const auto &rule) {
                       return rule.symbol == symbol;
                   }) |
                   std::views::transform([](const auto &rule) {
                       return LRItem{&rule, 0};
                   }) |
                   utils::ranges::to<std::vector<LRItem>>;
        } else {
            return {};
        }
    }

    static constexpr auto calc_impl(std::vector<LRItem> start_vec) noexcept {
        std::vector<LRItem> result;
        auto &stack = start_vec;
        while (!stack.empty()) {
            auto item = stack.back();
            stack.pop_back();
            if (std::find(result.begin(), result.end(), item) != result.end()) continue;
            result.push_back(item);
            auto next_items = items_of(item);
            stack.insert(stack.end(), next_items.begin(), next_items.end());
        }
        return result;
    }

    template <LRItem... items> static constexpr auto get_size() noexcept {
        return calc_impl({items...}).size();
    }

    template <LRItem... items> static constexpr auto calc() noexcept {
        return LRItemSet<get_size<items...>(), _R_N>{calc_impl({items...})};
    }

    constexpr static auto start_closure = ClosureHelper::calc<LRItem{&table.rules[0], 0}>();

    static std::ostream &print_result(std::ostream &os) {
        os << "Start Closure:\n";
        os << start_closure << '\n';
        return os;
    }
};

template <SimplifiedRuleTable table> struct ItemSetCollectionHelper {
    constexpr static size_t _R_N = table.rules[0].capacity();
    using LRItem = LRItem<_R_N>;

    template <size_t EleN, LRItemSet start_set>
    static constexpr std::vector<LRItemSet<EleN, _R_N>> calc_try() noexcept {
        std::vector<LRItemSet<EleN, _R_N>> collection;
        std::vector<LRItemSet<EleN, _R_N>> stack;
        stack.emplace_back(start_set);
        while (!stack.empty()) {
            auto set = stack.back();
            stack.pop_back();
            if (std::find(collection.begin(), collection.end(), set) != collection.end()) continue;
            collection.push_back(set);
            for (auto &symbol : set.next_symbols()) {
                auto next_items = set.next_items(symbol);
                auto next_set = ClosureHelper<table>::calc_impl(next_items);
                if (next_set.size() > EleN) return {};
                stack.emplace_back(next_set);
            }
        }
        return collection;
    }

    template <size_t EleN, LRItemSet start_set> static constexpr size_t calc_try_size() noexcept {
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
        return LRItemSetCollection<sizes.first, sizes.second, _R_N>{collection};
    }

    constexpr static auto result = calc<ClosureHelper<table>::start_closure>();

    static std::ostream &print_result(std::ostream &os) {
        os << "ItemSetCollection:\n";
        os << result << '\n';
        return os;
    }
};

} // namespace ctslrp::details
