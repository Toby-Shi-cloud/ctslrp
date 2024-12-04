#pragma once

#include "lexer.hpp"
#include "regex.hpp"
#include "slr_gen.hpp"
#include "type_map.hpp"
#include "utils.hpp"

#include <functional>
#include <tuple>
#include <type_traits>
#include <utility>

namespace ctslrp::details {

template <typename SymbolEnum> class SyntaxRuleGenerator {
    static_assert(std::is_enum_v<SymbolEnum>,
                  "SymbolEnum must be an enum class");

    /// magic cast any_value to value_wrapper<symbol> or regex_expression<...>
    template <any_value any> constexpr static auto magic_cast() {
        using T = decltype(any)::type;
        constexpr auto value = any.value;
        if constexpr (std::is_same_v<T, SymbolEnum>) {
            return value_wrapper<value>{};
        } else {
            return regex::to_regex_by_value_t<value>{};
        }
    }

 public: /// Declare a symbol
    template <SymbolEnum symbol> constexpr static auto decl() {
        using Symbol = GeneratedSymbol<value_wrapper<symbol>>;
        return Symbol{};
    }

 private:
    template <typename Sym> struct GeneratedSymbol {
        /// Define a rule for the symbol
        template <any_value... statements> constexpr static auto define() {
            using Rule =
                GeneratedRule<Sym, decltype(magic_cast<statements>())...>;
            return Rule{};
        }
    };

    template <typename Sym, typename... Stat> struct GeneratedRule {
        /// Bind a function to the rule
        template <typename BindT, typename Func>
        constexpr auto bind(Func &&func) const {
            using Bind = BindedRule<Sym, BindT, Func, Stat...>;
            return Bind{std::forward<Func>(func)};
        }
    };

    template <typename... Rules> class BindedRuleTable;
    template <typename Sym, typename BindT, typename BindF, typename... Stat>
    struct BindedRule {
        static_assert(static_cast<ssize_t>(Sym::value) >= 0,
                      "Symbol must be declared as positive");
        using Symbol = Sym;
        using BindType = BindT;
        using BindFunc = BindF;
        using Statements = std::tuple<Stat...>;
        template <size_t idx>
        using Statement = std::tuple_element_t<idx, Statements>;
        constexpr static size_t statement_size = sizeof...(Stat);

        BindFunc func;

        /// Call the binded function with arguments
        template <typename... Args> constexpr auto call(Args &&...args) const {
            return std::invoke(func, std::forward<Args>(args)...);
        }

        /// Call the binded function with arguments
        template <typename... Args> auto call(Args &&...args) {
            return std::invoke(func, std::forward<Args>(args)...);
        }

        template <typename... Ts>
        friend constexpr auto operator,(BindedRule r1, BindedRule<Ts...> r2) {
            using R = BindedRule<Ts...>;
            if constexpr (std::is_same_v<Symbol, typename R::Symbol> &&
                          !std::is_same_v<BindType, typename R::BindType>) {
                constexpr auto msg =
                    "Symbol `"_raw + valuename_of<Symbol::value>() +
                    "` was first binded as `"_raw + typename_of<BindType>() +
                    "` but now binded as `"_raw +
                    typename_of<typename R::BindType>() + "`"_raw;
                return CompileError<msg>{};
            } else {
                return BindedRuleTable<BindedRule, BindedRule<Ts...>>{
                    std::make_tuple(std::move(r1), std::move(r2))};
            }
        }
    };

    /// A table to store all the rules (with all type information)
    template <typename... Rules> class BindedRuleTable : std::tuple<Rules...> {
        using tuple = std::tuple<Rules...>;
        template <size_t idx> using rule_t = std::tuple_element_t<idx, tuple>;
        using BindedTypeMap = TypeMap<
            TypeMapEntry<typename Rules::Symbol, typename Rules::BindType>...>;

        template <typename K, typename V> constexpr static bool well_formed() {
            if constexpr (!BindedTypeMap::template contains<K>())
                return true;
            else
                return std::is_same_v<
                    typename BindedTypeMap::template ValueOf<K>, V>;
        }

     public:
        explicit constexpr BindedRuleTable(tuple &&t) : tuple(std::move(t)) {}
        explicit constexpr BindedRuleTable(const tuple &t) : tuple(t) {}

        template <typename... Ts>
        friend constexpr auto operator,(BindedRuleTable table,
                                        BindedRule<Ts...> rule) {
            using R = BindedRule<Ts...>;
            if constexpr (well_formed<typename R::Symbol,
                                      typename R::BindType>()) {
                return BindedRuleTable<Rules..., R>{
                    std::tuple_cat(static_cast<tuple &&>(std::move(table)),
                                   std::make_tuple(std::move(rule)))};
            } else {
                constexpr auto msg =
                    "Symbol `"_raw + valuename_of<R::Symbol::value>() +
                    "` was first binded as `"_raw +
                    typename_of<typename BindedTypeMap::template ValueOf<
                        typename R::Symbol>>() +
                    "` but now binded as `"_raw +
                    typename_of<typename R::BindType>() + "`"_raw;
                return CompileError<msg>{};
            }
        }

     private:
        /// add stat to map
        template <typename Map, typename Stat>
        constexpr static auto regex_to_token_map_impl_impl() {
            if constexpr (is_regex_v<Stat>) {
                return Map{} | TypeMapEntry<Stat, value_wrapper<Map::size()>>{};
            } else {
                return Map{};
            }
        }

        /// add all stat in rule to map
        template <typename Map, typename Rule, size_t idx = 0>
        constexpr static auto regex_to_token_map_impl() {
            if constexpr (idx == Rule::statement_size) {
                return Map{};
            } else {
                return regex_to_token_map_impl<
                    decltype(regex_to_token_map_impl_impl<
                             Map, typename Rule::template Statement<idx>>()),
                    Rule, idx + 1>();
            }
        }

        /// return TypeMap[ Regex -> wrapper_value<token_id> ]
        template <typename Map = TypeMap<>, size_t idx = 0>
        constexpr static auto regex_to_token_map() {
            if constexpr (idx == sizeof...(Rules)) {
                return Map{};
            } else {
                return regex_to_token_map<
                    decltype(regex_to_token_map_impl<Map, rule_t<idx>>()),
                    idx + 1>();
            }
        }

        // regex -> token_id
        using TokenMap = decltype(regex_to_token_map());

     private:
        template <typename Rule, size_t idx = 0>
        constexpr static auto simpilify_rule_impl() {
            if constexpr (idx == Rule::statement_size) {
                return any_sequence<>{};
            } else {
                using statement = typename Rule::template Statement<idx>;
                if constexpr (is_regex_v<statement>) {
                    constexpr auto token_id =
                        TokenMap::template ValueOf<statement>::value;
                    return any_sequence<slr_gen::Terminal{token_id}>{} +
                           simpilify_rule_impl<Rule, idx + 1>();
                } else {
                    constexpr auto sym_id =
                        static_cast<size_t>(statement::value);
                    return any_sequence<slr_gen::NonTerminal{sym_id}>{} +
                           simpilify_rule_impl<Rule, idx + 1>();
                }
            }
        }

        template <typename Rule> constexpr static auto simpilify_rule() {
            constexpr auto sym =
                slr_gen::NonTerminal{static_cast<size_t>(Rule::Symbol::value)};
            constexpr auto exps = simpilify_rule_impl<Rule>();
            return slr_gen::SimplifiedRule(any_sequence<sym>{} + exps);
        }

        constexpr static auto simpilify_rule_table() {
            return slr_gen::SimplifiedRuleTable<
                decltype(simpilify_rule<Rules>())...>{};
        }

     public:
        constexpr auto compile() const {
            using Re = TokenMap::Keys;
            auto lexer = Lexer("\\s"_r, Re{});
            auto table = simpilify_rule_table();
            return std::make_tuple(lexer, table);
        }
    };
};
} // namespace ctslrp::details
