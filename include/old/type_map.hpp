#pragma once

#include "utils.hpp"
#include <ostream>
#include <tuple>
#include <type_traits>

namespace ctslrp::details {
template <typename K, typename V> struct TypeMapEntry;
template <typename... Entries> struct TypeMap;

template <typename K, typename V> struct TypeMapEntry {
    using Key = K;
    using Value = V;
    using Entry = TypeMapEntry<Key, Value>;
    /* implicit */ TypeMap<Entry> operator()() const {
        return TypeMap<Entry>{};
    }
    template <typename K1, typename V1>
    constexpr auto operator+(TypeMapEntry<K1, V1>) const {
        return TypeMap<Entry, TypeMapEntry<K1, V1>>{};
    }
};

template <typename... Es> class TypeMap {
    template <typename T> struct TypeWrapper {
        using type = T;
    };

    template <typename K, typename E, typename... Other>
    constexpr static auto get_impl() {
        if constexpr (std::is_same_v<K, typename E::Key>)
            return TypeWrapper<typename E::Value>();
        else
            return get_impl<K, Other...>();
    }
    template <typename K> constexpr static void get_impl() {
        static_assert(sizeof(K) == 0, "Key not found in TypeMap");
    }

    template <typename K, typename E, typename... Other>
    constexpr static bool contains_impl() {
        if constexpr (std::is_same_v<K, typename E::Key>)
            return true;
        else
            return contains_impl<K, Other...>();
    }
    template <typename K> constexpr static bool contains_impl() {
        return false;
    }

 public:
    template <typename K, typename V>
    constexpr auto operator+(TypeMapEntry<K, V>) const {
        static_assert(!contains<K>(), "Key already exists in TypeMap");
        return TypeMap<Es..., TypeMapEntry<K, V>>{};
    }

    template <typename K, typename V>
    constexpr auto operator|(TypeMapEntry<K, V>) const {
        if constexpr (contains<K>()) {
            return *this;
        } else {
            return TypeMap<Es..., TypeMapEntry<K, V>>{};
        }
    }

    template <typename K, typename V>
    constexpr static auto set(TypeMapEntry<K, V> = {}) {
        if constexpr (contains<K>()) {
            return TypeMap<TypeMapEntry<
                typename Es::Key,
                std::conditional_t<std::is_same_v<typename Es::Key, K>, V,
                                   typename Es::Value>>...>{};
        } else {
            return TypeMap<Es..., TypeMapEntry<K, V>>{};
        }
    }

    template <typename K, typename V>
    constexpr auto operator*(TypeMapEntry<K, V>) const {
        return set<K, V>();
    }

    using Keys = std::tuple<typename Es::Key...>;
    using Values = std::tuple<typename Es::Value...>;
    using Entries = std::tuple<Es...>;

    constexpr static size_t size() { return sizeof...(Es); }

    template <typename K> constexpr static auto get() {
        return get_impl<K, Es...>();
    }

    template <typename K, typename Default> constexpr static auto get_or() {
        if constexpr (contains<K>()) {
            return get<K>();
        } else {
            return TypeWrapper<Default>{};
        }
    }

    template <typename K> using ValueOf = decltype(get<K>())::type;
    template <typename K, typename Default>
    using ValueOrDefault = decltype(get_or<K, Default>())::type;

    template <typename K> constexpr static bool contains() {
        return contains_impl<K, Es...>();
    }

    friend std::ostream &operator<<(std::ostream &os, TypeMap) {
        os << "TypeMap {\n";
        ((os << "  " << typename_of<typename Es::Key>() << " -> "
             << typename_of<typename Es::Value>() << '\n'),
         ...);
        return os << "}\n";
    }
};
} // namespace ctslrp::details
