#pragma once

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

template <typename... Entries> struct TypeMap {
    template <typename K, typename V>
    constexpr auto operator+(TypeMapEntry<K, V>) const {
        return TypeMap<Entries..., TypeMapEntry<K, V>>{};
    }

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

    template <typename K>
    using ValueOf = decltype(TypeMap::template get_impl<K, Entries...>())::type;

    template <typename K> constexpr static bool contains() {
        return contains_impl<K, Entries...>();
    }
};
} // namespace tbslr::details
