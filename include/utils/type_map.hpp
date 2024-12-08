#pragma once

/// type_map

#include "utils/sequence.hpp"

namespace ctslrp::utils {
template <typename K, typename V> struct type_pair {
    using key = K;
    using value = V;
};

template <typename T>
concept valid_type_pair = requires {
    typename T::key;
    typename T::value;
};

template <valid_type_pair... Es> struct type_map {
    using key_sequence = type_sequence<typename Es::key...>;
    using value_sequence = type_sequence<typename Es::value...>;

    template <typename K> consteval static bool contains() noexcept {
        return key_sequence::template contains<K>();
    }

    template <typename K>
    using at = value_sequence::template type<key_sequence::template find<K>()>;

    template<valid_type_pair pair>
    consteval auto operator+(pair) noexcept {
        return type_map<Es..., pair>{};
    }
};
}; // namespace ctslrp::utils
