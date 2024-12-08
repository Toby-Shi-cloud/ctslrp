#pragma once

/// utils of sequence
/// all of the functions are consteval and noexcept

#include <algorithm>
#include <array>
#include <concepts>
#include <functional>
#include <numeric>
#include <ranges>
#include <tuple>
#include <type_traits>
#include <utility>

/// sequence define
namespace ctslrp::utils {
namespace details {
template <typename T, typename U> consteval bool test_eq(const T &t, const U &u) {
    if constexpr (std::equality_comparable_with<T, U>) {
        return t == u;
    } else {
        return false;
    }
};
} // namespace details

template <typename... Ts> struct type_sequence {
    template <size_t idx> using type = std::tuple_element_t<idx, std::tuple<Ts...>>;
    consteval static size_t size() noexcept { return sizeof...(Ts); }
    template <typename U> consteval static bool contains() noexcept {
        return (std::is_same_v<Ts, U> || ...);
    }
    template <typename U> consteval static size_t find() noexcept {
        return std::ranges::find(std::array{std::is_same_v<Ts, U>...}, true);
    }
};

template <typename T, T... vs> struct typed_value_sequence {
    using type = T;
    consteval static size_t size() noexcept { return sizeof...(vs); }
    consteval static auto data() noexcept { return std::array{vs...}; }
    consteval static auto as_array() noexcept { return data(); }
    consteval operator std::array<T, size()>() const noexcept { return data(); }
    consteval static T at(size_t idx) noexcept { return data()[idx]; }
    consteval T operator[](size_t idx) const noexcept { return at(idx); }
    consteval static bool contains(const auto &value) noexcept {
        return (details::test_eq(vs, value) || ...);
    }
    consteval static size_t find(const auto &value) noexcept {
        return std::ranges::find(std::array{details::test_eq(vs, value)...}, true);
    }

    consteval typed_value_sequence() noexcept = default;
    template <typename U> // note: use template here, because T could be something out of integer
        requires std::same_as<T, U>
    consteval typed_value_sequence(std::integer_sequence<U, vs...>) noexcept {}
};
template <typename T, T... vs>
typed_value_sequence(std::integer_sequence<T, vs...>) -> typed_value_sequence<T, vs...>;

template <auto... vs> struct any_value_sequence {
    template <size_t idx> using type = std::tuple_element_t<idx, std::tuple<decltype(vs)...>>;
    template <size_t idx> constexpr static auto value = std::get<idx>(std::tuple{vs...});
    consteval static size_t size() noexcept { return sizeof...(vs); }
    consteval static bool contains(const auto &value) noexcept {
        return (details::test_eq(vs, value) || ...);
    }
    consteval static size_t find(const auto &value) noexcept {
        return std::ranges::find(std::array{details::test_eq(vs, value)...}, true);
    }
};

template <auto v> struct value_wrapper {
    using type = decltype(v);
    constexpr static type value = v;
};
} // namespace ctslrp::utils

/// sequence operator
namespace ctslrp::utils {
template <typename... Ts, typename... Us>
consteval auto operator+(type_sequence<Ts...>, type_sequence<Us...>) noexcept {
    return type_sequence<Ts..., Us...>{};
}

template <typename T, T... vs, T... ws>
consteval auto operator+(typed_value_sequence<T, vs...>, typed_value_sequence<T, ws...>) noexcept {
    return typed_value_sequence<T, vs..., ws...>{};
}

template <auto... vs, auto... ws>
consteval auto operator+(any_value_sequence<vs...>, any_value_sequence<ws...>) noexcept {
    return any_value_sequence<vs..., ws...>{};
}
} // namespace ctslrp::utils

/// sequence helper
namespace ctslrp::utils {
namespace ranges {
template <typename T>
concept container = std::ranges::range<T> && !std::ranges::view<T>;

template <container C> struct to_impl {
    template <std::ranges::range R>
        requires std::convertible_to<std::ranges::range_value_t<R>, typename C::value_type>
    constexpr C operator()(R &&r) {
        return C{r.begin(), r.end()};
    }
};
template <container C, std::ranges::range R> constexpr C operator|(R &&r, to_impl<C> to) {
    return to(std::forward<R>(r));
}
template <container C> constexpr to_impl<C> to = {};
} // namespace ranges

template <size_t N> consteval auto make_index_sequence() noexcept {
    return typed_value_sequence(std::make_index_sequence<N>{});
}

template <std::array arr> consteval auto array_as_sequence() noexcept {
    return []<size_t... Is>(std::index_sequence<Is...>) {
        return typed_value_sequence<typename decltype(arr)::value_type, arr[Is]...>{};
    }(std::make_index_sequence<arr.size()>());
}

template <auto func = std::plus<>{}, typename T, T v, T... vs>
consteval auto reduce(typed_value_sequence<T, v, vs...>) noexcept {
    constexpr auto array = std::array{vs...};
    return std::reduce(std::begin(array), std::end(array), v, func);
}

template <typename Func, typename T, T v, T... vs>
consteval auto reduce(Func &&func, typed_value_sequence<T, v, vs...>) noexcept {
    constexpr auto array = std::array{vs...};
    return std::reduce(std::begin(array), std::end(array), v, func);
}

template <auto func = std::plus<>{}, auto value>
consteval auto reduce(any_value_sequence<value>) noexcept {
    return value;
}

template <auto func = std::plus<>{}, auto v1, auto v2, auto... vs>
consteval auto reduce(any_value_sequence<v1, v2, vs...>) noexcept {
    return reduce<func>(any_value_sequence<std::invoke(func, v1, v2), vs...>{});
}

template <auto func, typename T, T... vs>
consteval auto map(typed_value_sequence<T, vs...>) noexcept {
    return typed_value_sequence<std::invoke_result_t<decltype(func), T>,
                                std::invoke(func, vs)...>{};
}

template <auto func, auto... vs> consteval auto map(any_value_sequence<vs...>) noexcept {
    return any_value_sequence<std::invoke(func, vs)...>{};
}

template <typename Func, typename T, T... vs>
consteval auto map_to_array(Func &&func, typed_value_sequence<T, vs...>) noexcept {
    return std::array{std::invoke(func, vs)...};
}

template <typename Func, auto... vs>
consteval auto map_to_array(Func &&func, any_value_sequence<vs...>) noexcept {
    return std::array{std::invoke(func, vs)...};
}

template <typename Func, typename... Ts>
consteval auto map_to_array(Func &&func, const std::tuple<Ts...> &value) noexcept {
    return [&]<size_t... Is>(std::index_sequence<Is...>) {
        return std::array{std::invoke(func, std::get<Is>(value))...};
    }(std::make_index_sequence<sizeof...(Ts)>());
}

template <typename Func, typename T, T... vs>
consteval auto for_each(Func &&func, typed_value_sequence<T, vs...>) noexcept {
    (std::invoke(func, vs), ...);
}

template <typename Func, auto... vs>
consteval auto for_each(Func &&func, any_value_sequence<vs...>) noexcept {
    (std::invoke(func, vs), ...);
}

template <template <auto> typename Wrapper = value_wrapper, typename T, auto... vs>
consteval auto wrap(typed_value_sequence<T, vs...>) noexcept {
    return type_sequence<Wrapper<vs>...>{};
}

template <template <auto> typename Wrapper = value_wrapper, auto... vs>
consteval auto wrap(any_value_sequence<vs...>) noexcept {
    return type_sequence<Wrapper<vs>...>{};
}

template <typename T, size_t N> consteval auto sorted(std::array<T, N> arr) noexcept {
    std::sort(arr.begin(), arr.end());
    return arr;
}

template <typename T, T... vs> consteval auto sorted(typed_value_sequence<T, vs...>) noexcept {
    constexpr auto arr = typed_value_sequence<T, vs...>::as_array();
    return array_as_sequence<sorted(arr)>();
}
} // namespace ctslrp::utils
