#pragma once

#include <algorithm>
#include <array>
#include <concepts>
#include <exception>
#include <functional>
#include <ostream>
#include <ranges>
#include <utility>
#include <vector>

namespace ctslrp::utils {
template <typename T, size_t N> struct wrapped_array {
    std::array<T, N> data;
    size_t actual_size = 0;

    constexpr wrapped_array() noexcept = default;
    constexpr wrapped_array(std::initializer_list<T> list) noexcept {
        for (auto &&elem : list) {
            if (actual_size == N) std::terminate();
            data[actual_size++] = std::move(elem);
        }
    }
    constexpr wrapped_array(std::ranges::range auto &&range) noexcept {
        for (auto &&elem : range) {
            if (actual_size == N) std::terminate();
            data[actual_size++] = std::forward<decltype(elem)>(elem);
        }
    }

    constexpr size_t size() const noexcept { return actual_size; }
    constexpr static size_t capacity() noexcept { return N; }
    constexpr auto begin() noexcept { return data.begin(); }
    constexpr auto begin() const noexcept { return data.begin(); }
    constexpr auto end() noexcept { return data.begin() + actual_size; }
    constexpr auto end() const noexcept { return data.begin() + actual_size; }
    constexpr auto &operator[](size_t idx) noexcept { return data[idx]; }
    constexpr const auto &operator[](size_t idx) const noexcept { return data[idx]; }
    friend constexpr bool operator==(const wrapped_array &lhs, const wrapped_array &rhs) noexcept {
        return lhs.actual_size == rhs.actual_size && std::ranges::equal(lhs, rhs);
    }

    constexpr auto find(const T &value) const noexcept {
        return std::find(this->begin(), this->end(), value);
    }
    constexpr bool contains(const T &value) const noexcept {
        return this->find(value) != this->end();
    }

    template <std::same_as<T> U>
    friend std::ostream &operator<<(std::ostream &os, const wrapped_array<U, N> &arr) {
        os << "{";
        for (size_t i = 0; i < arr.actual_size; i++) {
            os << arr.data[i] << ",}"[i + 1 == arr.actual_size];
        }
        if (arr.actual_size == 0) os << "}";
        return os;
    }
};
template <typename T, size_t N> wrapped_array(std::array<T, N> &&arr) -> wrapped_array<T, N>;

template <typename T, size_t N> struct sorded_array : wrapped_array<T, N> {
    constexpr sorded_array() noexcept = default;
    constexpr sorded_array(std::initializer_list<T> list) noexcept : wrapped_array<T, N>{list} {
        std::sort(this->begin(), this->end());
    }
    constexpr sorded_array(std::ranges::range auto &&range) noexcept
        : wrapped_array<T, N>{std::forward<decltype(range)>(range)} {
        std::sort(this->begin(), this->end());
    }

    constexpr auto find(const T &value) const noexcept {
        auto it = std::lower_bound(this->begin(), this->end(), value);
        if (it != this->end() && *it == value) return it;
        return this->end();
    }
    constexpr bool contains(const T &value) const noexcept {
        return std::ranges::binary_search(*this, value);
    }
};

/*
template <typename T> class light_hash_set {
 public:
    using key_type = T;
    using value_type = T;
    using refrerence = T &;
    using const_reference = const T &;

 private:
    constexpr static auto hasher = std::hash<T>{};
    size_t m_size = 0;
    std::vector<std::vector<T>> data;

    template <typename InnerIt, typename OutterIt> class iterator_impl {
     public:
        typedef InnerIt iterator_type;
        typedef typename std::iterator_traits<iterator_type>::value_type value_type;
        typedef typename std::iterator_traits<iterator_type>::difference_type difference_type;
        typedef typename std::iterator_traits<iterator_type>::pointer pointer;
        typedef typename std::iterator_traits<iterator_type>::reference reference;
        typedef typename std::iterator_traits<iterator_type>::iterator_category iterator_category;
        typedef std::contiguous_iterator_tag iterator_concept;

     private:
        friend light_hash_set;
        InnerIt inner;
        OutterIt outter;
        const light_hash_set *parent;

        constexpr iterator_impl() noexcept = default;
        constexpr iterator_impl(OutterIt outter, const light_hash_set *parent) noexcept
            : inner(), outter(outter), parent(parent) {
            if (outter != parent->data.end()) {
                inner = outter->begin();
                if (inner == outter->end()) ++*this;
            }
        }
        constexpr iterator_impl(InnerIt inner, OutterIt outter,
                                const light_hash_set *parent) noexcept
            : inner(inner), outter(outter), parent(parent) {}

        constexpr bool next_outter() noexcept {
            while (outter != parent->data.end() && outter->begin() == outter->end()) {
                ++outter;
            }
            return outter != parent->data.end();
        }

        constexpr void prev_outter() noexcept {
            while (outter->begin() == outter->end()) {
                --outter;
            }
        }

     public:
        constexpr iterator_impl &operator++() noexcept {
            ++inner;
            if (inner == outter->end()) {
                ++outter;
                inner = next_outter() ? outter->begin() : InnerIt{};
            }
            return *this;
        }
        constexpr iterator_impl &operator++(int) noexcept {
            auto copy = *this;
            ++*this;
            return copy;
        }
        constexpr iterator_impl &operator--() noexcept {
            if (inner == outter->begin()) {
                --outter;
                prev_outter();
                inner = outter->end();
            }
            --inner;
            return *this;
        }
        constexpr iterator_impl &operator--(int) noexcept {
            auto copy = *this;
            --*this;
            return copy;
        }
        constexpr auto &operator*() const noexcept { return *inner; }
        constexpr auto operator->() const noexcept { return inner.operator->(); }
        template <typename I1, typename I2>
        constexpr bool operator==(const iterator_impl<I1, I2> &other) const noexcept {
            return inner == other.inner && outter == other.outter && parent == other.parent;
        }
        template <typename I1, typename I2>
        constexpr bool operator!=(const iterator_impl<I1, I2> &other) const noexcept {
            return !(*this == other);
        }
    };

    constexpr void rehash(size_t new_size) {
        std::vector<std::vector<T>> new_data(new_size);
        for (auto &bucket : data) {
            for (auto &&elem : bucket) {
                new_data[hasher(elem) & (new_data.size() - 1)].push_back(std::move(elem));
            }
        }
        data = std::move(new_data);
    }

 public:
    using iterator = iterator_impl<typename std::vector<T>::iterator,
                                   typename std::vector<std::vector<T>>::iterator>;
    using const_iterator = iterator_impl<typename std::vector<T>::const_iterator,
                                         typename std::vector<std::vector<T>>::const_iterator>;

    constexpr auto begin() noexcept { return iterator{data.begin(), this}; }
    constexpr auto begin() const noexcept { return const_iterator{data.begin(), this}; }
    constexpr auto end() noexcept { return iterator{data.end(), this}; }
    constexpr auto end() const noexcept { return const_iterator{data.end(), this}; }
    constexpr auto size() const noexcept { return m_size; }

 public:
    constexpr light_hash_set() noexcept : data(32) {}
    constexpr explicit light_hash_set(std::ranges::range auto &&range) : light_hash_set() {
        insert_range(range);
    }

    constexpr void clear() noexcept {
        for (auto &bucket : data)
            bucket.clear();
        m_size = 0;
    }

    constexpr void reserve(size_t new_size) {
        auto bucket_size_need = new_size * 3 / 2;
        if (bucket_size_need < data.size()) return;
        size_t new_bucket_size = 32;
        while (new_bucket_size < bucket_size_need)
            new_bucket_size <<= 1;
        rehash(new_bucket_size);
    }

    constexpr bool insert(T &&elem) {
        if ((size() + 1) * 3 / 2 > data.size()) rehash(data.size() << 1);
        auto &bucket = data[hasher(elem) & (data.size() - 1)];
        if (std::ranges::find(bucket, elem) != bucket.end()) return false;
        bucket.push_back(elem);
        ++m_size;
        return true;
    }

    constexpr bool insert(const T &elem) {
        if ((size() + 1) * 3 / 2 > data.size()) rehash(data.size() << 1);
        auto &bucket = data[hasher(elem) & (data.size() - 1)];
        if (std::ranges::find(bucket, elem) != bucket.end()) return false;
        bucket.push_back(std::move(elem));
        ++m_size;
        return true;
    }

    constexpr void insert_range(std::ranges::range auto &&range) {
        if constexpr (std::ranges::sized_range<decltype(range)>)
            reserve(std::ranges::size(range) + size());
        for (auto &&elem : range)
            insert(std::forward<decltype(elem)>(elem));
    }

    template <std::equality_comparable_with<T> U> constexpr iterator erase(const U &key) noexcept {
        auto outter = data.begin() + (std::hash<U>()(key) & (data.size() - 1));
        auto &bucket = *outter;
        auto it = std::ranges::find(bucket, key);
        if (it != bucket.end()) {
            std::swap(*it, bucket.back());
            bucket.pop_back();
            --m_size;
        }
        iterator result{it, outter, this};
        if (result == outter->end()) ++result;
        return result;
    }

    constexpr iterator erase(iterator it) noexcept {
        auto &bucket = *it.outter;
        std::swap(*it.inner, bucket.back());
        bucket.pop_back();
        --m_size;
        if (it == bucket.end()) ++it;
        return it;
    }

    template <std::equality_comparable_with<T> U> constexpr iterator find(const U &key) noexcept {
        auto outter = data.begin() + (std::hash<U>()(key) & (data.size() - 1));
        auto &bucket = *outter;
        auto it = std::ranges::find(bucket, key);
        if (it != bucket.end()) return iterator{it, outter, this};
        return end();
    }

    template <std::equality_comparable_with<T> U>
    constexpr const_iterator find(const U &key) const noexcept {
        auto outter = data.begin() + (std::hash<U>()(key) & (data.size() - 1));
        auto &bucket = *outter;
        auto it = std::ranges::find(bucket, key);
        if (it != bucket.end()) return const_iterator{it, outter, this};
        return end();
    }

    constexpr bool contains(const std::equality_comparable_with<T> auto &key) const noexcept {
        return find(key) != end();
    }
};
*/
} // namespace ctslrp::utils
