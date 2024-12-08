#pragma once

#include <cstddef>
#include <ostream>

namespace ctslrp::details {
struct Terminal {
    size_t id = 0;
    friend constexpr auto operator<=>(Terminal, Terminal) noexcept = default;
    friend std::ostream &operator<<(std::ostream &os, Terminal t) {
        if (t.id == size_t(-1)) return os << "#";
        if (t.id == size_t(-2)) return os << "ε";
        if (t.id == size_t(-3)) return os << "<error>";
        return os << "T" << t.id;
    }
};

struct NonTerminal {
    size_t id = 0;
    friend constexpr auto operator<=>(NonTerminal, NonTerminal) noexcept = default;
    friend std::ostream &operator<<(std::ostream &os, NonTerminal t) { return os << "S" << t.id; }
};

namespace literals {
constexpr inline Terminal sharp = Terminal{static_cast<size_t>(-1)};
constexpr inline Terminal epsilon = Terminal{static_cast<size_t>(-2)};
constexpr inline Terminal error = Terminal{static_cast<size_t>(-3)};
} // namespace literals

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
    friend constexpr auto operator<=>(SymbolT, SymbolT) noexcept = default;
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
} // namespace ctslrp::details

namespace std {
template <> struct std::hash<ctslrp::details::Terminal> {
    constexpr size_t operator()(ctslrp::details::Terminal t) const noexcept { return t.id; }
};
template <> struct std::hash<ctslrp::details::NonTerminal> {
    constexpr size_t operator()(ctslrp::details::NonTerminal t) const noexcept { return t.id; }
};
template <> struct std::hash<ctslrp::details::SymbolT> {
    constexpr size_t operator()(ctslrp::details::SymbolT t) const noexcept {
        return t.id << 1 | (static_cast<size_t>(t.type) - 1);
    }
};
} // namespace std
