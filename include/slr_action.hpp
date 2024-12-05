#pragma once

#include <array>
#include <cstddef>
#include <cstring>
#include <iomanip>
#include <ostream>
#include <string>

namespace ctslrp::details {
struct Action {
    enum ActionType {
        ERROR,
        SHIFT,
        REDUCE,
        ACCEPT,
    } type : 4 = ERROR;
    size_t idx : 60 = 0;

    std::string to_string() const {
        switch (type) {
        case Action::ERROR:
            return "E";
        case Action::SHIFT:
            return "S" + std::to_string(idx);
        case Action::REDUCE:
            return "R" + std::to_string(idx);
        case Action::ACCEPT:
            return "ACC";
        }
        __builtin_unreachable();
    }

    constexpr bool operator==(Action other) const noexcept {
        return type == other.type && idx == other.idx;
    }
};

struct Goto {
    static constexpr size_t error = -1;
    size_t idx = error;

    std::string to_string() const {
        if (idx == error) return "E";
        return std::to_string(idx);
    }
};

struct Conflict {
    enum Type { S_R, R_R } type;
    size_t state, symbol;
    Action action1, action2;
};

template <size_t token_size> struct ActionTableLine {
    std::array<Action, token_size + 1> data;

    constexpr auto operator[](size_t state) const noexcept {
        if (state == -1) return data[token_size];
        return data[state];
    }
    constexpr auto &operator[](size_t state) noexcept {
        if (state == -1) return data[token_size];
        return data[state];
    }
};

template <size_t state_size, size_t token_size>
using ActionTable = std::array<ActionTableLine<token_size>, state_size>;

template <size_t state_size, size_t symbol_size>
using GotoTable = std::array<std::array<Goto, symbol_size>, state_size>;

template <size_t state_size, size_t token_size, size_t symbol_size>
struct LRActionGotoTable {
    ActionTable<state_size, token_size> actions;
    GotoTable<state_size, symbol_size> gotos;

    friend std::ostream &operator<<(std::ostream &os,
                                    const LRActionGotoTable &table) {
        os << std::setw(4) << "ACT";
        for (size_t i = 0; i < token_size; i++)
            os << std::setw(4) << i;
        os << std::setw(4) << "#";
        os << " | ";
        os << std::setw(4) << "GOTO";
        for (size_t i = 0; i < symbol_size; i++)
            os << std::setw(4) << i;
        os << '\n';
        for (size_t i = 0; i < state_size; i++) {
            os << std::setw(4) << "I" + std::to_string(i);
            for (size_t j = 0; j < token_size; j++)
                os << std::setw(4) << table.actions[i][j].to_string();
            os << std::setw(4) << table.actions[i][-1].to_string();
            os << " | ";
            os << std::setw(4) << "I" + std::to_string(i);
            for (size_t j = 0; j < symbol_size; j++)
                os << std::setw(4) << table.gotos[i][j].to_string();
            os << '\n';
        }
        return os;
    }
};
} // namespace ctslrp::details
