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
    } type = ERROR;
    size_t idx = 0;

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

template <size_t state_size, size_t token_size>
using ActionTable = std::array<std::array<Action, token_size>, state_size>;

template <size_t state_size, size_t symbol_size>
using GotoTable = std::array<std::array<Goto, symbol_size>, state_size>;

template <size_t state_size, size_t token_size, size_t symbol_size>
struct LRActionGotoTable {
    ActionTable<state_size, token_size> actions;
    GotoTable<state_size, state_size> gotos;

    constexpr auto copy_set_actions(size_t state, size_t token_id,
                                    Action action) const noexcept {
        auto copied = *this;
        copied.actions[state][token_id] = action;
        return copied;
    }

    constexpr auto copy_set_gotos(size_t state, size_t symbol_id,
                                  Goto go_to) const noexcept {
        auto copied = *this;
        copied.gotos[state][symbol_id] = go_to;
        return copied;
    }

    friend std::ostream &operator<<(std::ostream &os,
                                    const LRActionGotoTable &table) {
        os << std::setw(4) << "ACT";
        for (size_t i = 0; i < token_size; i++)
            os << std::setw(4) << i;
        os << " | ";
        os << std::setw(4) << "GOTO";
        for (size_t i = 0; i < symbol_size; i++)
            os << std::setw(4) << i;
        os << '\n';
        for (size_t i = 0; i < state_size; i++) {
            os << std::setw(4) << "I" + std::to_string(i);
            for (size_t j = 0; j < token_size; j++)
                os << std::setw(4) << table.actions[i][j].to_string();
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
