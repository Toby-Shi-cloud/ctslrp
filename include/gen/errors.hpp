#pragma once

#include "slr/slr_action.hpp"
#include <exception>

namespace ctslrp::details {
struct undefined_symbol_error : std::exception {
    size_t symbol_id;
    explicit undefined_symbol_error(size_t symbol_id) noexcept : symbol_id(symbol_id) {}
    const char *what() const noexcept override { return "Undefined symbol"; }
};
struct conflict_error : std::exception {
    Conflict conflict;
    explicit conflict_error(Conflict conflict) noexcept : conflict(conflict) {}
    const char *what() const noexcept override { return "Conflict detected"; }
};
} // namespace ctslrp::details