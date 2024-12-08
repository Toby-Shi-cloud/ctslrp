#include "sysy.hpp"

constexpr auto lrtable = sysy_syntax_table().compile_lrtable<CompUnit>();
constexpr auto parser = sysy_syntax_table().compile<CompUnit>();

int main() {
    std::cout << lrtable.lr_action_goto_table << std::endl;
    return 0;
}
