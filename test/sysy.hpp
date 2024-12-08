#pragma once

#include "ctslrp.hpp"
#include <iostream>
#include <string_view>
#include <type_traits>
#include <utility>

enum GrammarType {
    // clang-format off
    CompUnit, Decl,
    ConstDecl, BType, ConstDef, ConstInitVal,
    VarDecl, VarDef, InitVal,
    FuncDef, FuncType, FuncFParams, FuncFParam,
    Block, BlockItem, Stmt,
    ForStmt, Exp, Cond, LVal, PrimaryExp, Number, Character, UnaryExp, UnaryOp,
    FuncRParams, MulExp, AddExp, RelExp, EqExp, LAndExp, LOrExp, ConstExp,
    // MARKER: tokens
    IDENFR, STRCON,
    SIZEOF_GRAMMAR_TYPE,
    // MARKER: tmp types
    ManyDeclFuncDef,
    CommaConstDef, CommaConstExp,
    CommaVarDef, CommaExp,
    BlockItems,
    OptionalForStmt, OptionalCond,
    // clang-format on
};

using ctslrp::details::valuename_of;
namespace seq = ctslrp::details::seq;
inline constexpr auto grammar_type_name_impl = seq::wrapped_map<[](auto wrapped) {
    return valuename_of<static_cast<GrammarType>(decltype(wrapped)::value)>();
}>(seq::typed_sequence{std::make_index_sequence<SIZEOF_GRAMMAR_TYPE>()});
inline constexpr auto grammar_type_name = seq::map(
    [](const auto &literal) { return literal.sv(); }, grammar_type_name_impl);

inline constexpr auto print_tokens_impl() {
    return [](auto &&...args) {
        seq::for_each(
            [](auto &&arg) {
                if constexpr (std::is_same_v<std::decay_t<decltype(arg)>,
                                             ctslrp::Token>) {
                    std::cout << arg.value << '\n';
                }
            },
            std::forward(args)...);
    };
}
constexpr auto print_tokens = print_tokens_impl();

inline constexpr auto func_of(GrammarType type) {
    return [=](auto &&...args) {
        print_tokens(std::forward(args)...);
        std::cout << grammar_type_name[(size_t)type] << std::endl;
    };
}

using empty_t = std::monostate;

inline constexpr auto sysy_syntax_table() {
    using namespace ctslrp::literals;
    constexpr auto gen = ctslrp::SyntaxRuleGenerator<GrammarType>();
    constexpr auto table = (
        // clang-format off
        /// compUnit: decl* funcDef* mainFuncDef;
        gen.decl<CompUnit>().define<ManyDeclFuncDef, ManyDeclFuncDef>().bind<>(func_of(CompUnit)),
        gen.decl<ManyDeclFuncDef>().define<>().bind<>(print_tokens),
        gen.decl<ManyDeclFuncDef>().define<Decl>().bind<>(print_tokens),
        gen.decl<ManyDeclFuncDef>().define<FuncDef>().bind<>(print_tokens),
        gen.decl<ManyDeclFuncDef>().define<ManyDeclFuncDef, Decl>().bind<>(print_tokens),
        gen.decl<ManyDeclFuncDef>().define<ManyDeclFuncDef, FuncDef>().bind<>(print_tokens),
        /// decl: constDecl | varDecl;
        gen.decl<Decl>().define<VarDecl, ConstDecl>().bind<>(func_of(Decl)),
        /// constDecl: CONSTTK bType constDef (COMMA constDef)* SEMICN;
        gen.decl<ConstDecl>().define<"const", BType, CommaConstDef, ";">().bind<>(func_of(ConstDecl)),
        gen.decl<CommaConstDef>().define<ConstDef>().bind<>(print_tokens),
        gen.decl<CommaConstDef>().define<CommaConstDef, ",", ConstDef>().bind<>(print_tokens),
        /// bType: INTTK | CHARTK;
        gen.decl<BType>().define<"int">().bind<>(func_of(BType)),
        gen.decl<BType>().define<"char">().bind<>(func_of(BType)),
        /// constDef: IDENFR (LBRACK constExp RBRACK)? ASSIGN constInitVal;
        gen.decl<ConstDef>().define<IDENFR, "=", ConstInitVal>().bind<>(func_of(ConstDef)),
        gen.decl<ConstDef>().define<IDENFR, "[", ConstExp, "]" , "=", ConstInitVal>().bind<>(func_of(ConstDef)),
        /// constInitVal: constExp | LBRACE constExp (COMMA constExp)* RBRACE;
        gen.decl<ConstInitVal>().define<ConstExp>().bind<>(func_of(ConstInitVal)),
        gen.decl<ConstInitVal>().define<"{", CommaConstExp, "}">().bind<>(func_of(ConstInitVal)),
        gen.decl<CommaConstExp>().define<ConstExp>().bind<>(print_tokens),
        gen.decl<CommaConstExp>().define<CommaConstExp, ",", ConstExp>().bind<>(print_tokens),
        /// varDecl: bType varDef (COMMA varDef)* SEMICN;
        gen.decl<VarDecl>().define<BType, CommaVarDef, ";">().bind<>(func_of(VarDecl)),
        gen.decl<CommaVarDef>().define<VarDef>().bind<>(print_tokens),
        gen.decl<CommaVarDef>().define<CommaVarDef, ",", VarDef>().bind<>(print_tokens),
        /// varDef: IDENFR (LBRACK constExp RBRACK)? (ASSIGN initVal)?;
        gen.decl<VarDef>().define<IDENFR, "=", InitVal>().bind<>(func_of(VarDef)),
        gen.decl<VarDef>().define<IDENFR, "[", ConstExp, "]" , "=", InitVal>().bind<>(func_of(VarDef)),
        gen.decl<VarDef>().define<IDENFR>().bind<>(func_of(VarDef)),
        gen.decl<VarDef>().define<IDENFR, "[", ConstExp, "]">().bind<>(func_of(VarDef)),
        /// initVal: exp | LBRACE exp (COMMA exp)* RBRACE;
        gen.decl<InitVal>().define<Exp>().bind<>(func_of(InitVal)),
        gen.decl<InitVal>().define<"{", CommaConstExp, "}">().bind<>(func_of(InitVal)),
        gen.decl<CommaExp>().define<Exp>().bind<>(print_tokens),
        gen.decl<CommaExp>().define<CommaExp, ",", Exp>().bind<>(print_tokens),
        /// funcDef: funcType IDENFR LPARENT funcFParams? RPARENT block;
        gen.decl<FuncDef>().define<FuncType, IDENFR, "(", ")", Block>().bind<>(func_of(FuncDef)),
        gen.decl<FuncDef>().define<FuncType, IDENFR, "(", FuncFParams, ")", Block>().bind<>(func_of(FuncDef)),
        /// mainFuncDef: INTTK MAINTK LPARENT RPARENT block;
        gen.decl<FuncDef>().define<"int", "main", "(", ")", Block>().bind<>(func_of(FuncDef)),
        /// funcType: INTTK | CHARTK | VOIDTK;
        gen.decl<FuncType>().define<"int">().bind<>(func_of(FuncType)),
        gen.decl<FuncType>().define<"char">().bind<>(func_of(FuncType)),
        gen.decl<FuncType>().define<"void">().bind<>(func_of(FuncType)),
        /// funcFParams: funcFParam (COMMA funcFParam)*;
        gen.decl<FuncFParams>().define<FuncFParam>().bind<>(print_tokens),
        gen.decl<FuncFParams>().define<FuncFParams, ",", FuncFParam>().bind<>(print_tokens),
        /// funcFParam: bType IDENFR (LBRACK RBRACK)?;
        gen.decl<FuncFParam>().define<BType, IDENFR>().bind<>(func_of(FuncFParam)),
        gen.decl<FuncFParam>().define<BType, IDENFR, "[", "]">().bind<>(func_of(FuncFParam)),
        /// block: LBRACE blockItem* RBRACE;
        gen.decl<Block>().define<"{", BlockItems, "}">().bind<>(func_of(Block)),
        gen.decl<BlockItems>().define<>().bind<>(print_tokens),
        gen.decl<BlockItems>().define<BlockItems, BlockItem>().bind<>(print_tokens),
        /// blockItem: decl | stmt;
        gen.decl<BlockItem>().define<Decl>().bind<>(func_of(BlockItem)),
        gen.decl<BlockItem>().define<Stmt>().bind<>(func_of(BlockItem)),
        /// stmt: lVal ASSIGN exp SEMICN # assignStmt
        gen.decl<Stmt>().define<LVal, "=", Exp, ";">().bind<>(func_of(Stmt)),
        /// stmt: exp? SEMICN # expStmt
        gen.decl<Stmt>().define<Exp, ";">().bind<>(func_of(Stmt)),
        gen.decl<Stmt>().define<";">().bind<>(func_of(Stmt)),
        /// stmt: block # blockStmt
        gen.decl<Stmt>().define<Block>().bind<>(func_of(Stmt)),
        /// stmt: IFTK LPARENT cond RPARENT stmt (ELSETK stmt)? # ifStmt
        gen.decl<Stmt>().define<"if", "(", Cond, ")", Stmt>().bind<>(func_of(Stmt)),
        gen.decl<Stmt>().define<"if", "(", Cond, ")", Stmt, "else", Stmt>().bind<>(func_of(Stmt)),
        /// stmt: WHILETK LPARENT cond RPARENT stmt # whileStmt
        gen.decl<Stmt>().define<"while", "(", Cond, ")", Stmt>().bind<>(func_of(Stmt)),
        /// FORTK LPARENT forStmt? SEMICN cond? SEMICN forStmt? RPARENT stmt # forLoopStmt
        gen.decl<Stmt>().define<"for", "(", OptionalForStmt, ";", OptionalCond, ";", OptionalForStmt, ")", Stmt>().bind<>(func_of(Stmt)),
        gen.decl<OptionalForStmt>().define<>().bind<>(print_tokens),
        gen.decl<OptionalForStmt>().define<ForStmt>().bind<>(print_tokens),
        gen.decl<OptionalCond>().define<>().bind<>(print_tokens),
        gen.decl<OptionalCond>().define<Cond>().bind<>(print_tokens),
        /// stmt: BREAKTK SEMICN # breakStmt
        gen.decl<Stmt>().define<"break", ";">().bind<>(func_of(Stmt)),
        /// stmt: CONTINUETK SEMICN # continueStmt
        gen.decl<Stmt>().define<"continue", ";">().bind<>(func_of(Stmt)),
        /// stmt: RETURNTK exp? SEMICN # returnStmt
        gen.decl<Stmt>().define<"return", ";">().bind<>(func_of(Stmt)),
        gen.decl<Stmt>().define<"return", Exp, ";">().bind<>(func_of(Stmt)),
        /// forStmt: lVal ASSIGN exp;
        gen.decl<ForStmt>().define<LVal, "=", Exp>().bind<>(func_of(ForStmt)),
        /// exp: addExp;
        gen.decl<Exp>().define<AddExp>().bind<>(func_of(Exp)),
        /// cond: lOrExp;
        gen.decl<Cond>().define<LOrExp>().bind<>(func_of(Cond)),
        /// lVal: IDENFR (LBRACK exp RBRACK)?;
        gen.decl<LVal>().define<IDENFR>().bind<>(func_of(LVal)),
        gen.decl<LVal>().define<IDENFR, "[", Exp, "]">().bind<>(func_of(LVal)),
        /// primaryExp: LPARENT exp RPARENT | lVal | number | character | strcon;
        gen.decl<PrimaryExp>().define<"(", Exp, ")">().bind<>(func_of(PrimaryExp)),
        gen.decl<PrimaryExp>().define<LVal>().bind<>(func_of(PrimaryExp)),
        gen.decl<PrimaryExp>().define<Number>().bind<>(func_of(PrimaryExp)),
        gen.decl<PrimaryExp>().define<Character>().bind<>(func_of(PrimaryExp)),
        gen.decl<PrimaryExp>().define<STRCON>().bind<>(func_of(PrimaryExp)),
        /// number: INTCON;
        gen.decl<Number>().define<"[0-9]*"_r>().bind<>(func_of(Number)),
        /// character: CHARCON;
        gen.decl<Character>().define<"'[a-zA-Z]'"_r>().bind<>(func_of(Character)),
        gen.decl<Character>().define<"'\\\\.'"_r>().bind<>(func_of(Character)),
        /// strcon: STRCON;
        gen.decl<STRCON>().define<R"("([^"]|\\")*")"_r>().bind<>(func_of(STRCON)),
        /// unaryExp: primaryExp | IDENFR LPARENT funcRParams? RPARENT | unaryOp unaryExp;
        gen.decl<UnaryExp>().define<PrimaryExp>().bind<>(func_of(UnaryExp)),
        gen.decl<UnaryExp>().define<IDENFR, "(", ")">().bind<>(func_of(UnaryExp)),
        gen.decl<UnaryExp>().define<IDENFR, "(", FuncRParams, ")">().bind<>(func_of(UnaryExp)),
        gen.decl<UnaryExp>().define<UnaryOp, UnaryExp>().bind<>(func_of(UnaryExp)),
        /// unaryOp: PLUS | MINU | NOT;
        gen.decl<UnaryOp>().define<"+">().bind<>(func_of(UnaryOp)),
        gen.decl<UnaryOp>().define<"-">().bind<>(func_of(UnaryOp)),
        gen.decl<UnaryOp>().define<"!">().bind<>(func_of(UnaryOp)),
        /// funcRParams: exp (COMMA exp)*;
        gen.decl<FuncRParams>().define<Exp>().bind<>(print_tokens),
        gen.decl<FuncRParams>().define<FuncRParams, ",", Exp>().bind<>(print_tokens),
        /// mulExp: unaryExp ((MULT | DIV | MOD) unaryExp)*;
        gen.decl<MulExp>().define<UnaryExp>().bind<>(func_of(MulExp)),
        gen.decl<MulExp>().define<MulExp, "*", UnaryExp>().bind<>(func_of(MulExp)),
        gen.decl<MulExp>().define<MulExp, "/", UnaryExp>().bind<>(func_of(MulExp)),
        gen.decl<MulExp>().define<MulExp, "%", UnaryExp>().bind<>(func_of(MulExp)),
        /// addExp: mulExp ((PLUS | MINU) mulExp)*;
        gen.decl<AddExp>().define<MulExp>().bind<>(func_of(AddExp)),
        gen.decl<AddExp>().define<AddExp, "+", MulExp>().bind<>(func_of(AddExp)),
        gen.decl<AddExp>().define<AddExp, "-", MulExp>().bind<>(func_of(AddExp)),
        /// relExp: addExp ((LT | RT | LEQ | GEQ) addExp)*;
        gen.decl<RelExp>().define<AddExp>().bind<>(func_of(RelExp)),
        gen.decl<RelExp>().define<RelExp, "<", AddExp>().bind<>(func_of(RelExp)),
        gen.decl<RelExp>().define<RelExp, ">", AddExp>().bind<>(func_of(RelExp)),
        gen.decl<RelExp>().define<RelExp, "<=", AddExp>().bind<>(func_of(RelExp)),
        gen.decl<RelExp>().define<RelExp, ">=", AddExp>().bind<>(func_of(RelExp)),
        /// eqExp: relExp ((EQ | NEQ) relExp)*;
        gen.decl<EqExp>().define<RelExp>().bind<>(func_of(EqExp)),
        gen.decl<EqExp>().define<EqExp, "==", RelExp>().bind<>(func_of(EqExp)),
        gen.decl<EqExp>().define<EqExp, "!=", RelExp>().bind<>(func_of(EqExp)),
        /// lAndExp: eqExp (AND eqExp)*;
        gen.decl<LAndExp>().define<EqExp>().bind<>(func_of(LAndExp)),
        gen.decl<LAndExp>().define<LAndExp, "&&", EqExp>().bind<>(func_of(LAndExp)),
        /// lOrExp: lAndExp (OR lAndExp)*;
        gen.decl<LOrExp>().define<LAndExp>().bind<>(func_of(LOrExp)),
        gen.decl<LOrExp>().define<LOrExp, "||", LAndExp>().bind<>(func_of(LOrExp)),
        /// constExp: addExp;
        gen.decl<ConstExp>().define<AddExp>().bind<>(func_of(ConstExp)),
        /// IDENFR:
        gen.decl<IDENFR>().define<R"([a-zA-Z_]\w*)"_r>().bind<>(func_of(IDENFR))
        // clang-format on
    );
    return table;
}
