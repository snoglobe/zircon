//
// Created by snwy on 1/15/23.
//

#include "zircon.h"
#include <iostream>

std::string tokenTypeToString(zircon::TokenType type) {
    switch (type) {
        case zircon::TokenType::LEFT_PAREN:
            return "LEFT_PAREN";
        case zircon::TokenType::RIGHT_PAREN:
            return "RIGHT_PAREN";
        case zircon::TokenType::LEFT_BRACE:
            return "LEFT_BRACE";
        case zircon::TokenType::RIGHT_BRACE:
            return "RIGHT_BRACE";
        case zircon::TokenType::COMMA:
            return "COMMA";
        case zircon::TokenType::MINUS:
            return "MINUS";
        case zircon::TokenType::PLUS:
            return "PLUS";
        case zircon::TokenType::SLASH:
            return "SLASH";
        case zircon::TokenType::STAR:
            return "STAR";
        case zircon::TokenType::MODULO:
            return "MODULO";
        case zircon::TokenType::LESS:
            return "LESS";
        case zircon::TokenType::LESS_EQUAL:
            return "LESS_EQUAL";
        case zircon::TokenType::GREATER:
            return "GREATER";
        case zircon::TokenType::GREATER_EQUAL:
            return "GREATER_EQUAL";
        case zircon::TokenType::EQUAL:
            return "EQUAL";
        case zircon::TokenType::NOT_EQUAL:
            return "NOT_EQUAL";
        case zircon::TokenType::AND:
            return "AND";
        case zircon::TokenType::OR:
            return "OR";
        case zircon::TokenType::NOT:
            return "NOT";
        case zircon::TokenType::COLON:
            return "COLON";
        case zircon::TokenType::IDENTIFIER:
            return "IDENTIFIER";
        case zircon::TokenType::STRING:
            return "STRING";
        case zircon::TokenType::NUMBER:
            return "NUMBER";
        case zircon::TokenType::TRUE:
            return "TRUE";
        case zircon::TokenType::FALSE:
            return "FALSE";
        case zircon::TokenType::NIL:
            return "NIL";
        case zircon::TokenType::IF:
            return "IF";
        case zircon::TokenType::ELSE:
            return "ELSE";
        case zircon::TokenType::WHILE:
            return "WHILE";
        case zircon::TokenType::FOR:
            return "FOR";
        case zircon::TokenType::FN:
            return "FN";
        case zircon::TokenType::RETURN:
            return "RETURN";
        case zircon::TokenType::BREAK:
            return "BREAK";
        case zircon::TokenType::CONTINUE:
            return "CONTINUE";
        case zircon::TokenType::NEWLINE:
            return "NEWLINE";
        case zircon::TokenType::END:
            return "END";
        case zircon::TokenType::ASSIGN:
            return "ASSIGN";
        case zircon::TokenType::PLUS_ASSIGN:
            return "PLUS_ASSIGN";
        case zircon::TokenType::MINUS_ASSIGN:
            return "MINUS_ASSIGN";
        case zircon::TokenType::STAR_ASSIGN:
            return "STAR_ASSIGN";
        case zircon::TokenType::SLASH_ASSIGN:
            return "SLASH_ASSIGN";
        case zircon::TokenType::MODULO_ASSIGN:
            return "MODULO_ASSIGN";
        case zircon::TokenType::ARROW:
            return "ARROW";
        case zircon::TokenType::EOF_:
            return "EOF";
    }
}

int main() {
    std::string input = R"(fn foo a, b -> a + b
    print "foo")";
    zircon::ZirconContext context(input);
    context.statement();
    context.statement();
    return 0;
}