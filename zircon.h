//
// Created by snwy on 1/15/23.
//

#ifndef ZIRCON_ZIRCON_H
#define ZIRCON_ZIRCON_H

#include <string>
#include <unordered_map>
#include <any>
#include <utility>
#include <vector>
#include <iostream>
#include <variant>
#include <stack>

namespace zircon {
    class ZirconException : public std::exception {
        std::string message;
    public:
        explicit ZirconException(std::string message) : message(std::move(message)) {}
        [[nodiscard]] const char *what() const noexcept override {
            return message.c_str();
        }
    };
    class InternalZirconException : public std::exception {
    public:
        enum class Type {
            BREAK, CONTINUE
        };
        Type type;
        explicit InternalZirconException(Type type) : type(type) {}
    };
    enum class TokenType {
        LEFT_PAREN, RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE,
        COMMA, MINUS, PLUS, SLASH, STAR, MODULO,
        LESS, LESS_EQUAL, GREATER, GREATER_EQUAL,
        EQUAL, NOT_EQUAL, AND, OR, NOT, COLON,
        IDENTIFIER, STRING, NUMBER, TRUE, FALSE, NIL,
        IF, ELSE, WHILE, FOR, FN, RETURN, BREAK, CONTINUE,
        NEWLINE, END, ASSIGN, PLUS_ASSIGN, MINUS_ASSIGN, STAR_ASSIGN, SLASH_ASSIGN, MODULO_ASSIGN,
        ARROW, EOF_,
        // TODO: DEBUG
        PRINT,
    };
    std::unordered_map<std::string, TokenType> keywords = {
            {"and",      TokenType::AND},
            {"or",       TokenType::OR},
            {"not",      TokenType::NOT},
            {"if",       TokenType::IF},
            {"else",     TokenType::ELSE},
            {"while",    TokenType::WHILE},
            {"for",      TokenType::FOR},
            {"fn",       TokenType::FN},
            {"return",   TokenType::RETURN},
            {"break",    TokenType::BREAK},
            {"continue", TokenType::CONTINUE},
            {"true",     TokenType::TRUE},
            {"false",    TokenType::FALSE},
            {"nil",      TokenType::NIL},
            {"end",      TokenType::END},
            {"print",    TokenType::PRINT},
    };
    struct Token {
        TokenType type;
        std::string lexeme;
        std::any literal;
        int line;
        Token(TokenType type, std::string lexeme, std::any literal, int line)
        : type(type), lexeme(std::move(lexeme)), literal(std::move(literal)), line(line) {}
    };
    class Scanner {
    private:
        std::string source;
        int line = 1;
        int current = 0;

        char advance() {
            if (current >= source.length()) return '\0';
            return source[current++];
        }
        char peek() {
            if (current >= source.length()) return '\0';
            return source[current];
        }
        bool match(char expected) {
            if (source[current] != expected || current >= source.length()) return false;
            current++;
            return true;
        }
        static bool isAlpha(char c) {
            return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
        }
        Token string() {
            std::string value;
            while (peek() != '"' && current < source.length()) {
                if (peek() == '\n') line++;
                value += advance();
            }
            if (current >= source.length())
                throw ZirconException("Unterminated string.");
            advance();
            return {TokenType::STRING, value, value, line};
        }
        Token number() {
            std::string value;
            while (isdigit(peek())) value += advance();
            return {TokenType::NUMBER, value, std::stoi(value), line};
        }
        Token identifier() {
            std::string value;
            while (isAlpha(peek()) || isdigit(peek())) value += advance();
            if (keywords.count(value)) return {keywords[value], value, std::any(), line};
            return {TokenType::IDENTIFIER, value, std::any(), line};
        }
    public:
        explicit Scanner(std::string source) : source(std::move(source)) {}

        Token nextToken();
    };
    Token Scanner::nextToken() {
        char c = advance();
        switch (c) {
            case '(': return {TokenType::LEFT_PAREN, "(", nullptr, line};
            case ')': return {TokenType::RIGHT_PAREN, ")", nullptr, line};
            case '{': return {TokenType::LEFT_BRACE, "{", nullptr, line};
            case '}': return {TokenType::RIGHT_BRACE, "}", nullptr, line};
            case ',': return {TokenType::COMMA, ",", nullptr, line};
            case '-': if (match('>')) return {TokenType::ARROW, "->", nullptr, line};
                      else if (match('=')) return {TokenType::MINUS_ASSIGN, "-=", nullptr, line};
                      else return {TokenType::MINUS, "-", nullptr, line};
            case '+': if (match('=')) return {TokenType::PLUS_ASSIGN, "+=", nullptr, line};
                      else return {TokenType::PLUS, "+", nullptr, line};
            case '/': if (match('=')) return {TokenType::SLASH_ASSIGN, "/=", nullptr, line};
                      else return {TokenType::SLASH, "/", nullptr, line};
            case '*': if (match('=')) return {TokenType::STAR_ASSIGN, "*=", nullptr, line};
                      else return {TokenType::STAR, "*", nullptr, line};
            case '%': if (match('=')) return {TokenType::MODULO_ASSIGN, "%=", nullptr, line};
                      else return {TokenType::MODULO, "%", nullptr, line};
            case '<': if (match('=')) return {TokenType::LESS_EQUAL, "<=", nullptr, line};
                      else return {TokenType::LESS, "<", nullptr, line};
            case '>': if (match('=')) return {TokenType::GREATER_EQUAL, ">=", nullptr, line};
                      else return {TokenType::GREATER, ">", nullptr, line};
            case '=': if (match('=')) return {TokenType::EQUAL, "==", nullptr, line};
                      else return {TokenType::ASSIGN, "=", nullptr, line};
            case ':': return {TokenType::COLON, ":", nullptr, line};
            case '\n': line++; return {TokenType::NEWLINE, "\n", nullptr, line};
            case ' ': case '\r': case '\t': return nextToken();
            case '"': return string();
            case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':
                current--;
                return number();
            case '\0': return {TokenType::EOF_, "", nullptr, line};
            default: if (isAlpha(c)) { current--; return identifier(); }
                     else throw ZirconException("Unexpected character " + std::string(1, c));
        }
    }
    enum class InstructionType {
        PUSH, PUSHI, TRUE, FALSE, POP, ADD, SUB, MUL, DIV, MOD, NEG, NOT, AND, OR, EQ, NEQ, LT, GT, LTE, GTE, CALL, RET, LOAD, STORE, IF, FLOOP, WLOOP, IDX, SETIDX, BREAK, CONTINUE, DUP, PRINT
    };
    struct Instruction {
        InstructionType type;
        int operand;
    };
    struct ZirconObj;
    struct ZirconCodeUnit {
        std::vector<Instruction> instructions;
        std::vector<ZirconObj> constants;
        void emit(InstructionType type, int operand = 0) {
            instructions.push_back({type, operand});
        }
        int getConstantIndex(const ZirconObj& obj);
    };
    struct ZirconFunction {
        std::vector<std::string> parameters;
        ZirconCodeUnit code;
    };
    struct ZirconObj {
        std::variant<int, std::string, bool, std::vector<ZirconObj>, ZirconFunction, ZirconCodeUnit, std::monostate> value{};
        explicit ZirconObj() : value(std::monostate()) {}
        explicit ZirconObj(int value) : value(value) {}
        explicit ZirconObj(std::string value) : value(std::move(value)) {}
        explicit ZirconObj(bool value) : value(value) {}
        explicit ZirconObj(std::vector<ZirconObj> value) : value(std::move(value)) {}
        explicit ZirconObj(ZirconFunction value) : value(std::move(value)) {}
        explicit ZirconObj(ZirconCodeUnit value) : value(std::move(value)) {}
        [[nodiscard]] int asInt() const { return std::get<int>(value); }
        [[nodiscard]] std::string asString() const { return std::get<std::string>(value); }
        [[nodiscard]] bool asBool() const { return std::get<bool>(value); }
        [[nodiscard]] std::vector<ZirconObj> asVector() const { return std::get<std::vector<ZirconObj>>(value); }
        [[nodiscard]] ZirconFunction asFunction() const { return std::get<ZirconFunction>(value); }
        [[nodiscard]] ZirconCodeUnit asCodeUnit() const { return std::get<ZirconCodeUnit>(value); }
        bool operator==(const ZirconObj& other) const {
            if(value.index() != other.value.index()) return false;
            switch (value.index()) {
                case 0: return asInt() == other.asInt();
                case 1: return asString() == other.asString();
                case 2: return asBool() == other.asBool();
                case 3: return asVector() == other.asVector();
                case 4: case 5: return false;
                default: throw ZirconException("Invalid ZirconObj type.");
            }
        }
        bool operator!=(const ZirconObj& other) const { return !(*this == other); }
    };
    int ZirconCodeUnit::getConstantIndex(const ZirconObj& obj) {
        constants.push_back(obj);
        return constants.size() - 1;
    }
    class ZirconContext {
        bool compileMode = false;
        Scanner scanner;
        Token current;
        std::unordered_map<std::string, ZirconObj> globals;
        std::vector<std::unordered_map<std::string, ZirconObj>> locals;
        ZirconCodeUnit currentCodeUnit;
        TokenType peek() const { return current.type; }
        bool ignoreNewlines = false;
        bool peek(TokenType type) const { return current.type == type; }
        Token eat(TokenType type) {
            if(ignoreNewlines) {
                while(peek(TokenType::NEWLINE)) current = scanner.nextToken();
            }
            if (current.type == type) {
                Token token = current;
                current = scanner.nextToken();
                return token;
            }
            throw ZirconException("Unexpected token " + std::to_string((int)peek()) + " at line " + std::to_string(current.line) + " column " + std::to_string(current.line));
        }
        ZirconObj lookupVariable(const std::string& name) {
            if(locals.empty()) {
                if(globals.find(name) == globals.end()) throw ZirconException("Undefined variable.");
                return globals[name];
            }
            auto search = locals.end() - 1;
            while (search->find(name) == search->end()) {
                if (search == locals.begin()) {
                    if (globals.find(name) == globals.end()) throw ZirconException("Undefined variable " + name + ".");
                    return globals[name];
                }
                search--;
            }
            return (*search)[name];
        }
        void setVariable(const std::string& name, ZirconObj value) {
            if(locals.empty()) {
                globals[name] = value;
                return;
            }
            auto search = locals.end() - 1;
            while (search->find(name) == search->end()) {
                if (search == locals.begin()) {
                    if (globals.find(name) == globals.end()) {
                        locals.back()[name] = value;
                        return;
                    } else {
                        globals[name] = value;
                        return;
                    }
                }
                search--;
            }
            (*search)[name] = std::move(value);
        }
    public:
        ZirconObj execute(ZirconCodeUnit unit) {
            std::stack<ZirconObj> stack{};
            for(int pc = 0; pc < unit.instructions.size(); pc++) {
                switch (unit.instructions[pc].type) {
                    case InstructionType::PUSH: stack.push(unit.constants[unit.instructions[pc].operand]); break;
                    case InstructionType::PUSHI: stack.push(ZirconObj(unit.instructions[pc].operand)); break;
                    case InstructionType::TRUE: stack.push(ZirconObj(true)); break;
                    case InstructionType::FALSE: stack.push(ZirconObj(false)); break;
                    case InstructionType::POP: stack.pop(); break;
                    case InstructionType::ADD: {
                        ZirconObj b = stack.top(); stack.pop();
                        ZirconObj a = stack.top(); stack.pop();
                        if (a.value.index() == 0 && b.value.index() == 0) stack.push(ZirconObj(a.asInt() + b.asInt()));
                        else if (a.value.index() == 1 && b.value.index() == 1) stack.push(ZirconObj(a.asString() + b.asString()));
                        else throw ZirconException("Invalid operands for +.");
                        break;
                    }
                    case InstructionType::SUB: {
                        ZirconObj b = stack.top(); stack.pop();
                        ZirconObj a = stack.top(); stack.pop();
                        if (a.value.index() == 0 && b.value.index() == 0) stack.push(ZirconObj(a.asInt() - b.asInt()));
                        else throw ZirconException("Invalid operands for -.");
                        break;
                    }
                    case InstructionType::MUL: {
                        ZirconObj b = stack.top(); stack.pop();
                        ZirconObj a = stack.top(); stack.pop();
                        if (a.value.index() == 0 && b.value.index() == 0) stack.push(ZirconObj(a.asInt() * b.asInt()));
                        else throw ZirconException("Invalid operands for *.");
                        break;
                    }
                    case InstructionType::DIV: {
                        ZirconObj b = stack.top(); stack.pop();
                        ZirconObj a = stack.top(); stack.pop();
                        if (a.value.index() == 0 && b.value.index() == 0) stack.push(ZirconObj(a.asInt() / b.asInt()));
                        else throw ZirconException("Invalid operands for /.");
                        break;
                    }
                    case InstructionType::MOD: {
                        ZirconObj b = stack.top(); stack.pop();
                        ZirconObj a = stack.top(); stack.pop();
                        if (a.value.index() == 0 && b.value.index() == 0) stack.push(ZirconObj(a.asInt() % b.asInt()));
                        else throw ZirconException("Invalid operands for %.");
                        break;
                    }
                    case InstructionType::NEG: {
                        ZirconObj a = stack.top(); stack.pop();
                        if (a.value.index() == 0) stack.push(ZirconObj(-a.asInt()));
                        else throw ZirconException("Invalid operand for -.");
                        break;
                    }
                    case InstructionType::NOT: {
                        ZirconObj a = stack.top(); stack.pop();
                        if (a.value.index() == 2) stack.push(ZirconObj(!a.asBool()));
                        else throw ZirconException("Invalid operand for !.");
                        break;
                    }
                    case InstructionType::AND: {
                        ZirconObj b = stack.top(); stack.pop();
                        ZirconObj a = stack.top(); stack.pop();
                        if (a.value.index() == 2 && b.value.index() == 2) stack.push(ZirconObj(a.asBool() && b.asBool()));
                        else throw ZirconException("Invalid operands for &&.");
                        break;
                    }
                    case InstructionType::OR: {
                        ZirconObj b = stack.top(); stack.pop();
                        ZirconObj a = stack.top(); stack.pop();
                        if (a.value.index() == 2 && b.value.index() == 2) stack.push(ZirconObj(a.asBool() || b.asBool()));
                        else throw ZirconException("Invalid operands for ||.");
                        break;
                    }
                    case InstructionType::EQ: {
                        ZirconObj b = stack.top(); stack.pop();
                        ZirconObj a = stack.top(); stack.pop();
                        stack.push(ZirconObj(a == b));
                        break;
                    }
                    case InstructionType::NEQ: {
                        ZirconObj b = stack.top(); stack.pop();
                        ZirconObj a = stack.top(); stack.pop();
                        stack.push(ZirconObj(a != b));
                        break;
                    }
                    case InstructionType::LT: {
                        ZirconObj b = stack.top(); stack.pop();
                        ZirconObj a = stack.top(); stack.pop();
                        if (a.value.index() == 0 && b.value.index() == 0) stack.push(ZirconObj(a.asInt() < b.asInt()));
                        else if (a.value.index() == 1 && b.value.index() == 1) stack.push(ZirconObj(a.asString() < b.asString()));
                        else throw ZirconException("Invalid operands for <.");
                        break;
                    }
                    case InstructionType::GT: {
                        ZirconObj b = stack.top(); stack.pop();
                        ZirconObj a = stack.top(); stack.pop();
                        if (a.value.index() == 0 && b.value.index() == 0) stack.push(ZirconObj(a.asInt() > b.asInt()));
                        else if (a.value.index() == 1 && b.value.index() == 1) stack.push(ZirconObj(a.asString() > b.asString()));
                        else throw ZirconException("Invalid operands for >.");
                        break;
                    }
                    case InstructionType::LTE: {
                        ZirconObj b = stack.top(); stack.pop();
                        ZirconObj a = stack.top(); stack.pop();
                        if (a.value.index() == 0 && b.value.index() == 0) stack.push(ZirconObj(a.asInt() <= b.asInt()));
                        else if (a.value.index() == 1 && b.value.index() == 1) stack.push(ZirconObj(a.asString() <= b.asString()));
                        else throw ZirconException("Invalid operands for <=.");
                        break;
                    }
                    case InstructionType::GTE: {
                        ZirconObj b = stack.top(); stack.pop();
                        ZirconObj a = stack.top(); stack.pop();
                        if (a.value.index() == 0 && b.value.index() == 0) stack.push(ZirconObj(a.asInt() >= b.asInt()));
                        else if (a.value.index() == 1 && b.value.index() == 1) stack.push(ZirconObj(a.asString() >= b.asString()));
                        else throw ZirconException("Invalid operands for >=.");
                        break;
                    }
                    case InstructionType::IF: {
                        ZirconCodeUnit then = stack.top().asCodeUnit(); stack.pop();
                        ZirconCodeUnit else_ = stack.top().asCodeUnit(); stack.pop();
                        ZirconObj cond = stack.top(); stack.pop();
                        if (cond.value.index() == 2) {
                            locals.emplace_back();
                            if (cond.asBool()) execute(then);
                            else execute(else_);
                            locals.pop_back();
                        } else throw ZirconException("Invalid operand for if.");
                        break;
                    }
                    case InstructionType::WLOOP: {
                        ZirconCodeUnit body = stack.top().asCodeUnit(); stack.pop();
                        ZirconCodeUnit cond = stack.top().asCodeUnit(); stack.pop();
                        while (execute(cond).asBool()) {
                            locals.emplace_back();
                            execute(body);
                            locals.pop_back();
                        }
                        break;
                    }
                    case InstructionType::FLOOP: {
                        ZirconCodeUnit body = stack.top().asCodeUnit(); stack.pop();
                        ZirconObj iter = stack.top(); stack.pop();
                        auto name = unit.constants[unit.instructions[pc].operand].asString();
                        if (iter.value.index() == 3) {
                            for (auto& i : iter.asVector()) {
                                try {
                                    locals.emplace_back();
                                    locals.back().emplace(name, i);
                                    execute(body);
                                    locals.pop_back();
                                } catch (InternalZirconException& e) {
                                    if (e.type == InternalZirconException::Type::BREAK) {
                                        locals.pop_back();
                                        break;
                                    }
                                    else if (e.type == InternalZirconException::Type::CONTINUE) {
                                        locals.pop_back();
                                        continue;
                                    }
                                }
                            }
                        } else if(iter.value.index() == 1) {
                            for (auto& i : iter.asString()) {
                                try {
                                    locals.emplace_back();
                                    locals.back().emplace(name, ZirconObj(std::string(1, i)));
                                    execute(body);
                                    locals.pop_back();
                                } catch (InternalZirconException& e) {
                                    if (e.type == InternalZirconException::Type::BREAK) {
                                        locals.pop_back();
                                        break;
                                    }
                                    else if (e.type == InternalZirconException::Type::CONTINUE) {
                                        locals.pop_back();
                                        continue;
                                    }
                                }
                            }
                        } else throw ZirconException("Invalid operand for for.");
                        break;
                    }
                    case InstructionType::CALL: {
                        ZirconObj a = stack.top(); stack.pop();
                        if (a.value.index() == 4) {
                            ZirconFunction fn = a.asFunction();
                            auto tmpLocals = locals;
                            locals = {};
                            locals.emplace_back();
                            for (int i = 0; i < unit.instructions[pc].operand; i++) {
                                locals.back()[fn.parameters[i]] = stack.top();
                                stack.pop();
                            }
                            auto result = execute(fn.code);
                            locals = tmpLocals;
                            stack.push(result);
                        }
                        break;
                    }
                    case InstructionType::RET:
                        return stack.top();
                    case InstructionType::LOAD:
                        stack.push(lookupVariable(unit.constants[unit.instructions[pc].operand].asString()));
                        break;
                    case InstructionType::STORE:
                        setVariable(unit.constants[unit.instructions[pc].operand].asString(), stack.top());
                        stack.pop();
                        break;
                    case InstructionType::IDX: {
                        ZirconObj a = stack.top(); stack.pop();
                        ZirconObj b = stack.top(); stack.pop();
                        if (a.value.index() == 3 && b.value.index() == 0) stack.push(a.asVector()[b.asInt()]);
                        else if (a.value.index() == 1 && b.value.index() == 0) stack.push(ZirconObj(std::string(1, a.asString()[b.asInt()])));
                        else throw ZirconException("Invalid operands for indexing.");
                        break;
                    }
                    case InstructionType::SETIDX: {
                        ZirconObj a = stack.top(); stack.pop();
                        ZirconObj c = stack.top(); stack.pop();
                        ZirconObj b = stack.top(); stack.pop();
                        if (a.value.index() == 3 && b.value.index() == 0) a.asVector()[b.asInt()] = c;
                        else if (a.value.index() == 1 && b.value.index() == 0) a.asString()[b.asInt()] = c.asString()[0];
                        else throw ZirconException("Invalid operands for indexing.");
                        break;
                    }
                    case InstructionType::BREAK:
                        throw InternalZirconException(InternalZirconException::Type::BREAK);
                    case InstructionType::CONTINUE:
                        throw InternalZirconException(InternalZirconException::Type::CONTINUE);
                    case InstructionType::DUP:
                        stack.push(stack.top());
                        break;
                    case InstructionType::PRINT: // TODO: DEBUG
                        switch(stack.top().value.index()) {
                            case 0:
                                std::cout << stack.top().asInt();
                                break;
                            case 1:
                                std::cout << stack.top().asString();
                                break;
                            case 2:
                                std::cout << (stack.top().asBool() ? "true" : "false");
                                break;
                        }
                        stack.pop();
                        break;
                }
            }
            return ZirconObj();
        }
        explicit ZirconContext(std::string source) : scanner(std::move(source)), current(scanner.nextToken()) {}
        std::vector<ZirconObj> exprList() {
            std::vector<ZirconObj> exprs;
            exprs.push_back(expression());
            while (peek(TokenType::COMMA)) {
                eat(TokenType::COMMA);
                exprs.push_back(expression());
            }
            return exprs;
        }
        std::vector<std::string> idList() {
            std::vector<std::string> ids;
            ids.push_back(eat(TokenType::IDENTIFIER).lexeme);
            while (peek(TokenType::COMMA)) {
                eat(TokenType::COMMA);
                ids.push_back(eat(TokenType::IDENTIFIER).lexeme);
            }
            return ids;
        }
        ZirconObj primary() {
            switch (peek()) {
                case TokenType::NUMBER: {
                    if (compileMode) { currentCodeUnit.emit(InstructionType::PUSHI,  std::any_cast<int>(eat(TokenType::NUMBER).literal)); return ZirconObj(); }
                    else return ZirconObj(std::any_cast<int>(eat(TokenType::NUMBER).literal));
                    break;
                }
                case TokenType::STRING: {
                    if (compileMode) { currentCodeUnit.emit(InstructionType::PUSH, currentCodeUnit.getConstantIndex(ZirconObj(eat(TokenType::STRING).lexeme))); return ZirconObj(); }
                    else return ZirconObj(eat(TokenType::STRING).lexeme);
                    break;
                }
                case TokenType::TRUE: {
                    eat(TokenType::TRUE);
                    if (compileMode) { currentCodeUnit.emit(InstructionType::TRUE); return ZirconObj(); }
                    else return ZirconObj(true);
                    break;
                }
                case TokenType::FALSE: {
                    eat(TokenType::FALSE);
                    if (compileMode) { currentCodeUnit.emit(InstructionType::FALSE); return ZirconObj(); }
                    else return ZirconObj(false);
                    break;
                }
                case TokenType::LEFT_PAREN: {
                    eat(TokenType::LEFT_PAREN);
                    bool prev = ignoreNewlines;
                    ignoreNewlines = true;
                    auto expr = expression();
                    ignoreNewlines = prev;
                    eat(TokenType::RIGHT_PAREN);
                    return expr;
                }
                case TokenType::IDENTIFIER: {
                    if(compileMode) {
                        currentCodeUnit.emit(InstructionType::LOAD, currentCodeUnit.getConstantIndex(ZirconObj(eat(TokenType::IDENTIFIER).lexeme)));
                        return ZirconObj();
                    } else {
                        auto identifier = eat(TokenType::IDENTIFIER).lexeme;
                        return lookupVariable(identifier);
                    }
                    break;
                }
                default: throw ZirconException("Unexpected token " + std::to_string((int)peek()) + " at line " + std::to_string(current.line));
            }
        }
        ZirconObj callFn(ZirconFunction func, std::vector<ZirconObj> args) {
            if (func.parameters.size() != args.size()) throw ZirconException("Wrong number of arguments.");
            locals.emplace_back();
            for (int i = 0; i < args.size(); i++) {
                locals.back()[func.parameters[i]] = args[i];
            }
            auto tmpLocals = locals;
            locals = {};
            locals.emplace_back();
            for(int i = 0; i < func.parameters.size(); i++) {
                locals.back()[func.parameters[i]] = args[i];
            }
            auto result = execute(func.code);
            locals = tmpLocals;
            locals.pop_back();
            return result;
        }
        ZirconObj call() {
            auto left = primary();
            if(peek(TokenType::LEFT_PAREN)) {
                eat(TokenType::LEFT_PAREN);
                auto args = exprList();
                eat(TokenType::RIGHT_PAREN);
                if(compileMode) {
                    currentCodeUnit.emit(InstructionType::CALL, args.size());
                    return ZirconObj();
                } else {
                    auto func = left.asFunction();
                    return callFn(func, args);
                }
            } else if(peek(TokenType::COLON)) {
                eat(TokenType::COLON);
                auto index = primary();
                if(compileMode) {
                    currentCodeUnit.emit(InstructionType::IDX, index.asInt());
                    return ZirconObj();
                } else {
                    auto vec = left.asVector();
                    if (index.asInt() >= vec.size()) throw ZirconException("Index out of bounds.");
                    return vec[index.asInt()];
                }
            } else {
                return left;
            }
        }
        ZirconObj unary() {
            if (peek(TokenType::NOT) || peek(TokenType::MINUS)) {
                auto op = eat(peek(TokenType::NOT) ? TokenType::NOT: TokenType::MINUS);
                auto right = unary();
                if(compileMode) {
                    if (op.type == TokenType::NOT) currentCodeUnit.emit(InstructionType::NOT);
                    else currentCodeUnit.emit(InstructionType::NEG);
                    return ZirconObj();
                } else {
                    if (op.type == TokenType::NOT) return ZirconObj(!right.asBool());
                    else return ZirconObj(-right.asInt());
                }
            } else {
                return call();
            }
        }
        ZirconObj factor() {
            auto left = unary();
            while (peek(TokenType::STAR) || peek(TokenType::SLASH)) {
                auto op = eat(peek(TokenType::STAR) ? TokenType::STAR : TokenType::SLASH);
                auto right = unary();
                if(compileMode) {
                    if (op.type == TokenType::STAR) currentCodeUnit.emit(InstructionType::MUL);
                    else currentCodeUnit.emit(InstructionType::DIV);
                    return ZirconObj();
                } else {
                    if (op.type == TokenType::STAR) left = ZirconObj(left.asInt() * right.asInt());
                    else left = ZirconObj(left.asInt() / right.asInt());
                }
            }
            return left;
        }
        ZirconObj term() {
            auto left = factor();
            while (peek(TokenType::PLUS) || peek(TokenType::MINUS)) {
                auto op = eat(peek(TokenType::PLUS) ? TokenType::PLUS : TokenType::MINUS);
                auto right = factor();
                if(compileMode) {
                    if (op.type == TokenType::PLUS) currentCodeUnit.emit(InstructionType::ADD);
                    else currentCodeUnit.emit(InstructionType::SUB);
                    return ZirconObj();
                } else {
                    if (op.type == TokenType::PLUS) left = ZirconObj(left.asInt() + right.asInt());
                    else left = ZirconObj(left.asInt() - right.asInt());
                }
            }
            return left;
        }
        ZirconObj comparison() {
            auto left = term();
            while (peek(TokenType::GREATER) || peek(TokenType::GREATER_EQUAL) || peek(TokenType::LESS) || peek(TokenType::LESS_EQUAL)) {
                auto op = eat(peek(TokenType::GREATER) ? TokenType::GREATER : peek(TokenType::GREATER_EQUAL) ? TokenType::GREATER_EQUAL : peek(TokenType::LESS) ? TokenType::LESS : TokenType::LESS_EQUAL);
                auto right = term();
                if(compileMode) {
                    if (op.type == TokenType::GREATER) currentCodeUnit.emit(InstructionType::GT);
                    else if (op.type == TokenType::GREATER_EQUAL) currentCodeUnit.emit(InstructionType::GTE);
                    else if (op.type == TokenType::LESS) currentCodeUnit.emit(InstructionType::LT);
                    else currentCodeUnit.emit(InstructionType::LTE);
                    return ZirconObj();
                } else {
                    if (op.type == TokenType::GREATER) left = ZirconObj(left.asInt() > right.asInt());
                    else if (op.type == TokenType::GREATER_EQUAL) left = ZirconObj(left.asInt() >= right.asInt());
                    else if (op.type == TokenType::LESS) left = ZirconObj(left.asInt() < right.asInt());
                    else left = ZirconObj(left.asInt() <= right.asInt());
                }
            }
            return left;
        }
        ZirconObj equality() {
            auto left = comparison();
            while (peek(TokenType::NOT_EQUAL) || peek(TokenType::EQUAL)) {
                auto op = eat(peek(TokenType::NOT_EQUAL) ? TokenType::NOT_EQUAL : TokenType::EQUAL);
                auto right = comparison();
                if(compileMode) {
                    if (op.type == TokenType::NOT_EQUAL) currentCodeUnit.emit(InstructionType::NEQ);
                    else currentCodeUnit.emit(InstructionType::EQ);
                    return ZirconObj();
                } else {
                    if (op.type == TokenType::NOT_EQUAL) left = ZirconObj(left != right);
                    else left = ZirconObj(left == right);
                }
            }
            return left;
        }
        ZirconObj andExpr() {
            auto left = equality();
            while (peek(TokenType::AND)) {
                eat(TokenType::AND);
                auto right = equality();
                if(compileMode) {
                    currentCodeUnit.emit(InstructionType::AND);
                    return ZirconObj();
                } else {
                    left = ZirconObj(left.asBool() && right.asBool());
                }
            }
            return left;
        }
        ZirconObj expression() {
            auto left = andExpr();
            while (peek(TokenType::OR)) {
                eat(TokenType::OR);
                auto right = andExpr();
                if(compileMode) {
                    currentCodeUnit.emit(InstructionType::OR);
                    return ZirconObj();
                } else {
                    left = ZirconObj(left.asBool() || right.asBool());
                }
            }
            return left;
        }
        void if_statement() {
            eat(TokenType::IF);
            auto condition = expression();
            if(compileMode) {
                if(peek(TokenType::NEWLINE)) {
                    eat(TokenType::NEWLINE);
                    auto prev = currentCodeUnit;
                    currentCodeUnit = ZirconCodeUnit();
                    while (!peek(TokenType::ELSE) && !peek(TokenType::END)) {
                        statement();
                    }
                    auto ifBlock = currentCodeUnit;
                    auto elseBlock = ZirconCodeUnit();
                    if (peek(TokenType::ELSE)) {
                        eat(TokenType::ELSE);
                        eat(TokenType::NEWLINE);
                        currentCodeUnit = ZirconCodeUnit();
                        while (!peek(TokenType::END)) {
                            statement();
                        }
                        elseBlock = currentCodeUnit;
                    }
                    currentCodeUnit = prev;
                    eat(TokenType::END);
                    eat(TokenType::NEWLINE);
                    currentCodeUnit.emit(InstructionType::PUSH, currentCodeUnit.getConstantIndex(ZirconObj(elseBlock)));
                    currentCodeUnit.emit(InstructionType::PUSH, currentCodeUnit.getConstantIndex(ZirconObj(ifBlock)));
                    currentCodeUnit.emit(InstructionType::IF);
                } else {
                    auto prev = currentCodeUnit;
                    currentCodeUnit = ZirconCodeUnit();
                    statement();
                    auto ifBlock = currentCodeUnit;
                    auto elseBlock = ZirconCodeUnit();
                    if (peek(TokenType::ELSE)) {
                        eat(TokenType::ELSE);
                        currentCodeUnit = ZirconCodeUnit();
                        statement();
                        elseBlock = currentCodeUnit;
                    }
                    currentCodeUnit = prev;
                    currentCodeUnit.emit(InstructionType::PUSH, currentCodeUnit.getConstantIndex(ZirconObj(elseBlock)));
                    currentCodeUnit.emit(InstructionType::PUSH, currentCodeUnit.getConstantIndex(ZirconObj(ifBlock)));
                    currentCodeUnit.emit(InstructionType::IF);
                }
            } else {
                if(peek(TokenType::NEWLINE)) {
                    eat(TokenType::NEWLINE);
                    currentCodeUnit = ZirconCodeUnit();
                    compileMode = true;
                    while (!peek(TokenType::ELSE) && !peek(TokenType::END)) {
                        statement();
                    }
                    auto ifCodeUnit = currentCodeUnit;
                    currentCodeUnit = ZirconCodeUnit();
                    if (peek(TokenType::ELSE)) {
                        eat(TokenType::ELSE);
                        if(peek(TokenType::NEWLINE)) {
                            eat(TokenType::NEWLINE);
                            while (!peek(TokenType::END)) {
                                statement();
                            }
                        } else {
                            statement();
                        }
                    }
                    eat(TokenType::END);
                    eat(TokenType::NEWLINE);
                    auto elseCodeUnit = currentCodeUnit;
                    if(condition.asBool()) {
                        execute(ifCodeUnit);
                    } else {
                        execute(elseCodeUnit);
                    }
                    compileMode = false;
                } else {
                    currentCodeUnit = ZirconCodeUnit();
                    compileMode = true;
                    statement();
                    auto ifCodeUnit = currentCodeUnit;
                    currentCodeUnit = ZirconCodeUnit();
                    if (peek(TokenType::ELSE)) {
                        eat(TokenType::ELSE);
                        statement();
                    }
                    auto elseCodeUnit = currentCodeUnit;
                    if(condition.asBool()) {
                        execute(ifCodeUnit);
                    } else {
                        execute(elseCodeUnit);
                    }
                    compileMode = false;
                }
            }
        }
        void while_statement() {
            eat(TokenType::WHILE);
            ZirconCodeUnit cond;
            if(compileMode) {
                auto prev = currentCodeUnit;
                currentCodeUnit = ZirconCodeUnit();
                expression();
                currentCodeUnit.emit(InstructionType::RET);
                cond = currentCodeUnit;
                currentCodeUnit = prev;
                if(peek(TokenType::NEWLINE)) {
                    eat(TokenType::NEWLINE);
                    currentCodeUnit = ZirconCodeUnit();
                    while (!peek(TokenType::END)) {
                        statement();
                    }
                    auto body = currentCodeUnit;
                    currentCodeUnit = prev;
                    eat(TokenType::END);
                    eat(TokenType::NEWLINE);
                    currentCodeUnit.emit(InstructionType::PUSH, currentCodeUnit.getConstantIndex(ZirconObj(cond)));
                    currentCodeUnit.emit(InstructionType::PUSH, currentCodeUnit.getConstantIndex(ZirconObj(body)));
                    currentCodeUnit.emit(InstructionType::WLOOP);
                } else {
                    currentCodeUnit = ZirconCodeUnit();
                    statement();
                    auto body = currentCodeUnit;
                    currentCodeUnit = prev;
                    currentCodeUnit.emit(InstructionType::PUSH, currentCodeUnit.getConstantIndex(ZirconObj(cond)));
                    currentCodeUnit.emit(InstructionType::PUSH, currentCodeUnit.getConstantIndex(ZirconObj(body)));
                    currentCodeUnit.emit(InstructionType::WLOOP);
                }
                currentCodeUnit = prev;
            } else {
                compileMode = true;
                currentCodeUnit = ZirconCodeUnit();
                expression();
                currentCodeUnit.emit(InstructionType::RET);
                cond = currentCodeUnit;
                compileMode = false;
                if(peek(TokenType::NEWLINE)) {
                    eat(TokenType::NEWLINE);
                    currentCodeUnit = ZirconCodeUnit();
                    compileMode = true;
                    while (!peek(TokenType::END)) {
                        statement();
                    }
                    auto body = currentCodeUnit;
                    currentCodeUnit = ZirconCodeUnit();
                    compileMode = false;
                    eat(TokenType::END);
                    eat(TokenType::NEWLINE);
                    while(execute(cond).asBool()) {
                        execute(body);
                        execute(cond);
                    }
                } else {
                    currentCodeUnit = ZirconCodeUnit();
                    compileMode = true;
                    statement();
                    auto body = currentCodeUnit;
                    currentCodeUnit = ZirconCodeUnit();
                    compileMode = false;
                    while(execute(cond).asBool()) {
                        execute(body);
                    }
                }
            }
        }
        void for_statement() {
            eat(TokenType::FOR);
            auto name = eat(TokenType::IDENTIFIER).lexeme;
            eat(TokenType::COMMA);
            auto iter = expression();
            if(compileMode) {
                auto prev = currentCodeUnit;
                currentCodeUnit = ZirconCodeUnit();
                if(peek(TokenType::NEWLINE)) {
                    eat(TokenType::NEWLINE);
                    while (!peek(TokenType::END)) {
                        statement();
                    }
                    auto body = currentCodeUnit;
                    currentCodeUnit = prev;
                    eat(TokenType::END);
                    eat(TokenType::NEWLINE);
                    currentCodeUnit.emit(InstructionType::PUSH, currentCodeUnit.getConstantIndex(ZirconObj(iter)));
                    currentCodeUnit.emit(InstructionType::PUSH, currentCodeUnit.getConstantIndex(ZirconObj(body)));
                    currentCodeUnit.emit(InstructionType::FLOOP, currentCodeUnit.getConstantIndex(ZirconObj(name)));
                } else {
                    statement();
                    auto body = currentCodeUnit;
                    currentCodeUnit = prev;
                    currentCodeUnit.emit(InstructionType::PUSH, currentCodeUnit.getConstantIndex(ZirconObj(iter)));
                    currentCodeUnit.emit(InstructionType::PUSH, currentCodeUnit.getConstantIndex(ZirconObj(body)));
                    currentCodeUnit.emit(InstructionType::FLOOP, currentCodeUnit.getConstantIndex(ZirconObj(name)));
                }
            } else {
                if(peek(TokenType::NEWLINE)) {
                    eat(TokenType::NEWLINE);
                    currentCodeUnit = ZirconCodeUnit();
                    compileMode = true;
                    while (!peek(TokenType::END)) {
                        statement();
                    }
                    auto body = currentCodeUnit;
                    currentCodeUnit = ZirconCodeUnit();
                    compileMode = false;
                    eat(TokenType::END);
                    eat(TokenType::NEWLINE);
                    for(const auto& i : iter.asVector()) {
                        locals.emplace_back();
                        locals.back()[name] = i;
                        execute(body);
                        locals.pop_back();
                    }
                } else {
                    currentCodeUnit = ZirconCodeUnit();
                    compileMode = true;
                    statement();
                    auto body = currentCodeUnit;
                    currentCodeUnit = ZirconCodeUnit();
                    compileMode = false;
                    for(const auto& i : iter.asVector()) {
                        locals.emplace_back();
                        locals.back()[name] = i;
                        execute(body);
                        locals.pop_back();
                    }
                }
            }
        }
        void function_statement() {
            eat(TokenType::FN);
            auto name = eat(TokenType::IDENTIFIER).lexeme;
            auto params = std::vector<std::string>();
            if (!peek(TokenType::NEWLINE) && !peek(TokenType::ARROW)) {
                params = idList();
            }
            if (peek(TokenType::ARROW)) {
                if(compileMode) {
                    auto prev = currentCodeUnit;
                    currentCodeUnit = ZirconCodeUnit();
                    eat(TokenType::ARROW);
                    expression();
                    currentCodeUnit.emit(InstructionType::RET);
                    auto body = currentCodeUnit;
                    currentCodeUnit = prev;
                    ZirconFunction fn {params, body};
                    currentCodeUnit.emit(InstructionType::PUSH, currentCodeUnit.getConstantIndex(ZirconObj(fn)));
                    currentCodeUnit.emit(InstructionType::STORE, currentCodeUnit.getConstantIndex(ZirconObj(name)));
                } else {
                    compileMode = true;
                    currentCodeUnit = ZirconCodeUnit();
                    eat(TokenType::ARROW);
                    expression();
                    currentCodeUnit.emit(InstructionType::RET);
                    auto body = currentCodeUnit;
                    currentCodeUnit = ZirconCodeUnit();
                    compileMode = false;
                    ZirconFunction fn {params, body};
                    setVariable(name, ZirconObj(fn));
                }
            }
            // TODO: FINISH THIS
        }
        static bool isAssignOp(TokenType type) {
            return type == TokenType::ASSIGN
            || type == TokenType::PLUS_ASSIGN
            || type == TokenType::MINUS_ASSIGN
            || type == TokenType::STAR_ASSIGN
            || type == TokenType::SLASH_ASSIGN
            || type == TokenType::MODULO_ASSIGN;
        }
        TokenType assignOp() {
            if(isAssignOp(peek())) {
                return eat(peek()).type;
            }
            throw ZirconException("Expected assignment operator");
        }
        void statement() {
            switch (peek()) {
                case TokenType::IF:
                    if_statement();
                    break;
                case TokenType::WHILE:
                    while_statement();
                    break;
                case TokenType::FOR:
                    for_statement();
                    break;
                case TokenType::FN:
                    function_statement();
                    break;
                case TokenType::RETURN:
                    if(!compileMode) {
                        throw ZirconException("Cannot return outside of function");
                    } else {
                        eat(TokenType::RETURN);
                        if(peek(TokenType::NEWLINE)) {
                            currentCodeUnit.emit(InstructionType::PUSH, currentCodeUnit.getConstantIndex(ZirconObj()));
                            currentCodeUnit.emit(InstructionType::RET);
                        } else {
                            expression();
                            currentCodeUnit.emit(InstructionType::RET);
                            eat(TokenType::NEWLINE);
                        }
                    }
                    break;
                case TokenType::BREAK:
                    if(!compileMode) {
                        throw ZirconException("Cannot break outside of loop");
                    } else {
                        eat(TokenType::BREAK);
                        currentCodeUnit.emit(InstructionType::BREAK);
                        eat(TokenType::NEWLINE);
                    }
                    break;
                case TokenType::CONTINUE:
                    if(!compileMode) {
                        throw ZirconException("Cannot continue outside of loop");
                    } else {
                        eat(TokenType::CONTINUE);
                        currentCodeUnit.emit(InstructionType::CONTINUE);
                        eat(TokenType::NEWLINE);
                    }
                    break;
                case TokenType::IDENTIFIER: {
                    auto name = eat(TokenType::IDENTIFIER).lexeme;
                    if(isAssignOp(peek())) {
                        auto op = eat(peek()).type;
                        if (compileMode) {
                            expression();
                            switch (op) {
                                case TokenType::ASSIGN:
                                    currentCodeUnit.emit(InstructionType::STORE, currentCodeUnit.getConstantIndex(ZirconObj(name)));
                                    break;
                                case TokenType::PLUS_ASSIGN:
                                    currentCodeUnit.emit(InstructionType::LOAD, currentCodeUnit.getConstantIndex(ZirconObj(name)));
                                    currentCodeUnit.emit(InstructionType::ADD);
                                    currentCodeUnit.emit(InstructionType::STORE, currentCodeUnit.getConstantIndex(ZirconObj(name)));
                                    break;
                                case TokenType::MINUS_ASSIGN:
                                    currentCodeUnit.emit(InstructionType::LOAD, currentCodeUnit.getConstantIndex(ZirconObj(name)));
                                    currentCodeUnit.emit(InstructionType::SUB);
                                    currentCodeUnit.emit(InstructionType::STORE, currentCodeUnit.getConstantIndex(ZirconObj(name)));
                                    break;
                                case TokenType::STAR_ASSIGN:
                                    currentCodeUnit.emit(InstructionType::LOAD, currentCodeUnit.getConstantIndex(ZirconObj(name)));
                                    currentCodeUnit.emit(InstructionType::MUL);
                                    currentCodeUnit.emit(InstructionType::STORE, currentCodeUnit.getConstantIndex(ZirconObj(name)));
                                    break;
                                case TokenType::SLASH_ASSIGN:
                                    currentCodeUnit.emit(InstructionType::LOAD, currentCodeUnit.getConstantIndex(ZirconObj(name)));
                                    currentCodeUnit.emit(InstructionType::DIV);
                                    currentCodeUnit.emit(InstructionType::STORE, currentCodeUnit.getConstantIndex(ZirconObj(name)));
                                    break;
                                case TokenType::MODULO_ASSIGN:
                                    currentCodeUnit.emit(InstructionType::LOAD, currentCodeUnit.getConstantIndex(ZirconObj(name)));
                                    currentCodeUnit.emit(InstructionType::MOD);
                                    currentCodeUnit.emit(InstructionType::STORE, currentCodeUnit.getConstantIndex(ZirconObj(name)));
                                    break;
                            }
                        } else {
                            auto val = expression();
                            switch (op) {
                                case TokenType::ASSIGN:
                                    setVariable(name, val);
                                    break;
                                case TokenType::PLUS_ASSIGN:
                                    setVariable(name, ZirconObj(lookupVariable(name).asInt() + val.asInt()));
                                    break;
                                case TokenType::MINUS_ASSIGN:
                                    setVariable(name, ZirconObj(lookupVariable(name).asInt() - val.asInt()));
                                    break;
                                case TokenType::STAR_ASSIGN:
                                    setVariable(name, ZirconObj(lookupVariable(name).asInt() * val.asInt()));
                                    break;
                                case TokenType::SLASH_ASSIGN:
                                    setVariable(name, ZirconObj(lookupVariable(name).asInt() / val.asInt()));
                                    break;
                                case TokenType::MODULO_ASSIGN:
                                    setVariable(name, ZirconObj(lookupVariable(name).asInt() % val.asInt()));
                                    break;
                            }
                        }
                    } else if(peek(TokenType::COLON)) {
                        eat(TokenType::COLON);
                        auto index = expression();
                        auto op = assignOp();
                        if (compileMode) {
                            currentCodeUnit.emit(InstructionType::DUP);
                            expression();
                            switch (op) {
                                case TokenType::ASSIGN:
                                    currentCodeUnit.emit(InstructionType::LOAD,
                                                         currentCodeUnit.getConstantIndex(ZirconObj(name)));
                                    currentCodeUnit.emit(InstructionType::SETIDX);
                                    currentCodeUnit.emit(InstructionType::POP);
                                    break;
                                case TokenType::PLUS_ASSIGN:
                                    currentCodeUnit.emit(InstructionType::LOAD,
                                                         currentCodeUnit.getConstantIndex(ZirconObj(name)));
                                    currentCodeUnit.emit(InstructionType::IDX);
                                    currentCodeUnit.emit(InstructionType::ADD);
                                    currentCodeUnit.emit(InstructionType::LOAD,
                                                         currentCodeUnit.getConstantIndex(ZirconObj(name)));
                                    currentCodeUnit.emit(InstructionType::SETIDX);
                                    break;
                                case TokenType::MINUS_ASSIGN:
                                    currentCodeUnit.emit(InstructionType::LOAD,
                                                         currentCodeUnit.getConstantIndex(ZirconObj(name)));
                                    currentCodeUnit.emit(InstructionType::IDX);
                                    currentCodeUnit.emit(InstructionType::SUB);
                                    currentCodeUnit.emit(InstructionType::LOAD,
                                                         currentCodeUnit.getConstantIndex(ZirconObj(name)));
                                    currentCodeUnit.emit(InstructionType::SETIDX);
                                    break;
                                case TokenType::STAR_ASSIGN:
                                    currentCodeUnit.emit(InstructionType::LOAD,
                                                         currentCodeUnit.getConstantIndex(ZirconObj(name)));
                                    currentCodeUnit.emit(InstructionType::IDX);
                                    currentCodeUnit.emit(InstructionType::MUL);
                                    currentCodeUnit.emit(InstructionType::LOAD,
                                                         currentCodeUnit.getConstantIndex(ZirconObj(name)));
                                    currentCodeUnit.emit(InstructionType::SETIDX);
                                    break;
                                case TokenType::SLASH_ASSIGN:
                                    currentCodeUnit.emit(InstructionType::LOAD,
                                                         currentCodeUnit.getConstantIndex(ZirconObj(name)));
                                    currentCodeUnit.emit(InstructionType::IDX);
                                    currentCodeUnit.emit(InstructionType::DIV);
                                    currentCodeUnit.emit(InstructionType::LOAD,
                                                         currentCodeUnit.getConstantIndex(ZirconObj(name)));
                                    currentCodeUnit.emit(InstructionType::SETIDX);
                                    break;
                                case TokenType::MODULO_ASSIGN:
                                    currentCodeUnit.emit(InstructionType::LOAD,
                                                         currentCodeUnit.getConstantIndex(ZirconObj(name)));
                                    currentCodeUnit.emit(InstructionType::IDX);
                                    currentCodeUnit.emit(InstructionType::MOD);
                                    currentCodeUnit.emit(InstructionType::LOAD,
                                                         currentCodeUnit.getConstantIndex(ZirconObj(name)));
                                    currentCodeUnit.emit(InstructionType::SETIDX);
                                    break;
                            }
                        } else {
                            auto val = expression();
                            switch (op) {
                                case TokenType::PLUS_ASSIGN:
                                    lookupVariable(name).asVector()[index.asInt()] = ZirconObj(
                                            lookupVariable(name).asVector()[index.asInt()].asInt() + val.asInt());
                                    break;
                                case TokenType::MINUS_ASSIGN:
                                    lookupVariable(name).asVector()[index.asInt()] = ZirconObj(
                                            lookupVariable(name).asVector()[index.asInt()].asInt() - val.asInt());
                                    break;
                                case TokenType::STAR_ASSIGN:
                                    lookupVariable(name).asVector()[index.asInt()] = ZirconObj(
                                            lookupVariable(name).asVector()[index.asInt()].asInt() * val.asInt());
                                    break;
                                case TokenType::SLASH_ASSIGN:
                                    lookupVariable(name).asVector()[index.asInt()] = ZirconObj(
                                            lookupVariable(name).asVector()[index.asInt()].asInt() / val.asInt());
                                    break;
                                case TokenType::MODULO_ASSIGN:
                                    lookupVariable(name).asVector()[index.asInt()] = ZirconObj(
                                            lookupVariable(name).asVector()[index.asInt()].asInt() % val.asInt());
                                    break;
                            }
                        }
                    } else {
                        if(compileMode) {
                            int args = exprList().size();
                            currentCodeUnit.emit(InstructionType::LOAD, currentCodeUnit.getConstantIndex(ZirconObj(name)));
                            currentCodeUnit.emit(InstructionType::CALL, args);
                        } else {
                            auto func = lookupVariable(name).asFunction();
                            auto args = exprList();
                            callFn(func, args);
                        }
                    }
                    eat(TokenType::NEWLINE);
                    break;
                }
                case TokenType::NEWLINE:
                    eat(TokenType::NEWLINE);
                    break;
                case TokenType::PRINT:
                    eat(TokenType::PRINT);
                    if(compileMode) {
                        expression();
                        currentCodeUnit.emit(InstructionType::PRINT);
                    } else {
                        std::cout << expression().asString() << std::endl;
                    }
                    eat(TokenType::NEWLINE);
                    break;
                default:
                    throw ZirconException("Unexpected token " + std::to_string((int)peek()) + " at line " + std::to_string(current.line));
            }
        }
    };
}
#endif //ZIRCON_ZIRCON_H
