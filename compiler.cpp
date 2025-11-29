#include <iostream>
#include <string>
#include <vector>
#include <cctype>
#include <map>
#include <fstream>
#include <sstream>
#include <memory>
#include <algorithm>

// 1. LEXER

enum TokenType {
    kw_func, kw_return, kw_new, kw_print, kw_getinput,
    kw_for, kw_while, kw_if, kw_else,
    id, integer, string_lit, type_def,
    lshift, rshift, 
    colon, semicolon, comma, 
    assign, assign_plus, 
    lparen, rparen, lbrace, rbrace,
    op_add, op_sub, op_mul, op_div,
    op_eq, op_ne, op_lt, op_gt, op_le, op_ge, 
    eof
};

struct Token {
    TokenType type;
    std::string value;
    int line;
};

class Lexer {
    std::string src;
    size_t pos = 0;
    int line = 1;

public:
    Lexer(std::string source) : src(source) {}

    Token nextToken() {
        skipWhitespace();
        if (pos >= src.length()) return {eof, "", line};

        char current = src[pos];

        if (current == '/' && pos + 1 < src.length() && src[pos + 1] == '/') {
            while (pos < src.length() && src[pos] != '\n') pos++;
            return nextToken();
        }

        if (isdigit(current)) {
            std::string num;
            while (pos < src.length() && isdigit(src[pos])) num += src[pos++];
            return {integer, num, line};
        }

        if (isalpha(current)) {
            std::string text;
            while (pos < src.length() && (isalnum(src[pos]) || src[pos] == '_')) text += src[pos++];
            if (text == "if") return {kw_if, text, line};
            if (text == "else") return {kw_else, text, line};
            if (text == "func") return {kw_func, text, line};
            if (text == "return") return {kw_return, text, line};
            if (text == "new") return {kw_new, text, line};
            if (text == "print") return {kw_print, text, line};
            if (text == "getinput") return {kw_getinput, text, line};
            if (text == "for") return {kw_for, text, line};
            if (text == "while") return {kw_while, text, line};
            if (text == "int" || text == "str" || text == "string" || text == "none") return {type_def, text, line};
            return {id, text, line};
        }

        if (current == '"') {
            pos++; 
            std::string str;
            while (pos < src.length() && src[pos] != '"') str += src[pos++];
            pos++; 
            return {string_lit, str, line};
        }

        if (current == '<' && match('<')) return {lshift, "<<", line};
        if (current == '>' && match('>')) return {rshift, ">>", line};
        
        if (current == '=' && match('=')) return {op_eq, "==", line};
        if (current == '!' && match('=')) return {op_ne, "!=", line};
        if (current == '<' && match('=')) return {op_le, "<=", line};
        if (current == '>' && match('=')) return {op_ge, ">=", line};
        if (current == '+' && match('=')) return {assign_plus, "+=", line};

        pos++;
        switch (current) {
            case ':': return {colon, ":", line};
            case ';': return {semicolon, ";", line};
            case ',': return {comma, ",", line};
            case '=': return {assign, "=", line};
            case '(': return {lparen, "(", line};
            case ')': return {rparen, ")", line};
            case '{': return {lbrace, "{", line};
            case '}': return {rbrace, "}", line};
            case '+': return {op_add, "+", line};
            case '*': return {op_mul, "*", line};
            case '<': return {op_lt, "<", line};
            case '>': return {op_gt, ">", line};
            case '-': return {op_sub, "-", line}; 
            case '/': return {op_div, "/", line}; 
            default:  
                std::cerr << "Unknown char: " << current << " at line " << line << std::endl;
                exit(1);
        }
    }

private:
    bool match(char expected) {
        if (pos + 1 < src.length() && src[pos + 1] == expected) {
            pos += 2;
            return true;
        }
        return false;
    }

    void skipWhitespace() {
        while (pos < src.length() && isspace(src[pos])) {
            if (src[pos] == '\n') line++;
            pos++;
        }
    }
};


// 2. AST NODES


struct ASTNode { virtual ~ASTNode() = default; virtual std::string gen() = 0; };
struct ExprNode : ASTNode {};
struct StatementNode : ASTNode {};

struct LiteralNode : ExprNode {
    std::string val;
    bool isString;
    LiteralNode(std::string v, bool s) : val(v), isString(s) {}
    std::string gen() override { return isString ? "std::string(\"" + val + "\")" : val; }
};

struct VarAccessNode : ExprNode {
    std::string name;
    VarAccessNode(std::string n) : name(n) {}
    std::string gen() override { return name; }
};

struct BinOpNode : ExprNode {
    std::unique_ptr<ExprNode> left, right;
    std::string op;
    BinOpNode(std::unique_ptr<ExprNode> l, std::string o, std::unique_ptr<ExprNode> r) 
        : left(std::move(l)), op(o), right(std::move(r)) {}
    std::string gen() override { return "(" + left->gen() + " " + op + " " + right->gen() + ")"; }
};
struct IfNode : StatementNode {
    std::unique_ptr<ExprNode> condition;
    std::unique_ptr<StatementNode> thenBlock;
    std::unique_ptr<StatementNode> elseBlock;

    IfNode(std::unique_ptr<ExprNode> c, std::unique_ptr<StatementNode> t, std::unique_ptr<StatementNode> e) 
        : condition(std::move(c)), thenBlock(std::move(t)), elseBlock(std::move(e)) {}

    std::string gen() override {
        std::string s = "if (" + condition->gen() + ") " + thenBlock->gen();
        if (elseBlock) s += " else " + elseBlock->gen();
        return s;
    }
};
struct FuncCallNode : ExprNode {
    std::string funcName;
    std::vector<std::unique_ptr<ExprNode>> args;
    FuncCallNode(std::string n, std::vector<std::unique_ptr<ExprNode>> a) : funcName(n), args(std::move(a)) {}
    std::string gen() override {
        if (funcName == "print") {
            std::string s = "std::cout";
            for (auto& arg : args) s += " << " + arg->gen();
            s += " << std::endl";
            return s; 
        }
        if (funcName == "getinput") return "get_input()";
        
        std::string s = funcName + "(";
        for (size_t i = 0; i < args.size(); ++i) {
            s += args[i]->gen();
            if (i < args.size() - 1) s += ", ";
        }
        s += ")";
        return s;
    }
};

struct NewObjNode : ExprNode {
    std::string type;
    std::unique_ptr<ExprNode> expr;
    NewObjNode(std::string t, std::unique_ptr<ExprNode> e) : type(t), expr(std::move(e)) {}
    std::string gen() override { return expr->gen(); }
};

struct VarDeclNode : StatementNode {
    std::string name, type;
    std::unique_ptr<ExprNode> init;
    VarDeclNode(std::string n, std::string t, std::unique_ptr<ExprNode> i) : name(n), type(t), init(std::move(i)) {}
    std::string gen() override {
        std::string cppType = (type == "str" || type == "string") ? "std::string" : "int";
        return cppType + " " + name + " = " + init->gen() + ";";
    }
};

struct AssignNode : StatementNode {
    std::string name;
    std::unique_ptr<ExprNode> expr;
    std::string op; 
    AssignNode(std::string n, std::unique_ptr<ExprNode> e, std::string o = "=") : name(n), expr(std::move(e)), op(o) {}
    std::string gen() override { return name + " " + op + " " + expr->gen() + ";"; }
};

struct ReturnNode : StatementNode {
    std::unique_ptr<ExprNode> expr;
    ReturnNode(std::unique_ptr<ExprNode> e) : expr(std::move(e)) {}
    std::string gen() override { return "return " + expr->gen() + ";"; }
};

struct ExprStmtNode : StatementNode {
    std::unique_ptr<ExprNode> expr;
    ExprStmtNode(std::unique_ptr<ExprNode> e) : expr(std::move(e)) {}
    std::string gen() override { return expr->gen() + ";"; }
};

struct BlockNode : StatementNode {
    std::vector<std::unique_ptr<StatementNode>> stmts;
    std::string gen() override {
        std::string s = "{\n";
        for (auto& stmt : stmts) s += stmt->gen() + "\n";
        s += "}";
        return s;
    }
};

struct WhileNode : StatementNode {
    std::unique_ptr<ExprNode> condition;
    std::unique_ptr<BlockNode> body;
    WhileNode(std::unique_ptr<ExprNode> c, std::unique_ptr<BlockNode> b) : condition(std::move(c)), body(std::move(b)) {}
    std::string gen() override {
        return "while (" + condition->gen() + ")\n" + body->gen();
    }
};

struct ForNode : StatementNode {
    std::unique_ptr<StatementNode> init;
    std::unique_ptr<ExprNode> cond;
    std::unique_ptr<StatementNode> incr;
    std::unique_ptr<BlockNode> body;
    ForNode(std::unique_ptr<StatementNode> i, std::unique_ptr<ExprNode> c, std::unique_ptr<StatementNode> inc, std::unique_ptr<BlockNode> b) 
        : init(std::move(i)), cond(std::move(c)), incr(std::move(inc)), body(std::move(b)) {}
    
    std::string gen() override {
        std::string iStr = init->gen(); 
        if (!iStr.empty() && iStr.back() == ';') iStr.pop_back(); 
        
        std::string incStr = incr->gen();
        if (!incStr.empty() && incStr.back() == ';') incStr.pop_back();

        return "for (" + iStr + "; " + cond->gen() + "; " + incStr + ")\n" + body->gen();
    }
};

struct FunctionNode : ASTNode {
    std::string name;
    struct Arg { std::string type, name; };
    std::vector<Arg> args;
    std::string retType;
    std::unique_ptr<BlockNode> body;

    std::string gen() override {
        std::string rt;
        if (name == "main") rt = "int";
        else rt = (retType == "none") ? "void" : (retType == "str" || retType == "string" ? "std::string" : retType);
        
        std::string s = rt + " " + name + "(";
        for (size_t i = 0; i < args.size(); ++i) {
            std::string at = (args[i].type == "str" || args[i].type == "string") ? "std::string" : args[i].type;
            s += at + " " + args[i].name;
            if (i < args.size() - 1) s += ", ";
        }
        s += ")\n" + body->gen();
        return s;
    }
};


// 3. PARSER

class PeekingLexer {
    std::vector<Token> tokens;
    size_t idx = 0;
public:
    PeekingLexer(Lexer l) {
        Token t;
        do { t = l.nextToken(); tokens.push_back(t); } while (t.type != eof);
    }
    Token peek(int offset = 0) {
        if (idx + offset >= tokens.size()) return tokens.back();
        return tokens[idx + offset];
    }
    Token advance() {
        if (idx < tokens.size()) return tokens[idx++];
        return tokens.back();
    }
};

class BetterParser {
    PeekingLexer& lexer;

    void eat(TokenType t) {
        if (lexer.peek().type == t) lexer.advance();
        else {
            std::cerr << "Expected token type " << t << " but got " << lexer.peek().value << " line " << lexer.peek().line << std::endl;
            exit(1);
        }
    }

    std::unique_ptr<ExprNode> parseFactor() {
        Token t = lexer.peek();
        if (t.type == integer) { lexer.advance(); return std::make_unique<LiteralNode>(t.value, false); }
        if (t.type == string_lit) { lexer.advance(); return std::make_unique<LiteralNode>(t.value, true); }
        if (t.type == kw_getinput) {
            lexer.advance(); eat(lparen); eat(rparen);
            return std::make_unique<FuncCallNode>("getinput", std::vector<std::unique_ptr<ExprNode>>{});
        }
        if (t.type == kw_new) {
            lexer.advance(); eat(colon); 
            std::string type = lexer.peek().value; 
            eat(type_def); eat(lparen);
            auto e = parseExpression();
            eat(rparen);
            return std::make_unique<NewObjNode>(type, std::move(e));
        }
        if (t.type == kw_print) {
            lexer.advance(); eat(lparen);
            std::vector<std::unique_ptr<ExprNode>> args;
            args.push_back(parseExpression());
            eat(rparen);
            return std::make_unique<FuncCallNode>("print", std::move(args));
        }
        if (t.type == id) {
            std::string name = t.value;
            lexer.advance();
            if (lexer.peek().type == rshift) { 
                lexer.advance();
                std::vector<std::unique_ptr<ExprNode>> args;
                args.push_back(parseExpression());
                while(lexer.peek().type == comma) {
                    lexer.advance();
                    args.push_back(parseExpression());
                }
                return std::make_unique<FuncCallNode>(name, std::move(args));
            }
            if (lexer.peek().type == lparen) { 
                lexer.advance();
                std::vector<std::unique_ptr<ExprNode>> args;
                if(lexer.peek().type != rparen) {
                    args.push_back(parseExpression());
                    while(lexer.peek().type == comma) {
                        lexer.advance();
                        args.push_back(parseExpression());
                    }
                }
                eat(rparen);
                return std::make_unique<FuncCallNode>(name, std::move(args));
            }
            return std::make_unique<VarAccessNode>(name);
        }
        if (t.type == lparen) {
             lexer.advance();
             auto e = parseExpression();
             eat(rparen);
             return e;
        }
        std::cerr << "Unexpected token in factor: " << t.value << " line " << t.line << std::endl;
        exit(1);
    }
std::unique_ptr<ExprNode> parseTerm() {
        auto left = parseFactor();
        while (lexer.peek().type == op_mul || lexer.peek().type == op_div) {
            std::string op = lexer.peek().value;
            lexer.advance();
            left = std::make_unique<BinOpNode>(std::move(left), op, parseFactor());
        }
        return left;
    }

    std::unique_ptr<ExprNode> parseAdditive() {
        auto left = parseTerm();
        while (lexer.peek().type == op_add || lexer.peek().type == op_sub) {
            std::string op = lexer.peek().value;
            lexer.advance();
            left = std::make_unique<BinOpNode>(std::move(left), op, parseTerm());
        }
        return left;
    }

    std::unique_ptr<ExprNode> parseExpression() {
        auto left = parseAdditive();
        Token t = lexer.peek();
        while (t.type == op_eq || t.type == op_ne || t.type == op_lt || 
               t.type == op_gt || t.type == op_le || t.type == op_ge) {
            std::string op = t.value;
            lexer.advance();
            left = std::make_unique<BinOpNode>(std::move(left), op, parseAdditive());
            t = lexer.peek();
        }
        return left;
    }

    std::unique_ptr<StatementNode> parseStatement() {
        Token t = lexer.peek();
        // IF / ELSE
        if (t.type == kw_if) {
            lexer.advance();
            eat(lparen);
            auto cond = parseExpression();
            eat(rparen);
            auto thenBlock = parseBlock();
            
            std::unique_ptr<StatementNode> elseBlock = nullptr;
            if (lexer.peek().type == kw_else) {
                lexer.advance();
                // Handle "else if" by checking if the next token is "if"
                if (lexer.peek().type == kw_if) {
                    elseBlock = parseStatement();
                } else {
                    elseBlock = parseBlock();
                }
            }
            return std::make_unique<IfNode>(std::move(cond), std::move(thenBlock), std::move(elseBlock));
        }
        // RETURN
        if (t.type == kw_return) {
            lexer.advance();
            auto e = parseExpression();
            eat(semicolon);
            return std::make_unique<ReturnNode>(std::move(e));
        }

        // WHILE
        if (t.type == kw_while) {
            lexer.advance();
            eat(lparen);
            auto cond = parseExpression();
            eat(rparen);
            auto body = parseBlock();
            return std::make_unique<WhileNode>(std::move(cond), std::move(body));
        }

        // FOR
        if (t.type == kw_for) {
            lexer.advance();
            eat(lparen);
            auto init = parseStatement(); 
            auto cond = parseExpression();
            eat(semicolon);
            
            if (lexer.peek().type != id) {
                 std::cerr << "Expected ID in for-loop increment line " << lexer.peek().line << std::endl;
                 exit(1);
            }
            std::string name = lexer.peek().value;
            lexer.advance(); 
            
            std::string op = lexer.peek().value;
            if (op != "=" && op != "+=") {
                 std::cerr << "Expected assignment in for-loop increment" << std::endl;
                 exit(1);
            }
            lexer.advance();
            
            auto val = parseExpression();
            auto incr = std::make_unique<AssignNode>(name, std::move(val), op);

            eat(rparen);
            auto body = parseBlock();
            return std::make_unique<ForNode>(std::move(init), std::move(cond), std::move(incr), std::move(body));
        }

        // Declarations
        if (t.type == id && lexer.peek(1).type == colon) {
            std::string name = lexer.peek().value;
            lexer.advance(); // id
            lexer.advance(); // colon
            std::string type = lexer.peek().value;
            eat(type_def);
            eat(assign);
            auto expr = parseExpression();
            eat(semicolon);
            return std::make_unique<VarDeclNode>(name, type, std::move(expr));
        }

        // C-Style Declarations
        if (t.type == type_def && lexer.peek(1).type == id && lexer.peek(2).type == assign) {
            std::string type = t.value;
            lexer.advance(); // type
            std::string name = lexer.peek().value;
            lexer.advance(); // id
            eat(assign);
            auto expr = parseExpression();
            eat(semicolon);
            return std::make_unique<VarDeclNode>(name, type, std::move(expr));
        }

        // Assignment
        if (t.type == id && (lexer.peek(1).type == assign || lexer.peek(1).type == assign_plus)) {
            std::string name = lexer.peek().value;
            lexer.advance();
            std::string op = lexer.peek().value;
            lexer.advance();
            auto expr = parseExpression();
            eat(semicolon);
            return std::make_unique<AssignNode>(name, std::move(expr), op);
        }
        
        auto expr = parseExpression();
        eat(semicolon);
        return std::make_unique<ExprStmtNode>(std::move(expr));
    }

    std::unique_ptr<BlockNode> parseBlock() {
        eat(lbrace);
        auto block = std::make_unique<BlockNode>();
        while (lexer.peek().type != rbrace && lexer.peek().type != eof) {
            block->stmts.push_back(parseStatement());
        }
        eat(rbrace);
        return block;
    }

public:
    BetterParser(PeekingLexer& l) : lexer(l) {}

    std::vector<std::unique_ptr<FunctionNode>> parseProgram() {
        std::vector<std::unique_ptr<FunctionNode>> funcs;
        while (lexer.peek().type != eof) {
            if (lexer.peek().type == kw_func) {
                eat(kw_func);
                auto fn = std::make_unique<FunctionNode>();
                fn->name = lexer.peek().value;
                eat(id);
                eat(lshift);
                while (lexer.peek().type != rshift) {
                    std::string type = lexer.peek().value;
                    eat(type_def);
                    eat(colon);
                    std::string name = lexer.peek().value;
                    eat(id);
                    fn->args.push_back({type, name});
                    if (lexer.peek().type == comma) lexer.advance();
                }
                eat(rshift);
                if (lexer.peek().type == type_def) {
                    fn->retType = lexer.peek().value;
                    lexer.advance();
                }
                eat(semicolon);
                fn->body = parseBlock();
                funcs.push_back(std::move(fn));
            } else {
                std::cerr << "Unexpected global token: " << lexer.peek().value << std::endl;
                lexer.advance();
            }
        }
        return funcs;
    }
};

// 4. MAIN


int main(int argc, char** argv) {
    if (argc < 2) {
        std::cerr << "Usage: compiler <source_file>" << std::endl;
        return 1;
    }
    
    std::string arg2 = argv[1];
    if (arg2.length() >= 4 && arg2.substr(arg2.length() - 4) != ".c+-") {
    std::cerr << "FILE MUST BE .c+-" << std::endl;
    return 1;
    }
    
    
    std::ifstream f(argv[1]);
    if (!f.is_open()) {
        std::cerr << "Cannot open file." << std::endl;
        return 1;
    }
    std::stringstream buffer;
    buffer << f.rdbuf();

    Lexer l(buffer.str());
    PeekingLexer pl(l);
    BetterParser parser(pl);
    
    auto functions = parser.parseProgram();

    std::cout << "#include <iostream>\n";
    std::cout << "#include <string>\n";
    std::cout << "#include <type_traits>\n"; 
    std::cout << "\n";
    std::cout << "// Helpers for string concatenation (Only for numbers)\n";
    std::cout << "template<typename T> typename std::enable_if<std::is_arithmetic<T>::value, std::string>::type operator+(const std::string& lhs, T rhs) { return lhs + std::to_string(rhs); }\n";
    std::cout << "template<typename T> typename std::enable_if<std::is_arithmetic<T>::value, std::string>::type operator+(const char* lhs, T rhs) { return std::string(lhs) + std::to_string(rhs); }\n";
    std::cout << "\n";
    std::cout << "std::string get_input() { std::string s; std::getline(std::cin, s); return s; }\n";
    std::cout << "int get_int_input() { std::string s; std::getline(std::cin, s); return std::stoi(s); }\n";
    std::cout << "\n";

    // Forward declarations
    for (const auto& fn : functions) {
        std::string rt;
        if (fn->name == "main") rt = "int";
        else rt = (fn->retType == "none") ? "void" : (fn->retType == "str" || fn->retType == "string" ? "std::string" : fn->retType);
        
        std::cout << rt << " " << fn->name << "(";
        for (size_t i=0; i < fn->args.size(); i++) {
             std::string at = (fn->args[i].type == "str" || fn->args[i].type == "string") ? "std::string" : fn->args[i].type;
             std::cout << at << (i < fn->args.size()-1 ? ", " : "");
        }
        std::cout << ");\n";
    }
    std::cout << "\n";

    for (const auto& fn : functions) {
        std::cout << fn->gen() << "\n";
    }

    return 0;
}
