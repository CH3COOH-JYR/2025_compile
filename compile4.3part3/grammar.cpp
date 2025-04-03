#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <vector>

using namespace std;

enum TokenType
{
    IDENFR,
    INTCON,
    STRCON,
    MAINTK,
    CONSTTK,
    INTTK,
    BREAKTK,
    CONTINUETK,
    IFTK,
    ELSETK,
    VOIDTK,
    WHILETK,
    GETINTTK,
    PRINTFTK,
    RETURNTK,
    ASSIGN,
    PLUS,
    MINU,
    MULT,
    DIV,
    MOD,
    NOT,
    AND,
    OR,
    LSS,
    LEQ,
    GRE,
    GEQ,
    EQL,
    NEQ,
    SEMICN,
    COMMA,
    LPARENT,
    RPARENT,
    LBRACK,
    RBRACK,
    LBRACE,
    RBRACE,
    UNDEFINED
};

// 词法分析辅助函数声明
bool isLetter(char c);
bool isDigit(char c);
TokenType getKeywordType(const string &ident);

// 将TokenTypeToString的声明移到这里
map<TokenType, string> TokenTypeToString = {
    {IDENFR, "IDENFR"}, {INTCON, "INTCON"}, {STRCON, "STRCON"}, {MAINTK, "MAINTK"}, {CONSTTK, "CONSTTK"}, {INTTK, "INTTK"}, {BREAKTK, "BREAKTK"}, {CONTINUETK, "CONTINUETK"}, {IFTK, "IFTK"}, {ELSETK, "ELSETK"}, {VOIDTK, "VOIDTK"}, {WHILETK, "WHILETK"}, {GETINTTK, "GETINTTK"}, {PRINTFTK, "PRINTFTK"}, {RETURNTK, "RETURNTK"}, {ASSIGN, "ASSIGN"}, {PLUS, "PLUS"}, {MINU, "MINU"}, {MULT, "MULT"}, {DIV, "DIV"}, {MOD, "MOD"}, {NOT, "NOT"}, {AND, "AND"}, {OR, "OR"}, {LSS, "LSS"}, {LEQ, "LEQ"}, {GRE, "GRE"}, {GEQ, "GEQ"}, {EQL, "EQL"}, {NEQ, "NEQ"}, {SEMICN, "SEMICN"}, {COMMA, "COMMA"}, {LPARENT, "LPARENT"}, {RPARENT, "RPARENT"}, {LBRACK, "LBRACK"}, {RBRACK, "RBRACK"}, {LBRACE, "LBRACE"}, {RBRACE, "RBRACE"}};

struct Token {
    TokenType type;
    string value;
    int line;
};

class Parser {
private:
    vector<Token> tokens;
    int currentTokenIndex;
    ofstream& output;
    
    Token currentToken() {
        if (currentTokenIndex < tokens.size()) {
            return tokens[currentTokenIndex];
        }
        return {UNDEFINED, "", -1};
    }
    
    void nextToken() {
        if (currentTokenIndex < tokens.size()) {
            // 输出当前单词信息
            Token token = tokens[currentTokenIndex];
            output << TokenTypeToString[token.type] << " " << token.value << endl;
            currentTokenIndex++;
        }
    }
    
    bool match(TokenType expected) {
        if (currentToken().type == expected) {
            nextToken();
            return true;
        }
        return false;
    }
    
    bool peek(TokenType expected) {
        return currentToken().type == expected;
    }
    
    void outputGrammarComponent(const string& name) {
        // 输出语法成分名称，除了<BlockItem>, <Decl>, <BType>
        if (name != "BlockItem" && name != "Decl" && name != "BType") {
            output << "<" << name << ">" << endl;
        }
    }

public:
    Parser(const vector<Token>& tokens, ofstream& output) 
        : tokens(tokens), currentTokenIndex(0), output(output) {}
    
    // CompUnit → {Decl} {FuncDef} MainFuncDef
    void parseCompUnit() {
        // 解析声明
        while (peek(CONSTTK) || (peek(INTTK) && 
               currentTokenIndex + 1 < tokens.size() && 
               tokens[currentTokenIndex + 1].type == IDENFR && 
               currentTokenIndex + 2 < tokens.size() && 
               tokens[currentTokenIndex + 2].type != LPARENT)) {
            parseDecl();
        }
        
        // 解析函数定义
        while ((peek(VOIDTK) || peek(INTTK)) && 
               currentTokenIndex + 1 < tokens.size() && 
               tokens[currentTokenIndex + 1].type == IDENFR && 
               tokens[currentTokenIndex + 1].value != "main") {
            parseFuncDef();
        }
        
        // 解析主函数
        parseMainFuncDef();
        
        outputGrammarComponent("CompUnit");
    }
    
    // Decl → ConstDecl | VarDecl
    void parseDecl() {
        if (peek(CONSTTK)) {
            parseConstDecl();
        } else {
            parseVarDecl();
        }
    }
    
    // ConstDecl → 'const' BType ConstDef { ',' ConstDef } ';'
    void parseConstDecl() {
        match(CONSTTK);
        parseBType();
        parseConstDef();
        
        while (peek(COMMA)) {
            match(COMMA);
            parseConstDef();
        }
        
        match(SEMICN);
        
        outputGrammarComponent("ConstDecl");
    }
    
    // BType → 'int'
    void parseBType() {
        match(INTTK);
    }
    
    // ConstDef → Ident [ '[' ConstExp ']' ] '=' ConstInitVal
    void parseConstDef() {
        match(IDENFR);
        
        if (peek(LBRACK)) {
            match(LBRACK);
            parseConstExp();
            match(RBRACK);
        }
        
        match(ASSIGN);
        parseConstInitVal();
        
        outputGrammarComponent("ConstDef");
    }
    
    // ConstInitVal → ConstExp | '{' [ ConstInitVal { ',' ConstInitVal } ] '}'
    void parseConstInitVal() {
        if (peek(LBRACE)) {
            match(LBRACE);
            
            if (!peek(RBRACE)) {
                parseConstInitVal();
                
                while (peek(COMMA)) {
                    match(COMMA);
                    parseConstInitVal();
                }
            }
            
            match(RBRACE);
        } else {
            parseConstExp();
        }
        
        outputGrammarComponent("ConstInitVal");
    }
    
    // VarDecl → BType VarDef { ',' VarDef } ';'
    void parseVarDecl() {
        parseBType();
        parseVarDef();
        
        while (peek(COMMA)) {
            match(COMMA);
            parseVarDef();
        }
        
        match(SEMICN);
        
        outputGrammarComponent("VarDecl");
    }
    
    // VarDef → Ident [ '[' ConstExp ']' ] | Ident [ '[' ConstExp ']' ] '=' InitVal
    void parseVarDef() {
        match(IDENFR);
        
        if (peek(LBRACK)) {
            match(LBRACK);
            parseConstExp();
            match(RBRACK);
        }
        
        if (peek(ASSIGN)) {
            match(ASSIGN);
            parseInitVal();
        }
        
        outputGrammarComponent("VarDef");
    }
    
    // InitVal → Exp | '{' [ InitVal { ',' InitVal } ] '}'
    void parseInitVal() {
        if (peek(LBRACE)) {
            match(LBRACE);
            
            if (!peek(RBRACE)) {
                parseInitVal();
                
                while (peek(COMMA)) {
                    match(COMMA);
                    parseInitVal();
                }
            }
            
            match(RBRACE);
        } else {
            parseExp();
        }
        
        outputGrammarComponent("InitVal");
    }
    
    // 其他函数的实现，包括各种表达式、语句等的解析
    
    // FuncDef → FuncType Ident '(' [FuncFParams] ')' Block
    void parseFuncDef() {
        parseFuncType();
        match(IDENFR);
        match(LPARENT);
        
        if (!peek(RPARENT)) {
            parseFuncFParams();
        }
        
        match(RPARENT);
        parseBlock();
        
        outputGrammarComponent("FuncDef");
    }
    
    // MainFuncDef → 'int' 'main' '(' ')' Block
    void parseMainFuncDef() {
        match(INTTK);
        match(MAINTK);
        match(LPARENT);
        match(RPARENT);
        parseBlock();
        
        outputGrammarComponent("MainFuncDef");
    }
    
    // FuncType → 'void' | 'int'
    void parseFuncType() {
        if (peek(VOIDTK)) {
            match(VOIDTK);
        } else {
            match(INTTK);
        }
        
        outputGrammarComponent("FuncType");
    }
    
    // FuncFParams → FuncFParam { ',' FuncFParam }
    void parseFuncFParams() {
        parseFuncFParam();
        
        while (peek(COMMA)) {
            match(COMMA);
            parseFuncFParam();
        }
        
        outputGrammarComponent("FuncFParams");
    }
    
    // FuncFParam → BType Ident
    void parseFuncFParam() {
        parseBType();
        match(IDENFR);
        
        outputGrammarComponent("FuncFParam");
    }
    
    // Block → '{' { BlockItem } '}'
    void parseBlock() {
        match(LBRACE);
        
        while (!peek(RBRACE)) {
            parseBlockItem();
        }
        
        match(RBRACE);
        
        outputGrammarComponent("Block");
    }
    
    // BlockItem → Decl | Stmt
    void parseBlockItem() {
        if (peek(CONSTTK) || (peek(INTTK) && !peek(MAINTK))) {
            parseDecl();
        } else {
            parseStmt();
        }
    }
    
    // Stmt → LVal '=' Exp ';' | [Exp] ';' | Block | 'if' '(' Cond ')' Stmt [ 'else' Stmt ] | ...
    void parseStmt() {
        if (peek(IFTK)) {
            match(IFTK);
            match(LPARENT);
            parseCond();
            match(RPARENT);
            parseStmt();
            
            if (peek(ELSETK)) {
                match(ELSETK);
                parseStmt();
            }
        } else if (peek(WHILETK)) {
            match(WHILETK);
            match(LPARENT);
            parseCond();
            match(RPARENT);
            parseStmt();
        } else if (peek(BREAKTK)) {
            match(BREAKTK);
            match(SEMICN);
        } else if (peek(CONTINUETK)) {
            match(CONTINUETK);
            match(SEMICN);
        } else if (peek(RETURNTK)) {
            match(RETURNTK);
            
            if (!peek(SEMICN)) {
                parseExp();
            }
            
            match(SEMICN);
        } else if (peek(PRINTFTK)) {
            match(PRINTFTK);
            match(LPARENT);
            match(STRCON);
            
            while (peek(COMMA)) {
                match(COMMA);
                parseExp();
            }
            
            match(RPARENT);
            match(SEMICN);
        } else if (peek(LBRACE)) {
            parseBlock();
        } else if (peek(SEMICN)) {
            match(SEMICN);
        } else {
            // 处理LVal = Exp; 或 LVal = getint(); 或 Exp;
            int savedIndex = currentTokenIndex;
            bool isLValAssign = false;
            
            try {
                parseLVal();
                if (peek(ASSIGN)) {
                    isLValAssign = true;
                    match(ASSIGN);
                    
                    if (peek(GETINTTK)) {
                        match(GETINTTK);
                        match(LPARENT);
                        match(RPARENT);
                    } else {
                        parseExp();
                    }
                    
                    match(SEMICN);
                }
            } catch (...) {
                // 回溯
                currentTokenIndex = savedIndex;
                isLValAssign = false;
            }
            
            if (!isLValAssign) {
                currentTokenIndex = savedIndex;
                // Exp;
                parseExp();
                match(SEMICN);
            }
        }
        
        outputGrammarComponent("Stmt");
    }
    
    // 表达式相关的解析函数
    
    // Exp → AddExp
    void parseExp() {
        parseAddExp();
        
        outputGrammarComponent("Exp");
    }
    
    // Cond → LOrExp
    void parseCond() {
        parseLOrExp();
        
        outputGrammarComponent("Cond");
    }
    
    // LVal → Ident ['[' Exp ']']
    void parseLVal() {
        match(IDENFR);
        
        if (peek(LBRACK)) {
            match(LBRACK);
            parseExp();
            match(RBRACK);
        }
        
        outputGrammarComponent("LVal");
    }
    
    // PrimaryExp → '(' Exp ')' | LVal | Number
    void parsePrimaryExp() {
        if (peek(LPARENT)) {
            match(LPARENT);
            parseExp();
            match(RPARENT);
        } else if (peek(INTCON)) {
            parseNumber();
        } else {
            parseLVal();
        }
        
        outputGrammarComponent("PrimaryExp");
    }
    
    // Number → IntConst
    void parseNumber() {
        match(INTCON);
        
        outputGrammarComponent("Number");
    }
    
    // UnaryExp → PrimaryExp | Ident '(' [FuncRParams] ')' | UnaryOp UnaryExp
    void parseUnaryExp() {
        if ((peek(PLUS) || peek(MINU) || peek(NOT))) {
            parseUnaryOp();
            parseUnaryExp();
        } else if (peek(IDENFR) && 
                  currentTokenIndex + 1 < tokens.size() && 
                  tokens[currentTokenIndex + 1].type == LPARENT) {
            match(IDENFR);
            match(LPARENT);
            
            if (!peek(RPARENT)) {
                parseFuncRParams();
            }
            
            match(RPARENT);
        } else {
            parsePrimaryExp();
        }
        
        outputGrammarComponent("UnaryExp");
    }
    
    // UnaryOp → '+' | '−' | '!'
    void parseUnaryOp() {
        if (peek(PLUS)) {
            match(PLUS);
        } else if (peek(MINU)) {
            match(MINU);
        } else {
            match(NOT);
        }
        
        outputGrammarComponent("UnaryOp");
    }
    
    // FuncRParams → Exp { ',' Exp }
    void parseFuncRParams() {
        parseExp();
        
        while (peek(COMMA)) {
            match(COMMA);
            parseExp();
        }
        
        outputGrammarComponent("FuncRParams");
    }
    
    // MulExp → UnaryExp | MulExp ('*' | '/' | '%') UnaryExp
    void parseMulExp() {
        parseUnaryExp();
        outputGrammarComponent("MulExp");
        
        while (peek(MULT) || peek(DIV) || peek(MOD)) {
            if (peek(MULT)) {
                match(MULT);
            } else if (peek(DIV)) {
                match(DIV);
            } else {
                match(MOD);
            }
            
            parseUnaryExp();
            outputGrammarComponent("MulExp");
        }
    }
    
    // AddExp → MulExp | AddExp ('+' | '−') MulExp
    void parseAddExp() {
        parseMulExp();
        outputGrammarComponent("AddExp");
        
        while (peek(PLUS) || peek(MINU)) {
            if (peek(PLUS)) {
                match(PLUS);
            } else {
                match(MINU);
            }
            
            parseMulExp();
            outputGrammarComponent("AddExp");
        }
    }
    
    // RelExp → AddExp | RelExp ('<' | '>' | '<=' | '>=') AddExp
    void parseRelExp() {
        parseAddExp();
        outputGrammarComponent("RelExp");
        
        while (peek(LSS) || peek(GRE) || peek(LEQ) || peek(GEQ)) {
            if (peek(LSS)) {
                match(LSS);
            } else if (peek(GRE)) {
                match(GRE);
            } else if (peek(LEQ)) {
                match(LEQ);
            } else {
                match(GEQ);
            }
            
            parseAddExp();
            outputGrammarComponent("RelExp");
        }
    }
    
    // EqExp → RelExp | EqExp ('==' | '!=') RelExp
    void parseEqExp() {
        parseRelExp();
        outputGrammarComponent("EqExp");
        
        while (peek(EQL) || peek(NEQ)) {
            if (peek(EQL)) {
                match(EQL);
            } else {
                match(NEQ);
            }
            
            parseRelExp();
            outputGrammarComponent("EqExp");
        }
    }
    
    // LAndExp → EqExp | LAndExp '&&' EqExp
    void parseLAndExp() {
        parseEqExp();
        outputGrammarComponent("LAndExp");
        
        while (peek(AND)) {
            match(AND);
            parseEqExp();
            outputGrammarComponent("LAndExp");
        }
    }
    
    // LOrExp → LAndExp | LOrExp '||' LAndExp
    void parseLOrExp() {
        parseLAndExp();
        outputGrammarComponent("LOrExp");
        
        while (peek(OR)) {
            match(OR);
            parseLAndExp();
            outputGrammarComponent("LOrExp");
        }
    }
    
    // ConstExp → AddExp
    void parseConstExp() {
        parseAddExp();
        
        outputGrammarComponent("ConstExp");
    }
};

vector<Token> lexicalAnalysis(const string &input) {
    vector<Token> tokens;
    size_t index = 0;
    int lineNumber = 1; // 跟踪行号
    
    while (index < input.length()) {
        char currentChar = input[index];

        // 统计行号
        if (currentChar == '\n') {
            lineNumber++;
            index++;
            continue;
        }

        // 跳过空白字符
        if (isspace(currentChar)) {
            index++;
            continue;
        }

        // 处理标识符和关键字
        if (isLetter(currentChar)) {
            string ident;
            while (index < input.length() && (isLetter(input[index]) || isDigit(input[index]))) {
                ident += input[index];
                index++;
            }
            
            TokenType type = getKeywordType(ident);
            tokens.push_back({type, ident, lineNumber});
            continue;
        }

        // 处理数字
        if (isDigit(currentChar)) {
            string number;
            while (index < input.length() && isDigit(input[index])) {
                number += input[index];
                index++;
            }
            
            tokens.push_back({INTCON, number, lineNumber});
            continue;
        }

        // 处理字符串
        if (currentChar == '"') {
            string str;
            index++; // 跳过开始的引号
            
            while (index < input.length() && input[index] != '"') {
                str += input[index];
                index++;
            }
            
            if (index < input.length()) {
                index++; // 跳过结束的引号
            }
            
            tokens.push_back({STRCON, "\"" + str + "\"", lineNumber});
            continue;
        }

        // 处理注释
        if (currentChar == '/') {
            if (index + 1 < input.length()) {
                if (input[index + 1] == '/') { // 单行注释
                    while (index < input.length() && input[index] != '\n')
                        index++;
                    continue;
                }
                else if (input[index + 1] == '*') { // 多行注释
                    index += 2;
                    while (index + 1 < input.length() && !(input[index] == '*' && input[index + 1] == '/')) {
                        if (input[index] == '\n') {
                            lineNumber++;
                        }
                        index++;
                    }
                    if (index + 1 < input.length()) {
                        index += 2;
                    }
                    continue;
                }
            }
        }

        // 处理操作符和符号
        TokenType tokenType = UNDEFINED;
        string symbol(1, currentChar);

        // 检查双字符操作符
        if (index + 1 < input.length()) {
            string symbolPair = symbol + input[index + 1];
            if (symbolPair == "==") {
                tokenType = EQL;
                tokens.push_back({tokenType, symbolPair, lineNumber});
                index += 2;
                continue;
            }
            else if (symbolPair == ">=") {
                tokenType = GEQ;
                tokens.push_back({tokenType, symbolPair, lineNumber});
                index += 2;
                continue;
            }
            else if (symbolPair == "<=") {
                tokenType = LEQ;
                tokens.push_back({tokenType, symbolPair, lineNumber});
                index += 2;
                continue;
            }
            else if (symbolPair == "!=") {
                tokenType = NEQ;
                tokens.push_back({tokenType, symbolPair, lineNumber});
                index += 2;
                continue;
            }
            else if (symbolPair == "&&") {
                tokenType = AND;
                tokens.push_back({tokenType, symbolPair, lineNumber});
                index += 2;
                continue;
            }
            else if (symbolPair == "||") {
                tokenType = OR;
                tokens.push_back({tokenType, symbolPair, lineNumber});
                index += 2;
                continue;
            }
        }

        // 单字符操作符
        if (symbol == "<")
            tokenType = LSS;
        else if (symbol == ">")
            tokenType = GRE;
        else if (symbol == "=")
            tokenType = ASSIGN;
        else if (symbol == "+")
            tokenType = PLUS;
        else if (symbol == "-")
            tokenType = MINU;
        else if (symbol == "*")
            tokenType = MULT;
        else if (symbol == "/")
            tokenType = DIV;
        else if (symbol == "%")
            tokenType = MOD;
        else if (symbol == "(")
            tokenType = LPARENT;
        else if (symbol == ")")
            tokenType = RPARENT;
        else if (symbol == ",")
            tokenType = COMMA;
        else if (symbol == ";")
            tokenType = SEMICN;
        else if (symbol == "[")
            tokenType = LBRACK;
        else if (symbol == "]")
            tokenType = RBRACK;
        else if (symbol == "!")
            tokenType = NOT;
        else if (symbol == "{")
            tokenType = LBRACE;
        else if (symbol == "}")
            tokenType = RBRACE;

        if (tokenType != UNDEFINED) {
            tokens.push_back({tokenType, symbol, lineNumber});
            index++;
        } else {
            // 跳过无法识别的字符
            index++;
        }
    }
    
    return tokens;
}

// 原来analyze.cpp中的辅助函数，移植到这里
bool isLetter(char c) {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}

bool isDigit(char c) {
    return c >= '0' && c <= '9';
}

TokenType getKeywordType(const string &ident) {
    if (ident == "main")
        return MAINTK;
    if (ident == "const")
        return CONSTTK;
    if (ident == "int")
        return INTTK;
    if (ident == "break")
        return BREAKTK;
    if (ident == "continue")
        return CONTINUETK;
    if (ident == "if")
        return IFTK;
    if (ident == "else")
        return ELSETK;
    if (ident == "void")
        return VOIDTK;
    if (ident == "while")
        return WHILETK;
    if (ident == "getint")
        return GETINTTK;
    if (ident == "printf")
        return PRINTFTK;
    if (ident == "return")
        return RETURNTK;
    return IDENFR; // 如果不是关键字则为标识符
}

int main() {
    ifstream inputFile("testfile.txt");
    ofstream outputFile("output.txt");
    string input((istreambuf_iterator<char>(inputFile)), istreambuf_iterator<char>());
    
    // 进行词法分析
    vector<Token> tokens = lexicalAnalysis(input);
    
    // 进行语法分析
    Parser parser(tokens, outputFile);
    parser.parseCompUnit();
    
    inputFile.close();
    outputFile.close();
    return 0;
}
