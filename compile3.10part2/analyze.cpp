#include <iostream>
#include <fstream>
#include <string>
#include <map>

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

map<TokenType, string> TokenTypeToString = {
    {IDENFR, "IDENFR"}, {INTCON, "INTCON"}, {STRCON, "STRCON"}, {MAINTK, "MAINTK"}, {CONSTTK, "CONSTTK"}, {INTTK, "INTTK"}, {BREAKTK, "BREAKTK"}, {CONTINUETK, "CONTINUETK"}, {IFTK, "IFTK"}, {ELSETK, "ELSETK"}, {VOIDTK, "VOIDTK"}, {WHILETK, "WHILETK"}, {GETINTTK, "GETINTTK"}, {PRINTFTK, "PRINTFTK"}, {RETURNTK, "RETURNTK"}, {ASSIGN, "ASSIGN"}, {PLUS, "PLUS"}, {MINU, "MINU"}, {MULT, "MULT"}, {DIV, "DIV"}, {MOD, "MOD"}, {NOT, "NOT"}, {AND, "AND"}, {OR, "OR"}, {LSS, "LSS"}, {LEQ, "LEQ"}, {GRE, "GRE"}, {GEQ, "GEQ"}, {EQL, "EQL"}, {NEQ, "NEQ"}, {SEMICN, "SEMICN"}, {COMMA, "COMMA"}, {LPARENT, "LPARENT"}, {RPARENT, "RPARENT"}, {LBRACK, "LBRACK"}, {RBRACK, "RBRACK"}, {LBRACE, "LBRACE"}, {RBRACE, "RBRACE"}};

bool isLetter(char c)
{
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}

bool isDigit(char c)
{
    return c >= '0' && c <= '9';
}

TokenType getKeywordType(const string &ident)
{
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
    return IDENFR; // Default to IDENFR if not a keyword
}

void processIdentifier(const string &ident, ofstream &output)
{
    TokenType tokenType = getKeywordType(ident);
    output << TokenTypeToString[tokenType] << " " << ident << endl;
}

void processNumber(const string &number, ofstream &output)
{
    output << TokenTypeToString[INTCON] << " " << number << endl;
}

void processString(const string &str, ofstream &output)
{
    output << TokenTypeToString[STRCON] << " \"" << str << "\"" << endl;
}

void processOperator(const string &symbol, ofstream &output, size_t &index, const string &input)
{
    TokenType tokenType = UNDEFINED;

    // Check for two-character operators
    if (index + 1 < input.length())
    {
        string symbolPair = symbol + input[index + 1];
        if (symbolPair == "==")
            tokenType = EQL;
        else if (symbolPair == ">=")
            tokenType = GEQ;
        else if (symbolPair == "<=")
            tokenType = LEQ;
        else if (symbolPair == "!=")
            tokenType = NEQ;
        else if (symbolPair == "&&")
            tokenType = AND;
        else if (symbolPair == "||")
            tokenType = OR;
        if (tokenType != UNDEFINED)
        {
            output << TokenTypeToString[tokenType] << " " << symbolPair << endl;
            index += 2;
            return;
        }
    }

    // Single character operators
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

    if (tokenType != UNDEFINED)
    {
        output << TokenTypeToString[tokenType] << " " << symbol << endl;
        index++;
    }
}

void Solve(const string &input, ofstream &output)
{
    size_t index = 0;
    while (index < input.length())
    {
        char currentChar = input[index];

        // Skip whitespace
        if (isspace(currentChar))
        {
            index++;
            continue;
        }

        // Process identifiers
        if (isLetter(currentChar))
        {
            string ident;
            while (index < input.length() && (isLetter(currentChar) || isDigit(currentChar)))
            {
                ident += currentChar;
                index++;
                if (index >= input.length())
                    break;
                currentChar = input[index];
            }
            processIdentifier(ident, output);
            continue;
        }

        // Process numbers
        if (isDigit(currentChar))
        {
            string number;
            while (index < input.length() && isDigit(currentChar))
            {
                number += currentChar;
                index++;
                if (index >= input.length())
                    break;
                currentChar = input[index];
            }
            processNumber(number, output);
            continue;
        }

        // Process strings
        if (currentChar == '"')
        {
            string str;
            index++;
            while (index < input.length() && input[index] != '"')
            {
                str += input[index];
                index++;
            }
            if (index < input.length())
            {
                index++;
            }
            processString(str, output);
            continue;
        }

        // Process comments
        if (currentChar == '/')
        {
            if (index + 1 < input.length())
            {
                if (input[index + 1] == '/')
                {
                    while (index < input.length() && input[index] != '\n')
                        index++;
                    continue;
                }
                else if (input[index + 1] == '*')
                {
                    index += 2;
                    while (index + 1 < input.length() && !(input[index] == '*' && input[index + 1] == '/'))
                    {
                        index++;
                    }
                    if (index + 1 < input.length())
                    {
                        index += 2;
                    }
                    continue;
                }
            }
        }

        // Process operators and symbols
        string symbol(1, currentChar);
        processOperator(symbol, output, index, input);
    }
}

int main()
{
    ifstream inputFile("testfile.txt");
    ofstream outputFile("output.txt");
    string input((istreambuf_iterator<char>(inputFile)), istreambuf_iterator<char>());
    Solve(input, outputFile);
    inputFile.close();
    outputFile.close();
    return 0;
}