%{
#include <stdio.h>
#include <string.h>

// 输出文件
FILE *out;

// 定义token类别码
typedef enum {
    IDENFR,     // 标识符
    INTCON,     // 整型常量
    STRCON,     // 字符串常量
    MAINTK,     // main
    CONSTTK,    // const
    INTTK,      // int
    BREAKTK,    // break
    CONTINUETK, // continue
    IFTK,       // if
    ELSETK,     // else
    NOT,        // !
    AND,        // &&
    OR,         // ||
    WHILETK,    // while
    GETINTTK,   // getint
    PRINTFTK,   // printf
    PLUS,       // +
    MINU,       // -
    VOIDTK,     // void
    MULT,       // *
    DIV,        // /
    MOD,        // %
    LSS,        // <
    LEQ,        // <=
    GRE,        // >
    GEQ,        // >=
    EQL,        // ==
    NEQ,        // !=
    ASSIGN,     // =
    SEMICN,     // ;
    COMMA,      // ,
    LPARENT,    // (
    RPARENT,    // )
    LBRACK,     // [
    RBRACK,     // ]
    LBRACE,     // {
    RBRACE,     // }
    RETURNTK    // return
} TokenType;

// 辅助函数：将枚举值映射为字符串
const char* getTokenName(TokenType type) {
    static const char* tokenNames[] = {
        "IDENFR", "INTCON", "STRCON", "MAINTK", "CONSTTK", "INTTK", "BREAKTK", 
        "CONTINUETK", "IFTK", "ELSETK", "NOT", "AND", "OR", "WHILETK", "GETINTTK", 
        "PRINTFTK", "PLUS", "MINU", "VOIDTK", "MULT", "DIV", "MOD", "LSS", "LEQ", 
        "GRE", "GEQ", "EQL", "NEQ", "ASSIGN", "SEMICN", "COMMA", "LPARENT", 
        "RPARENT", "LBRACK", "RBRACK", "LBRACE", "RBRACE", "RETURNTK"
    };
    return tokenNames[type];
}

// 输出token的函数
void output_token(TokenType type, const char* text) {
    fprintf(out, "%s %s\n", getTokenName(type), text);
}
%}

/* Flex选项 */
%option noyywrap
%option nounput
%option noinput

/* 正则表达式定义 */
delim       [ \t\n\r]
ws          {delim}+
digit       [0-9]
nonzero     [1-9]
integer     {nonzero}{digit}*|0
letter      [a-zA-Z]
identifier  [a-zA-Z_][a-zA-Z0-9_]*
formatstr   \"[^"]*\"

%%

{ws}        { /* 忽略空白字符 */ }
"//".*      { /* 忽略单行注释 */ }
"/*"([^*]|[\r\n]|(\*+([^*/]|[\r\n])))*\*+"/" { /* 忽略多行注释 */ }

"main"      { output_token(MAINTK, yytext); }
"const"     { output_token(CONSTTK, yytext); }
"int"       { output_token(INTTK, yytext); }
"break"     { output_token(BREAKTK, yytext); }
"continue"  { output_token(CONTINUETK, yytext); }
"if"        { output_token(IFTK, yytext); }
"else"      { output_token(ELSETK, yytext); }
"while"     { output_token(WHILETK, yytext); }
"getint"    { output_token(GETINTTK, yytext); }
"printf"    { output_token(PRINTFTK, yytext); }
"void"      { output_token(VOIDTK, yytext); }
"return"    { output_token(RETURNTK, yytext); }

"!"         { output_token(NOT, yytext); }
"&&"        { output_token(AND, yytext); }
"||"        { output_token(OR, yytext); }
"+"         { output_token(PLUS, yytext); }
"-"         { output_token(MINU, yytext); }
"*"         { output_token(MULT, yytext); }
"/"         { output_token(DIV, yytext); }
"%"         { output_token(MOD, yytext); }
"<"         { output_token(LSS, yytext); }
"<="        { output_token(LEQ, yytext); }
">"         { output_token(GRE, yytext); }
">="        { output_token(GEQ, yytext); }
"=="        { output_token(EQL, yytext); }
"!="        { output_token(NEQ, yytext); }
"="         { output_token(ASSIGN, yytext); }
";"         { output_token(SEMICN, yytext); }
","         { output_token(COMMA, yytext); }
"("         { output_token(LPARENT, yytext); }
")"         { output_token(RPARENT, yytext); }
"["         { output_token(LBRACK, yytext); }
"]"         { output_token(RBRACK, yytext); }
"{"         { output_token(LBRACE, yytext); }
"}"         { output_token(RBRACE, yytext); }

{integer}   { output_token(INTCON, yytext); }
{formatstr} { output_token(STRCON, yytext); }
{identifier} { 
    /* 检查是否是关键字 - 实际上关键字已在上面匹配 */
    output_token(IDENFR, yytext); 
}
.           { fprintf(stderr, "Error: unexpected character: %s\n", yytext); }

%%

int main(int argc, char** argv) {
    if (argc < 3) {
        fprintf(stderr, "Usage: %s <input_file> <output_file>\n", argv[0]);
        return 1;
    }

    FILE *in = fopen(argv[1], "r");
    if (!in) {
        fprintf(stderr, "Error: Could not open input file %s\n", argv[1]);
        return 1;
    }

    out = fopen(argv[2], "w");
    if (!out) {
        fprintf(stderr, "Error: Could not open output file %s\n", argv[2]);
        fclose(in);
        return 1;
    }

    yyin = in;
    yylex();

    fclose(in);
    fclose(out);
    return 0;
} 