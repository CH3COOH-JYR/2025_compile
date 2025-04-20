%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "y.tab.h"  // 包含yacc生成的头文件

// 对词法分析结果的报告
extern FILE *yylexout;  // 词法分析输出文件

// 解决strdup在某些系统上的兼容性问题
#ifndef strdup
char *strdup(const char *s) {
    char *d = malloc(strlen(s) + 1);
    if (d == NULL) return NULL;
    strcpy(d, s);
    return d;
}
#endif

// 获取token名称的辅助函数
const char* get_token_name(int token) {
    switch(token) {
        case IDENFR: return "IDENFR";
        case INTCON: return "INTCON";
        case STRCON: return "STRCON";
        case MAINTK: return "MAINTK";
        case CONSTTK: return "CONSTTK";
        case INTTK: return "INTTK";
        case BREAKTK: return "BREAKTK";
        case CONTINUETK: return "CONTINUETK";
        case IFTK: return "IFTK";
        case ELSETK: return "ELSETK";
        case NOT: return "NOT";
        case AND: return "AND";
        case OR: return "OR";
        case WHILETK: return "WHILETK";
        case GETINTTK: return "GETINTTK";
        case PRINTFTK: return "PRINTFTK";
        case PLUS: return "PLUS";
        case MINU: return "MINU";
        case VOIDTK: return "VOIDTK";
        case MULT: return "MULT";
        case DIV: return "DIV";
        case MOD: return "MOD";
        case LSS: return "LSS";
        case LEQ: return "LEQ";
        case GRE: return "GRE";
        case GEQ: return "GEQ";
        case EQL: return "EQL";
        case NEQ: return "NEQ";
        case ASSIGN: return "ASSIGN";
        case SEMICN: return "SEMICN";
        case COMMA: return "COMMA";
        case LPARENT: return "LPARENT";
        case RPARENT: return "RPARENT";
        case LBRACK: return "LBRACK";
        case RBRACK: return "RBRACK";
        case LBRACE: return "LBRACE";
        case RBRACE: return "RBRACE";
        case RETURNTK: return "RETURNTK";
        default: return "UNKNOWN";
    }
}

// 输出token的函数 - 已修改为不输出词法标记
void print_token(int token, char* text) {
    // 注释掉原来的输出代码，使其不再输出词法标记
    /*if (yylexout) {
        fprintf(yylexout, "%s %s\n", get_token_name(token), text);
    }*/
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

[ \t\n\r]+ { /* 忽略空白字符 */ }
"//"[^\n]*  { /* 忽略单行注释 */ }
"/*"([^*]|"*"[^/])*"*/"  { /* 忽略多行注释 */ }

"main"    { print_token(MAINTK, yytext); return MAINTK; }
"const"   { print_token(CONSTTK, yytext); return CONSTTK; }
"int"     { print_token(INTTK, yytext); return INTTK; }
"break"   { print_token(BREAKTK, yytext); return BREAKTK; }
"continue" { print_token(CONTINUETK, yytext); return CONTINUETK; }
"if"      { print_token(IFTK, yytext); return IFTK; }
"else"    { print_token(ELSETK, yytext); return ELSETK; }
"while"   { print_token(WHILETK, yytext); return WHILETK; }
"getint"  { print_token(GETINTTK, yytext); return GETINTTK; }
"printf"  { print_token(PRINTFTK, yytext); return PRINTFTK; }
"void"    { print_token(VOIDTK, yytext); return VOIDTK; }
"return"  { print_token(RETURNTK, yytext); return RETURNTK; }

"!"       { print_token(NOT, yytext); return NOT; }
"&&"      { print_token(AND, yytext); return AND; }
"||"      { print_token(OR, yytext); return OR; }
"+"       { print_token(PLUS, yytext); return PLUS; }
"-"       { print_token(MINU, yytext); return MINU; }
"*"       { print_token(MULT, yytext); return MULT; }
"/"       { print_token(DIV, yytext); return DIV; }
"%"       { print_token(MOD, yytext); return MOD; }
"<"       { print_token(LSS, yytext); return LSS; }
"<="      { print_token(LEQ, yytext); return LEQ; }
">"       { print_token(GRE, yytext); return GRE; }
">="      { print_token(GEQ, yytext); return GEQ; }
"=="      { print_token(EQL, yytext); return EQL; }
"!="      { print_token(NEQ, yytext); return NEQ; }
"="       { print_token(ASSIGN, yytext); return ASSIGN; }
";"       { print_token(SEMICN, yytext); return SEMICN; }
","       { print_token(COMMA, yytext); return COMMA; }
"("       { print_token(LPARENT, yytext); return LPARENT; }
")"       { print_token(RPARENT, yytext); return RPARENT; }
"["       { print_token(LBRACK, yytext); return LBRACK; }
"]"       { print_token(RBRACK, yytext); return RBRACK; }
"{"       { print_token(LBRACE, yytext); return LBRACE; }
"}"       { print_token(RBRACE, yytext); return RBRACE; }

[0-9]+    { 
    print_token(INTCON, yytext); 
    yylval.int_val = atoi(yytext); 
    return INTCON; 
}

\"([^\"\n]|\\\")*\"    { 
    print_token(STRCON, yytext); 
    yylval.str_val = strdup(yytext); 
    return STRCON; 
}

[a-zA-Z_][a-zA-Z0-9_]* { 
    print_token(IDENFR, yytext); 
    yylval.id_val = strdup(yytext);
    return IDENFR; 
}

.         { fprintf(stderr, "Error: unexpected character: %s\n", yytext); }

%% 