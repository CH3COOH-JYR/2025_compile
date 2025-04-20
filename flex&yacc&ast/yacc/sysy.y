%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// 输出文件
FILE *yylexout = NULL;  // 词法分析输出
FILE *yyparsout = NULL; // 语法分析输出

extern int yylex();
extern FILE* yyin;
extern char* yytext;

void yyerror(const char* s);

// 输出语法成分的函数
void output_grammar_component(const char* name) {
    if (yyparsout) {
        fprintf(yyparsout, "<%s>\n", name);
    }
}

// 全局变量，用于追踪是否是最后一次CompUnit规则的应用
int is_final_comp_unit = 0;
%}

%union {
    int int_val;
    char* str_val;
    char* id_val;
}

// 定义所有的token类型
%token <id_val> IDENFR
%token <int_val> INTCON
%token <str_val> STRCON
%token MAINTK CONSTTK INTTK BREAKTK CONTINUETK
%token IFTK ELSETK NOT AND OR WHILETK
%token GETINTTK PRINTFTK PLUS MINU VOIDTK
%token MULT DIV MOD LSS LEQ GRE GEQ
%token EQL NEQ ASSIGN SEMICN COMMA
%token LPARENT RPARENT LBRACK RBRACK LBRACE RBRACE
%token RETURNTK

// 指定文法的开始符号
%start CompUnit

%%

// CompUnit 是整个程序的起始点
CompUnit    : Decl                          
            | FuncDef
            | CompUnit Decl 
            | CompUnit FuncDef              { 
                // 这里不立即输出CompUnit标记，等到程序结束时再输出
                is_final_comp_unit = 1;
             }
            ;

// 声明
Decl        : ConstDecl
            | VarDecl
            ;

// 常量声明
ConstDecl   : CONSTTK INTTK ConstDef SEMICN             { output_grammar_component("ConstDecl"); }
            | CONSTTK INTTK ConstDef ConstDefList SEMICN  { output_grammar_component("ConstDecl"); }
            ;

ConstDefList: COMMA ConstDef
            | ConstDefList COMMA ConstDef
            ;

// 常量定义
ConstDef    : IDENFR ASSIGN ConstInitVal
            | IDENFR ArrayDims ASSIGN ConstInitVal
            ;

// 数组维度
ArrayDims   : LBRACK ConstExp RBRACK
            | ArrayDims LBRACK ConstExp RBRACK
            ;

// 常量初值
ConstInitVal: ConstExp
            | LBRACE RBRACE
            | LBRACE ConstInitVal RBRACE
            | LBRACE ConstInitVal ConstInitValList RBRACE
            ;

ConstInitValList: COMMA ConstInitVal
                | ConstInitValList COMMA ConstInitVal
                ;

// 变量声明
VarDecl     : INTTK VarDef SEMICN            { output_grammar_component("VarDecl"); }
            | INTTK VarDef VarDefList SEMICN { output_grammar_component("VarDecl"); }
            ;

VarDefList  : COMMA VarDef
            | VarDefList COMMA VarDef
            ;

// 变量定义
VarDef      : IDENFR
            | IDENFR ArrayDims
            | IDENFR ASSIGN InitVal
            | IDENFR ArrayDims ASSIGN InitVal
            ;

// 变量初值
InitVal     : Exp
            | LBRACE RBRACE
            | LBRACE InitVal RBRACE
            | LBRACE InitVal InitValList RBRACE
            ;

InitValList : COMMA InitVal
            | InitValList COMMA InitVal
            ;

// 函数定义
FuncDef     : VOIDTK IDENFR LPARENT RPARENT Block            { output_grammar_component("FuncDef"); }
            | VOIDTK IDENFR LPARENT FuncFParams RPARENT Block { output_grammar_component("FuncDef"); }
            | INTTK IDENFR LPARENT RPARENT Block              { output_grammar_component("FuncDef"); }
            | INTTK IDENFR LPARENT FuncFParams RPARENT Block  { output_grammar_component("FuncDef"); }
            | INTTK MAINTK LPARENT RPARENT Block              { output_grammar_component("MainFuncDef"); }
            ;

// 函数形参表
FuncFParams : FuncFParam
            | FuncFParams COMMA FuncFParam
            ;

// 函数形参
FuncFParam  : INTTK IDENFR
            | INTTK IDENFR LBRACK RBRACK
            | INTTK IDENFR LBRACK RBRACK FuncFParamArrayDims
            ;

FuncFParamArrayDims: LBRACK ConstExp RBRACK
                   | FuncFParamArrayDims LBRACK ConstExp RBRACK
                   ;

// 语句块
Block       : LBRACE BlockItems RBRACE   { output_grammar_component("Block"); }
            | LBRACE RBRACE              { output_grammar_component("Block"); }
            ;

BlockItems  : BlockItem
            | BlockItems BlockItem
            ;

BlockItem   : Decl
            | Stmt
            ;

// 语句
Stmt        : LVal ASSIGN Exp SEMICN                     { output_grammar_component("Stmt"); }
            | SEMICN                                     { output_grammar_component("Stmt"); }
            | Exp SEMICN                                 { output_grammar_component("Stmt"); }
            | Block                                      { output_grammar_component("Stmt"); }
            | IFTK LPARENT Cond RPARENT Stmt            { output_grammar_component("Stmt"); }
            | IFTK LPARENT Cond RPARENT Stmt ELSETK Stmt { output_grammar_component("Stmt"); }
            | WHILETK LPARENT Cond RPARENT Stmt         { output_grammar_component("Stmt"); }
            | BREAKTK SEMICN                            { output_grammar_component("Stmt"); }
            | CONTINUETK SEMICN                         { output_grammar_component("Stmt"); }
            | RETURNTK SEMICN                           { output_grammar_component("Stmt"); }
            | RETURNTK Exp SEMICN                       { output_grammar_component("Stmt"); }
            | LVal ASSIGN GETINTTK LPARENT RPARENT SEMICN  { output_grammar_component("Stmt"); }
            | PRINTFTK LPARENT STRCON PrintfParams RPARENT SEMICN  { output_grammar_component("Stmt"); }
            | PRINTFTK LPARENT STRCON RPARENT SEMICN    { output_grammar_component("Stmt"); }
            ;

PrintfParams: COMMA Exp
            | PrintfParams COMMA Exp
            ;

// 表达式
Exp         : AddExp
            ;

// 条件表达式
Cond        : LOrExp
            ;

// 左值表达式
LVal        : IDENFR
            | IDENFR ArraySubscripts
            ;

ArraySubscripts: LBRACK Exp RBRACK
               | ArraySubscripts LBRACK Exp RBRACK
               ;

// 基本表达式
PrimaryExp  : LPARENT Exp RPARENT
            | LVal
            | Number
            ;

Number      : INTCON
            ;

// 一元表达式
UnaryExp    : PrimaryExp
            | IDENFR LPARENT RPARENT
            | IDENFR LPARENT FuncRParams RPARENT
            | UnaryOp UnaryExp
            ;

UnaryOp     : PLUS
            | MINU
            | NOT
            ;

// 函数实参表
FuncRParams : Exp
            | FuncRParams COMMA Exp
            ;

// 乘除模表达式
MulExp      : UnaryExp
            | MulExp MULT UnaryExp
            | MulExp DIV UnaryExp
            | MulExp MOD UnaryExp
            ;

// 加减表达式
AddExp      : MulExp
            | AddExp PLUS MulExp
            | AddExp MINU MulExp
            ;

// 关系表达式
RelExp      : AddExp
            | RelExp LSS AddExp
            | RelExp LEQ AddExp
            | RelExp GRE AddExp
            | RelExp GEQ AddExp
            ;

// 相等性表达式
EqExp       : RelExp
            | EqExp EQL RelExp
            | EqExp NEQ RelExp
            ;

// 逻辑与表达式
LAndExp     : EqExp
            | LAndExp AND EqExp
            ;

// 逻辑或表达式
LOrExp      : LAndExp
            | LOrExp OR LAndExp
            ;

// 常量表达式
ConstExp    : AddExp
            ;

%%

void yyerror(const char* s) {
    fprintf(stderr, "Error: %s\n", s);
}

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

    yylexout = fopen(argv[2], "w");
    if (!yylexout) {
        fprintf(stderr, "Error: Could not open output file %s\n", argv[2]);
        fclose(in);
        return 1;
    }

    // 语法分析输出也输出到同一个文件
    yyparsout = yylexout;

    yyin = in;
    yyparse();
    
    // 解析完成后，如果设置了is_final_comp_unit标志，输出CompUnit标记
    if (is_final_comp_unit) {
        output_grammar_component("CompUnit");
    }

    fclose(in);
    fclose(yylexout);
    return 0;
} 