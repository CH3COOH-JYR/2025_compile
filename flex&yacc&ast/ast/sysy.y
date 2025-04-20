%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "ast.h"

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

// AST根节点
ASTNode *ast_root = NULL;
%}

%union {
    int int_val;
    char* str_val;
    char* id_val;
    struct ASTNode *ast_node;
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

// 非终结符类型声明
%type <ast_node> CompUnit Decl ConstDecl ConstDef ConstInitVal ArrayDims
%type <ast_node> VarDecl VarDef InitVal FuncDef Block
%type <ast_node> BlockItem Stmt Exp Cond LVal PrimaryExp Number
%type <ast_node> UnaryExp UnaryOp MulExp AddExp RelExp EqExp LAndExp LOrExp
%type <ast_node> FuncRParams FuncFParams FuncFParam ConstExp ConstDefList
%type <ast_node> VarDefList InitValList ConstInitValList FuncFParamArrayDims
%type <ast_node> BlockItems ArraySubscripts PrintfParams

// 指定文法的开始符号
%start CompUnit

%%

// CompUnit 是整个程序的起始点
CompUnit    : Decl                          
            {
                $$ = create_node(NODE_COMP_UNIT, "CompUnit");
                add_child($$, $1);
                ast_root = $$;
            }
            | FuncDef
            {
                $$ = create_node(NODE_COMP_UNIT, "CompUnit");
                add_child($$, $1);
                ast_root = $$;
            }
            | CompUnit Decl 
            {
                $$ = $1;
                add_child($$, $2);
            }
            | CompUnit FuncDef              
            { 
                output_grammar_component("CompUnit");
                $$ = $1;
                add_child($$, $2);
                ast_root = $$;
            }
            ;

// 声明
Decl        : ConstDecl
            {
                $$ = $1;
            }
            | VarDecl
            {
                $$ = $1;
            }
            ;

// 常量声明
ConstDecl   : CONSTTK INTTK ConstDef SEMICN             
            { 
                output_grammar_component("ConstDecl");
                $$ = create_node(NODE_CONST_DECL, "ConstDecl");
                add_child($$, $3);
            }
            | CONSTTK INTTK ConstDef ConstDefList SEMICN  
            { 
                output_grammar_component("ConstDecl");
                $$ = create_node(NODE_CONST_DECL, "ConstDecl");
                add_child($$, $3);
                
                // 添加所有常量定义并安全释放列表节点
                merge_node_list($$, $4);
            }
            ;

ConstDefList: COMMA ConstDef
            {
                $$ = create_node(NODE_LIST, "ConstDefList");
                add_child($$, $2);
            }
            | ConstDefList COMMA ConstDef
            {
                $$ = $1;
                add_child($$, $3);
            }
            ;

// 常量定义
ConstDef    : IDENFR ASSIGN ConstInitVal
            {
                $$ = create_node(NODE_CONST_DEF, "ConstDef");
                
                // 添加标识符节点
                ASTNode *ident = create_node(NODE_IDENT, "Identifier");
                ident->value.str_value = strdup($1);
                add_child($$, ident);
                
                // 添加初值节点
                add_child($$, $3);
                free($1);
            }
            | IDENFR ArrayDims ASSIGN ConstInitVal
            {
                $$ = create_node(NODE_CONST_DEF, "ConstDef");
                
                // 添加标识符节点
                ASTNode *ident = create_node(NODE_IDENT, "Identifier");
                ident->value.str_value = strdup($1);
                add_child($$, ident);
                
                // 添加数组维度节点
                add_child($$, $2);
                
                // 添加初值节点
                add_child($$, $4);
                free($1);
            }
            ;

// 数组维度
ArrayDims   : LBRACK ConstExp RBRACK
            {
                $$ = create_node(NODE_ARRAY_DIMS, "ArrayDims");
                add_child($$, $2);
            }
            | ArrayDims LBRACK ConstExp RBRACK
            {
                $$ = $1;
                add_child($$, $3);
            }
            ;

// 常量初值
ConstInitVal: ConstExp
            {
                $$ = create_node(NODE_CONST_INIT_VAL, "ConstInitVal");
                add_child($$, $1);
            }
            | LBRACE RBRACE
            {
                $$ = create_node(NODE_CONST_INIT_VAL, "ConstInitVal");
            }
            | LBRACE ConstInitVal RBRACE
            {
                $$ = create_node(NODE_CONST_INIT_VAL, "ConstInitVal");
                add_child($$, $2);
            }
            | LBRACE ConstInitVal ConstInitValList RBRACE
            {
                $$ = create_node(NODE_CONST_INIT_VAL, "ConstInitVal");
                add_child($$, $2);
                
                // 使用安全的列表合并函数
                merge_node_list($$, $3);
            }
            ;

ConstInitValList: COMMA ConstInitVal
                {
                    $$ = create_node(NODE_LIST, "ConstInitValList");
                    add_child($$, $2);
                }
                | ConstInitValList COMMA ConstInitVal
                {
                    $$ = $1;
                    add_child($$, $3);
                }
                ;

// 变量声明
VarDecl     : INTTK VarDef SEMICN            
            { 
                output_grammar_component("VarDecl");
                $$ = create_node(NODE_VAR_DECL, "VarDecl");
                add_child($$, $2);
            }
            | INTTK VarDef VarDefList SEMICN 
            { 
                output_grammar_component("VarDecl");
                $$ = create_node(NODE_VAR_DECL, "VarDecl");
                add_child($$, $2);
                
                // 使用安全的列表合并函数
                merge_node_list($$, $3);
            }
            ;

VarDefList  : COMMA VarDef
            {
                $$ = create_node(NODE_LIST, "VarDefList");
                add_child($$, $2);
            }
            | VarDefList COMMA VarDef
            {
                $$ = $1;
                add_child($$, $3);
            }
            ;

// 变量定义
VarDef      : IDENFR
            {
                $$ = create_node(NODE_VAR_DEF, "VarDef");
                
                // 添加标识符节点
                ASTNode *ident = create_node(NODE_IDENT, "Identifier");
                ident->value.str_value = strdup($1);
                add_child($$, ident);
                free($1);
            }
            | IDENFR ArrayDims
            {
                $$ = create_node(NODE_VAR_DEF, "VarDef");
                
                // 添加标识符节点
                ASTNode *ident = create_node(NODE_IDENT, "Identifier");
                ident->value.str_value = strdup($1);
                add_child($$, ident);
                
                // 添加数组维度节点
                add_child($$, $2);
                free($1);
            }
            | IDENFR ASSIGN InitVal
            {
                $$ = create_node(NODE_VAR_DEF, "VarDef");
                
                // 添加标识符节点
                ASTNode *ident = create_node(NODE_IDENT, "Identifier");
                ident->value.str_value = strdup($1);
                add_child($$, ident);
                
                // 添加初值节点
                add_child($$, $3);
                free($1);
            }
            | IDENFR ArrayDims ASSIGN InitVal
            {
                $$ = create_node(NODE_VAR_DEF, "VarDef");
                
                // 添加标识符节点
                ASTNode *ident = create_node(NODE_IDENT, "Identifier");
                ident->value.str_value = strdup($1);
                add_child($$, ident);
                
                // 添加数组维度节点
                add_child($$, $2);
                
                // 添加初值节点
                add_child($$, $4);
                free($1);
            }
            ;

// 变量初值
InitVal     : Exp
            {
                $$ = create_node(NODE_INIT_VAL, "InitVal");
                add_child($$, $1);
            }
            | LBRACE RBRACE
            {
                $$ = create_node(NODE_INIT_VAL, "InitVal");
            }
            | LBRACE InitVal RBRACE
            {
                $$ = create_node(NODE_INIT_VAL, "InitVal");
                add_child($$, $2);
            }
            | LBRACE InitVal InitValList RBRACE
            {
                $$ = create_node(NODE_INIT_VAL, "InitVal");
                add_child($$, $2);
                
                // 使用安全的列表合并函数
                merge_node_list($$, $3);
            }
            ;

InitValList : COMMA InitVal
            {
                $$ = create_node(NODE_LIST, "InitValList");
                add_child($$, $2);
            }
            | InitValList COMMA InitVal
            {
                $$ = $1;
                add_child($$, $3);
            }
            ;

// 函数定义
FuncDef     : VOIDTK IDENFR LPARENT RPARENT Block            
            { 
                output_grammar_component("FuncDef");
                $$ = create_node(NODE_FUNC_DEF, "FuncDef");
                
                // 添加返回类型节点
                ASTNode *ret_type = create_node(NODE_IDENT, "ReturnType");
                ret_type->value.str_value = strdup("void");
                add_child($$, ret_type);
                
                // 添加函数名节点
                ASTNode *func_name = create_node(NODE_IDENT, "FunctionName");
                func_name->value.str_value = strdup($2);
                add_child($$, func_name);
                
                // 添加函数体节点
                add_child($$, $5);
                free($2);
            }
            | VOIDTK IDENFR LPARENT FuncFParams RPARENT Block 
            { 
                output_grammar_component("FuncDef");
                $$ = create_node(NODE_FUNC_DEF, "FuncDef");
                
                // 添加返回类型节点
                ASTNode *ret_type = create_node(NODE_IDENT, "ReturnType");
                ret_type->value.str_value = strdup("void");
                add_child($$, ret_type);
                
                // 添加函数名节点
                ASTNode *func_name = create_node(NODE_IDENT, "FunctionName");
                func_name->value.str_value = strdup($2);
                add_child($$, func_name);
                
                // 添加参数节点
                add_child($$, $4);
                
                // 添加函数体节点
                add_child($$, $6);
                free($2);
            }
            | INTTK IDENFR LPARENT RPARENT Block              
            { 
                output_grammar_component("FuncDef");
                $$ = create_node(NODE_FUNC_DEF, "FuncDef");
                
                // 添加返回类型节点
                ASTNode *ret_type = create_node(NODE_IDENT, "ReturnType");
                ret_type->value.str_value = strdup("int");
                add_child($$, ret_type);
                
                // 添加函数名节点
                ASTNode *func_name = create_node(NODE_IDENT, "FunctionName");
                func_name->value.str_value = strdup($2);
                add_child($$, func_name);
                
                // 添加函数体节点
                add_child($$, $5);
                free($2);
            }
            | INTTK IDENFR LPARENT FuncFParams RPARENT Block  
            { 
                output_grammar_component("FuncDef");
                $$ = create_node(NODE_FUNC_DEF, "FuncDef");
                
                // 添加返回类型节点
                ASTNode *ret_type = create_node(NODE_IDENT, "ReturnType");
                ret_type->value.str_value = strdup("int");
                add_child($$, ret_type);
                
                // 添加函数名节点
                ASTNode *func_name = create_node(NODE_IDENT, "FunctionName");
                func_name->value.str_value = strdup($2);
                add_child($$, func_name);
                
                // 添加参数节点
                add_child($$, $4);
                
                // 添加函数体节点
                add_child($$, $6);
                free($2);
            }
            | INTTK MAINTK LPARENT RPARENT Block              
            { 
                output_grammar_component("MainFuncDef");
                $$ = create_node(NODE_MAIN_FUNC_DEF, "MainFuncDef");
                
                // 添加函数体节点
                add_child($$, $5);
            }
            ;

// 函数形参表
FuncFParams : FuncFParam
            {
                $$ = create_node(NODE_FUNC_PARAM_LIST, "FuncFParams");
                add_child($$, $1);
            }
            | FuncFParams COMMA FuncFParam
            {
                $$ = $1;
                add_child($$, $3);
            }
            ;

// 函数形参
FuncFParam  : INTTK IDENFR
            {
                $$ = create_node(NODE_FUNC_PARAM, "FuncFParam");
                
                // 添加参数类型节点
                ASTNode *param_type = create_node(NODE_IDENT, "ParamType");
                param_type->value.str_value = strdup("int");
                add_child($$, param_type);
                
                // 添加参数名节点
                ASTNode *param_name = create_node(NODE_IDENT, "ParamName");
                param_name->value.str_value = strdup($2);
                add_child($$, param_name);
                free($2);
            }
            | INTTK IDENFR LBRACK RBRACK
            {
                $$ = create_node(NODE_FUNC_PARAM, "FuncFParam");
                
                // 添加参数类型节点
                ASTNode *param_type = create_node(NODE_IDENT, "ParamType");
                param_type->value.str_value = strdup("int[]");
                add_child($$, param_type);
                
                // 添加参数名节点
                ASTNode *param_name = create_node(NODE_IDENT, "ParamName");
                param_name->value.str_value = strdup($2);
                add_child($$, param_name);
                free($2);
            }
            | INTTK IDENFR LBRACK RBRACK FuncFParamArrayDims
            {
                $$ = create_node(NODE_FUNC_PARAM, "FuncFParam");
                
                // 添加参数类型节点
                ASTNode *param_type = create_node(NODE_IDENT, "ParamType");
                param_type->value.str_value = strdup("int[]");
                add_child($$, param_type);
                
                // 添加参数名节点
                ASTNode *param_name = create_node(NODE_IDENT, "ParamName");
                param_name->value.str_value = strdup($2);
                add_child($$, param_name);
                
                // 添加数组额外维度
                add_child($$, $5);
                free($2);
            }
            ;

FuncFParamArrayDims: LBRACK ConstExp RBRACK
                   {
                       $$ = create_node(NODE_ARRAY_DIMS, "FuncFParamArrayDims");
                       add_child($$, $2);
                   }
                   | FuncFParamArrayDims LBRACK ConstExp RBRACK
                   {
                       $$ = $1;
                       add_child($$, $3);
                   }
                   ;

// 语句块
Block       : LBRACE BlockItems RBRACE   
            { 
                output_grammar_component("Block");
                $$ = create_node(NODE_BLOCK, "Block");
                
                // 使用安全的列表合并函数
                if ($2) {
                    merge_node_list($$, $2);
                }
            }
            | LBRACE RBRACE              
            { 
                output_grammar_component("Block");
                $$ = create_node(NODE_BLOCK, "Block");
            }
            ;

BlockItems  : BlockItem
            {
                $$ = create_node(NODE_LIST, "BlockItems");
                if ($1) add_child($$, $1);
            }
            | BlockItems BlockItem
            {
                $$ = $1;
                if ($2) add_child($$, $2);
            }
            ;

BlockItem   : Decl
            {
                $$ = $1;
            }
            | Stmt
            {
                $$ = $1;
            }
            ;

// 语句
Stmt        : LVal ASSIGN Exp SEMICN                     
            { 
                output_grammar_component("Stmt");
                $$ = create_node(NODE_ASSIGN_STMT, "AssignStmt");
                add_child($$, $1);
                add_child($$, $3);
            }
            | SEMICN                                     
            { 
                output_grammar_component("Stmt");
                $$ = create_node(NODE_EMPTY_STMT, "EmptyStmt");
            }
            | Exp SEMICN                                 
            { 
                output_grammar_component("Stmt");
                $$ = create_node(NODE_STMT, "Stmt");
                add_child($$, $1);
            }
            | Block                                      
            { 
                output_grammar_component("Stmt");
                $$ = $1;
            }
            | IFTK LPARENT Cond RPARENT Stmt            
            { 
                output_grammar_component("Stmt");
                $$ = create_node(NODE_IF_STMT, "IfStmt");
                add_child($$, $3);
                add_child($$, $5);
            }
            | IFTK LPARENT Cond RPARENT Stmt ELSETK Stmt 
            { 
                output_grammar_component("Stmt");
                $$ = create_node(NODE_IF_STMT, "IfStmt");
                add_child($$, $3);
                add_child($$, $5);
                add_child($$, $7);
            }
            | WHILETK LPARENT Cond RPARENT Stmt         
            { 
                output_grammar_component("Stmt");
                $$ = create_node(NODE_WHILE_STMT, "WhileStmt");
                add_child($$, $3);
                add_child($$, $5);
            }
            | BREAKTK SEMICN                            
            { 
                output_grammar_component("Stmt");
                $$ = create_node(NODE_BREAK_STMT, "BreakStmt");
            }
            | CONTINUETK SEMICN                         
            { 
                output_grammar_component("Stmt");
                $$ = create_node(NODE_CONTINUE_STMT, "ContinueStmt");
            }
            | RETURNTK SEMICN                           
            { 
                output_grammar_component("Stmt");
                $$ = create_node(NODE_RETURN_STMT, "ReturnStmt");
            }
            | RETURNTK Exp SEMICN                       
            { 
                output_grammar_component("Stmt");
                $$ = create_node(NODE_RETURN_STMT, "ReturnStmt");
                add_child($$, $2);
            }
            | LVal ASSIGN GETINTTK LPARENT RPARENT SEMICN  
            { 
                output_grammar_component("Stmt");
                $$ = create_node(NODE_STMT, "GetintStmt");
                add_child($$, $1);
                
                // 创建getint函数调用节点
                ASTNode *getint = create_node(NODE_FUNC_CALL, "FunctionCall");
                ASTNode *func_name = create_node(NODE_IDENT, "FunctionName");
                func_name->value.str_value = strdup("getint");
                add_child(getint, func_name);
                
                add_child($$, getint);
            }
            | PRINTFTK LPARENT STRCON PrintfParams RPARENT SEMICN  
            { 
                output_grammar_component("Stmt");
                $$ = create_node(NODE_PRINTF_STMT, "PrintfStmt");
                
                // 添加字符串常量节点
                ASTNode *str = create_node(NODE_STRING_CONST, "StringConst");
                str->value.str_value = strdup($3);
                add_child($$, str);
                
                // 使用安全的列表合并函数
                if ($4) {
                    merge_node_list($$, $4);
                }
                free($3);
            }
            | PRINTFTK LPARENT STRCON RPARENT SEMICN    
            { 
                output_grammar_component("Stmt");
                $$ = create_node(NODE_PRINTF_STMT, "PrintfStmt");
                
                // 添加字符串常量节点
                ASTNode *str = create_node(NODE_STRING_CONST, "StringConst");
                str->value.str_value = strdup($3);
                add_child($$, str);
                free($3);
            }
            ;

PrintfParams: COMMA Exp
            {
                $$ = create_node(NODE_LIST, "PrintfParams");
                add_child($$, $2);
            }
            | PrintfParams COMMA Exp
            {
                $$ = $1;
                add_child($$, $3);
            }
            ;

// 表达式
Exp         : AddExp
            {
                $$ = create_node(NODE_EXP, "Exp");
                add_child($$, $1);
            }
            ;

// 条件表达式
Cond        : LOrExp
            {
                $$ = create_node(NODE_COND, "Cond");
                add_child($$, $1);
            }
            ;

// 左值表达式
LVal        : IDENFR
            {
                $$ = create_node(NODE_LVAL, "LVal");
                
                // 添加标识符节点
                ASTNode *ident = create_node(NODE_IDENT, "Identifier");
                ident->value.str_value = strdup($1);
                add_child($$, ident);
                free($1);
            }
            | IDENFR ArraySubscripts
            {
                $$ = create_node(NODE_ARRAY_ACCESS, "ArrayAccess");
                
                // 添加标识符节点
                ASTNode *ident = create_node(NODE_IDENT, "Identifier");
                ident->value.str_value = strdup($1);
                add_child($$, ident);
                
                // 使用安全的列表合并函数
                merge_node_list($$, $2);
                free($1);
            }
            ;

ArraySubscripts: LBRACK Exp RBRACK
               {
                   $$ = create_node(NODE_LIST, "ArraySubscripts");
                   add_child($$, $2);
               }
               | ArraySubscripts LBRACK Exp RBRACK
               {
                   $$ = $1;
                   add_child($$, $3);
               }
               ;

// 基本表达式
PrimaryExp  : LPARENT Exp RPARENT
            {
                $$ = $2;
            }
            | LVal
            {
                $$ = $1;
            }
            | Number
            {
                $$ = $1;
            }
            ;

Number      : INTCON
            {
                $$ = create_node(NODE_INT_CONST, "IntConst");
                $$->value.int_value = $1;
            }
            ;

// 一元表达式
UnaryExp    : PrimaryExp
            {
                $$ = $1;
            }
            | IDENFR LPARENT RPARENT
            {
                $$ = create_node(NODE_FUNC_CALL, "FunctionCall");
                
                // 添加函数名节点
                ASTNode *func_name = create_node(NODE_IDENT, "FunctionName");
                func_name->value.str_value = strdup($1);
                add_child($$, func_name);
                free($1);
            }
            | IDENFR LPARENT FuncRParams RPARENT
            {
                $$ = create_node(NODE_FUNC_CALL, "FunctionCall");
                
                // 添加函数名节点
                ASTNode *func_name = create_node(NODE_IDENT, "FunctionName");
                func_name->value.str_value = strdup($1);
                add_child($$, func_name);
                
                // 使用安全的列表合并函数
                merge_node_list($$, $3);
                free($1);
            }
            | UnaryOp UnaryExp
            {
                $$ = create_node(NODE_UNARY_EXP, "UnaryExp");
                add_child($$, $1);
                add_child($$, $2);
            }
            ;

UnaryOp     : PLUS
            {
                $$ = create_node(NODE_PLUS, "Plus");
                $$->value.op = strdup("+");
            }
            | MINU
            {
                $$ = create_node(NODE_MINUS, "Minus");
                $$->value.op = strdup("-");
            }
            | NOT
            {
                $$ = create_node(NODE_NOT, "Not");
                $$->value.op = strdup("!");
            }
            ;

// 函数实参表
FuncRParams : Exp
            {
                $$ = create_node(NODE_FUNC_RPARAM_LIST, "FuncRParams");
                add_child($$, $1);
            }
            | FuncRParams COMMA Exp
            {
                $$ = $1;
                add_child($$, $3);
            }
            ;

// 乘除模表达式
MulExp      : UnaryExp
            {
                $$ = $1;
            }
            | MulExp MULT UnaryExp
            {
                $$ = create_node(NODE_BINARY_EXP, "MulExp");
                $$->value.op = strdup("*");
                add_child($$, $1);
                add_child($$, $3);
            }
            | MulExp DIV UnaryExp
            {
                $$ = create_node(NODE_BINARY_EXP, "MulExp");
                $$->value.op = strdup("/");
                add_child($$, $1);
                add_child($$, $3);
            }
            | MulExp MOD UnaryExp
            {
                $$ = create_node(NODE_BINARY_EXP, "MulExp");
                $$->value.op = strdup("%");
                add_child($$, $1);
                add_child($$, $3);
            }
            ;

// 加减表达式
AddExp      : MulExp
            {
                $$ = $1;
            }
            | AddExp PLUS MulExp
            {
                $$ = create_node(NODE_BINARY_EXP, "AddExp");
                $$->value.op = strdup("+");
                add_child($$, $1);
                add_child($$, $3);
            }
            | AddExp MINU MulExp
            {
                $$ = create_node(NODE_BINARY_EXP, "AddExp");
                $$->value.op = strdup("-");
                add_child($$, $1);
                add_child($$, $3);
            }
            ;

// 关系表达式
RelExp      : AddExp
            {
                $$ = $1;
            }
            | RelExp LSS AddExp
            {
                $$ = create_node(NODE_BINARY_EXP, "RelExp");
                $$->value.op = strdup("<");
                add_child($$, $1);
                add_child($$, $3);
            }
            | RelExp LEQ AddExp
            {
                $$ = create_node(NODE_BINARY_EXP, "RelExp");
                $$->value.op = strdup("<=");
                add_child($$, $1);
                add_child($$, $3);
            }
            | RelExp GRE AddExp
            {
                $$ = create_node(NODE_BINARY_EXP, "RelExp");
                $$->value.op = strdup(">");
                add_child($$, $1);
                add_child($$, $3);
            }
            | RelExp GEQ AddExp
            {
                $$ = create_node(NODE_BINARY_EXP, "RelExp");
                $$->value.op = strdup(">=");
                add_child($$, $1);
                add_child($$, $3);
            }
            ;

// 相等性表达式
EqExp       : RelExp
            {
                $$ = $1;
            }
            | EqExp EQL RelExp
            {
                $$ = create_node(NODE_BINARY_EXP, "EqExp");
                $$->value.op = strdup("==");
                add_child($$, $1);
                add_child($$, $3);
            }
            | EqExp NEQ RelExp
            {
                $$ = create_node(NODE_BINARY_EXP, "EqExp");
                $$->value.op = strdup("!=");
                add_child($$, $1);
                add_child($$, $3);
            }
            ;

// 逻辑与表达式
LAndExp     : EqExp
            {
                $$ = $1;
            }
            | LAndExp AND EqExp
            {
                $$ = create_node(NODE_BINARY_EXP, "LAndExp");
                $$->value.op = strdup("&&");
                add_child($$, $1);
                add_child($$, $3);
            }
            ;

// 逻辑或表达式
LOrExp      : LAndExp
            {
                $$ = $1;
            }
            | LOrExp OR LAndExp
            {
                $$ = create_node(NODE_BINARY_EXP, "LOrExp");
                $$->value.op = strdup("||");
                add_child($$, $1);
                add_child($$, $3);
            }
            ;

// 常量表达式
ConstExp    : AddExp
            {
                $$ = create_node(NODE_EXP, "ConstExp");
                add_child($$, $1);
            }
            ;

%%

void yyerror(const char* s) {
    fprintf(stderr, "Error: %s\n", s);
}

int main(int argc, char** argv) {
    if (argc < 3) {
        fprintf(stderr, "Usage: %s <input_file> <output_file> [<ast_dot_file>]\n", argv[0]);
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

    // 如果提供了AST dot文件名，则输出AST
    if (argc > 3 && ast_root) {
        generate_ast_dot(ast_root, argv[3]);
    }

    // 清理资源
    if (ast_root) {
        free_ast(ast_root);
    }
    
    fclose(in);
    fclose(yylexout);
    return 0;
} 