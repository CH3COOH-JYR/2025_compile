#ifndef AST_H
#define AST_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// AST节点类型枚举
typedef enum {
    // 程序结构
    NODE_PROGRAM,        // 整个程序
    NODE_COMP_UNIT,      // 编译单元
    
    // 声明
    NODE_CONST_DECL,     // 常量声明
    NODE_CONST_DEF,      // 常量定义
    NODE_CONST_INIT_VAL, // 常量初值
    NODE_VAR_DECL,       // 变量声明
    NODE_VAR_DEF,        // 变量定义
    NODE_INIT_VAL,       // 变量初值
    
    // 函数相关
    NODE_FUNC_DEF,       // 函数定义
    NODE_MAIN_FUNC_DEF,  // 主函数定义
    NODE_FUNC_PARAM,     // 函数参数
    NODE_FUNC_CALL,      // 函数调用
    NODE_FUNC_PARAM_LIST,// 函数参数列表
    NODE_FUNC_RPARAM_LIST,// 函数实参列表
    
    // 语句
    NODE_BLOCK,          // 语句块
    NODE_STMT,           // 语句
    NODE_ASSIGN_STMT,    // 赋值语句
    NODE_IF_STMT,        // if语句
    NODE_WHILE_STMT,     // while语句
    NODE_BREAK_STMT,     // break语句
    NODE_CONTINUE_STMT,  // continue语句
    NODE_RETURN_STMT,    // return语句
    NODE_EMPTY_STMT,     // 空语句
    NODE_PRINTF_STMT,    // printf语句
    
    // 表达式
    NODE_EXP,            // 表达式
    NODE_COND,           // 条件表达式
    NODE_LVAL,           // 左值表达式
    NODE_PRIMARY_EXP,    // 基本表达式
    NODE_UNARY_EXP,      // 一元表达式
    NODE_BINARY_EXP,     // 二元表达式
    NODE_NUMBER,         // 数字常量
    NODE_ARRAY_ACCESS,   // 数组访问

    // 运算符
    NODE_PLUS,           // +
    NODE_MINUS,          // -
    NODE_MULTIPLY,       // *
    NODE_DIVIDE,         // /
    NODE_MOD,            // %
    NODE_LESS,           // <
    NODE_LESS_EQUAL,     // <=
    NODE_GREATER,        // >
    NODE_GREATER_EQUAL,  // >=
    NODE_EQUAL,          // ==
    NODE_NOT_EQUAL,      // !=
    NODE_AND,            // &&
    NODE_OR,             // ||
    NODE_NOT,            // !

    // 标识符和常量
    NODE_IDENT,          // 标识符
    NODE_INT_CONST,      // 整型常量
    NODE_STRING_CONST,   // 字符串常量
    
    // 其他
    NODE_ARRAY_DIMS,     // 数组维度
    NODE_LIST            // 通用列表节点
} ASTNodeType;

// AST节点结构体
typedef struct ASTNode {
    ASTNodeType type;          // 节点类型
    char *name;                // 节点名称（用于dot输出）
    
    // 节点值
    union {
        int int_value;         // 整型值
        char *str_value;       // 字符串值
        char *op;              // 运算符
    } value;
    
    // 子节点
    struct ASTNode **children; // 子节点数组
    int child_count;           // 子节点数量
    int child_capacity;        // 子节点容量

    // 节点属性
    char *data_type;           // 数据类型
    int line_no;               // 行号
} ASTNode;

// AST操作函数
ASTNode* create_node(ASTNodeType type, const char *name);
void add_child(ASTNode *parent, ASTNode *child);
void free_ast(ASTNode *node);
void print_ast_dot(ASTNode *root, FILE *fp);
void generate_ast_dot(ASTNode *root, const char *filename);
void merge_node_list(ASTNode *dest, ASTNode *src);

#endif // AST_H 