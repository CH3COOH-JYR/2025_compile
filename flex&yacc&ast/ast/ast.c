#include "ast.h"
#include <ctype.h>

// 创建一个新的AST节点
ASTNode* create_node(ASTNodeType type, const char *name) {
    ASTNode *node = (ASTNode *)malloc(sizeof(ASTNode));
    if (!node) {
        fprintf(stderr, "Error: Out of memory\n");
        exit(1);
    }
    
    // 初始化节点
    node->type = type;
    node->name = name ? strdup(name) : NULL;
    node->children = NULL;
    node->child_count = 0;
    node->child_capacity = 0;
    node->data_type = NULL;
    node->line_no = 0;
    
    // 初始化值
    node->value.int_value = 0;
    node->value.str_value = NULL;
    node->value.op = NULL;
    
    return node;
}

// 向父节点添加子节点
void add_child(ASTNode *parent, ASTNode *child) {
    if (!parent || !child) return;
    
    // 扩容子节点数组（如果需要）
    if (parent->child_count >= parent->child_capacity) {
        int new_capacity = parent->child_capacity == 0 ? 4 : parent->child_capacity * 2;
        ASTNode **new_children = (ASTNode **)realloc(parent->children, new_capacity * sizeof(ASTNode *));
        if (!new_children) {
            fprintf(stderr, "Error: Out of memory\n");
            exit(1);
        }
        parent->children = new_children;
        parent->child_capacity = new_capacity;
    }
    
    // 添加子节点
    parent->children[parent->child_count++] = child;
}

// 释放AST节点及其所有子节点
void free_ast(ASTNode *node) {
    if (!node) return;
    
    // 使用节点地址作为防护措施，标记已经被释放的节点
    static void *freed_nodes[1024] = {0};
    static int freed_count = 0;
    
    // 检查节点是否已被释放
    for (int i = 0; i < freed_count; i++) {
        if (freed_nodes[i] == node) {
            // 该节点已经被释放过，不再重复释放
            return;
        }
    }
    
    // 记录即将释放的节点
    if (freed_count < 1024) {
        freed_nodes[freed_count++] = node;
    }
    
    // 记录子节点并设置为NULL以防止重复释放
    ASTNode **children = node->children;
    int child_count = node->child_count;
    
    // 立即清空子节点数组防止重复释放
    node->children = NULL;
    node->child_count = 0;
    node->child_capacity = 0;
    
    // 释放所有子节点
    for (int i = 0; i < child_count; i++) {
        if (children[i]) {
            free_ast(children[i]);
        }
    }
    
    // 释放子节点数组
    if (children) free(children);
    
    // 释放节点数据
    if (node->name) free(node->name);
    if (node->value.str_value) free(node->value.str_value);
    if (node->value.op) free(node->value.op);
    if (node->data_type) free(node->data_type);
    
    // 释放节点本身
    free(node);
}

// 获取节点类型的字符串表示
const char* get_node_type_str(ASTNodeType type) {
    switch (type) {
        case NODE_PROGRAM: return "Program";
        case NODE_COMP_UNIT: return "CompUnit";
        case NODE_CONST_DECL: return "ConstDecl";
        case NODE_CONST_DEF: return "ConstDef";
        case NODE_CONST_INIT_VAL: return "ConstInitVal";
        case NODE_VAR_DECL: return "VarDecl";
        case NODE_VAR_DEF: return "VarDef";
        case NODE_INIT_VAL: return "InitVal";
        case NODE_FUNC_DEF: return "FuncDef";
        case NODE_MAIN_FUNC_DEF: return "MainFuncDef";
        case NODE_FUNC_PARAM: return "FuncParam";
        case NODE_FUNC_CALL: return "FuncCall";
        case NODE_FUNC_PARAM_LIST: return "FuncParamList";
        case NODE_FUNC_RPARAM_LIST: return "FuncRParamList";
        case NODE_BLOCK: return "Block";
        case NODE_STMT: return "Stmt";
        case NODE_ASSIGN_STMT: return "AssignStmt";
        case NODE_IF_STMT: return "IfStmt";
        case NODE_WHILE_STMT: return "WhileStmt";
        case NODE_BREAK_STMT: return "BreakStmt";
        case NODE_CONTINUE_STMT: return "ContinueStmt";
        case NODE_RETURN_STMT: return "ReturnStmt";
        case NODE_EMPTY_STMT: return "EmptyStmt";
        case NODE_PRINTF_STMT: return "PrintfStmt";
        case NODE_EXP: return "Exp";
        case NODE_COND: return "Cond";
        case NODE_LVAL: return "LVal";
        case NODE_PRIMARY_EXP: return "PrimaryExp";
        case NODE_UNARY_EXP: return "UnaryExp";
        case NODE_BINARY_EXP: return "BinaryExp";
        case NODE_NUMBER: return "Number";
        case NODE_ARRAY_ACCESS: return "ArrayAccess";
        case NODE_PLUS: return "Plus";
        case NODE_MINUS: return "Minus";
        case NODE_MULTIPLY: return "Multiply";
        case NODE_DIVIDE: return "Divide";
        case NODE_MOD: return "Mod";
        case NODE_LESS: return "Less";
        case NODE_LESS_EQUAL: return "LessEqual";
        case NODE_GREATER: return "Greater";
        case NODE_GREATER_EQUAL: return "GreaterEqual";
        case NODE_EQUAL: return "Equal";
        case NODE_NOT_EQUAL: return "NotEqual";
        case NODE_AND: return "And";
        case NODE_OR: return "Or";
        case NODE_NOT: return "Not";
        case NODE_IDENT: return "Identifier";
        case NODE_INT_CONST: return "IntConst";
        case NODE_STRING_CONST: return "StringConst";
        case NODE_ARRAY_DIMS: return "ArrayDims";
        case NODE_LIST: return "List";
        default: return "Unknown";
    }
}

// 生成唯一的节点ID（用于dot输出）
int node_id_counter = 0;
int get_node_id() {
    return node_id_counter++;
}

// 添加一个辅助函数用于转义字符串
char* escape_string(const char* str) {
    if (!str) return NULL;
    
    size_t len = strlen(str);
    // 分配足够的内存以容纳可能的转义序列（每个字符最多6个字符：\uXXXX）
    char* escaped = (char*)malloc(len * 6 + 1);
    if (!escaped) return NULL;
    
    size_t j = 0;
    for (size_t i = 0; i < len; i++) {
        unsigned char c = (unsigned char)str[i];
        
        if (c == '\\') {
            // 转义反斜杠
            escaped[j++] = '\\';
            escaped[j++] = '\\';
        } else if (c == '"') {
            // 转义引号
            escaped[j++] = '\\';
            escaped[j++] = '"';
        } else if (c == '\n') {
            // 转义换行符
            escaped[j++] = '\\';
            escaped[j++] = 'n';
        } else if (c < 128 && isprint(c)) {
            // ASCII可打印字符，直接复制
            escaped[j++] = c;
        } else {
            // 非ASCII字符，使用Unicode转义序列
            sprintf(escaped + j, "\\u%04x", c);
            j += 6;
        }
    }
    escaped[j] = '\0';
    
    return escaped;
}

// 输出节点到dot文件
void print_node_dot(ASTNode *node, FILE *fp, int node_id) {
    if (!node || !fp) return;
    
    // 节点类型显示名称
    const char *type_str = get_node_type_str(node->type);
    
    // 节点标签
    fprintf(fp, "  node%d [label=\"", node_id);
    
    // 节点类型
    fprintf(fp, "%s", type_str);
    
    // 节点名称（如果有）
    if (node->name) {
        char* escaped_name = escape_string(node->name);
        fprintf(fp, "\\n%s", escaped_name);
        free(escaped_name);
    }
    
    // 节点值（根据节点类型添加不同值）
    switch (node->type) {
        case NODE_INT_CONST:
            fprintf(fp, "\\nValue: %d", node->value.int_value);
            break;
        case NODE_STRING_CONST:
        case NODE_IDENT:
            if (node->value.str_value) {
                char* escaped_value = escape_string(node->value.str_value);
                fprintf(fp, "\\nValue: %s", escaped_value);
                free(escaped_value);
            }
            break;
        case NODE_BINARY_EXP:
            if (node->value.op) {
                char* escaped_op = escape_string(node->value.op);
                fprintf(fp, "\\nOp: %s", escaped_op);
                free(escaped_op);
            }
            break;
        default:
            break;
    }
    
    fprintf(fp, "\"];\n");
    
    // 输出子节点并连接边
    for (int i = 0; i < node->child_count; i++) {
        int child_id = get_node_id();
        fprintf(fp, "  node%d -> node%d;\n", node_id, child_id);
        print_node_dot(node->children[i], fp, child_id);
    }
}

// 输出整个AST到dot文件
void print_ast_dot(ASTNode *root, FILE *fp) {
    if (!root || !fp) return;
    
    // dot文件头
    fprintf(fp, "digraph AST {\n");
    fprintf(fp, "  node [shape=box, style=filled, color=lightblue];\n");
    
    // 重置ID计数器
    node_id_counter = 0;
    
    // 输出根节点和所有子节点
    int root_id = get_node_id();
    print_node_dot(root, fp, root_id);
    
    // dot文件尾
    fprintf(fp, "}\n");
}

// 生成AST的dot文件
void generate_ast_dot(ASTNode *root, const char *filename) {
    if (!root || !filename) return;
    
    FILE *fp = fopen(filename, "w");
    if (!fp) {
        fprintf(stderr, "Error: Could not open file %s for writing\n", filename);
        return;
    }
    
    print_ast_dot(root, fp);
    fclose(fp);
    
    printf("AST written to %s\n", filename);
}

// 将源节点的所有子节点添加到目标节点，并安全释放源节点
void merge_node_list(ASTNode *dest, ASTNode *src) {
    if (!dest || !src) return;
    
    // 添加所有子节点到目标节点
    for (int i = 0; i < src->child_count; i++) {
        if (src->children[i]) {
            add_child(dest, src->children[i]);
        }
    }
    
    // 清空源节点的子节点引用以避免释放它们
    src->child_count = 0;
    
    // 释放源节点
    free_ast(src);
} 