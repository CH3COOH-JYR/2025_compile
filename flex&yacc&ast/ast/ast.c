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

// 为节点确定颜色（改进视觉效果）
const char* get_node_color(ASTNodeType type) {
    switch (type) {
        case NODE_COMP_UNIT:
            return "lightblue";
        case NODE_MAIN_FUNC_DEF:
            return "salmon";
        case NODE_FUNC_DEF:
            return "salmon";
        case NODE_BLOCK:
            return "lightgrey";
        case NODE_STMT:
            return "skyblue";
        case NODE_ASSIGN_STMT:
            return "coral";
        case NODE_VAR_DECL:
            return "palegreen";
        case NODE_CONST_DECL:
            return "khaki";
        case NODE_RETURN_STMT:
            return "skyblue";
        case NODE_IF_STMT:
        case NODE_WHILE_STMT:
            return "skyblue";
        default:
            return "white";
    }
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

// 获取节点边的标签
char* get_edge_label(ASTNode *parent, int child_index) {
    static char buffer[64]; // 静态缓冲区
    
    // 根据父节点类型和子节点索引确定边标签
    switch(parent->type) {
        case NODE_COMP_UNIT:
            if (child_index == 0) return "decl";
            else if (parent->children[child_index]->type == NODE_MAIN_FUNC_DEF) 
                return "main_func_def";
            else return "func_def";
        
        case NODE_FUNC_DEF:
        case NODE_MAIN_FUNC_DEF:
            if (child_index == 0) return "return_type";
            else if (child_index == 1) return "name";
            else if (parent->children[child_index]->type == NODE_BLOCK) 
                return "block";
            else return "params";
            
        case NODE_BLOCK:
            sprintf(buffer, "item%d", child_index);
            return buffer;
            
        case NODE_STMT:
            if (child_index == 0) {
                if (parent->children[0]->type == NODE_LVAL) return "assignment";
                else return "expression";
            }
            else return "value";
            
        case NODE_ASSIGN_STMT:
            if (child_index == 0) return "assignment";
            else if (child_index == 1) {
                if (parent->children[1]->type == NODE_FUNC_CALL && 
                    parent->children[1]->children[0]->value.str_value &&
                    strcmp(parent->children[1]->children[0]->value.str_value, "getint") == 0)
                    return "is_getint: true";
                else 
                    return "value";
            }
            
        case NODE_VAR_DECL:
        case NODE_CONST_DECL:
            if (child_index == 0) return "decl";
            
        case NODE_VAR_DEF:
        case NODE_CONST_DEF:
            if (child_index == 0) return "ident";
            else if (child_index == 1) {
                if (parent->children[1]->type == NODE_ARRAY_DIMS)
                    return "dims";
                else
                    return "value";
            }
            
        case NODE_LVAL:
            return "ident";
            
        case NODE_BINARY_EXP:
            if (child_index == 0) return "left";
            else return "right";
            
        case NODE_RETURN_STMT:
            return "value";
            
        default:
            return "";
    }
}

// 输出节点到dot文件
void print_node_dot(ASTNode *node, FILE *fp, int node_id) {
    if (!node || !fp) return;
    
    // 节点类型显示名称
    const char *type_str = get_node_type_str(node->type);
    const char *node_color = get_node_color(node->type);
    
    // 节点标签
    fprintf(fp, "  node%d [label=\"", node_id);
    
    // 节点类型（主要标签）
    fprintf(fp, "%s", type_str);
    
    // 添加更多有价值的信息，类似于示例图片
    switch (node->type) {
        case NODE_MAIN_FUNC_DEF:
            fprintf(fp, "\\nint main()");
            break;
        case NODE_VAR_DECL:
            if (node->children && node->children[0] && 
                node->children[0]->children && node->children[0]->children[0]) {
                ASTNode *ident = node->children[0]->children[0];
                if (ident->type == NODE_IDENT && ident->value.str_value) {
                    fprintf(fp, "\\nident: %s\\ntype: int", ident->value.str_value);
                }
            }
            break;
        case NODE_CONST_DECL:
            if (node->children && node->children[0] && 
                node->children[0]->children && node->children[0]->children[0]) {
                ASTNode *ident = node->children[0]->children[0];
                if (ident->type == NODE_IDENT && ident->value.str_value) {
                    // 如果有常量值，获取它
                    if (node->children[0]->children[1] && 
                        node->children[0]->children[1]->children && 
                        node->children[0]->children[1]->children[0] &&
                        node->children[0]->children[1]->children[0]->type == NODE_INT_CONST) {
                        int value = node->children[0]->children[1]->children[0]->value.int_value;
                        fprintf(fp, "\\nident: %s\\ntype: int\\nvalue: %d", 
                                ident->value.str_value, value);
                    } else {
                        fprintf(fp, "\\nident: %s\\ntype: int", ident->value.str_value);
                    }
                }
            }
            break;
        case NODE_ASSIGN_STMT:
            fprintf(fp, "\\nkind: ASSIGNMENT");
            break;
        case NODE_RETURN_STMT:
            fprintf(fp, "\\nkind: RETURN");
            if (node->children && node->children[0] && 
                node->children[0]->type == NODE_LVAL && 
                node->children[0]->children[0]->value.str_value) {
                fprintf(fp, "\\nident: %s", node->children[0]->children[0]->value.str_value);
            }
            break;
        case NODE_INT_CONST:
            fprintf(fp, "\\nValue: %d", node->value.int_value);
            break;
        case NODE_IDENT:
            if (node->value.str_value) {
                char* escaped_value = escape_string(node->value.str_value);
                fprintf(fp, "\\nIdent: %s", escaped_value);
                free(escaped_value);
            }
            break;
        case NODE_FUNC_CALL:
            if (node->children && node->children[0] && 
                node->children[0]->type == NODE_IDENT && 
                node->children[0]->value.str_value) {
                fprintf(fp, "\\nFunction: %s", node->children[0]->value.str_value);
            }
            break;
        default:
            break;
    }
    
    fprintf(fp, "\", style=filled, color=%s];\n", node_color);
    
    // 输出子节点并连接边
    for (int i = 0; i < node->child_count; i++) {
        if (!node->children[i]) continue;
        
        int child_id = get_node_id();
        
        // 获取边标签
        char *edge_label = get_edge_label(node, i);
        
        // 添加带标签的边
        if (edge_label && strlen(edge_label) > 0) {
            fprintf(fp, "  node%d -> node%d [label=\"%s\"];\n", node_id, child_id, edge_label);
        } else {
            fprintf(fp, "  node%d -> node%d;\n", node_id, child_id);
        }
        
        print_node_dot(node->children[i], fp, child_id);
    }
}

// 输出整个AST到dot文件
void print_ast_dot(ASTNode *root, FILE *fp) {
    if (!root || !fp) return;
    
    // dot文件头
    fprintf(fp, "digraph AST {\n");
    fprintf(fp, "  node [shape=box, style=filled];\n");
    fprintf(fp, "  edge [fontsize=10];\n");
    
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