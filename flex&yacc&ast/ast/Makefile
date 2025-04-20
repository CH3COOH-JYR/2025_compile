CC = gcc
CFLAGS = -Wall -g -Wno-implicit-function-declaration

# 尝试定位bison/yacc
BISON_PATHS = /opt/homebrew/opt/bison/bin/bison /usr/local/opt/bison/bin/bison /usr/bin/bison
YACC = bison -d -y

# 目标文件
TARGET = sysy_parser

all: $(TARGET)

# 一步完成编译
$(TARGET): sysy.l sysy.y ast.c ast.h
	@# 确保清理之前的生成文件
	rm -f lex.yy.c y.tab.c y.tab.h $(TARGET)
	
	@# 生成语法分析器
	$(YACC) sysy.y
	
	@# 生成词法分析器
	flex sysy.l
	
	@# 编译
	$(CC) $(CFLAGS) -o $(TARGET) lex.yy.c y.tab.c ast.c

# 测试语法分析器和AST生成
test: $(TARGET) factorial.sy
	./$(TARGET) factorial.sy output.txt ast.dot
	@echo "请运行: dot -Tpng ast.dot -o ast.png 生成图像"

# 创建测试目录和示例
test_dir:
	mkdir -p test/ast_example
	@echo '// 测试SysY程序 - 复杂版本\n\n// 常量声明\nconst int MAX = 100;\nconst int MIN = 1;\n\n// 全局变量声明\nint global_var = 42;\nint array[10];\n\n// 函数声明\nint add(int a, int b) {\n    return a + b;\n}\n\nint factorial(int n) {\n    if (n <= 1) {\n        return 1;\n    } else {\n        return n * factorial(n - 1);\n    }\n}\n\n// 计算数组元素和的函数\nint sum_array(int arr[], int size) {\n    int i = 0;\n    int sum = 0;\n    \n    while (i < size) {\n        sum = sum + arr[i];\n        i = i + 1;\n    }\n    \n    return sum;\n}\n\n// 主函数\nint main() {\n    // 局部变量声明\n    int i = 0;\n    int sum = 0;\n    int result = 0;\n    \n    // 初始化数组\n    while (i < 10) {\n        array[i] = i * i;\n        i = i + 1;\n    }\n    \n    // 条件语句\n    if (i == 10) {\n        printf("Array initialized");\n    } else {\n        printf("Initialization failed");\n    }\n    \n    // 函数调用\n    sum = sum_array(array, 10);\n    printf("Sum of squares: %d", sum);\n    \n    // 嵌套条件\n    if (sum > 200) {\n        if (sum > 300) {\n            printf("Sum is very large");\n        } else {\n            printf("Sum is large");\n        }\n    } else {\n        printf("Sum is normal");\n    }\n    \n    // 计算阶乘\n    result = factorial(5);\n    printf("Factorial of 5: %d", result);\n    \n    return 0;\n}' > test/ast_example/complex.sy
	@echo 'int main() {\n    return 0;\n}' > test/ast_example/simple.sy

# 创建简单的阶乘示例文件
factorial.sy:
	@echo '// Factorial function example\n\n// Function to calculate factorial recursively\nint factorial(int n) {\n    if (n <= 1) {\n        return 1;\n    } else {\n        return n * factorial(n - 1);\n    }\n}\n\n// Main function\nint main() {\n    int result;\n    \n    // Call factorial function\n    result = factorial(5);\n    \n    // Print result\n    printf("Factorial of 5: %d", result);\n    \n    return 0;\n}' > factorial.sy

# 测试目录中的文件
test_ast: $(TARGET) test_dir
	@echo "Testing simple example..."
	./$(TARGET) test/ast_example/simple.sy output_simple.txt ast_simple.dot
	dot -Tpng ast_simple.dot -o ast_simple.png || echo "注意：生成PNG图像需要安装graphviz"
	
	@echo "Testing complex example..."
	./$(TARGET) test/ast_example/complex.sy output_complex.txt ast_complex.dot
	dot -Tpng ast_complex.dot -o ast_complex.png || echo "注意：生成PNG图像需要安装graphviz"

# 清理生成的文件
clean:
	rm -f lex.yy.c y.tab.c y.tab.h $(TARGET) output.txt ast.dot ast.png
	rm -f output_simple.txt ast_simple.dot ast_simple.png
	rm -f output_complex.txt ast_complex.dot ast_complex.png

.PHONY: all test clean test_dir test_ast factorial.sy 