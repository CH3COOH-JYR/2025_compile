CC = gcc
CFLAGS = -Wall -g -Wno-implicit-function-declaration

# 尝试定位bison/yacc
BISON_PATHS = /opt/homebrew/opt/bison/bin/bison /usr/local/opt/bison/bin/bison /usr/bin/bison
YACC = bison -d -y

# 目标文件
TARGET = sysy_parser

all: $(TARGET)

# 一步完成编译
$(TARGET): sysy.l sysy.y
	@# 确保清理之前的生成文件
	rm -f lex.yy.c y.tab.c y.tab.h $(TARGET)
	
	@# 生成语法分析器
	$(YACC) sysy.y
	
	@# 生成词法分析器
	flex sysy.l
	
	@# 编译
	$(CC) $(CFLAGS) -o $(TARGET) lex.yy.c y.tab.c

# 测试语法分析器
test: $(TARGET)
	./$(TARGET) input.txt output.txt

# 创建测试文件
input.txt:
	@echo '// 测试SysY程序\n\n// 常量声明\nconst int MAX = 100;\nconst int MIN = 0;\n\n// 变量声明\nint global_var = 42;\n\n// 主函数\nint main() {\n    // 局部变量声明\n    int i = 0;\n    int sum = 0;\n    \n    // 循环语句\n    while (i < 10) {\n        sum = sum + i;\n        i = i + 1;\n    }\n    \n    // 条件语句\n    if (i == 10) {\n        printf("Loop completed");\n    } else {\n        printf("Loop failed");\n    }\n    \n    return 0;\n}' > input.txt

# 清理生成的文件
clean:
	rm -f lex.yy.c y.tab.c y.tab.h $(TARGET) output.txt

.PHONY: all test clean input.txt 