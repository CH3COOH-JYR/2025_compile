CC = gcc
CFLAGS = -Wall -g

# Flex生成的C文件
LEX_SOURCES = lex.yy.c
LEX_EXECS = sysy_lexer

all: $(LEX_EXECS)

# 生成词法分析器
$(LEX_SOURCES): sysy.l
	flex sysy.l

# 编译词法分析器
$(LEX_EXECS): $(LEX_SOURCES)
	$(CC) $(CFLAGS) -o $@ $<

# 测试词法分析器
test: $(LEX_EXECS)
	./$(LEX_EXECS) input.txt output.txt

# 清理生成的文件
clean:
	rm -f $(LEX_SOURCES) $(LEX_EXECS) output.txt

.PHONY: all test clean 