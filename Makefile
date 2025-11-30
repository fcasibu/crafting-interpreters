CC = clang
CFLAGS = -std=c2x -Wall -Wextra -Wpedantic
INCLUDE_LIBS = -I./include
SRC = main.c
TARGET = lox
TEST_RUNNER = ./tests/run-tests.js

$(TARGET): $(SRC)
	$(CC) $(CFLAGS) $(INCLUDE_LIBS) -o $@ $^

test:

all: $(TARGET) $(TEST_RUNNER)
	@chmod +x $(TEST_RUNNER)
	@$(TEST_RUNNER) all

parser: $(TARGET) $(TEST_RUNNER)
	@chmod +x $(TEST_RUNNER)
	@$(TEST_RUNNER) parser

eval: $(TARGET) $(TEST_RUNNER)
	@chmod +x $(TEST_RUNNER)
	@$(TEST_RUNNER) eval

clean:
	rm -f $(TARGET)

.PHONY: test parser eval all clean

