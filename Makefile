CC = gcc
CFLAGS = -Wall -Wextra -std=c2x -pedantic
SRC_FILES = main.c lex.c log.c token.c expr.c utils.c
TARGET = lox

$(TARGET): $(SRC_FILES)
	$(CC) $(CFLAGS) -o $@ $^

run: $(TARGET)
	./$(TARGET) $(file)

clean:
	rm -f $(TARGET)

