CC = gcc
CFLAGS = -Wall -Wextra -std=c2x -pedantic
SRC_FILES = main.c lex.c lox.c
TARGET = lox

$(TARGET): $(SRC_FILES)
	$(CC) $(CFLAGS) -o $@ $^

run: $(TARGET)
	./$(TARGET)

clean:
	rm -f $(TARGET)

