CC = clang
CFLAGS = -std=c2x -Wall -Wextra -Wpedantic
INCLUDE_LIBS = -I./include
SRC = main.c
TARGET = lox

$(TARGET): $(SRC)
	$(CC) $(CFLAGS) $(INCLUDE_LIBS) -o $@ $^

clean:
	rm -f $(TARGET)

