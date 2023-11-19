#include "lex.h"

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define BUFFER_SIZE 1024

char *read_file(char *path);
void run_prompt();
void run(const char *source, char *file_name);

int main(int argc, char **argv) {
  char *file_name = argv[1];
  char *file = read_file(file_name);
  run(file, file_name);
  free(file);
  // run("(){},.-+;/*=\n"
  //     "!!===>>=<<=\n"
  //     "// This is a comment");
  // run_prompt();
  return 0;
}

void run_prompt() {
  for (;;) {
    printf("> ");
    char *line = (char *)malloc(BUFFER_SIZE);
    scanf("%s", line);

    printf("Line: %s\n", line);
    run(line, "");
    free(line);
  }
}

char *read_file(char *path) {
  FILE *ptr;

  if ((ptr = fopen(path, "r")) == NULL) {
    printf("Error opening file %s\n", path);
    exit(1);
  }

  char *buffer = (char *)malloc(2048);
  char ch;

  int i = 0;
  while ((ch = fgetc(ptr)) != EOF) {
    buffer[i++] = ch;
  }
  fclose(ptr);
  buffer[strlen(buffer)] = '\0';

  return buffer;
}

void run(const char *source, char *file_name) {
  Lexer *lexer = scan_tokens(source, file_name);

  for (int i = 0; i < lexer->size; ++i) {
    print_token(lexer->tokens[i], file_name);
  }

  free_lexer(lexer);
}
