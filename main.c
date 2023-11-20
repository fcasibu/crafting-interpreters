#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "expr.h"
#include "lex.h"
#include "token.h"

#define BUFFER_SIZE 1024

char *read_file(char *path);
void run_prompt();
void run(const char *source, char *file_name);

int main(int argc, char **argv) {
  if (argc > 2) {
    printf("Only need one argument");
    exit(1);
  }

  Expression *Expr = create_binary_expr(
      create_binary_expr(create_literal_int_expr(4),
                         create_token(STAR, "*", NULL, 1, 1),
                         create_literal_int_expr(6)),
      create_token(PLUS, "+", NULL, 1, 1),
      create_binary_expr(
          create_binary_expr(create_literal_double_expr(44.64),
                             create_token(SLASH, "/", NULL, 1, 1),
                             create_literal_int_expr(3)),
          create_token(PLUS, "+", NULL, 1, 1),
          create_binary_expr(create_literal_int_expr(9),
                             create_token(STAR, "*", NULL, 1, 1),
                             create_literal_int_expr(22))));

  print(Expr);
  char *file_name = argv[1];
  char *file = read_file(file_name);
  run(file, file_name);
  free(file);
  free_expr(Expr);
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
