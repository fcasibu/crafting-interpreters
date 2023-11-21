#ifndef _LEX_H_
#define _LEX_H_

#include "token.h"

typedef struct {
  int size;
  int cursor;
  int line;
  int col;
  char ch;
  const char *file_name;
  Token *tokens;
} Lexer;

Lexer *create_lexer(const char *source, const char *file_name);
Lexer *scan_tokens(const char *source, const char *file_name);

void free_lexer(Lexer *lexer);

#endif // _LEX_H_
