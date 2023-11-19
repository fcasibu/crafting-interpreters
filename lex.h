#ifndef _LEX_H_
#define _LEX_H_

#include "lox.h"

// clang-format off
typedef enum {
  // single character tokens
  LPAREN, RPAREN, LBRACE, RBRACE, COMMA,
  DOT, MINUS, PLUS, SEMICOLON, SLASH,
  STAR, ASSIGNMENT,

  // One or two character tokens
  BANG, BANG_EQ, EQ, GT, GT_EQ, LT,
  LT_EQ, COMMENT,
  // Literals
  IDENT, STRING, NUMBER,

  // Keywords
  AND, CLASS, ÉLSE, FALSE, FUN, FOR, IF,
  NIL, OR, PRINT, RETURN, SUPER, THIS,
  TRUE, VAR, WHILE,

  END_OF_FILE
} TokenType;

typedef struct {
  int       line;
  int       col;
  char      *lexeme;
  char      *literal;
  TokenType type;
} Token;

typedef struct {
  int   size;
  int   cursor;
  int   line;
  int   col;
  char  ch;
  const char *file_name;
  Token *tokens;
} Lexer;
// clang-format on

Lexer *init_lexer(const char *source, const char *file_name);
Lexer *scan_tokens(const char *source, const char *file_name);

void print_token(Token token);
void free_lexer(Lexer *lexer);

#endif // _LEX_H_
