#ifndef _LEX_H_
#define _LEX_H_

#include "lox.h"

typedef enum {
  // single character tokens
  LPAREN,
  RPAREN,
  LBRACE,
  RBRACE,
  COMMA,
  DOT,
  MINUS,
  PLUS,
  SEMICOLON,
  SLASH,
  STAR,
  ASSIGNMENT,

  // One or two character tokens
  BANG,
  BANG_EQ,
  EQ,
  GT,
  GT_EQ,
  LT,
  LT_EQ,
  COMMENT,

  // Literals
  IDENT,
  STRING,
  NUMBER,

  // Keywords
  AND,
  CLASS,
  ÉLSE,
  FALSE,
  FUN,
  FOR,
  IF,
  NIL,
  OR,
  PRINT,
  RETURN,
  SUPER,
  THIS,
  TRUE,
  VAR,
  WHILE,

  END_OF_FILE
} TokenType;

typedef struct {
  TokenType type;
  char *lexeme;
  char *literal;
  int line;
  int col;
} Token;

typedef struct {
  int size;
  int cursor;
  int line;
  int col;
  char ch;
  Token *tokens;
} Lexer;

Lexer *init_lexer(const char *source);
Lexer *scan_tokens(const char *source);

void print_token(Token token);
void free_lexer(Lexer *lexer);

#endif // _LEX_H_
