#ifndef _TOKEN_H_
#define _TOKEN_H_

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
  AND, CLASS, ELSE, FALSE, FUN, FOR, IF,
  NIL, OR, PRINT, RETURN, SUPER, THIS,
  TRUE, VAR, WHILE,

  END_OF_FILE
} TokenType;

typedef struct {
  int       line;
  int       col;
  char      *lexeme;
  void      *literal;
  TokenType type;
} Token;
// clang-format on

Token create_token(TokenType type, char *lexeme, char *literal, int line,
                   int col);
const char *format_token_type(TokenType token_type);
void print_token(Token token, const char *file_name);

#endif // _TOKEN_H_
