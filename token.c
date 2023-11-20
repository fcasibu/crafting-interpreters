#include <stdio.h>

#include "token.h"

Token create_token(TokenType type, char *lexeme, char *literal, int line,
                   int col) {
  return (Token){.type = type,
                 .lexeme = lexeme,
                 .literal = literal,
                 .line = line,
                 .col = col};
}

void print_token(Token token, const char *file_name) {
  printf("%s:%d:%d: \n"
         "Token: %s\n"
         "Lexeme: %s\n"
         "Literal: %s\n",
         file_name, token.line, token.col, format_token_type(token.type),
         token.lexeme, (char *)token.literal);
}

const char *format_token_type(TokenType token_type) {
  switch (token_type) {
  // single character tokens
  case LPAREN:
    return "LPAREN";
  case RPAREN:
    return "RPAREN";
  case LBRACE:
    return "LBRACE";
  case RBRACE:
    return "RBRACE";
  case COMMA:
    return "COMMA";
  case DOT:
    return "DOT";
  case MINUS:
    return "MINUS";
  case PLUS:
    return "PLUS";
  case SEMICOLON:
    return "SEMICOLON";
  case SLASH:
    return "SLASH";
  case STAR:
    return "STAR";
  case ASSIGNMENT:
    return "ASSIGNMENT";

  // One or two character tokens
  case BANG:
    return "BANG";
  case BANG_EQ:
    return "BANG_EQ";
  case EQ:
    return "EQ";
  case GT:
    return "GT";
  case GT_EQ:
    return "GT_EQ";
  case LT:
    return "LT";
  case LT_EQ:
    return "LT_EQ";
  case COMMENT:
    return "COMMENT";

  // Literals
  case IDENT:
    return "IDENT";
  case STRING:
    return "STRING";
  case NUMBER:
    return "NUMBER";

  // Keywords
  case AND:
    return "AND";
  case CLASS:
    return "CLASS";
  case ELSE:
    return "ELSE";
  case FALSE:
    return "FALSE";
  case FUN:
    return "FUN";
  case FOR:
    return "FOR";
  case IF:
    return "IF";
  case NIL:
    return "NIL";
  case OR:
    return "OR";
  case PRINT:
    return "PRINT";
  case RETURN:
    return "RETURN";
  case SUPER:
    return "SUPER";
  case THIS:
    return "THIS";
  case TRUE:
    return "TRUE";
  case VAR:
    return "VAR";
  case WHILE:
    return "WHILE";

  case END_OF_FILE:
    return "END_OF_FILE";
  }
}
