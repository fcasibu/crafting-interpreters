#ifndef _PARSER_H_
#define _PARSER_H_

#include "token.h"

typedef struct {
  const char *file_name;
  Token *tokens;
  int current;
} Parser;

Parser *create_parser(const char *file_name, Token *tokens);
void free_parser(Parser *parser);

Expression *parse(Parser *parser);

#endif // _PARSER_H_
