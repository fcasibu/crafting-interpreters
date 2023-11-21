#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "expr.h"
#include "log.h"
#include "parser.h"
#include "token.h"

static Expression *expression(Parser *parser);
static Expression *equality(Parser *parser);
static Expression *comparison(Parser *parser);
static Expression *term(Parser *parser);
static Expression *factor(Parser *parser);
static Expression *unary(Parser *parser);
static Expression *primary(Parser *parser);

#if 0
static void synchronize(Parser *parser);
#endif

static Token previous(const Parser *parser);
static Token peek(const Parser *parser);
static Token consume(Parser *parser, Token token, const char *message);
static Token advance(Parser *parser);
static bool match_token_types(Parser *parser, int arg_count, ...);
static bool check(const Parser *parser, TokenType type);
static bool is_at_end(const Parser *parser);

Parser *create_parser(const char *file_name, Token *tokens) {
  Parser *parser = (Parser *)malloc(sizeof(Parser));

  if (parser == NULL)
    return NULL;

  parser->tokens = tokens;
  parser->current = 0;
  parser->file_name = file_name;

  return parser;
}

void free_parser(Parser *parser) {
  if (parser == NULL)
    return;

  free(parser);
}

Expression *parse(Parser *parser) { return expression(parser); }

static Expression *expression(Parser *parser) { return equality(parser); }

static Expression *equality(Parser *parser) {
  Expression *expr = comparison(parser);

  if (expr == NULL)
    return NULL;

  while (match_token_types(parser, 2, BANG_EQ, EQ)) {
    Token operator= previous(parser);
    Expression *right = comparison(parser);

    if (right == NULL)
      break;

    expr = create_binary_expr(expr, operator, right);
  }

  return expr;
}

static Expression *comparison(Parser *parser) {
  Expression *expr = term(parser);

  if (expr == NULL)
    return NULL;

  while (match_token_types(parser, 4, GT, GT_EQ, LT, LT_EQ)) {
    Token operator= previous(parser);
    Expression *right = term(parser);

    if (right == NULL)
      break;

    expr = create_binary_expr(expr, operator, right);
  }

  return expr;
}

static Expression *term(Parser *parser) {
  Expression *expr = factor(parser);

  if (expr == NULL)
    return NULL;

  while (match_token_types(parser, 2, MINUS, PLUS)) {
    Token operator= previous(parser);
    Expression *right = factor(parser);

    if (right == NULL)
      break;

    expr = create_binary_expr(expr, operator, right);
  }

  return expr;
}

static Expression *factor(Parser *parser) {
  Expression *expr = unary(parser);

  if (expr == NULL)
    return NULL;

  while (match_token_types(parser, 2, SLASH, STAR)) {
    Token operator= previous(parser);
    Expression *right = unary(parser);

    if (right == NULL)
      break;

    expr = create_binary_expr(expr, operator, right);
  }

  return expr;
}

static Expression *unary(Parser *parser) {
  if (match_token_types(parser, 2, BANG, MINUS)) {
    Token operator= previous(parser);
    Expression *right = unary(parser);

    if (right == NULL)
      return NULL;

    return create_unary_expr(operator, right);
  }

  return primary(parser);
}

static Expression *primary(Parser *parser) {
  if (match_token_types(parser, 1, FALSE))
    return create_literal_boolean_expr(false);
  if (match_token_types(parser, 1, TRUE))
    return create_literal_boolean_expr(true);
  if (match_token_types(parser, 1, NIL))
    return create_literal_null_expr();
  if (match_token_types(parser, 1, NUMBER)) {
    // TODO: double literal (?)
    return create_literal_int_expr(atoi(previous(parser).lexeme));
  }
  if (match_token_types(parser, 1, STRING)) {
    return create_literal_string_expr(previous(parser).literal);
  }

  if (match_token_types(parser, 1, LPAREN)) {
    Expression *expr = expression(parser);
    consume(parser, peek(parser), "Expect ')' after expression.");
    return create_grouping_expr(expr);
  }

  const Token token = peek(parser);
  error(parser->file_name, token.line, token.col, "Expect expression.");
  exit(1);
}

static bool match_token_types(Parser *parser, int arg_count, ...) {
  va_list args;

  va_start(args, arg_count);

  for (int i = 0; i < arg_count; ++i) {
    const TokenType type = va_arg(args, TokenType);

    if (check(parser, type)) {
      advance(parser);

      va_end(args);
      return true;
    }
  }

  va_end(args);

  return false;
}

#if 0
static void synchronize(Parser *parser) {
  advance(parser);

  while (!is_at_end(parser)) {
    switch (peek(parser).type) {
    case CLASS:
    case FUN:
    case VAR:
    case FOR:
    case IF:
    case WHILE:
    case PRINT:
    case RETURN:
      return;

    default:
      advance(parser);
    }
  }
}
#endif

static Token previous(const Parser *parser) {
  return parser->tokens[parser->current - 1];
}

static Token peek(const Parser *parser) {
  return parser->tokens[parser->current];
}

static Token consume(Parser *parser, Token token, const char *message) {
  if (check(parser, token.type)) {
    return advance(parser);
  }

  error(parser->file_name, token.line, token.col, message);
  exit(1);
}

static bool check(const Parser *parser, TokenType type) {
  if (is_at_end(parser))
    return false;

  return peek(parser).type == type;
}

static Token advance(Parser *parser) {
  if (!is_at_end(parser))
    parser->current += 1;

  return previous(parser);
}

static bool is_at_end(const Parser *parser) {
  return peek(parser).type == END_OF_FILE;
}
