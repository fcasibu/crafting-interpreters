#ifndef _EXPR_H_
#define _EXPR_H_

#include "lex.h"

typedef enum {
  INT_TYPE,
  DOUBLE_TYPE,
  STRING_TYPE,
  BOOLEAN_TYPE,
  NULL_TYPE
} LiteralType;

typedef struct {
  LiteralType type;
  union {
    int int_val;
    double double_val;
    const char *string_val;
    bool boolean_val;
    const void *null_val;
  } value;
} LiteralValue;

typedef struct {
  Token op;
  struct Expression *right;
} UnaryExpr;

typedef struct {
  struct Expression *left;
  Token op;
  struct Expression *right;
} BinaryExpr;

typedef struct {
  struct Expression *expr;
} GroupingExpr;

typedef struct {
  LiteralValue literal;
} LiteralExpr;

typedef struct {
  UnaryExpr *Unary;
  BinaryExpr *Binary;
  GroupingExpr *Grouping;
  LiteralExpr *Literal;
} ExpressionType;

typedef struct Expression {
  ExpressionType *Type;
} Expression;

void print(const Expression *expr);

void free_expr(Expression *expr);
Expression *create_unary_expr(Token op, Expression *right);
Expression *create_binary_expr(Expression *left, Token op, Expression *right);
Expression *create_grouping_expr(Expression *expr);
Expression *create_literal_int_expr(int value);
Expression *create_literal_double_expr(double value);
Expression *create_literal_string_expr(const char *value);
Expression *create_literal_boolean_expr(bool value);
Expression *create_literal_null_expr();

#endif // _EXPR_H_
