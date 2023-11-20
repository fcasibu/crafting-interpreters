#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "expr.h"

static char *parenthesize(char *name, int expr_count, ...);

static char *visit_unary_expr(Expression *Expr);
static char *visit_binary_expr(Expression *Expr);
static char *visit_grouping_expr(Expression *Expr);
static LiteralValue visit_literal_expr(Expression *Expr);

static void free_unary_expr(Expression *Expr);
static void free_binary_expr(Expression *Expr);
static void free_grouping_expr(Expression *Expr);
static void free_literal_expr(Expression *Expr);

static Expression *create_expr();
static Expression *create_literal_expr();

Expression *create_expr() {
  Expression *Expr = (Expression *)malloc(sizeof(Expression));

  if (Expr == NULL)
    return NULL;

  Expr->Type = (ExpressionType *)malloc(sizeof(ExpressionType));

  if (Expr->Type == NULL)
    return NULL;

  Expr->Type->Unary = NULL;
  Expr->Type->Binary = NULL;
  Expr->Type->Grouping = NULL;
  Expr->Type->Literal = NULL;

  return Expr;
}

void free_expr(Expression *Expr) {
  if (Expr == NULL)
    return;

  if (Expr->Type == NULL) {
    free(Expr);
    return;
  }

  free_unary_expr(Expr);
  free_binary_expr(Expr);
  free_grouping_expr(Expr);
  free_literal_expr(Expr);

  free(Expr->Type);
  free(Expr);
}

void free_unary_expr(Expression *Expr) {
  if (Expr->Type->Unary == NULL)
    return;

  free_expr(Expr->Type->Unary->right);
  free(Expr->Type->Unary);
}

void free_binary_expr(Expression *Expr) {
  if (Expr->Type->Binary == NULL)
    return;

  free_expr(Expr->Type->Binary->left);
  free_expr(Expr->Type->Binary->right);

  free(Expr->Type->Binary);
}

void free_grouping_expr(Expression *Expr) {
  if (Expr->Type->Grouping == NULL)
    return;

  free_expr(Expr->Type->Grouping->expr);

  free(Expr->Type->Grouping);
}

void free_literal_expr(Expression *Expr) {
  if (Expr->Type->Literal == NULL)
    return;

  free(Expr->Type->Literal);
}

Expression *create_unary_expr(Token op, Expression *right) {
  Expression *Expr = create_expr();

  if (Expr == NULL)
    return NULL;

  Expr->Type->Unary = (UnaryExpr *)malloc(sizeof(UnaryExpr));

  if (Expr->Type->Unary == NULL)
    return NULL;

  Expr->Type->Unary->op = op;
  Expr->Type->Unary->right = right;

  return Expr;
}

Expression *create_binary_expr(Expression *left, Token op, Expression *right) {
  Expression *Expr = create_expr();

  if (Expr == NULL)
    return NULL;

  Expr->Type->Binary = (BinaryExpr *)malloc(sizeof(BinaryExpr));

  if (Expr->Type->Binary == NULL)
    return NULL;

  Expr->Type->Binary->left = left;
  Expr->Type->Binary->op = op;
  Expr->Type->Binary->right = right;

  return Expr;
}

Expression *create_grouping_expr(Expression *expr_value) {
  Expression *Expr = create_expr();

  if (Expr == NULL)
    return NULL;

  Expr->Type->Grouping = (GroupingExpr *)malloc(sizeof(GroupingExpr));

  if (Expr->Type->Grouping == NULL)
    return NULL;

  Expr->Type->Grouping->expr = expr_value;

  return Expr;
}

static Expression *create_literal_expr() {
  Expression *Expr = create_expr();

  if (Expr == NULL)
    return NULL;

  Expr->Type->Literal = (LiteralExpr *)malloc(sizeof(LiteralExpr));

  if (Expr->Type->Literal == NULL)
    return NULL;

  return Expr;
}

Expression *create_literal_int_expr(int value) {
  Expression *Expr = create_literal_expr();

  if (Expr == NULL)
    return NULL;

  Expr->Type->Literal->literal.type = INT_TYPE;
  Expr->Type->Literal->literal.value.int_val = value;

  return Expr;
}
Expression *create_literal_double_expr(double value) {
  Expression *Expr = create_literal_expr();

  if (Expr == NULL)
    return NULL;

  Expr->Type->Literal->literal.type = DOUBLE_TYPE;
  Expr->Type->Literal->literal.value.double_val = value;

  return Expr;
}
Expression *create_literal_string_expr(char *value) {
  Expression *Expr = create_literal_expr();

  if (Expr == NULL)
    return NULL;

  Expr->Type->Literal->literal.type = STRING_TYPE;
  Expr->Type->Literal->literal.value.string_val = value;

  return Expr;
}

char *visit_unary_expr(Expression *Expr) {
  return parenthesize(Expr->Type->Unary->op.lexeme, 1,
                      Expr->Type->Unary->right);
}

char *visit_binary_expr(Expression *Expr) {
  return parenthesize(Expr->Type->Binary->op.lexeme, 2,
                      Expr->Type->Binary->left, Expr->Type->Binary->right);
}

char *visit_grouping_expr(Expression *Expr) {
  return parenthesize("group", 1, Expr->Type->Grouping->expr);
}

LiteralValue visit_literal_expr(Expression *Expr) {
  return Expr->Type->Literal->literal;
}

// TODO: Visitor pattern in c? (how lol)
void print(Expression *Expr) {
  if (Expr->Type->Unary != NULL) {
    printf("Result: %s\n", visit_unary_expr(Expr));
  } else if (Expr->Type->Binary != NULL) {
    printf("Result: %s\n", visit_binary_expr(Expr));
  } else if (Expr->Type->Grouping != NULL) {
    printf("Result: %s\n", visit_grouping_expr(Expr));
  } else if (Expr->Type->Literal != NULL) {
    LiteralValue lv = visit_literal_expr(Expr);
    switch (lv.type) {
    case INT_TYPE:
      printf("Result: %d\n", lv.value.int_val);
      break;
    case DOUBLE_TYPE:
      printf("Result: %f\n", lv.value.double_val);
    case STRING_TYPE:
      printf("Result: %s\n", lv.value.string_val);
      break;
    case NULL_TYPE:
      printf("Result: %s\n", (char *)lv.value.null_val);
      break;
    }
  }
}

#define BUFFER_SIZE 2048

char *parenthesize(char *name, int expr_count, ...) {
  // TODO: create a better string builder
  char *result = (char *)malloc(BUFFER_SIZE);
  if (result == NULL) {
    return NULL;
  }

  va_list args;
  va_start(args, expr_count);

  size_t index = 0;
  result[index++] = '(';

  snprintf(result + index, BUFFER_SIZE, "%s", name);
  index += strlen(result + index);

  for (int i = 0; i < expr_count; ++i) {
    Expression *Expr = va_arg(args, Expression *);
    result[index++] = ' ';

    if (Expr->Type->Unary != NULL) {
      snprintf(result + index, BUFFER_SIZE, "%s", visit_unary_expr(Expr));
    } else if (Expr->Type->Binary != NULL) {
      snprintf(result + index, BUFFER_SIZE, "%s", visit_binary_expr(Expr));
    } else if (Expr->Type->Grouping != NULL) {
      snprintf(result + index, BUFFER_SIZE, "%s", visit_grouping_expr(Expr));
    } else if (Expr->Type->Literal != NULL) {
      // TODO: there will be a lot of switch cases with this union, is there a
      // better way :thinking:
      LiteralValue lv = visit_literal_expr(Expr);
      switch (lv.type) {
      case INT_TYPE:
        snprintf(result + index, BUFFER_SIZE, "%d", lv.value.int_val);
        break;
      case DOUBLE_TYPE:
        snprintf(result + index, BUFFER_SIZE, "%f", lv.value.double_val);
        break;
      case STRING_TYPE:
        snprintf(result + index, BUFFER_SIZE, "%s", lv.value.string_val);
        break;
      case NULL_TYPE:
        snprintf(result + index, BUFFER_SIZE, "%s", (char *)lv.value.null_val);
        break;
      }
    }
    index += strlen(result + index);
  }

  va_end(args);
  result[index++] = ')';
  result[index] = '\0';

  return result;
}
