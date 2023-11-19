#include "lex.h"

static TokenType scan_token(Lexer *lexer, const char *source);
static Token create_token(TokenType type, char *lexeme, char *literal, int line,
                          int col);
static void add_token(Lexer *lexer, Token token);

static const char *format_token_type(TokenType token_type);
static char peek(Lexer *lexer, const char *source);
static int get_current_col(Token token);
static void read_char(Lexer *lexer, const char *source);
static void read_line(Lexer *lexer, const char *source);
static bool match(Lexer *lexer, const char *source, char c);
static bool is_at_end(Lexer *lex, const char *source);
static bool is_whitespace(const char ch);
static void skip_whitespace(Lexer *lexer, const char *source);

Lexer *scan_tokens(const char *source, const char *file_name) {
  Lexer *lex = init_lexer(source, file_name);
  if (lex == NULL)
    return lex;

  while (!is_at_end(lex, source)) {
    read_line(lex, source);
  }

  add_token(lex, create_token(END_OF_FILE, NULL, NULL, lex->line, lex->col));

  return lex;
}

void print_token(Token token) {
  printf("main.lox:%d:%d: Type: %s\n"
         "               Lexeme: %s\n"
         "               Literal: %s\n",
         token.line, token.col, format_token_type(token.type), token.lexeme,
         token.literal);
}

void free_lexer(Lexer *lexer) {
  if (lexer == NULL)
    return;

  if (lexer->tokens == NULL)
    return;

  for (int i = 0; i < lexer->size; ++i) {
    if (lexer->tokens[i].lexeme != NULL) {
      free(lexer->tokens[i].lexeme);
      lexer->tokens[i].lexeme = NULL;
    }
  }

  free(lexer->tokens);
  lexer->tokens = NULL;
  free(lexer);
}

Lexer *init_lexer(const char *source, const char *file_name) {
  Lexer *lexer = (Lexer *)malloc(sizeof(Lexer));
  lexer->tokens = (Token *)malloc(sizeof(Token) * lexer->size);
  lexer->size = 0;
  lexer->cursor = 0;
  lexer->line = 1;
  lexer->col = 1;
  lexer->file_name = file_name;

  if (lexer->tokens == NULL)
    return NULL;

  read_char(lexer, source);

  return lexer;
}

static void read_line(Lexer *lexer, const char *source) {
  skip_whitespace(lexer, source);

  int start = lexer->cursor - 1;
  TokenType type = scan_token(lexer, source);

  int bytes_to_copy = lexer->cursor - start - 1;
  char *text = (char *)malloc(sizeof(char) * bytes_to_copy + 1);

  if (text == NULL) {
    error(lexer->file_name, lexer->line, lexer->col,
          "Something went wrong with malloc");
    return;
  }

  strncpy(text, source + start, bytes_to_copy);
  text[strlen(text)] = '\0';

  Token token = create_token(type, text, NULL, lexer->line, lexer->col);
  add_token(lexer, token);
  lexer->col += get_current_col(token);
}

static TokenType scan_token(Lexer *lexer, const char *source) {
  TokenType token_type;

  switch (lexer->ch) {
  case '(':
    token_type = LPAREN;
    break;
  case ')':
    token_type = RPAREN;
    break;
  case '{':
    token_type = LBRACE;
    break;
  case '}':
    token_type = RBRACE;
    break;
  case ',':
    token_type = COMMA;
    break;
  case '.':
    token_type = DOT;
    break;
  case '-':
    token_type = MINUS;
    break;
  case '+':
    token_type = PLUS;
    break;
  case ';':
    token_type = SEMICOLON;
    break;
  case '*':
    token_type = STAR;
    break;
  case '!':
    token_type = match(lexer, source, '=') ? BANG_EQ : BANG;
    break;
  case '=':
    token_type = match(lexer, source, '=') ? EQ : ASSIGNMENT;
    break;
  case '<':
    token_type = match(lexer, source, '=') ? LT_EQ : LT;
    break;
  case '>':
    token_type = match(lexer, source, '=') ? GT_EQ : GT;
    break;
  case '/':
    if (match(lexer, source, '/')) {
      while (peek(lexer, source) != '\n' && !is_at_end(lexer, source))
        read_char(lexer, source);
      token_type = COMMENT;
    } else {
      token_type = SLASH;
    }
    break;
  default:
    error(lexer->file_name, lexer->line, lexer->col, "Unexpected character: ");
    printf("%c", lexer->ch);
    exit(1);
    break;
  }

  read_char(lexer, source);
  return token_type;
}

static void skip_whitespace(Lexer *lexer, const char *source) {
  while (is_whitespace(lexer->ch)) {
    lexer->col += 1;

    if (lexer->ch == '\n') {
      lexer->line += 1;
      lexer->col = 1;
    }

    read_char(lexer, source);
  }
}

static char peek(Lexer *lexer, const char *source) {
  if (is_at_end(lexer, source))
    return '\0';

  return source[lexer->cursor];
}

static void read_char(Lexer *lexer, const char *source) {
  lexer->ch = is_at_end(lexer, source) ? '\0' : source[lexer->cursor];
  lexer->cursor += 1;
}

static int get_current_col(Token token) {
  if (token.lexeme != NULL)
    return token.type == COMMENT ? 1 : strlen(token.lexeme);

  return 0;
}

static void add_token(Lexer *lexer, Token token) {
  lexer->tokens =
      (Token *)realloc(lexer->tokens, sizeof(Token) * ++lexer->size);

  if (lexer->tokens == NULL)
    return;

  lexer->tokens[lexer->size - 1] = token;
}

static bool match(Lexer *lexer, const char *source, char c) {
  if (is_at_end(lexer, source) || peek(lexer, source) != c)
    return false;

  lexer->cursor += 1;
  return true;
}

static Token create_token(TokenType type, char *lexeme, char *literal, int line,
                          int col) {
  return (Token){.type = type,
                 .lexeme = lexeme,
                 .literal = literal,
                 .line = line,
                 .col = col};
}

static const char *format_token_type(TokenType token_type) {
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
  case ÉLSE:
    return "ÉLSE";
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

static bool is_at_end(Lexer *lex, const char *source) {
  return source[lex->cursor] == '\0' || lex->cursor - 1 >= (int)strlen(source);
}

static bool is_whitespace(const char ch) {
  return ch == ' ' || ch == '\r' || ch == '\t' || ch == '\n';
}
