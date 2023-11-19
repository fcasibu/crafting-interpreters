#include "lex.h"
#include "log.h"

static TokenType scan_token(Lexer *lexer, const char *source);
static TokenType get_ident_token_type(char *ident);
static Token create_token(TokenType type, char *lexeme, char *literal, int line,
                          int col);
static void add_token(Lexer *lexer, Token token);

static const char *format_token_type(TokenType token_type);
static char peek(Lexer *lexer, const char *source);
static int get_current_col(Token token);
static void read_char(Lexer *lexer, const char *source);
static void read_line(Lexer *lexer, const char *source);
static void read_string(Lexer *lexer, const char *source);
static void read_number(Lexer *lexer, const char *source);
static void read_ident(Lexer *lexer, const char *source);
static void *get_literal(TokenType type, void *value);
static char *get_text(Lexer *lexer, const char *source, int start,
                      size_t bytes_to_copy);
static bool match(Lexer *lexer, const char *source, char c);
static char *strip_quotes(char *string);
static bool is_at_end(Lexer *lex, const char *source);
static bool is_whitespace(const char ch);
static bool is_digit(const char ch);
static bool is_alpha(const char ch);
#if 0
static bool is_alphanum(const char ch);
#endif
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

void print_token(Token token, const char *file_name) {
  printf("%s:%d:%d: \n"
         "Token: %s\n"
         "Lexeme: %s\n"
         "Literal: %s\n",
         file_name, token.line, token.col, format_token_type(token.type),
         token.lexeme, (char *)token.literal);
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

  size_t bytes_to_copy = lexer->cursor - start - 1;
  char *text = get_text(lexer, source, start, bytes_to_copy);

  if (type == IDENT) {
    type = get_ident_token_type(text);
  }

  Token token = create_token(type, text, get_literal(type, text), lexer->line,
                             lexer->col);
  add_token(lexer, token);
  lexer->col += get_current_col(token);
}

static void read_string(Lexer *lexer, const char *source) {
  while (peek(lexer, source) != '"' && !is_at_end(lexer, source))
    read_char(lexer, source);

  if (is_at_end(lexer, source)) {
    error(lexer->file_name, lexer->line, lexer->col, "Unterminated string");
    exit(1);
  }

  read_char(lexer, source);
}

static void read_number(Lexer *lexer, const char *source) {
  while (is_digit(lexer->ch)) {
    read_char(lexer, source);
  }

  if (lexer->ch == '.') {
    read_char(lexer, source);

    while (is_digit(lexer->ch))
      read_char(lexer, source);
  }
}

static void read_ident(Lexer *lexer, const char *source) {
  while (is_alpha(lexer->ch)) {
    read_char(lexer, source);
  }
}

static void *get_literal(TokenType type, void *value) {
  switch (type) {
  case STRING: {
    return strip_quotes(value);
  }
  case NUMBER: {
    return value;
  }
  default: {
    return NULL;
  }
  }
}
static char *get_text(Lexer *lexer, const char *source, int start,
                      size_t bytes_to_copy) {
  char *text = (char *)malloc(sizeof(char) * bytes_to_copy + 1);

  if (text == NULL) {
    warning(lexer->file_name, lexer->line, lexer->col, "Allocation failed");
    return NULL;
  }

  strncpy(text, source + start, bytes_to_copy);
  text[strlen(text)] = '\0';

  return text;
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
      while (peek(lexer, source) != '\n') {
        read_char(lexer, source);
      }
      token_type = COMMENT;
    } else {
      token_type = SLASH;
    }
    break;
  case '"':
    read_string(lexer, source);
    token_type = STRING;
    break;
  default:
    if (is_digit(lexer->ch)) {
      read_number(lexer, source);
      token_type = NUMBER;
      return token_type;
    } else if (is_alpha(lexer->ch)) {
      read_ident(lexer, source);
      token_type = IDENT;
      return token_type;
    }

    error(lexer->file_name, lexer->line, lexer->col, "Unexpected character: ");
    printf("%c", lexer->ch);
    exit(1);
  }

  read_char(lexer, source);
  return token_type;
}

static TokenType get_ident_token_type(char *ident) {
  // TODO: use hashmap
  if (strcmp(ident, "and") == 0)
    return AND;
  if (strcmp(ident, "class") == 0)
    return CLASS;
  if (strcmp(ident, "else") == 0)
    return ELSE;
  if (strcmp(ident, "false") == 0)
    return FALSE;
  if (strcmp(ident, "fun") == 0)
    return FUN;
  if (strcmp(ident, "for") == 0)
    return FOR;
  if (strcmp(ident, "if") == 0)
    return IF;
  if (strcmp(ident, "nil") == 0)
    return NIL;
  if (strcmp(ident, "or") == 0)
    return OR;
  if (strcmp(ident, "print") == 0)
    return PRINT;
  if (strcmp(ident, "return") == 0)
    return RETURN;
  if (strcmp(ident, "super") == 0)
    return SUPER;
  if (strcmp(ident, "this") == 0)
    return THIS;
  if (strcmp(ident, "true") == 0)
    return TRUE;
  if (strcmp(ident, "var") == 0)
    return VAR;
  if (strcmp(ident, "while") == 0)
    return WHILE;

  return IDENT;
}

static void skip_whitespace(Lexer *lexer, const char *source) {
  while (is_whitespace(lexer->ch)) {
    lexer->col += 1;

    read_char(lexer, source);
  }
}

static char peek(Lexer *lexer, const char *source) {
  if (is_at_end(lexer, source))
    return '\0';

  return source[lexer->cursor];
}

static void read_char(Lexer *lexer, const char *source) {
  if (lexer->ch == '\n') {
    lexer->line += 1;
    lexer->col = 1;
  }

  lexer->ch = is_at_end(lexer, source) ? '\0' : source[lexer->cursor];
  lexer->cursor += 1;
}

static int get_current_col(Token token) {
  if (token.lexeme != NULL)
    return strlen(token.lexeme);

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

static char *strip_quotes(char *string) {
  char *result = (char *)malloc(sizeof(char) * strlen((char *)string) + 1);

  if (result == NULL)
    return NULL;

  size_t bytes_to_copy = strlen(string) - 2;
  strncpy(result, (char *)string + 1, bytes_to_copy);
  result[strlen(result)] = '\0';
  return result;
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

static bool is_at_end(Lexer *lex, const char *source) {
  return lex->cursor >= (int)strlen(source);
}

static bool is_whitespace(const char ch) {
  return ch == ' ' || ch == '\r' || ch == '\t' || ch == '\n';
}
static bool is_digit(const char ch) { return ch >= '0' && ch <= '9'; }

static bool is_alpha(const char ch) {
  return ((ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z')) || ch == '_';
}

#if 0
static bool is_alphanum(const char ch) { return is_alpha(ch) || is_digit(ch); }
#endif
