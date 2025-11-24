#include <stdarg.h>

#define ARENA_IMPLEMENTATION
#include "arena.h"
#define FILE_IMPLEMENTATION
#include "file.h"
#define UTILS_IMPLEMENTATION
#include "utils.h"
#include "logger.h"
#include "type_defs.h"

#define ARENA_SIZE 1024
#define LANG_NAME "lox"
#define LANG_NAME_LEN 3

// clang-format off
typedef enum {
  // Single-character tokens.
  LEFT_PAREN, RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE,
  COMMA, DOT, MINUS, PLUS, SEMICOLON, SLASH, STAR,
  QUESTION_MARK, COLON,

  // One or two character tokens.
  BANG, BANG_EQUAL,
  EQUAL, EQUAL_EQUAL,
  GREATER, GREATER_EQUAL,
  LESS, LESS_EQUAL,

  // Literals.
  IDENTIFIER, STRING, NUMBER,

  // Keywords.
  AND, CLASS, ELSE, FALSE, FUN, FOR, IF, NIL, OR,
  PRINT, RETURN, SUPER, THIS, TRUE, VAR, WHILE,

  END_OF_FILE
} token_type_t;
// clang-format on

typedef enum {
    // clang-format off
    NODE_NUMBER, NODE_STRING, NODE_BOOL, NODE_NIL, NODE_UNARY, NODE_BINARY,
    NODE_PROGRAM, NODE_IDENTIFIER, NODE_TERNARY
    // clang-format on
} node_type_t;

typedef struct {
    token_type_t type;
    const char *lexeme;
    usize lexeme_len;
    usize line;
    usize col;
    usize cursor;
    usize line_start;
} token_t;

typedef struct {
    token_t *items;
    usize size;
    usize capacity;
} tokens_t;

typedef struct {
    usize line_idx;
    usize col_idx;
    usize line_start;
    usize cursor;
    tokens_t tokens;
} lexer_t;

typedef union {
    struct {
        const char *string;
        double number;
        bool boolean;
    } literal;

    struct {
        const char *name;
    } identifier;

    struct {
        token_type_t op;
        struct ast_node *child;
    } unary;

    struct {
        token_type_t op;
        struct ast_node *left;
        struct ast_node *right;
    } binary;

    struct {
        struct ast_node *condition;
        struct ast_node *true_branch;
        struct ast_node *false_branch;
    } ternary;

    struct {
        struct ast_node **statements;
        usize size;
    } program;
} node_value_t;

typedef struct ast_node {
    node_type_t type;
    node_value_t value;
    usize start, end;
} ast_node_t;

typedef struct {
    ast_node_t **items;
    usize size;
    usize capacity;
} ast_nodes_t;

typedef struct {
    token_t *tokens;
    usize tokens_size;

    ast_node_t *root;
    usize current_index;

    usize start;
    usize end;
} parser_t;

static const struct {
    const char *identifier;
    token_type_t type;
} reserved_words_table[] = {
    // clang-format off
    {"and", AND},
    {"class", CLASS},
    {"else", ELSE},
    {"false", FALSE},
    {"fun", FUN},
    {"for", FOR},
    {"if", IF},
    {"nil", NIL},
    {"or", OR},
    {"print", PRINT},
    {"return", RETURN},
    {"super", SUPER},
    {"this", THIS},
    {"true", TRUE},
    {"var", VAR},
    {"while", WHILE},
    // clang-format on
};

typedef struct string_pool_entry {
    const char *str;
    struct string_pool_entry *next;
} string_pool_entry_t;

typedef struct {
    struct string_pool_entry *head;
} string_pool_t;

typedef struct {
    arena_t *arena;
    string_pool_t string_pool;
    const char *source;
    const char *source_filename;
    usize source_len;
} context_t;

void report(usize line_idx, usize col_idx, const char *source_filename, const char *source,
            usize line_start, const char *message);
void report_unexpected_token(context_t *ctx, token_t tok, const char *expected);
void run(context_t *ctx);
void tokenize(context_t *ctx, lexer_t *lexer);
void parse(context_t *ctx, parser_t *parser);
ast_node_t *parse_statement(context_t *ctx, parser_t *parser);
ast_node_t *parse_expression(context_t *ctx, parser_t *parser);
ast_node_t *parse_ternary(context_t *ctx, parser_t *parser);
ast_node_t *parse_logical_or(context_t *ctx, parser_t *parser);
ast_node_t *parse_logical_and(context_t *ctx, parser_t *parser);
ast_node_t *parse_equality(context_t *ctx, parser_t *parser);
ast_node_t *parse_comparison(context_t *ctx, parser_t *parser);
ast_node_t *parse_term(context_t *ctx, parser_t *parser);
ast_node_t *parse_factor(context_t *ctx, parser_t *parser);
ast_node_t *parse_unary(context_t *ctx, parser_t *parser);
ast_node_t *parse_primary(context_t *ctx, parser_t *parser);
ast_node_t *parse_number(context_t *ctx, parser_t *parser);
ast_node_t *parse_string(context_t *ctx, parser_t *parser);
ast_node_t *parse_boolean(context_t *ctx, parser_t *parser);
ast_node_t *parse_identifier(context_t *ctx, parser_t *parser);
void append_token(context_t *ctx, lexer_t *lexer, token_type_t type, const char *lexeme,
                  usize lexeme_len, usize line_idx, usize col_idx, usize cursor, usize line_start);
bool match(context_t *ctx, lexer_t *lexer, char ch);
char advance(context_t *ctx, lexer_t *lexer);
char peek(context_t *ctx, lexer_t *lexer);
char peek_next(context_t *ctx, lexer_t *lexer);
void string(context_t *ctx, lexer_t *lexer);
void number(context_t *ctx, lexer_t *lexer);
void identifier(context_t *ctx, lexer_t *lexer);
void comment_block(context_t *ctx, lexer_t *lexer);
bool is_at_end(context_t *ctx, lexer_t *lexer);
token_type_t lookup_keyword(const char *s);
bool is_alphanum(char ch);
ast_node_t *create_node(arena_t *arena, node_type_t type, node_value_t value);
bool parser_is_eof(parser_t *parser);
token_t peek_parser(parser_t *parser);
token_t peek_next_parser(parser_t *parser);
token_t previous_parser(parser_t *parser);
token_t advance_parser(parser_t *parser);
bool match_parser(parser_t *parser, usize count, ...);
void synchronize(parser_t *parser);
const char *intern(context_t *ctx, const char *s);
const char *intern_token_lexeme(context_t *ctx, parser_t *parser);

static bool had_error = false;

static void print_indent(int indent)
{
    for (int i = 0; i < indent; i++)
        printf("  ");
}

static void print_token_type(token_type_t type)
{
    switch (type) {
    case PLUS:
        printf("+");
        break;
    case MINUS:
        printf("-");
        break;
    case STAR:
        printf("*");
        break;
    case SLASH:
        printf("/");
        break;
    case LESS:
        printf("<");
        break;
    case LESS_EQUAL:
        printf("<=");
        break;
    case EQUAL:
        printf("=");
        break;
    case EQUAL_EQUAL:
        printf("==");
        break;
    case GREATER:
        printf(">");
        break;
    case GREATER_EQUAL:
        printf(">=");
        break;
    case COMMA:
        printf(",");
        break;
    case QUESTION_MARK:
        printf("?");
        break;
    case COLON:
        printf(":");
        break;
    case OR:
        printf("or");
        break;
    default:
        printf("tok(%d)", type);
        break;
    }
}

void print_ast(ast_node_t *node, int indent)

{
    if (!node)
        return;

    print_indent(indent);

    switch (node->type) {
    case NODE_NUMBER:
        printf("Number: %g (start %zu, end %zu)\n", node->value.literal.number, node->start,
               node->end);
        break;

    case NODE_STRING:
        printf("String: \"%s\" (start %zu, end %zu)\n", node->value.literal.string, node->start,
               node->end);
        break;

    case NODE_BOOL:
        printf("Boolean: %s (start %zu, end %zu)\n", node->value.literal.boolean ? "true" : "false",
               node->start, node->end);
        break;

    case NODE_NIL:
        printf("Nil (start %zu, end %zu)\n", node->start, node->end);
        break;

    case NODE_IDENTIFIER:
        printf("Identifier: %s (start %zu, end %zu)\n", node->value.identifier.name, node->start,
               node->end);
        break;

    case NODE_UNARY:
        printf("Unary: ");
        print_token_type(node->value.unary.op);
        printf(" (start %zu, end %zu)\n", node->start, node->end);
        print_ast(node->value.unary.child, indent + 1);
        break;

    case NODE_BINARY:
        printf("Binary: ");
        print_token_type(node->value.binary.op);
        printf(" (start %zu, end %zu)\n", node->start, node->end);
        print_ast(node->value.binary.left, indent + 1);
        print_ast(node->value.binary.right, indent + 1);
        break;

    case NODE_TERNARY:
        printf("Ternary \n");
        printf("%*sCondition:\n", (indent + 1) * 2, "");
        print_ast(node->value.ternary.condition, indent + 2);

        printf("%*sTrue branch:\n", (indent + 1) * 2, "");
        print_ast(node->value.ternary.true_branch, indent + 2);

        printf("%*sFalse branch:\n", (indent + 1) * 2, "");
        print_ast(node->value.ternary.false_branch, indent + 2);
        break;

    case NODE_PROGRAM:
        printf("Program (start %zu, end %zu)\n", node->start, node->end);
        for (usize i = 0; i < node->value.program.size; i++)
            print_ast(node->value.program.statements[i], indent + 1);
        break;

    default:
        printf("Unknown node type %d (start %zu, end %zu)\n", node->type, node->start, node->end);
        break;
    }
}

int main(int argc, char **argv)
{
    if (argc < 2) {
        LOG(LOG_ERROR, "%s", "No input files");
        return 1;
    }

    usize file_name_len = strlen(argv[1]);
    if (file_name_len < LANG_NAME_LEN ||
        strcmp(argv[1] + file_name_len - LANG_NAME_LEN, LANG_NAME) != 0) {
        LOG(LOG_ERROR, "Invalid source file '%s'", argv[1]);
        return 1;
    }

    arena_t arena = { 0 };
    if (!arena_create(&arena, ARENA_SIZE)) {
        LOG(LOG_ERROR, "%s", "Failed to create arena");
        return 1;
    }

    const char *source_filename = argv[1];

    const char *source = get_input(&arena, source_filename);

    if (!source) {
        LOG(LOG_ERROR, "Failed to open file '%s'", source_filename);
        return 1;
    }

    string_pool_t pool = { 0 };

    context_t ctx = {
        .source_filename = source_filename,
        .source = source,
        .arena = &arena,
        .source_len = strlen(source),
        .string_pool = pool,
    };

    run(&ctx);

    if (had_error)
        return 1;

    return 0;
}

void run(context_t *ctx)
{
    lexer_t lexer = { 0 };
    arena_da_init(ctx->arena, &lexer.tokens);

    tokenize(ctx, &lexer);

    for (usize i = 0; i < lexer.tokens.size; ++i) {
        token_t tok = lexer.tokens.items[i];
        LOG(LOG_INFO, "TOKEN = %.*s", (int)tok.lexeme_len, tok.lexeme);
    }

    parser_t parser = { 0 };
    parser.tokens = lexer.tokens.items;
    parser.tokens_size = lexer.tokens.size;
    parse(ctx, &parser);

    if (had_error)
        return;

    print_ast(parser.root, 0);
}

void parse(context_t *ctx, parser_t *parser)
{
    ast_nodes_t prog_stmts = { 0 };
    arena_da_init(ctx->arena, &prog_stmts);

    while (parser->current_index < parser->tokens_size && !parser_is_eof(parser)) {
        ast_node_t *node = parse_statement(ctx, parser);

        if (!node) {
            continue;
        }

        arena_da_append(ctx->arena, &prog_stmts, node);
    }

    parser->root = create_node(ctx->arena, NODE_PROGRAM,
                               (node_value_t){ .program.statements = prog_stmts.items,
                                               .program.size = prog_stmts.size });
}

ast_node_t *parse_statement(context_t *ctx, parser_t *parser)
{
    token_t tok = peek_parser(parser);

    switch (tok.type) {
    default: {
        ast_node_t *left = parse_expression(ctx, parser);
        if (!left) {
            synchronize(parser);
            return NULL;
        }

        while (!parser_is_eof(parser) && match_parser(parser, 1, COMMA)) {
            token_t op = advance_parser(parser);

            ast_node_t *right = parse_expression(ctx, parser);

            if (!right) {
                synchronize(parser);
                return NULL;
            }

            node_value_t node_value = {
                .binary = { .op = op.type, .left = left, .right = right },
            };
            left = create_node(ctx->arena, NODE_BINARY, node_value);
        }

        return left;
    }
    }
}

ast_node_t *parse_expression(context_t *ctx, parser_t *parser)
{
    return parse_ternary(ctx, parser);
}

ast_node_t *parse_ternary(context_t *ctx, parser_t *parser)
{
    ast_node_t *condition = parse_logical_or(ctx, parser);
    if (!condition) {
        report_unexpected_token(ctx, peek_parser(parser), "expression");
        synchronize(parser);
        return NULL;
    }

    if (!match_parser(parser, 1, QUESTION_MARK))
        return condition;

    advance_parser(parser);

    ast_node_t *true_branch = parse_ternary(ctx, parser);
    if (!true_branch) {
        report_unexpected_token(ctx, peek_parser(parser), "expression");
        synchronize(parser);
        return NULL;
    }

    token_t colon = advance_parser(parser);
    if (colon.type != COLON) {
        report_unexpected_token(ctx, colon, ":");
        return NULL;
    }

    ast_node_t *false_branch = parse_ternary(ctx, parser);
    if (!false_branch) {
        synchronize(parser);
        return NULL;
    }

    node_value_t value = { .ternary = { .condition = condition,
                                        .true_branch = true_branch,
                                        .false_branch = false_branch } };

    return create_node(ctx->arena, NODE_TERNARY, value);
}

ast_node_t *parse_logical_or(context_t *ctx, parser_t *parser)
{
    ast_node_t *left = parse_logical_and(ctx, parser);

    while (!parser_is_eof(parser) && match_parser(parser, 1, OR)) {
        token_t op = advance_parser(parser);

        ast_node_t *right = parse_logical_and(ctx, parser);

        if (!right)
            return NULL;

        node_value_t value = { .binary = { .left = left, .op = op.type, .right = right } };
        left = create_node(ctx->arena, NODE_BINARY, value);
    }

    return left;
}

ast_node_t *parse_logical_and(context_t *ctx, parser_t *parser)
{
    ast_node_t *left = parse_equality(ctx, parser);

    while (!parser_is_eof(parser) && match_parser(parser, 1, AND)) {
        token_t op = advance_parser(parser);

        ast_node_t *right = parse_equality(ctx, parser);

        if (!right)
            return NULL;

        node_value_t value = { .binary = { .left = left, .op = op.type, .right = right } };
        left = create_node(ctx->arena, NODE_BINARY, value);
    }

    return left;
}

ast_node_t *parse_equality(context_t *ctx, parser_t *parser)
{
    ast_node_t *left = parse_comparison(ctx, parser);

    while (!parser_is_eof(parser) && match_parser(parser, 2, EQUAL_EQUAL, BANG_EQUAL)) {
        token_t op = advance_parser(parser);

        ast_node_t *right = parse_comparison(ctx, parser);

        if (!right)
            return NULL;

        node_value_t value = { .binary = { .left = left, .op = op.type, .right = right } };
        left = create_node(ctx->arena, NODE_BINARY, value);
    }

    return left;
}

ast_node_t *parse_comparison(context_t *ctx, parser_t *parser)
{
    ast_node_t *left = parse_term(ctx, parser);

    while (!parser_is_eof(parser) &&
           match_parser(parser, 4, LESS, LESS_EQUAL, GREATER, GREATER_EQUAL)) {
        token_t op = advance_parser(parser);

        ast_node_t *right = parse_term(ctx, parser);

        if (!right)
            return NULL;

        node_value_t value = { .binary = { .left = left, .op = op.type, .right = right } };
        left = create_node(ctx->arena, NODE_BINARY, value);
    }

    return left;
}
ast_node_t *parse_term(context_t *ctx, parser_t *parser)
{
    ast_node_t *left = parse_factor(ctx, parser);

    while (!parser_is_eof(parser) && match_parser(parser, 2, PLUS, MINUS)) {
        token_t op = advance_parser(parser);

        ast_node_t *right = parse_factor(ctx, parser);

        if (!right)
            return NULL;

        node_value_t value = { .binary = { .left = left, .op = op.type, .right = right } };
        left = create_node(ctx->arena, NODE_BINARY, value);
    }

    return left;
}
ast_node_t *parse_factor(context_t *ctx, parser_t *parser)
{
    ast_node_t *left = parse_unary(ctx, parser);

    while (!parser_is_eof(parser) && match_parser(parser, 2, STAR, SLASH)) {
        token_t op = advance_parser(parser);

        ast_node_t *right = parse_unary(ctx, parser);

        if (!right)
            return NULL;

        node_value_t value = { .binary = { .left = left, .op = op.type, .right = right } };
        left = create_node(ctx->arena, NODE_BINARY, value);
    }

    return left;
}

ast_node_t *parse_unary(context_t *ctx, parser_t *parser)
{
    token_t tok = peek_parser(parser);

    switch (tok.type) {
    case PLUS: {
        advance_parser(parser);
        ast_node_t *node = parse_primary(ctx, parser);

        node_value_t value = { .unary = { .op = tok.type, .child = node } };
        return create_node(ctx->arena, NODE_UNARY, value);
    };

    case MINUS: {
        advance_parser(parser);
        ast_node_t *node = parse_primary(ctx, parser);

        node_value_t value = { .unary = { .op = tok.type, .child = node } };
        return create_node(ctx->arena, NODE_UNARY, value);
    };

    default:
        break;
    }

    return parse_primary(ctx, parser);
}
ast_node_t *parse_primary(context_t *ctx, parser_t *parser)
{
    token_t tok = peek_parser(parser);

    switch (tok.type) {
    case NUMBER: {
        ast_node_t *node = parse_number(ctx, parser);
        advance_parser(parser);

        return node;
    }

    case STRING: {
        ast_node_t *node = parse_string(ctx, parser);
        advance_parser(parser);

        return node;
    }

    case TRUE:
    case FALSE: {
        ast_node_t *node = parse_boolean(ctx, parser);
        advance_parser(parser);

        return node;
    }

    case NIL: {
        advance_parser(parser);

        return create_node(ctx->arena, NODE_NIL, (node_value_t){});
    }

    case IDENTIFIER: {
        ast_node_t *node = parse_identifier(ctx, parser);
        advance_parser(parser);

        return node;
    }

    case LEFT_PAREN: {
        advance_parser(parser);
        ast_node_t *node = parse_expression(ctx, parser);
        token_t next_tok = advance_parser(parser);
        if (next_tok.type != RIGHT_PAREN) {
            report_unexpected_token(ctx, next_tok, ")");
            return NULL;
        }

        return node;
    }

    default: {
        report_unexpected_token(ctx, tok, "expression");
        return NULL;
    }
    }
}

ast_node_t *parse_number(context_t *ctx, parser_t *parser)
{
    token_t tok = peek_parser(parser);
    if (tok.type != NUMBER) {
        report_unexpected_token(ctx, tok, "number");
        return NULL;
    }

    char tmp[tok.lexeme_len + 1];
    memcpy(tmp, tok.lexeme, tok.lexeme_len);
    tmp[tok.lexeme_len] = '\0';

    errno = 0;
    char *end = NULL;
    double value = strtod(tmp, &end);

    if (errno == ERANGE || *end != '\0') {
        report(tok.line - 1, tok.col - 1, ctx->source_filename, ctx->source, tok.line_start,
               "Numeric conversion failed: value out of range or extra characters after number.");

        return NULL;
    }

    node_value_t node_value = { .literal = { .number = value } };
    return create_node(ctx->arena, NODE_NUMBER, node_value);
}
ast_node_t *parse_string(context_t *ctx, parser_t *parser)
{
    token_t tok = peek_parser(parser);
    if (tok.type != STRING) {
        report_unexpected_token(ctx, tok, "string");
        return NULL;
    }

    char tmp[tok.lexeme_len];
    // remove quotes
    memcpy(tmp, tok.lexeme + 1, tok.lexeme_len - 1);
    tmp[tok.lexeme_len] = '\0';

    const char *value = intern(ctx, tmp);
    if (!value)
        return NULL;

    node_value_t node_value = { .literal = { .string = value } };
    return create_node(ctx->arena, NODE_STRING, node_value);
}

ast_node_t *parse_boolean(context_t *ctx, parser_t *parser)
{
    token_t tok = peek_parser(parser);
    if (tok.type != TRUE && tok.type != FALSE) {
        report_unexpected_token(ctx, tok, "boolean");
        return NULL;
    }

    char tmp[tok.lexeme_len + 1];
    memcpy(tmp, tok.lexeme, tok.lexeme_len);
    tmp[tok.lexeme_len] = '\0';

    node_value_t node_value = { .literal = { .boolean = strcmp(tmp, "true") == 0 } };
    return create_node(ctx->arena, NODE_BOOL, node_value);
}
ast_node_t *parse_identifier(context_t *ctx, parser_t *parser)
{
    token_t tok = peek_parser(parser);
    if (tok.type != IDENTIFIER) {
        report_unexpected_token(ctx, tok, "identifier");
        return NULL;
    }

    char tmp[tok.lexeme_len + 1];
    memcpy(tmp, tok.lexeme, tok.lexeme_len);
    tmp[tok.lexeme_len] = '\0';

    const char *value = intern(ctx, tmp);
    if (!value)
        return NULL;

    node_value_t node_value = { .identifier = { .name = value } };
    return create_node(ctx->arena, NODE_IDENTIFIER, node_value);
}

void tokenize(context_t *ctx, lexer_t *lexer)
{
    usize source_len = strlen(ctx->source);

    while (lexer->cursor < source_len) {
        usize line_start = lexer->line_start;
        usize col_idx = lexer->col_idx;
        usize line_idx = lexer->line_idx;
        usize cursor = lexer->cursor;
        const char *lexeme = ctx->source + lexer->cursor;

        char ch = advance(ctx, lexer);

        if (isspace(ch))
            continue;

        switch (ch) {
        case '(': {
            append_token(ctx, lexer, LEFT_PAREN, lexeme, 1, line_idx, col_idx, cursor, line_start);
        } break;

        case ')': {
            append_token(ctx, lexer, RIGHT_PAREN, lexeme, 1, line_idx, col_idx, cursor, line_start);
        } break;

        case '{': {
            append_token(ctx, lexer, LEFT_BRACE, lexeme, 1, line_idx, col_idx, cursor, line_start);
        } break;

        case '}': {
            append_token(ctx, lexer, RIGHT_BRACE, lexeme, 1, line_idx, col_idx, cursor, line_start);
        } break;

        case ',': {
            append_token(ctx, lexer, COMMA, lexeme, 1, line_idx, col_idx, cursor, line_start);
        } break;

        case '.': {
            append_token(ctx, lexer, DOT, lexeme, 1, line_idx, col_idx, cursor, line_start);
        } break;

        case '-': {
            append_token(ctx, lexer, MINUS, lexeme, 1, line_idx, col_idx, cursor, line_start);
        } break;

        case '+': {
            append_token(ctx, lexer, PLUS, lexeme, 1, line_idx, col_idx, cursor, line_start);
        } break;

        case ';': {
            append_token(ctx, lexer, SEMICOLON, lexeme, 1, line_idx, col_idx, cursor, line_start);
        } break;

        case '*': {
            append_token(ctx, lexer, STAR, lexeme, 1, line_idx, col_idx, cursor, line_start);
        } break;

        case '?': {
            append_token(ctx, lexer, QUESTION_MARK, lexeme, 1, line_idx, col_idx, cursor,
                         line_start);
        } break;

        case ':': {
            append_token(ctx, lexer, COLON, lexeme, 1, line_idx, col_idx, cursor, line_start);
        } break;

        case '!': {
            bool is_match = match(ctx, lexer, '=');
            append_token(ctx, lexer, is_match ? BANG_EQUAL : BANG, lexeme, is_match ? 2 : 1,
                         line_idx, col_idx, cursor, line_start);
        } break;

        case '=': {
            bool is_match = match(ctx, lexer, '=');
            append_token(ctx, lexer, is_match ? EQUAL_EQUAL : EQUAL, lexeme, is_match ? 2 : 1,
                         line_idx, col_idx, cursor, line_start);
        } break;

        case '>': {
            bool is_match = match(ctx, lexer, '=');
            append_token(ctx, lexer, is_match ? GREATER_EQUAL : GREATER, lexeme, is_match ? 2 : 1,
                         line_idx, col_idx, cursor, line_start);
        } break;

        case '<': {
            bool is_match = match(ctx, lexer, '=');
            append_token(ctx, lexer, is_match ? LESS_EQUAL : LESS, lexeme, is_match ? 2 : 1,
                         line_idx, col_idx, cursor, line_start);
        } break;

        case '/': {
            if (match(ctx, lexer, '/')) {
                while (peek(ctx, lexer) != '\n' && !is_at_end(ctx, lexer))
                    advance(ctx, lexer);
            } else if (match(ctx, lexer, '*')) {
                comment_block(ctx, lexer);
            } else {
                append_token(ctx, lexer, SLASH, lexeme, 1, line_idx, col_idx, cursor, line_start);
            }
        } break;

        case '"': {
            string(ctx, lexer);
        } break;

        default: {
            if (isdigit(ch)) {
                number(ctx, lexer);
            } else if (is_alphanum(ch)) {
                identifier(ctx, lexer);
            } else {
                report(line_idx, col_idx, ctx->source_filename, ctx->source, line_start,
                       "Unexpected character.");
            }
        } break;
        }
    }

    append_token(ctx, lexer, END_OF_FILE, "", 0, lexer->line_idx, 0, lexer->cursor, lexer->cursor);
}

void string(context_t *ctx, lexer_t *lexer)
{
    usize line_start = lexer->line_start;
    usize start = lexer->cursor - 1;
    usize start_col_idx = lexer->col_idx - 1; // we advanced once
    usize start_line_idx = lexer->line_idx;

    while (!is_at_end(ctx, lexer) && peek(ctx, lexer) != '"')
        advance(ctx, lexer);

    usize string_len = lexer->cursor - start;

    if (is_at_end(ctx, lexer)) {
        report(start_line_idx, start_col_idx + string_len, ctx->source_filename, ctx->source,
               line_start, "Unterminated string.");
        return;
    }

    assert(peek(ctx, lexer) == '"');
    advance(ctx, lexer);

    append_token(ctx, lexer, STRING, ctx->source + start, string_len, start_line_idx, start_col_idx,
                 start, line_start);
}

void number(context_t *ctx, lexer_t *lexer)
{
    usize line_start = lexer->line_start;
    usize start = lexer->cursor - 1; // we advanced once
    usize start_col_idx = lexer->col_idx - 1; // we advanced once
    usize start_line_idx = lexer->line_idx;

    while (isdigit(peek(ctx, lexer)))
        advance(ctx, lexer);

    if (peek(ctx, lexer) == '.') {
        advance(ctx, lexer);

        usize string_len = lexer->cursor - start;

        if (!isdigit(peek(ctx, lexer))) {
            report(start_line_idx, start_col_idx + string_len, ctx->source_filename, ctx->source,
                   line_start, "Invalid numeric literal: expected digits after '.'.");
            return;
        }

        while (!is_at_end(ctx, lexer) && isdigit(peek(ctx, lexer)))
            advance(ctx, lexer);
    }

    usize string_len = lexer->cursor - start;

    if (is_alphanum(peek(ctx, lexer))) {
        report(start_line_idx, start_col_idx + string_len, ctx->source_filename, ctx->source,
               line_start, "Invalid numeric literal.");
        return;
    }

    append_token(ctx, lexer, NUMBER, ctx->source + start, string_len, start_line_idx, start_col_idx,
                 start, line_start);
}

void identifier(context_t *ctx, lexer_t *lexer)
{
    usize line_start = lexer->line_start;
    usize start = lexer->cursor - 1; // we advanced once
    usize start_col_idx = lexer->col_idx - 1; // we advanced once
    usize start_line_idx = lexer->line_idx;

    while (is_alphanum(peek(ctx, lexer)))
        advance(ctx, lexer);

    usize string_len = lexer->cursor - start;
    char tmp[string_len + 1];
    memcpy(tmp, ctx->source + start, string_len);
    tmp[string_len] = '\0';

    token_type_t type = lookup_keyword(tmp);

    append_token(ctx, lexer, type, ctx->source + start, string_len, start_line_idx, start_col_idx,
                 start, line_start);
}

void comment_block(context_t *ctx, lexer_t *lexer)
{
    usize line_start = lexer->line_start;
    usize start_col_idx = lexer->col_idx - 2; // we advanced twice
    usize start_line_idx = lexer->line_idx;

    while (!is_at_end(ctx, lexer)) {
        if (peek(ctx, lexer) == '*' && peek_next(ctx, lexer) == '/')
            break;

        advance(ctx, lexer);
    }

    if ((peek(ctx, lexer) != '*' && peek_next(ctx, lexer) != '/')) {
        report(start_line_idx, start_col_idx, ctx->source_filename, ctx->source, line_start,
               "Unterminated comment.");
        return;
    }

    advance(ctx, lexer);
    advance(ctx, lexer);
}

char advance(context_t *ctx, lexer_t *lexer)
{
    if (lexer->cursor >= ctx->source_len)
        return '\0';

    char ch = ctx->source[lexer->cursor++];

    if (ch == '\n') {
        lexer->line_start = lexer->cursor;
        lexer->line_idx += 1;
        lexer->col_idx = 0;
    } else {
        lexer->col_idx += 1;
    }

    return ch;
}

char peek(context_t *ctx, lexer_t *lexer)
{
    if (lexer->cursor >= ctx->source_len)
        return '\0';

    return ctx->source[lexer->cursor];
}

char peek_next(context_t *ctx, lexer_t *lexer)
{
    if (lexer->cursor >= ctx->source_len)
        return '\0';

    return ctx->source[lexer->cursor + 1];
}

bool match(context_t *ctx, lexer_t *lexer, char ch)
{
    if (ctx->source[lexer->cursor] != ch)
        return false;

    advance(ctx, lexer);
    return true;
}

bool is_at_end(context_t *ctx, lexer_t *lexer)
{
    return lexer->cursor >= ctx->source_len;
}

void append_token(context_t *ctx, lexer_t *lexer, token_type_t type, const char *lexeme,
                  usize lexeme_len, usize line_idx, usize col_idx, usize cursor, usize line_start)
{
    token_t token = {
        .lexeme = lexeme,
        .lexeme_len = lexeme_len,
        .line = line_idx + 1,
        .col = col_idx + 1,
        .line_start = line_start,
        .cursor = cursor,
        .type = type,
    };

    arena_da_append(ctx->arena, &lexer->tokens, token);
}

void report(usize line_idx, usize col_idx, const char *source_filename, const char *source,
            usize line_start, const char *message)
{
    usize col = col_idx + 1;
    usize line = line_idx + 1;

    const char *line_end = source + line_start;
    while (*line_end && *line_end != '\n') {
        line_end++;
    }

    isize text_len = line_end - (source + line_start);

    fprintf(stderr, "%s:%zu:%zu: %s\n", source_filename, line, col, message);
    fprintf(stderr, "%4zu | %.*s\n", line, (int)text_len, source + line_start);
    fprintf(stderr, "     | %*s\n", (int)col, "^");

    had_error = true;
}

void report_unexpected_token(context_t *ctx, token_t tok, const char *expected)
{
    char error_msg[256];
    usize error_msg_len = sizeof(error_msg);
    usize max_lexeme_len = tok.lexeme_len < error_msg_len ? tok.lexeme_len : error_msg_len;
    snprintf(error_msg, sizeof(error_msg), "expected %s, but got %.*s%s", expected,
             (int)max_lexeme_len, tok.lexeme, tok.lexeme_len > max_lexeme_len ? "..." : "");

    report(tok.line - 1, tok.col - 1, ctx->source_filename, ctx->source, tok.line_start, error_msg);
}

token_type_t lookup_keyword(const char *s)
{
    size_t n = sizeof reserved_words_table / sizeof reserved_words_table[0];

    for (size_t i = 0; i < n; ++i) {
        if (strcmp(reserved_words_table[i].identifier, s) == 0)
            return reserved_words_table[i].type;
    }

    return IDENTIFIER;
}

bool is_alphanum(char ch)
{
    return isalnum(ch) || ch == '_';
}

ast_node_t *create_node(arena_t *arena, node_type_t type, node_value_t value)
{
    ast_node_t *node = arena_alloc(arena, sizeof(*node));

    node->value = value;
    node->start = 0;
    node->end = 0;
    node->type = type;

    return node;
}

token_t previous_parser(parser_t *parser)
{
    assert(parser->current_index > 0);

    return parser->tokens[parser->current_index - 1];
}

token_t peek_parser(parser_t *parser)
{
    if (parser->current_index >= parser->tokens_size)
        return parser->tokens[parser->tokens_size - 1];

    return parser->tokens[parser->current_index];
}

token_t peek_next_parser(parser_t *parser)
{
    if (parser->current_index + 1 >= parser->tokens_size)
        return parser->tokens[parser->tokens_size - 1];

    return parser->tokens[parser->current_index + 1];
}

token_t advance_parser(parser_t *parser)
{
    if (parser->current_index >= parser->tokens_size)
        return parser->tokens[parser->tokens_size - 1];

    return parser->tokens[parser->current_index++];
}

bool match_parser(parser_t *parser, usize count, ...)
{
    va_list args;
    va_start(args, count);

    token_t tok = peek_parser(parser);
    for (usize i = 0; i < count; ++i) {
        if (tok.type == va_arg(args, token_type_t)) {
            va_end(args);
            return true;
        }
    }

    va_end(args);
    return false;
}

void synchronize(parser_t *parser)
{
    advance_parser(parser);

    while (!parser_is_eof(parser)) {
        if (previous_parser(parser).type == SEMICOLON)
            return;

        switch (peek_parser(parser).type) {
        case CLASS:
        case FUN:
        case QUESTION_MARK:
        case COLON:
        case VAR:
        case FOR:
        case IF:
        case WHILE:
        case PRINT:
        case RETURN:
            return;
        default:
            break;
        }

        advance_parser(parser);
    }
}

const char *intern(context_t *ctx, const char *s)
{
    string_pool_entry_t *current = ctx->string_pool.head;

    while (current) {
        if (strcmp(current->str, s) == 0)
            return current->str;
        current = current->next;
    }

    string_pool_entry_t *entry = arena_alloc(ctx->arena, sizeof(*entry));
    if (!entry)
        return NULL;

    entry->str = strdup(s);
    if (!entry->str)
        return NULL;

    entry->next = ctx->string_pool.head;
    ctx->string_pool.head = entry;

    return entry->str;
}

bool parser_is_eof(parser_t *parser)
{
    return peek_parser(parser).type == END_OF_FILE;
}
