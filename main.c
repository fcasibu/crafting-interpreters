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
    NODE_PROGRAM, NODE_IDENTIFIER, NODE_TERNARY, NODE_NOTHING
    // clang-format on
} node_type_t;

typedef enum {
    // clang-format off
    VALUE_NUMBER, VALUE_STRING, VALUE_BOOL, VALUE_NIL,
    VALUE_ERROR
    // clang-format on
} value_type_t;

typedef union {
    double number;
    const char *string;
    bool boolean;
} value_data_t;

typedef struct {
    value_type_t type;
    value_data_t value;
} value_t;

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
        token_t op;
        struct ast_node *child;
    } unary;

    struct {
        token_t op;
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
    usize start;
    usize end;
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
void report_runtime(usize line_idx, usize col_idx, const char *source_filename, const char *source,
                    usize line_start, const char *message);
void report_unexpected_token(context_t *ctx, token_t tok, const char *expected);
void report_missing_expression(context_t *ctx, token_t tok, const char *error_msg);
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
void consume(context_t *ctx, parser_t *parser, token_type_t type, const char *message);
char peek(context_t *ctx, lexer_t *lexer);
char peek_next(context_t *ctx, lexer_t *lexer);
void string(context_t *ctx, lexer_t *lexer);
void number(context_t *ctx, lexer_t *lexer);
void identifier(context_t *ctx, lexer_t *lexer);
void comment_block(context_t *ctx, lexer_t *lexer);
bool is_at_end(context_t *ctx, lexer_t *lexer);
token_type_t lookup_keyword(const char *s);
bool is_alphanum(char ch);
ast_node_t *create_node(arena_t *arena, node_type_t type, node_value_t value, usize start,
                        usize end);
bool parser_is_eof(parser_t *parser);
token_t peek_parser(parser_t *parser);
token_t peek_next_parser(parser_t *parser);
token_t previous_parser(parser_t *parser);
token_t advance_parser(parser_t *parser);
bool match_parser(parser_t *parser, usize count, ...);
void synchronize(parser_t *parser);
const char *intern(context_t *ctx, const char *s);
const char *intern_token_lexeme(context_t *ctx, parser_t *parser);
void consume_terminator(parser_t *parser, const char *message);
bool is_truthy(value_t value);
bool is_equal(value_t a, value_t b);
const char *stringify_value(context_t *ctx, value_t value);
bool interpret_comparison(token_type_t op, value_t a, value_t b);
value_t interpret_binary(context_t *ctx, ast_node_t *node);
value_t interpret_unary(context_t *ctx, ast_node_t *node);
value_t interpret_ternary(context_t *ctx, ast_node_t *node);
value_t interpret(context_t *ctx, ast_node_t *node);
value_t create_value(value_type_t type, value_data_t value);
value_t create_error_value();
bool check_number_operand(context_t *ctx, token_t tok, value_t child);
bool check_number_operands(context_t *ctx, token_t tok, value_t left, value_t right);
bool check_same_operands(context_t *ctx, token_t tok, value_t left, value_t right);

static bool had_error = false;
static bool had_runtime_error = false;

void print_token_type(token_type_t type)
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
    case BANG_EQUAL:
        printf("!=");
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
    case AND:
        printf("and");
        break;
    default:
        printf("tok(%d)", type);
        break;
    }
}

void print_indent(usize indent)
{
    for (usize i = 0; i < indent; ++i)
        printf("  ");
}

void print_ast(ast_node_t *node, usize indent)
{
    if (!node)
        return;

    print_indent(indent);

    switch (node->type) {
    case NODE_NUMBER:
        printf("(number %g)\n", node->value.literal.number);
        break;
    case NODE_STRING:
        printf("(string \"%s\")\n", node->value.literal.string);
        break;
    case NODE_BOOL:
        printf("(boolean %s)\n", node->value.literal.boolean ? "true" : "false");
        break;
    case NODE_NIL:
        printf("(nil)\n");
        break;
    case NODE_IDENTIFIER:
        printf("(identifier \"%s\")\n", node->value.identifier.name);
        break;
    case NODE_UNARY:
        printf("(unary ");
        print_token_type(node->value.unary.op.type);
        printf("\n");
        print_ast(node->value.unary.child, indent + 1);
        print_indent(indent);
        printf(")\n");
        break;
    case NODE_BINARY:
        printf("(binary ");
        print_token_type(node->value.binary.op.type);
        printf("\n");
        print_ast(node->value.binary.left, indent + 1);
        print_ast(node->value.binary.right, indent + 1);
        print_indent(indent);
        printf(")\n");
        break;
    case NODE_TERNARY:
        printf("(ternary\n");
        print_ast(node->value.ternary.condition, indent + 1);
        print_ast(node->value.ternary.true_branch, indent + 1);
        print_ast(node->value.ternary.false_branch, indent + 1);
        print_indent(indent);
        printf(")\n");
        break;
    case NODE_PROGRAM:
        printf("(program\n");
        for (usize i = 0; i < node->value.program.size; i++)
            print_ast(node->value.program.statements[i], indent + 1);
        print_indent(indent);
        printf(")\n");
        break;
    default:
        printf("(unknown)\n");
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
        return 65;

    if (had_runtime_error)
        return 70;

    return 0;
}

void run(context_t *ctx)
{
    lexer_t lexer = { 0 };
    arena_da_init(ctx->arena, &lexer.tokens);

    tokenize(ctx, &lexer);

    parser_t parser = { 0 };
    parser.tokens = lexer.tokens.items;
    parser.tokens_size = lexer.tokens.size;
    parse(ctx, &parser);

    if (had_error)
        return;

    for (usize i = 0; i < parser.root->value.program.size; ++i) {
        value_t value = interpret(ctx, parser.root->value.program.statements[i]);

        switch (value.type) {
        case VALUE_NUMBER: {
            printf("%g\n", value.value.number);
        } break;

        case VALUE_BOOL: {
            printf("%d\n", value.value.boolean);
        } break;

        case VALUE_STRING: {
            printf("%s\n", value.value.string);
        } break;

        case VALUE_NIL: {
            printf("nil\n");
        } break;

        default:
            break;
        }
    }
    // print_ast(parser.root, 0);
}

void parse(context_t *ctx, parser_t *parser)
{
    ast_nodes_t prog_stmts = { 0 };
    arena_da_init(ctx->arena, &prog_stmts);

    while (parser->current_index < parser->tokens_size && !parser_is_eof(parser)) {
        ast_node_t *node = parse_statement(ctx, parser);

        if (!node)
            continue;

        arena_da_append(ctx->arena, &prog_stmts, node);
    }

    parser->root = create_node(
        ctx->arena, NODE_PROGRAM,
        (node_value_t){ .program.statements = prog_stmts.items, .program.size = prog_stmts.size },
        0, prog_stmts.size > 0 ? prog_stmts.items[prog_stmts.size - 1]->end : 0);
}

ast_node_t *parse_statement(context_t *ctx, parser_t *parser)
{
    token_t tok = peek_parser(parser);

    switch (tok.type) {
    default: {
        ast_node_t *left = parse_expression(ctx, parser);

        while (!parser_is_eof(parser) && match_parser(parser, 1, COMMA)) {
            token_t op = advance_parser(parser);

            ast_node_t *right = parse_expression(ctx, parser);

            if (!right)
                return NULL;

            node_value_t node_value = {
                .binary = { .op = op, .left = left, .right = right },
            };
            left = create_node(ctx->arena, NODE_BINARY, node_value, left->start, right->start);
        }

        consume(ctx, parser, SEMICOLON, "Expected semicolon");

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
    token_t tok = peek_parser(parser);
    if (tok.type == END_OF_FILE)
        return NULL;

    ast_node_t *condition = parse_logical_or(ctx, parser);
    if (!condition) {
        report_unexpected_token(ctx, tok, "expression");
        synchronize(parser);
        return create_node(ctx->arena, NODE_NOTHING, (node_value_t){}, 0, 0);
    }

    if (!match_parser(parser, 1, QUESTION_MARK))
        return condition;

    advance_parser(parser);

    ast_node_t *true_branch = parse_ternary(ctx, parser);
    if (!true_branch) {
        report_unexpected_token(ctx, previous_parser(parser), "expression");
        synchronize(parser);
        return create_node(ctx->arena, NODE_NOTHING, (node_value_t){}, 0, 0);
    }

    if (!match_parser(parser, 1, COLON)) {
        report_missing_expression(ctx, previous_parser(parser),
                                  "Expected false branch expression for ternary");
        synchronize(parser);
        return create_node(ctx->arena, NODE_NOTHING, (node_value_t){}, 0, 0);
    }

    advance_parser(parser);

    ast_node_t *false_branch = parse_ternary(ctx, parser);
    if (!false_branch) {
        report_missing_expression(ctx, previous_parser(parser), "Expected expression after ':'");
        synchronize(parser);
        return create_node(ctx->arena, NODE_NOTHING, (node_value_t){}, 0, 0);
    }

    node_value_t value = { .ternary = { .condition = condition,
                                        .true_branch = true_branch,
                                        .false_branch = false_branch } };

    return create_node(ctx->arena, NODE_TERNARY, value, condition->start, false_branch->end);
}

ast_node_t *parse_logical_or(context_t *ctx, parser_t *parser)
{
    token_t tok = peek_parser(parser);
    ast_node_t *left = parse_logical_and(ctx, parser);

    while (!parser_is_eof(parser) && match_parser(parser, 1, OR)) {
        if (!left) {
            report_missing_expression(ctx, tok, "Missing left-hand expression");
            synchronize(parser);

            return create_node(ctx->arena, NODE_NOTHING, (node_value_t){}, 0, 0);
        }

        token_t op = advance_parser(parser);

        ast_node_t *right = parse_logical_and(ctx, parser);

        if (!right) {
            report_missing_expression(ctx, previous_parser(parser),
                                      "Missing right-hand expression");
            synchronize(parser);

            return create_node(ctx->arena, NODE_NOTHING, (node_value_t){}, 0, 0);
        }

        node_value_t value = { .binary = { .left = left, .op = op, .right = right } };
        left = create_node(ctx->arena, NODE_BINARY, value, left->start, right->end);
    }

    return left;
}

ast_node_t *parse_logical_and(context_t *ctx, parser_t *parser)
{
    token_t tok = peek_parser(parser);
    ast_node_t *left = parse_equality(ctx, parser);

    while (!parser_is_eof(parser) && match_parser(parser, 1, AND)) {
        if (!left) {
            report_missing_expression(ctx, tok, "Missing left-hand expression");
            synchronize(parser);

            return create_node(ctx->arena, NODE_NOTHING, (node_value_t){}, 0, 0);
        }

        token_t op = advance_parser(parser);

        ast_node_t *right = parse_equality(ctx, parser);

        if (!right) {
            report_missing_expression(ctx, previous_parser(parser),
                                      "Missing right-hand expression");
            synchronize(parser);

            return create_node(ctx->arena, NODE_NOTHING, (node_value_t){}, 0, 0);
        }

        node_value_t value = { .binary = { .left = left, .op = op, .right = right } };
        left = create_node(ctx->arena, NODE_BINARY, value, left->start, right->end);
    }

    return left;
}

ast_node_t *parse_equality(context_t *ctx, parser_t *parser)
{
    token_t tok = peek_parser(parser);
    ast_node_t *left = parse_comparison(ctx, parser);

    while (!parser_is_eof(parser) && match_parser(parser, 2, EQUAL_EQUAL, BANG_EQUAL)) {
        if (!left) {
            report_missing_expression(ctx, tok, "Equality operators must have a left-hand operand");
            synchronize(parser);

            return create_node(ctx->arena, NODE_NOTHING, (node_value_t){}, 0, 0);
        }

        token_t op = advance_parser(parser);

        ast_node_t *right = parse_comparison(ctx, parser);

        if (!right) {
            report_missing_expression(ctx, previous_parser(parser),
                                      "Equality operators must have a right-hand operand");
            synchronize(parser);

            return create_node(ctx->arena, NODE_NOTHING, (node_value_t){}, 0, 0);
        }

        node_value_t value = { .binary = { .left = left, .op = op, .right = right } };
        left = create_node(ctx->arena, NODE_BINARY, value, left->start, right->end);
    }

    return left;
}

ast_node_t *parse_comparison(context_t *ctx, parser_t *parser)
{
    token_t tok = peek_parser(parser);
    ast_node_t *left = parse_term(ctx, parser);

    while (!parser_is_eof(parser) &&
           match_parser(parser, 4, LESS, LESS_EQUAL, GREATER, GREATER_EQUAL)) {
        if (!left) {
            report_missing_expression(ctx, tok,
                                      "Comparison operators must have a left-hand operand");
            synchronize(parser);

            return create_node(ctx->arena, NODE_NOTHING, (node_value_t){}, 0, 0);
        }
        token_t op = advance_parser(parser);

        ast_node_t *right = parse_term(ctx, parser);

        if (!right) {
            report_missing_expression(ctx, previous_parser(parser),
                                      "Comparison operators must have a right-hand operand");
            synchronize(parser);

            return create_node(ctx->arena, NODE_NOTHING, (node_value_t){}, 0, 0);
        }

        node_value_t value = { .binary = { .left = left, .op = op, .right = right } };
        left = create_node(ctx->arena, NODE_BINARY, value, left->start, right->end);
    }

    return left;
}
ast_node_t *parse_term(context_t *ctx, parser_t *parser)
{
    token_t tok = peek_parser(parser);
    ast_node_t *left = parse_factor(ctx, parser);

    while (!parser_is_eof(parser) && match_parser(parser, 2, PLUS, MINUS)) {
        if (!left) {
            report_missing_expression(
                ctx, tok, "Addition/subtraction operators must have a left-hand operand");
            synchronize(parser);

            return create_node(ctx->arena, NODE_NOTHING, (node_value_t){}, 0, 0);
        }
        token_t op = advance_parser(parser);

        ast_node_t *right = parse_factor(ctx, parser);

        if (!right) {
            report_missing_expression(
                ctx, previous_parser(parser),
                "Addition/subtraction operators must have a right-hand operand");
            synchronize(parser);

            return create_node(ctx->arena, NODE_NOTHING, (node_value_t){}, 0, 0);
        }

        node_value_t value = { .binary = { .left = left, .op = op, .right = right } };
        left = create_node(ctx->arena, NODE_BINARY, value, left->start, right->end);
    }

    return left;
}
ast_node_t *parse_factor(context_t *ctx, parser_t *parser)
{
    token_t tok = peek_parser(parser);
    ast_node_t *left = parse_unary(ctx, parser);

    while (!parser_is_eof(parser) && match_parser(parser, 2, STAR, SLASH)) {
        if (!left) {
            report_missing_expression(
                ctx, tok, "Multiplication/division operators must have a left-hand operand");
            synchronize(parser);

            return create_node(ctx->arena, NODE_NOTHING, (node_value_t){}, 0, 0);
        }
        token_t op = advance_parser(parser);

        ast_node_t *right = parse_unary(ctx, parser);

        if (!right) {
            report_missing_expression(
                ctx, previous_parser(parser),
                "Multiplication/division operators must have a right-hand operand");
            synchronize(parser);

            return create_node(ctx->arena, NODE_NOTHING, (node_value_t){}, 0, 0);
        }

        node_value_t value = { .binary = { .left = left, .op = op, .right = right } };
        left = create_node(ctx->arena, NODE_BINARY, value, left->start, right->end);
    }

    return left;
}

ast_node_t *parse_unary(context_t *ctx, parser_t *parser)
{
    token_t tok = peek_parser(parser);

    switch (tok.type) {
    case PLUS:
    case MINUS:
    case BANG: {
        advance_parser(parser);
        ast_node_t *node = parse_primary(ctx, parser);
        if (!node) {
            report_missing_expression(ctx, tok, "Unary operator must have an operand");
            return create_node(ctx->arena, NODE_NOTHING, (node_value_t){}, 0, 0);
        }

        node_value_t value = { .unary = { .op = tok, .child = node } };
        return create_node(ctx->arena, NODE_UNARY, value, node->start, node->end);
    };

        // case MINUS: {
        //     advance_parser(parser);
        //     ast_node_t *node = parse_primary(ctx, parser);
        //     if (!node) {
        //         report_missing_expression(ctx, tok, "Unary operator must have an operand");
        //         return create_node(ctx->arena, NODE_NOTHING, (node_value_t){}, 0, 0);
        //     }
        //
        //     node_value_t value = { .unary = { .op = tok.type, .child = node } };
        //     return create_node(ctx->arena, NODE_UNARY, value, node->start, node->end);
        // };
        //
        // case BANG: {
        //     advance_parser(parser);
        //     ast_node_t *node = parse_primary(ctx, parser);
        //     if (!node) {
        //         report_missing_expression(ctx, tok, "Unary operator must have an operand");
        //         return create_node(ctx->arena, NODE_NOTHING, (node_value_t){}, 0, 0);
        //     }
        //
        //     node_value_t value = { .unary = { .op = tok.type, .child = node } };
        //     return create_node(ctx->arena, NODE_UNARY, value, node->start, node->end);
        // };

    default:
        break;
    }

    return parse_primary(ctx, parser);
}
ast_node_t *parse_primary(context_t *ctx, parser_t *parser)
{
    if (match_parser(parser, 2, TRUE, FALSE)) {
        ast_node_t *node = parse_boolean(ctx, parser);
        advance_parser(parser);

        return node;
    }

    if (match_parser(parser, 1, NIL)) {
        advance_parser(parser);

        return create_node(ctx->arena, NODE_NIL, (node_value_t){}, 0, 0);
    }

    if (match_parser(parser, 1, IDENTIFIER)) {
        ast_node_t *node = parse_identifier(ctx, parser);
        advance_parser(parser);

        return node;
    }

    if (match_parser(parser, 1, LEFT_PAREN)) {
        advance_parser(parser);
        ast_node_t *node = parse_expression(ctx, parser);

        if (!node) {
            report_missing_expression(ctx, previous_parser(parser),
                                      "Expected expression inside parentheses");
            synchronize(parser);

            return create_node(ctx->arena, NODE_NOTHING, (node_value_t){}, 0, 0);
        }

        token_t next_tok = advance_parser(parser);

        if (next_tok.type != RIGHT_PAREN) {
            report_unexpected_token(ctx, next_tok, ")");
            synchronize(parser);

            return create_node(ctx->arena, NODE_NOTHING, (node_value_t){}, 0, 0);
        }

        return node;
    }

    if (match_parser(parser, 1, STRING)) {
        ast_node_t *node = parse_string(ctx, parser);
        advance_parser(parser);

        return node;
    }

    if (match_parser(parser, 1, DOT)) {
        report_unexpected_token(ctx, peek_parser(parser), "number");
        synchronize(parser);

        return create_node(ctx->arena, NODE_NOTHING, (node_value_t){}, 0, 0);
    }

    if (match_parser(parser, 1, NUMBER)) {
        ast_node_t *node = parse_number(ctx, parser);
        advance_parser(parser);

        return node;
    }

    return NULL;
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
    return create_node(ctx->arena, NODE_NUMBER, node_value, tok.cursor,
                       tok.cursor + tok.lexeme_len);
}
ast_node_t *parse_string(context_t *ctx, parser_t *parser)
{
    token_t tok = peek_parser(parser);
    if (tok.type != STRING) {
        report_unexpected_token(ctx, tok, "string");
        return NULL;
    }

    char tmp[tok.lexeme_len - 2];
    // remove quotes
    memcpy(tmp, tok.lexeme + 1, tok.lexeme_len - 1);
    tmp[tok.lexeme_len - 1] = '\0';

    const char *value = intern(ctx, tmp);
    if (!value)
        return NULL;

    node_value_t node_value = { .literal = { .string = value } };
    return create_node(ctx->arena, NODE_STRING, node_value, tok.cursor, tok.lexeme_len);
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
    return create_node(ctx->arena, NODE_BOOL, node_value, tok.cursor, tok.lexeme_len);
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
    return create_node(ctx->arena, NODE_IDENTIFIER, node_value, tok.cursor, tok.lexeme_len);
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
        report(start_line_idx, start_col_idx, ctx->source_filename, ctx->source, line_start,
               "Unterminated string.");
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

void report_runtime(usize line_idx, usize col_idx, const char *source_filename, const char *source,
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

    had_runtime_error = true;
}

void report_unexpected_token(context_t *ctx, token_t tok, const char *expected)
{
    char error_msg[256];
    usize error_msg_len = sizeof(error_msg);
    usize max_lexeme_len = tok.lexeme_len < error_msg_len ? tok.lexeme_len : error_msg_len;
    snprintf(error_msg, sizeof(error_msg), "expected %s but got %.*s%s", expected,
             (int)max_lexeme_len, tok.lexeme, tok.lexeme_len > max_lexeme_len ? "..." : "");

    report(tok.line - 1, tok.col - 1, ctx->source_filename, ctx->source, tok.line_start, error_msg);
}

char x[] = "1234";

void report_missing_expression(context_t *ctx, token_t tok, const char *error_msg)
{
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

ast_node_t *create_node(arena_t *arena, node_type_t type, node_value_t value, usize start,
                        usize end)
{
    ast_node_t *node = arena_alloc(arena, sizeof(*node));

    node->value = value;
    node->start = start;
    node->end = end;
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

void consume(context_t *ctx, parser_t *parser, token_type_t type, const char *message)
{
    if (match_parser(parser, 1, type)) {
        advance_parser(parser);
        return;
    }

    token_t tok = previous_parser(parser);
    report(tok.line - 1, tok.col - 1, ctx->source_filename, ctx->source, tok.line_start, message);
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

bool is_truthy(value_t value)
{
    switch (value.type) {
    case VALUE_NUMBER:
        return value.value.number != 0;

    case VALUE_STRING:
        return strcmp(value.value.string, "");

    case VALUE_BOOL:
        return value.value.boolean;

    default:
        return false;
    }
}

bool is_equal(value_t a, value_t b)
{
    switch (a.type) {
    case VALUE_NUMBER: {
        if (b.type != VALUE_NUMBER)
            return false;

        return a.value.number == b.value.number;
    }

    case VALUE_STRING: {
        if (b.type != VALUE_STRING)
            return false;

        return strcmp(a.value.string, b.value.string) == 0;
    }

    case VALUE_BOOL:
        if (b.type != VALUE_BOOL)
            return false;

        return a.value.boolean == b.value.boolean;

    default:
        return false;
    }
}

const char *stringify_value(context_t *ctx, value_t value)
{
    switch (value.type) {
    case VALUE_BOOL:
        return value.value.boolean ? "true" : "false";
    case VALUE_NUMBER:
        return number_to_string(ctx->arena, value.value.number);
    case VALUE_NIL:
        return "nil";
    case VALUE_ERROR:
        return "";
    case VALUE_STRING:
        return value.value.string;
    }
}

bool interpret_comparison(token_type_t op, value_t a, value_t b)
{
    assert(a.type == b.type);

    switch (a.type) {
    case VALUE_NUMBER: {
        double x = a.value.number;
        double y = b.value.number;

        switch (op) {
        case LESS:
            return x < y;
        case LESS_EQUAL:
            return x <= y;
        case GREATER:
            return x > y;
        case GREATER_EQUAL:
            return x >= y;
        default:
            return false;
        }
    }

    case VALUE_STRING: {
        int cmp = strcmp(a.value.string, b.value.string);

        switch (op) {
        case LESS:
            return cmp < 0;
        case LESS_EQUAL:
            return cmp <= 0;
        case GREATER:
            return cmp > 0;
        case GREATER_EQUAL:
            return cmp >= 0;
        default:
            return false;
        }
    }

    case VALUE_BOOL: {
        int x = a.value.boolean;
        int y = b.value.boolean;

        switch (op) {
        case LESS:
            return x < y;
        case LESS_EQUAL:
            return x <= y;
        case GREATER:
            return x > y;
        case GREATER_EQUAL:
            return x >= y;
        default:
            return false;
        }
    }

    default:
        return false;
    }
}

value_t interpret_binary(context_t *ctx, ast_node_t *node)
{
    assert(node->type == NODE_BINARY);

    value_t left = interpret(ctx, node->value.binary.left);
    value_t right = interpret(ctx, node->value.binary.right);

    switch (node->value.binary.op.type) {
    case OR: {
        return create_value(VALUE_BOOL,
                            (value_data_t){ .boolean = is_truthy(left) || is_truthy(right) });
    }

    case AND: {
        return create_value(VALUE_BOOL,
                            (value_data_t){ .boolean = is_truthy(left) && is_truthy(right) });
    }

    case EQUAL_EQUAL: {
        return create_value(VALUE_BOOL, (value_data_t){ .boolean = is_equal(left, right) });
    }

    case BANG_EQUAL: {
        return create_value(VALUE_BOOL, (value_data_t){ .boolean = !is_equal(left, right) });
    }

    case LESS:
    case LESS_EQUAL:
    case GREATER:
    case GREATER_EQUAL: {
        if (!check_number_operands(ctx, node->value.binary.op, left, right))
            return create_error_value();

        return create_value(
            VALUE_BOOL, (value_data_t){ .boolean = interpret_comparison(node->value.binary.op.type,
                                                                        left, right) });
    }

    case PLUS: {
        if (left.type == VALUE_STRING || right.type == VALUE_STRING) {
            const char *left_str = stringify_value(ctx, left);
            const char *right_str = stringify_value(ctx, right);

            return create_value(VALUE_STRING,
                                (value_data_t){
                                    .string = concat_str(ctx->arena, left_str, right_str),
                                });
        }

        if (!check_number_operands(ctx, node->value.binary.op, left, right))
            return create_error_value();

        return create_value(VALUE_NUMBER,
                            (value_data_t){ .number = left.value.number + right.value.number });
    }
    case MINUS: {
        if (!check_number_operands(ctx, node->value.binary.op, left, right))
            return create_error_value();

        return create_value(VALUE_NUMBER,
                            (value_data_t){ .number = left.value.number - right.value.number });
    }

    case STAR: {
        if (!check_number_operands(ctx, node->value.binary.op, left, right))
            return create_error_value();

        return create_value(VALUE_NUMBER,
                            (value_data_t){ .number = left.value.number * right.value.number });
    }

    case SLASH: {
        if (!check_number_operands(ctx, node->value.binary.op, left, right))
            return create_error_value();

        if (right.value.number == 0.0) {
            token_t tok = node->value.binary.op;
            report_runtime(tok.line - 1, tok.col - 1, ctx->source_filename, ctx->source,
                           tok.line_start, "Division by zero.");
            return create_error_value();
        }

        return create_value(VALUE_NUMBER,
                            (value_data_t){ .number = left.value.number / right.value.number });
    }

    default: {
        __builtin_unreachable();
    }
    }
}

value_t interpret_unary(context_t *ctx, ast_node_t *node)
{
    switch (node->value.unary.op.type) {
    case BANG: {
        return create_value(
            VALUE_BOOL,
            (value_data_t){ .boolean = !is_truthy(interpret(ctx, node->value.unary.child)) });
    }

    case MINUS: {
        value_t child = interpret(ctx, node->value.unary.child);
        if (!check_number_operand(ctx, node->value.unary.op, child))
            return create_error_value();

        return create_value(VALUE_NUMBER, (value_data_t){ .number = -child.value.number });
    }

    case PLUS: {
        value_t child = interpret(ctx, node->value.unary.child);
        if (!check_number_operand(ctx, node->value.unary.op, child))
            return create_error_value();

        return create_value(VALUE_NUMBER, (value_data_t){ .number = +child.value.number });
    }

    default: {
        __builtin_unreachable();
    }
    }
}

value_t interpret_ternary(context_t *ctx, ast_node_t *node)
{
    value_t condition = interpret(ctx, node->value.ternary.condition);
    value_t true_branch = interpret(ctx, node->value.ternary.true_branch);
    value_t false_branch = interpret(ctx, node->value.ternary.false_branch);

    return condition.value.boolean ? true_branch : false_branch;
}

value_t interpret(context_t *ctx, ast_node_t *node)
{
    switch (node->type) {
    case NODE_NUMBER:
        return create_value(VALUE_NUMBER, (value_data_t){ .number = node->value.literal.number });

    case NODE_STRING:
        return create_value(VALUE_STRING, (value_data_t){ .string = node->value.literal.string });

    case NODE_BOOL:
        return create_value(VALUE_BOOL, (value_data_t){ .boolean = node->value.literal.boolean });

    case NODE_NIL:
        return create_value(VALUE_NIL, (value_data_t){});

    case NODE_BINARY:
        return interpret_binary(ctx, node);

    case NODE_UNARY:
        return interpret_unary(ctx, node);

    case NODE_TERNARY:
        return interpret_ternary(ctx, node);

    default: {
        __builtin_unreachable();
    }
    }
}

value_t create_value(value_type_t type, value_data_t value)
{
    return (value_t){ .type = type, .value = value };
}

value_t create_error_value()
{
    return (value_t){ .type = VALUE_ERROR };
}

bool check_number_operand(context_t *ctx, token_t tok, value_t child)
{
    if (child.type == VALUE_NUMBER)
        return true;

    report_runtime(tok.line - 1, tok.col - 1, ctx->source_filename, ctx->source, tok.line_start,
                   "Operand must be a number.");

    return false;
}

bool check_number_operands(context_t *ctx, token_t tok, value_t left, value_t right)
{
    if (left.type == VALUE_NUMBER && right.type == VALUE_NUMBER)
        return true;

    report_runtime(tok.line - 1, tok.col - 1, ctx->source_filename, ctx->source, tok.line_start,
                   "Operands must be numbers.");

    return false;
}

bool check_same_operands(context_t *ctx, token_t tok, value_t left, value_t right)
{
    if (left.type == right.type)
        return true;

    report_runtime(tok.line - 1, tok.col - 1, ctx->source_filename, ctx->source, tok.line_start,
                   "Operands must be of same type.");

    return false;
}
