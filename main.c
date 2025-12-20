#define ARENA_IMPLEMENTATION
#include "arena.h"
#define FILE_IMPLEMENTATION
#include "file.h"
#define UTILS_IMPLEMENTATION
#include "utils.h"
#include "logger.h"
#include "type_defs.h"

#define ARENA_SIZE 1024 * 1024 * 1024
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
  BREAK,

  END_OF_FILE
} token_type_t;
// clang-format on

typedef enum {
    // clang-format off
    NODE_NUMBER, NODE_STRING, NODE_BOOL, NODE_NIL, NODE_UNARY, NODE_BINARY,
    NODE_PROGRAM, NODE_IDENTIFIER, NODE_TERNARY, NODE_PRINT, NODE_VAR,
    NODE_ASSIGN, NODE_BLOCK, NODE_IF, NODE_WHILE, NODE_FOR, NODE_BREAK,
    NODE_FUN, NODE_CALL, NODE_RETURN, NODE_UNINITIALIZED
    // clang-format on
} node_type_t;

typedef enum {
    // clang-format off
    VALUE_NUMBER = 1, VALUE_STRING, VALUE_BOOL, VALUE_NIL,
    VALUE_FUN, VALUE_UNINITIALIZED
    // clang-format on
} value_type_t;

typedef enum { EVAL_OK, EVAL_BREAK, EVAL_ERROR, EVAL_RETURN } eval_type_t;

typedef union {
    double number;
    const char *string;
    bool boolean;
    struct {
        const char *name;
        struct ast_nodes *params;
        struct ast_node *body;
        struct environment *closure;
    } fun_def;
} value_as_t;

typedef struct {
    value_type_t type;
    value_as_t as;
} value_t;

typedef struct {
    eval_type_t type;
    value_t value;
} eval_result_t;

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

typedef struct {
    usize line;
    usize col;
    usize line_start;
    usize cursor;
} source_loc_t;

typedef union {
    const char *string_literal;
    double number_literal;
    bool boolean_literal;

    struct {
        const char *name;
        // TODO(fcasibu): maybe source loc should be on the node..
        source_loc_t loc;
    } identifier;

    struct {
        struct ast_node *child;
        token_t op;
    } unary;

    struct {
        struct ast_node *left;
        struct ast_node *right;
        token_t op;
    } binary;

    struct {
        struct ast_node *condition;
        struct ast_node *true_branch;
        struct ast_node *false_branch;
    } ternary;

    struct ast_node *print_stmt;

    struct {
        struct ast_node *condition;
        struct ast_node *then_branch;
        struct ast_node *else_branch;
    } if_stmt;

    struct {
        struct ast_node *condition;
        struct ast_node *body;
    } while_stmt;

    struct {
        struct ast_node *initializer;
        struct ast_node *condition;
        struct ast_node *increment;
        struct ast_node *body;
    } for_stmt;

    struct ast_node *return_value;

    struct {
        struct ast_node *identifier;
        struct ast_node *expression;
    } var_decl;

    struct {
        struct ast_nodes *params;
        struct ast_node *name;
        struct ast_node *body;
    } fun_decl;

    struct {
        struct ast_node *identifier;
        struct ast_node *expression;
    } assign;

    struct ast_nodes *block_declarations;

    struct {
        struct ast_nodes *args;
        struct ast_node *callee;
        // TODO(fcasibu): ok twice now, so refactor source location later
        source_loc_t loc;
    } call;

    struct ast_nodes *prog_declarations;
} node_value_t;

typedef struct ast_node {
    node_type_t type;
    node_value_t value;
    usize start;
    usize end;
} ast_node_t;

typedef struct ast_nodes {
    ast_node_t **items;
    usize size;
    usize capacity;
} ast_nodes_t;

typedef struct {
    token_t *tokens;
    usize tokens_size;

    ast_node_t *root;
    usize current_index;

    usize loop_depth;
    usize function_depth;
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
    {"break", BREAK},
    // clang-format on
};

typedef struct string_pool_entry {
    const char *str;
    struct string_pool_entry *next;
} string_pool_entry_t;

typedef struct {
    string_pool_entry_t *head;
} string_pool_t;

// TODO(fcasibu): hashmap
typedef struct var_pool_entry {
    const char *str;
    value_t value;
    struct var_pool_entry *next;
} var_pool_entry_t;

typedef struct {
    var_pool_entry_t *head;
} var_pool_t;

typedef struct scope {
    struct scope *enclosing;
    var_pool_t pool;
} scope_t;

typedef struct environment {
    struct environment *parent_env;
    var_pool_t pool;
} environment_t;

typedef struct {
    arena_t *arena;
    const char *source;
    const char *source_filename;
    usize source_len;
    const char *cmd;
} context_t;

void lexer_tokenize(context_t *ctx, lexer_t *lexer);
void lexer_append_token(context_t *ctx, lexer_t *lexer, token_type_t type, const char *lexeme,
                        usize lexeme_len, usize line_idx, usize col_idx, usize cursor,
                        usize line_start);
bool lexer_match(context_t *ctx, lexer_t *lexer, char ch);
char lexer_advance(context_t *ctx, lexer_t *lexer);
char lexer_peek(context_t *ctx, lexer_t *lexer);
char lexer_peek_next(context_t *ctx, lexer_t *lexer);
void lexer_string(context_t *ctx, lexer_t *lexer);
void lexer_number(context_t *ctx, lexer_t *lexer);
void lexer_identifier(context_t *ctx, lexer_t *lexer);
void lexer_comment_block(context_t *ctx, lexer_t *lexer);
bool lexer_is_at_end(context_t *ctx, lexer_t *lexer);
bool lexer_is_alphanum(char ch);
token_type_t lexer_lookup_keyword(const char *s);

void parser_parse(context_t *ctx, parser_t *parser);
ast_node_t *parser_parse_declaration(context_t *ctx, parser_t *parser);
ast_node_t *parser_parse_var_declaration(context_t *ctx, parser_t *parser);
ast_node_t *parser_parse_fun_declaration(context_t *ctx, parser_t *parser);
ast_node_t *parser_parse_statement(context_t *ctx, parser_t *parser);
ast_node_t *parser_parse_block(context_t *ctx, parser_t *parser);
ast_node_t *parser_parse_if_stmt(context_t *ctx, parser_t *parser);
ast_node_t *parser_parse_while_stmt(context_t *ctx, parser_t *parser);
ast_node_t *parser_parse_for_stmt(context_t *ctx, parser_t *parser);
ast_node_t *parser_parse_print_stmt(context_t *ctx, parser_t *parser);
ast_node_t *parser_parse_expression(context_t *ctx, parser_t *parser);
ast_node_t *parser_parse_assignment(context_t *ctx, parser_t *parser);
ast_node_t *parser_parse_ternary(context_t *ctx, parser_t *parser);
ast_node_t *parser_parse_logical_or(context_t *ctx, parser_t *parser);
ast_node_t *parser_parse_logical_and(context_t *ctx, parser_t *parser);
ast_node_t *parser_parse_equality(context_t *ctx, parser_t *parser);
ast_node_t *parser_parse_comparison(context_t *ctx, parser_t *parser);
ast_node_t *parser_parse_term(context_t *ctx, parser_t *parser);
ast_node_t *parser_parse_factor(context_t *ctx, parser_t *parser);
ast_node_t *parser_parse_unary(context_t *ctx, parser_t *parser);
ast_node_t *parser_parse_call(context_t *ctx, parser_t *parser);
ast_node_t *parser_parse_primary(context_t *ctx, parser_t *parser);
ast_nodes_t *parser_parse_args(context_t *ctx, parser_t *parser);
ast_nodes_t *parser_parse_params(context_t *ctx, parser_t *parser);
ast_node_t *parser_parse_fun_expr(context_t *ctx, parser_t *parser);
ast_node_t *parser_parse_number(context_t *ctx, parser_t *parser);
ast_node_t *parser_parse_string(context_t *ctx, parser_t *parser);
ast_node_t *parser_parse_boolean(context_t *ctx, parser_t *parser);
ast_node_t *parser_parse_identifier(context_t *ctx, parser_t *parser);
ast_node_t *parser_create_node(arena_t *arena, node_type_t type, node_value_t value, usize start,
                               usize end);
token_t parser_peek(parser_t *parser);
token_t parser_previous(parser_t *parser);
token_t parser_advance(parser_t *parser);
void parser_consume(context_t *ctx, parser_t *parser, token_type_t type, const char *message);
bool parser_match_any(parser_t *parser, const token_type_t *types, usize count);
bool parser_is_eof(parser_t *parser);
void parser_synchronize(parser_t *parser);

bool interpret_comparison(token_type_t op, value_t a, value_t b);
eval_result_t interpret_binary(context_t *ctx, ast_node_t *node, environment_t *env);
eval_result_t interpret_unary(context_t *ctx, ast_node_t *node, environment_t *env);
eval_result_t interpret_ternary(context_t *ctx, ast_node_t *node, environment_t *env);
eval_result_t interpret(context_t *ctx, ast_node_t *node, environment_t *env);
eval_result_t create_eval_ok(void);
eval_result_t create_eval_error(void);
eval_result_t create_eval_break(void);
eval_result_t create_eval_return(void);
eval_result_t create_eval_result(eval_type_t type, value_t value);
value_t create_value(value_type_t type, value_as_t as);
value_t create_uninitialized_value(void);

bool is_truthy(value_t value);
bool is_equal(value_t a, value_t b);
bool check_number_operand(context_t *ctx, token_t tok, value_t child);
bool check_number_operands(context_t *ctx, token_t tok, value_t left, value_t right);
bool check_same_operands(context_t *ctx, token_t tok, value_t left, value_t right);

void report(usize line_idx, usize col_idx, const char *source_filename, const char *source,
            usize line_start, const char *message);
void report_runtime(usize line_idx, usize col_idx, const char *source_filename, const char *source,
                    usize line_start, const char *message);
void report_unexpected_token(context_t *ctx, token_t tok, const char *expected);
void report_error_at_token(context_t *ctx, token_t tok, const char *error_msg);
void run(context_t *ctx);
eval_result_t run_declarations(context_t *ctx, ast_nodes_t *declarations, environment_t *env);
const char *intern(arena_t *arena, const char *s);
err set_var_entry(arena_t *arena, var_pool_t *pool, const char *s, value_t value);
var_pool_entry_t *get_var_entry(var_pool_t *pool, const char *s);
var_pool_entry_t *get_var_in_scope(environment_t *env, const char *s);
err set_var_in_scope(arena_t *arena, environment_t *env, const char *s, value_t value);
environment_t *create_env(arena_t *arena, environment_t *parent);
const char *stringify_value(arena_t *arena, value_t data);

static bool had_error = false;
static bool had_runtime_error = false;

string_pool_t string_pool = { 0 };

#define parser_match(p, ...)                        \
    parser_match_any(                               \
        (p), (const token_type_t[]){ __VA_ARGS__ }, \
        (usize)(sizeof((const token_type_t[]){ __VA_ARGS__ }) / sizeof(token_type_t)))

int main(int argc, char **argv)
{
    if (argc < 3) {
        LOG(LOG_ERROR, "Usage: ./lox run file.lox");
        return 1;
    }

    if (strcmp(argv[1], "run") != 0 && strcmp(argv[1], "test") != 0) {
        LOG(LOG_ERROR, "Invalid command. Use `run` or `test`");
        return 1;
    }

    // TODO(fcasibu): implement better cli parsing later

    usize file_name_len = strlen(argv[2]);
    if (file_name_len < LANG_NAME_LEN ||
        strcmp(argv[2] + file_name_len - LANG_NAME_LEN, LANG_NAME) != 0) {
        LOG(LOG_ERROR, "Invalid source file '%s'", argv[2]);
        return 1;
    }

    const char *source_filename = argv[2];

    arena_t arena = { 0 };
    if (!arena_create(&arena, (usize)ARENA_SIZE)) {
        LOG(LOG_ERROR, "Failed to create arena");
        return 1;
    }

    const char *source = get_input(&arena, source_filename);

    if (!source) {
        arena_destroy(&arena);
        LOG(LOG_ERROR, "Failed to open file '%s'", source_filename);
        return 1;
    }

    context_t ctx = { .source_filename = source_filename,
                      .source = source,
                      .arena = &arena,
                      .source_len = strlen(source),
                      .cmd = argv[1] };

    run(&ctx);

    arena_destroy(&arena);

    if (had_error)
        return 65;

    if (had_runtime_error)
        return 70;

    return 0;
}

void run(context_t *ctx)
{
    lexer_t lexer = { 0 };
    arena_da_init(ctx->arena, &lexer.tokens, 256);

    lexer_tokenize(ctx, &lexer);

    parser_t parser = { 0 };
    parser.tokens = lexer.tokens.items;
    parser.tokens_size = lexer.tokens.size;
    parser.loop_depth = 0;
    parser.function_depth = 0;
    parser_parse(ctx, &parser);

    if (had_error)
        return;

    environment_t *global_env = create_env(ctx->arena, NULL);
    run_declarations(ctx, parser.root->value.prog_declarations, global_env);

    // print_ast(parser.root, 0);
}

eval_result_t run_declarations(context_t *ctx, ast_nodes_t *declarations, environment_t *env)
{
    for (usize i = 0; i < declarations->size; ++i) {
        eval_result_t result = interpret(ctx, declarations->items[i], env);

        if (result.type == EVAL_RETURN || result.type == EVAL_BREAK)
            return result;

        if (result.type == EVAL_ERROR) {
            had_runtime_error = true;
            return result;
        }

        if (declarations->items[i]->type == NODE_CALL ||
            declarations->items[i]->type == NODE_ASSIGN || strcmp(ctx->cmd, "run") == 0)
            continue;

        switch (result.value.type) {
        case VALUE_NUMBER: {
            printf("%g\n", result.value.as.number);

        } break;
        case VALUE_BOOL: {
            printf("%s\n", result.value.as.boolean ? "true" : "false");

        } break;
        case VALUE_STRING: {
            printf("%s\n", result.value.as.string);

        } break;
        case VALUE_NIL: {
            printf("nil\n");

        } break;
        default:
            break;
        }
    }

    return create_eval_ok();
}

void parser_parse(context_t *ctx, parser_t *parser)
{
    ast_nodes_t *prog_declarations = arena_alloc(ctx->arena, sizeof(*prog_declarations));
    arena_da_init(ctx->arena, prog_declarations, 256);

    while (parser->current_index < parser->tokens_size && !parser_is_eof(parser)) {
        ast_node_t *node = parser_parse_declaration(ctx, parser);

        if (!node)
            continue;

        arena_da_append(ctx->arena, prog_declarations, node);
    }

    parser->root = parser_create_node(
        ctx->arena, NODE_PROGRAM, (node_value_t){ .prog_declarations = prog_declarations }, 0,
        prog_declarations->size > 0 ? prog_declarations->items[prog_declarations->size - 1]->end :
                                      0);
}

ast_node_t *parser_parse_declaration(context_t *ctx, parser_t *parser)
{
    token_t tok = parser_peek(parser);

    switch (tok.type) {
    case VAR: {
        ast_node_t *node = parser_parse_var_declaration(ctx, parser);

        if (!node)
            parser_synchronize(parser);

        parser_consume(ctx, parser, SEMICOLON, "Expected ';' at the end of 'var' declaration");

        return node;
    }
    case FUN: {
        parser->function_depth += 1;
        ast_node_t *node = parser_parse_fun_declaration(ctx, parser);
        parser->function_depth -= 1;

        if (!node)
            parser_synchronize(parser);

        if (parser_match(parser, SEMICOLON))
            parser_advance(parser);

        return node;
    }
    default:
        return parser_parse_statement(ctx, parser);
    }
}

ast_node_t *parser_parse_var_declaration(context_t *ctx, parser_t *parser)
{
    token_t tok = parser_advance(parser);
    if (tok.type != VAR) {
        report_unexpected_token(ctx, parser_previous(parser), "var");
        return NULL;
    }

    ast_node_t *identifier = parser_parse_identifier(ctx, parser);

    if (!identifier)
        return NULL;

    if (parser_match(parser, SEMICOLON))
        return parser_create_node(
            ctx->arena, NODE_VAR,
            (node_value_t){ .var_decl.identifier = identifier,
                            .var_decl.expression = parser_create_node(
                                ctx->arena, NODE_UNINITIALIZED, (node_value_t){}, 0, 0) },
            tok.cursor, identifier->end);

    if (parser_advance(parser).type != EQUAL) {
        report_unexpected_token(ctx, parser_previous(parser), "=");
        return NULL;
    }

    ast_node_t *expression = parser_parse_expression(ctx, parser);

    if (!expression) {
        report_error_at_token(ctx, parser_previous(parser), "Expected expression after '='");
        return NULL;
    }

    return parser_create_node(ctx->arena, NODE_VAR,
                              (node_value_t){ .var_decl.identifier = identifier,
                                              .var_decl.expression = expression },
                              tok.cursor, identifier->end);
}

ast_node_t *parser_parse_fun_declaration(context_t *ctx, parser_t *parser)
{
    token_t tok = parser_advance(parser);
    if (tok.type != FUN) {
        report_unexpected_token(ctx, parser_previous(parser), "fun");
        return NULL;
    }

    ast_node_t *name = parser_parse_identifier(ctx, parser);
    if (!name)
        return NULL;

    if (parser_advance(parser).type != LEFT_PAREN) {
        report_unexpected_token(ctx, parser_previous(parser), "(");
        return NULL;
    }

    ast_nodes_t *params = NULL;

    if (!parser_match(parser, RIGHT_PAREN)) {
        params = parser_parse_params(ctx, parser);
    }

    if (parser_advance(parser).type != RIGHT_PAREN) {
        report_error_at_token(ctx, parser_previous(parser), "Expected ')' after parameters.");
        return NULL;
    }

    ast_node_t *body = parser_parse_block(ctx, parser);

    if (!body) {
        report_error_at_token(ctx, tok, "Expected function body");
        return NULL;
    }

    node_value_t value = { .fun_decl = { .name = name, .params = params, .body = body } };
    return parser_create_node(ctx->arena, NODE_FUN, value, tok.cursor, body->end);
}

ast_node_t *parser_parse_statement(context_t *ctx, parser_t *parser)
{
    token_t tok = parser_peek(parser);

    switch (tok.type) {
    case RETURN: {
        parser_advance(parser);

        if (parser->function_depth == 0)
            report_error_at_token(ctx, parser_previous(parser),
                                  "Syntax Error: Invalid return statement");

        ast_node_t *node = parser_parse_expression(ctx, parser);

        if (!node || node->type != NODE_FUN)
            parser_consume(ctx, parser, SEMICOLON, "Expected ';' after return");

        return parser_create_node(ctx->arena, NODE_RETURN, (node_value_t){ .return_value = node },
                                  tok.cursor, node ? node->end : tok.cursor + tok.lexeme_len - 1);
    }

    case BREAK: {
        parser_advance(parser);

        if (parser->loop_depth == 0)
            report_error_at_token(ctx, parser_previous(parser),
                                  "Syntax Error: Invalid break statement");

        parser_consume(ctx, parser, SEMICOLON, "Expected ';' after break");

        return parser_create_node(ctx->arena, NODE_BREAK, (node_value_t){}, tok.cursor, tok.cursor);
    }

    case FOR: {
        parser->loop_depth += 1;
        ast_node_t *node = parser_parse_for_stmt(ctx, parser);
        parser->loop_depth -= 1;

        return node;
    }

    case WHILE: {
        parser->loop_depth += 1;
        ast_node_t *node = parser_parse_while_stmt(ctx, parser);
        parser->loop_depth -= 1;

        return node;
    }

    case IF: {
        return parser_parse_if_stmt(ctx, parser);
    }

    case ELSE: {
        parser_advance(parser);
        report(tok.line - 1, tok.col - 1, ctx->source_filename, ctx->source, tok.line_start,
               "Missing 'if' statement");
        return NULL;
    }

    case PRINT: {
        ast_node_t *node = parser_parse_print_stmt(ctx, parser);

        parser_consume(ctx, parser, SEMICOLON, "Expected ';' after print statement");

        return node;
    }

    case LEFT_BRACE: {
        return parser_parse_block(ctx, parser);
    }

    default: {
        ast_node_t *node = parser_parse_expression(ctx, parser);

        if (!node) {
            parser_synchronize(parser);
            return NULL;
        }

        parser_consume(ctx, parser, SEMICOLON, "Expected ';' after expression");

        return node;
    }
    }
}

ast_node_t *parser_parse_block(context_t *ctx, parser_t *parser)
{
    token_t tok = parser_advance(parser);
    if (tok.type != LEFT_BRACE) {
        report_unexpected_token(ctx, parser_previous(parser), "{");
        return NULL;
    }

    ast_nodes_t *block_declarations = arena_alloc(ctx->arena, sizeof(*block_declarations));
    arena_da_init(ctx->arena, block_declarations, 256);

    while (!parser_match(parser, RIGHT_BRACE) && !parser_is_eof(parser)) {
        ast_node_t *node = parser_parse_declaration(ctx, parser);

        if (!node)
            continue;

        arena_da_append(ctx->arena, block_declarations, node);
    }

    token_t end_tok = parser_advance(parser);
    if (end_tok.type != RIGHT_BRACE) {
        report(end_tok.line - 1, end_tok.col - 1, ctx->source_filename, ctx->source,
               end_tok.line_start, "Unterminated block.");
        return NULL;
    }

    return parser_create_node(ctx->arena, NODE_BLOCK,
                              (node_value_t){ .block_declarations = block_declarations },
                              tok.cursor, end_tok.cursor);
}

ast_node_t *parser_parse_if_stmt(context_t *ctx, parser_t *parser)
{
    token_t tok = parser_advance(parser);
    if (tok.type != IF) {
        report_unexpected_token(ctx, parser_previous(parser), "if");
        return NULL;
    }

    token_t left_paren_tok = parser_advance(parser);
    if (left_paren_tok.type != LEFT_PAREN) {
        report_unexpected_token(ctx, parser_previous(parser), "(");
        return NULL;
    }

    ast_node_t *condition = parser_parse_expression(ctx, parser);

    if (!condition) {
        report_error_at_token(ctx, parser_previous(parser),
                              "Expected if statement condition within parentheses");
        return NULL;
    }

    if (parser_advance(parser).type != RIGHT_PAREN) {
        report_error_at_token(ctx, left_paren_tok, "Expected closing parenthesis");
        return NULL;
    }

    ast_node_t *then_branch = parser_parse_statement(ctx, parser);

    if (!then_branch) {
        report_error_at_token(ctx, parser_previous(parser), "Expected statement after 'if (cond)'");
        return NULL;
    }

    ast_node_t *else_branch = NULL;

    if (parser_match(parser, ELSE)) {
        token_t else_token = parser_advance(parser);
        else_branch = parser_parse_statement(ctx, parser);

        if (!else_branch) {
            report_error_at_token(ctx, else_token, "Expected statement after 'else'");

            return NULL;
        }
    }

    return parser_create_node(ctx->arena, NODE_IF,
                              (node_value_t){ .if_stmt = {
                                                  .condition = condition,
                                                  .then_branch = then_branch,
                                                  .else_branch = else_branch,
                                              } },
                                tok.cursor,
                                else_branch ? else_branch->end : then_branch->end
                              );
}

ast_node_t *parser_parse_while_stmt(context_t *ctx, parser_t *parser)
{
    token_t tok = parser_advance(parser);
    if (tok.type != WHILE) {
        report_unexpected_token(ctx, parser_previous(parser), "while");
        return NULL;
    }

    token_t left_paren_tok = parser_advance(parser);
    if (left_paren_tok.type != LEFT_PAREN) {
        report_unexpected_token(ctx, parser_previous(parser), "(");
        return NULL;
    }

    ast_node_t *condition = parser_parse_expression(ctx, parser);

    if (!condition) {
        report_error_at_token(ctx, parser_previous(parser),
                              "Expected if statement condition within parentheses");
        return NULL;
    }

    if (parser_advance(parser).type != RIGHT_PAREN) {
        report_error_at_token(ctx, left_paren_tok, "Expected closing parenthesis");
        return NULL;
    }

    ast_node_t *body = parser_parse_statement(ctx, parser);

    if (!body) {
        report_error_at_token(ctx, parser_previous(parser),
                              "Expected statement after 'while (cond)'");
        return NULL;
    }

    return parser_create_node(ctx->arena, NODE_WHILE,
                              (node_value_t){ .while_stmt = {
                                                  .condition = condition,
                                                  .body = body,
                                              } },
                                tok.cursor,
                                body->end
                              );
}

ast_node_t *parser_parse_for_stmt(context_t *ctx, parser_t *parser)
{
    token_t tok = parser_advance(parser);
    if (tok.type != FOR) {
        report_unexpected_token(ctx, parser_previous(parser), "for");
        return NULL;
    }

    token_t left_paren_tok = parser_advance(parser);
    if (left_paren_tok.type != LEFT_PAREN) {
        report_unexpected_token(ctx, parser_previous(parser), "(");
        return NULL;
    }

    ast_node_t *initializer;

    if (parser_match(parser, VAR)) {
        initializer = parser_parse_var_declaration(ctx, parser);
    } else {
        initializer = parser_parse_expression(ctx, parser);
    }

    if (!initializer && !parser_match(parser, SEMICOLON)) {
        parser_advance(parser);
        report_error_at_token(ctx, parser_previous(parser), "Expected for loop initializer or ';'");
        return NULL;
    }

    if (parser_advance(parser).type != SEMICOLON) {
        report_unexpected_token(ctx, parser_previous(parser), ";");
        return NULL;
    }

    ast_node_t *condition = parser_parse_expression(ctx, parser);

    if (!condition && !parser_match(parser, SEMICOLON)) {
        parser_advance(parser);
        report_error_at_token(ctx, parser_previous(parser), "Expected for loop condition or ';'");
        return NULL;
    }

    if (parser_advance(parser).type != SEMICOLON) {
        report_unexpected_token(ctx, parser_previous(parser), ";");
        return NULL;
    }

    ast_node_t *increment = parser_parse_expression(ctx, parser);

    if (parser_advance(parser).type != RIGHT_PAREN) {
        report_error_at_token(ctx, left_paren_tok, "Expected closing parenthesis");
        return NULL;
    }

    ast_node_t *body = parser_parse_statement(ctx, parser);

    if (!body) {
        report_error_at_token(ctx, parser_previous(parser), "Expected body after 'for'");
        return NULL;
    }

    return parser_create_node(ctx->arena, NODE_FOR,
                              (node_value_t){ .for_stmt = { .initializer = initializer,
                                                            .condition = condition,
                                                            .increment = increment,
                                                            .body = body,
                              } },
                              tok.cursor, body->end);
}

ast_node_t *parser_parse_print_stmt(context_t *ctx, parser_t *parser)
{
    token_t tok = parser_advance(parser);
    if (tok.type != PRINT) {
        report_unexpected_token(ctx, parser_previous(parser), "print");
        return NULL;
    }

    ast_node_t *expression = parser_parse_expression(ctx, parser);

    if (!expression) {
        report_unexpected_token(ctx, parser_previous(parser), "expression");
        return NULL;
    }

    return parser_create_node(ctx->arena, NODE_PRINT, ((node_value_t){ .print_stmt = expression }),
                              tok.cursor, expression->end);
}

ast_node_t *parser_parse_expression(context_t *ctx, parser_t *parser)
{
    ast_node_t *left = parser_parse_assignment(ctx, parser);

    if (!left)
        return NULL;

    while (!parser_is_eof(parser) && parser_match(parser, COMMA)) {
        token_t op = parser_advance(parser);

        ast_node_t *right = parser_parse_expression(ctx, parser);

        if (!right)
            return NULL;

        node_value_t node_value = {
            .binary = { .op = op, .left = left, .right = right },
        };
        left = parser_create_node(ctx->arena, NODE_BINARY, node_value, left->start, right->start);
    }

    return left;
}

ast_node_t *parser_parse_assignment(context_t *ctx, parser_t *parser)
{
    ast_node_t *left = parser_parse_ternary(ctx, parser);

    if (!left)
        return NULL;

    if (parser_match(parser, EQUAL)) {
        if (left->type != NODE_IDENTIFIER) {
            report_error_at_token(ctx, parser_previous(parser), "Invalid assignment target.");
            return NULL;
        }

        parser_consume(ctx, parser, EQUAL, "Expected '='");
        ast_node_t *right = parser_parse_assignment(ctx, parser);

        if (!right) {
            report_error_at_token(ctx, parser_previous(parser), "Expected expression after '='");
            return NULL;
        }

        return parser_create_node(ctx->arena, NODE_ASSIGN,
                                  (node_value_t){ .assign.identifier = left,
                                                  .assign.expression = right },
                                  left->start, right->end);
    }

    return left;
}

ast_node_t *parser_parse_ternary(context_t *ctx, parser_t *parser)
{
    ast_node_t *condition = parser_parse_logical_or(ctx, parser);

    if (!parser_match(parser, QUESTION_MARK))
        return condition;

    parser_advance(parser);

    if (!condition) {
        report_error_at_token(ctx, parser_previous(parser), "Expected condition before '?'");
        return NULL;
    }

    ast_node_t *true_branch = parser_parse_expression(ctx, parser);
    if (!true_branch) {
        report_unexpected_token(ctx, parser_previous(parser), "expression");
        return NULL;
    }

    if (!parser_match(parser, COLON)) {
        report_error_at_token(ctx, parser_previous(parser),
                              "Expected false branch expression for ternary");
        return NULL;
    }

    parser_advance(parser);

    ast_node_t *false_branch = parser_parse_ternary(ctx, parser);
    if (!false_branch) {
        report_error_at_token(ctx, parser_previous(parser), "Expected expression after ':'");
        return NULL;
    }

    node_value_t value = { .ternary = { .condition = condition,
                                        .true_branch = true_branch,
                                        .false_branch = false_branch } };

    return parser_create_node(ctx->arena, NODE_TERNARY, value, condition->start, false_branch->end);
}

ast_node_t *parser_parse_logical_or(context_t *ctx, parser_t *parser)
{
    token_t tok = parser_peek(parser);
    ast_node_t *left = parser_parse_logical_and(ctx, parser);

    while (!parser_is_eof(parser) && parser_match(parser, OR)) {
        if (!left) {
            report_error_at_token(ctx, tok, "Missing left-hand expression");

            return NULL;
        }

        token_t op = parser_advance(parser);

        ast_node_t *right = parser_parse_logical_and(ctx, parser);

        if (!right) {
            report_error_at_token(ctx, parser_previous(parser), "Missing right-hand expression");

            return NULL;
        }

        node_value_t value = { .binary = { .left = left, .op = op, .right = right } };
        left = parser_create_node(ctx->arena, NODE_BINARY, value, left->start, right->end);
    }

    return left;
}

ast_node_t *parser_parse_logical_and(context_t *ctx, parser_t *parser)
{
    token_t tok = parser_peek(parser);
    ast_node_t *left = parser_parse_equality(ctx, parser);

    while (!parser_is_eof(parser) && parser_match(parser, AND)) {
        if (!left) {
            report_error_at_token(ctx, tok, "Missing left-hand expression");

            return NULL;
        }

        token_t op = parser_advance(parser);

        ast_node_t *right = parser_parse_equality(ctx, parser);

        if (!right) {
            report_error_at_token(ctx, parser_previous(parser), "Missing right-hand expression");

            return NULL;
        }

        node_value_t value = { .binary = { .left = left, .op = op, .right = right } };
        left = parser_create_node(ctx->arena, NODE_BINARY, value, left->start, right->end);
    }

    return left;
}

ast_node_t *parser_parse_equality(context_t *ctx, parser_t *parser)
{
    token_t tok = parser_peek(parser);
    ast_node_t *left = parser_parse_comparison(ctx, parser);

    while (!parser_is_eof(parser) && parser_match(parser, EQUAL_EQUAL, BANG_EQUAL)) {
        if (!left) {
            report_error_at_token(ctx, tok, "Equality operators must have a left-hand operand");

            return NULL;
        }

        token_t op = parser_advance(parser);

        ast_node_t *right = parser_parse_comparison(ctx, parser);

        if (!right) {
            report_error_at_token(ctx, parser_previous(parser),
                                  "Equality operators must have a right-hand operand");

            return NULL;
        }

        node_value_t value = { .binary = { .left = left, .op = op, .right = right } };
        left = parser_create_node(ctx->arena, NODE_BINARY, value, left->start, right->end);
    }

    return left;
}

ast_node_t *parser_parse_comparison(context_t *ctx, parser_t *parser)
{
    token_t tok = parser_peek(parser);
    ast_node_t *left = parser_parse_term(ctx, parser);

    while (!parser_is_eof(parser) &&
           parser_match(parser, LESS, LESS_EQUAL, GREATER, GREATER_EQUAL)) {
        if (!left) {
            report_error_at_token(ctx, tok, "Comparison operators must have a left-hand operand");

            return NULL;
        }

        token_t op = parser_advance(parser);

        ast_node_t *right = parser_parse_term(ctx, parser);

        if (!right) {
            report_error_at_token(ctx, parser_previous(parser),
                                  "Comparison operators must have a right-hand operand");

            return NULL;
        }

        node_value_t value = { .binary = { .left = left, .op = op, .right = right } };
        left = parser_create_node(ctx->arena, NODE_BINARY, value, left->start, right->end);
    }

    return left;
}
ast_node_t *parser_parse_term(context_t *ctx, parser_t *parser)
{
    token_t tok = parser_peek(parser);
    ast_node_t *left = parser_parse_factor(ctx, parser);

    while (!parser_is_eof(parser) && parser_match(parser, PLUS, MINUS)) {
        if (!left) {
            report_error_at_token(ctx, tok,
                                  "Addition/subtraction operators must have a left-hand operand");

            return NULL;
        }
        token_t op = parser_advance(parser);

        ast_node_t *right = parser_parse_factor(ctx, parser);

        if (!right) {
            report_error_at_token(ctx, parser_previous(parser),
                                  "Addition/subtraction operators must have a right-hand operand");

            return NULL;
        }

        node_value_t value = { .binary = { .left = left, .op = op, .right = right } };
        left = parser_create_node(ctx->arena, NODE_BINARY, value, left->start, right->end);
    }

    return left;
}
ast_node_t *parser_parse_factor(context_t *ctx, parser_t *parser)
{
    token_t tok = parser_peek(parser);
    ast_node_t *left = parser_parse_unary(ctx, parser);

    while (!parser_is_eof(parser) && parser_match(parser, STAR, SLASH)) {
        if (!left) {
            report_error_at_token(
                ctx, tok, "Multiplication/division operators must have a left-hand operand");

            return NULL;
        }
        token_t op = parser_advance(parser);

        ast_node_t *right = parser_parse_unary(ctx, parser);

        if (!right) {
            report_error_at_token(
                ctx, parser_previous(parser),
                "Multiplication/division operators must have a right-hand operand");

            return NULL;
        }

        node_value_t value = { .binary = { .left = left, .op = op, .right = right } };
        left = parser_create_node(ctx->arena, NODE_BINARY, value, left->start, right->end);
    }

    return left;
}

ast_node_t *parser_parse_unary(context_t *ctx, parser_t *parser)
{
    token_t tok = parser_peek(parser);

    switch (tok.type) {
    case PLUS:
    case MINUS: {
        parser_advance(parser);
        ast_node_t *node = parser_parse_call(ctx, parser);
        if (!node) {
            report_error_at_token(ctx, tok, "Unary operator must have an operand");
            return NULL;
        }

        node_value_t value = { .unary = { .op = tok, .child = node } };
        return parser_create_node(ctx->arena, NODE_UNARY, value, node->start, node->end);
    };

    case BANG: {
        parser_advance(parser);

        bool is_bang = parser_match(parser, BANG);
        ast_node_t *node = is_bang ? parser_parse_unary(ctx, parser) :
                                     parser_parse_call(ctx, parser);

        if (!node) {
            report_error_at_token(ctx, tok, "Unary operator must have an operand");
            return NULL;
        }

        node_value_t value = { .unary = { .op = tok, .child = node } };
        return parser_create_node(ctx->arena, NODE_UNARY, value, node->start, node->end);
    }

    default:
        break;
    }

    return parser_parse_call(ctx, parser);
}

ast_node_t *parser_parse_call(context_t *ctx, parser_t *parser)
{
    token_t callee_tok = parser_peek(parser);
    ast_node_t *left = parser_parse_primary(ctx, parser);

    while (parser_match(parser, LEFT_PAREN)) {
        parser_advance(parser);

        ast_nodes_t *args = parser_parse_args(ctx, parser);

        token_t tok = parser_advance(parser);
        if (tok.type != RIGHT_PAREN)
            report_error_at_token(ctx, tok, "Expected ')' after arguments.");

        if (args && args->size > 255)
            report_error_at_token(ctx, callee_tok, "Can't have more than 255 arguments.");

        source_loc_t loc = {
            .line = callee_tok.line,
            .col = callee_tok.col,
            .line_start = callee_tok.line_start,
            .cursor = callee_tok.cursor,
        };

        node_value_t value = { .call = {
                                   .callee = left,
                                   .args = args,
                                   .loc = loc,
                               } };

        left = parser_create_node(ctx->arena, NODE_CALL, value, left->start, tok.cursor);
    }

    return left;
}

ast_node_t *parser_parse_primary(context_t *ctx, parser_t *parser)
{
    if (parser_match(parser, TRUE, FALSE))
        return parser_parse_boolean(ctx, parser);

    if (parser_match(parser, NIL)) {
        parser_advance(parser);

        return parser_create_node(ctx->arena, NODE_NIL, (node_value_t){}, 0, 0);
    }

    if (parser_match(parser, IDENTIFIER))
        return parser_parse_identifier(ctx, parser);

    if (parser_match(parser, LEFT_PAREN)) {
        parser_advance(parser);
        ast_node_t *node = parser_parse_expression(ctx, parser);

        if (!node) {
            report_error_at_token(ctx, parser_previous(parser),
                                  "Expected expression inside parentheses");

            return NULL;
        }

        if (parser_advance(parser).type != RIGHT_PAREN) {
            report_unexpected_token(ctx, parser_previous(parser), ")");

            return NULL;
        }

        return node;
    }

    if (parser_match(parser, STRING))
        return parser_parse_string(ctx, parser);

    if (parser_match(parser, DOT)) {
        report_unexpected_token(ctx, parser_peek(parser), "number");

        return NULL;
    }

    if (parser_match(parser, NUMBER))
        return parser_parse_number(ctx, parser);

    if (parser_match(parser, FUN)) {
        parser->function_depth += 1;
        ast_node_t *node = parser_parse_fun_expr(ctx, parser);
        parser->function_depth -= 1;

        return node;
    }

    return NULL;
}

ast_nodes_t *parser_parse_args(context_t *ctx, parser_t *parser)
{
    ast_node_t *node = parser_parse_assignment(ctx, parser);
    if (!node)
        return NULL;

    ast_nodes_t *args = arena_alloc(ctx->arena, sizeof(*args));
    arena_da_init(ctx->arena, args, 256);
    arena_da_append(ctx->arena, args, node);

    while (!parser_is_eof(parser) && parser_match(parser, COMMA)) {
        parser_advance(parser);

        ast_node_t *arg = parser_parse_assignment(ctx, parser);
        if (!arg)
            return NULL;

        arena_da_append(ctx->arena, args, arg);
    }

    return args;
}

ast_nodes_t *parser_parse_params(context_t *ctx, parser_t *parser)
{
    ast_node_t *node = parser_parse_identifier(ctx, parser);
    if (!node)
        return NULL;

    ast_nodes_t *params = arena_alloc(ctx->arena, sizeof(*params));
    arena_da_init(ctx->arena, params, 256);
    arena_da_append(ctx->arena, params, node);

    while (!parser_is_eof(parser) && parser_match(parser, COMMA)) {
        parser_advance(parser);

        // TODO(fcasibu): not really stated in the chapter
        // but do we want to handle param name collision?
        ast_node_t *param = parser_parse_identifier(ctx, parser);
        if (!param)
            return NULL;

        arena_da_append(ctx->arena, params, param);
    }

    return params;
}

ast_node_t *parser_parse_fun_expr(context_t *ctx, parser_t *parser)
{
    token_t tok = parser_advance(parser);
    if (tok.type != FUN) {
        report_unexpected_token(ctx, parser_previous(parser), "fun");
        return NULL;
    }

    if (parser_advance(parser).type != LEFT_PAREN) {
        report_unexpected_token(ctx, parser_previous(parser), "(");
        return NULL;
    }

    ast_nodes_t *params = NULL;

    if (!parser_match(parser, RIGHT_PAREN)) {
        params = parser_parse_params(ctx, parser);
    }

    if (parser_advance(parser).type != RIGHT_PAREN) {
        report_error_at_token(ctx, parser_previous(parser), "Expected ')' after parameters.");
        return NULL;
    }

    ast_node_t *body = parser_parse_block(ctx, parser);

    if (!body) {
        report_error_at_token(ctx, tok, "Expected function body");
        return NULL;
    }

    node_value_t value = { .fun_decl = { .name = NULL, .params = params, .body = body } };
    return parser_create_node(ctx->arena, NODE_FUN, value, tok.cursor, body->end);
}

ast_node_t *parser_parse_number(context_t *ctx, parser_t *parser)
{
    token_t tok = parser_advance(parser);
    if (tok.type != NUMBER) {
        report_unexpected_token(ctx, parser_previous(parser), "number");
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

    node_value_t node_value = { .number_literal = value };
    return parser_create_node(ctx->arena, NODE_NUMBER, node_value, tok.cursor,
                              tok.cursor + tok.lexeme_len);
}

ast_node_t *parser_parse_string(context_t *ctx, parser_t *parser)
{
    token_t tok = parser_advance(parser);
    if (tok.type != STRING) {
        report_unexpected_token(ctx, parser_previous(parser), "string");
        return NULL;
    }

    char tmp[tok.lexeme_len - 1];
    // remove quotes
    memcpy(tmp, tok.lexeme + 1, tok.lexeme_len - 2);
    tmp[tok.lexeme_len - 2] = '\0';

    const char *value = intern(ctx->arena, tmp);
    if (!value)
        return NULL;

    node_value_t node_value = { .string_literal = value };
    return parser_create_node(ctx->arena, NODE_STRING, node_value, tok.cursor, tok.lexeme_len);
}

ast_node_t *parser_parse_boolean(context_t *ctx, parser_t *parser)
{
    token_t tok = parser_advance(parser);
    if (tok.type != TRUE && tok.type != FALSE) {
        report_unexpected_token(ctx, parser_previous(parser), "boolean");
        return NULL;
    }

    char tmp[tok.lexeme_len + 1];
    memcpy(tmp, tok.lexeme, tok.lexeme_len);
    tmp[tok.lexeme_len] = '\0';

    node_value_t node_value = { .boolean_literal = strcmp(tmp, "true") == 0 };
    return parser_create_node(ctx->arena, NODE_BOOL, node_value, tok.cursor, tok.lexeme_len);
}
ast_node_t *parser_parse_identifier(context_t *ctx, parser_t *parser)
{
    token_t tok = parser_advance(parser);
    if (tok.type != IDENTIFIER) {
        report_unexpected_token(ctx, parser_previous(parser), "identifier");
        return NULL;
    }

    char tmp[tok.lexeme_len + 1];
    memcpy(tmp, tok.lexeme, tok.lexeme_len);
    tmp[tok.lexeme_len] = '\0';

    const char *value = intern(ctx->arena, tmp);
    if (!value)
        return NULL;

    node_value_t node_value = { 
        .identifier = { 
            .name = value,
            .loc = {
                .line = tok.line,
                .col = tok.col,
                .line_start = tok.line_start,
                .cursor = tok.cursor,
            },
        }
    };
    return parser_create_node(ctx->arena, NODE_IDENTIFIER, node_value, tok.cursor, tok.lexeme_len);
}

void lexer_tokenize(context_t *ctx, lexer_t *lexer)
{
    usize source_len = strlen(ctx->source);

    while (lexer->cursor < source_len) {
        usize line_start = lexer->line_start;
        usize col_idx = lexer->col_idx;
        usize line_idx = lexer->line_idx;
        usize cursor = lexer->cursor;
        const char *lexeme = ctx->source + lexer->cursor;

        char ch = lexer_advance(ctx, lexer);

        if (isspace(ch))
            continue;

        switch (ch) {
        case '(': {
            lexer_append_token(ctx, lexer, LEFT_PAREN, lexeme, 1, line_idx, col_idx, cursor,
                               line_start);
        } break;

        case ')': {
            lexer_append_token(ctx, lexer, RIGHT_PAREN, lexeme, 1, line_idx, col_idx, cursor,
                               line_start);
        } break;

        case '{': {
            lexer_append_token(ctx, lexer, LEFT_BRACE, lexeme, 1, line_idx, col_idx, cursor,
                               line_start);
        } break;

        case '}': {
            lexer_append_token(ctx, lexer, RIGHT_BRACE, lexeme, 1, line_idx, col_idx, cursor,
                               line_start);
        } break;

        case ',': {
            lexer_append_token(ctx, lexer, COMMA, lexeme, 1, line_idx, col_idx, cursor, line_start);
        } break;

        case '.': {
            lexer_append_token(ctx, lexer, DOT, lexeme, 1, line_idx, col_idx, cursor, line_start);
        } break;

        case '-': {
            lexer_append_token(ctx, lexer, MINUS, lexeme, 1, line_idx, col_idx, cursor, line_start);
        } break;

        case '+': {
            lexer_append_token(ctx, lexer, PLUS, lexeme, 1, line_idx, col_idx, cursor, line_start);
        } break;

        case ';': {
            lexer_append_token(ctx, lexer, SEMICOLON, lexeme, 1, line_idx, col_idx, cursor,
                               line_start);
        } break;

        case '*': {
            lexer_append_token(ctx, lexer, STAR, lexeme, 1, line_idx, col_idx, cursor, line_start);
        } break;

        case '?': {
            lexer_append_token(ctx, lexer, QUESTION_MARK, lexeme, 1, line_idx, col_idx, cursor,
                               line_start);
        } break;

        case ':': {
            lexer_append_token(ctx, lexer, COLON, lexeme, 1, line_idx, col_idx, cursor, line_start);
        } break;

        case '!': {
            bool is_match = lexer_match(ctx, lexer, '=');
            lexer_append_token(ctx, lexer, is_match ? BANG_EQUAL : BANG, lexeme, is_match ? 2 : 1,
                               line_idx, col_idx, cursor, line_start);
        } break;

        case '=': {
            bool is_match = lexer_match(ctx, lexer, '=');
            lexer_append_token(ctx, lexer, is_match ? EQUAL_EQUAL : EQUAL, lexeme, is_match ? 2 : 1,
                               line_idx, col_idx, cursor, line_start);
        } break;

        case '>': {
            bool is_match = lexer_match(ctx, lexer, '=');
            lexer_append_token(ctx, lexer, is_match ? GREATER_EQUAL : GREATER, lexeme,
                               is_match ? 2 : 1, line_idx, col_idx, cursor, line_start);
        } break;

        case '<': {
            bool is_match = lexer_match(ctx, lexer, '=');
            lexer_append_token(ctx, lexer, is_match ? LESS_EQUAL : LESS, lexeme, is_match ? 2 : 1,
                               line_idx, col_idx, cursor, line_start);
        } break;

        case '/': {
            if (lexer_match(ctx, lexer, '/')) {
                while (lexer_peek(ctx, lexer) != '\n' && !lexer_is_at_end(ctx, lexer))
                    lexer_advance(ctx, lexer);
            } else if (lexer_match(ctx, lexer, '*')) {
                lexer_comment_block(ctx, lexer);
            } else {
                lexer_append_token(ctx, lexer, SLASH, lexeme, 1, line_idx, col_idx, cursor,
                                   line_start);
            }
        } break;

        case '"': {
            lexer_string(ctx, lexer);
        } break;

        default: {
            if (isdigit(ch)) {
                lexer_number(ctx, lexer);
            } else if (lexer_is_alphanum(ch)) {
                lexer_identifier(ctx, lexer);
            } else {
                report(line_idx, col_idx, ctx->source_filename, ctx->source, line_start,
                       "Unexpected character.");
            }
        } break;
        }
    }

    lexer_append_token(ctx, lexer, END_OF_FILE, "", 0, lexer->line_idx, 0, lexer->cursor,
                       lexer->cursor);
}

void lexer_string(context_t *ctx, lexer_t *lexer)
{
    usize line_start = lexer->line_start;
    usize start = lexer->cursor - 1;
    usize start_col_idx = lexer->col_idx - 1; // we advanced once
    usize start_line_idx = lexer->line_idx;

    while (!lexer_is_at_end(ctx, lexer) && lexer_peek(ctx, lexer) != '"')
        lexer_advance(ctx, lexer);

    if (lexer_is_at_end(ctx, lexer)) {
        report(start_line_idx, start_col_idx, ctx->source_filename, ctx->source, line_start,
               "Unterminated string.");
        return;
    }

    assert(lexer_peek(ctx, lexer) == '"');
    lexer_advance(ctx, lexer);

    usize string_len = lexer->cursor - start;

    lexer_append_token(ctx, lexer, STRING, ctx->source + start, string_len, start_line_idx,
                       start_col_idx, start, line_start);
}

void lexer_number(context_t *ctx, lexer_t *lexer)
{
    usize line_start = lexer->line_start;
    usize start = lexer->cursor - 1; // we advanced once
    usize start_col_idx = lexer->col_idx - 1; // we advanced once
    usize start_line_idx = lexer->line_idx;

    while (isdigit(lexer_peek(ctx, lexer)))
        lexer_advance(ctx, lexer);

    if (lexer_peek(ctx, lexer) == '.') {
        lexer_advance(ctx, lexer);

        usize string_len = lexer->cursor - start;

        if (!isdigit(lexer_peek(ctx, lexer))) {
            report(start_line_idx, start_col_idx + string_len, ctx->source_filename, ctx->source,
                   line_start, "Invalid numeric literal: expected digits after '.'.");
            return;
        }

        while (!lexer_is_at_end(ctx, lexer) && isdigit(lexer_peek(ctx, lexer)))
            lexer_advance(ctx, lexer);
    }

    usize string_len = lexer->cursor - start;

    if (lexer_is_alphanum(lexer_peek(ctx, lexer))) {
        report(start_line_idx, start_col_idx + string_len, ctx->source_filename, ctx->source,
               line_start, "Invalid numeric literal.");
        return;
    }

    lexer_append_token(ctx, lexer, NUMBER, ctx->source + start, string_len, start_line_idx,
                       start_col_idx, start, line_start);
}

void lexer_identifier(context_t *ctx, lexer_t *lexer)
{
    usize line_start = lexer->line_start;
    usize start = lexer->cursor - 1; // we advanced once
    usize start_col_idx = lexer->col_idx - 1; // we advanced once
    usize start_line_idx = lexer->line_idx;

    while (lexer_is_alphanum(lexer_peek(ctx, lexer)))
        lexer_advance(ctx, lexer);

    usize string_len = lexer->cursor - start;
    char tmp[string_len + 1];
    memcpy(tmp, ctx->source + start, string_len);
    tmp[string_len] = '\0';

    token_type_t type = lexer_lookup_keyword(tmp);

    lexer_append_token(ctx, lexer, type, ctx->source + start, string_len, start_line_idx,
                       start_col_idx, start, line_start);
}

void lexer_comment_block(context_t *ctx, lexer_t *lexer)
{
    usize line_start = lexer->line_start;
    usize start_col_idx = lexer->col_idx - 2; // we advanced twice
    usize start_line_idx = lexer->line_idx;

    while (!lexer_is_at_end(ctx, lexer)) {
        if (lexer_peek(ctx, lexer) == '*' && lexer_peek_next(ctx, lexer) == '/')
            break;

        lexer_advance(ctx, lexer);
    }

    if ((lexer_peek(ctx, lexer) != '*' && lexer_peek_next(ctx, lexer) != '/')) {
        report(start_line_idx, start_col_idx, ctx->source_filename, ctx->source, line_start,
               "Unterminated comment.");
        return;
    }

    lexer_advance(ctx, lexer);
    lexer_advance(ctx, lexer);
}

char lexer_advance(context_t *ctx, lexer_t *lexer)
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

char lexer_peek(context_t *ctx, lexer_t *lexer)
{
    if (lexer->cursor >= ctx->source_len)
        return '\0';

    return ctx->source[lexer->cursor];
}

char lexer_peek_next(context_t *ctx, lexer_t *lexer)
{
    if (lexer->cursor >= ctx->source_len)
        return '\0';

    return ctx->source[lexer->cursor + 1];
}

bool lexer_match(context_t *ctx, lexer_t *lexer, char ch)
{
    if (ctx->source[lexer->cursor] != ch)
        return false;

    lexer_advance(ctx, lexer);
    return true;
}

bool lexer_is_at_end(context_t *ctx, lexer_t *lexer)
{
    return lexer->cursor >= ctx->source_len;
}

void lexer_append_token(context_t *ctx, lexer_t *lexer, token_type_t type, const char *lexeme,
                        usize lexeme_len, usize line_idx, usize col_idx, usize cursor,
                        usize line_start)
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

void report_error_at_token(context_t *ctx, token_t tok, const char *error_msg)
{
    report(tok.line - 1, tok.col - 1, ctx->source_filename, ctx->source, tok.line_start, error_msg);
}

token_type_t lexer_lookup_keyword(const char *s)
{
    size_t n = sizeof reserved_words_table / sizeof reserved_words_table[0];

    for (size_t i = 0; i < n; ++i) {
        if (strcmp(reserved_words_table[i].identifier, s) == 0)
            return reserved_words_table[i].type;
    }

    return IDENTIFIER;
}

bool lexer_is_alphanum(char ch)
{
    return isalnum(ch) || ch == '_';
}

ast_node_t *parser_create_node(arena_t *arena, node_type_t type, node_value_t value, usize start,
                               usize end)
{
    ast_node_t *node = arena_alloc(arena, sizeof(*node));

    node->value = value;
    node->start = start;
    node->end = end;
    node->type = type;

    return node;
}

token_t parser_previous(parser_t *parser)
{
    assert(parser->current_index > 0);

    return parser->tokens[parser->current_index - 1];
}

token_t parser_peek(parser_t *parser)
{
    if (parser->current_index >= parser->tokens_size)
        return parser->tokens[parser->tokens_size - 1];

    return parser->tokens[parser->current_index];
}

token_t parser_advance(parser_t *parser)
{
    if (parser->current_index >= parser->tokens_size)
        return parser->tokens[parser->tokens_size - 1];

    return parser->tokens[parser->current_index++];
}

bool parser_match_any(parser_t *parser, const token_type_t *types, usize count)
{
    token_t tok = parser_peek(parser);

    for (usize i = 0; i < count; ++i) {
        if (tok.type == types[i])
            return true;
    }

    return false;
}

void parser_consume(context_t *ctx, parser_t *parser, token_type_t type, const char *message)
{
    token_t tok = parser_peek(parser);
    if (tok.type == type) {
        parser_advance(parser);
        return;
    }

    report_error_at_token(ctx, tok, message);
}

void parser_synchronize(parser_t *parser)
{
    parser_advance(parser);

    while (!parser_is_eof(parser)) {
        if (parser_previous(parser).type == SEMICOLON)
            return;

        switch (parser_peek(parser).type) {
        case CLASS:
        case FUN:
        case VAR:
        case FOR:
        case IF:
        case WHILE:
        case BREAK:
        case PRINT:
        case RETURN:
            return;
        default:
            break;
        }

        parser_advance(parser);
    }
}

const char *intern(arena_t *arena, const char *s)
{
    string_pool_entry_t *current = string_pool.head;

    while (current) {
        if (strcmp(current->str, s) == 0)
            return current->str;
        current = current->next;
    }

    string_pool_entry_t *entry = arena_alloc(arena, sizeof(*entry));
    if (!entry)
        return NULL;

    usize s_len = strlen(s);
    char *str = arena_alloc(arena, sizeof(*str) * s_len + 1);
    memcpy(str, s, s_len);
    str[s_len] = '\0';

    entry->str = str;
    if (!entry->str)
        return NULL;

    entry->next = string_pool.head;
    string_pool.head = entry;

    return entry->str;
}

err set_var_entry(arena_t *arena, var_pool_t *pool, const char *s, value_t value)
{
    if (!pool)
        return 1;

    var_pool_entry_t *entry = arena_alloc(arena, sizeof(*entry));
    if (!entry)
        return 1;

    entry->str = s;
    entry->value = value;
    entry->next = pool->head;
    pool->head = entry;

    return 0;
}

var_pool_entry_t *get_var_entry(var_pool_t *pool, const char *s)
{
    var_pool_entry_t *current = pool->head;

    while (current) {
        // already interned, can check reference
        if (current->str == s)
            return current;
        current = current->next;
    }

    return NULL;
}

var_pool_entry_t *get_var_in_scope(environment_t *env, const char *s)
{
    environment_t *current = env;

    while (current) {
        var_pool_entry_t *var_entry = get_var_entry(&current->pool, s);
        if (var_entry)
            return var_entry;

        current = current->parent_env;
    }

    return NULL;
}

err set_var_in_scope(arena_t *arena, environment_t *env, const char *s, value_t value)
{
    environment_t *current = env;

    while (current) {
        var_pool_entry_t *var_entry = get_var_entry(&current->pool, s);

        if (var_entry) {
            var_entry->value = value;

            return 0;
        }

        current = current->parent_env;
    }

    return set_var_entry(arena, &env->pool, s, value);
}

bool parser_is_eof(parser_t *parser)
{
    return parser_match(parser, END_OF_FILE);
}

bool is_truthy(value_t value)
{
    switch (value.type) {
    case VALUE_NUMBER:
        return value.as.number != 0;

    case VALUE_STRING:
        return strcmp(value.as.string, "");

    case VALUE_BOOL:
        return value.as.boolean;

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

        return a.as.number == b.as.number;
    }

    case VALUE_STRING: {
        if (b.type != VALUE_STRING)
            return false;

        return strcmp(a.as.string, b.as.string) == 0;
    }

    case VALUE_BOOL:
        if (b.type != VALUE_BOOL)
            return false;

        return a.as.boolean == b.as.boolean;

    default:
        return false;
    }
}

const char *stringify_value(arena_t *arena, value_t data)
{
    switch (data.type) {
    case VALUE_BOOL:
        return data.as.boolean ? "true" : "false";
    case VALUE_NUMBER:
        return number_to_string(arena, data.as.number);
    case VALUE_NIL:
        return "nil";
    case VALUE_STRING:
        return data.as.string;
    case VALUE_FUN: {
        const char *name = data.as.fun_def.name;
        if (!name)
            return "<fn (nil)>";

        usize num_bytes = strlen(name) + 6;
        char *str = arena_alloc(arena, num_bytes);
        snprintf(str, num_bytes, "<fn %s>", name);
        return str;
    }
    case VALUE_UNINITIALIZED:
        return NULL;
    }
}

bool interpret_comparison(token_type_t op, value_t a, value_t b)
{
    assert(a.type == b.type);

    switch (a.type) {
    case VALUE_NUMBER: {
        double x = a.as.number;
        double y = b.as.number;

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
        int cmp = strcmp(a.as.string, b.as.string);

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
        int x = a.as.boolean;
        int y = b.as.boolean;

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

eval_result_t interpret_binary(context_t *ctx, ast_node_t *node, environment_t *env)
{
    assert(node->type == NODE_BINARY);

    switch (node->value.binary.op.type) {
    case OR: {
        eval_result_t left = interpret(ctx, node->value.binary.left, env);
        eval_result_t right = interpret(ctx, node->value.binary.right, env);

        return is_truthy(left.value) ? left : right;
    }

    case AND: {
        eval_result_t left = interpret(ctx, node->value.binary.left, env);
        eval_result_t right = interpret(ctx, node->value.binary.right, env);

        return is_truthy(left.value) ? right : left;
    }

    case EQUAL_EQUAL: {
        eval_result_t left = interpret(ctx, node->value.binary.left, env);
        eval_result_t right = interpret(ctx, node->value.binary.right, env);

        return create_eval_result(
            EVAL_OK,
            create_value(VALUE_BOOL, (value_as_t){ .boolean = is_equal(left.value, right.value) }));
    }

    case BANG_EQUAL: {
        eval_result_t left = interpret(ctx, node->value.binary.left, env);
        eval_result_t right = interpret(ctx, node->value.binary.right, env);

        return create_eval_result(
            EVAL_OK, create_value(VALUE_BOOL,
                                  (value_as_t){ .boolean = !is_equal(left.value, right.value) }));
    }

    case LESS:
    case LESS_EQUAL:
    case GREATER:
    case GREATER_EQUAL: {
        eval_result_t left = interpret(ctx, node->value.binary.left, env);
        eval_result_t right = interpret(ctx, node->value.binary.right, env);

        if (!check_number_operands(ctx, node->value.binary.op, left.value, right.value))
            return create_eval_error();

        return create_eval_result(
            EVAL_OK,
            create_value(VALUE_BOOL,
                         (value_as_t){ .boolean = interpret_comparison(node->value.binary.op.type,
                                                                       left.value, right.value) }));
    }

    case PLUS: {
        eval_result_t left = interpret(ctx, node->value.binary.left, env);
        eval_result_t right = interpret(ctx, node->value.binary.right, env);

        if (left.value.type == VALUE_STRING || right.value.type == VALUE_STRING) {
            const char *left_str = stringify_value(ctx->arena, left.value);
            const char *right_str = stringify_value(ctx->arena, right.value);

            return create_eval_result(
                EVAL_OK, create_value(VALUE_STRING,
                                      (value_as_t){
                                          .string = concat_str(ctx->arena, left_str, right_str),
                                      }));
        }

        if (!check_number_operands(ctx, node->value.binary.op, left.value, right.value))
            return create_eval_error();

        return create_eval_result(
            EVAL_OK, create_value(VALUE_NUMBER, (value_as_t){ .number = left.value.as.number +
                                                                        right.value.as.number }));
    }
    case MINUS: {
        eval_result_t left = interpret(ctx, node->value.binary.left, env);
        eval_result_t right = interpret(ctx, node->value.binary.right, env);

        if (!check_number_operands(ctx, node->value.binary.op, left.value, right.value))
            return create_eval_error();

        return create_eval_result(
            EVAL_OK, create_value(VALUE_NUMBER, (value_as_t){ .number = left.value.as.number -
                                                                        right.value.as.number }));
    }

    case STAR: {
        eval_result_t left = interpret(ctx, node->value.binary.left, env);
        eval_result_t right = interpret(ctx, node->value.binary.right, env);

        if (!check_number_operands(ctx, node->value.binary.op, left.value, right.value))
            return create_eval_error();

        return create_eval_result(
            EVAL_OK, create_value(VALUE_NUMBER, (value_as_t){ .number = left.value.as.number *
                                                                        right.value.as.number }));
    }

    case SLASH: {
        eval_result_t left = interpret(ctx, node->value.binary.left, env);
        eval_result_t right = interpret(ctx, node->value.binary.right, env);

        if (!check_number_operands(ctx, node->value.binary.op, left.value, right.value))
            return create_eval_error();

        if (right.value.as.number == 0.0) {
            token_t tok = node->value.binary.op;
            report_runtime(tok.line - 1, tok.col - 1, ctx->source_filename, ctx->source,
                           tok.line_start, "Division by zero.");
            return create_eval_error();
        }

        return create_eval_result(
            EVAL_OK, create_value(VALUE_NUMBER, (value_as_t){ .number = left.value.as.number /
                                                                        right.value.as.number }));
    }

    case COMMA: {
        interpret(ctx, node->value.binary.left, env);
        return interpret(ctx, node->value.binary.right, env);
    }

    default: {
        __builtin_unreachable();
    }
    }
}

eval_result_t interpret_unary(context_t *ctx, ast_node_t *node, environment_t *env)
{
    switch (node->value.unary.op.type) {
    case BANG: {
        return create_eval_result(
            EVAL_OK,
            create_value(VALUE_BOOL,
                         (value_as_t){ .boolean = !is_truthy(
                                           interpret(ctx, node->value.unary.child, env).value) }));
    }

    case MINUS: {
        eval_result_t child = interpret(ctx, node->value.unary.child, env);
        if (!check_number_operand(ctx, node->value.unary.op, child.value))
            return create_eval_error();

        return create_eval_result(
            EVAL_OK, create_value(VALUE_NUMBER, (value_as_t){ .number = -child.value.as.number }));
    }

    case PLUS: {
        eval_result_t child = interpret(ctx, node->value.unary.child, env);
        if (!check_number_operand(ctx, node->value.unary.op, child.value))
            return create_eval_error();

        return create_eval_result(
            EVAL_OK, create_value(VALUE_NUMBER, (value_as_t){ .number = +child.value.as.number }));
    }

    default: {
        __builtin_unreachable();
    }
    }
}

eval_result_t interpret_ternary(context_t *ctx, ast_node_t *node, environment_t *env)
{
    bool condition = is_truthy(interpret(ctx, node->value.ternary.condition, env).value);
    if (condition)
        return interpret(ctx, node->value.ternary.true_branch, env);

    return interpret(ctx, node->value.ternary.false_branch, env);
}

eval_result_t interpret(context_t *ctx, ast_node_t *node, environment_t *env)
{
    assert(node);

    switch (node->type) {
    case NODE_NUMBER:
        return create_eval_result(
            EVAL_OK,
            create_value(VALUE_NUMBER, (value_as_t){ .number = node->value.number_literal }));

    case NODE_STRING:
        return create_eval_result(
            EVAL_OK,
            create_value(VALUE_STRING, (value_as_t){ .string = node->value.string_literal }));

    case NODE_BOOL:
        return create_eval_result(
            EVAL_OK,
            create_value(VALUE_BOOL, (value_as_t){ .boolean = node->value.boolean_literal }));

    case NODE_NIL:
        return create_eval_result(EVAL_OK, create_value(VALUE_NIL, (value_as_t){}));

    case NODE_FUN: {
        assert(node->value.fun_decl.body);

        value_t data =
            create_value(VALUE_FUN, (value_as_t){ .fun_def = {
                                                      .params = node->value.fun_decl.params,
                                                      .body = node->value.fun_decl.body,
                                                      .closure = env,
                                                  } });

        if (node->value.fun_decl.name) {
            const char *name = node->value.fun_decl.name->value.identifier.name;
            data.as.fun_def.name = name;

            if (set_var_entry(ctx->arena, &env->pool, name, data) != 0)
                return create_eval_error();

            return create_eval_ok();
        }

        return create_eval_result(EVAL_OK, data);
    }

    case NODE_IDENTIFIER: {
        var_pool_entry_t *entry = get_var_in_scope(env, node->value.identifier.name);
        source_loc_t loc = node->value.identifier.loc;

        if (!entry) {
            report_runtime(loc.line - 1, loc.col - 1, ctx->source_filename, ctx->source,
                           loc.line_start, "Variable is not defined");
            return create_eval_error();
        }

        if (entry->value.type == VALUE_UNINITIALIZED) {
            report_runtime(loc.line - 1, loc.col - 1, ctx->source_filename, ctx->source,
                           loc.line_start, "Uninitialized variable");
            return create_eval_error();
        }

        return create_eval_result(EVAL_OK, entry->value);
    }

    case NODE_BINARY:
        return interpret_binary(ctx, node, env);

    case NODE_UNARY:
        return interpret_unary(ctx, node, env);

    case NODE_TERNARY:
        return interpret_ternary(ctx, node, env);

    case NODE_PRINT: {
        printf("%s\n",
               stringify_value(ctx->arena, interpret(ctx, node->value.print_stmt, env).value));

        return create_eval_ok();
    }

    case NODE_VAR: {
        const char *identifier = node->value.var_decl.identifier->value.identifier.name;
        value_t uninitialized_value = create_uninitialized_value();

        if (node->value.var_decl.expression->type == NODE_UNINITIALIZED) {
            if (set_var_entry(ctx->arena, &env->pool, identifier, uninitialized_value) != 0)
                return create_eval_error();

            return create_eval_ok();
        }

        if (set_var_entry(ctx->arena, &env->pool, identifier,
                          interpret(ctx, node->value.var_decl.expression, env).value) != 0)
            return create_eval_error();

        return create_eval_ok();
    }
    case NODE_ASSIGN: {
        const char *var_name = node->value.assign.identifier->value.identifier.name;
        source_loc_t loc = node->value.assign.identifier->value.identifier.loc;
        var_pool_entry_t *var_entry = get_var_in_scope(env, var_name);

        if (!var_entry) {
            report_runtime(loc.line - 1, loc.col - 1, ctx->source_filename, ctx->source,
                           loc.line_start, "Use of undeclared identifier");
            return create_eval_error();
        }

        eval_result_t value = interpret(ctx, node->value.assign.expression, env);
        if (set_var_in_scope(ctx->arena, env, var_name, value.value) != 0)
            return create_eval_error();

        assert(value.type == EVAL_OK);

        return value;
    }

    case NODE_BLOCK: {
        environment_t *local_env = create_env(ctx->arena, env);

        return run_declarations(ctx, node->value.block_declarations, local_env);
    };

    case NODE_IF: {
        assert(node->value.if_stmt.condition);

        eval_result_t condition = interpret(ctx, node->value.if_stmt.condition, env);

        if (is_truthy(condition.value)) {
            assert(node->value.if_stmt.then_branch);

            eval_result_t result = interpret(ctx, node->value.if_stmt.then_branch, env);
            if (result.type != EVAL_OK)
                return result;
        } else if (node->value.if_stmt.else_branch) {
            eval_result_t result = interpret(ctx, node->value.if_stmt.else_branch, env);
            if (result.type != EVAL_OK)
                return result;
        }

        return create_eval_ok();
    };

    case NODE_WHILE: {
        assert(node->value.while_stmt.condition);
        assert(node->value.while_stmt.body);

        while (is_truthy(interpret(ctx, node->value.while_stmt.condition, env).value)) {
            eval_result_t result = interpret(ctx, node->value.while_stmt.body, env);

            if (result.type == EVAL_BREAK)
                break;

            if (result.type == EVAL_ERROR || result.type == EVAL_RETURN)
                return result;
        }

        return create_eval_ok();
    };

    case NODE_FOR: {
        assert(node->value.for_stmt.body);
        ast_node_t *condition = node->value.for_stmt.condition;

        environment_t *local_env = create_env(ctx->arena, env);

        if (node->value.for_stmt.initializer)
            interpret(ctx, node->value.for_stmt.initializer, local_env);

        while ((condition ? is_truthy(interpret(ctx, condition, local_env).value) : true)) {
            eval_result_t result = interpret(ctx, node->value.for_stmt.body, local_env);

            if (result.type == EVAL_BREAK)
                break;

            if (result.type == EVAL_ERROR || result.type == EVAL_RETURN)
                return result;

            if (node->value.for_stmt.increment)
                interpret(ctx, node->value.for_stmt.increment, local_env);
        }

        return create_eval_ok();
    };

    case NODE_BREAK: {
        return create_eval_break();
    }

    case NODE_RETURN: {
        ast_node_t *value = node->value.return_value;
        return create_eval_result(EVAL_RETURN, value ? interpret(ctx, value, env).value :
                                                       create_value(VALUE_NIL, (value_as_t){}));
    }

    case NODE_CALL: {
        eval_result_t callee_result = interpret(ctx, node->value.call.callee, env);
        source_loc_t loc = node->value.call.loc;

        if (callee_result.value.type != VALUE_FUN) {
            report_runtime(loc.line - 1, loc.col - 1, ctx->source_filename, ctx->source,
                           loc.line_start, "Use of undeclared function");

            return create_eval_error();
        }

        ast_nodes_t *params = callee_result.value.as.fun_def.params;
        ast_nodes_t *args = node->value.call.args;
        usize parameters_len = params ? params->size : 0;
        usize arguments_len = args ? args->size : 0;

        if (parameters_len != arguments_len) {
            char buf[256];
            snprintf(buf, sizeof(buf), "Expected %zu arguments but found %zu", parameters_len,
                     arguments_len);
            report_runtime(loc.line - 1, loc.col - 1, ctx->source_filename, ctx->source,
                           loc.line_start, buf);

            return create_eval_error();
        }

        environment_t *closure = callee_result.value.as.fun_def.closure;
        environment_t *local_env = create_env(ctx->arena, closure);

        if (args) {
            for (usize i = 0; i < args->size; ++i) {
                eval_result_t result = interpret(ctx, args->items[i], env);

                if (set_var_entry(ctx->arena, &local_env->pool,
                                  params->items[i]->value.identifier.name, result.value) != 0)
                    return create_eval_error();
            }
        }

        ast_node_t *body = callee_result.value.as.fun_def.body;
        assert(body);

        eval_result_t result = run_declarations(ctx, body->value.block_declarations, local_env);

        if (result.type == EVAL_OK)
            return create_eval_result(EVAL_OK, create_value(VALUE_NIL, (value_as_t){}));
        if (result.type != EVAL_RETURN)
            return result;

        return create_eval_result(EVAL_OK, result.value);
    }

    default: {
        __builtin_unreachable();
    }
    }
}

eval_result_t create_eval_result(eval_type_t type, value_t value)
{
    return (eval_result_t){ .type = type, .value = value };
}

eval_result_t create_eval_ok(void)
{
    return (eval_result_t){ .type = EVAL_OK, .value = { 0 } };
}

eval_result_t create_eval_error(void)
{
    return (eval_result_t){ .type = EVAL_ERROR, .value = { 0 } };
}

eval_result_t create_eval_break(void)
{
    return (eval_result_t){ .type = EVAL_BREAK, .value = { 0 } };
}

value_t create_value(value_type_t type, value_as_t as)
{
    return (value_t){ .type = type, .as = as };
}

value_t create_uninitialized_value(void)
{
    return (value_t){ .type = VALUE_UNINITIALIZED, .as = { 0 } };
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

environment_t *create_env(arena_t *arena, environment_t *parent)
{
    environment_t *env = arena_alloc(arena, sizeof(*env));
    if (!env) {
        LOG(LOG_ERROR, "Out of memory");
        abort();
    }

    env->parent_env = parent;
    env->pool.head = NULL;

    return env;
}
