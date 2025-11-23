#pragma once

#include "type_defs.h"
#include "arena.h"

typedef struct {
    char **items;
    usize size;
    usize capacity;
} string_chunks_t;

string_chunks_t *split_str(arena_t *a, const char *input, const char *delim);
i64 parse_int(const char *source, int base);

#ifdef UTILS_IMPLEMENTATION

#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <stddef.h>

#define ARENA_IMPLEMENTATION
#include "arena.h"

#define STRING_CHUNKS_CAPACITY 256

#define TODO(X)                                                        \
    do {                                                               \
        fprintf(stderr, "%s:%d: TODO: %s\n", __FILE__, __LINE__, (X)); \
        abort();                                                       \
    } while (0)

string_chunks_t *split_str(arena_t *arena, const char *input, const char *delim)
{
    string_chunks_t *chunks = arena_alloc(arena, sizeof(*chunks));
    if (!chunks)
        return NULL;

    arena_da_init(arena, chunks);

    const char *current_pos = input;
    const char *next_delim = NULL;
    size_t delim_len = strlen(delim);

    if (delim_len == 0) {
        return chunks;
    }

    while ((next_delim = strstr(current_pos, delim)) != NULL) {
        assert(next_delim >= current_pos);

        isize len = next_delim - current_pos;

        char *token = arena_alloc(arena, (usize)(len + 1));
        if (!token)
            return NULL;

        memcpy(token, current_pos, len);
        token[len] = '\0';
        arena_da_append(arena, chunks, token);

        current_pos = next_delim + delim_len;
    }

    size_t last_len = strlen(current_pos);

    if (last_len > 0) {
        char *token = arena_alloc(arena, last_len + 1);
        if (!token)
            return NULL;

        memcpy(token, current_pos, last_len);
        token[last_len] = '\0';
        arena_da_append(arena, chunks, token);
    }

    return chunks;
}

i64 parse_int(const char *source, int base)
{
    errno = 0;
    char *end = { 0 };
    i64 value = strtoll(source, &end, base);

    if (*end != '\0' || errno == EINVAL || errno == ERANGE) {
        fprintf(stderr, "Failed to convert \"%s\" to i64\n", source);
        exit(EXIT_FAILURE);
    }

    return value;
}

#endif // UTILS_IMPLEMENTATION
