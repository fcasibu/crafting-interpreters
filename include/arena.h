#pragma once

#include "type_defs.h"
#include <stdbool.h>

typedef struct arena {
    unsigned char *base;
    usize capacity;
    usize offset;
    struct arena *next;
} arena_t;

bool arena_create(arena_t *arena, usize capacity);
void *arena_alloc(arena_t *arena, usize size);
void *arena_realloc(arena_t *arena, void *ptr, usize old_size, usize size);
void arena_clean(arena_t *arena);
void arena_destroy(arena_t *arena);

#ifdef ARENA_IMPLEMENTATION

#include <assert.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#define ARENA_DA_CAPACITY 256

#define arena_da_init(a, da)                                                   \
    do {                                                                       \
        (da)->capacity = ARENA_DA_CAPACITY;                                    \
        (da)->size = 0;                                                        \
        (da)->items = arena_alloc((a), (da)->capacity * sizeof(*(da)->items)); \
        if (!(da)->items) {                                                    \
            fprintf(stderr, "Out of memory\n");                                \
            abort();                                                           \
        }                                                                      \
    } while (0)

#define arena_da_append(a, da, item)                                                          \
    do {                                                                                      \
        if ((da)->size >= (da)->capacity) {                                                   \
            (da)->capacity = (da)->capacity == 0 ? ARENA_DA_CAPACITY : (da)->capacity * 2;    \
            typeof(*(da)->items) *tmp = arena_realloc((a), (da)->items,                       \
                                                      (da)->size * sizeof(*(da)->items),      \
                                                      (da)->capacity * sizeof(*(da)->items)); \
            if (!tmp) {                                                                       \
                fprintf(stderr, "Out of memory\n");                                           \
                abort();                                                                      \
            }                                                                                 \
            (da)->items = tmp;                                                                \
        }                                                                                     \
        (da)->items[(da)->size++] = (item);                                                   \
    } while (0)

bool arena_create(arena_t *arena, usize capacity)
{
    if (!arena || capacity == 0)
        return false;

    arena->base = malloc(capacity);
    if (!arena->base)
        return false;

    arena->capacity = capacity;
    arena->offset = 0;
    arena->next = NULL;
    return true;
}

void *arena_alloc(arena_t *arena, usize size)
{
    if (!arena || !arena->base || size == 0)
        return NULL;

    arena_t *end = arena;
    while (end->next)
        end = end->next;

    if (end->offset + size > end->capacity) {
        arena_t *next = malloc(sizeof(*next));
        if (!next)
            return NULL;

        usize new_cap = size > end->capacity * 2 ? size : end->capacity * 2;
        if (!arena_create(next, new_cap)) {
            free(next);
            return NULL;
        }

        end->next = next;
        end = next;
    }

    void *ptr = end->base + end->offset;
    end->offset += size;

    return ptr;
}

void *arena_realloc(arena_t *arena, void *oldptr, usize old_size, usize new_size)
{
    assert(old_size != 0);

    if (new_size <= old_size)
        return oldptr;

    void *newptr = arena_alloc(arena, new_size);
    if (!newptr)
        return NULL;

    memcpy(newptr, oldptr, old_size);
    return newptr;
}

void arena_clean(arena_t *arena)
{
    while (arena) {
        arena->offset = 0;
        arena = arena->next;
    }
}

void arena_destroy(arena_t *arena)
{
    if (!arena)
        return;

    free(arena->base);
    arena->base = NULL;
    arena->capacity = 0;
    arena->offset = 0;

    if (arena->next) {
        arena_destroy(arena->next);
        free(arena->next);
        arena->next = NULL;
    }
}

#endif // ARENA_IMPLEMENTATION
