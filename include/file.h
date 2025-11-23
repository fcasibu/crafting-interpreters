#pragma once

#include "arena.h"

char *get_input(arena_t *a, const char *filename);

#ifdef FILE_IMPLEMENTATION

#include <assert.h>
#include <stdio.h>

char *get_input(arena_t *a, const char *filename)
{
    FILE *file_ptr = fopen(filename, "r");

    if (!file_ptr) {
        perror("fopen failed");
        return NULL;
    }

    if (fseek(file_ptr, 0, SEEK_END) == -1) {
        perror("fseek failed");
        goto cleanup;
    }

    long size = ftell(file_ptr);

    if (size < 0) {
        perror("ftell failed");
        goto cleanup;
    }

    if (fseek(file_ptr, 0, SEEK_SET) == -1) {
        perror("fseek failed");
        goto cleanup;
    }

    assert(size > 0);
    char *input = arena_alloc(a, (usize)size + 1);
    usize bytes_read = fread(input, sizeof(char), (unsigned long)size, file_ptr);
    input[bytes_read] = '\0';

    if (ferror(file_ptr) != 0) {
        perror("fread failed");
        goto cleanup;
    }

    fclose(file_ptr);

    return input;

cleanup:
    fclose(file_ptr);
    return NULL;
}

#endif // FILE_IMPLEMENTATION
