#pragma once

typedef enum {
    LOG_DEBUG,
    LOG_INFO,
    LOG_WARN,
    LOG_ERROR,
} log_level_t;

#ifndef LOG_LEVEL
#define LOG_LEVEL LOG_INFO
#endif

#include <stdio.h>

static inline const char *log_level_str(log_level_t level);

#define LOG(level, fmt, ...)                                                                       \
    do {                                                                                           \
        if (level >= LOG_LEVEL) {                                                                  \
            printf("%s %s::%s::%d: " fmt "\n", log_level_str(level), __FILE__, __func__, __LINE__, \
                   __VA_ARGS__);                                                                   \
        }                                                                                          \
    } while (0)

static inline const char *log_level_str(log_level_t level)
{
    switch (level) {
    case LOG_DEBUG:
        return "DEBUG";
    case LOG_INFO:
        return "INFO";
    case LOG_WARN:
        return "WARN";
    case LOG_ERROR:
        return "ERROR";
    default:
        return "UNKNOWN";
    }
}
