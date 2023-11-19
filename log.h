#ifndef _LOG_H_
#define _LOG_H_

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void error(const char *file_name, int line, int col, const char *message);
void warning(const char *file_name, int line, int col, const char *message);
void report(const char *file_name, int line, int col, const char *where,
            const char *message, const char *message_type);

#endif //_LOG_H_
