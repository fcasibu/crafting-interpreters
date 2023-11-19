#include "lox.h"

void error(const char *file_name, int line, int col, const char *message) {
  report(file_name, line, col, "", message, "Error");
}

void warning(const char *file_name, int line, int col, const char *message) {
  report(file_name, line, col, "", message, "Warning");
}

void report(const char *file_name, int line, int col, const char *where,
            const char *message, const char *message_type) {
  printf("%s:%d:%d: %s%s: %s\n", file_name, line, col, message_type, where,
         message);
}
