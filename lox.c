#include "lox.h"

void error(const char *file_name, int line, int col, const char *message) {
  report(file_name, line, col, "", message);
}
void report(const char *file_name, int line, int col, const char *where,
            const char *message) {
  printf("%s:%d:%d: Error%s: %s\n", file_name, line, col, where, message);
}
