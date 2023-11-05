#include "lox.h"

void error(int line, const char *message) { report(line, "", message); }
void report(int line, const char *where, const char *message) {
  printf("[line %d] Error%s: %s\n", line, where, message);
}
