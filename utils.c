#include "utils.h"

bool is_whitespace(const char ch) {
  return ch == ' ' || ch == '\r' || ch == '\t' || ch == '\n';
}
bool is_digit(const char ch) { return ch >= '0' && ch <= '9'; }

bool is_alpha(const char ch) {
  return ((ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z')) || ch == '_';
}

bool is_alphanum(const char ch) { return is_alpha(ch) || is_digit(ch); }
