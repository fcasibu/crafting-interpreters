#ifndef _LOX_H_
#define _LOX_H_

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void error(int line, const char *message);
void report(int line, const char *where, const char *message);

#endif //_LOX_H_
