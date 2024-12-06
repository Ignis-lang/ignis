#ifndef IGNIS_STD_CHAR_H
#define IGNIS_STD_CHAR_H

#ifndef __DEBUG
#include "../types/types.h"
#else
#include <types/types.h>
#endif

char charToUpper(char value);
char charToLower(char value);
boolean charIsDigit(char value);
boolean charIsLetter(char value);
boolean charIsWhitespace(char value);
unknown charToNumber(char value, IgnisType type);
u8 charCharCode(char value);

#endif
