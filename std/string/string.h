// vim: set filetype=c:
/**
 * @file string.h
 * @brief The Ignis standard library string header.
 *
 */
#ifndef IGNIS_STD_STRING_H
#define IGNIS_STD_STRING_H

#ifndef __DEBUG
#include "../types/types.h"
#else
#include <types/types.h>
#endif

u32 stringLength(const string value);

boolean stringIncludes(const string value, const string search);
string stringSplit(const string value, const string separator);
char stringCharAt(const string value, int index);
string stringToUpperCase(const string value);
string stringToLowerCase(const string value);
void *stringToNumber(const string value);
boolean stringToBoolean(const string value);
u8 *stringToBytes(const string value);
string stringTrim(const string value);
string stringTrimStart(const string value);
string stringTrimEnd(const string value);
boolean stringStartsWith(const string value, const string search);
boolean stringEndWith(const string value, const string search);
string stringSlice(const string value, int start, int end);
string stringClone(const string value);

// #region Mutating Methods

void stringPush(string *value, string str);
char stringPop(string *value);
string stringConcat(string *value, string str);
void stringClear(string *value);
void stringReplace(string *value, const string search, const string replace);

// #endregion Mutating Methods

#endif
