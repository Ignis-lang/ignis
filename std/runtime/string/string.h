#pragma once

#include "../types/types.h"

// Length and basic operations
u64 stringLength(const string s);
i32 stringCompare(const string a, const string b);

// String construction
string stringConcat(const string a, const string b);
string stringSubString(const string s, i64 start, i64 len);

// Character access
i8 stringCharAt(const string s, i64 index);

// Search functions
i64 stringIndexOf(const string haystack, const string needle);
boolean stringContains(const string haystack, const string needle);

// Conversion
string stringToUpperCase(const string s);
string stringToLowerCase(const string s);

// Mutation
void stringPushChar(string *s, i8 c);
