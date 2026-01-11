#pragma once

#include "../ignis_rt.h"

// =============================================================================
// String operations
// =============================================================================
// These functions operate on IgnisString* (the new heap-managed string type).
// The 'string' typedef is now an alias for IgnisString*.

// Length and basic operations
u64 stringLength(const string s);
i32 stringCompare(const string a, const string b);

// String construction (returns new heap-allocated string)
string stringConcat(const string a, const string b);
string stringSubstring(const string s, i64 start, i64 len);

// Character access
i8 stringCharAt(const string s, i64 index);

// Search functions
i64 stringIndexOf(const string haystack, const string needle);
boolean stringContains(const string haystack, const string needle);

// Conversion (returns new heap-allocated string)
string stringToUpperCase(const string s);
string stringToLowerCase(const string s);

// Mutation (modifies string in place)
void stringPushChar(string s, i8 c);
