#include "io.h"
#include <stdio.h>

// =============================================================================
// I/O functions
// =============================================================================

void print(string s) {
    if (s != NULL) {
        const char* cstr = ignis_string_cstr(s);
        if (cstr != NULL) {
            printf("%s", cstr);
        }
    }
}

void eprint(string s) {
    if (s != NULL) {
        const char* cstr = ignis_string_cstr(s);
        if (cstr != NULL) {
            fprintf(stderr, "%s", cstr);
        }
    }
}
