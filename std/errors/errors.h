#ifndef IGNIS_STD_ERRORS_H
#define IGNIS_STD_ERRORS_H

#ifndef __DEBUG
#include "../types/types.h"
#else
#include <types/types.h>
#endif

void panic(const string format, ...);
void todo(const string format, ...);

#endif
