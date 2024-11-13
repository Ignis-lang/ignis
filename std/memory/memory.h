#ifndef IGNIS_STD_MEMORY_H
#define IGNIS_STD_MEMORY_H

#ifndef __DEBUG
#include "../types/types.h"
#else
#include <types/types.h>
#endif

void *allocate(u64 size);
void deallocate(void *address);
void *reallocate(void *address, u64 size);
void *allocateZeroed(u64 size, u64 count);

#endif
