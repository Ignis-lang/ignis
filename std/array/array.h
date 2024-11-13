#ifndef IGNIS_STD_ARRAY_H
#define IGNIS_STD_ARRAY_H
#include <stdlib.h>
#ifndef __DEBUG
#include "../types/types.h"
#else
#include <types/types.h>
#endif

typedef struct IgnisArray {
  u32 length;
  u64 elementSize;
  IgnisType type;
  void *data;
} IgnisArray;

IgnisArray *arrayNew(IgnisType type, u32 length, void *data);
void freeArray(IgnisArray *array);

// #region Methods with callback

u32 array_length(IgnisArray *array, string type);

typedef unknown (*IgnisMapFunction)(unknown value, u32 index, IgnisArray *array);

IgnisArray *arrayMap(IgnisArray *array, IgnisMapFunction fn);

typedef boolean (*IgnisFindFunction)(unknown value, u32 index, IgnisArray *array);

IgnisArray *arrayFind(IgnisArray *array, IgnisFindFunction fn);

typedef u32 (*IgnisFindIndexFunction)(unknown value, u32 index, IgnisArray *array);

u32 arrayFindIndex(IgnisArray *array, IgnisFindFunction fn);

typedef boolean (*IgnisFilterFunction)(unknown value, u32 index, IgnisArray *array);

IgnisArray *arrayFilter(IgnisArray *array, IgnisFilterFunction fn);

typedef unknown (*IgnisReduceFunction)(unknown value, u32 index, IgnisArray *array);

IgnisArray *arrayReduce(IgnisArray *array, IgnisReduceFunction fn, unknown arrayialValue);

typedef boolean (*IgnisSomeFunction)(unknown value, u32 index, IgnisArray *array);

boolean arraySome(IgnisArray *value, IgnisSomeFunction fn);

typedef boolean (*IgnisEveryFunction)(unknown value, u32 index, IgnisArray *array);

boolean arrayEvery(IgnisArray *value, IgnisEveryFunction fn);

// #endregion

boolean arrayIncludes(IgnisArray *value, string search);

IgnisArray *arrayMax(IgnisArray *array);

IgnisArray *arrayMin(IgnisArray *array);

IgnisArray *arrayClone(IgnisArray *array);

// #region Array Mutation

void arrayPush(IgnisArray *array, unknown value);

IgnisArray *arrayPop(unknown value, unknown array);

IgnisArray *arrayShift(unknown value, unknown array);

// #endregion

#endif
