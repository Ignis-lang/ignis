#ifndef IGNIS_STD_NUMBER_H
#define IGNIS_STD_NUMBER_H

#ifndef __DEBUG
#include "../types/types.h"
#else
#include <types/types.h>
#endif

string numberToString(unknown value, IgnisType type);
unknown numberAbs(unknown inputValue, IgnisType type);
unknown numberToFixed(unknown inputValue, u32 digits, IgnisType type);
unknown numberRound(unknown inputValue, IgnisType type);
unknown numberFloor(unknown inputValue, IgnisType type);
unknown numberCeil(unknown inputValue, IgnisType type);

#endif // IGNIS_STD_NUMBER_H
