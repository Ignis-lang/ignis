#ifndef IGNIS_STD_TYPES_H
#define IGNIS_STD_TYPES_H

// Unsigned types
typedef unsigned char u8;
typedef unsigned short u16;
typedef unsigned int u32;
typedef unsigned long u64;

// Signed types
typedef signed char i8;
typedef signed short i16;
typedef signed int i32;
typedef signed long i64;

// Floating point types
typedef float f32;
typedef double f64;

// Boolean type
typedef u8 boolean;

// String type
typedef char *string;

// Null pointer type
typedef void *null;

typedef u16 hex;

#define NULL ((void*)0)

#define TRUE 0
#define FALSE 1

typedef enum IgnisType {
  TYPE_I8,
  TYPE_I16,
  TYPE_I32,
  TYPE_I64,
  TYPE_U8,
  TYPE_U16,
  TYPE_U32,
  TYPE_U64,
  TYPE_F32,
  TYPE_F64,
  TYPE_BOOL,
  TYPE_CHAR,
  TYPE_STRING,
  TYPE_UNKNOWN,
} IgnisType;

typedef union {
  u8 u8;
  u16 u16;
  u32 u32;
  u64 u64;
  i8 i8;
  i16 i16;
  i32 i32;
  i64 i64;
  f32 f32;
  f64 f64;
  boolean boolean;
  string string;
  char char_;
  void* unknown;
} IgnisGenericTypeUnion;

typedef struct {
  IgnisType type;
  IgnisGenericTypeUnion data;
} IgnisGenericType;

IgnisGenericType* ignisGenericTypeNew(IgnisType type);
void ignisGenericTypeFree(IgnisGenericType *type);

typedef IgnisGenericType *unknown;

static const u64 TYPE_SIZES[14] = {
    sizeof(i8),      // TYPE_I8
    sizeof(i16),     // TYPE_I16
    sizeof(i32),     // TYPE_I32
    sizeof(i64),     // TYPE_I64
    sizeof(u8),      // TYPE_U8
    sizeof(u16),     // TYPE_U16
    sizeof(u32),     // TYPE_U32
    sizeof(u64),     // TYPE_U64
    sizeof(f32),     // TYPE_F32
    sizeof(f64),     // TYPE_F64
    sizeof(boolean), // TYPE_BOOL
    sizeof(char),    // TYPE_CHAR
    sizeof(string),  // TYPE_STRING
    sizeof(unknown), // TYPE_UNKNOWN
};

IgnisType typeOf(unknown value);
boolean isType(unknown value, IgnisType type);
boolean isNumber(unknown value);
boolean isString(unknown value);
boolean isBoolean(unknown value);
boolean isChar(unknown value);
boolean isNull(unknown value);
boolean isArray(unknown value);

#endif // IGNIS_STD_TYPES_H
