#ifdef __DEBUG
#include <errors/errors.h>
#include <memory/memory.h>
#else
#include "../errors/errors.h"
#include "./memory.h"
#endif

#include <memory.h>

#define __USE_C_STD

#ifdef __USE_C_STD
#include <stdlib.h>
#endif

/**
 * @brief Allocates a block of memory.
 *
 * This function attempts to allocate a block of memory of the specified size.
 * If the standard C library allocation (`malloc`) is enabled with `__USE_C_STD`,
 * it uses `malloc()`. Otherwise, it is designed to call a custom memory
 * allocator (not yet implemented).
 *
 * @param size The size of the memory block to allocate, in bytes.
 *
 * @return Returns a pointer to the allocated memory. If allocation fails,
 *         the function calls `panic()` to terminate the program.
 *
 * @details The function first checks if the macro `__USE_C_STD` is defined.
 *          If so, it uses the standard `malloc()` function from the C library
 *          to allocate memory. If not, a placeholder for custom memory
 *          allocation is used, with a `todo()` call indicating that this
 *          part of the functionality is still pending.
 *
 *          In case of a failure to allocate memory (i.e., if `malloc()`
 *          returns `NULL`), the function will call `panic()` with an
 *          appropriate error message and terminate the program.
 */
void *allocate(u64 size) {
  void *address = NULL;
#ifdef __USE_C_STD
  address = malloc(size);
#else
  todo("Implement memory allocation");
#endif

  if (!address) {
    panic("Failed to allocate memory");
  }

  return address;
}

/**
 * @brief Frees a block of allocated memory.
 *
 * This function releases a block of memory that was previously allocated
 * using `allocate()`. If the standard C library memory management is used
 * (i.e., `__USE_C_STD` is defined), it uses the standard `free()` function.
 * Otherwise, it is intended to call a custom memory deallocator.
 *
 * @param address A pointer to the memory block to free.
 *
 * @details If `__USE_C_STD` is defined, the function calls `free()` to
 *          release the memory. Otherwise, a `todo()` function placeholder
 *          indicates that custom memory deallocation needs to be implemented.
 */
void deallocate(void *address) {
#ifdef __USE_C_STD
  free(address);
#else
  todo("Implement free memory");
#endif
}

/**
 * @brief Reallocates a block of memory to a new size.
 *
 * This function attempts to resize a block of memory that was previously
 * allocated using `allocate()`. If the standard C library memory management
 * is enabled (`__USE_C_STD` is defined), it uses `realloc()` to resize the
 * block. Otherwise, it is designed to use a custom reallocator (not yet
 * implemented).
 *
 * @param address A pointer to the memory block to reallocate.
 *
 * @param size The new size for the memory block, in bytes.
 *
 * @return Returns a pointer to the reallocated memory. If reallocation fails,
 *         the function calls `panic()` and terminates the program.
 *
 * @details If `__USE_C_STD` is defined, the function uses `realloc()` to
 *          attempt resizing the memory block. If the reallocation fails
 *          (i.e., `realloc()` returns `NULL`), the original memory block
 *          remains valid, but the function will call `panic()` and terminate
 *          the program.
 */
void *reallocate(void *address, u64 size) {
  void *addressRealloced = NULL;
#ifdef __USE_C_STD
  addressRealloced = realloc(address, size);
#else
  todo("Implement memory reallocate");
#endif

  if (!addressRealloced) {
    panic("Failed to reallocate memory");
  }

  return addressRealloced;
}

/**
 * @brief Allocates a block of zero-initialized memory.
 *
 * This function allocates a block of memory for an array of `count` elements,
 * each of `size` bytes, and initializes all bits in the allocated memory
 * to zero. If the standard C library allocation (`calloc`) is enabled with
 * `__USE_C_STD`, it uses `calloc()`. Otherwise, it is designed to call a
 * custom allocator (not yet implemented).
 *
 * @param size The size of each element to allocate, in bytes.
 *
 * @param count The number of elements to allocate.
 *
 * @return Returns a pointer to the allocated memory. If allocation fails,
 *         the function calls `panic()` to terminate the program.
 *
 * @details The function first checks if `__USE_C_STD` is defined. If so,
 *          it uses the standard `calloc()` function from the C library to
 *          allocate and zero-initialize memory. If the allocation fails
 *          (i.e., `calloc()` returns `NULL`), the function will call `panic()`
 *          with an appropriate error message and terminate the program.
 */
void *allocateZeroed(u64 size, u64 count) {
  void *address = NULL;
#ifdef __USE_C_STD
  address = calloc(size, count);
#else
  todo("Implement memory allocateZeroed");
#endif

  if (!address) {
    panic("Failed to allocate memory");
  }

  return address;
}
