# Bump Allocator

A simple bump (arena) allocator implementation in Ignis.

Based on [rallocator](https://github.com/0xErwin1/rallocator), a Rust implementation of a memory allocator using `sbrk`.

## Files

- `block.ign` - Block header structure for tracking allocations
- `allocator.ign` - BumpAllocator implementation with linked list of blocks
- `main.ign` - Test program demonstrating allocator usage

## Build & Run

```bash
ignis build example/allocator/src/main.ign
./build/main
```
