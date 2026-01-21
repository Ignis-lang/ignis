# Memory Allocators

Memory allocator implementations in Ignis.

Based on [rulloc](https://github.com/antoniosarosi/memalloc-rust), a Rust implementation of memory allocators.

## Files

- `block.ign` - Block header structure for tracking allocations (used by HeapAllocator)
- `heap_allocator.ign` - General-purpose heap allocator with linked list of blocks and search strategies
- `arena_allocator.ign` - Simple bump/arena allocator for fast allocation with bulk deallocation
- `main.ign` - Test program demonstrating allocator usage

## Allocator Comparison

| Feature | HeapAllocator | ArenaAllocator |
|---------|---------------|----------------|
| Individual free | ✓ | ✗ |
| Bulk free | ✗ | ✓ (reset) |
| Per-alloc overhead | Header per block | None |
| Allocation speed | O(n) search | O(1) bump |
| Memory reuse | Via free blocks | Via reset only |
| Best for | General purpose | Temporary/scoped allocations |

## Build & Run

```bash
ignis build example/allocator/src/main.ign
./build/main
```
