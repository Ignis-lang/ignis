# Ignis Standard Data Structures

This guide explains when to use the main built-in and standard-library data
structures that already exist in Ignis today:

- `str`
- `String`
- fixed-size arrays (`T[N]`)
- `Vector<T>`
- `HashMap<K, V>`
- `HashSet<T>`

It also contrasts them with lower-level compiler/language primitives so it is
clear where the standard library begins and where the language itself provides
the abstraction.

## 1. Quick Selection Guide

| Need | Recommended type | Why |
|---|---|---|
| Immutable string literal or borrowed string data | `str` | Primitive slice-like string type, no ownership |
| Owned, growable heap string | `String` | Owns bytes, implements `Drop`, supports mutation |
| Fixed-size collection known at compile time | `T[N]` | Language primitive, inline storage, no heap growth |
| Growable contiguous sequence | `Vector<T>` | Heap-backed dynamic array |
| Key/value lookup by hash | `HashMap<K, V>` | Average `O(1)` insert/get/remove |
| Unique membership test by hash | `HashSet<T>` | Thin set wrapper over `HashMap` |

## 2. `str` vs `String`

### `str`

`str` is the primitive immutable string type of Ignis. String literals have
type `str`.

```ignis
function main(): i32 {
  let name: str = "ignis";
  return 0;
}
```

Use `str` when:

- the data already exists as a literal or borrowed C-style string
- you do not need ownership
- you do not need to mutate or grow the bytes

### `String`

`String` is the owned heap-backed standard-library string type.

```ignis
import String from "std::string";

function main(): i32 {
  let mut value: String = String::create("ignis");
  value.pushStr(" compiler");
  return value.length() as i32;
}
```

Use `String` when:

- you need owned storage
- you need concatenation, mutation, trimming, splitting, or cloning
- you need to store strings in `Vector`, `HashMap`, or `HashSet`

## 3. Fixed-Size Arrays vs `Vector<T>`

This is the most important comparison.

### Fixed-size arrays: `T[N]`

Fixed-size arrays are language/compiler primitives, not a standard-library
container. If you think of them as a “static vector”, that is a reasonable
mental model, but in Ignis they are part of the language type system rather
than `std::vector`.

```ignis
function main(): i32 {
  let values: i32[3] = [10, 20, 30];
  return values[1];
}
```

Properties:

- size is part of the type
- size must be known at compile time
- storage is fixed after creation
- no `push`, `pop`, or automatic growth
- good for small, fixed layouts and ABI-oriented data

Use fixed-size arrays when:

- the element count is known statically
- you want simple inline storage
- you do not need resizing

### `Vector<T>`

`Vector<T>` is the standard-library growable contiguous array.

```ignis
import Vector from "std::vector";

function main(): i32 {
  let mut values: Vector<i32> = Vector::init<i32>();
  values.push(10);
  values.push(20);
  values.push(30);
  return values.length() as i32;
}
```

Properties:

- heap-backed
- dynamic length
- geometric growth
- manual container implementation in stdlib
- automatic cleanup through `Drop`

Use `Vector<T>` when:

- the number of elements is not known ahead of time
- you need `push`, `pop`, `get`, `getMut`, `clear`, `map`, `filter`, `fold`, or sorting
- you want a standard dynamic array abstraction rather than manual raw-memory management

### Practical rule

- Use `T[N]` for fixed-size data known at compile time.
- Use `Vector<T>` for runtime-sized collections.

## 4. `HashMap<K, V>`

`HashMap<K, V>` is the standard-library hash table.

It is appropriate when you need to map keys to values with average constant-time
lookup.

### Key requirements

Keys must satisfy `Hash & Eq`.

For user-defined key types, that usually means:

```ignis
import Eq from "std::collections";
import Hash from "std::collections";
import Hasher from "std::hash";

@implements(Hash, Eq)
record Key {
  id: i32;

  hash(&self, hasher: &mut Hasher): void {
    let mut state: &mut Hasher = hasher;
    state.writeI32(self.id);
  }

  equals(&self, other: &Key): boolean {
    return self.id == other.id;
  }
}
```

### Basic usage

```ignis
import Eq from "std::collections";
import Hash from "std::collections";
import HashMap from "std::collections";
import Hasher from "std::hash";

@implements(Hash, Eq)
record Key {
  id: i32;

  hash(&self, hasher: &mut Hasher): void {
    let mut state: &mut Hasher = hasher;
    state.writeI32(self.id);
  }

  equals(&self, other: &Key): boolean {
    return self.id == other.id;
  }
}

function main(): i32 {
  let mut map: HashMap<Key, i32> = HashMap::init<Key, i32>();
  map.insert(Key { id: 1 }, 100);
  map.insert(Key { id: 2 }, 200);

  let lookup: Key = Key { id: 2 };
  return match (map.get(&lookup)) {
    Option::SOME(value) -> *value,
    Option::NONE -> -1,
  };
}
```

### When to use it

Use `HashMap<K, V>` when:

- lookup by key matters more than preserving insertion order
- you need `insert`, `get`, `getMut`, `remove`, `contains`, and `reserve`
- keys are naturally hashable

Do not use it when:

- order matters
- you need range queries
- the collection size is tiny and fixed, where an array/vector may be simpler

## 5. `HashSet<T>`

`HashSet<T>` is the standard-library set type for unique values.

It has the same key requirements as `HashMap` because membership is based on
hashing and equality.

### Basic usage

```ignis
import Eq from "std::collections";
import Hash from "std::collections";
import HashSet from "std::collections";
import Hasher from "std::hash";

@implements(Hash, Eq)
record Key {
  id: i32;

  hash(&self, hasher: &mut Hasher): void {
    let mut state: &mut Hasher = hasher;
    state.writeI32(self.id);
  }

  equals(&self, other: &Key): boolean {
    return self.id == other.id;
  }
}

function main(): i32 {
  let mut set: HashSet<Key> = HashSet::init<Key>();
  set.insert(Key { id: 7 });
  set.insert(Key { id: 7 });

  let lookup: Key = Key { id: 7 };
  return set.contains(&lookup) ? 0 : 1;
}
```

### When to use it

Use `HashSet<T>` when:

- you only care about presence/absence
- you need uniqueness
- you do not need to associate a separate value with each key

If you need an associated payload, use `HashMap<K, V>` instead.

## 6. Choosing Between Primitive and Stdlib Structures

### Fixed-size array vs `Vector<T>`

| Question | Prefer |
|---|---|
| Is the size known at compile time? | `T[N]` |
| Will the collection grow or shrink? | `Vector<T>` |
| Do you want heap-backed dynamic storage? | `Vector<T>` |
| Do you want the simplest inline layout? | `T[N]` |

### `str` vs `String`

| Question | Prefer |
|---|---|
| Is it a borrowed literal or immutable external string? | `str` |
| Do you need ownership or mutation? | `String` |

### `Vector<T>` vs `HashMap<K, V>` vs `HashSet<T>`

| Need | Prefer |
|---|---|
| Ordered contiguous values by index | `Vector<T>` |
| Key/value lookup | `HashMap<K, V>` |
| Unique membership only | `HashSet<T>` |

## 7. Low-Level Alternatives

Ignis also exposes lower-level memory primitives in `std::memory`.

Examples:

- `Memory::allocateVector<T>(n)`
- `Memory::reallocateVector<T>(ptr, n)`
- `Memory::free(ptr)`

Those are useful when implementing containers or interoperating with lower-level
code, but application code should usually prefer `String`, `Vector`, `HashMap`,
and `HashSet`.

## 8. Current Scope

As of the current tree, the standard library already includes practical dynamic
string, vector, map, and set support. More advanced structures such as
`VecDeque`, `BTreeMap`, or `BinaryHeap` are not required for most programs and
are intentionally not treated as core prerequisites.
