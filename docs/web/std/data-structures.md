---
title: Data structures
description: Choosing between str and String, arrays and Vector, and the three hash-based collections.
section: std
order: 1
status: stable
---

| You need | Use | Because |
| --- | --- | --- |
| A literal or borrowed string | `str` | Language primitive, no ownership |
| An owned, growable string | `String` | Owns its bytes, implements `Drop`, can be mutated |
| A sequence sized at compile time | `T[N]` | Language primitive, stored inline, never grows |
| A sequence sized at runtime | `Vector<T>` | Heap-backed, grows geometrically |
| Lookup by key | `HashMap<K, V>` | Average constant-time insert, get and remove |
| Membership only | `HashSet<T>` | A thin set over the same table |
| Membership over dense `u32` indexes | `BitSet` | Packed bit storage |

Standard-library records are constructed with `::new()`. Some older ones still answer to `::init()`
for compatibility; new code should not use those.

## str and String

`str` is the language's immutable string type, and every string literal is one.

```ignis
function main(): i32 {
    let name: str = "ignis";

    return 0;
}
```

Reach for it when the bytes already exist and you neither own nor mutate them.

`String` owns a heap buffer and can grow.

```ignis
import String from "std::string";

function main(): i32 {
    let mut value: String = String::create("ignis");
    value.pushStr(" compiler");

    return value.length() as i32;
}
```

Reach for it when you need concatenation, trimming, splitting or cloning — and whenever you want to
store a string inside a `Vector`, `HashMap` or `HashSet`, since those need an owned value.

## Arrays and Vector

This is the comparison that matters most, and the two sit on opposite sides of the language
boundary.

A fixed-size array `T[N]` is part of the type system. Its length is part of its type, it lives
inline, and it never grows.

```ignis
function main(): i32 {
    let values: i32[3] = [10, 20, 30];

    return values[0];
}
```

`Vector<T>` is a standard-library container: heap-backed, growable, cleaned up through `Drop`.

```ignis
import Vector from "std::vector";

function main(): i32 {
    let mut values: Vector<i32> = Vector::new<i32>();
    values.push(10);
    values.push(20);
    values.push(30);

    return values.length() as i32;
}
```

It carries the operations you would expect — `push`, `pop`, `get`, `getMut`, `clear`, `map`,
`filter`, `fold`, sorting.

The rule is short: compile-time size, use the array; runtime size, use the vector.

## HashMap

`HashMap<K, V>` maps keys to values with average constant-time lookup. Keys must satisfy `Hash` and
`Eq`, which for your own types means implementing both.

```ignis
import Eq from "std::hash";
import Hash from "std::collections";
import HashMap from "std::collections";
import Hasher from "std::hash";

@implements(Hash, Eq)
record Key {
    id: i32;

    hash(&self, hasher: &mut Hasher): void {
        let mut state: &mut Hasher = hasher;
        state.writeI32(self.id);
        return;
    }

    equals(&self, other: &Key): boolean {
        return self.id == other.id;
    }
}

function main(): i32 {
    let mut map: HashMap<Key, i32> = HashMap::new<Key, i32>();
    map.insert(Key { id: 1 }, 100);
    map.insert(Key { id: 2 }, 200);

    let lookup: Key = Key { id: 2 };

    return match (map.get(&lookup)) {
        Option::SOME(value) -> *value,
        Option::NONE -> -1,
    };
}
```

`get` hands back an `Option`, so a missing key is a case you handle rather than a sentinel you
remember to check.

Do not reach for it when order matters, when you need range queries, or when the collection is tiny
and fixed — an array is simpler and faster at that size.

## HashSet

Same requirements as `HashMap`, no payload.

```ignis
import Eq from "std::hash";
import Hash from "std::collections";
import HashSet from "std::collections";
import Hasher from "std::hash";

@implements(Hash, Eq)
record Key {
    id: i32;

    hash(&self, hasher: &mut Hasher): void {
        let mut state: &mut Hasher = hasher;
        state.writeI32(self.id);
        return;
    }

    equals(&self, other: &Key): boolean {
        return self.id == other.id;
    }
}

function main(): i32 {
    let mut set: HashSet<Key> = HashSet::new<Key>();
    set.insert(Key { id: 7 });
    set.insert(Key { id: 7 });

    let lookup: Key = Key { id: 7 };

    return set.contains(&lookup) ? 0 : 1;
}
```

If you find yourself keeping a parallel structure with the values that go alongside the keys, you
wanted a `HashMap`.

## BitSet

`BitSet` stores membership for `u32` indexes in packed `u64` words. It is the right shape for
compiler-style ids and dataflow sets, where indexes are dense and small.

```ignis
import BitSet from "std::collections";

function main(): i32 {
    let mut visited: BitSet = BitSet::new();
    visited.insert(42);

    return visited.contains(42) ? 0 : 1;
}
```

When the values are not dense integers, the packing works against you and `HashSet<T>` is the
better fit.
