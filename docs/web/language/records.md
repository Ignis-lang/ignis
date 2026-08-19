---
title: Records
description: Named product types with fields, methods, static members and layout attributes.
section: language
order: 2
status: stable
---

A record is a named product type. Fields and methods are private by default; `public` opts a member
into the outside world.

```ignis
record Counter {
    public value: i32;

    get(&self): i32 {
        return self.value;
    }

    increment(&mut self): void {
        self.value += 1;
        return;
    }

    public static new(start: i32): Counter {
        return Counter { value: start };
    }
}
```

## Methods and self

A method takes an explicit `self` receiver, and the form of that receiver decides what the method
may do:

- `&self` borrows the record and can read its fields.
- `&mut self` borrows it exclusively and can write them.

A method declared without a receiver is not callable on an instance. Use `static` for constructors
and other associated functions.

## Static members

Records carry static fields and static methods, reached through `::`.

```ignis
record Config {
    static MAX_SIZE: i32 = 1024;

    public static default(): Config {
        return Config {};
    }
}

function limit(): i32 {
    return Config::MAX_SIZE;
}
```

For standard-library records, `::new()` is the canonical constructor name — `Vector::new<T>()`,
`HashMap::new<K, V>()`, `Layout::new<T>()`, `ArenaAllocator::new(size)`. Some of them still answer
to `::init()` for compatibility; new code should not use those aliases.

## Initialization

A record literal names the record and assigns every field the initializer is responsible for.

```ignis
record Household {
    public city: str;
    public price: i32;
    public hasWifi: boolean;
}

function main(): i32 {
    let home: Household = Household {
        city: "Barcelona",
        price: 109,
        hasWifi: true,
    };

    return 0;
}
```

The value is immutable unless the binding that holds it is declared `let mut`.

## Layout attributes

| Attribute | Effect |
| --- | --- |
| `@packed` | Removes padding between fields in the emitted C struct. |
| `@aligned(N)` | Forces the struct alignment to N bytes. |
| `@implements(Copy)` | Gives the record copy semantics instead of move semantics. |
| `@implements(Clone)` | Makes the record explicitly cloneable. |
| `@implements(Drop)` | Runs the record's drop code when a value goes out of scope. |

These attributes reach the generated C directly, so they are the place where Ignis layout and C
layout have to agree — check them first when an FFI boundary misbehaves.
