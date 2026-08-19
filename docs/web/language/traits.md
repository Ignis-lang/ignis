---
title: Traits
description: Shared behaviour declared as a contract, plus the three lang traits the compiler enforces itself.
section: language
order: 5
status: stable
---

A trait declares methods a type must provide. Methods may carry a default body, which an
implementing type can leave alone.

```ignis
trait Describable {
    describe(&self): i32;

    code(&self): i32 {
        return 0;
    }
}

@implements(Describable)
record Item {
    public id: i32;

    describe(&self): i32 {
        return self.id;
    }
}
```

A type opts in with `@implements(TraitName)` and then has to satisfy every method without a
default. Missing one is a compile-time error, not a runtime surprise.

## Lang traits

Three traits are known to the compiler and change how values behave rather than only what methods
they carry.

| Trait | Requires | What changes |
| --- | --- | --- |
| `Drop` | `drop(&mut self): void` | The compiler inserts drop calls at every scope exit |
| `Clone` | `clone(&self): Self` | `.clone()` produces an independent copy without moving the original |
| `Copy` | nothing | Assignment copies instead of moving |

```ignis
@implements(Drop, Clone)
record Buffer {
    public len: i32;

    drop(&mut self): void {
        return;
    }

    clone(&self): Buffer {
        return Buffer { len: self.len };
    }
}
```

Two rules that catch people out:

- A type with `Drop` is never `Copy`, even when every field is a primitive. Something that needs
  cleanup cannot be duplicated by assignment.
- `Copy` is mostly automatic. A record or enum whose fields are all recursively copyable already
  behaves that way with no annotation; writing `@implements(Copy)` asks the compiler to check that
  claim and fail if it is wrong.

See [ownership and borrowing](/docs/language/ownership) for what moving, copying and dropping do to
a value's lifetime.
