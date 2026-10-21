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

A trait declared in a namespace is named by its path, `@implements(Shapes::Measured)`, or by its
bare name where it is in scope. A generic bound `T: Shapes::Measured` accepts either form. Both
names resolve to the trait they refer to, the way a type name does, so two traits that share a name
in different namespaces stay apart: implementing `A::Named` does not satisfy a `B::Named` bound.
A trait has to be in scope, declared, in an enclosing namespace or imported, like any type; only
the lang traits and `Eq` and `Hash` are accepted without a declaration. Naming something that is
not a trait in scope, in `@implements` or in a bound, is a compile-time error.

## Lang traits

Five traits are known to the compiler and change how values behave rather than only what methods
they carry.

| Trait | Requires | What changes |
| --- | --- | --- |
| `Drop` | `drop(&mut self): void` | The compiler inserts drop calls at every scope exit |
| `Clone` | `clone(&self): Self` | `.clone()` produces an independent copy without moving the original |
| `Copy` | nothing | Assignment copies instead of moving |
| `Send` | nothing | The value may move to another thread |
| `Sync` | nothing | The value may be shared between threads through `&` |

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

### `Send` and `Sync`

`Send` and `Sync` are judged from a type's structure, so most types never name them:

- Primitives, `str`, ranges and raw C function pointers are both.
- Raw pointers (`*T`, `*mut T`) are neither: the compiler does not track what they point to.
- `&T` is either one only when `T` is `Sync`; `&mut T` follows `T`. Slices (`T[]`) follow `&T`.
- Arrays, tuples, records and enums are `Send` (or `Sync`) when every element, field or payload is,
  with a generic instance judged after its type arguments are substituted.
- A function value is neither. Only a call argument is judged: a closure written at a call to
  `function spawn<F: Send>(task: F)`, or to a `@sendable` parameter, is judged by what it captures.
  A value it moves in has to be `Send`, one it reads by reference has to be `Sync`, and a closure
  bound to `Sync` may not capture anything by mutable reference. A named function has nothing to
  capture and is accepted, and so is a `@sendable` parameter forwarded as is. A function value held
  in a variable is rejected (A0253), since its captures cannot be known there.

`F: Send` cannot be called inside the function; `@sendable` on a function-typed parameter can:

```ignis
function spawn(@sendable task: () -> i32): i32 {
    return task();
}
```

A type whose structure says no can still declare `@implements(Send)` or `@implements(Sync)`, for
example a record that guards a raw pointer itself. The declaration is honored, and the
`thread_safety` lint warns that the compiler can no longer check it (A0254). Prefer leaving the
structure to prove it; when the declaration is deliberate, `@allow(thread_safety)` on the record
keeps it without the warning.

On a generic type the declaration covers only its own fields: an instance has the trait only when
every type argument has it too. `Vector<String>` is Send; `Vector<*mut i32>` is not. A type
parameter bounded by `Send` or `Sync` changes that condition for its argument, which then only has
to satisfy those bounds: `Mutex<T: Send>` declares Send and Sync, so `Mutex<T>` is both whenever `T`
is Send, and `Arc<Mutex<T>>` shares it between threads.

```ignis
@allow(thread_safety)
@implements(Send, Sync)
record Handle {
    public raw: *mut i32;
}
```

See [ownership and borrowing](/docs/language/ownership) for what moving, copying and dropping do to
a value's lifetime.
