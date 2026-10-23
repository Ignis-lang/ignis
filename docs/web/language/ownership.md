---
title: Ownership and borrowing
description: Moves, copies, clones, drops and the exclusivity rules the borrow checker enforces.
section: language
order: 7
status: stable
---

Ignis tracks ownership of values that need cleanup — anything carrying `@implements(Drop)`, or
containing something that does. Those values move by default, and using one after it has been
moved, dropped or freed is a compile-time error. There is no garbage collector and no reference
counting behind this: the checks happen before any C is emitted.

## Moves

```ignis
@implements(Drop)
record Resource {
    public id: i32;

    drop(&mut self): void {
        return;
    }
}

function main(): i32 {
    let r = Resource { id: 1 };
    let r2 = r;

    return r2.id;
}
```

After `let r2 = r`, reading `r.id` is an error. Passing a non-copy value to a function moves it the
same way. A closure that owns its environment moves too; see
[closures](/docs/language/closures). Assigning to a moved variable makes it valid again:

```ignis
@implements(Drop)
record Resource {
    public id: i32;

    drop(&mut self): void {
        return;
    }
}

function main(): i32 {
    let mut r = Resource { id: 1 };
    let r2 = r;

    r = Resource { id: 2 };

    return r.id + r2.id;
}
```

## Copy

Primitives — integers, floats, `boolean`, `char`, pointers, references — are copied, and the
original stays valid. Records and enums are copyable structurally when every field or payload is
recursively copyable, with no annotation needed.

```ignis
record Vec2 {
    public x: f32;
    public y: f32;
}

function main(): i32 {
    let a = Vec2 { x: 1.0, y: 2.0 };
    let b = a;

    return 0;
}
```

Writing `@implements(Copy)` asks the compiler to verify that claim. A type with `Drop` is never
copyable, however primitive its fields look.

## Clone

`@implements(Clone)` requires `clone(&self): Self`. Calling it produces an independent value and
leaves the original where it is.

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

function main(): i32 {
    let a = Buffer { len: 10 };
    let b = a.clone();

    return a.len + b.len;
}
```

## Drop

A type with `@implements(Drop)` must provide `drop(&mut self): void`. The compiler inserts the call
at every exit: the end of a scope, an early `return`, a `break` or `continue`, and before a live
variable is overwritten. Double drops and use-after-drop are compile-time errors, with a runtime
guard behind them as a second line of defence.

A temporary, a value no name holds, is dropped at the end of the statement that created it. An
`if` condition's temporaries last through the branch, a `match` scrutinee's through the arm and
a `for ... of` iterable's through the whole loop.
A temporary never dies mid-statement, so passing `make().toStr()` as an argument or using it in
a template literal is fine. Keeping a view of a temporary past its statement is rejected (A0202,
see Views below).

## Borrowing

References follow exclusivity:

- Any number of `&T` may exist at once.
- A `&mut T` is exclusive. No other reference to that value may exist while it lives.
- Mutating a variable while a borrow of it is active is an error.
- Returning a reference to a local is an error.

```ignis
function main(): i32 {
    let mut x: i32 = 10;
    let r1 = &x;
    let r2 = &x;

    return *r1 + *r2;
}
```

### Views

A method that takes `&self` or `&mut self` and returns a `str`, a `&T`, a `&mut T` or a
slice returns a view of its receiver: the result points into the receiver's storage. A `str`
result counts as a view only when the receiver's type needs a drop, the way `String` does. The
owner is followed through method chains, field access, indexing, references and locals that
were initialized from a view. A free function, an extern function or a static method
that returns a view returns a view of each of its `&T`, `&mut T`, `str` and slice arguments, and
the shortest-lived owner among them counts. Other arguments create no dependency. When such a
function really returns a literal or a fresh copy, bind its argument to a local first.

A view must not outlive its owner:

- Keeping a view of a temporary in a `let` or an assignment is an error (A0202). The
  temporary is dropped when its statement ends.
- Storing a view of a local into a binding of an outer scope is an error (A0202).
- Returning a view of a local or of a temporary is an error (A0043).
- The same holds for a view kept inside a value: a record field, an enum payload, a tuple
  element, an `Option<&T>` a method returned, or the value a `match` arm produces.
- A pattern binding lives in its scrutinee. A view of a binding of a temporary scrutinee can
  be used inside the arm but not stored into a local declared outside it, and a
  `let ... else` cannot bind into a temporary by reference.
- While a binding keeps a view, its owner is borrowed until the end of the scope, so mutating
  or moving the owner (A0258) is reported. A binding typed `&mut T` borrows its owner mutably,
  so even reading the owner is reported until the binding's scope ends.
- A view taken through a view local that holds such a borrow borrows that local, not its
  owner again. With `let store: &mut Store = guard.get();`, `store.tasks.get(0).unwrap()`
  borrows `store`, so views derived from `store` only conflict with each other, while `guard`
  stays borrowed by `store` until `store`'s scope ends.
- A view cannot be stored where its owner cannot be seen: a `static` or `static mut` field of a
  view type is an error (A0268). A `static str` field is accepted, because a static string owns
  its bytes. A generic record or enum whose declared field or payload is a view is a view type
  whatever its type arguments are: `Tagged<i32>` with a `text: str` field is one.
- A view type is neither `Send` nor `Sync`, so every API bounded by them rejects one: a
  `Channel<T: Send>`, a `Thread::spawn` task, an `Arc<T>` and their friends cannot carry a
  `str`, a reference, a slice, or a record or container holding one. Sharing a view by
  reference across threads (`&T: Sync`) has the same dangling-owner risk as sending it, so
  `Sync` is refused too. A closure that captures by
  reference — or by value a view — is itself a view, and is rejected the same way; a closure
  that owns everything it captures is not.
- A closure whose captures are views obeys the same storage depth rule as a view: storing it
  in a `let`, an assignment, a field or an aggregate built in place keeps every capture alive
  that long, so a capture taken from a local whose scope ends before the binding is an error
  (A0202, A0043 for a return). Calling the closure later would read storage that was already
  dropped.
- A type parameter written `@owns` (`record Slot<@owns T>`) is an owning storage slot: the
  declaring type keeps the argument in storage it allocates, where no owner is visible, so
  instantiating it with a view type is an error (A0269). `Option`, `Result` and `Mutex` hold
  their payload inline and stay unmarked, so `Option<&T>` is legal. The standard library's
  heap-owning containers are marked: `Vector`, `HashMap` (both parameters), `HashSet`, `Rc`,
  `Weak`, `Arc`, and the channel's `ChannelQueue` and `ChannelState`.

A view of a temporary used inside the statement that created it is fine, as a call argument
or in a template literal. To keep it, bind the owner to a local first:

```ignis
function measure(text: str): u64 {
    let copy: String = String::create(text);

    return copy.length();
}

function main(): i32 {
    // let text: str = String::create("abc").toStr();   // error A0202
    let owned: String = String::create("abc");
    let text: str = owned.toStr();

    return (measure(text) + measure(String::create("abc").toStr())) as i32;
}
```

## Patterns and matching

What a destructuring pattern binds depends on what you matched, not on what the payload
is:

| Scrutinee | Binding mode | What the binding is | Who frees the payload |
| --- | --- | --- | --- |
| owned (`T`) | by value | the payload, moved out of the scrutinee | the binding, or whatever it is moved into |
| reference (`&T` / `&mut T`) | by reference | a place inside the referent | the referent's owner; the match owes nothing |

Matching an owned value moves its payload into the binding, and the binding is what frees
it. Matching a reference does not move anything: the binding names storage the referent
still owns, so a reference taken from it points into the referent and stays valid after
the match ends.

```ignis
enum Slot {
    FILLED(String),
    EMPTY,
}

function borrowed(slot: &Slot): Option<&String> {
    // `payload` names the String inside `slot`, so `&payload` outlives the match.
    return match (slot) {
        Slot::FILLED(payload) -> Option::SOME(&payload),
        Slot::EMPTY -> Option::NONE,
    };
}

function owned(slot: Slot): String {
    // `payload` *is* the String, moved out of `slot`.
    return match (slot) {
        Slot::FILLED(payload) -> payload,
        Slot::EMPTY -> String::create(""),
    };
}
```

A `&mut` scrutinee hands out places you may write through, the same way `&mut record.field`
does, and that permission carries down through nested patterns:

```ignis
match (slot.asMut()) {
    Slot::FILLED(payload) -> {
        (*payload).pushStr("!");
    },
    Slot::EMPTY -> {},
};
```

Assigning through the binding is not allowed. `payload = other` is rejected, and so is
every write reached from it — a field (`payload.count = 1`), an element
(`payload[0] = 1`), a dereference (`*payload = other`). The name stands for a place the
referent owns, and replacing what lives there is the referent's decision, not the
match's. Calling a mutating method through an explicit deref, as above, is unaffected:
that reads the place and mutates what is already there rather than overwriting it.

Payloads that need no drop (an `i32`, say) are still copied into the binding rather than
named in place. Reading one is the same either way; taking `&binding` gives you the
copy's address, not an address inside the referent.

## Across the FFI boundary

An extern function does not take ownership of what you hand it, unless the declaration says so with
`@takes`.

```ignis
extern rt {
    function release(@takes ptr: *mut void): void;
}
```

Without it, passing an owned non-copy value into an extern function warns about a likely leak — the
compiler cannot see what the C side does with the pointer, so it tells you instead of guessing.
