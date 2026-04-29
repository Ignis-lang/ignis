# Ignis Testing

This guide describes the language-level testing system available in Ignis.

## Overview

Ignis ships a native test runner and a standard-library testing namespace:

- `ignis test` — run project tests
- `ignis test path/to/file.ign` — run tests from a single file
- `std::test::Test` — assertions and snapshot helpers

The current supported surface is:

- `@test` top-level functions
- `Test::assert(condition)`
- `Test::fail()`
- `Test::assertEq<T>(left, right)`
- `Test::assertNe<T>(left, right)`
- `Test::assertSnapshot(name, actual)`
- `Test::assertFileSnapshot(name, filePath)`

## Declaring Tests

Tests are top-level functions annotated with `@test`.

Rules:

- they must be top-level functions
- they must not be `extern`
- they must not be generic
- they must take zero parameters
- they must return `void`

`@test` remains the native runner's source of truth.
Directive functions may coexist with tests, but they do not register native test
cases on their own.

```ignis
import Test from "std::test";

@test
function smoke(): void {
    Test::assert(true);
}
```

## Running Tests

### Project mode

```bash
ignis test
```

Optional substring filtering:

```bash
ignis test string
```

### Single-file mode

```bash
ignis test src/example.ign
```

Single-file mode uses the same native harness flow as project mode. It still
needs access to the standard library, typically through `IGNIS_STD_PATH`.

## Assertions

### Boolean assertions

```ignis
import Test from "std::test";

@test
function booleanChecks(): void {
    Test::assert(2 + 2 == 4);
}
```

### Generic equality assertions

`assertEq<T>` and `assertNe<T>` route through canonical builtin equality.

```ignis
import String from "std::string";
import Test from "std::test";

@test
function genericChecks(): void {
    let left: String = String::create("same");
    let right: String = String::create("same");

    Test::assertEq<String>(left, right);
    Test::assertNe<i32>(1, 2);
    Test::assertEq<str>("abc", "abc");
}
```

Canonical equality uses `std::hash::Eq` for records and enums. Unsupported
equality must be rejected during analysis before code generation.

## Snapshots

### Text snapshots

```ignis
import Test from "std::test";

@test
function snapshotText(): void {
    Test::assertSnapshot("rendered", "hello snapshot\n");
}
```

### File snapshots

```ignis
import Test from "std::test";

@test
function snapshotFile(): void {
    Test::assertFileSnapshot("artifact", "./actual-output.txt");
}
```

Snapshot files are stored under `__snapshots__/` next to the Ignis module under
test. File names are deterministic and escaped so that multiple modules can use
the same snapshot label safely.

### Updating snapshots

```bash
ignis test --update-snapshots
```

With update mode enabled:

- missing snapshots are created
- mismatched snapshots are replaced

Without update mode:

- missing snapshots fail the test
- mismatched snapshots fail the test

## Failure Reporting

The runner executes tests in deterministic order and continues after failures.
Failed tests print bounded stderr/stdout excerpts to avoid unbounded noise.

Snapshot mismatch reports include:

- failure reason
- snapshot path
- expected byte count
- actual byte count

## Equality Contract

For user-defined types, generic equality relies on canonical `Eq`:

```ignis
import Eq from "std::hash";

@implements(Eq)
record UserId {
    public value: i32;

    equals(&self, other: &UserId): boolean {
        return self.value == other.value;
    }
}
```

If a type does not support equality, the compiler must reject the test code
before harness build and before C code generation.
