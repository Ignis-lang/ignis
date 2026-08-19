---
title: Testing
description: Writing @test functions, the assertions available, and how snapshots are stored and updated.
section: tooling
order: 3
status: stable
---

A test is a top-level function marked `@test`. The contract is strict and every part of it is
checked: top level, not `extern`, not generic, no parameters, returns `void`.

```ignis
import Test from "std::test";

@test
function smoke(): void {
    Test::assert(true);
    return;
}
```

Run them with [`ignis test`](/docs/tooling/cli), in a project or against a single file.

## Assertions

```ignis
import String from "std::string";
import Test from "std::test";

@test
function checks(): void {
    Test::assert(2 + 2 == 4);
    Test::assertEq<String>(String::create("same"), String::create("same"));
    Test::assertNe<i32>(1, 2);
    Test::assertEq<str>("abc", "abc");
    return;
}
```

`assertEq<T>` and `assertNe<T>` route through canonical equality: `std::hash::Eq` for records and
enums, the builtin `@eq<T>` underneath. A type that does not support equality is rejected during
analysis — the failure arrives as a diagnostic, not as a crash in generated code.

## Snapshots

A snapshot assertion compares against a stored baseline instead of a value written in the test.

```ignis
import Test from "std::test";

@test
function snapshotText(): void {
    Test::assertSnapshot("rendered", "hello snapshot\n");
    return;
}

@test
function snapshotFile(): void {
    Test::assertFileSnapshot("artifact", "./actual-output.txt");
    return;
}
```

Baselines live in a `__snapshots__/` directory next to the module under test. File names are
deterministic and escaped, so two modules can use the same label without colliding.

```bash
ignis test --update-snapshots
```

With update mode, missing baselines are created and mismatched ones replaced. Without it, both
cases fail — which is what you want in CI.

One thing to watch: project mode and single-file mode use different roots. A snapshot written while
running one file is not the one found when running the project.

## Failure reporting

Tests run in a deterministic order and the runner keeps going after a failure, so one broken test
does not hide the rest. Failed tests print bounded excerpts of stdout and stderr rather than
everything the process emitted.

A snapshot mismatch reports the reason, the path to the baseline, and the expected and actual byte
counts.
