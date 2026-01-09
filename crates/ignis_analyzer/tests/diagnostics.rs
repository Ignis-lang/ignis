mod common;

/// Tests that verify specific error codes appear at specific lines.
/// These tests are more resilient to message text changes than snapshots.

#[test]
fn break_outside_loop_at_correct_line() {
  // Line 3: break
  common::assert_diagnostic_at_line(
    r#"
function main(): void {
    break;
}"#,
    "A0040", // BreakOutsideLoop
    3,
  );
}

#[test]
fn continue_outside_loop_at_correct_line() {
  // Line 3: continue
  common::assert_diagnostic_at_line(
    r#"
function main(): void {
    continue;
}"#,
    "A0041", // ContinueOutsideLoop
    3,
  );
}

#[test]
fn mutable_reference_to_immutable_at_correct_line() {
  // Line 4: &mut x
  common::assert_diagnostic_at_line(
    r#"
function main(): void {
    let x: i32 = 1;
    let y: &mut i32 = &mut x;
    return;
}"#,
    "A0014", // MutableReferenceToImmutable
    4,
  );
}

#[test]
fn argument_count_mismatch_at_call_site() {
  // Line 7: add(1)
  common::assert_diagnostic_at_line(
    r#"
function add(a: i32, b: i32): i32 {
    return a + b;
}

function main(): void {
    let x: i32 = add(1);
    return;
}"#,
    "A0017", // ArgumentCountMismatch
    7,
  );
}

#[test]
fn unreachable_code_after_return() {
  // Line 4: let x
  common::assert_diagnostic_at_line(
    r#"
function main(): void {
    return;
    let x: i32 = 1;
}"#,
    "A0025", // UnreachableCode
    4,
  );
}

#[test]
fn dereference_non_pointer_at_correct_line() {
  // Line 4: *x
  common::assert_diagnostic_at_line(
    r#"
function main(): void {
    let x: i32 = 1;
    let y: i32 = *x;
    return;
}"#,
    "A0021", // DereferenceNonPointer
    4,
  );
}
