mod common;

use insta::assert_snapshot;

#[test]
fn undeclared_identifier() {
  let result = common::analyze(
    r#"
function main(): void {
    let x: i32 = unknown_var;
    return;
}
"#,
  );

  // Note: UndeclaredVariable uses code I0033, not A0035
  common::assert_err(
    r#"
function main(): void {
    let x: i32 = unknown_var;
    return;
}
"#,
    &["I0033"],
  );

  assert_snapshot!("undeclared_identifier", common::format_diagnostics(&result.output.diagnostics));
}

#[test]
fn immutable_assignment() {
  let result = common::analyze(
    r#"
function main(): void {
    let x: i32 = 1;
    x = 2;
    return;
}
"#,
  );

  common::assert_err(
    r#"
function main(): void {
    let x: i32 = 1;
    x = 2;
    return;
}
"#,
    &["A0013"],
  );

  assert_snapshot!("immutable_assignment", common::format_diagnostics(&result.output.diagnostics));
}

#[test]
fn assignment_type_mismatch() {
  let result = common::analyze(
    r#"
function main(): void {
    let x: i32 = "hello";
    return;
}
"#,
  );

  assert_snapshot!(
    "assignment_type_mismatch",
    common::format_diagnostics(&result.output.diagnostics)
  );
}

#[test]
fn break_outside_loop() {
  let result = common::analyze(
    r#"
function main(): void {
    break;
}
"#,
  );

  common::assert_err(
    r#"
function main(): void {
    break;
}
"#,
    &["A0040"],
  );

  assert_snapshot!("break_outside_loop", common::format_diagnostics(&result.output.diagnostics));
}

#[test]
fn continue_outside_loop() {
  let result = common::analyze(
    r#"
function main(): void {
    continue;
}
"#,
  );

  common::assert_err(
    r#"
function main(): void {
    continue;
}
"#,
    &["A0041"],
  );

  assert_snapshot!("continue_outside_loop", common::format_diagnostics(&result.output.diagnostics));
}

#[test]
fn return_type_mismatch() {
  let result = common::analyze(
    r#"
function main(): i32 {
    return "hello";
}
"#,
  );

  assert_snapshot!("return_type_mismatch", common::format_diagnostics(&result.output.diagnostics));
}

#[test]
fn missing_return_value() {
  let result = common::analyze(
    r#"
function getValue(): i32 {
    return;
}
"#,
  );

  assert_snapshot!("missing_return_value", common::format_diagnostics(&result.output.diagnostics));
}

#[test]
fn mutable_reference_to_immutable() {
  let result = common::analyze(
    r#"
function main(): void {
    let x: i32 = 1;
    let y: &mut i32 = &mut x;
    return;
}
"#,
  );

  assert_snapshot!(
    "mutable_reference_to_immutable",
    common::format_diagnostics(&result.output.diagnostics)
  );
}

#[test]
fn invalid_binary_operand() {
  let result = common::analyze(
    r#"
function main(): void {
    let x: bool = true + false;
    return;
}
"#,
  );

  assert_snapshot!("invalid_binary_operand", common::format_diagnostics(&result.output.diagnostics));
}

#[test]
fn argument_count_mismatch() {
  let result = common::analyze(
    r#"
function add(a: i32, b: i32): i32 {
    return a + b;
}

function main(): void {
    let x: i32 = add(1);
    return;
}
"#,
  );

  assert_snapshot!(
    "argument_count_mismatch",
    common::format_diagnostics(&result.output.diagnostics)
  );
}

#[test]
fn argument_type_mismatch() {
  let result = common::analyze(
    r#"
function add(a: i32, b: i32): i32 {
    return a + b;
}

function main(): void {
    let x: i32 = add(1, "hello");
    return;
}
"#,
  );

  assert_snapshot!("argument_type_mismatch", common::format_diagnostics(&result.output.diagnostics));
}

// NOTE: extern_with_body test removed - parser doesn't support extern function with body

#[test]
fn dereference_non_pointer() {
  let result = common::analyze(
    r#"
function main(): void {
    let x: i32 = 1;
    let y: i32 = *x;
    return;
}
"#,
  );

  assert_snapshot!(
    "dereference_non_pointer",
    common::format_diagnostics(&result.output.diagnostics)
  );
}

#[test]
fn unreachable_code() {
  let result = common::analyze(
    r#"
function main(): void {
    return;
    let x: i32 = 1;
}
"#,
  );

  assert_snapshot!("unreachable_code", common::format_diagnostics(&result.output.diagnostics));
}

#[test]
fn integer_overflow() {
  let result = common::analyze(
    r#"
function main(): void {
    let x: i8 = 256;
    return;
}
"#,
  );

  common::assert_err(
    r#"
function main(): void {
    let x: i8 = 256;
    return;
}
"#,
    &["A0046"],
  );

  assert_snapshot!("integer_overflow", common::format_diagnostics(&result.output.diagnostics));
}

// =============================================================================
// Ownership errors
// =============================================================================

#[test]
fn use_after_move() {
  // Assigning an owned type (string) to another variable moves it.
  // Using the original variable after the move is an error.
  let result = common::analyze(
    r#"
function main(): void {
    let a: string = "hello";
    let b: string = a;
    let c: string = a;
    return;
}
"#,
  );

  common::assert_err(
    r#"
function main(): void {
    let a: string = "hello";
    let b: string = a;
    let c: string = a;
    return;
}
"#,
    &["O0001"],
  );

  assert_snapshot!("use_after_move", common::format_diagnostics(&result.output.diagnostics));
}

#[test]
fn move_in_function_call() {
  // Passing an owned type to a function moves it.
  // Using the variable after the call is an error.
  let result = common::analyze(
    r#"
function consume(s: string): void {
    return;
}

function main(): void {
    let a: string = "hello";
    consume(a);
    consume(a);
    return;
}
"#,
  );

  common::assert_err(
    r#"
function consume(s: string): void {
    return;
}

function main(): void {
    let a: string = "hello";
    consume(a);
    consume(a);
    return;
}
"#,
    &["O0001"],
  );

  assert_snapshot!("move_in_function_call", common::format_diagnostics(&result.output.diagnostics));
}

#[test]
fn inconsistent_move_in_branches() {
  // Moving in one branch but not the other is an error.
  // This requires an if-else where the variable is moved in one branch only.
  let result = common::analyze(
    r#"
function consume(s: string): void {
    return;
}

function main(): void {
    let a: string = "hello";
    let cond: boolean = true;

    if cond {
        consume(a);
    } else {
        let x: i32 = 1;
    }

    return;
}
"#,
  );

  common::assert_err(
    r#"
function consume(s: string): void {
    return;
}

function main(): void {
    let a: string = "hello";
    let cond: boolean = true;

    if cond {
        consume(a);
    } else {
        let x: i32 = 1;
    }

    return;
}
"#,
    &["O0003"],
  );

  assert_snapshot!(
    "inconsistent_move_in_branches",
    common::format_diagnostics(&result.output.diagnostics)
  );
}

#[test]
fn ffi_leak_warning() {
  // Passing an owned type to an extern function produces a warning.
  let result = common::analyze(
    r#"
extern function ffiConsume(s: string): void;

function main(): void {
    let a: string = "hello";
    ffiConsume(a);
    return;
}
"#,
  );

  // O0004 is a warning, not an error - but it should be in diagnostics
  let codes: Vec<_> = result
    .output
    .diagnostics
    .iter()
    .map(|d| d.error_code.as_str())
    .collect();
  assert!(codes.contains(&"O0004"), "Expected O0004 warning, got: {:?}", codes);

  assert_snapshot!("ffi_leak_warning", common::format_diagnostics(&result.output.diagnostics));
}

#[test]
fn copy_type_no_move() {
  // Copy types (primitives) don't move - this should NOT produce an error
  common::assert_ok(
    r#"
function main(): void {
    let a: i32 = 42;
    let b: i32 = a;
    let c: i32 = a;
    return;
}
"#,
  );
}
