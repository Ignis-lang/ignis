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

  assert_snapshot!(
    "undeclared_identifier",
    common::format_diagnostics(&result.output.diagnostics)
  );
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

  assert_snapshot!(
    "immutable_assignment",
    common::format_diagnostics(&result.output.diagnostics)
  );
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

  assert_snapshot!(
    "break_outside_loop",
    common::format_diagnostics(&result.output.diagnostics)
  );
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

  assert_snapshot!(
    "continue_outside_loop",
    common::format_diagnostics(&result.output.diagnostics)
  );
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

  assert_snapshot!(
    "return_type_mismatch",
    common::format_diagnostics(&result.output.diagnostics)
  );
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

  assert_snapshot!(
    "missing_return_value",
    common::format_diagnostics(&result.output.diagnostics)
  );
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

  assert_snapshot!(
    "invalid_binary_operand",
    common::format_diagnostics(&result.output.diagnostics)
  );
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

  assert_snapshot!(
    "argument_type_mismatch",
    common::format_diagnostics(&result.output.diagnostics)
  );
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

  assert_snapshot!(
    "unreachable_code",
    common::format_diagnostics(&result.output.diagnostics)
  );
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

  assert_snapshot!(
    "integer_overflow",
    common::format_diagnostics(&result.output.diagnostics)
  );
}
