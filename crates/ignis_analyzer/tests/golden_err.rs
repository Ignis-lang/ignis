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
extern ffi {
    function ffiConsume(s: string): void;
}

function main(): void {
    let a: string = "hello";
    ffi::ffiConsume(a);
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

// =============================================================================
// Literal coercion tests
// =============================================================================

#[test]
fn literal_coercion_signed_to_unsigned() {
  common::assert_ok(
    r#"
function main(): void {
    let x: u32 = 42;
    let y: u8 = 100;
    let z: u64 = 0;
    return;
}
"#,
  );
}

#[test]
fn literal_coercion_widening() {
  common::assert_ok(
    r#"
function main(): void {
    let x: i64 = 100;
    let y: u64 = 255;
    return;
}
"#,
  );
}

#[test]
fn literal_coercion_negative_boundary() {
  common::assert_ok(
    r#"
function main(): void {
    let a: i8 = -128;
    let b: i16 = -32768;
    let c: i32 = -2147483648;
    return;
}
"#,
  );
}

#[test]
fn literal_coercion_negative_to_unsigned_error() {
  let result = common::analyze(
    r#"
function main(): void {
    let x: u32 = -1;
    return;
}
"#,
  );

  assert!(
    result.output.diagnostics.iter().any(|d| d.error_code == "A0046"),
    "Expected overflow error A0046"
  );

  assert_snapshot!(
    "literal_coercion_negative_to_unsigned",
    common::format_diagnostics(&result.output.diagnostics)
  );
}

#[test]
fn literal_coercion_overflow_error() {
  let result = common::analyze(
    r#"
function main(): void {
    let x: i8 = 200;
    let y: u8 = 300;
    return;
}
"#,
  );

  let overflow_count = result
    .output
    .diagnostics
    .iter()
    .filter(|d| d.error_code == "A0046")
    .count();
  assert_eq!(overflow_count, 2, "Expected 2 overflow errors, got {}", overflow_count);

  assert_snapshot!(
    "literal_coercion_overflow",
    common::format_diagnostics(&result.output.diagnostics)
  );
}

#[test]
fn literal_coercion_float() {
  common::assert_ok(
    r#"
function main(): void {
    let x: f32 = 3.14;
    let y: f64 = 2.5;
    return;
}
"#,
  );
}

#[test]
fn literal_coercion_in_call() {
  common::assert_ok(
    r#"
function takeU32(x: u32): void {
    return;
}

function main(): void {
    takeU32(42);
    return;
}
"#,
  );
}

#[test]
fn literal_coercion_in_assignment() {
  common::assert_ok(
    r#"
function main(): void {
    let mut x: i64 = 0;
    x = 42;
    return;
}
"#,
  );
}

#[test]
fn literal_coercion_in_binary() {
  common::assert_ok(
    r#"
function main(): void {
    let x: i64 = 100;
    let y: i64 = x + 1;
    return;
}
"#,
  );
}

#[test]
fn literal_coercion_hex() {
  common::assert_ok(
    r#"
function main(): void {
    let x: u8 = 0xFF;
    let y: u16 = 0xABCD;
    return;
}
"#,
  );
}

#[test]
fn literal_coercion_hex_overflow() {
  let result = common::analyze(
    r#"
function main(): void {
    let x: u8 = 0x100;
    return;
}
"#,
  );

  assert!(
    result.output.diagnostics.iter().any(|d| d.error_code == "A0046"),
    "Expected overflow error A0046"
  );

  assert_snapshot!(
    "literal_coercion_hex_overflow",
    common::format_diagnostics(&result.output.diagnostics)
  );
}

// =============================================================================
// allocate/deallocate tests
// =============================================================================

#[test]
fn deallocate_pointer_coercion() {
  common::assert_ok(
    r#"
extern mem {
    function allocate(size: u32): *mut u8;
    function deallocate(ptr: *mut u8): void;
}

function main(): void {
    let mut p: *mut i32 = mem::allocate(16) as *mut i32;
    mem::deallocate(p as *mut u8);
    return;
}
"#,
  );
}

#[test]
fn use_after_free_write() {
  let result = common::analyze(
    r#"
extern mem {
    function allocate(size: u32): *mut u8;
    function deallocate(ptr: *mut u8): void;
}

function main(): void {
    let mut p: *mut i32 = mem::allocate(16) as *mut i32;
    mem::deallocate(p as *mut u8);
    *p = 42;
    return;
}
"#,
  );

  assert!(
    result.output.diagnostics.iter().any(|d| d.error_code == "O0002"),
    "Expected use-after-free error O0002"
  );

  assert_snapshot!("use_after_free_write", common::format_diagnostics(&result.output.diagnostics));
}

#[test]
fn use_after_free_read() {
  let result = common::analyze(
    r#"
extern mem {
    function allocate(size: u32): *mut u8;
    function deallocate(ptr: *mut u8): void;
}

function main(): void {
    let mut p: *mut i32 = mem::allocate(16) as *mut i32;
    *p = 42;
    mem::deallocate(p as *mut u8);
    let x: i32 = *p;
    return;
}
"#,
  );

  assert!(
    result.output.diagnostics.iter().any(|d| d.error_code == "O0002"),
    "Expected use-after-free error O0002"
  );

  assert_snapshot!("use_after_free_read", common::format_diagnostics(&result.output.diagnostics));
}

#[test]
fn no_use_after_free_before_deallocate() {
  common::assert_ok(
    r#"
extern mem {
    function allocate(size: u32): *mut u8;
    function deallocate(ptr: *mut u8): void;
}

function main(): void {
    let mut p: *mut i32 = mem::allocate(16) as *mut i32;
    *p = 42;
    let x: i32 = *p;
    mem::deallocate(p as *mut u8);
    return;
}
"#,
  );
}

#[test]
fn allocate_literal_coercion() {
  common::assert_ok(
    r#"
extern mem {
    function allocate(size: u32): *mut u8;
}

function main(): void {
    let p: *mut u8 = mem::allocate(16);
    return;
}
"#,
  );
}

// =============================================================================
// Path expression tests
// =============================================================================

#[test]
fn path_function_as_callee_ok() {
  // Function paths are allowed as call targets
  common::assert_ok(
    r#"
namespace Math {
    function add(a: i32, b: i32): i32 {
        return a + b;
    }
}

function main(): void {
    let x: i32 = Math::add(1, 2);
    return;
}
"#,
  );
}

#[test]
fn path_function_outside_callee_is_error() {
  // Function paths outside of call position should produce an error
  // Note: We need to use a context where the path is valid syntactically
  // but semantically incorrect (e.g., as an argument to another function)
  let result = common::analyze(
    r#"
namespace Math {
    function add(a: i32, b: i32): i32 {
        return a + b;
    }
}

function takeFn(x: i32): void {
    return;
}

function main(): void {
    takeFn(Math::add);
    return;
}
"#,
  );

  common::assert_err(
    r#"
namespace Math {
    function add(a: i32, b: i32): i32 {
        return a + b;
    }
}

function takeFn(x: i32): void {
    return;
}

function main(): void {
    takeFn(Math::add);
    return;
}
"#,
    &["A0050"],
  );

  assert_snapshot!(
    "path_function_outside_callee",
    common::format_diagnostics(&result.output.diagnostics)
  );
}

#[test]
fn path_const_in_expression_ok() {
  // Constant paths are allowed in any expression position
  common::assert_ok(
    r#"
extern libc {
    const BUFSIZ: i32;
}

function main(): void {
    let x: i32 = libc::BUFSIZ;
    return;
}
"#,
  );
}
