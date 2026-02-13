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
    let x: boolean = true + false;
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

    if (cond) {
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

    if (cond) {
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
fn ffi_no_leak_for_named_vars() {
  // Passing an owned type to an extern function is now handled properly -
  // the compiler tracks that extern calls don't consume ownership, so the
  // caller is responsible for dropping the value. Named variables get their
  // drops scheduled by DropSchedules, so no warning is needed.
  common::assert_ok(
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
}

#[test]
fn extern_does_not_consume_ownership() {
  // Extern functions don't consume ownership, so the variable can be reused.
  // This is valid because extern calls don't move the value.
  common::assert_ok(
    r#"
extern io {
    function print(s: string): void;
}

function main(): void {
    let a: string = "hello";
    io::print(a);
    io::print(a);
    return;
}
"#,
  );
}

#[test]
fn non_extern_consumes_ownership() {
  // Non-extern (Ignis) functions DO consume ownership.
  // Using the variable after passing to an Ignis function is an error.
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
}

#[test]
fn extern_then_ignis_call() {
  // After extern call (doesn't consume), variable is still valid.
  // After Ignis call (consumes), variable is moved.
  common::assert_err(
    r#"
extern io {
    function print(s: string): void;
}

function consume(s: string): void {
    return;
}

function main(): void {
    let a: string = "hello";
    io::print(a);
    consume(a);
    io::print(a);
    return;
}
"#,
    &["O0001"],
  );
}

#[test]
fn ffi_leak_warning_without_takes() {
  // Passing an owned type to an extern function without @takes emits O0004.
  common::assert_err(
    r#"
extern io {
    function consumeString(s: string): void;
}

function main(): void {
    let a: string = "hello";
    io::consumeString(a);
    return;
}
"#,
    &["O0004"],
  );
}

#[test]
fn ffi_no_warning_with_takes() {
  // @takes suppresses the warning and consumes ownership instead.
  common::assert_ok(
    r#"
extern io {
    function freeString(@takes s: string): void;
}

function main(): void {
    let a: string = "hello";
    io::freeString(a);
    return;
}
"#,
  );
}

#[test]
fn ffi_takes_consumes_ownership() {
  // After passing to a @takes extern param, the value is moved — using it again is O0001.
  common::assert_err(
    r#"
extern io {
    function freeString(@takes s: string): void;
}

function consume(s: string): void {
    return;
}

function main(): void {
    let a: string = "hello";
    io::freeString(a);
    consume(a);
    return;
}
"#,
    &["O0001"],
  );
}

#[test]
fn ffi_no_warning_for_copy_types() {
  // Copy types (primitives, raw pointers) don't trigger the FFI leak warning.
  common::assert_ok(
    r#"
extern ffi {
    function doSomething(x: i32, p: *mut void): void;
}

function main(): void {
    let n: i32 = 42;
    let p: *mut void = 0 as *mut void;
    ffi::doSomething(n, p);
    return;
}
"#,
  );
}

#[test]
fn unknown_param_attribute() {
  // Unknown param attributes are rejected.
  common::assert_err(
    r#"
function foo(@bogus x: i32): void {
    return;
}

function main(): void {
    return;
}
"#,
    &["A0117"],
  );
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
// Copy structural validation
// =============================================================================

#[test]
fn copy_on_noncopy_field() {
  common::assert_err(
    r#"
@implements(Copy)
record Bad {
    name: string;
    id: i32;
}

function main(): void {
    return;
}
"#,
    &["A0148"],
  );
}

#[test]
fn copy_on_all_copy_fields_ok() {
  common::assert_ok(
    r#"
@implements(Copy)
record Point {
    public x: i32;
    public y: i32;
}

function main(): void {
    let a: Point = Point { x: 1, y: 2 };
    let b: Point = a;
    let c: Point = a;
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
// Double-free tests
// =============================================================================

#[test]
fn double_free_immediate() {
  let result = common::analyze(
    r#"
extern mem {
    function allocate(size: u32): *mut u8;
    function deallocate(ptr: *mut u8): void;
}

function main(): void {
    let mut p: *mut i32 = mem::allocate(16) as *mut i32;
    mem::deallocate(p as *mut u8);
    mem::deallocate(p as *mut u8);
    return;
}
"#,
  );

  assert!(
    result.output.diagnostics.iter().any(|d| d.error_code == "O0009"),
    "Expected double-free error O0009"
  );

  assert_snapshot!("double_free_immediate", common::format_diagnostics(&result.output.diagnostics));
}

#[test]
fn double_free_in_branch() {
  let result = common::analyze(
    r#"
extern mem {
    function allocate(size: u32): *mut u8;
    function deallocate(ptr: *mut u8): void;
}

function main(): void {
    let mut p: *mut i32 = mem::allocate(16) as *mut i32;
    mem::deallocate(p as *mut u8);
    let cond: boolean = true;
    if (cond) {
        mem::deallocate(p as *mut u8);
    }
    return;
}
"#,
  );

  assert!(
    result.output.diagnostics.iter().any(|d| d.error_code == "O0009"),
    "Expected double-free error O0009"
  );

  assert_snapshot!("double_free_in_branch", common::format_diagnostics(&result.output.diagnostics));
}

// =============================================================================
// Double-free alias tests
// =============================================================================

#[test]
fn double_free_via_alias() {
  // q = p creates an alias; deallocate(q) then deallocate(p) is a double-free
  let result = common::analyze(
    r#"
extern mem {
    function allocate(size: u32): *mut u8;
    function deallocate(ptr: *mut u8): void;
}

function main(): void {
    let mut p: *mut i32 = mem::allocate(16) as *mut i32;
    let q: *mut i32 = p;
    mem::deallocate(q as *mut u8);
    mem::deallocate(p as *mut u8);
    return;
}
"#,
  );

  assert!(
    result.output.diagnostics.iter().any(|d| d.error_code == "O0009"),
    "Expected double-free via alias error O0009, got: {:?}",
    result
      .output
      .diagnostics
      .iter()
      .map(|d| &d.error_code)
      .collect::<Vec<_>>()
  );

  assert_snapshot!("double_free_via_alias", common::format_diagnostics(&result.output.diagnostics));
}

#[test]
fn free_then_reassign_then_free_ok() {
  // After reassignment with a fresh allocate, the freed state is cleared
  common::assert_ok(
    r#"
extern mem {
    function allocate(size: u32): *mut u8;
    function deallocate(ptr: *mut u8): void;
}

function main(): void {
    let mut p: *mut i32 = mem::allocate(16) as *mut i32;
    mem::deallocate(p as *mut u8);
    p = mem::allocate(32) as *mut i32;
    mem::deallocate(p as *mut u8);
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

#[test]
fn type_alias_cycle() {
  let result = common::analyze(
    r#"
type A = A;

function main(): void {
    return;
}
"#,
  );

  common::assert_err(
    r#"
type A = A;

function main(): void {
    return;
}
"#,
    &["A0066"],
  );

  assert_snapshot!("type_alias_cycle", common::format_diagnostics(&result.output.diagnostics));
}

#[test]
fn type_alias_indirect_cycle() {
  let result = common::analyze(
    r#"
type A = B;
type B = A;

function main(): void {
    return;
}
"#,
  );

  common::assert_err(
    r#"
type A = B;
type B = A;

function main(): void {
    return;
}
"#,
    &["A0066"],
  );

  assert_snapshot!(
    "type_alias_indirect_cycle",
    common::format_diagnostics(&result.output.diagnostics)
  );
}

// =============================================================================
// Record/Enum static field tests
// =============================================================================

#[test]
fn static_field_no_init() {
  let result = common::analyze(
    r#"
record Config {
    static MAX: i32;
}

function main(): void {
    return;
}
"#,
  );

  common::assert_err(
    r#"
record Config {
    static MAX: i32;
}

function main(): void {
    return;
}
"#,
    &["A0065"],
  );

  assert_snapshot!("static_field_no_init", common::format_diagnostics(&result.output.diagnostics));
}

#[test]
fn enum_field_no_init() {
  let result = common::analyze(
    r#"
enum Color {
    Red,
    Green,

    value: i32;
}

function main(): void {
    return;
}
"#,
  );

  common::assert_err(
    r#"
enum Color {
    Red,
    Green,

    value: i32;
}

function main(): void {
    return;
}
"#,
    &["A0065"],
  );

  assert_snapshot!("enum_field_no_init", common::format_diagnostics(&result.output.diagnostics));
}

#[test]
fn static_on_enum_variant() {
  // Note: This error is detected at parse time, not analysis time.
  // The parser returns StaticOnEnumVariant diagnostic.
  // We use analyze_with_parse_errors to capture parser diagnostics.
  let result = common::analyze_allowing_parse_errors(
    r#"
enum Color {
    static Red,
}

function main(): void {
    return;
}
"#,
  );

  let codes: Vec<_> = result
    .output
    .diagnostics
    .iter()
    .map(|d| d.error_code.as_str())
    .collect();
  assert!(
    codes.contains(&"A0067"),
    "Expected A0067 (StaticOnEnumVariant), got: {:?}",
    codes
  );

  assert_snapshot!("static_on_enum_variant", common::format_diagnostics(&result.output.diagnostics));
}

#[test]
fn dot_on_type() {
  // Using . on a type should suggest using ::
  let result = common::analyze(
    r#"
record User {
    id: i32;

    static create(): User {
        return User { id: 0 };
    }
}

function main(): void {
    let u: User = User.create();
    return;
}
"#,
  );

  assert_snapshot!("dot_on_type", common::format_diagnostics(&result.output.diagnostics));
}

#[test]
fn double_colon_on_value() {
  // Using :: on a value should ideally error with A0063 (StaticAccessOnNonType)
  // Currently the resolver treats `u::id` as a path and looks up `u` as a type,
  // failing with A0058 (TypeNotFound) instead of detecting that `u` is a variable.
  let result = common::analyze(
    r#"
record User {
    id: i32;
}

function main(): void {
    let u: User = User { id: 42 };
    let x: i32 = u::id;
    return;
}
"#,
  );

  common::assert_err(
    r#"
record User {
    id: i32;
}

function main(): void {
    let u: User = User { id: 42 };
    let x: i32 = u::id;
    return;
}
"#,
    &["A0058"],
  );

  assert_snapshot!("double_colon_on_value", common::format_diagnostics(&result.output.diagnostics));
}

#[test]
fn static_field_not_const() {
  // Static field initialized with non-const expression should error (A0068)
  let result = common::analyze(
    r#"
function getValue(): i32 {
    return 42;
}

record Config {
    static VALUE: i32 = getValue();
}

function main(): void {
    return;
}
"#,
  );

  common::assert_err(
    r#"
function getValue(): i32 {
    return 42;
}

record Config {
    static VALUE: i32 = getValue();
}

function main(): void {
    return;
}
"#,
    &["A0068"],
  );

  assert_snapshot!("static_field_not_const", common::format_diagnostics(&result.output.diagnostics));
}

// ============================================================================
// for-of loop error tests
// ============================================================================

#[test]
fn for_of_non_iterable() {
  let result = common::analyze(
    r#"
function main(): void {
    let x: i32 = 42;
    for (let y of x) {
        return;
    }
}
"#,
  );

  assert_snapshot!("for_of_non_iterable", common::format_diagnostics(&result.output.diagnostics));
}

#[test]
fn for_of_mut_ref_on_immutable_array() {
  let result = common::analyze(
    r#"
function main(): void {
    let arr: i32[3] = [1, 2, 3];
    for (let x: &mut i32 of arr) {
        *x = 0;
    }
}
"#,
  );

  assert_snapshot!(
    "for_of_mut_ref_on_immutable_array",
    common::format_diagnostics(&result.output.diagnostics)
  );
}

// ============================================================================
// Error cascading prevention tests
// ============================================================================

#[test]
fn private_field_no_cascading_errors() {
  // When accessing a private field, only the "field is private" error should be emitted.
  // Previously, this would also emit "Binary operator cannot be applied to types 'error' and 'null'"
  // and "Cannot infer pointer type for null literal" as cascading errors.
  let result = common::analyze(
    r#"
record Foo {
    bar: *mut u8;

    public static init(): Foo {
        return Foo { bar: null };
    }
}

function main(): void {
    let foo: Foo = Foo::init();
    if (foo.bar != null) {
        return;
    }
}
"#,
  );

  let error_codes: Vec<&str> = result
    .output
    .diagnostics
    .iter()
    .map(|d| d.error_code.as_str())
    .collect();

  // Should only have the private field error, no cascading errors
  assert_eq!(
    error_codes,
    vec!["A0105"],
    "Expected only A0105 (private field), got: {:?}",
    error_codes
  );

  assert_snapshot!(
    "private_field_no_cascading_errors",
    common::format_diagnostics(&result.output.diagnostics)
  );
}

#[test]
fn extension_invalid_target_type() {
  common::assert_err(
    r#"
@extension(Foo)
function something(value: i32): i32 {
    return value;
}

function main(): void {
    return;
}
"#,
    &["A0135"],
  );
}

#[test]
fn extension_no_params() {
  common::assert_err(
    r#"
@extension(i32)
function noParams(): i32 {
    return 0;
}

function main(): void {
    return;
}
"#,
    &["A0136"],
  );
}

#[test]
fn extension_receiver_type_mismatch() {
  common::assert_err(
    r#"
@extension(i32)
function wrongReceiver(value: f64): f64 {
    return value;
}

function main(): void {
    return;
}
"#,
    &["A0137"],
  );
}

#[test]
fn extension_mut_on_immutable() {
  common::assert_err(
    r#"
@extension(string, mut)
function append(s: string, suffix: string): void {
    return;
}

function main(): void {
    let name: string = "hello";
    name.append(" world");
    return;
}
"#,
    &["A0076"],
  );
}

#[test]
fn extension_method_on_literal() {
  common::assert_err(
    r#"
@extension(i32)
function doubled(value: i32): i32 {
    return value * 2;
}

function main(): void {
    let _x: i32 = 10.doubled();
    return;
}
"#,
    &["A0138"],
  );
}

#[test]
fn extension_method_on_temporary_binop() {
  common::assert_err(
    r#"
@extension(i32)
function doubled(value: i32): i32 {
    return value * 2;
}

function main(): void {
    let a: i32 = 5;
    let b: i32 = 3;
    let _x: i32 = (a + b).doubled();
    return;
}
"#,
    &["A0139"],
  );
}

#[test]
fn extension_method_on_temporary_call() {
  common::assert_err(
    r#"
@extension(i32)
function doubled(value: i32): i32 {
    return value * 2;
}

function getNumber(): i32 {
    return 42;
}

function main(): void {
    let _x: i32 = getNumber().doubled();
    return;
}
"#,
    &["A0139"],
  );
}

#[test]
fn extension_method_on_temporary_unary() {
  common::assert_err(
    r#"
@extension(i32)
function doubled(value: i32): i32 {
    return value * 2;
}

function main(): void {
    let a: i32 = 5;
    let _x: i32 = (-a).doubled();
    return;
}
"#,
    &["A0139"],
  );
}

// ============================================================================
// Trait Error Tests
// ============================================================================

#[test]
fn trait_missing_required_method() {
  common::assert_err(
    r#"
trait Greetable {
    greet(&self): i32;
}

@implements(Greetable)
record Person {
    public age: i32;
}

function main(): void {
    return;
}
"#,
    &["A0140"],
  );
}

#[test]
fn trait_method_signature_mismatch() {
  common::assert_err(
    r#"
trait Greetable {
    greet(&self): i32;
}

@implements(Greetable)
record Person {
    public age: i32;

    greet(&self): void {
        return;
    }
}

function main(): void {
    return;
}
"#,
    &["A0141"],
  );
}

#[test]
fn trait_unknown_in_implements() {
  common::assert_err(
    r#"
@implements(NonexistentTrait)
record Foo {
    public x: i32;
}

function main(): void {
    return;
}
"#,
    &["A0142"],
  );
}

#[test]
fn trait_in_extern_block() {
  let result = common::analyze_allowing_parse_errors(
    r#"
extern C {
    trait Foo {
        bar(&self): void;
    }
}

function main(): void {
    return;
}
"#,
  );

  let codes: Vec<_> = result
    .output
    .diagnostics
    .iter()
    .map(|d| d.error_code.as_str())
    .collect();
  assert!(
    codes.contains(&"A0143"),
    "Expected A0143 (TraitInExternBlock), got: {:?}",
    codes
  );
}

#[test]
fn trait_with_field() {
  let result = common::analyze_allowing_parse_errors(
    r#"
trait Foo {
    x: i32;
}

function main(): void {
    return;
}
"#,
  );

  let codes: Vec<_> = result
    .output
    .diagnostics
    .iter()
    .map(|d| d.error_code.as_str())
    .collect();
  assert!(
    codes.contains(&"A0145"),
    "Expected A0145 (TraitFieldNotAllowed), got: {:?}",
    codes
  );
}

#[test]
fn trait_static_method() {
  common::assert_err(
    r#"
trait Foo {
    bar(): void;
}

function main(): void {
    return;
}
"#,
    &["A0146"],
  );
}

// =============================================================================
// Clone lang trait
// =============================================================================

#[test]
fn clone_missing_method() {
  common::assert_err(
    r#"
@implements(Clone)
record Foo {
    public x: i32;
}

function main(): void {
    return;
}
"#,
    &["A0132"],
  );
}

#[test]
fn clone_with_valid_method() {
  common::assert_ok(
    r#"
@implements(Clone)
record Foo {
    public x: i32;

    clone(&self): Foo {
        return Foo { x: self.x };
    }
}

function main(): void {
    return;
}
"#,
  );
}

#[test]
fn enum_drop_valid() {
  common::assert_ok(
    r#"
@implements(Drop)
enum Resource {
    Active(i32),
    Idle

    drop(&mut self): void {
        return;
    }
}

function main(): void {
    return;
}
"#,
  );
}

#[test]
fn enum_drop_missing_method() {
  let result = common::analyze(
    r#"
@implements(Drop)
enum Resource {
    Active(i32),
    Idle
}

function main(): void {
    return;
}
"#,
  );

  assert_snapshot!(
    "enum_drop_missing_method_diags",
    common::format_diagnostics(&result.output.diagnostics)
  );
}

#[test]
fn enum_copy_noncopy_payload() {
  let result = common::analyze(
    r#"
@implements(Copy)
enum MaybeMsg {
    Some(string),
    None
}

function main(): void {
    return;
}
"#,
  );

  assert_snapshot!(
    "enum_copy_noncopy_payload_diags",
    common::format_diagnostics(&result.output.diagnostics)
  );
}

#[test]
fn drop_on_complex_receiver_field_access() {
  // Calling .drop() on a field access is a complex receiver — O0010 warning.
  common::assert_err(
    r#"
@implements(Drop, Clone)
record Inner {
    value: i32;

    clone(&self): Inner {
        return Inner { value: self.value };
    }

    drop(&mut self): void {
        return;
    }
}

record Container {
    public inner: Inner;
}

function main(): void {
    let mut c: Container = Container { inner: Inner { value: 1 } };
    c.inner.drop();
    return;
}
"#,
    &["O0010"],
  );
}

#[test]
fn drop_on_simple_receiver_no_warning() {
  // Calling .drop() on a simple variable should NOT emit O0010.
  common::assert_ok(
    r#"
@implements(Drop, Clone)
record Foo {
    value: i32;

    clone(&self): Foo {
        return Foo { value: self.value };
    }

    drop(&mut self): void {
        return;
    }
}

function main(): void {
    let mut f: Foo = Foo { value: 1 };
    f.drop();
    return;
}
"#,
  );
}

#[test]
fn interprocedural_function_drop_marks_caller_value_dropped() {
  common::assert_err(
    r#"
@implements(Drop)
record Foo {
    value: i32;

    drop(&mut self): void {
        return;
    }
}

function takesAndDrops(x: &mut Foo): void {
    x.drop();
    return;
}

function consume(x: Foo): void {
    return;
}

function main(): void {
    let mut f: Foo = Foo { value: 1 };
    takesAndDrops(&mut f);
    consume(f);
    return;
}
"#,
    &["O0006"],
  );
}

#[test]
fn interprocedural_method_drop_marks_receiver_dropped() {
  common::assert_err(
    r#"
@implements(Drop)
record Foo {
    value: i32;

    drop(&mut self): void {
        return;
    }

    consume(&mut self): void {
        self.drop();
        return;
    }
}

function main(): void {
    let mut f: Foo = Foo { value: 1 };
    f.consume();
    f.drop();
    return;
}
"#,
    &["O0007"],
  );
}

#[test]
fn interprocedural_function_without_drop_does_not_consume_owner() {
  common::assert_ok(
    r#"
@implements(Drop)
record Foo {
    value: i32;

    drop(&mut self): void {
        return;
    }
}

function inspect(x: &mut Foo): void {
    return;
}

function consume(x: Foo): void {
    return;
}

function main(): void {
    let mut f: Foo = Foo { value: 1 };
    inspect(&mut f);
    consume(f);
    return;
}
"#,
  );
}

#[test]
fn interprocedural_conditional_drop_does_not_always_consume() {
  common::assert_ok(
    r#"
@implements(Drop)
record Foo {
    value: i32;

    drop(&mut self): void {
        return;
    }

    maybeDrop(&mut self, flag: i32): void {
        if (flag > 0) {
            self.drop();
        }
        return;
    }
}

function consume(x: Foo): void {
    return;
}

function main(): void {
    let mut f: Foo = Foo { value: 1 };
    f.maybeDrop(0);
    consume(f);
    return;
}
"#,
  );
}

#[test]
fn interprocedural_if_else_drop_consumes() {
  common::assert_err(
    r#"
@implements(Drop)
record Foo {
    value: i32;

    drop(&mut self): void {
        return;
    }
}

function alwaysDrop(x: &mut Foo, flag: i32): void {
    if (flag > 0) {
        takesAndDrops(x);
    } else {
        takesAndDrops(x);
    }
    return;
}

function takesAndDrops(x: &mut Foo): void {
    x.drop();
    return;
}

function consume(x: Foo): void {
    return;
}

function main(): void {
    let mut f: Foo = Foo { value: 1 };
    alwaysDrop(&mut f, 0);
    consume(f);
    return;
}
"#,
    &["O0006"],
  );
}

#[test]
fn interprocedural_loop_drop_not_guaranteed() {
  common::assert_ok(
    r#"
@implements(Drop)
record Foo {
    value: i32;

    drop(&mut self): void {
        return;
    }

    maybeDropInLoop(&mut self, runs: i32): void {
        while (runs > 0) {
            self.drop();
            break;
        }
        return;
    }
}

function consume(x: Foo): void {
    return;
}

function main(): void {
    let mut f: Foo = Foo { value: 1 };
    f.maybeDropInLoop(0);
    consume(f);
    return;
}
"#,
  );
}

#[test]
fn interprocedural_transitive_wrapper_drop_consumes() {
  common::assert_err(
    r#"
@implements(Drop)
record Foo {
    value: i32;

    drop(&mut self): void {
        return;
    }
}

function innerDrop(x: &mut Foo): void {
    x.drop();
    return;
}

function outerDrop(x: &mut Foo): void {
    innerDrop(x);
    return;
}

function consume(x: Foo): void {
    return;
}

function main(): void {
    let mut f: Foo = Foo { value: 1 };
    outerDrop(&mut f);
    consume(f);
    return;
}
"#,
    &["O0006"],
  );
}

#[test]
fn interprocedural_transitive_method_wrapper_drop_consumes() {
  common::assert_err(
    r#"
@implements(Drop)
record Foo {
    value: i32;

    drop(&mut self): void {
        return;
    }

    innerDrop(&mut self): void {
        self.drop();
        return;
    }

    outerDrop(&mut self): void {
        self.innerDrop();
        return;
    }
}

function main(): void {
    let mut f: Foo = Foo { value: 1 };
    f.outerDrop();
    f.drop();
    return;
}
"#,
    &["O0007"],
  );
}

#[test]
fn interprocedural_complex_mut_argument_warns_untracked_drop_effect() {
  common::assert_err(
    r#"
@implements(Drop)
record Foo {
    value: i32;

    drop(&mut self): void {
        return;
    }
}

record Container {
    public inner: Foo;
}

function takesAndDrops(x: &mut Foo): void {
    x.drop();
    return;
}

function main(): void {
    let mut c: Container = Container { inner: Foo { value: 1 } };
    takesAndDrops(&mut c.inner);
    return;
}
"#,
    &["O0011"],
  );
}
