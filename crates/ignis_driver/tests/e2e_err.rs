mod common;

use insta::assert_snapshot;

fn e2e_error_test(
  name: &str,
  source: &str,
) {
  let diagnostics = common::compile_diagnostics(source).expect("lex/parse failed");
  assert!(!diagnostics.is_empty(), "expected errors");
  assert_snapshot!(name, diagnostics.join("\n"));
}

#[test]
fn e2e_err_index_out_of_bounds_positive() {
  e2e_error_test(
    "err_index_out_of_bounds_positive",
    r#"
function main(): i32 {
    let arr: i32[3] = [1, 2, 3];
    return arr[5];
}
"#,
  );
}

#[test]
fn e2e_err_index_out_of_bounds_negative() {
  e2e_error_test(
    "err_index_out_of_bounds_negative",
    r#"
function main(): i32 {
    let arr: i32[3] = [1, 2, 3];
    return arr[-1];
}
"#,
  );
}

#[test]
fn e2e_err_index_out_of_bounds_exact() {
  e2e_error_test(
    "err_index_out_of_bounds_exact",
    r#"
function main(): i32 {
    let arr: i32[3] = [1, 2, 3];
    return arr[3];
}
"#,
  );
}

#[test]
fn e2e_err_null_non_pointer() {
  e2e_error_test(
    "err_null_non_pointer",
    r#"
function main(): i32 {
    let x: i32 = null;
    return x;
}
"#,
  );
}

#[test]
fn e2e_err_null_uninferred() {
  e2e_error_test(
    "err_null_uninferred",
    r#"
function main(): i32 {
    null;
    return 0;
}
"#,
  );
}

#[test]
fn e2e_err_null_deref() {
  e2e_error_test(
    "err_null_deref",
    r#"
function main(): i32 {
    let x: i32 = *null;
    return x;
}
"#,
  );
}

#[test]
fn e2e_err_pointer_diff_mismatch() {
  e2e_error_test(
    "err_pointer_diff_mismatch",
    r#"
function main(): i32 {
    let mut a: i32 = 0;
    let mut b: f64 = 0.0;
    let pa: *mut i32 = &mut a as *mut i32;
    let pb: *mut f64 = &mut b as *mut f64;
    let d: i64 = pa - pb;
    return d as i32;
}
"#,
  );
}

#[test]
fn e2e_err_null_ptr_diff() {
  e2e_error_test(
    "err_null_ptr_diff",
    r#"
function main(): i32 {
    let d: i64 = null - null;
    return d as i32;
}
"#,
  );
}

#[test]
fn e2e_err_builtin_read_null() {
  e2e_error_test(
    "err_builtin_read_null",
    r#"
function main(): i32 {
    let value: i32 = __builtin_read<i32>(null);
    return value;
}
"#,
  );
}

#[test]
fn e2e_err_builtin_write_null() {
  e2e_error_test(
    "err_builtin_write_null",
    r#"
function main(): i32 {
    __builtin_write<i32>(null, 10);
    return 0;
}
"#,
  );
}

// ========================================================================
// @builtin(...) error tests
// ========================================================================

#[test]
fn e2e_err_compile_error() {
  e2e_error_test(
    "err_compile_error",
    r#"
function main(): void {
    @compileError("this should not compile");
}
"#,
  );
}

#[test]
fn e2e_err_unknown_builtin() {
  e2e_error_test(
    "err_unknown_builtin",
    r#"
function main(): void {
    @bogus();
}
"#,
  );
}

#[test]
fn e2e_err_config_flag_non_string() {
  e2e_error_test(
    "err_config_flag_non_string",
    r#"
function main(): void {
    let flag: boolean = @configFlag(42);
}
"#,
  );
}

#[test]
fn e2e_err_builtin_arg_count() {
  e2e_error_test(
    "err_builtin_arg_count",
    r#"
function main(): void {
    let flag: boolean = @configFlag();
}
"#,
  );
}

// =========================================================================
// Attribute Error Tests
// =========================================================================

#[test]
fn e2e_err_attr_unknown() {
  e2e_error_test(
    "err_attr_unknown",
    r#"
@bogusAttr
record Foo {
    x: i32;
}

function main(): void {
    return;
}
"#,
  );
}

#[test]
fn e2e_err_attr_packed_on_function() {
  e2e_error_test(
    "err_attr_packed_on_function",
    r#"
@packed
function foo(): void {
    return;
}

function main(): void {
    return;
}
"#,
  );
}

#[test]
fn e2e_err_attr_alignment_not_power_of_two() {
  e2e_error_test(
    "err_attr_alignment_not_power_of_two",
    r#"
@aligned(7)
record BadAlign {
    x: i32;
}

function main(): void {
    return;
}
"#,
  );
}
