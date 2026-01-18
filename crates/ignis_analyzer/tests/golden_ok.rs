mod common;

use insta::assert_snapshot;

#[test]
fn basic_function() {
  let result = common::analyze(
    r#"
function main(): void {
    return;
}
"#,
  );

  assert_snapshot!("basic_function_diags", common::format_diagnostics(&result.output.diagnostics));
  assert_snapshot!("basic_function_hir", common::format_hir(&result));
}

#[test]
fn function_with_params() {
  let result = common::analyze(
    r#"
function add(a: i32, b: i32): i32 {
    return a + b;
}
"#,
  );

  assert_snapshot!(
    "function_with_params_diags",
    common::format_diagnostics(&result.output.diagnostics)
  );
  assert_snapshot!("function_with_params_hir", common::format_hir(&result));
}

#[test]
fn extern_function() {
  let result = common::analyze(
    r#"
extern C {
    function puts(s: string): i32;
}
"#,
  );

  assert_snapshot!("extern_function_diags", common::format_diagnostics(&result.output.diagnostics));
  assert_snapshot!("extern_function_hir", common::format_hir(&result));
}

#[test]
fn mutable_variable() {
  let result = common::analyze(
    r#"
function main(): void {
    let mut x: i32 = 1;
    x = 2;
    return;
}
"#,
  );

  assert_snapshot!("mutable_variable_diags", common::format_diagnostics(&result.output.diagnostics));
  assert_snapshot!("mutable_variable_hir", common::format_hir(&result));
}

#[test]
fn mutable_reference() {
  let result = common::analyze(
    r#"
function modify(x: &mut i32): void {
    x = 42;
    return;
}
"#,
  );

  assert_snapshot!(
    "mutable_reference_diags",
    common::format_diagnostics(&result.output.diagnostics)
  );
  assert_snapshot!("mutable_reference_hir", common::format_hir(&result));
}

#[test]
fn if_statement() {
  let result = common::analyze(
    r#"
function check(x: i32): i32 {
    if x > 0 {
        return 1;
    } else {
        return 0;
    }
}
"#,
  );

  assert_snapshot!("if_statement_diags", common::format_diagnostics(&result.output.diagnostics));
  assert_snapshot!("if_statement_hir", common::format_hir(&result));
}

#[test]
fn while_loop() {
  let result = common::analyze(
    r#"
function count(): i32 {
    let mut i: i32 = 0;
    while i < 10 {
        i = i + 1;
    }
    return i;
}
"#,
  );

  assert_snapshot!("while_loop_diags", common::format_diagnostics(&result.output.diagnostics));
  assert_snapshot!("while_loop_hir", common::format_hir(&result));
}

#[test]
fn for_loop() {
  let result = common::analyze(
    r#"
function sum(): i32 {
    let mut total: i32 = 0;
    for (let i = 0; i < 10; i = i + 1) {
        total = total + i;
    }
    return total;
}
"#,
  );

  assert_snapshot!("for_loop_diags", common::format_diagnostics(&result.output.diagnostics));
  assert_snapshot!("for_loop_hir", common::format_hir(&result));
}

#[test]
fn numeric_cast() {
  let result = common::analyze(
    r#"
function convert(): f64 {
    let x: i32 = 42;
    return x as f64;
}
"#,
  );

  assert_snapshot!("numeric_cast_diags", common::format_diagnostics(&result.output.diagnostics));
  assert_snapshot!("numeric_cast_hir", common::format_hir(&result));
}

#[test]
fn multiple_functions() {
  let result = common::analyze(
    r#"
function helper(x: i32): i32 {
    return x * 2;
}

function main(): i32 {
    return helper(21);
}
"#,
  );

  assert_snapshot!(
    "multiple_functions_diags",
    common::format_diagnostics(&result.output.diagnostics)
  );
  assert_snapshot!("multiple_functions_hir", common::format_hir(&result));
}

#[test]
fn extern_const() {
  let result = common::analyze(
    r#"
extern C {
    const PI: f64;
}

function area(radius: f64): f64 {
    return C::PI * radius * radius;
}
"#,
  );

  assert_snapshot!("extern_const_diags", common::format_diagnostics(&result.output.diagnostics));
  assert_snapshot!("extern_const_hir", common::format_hir(&result));
}

#[test]
fn export_function() {
  let result = common::analyze(
    r#"
export function add(a: i32, b: i32): i32 {
    return a + b;
}
"#,
  );

  assert_snapshot!("export_function_diags", common::format_diagnostics(&result.output.diagnostics));
  assert_snapshot!("export_function_hir", common::format_hir(&result));
}

#[test]
fn literal_adapts_to_expected_type() {
  let result = common::analyze(
    r#"
function test(): i8 {
    let x: i8 = 42;
    let y: i16 = 1000;
    let z: f32 = 3.14;
    return x;
}
"#,
  );

  assert_snapshot!("literal_adapts_diags", common::format_diagnostics(&result.output.diagnostics));
  assert_snapshot!("literal_adapts_hir", common::format_hir(&result));
}

#[test]
fn const_array() {
  let result = common::analyze(
    r#"
const PRIMES: i32[4] = [2, 3, 5, 7];

function get_second(): i32 {
    return PRIMES[1];
}
"#,
  );

  assert_snapshot!("const_array_diags", common::format_diagnostics(&result.output.diagnostics));
  assert_snapshot!("const_array_hir", common::format_hir(&result));
}

// ============================================================================
// Type Alias Tests
// ============================================================================

#[test]
fn type_alias_basic() {
  let result = common::analyze(
    r#"
type Int = i32;

function main(): void {
    let x: Int = 42;
    return;
}
"#,
  );

  assert_snapshot!("type_alias_basic_diags", common::format_diagnostics(&result.output.diagnostics));
  assert_snapshot!("type_alias_basic_hir", common::format_hir(&result));
}

#[test]
fn type_alias_in_function() {
  let result = common::analyze(
    r#"
type Number = i32;

function add(a: Number, b: Number): Number {
    return a + b;
}
"#,
  );

  assert_snapshot!(
    "type_alias_in_function_diags",
    common::format_diagnostics(&result.output.diagnostics)
  );
  assert_snapshot!("type_alias_in_function_hir", common::format_hir(&result));
}

// ============================================================================
// Record Tests
// ============================================================================

#[test]
fn record_basic() {
  let result = common::analyze(
    r#"
record Point {
    x: i32;
    y: i32;
}

function main(): void {
    return;
}
"#,
  );

  assert_snapshot!("record_basic_diags", common::format_diagnostics(&result.output.diagnostics));
  assert_snapshot!("record_basic_hir", common::format_hir(&result));
}

#[test]
fn record_field_access() {
  let result = common::analyze(
    r#"
record Point {
    public x: i32;
    public y: i32;
}

function main(): i32 {
    let p: Point = Point { x: 10, y: 20 };
    return p.x + p.y;
}
"#,
  );

  assert_snapshot!(
    "record_field_access_diags",
    common::format_diagnostics(&result.output.diagnostics)
  );
  assert_snapshot!("record_field_access_hir", common::format_hir(&result));
}

#[test]
fn record_instance_method() {
  let result = common::analyze(
    r#"
record Counter {
    value: i32;

    public get(): i32 {
        return self.value;
    }
}

function main(): i32 {
    let c: Counter = Counter { value: 42 };
    return c.get();
}
"#,
  );

  assert_snapshot!(
    "record_instance_method_diags",
    common::format_diagnostics(&result.output.diagnostics)
  );
  assert_snapshot!("record_instance_method_hir", common::format_hir(&result));
}

#[test]
fn record_static_method() {
  let result = common::analyze(
    r#"
record Point {
    public x: i32;
    public y: i32;

    public static origin(): Point {
        return Point { x: 0, y: 0 };
    }
}

function main(): i32 {
    let p: Point = Point::origin();
    return p.x;
}
"#,
  );

  assert_snapshot!(
    "record_static_method_diags",
    common::format_diagnostics(&result.output.diagnostics)
  );
  assert_snapshot!("record_static_method_hir", common::format_hir(&result));
}

// ============================================================================
// Enum Tests
// ============================================================================

#[test]
fn enum_unit_variants() {
  let result = common::analyze(
    r#"
enum Color {
    Red,
    Green,
    Blue,
}

function main(): Color {
    return Color::Red;
}
"#,
  );

  assert_snapshot!(
    "enum_unit_variants_diags",
    common::format_diagnostics(&result.output.diagnostics)
  );
  assert_snapshot!("enum_unit_variants_hir", common::format_hir(&result));
}

#[test]
fn enum_with_payload() {
  let result = common::analyze(
    r#"
enum Option {
    Some(i32),
    None,
}

function main(): Option {
    return Option::Some(42);
}
"#,
  );

  assert_snapshot!(
    "enum_with_payload_diags",
    common::format_diagnostics(&result.output.diagnostics)
  );
  assert_snapshot!("enum_with_payload_hir", common::format_hir(&result));
}
