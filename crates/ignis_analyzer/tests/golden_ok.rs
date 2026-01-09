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
extern function puts(s: string): i32;
"#,
  );

  assert_snapshot!(
    "extern_function_diags",
    common::format_diagnostics(&result.output.diagnostics)
  );
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

  assert_snapshot!(
    "mutable_variable_diags",
    common::format_diagnostics(&result.output.diagnostics)
  );
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
extern const PI: f64;

function area(radius: f64): f64 {
    return PI * radius * radius;
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

  assert_snapshot!(
    "export_function_diags",
    common::format_diagnostics(&result.output.diagnostics)
  );
  assert_snapshot!("export_function_hir", common::format_hir(&result));
}
