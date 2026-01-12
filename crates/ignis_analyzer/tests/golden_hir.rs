mod common;

use insta::assert_snapshot;

#[test]
fn hir_nested_blocks() {
  let result = common::analyze(
    r#"
function main(): void {
    {
        let x: i32 = 1;
        {
            let y: i32 = 2;
        }
    }
    return;
}
"#,
  );

  assert_snapshot!("hir_nested_blocks", common::format_hir(&result));
}

#[test]
fn hir_nested_if() {
  let result = common::analyze(
    r#"
function classify(x: i32): i32 {
    if x > 0 {
        if x > 10 {
            return 2;
        } else {
            return 1;
        }
    } else {
        return 0;
    }
}
"#,
  );

  assert_snapshot!("hir_nested_if", common::format_hir(&result));
}

#[test]
fn hir_loop_with_break() {
  let result = common::analyze(
    r#"
function search(): i32 {
    let mut i: i32 = 0;
    while true {
        if i > 10 {
            break;
        }
        i = i + 1;
    }
    return i;
}
"#,
  );

  assert_snapshot!("hir_loop_with_break", common::format_hir(&result));
}

#[test]
fn hir_loop_with_continue() {
  let result = common::analyze(
    r#"
function skip_evens(): i32 {
    let mut sum: i32 = 0;
    let mut i: i32 = 0;
    while i < 10 {
        i = i + 1;
        if i % 2 == 0 {
            continue;
        }
        sum = sum + i;
    }
    return sum;
}
"#,
  );

  assert_snapshot!("hir_loop_with_continue", common::format_hir(&result));
}

#[test]
fn hir_complex_expression() {
  let result = common::analyze(
    r#"
function compute(a: i32, b: i32, c: i32): i32 {
    return (a + b) * c - (a / b);
}
"#,
  );

  assert_snapshot!("hir_complex_expression", common::format_hir(&result));
}

#[test]
fn hir_reference_operations() {
  let result = common::analyze(
    r#"
function swap(a: &mut i32, b: &mut i32): void {
    let temp: i32 = *a;
    a = *b;
    b = temp;
    return;
}
"#,
  );

  assert_snapshot!("hir_reference_operations", common::format_hir(&result));
}

#[test]
fn hir_cast_chain() {
  let result = common::analyze(
    r#"
function convert(x: i32): f64 {
    let y: i64 = x as i64;
    return y as f64;
}
"#,
  );

  assert_snapshot!("hir_cast_chain", common::format_hir(&result));
}

#[test]
fn hir_multiple_declarations() {
  let result = common::analyze(
    r#"
extern io {
    function print(s: string): void;
}

function helper(): i32 {
    return 42;
}

function main(): void {
    let x: i32 = helper();
    return;
}
"#,
  );

  assert_snapshot!("hir_multiple_declarations", common::format_hir(&result));
}

#[test]
fn hir_unary_operators() {
  let result = common::analyze(
    r#"
function negate(x: i32): i32 {
    return -x;
}

function invert(b: bool): bool {
    return !b;
}
"#,
  );

  assert_snapshot!("hir_unary_operators", common::format_hir(&result));
}

#[test]
fn hir_comparison_operators() {
  let result = common::analyze(
    r#"
function compare(a: i32, b: i32): bool {
    let lt: bool = a < b;
    let le: bool = a <= b;
    let gt: bool = a > b;
    let ge: bool = a >= b;
    let eq: bool = a == b;
    let ne: bool = a != b;
    return eq;
}
"#,
  );

  assert_snapshot!("hir_comparison_operators", common::format_hir(&result));
}

#[test]
fn hir_logical_operators() {
  let result = common::analyze(
    r#"
function logic(a: bool, b: bool): bool {
    let and_result: bool = a && b;
    let or_result: bool = a || b;
    return and_result || or_result;
}
"#,
  );

  assert_snapshot!("hir_logical_operators", common::format_hir(&result));
}
