mod common;

use insta::assert_snapshot;

#[test]
fn lir_simple_arithmetic() {
  let result = common::lower_to_lir(
    r#"
function add(a: i32, b: i32): i32 {
    return a + b;
}
"#,
  );

  common::assert_verifies(
    r#"
function add(a: i32, b: i32): i32 {
    return a + b;
}
"#,
  );

  assert_snapshot!("lir_simple_arithmetic", common::format_lir(&result));
}

#[test]
fn lir_local_variables() {
  let result = common::lower_to_lir(
    r#"
function compute(): i32 {
    let a: i32 = 10;
    let b: i32 = 20;
    let c: i32 = a + b;
    return c;
}
"#,
  );

  assert_snapshot!("lir_local_variables", common::format_lir(&result));
}

#[test]
fn lir_mutable_variables() {
  let result = common::lower_to_lir(
    r#"
function count(): i32 {
    let mut x: i32 = 0;
    x = x + 1;
    x = x + 1;
    return x;
}
"#,
  );

  assert_snapshot!("lir_mutable_variables", common::format_lir(&result));
}

#[test]
fn lir_if_else() {
  let result = common::lower_to_lir(
    r#"
function max(a: i32, b: i32): i32 {
    if a > b {
        return a;
    } else {
        return b;
    }
}
"#,
  );

  assert_snapshot!("lir_if_else", common::format_lir(&result));
}

#[test]
fn lir_if_without_else() {
  let result = common::lower_to_lir(
    r#"
function clamp_positive(x: i32): i32 {
    let mut result: i32 = x;
    if x < 0 {
        result = 0;
    }
    return result;
}
"#,
  );

  assert_snapshot!("lir_if_without_else", common::format_lir(&result));
}

#[test]
fn lir_while_loop() {
  let result = common::lower_to_lir(
    r#"
function sum_to_n(n: i32): i32 {
    let mut sum: i32 = 0;
    let mut i: i32 = 0;
    while i < n {
        sum = sum + i;
        i = i + 1;
    }
    return sum;
}
"#,
  );

  assert_snapshot!("lir_while_loop", common::format_lir(&result));
}

#[test]
fn lir_infinite_loop_with_break() {
  let result = common::lower_to_lir(
    r#"
function find_limit(): i32 {
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

  assert_snapshot!("lir_infinite_loop_with_break", common::format_lir(&result));
}

#[test]
fn lir_loop_with_continue() {
  let result = common::lower_to_lir(
    r#"
function sum_odds(n: i32): i32 {
    let mut sum: i32 = 0;
    let mut i: i32 = 0;
    while i < n {
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

  assert_snapshot!("lir_loop_with_continue", common::format_lir(&result));
}

#[test]
fn lir_function_call() {
  let result = common::lower_to_lir(
    r#"
function helper(x: i32): i32 {
    return x * 2;
}

function main(): i32 {
    let result: i32 = helper(21);
    return result;
}
"#,
  );

  assert_snapshot!("lir_function_call", common::format_lir(&result));
}

#[test]
fn lir_type_cast() {
  let result = common::lower_to_lir(
    r#"
function convert(x: i32): f64 {
    let y: i64 = x as i64;
    return y as f64;
}
"#,
  );

  assert_snapshot!("lir_type_cast", common::format_lir(&result));
}

#[test]
fn lir_unary_operators() {
  let result = common::lower_to_lir(
    r#"
function negate(x: i32): i32 {
    return -x;
}

function invert(b: bool): bool {
    return !b;
}
"#,
  );

  assert_snapshot!("lir_unary_operators", common::format_lir(&result));
}

#[test]
fn lir_comparison_operators() {
  let result = common::lower_to_lir(
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

  assert_snapshot!("lir_comparison_operators", common::format_lir(&result));
}

#[test]
fn lir_logical_and() {
  let result = common::lower_to_lir(
    r#"
function both(a: bool, b: bool): bool {
    return a && b;
}
"#,
  );

  assert_snapshot!("lir_logical_and", common::format_lir(&result));
}

#[test]
fn lir_logical_or() {
  let result = common::lower_to_lir(
    r#"
function either(a: bool, b: bool): bool {
    return a || b;
}
"#,
  );

  assert_snapshot!("lir_logical_or", common::format_lir(&result));
}

#[test]
fn lir_compound_assignment() {
  let result = common::lower_to_lir(
    r#"
function accumulate(): i32 {
    let mut x: i32 = 10;
    x = x + 5;
    x = x - 3;
    x = x * 2;
    return x;
}
"#,
  );

  assert_snapshot!("lir_compound_assignment", common::format_lir(&result));
}

#[test]
fn lir_extern_function() {
  let result = common::lower_to_lir(
    r#"
extern function print(msg: string): void;

function greet(): void {
    print("hello");
    return;
}
"#,
  );

  assert_snapshot!("lir_extern_function", common::format_lir(&result));
}

#[test]
fn lir_nested_blocks() {
  let result = common::lower_to_lir(
    r#"
function nested(): i32 {
    let x: i32 = 1;
    {
        let y: i32 = 2;
        {
            let z: i32 = x + y;
            return z;
        }
    }
}
"#,
  );

  assert_snapshot!("lir_nested_blocks", common::format_lir(&result));
}

#[test]
fn lir_void_function() {
  let result = common::lower_to_lir(
    r#"
function do_nothing(): void {
    return;
}
"#,
  );

  assert_snapshot!("lir_void_function", common::format_lir(&result));
}
