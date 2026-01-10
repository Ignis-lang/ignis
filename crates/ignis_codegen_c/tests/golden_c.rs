mod common;

use insta::assert_snapshot;

#[test]
fn c_simple_add() {
  let c_code = common::compile_to_c(
    r#"
function add(a: i32, b: i32): i32 {
    return a + b;
}

function main(): void {
    let x: i32 = add(1, 2);
    return;
}
"#,
  );
  assert_snapshot!("c_simple_add", c_code);
}

#[test]
fn c_arithmetic_ops() {
  let c_code = common::compile_to_c(
    r#"
function calc(a: i32, b: i32): i32 {
    let sum: i32 = a + b;
    let diff: i32 = a - b;
    let prod: i32 = a * b;
    let quot: i32 = a / b;
    return sum + diff + prod + quot;
}

function main(): void {
    let r: i32 = calc(10, 2);
    return;
}
"#,
  );
  assert_snapshot!("c_arithmetic_ops", c_code);
}

#[test]
fn c_pointer_deref() {
  let c_code = common::compile_to_c(
    r#"
function deref_add(a: &i32, b: i32): i32 {
    return *a + b;
}

function main(): void {
    let mut x: i32 = 42;
    let r: i32 = deref_add(&x, 10);
    return;
}
"#,
  );
  assert_snapshot!("c_pointer_deref", c_code);
}

#[test]
fn c_conditionals() {
  let c_code = common::compile_to_c(
    r#"
function max(a: i32, b: i32): i32 {
    if a > b {
        return a;
    } else {
        return b;
    }
}

function main(): void {
    let m: i32 = max(5, 10);
    return;
}
"#,
  );
  assert_snapshot!("c_conditionals", c_code);
}
