mod common;

use insta::assert_snapshot;

fn e2e_test(
  name: &str,
  source: &str,
) {
  let result = common::compile_and_run(source).expect(&format!("Compilation of '{}' failed", name));
  assert_snapshot!(name, common::format_e2e_result(&result));
}

#[test]
fn e2e_empty_main() {
  e2e_test(
    "empty_main",
    r#"
function main(): void {
    return;
}
"#,
  );
}

#[test]
fn e2e_return_code() {
  e2e_test(
    "return_code",
    r#"
function main(): i32 {
    return 42;
}
"#,
  );
}

#[test]
fn e2e_return_zero() {
  e2e_test(
    "return_zero",
    r#"
function main(): i32 {
    return 0;
}
"#,
  );
}

#[test]
fn e2e_arithmetic_add() {
  e2e_test(
    "arithmetic_add",
    r#"
function add(a: i32, b: i32): i32 {
    return a + b;
}

function main(): i32 {
    return add(10, 32);
}
"#,
  );
}

#[test]
fn e2e_arithmetic_sub() {
  e2e_test(
    "arithmetic_sub",
    r#"
function main(): i32 {
    let a: i32 = 50;
    let b: i32 = 8;
    return a - b;
}
"#,
  );
}

#[test]
fn e2e_arithmetic_mul() {
  e2e_test(
    "arithmetic_mul",
    r#"
function main(): i32 {
    return 6 * 7;
}
"#,
  );
}

#[test]
fn e2e_arithmetic_div() {
  e2e_test(
    "arithmetic_div",
    r#"
function main(): i32 {
    return 100 / 10;
}
"#,
  );
}

#[test]
fn e2e_arithmetic_mod() {
  e2e_test(
    "arithmetic_mod",
    r#"
function main(): i32 {
    return 47 % 5;
}
"#,
  );
}

#[test]
fn e2e_if_then() {
  e2e_test(
    "if_then",
    r#"
function main(): i32 {
    let x: i32 = 10;
    if x > 5 {
        return 1;
    }
    return 0;
}
"#,
  );
}

#[test]
fn e2e_if_else() {
  e2e_test(
    "if_else",
    r#"
function max(a: i32, b: i32): i32 {
    if a > b {
        return a;
    } else {
        return b;
    }
}

function main(): i32 {
    return max(5, 10);
}
"#,
  );
}

#[test]
fn e2e_if_else_chain() {
  e2e_test(
    "if_else_chain",
    r#"
function classify(n: i32): i32 {
    if n < 0 {
        return 0;
    } else {
        if n == 0 {
            return 1;
        } else {
            return 2;
        }
    }
}

function main(): i32 {
    return classify(5);
}
"#,
  );
}

#[test]
fn e2e_while_loop() {
  e2e_test(
    "while_loop",
    r#"
function sum_to(n: i32): i32 {
    let mut total: i32 = 0;
    let mut i: i32 = 1;
    while i <= n {
        total = total + i;
        i = i + 1;
    }
    return total;
}

function main(): i32 {
    return sum_to(10);
}
"#,
  );
}

#[test]
fn e2e_while_with_break() {
  e2e_test(
    "while_with_break",
    r#"
function main(): i32 {
    let mut i: i32 = 0;
    while true {
        i = i + 1;
        if i >= 5 {
            break;
        }
    }
    return i;
}
"#,
  );
}

#[test]
fn e2e_for_loop() {
  e2e_test(
    "for_loop",
    r#"
function main(): i32 {
    let mut sum: i32 = 0;
    for (let i = 0; i < 5; i++) {
        sum = sum + i;
    }
    return sum;
}
"#,
  );
}

#[test]
fn e2e_for_loop_countdown() {
  e2e_test(
    "for_loop_countdown",
    r#"
function main(): i32 {
    let mut last: i32 = 0;
    for (let i = 10; i > 0; i--) {
        last = i;
    }
    return last;
}
"#,
  );
}

#[test]
fn e2e_array_literal() {
  e2e_test(
    "array_literal",
    r#"
function main(): i32 {
    let arr: i32[3] = [10, 20, 30];
    return arr[1];
}
"#,
  );
}

#[test]
fn e2e_array_sum() {
  e2e_test(
    "array_sum",
    r#"
function main(): i32 {
    let arr: i32[5] = [1, 2, 3, 4, 5];
    let mut sum: i32 = 0;
    for (let i = 0; i < 5; i++) {
        sum = sum + arr[i];
    }
    return sum;
}
"#,
  );
}

#[test]
fn e2e_mutable_variable() {
  e2e_test(
    "mutable_variable",
    r#"
function main(): i32 {
    let mut x: i32 = 10;
    x = x + 5;
    x = x * 2;
    return x;
}
"#,
  );
}

#[test]
fn e2e_reference_read() {
  e2e_test(
    "reference_read",
    r#"
function get_value(x: &i32): i32 {
    return *x;
}

function main(): i32 {
    let value: i32 = 42;
    return get_value(&value);
}
"#,
  );
}

#[test]
fn e2e_reference_mut() {
  e2e_test(
    "reference_mut",
    r#"
function increment(x: &mut i32): void {
    *x = *x + 1;
    return;
}

function main(): i32 {
    let mut value: i32 = 41;
    increment(&mut value);
    return value;
}
"#,
  );
}

#[test]
fn e2e_recursion_factorial() {
  e2e_test(
    "recursion_factorial",
    r#"
function factorial(n: i32): i32 {
    if n <= 1 {
        return 1;
    }
    return n * factorial(n - 1);
}

function main(): i32 {
    return factorial(5);
}
"#,
  );
}

#[test]
fn e2e_recursion_fibonacci() {
  e2e_test(
    "recursion_fibonacci",
    r#"
function fib(n: i32): i32 {
    if n <= 1 {
        return n;
    }
    return fib(n - 1) + fib(n - 2);
}

function main(): i32 {
    return fib(10);
}
"#,
  );
}

#[test]
fn e2e_comparison_ops() {
  e2e_test(
    "comparison_ops",
    r#"
function main(): i32 {
    let mut result: i32 = 0;
    
    if 5 == 5 { result = result + 1; }
    if 5 != 3 { result = result + 1; }
    if 3 < 5 { result = result + 1; }
    if 5 > 3 { result = result + 1; }
    if 5 <= 5 { result = result + 1; }
    if 5 >= 5 { result = result + 1; }
    
    return result;
}
"#,
  );
}

#[test]
fn e2e_bitwise_ops() {
  e2e_test(
    "bitwise_ops",
    r#"
function main(): i32 {
    let a: i32 = 12;
    let b: i32 = 10;
    
    let and_result: i32 = a & b;
    let or_result: i32 = a | b;
    
    return and_result + or_result;
}
"#,
  );
}

#[test]
fn e2e_shift_ops() {
  e2e_test(
    "shift_ops",
    r#"
function main(): i32 {
    let x: i32 = 4;
    let left: i32 = x << 2;
    let right: i32 = left >> 1;
    return right;
}
"#,
  );
}

#[test]
fn e2e_compound_assignment() {
  e2e_test(
    "compound_assignment",
    r#"
function main(): i32 {
    let mut x: i32 = 10;
    x += 5;
    x -= 3;
    x *= 2;
    return x;
}
"#,
  );
}

#[test]
fn e2e_type_cast() {
  e2e_test(
    "type_cast",
    r#"
function main(): i32 {
    let x: i64 = 42;
    let y: i32 = x as i32;
    return y;
}
"#,
  );
}

#[test]
fn e2e_nested_calls() {
  e2e_test(
    "nested_calls",
    r#"
function times_two(x: i32): i32 {
    return x * 2;
}

function add_one(x: i32): i32 {
    return x + 1;
}

function main(): i32 {
    return times_two(add_one(times_two(5)));
}
"#,
  );
}

#[test]
fn e2e_multiple_functions() {
  e2e_test(
    "multiple_functions",
    r#"
function is_even(n: i32): boolean {
    return n % 2 == 0;
}

function abs(n: i32): i32 {
    if n < 0 {
        return 0 - n;
    }
    return n;
}

function main(): i32 {
    let x: i32 = abs(0 - 42);
    if is_even(x) {
        return x;
    }
    return 0;
}
"#,
  );
}

#[test]
fn e2e_bitwise_xor() {
  e2e_test(
    "bitwise_xor",
    r#"
function main(): i32 {
    let a: i32 = 12;
    let b: i32 = 10;
    return a ^ b;
}
"#,
  );
}

#[test]
fn e2e_bitwise_not() {
  e2e_test(
    "bitwise_not",
    r#"
function main(): i32 {
    let x: i32 = 0;
    let result: i32 = ~x;
    return result;
}
"#,
  );
}

#[test]
fn e2e_for_loop_typed() {
  e2e_test(
    "for_loop_typed",
    r#"
function main(): i32 {
    let mut sum: i32 = 0;
    for (let i: i32 = 0; i < 5; i++) {
        sum = sum + i;
    }
    return sum;
}
"#,
  );
}

#[test]
fn e2e_char_literal() {
  e2e_test(
    "char_literal",
    r#"
function main(): i32 {
    let c: char = 'A';
    return c as i32;
}
"#,
  );
}

#[test]
fn e2e_char_escape() {
  e2e_test(
    "char_escape",
    r#"
function main(): i32 {
    let newline: char = '\n';
    return newline as i32;
}
"#,
  );
}

#[test]
fn e2e_char_unicode() {
  e2e_test(
    "char_unicode",
    r#"
function main(): i32 {
    let heart: char = '\u{2764}';
    return (heart as i32) % 256;
}
"#,
  );
}
