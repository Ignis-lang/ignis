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
fn e2e_ternary() {
  e2e_test(
    "ternary",
    r#"
function max(a: i32, b: i32): i32 {
    return a > b ? a : b;
}

function main(): i32 {
    return max(5, 10);
}
"#,
  );
}

#[test]
fn e2e_null_pointer_ops() {
  e2e_test(
    "null_pointer_ops",
    r#"
function main(): i32 {
    let mut values: i32[3] = [10, 20, 30];
    let p0: *mut i32 = (&mut values[0]) as *mut i32;
    let p1: *mut i32 = p0 + 1;
    let p2: *mut i32 = p1 + 1;
    let diff: i64 = p2 - p0;

    let mut bytes: u8[2] = [0b1, 0b10];
    let b0: *mut u8 = (&mut bytes[0]) as *mut u8;
    let b1: *mut u8 = b0 + 1;
    let bdiff: i64 = b1 - b0;

    if (p0 == null) {
        return 0;
    }

    if (p0 != null) {
        return (diff + bdiff) as i32;
    }

    return 0;
}
"#,
  );
}

#[test]
fn e2e_builtin_read_write() {
  e2e_test(
    "builtin_read_write",
    r#"
function main(): i32 {
    let mut value: i32 = 41;
    let ptr: *mut i32 = (&mut value) as *mut i32;
    __builtin_write<i32>(ptr, 42);
    let out: i32 = __builtin_read<i32>(ptr);
    return out;
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

// ============================================================================
// Type Alias Tests
// ============================================================================

#[test]
fn e2e_type_alias_basic() {
  e2e_test(
    "type_alias_basic",
    r#"
type Number = i32;

function twice(x: Number): Number {
    return x * 2;
}

function main(): i32 {
    let n: Number = 21;
    return twice(n);
}
"#,
  );
}

// ============================================================================
// Record Tests
// ============================================================================

#[test]
fn e2e_record_create_access() {
  e2e_test(
    "record_create_access",
    r#"
record Point {
    x: i32;
    y: i32;
}

function main(): i32 {
    let p: Point = Point { x: 10, y: 32 };
    return p.x + p.y;
}
"#,
  );
}

#[test]
fn e2e_record_instance_method() {
  e2e_test(
    "record_instance_method",
    r#"
record Counter {
    value: i32;

    get(): i32 {
        return self.value;
    }
}

function main(): i32 {
    let c: Counter = Counter { value: 42 };
    return c.get();
}
"#,
  );
}

#[test]
fn e2e_record_static_method() {
  e2e_test(
    "record_static_method",
    r#"
record Point {
    x: i32;
    y: i32;

    static origin(): Point {
        return Point { x: 0, y: 0 };
    }
}

function main(): i32 {
    let p: Point = Point::origin();
    return p.x + p.y;
}
"#,
  );
}

// ============================================================================
// Enum Tests
// ============================================================================

#[test]
fn e2e_enum_unit_variant() {
  e2e_test(
    "enum_unit_variant",
    r#"
enum Color {
    Red,
    Green,
    Blue,
}

function main(): i32 {
    let c: Color = Color::Red;
    return 42;
}
"#,
  );
}

#[test]
fn e2e_enum_with_payload() {
  e2e_test(
    "enum_with_payload",
    r#"
enum Option {
    Some(i32),
    None,
}

function wrap(x: i32): Option {
    return Option::Some(x);
}

function main(): i32 {
    let opt: Option = wrap(42);
    return 0;
}
"#,
  );
}

#[test]
fn e2e_record_static_field() {
  e2e_test(
    "record_static_field",
    r#"
record Config {
    static MAX_SIZE: i32 = 1024;
}

function main(): i32 {
    return Config::MAX_SIZE;
}
"#,
  );
}

#[test]
fn e2e_enum_static_method() {
  e2e_test(
    "enum_static_method",
    r#"
enum Result {
    Ok(i32),
    Err,

    success(value: i32): Result {
        return Result::Ok(value);
    }
}

function main(): i32 {
    let r: Result = Result::success(42);
    return 0;
}
"#,
  );
}

#[test]
fn e2e_enum_static_field() {
  e2e_test(
    "enum_static_field",
    r#"
enum Priority {
    Low,
    High,

    DEFAULT_LEVEL: i32 = 1;
}

function main(): i32 {
    return Priority::DEFAULT_LEVEL;
}
"#,
  );
}

#[test]
fn e2e_modifiers_parsed() {
  // public/private modifiers are parsed but ignored in v0.2
  e2e_test(
    "modifiers_parsed",
    r#"
record User {
    public name: i32;
    private age: i32;

    public getValue(): i32 {
        return self.name + self.age;
    }

    private static secret(): i32 {
        return 42;
    }
}

function main(): i32 {
    let u: User = User { name: 10, age: 5 };
    return u.getValue() + User::secret();
}
"#,
  );
}

#[test]
fn e2e_enum_explicit_static() {
  // explicit "static" on enum method is redundant but valid (ignored)
  e2e_test(
    "enum_explicit_static",
    r#"
enum Option {
    Some(i32),
    None,

    static none(): Option {
        return Option::None;
    }
}

function main(): i32 {
    let opt: Option = Option::none();
    return 0;
}
"#,
  );
}

// ============================================================================
// for-of loop tests
// ============================================================================

#[test]
fn e2e_for_of_fixed_array_by_value() {
  e2e_test(
    "for_of_fixed_array_by_value",
    r#"
function main(): i32 {
    let arr: i32[3] = [10, 20, 12];
    let mut sum: i32 = 0;
    for (let x of arr) {
        sum = sum + x;
    }
    return sum;
}
"#,
  );
}

#[test]
fn e2e_for_of_fixed_array_by_ref() {
  e2e_test(
    "for_of_fixed_array_by_ref",
    r#"
function main(): i32 {
    let arr: i32[3] = [1, 2, 39];
    let mut sum: i32 = 0;
    for (let x: &i32 of arr) {
        sum = sum + *x;
    }
    return sum;
}
"#,
  );
}

#[test]
fn e2e_for_of_fixed_array_mut_ref() {
  e2e_test(
    "for_of_fixed_array_mut_ref",
    r#"
function main(): i32 {
    let mut arr: i32[3] = [10, 20, 12];
    for (let x: &mut i32 of arr) {
        *x = *x + 1;
    }
    return arr[0] + arr[1] + arr[2];
}
"#,
  );
}

#[test]
fn e2e_for_of_nested() {
  e2e_test(
    "for_of_nested",
    r#"
function main(): i32 {
    let a: i32[2] = [1, 2];
    let b: i32[3] = [10, 20, 12];
    let mut sum: i32 = 0;
    for (let x of a) {
        for (let y of b) {
            sum = sum + x * y;
        }
    }
    return sum;
}
"#,
  );
}

#[test]
fn e2e_for_of_with_break() {
  e2e_test(
    "for_of_with_break",
    r#"
function main(): i32 {
    let arr: i32[5] = [1, 2, 3, 4, 5];
    let mut sum: i32 = 0;
    for (let x of arr) {
        if (x > 3) {
            break;
        }
        sum = sum + x;
    }
    return sum;
}
"#,
  );
}

#[test]
fn e2e_for_of_with_continue() {
  e2e_test(
    "for_of_with_continue",
    r#"
function main(): i32 {
    let arr: i32[5] = [1, 2, 3, 4, 5];
    let mut sum: i32 = 0;
    for (let x of arr) {
        if (x == 3) {
            continue;
        }
        sum = sum + x;
    }
    return sum;
}
"#,
  );
}

// =============================================================================
// Generics Tests
// =============================================================================

#[test]
fn e2e_generic_function_explicit() {
  e2e_test(
    "generic_function_explicit",
    r#"
function identity<T>(x: T): T {
    return x;
}

function main(): i32 {
    return identity<i32>(42);
}
"#,
  );
}

#[test]
fn e2e_generic_function_inferred() {
  e2e_test(
    "generic_function_inferred",
    r#"
function identity<T>(x: T): T {
    return x;
}

function main(): i32 {
    let a: i32 = identity(42);
    return a;
}
"#,
  );
}

#[test]
fn e2e_generic_function_multiple_params() {
  e2e_test(
    "generic_function_multiple_params",
    r#"
function first<T, U>(a: T, b: U): T {
    return a;
}

function main(): i32 {
    return first<i32, i32>(42, 0);
}
"#,
  );
}

#[test]
fn e2e_generic_record_box() {
  e2e_test(
    "generic_record_box",
    r#"
record Box<T> {
    value: T;
}

function main(): i32 {
    let b: Box<i32> = Box { value: 42 };
    return b.value;
}
"#,
  );
}

#[test]
fn e2e_generic_record_with_method() {
  e2e_test(
    "generic_record_with_method",
    r#"
record Box<T> {
    value: T;

    get(): T {
        return self.value;
    }
}

function main(): i32 {
    let b: Box<i32> = Box { value: 42 };
    return b.get();
}
"#,
  );
}

#[test]
fn e2e_generic_enum_option() {
  e2e_test(
    "generic_enum_option",
    r#"
enum Option<T> {
    Some(T),
    None
}

function main(): i32 {
    let x: Option<i32> = Option::Some(42);
    return 0;
}
"#,
  );
}

#[test]
fn e2e_generic_enum_result() {
  e2e_test(
    "generic_enum_result",
    r#"
enum Result<T, E> {
    Ok(T),
    Err(E)
}

function main(): i32 {
    let x: Result<i32, i32> = Result::Ok(42);
    let y: Result<i32, i32> = Result::Err(1);
    return 0;
}
"#,
  );
}

#[test]
fn e2e_generic_record_pair() {
  e2e_test(
    "generic_record_pair",
    r#"
record Pair<A, B> {
    first: A;
    second: B;
}

function main(): i32 {
    let p: Pair<i32, i32> = Pair { first: 10, second: 32 };
    return p.first + p.second;
}
"#,
  );
}

#[test]
fn e2e_generic_nested_types() {
  e2e_test(
    "generic_nested_types",
    r#"
record Box<T> {
    value: T;
}

function main(): i32 {
    let inner: Box<i32> = Box { value: 42 };
    let outer: Box<Box<i32> > = Box { value: inner };
    return outer.value.value;
}
"#,
  );
}

#[test]
fn e2e_generic_multiple_instantiations() {
  e2e_test(
    "generic_multiple_instantiations",
    r#"
record Box<T> {
    value: T;
    get(): T {
        return self.value;
    }
}

function main(): i32 {
    let a: Box<i32> = Box { value: 10 };
    let b: Box<i32> = Box { value: 32 };
    return a.get() + b.get();
}
"#,
  );
}
