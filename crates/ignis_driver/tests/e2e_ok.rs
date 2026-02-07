mod common;

use insta::assert_snapshot;

fn e2e_test(
  name: &str,
  source: &str,
) {
  let result = common::compile_and_run(source).expect(&format!("Compilation of '{}' failed", name));
  assert_snapshot!(name, common::format_e2e_result(&result));
}

fn e2e_no_warnings(
  name: &str,
  source: &str,
) {
  let warnings = common::compile_warnings(source).expect(&format!("Compilation of '{}' failed", name));
  assert!(warnings.is_empty(), "expected no warnings for '{}', got: {:?}", name, warnings);
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
    if (x > 5) {
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
    if (a > b) {
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
    if (n < 0) {
        return 0;
    } else {
        if (n == 0) {
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
    while (i <= n) {
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
    while (true) {
        i = i + 1;
        if (i >= 5) {
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
    if (n <= 1) {
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
    if (n <= 1) {
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
    
    if (5 == 5) { result = result + 1; }
    if (5 != 3) { result = result + 1; }
    if (3 < 5) { result = result + 1; }
    if (5 > 3) { result = result + 1; }
    if (5 <= 5) { result = result + 1; }
    if (5 >= 5) { result = result + 1; }
    
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
    if (n < 0) {
        return 0 - n;
    }
    return n;
}

function main(): i32 {
    let x: i32 = abs(0 - 42);
    if (is_even(x)) {
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
    public x: i32;
    public y: i32;
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
    public x: i32;
    public y: i32;

    public static origin(): Point {
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
fn e2e_enum_unit_comparison() {
  e2e_test(
    "enum_unit_comparison",
    r#"
enum Color {
    Red,
    Green,
    Blue,
}

function main(): i32 {
    let c1: Color = Color::Red;
    let c2: Color = Color::Red;
    let c3: Color = Color::Blue;

    if (c1 == c2) {
        if (c1 != c3) {
            return 42;
        }
    }
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
    public value: T;
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
    public first: A;
    public second: B;
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
    public value: T;
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

#[test]
fn e2e_maxof_i8() {
  e2e_test(
    "maxof_i8",
    r#"
function main(): i32 {
    let max: i8 = maxOf<i8>();
    return max as i32;
}
"#,
  );
}

#[test]
fn e2e_minof_i8() {
  e2e_test(
    "minof_i8",
    r#"
function main(): i32 {
    let min: i8 = minOf<i8>();
    return min as i32 + 128;
}
"#,
  );
}

#[test]
fn e2e_maxof_u8() {
  e2e_test(
    "maxof_u8",
    r#"
function main(): i32 {
    let max: u8 = maxOf<u8>();
    return max as i32 - 213;
}
"#,
  );
}

#[test]
fn e2e_minof_u8() {
  e2e_test(
    "minof_u8",
    r#"
function main(): i32 {
    let min: u8 = minOf<u8>();
    return min as i32;
}
"#,
  );
}

#[test]
fn e2e_maxof_generic() {
  e2e_test(
    "maxof_generic",
    r#"
function getMax<T>(): T {
    return maxOf<T>();
}

function main(): i32 {
    let max: i8 = getMax<i8>();
    return max as i32;
}
"#,
  );
}

#[test]
fn e2e_alignof_u8() {
  e2e_test(
    "alignof_u8",
    r#"
function main(): i32 {
    let a: u64 = alignOf<u8>();
    return a as i32;
}
"#,
  );
}

#[test]
fn e2e_alignof_u64() {
  e2e_test(
    "alignof_u64",
    r#"
function main(): i32 {
    let a: u64 = alignOf<u64>();
    return a as i32;
}
"#,
  );
}

#[test]
fn e2e_alignof_i32() {
  e2e_test(
    "alignof_i32",
    r#"
function main(): i32 {
    let a: u64 = alignOf<i32>();
    return a as i32;
}
"#,
  );
}

#[test]
fn e2e_alignof_fixed_vector() {
  e2e_test(
    "alignof_fixed_vector",
    r#"
function main(): i32 {
    let elemAlign: u64 = alignOf<u64>();
    let vecAlign: u64 = alignOf<u64[4]>();
    if (elemAlign == vecAlign) {
        return vecAlign as i32;
    }
    return 0;
}
"#,
  );
}

#[test]
fn e2e_alignof_generic() {
  e2e_test(
    "alignof_generic",
    r#"
function getAlign<T>(): u64 {
    return alignOf<T>();
}

function main(): i32 {
    let a: u64 = getAlign<u64>();
    return a as i32;
}
"#,
  );
}

#[test]
fn e2e_ptr_to_u64_cast() {
  e2e_test(
    "ptr_to_u64_cast",
    r#"
function main(): i32 {
    let mut x: i32 = 42;
    let p: *mut i32 = (&mut x) as *mut i32;
    let addr: u64 = p as u64;
    if (addr != 0) {
        return 42;
    }
    return 0;
}
"#,
  );
}

#[test]
fn e2e_u64_to_ptr_cast() {
  e2e_test(
    "u64_to_ptr_cast",
    r#"
function main(): i32 {
    let mut x: i32 = 42;
    let p: *mut i32 = (&mut x) as *mut i32;
    let addr: u64 = p as u64;
    let p2: *mut i32 = addr as *mut i32;
    return *p2;
}
"#,
  );
}

// =========================================================================
// INLINE KEYWORD TESTS
// =========================================================================

#[test]
fn e2e_inline_function() {
  e2e_test(
    "inline_function",
    r#"
inline function add(a: i32, b: i32): i32 {
    return a + b;
}

function main(): i32 {
    return add(10, 32);
}
"#,
  );
}

#[test]
fn e2e_inline_always() {
  e2e_test(
    "inline_always",
    r#"
inline(always) function twice(x: i32): i32 {
    return x + x;
}

function main(): i32 {
    return twice(21);
}
"#,
  );
}

#[test]
fn e2e_inline_never() {
  e2e_test(
    "inline_never",
    r#"
inline(never) function triple(x: i32): i32 {
    return x + x + x;
}

function main(): i32 {
    return triple(14);
}
"#,
  );
}

// ========================================================================
// @panic tests
// ========================================================================

#[test]
fn e2e_panic_message() {
  let result = common::compile_and_run(
    r#"
function main(): i32 {
    @panic("Test");
    return 0;
}
"#,
  )
  .expect("Compilation of 'panic_message' failed");

  assert_eq!(result.exit_code, 101);
  assert!(
    result.stderr.contains("panic: Test"),
    "Expected stderr to contain 'panic: Test', got: {}",
    result.stderr
  );
}

// ========================================================================
// @builtin(...) syntax tests
// ========================================================================

#[test]
fn e2e_builtin_sizeof_new_syntax() {
  e2e_test(
    "builtin_sizeof_new_syntax",
    r#"
function main(): i32 {
    let s: u64 = @sizeOf<i32>();
    return s as i32;
}
"#,
  );
}

#[test]
fn e2e_builtin_alignof_new_syntax() {
  e2e_test(
    "builtin_alignof_new_syntax",
    r#"
function main(): i32 {
    let a: u64 = @alignOf<i32>();
    return a as i32;
}
"#,
  );
}

#[test]
fn e2e_config_flag_false() {
  e2e_test(
    "config_flag_false",
    r#"
function main(): i32 {
    let flag: boolean = @configFlag("feature.nonexistent");
    if (flag) {
        return 1;
    }
    return 0;
}
"#,
  );
}

#[test]
fn e2e_config_flag_build_debug() {
  e2e_test(
    "config_flag_build_debug",
    r#"
function main(): i32 {
    let debug: boolean = @configFlag("build.debug");
    if (debug) {
        return 42;
    }
    return 0;
}
"#,
  );
}

#[test]
fn e2e_config_flag_os() {
  e2e_test(
    "config_flag_os",
    r#"
function main(): i32 {
    let isLinux: boolean = @configFlag("os.linux");
    if (isLinux) {
        return 1;
    }
    return 0;
}
"#,
  );
}

// ========================================================================
// @typeName tests
// ========================================================================

#[test]
fn e2e_typename_i32() {
  e2e_test(
    "typename_i32",
    r#"
function main(): i32 {
    let name: string = @typeName<i32>();
    return 0;
}
"#,
  );
}

#[test]
fn e2e_typename_record() {
  e2e_test(
    "typename_record",
    r#"
record Point {
    public x: i32;
    public y: i32;
}

function main(): i32 {
    let name: string = @typeName<Point>();
    return 0;
}
"#,
  );
}

// ========================================================================
// Pointer/cast builtin tests
// ========================================================================

#[test]
fn e2e_pointer_cast() {
  e2e_test(
    "pointer_cast",
    r#"
function main(): i32 {
    let mut x: i32 = 42;
    let p: *mut i32 = (&mut x) as *mut i32;
    let q: *mut u8 = @pointerCast<*mut u8>(p);
    let r: *mut i32 = @pointerCast<*mut i32>(q);
    return *r;
}
"#,
  );
}

#[test]
fn e2e_integer_from_pointer() {
  e2e_test(
    "integer_from_pointer",
    r#"
function main(): i32 {
    let mut x: i32 = 7;
    let p: *mut i32 = (&mut x) as *mut i32;
    let addr: u64 = @integerFromPointer(p);
    if (addr != 0) {
        return 7;
    }
    return 0;
}
"#,
  );
}

#[test]
fn e2e_pointer_from_integer() {
  e2e_test(
    "pointer_from_integer",
    r#"
function main(): i32 {
    let mut x: i32 = 99;
    let p: *mut i32 = (&mut x) as *mut i32;
    let addr: u64 = @integerFromPointer(p);
    let q: *mut i32 = @pointerFromInteger<*mut i32>(addr);
    return *q;
}
"#,
  );
}

#[test]
fn e2e_bitcast_i32_f32() {
  e2e_test(
    "bitcast_i32_f32",
    r#"
function main(): i32 {
    let bits: i32 = 1065353216;
    let f: f32 = @bitCast<f32>(bits);
    let back: i32 = @bitCast<i32>(f);
    return back - 1065353216;
}
"#,
  );
}

// =========================================================================
// Attribute Metadata Tests
// =========================================================================

#[test]
fn e2e_attr_packed() {
  e2e_test(
    "attr_packed",
    r#"
@packed
record Compact {
    a: i32;
    b: i8;
}

function main(): i32 {
    let s: u64 = @sizeOf<Compact>();
    return s as i32;
}
"#,
  );
}

#[test]
fn e2e_attr_aligned() {
  e2e_test(
    "attr_aligned",
    r#"
@aligned(16)
record Aligned16 {
    x: i32;
}

function main(): i32 {
    let a: u64 = @alignOf<Aligned16>();
    return a as i32;
}
"#,
  );
}

#[test]
fn e2e_attr_aligned_field() {
  e2e_test(
    "attr_aligned_field",
    r#"
record WithAlignedField {
    a: i8;
    @aligned(64)
    b: i32;
}

function main(): i32 {
    let s: u64 = @sizeOf<WithAlignedField>();
    return s as i32;
}
"#,
  );
}

#[test]
fn e2e_attr_cold() {
  e2e_test(
    "attr_cold",
    r#"
@cold
function rarelyUsed(): i32 {
    return 77;
}

function main(): i32 {
    return rarelyUsed();
}
"#,
  );
}

#[test]
fn e2e_attr_extern_name() {
  e2e_test(
    "attr_extern_name",
    r#"
@externName("my_custom_symbol")
export function internalName(): i32 {
    return 99;
}

function main(): i32 {
    return internalName();
}
"#,
  );
}

#[test]
fn e2e_attr_packed_aligned() {
  e2e_test(
    "attr_packed_aligned",
    r#"
@packed
@aligned(8)
record PackedAligned {
    a: i32;
    b: i8;
    c: i16;
}

function main(): i32 {
    let s: u64 = @sizeOf<PackedAligned>();
    return s as i32;
}
"#,
  );
}

// =========================================================================
// Lint Tests (no warnings expected)
// =========================================================================

#[test]
fn e2e_lint_allow_unused_variable() {
  e2e_no_warnings(
    "lint_allow_unused_variable",
    r#"
@allow(unused_variable)
function main(): i32 {
    let x: i32 = 5;
    return 0;
}
"#,
  );
}

#[test]
fn e2e_lint_underscore_suppresses_unused() {
  e2e_no_warnings(
    "lint_underscore_suppresses_unused",
    r#"
function main(): i32 {
    let _unused: i32 = 5;
    return 0;
}
"#,
  );
}
