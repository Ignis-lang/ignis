mod common;

use insta::assert_snapshot;

fn e2e_test(
  name: &str,
  source: &str,
) {
  let result = common::compile_and_run(source).expect(&format!("Compilation of '{}' failed", name));

  assert!(
    !result.leaked,
    "LeakSanitizer detected a memory leak in '{}':\n{}",
    name, result.leak_report,
  );

  assert_snapshot!(name, common::format_e2e_result(&result));
}

fn e2e_test_allow_leak(
  name: &str,
  source: &str,
) {
  let result = common::compile_and_run_no_lsan(source).expect(&format!("Compilation of '{}' failed", name));
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
    @write<i32>(ptr, 42);
    let out: i32 = @read<i32>(ptr);
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

// ========================================================================
// Never-type control flow tests
// ========================================================================

#[test]
fn e2e_never_no_missing_return() {
  e2e_no_warnings(
    "never_no_missing_return",
    r#"
function failHard(): i32 {
    @panic("fatal error");
}

function main(): i32 {
    return 0;
}
"#,
  );
}

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

// ========================================================================
// Lang Traits (@implements)
// ========================================================================

#[test]
fn e2e_drop_basic() {
  e2e_test(
    "drop_basic",
    r#"
@implements(Drop)
record Resource {
  public value: i32;

  drop(&mut self): void {
    return;
  }
}

function main(): i32 {
    let r: Resource = Resource { value: 42 };
    return 0;
}
"#,
  );
}

#[test]
fn e2e_copy_explicit() {
  e2e_test(
    "copy_explicit",
    r#"
@implements(Copy)
record Point {
  public x: i32;
  public y: i32;
}

function main(): i32 {
    let p: Point = Point { x: 10, y: 20 };
    let q: Point = p;
    return p.x + q.y;
}
"#,
  );
}

#[test]
fn e2e_structural_copy() {
  e2e_test(
    "structural_copy",
    r#"
record Point {
  public x: i32;
  public y: i32;
}

function main(): i32 {
    let p: Point = Point { x: 10, y: 32 };
    let q: Point = p;
    return p.x + q.y;
}
"#,
  );
}

#[test]
fn e2e_drop_glue() {
  e2e_test(
    "drop_glue",
    r#"
record Named {
  public name: string;
  public value: i32;
}

function main(): i32 {
    let n: Named = Named { name: "hello", value: 42 };
    return n.value;
}
"#,
  );
}

#[test]
fn e2e_nested_drop_glue() {
  e2e_test(
    "nested_drop_glue",
    r#"
record Inner {
  public label: string;
}

record Outer {
  public inner: Inner;
  public code: i32;
}

function main(): i32 {
    let o: Outer = Outer {
        inner: Inner { label: "test" },
        code: 42
    };
    return o.code;
}
"#,
  );
}

#[test]
fn e2e_drop_glue_multiple_strings() {
  e2e_test(
    "drop_glue_multiple_strings",
    r#"
record Person {
  public first: string;
  public last: string;
  public age: i32;
}

function main(): i32 {
    let p: Person = Person { first: "John", last: "Doe", age: 30 };
    return p.age;
}
"#,
  );
}

#[test]
fn e2e_structural_copy_nested() {
  e2e_test(
    "structural_copy_nested",
    r#"
record Vec2 {
  public x: i32;
  public y: i32;
}

record Rect {
  public origin: Vec2;
  public size: Vec2;
}

function main(): i32 {
    let r: Rect = Rect {
        origin: Vec2 { x: 0, y: 0 },
        size: Vec2 { x: 10, y: 20 }
    };
    let r2: Rect = r;
    return r.size.y + r2.origin.x;
}
"#,
  );
}

#[test]
fn e2e_drop_glue_inner_explicit_drop() {
  e2e_test(
    "drop_glue_inner_explicit_drop",
    r#"
@implements(Drop)
record Managed {
  public value: i32;

  drop(&mut self): void {
      return;
  }
}

record Container {
  public managed: Managed;
  public tag: i32;
}

function main(): i32 {
    let c: Container = Container {
        managed: Managed { value: 99 },
        tag: 42
    };
    return c.tag;
}
"#,
  );
}

#[test]
fn e2e_drop_glue_explicit_with_string() {
  // The custom drop method intentionally does nothing, so the string field leaks.
  // This tests that the user's drop is called (not that it's correct).
  e2e_test_allow_leak(
    "drop_glue_explicit_with_string",
    r#"
@implements(Drop)
record Logger {
  public name: string;
  public level: i32;

  drop(&mut self): void {
      return;
  }
}

function main(): i32 {
    let l: Logger = Logger { name: "app", level: 3 };
    return l.level;
}
"#,
  );
}

#[test]
fn e2e_drop_reads_self_field() {
  e2e_test(
    "drop_reads_self_field",
    r#"
@implements(Drop)
record Resource {
  public tag: i32;

  drop(&mut self): void {
      let _t: i32 = self.tag;
      return;
  }
}

function main(): i32 {
    let r: Resource = Resource { tag: 42 };
    return r.tag;
}
"#,
  );
}

#[test]
fn e2e_drop_writes_self_field() {
  e2e_test(
    "drop_writes_self_field",
    r#"
@implements(Drop)
record Handle {
  public id: i32;

  drop(&mut self): void {
      self.id = 0;
      return;
  }
}

function main(): i32 {
    let h: Handle = Handle { id: 7 };
    return h.id;
}
"#,
  );
}

#[test]
fn e2e_drop_conditional_on_self_field() {
  e2e_test(
    "drop_conditional_on_self_field",
    r#"
@implements(Drop)
record Guard {
  public active: boolean;
  public code: i32;

  drop(&mut self): void {
      if (self.active) {
          self.active = false;
      }
      return;
  }
}

function main(): i32 {
    let g: Guard = Guard { active: true, code: 99 };
    return g.code;
}
"#,
  );
}

#[test]
fn e2e_drop_reads_multiple_fields() {
  e2e_test(
    "drop_reads_multiple_fields",
    r#"
@implements(Drop)
record Pair {
  public a: i32;
  public b: i32;

  drop(&mut self): void {
      let _sum: i32 = self.a + self.b;
      self.a = 0;
      self.b = 0;
      return;
  }
}

function main(): i32 {
    let p: Pair = Pair { a: 10, b: 32 };
    return p.a + p.b;
}
"#,
  );
}

#[test]
fn e2e_extension_method_i32() {
  e2e_test(
    "extension_method_i32",
    r#"
@extension(i32)
function doubled(value: i32): i32 {
    return value * 2;
}

function main(): i32 {
    let x: i32 = 21;
    return x.doubled();
}
"#,
  );
}

#[test]
fn e2e_extension_method_with_args() {
  e2e_test(
    "extension_method_with_args",
    r#"
@extension(i32)
function add(value: i32, other: i32): i32 {
    return value + other;
}

function main(): i32 {
    let x: i32 = 30;
    return x.add(12);
}
"#,
  );
}

#[test]
fn e2e_extension_method_bool() {
  e2e_test(
    "extension_method_bool",
    r#"
@extension(boolean)
function toInt(value: boolean): i32 {
    if (value) {
        return 1;
    }
    return 0;
}

function main(): i32 {
    let b: boolean = true;
    return b.toInt();
}
"#,
  );
}

#[test]
fn e2e_extension_method_f64() {
  e2e_test(
    "extension_method_f64",
    r#"
@extension(f64)
function truncated(value: f64): i32 {
    return value as i32;
}

function main(): i32 {
    let x: f64 = 42.9;
    return x.truncated();
}
"#,
  );
}

#[test]
fn e2e_extension_method_multiple_on_same_type() {
  e2e_test(
    "extension_method_multiple_on_same_type",
    r#"
@extension(i32)
function doubled(value: i32): i32 {
    return value * 2;
}

@extension(i32)
function plusTen(value: i32): i32 {
    return value + 10;
}

function main(): i32 {
    let x: i32 = 16;
    return x.doubled() + x.plusTen();
}
"#,
  );
}

#[test]
fn e2e_extension_method_chain_via_variable() {
  e2e_test(
    "extension_method_chain_via_variable",
    r#"
@extension(i32)
function doubled(value: i32): i32 {
    return value * 2;
}

function main(): i32 {
    let x: i32 = 7;
    let y: i32 = x.doubled();
    return y.doubled();
}
"#,
  );
}

#[test]
fn e2e_extension_method_mut() {
  e2e_test(
    "extension_method_mut",
    r#"
@extension(i32, mut)
function addSelf(value: i32): i32 {
    return value + value;
}

function main(): i32 {
    let mut x: i32 = 21;
    return x.addSelf();
}
"#,
  );
}

#[test]
fn e2e_structural_copy_use_after_copy() {
  e2e_test(
    "structural_copy_use_after_copy",
    r#"
record Pair {
  public a: i32;
  public b: i32;
}

function main(): i32 {
    let p1: Pair = Pair { a: 10, b: 32 };
    let p2: Pair = p1;
    return p1.a + p2.b;
}
"#,
  );
}

// ============================================================================
// Trait Tests
// ============================================================================

#[test]
fn e2e_trait_basic() {
  e2e_test(
    "trait_basic",
    r#"
trait Greetable {
    greet(&self): i32;
}

@implements(Greetable)
record Person {
    public age: i32;

    greet(&self): i32 {
        return self.age;
    }
}

function main(): i32 {
    let p: Person = Person { age: 42 };
    return p.greet();
}
"#,
  );
}

#[test]
fn e2e_trait_default_method() {
  e2e_test(
    "trait_default_method",
    r#"
trait Describable {
    describe(&self): i32 {
        return 99;
    }
}

@implements(Describable)
record Item {
    public value: i32;
}

function main(): i32 {
    let item: Item = Item { value: 5 };
    return item.describe();
}
"#,
  );
}

#[test]
fn e2e_trait_override_default() {
  e2e_test(
    "trait_override_default",
    r#"
trait Describable {
    describe(&self): i32 {
        return 0;
    }
}

@implements(Describable)
record Item {
    public value: i32;

    describe(&self): i32 {
        return self.value;
    }
}

function main(): i32 {
    let item: Item = Item { value: 42 };
    return item.describe();
}
"#,
  );
}

#[test]
fn e2e_trait_default_method_uses_self() {
  e2e_test(
    "trait_default_method_uses_self",
    r#"
trait HasValue {
    getValue(&self): i32;

    doubled(&self): i32 {
        return self.getValue() * 2;
    }
}

@implements(HasValue)
record Box {
    public value: i32;

    getValue(&self): i32 {
        return self.value;
    }
}

function main(): i32 {
    let b: Box = Box { value: 21 };
    return b.doubled();
}
"#,
  );
}

#[test]
fn e2e_trait_two_records_same_default() {
  e2e_test(
    "trait_two_records_same_default",
    r#"
trait Answerable {
    answer(&self): i32 {
        return 42;
    }
}

@implements(Answerable)
record Alpha {
    public x: i32;
}

@implements(Answerable)
record Beta {
    public y: i32;
}

function main(): i32 {
    let a: Alpha = Alpha { x: 1 };
    let b: Beta = Beta { y: 2 };
    return a.answer() + b.answer() - 42;
}
"#,
  );
}

#[test]
fn e2e_trait_method_with_params() {
  e2e_test(
    "trait_method_with_params",
    r#"
trait Addable {
    addTo(&self, other: i32): i32;
}

@implements(Addable)
record Counter {
    public count: i32;

    addTo(&self, other: i32): i32 {
        return self.count + other;
    }
}

function main(): i32 {
    let c: Counter = Counter { count: 30 };
    return c.addTo(12);
}
"#,
  );
}

#[test]
fn e2e_trait_multiple_traits() {
  e2e_test(
    "trait_multiple_traits",
    r#"
trait Greetable {
    greet(&self): i32;
}

trait Describable {
    describe(&self): i32;
}

@implements(Greetable)
@implements(Describable)
record Person {
    public age: i32;

    greet(&self): i32 {
        return self.age;
    }

    describe(&self): i32 {
        return self.age + 1;
    }
}

function main(): i32 {
    let p: Person = Person { age: 20 };
    return p.greet() + p.describe();
}
"#,
  );
}

// =========================================================================
// for..of over record with data/length fields (Vector-like iteration)
// =========================================================================

#[test]
fn e2e_for_of_record_by_value() {
  e2e_test(
    "for_of_record_by_value",
    r#"
record IntVec {
    data: *mut i32;
    length: u64;
    capacity: u64;
}

function main(): i32 {
    let mut arr: i32[3] = [10, 20, 12];
    let vec: IntVec = IntVec { data: (&mut arr[0]) as *mut i32, length: 3, capacity: 3 };

    let mut sum: i32 = 0;
    for (let item of vec) {
        sum += item;
    }

    return sum;
}
"#,
  );
}

#[test]
fn e2e_for_of_record_by_ref() {
  e2e_test(
    "for_of_record_by_ref",
    r#"
record IntVec {
    data: *mut i32;
    length: u64;
    capacity: u64;
}

function main(): i32 {
    let mut arr: i32[3] = [5, 15, 22];
    let vec: IntVec = IntVec { data: (&mut arr[0]) as *mut i32, length: 3, capacity: 3 };

    let mut sum: i32 = 0;
    for (let item: &i32 of vec) {
        sum += *item;
    }

    return sum;
}
"#,
  );
}

#[test]
fn e2e_for_of_record_with_break() {
  e2e_test(
    "for_of_record_with_break",
    r#"
record IntVec {
    data: *mut i32;
    length: u64;
    capacity: u64;
}

function main(): i32 {
    let mut arr: i32[4] = [1, 2, 100, 4];
    let vec: IntVec = IntVec { data: (&mut arr[0]) as *mut i32, length: 4, capacity: 4 };

    let mut sum: i32 = 0;
    for (let item of vec) {
        if (item == 100) {
            break;
        }
        sum += item;
    }

    return sum;
}
"#,
  );
}

#[test]
fn e2e_for_of_record_with_continue() {
  e2e_test(
    "for_of_record_with_continue",
    r#"
record IntVec {
    data: *mut i32;
    length: u64;
    capacity: u64;
}

function main(): i32 {
    let mut arr: i32[4] = [1, 100, 3, 5];
    let vec: IntVec = IntVec { data: (&mut arr[0]) as *mut i32, length: 4, capacity: 4 };

    let mut sum: i32 = 0;
    for (let item of vec) {
        if (item == 100) {
            continue;
        }
        sum += item;
    }

    return sum;
}
"#,
  );
}

#[test]
fn e2e_drop_then_reassign_then_use() {
  e2e_test(
    "drop_then_reassign_then_use",
    r#"
@implements(Drop)
record Resource {
    public tag: i32;

    drop(&mut self): void {
        return;
    }
}

function main(): i32 {
    let mut r: Resource = Resource { tag: 10 };
    r.drop();
    r = Resource { tag: 77 };
    return r.tag;
}
"#,
  );
}

/// Verifies that the runtime drop guard prevents double-drop when auto-drop fires
/// on a variable that was already manually dropped (defense-in-depth with Phase 1).
#[test]
fn e2e_drop_guard_prevents_double_auto_drop() {
  e2e_test(
    "drop_guard_prevents_double_auto_drop",
    r#"
@implements(Drop)
record Counter {
    public value: i32;

    drop(&mut self): void {
        self.value = self.value + 1;
        return;
    }
}

function main(): i32 {
    let mut c: Counter = Counter { value: 0 };
    c.drop();
    return 0;
}
"#,
  );
}

// =========================================================================
// Clone: .clone() does not move the original
// =========================================================================

#[test]
fn e2e_clone_does_not_move() {
  e2e_test(
    "clone_does_not_move",
    r#"
@implements(Drop, Clone)
record Resource {
    public tag: i32;

    drop(&mut self): void {
        return;
    }

    clone(&self): Resource {
        return Resource { tag: self.tag };
    }
}

function consume(r: Resource): i32 {
    return r.tag;
}

function main(): i32 {
    let r: Resource = Resource { tag: 42 };
    let r2: Resource = r.clone();
    return consume(r) + consume(r2);
}
"#,
  );
}

// =========================================================================
// Enum Drop
// =========================================================================

#[test]
fn e2e_enum_drop_custom() {
  e2e_test(
    "enum_drop_custom",
    r#"
@implements(Drop)
enum Resource {
    Active(i32),
    Idle

    drop(&mut self): void {
        return;
    }
}

function main(): i32 {
    let r: Resource = Resource::Active(10);
    return 0;
}
"#,
  );
}

#[test]
fn e2e_enum_drop_manual() {
  e2e_test(
    "enum_drop_manual",
    r#"
@implements(Drop)
enum Resource {
    Active(i32),
    Idle

    drop(&mut self): void {
        return;
    }
}

function main(): i32 {
    let mut r: Resource = Resource::Active(42);
    r.drop();
    return 0;
}
"#,
  );
}

#[test]
fn e2e_enum_drop_owned_payload() {
  e2e_test(
    "enum_drop_owned_payload",
    r#"
enum MaybeMsg {
    Some(string),
    None
}

function main(): i32 {
    let m: MaybeMsg = MaybeMsg::Some("hello");
    return 0;
}
"#,
  );
}

// === Rc<T> tests ===

#[test]
fn e2e_rc_new_i32() {
  e2e_test(
    "rc_new_i32",
    r#"
function main(): i32 {
    let r: Rc<i32> = Rc::new(42);
    return 0;
}
"#,
  );
}

#[test]
fn e2e_rc_move() {
  e2e_test(
    "rc_move",
    r#"
function main(): i32 {
    let a: Rc<i32> = Rc::new(10);
    let b: Rc<i32> = a;
    return 0;
}
"#,
  );
}

#[test]
fn e2e_rc_clone() {
  e2e_test(
    "rc_clone",
    r#"
function main(): i32 {
    let a: Rc<i32> = Rc::new(10);
    let b: Rc<i32> = a.clone();
    return 0;
}
"#,
  );
}

#[test]
fn e2e_rc_pass_to_function() {
  e2e_test(
    "rc_pass_to_function",
    r#"
function consume(r: Rc<i32>): i32 {
    return 0;
}

function main(): i32 {
    let a: Rc<i32> = Rc::new(7);
    return consume(a);
}
"#,
  );
}

#[test]
fn e2e_rc_pass_clone_to_function() {
  e2e_test(
    "rc_pass_clone_to_function",
    r#"
function consume(r: Rc<i32>): i32 {
    return 0;
}

function main(): i32 {
    let a: Rc<i32> = Rc::new(7);
    let result: i32 = consume(a.clone());
    return result;
}
"#,
  );
}

#[test]
fn e2e_rc_return_from_function() {
  e2e_test(
    "rc_return_from_function",
    r#"
function make(): Rc<i32> {
    let r: Rc<i32> = Rc::new(99);
    return r;
}

function main(): i32 {
    let x: Rc<i32> = make();
    return 0;
}
"#,
  );
}

#[test]
fn e2e_rc_in_loop() {
  e2e_test(
    "rc_in_loop",
    r#"
function main(): i32 {
    let mut i: i32 = 0;
    while (i < 10) {
        let r: Rc<i32> = Rc::new(i);
        i = i + 1;
    }
    return 0;
}
"#,
  );
}

#[test]
fn e2e_rc_overwrite() {
  e2e_test(
    "rc_overwrite",
    r#"
function main(): i32 {
    let mut r: Rc<i32> = Rc::new(1);
    r = Rc::new(2);
    r = Rc::new(3);
    return 0;
}
"#,
  );
}

#[test]
fn e2e_rc_in_record() {
  e2e_test(
    "rc_in_record",
    r#"
record Holder {
    public value: Rc<i32>;
    public tag: i32;
}

function main(): i32 {
    let h: Holder = Holder { value: Rc::new(42), tag: 1 };
    return h.tag;
}
"#,
  );
}

#[test]
fn e2e_rc_multiple_clones() {
  e2e_test(
    "rc_multiple_clones",
    r#"
function main(): i32 {
    let a: Rc<i32> = Rc::new(5);
    let b: Rc<i32> = a.clone();
    let c: Rc<i32> = a.clone();
    let d: Rc<i32> = b.clone();
    return 0;
}
"#,
  );
}

#[test]
fn e2e_rc_custom_hooks_symbols_and_balance() {
  e2e_test(
    "rc_custom_hooks_symbols_and_balance",
    r#"
@lang(rc_runtime)
extern RcRuntimeCustom {
    @externName("custom_rc_alloc")
    function alloc(payload_size: u64, payload_align: u64, drop_fn: (*mut u8) -> void): *mut void;

    @externName("custom_rc_get")
    function get(handle: *mut void): *mut u8;

    @externName("custom_rc_retain")
    function retain(handle: *mut void): void;

    @externName("custom_rc_release")
    function release(handle: *mut void): void;
}

extern RcStats {
    @externName("ignis_test_rc_reset_stats")
    function reset(): void;

    @externName("ignis_test_rc_retain_count")
    function retainCount(): i32;

    @externName("ignis_test_rc_release_count")
    function releaseCount(): i32;
}

function main(): i32 {
    RcStats::reset();

    let a: Rc<i32> = Rc::new(10);
    let b: Rc<i32> = a.clone();

    let retains: i32 = RcStats::retainCount();
    let releases: i32 = RcStats::releaseCount();

    // retain once on clone, release happens twice at scope end (a and b)
    return retains * 10 + releases;
}
"#,
  );
}

// =========================================================================
// Library-level SharedRef (Rc prototype)
// =========================================================================

#[test]
fn e2e_library_rc_basic() {
  e2e_test(
    "library_rc_basic",
    r#"
extern __rc_ffi {
    @externName("ignis_rc_alloc")
    function ignis_rc_alloc(payload_size: u64, payload_align: u64, drop_fn: (*mut u8) -> void): *mut void;

    @externName("ignis_rc_get")
    function ignis_rc_get(handle: *mut void): *mut void;

    @externName("ignis_rc_retain")
    function ignis_rc_retain(handle: *mut void): void;

    @externName("ignis_rc_release")
    function ignis_rc_release(handle: *mut void): void;
}

@implements(Drop, Clone)
record SharedRef<T> {
    public handle: *mut void;

    public static new(value: T): SharedRef<T> {
        let h: *mut void = __rc_ffi::ignis_rc_alloc(
            @sizeOf<T>(), @alignOf<T>(), @dropGlue<T>()
        );
        let payload: *mut void = __rc_ffi::ignis_rc_get(h);
        @write<T>(payload as *mut T, value);
        return SharedRef { handle: h };
    }

    clone(&self): SharedRef<T> {
        __rc_ffi::ignis_rc_retain(self.handle);
        return SharedRef { handle: self.handle };
    }

    drop(&mut self): void {
        __rc_ffi::ignis_rc_release(self.handle);
    }
}

function main(): i32 {
    let a: SharedRef<i32> = SharedRef::new<i32>(42);
    let b: SharedRef<i32> = a.clone();

    let payload_b: *mut void = __rc_ffi::ignis_rc_get(b.handle);
    let val: i32 = @read<i32>(payload_b as *mut i32);
    return val;
}
"#,
  );
}

// =========================================================================
// Generic Clone
// =========================================================================

#[test]
fn e2e_generic_record_clone() {
  e2e_test(
    "generic_record_clone",
    r#"
@implements(Drop, Clone)
record Box<T> {
    public value: T;

    clone(&self): Box<T> {
        return Box<T> { value: self.value };
    }

    drop(&mut self): void {
        return;
    }
}

function consume(b: Box<i32>): i32 {
    return b.value;
}

function main(): i32 {
    let a: Box<i32> = Box<i32> { value: 21 };
    let b: Box<i32> = a.clone();
    return consume(a) + consume(b);
}
"#,
  );
}
