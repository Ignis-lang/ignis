mod common;

use insta::assert_snapshot;

// =============================================================================
// Namespace mangling tests
// =============================================================================

#[test]
fn c_namespace_mangling() {
  let c_code = common::compile_to_c(
    r#"
namespace Math {
    function add(a: i32, b: i32): i32 {
        return a + b;
    }
}

function main(): void {
    let x: i32 = Math::add(1, 2);
    return;
}
"#,
  );
  assert_snapshot!("c_namespace_mangling", c_code);
}

#[test]
fn c_underscore_escaping() {
  // Test that underscores in names are escaped to avoid collisions.
  // `a::b_c` should produce `a_b__c` (underscore escaped)
  // `a_b::c` should produce `a__b_c` (underscore escaped)
  // These MUST be different C names.
  let c_code = common::compile_to_c(
    r#"
namespace a {
    function b_c(): i32 {
        return 1;
    }
}

namespace a_b {
    function c(): i32 {
        return 2;
    }
}

function main(): void {
    let x: i32 = a::b_c();
    let y: i32 = a_b::c();
    return;
}
"#,
  );

  // Verify they produce different names
  assert!(c_code.contains("a_b__c"), "Expected 'a_b__c' in output");
  assert!(c_code.contains("a__b_c"), "Expected 'a__b_c' in output");

  assert_snapshot!("c_underscore_escaping", c_code);
}

#[test]
fn c_extern_declarations() {
  let c_code = common::compile_to_c(
    r#"
extern libc {
    function puts(s: str): i32;
}

function main(): void {
    libc::puts("hello");
    return;
}
"#,
  );

  // Verify extern declaration is emitted
  assert!(c_code.contains("extern"), "Expected 'extern' declaration in output");

  assert_snapshot!("c_extern_declarations", c_code);
}

#[test]
fn c_extern_underscore_not_escaped() {
  // Extern names must not be escaped (underscores stay single).
  let c_code = common::compile_to_c(
    r#"
extern __rt {
    function ignis_alloc(size: u64): *mut u8;
    function ignis_memcpy(dest: *mut u8, src: *u8, n: u64): void;
}

function main(): void {
    let p: *mut u8 = __rt::ignis_alloc(16);
    __rt::ignis_memcpy(p, p as *u8, 8);
    return;
}
"#,
  );

  assert!(c_code.contains("ignis_alloc"), "single underscore");
  assert!(c_code.contains("ignis_memcpy"), "single underscore");
  assert!(!c_code.contains("ignis__alloc"), "no doubled underscores");
  assert!(!c_code.contains("ignis__memcpy"), "no doubled underscores");

  assert_snapshot!("c_extern_underscore_not_escaped", c_code);
}

// NOTE: Variadic syntax (...args: type[]) is not yet supported by the parser.
// This test will be enabled once variadic parameter parsing is implemented.
// #[test]
// fn c_extern_variadic() {
//   let c_code = common::compile_to_c(
//     r#"
// extern libc {
//     function printf(format: str, ...args: u8[]): i32;
// }
//
// function main(): void {
//     libc::printf("hello %d", 42);
//     return;
// }
// "#,
//   );
//
//   // Verify variadic is emitted as ...
//   assert!(c_code.contains("..."), "Expected variadic '...' in output");
//
//   assert_snapshot!("c_extern_variadic", c_code);
// }

// =============================================================================
// Basic tests
// =============================================================================

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
fn c_main_i32_wrapper() {
  let c_code = common::compile_to_c(
    r#"
function main(): i32 {
    return 7;
}
"#,
  );

  assert_snapshot!("c_main_i32_wrapper", c_code);
}

#[test]
fn c_main_result_i32_wrapper() {
  let c_code = common::compile_to_c(
    r#"
@lang(try)
enum Result<T, E> {
    OK(T),
    ERROR(E),
}

function main(): Result<i32, str> {
    return Result::OK(42);
}
"#,
  );

  assert_snapshot!("c_main_result_i32_wrapper", c_code);
}

#[test]
fn c_main_with_args_wrapper() {
  let c_code = common::compile_to_c(
    r#"
function main(argc: i32, argv: *str): i32 {
    return argc;
}
"#,
  );

  assert_snapshot!("c_main_with_args_wrapper", c_code);
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
    if (a > b) {
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

// =============================================================================
// Generic type mangling tests
// =============================================================================

#[test]
fn c_generic_box_mangling() {
  let c_code = common::compile_to_c(
    r#"
record Box<T> {
    value: T;
}

function main(): void {
    let b: Box<i32> = Box { value: 42 };
    return;
}
"#,
  );
  assert!(c_code.contains("Box____i32"));
  assert_snapshot!("c_generic_box_mangling", c_code);
}

#[test]
fn c_nested_generics_mangling() {
  let c_code = common::compile_to_c(
    r#"
record Box<T> {
    value: T;
}

function main(): void {
    let outer: Box<Box<i32> > = Box { value: Box { value: 42 } };
    return;
}
"#,
  );
  assert!(c_code.contains("Box____Box____i32"));
  assert_snapshot!("c_nested_generics_mangling", c_code);
}

#[test]
fn c_multi_param_mangling() {
  let c_code = common::compile_to_c(
    r#"
record Pair<A, B> {
    first: A;
    second: B;
}

function main(): void {
    let p: Pair<str, i32> = Pair { first: "hello", second: 42 };
    return;
}
"#,
  );
  assert!(c_code.contains("Pair____str____i32"));
  assert_snapshot!("c_multi_param_mangling", c_code);
}

#[test]
fn c_generic_function_mangling() {
  let c_code = common::compile_to_c(
    r#"
function identity<T>(x: T): T {
    return x;
}

function main(): void {
    let a: i32 = identity<i32>(42);
    let b: str = identity<str>("hello");
    return;
}
"#,
  );
  assert!(c_code.contains("identity____i32"));
  assert!(c_code.contains("identity____str"));
  assert_snapshot!("c_generic_function_mangling", c_code);
}

// =============================================================================
// Drop glue tests
// =============================================================================

#[test]
fn c_drop_glue_owned_field() {
  let c_code = common::compile_to_c(
    r#"
@implements(Drop)
record Owned {
    id: i32;
    drop(&mut self): void { return; }
}

record Named {
    inner: Owned;
    value: i32;
}

function main(): void {
    let n: Named = Named { inner: Owned { id: 1 }, value: 42 };
    return;
}
"#,
  );
  assert!(c_code.contains("Owned_drop"), "Expected Owned_drop call for owned field");
  assert_snapshot!("c_drop_glue_owned_field", c_code);
}

#[test]
fn c_drop_glue_nested_record() {
  let c_code = common::compile_to_c(
    r#"
@implements(Drop)
record Inner {
    tag: i32;
    drop(&mut self): void { return; }
}

record Outer {
    inner: Inner;
    code: i32;
}

function main(): void {
    let o: Outer = Outer { inner: Inner { tag: 1 }, code: 1 };
    return;
}
"#,
  );
  // Outer has no explicit drop, but Inner has @implements(Drop), so codegen
  // must recurse: Outer -> Inner -> Inner_drop
  assert!(c_code.contains("Inner_drop"), "Expected nested Inner_drop");
  assert_snapshot!("c_drop_glue_nested_record", c_code);
}

#[test]
fn c_drop_glue_explicit_drop_method() {
  let c_code = common::compile_to_c(
    r#"
@implements(Drop)
record Resource {
    id: i32;

    drop(&mut self): void {
        return;
    }
}

function main(): void {
    let r: Resource = Resource { id: 1 };
    return;
}
"#,
  );
  // With explicit @implements(Drop), codegen should call the drop method
  assert!(c_code.contains("Resource_drop"), "Expected Resource_drop call");
  assert_snapshot!("c_drop_glue_explicit_drop_method", c_code);
}

#[test]
fn c_no_drop_glue_primitive_record() {
  let c_code = common::compile_to_c(
    r#"
record Point {
    x: i32;
    y: i32;
}

function main(): void {
    let p: Point = Point { x: 1, y: 2 };
    return;
}
"#,
  );
  // All-primitive record: no drop calls at all
  assert!(!c_code.contains("_drop"), "No drop expected for primitive record");
  assert!(!c_code.contains("ignis_buf_drop"), "No buf drop expected for primitive record");
  assert_snapshot!("c_no_drop_glue_primitive_record", c_code);
}

#[test]
fn c_drop_glue_multiple_owned_fields() {
  let c_code = common::compile_to_c(
    r#"
@implements(Drop)
record Owned {
    id: i32;
    drop(&mut self): void { return; }
}

record Person {
    first: Owned;
    second: Owned;
    age: i32;
}

function main(): void {
    let p: Person = Person { first: Owned { id: 1 }, second: Owned { id: 2 }, age: 30 };
    return;
}
"#,
  );
  // Both owned fields must get drop calls
  let drop_count = c_code.matches("Owned_drop").count();
  assert!(drop_count >= 2, "Expected at least 2 Owned_drop calls, found {}", drop_count);
  assert_snapshot!("c_drop_glue_multiple_owned_fields", c_code);
}

#[test]
fn c_drop_glue_inner_explicit_drop() {
  let c_code = common::compile_to_c(
    r#"
@implements(Drop)
record Managed {
    value: i32;

    drop(&mut self): void {
        return;
    }
}

record Container {
    managed: Managed;
    tag: i32;
}

function main(): void {
    let c: Container = Container {
        managed: Managed { value: 99 },
        tag: 1
    };
    return;
}
"#,
  );
  // Container has no explicit drop, but its Managed field has one.
  // Codegen should call Managed's drop method for the inner field.
  assert!(c_code.contains("Managed_drop"), "Expected Managed_drop call for inner field");
  assert_snapshot!("c_drop_glue_inner_explicit_drop", c_code);
}

#[test]
fn c_drop_glue_explicit_with_owned_field() {
  let c_code = common::compile_to_c(
    r#"
@implements(Drop)
record Owned {
    id: i32;
    drop(&mut self): void { return; }
}

@implements(Drop)
record Logger {
    inner: Owned;
    tag: i32;

    drop(&mut self): void {
        return;
    }
}

function main(): void {
    let l: Logger = Logger { inner: Owned { id: 1 }, tag: 3 };
    return;
}
"#,
  );
  // With explicit @implements(Drop), the drop method is called,
  // NOT field-by-field glue. The user's drop method is responsible for cleanup.
  assert!(c_code.contains("Logger_drop"), "Expected Logger_drop call");
  assert_snapshot!("c_drop_glue_explicit_with_owned_field", c_code);
}

#[test]
fn c_no_drop_glue_nested_primitive() {
  let c_code = common::compile_to_c(
    r#"
record Vec2 {
    x: i32;
    y: i32;
}

record Rect {
    origin: Vec2;
    size: Vec2;
}

function main(): void {
    let r: Rect = Rect {
        origin: Vec2 { x: 0, y: 0 },
        size: Vec2 { x: 10, y: 20 }
    };
    return;
}
"#,
  );
  // Nested all-primitive records: no drops at all
  assert!(!c_code.contains("_drop"), "No drop expected for nested primitive records");
  assert!(
    !c_code.contains("ignis_buf_drop"),
    "No buf drop expected for nested primitive records"
  );
  assert_snapshot!("c_no_drop_glue_nested_primitive", c_code);
}

#[test]
fn c_drop_glue_manual_drop_sets_state() {
  let c_code = common::compile_to_c(
    r#"
@implements(Drop)
record Resource {
    id: i32;

    drop(&mut self): void {
        return;
    }
}

function main(): void {
    let mut r: Resource = Resource { id: 1 };
    r.drop();
    return;
}
"#,
  );
  // Manual .drop() should set __ignis_drop_state = 1 after the call
  assert!(
    c_code.contains("__ignis_drop_state = 1"),
    "Expected __ignis_drop_state = 1 after manual drop call"
  );
  assert_snapshot!("c_drop_glue_manual_drop_sets_state", c_code);
}
