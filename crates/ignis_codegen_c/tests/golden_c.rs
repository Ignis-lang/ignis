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
    function puts(s: string): i32;
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
//     function printf(format: string, ...args: u8[]): i32;
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
    let p: Pair<string, i32> = Pair { first: "hello", second: 42 };
    return;
}
"#,
  );
  assert!(c_code.contains("Pair____string____i32"));
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
    let b: string = identity<string>("hello");
    return;
}
"#,
  );
  assert!(c_code.contains("identity____i32"));
  assert!(c_code.contains("identity____string"));
  assert_snapshot!("c_generic_function_mangling", c_code);
}
