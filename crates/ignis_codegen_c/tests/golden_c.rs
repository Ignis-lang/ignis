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

// =============================================================================
// Drop glue tests
// =============================================================================

#[test]
fn c_drop_glue_string_field() {
  let c_code = common::compile_to_c(
    r#"
record Named {
    name: string;
    value: i32;
}

function main(): void {
    let n: Named = Named { name: "hello", value: 42 };
    return;
}
"#,
  );
  assert!(
    c_code.contains("ignis_string_drop"),
    "Expected ignis_string_drop call for string field"
  );
  assert_snapshot!("c_drop_glue_string_field", c_code);
}

#[test]
fn c_drop_glue_nested_record() {
  let c_code = common::compile_to_c(
    r#"
record Inner {
    label: string;
}

record Outer {
    inner: Inner;
    code: i32;
}

function main(): void {
    let o: Outer = Outer { inner: Inner { label: "test" }, code: 1 };
    return;
}
"#,
  );
  // Outer has no explicit drop, but Inner.label is a string, so codegen
  // must recurse: Outer -> Inner -> ignis_string_drop
  assert!(c_code.contains("ignis_string_drop"), "Expected nested ignis_string_drop");
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
  assert!(
    !c_code.contains("ignis_string_drop"),
    "No string drop expected for primitive record"
  );
  assert!(!c_code.contains("ignis_buf_drop"), "No buf drop expected for primitive record");
  assert_snapshot!("c_no_drop_glue_primitive_record", c_code);
}

#[test]
fn c_drop_glue_multiple_string_fields() {
  let c_code = common::compile_to_c(
    r#"
record Person {
    first: string;
    last: string;
    age: i32;
}

function main(): void {
    let p: Person = Person { first: "John", last: "Doe", age: 30 };
    return;
}
"#,
  );
  // Both string fields must get drop calls
  let drop_count = c_code.matches("ignis_string_drop").count();
  assert!(
    drop_count >= 2,
    "Expected at least 2 ignis_string_drop calls, found {}",
    drop_count
  );
  assert_snapshot!("c_drop_glue_multiple_string_fields", c_code);
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
fn c_drop_glue_explicit_with_string() {
  let c_code = common::compile_to_c(
    r#"
@implements(Drop)
record Logger {
    name: string;
    level: i32;

    drop(&mut self): void {
        return;
    }
}

function main(): void {
    let l: Logger = Logger { name: "app", level: 3 };
    return;
}
"#,
  );
  // With explicit @implements(Drop), the drop method is called,
  // NOT field-by-field glue. The user's drop method is responsible for cleanup.
  assert!(c_code.contains("Logger_drop"), "Expected Logger_drop call");
  assert_snapshot!("c_drop_glue_explicit_with_string", c_code);
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
  assert!(
    !c_code.contains("ignis_string_drop"),
    "No string drop for nested primitive records"
  );
  assert!(!c_code.contains("ignis_buf_drop"), "No buf drop for nested primitive records");
  assert!(!c_code.contains("_drop"), "No drop calls at all for nested primitive records");
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

// =============================================================================
// Rc<T> codegen tests
// =============================================================================

#[test]
fn c_rc_basic_drop() {
  let c_code = common::compile_to_c(
    r#"
@lang(rc_runtime)
extern RcRuntime {
    @externName("my_rc_alloc")
    function alloc(payload_size: u64, payload_align: u64, drop_fn: (*mut u8) -> void): *mut void;

    @externName("my_rc_get")
    function get(handle: *mut void): *mut u8;

    @externName("my_rc_retain")
    function retain(handle: *mut void): void;

    @externName("my_rc_release")
    function release(handle: *mut void): void;
}

function main(): i32 {
    let r: Rc<i32> = Rc::new(42);
    return 0;
}
"#,
  );

  assert!(c_code.contains("my_rc_alloc"), "Expected custom alloc hook call for Rc::new");
  assert!(
    c_code.contains("my_rc_release"),
    "Expected custom release hook call for Rc drop"
  );
  assert!(
    c_code.contains("my_rc_get"),
    "Expected custom get hook call for Rc payload write"
  );
  assert!(
    !c_code.contains("ignis_rc_alloc("),
    "Codegen should not hardcode ignis_rc_alloc"
  );
  assert_snapshot!("c_rc_basic_drop", c_code);
}

#[test]
fn c_rc_clone_retain() {
  let c_code = common::compile_to_c(
    r#"
@lang(rc_runtime)
extern RcRuntime {
    @externName("my_rc_alloc")
    function alloc(payload_size: u64, payload_align: u64, drop_fn: (*mut u8) -> void): *mut void;

    @externName("my_rc_get")
    function get(handle: *mut void): *mut u8;

    @externName("my_rc_retain")
    function retain(handle: *mut void): void;

    @externName("my_rc_release")
    function release(handle: *mut void): void;
}

function main(): i32 {
    let a: Rc<i32> = Rc::new(10);
    let b: Rc<i32> = a.clone();
    return 0;
}
"#,
  );

  assert!(
    c_code.contains("my_rc_retain"),
    "Expected custom retain hook when cloning an Rc"
  );

  let release_count = c_code.matches("my_rc_release").count();
  assert!(
    release_count >= 2,
    "Expected at least 2 custom release calls (one per Rc variable), got {}",
    release_count,
  );
  assert_snapshot!("c_rc_clone_retain", c_code);
}

#[test]
fn c_rc_record_field_drop() {
  let c_code = common::compile_to_c(
    r#"
@lang(rc_runtime)
extern RcRuntime {
    @externName("my_rc_alloc")
    function alloc(payload_size: u64, payload_align: u64, drop_fn: (*mut u8) -> void): *mut void;

    @externName("my_rc_get")
    function get(handle: *mut void): *mut u8;

    @externName("my_rc_retain")
    function retain(handle: *mut void): void;

    @externName("my_rc_release")
    function release(handle: *mut void): void;
}

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

  assert!(
    c_code.contains("my_rc_release"),
    "Expected custom release hook in field drop glue for Rc field"
  );
  assert_snapshot!("c_rc_record_field_drop", c_code);
}
