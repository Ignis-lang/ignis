mod common;

use std::process::Command;
use tempfile::tempdir;

fn gcc_compiles(source: &str) {
  let c_code = common::compile_to_c(source);

  let temp_dir = tempdir().expect("Failed to create temp dir");
  let c_path = temp_dir.path().join("out.c");
  let o_path = temp_dir.path().join("out.o");
  let fixtures_dir = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures");

  std::fs::write(&c_path, &c_code).expect("Failed to write C file");

  let output = Command::new("gcc")
    .arg("-I")
    .arg(fixtures_dir)
    .args(["-c", c_path.to_str().unwrap(), "-o", o_path.to_str().unwrap()])
    .output()
    .expect("Failed to execute gcc");

  assert!(
    output.status.success(),
    "gcc failed:\nC code:\n{}\n\nstderr:\n{}",
    c_code,
    String::from_utf8_lossy(&output.stderr)
  );
}

#[test]
fn gcc_simple_add() {
  gcc_compiles(
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
}

#[test]
fn gcc_main_i32_wrapper() {
  gcc_compiles(
    r#"
function main(): i32 {
    return 7;
}
"#,
  );
}

#[test]
fn gcc_main_result_i32_wrapper() {
  gcc_compiles(
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
}

#[test]
fn gcc_main_with_args_wrapper() {
  gcc_compiles(
    r#"
function main(argc: i32, argv: *str): i32 {
    return argc;
}
"#,
  );
}

#[test]
fn gcc_pointer_deref() {
  gcc_compiles(
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
}

#[test]
fn gcc_conditionals() {
  gcc_compiles(
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
}

#[test]
fn gcc_all_integer_types() {
  gcc_compiles(
    r#"
function test_types(
    a: i8, b: i16, c: i32, d: i64,
    e: u8, f: u16, g: u32, h: u64
): i32 {
    return c;
}

function main(): void {
    return;
}
"#,
  );
}

#[test]
fn gcc_float_types() {
  gcc_compiles(
    r#"
function test_floats(a: f32, b: f64): f64 {
    return b;
}

function main(): void {
    return;
}
"#,
  );
}

#[test]
fn gcc_boolean() {
  gcc_compiles(
    r#"
function test_bool(a: boolean, b: boolean): boolean {
    return a && b;
}

function main(): void {
    let x: boolean = test_bool(true, false);
    return;
}
"#,
  );
}

#[test]
fn gcc_array_literal() {
  gcc_compiles(
    r#"
function add(a: i32, b: i32): i32 {
    return a + b;
}

function main(): void {
    let values: i32[5] = [1, 2, 3, 4, 5];
    for (let i = 0; i < 5; i++) {
        let result: i32 = add(values[i], values[i]);
    }
    return;
}
"#,
  );
}

#[test]
fn gcc_drop_glue_owned_field() {
  gcc_compiles(
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
}

#[test]
fn gcc_drop_glue_nested() {
  gcc_compiles(
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
}

#[test]
fn gcc_drop_glue_explicit_method() {
  gcc_compiles(
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
}

#[test]
fn gcc_no_drop_primitive_record() {
  gcc_compiles(
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
}

#[test]
fn gcc_drop_glue_multiple_owned_fields() {
  gcc_compiles(
    r#"
@implements(Drop)
record Owned {
    id: i32;
    drop(&mut self): void { return; }
}

record Person {
    first: Owned;
    last: Owned;
    age: i32;
}

function main(): void {
    let p: Person = Person { first: Owned { id: 1 }, last: Owned { id: 2 }, age: 30 };
    return;
}
"#,
  );
}

#[test]
fn gcc_structural_copy_nested_primitive() {
  gcc_compiles(
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
}

#[test]
fn gcc_drop_glue_inner_explicit_drop() {
  gcc_compiles(
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
}

#[test]
fn gcc_drop_glue_explicit_with_owned_field() {
  gcc_compiles(
    r#"
@implements(Drop)
record Owned {
    id: i32;
    drop(&mut self): void { return; }
}

@implements(Drop)
record Logger {
    inner: Owned;
    level: i32;

    drop(&mut self): void {
        return;
    }
}

function main(): void {
    let l: Logger = Logger { inner: Owned { id: 1 }, level: 3 };
    return;
}
"#,
  );
}
