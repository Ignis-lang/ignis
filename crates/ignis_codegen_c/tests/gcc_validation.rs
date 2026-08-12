mod common;

use std::process::{Command, Stdio};
use std::sync::OnceLock;
use tempfile::tempdir;

fn gcc_compiles(source: &str) {
  gcc_compiles_c_source(&common::compile_to_c(source));
}

/// The standards to compile every fixture under, narrowed to what this host's gcc
/// actually accepts.
///
/// Both ends of the range matter and neither alone is enough. gcc 15 defaults to
/// gnu23, which accepts a struct redefinition as long as the member list is
/// identical, so C that is invalid for every earlier standard compiles clean on a
/// new host and fails on an older one. Checking only the older standard would trade
/// that blind spot for its mirror image: the driver passes no `-std`, so real builds
/// use whatever the host defaults to.
///
/// The C23 spelling is version-dependent — gcc 14 introduced `gnu23`, gcc 13 knows
/// only `gnu2x` — and an unrecognized `-std` is a hard driver error, so hardcoding
/// one spelling turns every test in this file red on the hosts that lack it.
fn supported_standards() -> &'static [String] {
  static STANDARDS: OnceLock<Vec<String>> = OnceLock::new();

  STANDARDS.get_or_init(|| {
    let mut standards = Vec::new();

    if gcc_accepts_standard("gnu17") {
      standards.push("gnu17".to_string());
    }

    for modern in ["gnu23", "gnu2x"] {
      if gcc_accepts_standard(modern) {
        standards.push(modern.to_string());
        break;
      }
    }

    assert!(!standards.is_empty(), "this host's gcc accepts none of gnu17, gnu23, gnu2x");

    standards
  })
}

fn gcc_accepts_standard(standard: &str) -> bool {
  Command::new("gcc")
    .arg(format!("-std={}", standard))
    .args(["-x", "c", "-fsyntax-only", "-"])
    .stdin(Stdio::null())
    .output()
    .expect("Failed to execute gcc")
    .status
    .success()
}

fn gcc_compiles_c_source(c_code: &str) {
  for standard in supported_standards() {
    gcc_compiles_c_source_under(c_code, standard);
  }
}

fn gcc_compiles_c_source_under(
  c_code: &str,
  standard: &str,
) {
  let temp_dir = tempdir().expect("Failed to create temp dir");
  let c_path = temp_dir.path().join("out.c");
  let o_path = temp_dir.path().join("out.o");
  let fixtures_dir = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures");

  std::fs::write(&c_path, c_code).expect("Failed to write C file");

  let output = Command::new("gcc")
    .arg(format!("-std={}", standard))
    .arg("-I")
    .arg(fixtures_dir)
    .args(["-c", c_path.to_str().unwrap(), "-o", o_path.to_str().unwrap()])
    .output()
    .expect("Failed to execute gcc");

  assert!(
    output.status.success(),
    "gcc -std={} failed:\nC code:\n{}\n\nstderr:\n{}",
    standard,
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
fn gcc_match_statement_arm_locals_feed_method_arguments() {
  gcc_compiles(
    r#"
enum MaybeI32 {
    SOME(i32),
    NONE,
}

record Box {
    value: i32;

    public static make(value: i32): Box {
        return Box { value: value };
    }

    public get(&self): i32 {
        return self.value;
    }
}

record Sink {
    value: i32;

    public set(&mut self, value: i32): void {
        self.value = value;
    }
}

function main(): void {
    let mut sink: Sink = Sink { value: 0 };
    let maybe: MaybeI32 = MaybeI32::SOME(1);

    match (maybe) {
        MaybeI32::SOME(value) -> {
            let boxed: Box = Box::make(value);
            sink.set(boxed.get());
        },
        MaybeI32::NONE -> {},
    };

    return;
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
fn gcc_slice_parameter_return_and_indexing() {
  gcc_compiles(
    r#"
function identity(values: i32[]): i32[] {
    return values;
}

function sumEdges(values: i32[]): i32 {
    return values[0] + values[2];
}

function main(): i32 {
    let data: i32[3] = [7, 11, 13];
    let view: i32[] = identity(data);
    return sumEdges(view);
}
"#,
  );
}

#[test]
fn gcc_str_slice_parameter_indexing() {
  gcc_compiles(
    r#"
function secondStartsWithB(labels: str[]): i32 {
    let second: str = labels[1];
    return ((second as *u8)[0]) as i32;
}

function main(): i32 {
    let labels: str[2] = ["a", "beta"];
    return secondStartsWithB(labels);
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
fn gcc_droppable_local_without_initializer_compiles() {
  gcc_compiles(
    r#"
@implements(Drop)
record Resource {
    id: i32;
    drop(&mut self): void { return; }
}

function main(): void {
    let mut resource: Resource;
    resource = Resource { id: 1 };
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

/// Exercises three distinct closure signatures, so a guard applied to only the
/// first entry of the emitter's signature loop still fails.
const THREE_CLOSURE_SIGNATURES: &str = r#"
function applyI32(value: i32, transform: (i32) -> i32): i32 {
    return transform(value);
}

function applyBool(value: i32, predicate: (i32) -> boolean): boolean {
    return predicate(value);
}

function applyPair(left: i32, right: i32, combine: (i32, i32) -> i32): i32 {
    return combine(left, right);
}

function main(): void {
    let _mapped: i32 = applyI32(1, (value: i32): i32 -> value + 1);
    let _tested: boolean = applyBool(1, (value: i32): boolean -> value > 0);
    let _joined: i32 = applyPair(1, 2, (left: i32, right: i32): i32 -> left + right);
    return;
}
"#;

/// Collect every closure struct tag defined in the generated C, paired with the
/// guard macro open immediately above it, if any.
fn closure_struct_definitions(c_code: &str) -> Vec<(String, Option<String>)> {
  let lines: Vec<&str> = c_code.lines().collect();

  lines
    .iter()
    .enumerate()
    .filter_map(|(index, line)| {
      let tag = line
        .trim_start()
        .strip_prefix("struct ")?
        .strip_suffix(" {")?
        .strip_prefix("__ignis_closure_")?;

      let guard = index
        .checked_sub(1)
        .and_then(|previous| lines[previous].strip_prefix("#define "))
        .map(str::to_string);

      Some((format!("__ignis_closure_{}", tag), guard))
    })
    .collect()
}

/// A translation unit includes its own module header, and that header defines the
/// closure struct for any signature another module consumes. The definition emitted
/// into the `.c` therefore has to be guarded, or the tag ends up defined twice in
/// one translation unit.
///
/// The guard macro also has to be derived from the struct tag: a guard that does not
/// track the name it protects would either suppress an unrelated definition or fail
/// to suppress the duplicate it exists for.
#[test]
fn every_closure_struct_definition_is_guarded_by_a_macro_naming_its_own_tag() {
  let c_code = common::compile_to_c(THREE_CLOSURE_SIGNATURES);

  let definitions = closure_struct_definitions(&c_code);

  assert_eq!(
    definitions.len(),
    3,
    "expected one struct per distinct closure signature:\n{}",
    c_code
  );

  for (tag, guard) in definitions {
    let guard = guard.unwrap_or_else(|| panic!("closure struct '{}' is not guarded:\n{}", tag, c_code));

    assert_eq!(
      guard,
      format!("IGNIS_TYPE_DEF_{}", tag.to_uppercase()),
      "the guard must be derived from the tag it protects:\n{}",
      c_code
    );
  }
}

/// Defining the same closure struct twice in one translation unit is accepted by
/// C23 and rejected by everything before it. The generated C must not rely on that.
#[test]
fn duplicated_closure_struct_definitions_survive_a_second_inclusion() {
  let c_code = common::compile_to_c(THREE_CLOSURE_SIGNATURES);

  let lines: Vec<&str> = c_code.lines().collect();

  let first_guard = lines
    .iter()
    .position(|line| line.starts_with("#ifndef IGNIS_TYPE_DEF___IGNIS_CLOSURE_"))
    .expect("expected a guarded closure struct definition in the generated C");

  let last_guard = lines
    .iter()
    .rposition(|line| line.starts_with("#endif // IGNIS_TYPE_DEF___IGNIS_CLOSURE_"))
    .expect("expected the closure struct guards to be closed");

  // Stands in for the module header, which defines the same structs under the same
  // guards whenever another module consumes the signatures. Every guarded block is
  // repeated, not only the first, and the copy is inserted in place rather than
  // prepended so the surrounding primitive typedefs still precede it.
  let mut duplicated: Vec<&str> = lines[..=last_guard].to_vec();
  duplicated.extend_from_slice(&lines[first_guard..=last_guard]);
  duplicated.extend_from_slice(&lines[last_guard + 1..]);

  gcc_compiles_c_source(&duplicated.join("\n"));
}
