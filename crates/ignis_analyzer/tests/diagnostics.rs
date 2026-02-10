mod common;

/// Tests that verify specific error codes appear at specific lines.
/// These tests are more resilient to message text changes than snapshots.

#[test]
fn break_outside_loop_at_correct_line() {
  // Line 3: break
  common::assert_diagnostic_at_line(
    r#"
function main(): void {
    break;
}"#,
    "A0040", // BreakOutsideLoop
    3,
  );
}

#[test]
fn continue_outside_loop_at_correct_line() {
  // Line 3: continue
  common::assert_diagnostic_at_line(
    r#"
function main(): void {
    continue;
}"#,
    "A0041", // ContinueOutsideLoop
    3,
  );
}

#[test]
fn mutable_reference_to_immutable_at_correct_line() {
  // Line 4: &mut x
  common::assert_diagnostic_at_line(
    r#"
function main(): void {
    let x: i32 = 1;
    let y: &mut i32 = &mut x;
    return;
}"#,
    "A0014", // MutableReferenceToImmutable
    4,
  );
}

#[test]
fn argument_count_mismatch_at_call_site() {
  // Line 7: add(1)
  common::assert_diagnostic_at_line(
    r#"
function add(a: i32, b: i32): i32 {
    return a + b;
}

function main(): void {
    let x: i32 = add(1);
    return;
}"#,
    "A0017", // ArgumentCountMismatch
    7,
  );
}

#[test]
fn unreachable_code_after_return() {
  // Line 4: let x
  common::assert_diagnostic_at_line(
    r#"
function main(): void {
    return;
    let x: i32 = 1;
}"#,
    "A0025", // UnreachableCode
    4,
  );
}

#[test]
fn dereference_non_pointer_at_correct_line() {
  // Line 4: *x
  common::assert_diagnostic_at_line(
    r#"
function main(): void {
    let x: i32 = 1;
    let y: i32 = *x;
    return;
}"#,
    "A0021", // DereferenceNonPointer
    4,
  );
}

// ============================================================================
// Record/Enum Type Error Tests
// ============================================================================

#[test]
fn field_not_found_on_record() {
  // Line 9: p.z - field 'z' doesn't exist
  common::assert_diagnostic_at_line(
    r#"
record Point {
    x: i32;
    y: i32;
}

function main(): i32 {
    let p: Point = Point { x: 1, y: 2 };
    return p.z;
}"#,
    "A0054", // FieldNotFound
    9,
  );
}

#[test]
fn method_must_be_called() {
  // Line 12: c.get without ()
  common::assert_diagnostic_at_line(
    r#"
record Counter {
    value: i32;

    get(): i32 {
        return self.value;
    }
}

function main(): i32 {
    let c: Counter = Counter { value: 42 };
    return c.get;
}"#,
    "A0061", // MethodMustBeCalled
    12,
  );
}

#[test]
fn dot_access_on_non_record() {
  // Line 3: x.field - i32 is not a record
  common::assert_diagnostic_at_line(
    r#"
function main(): i32 {
    let x: i32 = 42;
    return x.field;
}"#,
    "A0060", // DotAccessOnNonRecord
    4,
  );
}

#[test]
fn unknown_field_in_init() {
  // Line 8: Point { z: 1 } - field 'z' doesn't exist
  common::assert_diagnostic_at_line(
    r#"
record Point {
    x: i32;
    y: i32;
}

function main(): Point {
    return Point { z: 1, x: 0, y: 0 };
}"#,
    "A0056", // UnknownField
    8,
  );
}

#[test]
fn duplicate_field_init() {
  // Line 8: Point { x: 1, x: 2 } - duplicate field
  common::assert_diagnostic_at_line(
    r#"
record Point {
    x: i32;
    y: i32;
}

function main(): Point {
    return Point { x: 1, x: 2, y: 0 };
}"#,
    "A0064", // DuplicateFieldInit
    8,
  );
}

#[test]
fn missing_field_init() {
  // Line 8: Point { x: 1 } - missing field 'y'
  common::assert_diagnostic_at_line(
    r#"
record Point {
    x: i32;
    y: i32;
}

function main(): Point {
    return Point { x: 1 };
}"#,
    "A0055", // MissingFieldInit
    8,
  );
}

#[test]
fn not_a_record_in_init() {
  // Line 8: Color { } - enum cannot use record init syntax
  common::assert_diagnostic_at_line(
    r#"
enum Color {
    Red,
    Green,
}

function main(): Color {
    return Color { };
}"#,
    "A0053", // NotARecord
    8,
  );
}

#[test]
fn rc_hooks_missing_provider() {
  common::assert_err(
    r#"
function main(): i32 {
    let value: Rc<i32> = Rc::new(42);
    return 0;
}
"#,
    &["A0150"],
  );
}

#[test]
fn rc_hooks_invalid_signature() {
  common::assert_err(
    r#"
@lang(rc_runtime)
extern RcRuntime {
    function alloc(payload_size: i32, payload_align: u64, drop_fn: (*mut u8) -> void): *mut void;
    function get(handle: *mut void): *mut u8;
    function retain(handle: *mut void): void;
    function release(handle: *mut void): void;
}

function main(): i32 {
    let value: Rc<i32> = Rc::new(42);
    return 0;
}
"#,
    &["A0153"],
  );
}

#[test]
fn rc_hooks_duplicate_provider() {
  common::assert_err(
    r#"
@lang(rc_runtime)
extern RcRuntimeA {
    function alloc(payload_size: u64, payload_align: u64, drop_fn: (*mut u8) -> void): *mut void;
    function get(handle: *mut void): *mut u8;
    function retain(handle: *mut void): void;
    function release(handle: *mut void): void;
}

@lang(rc_runtime)
extern RcRuntimeB {
    function alloc(payload_size: u64, payload_align: u64, drop_fn: (*mut u8) -> void): *mut void;
    function get(handle: *mut void): *mut u8;
    function retain(handle: *mut void): void;
    function release(handle: *mut void): void;
}

function main(): i32 {
    let value: Rc<i32> = Rc::new(42);
    return 0;
}
"#,
    &["A0151"],
  );
}

// TODO: enum_variant_requires_payload test
// Currently enum variants accessed via path syntax (Option::Some) are not
// resolved through the MemberAccess static access code path, so the
// EnumVariantRequiresPayload error is never emitted. This requires deeper
// changes to how paths are resolved.

#[test]
fn static_member_not_found() {
  // Line 8: Point::nonexistent - doesn't exist
  // Note: Currently paths like Point::nonexistent are resolved as full paths,
  // so they emit UndeclaredIdentifier instead of StaticMemberNotFound
  common::assert_diagnostic_at_line(
    r#"
record Point {
    x: i32;
    y: i32;
}

function main(): i32 {
    return Point::nonexistent;
}"#,
    "A0035", // UndeclaredIdentifier (path not found)
    8,
  );
}

#[test]
fn type_already_defined() {
  // Line 6: second Point definition
  common::assert_diagnostic_at_line(
    r#"
record Point {
    x: i32;
}

record Point {
    y: i32;
}

function main(): void {
    return;
}"#,
    "A0052", // TypeAlreadyDefined
    6,
  );
}

#[test]
fn duplicate_overload_signature() {
  // Line 3: Duplicate signature for foo
  common::assert_diagnostic_at_line(
    r#"
function foo(x: i32): void { return; }
function foo(x: i32): void { return; }

function main(): void {
    return;
}"#,
    "A0103", // DuplicateOverload
    3,
  );
}

#[test]
fn no_overload_matches() {
  // Line 6: No overload matches foo(true)
  common::assert_diagnostic_at_line(
    r#"
function foo(x: i32): void { return; }
function foo(x: string): void { return; }

function main(): void {
    foo(true);
    return;
}"#,
    "A0100", // NoOverloadMatches
    6,
  );
}

#[test]
fn ambiguous_overload() {
  // Line 6: Ambiguous overload for foo(1)
  common::assert_diagnostic_at_line(
    r#"
function foo<T>(x: T): void { return; }
function foo<U>(x: U): void { return; }

function main(): void {
    foo(1);
    return;
}"#,
    "A0101", // AmbiguousOverload
    6,
  );
}

#[test]
fn overload_group_as_value() {
  // Line 6: Overload group used as value
  common::assert_diagnostic_at_line(
    r#"
function foo(x: i32): void { return; }
function foo(x: string): void { return; }

function main(): void {
    let f: i32 = foo;
    return;
}"#,
    "A0102", // OverloadGroupAsValue
    6,
  );
}

#[test]
fn main_function_cannot_be_overloaded() {
  // Line 3: Attempt to overload main
  common::assert_diagnostic_at_line(
    r#"
function main(): void { return; }
function main(args: i32): void { return; }
"#,
    "A0104", // MainFunctionCannotBeOverloaded
    3,
  );
}

#[test]
fn overloaded_instance_methods() {
  common::assert_ok(
    r#"
record Box {
    value: i32;

    get(): i32 {
        return self.value;
    }

    get(label: string): i32 {
        return self.value;
    }
}

function main(): void {
    let b: Box = Box { value: 1 };
    let a: i32 = b.get();
    let c: i32 = b.get("x");
    return;
}"#,
  );
}
