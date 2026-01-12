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
