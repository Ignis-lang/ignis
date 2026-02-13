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

#[test]
fn enum_method_using_self_requires_explicit_self_param() {
  common::assert_diagnostic_at_line(
    r#"
enum Option<T> {
    Some(T),
    None,

    unwrap(): T {
        return match (self) {
            Option::Some(value) -> value,
            Option::None -> @panic("Option is None"),
        };
    }
}
"#,
    "A0156", // MethodUsesSelfWithoutSelfParameter
    7,
  );
}

#[test]
fn match_guard_must_be_boolean() {
  common::assert_diagnostic_at_line(
    r#"
function main(): i32 {
    let x: i32 = 1;
    return match (x) {
        y if y -> 1,
        _ -> 0,
    };
}
"#,
    "A0162", // GuardNotBoolean
    5,
  );
}

#[test]
fn match_or_pattern_disallows_bindings() {
  common::assert_diagnostic_at_line(
    r#"
function main(): i32 {
    let x: i32 = 1;
    return match (x) {
        a | 2 -> 1,
        _ -> 0,
    };
}
"#,
    "A0157", // OrPatternBindingsDisallowed
    5,
  );
}

#[test]
fn match_unknown_multi_segment_variant_path() {
  common::assert_diagnostic_at_line(
    r#"
enum Option {
    Some(i32),
    None,
}

function main(): i32 {
    let x: Option = Option::Some(1);
    return match (x) {
        Unknown::Some(_) -> 1,
        _ -> 0,
    };
}
"#,
    "A0160", // UnknownVariant
    10,
  );
}

#[test]
fn match_arm_types_must_unify() {
  common::assert_diagnostic_at_line(
    r#"
function main(): i32 {
    let x: i32 = 1;
    return match (x) {
        1 -> 1,
        _ -> true,
    };
}
"#,
    "A0163", // MatchArmTypeMismatch
    4,
  );
}

#[test]
fn match_bindings_are_scoped_to_arm() {
  common::assert_diagnostic_at_line(
    r#"
function main(): i32 {
    let x: i32 = 1;
    let y: i32 = match (x) {
        value -> value,
    };

    return value;
}
"#,
    "I0033", // UndeclaredVariable
    8,
  );
}

#[test]
fn match_guard_can_use_pattern_binding() {
  common::assert_ok(
    r#"
function main(): i32 {
    let x: i32 = 3;
    return match (x) {
        value if value > 2 -> value,
        _ -> 0,
    };
}
"#,
  );
}

#[test]
fn match_non_exhaustive_reports_warning_diagnostic() {
  common::assert_diagnostic_at_line(
    r#"
function main(): i32 {
    let x: boolean = true;
    return match (x) {
        true -> 1,
    };
}
"#,
    "A0158", // NonExhaustiveMatch
    4,
  );
}

#[test]
fn match_tuple_pattern_type_mismatch() {
  common::assert_diagnostic_at_line(
    r#"
function main(): i32 {
    let x: i32 = 1;
    return match (x) {
        (a, b) -> 1,
        _ -> 0,
    };
}
"#,
    "A0159", // PatternTypeMismatch
    5,
  );
}
