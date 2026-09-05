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

#[test]
fn unknown_builtin_at_correct_line() {
  common::assert_diagnostic_at_line(
    r#"
function main(): void {
    @bogus();
}"#,
    "A0110",
    3,
  );
}

#[test]
fn unknown_param_attribute_at_correct_line() {
  common::assert_diagnostic_at_line(
    r#"
function foo(@bogus x: i32): void {
    return;
}

function main(): void {
    return;
}"#,
    "A0117",
    2,
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

    get(&self): i32 {
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
function foo(x: str): void { return; }

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
function foo(x: str): void { return; }

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

    get(&self): i32 {
        return self.value;
    }

    get(&self, label: str): i32 {
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
fn cannot_move_out_of_borrowed_result_unwrap() {
  common::assert_diagnostic_at_line(
    r#"
@implements(Drop)
record Box {
    value: i32;

    drop(&mut self): void {
        return;
    }
}

@lang(try)
enum Result<T, E> {
    OK(T),
    ERROR(E),

    unwrap(&self): T {
        return match (self) {
            Result::OK(value) -> value,
            Result::ERROR(_) -> @panic("boom"),
        };
    }
}

function main(): i32 {
    let result: Result<Box, i32> = Result::OK(Box { value: 42 });
    let value: Box = result.unwrap();
    return value.value;
}
"#,
    "A0186", // CannotMoveOutOfBorrowedValue
    26,
  );
}

#[test]
fn cannot_move_field_out_of_binding_destructured_from_reference() {
  common::assert_diagnostic_at_line(
    r#"
@implements(Drop)
record Payload {
    public value: i32;

    drop(&mut self): void {
        return;
    }
}

enum Source {
    SOME(Payload),
    NONE,
}

record ExportItem {
    public source: Source;
}

enum Node {
    Export(ExportItem),
    Nothing,
}

function readSource(item: &Node): i32 {
    return match (item) {
        Node::Export(exportItem) -> match (exportItem.source) {
            Source::SOME(path) -> path.value,
            Source::NONE -> 0,
        },
        Node::Nothing -> 0,
    };
}

function main(): i32 {
    let node: Node = Node::Export(ExportItem { source: Source::SOME(Payload { value: 42 }) });
    return readSource(&node);
}
"#,
    "A0186", // CannotMoveOutOfBorrowedValue
    27,
  );
}

#[test]
fn cannot_move_borrowed_binding_as_arm_result() {
  common::assert_diagnostic_at_line(
    r#"
@implements(Drop)
record Payload {
    public value: i32;

    drop(&mut self): void {
        return;
    }
}

enum Source {
    SOME(Payload),
    NONE,
}

function takeFromBorrow(src: &Source): i32 {
    let taken: Payload = match (src) {
        Source::SOME(payload) -> payload,
        Source::NONE -> @panic("none"),
    };
    return taken.value;
}

function main(): i32 {
    let source: Source = Source::SOME(Payload { value: 7 });
    return takeFromBorrow(&source);
}
"#,
    "A0186", // CannotMoveOutOfBorrowedValue
    18,
  );
}

#[test]
fn cannot_move_borrowed_binding_into_let() {
  common::assert_diagnostic_at_line(
    r#"
@implements(Drop)
record Payload {
    public value: i32;

    drop(&mut self): void {
        return;
    }
}

enum Source {
    SOME(Payload),
    NONE,
}

function takeFromBorrow(src: &Source): i32 {
    match (src) {
        Source::SOME(payload) -> {
            let owned: Payload = payload;
        },
        Source::NONE -> {},
    };
    return 0;
}

function main(): i32 {
    let source: Source = Source::SOME(Payload { value: 7 });
    return takeFromBorrow(&source);
}
"#,
    "A0186", // CannotMoveOutOfBorrowedValue
    19,
  );
}

#[test]
fn cannot_return_borrowed_binding_by_value() {
  common::assert_diagnostic_at_line(
    r#"
@implements(Drop)
record Payload {
    public value: i32;

    drop(&mut self): void {
        return;
    }
}

enum Source {
    SOME(Payload),
    NONE,
}

function takeFromBorrow(src: &Source): Payload {
    match (src) {
        Source::SOME(payload) -> {
            return payload;
        },
        Source::NONE -> {},
    };
    return Payload { value: 0 };
}

function main(): i32 {
    let source: Source = Source::SOME(Payload { value: 7 });
    let taken: Payload = takeFromBorrow(&source);
    return taken.value;
}
"#,
    "A0186", // CannotMoveOutOfBorrowedValue
    19,
  );
}

#[test]
fn cannot_pass_borrowed_binding_by_value() {
  common::assert_diagnostic_at_line(
    r#"
@implements(Drop)
record Payload {
    public value: i32;

    drop(&mut self): void {
        return;
    }
}

enum Source {
    SOME(Payload),
    NONE,
}

function consume(p: Payload): i32 {
    return p.value;
}

function takeFromBorrow(src: &Source): i32 {
    return match (src) {
        Source::SOME(payload) -> consume(payload),
        Source::NONE -> 0,
    };
}

function main(): i32 {
    let source: Source = Source::SOME(Payload { value: 7 });
    return takeFromBorrow(&source);
}
"#,
    "A0186", // CannotMoveOutOfBorrowedValue
    22,
  );
}

#[test]
fn cannot_assign_borrowed_binding_by_value() {
  common::assert_diagnostic_at_line(
    r#"
@implements(Drop)
record Payload {
    public value: i32;

    drop(&mut self): void {
        return;
    }
}

enum Source {
    SOME(Payload),
    NONE,
}

function takeFromBorrow(src: &Source): i32 {
    let mut owned: Payload = Payload { value: 0 };
    match (src) {
        Source::SOME(payload) -> {
            owned = payload;
        },
        Source::NONE -> {},
    };
    return owned.value;
}

function main(): i32 {
    let source: Source = Source::SOME(Payload { value: 7 });
    return takeFromBorrow(&source);
}
"#,
    "A0186", // CannotMoveOutOfBorrowedValue
    20,
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
fn match_arm_inference_propagates_expected_type_to_generic_enum_variants() {
  common::assert_ok(
    r#"
enum Option<T> {
    Some(T),
    None,
}

function fromBool(flag: boolean): Option<i32> {
    return match (flag) {
        true -> Option::Some(1),
        false -> Option::None,
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

/// Two namespaces may each declare a type with the same short name. Naming only the short
/// name in a mismatch produces `expected 'Point', found 'Point'`, which states that there is
/// a problem without saying which two types are involved.
#[test]
fn type_mismatch_between_same_named_types_names_both_namespaces() {
  let result = common::analyze(
    r#"
export namespace First {
    record Point {
        public x: i32;
    }

    function make(): Point {
        return Point { x: 1 };
    }
}

export namespace Second {
    record Point {
        public x: i32;
    }
}

function main(): i32 {
    let point: Second::Point = First::make();

    return point.x;
}
"#,
  );

  let mismatch = result
    .output
    .diagnostics
    .iter()
    .find(|diagnostic| diagnostic.error_code == "A0045")
    .expect("expected a type mismatch between the two same-named records");

  assert!(
    mismatch.message.contains("Second::Point") && mismatch.message.contains("First::Point"),
    "expected both namespaces to be named, got: {}",
    mismatch.message
  );
}

#[test]
fn constant_taking_a_type_name_still_reports_a_missing_record() {
  // Line 5: Point { x: 34 } — the only `Point` in scope is a constant, and no record of that
  // name exists anywhere, so the literal has to be reported rather than silently dropped.
  common::assert_diagnostic_at_line(
    r#"
export namespace Holder {
    const Point: i32 = 99;

    function make(): i32 {
        return Point { x: 34 }.x;
    }
}

function main(): i32 {
    return Holder::make();
}"#,
    "A0053", // NotARecord
    6,
  );
}

#[test]
fn diverging_initializer_is_accepted_for_a_local_binding() {
  // The binding is unreachable, so `never` cannot produce a wrongly-typed value. Only
  // `return` and `defer` used to exempt it, which made this a mismatch against code that
  // cannot run.
  let result = common::analyze(
    r#"
function bail(flag: boolean): u64 {
    if (flag) {
        let value: u64 = @panic("bail");
        return value;
    }

    return 7;
}
"#,
  );

  let mismatch = result
    .output
    .diagnostics
    .iter()
    .find(|diagnostic| diagnostic.error_code == "A0045");

  assert!(
    mismatch.is_none(),
    "a diverging initializer must not be reported as a type mismatch, got: {:?}",
    mismatch.map(|diagnostic| &diagnostic.message)
  );
}

#[test]
fn diverging_initializer_is_still_rejected_for_a_constant() {
  // A constant must produce a compile-time value, so divergence is a real error here
  // rather than unreachable code. Without the distinction the program reaches LIR
  // verification and fails with an internal invariant error instead of a diagnostic.
  let result = common::analyze(
    r#"
const LIMIT: u64 = @panic("no const");

function main(): i32 {
    return LIMIT as i32;
}
"#,
  );

  assert!(
    result
      .output
      .diagnostics
      .iter()
      .any(|diagnostic| diagnostic.error_code == "A0045"),
    "a diverging constant initializer must still be reported"
  );
}

#[test]
fn diverging_assignment_still_resolves_an_inference_variable() {
  // The divergence exemption sits after inference-variable unification on purpose: an
  // uninitialized `let` whose only assignment diverges still needs `never` unified in, or
  // the variable is left unresolved and reported as uninferable.
  let result = common::analyze(
    r#"
function main(): i32 {
    let mut value;
    value = @panic("bail");
    return 0;
}
"#,
  );

  let uninferable = result
    .output
    .diagnostics
    .iter()
    .find(|diagnostic| diagnostic.error_code == "A0077");

  assert!(
    uninferable.is_none(),
    "a diverging assignment must still resolve the inference variable, got: {:?}",
    uninferable.map(|diagnostic| &diagnostic.message)
  );
}

#[test]
fn cannot_move_borrowed_binding_from_let_else() {
  common::assert_diagnostic_at_line(
    r#"
@implements(Drop)
record Payload {
    public value: i32;

    drop(&mut self): void {
        return;
    }
}

enum Source {
    SOME(Payload),
    NONE,
}

function takeFromBorrow(src: &Source): i32 {
    let Source::SOME(payload) = src else {
        return 0;
    };

    let owned: Payload = payload;
    return owned.value;
}

function main(): i32 {
    let source: Source = Source::SOME(Payload { value: 7 });
    return takeFromBorrow(&source);
}
"#,
    "A0186", // CannotMoveOutOfBorrowedValue
    21,
  );
}

#[test]
fn cannot_return_borrowed_binding_from_let_else() {
  common::assert_diagnostic_at_line(
    r#"
@implements(Drop)
record Payload {
    public value: i32;

    drop(&mut self): void {
        return;
    }
}

enum Source {
    SOME(Payload),
    NONE,
}

function takeFromBorrow(src: &Source): Payload {
    let Source::SOME(payload) = src else {
        return Payload { value: 0 };
    };

    return payload;
}

function main(): i32 {
    let source: Source = Source::SOME(Payload { value: 7 });
    let taken: Payload = takeFromBorrow(&source);
    return taken.value;
}
"#,
    // The span is the destructure itself, not the `return`: the binding is refused as a
    // whole at the point it would leave borrowed storage.
    "A0186", // CannotMoveOutOfBorrowedValue
    21,
  );
}

#[test]
fn cannot_move_borrowed_binding_into_record_field() {
  common::assert_diagnostic_at_line(
    r#"
@implements(Drop)
record Payload {
    public value: i32;

    drop(&mut self): void {
        return;
    }
}

record Holder {
    public held: Payload;
}

enum Source {
    SOME(Payload),
    NONE,
}

function takeFromBorrow(src: &Source): i32 {
    match (src) {
        Source::SOME(payload) -> {
            let holder: Holder = Holder { held: payload };
            return holder.held.value;
        },
        Source::NONE -> {},
    };

    return 0;
}

function main(): i32 {
    let source: Source = Source::SOME(Payload { value: 7 });
    return takeFromBorrow(&source);
}
"#,
    "A0186", // CannotMoveOutOfBorrowedValue
    23,
  );
}

#[test]
fn cannot_move_borrowed_binding_into_enum_payload() {
  common::assert_diagnostic_at_line(
    r#"
@implements(Drop)
record Payload {
    public value: i32;

    drop(&mut self): void {
        return;
    }
}

enum Source {
    SOME(Payload),
    NONE,
}

function takeFromBorrow(src: &Source): i32 {
    match (src) {
        Source::SOME(payload) -> {
            let rewrapped: Source = Source::SOME(payload);
            return 1;
        },
        Source::NONE -> {},
    };

    return 0;
}

function main(): i32 {
    let source: Source = Source::SOME(Payload { value: 7 });
    return takeFromBorrow(&source);
}
"#,
    "A0186", // CannotMoveOutOfBorrowedValue
    19,
  );
}

#[test]
fn cannot_match_owned_payload_out_of_a_borrowed_field() {
  // Line 21: match over `node.slot`, where `node` is only borrowed. The owner's drop
  // cannot be suppressed here, so the payload may not be moved into the arm binding.
  common::assert_diagnostic_at_line(
    r#"
@implements(Drop)
record Payload {
    public value: i32;

    drop(&mut self): void {
        return;
    }
}

enum Slot {
    SOME(Payload),
    NONE,
}

record Node {
    public slot: Slot;
}

function readSlot(node: &Node): i32 {
    return match (node.slot) {
        Slot::SOME(payload) -> payload.value,
        Slot::NONE -> 0,
    };
}

function main(): i32 {
    let node: Node = Node { slot: Slot::SOME(Payload { value: 7 }) };
    return readSlot(&node);
}
"#,
    "A0186", // CannotMoveOutOfBorrowedValue
    21,
  );
}

#[test]
fn heterogeneous_vector_literal_at_correct_line() {
  // Line 3: [1, true, 3] — element 1 diverges from the first element's type
  common::assert_diagnostic_at_line(
    r#"
function main(): void {
    let a: i32[3] = [1, true, 3];
    return;
}"#,
    "I0031", // TypeMismatch
    3,
  );
}

#[test]
fn for_of_over_drop_bearing_record_requires_ref_at_correct_line() {
  // Line 13: unannotated binding over Wrapper[2] where Wrapper transitively contains a Drop type
  common::assert_diagnostic_at_line(
    r#"
@implements(Drop)
record Owned {
    public id: i32;
    drop(&mut self): void { return; }
}

record Wrapper {
    public inner: Owned;
}

function consume(items: Wrapper[2]): void {
    for (let item of items) {
        return;
    }
}

function main(): i32 {
    return 0;
}"#,
    "A0070", // ForOfRequiresCopyOrRef
    13,
  );
}

#[test]
fn for_of_over_drop_bearing_record_by_ref_is_clean() {
  common::assert_ok(
    r#"
@implements(Drop)
record Owned {
    public id: i32;
    drop(&mut self): void { return; }
}

record Wrapper {
    public inner: Owned;
}

function consume(items: Wrapper[2]): void {
    for (let item: &Wrapper of items) {
        return;
    }
}

function main(): i32 {
    return 0;
}"#,
  );
}

#[test]
fn bare_builtin_name_outside_callee_position_is_undeclared() {
  // Line 3: typeOf used as a value, not as a callee.
  common::assert_diagnostic_at_line(
    r#"
function main(): i32 {
    return typeOf as i32;
}"#,
    "I0033", // UndeclaredVariable
    3,
  );
}

#[test]
fn move_in_a_let_else_else_block_does_not_hide_a_later_double_move() {
  // Line 27: the second `consume(other)`, a genuine use after move.
  common::assert_diagnostic_at_line(
    r#"
@implements(Drop)
record Owned {
    public id: i32;
    drop(&mut self): void { return; }
}

enum Maybe {
    Some(i32),
    None,
}

function consume(value: Owned): i32 {
    return 1;
}

function run(input: Maybe): i32 {
    let owned: Owned = Owned { id: 1 };
    let other: Owned = Owned { id: 2 };

    let Maybe::Some(v) = input else {
        consume(owned);
        return -1;
    };

    consume(other);
    consume(other);
    return v;
}"#,
    "O0001", // UseAfterMove
    27,
  );
}

#[test]
fn borrow_checking_continues_after_a_let_else() {
  // Line 15: `owned = 5` while `r` still borrows it.
  common::assert_diagnostic_at_line(
    r#"
enum Maybe {
    Some(i32),
    None,
}

function run(input: Maybe): i32 {
    let mut owned: i32 = 1;

    let Maybe::Some(v) = input else {
        return -1;
    };

    let r: &mut i32 = &mut owned;
    owned = 5;
    return v + *r;
}"#,
    "A0047", // MutatedWhileBorrowed
    15,
  );
}
