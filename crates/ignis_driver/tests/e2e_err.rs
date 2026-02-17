mod common;

use insta::assert_snapshot;

fn e2e_error_test(
  name: &str,
  source: &str,
) {
  let diagnostics = common::compile_diagnostics(source).expect("lex/parse failed");
  assert!(!diagnostics.is_empty(), "expected errors");
  assert_snapshot!(name, diagnostics.join("\n"));
}

fn e2e_ownership_error_test(
  name: &str,
  source: &str,
) {
  let diagnostics = common::compile_ownership_diagnostics(source).expect("compilation failed before ownership check");
  assert!(!diagnostics.is_empty(), "expected ownership errors");
  assert_snapshot!(name, diagnostics.join("\n"));
}

fn e2e_warning_test(
  name: &str,
  source: &str,
) {
  let warnings = common::compile_warnings(source).expect("lex/parse failed");
  assert!(!warnings.is_empty(), "expected warnings");
  assert_snapshot!(name, warnings.join("\n"));
}

#[test]
fn e2e_err_index_out_of_bounds_positive() {
  e2e_error_test(
    "err_index_out_of_bounds_positive",
    r#"
function main(): i32 {
    let arr: i32[3] = [1, 2, 3];
    return arr[5];
}
"#,
  );
}

#[test]
fn e2e_err_index_out_of_bounds_negative() {
  e2e_error_test(
    "err_index_out_of_bounds_negative",
    r#"
function main(): i32 {
    let arr: i32[3] = [1, 2, 3];
    return arr[-1];
}
"#,
  );
}

#[test]
fn e2e_err_index_out_of_bounds_exact() {
  e2e_error_test(
    "err_index_out_of_bounds_exact",
    r#"
function main(): i32 {
    let arr: i32[3] = [1, 2, 3];
    return arr[3];
}
"#,
  );
}

#[test]
fn e2e_err_null_non_pointer() {
  e2e_error_test(
    "err_null_non_pointer",
    r#"
function main(): i32 {
    let x: i32 = null;
    return x;
}
"#,
  );
}

#[test]
fn e2e_err_null_uninferred() {
  e2e_error_test(
    "err_null_uninferred",
    r#"
function main(): i32 {
    null;
    return 0;
}
"#,
  );
}

#[test]
fn e2e_err_null_deref() {
  e2e_error_test(
    "err_null_deref",
    r#"
function main(): i32 {
    let x: i32 = *null;
    return x;
}
"#,
  );
}

#[test]
fn e2e_err_pointer_diff_mismatch() {
  e2e_error_test(
    "err_pointer_diff_mismatch",
    r#"
function main(): i32 {
    let mut a: i32 = 0;
    let mut b: f64 = 0.0;
    let pa: *mut i32 = &mut a as *mut i32;
    let pb: *mut f64 = &mut b as *mut f64;
    let d: i64 = pa - pb;
    return d as i32;
}
"#,
  );
}

#[test]
fn e2e_err_null_ptr_diff() {
  e2e_error_test(
    "err_null_ptr_diff",
    r#"
function main(): i32 {
    let d: i64 = null - null;
    return d as i32;
}
"#,
  );
}

#[test]
fn e2e_err_builtin_read_null() {
  e2e_error_test(
    "err_builtin_read_null",
    r#"
function main(): i32 {
    let value: i32 = @read<i32>(null);
    return value;
}
"#,
  );
}

#[test]
fn e2e_err_builtin_write_null() {
  e2e_error_test(
    "err_builtin_write_null",
    r#"
function main(): i32 {
    @write<i32>(null, 10);
    return 0;
}
"#,
  );
}

// ========================================================================
// @builtin(...) error tests
// ========================================================================

#[test]
fn e2e_err_compile_error() {
  e2e_error_test(
    "err_compile_error",
    r#"
function main(): void {
    @compileError("this should not compile");
}
"#,
  );
}

#[test]
fn e2e_err_unknown_builtin() {
  e2e_error_test(
    "err_unknown_builtin",
    r#"
function main(): void {
    @bogus();
}
"#,
  );
}

#[test]
fn e2e_err_config_flag_non_string() {
  e2e_error_test(
    "err_config_flag_non_string",
    r#"
function main(): void {
    let flag: boolean = @configFlag(42);
}
"#,
  );
}

#[test]
fn e2e_err_builtin_arg_count() {
  e2e_error_test(
    "err_builtin_arg_count",
    r#"
function main(): void {
    let flag: boolean = @configFlag();
}
"#,
  );
}

#[test]
fn e2e_err_bitcast_missing_arg() {
  e2e_error_test(
    "err_bitcast_missing_arg",
    r#"
function main(): void {
    let x: i32 = @bitCast<i32>();
}
"#,
  );
}

#[test]
fn e2e_err_pointer_cast_missing_arg() {
  e2e_error_test(
    "err_pointer_cast_missing_arg",
    r#"
function main(): void {
    let x: *mut i32 = @pointerCast<*mut i32>();
}
"#,
  );
}

#[test]
fn e2e_err_integer_from_pointer_missing_arg() {
  e2e_error_test(
    "err_integer_from_pointer_missing_arg",
    r#"
function main(): void {
    let x: u64 = @integerFromPointer();
}
"#,
  );
}

#[test]
fn e2e_err_pointer_from_integer_missing_arg() {
  e2e_error_test(
    "err_pointer_from_integer_missing_arg",
    r#"
function main(): void {
    let x: *mut i32 = @pointerFromInteger<*mut i32>();
}
"#,
  );
}

// =========================================================================
// Attribute Error Tests
// =========================================================================

#[test]
fn e2e_err_attr_unknown() {
  e2e_error_test(
    "err_attr_unknown",
    r#"
@bogusAttr
record Foo {
    x: i32;
}

function main(): void {
    return;
}
"#,
  );
}

#[test]
fn e2e_err_attr_packed_on_function() {
  e2e_error_test(
    "err_attr_packed_on_function",
    r#"
@packed
function foo(): void {
    return;
}

function main(): void {
    return;
}
"#,
  );
}

#[test]
fn e2e_err_attr_alignment_not_power_of_two() {
  e2e_error_test(
    "err_attr_alignment_not_power_of_two",
    r#"
@aligned(7)
record BadAlign {
    x: i32;
}

function main(): void {
    return;
}
"#,
  );
}

// =========================================================================
// Lint Warning Tests
// =========================================================================

#[test]
fn e2e_warn_unused_variable() {
  e2e_warning_test(
    "warn_unused_variable",
    r#"
function main(): i32 {
    let x: i32 = 5;
    return 0;
}
"#,
  );
}

#[test]
fn e2e_warn_deprecated_call() {
  e2e_warning_test(
    "warn_deprecated_call",
    r#"
@deprecated("use bar instead")
function foo(): i32 {
    return 1;
}

function main(): i32 {
    return foo();
}
"#,
  );
}

// =========================================================================
// Never-type Control Flow Warning Tests
// =========================================================================

#[test]
fn e2e_warn_unreachable_after_panic() {
  e2e_warning_test(
    "warn_unreachable_after_panic",
    r#"
function main(): i32 {
    @panic("bail");
    return 0;
}
"#,
  );
}

// =========================================================================
// Lint Error Tests
// =========================================================================

#[test]
fn e2e_err_unknown_lint() {
  e2e_error_test(
    "err_unknown_lint",
    r#"
@allow(bogusLint)
function main(): void {
    return;
}
"#,
  );
}

#[test]
fn e2e_err_deny_unused_variable() {
  e2e_error_test(
    "err_deny_unused_variable",
    r#"
@deny(unused_variable)
function main(): i32 {
    let x: i32 = 5;
    return 0;
}
"#,
  );
}

#[test]
fn e2e_err_extension_method_on_literal() {
  e2e_error_test(
    "err_extension_method_on_literal",
    r#"
@extension(i32)
function doubled(value: i32): i32 {
    return value * 2;
}

function main(): i32 {
    return 21.doubled();
}
"#,
  );
}

#[test]
fn e2e_err_extension_method_on_temporary() {
  e2e_error_test(
    "err_extension_method_on_temporary",
    r#"
@extension(i32)
function doubled(value: i32): i32 {
    return value * 2;
}

function main(): i32 {
    let x: i32 = 5;
    return (x + 1).doubled();
}
"#,
  );
}

#[test]
fn e2e_err_extension_mut_on_immutable() {
  e2e_error_test(
    "err_extension_mut_on_immutable",
    r#"
@extension(i32, mut)
function increment(value: i32): i32 {
    return value + 1;
}

function main(): i32 {
    let x: i32 = 5;
    return x.increment();
}
"#,
  );
}

// =========================================================================
// Ownership: Drop Safety Tests
// =========================================================================

#[test]
fn e2e_err_use_after_drop() {
  e2e_ownership_error_test(
    "err_use_after_drop",
    r#"
@implements(Drop)
record Resource {
    public tag: i32;

    drop(&mut self): void {
        return;
    }
}

function main(): i32 {
    let mut r: Resource = Resource { tag: 42 };
    r.drop();
    return r.tag;
}
"#,
  );
}

#[test]
fn e2e_err_double_drop() {
  e2e_ownership_error_test(
    "err_double_drop",
    r#"
@implements(Drop)
record Resource {
    public tag: i32;

    drop(&mut self): void {
        return;
    }
}

function main(): i32 {
    let mut r: Resource = Resource { tag: 42 };
    r.drop();
    r.drop();
    return 0;
}
"#,
  );
}

#[test]
fn e2e_err_move_after_drop() {
  e2e_ownership_error_test(
    "err_move_after_drop",
    r#"
@implements(Drop)
record Resource {
    public tag: i32;

    drop(&mut self): void {
        return;
    }
}

function consume(r: Resource): i32 {
    return r.tag;
}

function main(): i32 {
    let mut r: Resource = Resource { tag: 42 };
    r.drop();
    return consume(r);
}
"#,
  );
}

#[test]
fn e2e_err_conditional_drop_then_use() {
  e2e_ownership_error_test(
    "err_conditional_drop_then_use",
    r#"
@implements(Drop)
record Resource {
    public tag: i32;

    drop(&mut self): void {
        return;
    }
}

function main(): i32 {
    let mut r: Resource = Resource { tag: 42 };
    let cond: boolean = true;
    if (cond) {
        r.drop();
    }
    return r.tag;
}
"#,
  );
}

// =========================================================================
// Ownership + Loop Interaction Tests
// =========================================================================

#[test]
fn e2e_err_drop_in_while_use_after() {
  e2e_ownership_error_test(
    "err_drop_in_while_use_after",
    r#"
@implements(Drop)
record Resource {
    public tag: i32;

    drop(&mut self): void {
        return;
    }
}

function main(): i32 {
    let mut r: Resource = Resource { tag: 1 };
    let mut i: i32 = 0;
    while (i < 1) {
        r.drop();
        i = i + 1;
    }
    return r.tag;
}
"#,
  );
}

#[test]
fn e2e_err_double_drop_in_while() {
  e2e_ownership_error_test(
    "err_double_drop_in_while",
    r#"
@implements(Drop)
record Resource {
    public tag: i32;

    drop(&mut self): void {
        return;
    }
}

function main(): i32 {
    let mut r: Resource = Resource { tag: 1 };
    r.drop();
    let mut i: i32 = 0;
    while (i < 1) {
        r.drop();
        i = i + 1;
    }
    return 0;
}
"#,
  );
}

// =========================================================================
// Reachability: break makes subsequent code unreachable
// =========================================================================

#[test]
fn e2e_ok_break_skips_unreachable_code() {
  // The code after `break` in the while body is unreachable.
  // The ownership checker should NOT report UseAfterDrop for it.
  let diagnostics = common::compile_ownership_diagnostics(
    r#"
@implements(Drop)
record Resource {
    public tag: i32;

    drop(&mut self): void {
        return;
    }
}

function main(): i32 {
    let mut r: Resource = Resource { tag: 1 };
    let mut i: i32 = 0;
    while (i < 10) {
        r.drop();
        break;
        i = r.tag;
    }
    return 0;
}
"#,
  )
  .expect("compilation failed before ownership check");

  // Should be empty: `i = r.tag` is after `break`, so unreachable
  assert!(
    diagnostics.is_empty(),
    "Expected no ownership errors when break makes code unreachable, got: {:?}",
    diagnostics
  );
}

// =========================================================================
// Cross-iteration: drop in body visible on next iteration
// =========================================================================

#[test]
fn e2e_err_cross_iteration_drop_in_body() {
  // r.drop() in the body means on the second iteration, r is already dropped.
  // The two-pass analysis should catch this.
  e2e_ownership_error_test(
    "err_cross_iteration_drop",
    r#"
@implements(Drop)
record Resource {
    public tag: i32;

    drop(&mut self): void {
        return;
    }
}

function main(): i32 {
    let mut r: Resource = Resource { tag: 1 };
    let mut i: i32 = 0;
    while (i < 2) {
        r.drop();
        i = i + 1;
    }
    return 0;
}
"#,
  );
}

// =========================================================================
// UseAfterMove: contextual notes
// =========================================================================

#[test]
fn e2e_err_use_after_move_drop_record() {
  e2e_ownership_error_test(
    "err_use_after_move_drop_record",
    r#"
@implements(Drop)
record Resource {
    public tag: i32;

    drop(&mut self): void {
        return;
    }
}

function consume(r: Resource): i32 {
    return r.tag;
}

function main(): i32 {
    let r: Resource = Resource { tag: 42 };
    let x: i32 = consume(r);
    return consume(r);
}
"#,
  );
}

#[test]
fn e2e_err_use_after_move_noncopy_field() {
  e2e_ownership_error_test(
    "err_use_after_move_noncopy_field",
    r#"
@implements(Drop)
record Named {
    public id: i32;

    drop(&mut self): void {
        return;
    }
}

function consume(n: Named): i32 {
    return 0;
}

function main(): i32 {
    let n: Named = Named { id: 1 };
    let x: i32 = consume(n);
    return consume(n);
}
"#,
  );
}

#[test]
fn e2e_err_use_after_move_assign() {
  e2e_ownership_error_test(
    "err_use_after_move_assign",
    r#"
@implements(Drop)
record Handle {
    public id: i32;

    drop(&mut self): void {
        return;
    }
}

function main(): i32 {
    let h: Handle = Handle { id: 1 };
    let h2: Handle = h;
    return h.id;
}
"#,
  );
}

#[test]
fn e2e_err_enum_drop_use_after_move() {
  e2e_ownership_error_test(
    "enum_drop_use_after_move",
    r#"
@implements(Drop)
enum Resource {
    Active(i32),
    Idle

    drop(&mut self): void {
        return;
    }
}

function consume(r: Resource): i32 {
    return 0;
}

function main(): i32 {
    let mut r: Resource = Resource::Active(10);
    consume(r);
    r.drop();
    return 0;
}
"#,
  );
}

// ── Type Inference Errors ──────────────────────────────────

#[test]
fn e2e_err_cannot_infer_type() {
  e2e_error_test(
    "err_cannot_infer_type",
    r#"
function main(): void {
    let x;
    return;
}
"#,
  );
}

#[test]
fn e2e_err_conflicting_inferred_types() {
  e2e_error_test(
    "err_conflicting_inferred_types",
    r#"
function main(): void {
    let mut x;
    x = 42;
    x = true;
    return;
}
"#,
  );
}

#[test]
fn e2e_err_closure_escapes_with_ref_capture() {
  e2e_error_test(
    "err_closure_escapes_with_ref_capture",
    r#"
function makeCounter(): () -> i32 {
    let mut count: i32 = 0;
    let inc = (): i32 -> {
        count = count + 1;
        return count;
    };
    return inc;
}

function main(): i32 {
    let counter = makeCounter();
    return counter();
}
"#,
  );
}

#[test]
fn e2e_err_closure_borrow_conflict_mutref_capture() {
  e2e_ownership_error_test(
    "err_closure_borrow_conflict_mutref",
    r#"
function main(): i32 {
    let mut x: i32 = 0;
    let f = (): void -> { x = 1; };
    x = 2;
    return 0;
}
"#,
  );
}

#[test]
fn e2e_err_closure_escapes_without_noescape() {
  e2e_error_test(
    "err_closure_escapes_without_noescape",
    r#"
function apply(f: (i32) -> i32, x: i32): i32 {
    return f(x);
}

function main(): i32 {
    let mut val: i32 = 10;
    let inc = (n: i32): i32 -> {
        val = val + 1;
        return n + val;
    };
    return apply(inc, 5);
}
"#,
  );
}

// ===========================================================================
// Pipe Operator Error Tests
// ===========================================================================

#[test]
fn e2e_err_pipe_invalid_rhs_literal() {
  e2e_error_test(
    "err_pipe_invalid_rhs_literal",
    r#"
function main(): i32 {
    return 10 |> 42;
}
"#,
  );
}

#[test]
fn e2e_err_pipe_invalid_rhs_binary() {
  e2e_error_test(
    "err_pipe_invalid_rhs_binary",
    r#"
function main(): i32 {
    return 10 |> 1 + 2;
}
"#,
  );
}

#[test]
fn e2e_err_pipe_type_mismatch() {
  e2e_error_test(
    "err_pipe_type_mismatch",
    r#"
function doubleInt(x: i32): i32 {
    return x * 2;
}

function main(): i32 {
    return "hello" |> doubleInt;
}
"#,
  );
}

#[test]
fn e2e_err_pipe_wrong_arity() {
  e2e_error_test(
    "err_pipe_wrong_arity",
    r#"
function add(a: i32, b: i32): i32 {
    return a + b;
}

function main(): i32 {
    return 10 |> add;
}
"#,
  );
}

#[test]
fn e2e_err_pipe_method_bare_arity() {
  e2e_error_test(
    "err_pipe_method_bare_arity",
    r#"
record Box {
    public value: i32;

    getValue(&self): i32 {
        return self.value;
    }
}

function main(): i32 {
    let b: Box = Box { value: 10 };
    return 5 |> b.getValue;
}
"#,
  );
}

// =========================================================================
// PIPE PLACEHOLDER ERROR TESTS
// =========================================================================

#[test]
fn e2e_err_pipe_multiple_placeholders() {
  e2e_error_test(
    "err_pipe_multiple_placeholders",
    r#"
function f(a: i32, b: i32): i32 {
    return a + b;
}

function main(): i32 {
    return 1 |> f(_, _);
}
"#,
  );
}

#[test]
fn e2e_err_pipe_placeholder_outside_pipe() {
  e2e_error_test(
    "err_pipe_placeholder_outside_pipe",
    r#"
function main(): i32 {
    let y: i32 = _;
    return y;
}
"#,
  );
}

#[test]
fn e2e_err_pipe_placeholder_bare_rhs() {
  e2e_error_test(
    "err_pipe_placeholder_bare_rhs",
    r#"
function main(): i32 {
    return 42 |> _;
}
"#,
  );
}

#[test]
fn e2e_err_pipe_placeholder_arity() {
  e2e_error_test(
    "err_pipe_placeholder_arity",
    r#"
function f(a: i32, b: i32): i32 {
    return a + b;
}

function main(): i32 {
    return 1 |> f(1, _, 3);
}
"#,
  );
}

#[test]
fn e2e_err_pipe_method_call_arity() {
  e2e_error_test(
    "err_pipe_method_call_arity",
    r#"
record Box {
    public value: i32;

    add(&self, x: i32): i32 {
        return self.value + x;
    }
}

function main(): i32 {
    let b: Box = Box { value: 10 };
    return 5 |> b.add(1);
}
"#,
  );
}

// =========================================================================
// Defer Statement Error Tests
// =========================================================================

#[test]
fn e2e_err_defer_non_void_expression() {
  e2e_error_test(
    "err_defer_non_void_expression",
    r#"
function main(): i32 {
    defer 42;
    return 0;
}
"#,
  );
}

#[test]
fn e2e_err_defer_try_operator() {
  e2e_error_test(
    "err_defer_try_operator",
    r#"
@lang(try)
enum Result<T, E> {
    Ok(T),
    Err(E),
}

function fallible(): Result<i32, i32> {
    return Result::Ok(1);
}

function main(): Result<i32, i32> {
    defer fallible()!;
    return Result::Ok(0);
}
"#,
  );
}

// --- Pipe operator error tests ---

#[test]
fn e2e_pipe_multiple_deep_placeholders() {
  e2e_error_test(
    "pipe_multiple_deep_placeholders",
    r#"
function f(a: i32, b: i32): i32 {
    return a + b;
}

function g(x: i32): i32 {
    return x;
}

function main(): i32 {
    return 5 |> f(g(_), g(_));
}
"#,
  );
}

#[test]
fn e2e_pipe_record_init_no_placeholder() {
  e2e_error_test(
    "pipe_record_init_no_placeholder",
    r#"
record Wrapper {
    public value: i32;
}

function main(): i32 {
    let w = 42 |> Wrapper { value: 1 };
    return 0;
}
"#,
  );
}

#[test]
fn e2e_pipe_vector_no_placeholder() {
  e2e_error_test(
    "pipe_vector_no_placeholder",
    r#"
function main(): i32 {
    let v: i32[3] = 10 |> [1, 2, 3];
    return 0;
}
"#,
  );
}

#[test]
fn e2e_pipe_builtin_no_placeholder() {
  e2e_error_test(
    "pipe_builtin_no_placeholder",
    r#"
function main(): i32 {
    let mut x: i32 = 42;
    let ptr: *mut i32 = (&mut x) as *mut i32;
    return ptr |> @read<i32>();
}
"#,
  );
}
