mod common;

use insta::assert_snapshot;
use ignis_config::TargetBackend;

fn e2e_test(
  name: &str,
  source: &str,
) {
  let result =
    common::compile_and_run(source).unwrap_or_else(|error| panic!("Compilation of '{}' failed: {}", name, error));

  assert!(
    !result.leaked,
    "LeakSanitizer detected a memory leak in '{}':\n{}",
    name, result.leak_report,
  );

  assert_snapshot!(name, common::format_e2e_result(&result));
}

fn e2e_no_warnings(
  name: &str,
  source: &str,
) {
  let warnings = common::compile_warnings(source).unwrap_or_else(|_| panic!("Compilation of '{}' failed", name));
  assert!(warnings.is_empty(), "expected no warnings for '{}', got: {:?}", name, warnings);
}

#[test]
fn e2e_build_std_c_backend_succeeds() {
  let attempt = common::build_std_with_target(TargetBackend::C).expect("temporary std build setup should succeed");

  assert!(attempt.result.is_ok(), "expected build_std to succeed for the C backend");
  assert!(
    attempt.output_dir.join("std/lib/libignis_std.a").exists(),
    "expected build_std to emit the std archive"
  );
}

#[test]
fn e2e_check_std_c_backend_succeeds() {
  let attempt = common::check_std_with_target(TargetBackend::C).expect("temporary std check setup should succeed");

  assert!(attempt.result.is_ok(), "expected check_std to succeed for the C backend");
  assert!(
    attempt.output_dir.join("ignis_std.h").exists(),
    "expected check_std to emit the umbrella std header"
  );
}

#[test]
fn e2e_staged_driver_single_file() {
  let result = common::compile_project_and_run(
    r#"
function add(a: i32, b: i32): i32 {
    return a + b;
}

function main(): i32 {
    return add(19, 23);
}
"#,
  )
  .expect("staged driver build should succeed");

  assert!(
    !result.leaked,
    "LeakSanitizer detected a memory leak in 'staged_driver_single_file':\n{}",
    result.leak_report,
  );

  assert_snapshot!("staged_driver_single_file", common::format_e2e_result(&result));
}

// ========================================================================
// @panic tests
// ========================================================================

#[test]
fn e2e_panic_message() {
  let result = common::compile_and_run(
    r#"
function main(): i32 {
    @panic("Test");
    return 0;
}
"#,
  )
  .expect("Compilation of 'panic_message' failed");

  assert_eq!(result.exit_code, 101);
  assert!(
    result.stderr.contains("panic: Test"),
    "Expected stderr to contain 'panic: Test', got: {}",
    result.stderr
  );
}

// ========================================================================
// @builtin(...) syntax tests
// ========================================================================

const CONFIG_FLAG_BUILD_DEBUG_SOURCE: &str = r#"
function main(): i32 {
    let debug: boolean = @configFlag(@debug());
    if (debug) {
        return 42;
    }
    return 0;
}
"#;

/// `@debug()` reflects the project's build profile, not the host compiler's
/// own build profile. This case pins `CompilationContext::debug = true`
/// directly, as if resolved from `ignis.toml [build] debug = true` or
/// `--debug`.
#[test]
fn e2e_config_flag_build_debug_enabled() {
  let name = "config_flag_build_debug_enabled";
  let ctx = ignis_type::compilation_context::CompilationContext {
    debug: true,
    ..ignis_type::compilation_context::CompilationContext::default()
  };

  let result = common::compile_and_run_with_ctx(CONFIG_FLAG_BUILD_DEBUG_SOURCE, ctx)
    .unwrap_or_else(|error| panic!("Compilation of '{}' failed: {}", name, error));

  assert!(
    !result.leaked,
    "LeakSanitizer detected a memory leak in '{}':\n{}",
    name, result.leak_report,
  );

  assert_snapshot!(name, common::format_e2e_result(&result));
}

// ========================================================================
// Never-type control flow tests
// ========================================================================

#[test]
fn e2e_never_no_missing_return() {
  e2e_no_warnings(
    "never_no_missing_return",
    r#"
function failHard(): i32 {
    @panic("fatal error");
}

function main(): i32 {
    return 0;
}
"#,
  );
}

#[test]
fn e2e_lint_allow_unused_variable() {
  e2e_no_warnings(
    "lint_allow_unused_variable",
    r#"
@allow(unused_variable)
function main(): i32 {
    let x: i32 = 5;
    return 0;
}
"#,
  );
}

#[test]
fn e2e_lint_underscore_suppresses_unused() {
  e2e_no_warnings(
    "lint_underscore_suppresses_unused",
    r#"
function main(): i32 {
    let _unused: i32 = 5;
    return 0;
}
"#,
  );
}

// =========================================================================
// Enum Drop
// =========================================================================

#[test]
fn e2e_enum_drop_custom() {
  e2e_test(
    "enum_drop_custom",
    r#"
@implements(Drop)
enum Resource {
    Active(i32),
    Idle

    drop(&mut self): void {
        return;
    }
}

function main(): i32 {
    let r: Resource = Resource::Active(10);
    return 0;
}
"#,
  );
}

#[test]
fn e2e_enum_drop_manual() {
  e2e_test(
    "enum_drop_manual",
    r#"
@implements(Drop)
enum Resource {
    Active(i32),
    Idle

    drop(&mut self): void {
        return;
    }
}

function main(): i32 {
    let mut r: Resource = Resource::Active(42);
    r.drop();
    return 0;
}
"#,
  );
}

// ---------------------------------------------------------------------------
// Weak<T> tests
// ---------------------------------------------------------------------------

#[test]
fn e2e_match_non_exhaustive_runtime_panic() {
  let result = common::compile_and_run(
    r#"
function main(): i32 {
    let x: i32 = 3;

    return match (x) {
        1 -> 10,
        2 -> 20,
    };
}
"#,
  )
  .expect("Compilation of 'match_non_exhaustive_runtime_panic' failed");

  assert_eq!(result.exit_code, 101);
  assert!(
    result.stderr.contains("panic: non-exhaustive match"),
    "Expected stderr to contain 'panic: non-exhaustive match', got: {}",
    result.stderr
  );
}

// =========================================================================
// Unused Mut Lint - No Warning Tests
// =========================================================================

#[test]
fn e2e_no_warn_mut_assigned() {
  e2e_no_warnings(
    "no_warn_mut_assigned",
    r#"
function main(): i32 {
    let mut x: i32 = 0;
    x = 42;
    return x;
}
"#,
  );
}

#[test]
fn e2e_no_warn_mut_compound_assign() {
  e2e_no_warnings(
    "no_warn_mut_compound_assign",
    r#"
function main(): i32 {
    let mut x: i32 = 10;
    x += 5;
    return x;
}
"#,
  );
}

#[test]
fn e2e_no_warn_mut_postfix_increment() {
  e2e_no_warnings(
    "no_warn_mut_postfix_increment",
    r#"
function main(): i32 {
    let mut x: i32 = 0;
    x++;
    return x;
}
"#,
  );
}

#[test]
fn e2e_no_warn_mut_borrow() {
  e2e_no_warnings(
    "no_warn_mut_borrow",
    r#"
function main(): i32 {
    let mut x: i32 = 42;
    let ptr: *mut i32 = (&mut x) as *mut i32;
    return @read<i32>(ptr);
}
"#,
  );
}

#[test]
fn e2e_no_warn_mut_underscore() {
  e2e_no_warnings(
    "no_warn_mut_underscore",
    r#"
function main(): i32 {
    let mut _x: i32 = 42;
    return 0;
}
"#,
  );
}

#[test]
fn e2e_no_warn_mut_allow() {
  e2e_no_warnings(
    "no_warn_mut_allow",
    r#"
@allow(unused_mut)
function main(): i32 {
    let mut x: i32 = 42;
    return x;
}
"#,
  );
}
