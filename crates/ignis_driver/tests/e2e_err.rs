mod common;

use ignis_config::TargetBackend;

#[test]
fn e2e_err_unsupported_backend_iir() {
  let attempt = common::compile_project_single_file(
    r#"
function main(): i32 {
    return 0;
}
"#,
    TargetBackend::Iir,
  )
  .expect("temporary project setup should succeed");

  assert!(
    attempt.result.is_err(),
    "expected compile_project to fail for unsupported backend"
  );
  assert!(!attempt.bin_path.exists(), "unsupported backend should not emit a binary");
}

#[test]
fn e2e_err_build_std_unsupported_backend_iir() {
  let attempt = common::build_std_with_target(TargetBackend::Iir).expect("temporary std build setup should succeed");

  assert!(attempt.result.is_err(), "expected build_std to fail for unsupported backend");
  assert!(
    !attempt.output_dir.join("std/lib/libignis_std.a").exists(),
    "unsupported backend should not emit a std archive"
  );
}

#[test]
fn e2e_err_check_std_unsupported_backend_iir() {
  let attempt = common::check_std_with_target(TargetBackend::Iir).expect("temporary std check setup should succeed");

  assert!(attempt.result.is_err(), "expected check_std to fail for unsupported backend");
  assert!(
    !attempt.output_dir.join("ignis_std.h").exists(),
    "unsupported backend should not emit the umbrella std header"
  );
}

// ========================================================================
// @builtin(...) error tests
// ========================================================================

#[test]
fn e2e_err_config_flag_non_string() {
  use ignis_type::compilation_context::CompilationContext;
  let errors = common::parse_errors_with_ctx(
    r#"
function main(): void {
    let flag: boolean = @configFlag(42);
}
"#,
    CompilationContext::default(),
  );
  assert!(!errors.is_empty(), "Expected parse error for invalid condition");
  assert!(
    errors.iter().any(|e| e.contains("condition")),
    "Expected condition error, got: {:?}",
    errors
  );
}

#[test]
fn e2e_err_builtin_arg_count() {
  use ignis_type::compilation_context::CompilationContext;
  let errors = common::parse_errors_with_ctx(
    r#"
function main(): void {
    let flag: boolean = @configFlag();
}
"#,
    CompilationContext::default(),
  );
  assert!(!errors.is_empty(), "Expected parse error for empty condition");
  assert!(
    errors.iter().any(|e| e.contains("condition")),
    "Expected condition error, got: {:?}",
    errors
  );
}

#[test]
fn e2e_err_multi_scalar_char_literal() {
  use ignis_type::compilation_context::CompilationContext;

  let errors = common::parse_errors_with_ctx(
    r#"
function main(): i32 {
    let c: char = 'ab';
    return c as i32;
}
"#,
    CompilationContext::default(),
  );

  assert!(!errors.is_empty(), "Expected parse error for multi-scalar char literal");
  assert!(
    errors
      .iter()
      .any(|error| error.contains("Invalid char literal: expected exactly one Unicode scalar value")),
    "Expected Unicode-scalar char literal error, got: {:?}",
    errors
  );
}

#[test]
fn e2e_err_surrogate_char_literal_unicode_escape() {
  use ignis_type::compilation_context::CompilationContext;

  let errors = common::parse_errors_with_ctx(
    r#"
function main(): i32 {
    let c: char = '\u{D800}';
    return c as i32;
}
"#,
    CompilationContext::default(),
  );

  assert!(
    !errors.is_empty(),
    "Expected parse error for surrogate char literal unicode escape"
  );
  assert!(
    errors
      .iter()
      .any(|error| error.contains("Invalid char escape: expected a valid Unicode scalar")),
    "Expected Unicode-scalar char escape error, got: {:?}",
    errors
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
