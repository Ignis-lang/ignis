mod common;

use ignis_config::TargetBackend;

#[test]
fn backend_boundary_compile_project_rejects_unsupported_backend() {
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
fn backend_boundary_build_std_rejects_unsupported_backend() {
  let attempt = common::build_std_with_target(TargetBackend::Iir).expect("temporary std build setup should succeed");

  assert!(attempt.result.is_err(), "expected build_std to fail for unsupported backend");
  assert!(
    !attempt.output_dir.join("std/lib/libignis_std.a").exists(),
    "unsupported backend should not emit a std archive"
  );
}

#[test]
fn backend_boundary_check_std_rejects_unsupported_backend() {
  let attempt = common::check_std_with_target(TargetBackend::Iir).expect("temporary std check setup should succeed");

  assert!(attempt.result.is_err(), "expected check_std to fail for unsupported backend");
  assert!(
    !attempt.output_dir.join("ignis_std.h").exists(),
    "unsupported backend should not emit the umbrella std header"
  );
}

#[test]
fn backend_boundary_build_std_preserves_c_success_behavior() {
  let attempt = common::build_std_with_target(TargetBackend::C).expect("temporary std build setup should succeed");

  assert!(attempt.result.is_ok(), "expected build_std to succeed for the C backend");
  assert!(
    attempt.output_dir.join("std/lib/libignis_std.a").exists(),
    "expected build_std to emit the std archive"
  );
}

#[test]
fn backend_boundary_check_std_preserves_c_success_behavior() {
  let attempt = common::check_std_with_target(TargetBackend::C).expect("temporary std check setup should succeed");

  assert!(attempt.result.is_ok(), "expected check_std to succeed for the C backend");
  assert!(
    attempt.output_dir.join("ignis_std.h").exists(),
    "expected check_std to emit the umbrella std header"
  );
}
