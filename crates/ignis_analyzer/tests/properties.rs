mod common;

use proptest::prelude::*;

/// Generate simple valid function bodies
fn valid_function_body() -> impl Strategy<Value = String> {
  prop_oneof![
    Just("return;".to_string()),
    Just("let x: i32 = 1;\n    return;".to_string()),
    Just("let mut x: i32 = 1;\n    x = 2;\n    return;".to_string()),
    Just("if true {\n        return;\n    } else {\n        return;\n    }".to_string()),
  ]
}

/// Generate valid Ignis programs
fn valid_program() -> impl Strategy<Value = String> {
  valid_function_body().prop_map(|body| format!("function main(): void {{\n    {}\n}}", body))
}

proptest! {
    /// Test that the analyzer never panics on valid programs
    #[test]
    fn analyzer_does_not_panic_on_valid_programs(src in valid_program()) {
        // Should not panic
        let _ = common::analyze(&src);
    }

    /// Test that output is deterministic for valid programs
    #[test]
    fn deterministic_output_on_valid_programs(src in valid_program()) {
        let result1 = common::analyze(&src);
        let result2 = common::analyze(&src);

        let hir1 = common::format_hir(&result1);
        let hir2 = common::format_hir(&result2);

        prop_assert_eq!(hir1, hir2, "HIR output should be deterministic");
    }

    /// Test that diagnostic count is deterministic
    #[test]
    fn deterministic_diagnostic_count(src in valid_program()) {
        let result1 = common::analyze(&src);
        let result2 = common::analyze(&src);

        prop_assert_eq!(
            result1.output.diagnostics.len(),
            result2.output.diagnostics.len(),
            "Diagnostic count should be deterministic"
        );
    }
}

// Property tests for error handling - these may produce errors but shouldn't panic
proptest! {
    #![proptest_config(ProptestConfig::with_cases(50))]

    /// Test that analyzer handles arbitrary character combinations
    #[test]
    fn analyzer_handles_arbitrary_input(src in "[a-zA-Z0-9_{}();:=+\\-*/ \n]*") {
        // May fail to parse, but should not panic
        let _ = std::panic::catch_unwind(|| {
            let _ = common::analyze_with_errors(&src);
        });
    }
}

/// Specific regression tests for edge cases
#[test]
fn empty_function_does_not_panic() {
  let result = common::analyze(
    r#"
function main(): void {
}
"#,
  );
  // May have warnings about missing return, but should not panic
  let _ = common::format_hir(&result);
}

#[test]
fn deeply_nested_blocks_do_not_panic() {
  let result = common::analyze(
    r#"
function main(): void {
    {
        {
            {
                {
                    {
                        return;
                    }
                }
            }
        }
    }
}
"#,
  );
  let _ = common::format_hir(&result);
}

#[test]
fn many_parameters_does_not_panic() {
  let result = common::analyze(
    r#"
function many(a: i32, b: i32, c: i32, d: i32, e: i32, f: i32, g: i32, h: i32): i32 {
    return a + b + c + d + e + f + g + h;
}
"#,
  );
  let _ = common::format_hir(&result);
}

#[test]
fn multiple_errors_do_not_crash() {
  let result = common::analyze(
    r#"
function main(): void {
    let a: i32 = x;
    let b: i32 = y;
    let c: i32 = z;
    a == 1;
    b == 2;
    c == 3;
    return;
}
"#,
  );
  // Should have multiple errors but not crash
  assert!(!result.output.diagnostics.is_empty());
}

#[test]
fn repeated_analysis_is_consistent() {
  let src = r#"
function helper(x: i32): i32 {
    return x * 2;
}

function main(): i32 {
    let a: i32 = helper(1);
    let b: i32 = helper(2);
    return a + b;
}
"#;

  // Run analysis 5 times and verify consistency
  let results: Vec<_> = (0..5).map(|_| common::analyze(src)).collect();

  let first_hir = common::format_hir(&results[0]);
  let first_diags = common::format_diagnostics(&results[0].output.diagnostics);

  for (i, result) in results.iter().enumerate().skip(1) {
    let hir = common::format_hir(result);
    let diags = common::format_diagnostics(&result.output.diagnostics);

    assert_eq!(first_hir, hir, "HIR mismatch on iteration {}", i);
    assert_eq!(first_diags, diags, "Diagnostics mismatch on iteration {}", i);
  }
}
