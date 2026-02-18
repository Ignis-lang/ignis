mod common;

use std::collections::HashSet;

use ignis_type::compilation_context::{CompilationContext, TargetInfo};

fn linux_x86_ctx() -> CompilationContext {
  CompilationContext {
    target: TargetInfo::from_triple("x86_64-unknown-linux-gnu"),
    debug: false,
    features: HashSet::new(),
    known_features: None,
  }
}

fn macos_aarch64_ctx() -> CompilationContext {
  CompilationContext {
    target: TargetInfo::from_triple("aarch64-apple-darwin"),
    debug: false,
    features: HashSet::new(),
    known_features: None,
  }
}

fn ctx_with_features(features: &[&str]) -> CompilationContext {
  CompilationContext {
    target: TargetInfo::from_triple("x86_64-unknown-linux-gnu"),
    debug: false,
    features: features.iter().map(|s| s.to_string()).collect(),
    known_features: None,
  }
}

fn ctx_with_known_features(
  enabled: &[&str],
  known: &[&str],
) -> CompilationContext {
  CompilationContext {
    target: TargetInfo::from_triple("x86_64-unknown-linux-gnu"),
    debug: false,
    features: enabled.iter().map(|s| s.to_string()).collect(),
    known_features: Some(known.iter().map(|s| s.to_string()).collect()),
  }
}

// =============================================================================
// @if with @platform
// =============================================================================

#[test]
fn e2e_compile_directive_platform_true() {
  let source = r#"
    @if(@platform("linux")) {
      function main(): i32 {
        return 42;
      }
    }
  "#;

  let result = common::compile_and_run_with_ctx(source, linux_x86_ctx()).unwrap();
  assert_eq!(result.exit_code, 42);
}

#[test]
fn e2e_compile_directive_platform_false_with_else() {
  let source = r#"
    @if(@platform("macos")) {
      function main(): i32 {
        return 1;
      }
    } @else {
      function main(): i32 {
        return 99;
      }
    }
  "#;

  let result = common::compile_and_run_with_ctx(source, linux_x86_ctx()).unwrap();
  assert_eq!(result.exit_code, 99);
}

// =============================================================================
// @if with @arch
// =============================================================================

#[test]
fn e2e_compile_directive_arch_true() {
  let source = r#"
    @if(@arch("x86_64")) {
      function main(): i32 {
        return 64;
      }
    } @else {
      function main(): i32 {
        return 32;
      }
    }
  "#;

  let result = common::compile_and_run_with_ctx(source, linux_x86_ctx()).unwrap();
  assert_eq!(result.exit_code, 64);
}

#[test]
fn e2e_compile_directive_arch_false() {
  let source = r#"
    @if(@arch("x86_64")) {
      function main(): i32 {
        return 64;
      }
    } @else {
      function main(): i32 {
        return 32;
      }
    }
  "#;

  let result = common::compile_and_run_with_ctx(source, macos_aarch64_ctx()).unwrap();
  assert_eq!(result.exit_code, 32);
}

// =============================================================================
// @if with @target (full triple)
// =============================================================================

#[test]
fn e2e_compile_directive_target_triple() {
  let source = r#"
    @if(@target("x86_64-unknown-linux-gnu")) {
      function main(): i32 {
        return 10;
      }
    } @else {
      function main(): i32 {
        return 20;
      }
    }
  "#;

  let result = common::compile_and_run_with_ctx(source, linux_x86_ctx()).unwrap();
  assert_eq!(result.exit_code, 10);
}

// =============================================================================
// @if with @feature
// =============================================================================

#[test]
fn e2e_compile_directive_feature_enabled() {
  let source = r#"
    @if(@feature("simd")) {
      function main(): i32 {
        return 1;
      }
    } @else {
      function main(): i32 {
        return 0;
      }
    }
  "#;

  let result = common::compile_and_run_with_ctx(source, ctx_with_features(&["simd"])).unwrap();
  assert_eq!(result.exit_code, 1);
}

#[test]
fn e2e_compile_directive_feature_disabled() {
  let source = r#"
    @if(@feature("simd")) {
      function main(): i32 {
        return 1;
      }
    } @else {
      function main(): i32 {
        return 0;
      }
    }
  "#;

  let result = common::compile_and_run_with_ctx(source, ctx_with_features(&[])).unwrap();
  assert_eq!(result.exit_code, 0);
}

// =============================================================================
// @ifelse (mandatory else)
// =============================================================================

#[test]
fn e2e_compile_directive_ifelse_true() {
  let source = r#"
    @ifelse(@platform("linux")) {
      function main(): i32 {
        return 11;
      }
    } @else {
      function main(): i32 {
        return 22;
      }
    }
  "#;

  let result = common::compile_and_run_with_ctx(source, linux_x86_ctx()).unwrap();
  assert_eq!(result.exit_code, 11);
}

#[test]
fn e2e_compile_directive_ifelse_false() {
  let source = r#"
    @ifelse(@platform("linux")) {
      function main(): i32 {
        return 11;
      }
    } @else {
      function main(): i32 {
        return 22;
      }
    }
  "#;

  let result = common::compile_and_run_with_ctx(source, macos_aarch64_ctx()).unwrap();
  assert_eq!(result.exit_code, 22);
}

// =============================================================================
// Boolean combinators: &&, ||, !
// =============================================================================

#[test]
fn e2e_compile_directive_and_combinator() {
  let source = r#"
    @if(@platform("linux") && @arch("x86_64")) {
      function main(): i32 {
        return 1;
      }
    } @else {
      function main(): i32 {
        return 0;
      }
    }
  "#;

  let result = common::compile_and_run_with_ctx(source, linux_x86_ctx()).unwrap();
  assert_eq!(result.exit_code, 1);
}

#[test]
fn e2e_compile_directive_or_combinator() {
  let source = r#"
    @if(@platform("linux") || @platform("macos")) {
      function main(): i32 {
        return 1;
      }
    } @else {
      function main(): i32 {
        return 0;
      }
    }
  "#;

  let result = common::compile_and_run_with_ctx(source, macos_aarch64_ctx()).unwrap();
  assert_eq!(result.exit_code, 1);
}

#[test]
fn e2e_compile_directive_not_combinator() {
  let source = r#"
    @if(!@platform("windows")) {
      function main(): i32 {
        return 1;
      }
    } @else {
      function main(): i32 {
        return 0;
      }
    }
  "#;

  let result = common::compile_and_run_with_ctx(source, linux_x86_ctx()).unwrap();
  assert_eq!(result.exit_code, 1);
}

#[test]
fn e2e_compile_directive_nested_boolean() {
  let source = r#"
    @if((@platform("linux") || @platform("macos")) && !@arch("arm")) {
      function main(): i32 {
        return 1;
      }
    } @else {
      function main(): i32 {
        return 0;
      }
    }
  "#;

  let result = common::compile_and_run_with_ctx(source, linux_x86_ctx()).unwrap();
  assert_eq!(result.exit_code, 1);
}

// =============================================================================
// Block-level directives (inside function body)
// =============================================================================

#[test]
fn e2e_compile_directive_in_block() {
  let source = r#"
    function main(): i32 {
      let mut x: i32 = 0;

      @if(@platform("linux")) {
        x = 42;
      } @else {
        x = 99;
      }

      return x;
    }
  "#;

  let result = common::compile_and_run_with_ctx(source, linux_x86_ctx()).unwrap();
  assert_eq!(result.exit_code, 42);
}

#[test]
fn e2e_compile_directive_in_block_false_branch() {
  let source = r#"
    function main(): i32 {
      let mut x: i32 = 0;

      @if(@platform("linux")) {
        x = 42;
      } @else {
        x = 99;
      }

      return x;
    }
  "#;

  let result = common::compile_and_run_with_ctx(source, macos_aarch64_ctx()).unwrap();
  assert_eq!(result.exit_code, 99);
}

// =============================================================================
// @if without @else (items simply omitted)
// =============================================================================

#[test]
fn e2e_compile_directive_if_no_else_true() {
  let source = r#"
    @if(@platform("linux")) {
      function helper(): i32 {
        return 5;
      }
    }

    function main(): i32 {
      return helper();
    }
  "#;

  let result = common::compile_and_run_with_ctx(source, linux_x86_ctx()).unwrap();
  assert_eq!(result.exit_code, 5);
}

// =============================================================================
// Nested @if directives
// =============================================================================

#[test]
fn e2e_compile_directive_nested() {
  let source = r#"
    @if(@platform("linux")) {
      @if(@arch("x86_64")) {
        function main(): i32 {
          return 64;
        }
      } @else {
        function main(): i32 {
          return 32;
        }
      }
    } @else {
      function main(): i32 {
        return 0;
      }
    }
  "#;

  let result = common::compile_and_run_with_ctx(source, linux_x86_ctx()).unwrap();
  assert_eq!(result.exit_code, 64);
}

// =============================================================================
// Error cases
// =============================================================================

#[test]
fn e2e_compile_directive_unknown_feature_strict() {
  let source = r#"
    @if(@feature("nonexistent")) {
      function main(): i32 {
        return 1;
      }
    }
  "#;

  let ctx = ctx_with_known_features(&[], &["simd", "qbe"]);
  let errors = common::parse_errors_with_ctx(source, ctx);
  assert!(!errors.is_empty(), "Expected parse error for unknown feature");
  assert!(
    errors.iter().any(|e| e.contains("Unknown feature")),
    "Expected 'Unknown feature' error, got: {:?}",
    errors
  );
}

#[test]
fn e2e_compile_directive_unknown_predicate() {
  let source = r#"
    @if(@foobar("x")) {
      function main(): i32 {
        return 1;
      }
    }
  "#;

  let errors = common::parse_errors_with_ctx(source, linux_x86_ctx());
  assert!(!errors.is_empty(), "Expected parse error for unknown predicate");
  assert!(
    errors.iter().any(|e| e.contains("Unknown compile-time predicate")),
    "Expected 'Unknown compile-time predicate' error, got: {:?}",
    errors
  );
}

#[test]
fn e2e_compile_directive_orphan_else() {
  let source = r#"
    @else {
      function main(): i32 {
        return 1;
      }
    }
  "#;

  let errors = common::parse_errors_with_ctx(source, linux_x86_ctx());
  assert!(!errors.is_empty(), "Expected parse error for orphan @else");
  assert!(
    errors.iter().any(|e| e.contains("@else")),
    "Expected '@else' error, got: {:?}",
    errors
  );
}

#[test]
fn e2e_compile_directive_ifelse_missing_else() {
  let source = r#"
    @ifelse(@platform("linux")) {
      function main(): i32 {
        return 1;
      }
    }
  "#;

  let errors = common::parse_errors_with_ctx(source, linux_x86_ctx());
  assert!(!errors.is_empty(), "Expected parse error for @ifelse without else");
  assert!(
    errors.iter().any(|e| e.contains("requires an else branch")),
    "Expected 'requires an else branch' error, got: {:?}",
    errors
  );
}
