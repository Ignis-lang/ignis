use ignis_formatter::safety::{validate_formatted_output, validate_idempotence};
use ignis_formatter::{FormatOptions, format_text};

// --- Phase 5.1: Parse-back, token-loss, idempotence safety tests ---

#[test]
fn validate_formatted_output_rejects_non_parseable_formatted_text() {
  let source = "function main(): void { return; }\n";
  let formatted = "function main(): void { return; \"unterminated }\n";

  let error = validate_formatted_output(source, formatted, false)
    .expect_err("validation should reject formatted text that cannot be lexed/parsed back");

  assert!(
    error.to_string().contains("parse-back"),
    "expected parse-back validation failure, got {error}"
  );
}

#[test]
fn validate_formatted_output_rejects_formatted_text_with_missing_tokens() {
  let source = "function main(): void { let x: i32 = 1; return x; }\n";
  let formatted = "function main(): void {\n    return;\n}\n";

  let error = validate_formatted_output(source, formatted, false)
    .expect_err("validation should reject formatted text with fewer tokens than source");

  assert!(
    error.to_string().contains("token"),
    "expected token-loss validation failure, got {error}"
  );
}

#[test]
fn validate_formatted_output_rejects_formatted_text_with_extra_tokens() {
  let source = "function main(): void { return; }\n";
  let formatted = "function main(): void { let extra: i32 = 1; return; }\n";

  let error = validate_formatted_output(source, formatted, false)
    .expect_err("validation should reject formatted text with more tokens than source");

  assert!(
    error.to_string().contains("token"),
    "expected token-gain validation failure, got {error}"
  );
}

#[test]
fn validate_idempotence_rejects_when_second_format_pass_differs() {
  let first_pass = "function main(): void {\n    return;\n}\n";
  let different_second_pass = "function main(): void {\n  return;\n}\n";

  let error = validate_idempotence(first_pass, |_input| Ok(different_second_pass.to_string()))
    .expect_err("idempotence check should reject non-matching second pass");

  assert!(
    error.to_string().contains("idempotent"),
    "expected idempotence validation failure, got {error}"
  );
}

#[test]
fn validate_idempotence_accepts_when_second_pass_matches_first() {
  let first_pass = "function main(): void {\n  return;\n}\n";

  validate_idempotence(first_pass, |input| Ok(input.to_string()))
    .expect("idempotence check should accept matching second pass");
}

#[test]
fn validate_formatted_output_rejects_trailing_whitespace_in_formatted_output() {
  let source = "function main(): void { return; }\n";
  let formatted = "function main(): void {\n    return;   \n}\n";

  let error = validate_formatted_output(source, formatted, false)
    .expect_err("validation should reject formatted output with trailing whitespace");

  assert!(
    error.to_string().contains("trailing whitespace"),
    "expected trailing-whitespace validation failure, got {error}"
  );
}

#[test]
fn validate_formatted_output_rejects_missing_final_newline() {
  let source = "function main(): void { return; }\n";
  let formatted = "function main(): void {\n  return;\n}";

  let error = validate_formatted_output(source, formatted, false)
    .expect_err("validation should reject formatted output missing a final newline");

  assert!(
    error.to_string().contains("final newline"),
    "expected final-newline validation failure, got {error}"
  );
}

#[test]
fn format_text_end_to_end_safety_passes_for_canonical_simple_function() {
  let source = "function main(): void { return; }\n";

  let formatted =
    format_text(source, &FormatOptions::default()).expect("end-to-end format + safety should pass for simple function");

  assert!(formatted.ends_with('\n'), "formatted output should end with a newline");
  for line in formatted.lines() {
    assert_eq!(line, line.trim_end(), "no trailing whitespace: {line:?}");
  }
}

#[test]
fn format_text_preserves_mutable_complex_let_else_pattern() {
  let source = "function main(): void { let mut Option::SOME(active) = bag.getMut(activeIndex) else { @panic(\"missing active diagnostic\"); }; return; }\n";

  let formatted =
    format_text(source, &FormatOptions::default()).expect("formatter should preserve mut on complex let-else patterns");

  assert!(
    formatted.contains("let mut Option::SOME(active)"),
    "formatted output must preserve let-else mutability:\n{formatted}"
  );
}

#[test]
fn format_text_allows_cleaning_trailing_whitespace_in_comment_lines() {
  let source = "//! \n\nfunction main(): void { return; }\n";

  let formatted = format_text(source, &FormatOptions::default())
    .expect("comment safety should allow trimming trailing whitespace from comment lines");

  assert_eq!(formatted, "//!\n\nfunction main(): void {\n  return;\n}\n");
}

#[test]
fn format_text_end_to_end_idempotence_passes_for_record_with_comments() {
  let source = "/// A point in 2D space\nrecord Point {\n    x: i32;\n    y: i32;\n}\n";

  let first = format_text(source, &FormatOptions::default()).expect("first pass should succeed");
  let second = format_text(&first, &FormatOptions::default()).expect("second pass should succeed");

  assert_eq!(first, second, "formatted output must be idempotent across two passes");
}

#[test]
fn validate_no_trailing_whitespace_accepts_clean_lines() {
  let source = "function main(): void { return; }\n";
  let formatted = "function main(): void {\n  return;\n}\n";

  validate_formatted_output(source, formatted, false).expect("clean formatted output should pass all safety checks");
}

#[test]
fn validate_final_newline_accepts_empty_string() {
  let result = ignis_formatter::safety::validate_final_newline("");
  assert!(result.is_ok(), "empty string should not fail final newline check");
}

#[test]
fn validate_formatted_output_rejects_dropped_comment_content() {
  let source = "// important comment\nfunction main(): void { return; }\n";
  let formatted = "function main(): void {\n  return;\n}\n";

  let error = validate_formatted_output(source, formatted, false)
    .expect_err("validation should reject formatted text that drops a comment");

  assert!(
    error.to_string().contains("comment"),
    "expected comment-loss validation failure, got {error}"
  );
}

#[test]
fn validate_idempotence_accepts_when_reformatter_returns_identical_bytes() {
  let first_pass = "import Io from \"std::io\";\n\nfunction main(): void {\n  return;\n}\n";

  validate_idempotence(first_pass, |input| Ok(input.to_string()))
    .expect("byte-identical second pass should satisfy idempotence");
}

// --- Existing tests (Phase 3 safety baseline) ---

#[test]
fn validate_formatted_output_accepts_idempotent_token_preserving_results() {
  let source = "function main(): void { return; }\n";
  let formatted = "function main(): void {\n    return;\n}\n";

  validate_formatted_output(source, formatted, false).expect("validation should accept canonical result");
}

#[test]
fn validate_formatted_output_rejects_token_shape_changes() {
  let source = "function main(): void { return; }\n";
  let formatted = "function main(): void {\n    return\n}\n";

  let error =
    validate_formatted_output(source, formatted, false).expect_err("validation should reject missing semicolon");

  assert!(
    error.to_string().contains("token shape"),
    "expected token-shape validation failure, got {error}"
  );
}

#[test]
fn validate_formatted_output_rejects_reordered_imports() {
  let source = "import String from \"std::string\";\nimport LibC from \"std::libc\";\nimport Vector from \"std::vector\";\n\nimport _ from \"std::io::error\";\n";
  let formatted = "import LibC from \"std::libc\";\nimport String from \"std::string\";\nimport Vector from \"std::vector\";\n\nimport _ from \"std::io::error\";\n";

  let error =
    validate_formatted_output(source, formatted, false).expect_err("validation should reject reordered imports");

  assert!(
    error.to_string().contains("token shape"),
    "expected reorder validation failure, got {error}"
  );
}

#[test]
fn validate_formatted_output_rejects_comment_ownership_changes() {
  let source = "record Example {\n    value: i32; // keep with value\n    /// docs for next\n    next: i32;\n}\n";
  let formatted =
    "record Example {\n    value: i32;\n\n    // keep with value\n    /// docs for next\n    next: i32;\n}\n";

  let error = validate_formatted_output(source, formatted, false)
    .expect_err("validation should reject comment ownership changes even when tokens are preserved");

  assert!(
    error.to_string().contains("comment ownership"),
    "expected comment-ownership validation failure, got {error}"
  );
}

#[test]
fn validate_formatted_output_allows_reordered_imports_when_sorting_enabled() {
  let source = "import String from \"std::string\";\nimport LibC from \"std::libc\";\nimport Vector from \"std::vector\";\n\nimport _ from \"std::io::error\";\n";
  let formatted = "import LibC from \"std::libc\";\nimport String from \"std::string\";\nimport Vector from \"std::vector\";\n\nimport _ from \"std::io::error\";\n";

  validate_formatted_output(source, formatted, true)
    .expect("validation should accept reordered imports when sort_imports is enabled");
}
