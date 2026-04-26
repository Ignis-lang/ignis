use ignis_formatter::safety::validate_formatted_output;

#[test]
fn validate_formatted_output_accepts_idempotent_token_preserving_results() {
  let source = "function main(): void { return; }\n";
  let formatted = "function main(): void {\n    return;\n}\n";

  validate_formatted_output(source, formatted).expect("validation should accept canonical result");
}

#[test]
fn validate_formatted_output_rejects_token_shape_changes() {
  let source = "function main(): void { return; }\n";
  let formatted = "function main(): void {\n    return\n}\n";

  let error = validate_formatted_output(source, formatted).expect_err("validation should reject missing semicolon");

  assert!(
    error.to_string().contains("token shape"),
    "expected token-shape validation failure, got {error}"
  );
}

#[test]
fn validate_formatted_output_rejects_reordered_imports() {
  let source = "import String from \"std::string\";\nimport LibC from \"std::libc\";\nimport Vector from \"std::vector\";\n\nimport _ from \"std::io::error\";\n";
  let formatted = "import LibC from \"std::libc\";\nimport String from \"std::string\";\nimport Vector from \"std::vector\";\n\nimport _ from \"std::io::error\";\n";

  let error = validate_formatted_output(source, formatted).expect_err("validation should reject reordered imports");

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

  let error = validate_formatted_output(source, formatted)
    .expect_err("validation should reject comment ownership changes even when tokens are preserved");

  assert!(
    error.to_string().contains("comment ownership"),
    "expected comment-ownership validation failure, got {error}"
  );
}
