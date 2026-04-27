use ignis_formatter::{FormatOptions, format_text};

#[test]
fn rejects_parser_invalid_source_instead_of_recoverably_rewriting_it() {
  let source = "function main(): void { let value = ; }\n";

  let error = format_text(source, &FormatOptions::default()).expect_err("invalid source must fail fast");

  assert!(
    error.to_string().contains("parser")
      || error.to_string().contains("unsupported")
      || error.to_string().contains("failed"),
    "expected parser-invalid failure, got {error}"
  );
}

#[test]
fn formats_export_namespace_with_type_alias_inside() {
  let source = "export namespace CType {\n    type CVoidPtr = *mut void;\n}\n";

  let formatted = format_text(source, &FormatOptions::default())
    .expect("export namespace with type alias should format successfully");

  assert!(
    formatted.contains("export namespace CType {") && formatted.contains("type CVoidPtr = *mut void;"),
    "expected formatted namespace output, got {formatted}"
  );
}

#[test]
fn formats_export_const_with_source_preserving_fallback() {
  let source = "export const MY_CONST: i32 = 42;\n";

  let formatted = format_text(source, &FormatOptions::default())
    .expect("export const should not fail on valid code");

  assert!(
    formatted.contains("MY_CONST"),
    "formatted output should preserve the constant name, got {formatted}"
  );
}

#[test]
fn formats_export_variable_with_source_preserving_fallback() {
  let source = "export const MY_VALUE: i32 = 42;\n";

  let result = format_text(source, &FormatOptions::default());

  assert!(
    result.is_ok(),
    "formatter must not fail on valid export declarations: {:?}",
    result.err()
  );
}

#[test]
fn valid_code_never_produces_unsupported_error() {
  let sources = [
    "function main(): void { return; }\n",
    "import foo from \"bar\";\n",
    "record Point { x: i32; y: i32; }\n",
    "enum Color { RED, GREEN, BLUE, }\n",
    "function id(x: i32): i32 { return x; }\n",
    "export function hello(): void {}\n",
    "export record Pair { a: i32; b: i32; }\n",
    "export enum Status { OK, ERR, }\n",
    "export type Handle = i32;\n",
    "export namespace Inner {\n    function helper(): void {}\n}\n",
    "export trait Printable { format(&self): str; }\n",
  ];

  for source in &sources {
    let result = format_text(source, &FormatOptions::default());
    assert!(
      result.is_ok(),
      "valid source should not produce FormatError::Unsupported:\n  source: {source:?}\n  error: {:?}",
      result.err()
    );
  }
}

// Phase 2: Task 2.4 — Attribute fallback tests

#[test]
fn formats_attribute_only_declarations_without_failing() {
  let sources = [
    "@deprecated(\"old\")\nfunction legacy(): void { return; }\n",
    "@lang(try)\nexport enum Option<T> {\n  SOME(T),\n  NONE,\n}\n",
  ];

  for source in &sources {
    let result = format_text(source, &FormatOptions::default());
    assert!(
      result.is_ok(),
      "attribute-prefixed declarations should not fail:\n  source: {source:?}\n  error: {:?}",
      result.err()
    );
  }
}

#[test]
fn formats_unattached_comment_blocks_without_dropping_them() {
  let formatted = format_text(
    "// standalone comment\n\nfunction main(): void { return; }\n",
    &FormatOptions::default(),
  )
  .expect("standalone comment should not break formatting");

  assert!(
    formatted.contains("standalone comment"),
    "formatted output should preserve the standalone comment, got: {formatted}"
  );
}

#[test]
fn formats_multiple_attribute_declarations_without_reordering() {
  let source = "@deprecated\nfunction first(): void { return; }\n@lang(try)\nexport enum Option<T> {\n  SOME(T),\n  NONE,\n}\n";

  let formatted = format_text(source, &FormatOptions::default())
    .expect("multiple attribute declarations should format without reordering");

  let first_pos = formatted.find("first").expect("first should appear");
  let second_pos = formatted.find("Option").expect("Option should appear");
  assert!(
    first_pos < second_pos,
    "declarations must stay in original order"
  );
}
