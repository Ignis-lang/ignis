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

  let formatted = format_text(source, &FormatOptions::default()).expect("export namespace with type alias should format successfully");

  assert!(
    formatted.contains("export namespace CType {") && formatted.contains("type CVoidPtr = *mut void;"),
    "expected formatted namespace output, got {formatted}"
  );
}
