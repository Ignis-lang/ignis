use std::fs;
use std::path::PathBuf;

use ignis_formatter::{FormatOptions, FormatterConfig, format_text};
use insta::assert_snapshot;

fn workspace_root() -> PathBuf {
  PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .join("../..")
    .canonicalize()
    .expect("workspace root")
}

fn extract_slice(
  source: &str,
  start_marker: &str,
  end_marker: &str,
) -> String {
  let start = source
    .find(start_marker)
    .unwrap_or_else(|| panic!("missing start marker: {start_marker}\n{source}"));
  let end = source[start..]
    .find(end_marker)
    .map(|offset| start + offset)
    .unwrap_or_else(|| panic!("missing end marker: {end_marker}\n{source}"));

  source[start..end].trim_end().to_string() + "\n"
}

#[test]
fn preserves_real_import_order_while_formatting_directive_free_input() {
  let source = fs::read_to_string(workspace_root().join("std/io/mod.ign")).expect("read std/io source");
  let import_block = source.lines().skip(56).take(5).collect::<Vec<_>>().join("\n") + "\n";

  let formatted = format_text(&import_block, &FormatOptions::default()).expect("format real import block");

  assert_snapshot!("preserves_real_import_order_while_formatting_directive_free_input", formatted);
}

#[test]
fn formats_generic_function_signatures_without_angle_bracket_spacing() {
  let formatted = format_text(
    "function   identity<T>( value : Result<T, Error> ) : Result<T,Error> {return value;}\n",
    &FormatOptions::default(),
  )
  .expect("format generic function signature");

  assert_snapshot!("formats_generic_function_signatures_without_angle_bracket_spacing", formatted);
}

#[test]
fn preserves_comments_and_directives_while_formatting_generic_branch_code() {
  let formatted = format_text(
    "/// docs\n@if(flag){function  identity<T>( value: Result<T,Error>):Result<T,Error>{return value;}}@else{// disabled\nfunction disabled(): void {return;}}\n",
    &FormatOptions::default(),
  )
  .expect("format directive branch code");

  assert_snapshot!(
    "preserves_comments_and_directives_while_formatting_generic_branch_code",
    formatted
  );
}

#[test]
fn documents_directive_comment_policy_without_collapsing_inactive_branches() {
  let formatted = format_text(
    "// detached\n@if(featureFlag){/// docs\nfunction enabled<T>( value: Result<T,Error>):Result<T,Error>{return value;}}@else{// keep branch\nfunction disabled(): void {return;}}\n",
    &FormatOptions::default(),
  )
  .expect("format published directive policy example");

  assert_snapshot!(
    "documents_directive_comment_policy_without_collapsing_inactive_branches",
    formatted
  );
}

#[test]
fn preserves_export_trait_order_and_top_level_spacing_for_real_hash_slice() {
  let source = fs::read_to_string(workspace_root().join("std/hash/mod.ign")).expect("read std/hash source");
  let trait_slice = extract_slice(&source, "export trait Hash {", "export record Hasher {");

  let formatted = format_text(&trait_slice, &FormatOptions::default()).expect("format trait slice");

  assert_snapshot!(
    "preserves_export_trait_order_and_top_level_spacing_for_real_hash_slice",
    formatted
  );
}

#[test]
fn formats_control_flow_defer_and_cast_slices_without_reordering_statements() {
  let formatted = format_text(
    "function  walk( ptr : * mut Node ): void {defer Memory::free(ptr as *void);while (ptr != null) {if (ptr == null) {break;}continue;}}\n",
    &FormatOptions::default(),
  )
  .expect("format control-flow slice");

  assert_snapshot!(
    "formats_control_flow_defer_and_cast_slices_without_reordering_statements",
    formatted
  );
}

#[test]
fn formats_let_else_match_lambda_and_ternary_slices_without_reordering_arms() {
  let formatted = format_text(
    "function  compute(input: Option<i32>): i32 {let Some(value) = input else {return 0;};return match (value) {0 | 1 -> 1, _ if value > 10 -> ((x: i32): i32 -> x + 1)(value), _ -> value > 2 ? value : 2,};}\n",
    &FormatOptions::default(),
  )
  .expect("format let-else/match/lambda slice");

  assert_snapshot!(
    "formats_let_else_match_lambda_and_ternary_slices_without_reordering_arms",
    formatted
  );
}

#[test]
fn formats_generic_record_member_and_record_initializer_type_slices_without_reordering_members() {
  let formatted = format_text(
    "record  ReducerBox<T> { public values: T[]; public reducer: (T,&T)->T; }\nfunction  build<T>( values : T[], reducer : (T,&T)->T ) : ReducerBox<T> { return ReducerBox<T> { values: values, reducer: reducer, }; }\n",
    &FormatOptions::default(),
  )
  .expect("format generic record member and record initializer slice");

  assert_snapshot!(
    "formats_generic_record_member_and_record_initializer_type_slices_without_reordering_members",
    formatted
  );
}

#[test]
fn formats_nested_record_initializer_values_without_reordering_outer_fields() {
  let formatted = format_text(
    "record Child {\nleft: i32;\nright: i32;\n}\nrecord Parent {\nchild: Child;\ncount: i32;\n}\nfunction build(value: i32): Parent { return Parent { child: Child { left: value, right: value + 1, }, count: 2, }; }\n",
    &FormatOptions::default(),
  )
  .expect("format nested record initializer value slice");

  assert_snapshot!(
    "formats_nested_record_initializer_values_without_reordering_outer_fields",
    formatted
  );
}

#[test]
fn preserves_user_blank_lines_between_statements_inside_blocks() {
  let formatted = format_text(
    "function group(): void {\n  let first = 1;\n\n  let second = 2;\n\n  return;\n}\n",
    &FormatOptions::default(),
  )
  .expect("format grouped statements with preserved blank lines");

  assert_snapshot!("preserves_user_blank_lines_between_statements_inside_blocks", formatted);
}

#[test]
fn formats_pipe_expressions_with_correct_operator_spacing() {
  let formatted = format_text(
    "function main(): i32 { return value |> transform; }\n",
    &FormatOptions::default(),
  )
  .expect("pipe expressions should format successfully");

  assert_eq!(formatted, "function main(): i32 {\n  return value |> transform;\n}\n");
}

// Phase 2: Task 2.1 — Top-level declaration snapshots

#[test]
fn formats_multiple_imports_with_preserved_order() {
  let formatted = format_text(
    "import   Alpha , Beta  from  \"std::alpha\";\nimport  Gamma from \"std::gamma\";\n",
    &FormatOptions::default(),
  )
  .expect("multiple imports should format successfully");

  assert_snapshot!("formats_multiple_imports_with_preserved_order", formatted);
}

#[test]
fn wraps_import_items_when_line_width_is_exceeded() {
  let options = FormatOptions {
    config: FormatterConfig {
      line_width: 80,
      ..FormatterConfig::default()
    },
    ..FormatOptions::default()
  };

  let formatted = format_text(
    "import TomlArray, TomlArrayOfTables, TomlDateTime, TomlDateTimeKind, TomlDocument, TomlTable, TomlTableEntry, TomlTableState, TomlValue, TomlValueKind from \"./value\";\n",
    &options,
  )
  .expect("long import should format successfully");

  assert_eq!(
    formatted,
    "import\n  TomlArray,\n  TomlArrayOfTables,\n  TomlDateTime,\n  TomlDateTimeKind,\n  TomlDocument,\n  TomlTable,\n  TomlTableEntry,\n  TomlTableState,\n  TomlValue,\n  TomlValueKind,\nfrom \"./value\";\n"
  );
}

#[test]
fn keeps_import_items_inline_when_they_fit_line_width() {
  let options = FormatOptions {
    config: FormatterConfig {
      line_width: 80,
      ..FormatterConfig::default()
    },
    ..FormatOptions::default()
  };

  let formatted = format_text("import   Alpha , Beta  from  \"std::alpha\";\n", &options)
    .expect("short import should format successfully");

  assert_eq!(formatted, "import Alpha, Beta from \"std::alpha\";\n");
}

#[test]
fn groups_consecutive_imports_from_the_same_path() {
  let formatted = format_text(
    "import TomlArray from \"./value\";\nimport TomlArray from \"./value\";\n",
    &FormatOptions::default(),
  )
  .expect("duplicate same-path imports should format successfully");

  assert_eq!(formatted, "import TomlArray, TomlArray from \"./value\";\n");
}

#[test]
fn keeps_import_groups_separate_across_blank_lines() {
  let formatted = format_text(
    "import TomlArray from \"./value\";\n\nimport TomlValue from \"./value\";\n",
    &FormatOptions::default(),
  )
  .expect("same-path imports separated by a blank line should format successfully");

  assert_eq!(
    formatted,
    "import TomlArray from \"./value\";\n\nimport TomlValue from \"./value\";\n"
  );
}

#[test]
fn formats_export_name_and_reexport_without_reordering() {
  let formatted = format_text(
    "export   foo;\nexport   bar ,  baz  from  \"mod\";\n",
    &FormatOptions::default(),
  )
  .expect("export name and reexport variants should format successfully");

  assert_snapshot!("formats_export_name_and_reexport_without_reordering", formatted);
}

#[test]
fn wraps_reexport_items_when_line_width_is_exceeded() {
  let options = FormatOptions {
    config: FormatterConfig {
      line_width: 80,
      ..FormatterConfig::default()
    },
    ..FormatOptions::default()
  };

  let formatted = format_text(
    "export TomlArray, TomlArrayOfTables, TomlDateTime, TomlDateTimeKind, TomlDocument, TomlTable, TomlTableEntry, TomlTableState, TomlValue, TomlValueKind from \"./value\";\n",
    &options,
  )
  .expect("long re-export should format successfully");

  assert_eq!(
    formatted,
    "export\n  TomlArray,\n  TomlArrayOfTables,\n  TomlDateTime,\n  TomlDateTimeKind,\n  TomlDocument,\n  TomlTable,\n  TomlTableEntry,\n  TomlTableState,\n  TomlValue,\n  TomlValueKind,\nfrom \"./value\";\n"
  );
}

#[test]
fn groups_consecutive_reexports_from_the_same_path() {
  let formatted = format_text(
    "export TomlArray from \"./value\";\nexport TomlValue from \"./value\";\n",
    &FormatOptions::default(),
  )
  .expect("same-path re-exports should format successfully");

  assert_eq!(formatted, "export TomlArray, TomlValue from \"./value\";\n");
}

#[test]
fn formats_function_with_attributes_and_generic_params() {
  let formatted = format_text(
    "@deprecated(\"use newFunc\")\nfunction   process<T,U>( input : T ,  extra : U ) : Result<T,Error> { return ok(input); }\n",
    &FormatOptions::default(),
  )
  .expect("function with attributes and generic params should format successfully");

  assert_snapshot!("formats_function_with_attributes_and_generic_params", formatted);
}

#[test]
fn formats_function_with_empty_body() {
  let formatted = format_text("function   noop( ) : void  {}\n", &FormatOptions::default())
    .expect("function with empty body should format successfully");

  assert_snapshot!("formats_function_with_empty_body", formatted);
}

#[test]
fn formats_export_type_alias_with_generics() {
  let formatted = format_text("export  type   Pair<T>  =  Result<T,T>;\n", &FormatOptions::default())
    .expect("export type alias with generics should format successfully");

  assert_snapshot!("formats_export_type_alias_with_generics", formatted);
}

#[test]
fn formats_import_followed_by_function_with_blank_line_separator() {
  let formatted = format_text(
    "import foo from \"bar\";\nfunction main(): void { return; }\n",
    &FormatOptions::default(),
  )
  .expect("import-function transition should have blank line separator");

  assert_snapshot!("formats_import_followed_by_function_with_blank_line_separator", formatted);
}

#[test]
fn formats_multiline_function_signature_with_many_parameters() {
  let formatted = format_text(
    "function veryLongFunctionSignatureThatShouldBreak(firstArgument: i32, secondArgument: i32, thirdArgument: i32, fourthArgument: i32): i32 { return firstArgument; }\n",
    &FormatOptions::default(),
  )
  .expect("function with many parameters should format with multiline signature");

  assert_snapshot!("formats_multiline_function_signature_with_many_parameters", formatted);
}

#[test]
fn preserves_trailing_comma_in_multiline_extern_function_signature() {
  let formatted = format_text(
    "extern Foo {\n  function veryLongFunctionSignatureThatShouldBreak(firstArgument: i32, secondArgument: i32, thirdArgument: i32, fourthArgument: i32,): i32;\n}\n",
    &FormatOptions::default(),
  )
  .expect("extern function signature with trailing comma should format successfully");

  assert!(
    formatted.contains("fourthArgument: i32,\n  ): i32;"),
    "expected multiline signature to preserve trailing comma\nformatted:\n{formatted}"
  );
}

#[test]
fn removes_trailing_comma_when_signature_collapses_inline() {
  let formatted = format_text(
    "function snapshotFileName(testName: &String, snapshotName: str,): String { return snapshotName as String; }\n",
    &FormatOptions::default(),
  )
  .expect("inline signature should drop trailing comma");

  assert!(
    formatted.contains("function snapshotFileName(testName: &String, snapshotName: str): String {"),
    "expected inline signature without trailing comma\nformatted:\n{formatted}"
  );
}

// Triangulation for Task 2.1 — edge cases

#[test]
fn formats_import_with_single_item_and_whitespace_cleanup() {
  let formatted = format_text("import   OnlyItem   from  \"std::single\";\n", &FormatOptions::default())
    .expect("single-item import should clean up whitespace");

  assert_eq!(formatted, "import OnlyItem from \"std::single\";\n");
}

#[test]
fn formats_export_function_with_simple_signature() {
  let formatted = format_text(
    "export  function   helper(  x :  i32 ) : void  { return; }\n",
    &FormatOptions::default(),
  )
  .expect("export function should format with clean signature");

  assert_snapshot!("formats_export_function_with_simple_signature", formatted);
}

#[test]
fn formats_simple_type_alias_without_generics() {
  let formatted =
    format_text("type   MyInt  =  i32;\n", &FormatOptions::default()).expect("simple type alias should format cleanly");

  assert_eq!(formatted, "type MyInt = i32;\n");
}

#[test]
fn formats_type_alias_with_complex_target_type() {
  let formatted = format_text("type   Callback  =  ( i32 ,  str )  ->  void;\n", &FormatOptions::default())
    .expect("type alias with callable target should format correctly");

  assert_eq!(formatted, "type Callback = (i32, str) -> void;\n");
}

// Phase 2: Task 2.3 — Expression/control-flow snapshots

#[test]
fn formats_block_with_mixed_statement_types() {
  let formatted = format_text(
    "function mixed(): void { let x = 1;if (x > 0) {return;}for (let i = 0; i < 10; i += 1) {continue;}while (true) {break;}}\n",
    &FormatOptions::default(),
  )
  .expect("block with mixed statements should format successfully");

  assert_snapshot!("formats_block_with_mixed_statement_types", formatted);
}

#[test]
fn formats_nested_bare_blocks_with_current_indentation() {
  let formatted = format_text(
    "function scoped(): void { let x = 1; { let y = 2; } return; }\n",
    &FormatOptions::default(),
  )
  .expect("nested bare block should format successfully");

  assert_eq!(
    formatted,
    "function scoped(): void {\n  let x = 1;\n  {\n    let y = 2;\n  }\n  return;\n}\n"
  );
}

#[test]
fn formats_match_expression_with_multiple_arms() {
  let formatted = format_text(
    "function classify(n: i32): str { return match (n) { 0 -> \"zero\", 1 -> \"one\", _ -> \"many\", }; }\n",
    &FormatOptions::default(),
  )
  .expect("match with multiple arms should format successfully");

  assert_snapshot!("formats_match_expression_with_multiple_arms", formatted);
}

#[test]
fn formats_assignment_match_expression_without_adding_outer_parentheses() {
  let formatted = format_text(
    "function closes(next: Option<char>): boolean { let mut result: boolean = false; result = match (next) { Option::SOME(value) -> value == '/', Option::NONE -> false, }; return result; }\n",
    &FormatOptions::default(),
  )
  .expect("assignment match expression should preserve token shape");

  assert!(
    formatted.contains("result = match (next) {"),
    "match expression assigned to a variable must not gain outer parentheses:\n{formatted}"
  );
}

#[test]
fn formats_let_else_with_binding_type() {
  let formatted = format_text(
    "function test(input: Option<i32>): i32 { let Some(value) = input else { return 0; }; return value; }\n",
    &FormatOptions::default(),
  )
  .expect("let-else should format successfully");

  assert_snapshot!("formats_let_else_with_binding_type", formatted);
}

#[test]
fn formats_lambda_with_block_body() {
  let formatted = format_text(
    "function apply(fn: (i32) -> i32, x: i32): i32 { return fn(x); }\n",
    &FormatOptions::default(),
  )
  .expect("lambda with block body in arguments should format");

  assert_snapshot!("formats_lambda_with_block_body", formatted);
}

#[test]
fn formats_cast_expressions() {
  let formatted = format_text("function casts(x: i64): i32 { return x as i32; }\n", &FormatOptions::default())
    .expect("cast expressions should format");

  assert_snapshot!("formats_cast_expressions", formatted);
}

#[test]
fn formats_record_initializer_in_let_binding() {
  let formatted = format_text(
    "record Point { x: i32; y: i32; }\nfunction build(x: i32, y: i32): Point { let p = Point { x: x, y: y, }; return p; }\n",
    &FormatOptions::default(),
  )
  .expect("record initializer in let binding should format multiline");

  assert_snapshot!("formats_record_initializer_in_let_binding", formatted);
}

#[test]
fn formats_for_of_loop() {
  let formatted = format_text(
    "function sum(items: i32[]): i32 { let total = 0; for (let item of items) { total += item; } return total; }\n",
    &FormatOptions::default(),
  )
  .expect("for-of loop should format correctly");

  assert_snapshot!("formats_for_of_loop", formatted);
}

#[test]
fn formats_defer_statement() {
  let formatted = format_text(
    "function cleanup(): void { defer release(); return; }\n",
    &FormatOptions::default(),
  )
  .expect("defer statement should format correctly");

  assert_snapshot!("formats_defer_statement", formatted);
}

// Phase 3: Task 3.1 — Pipe chain layout

#[test]
fn formats_single_pipe_inline_when_it_fits() {
  let formatted = format_text(
    "function main(): i32 { return  value  |>   transform ; }\n",
    &FormatOptions::default(),
  )
  .expect("single pipe should format inline");

  assert_eq!(formatted, "function main(): i32 {\n  return value |> transform;\n}\n");
}

#[test]
fn formats_two_pipe_chain_multiline_with_indent() {
  let formatted = format_text(
    "function main(): i32 { return value |> step1 |> step2; }\n",
    &FormatOptions::default(),
  )
  .expect("two-pipe chain should format multiline");

  assert_eq!(
    formatted,
    "function main(): i32 {\n  return value\n    |> step1\n    |> step2;\n}\n"
  );
}

#[test]
fn formats_three_pipe_chain_multiline_with_indent() {
  let formatted = format_text(
    "function main(): i32 { return data |> parse |> validate |> execute; }\n",
    &FormatOptions::default(),
  )
  .expect("three-pipe chain should format multiline");

  assert_eq!(
    formatted,
    "function main(): i32 {\n  return data\n    |> parse\n    |> validate\n    |> execute;\n}\n"
  );
}

// Phase 3: Task 3.2 — Blank-line preservation

#[test]
fn preserves_single_blank_line_between_top_level_items() {
  let source = "function first(): void { return; }\n\nfunction second(): void { return; }\n";
  let formatted = format_text(source, &FormatOptions::default()).expect("blank line between items should format");

  assert_eq!(
    formatted,
    "function first(): void {\n  return;\n}\n\nfunction second(): void {\n  return;\n}\n"
  );
}

#[test]
fn collapses_multiple_blank_lines_to_single() {
  let source = "function first(): void { return; }\n\n\n\nfunction second(): void { return; }\n";
  let formatted = format_text(source, &FormatOptions::default()).expect("multiple blank lines should collapse");

  assert_eq!(
    formatted,
    "function first(): void {\n  return;\n}\n\nfunction second(): void {\n  return;\n}\n"
  );
}

#[test]
fn does_not_insert_blank_lines_between_consecutive_constants_without_source_gap() {
  let source = "const A: i32 = 1;\nconst B: i32 = 2;\nconst C: i32 = 3;\n";
  let formatted = format_text(source, &FormatOptions::default()).expect("consecutive constants without gaps");

  assert_eq!(formatted, "const A: i32 = 1;\nconst B: i32 = 2;\nconst C: i32 = 3;\n");
}

#[test]
fn preserves_existing_blank_line_between_constants() {
  let source = "const A: i32 = 1;\n\nconst B: i32 = 2;\n";
  let formatted =
    format_text(source, &FormatOptions::default()).expect("blank line between constants should be preserved");

  assert_eq!(formatted, "const A: i32 = 1;\n\nconst B: i32 = 2;\n");
}

#[test]
fn ensures_final_newline_in_formatted_output() {
  let source = "function main(): void { return; }";
  let formatted = format_text(source, &FormatOptions::default()).expect("output should have final newline");

  assert!(formatted.ends_with('\n'), "formatted output must end with a newline");
}

#[test]
fn trims_trailing_whitespace_from_lines() {
  let source = "function main(): void { return; }   \n";
  let formatted = format_text(source, &FormatOptions::default()).expect("trailing whitespace should be trimmed");

  for line in formatted.lines() {
    assert_eq!(line, line.trim_end(), "line has trailing whitespace: {:?}", line);
  }
}
