use std::fs;
use std::path::PathBuf;

use ignis_formatter::{FormatOptions, format_text};
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

  assert_eq!(
    formatted,
    "function main(): i32 {\n  return value |> transform;\n}\n"
  );
}
