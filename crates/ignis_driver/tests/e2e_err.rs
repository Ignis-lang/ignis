mod common;

use insta::assert_snapshot;

fn e2e_error_test(
  name: &str,
  source: &str,
) {
  let diagnostics = common::compile_diagnostics(source).expect("lex/parse failed");
  assert!(!diagnostics.is_empty(), "expected errors");
  assert_snapshot!(name, diagnostics.join("\n"));
}

#[test]
fn e2e_err_index_out_of_bounds_positive() {
  e2e_error_test(
    "err_index_out_of_bounds_positive",
    r#"
function main(): i32 {
    let arr: i32[3] = [1, 2, 3];
    return arr[5];
}
"#,
  );
}

#[test]
fn e2e_err_index_out_of_bounds_negative() {
  e2e_error_test(
    "err_index_out_of_bounds_negative",
    r#"
function main(): i32 {
    let arr: i32[3] = [1, 2, 3];
    return arr[-1];
}
"#,
  );
}

#[test]
fn e2e_err_index_out_of_bounds_exact() {
  e2e_error_test(
    "err_index_out_of_bounds_exact",
    r#"
function main(): i32 {
    let arr: i32[3] = [1, 2, 3];
    return arr[3];
}
"#,
  );
}
