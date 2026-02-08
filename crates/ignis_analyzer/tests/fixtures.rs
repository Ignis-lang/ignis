mod common;

use insta::assert_snapshot;
use std::fs;
use std::path::Path;

/// Test a fixture file from the test_cases directory
fn test_fixture(path: &str) {
  let full_path = Path::new(env!("CARGO_MANIFEST_DIR"))
    .parent()
    .unwrap()
    .parent()
    .unwrap()
    .join(path);

  let src = fs::read_to_string(&full_path).unwrap_or_else(|e| panic!("Failed to read {}: {}", path, e));

  let result = common::analyze(&src);

  let name = Path::new(path).file_stem().unwrap().to_str().unwrap().replace('-', "_");

  assert_snapshot!(
    format!("fixture_{}_diags", name),
    common::format_diagnostics(&result.output.diagnostics)
  );
}

// --- Mutability Tests ---

#[test]
fn fixture_immutable_assign() {
  test_fixture("test_cases/analyzer/mutability/immutable_assign.ign");
}

#[test]
fn fixture_mut_ref_to_immutable() {
  test_fixture("test_cases/analyzer/mutability/mut_ref_to_immutable.ign");
}

// --- Borrow Tests ---

#[test]
fn fixture_borrow_conflict_mut() {
  test_fixture("test_cases/analyzer/borrows/borrow_conflict_mut.ign");
}

#[test]
fn fixture_valid_borrows() {
  test_fixture("test_cases/analyzer/borrows/valid_borrows.ign");
}

#[test]
fn fixture_mutate_while_borrowed() {
  test_fixture("test_cases/analyzer/borrows/mutate_while_borrowed.ign");
}

// --- Cast Tests ---

#[test]
fn fixture_invalid_cast() {
  test_fixture("test_cases/analyzer/casts/invalid_cast.ign");
}

#[test]
fn fixture_numeric_cast() {
  test_fixture("test_cases/analyzer/casts/numeric_cast.ign");
}

// --- Type Error Tests ---

#[test]
fn fixture_assignment_mismatch() {
  test_fixture("test_cases/analyzer/type_errors/assignment_mismatch.ign");
}

// --- Unreachable Code Tests ---

#[test]
fn fixture_unreachable_after_return() {
  test_fixture("test_cases/analyzer/unreachable/unreachable_after_return.ign");
}

// --- Missing Return Tests ---

#[test]
fn fixture_missing_return() {
  test_fixture("test_cases/analyzer/missing_return/missing_return.ign");
}

#[test]
fn fixture_if_without_else() {
  test_fixture("test_cases/analyzer/missing_return/if_without_else.ign");
}

#[test]
fn fixture_if_else_both_return() {
  test_fixture("test_cases/analyzer/missing_return/if_else_both_return.ign");
}

#[test]
fn fixture_if_else_only_then_returns() {
  test_fixture("test_cases/analyzer/missing_return/if_else_only_then_returns.ign");
}

#[test]
fn fixture_if_else_only_else_returns() {
  test_fixture("test_cases/analyzer/missing_return/if_else_only_else_returns.ign");
}

#[test]
fn fixture_nested_if_all_paths_return() {
  test_fixture("test_cases/analyzer/missing_return/nested_if_all_paths_return.ign");
}

// --- Extern Tests ---

#[test]
fn fixture_valid_extern() {
  test_fixture("test_cases/analyzer/extern/valid_extern.ign");
}

// --- Generics Error Tests ---

#[test]
fn fixture_wrong_number_type_args_record() {
  test_fixture("test_cases/analyzer/generics/errors/wrong_number_type_args_record.ign");
}

#[test]
fn fixture_wrong_number_type_args_function() {
  test_fixture("test_cases/analyzer/generics/errors/wrong_number_type_args_function.ign");
}

#[test]
fn fixture_type_param_with_args() {
  test_fixture("test_cases/analyzer/generics/errors/type_param_with_args.ign");
}

// --- Generic Type Alias Tests ---

#[test]
fn fixture_generic_type_alias() {
  test_fixture("test_cases/analyzer/generics/type_alias/generic_type_alias.ign");
}

// --- Lang Trait Tests ---

#[test]
fn fixture_drop_valid() {
  test_fixture("test_cases/analyzer/lang_traits/drop_valid.ign");
}

#[test]
fn fixture_clone_valid() {
  test_fixture("test_cases/analyzer/lang_traits/clone_valid.ign");
}

#[test]
fn fixture_drop_clone_valid() {
  test_fixture("test_cases/analyzer/lang_traits/drop_clone_valid.ign");
}

#[test]
fn fixture_drop_copy_conflict() {
  test_fixture("test_cases/analyzer/lang_traits/drop_copy_conflict.ign");
}

#[test]
fn fixture_unknown_trait() {
  test_fixture("test_cases/analyzer/lang_traits/unknown_trait.ign");
}

#[test]
fn fixture_drop_missing_method() {
  test_fixture("test_cases/analyzer/lang_traits/drop_missing_method.ign");
}

#[test]
fn fixture_drop_wrong_signature() {
  test_fixture("test_cases/analyzer/lang_traits/drop_wrong_signature.ign");
}

#[test]
fn fixture_clone_wrong_return() {
  test_fixture("test_cases/analyzer/lang_traits/clone_wrong_return.ign");
}
