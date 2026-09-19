//! The verifier's rejection of a local freed twice on one path.
//!
//! The schedules are the source of these drops, and their recorders now list a value once
//! per site, so the only way to hand lowering a duplicate is to write one into the
//! schedule directly — which is what a second walk of a loop body used to do.

mod common;

use ignis_lir::verify::VerifyError;

/// A closure reassigned inside a loop: the shape PR #226 found the double free in.
const REASSIGNED_CLOSURE: &str = "function make(limit: i32): (i32) -> i32 {
  let base: i32 = 40;
  let mut f = (x: i32): i32 -> x + base;
  let mut index: i32 = 0;

  while (index < limit) {
    f = (x: i32): i32 -> x + base;
    index += 1;
  }

  return f;
}
";

fn double_drops(errors: &[VerifyError]) -> usize {
  errors
    .iter()
    .filter(|error| matches!(error, VerifyError::DoubleDrop { .. }))
    .count()
}

#[test]
fn one_drop_per_site_verifies() {
  let result = common::lower_to_lir(REASSIGNED_CLOSURE);

  assert!(
    result.verify_errors.is_empty(),
    "expected no verification errors, got: {:?}",
    result.verify_errors
  );
}

#[test]
fn a_site_that_frees_one_value_twice_is_rejected() {
  let result = common::lower_to_lir_with_schedule_edit(REASSIGNED_CLOSURE, |schedules| {
    for scheduled in schedules.on_overwrite.values_mut() {
      let repeated = scheduled.clone();
      scheduled.extend(repeated);
    }
  });

  assert_eq!(
    double_drops(&result.verify_errors),
    1,
    "expected the duplicated overwrite to be rejected, got: {:?}",
    result.verify_errors
  );
}

#[test]
fn a_scope_end_drop_and_the_return_drop_of_one_value_are_not_a_double_drop() {
  // Both schedules name `value`, and both fire — on different paths, in different blocks.
  // Reporting that pair is the false positive the intra-block rule exists to avoid.
  let result = common::lower_to_lir(
    "@implements(Drop)
record Owned {
  public id: i32;

  drop(&mut self): void {
    return;
  }
}

function run(): i32 {
  let value: Owned = Owned { id: 1 };

  return value.id;
}
",
  );

  assert!(
    result.verify_errors.is_empty(),
    "expected no verification errors, got: {:?}",
    result.verify_errors
  );
}
