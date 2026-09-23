//! The verifier's rejection of a local freed twice on one path.
//!
//! The schedules are the source of these drops, and their recorders now list a value once
//! per site, so the only way to hand lowering a duplicate is to write one into the
//! schedule directly — which is what a second walk of a loop body used to do.

mod common;

use std::collections::HashMap;

use ignis_lir::verify::{VerifyError, verify_lir};
use ignis_lir::{Instr, LocalId, Operand, TempId};

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
fn a_closure_drop_that_names_its_slot_directly_is_still_checked() {
  // Lowering loads the closure into a temp before freeing it, so the direct form has no
  // source-level spelling and has to be written here. A drop's own operand must not count
  // as taking the local's address: if it did, this pair would go unchecked.
  let mut result = common::lower_to_lir(REASSIGNED_CLOSURE);

  for func in result.program.functions.values_mut() {
    let block_ids: Vec<_> = func.blocks.iter().map(|(id, _)| id).collect();

    for block_id in block_ids {
      let block = func.blocks.get_mut(&block_id);
      let mut loaded_from: HashMap<TempId, LocalId> = HashMap::new();
      let mut rewritten: Vec<Instr> = Vec::with_capacity(block.instructions.len());

      for instr in block.instructions.drain(..) {
        if let Instr::Load { dest, source } = &instr {
          loaded_from.insert(*dest, *source);
        }

        let Instr::DropClosure {
          closure: Operand::Temp(temp),
          closure_type,
        } = &instr
        else {
          rewritten.push(instr);
          continue;
        };

        let Some(local) = loaded_from.get(temp).copied() else {
          rewritten.push(instr);
          continue;
        };

        let direct = Instr::DropClosure {
          closure: Operand::Local(local),
          closure_type: *closure_type,
        };

        rewritten.push(direct.clone());
        rewritten.push(direct);
      }

      block.instructions = rewritten;
    }
  }

  let errors = match verify_lir(&result.program, &result.types, &result.analyzer_output.defs) {
    Ok(()) => Vec::new(),
    Err(errors) => errors,
  };

  assert_eq!(
    double_drops(&errors),
    1,
    "expected the repeated closure drop to be rejected, got: {errors:?}"
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

#[test]
fn a_binding_from_a_reference_scrutinee_matched_again_is_never_freed() {
  // `inner` names a place inside the referent the caller owns. Matching it again used to
  // copy it into a drop-tracked scrutinee slot and free that copy, releasing the
  // caller's payload on every call.
  let result = common::lower_to_lir(
    "@implements(Drop)
record Owned {
  public id: i32;

  drop(&mut self): void {
    return;
  }
}

enum Inner {
  Held(Owned),
  Empty(i32),
}

enum Outer {
  Wrap(Inner),
  Nothing,
}

function probe(outer: &Outer): boolean {
  if (let Outer::Wrap(inner) = outer) {
    if (let Inner::Empty(_) = inner) {
      return true;
    }

    return match (inner) {
      Inner::Held(held) -> held.id == 0,
      _ -> false,
    };
  }

  return false;
}
",
  );

  assert!(
    result.verify_errors.is_empty(),
    "expected no verification errors, got: {:?}",
    result.verify_errors
  );

  let drops = result
    .program
    .functions
    .values()
    .flat_map(|function| function.blocks.iter().map(|(_, block)| block))
    .flat_map(|block| block.instructions.iter())
    .filter(|instr| matches!(instr, Instr::Drop { .. }))
    .count();

  assert_eq!(drops, 0, "expected no drop of the borrowed binding, got {drops}");
}
