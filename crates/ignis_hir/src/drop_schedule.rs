//! Drop scheduling: schedules that tell LIR lowering when to emit Drop instructions.

use std::collections::HashMap;

use ignis_type::definition::DefinitionId;

use crate::HIRId;

/// Key for early exit drop schedules.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExitKey {
  /// Explicit return statement
  Return(HIRId),

  /// Break out of loop
  Break(HIRId),

  /// Continue to next iteration
  Continue(HIRId),

  /// Synthetic return at function end (ensure_return, no explicit return HIRId)
  FnEnd(DefinitionId),
}

/// Drop schedules produced by ownership analysis.
/// All Vec<DefinitionId> are in drop order (reverse declaration, inner→outer).
/// All Vec<HIRId> for defers are in LIFO order (last registered = first executed).
#[derive(Debug, Default, Clone)]
pub struct DropSchedules {
  /// Drops at block end, keyed by Block HIRId.
  pub on_scope_end: HashMap<HIRId, Vec<DefinitionId>>,

  /// Drops at early exits (return/break/continue/FnEnd).
  pub on_exit: HashMap<ExitKey, Vec<DefinitionId>>,

  /// Drops before overwriting an owned variable, keyed by Assign HIRId.
  pub on_overwrite: HashMap<HIRId, Vec<DefinitionId>>,

  /// Deferred expression bodies at block end, keyed by Block HIRId.
  pub on_scope_end_defers: HashMap<HIRId, Vec<HIRId>>,

  /// Deferred expression bodies at early exits.
  pub on_exit_defers: HashMap<ExitKey, Vec<HIRId>>,
}

impl DropSchedules {
  pub fn new() -> Self {
    Self::default()
  }

  /// Check if there are any drops or defers scheduled anywhere.
  pub fn is_empty(&self) -> bool {
    self.on_scope_end.is_empty()
      && self.on_exit.is_empty()
      && self.on_overwrite.is_empty()
      && self.on_scope_end_defers.is_empty()
      && self.on_exit_defers.is_empty()
  }
}
