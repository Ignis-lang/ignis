//! Drop scheduling: schedules that tell LIR lowering when to emit Drop instructions.

use std::collections::HashMap;

use ignis_type::{definition::DefinitionId, span::Span};

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

/// A point where a binding's ownership was transferred away.
///
/// Recorded for debugging only (`--dump-drop-schedule`): a binding that is marked moved
/// is deliberately *not* dropped at the end of its scope, so the move site is the missing
/// half of the story when a drop is absent.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MoveSite {
  /// Function whose body contains the move.
  pub function: DefinitionId,

  /// Span of the expression that consumed the binding.
  pub span: Span,
}

/// Drop schedules produced by ownership analysis.
/// All Vec<DefinitionId> are in drop order (reverse declaration, inner→outer).
/// All Vec<HIRId> for defers are in LIFO order (last registered = first executed).
#[derive(Debug, Default, Clone)]
pub struct DropSchedules {
  /// Drops at block end, keyed by Block HIRId.
  pub on_scope_end: HashMap<HIRId, Vec<DefinitionId>>,

  /// Drops for a match arm's pattern bindings, keyed by the arm body's HIRId.
  ///
  /// Separate from `on_scope_end` because an arm body may itself be a block, and
  /// sharing the key would make `lower_block` and the match lowering each emit
  /// the whole list — one drop scheduled, two drops run.
  pub on_match_arm_end: HashMap<HIRId, Vec<DefinitionId>>,

  /// Drops at early exits (return/break/continue/FnEnd).
  pub on_exit: HashMap<ExitKey, Vec<DefinitionId>>,

  /// Drops before overwriting an owned variable, keyed by Assign HIRId.
  pub on_overwrite: HashMap<HIRId, Vec<DefinitionId>>,

  /// Drops before overwriting one field of an owned value, keyed by Assign HIRId
  /// and carrying the field's definition.
  ///
  /// Separate from `on_overwrite` because the two name different things: an entry
  /// there is a local, which lowering drops through its `LocalId`, while an entry
  /// here is a field, which lowering drops through the field pointer it already
  /// computed for the store. Writing a field replaces that field and nothing else,
  /// so the record around it must keep every drop it still owes.
  pub on_field_overwrite: HashMap<HIRId, Vec<DefinitionId>>,

  /// Deferred expression bodies at block end, keyed by Block HIRId.
  pub on_scope_end_defers: HashMap<HIRId, Vec<HIRId>>,

  /// Deferred expression bodies at early exits.
  pub on_exit_defers: HashMap<ExitKey, Vec<HIRId>>,

  /// Where each binding was marked moved. Debug metadata only: nothing in lowering
  /// reads it, and it is deliberately excluded from [`DropSchedules::is_empty`].
  pub moves: HashMap<DefinitionId, Vec<MoveSite>>,
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
      && self.on_field_overwrite.is_empty()
      && self.on_scope_end_defers.is_empty()
      && self.on_exit_defers.is_empty()
  }
}
