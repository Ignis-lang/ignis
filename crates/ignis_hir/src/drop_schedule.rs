//! Drop scheduling: schedules that tell LIR lowering when to emit Drop instructions.

use std::collections::{HashMap, HashSet};

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
///
/// # One entry per value per site
///
/// The ownership walk reaches the same node twice: `check_loop` goes through a loop body
/// a second time to simulate another iteration. Every schedule below therefore has to say
/// what a second visit does, and the two answers are:
///
/// - **Replaced.** `on_scope_end`, `on_exit`, `on_condition_fail`, `on_condition_skip`,
///   `on_scope_end_defers` and `on_exit_defers` are written with `HashMap::insert`, so the
///   second visit supersedes the first. The list is rebuilt from the scope stack each
///   time and a repeat is impossible by construction.
/// - **Accumulated.** `on_overwrite`, `on_field_overwrite`, `on_match_arm_end` and `moves`
///   grow across visits. A plain `push` there schedules the same free twice for one site,
///   which is a double free. Each has a recorder below that adds nothing already listed,
///   and nothing outside this type appends to them.
///
/// `borrowed_pattern_bindings` is a set and needs no rule. Closure environments have no
/// schedule of their own: a closure local is listed like any other value and lowering
/// turns its drop into `DropClosure`.
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

  /// Drops owed by a `&&` whose right operand evaluated to false, keyed by that
  /// `Binary` HIRId.
  ///
  /// A chain of `let` conditions (`if (let A = f() && let B = g())`) hands its bindings
  /// to the branch it guards, and the branch is the only place they are dropped. When a
  /// later link of the chain fails, that branch never runs, yet everything the earlier
  /// links bound is already initialized: the short-circuit false path is the only site
  /// where those bindings are live and unowned. Keying on the `&&` node — rather than on
  /// the `if` — is what keeps the drop precise: reaching this node's false path with the
  /// right operand evaluated means every `let` on the left succeeded, so exactly the
  /// bindings listed here hold a value.
  pub on_condition_fail: HashMap<HIRId, Vec<DefinitionId>>,

  /// Drops owed by a `&&` whose *left* operand evaluated to false, keyed by that
  /// `Binary` HIRId.
  ///
  /// A `let` condition consumes its scrutinee, and the ownership walk records that move
  /// once for the whole condition. Behind a short-circuit the move is conditional: with
  /// `if (before && let P = value)` a false `before` skips the `let` entirely, so `value`
  /// was never moved and no later site frees it — scope end already skipped it. This path
  /// is where it is still live and unowned, and the only place its drop can go.
  ///
  /// Only a scrutinee that is a *place* is listed: a call temporary the skipped operand
  /// would have produced was never created, so nothing is owed for it.
  pub on_condition_skip: HashMap<HIRId, Vec<DefinitionId>>,

  /// Drops before overwriting an owned variable, keyed by Assign HIRId.
  ///
  /// One assignment drops one value, so a definition appears at most once per
  /// entry. Record through [`DropSchedules::record_overwrite`], which enforces
  /// that: a loop body is walked twice to simulate a second iteration, and a
  /// plain push would schedule the same free twice for one assignment.
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

  /// Pattern bindings destructured out of a borrowed scrutinee, which name storage the
  /// match never owned and therefore appear in no drop schedule above.
  ///
  /// Ownership decides binding mode and lowering implements it: a binding listed here is
  /// lowered as a place inside the referent — its local holds the payload's address — so
  /// a reference taken from it points into the referent and outlives the match. Deciding
  /// it in lowering instead would let the two disagree, and a binding lowered as a
  /// pointer while ownership still schedules its drop frees a pointer.
  ///
  /// Not every listed binding gets an address: lowering also needs the scrutinee's own
  /// address, which it has only when the scrutinee is reference-typed. The rest stay
  /// by-value copies, exactly as before.
  pub borrowed_pattern_bindings: HashSet<DefinitionId>,
}

impl DropSchedules {
  pub fn new() -> Self {
    Self::default()
  }

  /// Schedule the drop of `def_id` before the overwrite at `assign`, once.
  ///
  /// The caller can reach the same assignment twice — `check_loop` walks a body
  /// a second time to simulate another iteration — and one assignment drops one
  /// value, so a repeat is the same schedule rather than a second drop.
  pub fn record_overwrite(
    &mut self,
    assign: HIRId,
    def_id: DefinitionId,
  ) {
    let scheduled = self.on_overwrite.entry(assign).or_default();

    if !scheduled.contains(&def_id) {
      scheduled.push(def_id);
    }
  }

  /// Schedule the drop of the field `def_id` before the overwrite at `assign`, once.
  ///
  /// One assignment replaces one field, so a repeat is the same schedule. An assignment
  /// inside a loop body is reached twice and would otherwise owe two frees for it.
  pub fn record_field_overwrite(
    &mut self,
    assign: HIRId,
    def_id: DefinitionId,
  ) {
    let scheduled = self.on_field_overwrite.entry(assign).or_default();

    if !scheduled.contains(&def_id) {
      scheduled.push(def_id);
    }
  }

  /// Schedule the drop of `def_id` at the end of the arm body `arm`, once.
  ///
  /// An arm inside a loop body is walked twice, and both the pattern bindings the arm owns
  /// and the values its siblings handed to the match result are recorded on each visit.
  pub fn record_match_arm_end(
    &mut self,
    arm: HIRId,
    def_id: DefinitionId,
  ) {
    let scheduled = self.on_match_arm_end.entry(arm).or_default();

    if !scheduled.contains(&def_id) {
      scheduled.push(def_id);
    }
  }

  /// Record that `def_id` was moved at `site`, once per distinct site.
  ///
  /// A binding really can be moved at two places — one per branch — so this keeps sites
  /// apart by span. What it rejects is the same span twice, which is the second loop walk
  /// re-reporting one move.
  pub fn record_move(
    &mut self,
    def_id: DefinitionId,
    site: MoveSite,
  ) {
    let sites = self.moves.entry(def_id).or_default();

    if !sites.contains(&site) {
      sites.push(site);
    }
  }

  /// Check if there are any drops or defers scheduled anywhere.
  pub fn is_empty(&self) -> bool {
    self.on_scope_end.is_empty()
      && self.on_exit.is_empty()
      && self.on_condition_fail.is_empty()
      && self.on_condition_skip.is_empty()
      && self.on_overwrite.is_empty()
      && self.on_field_overwrite.is_empty()
      && self.on_scope_end_defers.is_empty()
      && self.on_exit_defers.is_empty()
  }
}
