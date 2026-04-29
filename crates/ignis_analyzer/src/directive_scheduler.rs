use ignis_ast::NodeId;
use ignis_diagnostics::diagnostic_report::{Diagnostic, Severity};
use ignis_type::attribute::{DirectiveEffect, DirectivePhase};
use ignis_type::definition::{DefinitionId, DirectiveDefId, DirectiveProvenance};
use ignis_type::file::FileId;
use ignis_type::span::Span;
use ignis_type::BytePosition;

use crate::directive_registry::DirectiveRegistry;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DirectiveSchedulePlan {
  pub stages: Vec<DirectiveScheduleStage>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DirectiveScheduleStage {
  pub phase: DirectivePhase,
  pub entries: Vec<DirectiveScheduleEntry>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DirectiveScheduleEntry {
  pub source_order: usize,
  pub directive: DirectiveDefId,
  pub function_def_id: DefinitionId,
  pub target_node: NodeId,
  pub effect: DirectiveEffect,
  pub span: Span,
  pub provenance: DirectiveProvenance,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DirectiveFingerprint {
  Opaque(String),
}

impl DirectiveFingerprint {
  pub fn opaque(value: impl Into<String>) -> Self {
    Self::Opaque(value.into())
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DirectiveIterationFingerprint {
  pub components: Vec<DirectiveFingerprint>,
}

impl DirectiveIterationFingerprint {
  pub fn new(components: Vec<DirectiveFingerprint>) -> Self {
    Self { components }
  }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DirectiveIterationRecord {
  pub iteration: usize,
  pub fingerprint: DirectiveIterationFingerprint,
  pub requested_reanalysis: bool,
}

impl DirectiveIterationRecord {
  pub fn new(
    iteration: usize,
    fingerprint: DirectiveIterationFingerprint,
    requested_reanalysis: bool,
  ) -> Self {
    Self {
      iteration,
      fingerprint,
      requested_reanalysis,
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DirectiveStageResult {
  pub phase: DirectivePhase,
  pub fingerprints: Vec<DirectiveFingerprint>,
  pub requested_reanalysis: bool,
}

impl DirectiveStageResult {
  pub fn new(
    phase: DirectivePhase,
    fingerprints: Vec<DirectiveFingerprint>,
    requested_reanalysis: bool,
  ) -> Self {
    Self {
      phase,
      fingerprints,
      requested_reanalysis,
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DirectiveExecutionProgress {
  AdvancedStage { next_phase: DirectivePhase },
  StartedNextIteration { iteration: usize },
  Converged { iteration: usize },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DirectiveSchedulerError {
  NoCurrentStage,
  PhaseMismatch {
    expected: DirectivePhase,
    actual: DirectivePhase,
  },
  CycleLimitExceeded {
    limit: usize,
    iterations: Vec<DirectiveIterationRecord>,
  },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DirectiveExecutionState {
  pub plan: DirectiveSchedulePlan,
  cycle_limit: usize,
  iteration: usize,
  next_stage_index: usize,
  pending_fingerprints: Vec<DirectiveFingerprint>,
  pending_reanalysis: bool,
  completed_iterations: Vec<DirectiveIterationRecord>,
  converged: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DirectiveReanalysisRequest {
  pub iteration: usize,
  pub fingerprint: DirectiveIterationFingerprint,
}

pub trait DirectiveStageExecutor {
  fn execute_stage(
    &mut self,
    stage: &DirectiveScheduleStage,
  ) -> DirectiveStageResult;
}

pub trait DirectiveReanalysisHook {
  fn request_reanalysis(
    &mut self,
    request: DirectiveReanalysisRequest,
  );
}

#[derive(Debug, Clone)]
pub struct DirectiveSchedulerFailure {
  pub error: DirectiveSchedulerError,
  diagnostic: Option<Diagnostic>,
}

impl DirectiveSchedulerFailure {
  fn new(
    plan: &DirectiveSchedulePlan,
    error: DirectiveSchedulerError,
  ) -> Self {
    let diagnostic = match &error {
      DirectiveSchedulerError::CycleLimitExceeded { limit, iterations } => {
        Some(cycle_limit_diagnostic(plan, *limit, iterations))
      },
      DirectiveSchedulerError::NoCurrentStage | DirectiveSchedulerError::PhaseMismatch { .. } => None,
    };

    Self { error, diagnostic }
  }

  pub fn as_diagnostic(&self) -> Option<Diagnostic> {
    self.diagnostic.clone()
  }
}

#[derive(Debug, Clone, Default)]
pub struct DirectiveExecutionReport {
  pub executed_phases: Vec<DirectivePhase>,
  pub completed_iterations: Vec<DirectiveIterationRecord>,
  pub reanalysis_requests: Vec<DirectiveReanalysisRequest>,
  pub converged: bool,
  pub failure: Option<DirectiveSchedulerFailure>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DirectiveScheduler {
  cycle_limit: usize,
}

impl DirectiveScheduler {
  pub fn new(cycle_limit: usize) -> Self {
    Self {
      cycle_limit: cycle_limit.max(1),
    }
  }

  pub fn run(
    &mut self,
    plan: DirectiveSchedulePlan,
    executor: &mut impl DirectiveStageExecutor,
    reanalysis_hook: &mut impl DirectiveReanalysisHook,
  ) -> DirectiveExecutionReport {
    let mut state = DirectiveExecutionState::new(plan.clone(), self.cycle_limit);
    let mut executed_phases = Vec::new();
    let mut reanalysis_requests = Vec::new();

    while let Some(stage) = state.current_stage().cloned() {
      executed_phases.push(stage.phase.clone());

      match state.consume_current_stage(executor.execute_stage(&stage)) {
        Ok(DirectiveExecutionProgress::AdvancedStage { .. }) | Ok(DirectiveExecutionProgress::Converged { .. }) => {},
        Ok(DirectiveExecutionProgress::StartedNextIteration { .. }) => {
          if let Some(iteration) = state.completed_iterations().last() {
            let request = DirectiveReanalysisRequest {
              iteration: iteration.iteration,
              fingerprint: iteration.fingerprint.clone(),
            };

            reanalysis_hook.request_reanalysis(request.clone());
            reanalysis_requests.push(request);
          }
        },
        Err(error) => {
          return DirectiveExecutionReport {
            executed_phases,
            completed_iterations: state.completed_iterations().to_vec(),
            reanalysis_requests,
            converged: false,
            failure: Some(DirectiveSchedulerFailure::new(&plan, error)),
          };
        },
      }
    }

    DirectiveExecutionReport {
      executed_phases,
      completed_iterations: state.completed_iterations().to_vec(),
      reanalysis_requests,
      converged: state.is_converged(),
      failure: None,
    }
  }
}

#[derive(Debug, Default)]
pub struct NoopDirectiveStageExecutor;

impl DirectiveStageExecutor for NoopDirectiveStageExecutor {
  fn execute_stage(
    &mut self,
    stage: &DirectiveScheduleStage,
  ) -> DirectiveStageResult {
    DirectiveStageResult::new(stage.phase.clone(), Vec::new(), false)
  }
}

#[derive(Debug, Default)]
pub struct NoopDirectiveReanalysisHook;

impl DirectiveReanalysisHook for NoopDirectiveReanalysisHook {
  fn request_reanalysis(
    &mut self,
    _request: DirectiveReanalysisRequest,
  ) {
  }
}

impl DirectiveSchedulePlan {
  pub fn from_registry(registry: &DirectiveRegistry) -> Self {
    let mut entries_by_phase = Vec::new();

    for (source_order, directive_use) in registry.uses.iter().enumerate() {
      let Some(directive_def) = registry.defs.get(directive_use.directive.index() as usize) else {
        continue;
      };

      entries_by_phase.push((
        directive_def.phase.clone(),
        DirectiveScheduleEntry {
          source_order,
          directive: directive_use.directive,
          function_def_id: directive_def.function_def_id,
          target_node: directive_use.target_node,
          effect: directive_def.effect.clone(),
          span: directive_use.span.clone(),
          provenance: directive_use.provenance.clone(),
        },
      ));
    }

    let mut stages = Vec::new();

    for phase in phase_order() {
      let mut phase_entries = entries_by_phase
        .iter()
        .filter(|(entry_phase, _)| entry_phase == &phase)
        .map(|(_, entry)| entry.clone())
        .collect::<Vec<_>>();

      phase_entries
        .sort_by_key(|entry| (entry.span.file.index(), entry.span.start, entry.span.end, entry.source_order));

      if !phase_entries.is_empty() {
        stages.push(DirectiveScheduleStage {
          phase,
          entries: phase_entries,
        });
      }
    }

    Self { stages }
  }
}

impl DirectiveExecutionState {
  pub fn new(
    plan: DirectiveSchedulePlan,
    cycle_limit: usize,
  ) -> Self {
    Self {
      converged: plan.stages.is_empty(),
      plan,
      cycle_limit: cycle_limit.max(1),
      iteration: 0,
      next_stage_index: 0,
      pending_fingerprints: Vec::new(),
      pending_reanalysis: false,
      completed_iterations: Vec::new(),
    }
  }

  pub fn iteration(&self) -> usize {
    self.iteration
  }

  pub fn current_stage(&self) -> Option<&DirectiveScheduleStage> {
    if self.converged {
      return None;
    }

    self.plan.stages.get(self.next_stage_index)
  }

  pub fn completed_iterations(&self) -> &[DirectiveIterationRecord] {
    &self.completed_iterations
  }

  pub fn is_converged(&self) -> bool {
    self.converged
  }

  pub fn consume_current_stage(
    &mut self,
    result: DirectiveStageResult,
  ) -> Result<DirectiveExecutionProgress, DirectiveSchedulerError> {
    let Some(stage) = self.current_stage() else {
      return Err(DirectiveSchedulerError::NoCurrentStage);
    };

    if stage.phase != result.phase {
      return Err(DirectiveSchedulerError::PhaseMismatch {
        expected: stage.phase.clone(),
        actual: result.phase,
      });
    }

    self.pending_fingerprints.extend(result.fingerprints);
    self.pending_reanalysis |= result.requested_reanalysis;
    self.next_stage_index += 1;

    if let Some(next_stage) = self.plan.stages.get(self.next_stage_index) {
      return Ok(DirectiveExecutionProgress::AdvancedStage {
        next_phase: next_stage.phase.clone(),
      });
    }

    self.finish_iteration()
  }

  fn finish_iteration(&mut self) -> Result<DirectiveExecutionProgress, DirectiveSchedulerError> {
    let record = DirectiveIterationRecord::new(
      self.iteration,
      DirectiveIterationFingerprint::new(std::mem::take(&mut self.pending_fingerprints)),
      self.pending_reanalysis,
    );
    let previous_fingerprint = self
      .completed_iterations
      .last()
      .map(|iteration| iteration.fingerprint.clone());

    self.completed_iterations.push(record.clone());
    self.pending_reanalysis = false;

    let fingerprint_changed = previous_fingerprint
      .map(|previous| previous != record.fingerprint)
      .unwrap_or(true);

    if !record.requested_reanalysis || !fingerprint_changed {
      self.converged = true;
      return Ok(DirectiveExecutionProgress::Converged {
        iteration: record.iteration,
      });
    }

    if self.completed_iterations.len() >= self.cycle_limit {
      return Err(DirectiveSchedulerError::CycleLimitExceeded {
        limit: self.cycle_limit,
        iterations: self.completed_iterations.clone(),
      });
    }

    self.iteration += 1;
    self.next_stage_index = 0;

    Ok(DirectiveExecutionProgress::StartedNextIteration {
      iteration: self.iteration,
    })
  }
}

fn phase_order() -> [DirectivePhase; 5] {
  [
    DirectivePhase::Check,
    DirectivePhase::Expand,
    DirectivePhase::Collect,
    DirectivePhase::Finalize,
    DirectivePhase::Transform,
  ]
}

fn cycle_limit_diagnostic(
  plan: &DirectiveSchedulePlan,
  limit: usize,
  iterations: &[DirectiveIterationRecord],
) -> Diagnostic {
  let span = plan
    .stages
    .iter()
    .flat_map(|stage| stage.entries.iter())
    .map(|entry| entry.provenance.origin_attr_span.clone())
    .next()
    .unwrap_or_else(|| Span::new(FileId::SYNTHETIC, BytePosition(0), BytePosition(0)));

  let iteration_summary = iterations
    .iter()
    .map(|iteration| format!("iteration {} => {:?}", iteration.iteration, iteration.fingerprint.components))
    .collect::<Vec<_>>()
    .join("; ");

  Diagnostic::new(
    Severity::Error,
    format!(
      "directive expansion exceeded the cycle limit of {} iteration(s) without reaching a fixed point",
      limit
    ),
    "A0195".to_string(),
    span,
  )
  .with_note(format!("scheduler iterations: {iteration_summary}"))
}

#[cfg(test)]
mod tests {
  use std::collections::VecDeque;

  use ignis_ast::NodeId;
  use ignis_type::attribute::{DirectiveCapability, DirectiveEffect, DirectivePhase, DirectiveTarget};
  use ignis_type::definition::{DirectiveDefinition, DirectiveProvenance};
  use ignis_type::file::FileId;
  use ignis_type::symbol::SymbolId;
  use ignis_type::BytePosition;

  use super::*;

  fn span(
    start: u32,
    end: u32,
  ) -> Span {
    Span::new(FileId::SYNTHETIC, BytePosition(start), BytePosition(end))
  }

  fn directive_definition(
    function_index: u32,
    name_index: u32,
    phase: DirectivePhase,
    effect: DirectiveEffect,
  ) -> DirectiveDefinition {
    DirectiveDefinition {
      id: DirectiveDefId::default(),
      function_def_id: DefinitionId::new(function_index),
      name: SymbolId::new(name_index),
      target: DirectiveTarget::Record,
      phase,
      effect,
      group: None,
      capabilities: vec![DirectiveCapability::Diagnostics],
      provenance: DirectiveProvenance {
        origin_attr_span: span(function_index * 10, function_index * 10 + 4),
      },
    }
  }

  fn directive_use(
    target_index: u32,
    directive: DirectiveDefId,
    start: u32,
  ) -> crate::directive_registry::DirectiveUse {
    crate::directive_registry::DirectiveUse {
      target_node: NodeId::new(target_index),
      directive,
      span: span(start, start + 4),
      provenance: DirectiveProvenance {
        origin_attr_span: span(start, start + 4),
      },
    }
  }

  fn all_phase_plan() -> DirectiveSchedulePlan {
    let mut registry = DirectiveRegistry::default();

    let check =
      registry.register_definition(directive_definition(1, 1, DirectivePhase::Check, DirectiveEffect::Diagnose));
    let expand =
      registry.register_definition(directive_definition(2, 2, DirectivePhase::Expand, DirectiveEffect::Emit));
    let collect =
      registry.register_definition(directive_definition(3, 3, DirectivePhase::Collect, DirectiveEffect::Collect));
    let finalize =
      registry.register_definition(directive_definition(4, 4, DirectivePhase::Finalize, DirectiveEffect::Collect));
    let transform = registry.register_definition(directive_definition(
      5,
      5,
      DirectivePhase::Transform,
      DirectiveEffect::Transform,
    ));

    registry.uses = vec![
      directive_use(10, transform, 50),
      directive_use(11, check, 10),
      directive_use(12, finalize, 40),
      directive_use(13, expand, 20),
      directive_use(14, collect, 30),
    ];

    DirectiveSchedulePlan::from_registry(&registry)
  }

  #[test]
  fn execution_state_consumes_all_phases_in_deterministic_order() {
    let plan = all_phase_plan();
    let mut state = DirectiveExecutionState::new(plan, 2);

    let mut seen_phases = Vec::new();
    let mut seen_targets = Vec::new();

    while let Some(stage) = state.current_stage() {
      seen_phases.push(stage.phase.clone());
      seen_targets.push(stage.entries.iter().map(|entry| entry.target_node).collect::<Vec<_>>());

      let result = DirectiveStageResult::new(
        stage.phase.clone(),
        vec![DirectiveFingerprint::opaque(format!("{:?}-stable", stage.phase))],
        false,
      );

      let progress = state.consume_current_stage(result).expect("stage should be consumable");
      assert_ne!(progress, DirectiveExecutionProgress::StartedNextIteration { iteration: 1 });
    }

    assert_eq!(
      seen_phases,
      vec![
        DirectivePhase::Check,
        DirectivePhase::Expand,
        DirectivePhase::Collect,
        DirectivePhase::Finalize,
        DirectivePhase::Transform,
      ]
    );
    assert_eq!(
      seen_targets,
      vec![
        vec![NodeId::new(11)],
        vec![NodeId::new(13)],
        vec![NodeId::new(14)],
        vec![NodeId::new(12)],
        vec![NodeId::new(10)],
      ]
    );
    assert!(state.is_converged(), "consuming all stages without reanalysis should converge");
  }

  #[test]
  fn execution_state_restarts_at_check_when_reanalysis_fingerprint_changes_then_converges() {
    let plan = all_phase_plan();
    let mut state = DirectiveExecutionState::new(plan, 3);

    for phase in [
      DirectivePhase::Check,
      DirectivePhase::Expand,
      DirectivePhase::Collect,
      DirectivePhase::Finalize,
      DirectivePhase::Transform,
    ] {
      let progress = state
        .consume_current_stage(DirectiveStageResult::new(
          phase.clone(),
          vec![DirectiveFingerprint::opaque(format!("round-1-{phase:?}"))],
          true,
        ))
        .expect("first round should advance");

      if phase == DirectivePhase::Transform {
        assert_eq!(progress, DirectiveExecutionProgress::StartedNextIteration { iteration: 1 });
      }
    }

    assert_eq!(state.iteration(), 1);
    assert_eq!(
      state.current_stage().map(|stage| stage.phase.clone()),
      Some(DirectivePhase::Check)
    );

    for phase in [
      DirectivePhase::Check,
      DirectivePhase::Expand,
      DirectivePhase::Collect,
      DirectivePhase::Finalize,
      DirectivePhase::Transform,
    ] {
      state
        .consume_current_stage(DirectiveStageResult::new(
          phase.clone(),
          vec![DirectiveFingerprint::opaque(format!("round-1-{phase:?}"))],
          true,
        ))
        .expect("second round should converge on repeated fingerprint");
    }

    assert!(
      state.is_converged(),
      "repeating the same reanalysis fingerprint should reach a fixed point"
    );
    assert_eq!(state.completed_iterations().len(), 2);
    assert_eq!(
      state.completed_iterations()[0].fingerprint,
      state.completed_iterations()[1].fingerprint
    );
  }

  #[test]
  fn execution_state_reports_cycle_limit_failure_deterministically() {
    let plan = all_phase_plan();
    let mut state = DirectiveExecutionState::new(plan, 2);

    for phase in [
      DirectivePhase::Check,
      DirectivePhase::Expand,
      DirectivePhase::Collect,
      DirectivePhase::Finalize,
      DirectivePhase::Transform,
    ] {
      state
        .consume_current_stage(DirectiveStageResult::new(
          phase.clone(),
          vec![DirectiveFingerprint::opaque(format!("round-1-{phase:?}"))],
          true,
        ))
        .expect("first round should advance to second iteration");
    }

    for phase in [
      DirectivePhase::Check,
      DirectivePhase::Expand,
      DirectivePhase::Collect,
      DirectivePhase::Finalize,
      DirectivePhase::Transform,
    ] {
      let result = state.consume_current_stage(DirectiveStageResult::new(
        phase.clone(),
        vec![DirectiveFingerprint::opaque(format!("round-2-{phase:?}"))],
        true,
      ));

      if phase == DirectivePhase::Transform {
        let error = result.expect_err("second distinct fingerprint should exceed the cycle limit");

        assert_eq!(
          error,
          DirectiveSchedulerError::CycleLimitExceeded {
            limit: 2,
            iterations: vec![
              DirectiveIterationRecord::new(
                0,
                DirectiveIterationFingerprint::new(vec![
                  DirectiveFingerprint::opaque("round-1-Check".to_string()),
                  DirectiveFingerprint::opaque("round-1-Expand".to_string()),
                  DirectiveFingerprint::opaque("round-1-Collect".to_string()),
                  DirectiveFingerprint::opaque("round-1-Finalize".to_string()),
                  DirectiveFingerprint::opaque("round-1-Transform".to_string()),
                ]),
                true,
              ),
              DirectiveIterationRecord::new(
                1,
                DirectiveIterationFingerprint::new(vec![
                  DirectiveFingerprint::opaque("round-2-Check".to_string()),
                  DirectiveFingerprint::opaque("round-2-Expand".to_string()),
                  DirectiveFingerprint::opaque("round-2-Collect".to_string()),
                  DirectiveFingerprint::opaque("round-2-Finalize".to_string()),
                  DirectiveFingerprint::opaque("round-2-Transform".to_string()),
                ]),
                true,
              ),
            ],
          }
        );
      } else {
        result.expect("pre-transform stages should still advance before cycle detection");
      }
    }
  }

  #[derive(Default)]
  struct FakeStageExecutor {
    results: VecDeque<DirectiveStageResult>,
  }

  impl FakeStageExecutor {
    fn with_results(results: Vec<DirectiveStageResult>) -> Self {
      Self {
        results: results.into(),
      }
    }
  }

  impl DirectiveStageExecutor for FakeStageExecutor {
    fn execute_stage(
      &mut self,
      _stage: &DirectiveScheduleStage,
    ) -> DirectiveStageResult {
      self.results.pop_front().expect("expected another staged result")
    }
  }

  #[derive(Debug, Default, PartialEq, Eq)]
  struct RecordingReanalysisHook {
    requests: Vec<DirectiveReanalysisRequest>,
  }

  impl DirectiveReanalysisHook for RecordingReanalysisHook {
    fn request_reanalysis(
      &mut self,
      request: DirectiveReanalysisRequest,
    ) {
      self.requests.push(request);
    }
  }

  #[test]
  fn scheduler_requests_reanalysis_when_fingerprint_changes() {
    let plan = all_phase_plan();
    let mut scheduler = DirectiveScheduler::new(3);
    let mut executor = FakeStageExecutor::with_results(vec![
      DirectiveStageResult::new(DirectivePhase::Check, vec![DirectiveFingerprint::opaque("round-1-check")], true),
      DirectiveStageResult::new(
        DirectivePhase::Expand,
        vec![DirectiveFingerprint::opaque("round-1-expand")],
        true,
      ),
      DirectiveStageResult::new(
        DirectivePhase::Collect,
        vec![DirectiveFingerprint::opaque("round-1-collect")],
        true,
      ),
      DirectiveStageResult::new(
        DirectivePhase::Finalize,
        vec![DirectiveFingerprint::opaque("round-1-finalize")],
        true,
      ),
      DirectiveStageResult::new(
        DirectivePhase::Transform,
        vec![DirectiveFingerprint::opaque("round-1-transform")],
        true,
      ),
      DirectiveStageResult::new(DirectivePhase::Check, vec![DirectiveFingerprint::opaque("round-1-check")], true),
      DirectiveStageResult::new(
        DirectivePhase::Expand,
        vec![DirectiveFingerprint::opaque("round-1-expand")],
        true,
      ),
      DirectiveStageResult::new(
        DirectivePhase::Collect,
        vec![DirectiveFingerprint::opaque("round-1-collect")],
        true,
      ),
      DirectiveStageResult::new(
        DirectivePhase::Finalize,
        vec![DirectiveFingerprint::opaque("round-1-finalize")],
        true,
      ),
      DirectiveStageResult::new(
        DirectivePhase::Transform,
        vec![DirectiveFingerprint::opaque("round-1-transform")],
        true,
      ),
    ]);
    let mut hook = RecordingReanalysisHook::default();

    let report = scheduler.run(plan, &mut executor, &mut hook);

    assert!(report.failure.is_none(), "expected scheduler to converge: {:?}", report.failure);
    assert_eq!(
      hook.requests,
      vec![DirectiveReanalysisRequest {
        iteration: 0,
        fingerprint: DirectiveIterationFingerprint::new(vec![
          DirectiveFingerprint::opaque("round-1-check"),
          DirectiveFingerprint::opaque("round-1-expand"),
          DirectiveFingerprint::opaque("round-1-collect"),
          DirectiveFingerprint::opaque("round-1-finalize"),
          DirectiveFingerprint::opaque("round-1-transform"),
        ]),
      }]
    );
    assert_eq!(report.completed_iterations.len(), 2);
  }

  #[test]
  fn scheduler_reports_cycle_limit_failures_as_diagnostics() {
    let plan = all_phase_plan();
    let mut scheduler = DirectiveScheduler::new(2);
    let mut executor = FakeStageExecutor::with_results(vec![
      DirectiveStageResult::new(DirectivePhase::Check, vec![DirectiveFingerprint::opaque("round-1-check")], true),
      DirectiveStageResult::new(
        DirectivePhase::Expand,
        vec![DirectiveFingerprint::opaque("round-1-expand")],
        true,
      ),
      DirectiveStageResult::new(
        DirectivePhase::Collect,
        vec![DirectiveFingerprint::opaque("round-1-collect")],
        true,
      ),
      DirectiveStageResult::new(
        DirectivePhase::Finalize,
        vec![DirectiveFingerprint::opaque("round-1-finalize")],
        true,
      ),
      DirectiveStageResult::new(
        DirectivePhase::Transform,
        vec![DirectiveFingerprint::opaque("round-1-transform")],
        true,
      ),
      DirectiveStageResult::new(DirectivePhase::Check, vec![DirectiveFingerprint::opaque("round-2-check")], true),
      DirectiveStageResult::new(
        DirectivePhase::Expand,
        vec![DirectiveFingerprint::opaque("round-2-expand")],
        true,
      ),
      DirectiveStageResult::new(
        DirectivePhase::Collect,
        vec![DirectiveFingerprint::opaque("round-2-collect")],
        true,
      ),
      DirectiveStageResult::new(
        DirectivePhase::Finalize,
        vec![DirectiveFingerprint::opaque("round-2-finalize")],
        true,
      ),
      DirectiveStageResult::new(
        DirectivePhase::Transform,
        vec![DirectiveFingerprint::opaque("round-2-transform")],
        true,
      ),
    ]);
    let mut hook = RecordingReanalysisHook::default();

    let report = scheduler.run(plan, &mut executor, &mut hook);
    let diagnostic = report
      .failure
      .as_ref()
      .and_then(DirectiveSchedulerFailure::as_diagnostic)
      .expect("expected cycle-limit diagnostic");

    assert_eq!(diagnostic.error_code, "A0195");
    assert!(
      diagnostic
        .message
        .contains("directive expansion exceeded the cycle limit of 2 iteration(s)")
    );
    assert_eq!(report.completed_iterations.len(), 2);
  }
}
