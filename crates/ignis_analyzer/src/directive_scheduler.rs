use ignis_ast::NodeId;
use ignis_diagnostics::diagnostic_report::{Diagnostic, Severity};
use ignis_type::attribute::{DirectiveCapability, DirectiveEffect, DirectivePhase};
use ignis_type::definition::{DefinitionId, DirectiveDefId, DirectiveProvenance};
use ignis_type::file::FileId;
use ignis_type::span::Span;
use ignis_type::BytePosition;

use crate::directive_registry::DirectiveRegistry;
use crate::directive_vm::DirectiveVm;

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
  pub capabilities: Vec<DirectiveCapability>,
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

#[derive(Debug, Clone)]
pub struct DirectiveStageResult {
  pub phase: DirectivePhase,
  pub diagnostics: Vec<Diagnostic>,
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
      diagnostics: Vec::new(),
      fingerprints,
      requested_reanalysis,
    }
  }

  pub fn with_diagnostics(
    mut self,
    diagnostics: Vec<Diagnostic>,
  ) -> Self {
    self.diagnostics = diagnostics;
    self
  }
}

impl PartialEq for DirectiveStageResult {
  fn eq(
    &self,
    other: &Self,
  ) -> bool {
    self.phase == other.phase
      && self.requested_reanalysis == other.requested_reanalysis
      && self.fingerprints == other.fingerprints
      && diagnostics_signature(&self.diagnostics) == diagnostics_signature(&other.diagnostics)
  }
}

impl Eq for DirectiveStageResult {}

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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DirectiveExecutionSandbox {
  granted_capabilities: Vec<DirectiveCapability>,
}

impl DirectiveExecutionSandbox {
  pub fn allowing(capabilities: impl IntoIterator<Item = DirectiveCapability>) -> Self {
    let mut granted_capabilities = Vec::new();

    for capability in capabilities {
      if !granted_capabilities.contains(&capability) {
        granted_capabilities.push(capability);
      }
    }

    Self { granted_capabilities }
  }

  fn allows(
    &self,
    capability: &DirectiveCapability,
  ) -> bool {
    self.granted_capabilities.contains(capability)
  }
}

impl Default for DirectiveExecutionSandbox {
  fn default() -> Self {
    Self::allowing([DirectiveCapability::Diagnostics])
  }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DirectiveExecutionError {
  CapabilityDenied {
    phase: DirectivePhase,
    capability: DirectiveCapability,
    provenance: DirectiveProvenance,
  },
  UnsupportedGeneration {
    operation: String,
    span: Span,
    provenance: DirectiveProvenance,
  },
  StepLimitExceeded {
    span: Span,
    provenance: DirectiveProvenance,
  },
  CallDepthExceeded {
    span: Span,
    provenance: DirectiveProvenance,
  },
}

pub trait DirectiveStageExecutor {
  fn execute_stage(
    &mut self,
    stage: &DirectiveScheduleStage,
  ) -> Result<DirectiveStageResult, DirectiveExecutionError>;
}

pub trait DirectiveReanalysisHook {
  fn request_reanalysis(
    &mut self,
    request: DirectiveReanalysisRequest,
  );
}

#[derive(Debug, Clone)]
pub struct DirectiveSchedulerFailure {
  pub reason: DirectiveSchedulerFailureReason,
  diagnostic: Option<Diagnostic>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DirectiveSchedulerFailureReason {
  Scheduler(DirectiveSchedulerError),
  Execution(DirectiveExecutionError),
}

impl DirectiveSchedulerFailure {
  fn from_scheduler_error(
    plan: &DirectiveSchedulePlan,
    error: DirectiveSchedulerError,
  ) -> Self {
    let diagnostic = match &error {
      DirectiveSchedulerError::CycleLimitExceeded { limit, iterations } => {
        Some(cycle_limit_diagnostic(plan, *limit, iterations))
      },
      DirectiveSchedulerError::NoCurrentStage | DirectiveSchedulerError::PhaseMismatch { .. } => None,
    };

    Self {
      reason: DirectiveSchedulerFailureReason::Scheduler(error),
      diagnostic,
    }
  }

  fn from_execution_error(error: DirectiveExecutionError) -> Self {
    Self {
      reason: DirectiveSchedulerFailureReason::Execution(error.clone()),
      diagnostic: Some(execution_error_diagnostic(&error)),
    }
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
  pub diagnostics: Vec<Diagnostic>,
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
    let mut diagnostics = Vec::new();

    while let Some(stage) = state.current_stage().cloned() {
      executed_phases.push(stage.phase.clone());

      let stage_result = match executor.execute_stage(&stage) {
        Ok(result) => result,
        Err(error) => {
          return DirectiveExecutionReport {
            executed_phases,
            completed_iterations: state.completed_iterations().to_vec(),
            reanalysis_requests,
            diagnostics,
            converged: false,
            failure: Some(DirectiveSchedulerFailure::from_execution_error(error)),
          };
        },
      };

      diagnostics.extend(stage_result.diagnostics.clone());

      match state.consume_current_stage(stage_result) {
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
            diagnostics,
            converged: false,
            failure: Some(DirectiveSchedulerFailure::from_scheduler_error(&plan, error)),
          };
        },
      }
    }

    DirectiveExecutionReport {
      executed_phases,
      completed_iterations: state.completed_iterations().to_vec(),
      reanalysis_requests,
      diagnostics,
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
  ) -> Result<DirectiveStageResult, DirectiveExecutionError> {
    Ok(DirectiveStageResult::new(stage.phase.clone(), Vec::new(), false))
  }
}

#[derive(Clone, Default)]
pub struct CompileTimeDirectiveExecutor {
  sandbox: DirectiveExecutionSandbox,
  vm: Option<DirectiveVm>,
}

impl CompileTimeDirectiveExecutor {
  pub fn new(sandbox: DirectiveExecutionSandbox) -> Self {
    Self { sandbox, vm: None }
  }

  pub fn with_vm(
    sandbox: DirectiveExecutionSandbox,
    vm: DirectiveVm,
  ) -> Self {
    Self { sandbox, vm: Some(vm) }
  }
}

impl DirectiveStageExecutor for CompileTimeDirectiveExecutor {
  fn execute_stage(
    &mut self,
    stage: &DirectiveScheduleStage,
  ) -> Result<DirectiveStageResult, DirectiveExecutionError> {
    let mut diagnostics = Vec::new();

    for entry in &stage.entries {
      for capability in &entry.capabilities {
        if !self.sandbox.allows(capability) {
          return Err(DirectiveExecutionError::CapabilityDenied {
            phase: stage.phase.clone(),
            capability: capability.clone(),
            provenance: entry.provenance.clone(),
          });
        }
      }

      if stage.phase == DirectivePhase::Check
        && entry.effect == DirectiveEffect::Diagnose
        && let Some(vm) = &self.vm
      {
        diagnostics.extend(vm.execute_entry(entry)?);
      }
    }

    let fingerprints = stage
      .entries
      .iter()
      .map(|entry| deterministic_entry_fingerprint(entry, diagnostics.as_slice()))
      .collect::<Vec<_>>();
    let requested_reanalysis = stage
      .entries
      .iter()
      .any(|entry| effect_requests_reanalysis(&entry.effect));

    Ok(DirectiveStageResult::new(stage.phase.clone(), fingerprints, requested_reanalysis).with_diagnostics(diagnostics))
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
          capabilities: directive_def.capabilities.clone(),
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

fn execution_error_diagnostic(error: &DirectiveExecutionError) -> Diagnostic {
  match error {
    DirectiveExecutionError::CapabilityDenied {
      phase,
      capability,
      provenance,
    } => Diagnostic::new(
      Severity::Error,
      format!(
        "compile-time directive cannot use '{}' capability during {:?} because the sandbox denies it by default",
        capability_name(capability),
        phase
      ),
      "A0196".to_string(),
      provenance.origin_attr_span.clone(),
    )
    .with_note("allowed-by-default capabilities are limited to deterministic diagnostics".to_string()),
    DirectiveExecutionError::UnsupportedGeneration {
      operation,
      span,
      provenance,
    } => Diagnostic::new(
      Severity::Error,
      format!(
        "compile-time directive cannot call unsupported std::compile generation API '{}' in the diagnostics-only VM",
        operation
      ),
      "A0199".to_string(),
      span.clone(),
    )
    .with_label(provenance.origin_attr_span.clone(), "directive use".to_string())
    .with_note("this VM slice supports diagnostics only; generation and reintegration stay out of scope".to_string()),
    DirectiveExecutionError::StepLimitExceeded { span, provenance } => Diagnostic::new(
      Severity::Error,
      "compile-time directive exceeded the VM step limit during analyzer execution".to_string(),
      "A0200".to_string(),
      span.clone(),
    )
    .with_label(provenance.origin_attr_span.clone(), "directive use".to_string()),
    DirectiveExecutionError::CallDepthExceeded { span, provenance } => Diagnostic::new(
      Severity::Error,
      "compile-time directive exceeded the VM call-depth limit during analyzer execution".to_string(),
      "A0201".to_string(),
      span.clone(),
    )
    .with_label(provenance.origin_attr_span.clone(), "directive use".to_string()),
  }
}

fn diagnostics_signature(diagnostics: &[Diagnostic]) -> Vec<String> {
  diagnostics
    .iter()
    .map(|diagnostic| {
      let labels = diagnostic
        .labels
        .iter()
        .map(|label| format!("{}:{}:{}", label.span.start, label.span.end, label.message))
        .collect::<Vec<_>>()
        .join("|");
      let notes = diagnostic.notes.join("|");

      format!(
        "{:?}:{}:{}:{}:{}:{}:{}",
        diagnostic.severity,
        diagnostic.error_code,
        diagnostic.message,
        diagnostic.primary_span.start,
        diagnostic.primary_span.end,
        labels,
        notes
      )
    })
    .collect()
}

pub(crate) fn deterministic_entry_fingerprint(
  entry: &DirectiveScheduleEntry,
  diagnostics: &[Diagnostic],
) -> DirectiveFingerprint {
  let capabilities = if entry.capabilities.is_empty() {
    "none".to_string()
  } else {
    entry
      .capabilities
      .iter()
      .map(capability_name)
      .collect::<Vec<_>>()
      .join(",")
  };

  let diagnostic_summary = if diagnostics.is_empty() {
    "none".to_string()
  } else {
    diagnostics
      .iter()
      .map(|diagnostic| {
        format!(
          "{}:{}:{}",
          diagnostic.error_code, diagnostic.primary_span.start, diagnostic.message
        )
      })
      .collect::<Vec<_>>()
      .join("|")
  };

  DirectiveFingerprint::opaque(format!(
    "directive={};function={};target={};effect={:?};capabilities={capabilities};diagnostics={diagnostic_summary}",
    entry.directive.index(),
    entry.function_def_id.index(),
    entry.target_node.index(),
    entry.effect,
  ))
}

pub(crate) fn effect_requests_reanalysis(effect: &DirectiveEffect) -> bool {
  matches!(
    effect,
    DirectiveEffect::Emit | DirectiveEffect::Collect | DirectiveEffect::Transform
  )
}

fn capability_name(capability: &DirectiveCapability) -> &'static str {
  match capability {
    DirectiveCapability::Diagnostics => "diagnostics",
    DirectiveCapability::FileSystem => "filesystem",
    DirectiveCapability::Network => "network",
    DirectiveCapability::Process => "process",
    DirectiveCapability::Ffi => "ffi",
    DirectiveCapability::Clock => "clock",
  }
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

  fn directive_definition_with_capability(
    function_index: u32,
    name_index: u32,
    phase: DirectivePhase,
    effect: DirectiveEffect,
    capability: DirectiveCapability,
  ) -> DirectiveDefinition {
    let mut definition = directive_definition(function_index, name_index, phase, effect);
    definition.capabilities = vec![capability];
    definition
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
    ) -> Result<DirectiveStageResult, DirectiveExecutionError> {
      Ok(self.results.pop_front().expect("expected another staged result"))
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

  #[test]
  fn compile_time_executor_denies_file_capability_by_default() {
    let mut registry = DirectiveRegistry::default();
    let check = registry.register_definition(directive_definition_with_capability(
      1,
      1,
      DirectivePhase::Check,
      DirectiveEffect::Diagnose,
      DirectiveCapability::FileSystem,
    ));
    registry.uses = vec![directive_use(10, check, 10)];

    let plan = DirectiveSchedulePlan::from_registry(&registry);
    let mut scheduler = DirectiveScheduler::new(2);
    let mut executor = CompileTimeDirectiveExecutor::default();
    let mut hook = RecordingReanalysisHook::default();

    let report = scheduler.run(plan, &mut executor, &mut hook);
    let diagnostic = report
      .failure
      .as_ref()
      .and_then(DirectiveSchedulerFailure::as_diagnostic)
      .expect("expected denied capability diagnostic");

    assert_eq!(diagnostic.error_code, "A0196");
    assert!(diagnostic.message.contains("filesystem"));
    assert!(report.completed_iterations.is_empty());
    assert!(hook.requests.is_empty());
  }

  #[test]
  fn compile_time_executor_allows_supplied_capability() {
    let mut registry = DirectiveRegistry::default();
    let check = registry.register_definition(directive_definition_with_capability(
      1,
      1,
      DirectivePhase::Check,
      DirectiveEffect::Diagnose,
      DirectiveCapability::FileSystem,
    ));
    registry.uses = vec![directive_use(10, check, 10)];

    let mut plan = DirectiveSchedulePlan::from_registry(&registry);
    let stage = plan.stages.remove(0);
    let mut executor = CompileTimeDirectiveExecutor::new(DirectiveExecutionSandbox::allowing([
      DirectiveCapability::Diagnostics,
      DirectiveCapability::FileSystem,
    ]));

    let first = executor.execute_stage(&stage).expect("allowed capability should pass");
    let second = executor
      .execute_stage(&stage)
      .expect("execution should stay deterministic");

    assert_eq!(first, second);
    assert_eq!(first.phase, DirectivePhase::Check);
    assert!(!first.requested_reanalysis);
    assert_eq!(first.fingerprints.len(), 1);
  }

  #[test]
  fn compile_time_executor_drives_deterministic_scheduler_reanalysis() {
    let mut registry = DirectiveRegistry::default();
    let expand =
      registry.register_definition(directive_definition(2, 2, DirectivePhase::Expand, DirectiveEffect::Emit));
    registry.uses = vec![directive_use(13, expand, 20)];

    let plan = DirectiveSchedulePlan::from_registry(&registry);
    let mut scheduler = DirectiveScheduler::new(3);
    let mut executor = CompileTimeDirectiveExecutor::default();
    let mut hook = RecordingReanalysisHook::default();

    let report = scheduler.run(plan, &mut executor, &mut hook);

    assert!(
      report.failure.is_none(),
      "expected deterministic convergence: {:?}",
      report.failure
    );
    assert_eq!(report.executed_phases, vec![DirectivePhase::Expand, DirectivePhase::Expand]);
    assert_eq!(report.completed_iterations.len(), 2);
    assert_eq!(report.reanalysis_requests.len(), 1);
    assert_eq!(hook.requests, report.reanalysis_requests);
    assert_eq!(
      report.completed_iterations[0].fingerprint,
      report.completed_iterations[1].fingerprint
    );
  }
}
