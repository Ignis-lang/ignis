use ignis_ast::NodeId;
use ignis_type::attribute::{DirectiveEffect, DirectivePhase};
use ignis_type::definition::{DefinitionId, DirectiveDefId, DirectiveProvenance};
use ignis_type::span::Span;

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

fn phase_order() -> [DirectivePhase; 5] {
  [
    DirectivePhase::Check,
    DirectivePhase::Expand,
    DirectivePhase::Collect,
    DirectivePhase::Finalize,
    DirectivePhase::Transform,
  ]
}
