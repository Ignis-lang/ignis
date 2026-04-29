use std::collections::HashMap;

use ignis_ast::NodeId;
use ignis_type::definition::{
  DefinitionId, DirectiveDefId, DirectiveDefinition, DirectiveProvenance, GeneratedItemKind, GeneratedItemMetadata,
  GeneratedProvenance,
};
use ignis_type::span::Span;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DirectiveUse {
  pub target_node: NodeId,
  pub directive: DirectiveDefId,
  pub span: Span,
  pub provenance: DirectiveProvenance,
}

impl DirectiveUse {
  pub fn generated_provenance(
    &self,
    generation_id: u32,
  ) -> GeneratedProvenance {
    GeneratedProvenance {
      origin_attr_span: self.provenance.origin_attr_span.clone(),
      directive: self.directive,
      generation_id,
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct DirectiveRegistry {
  pub defs: Vec<DirectiveDefinition>,
  pub uses: Vec<DirectiveUse>,
  pub groups: HashMap<String, Vec<DirectiveDefId>>,
  pub generated_items: HashMap<DefinitionId, GeneratedSemanticItem>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GeneratedSemanticItem {
  pub definition: DefinitionId,
  pub metadata: GeneratedItemMetadata,
}

impl DirectiveRegistry {
  pub fn register_definition(
    &mut self,
    mut directive: DirectiveDefinition,
  ) -> DirectiveDefId {
    let id = DirectiveDefId::new(self.defs.len() as u32);

    directive.id = id;

    if let Some(group) = directive.group.clone() {
      self.groups.entry(group).or_default().push(id);
    }

    self.defs.push(directive);
    id
  }

  pub fn definition_for_function(
    &self,
    function_def_id: DefinitionId,
  ) -> Option<(DirectiveDefId, &DirectiveDefinition)> {
    self.defs.iter().enumerate().find_map(|(index, directive)| {
      (directive.function_def_id == function_def_id).then_some((DirectiveDefId::new(index as u32), directive))
    })
  }

  pub fn attach_generated_item(
    &mut self,
    definition: DefinitionId,
    metadata: GeneratedItemMetadata,
  ) {
    self
      .generated_items
      .insert(definition, GeneratedSemanticItem { definition, metadata });
  }

  pub fn generated_semantic_item(
    &self,
    definition: &DefinitionId,
  ) -> Option<&GeneratedSemanticItem> {
    self.generated_items.get(definition)
  }

  pub fn generated_item_metadata(
    &self,
    definition: &DefinitionId,
  ) -> Option<&GeneratedItemMetadata> {
    self.generated_semantic_item(definition).map(|item| &item.metadata)
  }

  pub fn generated_attached_method_items_for_owner(
    &self,
    owner_type: DefinitionId,
  ) -> Vec<GeneratedSemanticItem> {
    let mut methods = self
      .generated_items
      .values()
      .filter(|item| {
        matches!(
          item.metadata.kind,
          GeneratedItemKind::AttachedMethod {
            owner_type: metadata_owner,
            ..
          } if metadata_owner == owner_type
        )
      })
      .cloned()
      .collect::<Vec<_>>();

    methods.sort_by_key(|item| item.definition.index());
    methods
  }

  pub fn generated_implemented_trait_items_for_owner(
    &self,
    owner_type: DefinitionId,
  ) -> Vec<GeneratedSemanticItem> {
    let mut traits = self
      .generated_items
      .values()
      .filter(|item| {
        matches!(
          item.metadata.kind,
          GeneratedItemKind::ImplementedTrait {
            owner_type: metadata_owner,
            ..
          } if metadata_owner == owner_type
        )
      })
      .cloned()
      .collect::<Vec<_>>();

    traits.sort_by_key(|item| item.definition.index());
    traits
  }

  pub fn generated_attached_methods_for_owner(
    &self,
    owner_type: DefinitionId,
  ) -> Vec<(DefinitionId, GeneratedProvenance, bool)> {
    self
      .generated_attached_method_items_for_owner(owner_type)
      .into_iter()
      .filter_map(|item| match item.metadata.kind {
        GeneratedItemKind::AttachedMethod { is_static, .. } => {
          Some((item.definition, item.metadata.provenance, is_static))
        },
        _ => None,
      })
      .collect()
  }

  pub fn generated_implemented_traits_for_owner(
    &self,
    owner_type: DefinitionId,
  ) -> Vec<(DefinitionId, GeneratedProvenance)> {
    self
      .generated_implemented_trait_items_for_owner(owner_type)
      .into_iter()
      .filter_map(|item| match item.metadata.kind {
        GeneratedItemKind::ImplementedTrait { trait_def, .. } => Some((trait_def, item.metadata.provenance)),
        _ => None,
      })
      .collect()
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use ignis_ast::NodeId;
  use ignis_type::attribute::{DirectiveCapability, DirectiveEffect, DirectivePhase, DirectiveTarget};
  use ignis_type::BytePosition;
  use ignis_type::definition::{DirectiveDefinition, GeneratedProvenance};
  use ignis_type::file::FileId;
  use ignis_type::symbol::SymbolId;

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

  #[test]
  fn directive_use_can_create_generated_provenance() {
    let directive_use = DirectiveUse {
      target_node: NodeId::new(4),
      directive: DirectiveDefId::new(9),
      span: span(30, 42),
      provenance: DirectiveProvenance {
        origin_attr_span: span(10, 22),
      },
    };

    assert_eq!(
      directive_use.generated_provenance(15),
      GeneratedProvenance {
        origin_attr_span: span(10, 22),
        directive: DirectiveDefId::new(9),
        generation_id: 15,
      }
    );
  }

  #[test]
  fn registry_can_store_generated_item_metadata_by_definition() {
    let mut registry = DirectiveRegistry::default();
    let generated_definition = DefinitionId::new(12);
    let metadata = GeneratedItemMetadata::attached_method(
      GeneratedProvenance {
        origin_attr_span: span(5, 12),
        directive: DirectiveDefId::new(2),
        generation_id: 8,
      },
      DefinitionId::new(3),
      false,
    );

    registry.attach_generated_item(generated_definition, metadata.clone());

    assert_eq!(
      registry.generated_semantic_item(&generated_definition),
      Some(&GeneratedSemanticItem {
        definition: generated_definition,
        metadata: metadata.clone(),
      })
    );
    assert_eq!(registry.generated_item_metadata(&generated_definition), Some(&metadata));
  }

  #[test]
  fn directive_schedule_plan_groups_uses_by_phase_order() {
    let mut registry = DirectiveRegistry::default();

    let expand =
      registry.register_definition(directive_definition(1, 1, DirectivePhase::Expand, DirectiveEffect::Emit));
    let check =
      registry.register_definition(directive_definition(2, 2, DirectivePhase::Check, DirectiveEffect::Diagnose));
    let transform = registry.register_definition(directive_definition(
      3,
      3,
      DirectivePhase::Transform,
      DirectiveEffect::Transform,
    ));
    let collect =
      registry.register_definition(directive_definition(4, 4, DirectivePhase::Collect, DirectiveEffect::Collect));
    let finalize =
      registry.register_definition(directive_definition(5, 5, DirectivePhase::Finalize, DirectiveEffect::Collect));

    registry.uses = vec![
      DirectiveUse {
        target_node: NodeId::new(10),
        directive: transform,
        span: span(50, 55),
        provenance: DirectiveProvenance {
          origin_attr_span: span(50, 55),
        },
      },
      DirectiveUse {
        target_node: NodeId::new(11),
        directive: check,
        span: span(10, 15),
        provenance: DirectiveProvenance {
          origin_attr_span: span(10, 15),
        },
      },
      DirectiveUse {
        target_node: NodeId::new(12),
        directive: finalize,
        span: span(40, 45),
        provenance: DirectiveProvenance {
          origin_attr_span: span(40, 45),
        },
      },
      DirectiveUse {
        target_node: NodeId::new(13),
        directive: expand,
        span: span(20, 25),
        provenance: DirectiveProvenance {
          origin_attr_span: span(20, 25),
        },
      },
      DirectiveUse {
        target_node: NodeId::new(14),
        directive: collect,
        span: span(30, 35),
        provenance: DirectiveProvenance {
          origin_attr_span: span(30, 35),
        },
      },
    ];

    let plan = crate::directive_scheduler::DirectiveSchedulePlan::from_registry(&registry);

    assert_eq!(
      plan.stages.iter().map(|stage| stage.phase.clone()).collect::<Vec<_>>(),
      vec![
        DirectivePhase::Check,
        DirectivePhase::Expand,
        DirectivePhase::Collect,
        DirectivePhase::Finalize,
        DirectivePhase::Transform,
      ]
    );
    assert_eq!(plan.stages[0].entries[0].directive, check);
    assert_eq!(plan.stages[1].entries[0].directive, expand);
    assert_eq!(plan.stages[2].entries[0].directive, collect);
    assert_eq!(plan.stages[3].entries[0].directive, finalize);
    assert_eq!(plan.stages[4].entries[0].directive, transform);
  }

  #[test]
  fn directive_schedule_plan_preserves_deterministic_source_order_and_execution_fields() {
    let mut registry = DirectiveRegistry::default();

    let first_expand =
      registry.register_definition(directive_definition(1, 1, DirectivePhase::Expand, DirectiveEffect::Emit));
    let second_expand =
      registry.register_definition(directive_definition(2, 2, DirectivePhase::Expand, DirectiveEffect::Emit));

    registry.uses = vec![
      DirectiveUse {
        target_node: NodeId::new(20),
        directive: second_expand,
        span: span(90, 95),
        provenance: DirectiveProvenance {
          origin_attr_span: span(90, 95),
        },
      },
      DirectiveUse {
        target_node: NodeId::new(21),
        directive: first_expand,
        span: span(10, 15),
        provenance: DirectiveProvenance {
          origin_attr_span: span(10, 15),
        },
      },
      DirectiveUse {
        target_node: NodeId::new(22),
        directive: second_expand,
        span: span(90, 95),
        provenance: DirectiveProvenance {
          origin_attr_span: span(90, 95),
        },
      },
    ];

    let plan = crate::directive_scheduler::DirectiveSchedulePlan::from_registry(&registry);
    let expand_stage = &plan.stages[0];

    assert_eq!(expand_stage.phase, DirectivePhase::Expand);
    assert_eq!(
      expand_stage
        .entries
        .iter()
        .map(|entry| entry.target_node)
        .collect::<Vec<_>>(),
      vec![NodeId::new(21), NodeId::new(20), NodeId::new(22)]
    );
    assert_eq!(
      expand_stage
        .entries
        .iter()
        .map(|entry| entry.source_order)
        .collect::<Vec<_>>(),
      vec![1, 0, 2]
    );
    assert_eq!(expand_stage.entries[0].effect, DirectiveEffect::Emit);
    assert_eq!(expand_stage.entries[0].function_def_id, DefinitionId::new(1));
    assert_eq!(expand_stage.entries[0].provenance.origin_attr_span, span(10, 15));
  }
}
