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
  use ignis_type::BytePosition;
  use ignis_type::definition::GeneratedProvenance;
  use ignis_type::file::FileId;

  fn span(
    start: u32,
    end: u32,
  ) -> Span {
    Span::new(FileId::SYNTHETIC, BytePosition(start), BytePosition(end))
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
}
