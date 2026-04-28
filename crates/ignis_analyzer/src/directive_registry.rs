use std::collections::HashMap;

use ignis_ast::NodeId;
use ignis_type::definition::{DefinitionId, DirectiveDefId, DirectiveDefinition, DirectiveProvenance};
use ignis_type::span::Span;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DirectiveUse {
  pub target_node: NodeId,
  pub directive: DirectiveDefId,
  pub span: Span,
  pub provenance: DirectiveProvenance,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct DirectiveRegistry {
  pub defs: Vec<DirectiveDefinition>,
  pub uses: Vec<DirectiveUse>,
  pub groups: HashMap<String, Vec<DirectiveDefId>>,
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
}
