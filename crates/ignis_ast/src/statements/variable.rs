use ignis_type::symbol::SymbolId;

use crate::{NodeId, metadata::ASTMetadata, type_::IgnisTypeSyntax};

#[derive(Debug, Clone, PartialEq)]
pub struct ASTVariable {
  pub name: SymbolId,
  pub value: Option<NodeId>,
  pub type_: IgnisTypeSyntax,
  pub metadata: ASTMetadata,
}

impl ASTVariable {
  pub fn new(
    name: SymbolId,
    value: Option<NodeId>,
    type_: IgnisTypeSyntax,
    metadata: ASTMetadata,
  ) -> Self {
    Self {
      name,
      value,
      type_,
      metadata,
    }
  }
}
