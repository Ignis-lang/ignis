use ignis_type::{span::Span, symbol::SymbolId};

use crate::{NodeId, metadata::ASTMetadata, type_::IgnisTypeSyntax};

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ASTVariable {
  pub name: SymbolId,
  pub value: Option<NodeId>,
  pub type_: IgnisTypeSyntax,
  pub span: Span,
  pub metadata: ASTMetadata,
  pub doc: Option<String>,
}

impl ASTVariable {
  pub fn new(
    name: SymbolId,
    value: Option<NodeId>,
    type_: IgnisTypeSyntax,
    span: Span,
    metadata: ASTMetadata,
    doc: Option<String>,
  ) -> Self {
    Self {
      name,
      value,
      type_,
      span,
      metadata,
      doc,
    }
  }
}
