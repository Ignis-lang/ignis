use ignis_type::{span::Span, symbol::SymbolId};

use crate::{NodeId, metadata::ASTMetadata, type_::IgnisTypeSyntax};

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ASTVariable {
  pub name: SymbolId,
  pub name_span: Span,
  pub value: Option<NodeId>,
  pub type_: IgnisTypeSyntax,
  pub span: Span,
  pub metadata: ASTMetadata,
  pub doc: Option<String>,
}

impl ASTVariable {
  pub fn new(
    name: SymbolId,
    name_span: Span,
    value: Option<NodeId>,
    type_: IgnisTypeSyntax,
    span: Span,
    metadata: ASTMetadata,
    doc: Option<String>,
  ) -> Self {
    Self {
      name,
      name_span,
      value,
      type_,
      span,
      metadata,
      doc,
    }
  }
}
