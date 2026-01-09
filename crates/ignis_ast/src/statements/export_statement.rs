use ignis_type::{span::Span, symbol::SymbolId};

use crate::NodeId;

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum ASTExport {
  /// export <declaration>
  Declaration { decl: NodeId, span: Span },
  /// export foo;
  Name { name: SymbolId, span: Span },
}

impl ASTExport {
  pub fn declaration(
    decl: NodeId,
    span: Span,
  ) -> Self {
    Self::Declaration { decl, span }
  }

  pub fn name(
    name: SymbolId,
    span: Span,
  ) -> Self {
    Self::Name { name, span }
  }

  pub fn span(&self) -> &Span {
    match self {
      ASTExport::Declaration { span, .. } => span,
      ASTExport::Name { span, .. } => span,
    }
  }
}
