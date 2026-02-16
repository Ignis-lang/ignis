use ignis_type::{span::Span, symbol::SymbolId};

use crate::NodeId;
use crate::statements::import_statement::ASTImportItem;

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum ASTExport {
  /// export <declaration>
  Declaration { decl: NodeId, span: Span },
  /// export foo;
  Name { name: SymbolId, span: Span },
  /// export X, Y from "path";
  ReExportFrom {
    items: Vec<ASTImportItem>,
    from: String,
    from_span: Span,
    span: Span,
  },
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

  pub fn re_export_from(
    items: Vec<ASTImportItem>,
    from: String,
    from_span: Span,
    span: Span,
  ) -> Self {
    Self::ReExportFrom {
      items,
      from,
      from_span,
      span,
    }
  }

  pub fn span(&self) -> &Span {
    match self {
      ASTExport::Declaration { span, .. } => span,
      ASTExport::Name { span, .. } => span,
      ASTExport::ReExportFrom { span, .. } => span,
    }
  }
}
