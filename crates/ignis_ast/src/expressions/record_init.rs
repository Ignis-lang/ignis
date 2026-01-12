use ignis_type::{span::Span, symbol::SymbolId};

use crate::NodeId;

/// Record initialization expression: `Type { field: value, ... }`
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ASTRecordInit {
  pub path: Vec<(SymbolId, Span)>,
  pub fields: Vec<ASTRecordInitField>,
  pub span: Span,
}

impl ASTRecordInit {
  pub fn new(
    path: Vec<(SymbolId, Span)>,
    fields: Vec<ASTRecordInitField>,
    span: Span,
  ) -> Self {
    Self { path, fields, span }
  }
}

/// Field initialization in a record init expression
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ASTRecordInitField {
  pub name: SymbolId,
  pub value: NodeId,
  pub span: Span,
}

impl ASTRecordInitField {
  pub fn new(
    name: SymbolId,
    value: NodeId,
    span: Span,
  ) -> Self {
    Self { name, value, span }
  }
}
