use ignis_type::{span::Span, symbol::SymbolId};

use crate::{NodeId, type_::IgnisTypeSyntax};

/// Record initialization expression: `Type { field: value, ... }` or `Type<Args> { field: value, ... }`
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ASTRecordInit {
  pub path: Vec<(SymbolId, Span)>,
  /// Explicit type arguments: `Box<i32> { ... }` -> Some([i32])
  /// Non-generic: `Point { ... }` -> None
  pub type_args: Option<Vec<IgnisTypeSyntax>>,
  pub fields: Vec<ASTRecordInitField>,
  pub span: Span,
}

impl ASTRecordInit {
  pub fn new(
    path: Vec<(SymbolId, Span)>,
    type_args: Option<Vec<IgnisTypeSyntax>>,
    fields: Vec<ASTRecordInitField>,
    span: Span,
  ) -> Self {
    Self {
      path,
      type_args,
      fields,
      span,
    }
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
