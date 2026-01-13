use ignis_type::{span::Span, symbol::SymbolId};

use crate::{NodeId, type_::IgnisTypeSyntax};

/// Represents a for-of loop statement: `for (let x of v) { ... }`
///
/// Supports iteration over Vector types with optional type annotations:
/// - `for (let x of v) { ... }` - by value (requires Copy element)
/// - `for (let x: &T of v) { ... }` - by immutable reference
/// - `for (let x: &mut T of v) { ... }` - by mutable reference (requires mutable vector)
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ASTForOf {
  pub binding: ForOfBinding,
  pub iter: NodeId,
  pub body: NodeId,
  pub span: Span,
}

/// The binding for a for-of loop variable
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ForOfBinding {
  pub name: SymbolId,
  pub type_annotation: Option<IgnisTypeSyntax>,
  pub span: Span,
}

impl ASTForOf {
  pub fn new(
    binding: ForOfBinding,
    iter: NodeId,
    body: NodeId,
    span: Span,
  ) -> Self {
    Self {
      binding,
      iter,
      body,
      span,
    }
  }
}

impl ForOfBinding {
  pub fn new(
    name: SymbolId,
    type_annotation: Option<IgnisTypeSyntax>,
    span: Span,
  ) -> Self {
    Self {
      name,
      type_annotation,
      span,
    }
  }
}
