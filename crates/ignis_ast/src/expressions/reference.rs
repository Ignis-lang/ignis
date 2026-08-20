use ignis_type::span::Span;

use crate::NodeId;

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ASTReference {
  pub inner: NodeId,
  pub mutable: bool,
  /// Set for the borrow a template literal inserts around an interpolated place.
  ///
  /// Reference types do not nest in Ignis: `& &T` in type position collapses to
  /// `&T`, so a slot that already holds a reference must not gain another one.
  /// The borrow is inserted before types are known, so the decision is deferred
  /// to typechecking, which drops it when the inner expression is already a
  /// reference.
  pub template_slot: bool,
  pub span: Span,
}

impl ASTReference {
  pub fn new(
    inner: NodeId,
    mutable: bool,
    span: Span,
  ) -> Self {
    ASTReference {
      inner,
      mutable,
      template_slot: false,
      span,
    }
  }

  pub fn new_template_slot(
    inner: NodeId,
    span: Span,
  ) -> Self {
    ASTReference {
      inner,
      mutable: false,
      template_slot: true,
      span,
    }
  }
}
