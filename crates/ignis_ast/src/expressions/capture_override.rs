use ignis_type::span::Span;

use crate::NodeId;

/// Capture mode override for closure captures: `@move x`, `@ref x`, `@refMut x`.
#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq)]
pub enum CaptureOverrideKind {
  Move,
  Ref,
  RefMut,
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ASTCaptureOverride {
  pub kind: CaptureOverrideKind,
  pub inner: NodeId,
  pub span: Span,
}

impl ASTCaptureOverride {
  pub fn new(
    kind: CaptureOverrideKind,
    inner: NodeId,
    span: Span,
  ) -> Self {
    Self { kind, inner, span }
  }
}
