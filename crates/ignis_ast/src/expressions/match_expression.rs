use ignis_type::span::Span;

use crate::{NodeId, pattern::ASTPattern};

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ASTMatch {
  pub scrutinee: NodeId,
  pub arms: Vec<ASTMatchArm>,
  pub span: Span,
}

impl ASTMatch {
  pub fn new(
    scrutinee: NodeId,
    arms: Vec<ASTMatchArm>,
    span: Span,
  ) -> Self {
    Self { scrutinee, arms, span }
  }
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ASTMatchArm {
  pub pattern: ASTPattern,
  pub guard: Option<NodeId>,
  pub body: NodeId,
  pub span: Span,
}

impl ASTMatchArm {
  pub fn new(
    pattern: ASTPattern,
    guard: Option<NodeId>,
    body: NodeId,
    span: Span,
  ) -> Self {
    Self {
      pattern,
      guard,
      body,
      span,
    }
  }
}
