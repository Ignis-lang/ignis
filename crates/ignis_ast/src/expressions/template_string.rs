use ignis_type::span::Span;

use crate::NodeId;

/// Template literal: `` `a${x}b` ``.
///
/// The node keeps the surface form so the formatter can print the literal back
/// as written, while every phase after parsing works on `desugared`, which is
/// the equivalent `String::create(..).concat(..)` chain.
///
/// `expressions` holds the same `NodeId`s that already appear inside
/// `desugared`. It is an index for the formatter, not a second set of children:
/// a traversal that walks both would visit each interpolated expression twice.
#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct ASTTemplateString {
  /// Literal chunks in source order. Always `expressions.len() + 1` entries.
  pub quasis: Vec<String>,
  pub expressions: Vec<NodeId>,
  pub desugared: NodeId,
  pub span: Span,
}

impl ASTTemplateString {
  pub fn new(
    quasis: Vec<String>,
    expressions: Vec<NodeId>,
    desugared: NodeId,
    span: Span,
  ) -> Self {
    Self {
      quasis,
      expressions,
      desugared,
      span,
    }
  }
}
