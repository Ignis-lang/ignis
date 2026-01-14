use ignis_type::span::Span;

use crate::{NodeId, type_::IgnisTypeSyntax};

/// Call expression: `foo<i32, bool>(arg1, arg2)`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ASTCallExpression {
  pub callee: NodeId,
  /// Explicit type arguments: `foo<i32, bool>(...)` -> Some([i32, bool])
  /// Inferred type arguments: `foo(...)` -> None
  pub type_args: Option<Vec<IgnisTypeSyntax>>,
  pub span: Span,
  pub arguments: Vec<NodeId>,
}

impl ASTCallExpression {
  pub fn new(
    callee: NodeId,
    type_args: Option<Vec<IgnisTypeSyntax>>,
    span: Span,
    arguments: Vec<NodeId>,
  ) -> Self {
    ASTCallExpression {
      span,
      callee,
      type_args,
      arguments,
    }
  }
}
