use ignis_type::{span::Span, symbol::SymbolId};

use crate::NodeId;

/// Access operator for member access expressions
#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq)]
pub enum ASTAccessOp {
  /// `.` for instance access (fields, methods)
  Dot,
  /// `::` for static/associated access
  DoubleColon,
}

/// Member access expression: `object.member` or `Type::member`
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ASTMemberAccess {
  pub object: NodeId,
  pub op: ASTAccessOp,
  pub member: SymbolId,
  pub member_span: Span,
  pub span: Span,
}

impl ASTMemberAccess {
  pub fn new(
    object: NodeId,
    op: ASTAccessOp,
    member: SymbolId,
    member_span: Span,
    span: Span,
  ) -> Self {
    Self {
      object,
      op,
      member,
      member_span,
      span,
    }
  }
}
