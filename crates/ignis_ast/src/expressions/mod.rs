use ignis_type::span::Span;

pub mod assignment;
pub mod binary;
pub mod call;
pub mod cast;
pub mod dereference;
pub mod grouped;
pub mod literal;
pub mod member_access;
pub mod path;
pub mod record_init;
pub mod reference;
pub mod ternary;
pub mod unary;
pub mod variable;
pub mod vector;
pub mod vector_access;

pub use assignment::ASTAssignment;
pub use binary::ASTBinary;
pub use call::ASTCallExpression;
pub use cast::ASTCast;
pub use dereference::ASTDereference;
pub use grouped::ASTGrouped;
pub use literal::ASTLiteral;
pub use member_access::{ASTAccessOp, ASTMemberAccess};
pub use path::ASTPath;
pub use record_init::{ASTRecordInit, ASTRecordInitField};
pub use reference::ASTReference;
pub use ternary::ASTTernary;
pub use unary::ASTUnary;
pub use variable::ASTVariableExpression;
pub use vector::ASTVector;
pub use vector_access::ASTVectorAccess;

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum ASTExpression {
  Assignment(ASTAssignment),
  Binary(ASTBinary),
  Ternary(ASTTernary),
  Cast(ASTCast),
  Call(ASTCallExpression),
  Dereference(ASTDereference),
  Grouped(ASTGrouped),
  Reference(ASTReference),
  Unary(ASTUnary),
  Literal(ASTLiteral),
  Variable(ASTVariableExpression),
  Vector(ASTVector),
  VectorAccess(ASTVectorAccess),
  Path(ASTPath),
  PostfixIncrement { expr: crate::NodeId, span: Span },
  PostfixDecrement { expr: crate::NodeId, span: Span },
  MemberAccess(ASTMemberAccess),
  RecordInit(ASTRecordInit),
}

impl ASTExpression {
  pub fn span(&self) -> &Span {
    match self {
      ASTExpression::Assignment(expr) => &expr.span,
      ASTExpression::Binary(expr) => &expr.span,
      ASTExpression::Ternary(expr) => &expr.span,
      ASTExpression::Cast(expr) => &expr.span,
      ASTExpression::Call(expr) => &expr.span,
      ASTExpression::Dereference(expr) => &expr.span,
      ASTExpression::Grouped(expr) => &expr.span,
      ASTExpression::Reference(expr) => &expr.span,
      ASTExpression::Unary(expr) => &expr.span,
      ASTExpression::Literal(expr) => &expr.span,
      ASTExpression::Variable(expr) => &expr.span,
      ASTExpression::Vector(expr) => &expr.span,
      ASTExpression::VectorAccess(expr) => &expr.span,
      ASTExpression::Path(expr) => &expr.span,
      ASTExpression::PostfixIncrement { span, .. } => span,
      ASTExpression::PostfixDecrement { span, .. } => span,
      ASTExpression::MemberAccess(expr) => &expr.span,
      ASTExpression::RecordInit(expr) => &expr.span,
    }
  }
}
