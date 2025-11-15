use ignis_type::span::Span;

use crate::expressions::{
  assignment::ASTAssignment, binary::ASTBinary, call::ASTCallExpression, cast::ASTCast,
  dereference::ASTDereference, grouped::ASTGrouped, literal::ASTLiteral, path::ASTPath,
  reference::ASTReference, unary::ASTUnary, variable::ASTVariableExpression, vector::ASTVector,
  vector_access::ASTVectorAccess,
};

pub mod assignment;
pub mod binary;
pub mod call;
pub mod cast;
pub mod dereference;
pub mod grouped;
pub mod literal;
pub mod path;
pub mod reference;
pub mod unary;
pub mod variable;
pub mod vector;
pub mod vector_access;

#[derive(Debug, PartialEq, Clone)]
pub enum ASTExpression {
  Assignment(ASTAssignment),
  Binary(ASTBinary),
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
}

impl ASTExpression {
  pub fn span(&self) -> &Span {
    match self {
      ASTExpression::Assignment(expr) => &expr.span,
      ASTExpression::Binary(expr) => &expr.span,
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
    }
  }
}
