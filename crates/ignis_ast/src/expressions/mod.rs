use ignis_type::span::Span;

use crate::expressions::{
  assignment::ASTAssignment, binary::ASTBinary, call::ASTFunctionCall, cast::ASTCast, grouping::ASTGrouping,
  literal::ASTLiteral, path::ASTPath, variable::ASTVariableExpression, vector::ASTVector,
  vector_access::ASTVectorAccess,
};

pub mod assignment;
pub mod binary;
pub mod call;
pub mod cast;
pub mod grouping;
pub mod literal;
pub mod path;
pub mod variable;
pub mod vector;
pub mod vector_access;

#[derive(Debug, PartialEq, Clone)]
pub enum ASTExpression {
  Assignment(ASTAssignment),
  Binary(ASTBinary),
  Cast(ASTCast),
  FunctionCall(ASTFunctionCall),
  Grouping(ASTGrouping),
  Literal(ASTLiteral),
  Variable(ASTVariableExpression),
  Vector(ASTVector),
  VectorAccess(ASTVectorAccess),
  Path(ASTPath),
}

impl ASTExpression {
  pub fn span(&self) -> Span {
    match self {
      ASTExpression::Assignment(expr) => expr.span.clone(),
      ASTExpression::Binary(expr) => expr.span.clone(),
      ASTExpression::Cast(expr) => expr.span.clone(),
      ASTExpression::FunctionCall(expr) => expr.span.clone(),
      ASTExpression::Grouping(expr) => expr.span.clone(),
      ASTExpression::Literal(expr) => expr.span.clone(),
      ASTExpression::Variable(expr) => expr.span.clone(),
      ASTExpression::Vector(expr) => expr.span.clone(),
      ASTExpression::VectorAccess(expr) => expr.span.clone(),
      ASTExpression::Path(expr) => expr.span.clone(),
    }
  }
}
