use ignis_type::span::Span;

use crate::{NodeId, generics::ASTGenericParams, statements::function::ASTParameter, type_::IgnisTypeSyntax};

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum LambdaBody {
  Expression(NodeId),
  Block(NodeId),
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ASTLambda {
  pub type_params: Option<ASTGenericParams>,
  pub params: Vec<ASTParameter>,
  pub return_type: IgnisTypeSyntax,
  pub body: LambdaBody,
  pub span: Span,
}

impl ASTLambda {
  pub fn new(
    type_params: Option<ASTGenericParams>,
    params: Vec<ASTParameter>,
    return_type: IgnisTypeSyntax,
    body: LambdaBody,
    span: Span,
  ) -> Self {
    Self {
      type_params,
      params,
      return_type,
      body,
      span,
    }
  }
}
