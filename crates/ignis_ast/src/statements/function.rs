use ignis_type::{span::Span, symbol::SymbolId};

use crate::{generics::ASTGenericParams, metadata::ASTMetadata, NodeId, type_::IgnisTypeSyntax};

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ASTParameter {
  pub name: SymbolId,
  pub type_: IgnisTypeSyntax,
  pub span: Span,
  pub metadata: ASTMetadata,
}

impl ASTParameter {
  pub fn new(
    name: SymbolId,
    type_: IgnisTypeSyntax,
    span: Span,
    metadata: ASTMetadata,
  ) -> Self {
    Self {
      name,
      type_,
      span,
      metadata,
    }
  }
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ASTFunctionSignature {
  pub name: SymbolId,
  pub type_params: Option<ASTGenericParams>,
  pub parameters: Vec<ASTParameter>,
  pub return_type: IgnisTypeSyntax,
  pub span: Span,
  pub metadata: ASTMetadata,
}

impl ASTFunctionSignature {
  pub fn new(
    name: SymbolId,
    type_params: Option<ASTGenericParams>,
    parameters: Vec<ASTParameter>,
    return_type: IgnisTypeSyntax,
    span: Span,
    metadata: ASTMetadata,
  ) -> Self {
    Self {
      name,
      type_params,
      parameters,
      return_type,
      span,
      metadata,
    }
  }
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ASTFunction {
  pub signature: ASTFunctionSignature,
  pub body: Option<NodeId>,
}

impl ASTFunction {
  pub fn new(
    signature: ASTFunctionSignature,
    body: Option<NodeId>,
  ) -> Self {
    Self { signature, body }
  }
}
