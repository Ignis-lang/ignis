use ignis_type::{definition::InlineMode, span::Span, symbol::SymbolId};

use crate::{attribute::ASTAttribute, generics::ASTGenericParams, metadata::ASTMetadata, NodeId, type_::IgnisTypeSyntax};

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ASTParameter {
  pub name: SymbolId,
  pub type_: IgnisTypeSyntax,
  pub span: Span,
  pub metadata: ASTMetadata,
  pub attrs: Vec<ASTAttribute>,
}

impl ASTParameter {
  pub fn new(
    name: SymbolId,
    type_: IgnisTypeSyntax,
    span: Span,
    metadata: ASTMetadata,
    attrs: Vec<ASTAttribute>,
  ) -> Self {
    Self {
      name,
      type_,
      span,
      metadata,
      attrs,
    }
  }
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ASTFunctionSignature {
  pub name: SymbolId,
  pub name_span: Span,
  pub type_params: Option<ASTGenericParams>,
  pub parameters: Vec<ASTParameter>,
  pub return_type: IgnisTypeSyntax,
  pub span: Span,
  pub metadata: ASTMetadata,
  pub doc: Option<String>,
  pub inline_mode: InlineMode,
  pub attrs: Vec<ASTAttribute>,
}

impl ASTFunctionSignature {
  pub fn new(
    name: SymbolId,
    name_span: Span,
    type_params: Option<ASTGenericParams>,
    parameters: Vec<ASTParameter>,
    return_type: IgnisTypeSyntax,
    span: Span,
    metadata: ASTMetadata,
    doc: Option<String>,
    inline_mode: InlineMode,
    attrs: Vec<ASTAttribute>,
  ) -> Self {
    Self {
      name,
      name_span,
      type_params,
      parameters,
      return_type,
      span,
      metadata,
      doc,
      inline_mode,
      attrs,
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
