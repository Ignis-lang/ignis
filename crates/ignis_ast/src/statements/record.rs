use ignis_type::{span::Span, symbol::SymbolId};

use crate::{generics::ASTGenericParams, metadata::ASTMetadata, type_::IgnisTypeSyntax, NodeId};

use super::function::ASTParameter;

/// Record declaration: `record Name<T, U> { fields, methods }`
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ASTRecord {
  pub name: SymbolId,
  pub type_params: Option<ASTGenericParams>,
  pub items: Vec<ASTRecordItem>,
  pub span: Span,
}

impl ASTRecord {
  pub fn new(
    name: SymbolId,
    type_params: Option<ASTGenericParams>,
    items: Vec<ASTRecordItem>,
    span: Span,
  ) -> Self {
    Self {
      name,
      type_params,
      items,
      span,
    }
  }
}

/// Item inside a record: either a field or a method
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum ASTRecordItem {
  Field(ASTRecordField),
  Method(ASTMethod),
}

/// Field declaration inside a record
///
/// Instance: `name: type;`
/// Static: `static name: type = expr;`
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ASTRecordField {
  pub name: SymbolId,
  pub type_: IgnisTypeSyntax,
  pub value: Option<NodeId>,
  pub metadata: ASTMetadata,
  pub span: Span,
}

impl ASTRecordField {
  pub fn new(
    name: SymbolId,
    type_: IgnisTypeSyntax,
    value: Option<NodeId>,
    metadata: ASTMetadata,
    span: Span,
  ) -> Self {
    Self {
      name,
      type_,
      value,
      metadata,
      span,
    }
  }

  pub fn is_static(&self) -> bool {
    self.metadata.contains(ASTMetadata::STATIC)
  }
}

/// Method declaration (without `function` keyword)
///
/// Syntax: `name<U>(params): returnType { body }`
/// Static: `static name<U>(params): returnType { body }`
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ASTMethod {
  pub name: SymbolId,
  pub type_params: Option<ASTGenericParams>,
  pub parameters: Vec<ASTParameter>,
  pub return_type: IgnisTypeSyntax,
  pub body: NodeId,
  pub metadata: ASTMetadata,
  pub span: Span,
}

impl ASTMethod {
  pub fn new(
    name: SymbolId,
    type_params: Option<ASTGenericParams>,
    parameters: Vec<ASTParameter>,
    return_type: IgnisTypeSyntax,
    body: NodeId,
    metadata: ASTMetadata,
    span: Span,
  ) -> Self {
    Self {
      name,
      type_params,
      parameters,
      return_type,
      body,
      metadata,
      span,
    }
  }

  pub fn is_static(&self) -> bool {
    self.metadata.contains(ASTMetadata::STATIC)
  }
}
