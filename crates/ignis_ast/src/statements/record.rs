use ignis_type::{span::Span, symbol::SymbolId};

use crate::{metadata::ASTMetadata, type_::IgnisTypeSyntax, NodeId};

use super::function::ASTParameter;

/// Record declaration: `record Name { fields, methods }`
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ASTRecord {
  pub name: SymbolId,
  pub items: Vec<ASTRecordItem>,
  pub span: Span,
}

impl ASTRecord {
  pub fn new(
    name: SymbolId,
    items: Vec<ASTRecordItem>,
    span: Span,
  ) -> Self {
    Self { name, items, span }
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
/// Syntax: `name(params): returnType { body }`
/// Static: `static name(params): returnType { body }`
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ASTMethod {
  pub name: SymbolId,
  pub parameters: Vec<ASTParameter>,
  pub return_type: IgnisTypeSyntax,
  pub body: NodeId,
  pub metadata: ASTMetadata,
  pub span: Span,
}

impl ASTMethod {
  pub fn new(
    name: SymbolId,
    parameters: Vec<ASTParameter>,
    return_type: IgnisTypeSyntax,
    body: NodeId,
    metadata: ASTMetadata,
    span: Span,
  ) -> Self {
    Self {
      name,
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
