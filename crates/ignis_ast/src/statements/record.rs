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
  pub doc: Option<String>,
}

impl ASTRecord {
  pub fn new(
    name: SymbolId,
    type_params: Option<ASTGenericParams>,
    items: Vec<ASTRecordItem>,
    span: Span,
    doc: Option<String>,
  ) -> Self {
    Self {
      name,
      type_params,
      items,
      span,
      doc,
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
  pub name_span: Span,
  pub type_: IgnisTypeSyntax,
  pub value: Option<NodeId>,
  pub metadata: ASTMetadata,
  pub span: Span,
}

impl ASTRecordField {
  pub fn new(
    name: SymbolId,
    name_span: Span,
    type_: IgnisTypeSyntax,
    value: Option<NodeId>,
    metadata: ASTMetadata,
    span: Span,
  ) -> Self {
    Self {
      name,
      name_span,
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
/// Instance methods can have `&self` or `&mut self` as first parameter.
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ASTMethod {
  pub name: SymbolId,
  pub name_span: Span,
  pub type_params: Option<ASTGenericParams>,
  pub parameters: Vec<ASTParameter>,
  pub return_type: IgnisTypeSyntax,
  pub body: NodeId,
  pub metadata: ASTMetadata,
  /// Whether the method has `&mut self` (true) or `&self` (false) or no self (None).
  /// None means static method or instance method without explicit self.
  pub self_param: Option<bool>,
  pub span: Span,
  pub doc: Option<String>,
}

impl ASTMethod {
  pub fn new(
    name: SymbolId,
    name_span: Span,
    type_params: Option<ASTGenericParams>,
    parameters: Vec<ASTParameter>,
    return_type: IgnisTypeSyntax,
    body: NodeId,
    metadata: ASTMetadata,
    self_param: Option<bool>,
    span: Span,
    doc: Option<String>,
  ) -> Self {
    Self {
      name,
      name_span,
      type_params,
      parameters,
      return_type,
      body,
      metadata,
      self_param,
      span,
      doc,
    }
  }

  pub fn is_static(&self) -> bool {
    self.metadata.contains(ASTMetadata::STATIC)
  }

  /// Returns true if method has `&mut self`
  pub fn has_mut_self(&self) -> bool {
    self.self_param == Some(true)
  }

  /// Returns true if method has `&self` (immutable)
  pub fn has_self(&self) -> bool {
    self.self_param.is_some()
  }
}
