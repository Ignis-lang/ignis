use ignis_type::{
  definition::{InlineMode, SelfReceiver},
  span::Span,
  symbol::SymbolId,
};

use crate::{attribute::ASTAttribute, generics::ASTGenericParams, metadata::ASTMetadata, type_::IgnisTypeSyntax, NodeId};

use super::function::ASTParameter;

/// Record declaration: `record Name<T, U> { fields, methods }`
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ASTRecord {
  pub name: SymbolId,
  pub type_params: Option<ASTGenericParams>,
  pub items: Vec<ASTRecordItem>,
  pub span: Span,
  pub doc: Option<String>,
  pub attrs: Vec<ASTAttribute>,
}

impl ASTRecord {
  pub fn new(
    name: SymbolId,
    type_params: Option<ASTGenericParams>,
    items: Vec<ASTRecordItem>,
    span: Span,
    doc: Option<String>,
    attrs: Vec<ASTAttribute>,
  ) -> Self {
    Self {
      name,
      type_params,
      items,
      span,
      doc,
      attrs,
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
  pub doc: Option<String>,
  pub attrs: Vec<ASTAttribute>,
}

impl ASTRecordField {
  pub fn new(
    name: SymbolId,
    name_span: Span,
    type_: IgnisTypeSyntax,
    value: Option<NodeId>,
    metadata: ASTMetadata,
    span: Span,
    doc: Option<String>,
    attrs: Vec<ASTAttribute>,
  ) -> Self {
    Self {
      name,
      name_span,
      type_,
      value,
      metadata,
      span,
      doc,
      attrs,
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
  /// How the method takes its receiver, or `None` for a static method or an
  /// instance method written without an explicit receiver.
  pub self_param: Option<SelfReceiver>,
  pub span: Span,
  pub doc: Option<String>,
  pub inline_mode: InlineMode,
  pub attrs: Vec<ASTAttribute>,
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
    self_param: Option<SelfReceiver>,
    span: Span,
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
      body,
      metadata,
      self_param,
      span,
      doc,
      inline_mode,
      attrs,
    }
  }

  pub fn is_static(&self) -> bool {
    self.metadata.contains(ASTMetadata::STATIC)
  }

  /// Returns true if method has `&mut self`
  pub fn has_mut_self(&self) -> bool {
    self.self_param == Some(SelfReceiver::RefMut)
  }

  /// Returns true if the method has any explicit receiver
  pub fn has_self(&self) -> bool {
    self.self_param.is_some()
  }
}
