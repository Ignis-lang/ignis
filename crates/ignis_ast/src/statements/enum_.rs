use ignis_type::{span::Span, symbol::SymbolId};

use crate::{attribute::ASTAttribute, generics::ASTGenericParams, metadata::ASTMetadata, type_::IgnisTypeSyntax, NodeId};

use super::record::ASTMethod;

/// Enum declaration: `enum Name<T> { variants, methods, fields }`
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ASTEnum {
  pub name: SymbolId,
  pub type_params: Option<ASTGenericParams>,
  pub items: Vec<ASTEnumItem>,
  pub span: Span,
  pub doc: Option<String>,
  pub attrs: Vec<ASTAttribute>,
}

impl ASTEnum {
  pub fn new(
    name: SymbolId,
    type_params: Option<ASTGenericParams>,
    items: Vec<ASTEnumItem>,
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

/// Item inside an enum: variant, method, or field
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum ASTEnumItem {
  Variant(ASTEnumVariant),
  Method(ASTMethod),
  Field(ASTEnumField),
}

/// Enum variant: `Name` or `Name(Type, Type)`
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ASTEnumVariant {
  pub name: SymbolId,
  pub name_span: Span,
  pub payload: Vec<IgnisTypeSyntax>,
  pub span: Span,
  pub doc: Option<String>,
}

impl ASTEnumVariant {
  pub fn new(
    name: SymbolId,
    name_span: Span,
    payload: Vec<IgnisTypeSyntax>,
    span: Span,
    doc: Option<String>,
  ) -> Self {
    Self {
      name,
      name_span,
      payload,
      span,
      doc,
    }
  }
}

/// Field inside an enum (implicitly static)
///
/// Syntax: `name: type = expr;`
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ASTEnumField {
  pub name: SymbolId,
  pub name_span: Span,
  pub type_: IgnisTypeSyntax,
  pub value: Option<NodeId>,
  pub metadata: ASTMetadata,
  pub span: Span,
  pub doc: Option<String>,
}

impl ASTEnumField {
  pub fn new(
    name: SymbolId,
    name_span: Span,
    type_: IgnisTypeSyntax,
    value: Option<NodeId>,
    metadata: ASTMetadata,
    span: Span,
    doc: Option<String>,
  ) -> Self {
    Self {
      name,
      name_span,
      type_,
      value,
      metadata,
      span,
      doc,
    }
  }
}
