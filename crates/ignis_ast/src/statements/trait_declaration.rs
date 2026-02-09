use ignis_type::{definition::InlineMode, span::Span, symbol::SymbolId};

use crate::{attribute::ASTAttribute, generics::ASTGenericParams, type_::IgnisTypeSyntax, NodeId};

use super::function::ASTParameter;

/// Trait declaration: `trait Name<T> { methods }`
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ASTTrait {
  pub name: SymbolId,
  pub type_params: Option<ASTGenericParams>,
  pub methods: Vec<ASTTraitMethod>,
  pub span: Span,
  pub doc: Option<String>,
  pub attrs: Vec<ASTAttribute>,
}

impl ASTTrait {
  pub fn new(
    name: SymbolId,
    type_params: Option<ASTGenericParams>,
    methods: Vec<ASTTraitMethod>,
    span: Span,
    doc: Option<String>,
    attrs: Vec<ASTAttribute>,
  ) -> Self {
    Self {
      name,
      type_params,
      methods,
      span,
      doc,
      attrs,
    }
  }
}

/// Method declaration inside a trait.
/// Required methods have `body: None`; default methods have a body.
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ASTTraitMethod {
  pub name: SymbolId,
  pub name_span: Span,
  pub type_params: Option<ASTGenericParams>,
  pub parameters: Vec<ASTParameter>,
  pub return_type: IgnisTypeSyntax,
  /// `None` = required method (no body), `Some` = default method (with body)
  pub body: Option<NodeId>,
  /// `Some(true)` = `&mut self`, `Some(false)` = `&self`, `None` = no self param
  pub self_param: Option<bool>,
  pub span: Span,
  pub doc: Option<String>,
  pub inline_mode: InlineMode,
  pub attrs: Vec<ASTAttribute>,
}

impl ASTTraitMethod {
  pub fn new(
    name: SymbolId,
    name_span: Span,
    type_params: Option<ASTGenericParams>,
    parameters: Vec<ASTParameter>,
    return_type: IgnisTypeSyntax,
    body: Option<NodeId>,
    self_param: Option<bool>,
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
      self_param,
      span,
      doc,
      inline_mode,
      attrs,
    }
  }

  pub fn has_default(&self) -> bool {
    self.body.is_some()
  }
}
