use ignis_type::symbol::SymbolId;

use crate::statements::function::ASTFunctionSignature;

#[derive(Debug, Clone, PartialEq)]
pub struct ASTExternModule {
  pub name: SymbolId,
  pub items: Vec<ASTExternItem>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ASTExternItem {
  Function(ASTFunctionSignature),
  // Type(SymbolId),
}
