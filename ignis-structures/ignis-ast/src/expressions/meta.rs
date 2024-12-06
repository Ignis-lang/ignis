use crate::statements::ASTStatement;

use super::ASTExpression;

#[derive(Debug, Clone, PartialEq)]
pub struct ASTMeta {
  pub expression: Box<ASTExpression>,
}

impl ASTMeta {
  pub fn new(
    expression: Box<ASTExpression>,
  ) -> Self {
    Self { expression }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTMetaEntity {
  pub metas: Vec<ASTMeta>,
  pub entity: Option<ASTStatement>,
}

impl ASTMetaEntity {
  pub fn new(
    metas: Vec<ASTMeta>,
    entity: Option<ASTStatement>,
  ) -> Self {
    Self { metas, entity }
  }
}
