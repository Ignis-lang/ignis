pub mod display;
pub mod expressions;
pub mod metadata;
pub mod statements;
pub mod type_;
pub mod visitor;

use ignis_type::{Id, span::Span};

use crate::expressions::ASTExpression;
use crate::statements::ASTStatement;

pub type NodeId = Id<ASTNode>;

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum ASTNode {
  Expression(ASTExpression),
  Statement(ASTStatement),
}

impl ASTNode {
  pub fn span(&self) -> &Span {
    match self {
      ASTNode::Expression(expr) => expr.span(),
      ASTNode::Statement(stmt) => stmt.span(),
    }
  }
}
