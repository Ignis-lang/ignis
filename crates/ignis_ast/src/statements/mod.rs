use ignis_type::span::Span;

use crate::{
  expressions::ASTExpression,
  statements::{
    block::ASTBlock, break_statement::ASTBreak, comment_statement::ASTComment, continue_statement::ASTContinue,
    extern_statement::ASTExternModule, for_statement::ASTFor, function::ASTFunction, if_statement::ASTIf,
    import_statement::ASTImport, return_statement::ASTReturn, variable::ASTVariable, while_statement::ASTWhile,
  },
};

pub mod block;
pub mod break_statement;
pub mod comment_statement;
pub mod continue_statement;
pub mod extern_statement;
pub mod for_statement;
pub mod function;
pub mod if_statement;
pub mod import_statement;
pub mod return_statement;
pub mod variable;
pub mod while_statement;

#[derive(Debug, PartialEq, Clone)]
pub enum ASTStatement {
  Expression(ASTExpression),
  Variable(ASTVariable),
  Function(ASTFunction),
  Block(ASTBlock),
  If(ASTIf),
  While(ASTWhile),
  For(ASTFor),
  Return(ASTReturn),
  Continue(ASTContinue),
  Break(ASTBreak),
  Import(ASTImport),
  Extern(ASTExternModule),
  Comment(ASTComment),
}

impl ASTStatement {
  pub fn span(&self) -> &Span {
    match self {
      ASTStatement::Expression(expr) => expr.span(),
      _ => todo!(),
    }
  }
}
