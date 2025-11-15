use ignis_type::span::Span;

use crate::{
  expressions::ASTExpression,
  statements::{
    block::ASTBlock, break_statement::ASTBreak, comment_statement::ASTComment, 
    const_statement::ASTConstant, continue_statement::ASTContinue,
    export_statement::ASTExport, extern_statement::ASTExtern, for_statement::ASTFor, 
    function::ASTFunction, if_statement::ASTIf, import_statement::ASTImport, 
    return_statement::ASTReturn, variable::ASTVariable, while_statement::ASTWhile,
  },
};

pub mod block;
pub mod break_statement;
pub mod comment_statement;
pub mod const_statement;
pub mod continue_statement;
pub mod export_statement;
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
  Extern(ASTExtern),
  Constant(ASTConstant),
  Export(ASTExport),
  Comment(ASTComment),
}

impl ASTStatement {
  pub fn span(&self) -> &Span {
    match self {
      ASTStatement::Expression(expr) => expr.span(),
      ASTStatement::Variable(var) => &var.span,
      ASTStatement::Function(func) => &func.signature.span,
      ASTStatement::Block(block) => &block.span,
      ASTStatement::If(if_stmt) => &if_stmt.span,
      ASTStatement::While(while_stmt) => &while_stmt.span,
      ASTStatement::For(for_stmt) => &for_stmt.span,
      ASTStatement::Return(ret) => &ret.span,
      ASTStatement::Continue(cont) => &cont.span,
      ASTStatement::Break(brk) => &brk.span,
      ASTStatement::Import(imp) => &imp.span,
      ASTStatement::Extern(ext) => &ext.span,
      ASTStatement::Constant(const_) => &const_.span,
      ASTStatement::Export(exp) => exp.span(),
      ASTStatement::Comment(comment) => &comment.span,
    }
  }
}
