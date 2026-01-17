use ignis_type::span::Span;

use crate::expressions::ASTExpression;

pub mod block;
pub mod break_statement;
pub mod comment_statement;
pub mod const_statement;
pub mod continue_statement;
pub mod enum_;
pub mod export_statement;
pub mod extern_statement;
pub mod for_of;
pub mod for_statement;
pub mod function;
pub mod if_statement;
pub mod import_statement;
pub mod namespace_statement;
pub mod record;
pub mod return_statement;
pub mod type_alias;
pub mod variable;
pub mod while_statement;

pub use block::ASTBlock;
pub use break_statement::ASTBreak;
pub use comment_statement::ASTComment;
pub use const_statement::ASTConstant;
pub use continue_statement::ASTContinue;
pub use enum_::{ASTEnum, ASTEnumField, ASTEnumItem, ASTEnumVariant};
pub use export_statement::ASTExport;
pub use extern_statement::ASTExtern;
pub use for_of::{ASTForOf, ForOfBinding};
pub use for_statement::ASTFor;
pub use function::ASTFunction;
pub use if_statement::ASTIf;
pub use import_statement::{ASTImport, ASTImportItem};
pub use namespace_statement::ASTNamespace;
pub use record::{ASTMethod, ASTRecord, ASTRecordField, ASTRecordItem};
pub use return_statement::ASTReturn;
pub use type_alias::ASTTypeAlias;
pub use variable::ASTVariable;
pub use while_statement::ASTWhile;

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum ASTStatement {
  Expression(ASTExpression),
  Variable(ASTVariable),
  Function(ASTFunction),
  Block(ASTBlock),
  If(ASTIf),
  While(ASTWhile),
  For(ASTFor),
  ForOf(ASTForOf),
  Return(ASTReturn),
  Continue(ASTContinue),
  Break(ASTBreak),
  Import(ASTImport),
  Extern(ASTExtern),
  Constant(ASTConstant),
  Export(ASTExport),
  Comment(ASTComment),
  Namespace(ASTNamespace),
  TypeAlias(ASTTypeAlias),
  Record(ASTRecord),
  Enum(ASTEnum),
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
      ASTStatement::ForOf(for_of_stmt) => &for_of_stmt.span,
      ASTStatement::Return(ret) => &ret.span,
      ASTStatement::Continue(cont) => &cont.span,
      ASTStatement::Break(brk) => &brk.span,
      ASTStatement::Import(imp) => &imp.span,
      ASTStatement::Extern(ext) => &ext.span,
      ASTStatement::Constant(const_) => &const_.span,
      ASTStatement::Export(exp) => exp.span(),
      ASTStatement::Comment(comment) => &comment.span,
      ASTStatement::Namespace(ns) => &ns.span,
      ASTStatement::TypeAlias(ta) => &ta.span,
      ASTStatement::Record(rec) => &rec.span,
      ASTStatement::Enum(en) => &en.span,
    }
  }
}
