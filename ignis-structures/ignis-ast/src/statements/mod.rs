pub mod block;
pub mod comment;
pub mod constant;
pub mod declare;
pub mod enum_statement;
pub mod export;
pub mod r#extern;
pub mod for_of_statement;
pub mod for_statement;
pub mod function;
pub mod if_statement;
pub mod import;
pub mod meta;
pub mod method;
pub mod namespace;
pub mod property;
pub mod record;
pub mod return_;
pub mod type_alias;
pub mod variable;
pub mod while_statement;

use block::ASTBlock;
use comment::ASTComment;
use enum_statement::ASTEnum;
use r#extern::ASTExtern;
use for_of_statement::ASTForOf;
use for_statement::ASTFor;
use function::ASTFunction;
use if_statement::ASTIf;
use ignis_token::{token::Token, token_types::TokenType};
use import::ASTImport;
use meta::ASTMetaStatement;
use method::ASTMethod;
use namespace::ASTNamespace;
use property::ASTProperty;
use record::ASTRecord;
use return_::ASTReturn;
use serde::Serialize;
use type_alias::ASTTypeAlias;
use variable::ASTVariable;
use while_statement::ASTWhile;

use crate::{expressions::ASTExpression, visitor::ASTVisitor};
use constant::ASTConstant;

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum ASTStatement {
  Expression(Box<ASTExpression>),
  Constant(Box<ASTConstant>),
  Function(Box<ASTFunction>),
  Variable(Box<ASTVariable>),
  Block(Box<ASTBlock>),
  If(Box<ASTIf>),
  For(Box<ASTFor>),
  ForOf(Box<ASTForOf>),
  While(Box<ASTWhile>),
  Break { token: Token },
  Continue { token: Token },
  Return(Box<ASTReturn>),
  Import(Box<ASTImport>),
  Comment(Box<ASTComment>),
  Record(Box<ASTRecord>),
  Method(Box<ASTMethod>),
  Property(Box<ASTProperty>),
  Extern(Box<ASTExtern>),
  Namespace(Box<ASTNamespace>),
  TypeAlias(Box<ASTTypeAlias>),
  Enum(Box<ASTEnum>),
  Meta(Box<ASTMetaStatement>),
}

impl ASTStatement {
  pub fn as_expression(&self) -> &ASTExpression {
    match self {
      ASTStatement::Expression(expression) => expression,
      _ => unreachable!(),
    }
  }

  pub fn accept<R>(
    &self,
    visitor: &mut dyn ASTVisitor<R>,
  ) -> R {
    match self {
      ASTStatement::Expression(expression) => visitor.visit_expression_statement(expression),
      ASTStatement::Constant(constant) => visitor.visit_constant_statement(constant),
      ASTStatement::Function(function) => visitor.visit_function_statement(function),
      ASTStatement::Variable(variable) => visitor.visit_variable_statement(variable),
      ASTStatement::Block(block) => visitor.visit_block_statement(block),
      ASTStatement::If(if_statement) => visitor.visit_if_statement(if_statement),
      ASTStatement::For(for_statement) => visitor.visit_for_statement(for_statement),
      ASTStatement::ForOf(for_of_statement) => visitor.visit_for_of_statement(for_of_statement),
      ASTStatement::While(while_statement) => visitor.visit_while_statement(while_statement),
      ASTStatement::Break { token } => visitor.visit_break_statement(token),
      ASTStatement::Continue { token } => visitor.visit_continue_statement(token),
      ASTStatement::Return(return_) => visitor.visit_return_statement(return_),
      ASTStatement::Import(import) => visitor.visit_import_statement(import),
      ASTStatement::Comment(comment) => visitor.visit_comment_statement(comment),
      ASTStatement::Record(record) => visitor.visit_record_statement(record),
      ASTStatement::Method(method) => visitor.visit_method_statement(method),
      ASTStatement::Property(property) => visitor.visit_property_statement(property),
      ASTStatement::Extern(extern_) => visitor.visit_extern_statement(extern_),
      ASTStatement::Namespace(namespace) => visitor.visit_namespace_statement(namespace),
      ASTStatement::TypeAlias(type_alias) => visitor.visit_type_alias_statement(type_alias),
      ASTStatement::Enum(enum_) => visitor.visitor_enum_statement(enum_),
      ASTStatement::Meta(meta) => visitor.visit_meta_statement(meta),
    }
  }
}

impl Into<TokenType> for &ASTStatement {
  fn into(self) -> TokenType {
    match self {
      ASTStatement::Expression(_) => todo!(),
      ASTStatement::Constant(_) => TokenType::Const,
      ASTStatement::Function(_) => TokenType::Function,
      ASTStatement::Variable(_) => TokenType::Identifier,
      ASTStatement::Block(_) => TokenType::LeftBrace,
      ASTStatement::If(_) => TokenType::If,
      ASTStatement::For(_) => TokenType::For,
      ASTStatement::ForOf(_) => TokenType::For,
      ASTStatement::While(_) => TokenType::While,
      ASTStatement::Break { .. } => TokenType::Break,
      ASTStatement::Continue { .. } => TokenType::Continue,
      ASTStatement::Return(_) => TokenType::Return,
      ASTStatement::Import(_) => TokenType::Import,
      ASTStatement::Comment(_) => TokenType::Comment,
      ASTStatement::Record(_) => TokenType::Record,
      ASTStatement::Method(_) => TokenType::Identifier,
      ASTStatement::Property(_) => TokenType::Identifier,
      ASTStatement::Extern(_) => TokenType::Extern,
      ASTStatement::Namespace(_) => TokenType::Namespace,
      ASTStatement::TypeAlias(_) => TokenType::Type,
      ASTStatement::Enum(_) => TokenType::Enum,
      ASTStatement::Meta(_) => TokenType::Meta,
    }
  }
}

impl Into<Token> for &ASTStatement {
  fn into(self) -> Token {
    match self {
      ASTStatement::Expression(expression) => expression.as_ref().into(),
      ASTStatement::Constant(constant) => constant.name.clone(),
      ASTStatement::Function(function) => function.name.clone(),
      ASTStatement::Variable(variable) => variable.name.clone(),
      ASTStatement::Block(_) => todo!(),
      ASTStatement::If(if_) => if_.condition.as_ref().into(),
      ASTStatement::For(for_) => for_.variable.name.clone(),
      ASTStatement::ForOf(for_of) => for_of.variable.name.clone(),
      ASTStatement::While(while_) => while_.condition.as_ref().into(),
      ASTStatement::Break { token } => token.clone(),
      ASTStatement::Continue { token } => token.clone(),
      ASTStatement::Return(return_) => return_.token.clone(),
      ASTStatement::Import(import) => import.module_path.clone(),
      ASTStatement::Comment(comment) => comment.token.clone(),
      ASTStatement::Record(record) => record.name.clone(),
      ASTStatement::Method(method) => method.name.clone(),
      ASTStatement::Property(property) => property.name.as_ref().clone(),
      ASTStatement::Extern(extern_) => extern_.name.clone(),
      ASTStatement::Namespace(namespace) => namespace.name.clone(),
      ASTStatement::TypeAlias(type_alias) => type_alias.name.clone(),
      ASTStatement::Enum(enum_) => enum_.name.clone(),
      ASTStatement::Meta(meta) => meta.name.clone(),
    }
  }
}
