pub mod block;
pub mod comment;
pub mod constant;
pub mod declare;
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
pub mod variable;
pub mod while_statement;

use block::ASTBlock;
use comment::ASTComment;
use r#extern::ASTExtern;
use for_of_statement::ASTForOf;
use for_statement::ASTFor;
use function::ASTFunction;
use if_statement::ASTIf;
use ignis_token::{token::Token, token_types::TokenType};
use import::ASTImport;
use method::ASTMethod;
use namespace::ASTNamespace;
use property::ASTProperty;
use record::ASTRecord;
use return_::ASTReturn;
use serde_json::json;
use variable::ASTVariable;
use while_statement::ASTWhile;

use crate::{expressions::ASTExpression, visitor::ASTVisitor};
use constant::ASTConstant;

#[derive(Debug, Clone, PartialEq)]
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
  Include(Box<Token>),
  Source(Box<Token>),
  Namespace(Box<ASTNamespace>),
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
      ASTStatement::Include(include) => visitor.visit_include_statement(include),
      ASTStatement::Source(source) => visitor.visit_source_statement(source),
      ASTStatement::Namespace(namespace) => visitor.visit_namespace_statement(namespace),
    }
  }

  pub fn to_json(&self) -> serde_json::Value {
    match self {
      ASTStatement::Expression(expression) => expression.to_json(),
      ASTStatement::Constant(constant) => {
        json!({
          "type": "CONSTANT",
          "name": constant.name.lexeme,
          "value": constant.value.to_json(),
          "type_annotation": constant.type_annotation,
          "metadata": constant.metadata.to_json(),
        })
      },
      ASTStatement::Function(function) => {
        json!({
          "type": "FUNCTION",
          "name": function.name.lexeme,
          "parameters": function.parameters.iter().map(|p| ASTStatement::Variable(Box::new(p.clone())).to_json()).collect::<Vec<serde_json::Value>>(),
          "body": function.body.iter().map(|s| s.to_json()).collect::<Vec<serde_json::Value>>(),
          "return_type": function.return_type,
          "metadata": function.metadata.to_json(),
        })
      },
      ASTStatement::Variable(variable) => {
        json!({
          "type": "VARIABLE",
          "name": variable.name.lexeme,
          "initializer": variable.initializer.as_ref().map(|e| e.to_json()),
          "type_annotation": variable.type_annotation,
          "metadata": variable.metadata.to_json(),
        })
      },
      ASTStatement::Block(block) => {
        json!({
          "type": "BLOCK",
          "statements": block.statements.iter().map(|s| s.to_json()).collect::<Vec<serde_json::Value>>(),
        })
      },
      ASTStatement::If(if_statement) => {
        json!({
          "type": "IF",
          "condition": if_statement.condition.to_json(),
          "then_branch": if_statement.then_branch.to_json(),
          "else_branch": if_statement.else_branch.as_ref().map(|s| s.to_json()),
        })
      },
      ASTStatement::For(for_statement) => {
        json!({
          "type": "FOR",
          "variable": ASTStatement::Variable(for_statement.variable.clone()).to_json(),
          "condition": for_statement.condition.to_json(),
          "increment": for_statement.increment.to_json(),
          "body": for_statement.body.to_json(),
        })
      },
      ASTStatement::ForOf(for_of_statement) => {
        json!({
          "type": "FOR_OF",
          "variable": ASTStatement::Variable(for_of_statement.variable.clone()).to_json(),
          "iterable": for_of_statement.iterable.to_json(),
          "body": for_of_statement.body.to_json(),
          "token": for_of_statement.token,
        })
      },
      ASTStatement::While(while_statement) => {
        json!({
          "type": "WHILE",
          "condition": while_statement.condition.to_json(),
          "body": while_statement.body.to_json(),
        })
      },
      ASTStatement::Break { token } => {
        json!({
          "type": "BREAK",
          "token": token.lexeme,
        })
      },
      ASTStatement::Continue { token } => {
        json!({
          "type": "CONTINUE",
          "token": token.lexeme,
        })
      },
      ASTStatement::Return(return_) => {
        json!({
          "type": "RETURN",
          "value": return_.value.as_ref().map(|v| v.to_json()),
          "token": return_.token.lexeme,
        })
      },
      ASTStatement::Import(import) => {
        json!({
          "type": "IMPORT",
          "module_path": import.module_path.lexeme,
          "symbols": import.symbols.iter().map(|s| s.to_json()).collect::<Vec<serde_json::Value>>(),
          "is_std": import.is_std,
          "source": import.source,
        })
      },
      ASTStatement::Comment(comment) => {
        json!({
          "type": "COMMENT",
          "comment": comment.comment,
          "type": comment.type_,
        })
      },
      ASTStatement::Record(record) => {
        json!({
          "type": "RECORD",
          "name": record.name,
          "items": record.items.iter().map(|s| s.to_json()).collect::<Vec<serde_json::Value>>(),
          "metadata": record.metadata.to_json(),
        })
      },
      ASTStatement::Method(method) => {
        json!({
          "type": "METHOD",
          "name": method.name.lexeme,
          "parameters": method.parameters.iter().map(|p| ASTStatement::Variable(Box::new(p.clone())).to_json()).collect::<Vec<serde_json::Value>>(),
          "body": ASTStatement::Block(Box::new(method.body.clone())).to_json(),
          "return_type": method.return_type,
          "metadata": method.metadata.to_json(),
          "class_name": method.class_name.lexeme,
        })
      },
      ASTStatement::Property(property) => {
        json!({
          "type": "PROPERTY",
          "name": property.name.lexeme,
          "initializer": property.initializer.as_ref().map(|e| e.to_json()),
          "type_annotation": property.type_annotation,
          "metadata": property.metadata.to_json(),
        })
      },
      ASTStatement::Extern(extern_) => {
        json!({
          "type": "EXTERN",
          "name": extern_.name.as_ref().to_json(),
          "body": extern_.body.iter().map(|s| s.to_json()).collect::<Vec<serde_json::Value>>(),
          "metadata": extern_.metadata.to_json(),
        })
      },
      ASTStatement::Include(include) => {
        json!({
          "type": "INCLUDE",
          "path": include,
        })
      },
      ASTStatement::Source(source) => {
        json!({
          "type": "SOURCE",
          "path": source,
        })
      },
      ASTStatement::Namespace(namespace) => {
        json!({
          "type": "NAMESPACE",
          "name": namespace.name.to_json(),
          "members": namespace.members.iter().map(|s| s.to_json()).collect::<Vec<serde_json::Value>>(),
          "metadata": namespace.metadata.to_json(),
        })
      },
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
      ASTStatement::Include(_) => TokenType::Include,
      ASTStatement::Source(_) => TokenType::Source,
      ASTStatement::Namespace(_) => TokenType::Namespace,
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
      ASTStatement::Extern(extern_) => extern_.name.as_ref().into(),
      ASTStatement::Include(include) => include.as_ref().clone(),
      ASTStatement::Source(source) => source.as_ref().clone(),
      ASTStatement::Namespace(namespace) => namespace.name.as_ref().into(),
    }
  }
}
