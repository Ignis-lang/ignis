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
use ignis_token::token::Token;
use import::ASTImport;
use method::ASTMethod;
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
}

impl ASTStatement {
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
          "is_exported": function.is_exported,
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
          "variable": ASTStatement::Variable(Box::new(for_of_statement.variable.clone())).to_json(),
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
          "name": extern_.name.lexeme,
          "body": extern_.body.iter().map(|s| s.to_json()).collect::<Vec<serde_json::Value>>(),
          "metadata": extern_.metadata.to_json(),
        })
      },
    }
  }
}
