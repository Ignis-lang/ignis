pub mod block;
pub mod break_statement;
pub mod class;
pub mod continue_statement;
pub mod export;
pub mod expression;
pub mod extern_statement;
pub mod for_in;
pub mod for_statement;
pub mod function;
pub mod if_statement;
pub mod import;
pub mod return_statement;
pub mod variable;
pub mod while_statement;

use serde_json::json;

use self::{
  expression::ExpressionStatement, variable::Variable, if_statement::IfStatement, block::Block,
  while_statement::WhileStatement, function::FunctionStatement, return_statement::Return,
  class::Class, for_in::ForIn, for_statement::For, import::Import, break_statement::BreakStatement,
  continue_statement::Continue,
};

use crate::{visitor::Visitor, statement::import::ImportSource};

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
  Expression(ExpressionStatement),
  Variable(Variable),
  Block(Block),
  IfStatement(IfStatement),
  WhileStatement(WhileStatement),
  FunctionStatement(FunctionStatement),
  Return(Return),
  Class(Class),
  For(For),
  ForIn(ForIn),
  Import(Import),
  Break(BreakStatement),
  Continue(Continue),
}

impl Statement {
  pub fn accept<R>(&self, visitor: &mut dyn Visitor<R>) -> R {
    match self {
      Statement::Expression(expression) => visitor.visit_expression_statement(expression),
      Statement::Variable(variable) => visitor.visit_variable_statement(variable),
      Statement::Block(block) => visitor.visit_block(block),
      Statement::IfStatement(if_statement) => visitor.visit_if_statement(if_statement),
      Statement::WhileStatement(while_statement) => visitor.visit_while_statement(while_statement),
      Statement::FunctionStatement(function_statement) => {
        visitor.visit_function_statement(function_statement)
      }
      Statement::Return(r) => visitor.visit_return_statement(r),
      Statement::Class(class) => visitor.visit_class_statement(class),
      Statement::ForIn(for_in) => visitor.visit_for_in_statement(for_in),
      Statement::Import(import) => visitor.visit_import_statement(import),
      Statement::Break(break_statement) => visitor.visit_break_statement(break_statement),
      Statement::Continue(continue_statement) => {
        visitor.visit_continue_statement(continue_statement)
      }
      Statement::For(_) => todo!(),
    }
  }

  pub fn to_json(&self) -> serde_json::Value {
    match self {
      Statement::Expression(expression) => expression.expression.to_json(),
      Statement::Variable(variable) => {
        let initializer = match &variable.initializer {
          Some(initializer) => initializer.to_json(),
          None => json!(null),
        };

        json!({
          "type": "Variable",
            "name": variable.name.span.literal,
            "initializer": initializer,
            "type_annotation": variable.type_annotation.to_string(),
            "is_mutable": variable.metadata.is_mutable,
            "is_global": variable.metadata.is_global,
            "is_static": variable.metadata.is_static,
            "is_public": variable.metadata.is_public,
            "is_reference": variable.metadata.is_reference,
        })
      }
      Statement::Block(block) => {
        json!({
          "type": "Block",
          "statements": block.statements.iter().map(|x| x.to_json()).collect::<Vec<serde_json::Value>>(),
        })
      }
      Statement::IfStatement(if_statement) => {
        json!({
          "type": "IfStatement",
          "condition": if_statement.condition.to_json(),
          "then_branch": if_statement.then_branch.to_json(),
          "else_branch": match &if_statement.else_branch {
            Some(else_branch) => else_branch.to_json(),
            None => json!(null),
          },
        })
      }
      Statement::WhileStatement(while_statement) => {
        json!({
          "type": "WhileStatement",
          "condition": while_statement.condition.to_json(),
          "body": while_statement.body.to_json(),
        })
      }
      Statement::FunctionStatement(function) => {
        json!({
          "type": "FunctionStatement",
          "name": function.name.span.literal,
          "parameters": function.parameters.iter().map(|x| x.to_json()).collect::<Vec<serde_json::Value>>(),
          "body": function.body.iter().map(|x| x.to_json()).collect::<Vec<serde_json::Value>>(),
          "return_type": match &function.return_type {
            Some(return_type) => return_type.to_string(),
            None => String::new(),
          },
        })
      }
      Statement::Return(return_statement) => {
        json!({
          "type": "Return",
          "value": match &return_statement.value {
            Some(value) => value.to_json(),
            None => json!(null),
          },
        })
      }
      Statement::Class(class) => {
        json!({
          "type": "Class",
          "name": class.name.span.literal,
          // "methods": class.methods.iter().map(|x| x.to_json()).collect::<Vec<serde_json::Value>>(),
          // "properties": class.properties.iter().map(|x| x.to_json()).collect::<Vec<serde_json::Value>>(),
        })
      }
      Statement::ForIn(for_in) => {
        json!({
          "type": "ForIn",
          "iterable": for_in.iterable.to_json(),
          "body": for_in.body.to_json(),
        })
      }
      Statement::Import(import) => {
        let symbol = import
          .symbols
          .iter()
          .map(|x| x.to_json())
          .collect::<Vec<serde_json::Value>>();

        json!({
          "type": "Import",
          "module_path": import.module_path.span.literal,
          "symbol": symbol,
          "is_std": import.is_std,
          "source": match &import.source {
            ImportSource::StandardLibrary => json!("StandardLibrary"),
            ImportSource::FileSystem => json!("FileSystem"),
            ImportSource::Package => json!("Package"),
          },
        })
      }
      Statement::Break(_) => {
        json!({
            "type": "Break",
        })
      }
      Statement::Continue(_) => {
        json!({
            "type": "Continue",
        })
      }
      Statement::For(_) => todo!(),
    }
  }
}
