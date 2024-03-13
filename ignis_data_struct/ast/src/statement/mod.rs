pub mod block;
pub mod break_statement;
pub mod class;
pub mod continue_statement;
pub mod enum_statement;
pub mod export;
pub mod expression;
pub mod extern_statement;
pub mod for_of;
pub mod for_statement;
pub mod function;
pub mod if_statement;
pub mod import;
pub mod interface_statement;
pub mod method;
pub mod property;
pub mod return_statement;
pub mod variable;
pub mod while_statement;

use serde_json::json;

use self::{
  block::Block, break_statement::BreakStatement, class::Class, continue_statement::Continue, enum_statement::Enum,
  expression::ExpressionStatement, for_of::ForOf, for_statement::For, function::FunctionStatement,
  if_statement::IfStatement, import::Import, interface_statement::InterfaceStatement, method::MethodStatement,
  property::PropertyStatement, return_statement::Return, variable::Variable, while_statement::WhileStatement,
  import::ImportSource,
};

use crate::visitor::Visitor;

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
  ForOf(ForOf),
  Import(Import),
  Break(BreakStatement),
  Continue(Continue),
  Method(MethodStatement),
  Property(PropertyStatement),
  Interface(InterfaceStatement),
  Enum(Enum),
}

impl Statement {
  pub fn accept<R>(
    &self,
    visitor: &mut dyn Visitor<R>,
  ) -> R {
    match self {
      Statement::Expression(expression) => visitor.visit_expression_statement(expression),
      Statement::Variable(variable) => visitor.visit_variable_statement(variable),
      Statement::Block(block) => visitor.visit_block(block),
      Statement::IfStatement(if_statement) => visitor.visit_if_statement(if_statement),
      Statement::WhileStatement(while_statement) => visitor.visit_while_statement(while_statement),
      Statement::FunctionStatement(function_statement) => visitor.visit_function_statement(function_statement),
      Statement::Return(r) => visitor.visit_return_statement(r),
      Statement::Class(class) => visitor.visit_class_statement(class),
      Statement::ForOf(for_of) => visitor.visit_for_of_statement(for_of),
      Statement::Import(import) => visitor.visit_import_statement(import),
      Statement::Break(break_statement) => visitor.visit_break_statement(break_statement),
      Statement::Continue(continue_statement) => visitor.visit_continue_statement(continue_statement),
      Statement::For(_for) => visitor.visit_for_statement(_for),
      Statement::Method(method) => visitor.visit_method_statement(method),
      Statement::Property(property) => visitor.visit_property_statement(property),
      Statement::Interface(interface) => visitor.visit_interface_statement(interface),
      Statement::Enum(enum_) => visitor.visit_enum_statement(enum_),
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
            "is_enum_member": variable.metadata.is_enum_member,
        })
      },
      Statement::Block(block) => {
        json!({
          "type": "Block",
          "statements": block.statements.iter().map(|x| x.to_json()).collect::<Vec<serde_json::Value>>(),
        })
      },
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
      },
      Statement::WhileStatement(while_statement) => {
        json!({
          "type": "WhileStatement",
          "condition": while_statement.condition.to_json(),
          "body": while_statement.body.to_json(),
        })
      },
      Statement::FunctionStatement(function) => {
        json!({
          "type": "FunctionStatement",
          "name": function.name.span.literal,
          "parameters": function.parameters.iter().map(|x| Statement::Variable(x.clone()).to_json()).collect::<Vec<serde_json::Value>>(),
          "body": function.body.iter().map(|x| x.to_json()).collect::<Vec<serde_json::Value>>(),
          "return_type": match &function.return_type {
            Some(return_type) => return_type.to_string(),
            None => String::new(),
          },
        })
      },
      Statement::Return(return_statement) => {
        json!({
          "type": "Return",
          "value": match &return_statement.value {
            Some(value) => value.to_json(),
            None => json!(null),
          },
        })
      },
      Statement::Class(class) => {
        json!({
          "type": "Class",
          "name": class.name.span.literal,
          "methods": class.methods.iter().map(|x| x.to_json()).collect::<Vec<serde_json::Value>>(),
          "properties": class.properties.iter().map(|x| x.to_json()).collect::<Vec<serde_json::Value>>(),
        })
      },
      Statement::ForOf(for_of) => {
        json!({
          "type": "ForOf",
          "iterable": for_of.iterable.to_json(),
          "body": for_of.body.to_json(),
        })
      },
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
      },
      Statement::Break(_) => {
        json!({
            "type": "Break",
        })
      },
      Statement::Continue(_) => {
        json!({
            "type": "Continue",
        })
      },
      Statement::For(_for) => {
        json!({
          "type": "For",
          "condition": _for.condition.to_json(),
          "increment": _for.increment.to_json(),
          "body": _for.body.to_json(),
        })
      },
      Statement::Method(method) => {
        json!({
          "type": "Method",
          "name": method.name.span.literal,
          "parameters": method.parameters.iter().map(|x| Statement::Variable(x.clone()).to_json()).collect::<Vec<serde_json::Value>>(),
          "body": method.body.iter().map(|x| x.to_json()).collect::<Vec<serde_json::Value>>(),
          "return_type": match &method.return_type {
            Some(return_type) => return_type.to_string(),
            None => String::new(),
          },
          "is_static": method.metadata.is_static,
          "is_public": method.metadata.is_public,
        })
      },
      Statement::Property(property) => {
        json!({
          "type": "Property",
          "name": property.name.span.literal,
          "initializer": match &property.initializer {
            Some(initializer) => initializer.to_json(),
            None => json!(null),
          },
          "type_annotation": property.type_annotation.to_string(),
          "is_static": property.metadata.is_static,
          "is_public": property.metadata.is_public,
        })
      },
      Statement::Interface(interface) => {
        json!({
          "type": "Interface",
          "name": interface.name.span.literal,
          "methods": interface.methods.iter().map(|x| x.to_json()).collect::<Vec<serde_json::Value>>(),
        })
      },
      Statement::Enum(enum_) => {
        json!({
          "type": "Enum",
          "name": enum_.name.span.literal,
          "members": enum_.members.iter().map(|x| Statement::Variable(x.clone()).to_json() ).collect::<Vec<serde_json::Value>>(),
          "is_exported": enum_.is_exported,
          "generic": match &enum_.generic {
            Some(generic) => generic.iter().map(|x| x.to_string()).collect::<Vec<String>>(),
            None => vec![],
          },
        })
      },
    }
  }
}
