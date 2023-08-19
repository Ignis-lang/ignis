use std::{cell::RefCell, rc::Rc, env::args_os};

use crate::{
  ast::{
    data_type::DataType,
    evaluator::{Evaluator, EvaluatorValue, EvaluatorResult},
    statement::function::FunctionStatement,
    environment::Environment,
  },
  diagnostic::error::DiagnosticError,
};

use super::Callable;

#[derive(Debug)]
pub struct Function {
  declaration: FunctionStatement,
  closure: Box<Environment>,
}

impl Clone for Function {
  fn clone(&self) -> Self {
    Self {
      declaration: self.declaration.clone(),
      closure: self.closure.clone(),
    }
  }
}

impl Function {
  pub fn new(declaration: FunctionStatement, closure: Box<Environment>) -> Self {
    Self {
      declaration,
      closure,
    }
  }
}

impl Callable for Function {
  fn call(
    &self,
    arguments: Vec<EvaluatorValue>,
    evaluator: &mut Box<Evaluator>,
  ) -> EvaluatorResult<EvaluatorValue> {
    let mut environment = Environment::new(Some(self.closure.clone()));

    for (i, parameter) in self.declaration.parameters.iter().enumerate() {
      match arguments.get(i) {
        Some(argument) => {
          if argument.to_data_type() != parameter.data_type {
            return Err(DiagnosticError::AssingInvalidType(
              argument.to_data_type(),
              parameter.data_type.clone(),
              parameter.name.clone(),
            ));
          }

          let _ = environment.define(
            parameter.name.span.literal.clone(),
            argument.to_variable_environment(),
          );
        }
        None => {
          return Err(DiagnosticError::MissingArgument(
            parameter.name.span.literal.clone(),
            parameter.name.clone(),
          ))
        }
      };
    }

    match evaluator.execute_block(&self.declaration.body, Rc::new(RefCell::new(environment)))? {
      EvaluatorValue::Return(value) => Ok(EvaluatorValue::Return(value)),
      _ => Ok(EvaluatorValue::Return(Box::new(EvaluatorValue::Null))),
    }
  }

  fn arity(&self) -> usize {
    self.declaration.parameters.len()
  }

  fn get_type(&self) -> Option<DataType> {
    self.declaration.return_type.clone()
  }

  fn clone_box(&self) -> Box<dyn Callable> {
    Box::new(self.clone())
  }
}
