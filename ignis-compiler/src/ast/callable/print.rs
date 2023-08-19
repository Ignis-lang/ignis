use crate::{
  ast::{
    expression::Expression,
    statement::function::FunctionStatement,
    evaluator::{Evaluator, EvaluatorResult, EvaluatorValue},
    data_type::DataType,
  },
  diagnostic::error::DiagnosticError,
};

use super::Callable;

#[derive(Debug, PartialEq, Clone)]
pub struct Println {}

impl Println {
  pub fn new() -> Self {
    Self {}
  }
}

impl Callable for Println {
  fn arity(&self) -> usize {
    return 1;
  }

  fn call(
    &self,
    arguments: Vec<EvaluatorValue>,
    evaluator: &mut Box<Evaluator>,
  ) -> EvaluatorResult<EvaluatorValue> {
    let mut value: String = String::new();

    for argument in arguments {
      match argument {
        EvaluatorValue::String(s) => value = s,
        EvaluatorValue::Int(i) => value = i.to_string(),
        EvaluatorValue::Double(d) => value = d.to_string(),
        EvaluatorValue::Boolean(b) => value = b.to_string(),
        EvaluatorValue::Return(r) => value = r.to_string(),
        EvaluatorValue::Null => value = "null".to_string(),
        EvaluatorValue::Callable(_) | EvaluatorValue::None => {
          return Err(DiagnosticError::InvalidArgumentType(argument))
        }
      };
    }

    println!("{}", value);

    Ok(EvaluatorValue::None)
  }

  fn get_type(&self) -> Option<DataType> {
    Some(DataType::Void)
  }

  fn clone_box(&self) -> Box<dyn Callable> {
    Box::new(self.clone())
  }
}
