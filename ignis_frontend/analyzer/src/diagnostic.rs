use ast::expression::variable::VariableExpression;
use enums::data_type::DataType;
use intermediate_representation::analyzer_value::AnalyzerValue;
use crate::token::Token;
use diagnostic_report::{DiagnosticReport, DiagnosticLevel};

#[derive(Debug, Clone)]
pub enum AnalyzerDiagnosticError {
  UndeclaredVariable(Token),
  InvalidUnaryOperatorForDataType(Token, AnalyzerValue),
  NotCallable(Token),
  InvalidNumberOfArguments(usize, usize, Token),
  AssingInvalidType(DataType, DataType, Token),
  InvalidArgumentType(AnalyzerValue, Token),
  MissingArgument(String, Token),
  InvalidComparison(AnalyzerValue, AnalyzerValue, Token),
  InvalidOperator(Token),
  InvalidUnaryOperator(Token),
  UndefinedVariable(Token),
  VariableAlreadyDefined(String, Token),
  InvalidAssignmentTarget(Token),
  InvalidReassignedVariable(Token),
  TypeMismatch(DataType, DataType, Token),
  TypeMismatchUnary(DataType, Token),
  CannotSubtract(AnalyzerValue, AnalyzerValue, Token),
  CannotMultiply(AnalyzerValue, AnalyzerValue, Token),
  CannotDivide(AnalyzerValue, AnalyzerValue, Token),
  CannotModulo(AnalyzerValue, AnalyzerValue, Token),
  FunctionAlreadyDefined(String, Token),
  ClassAlreadyDefined(String),
  ArgumentTypeMismatch(DataType, DataType, Token),
  ImmutableVariableAsMutableParameter(String, String, Token),
  ReturnOutsideFunction(Token),
  NotIterable(Token),
  ArrayElementTypeMismatch(Token),
  ModuleNotFound(Token),
  ImportedFunctionIsNotExported(Token),
  BreakOutsideLoop(Token),
  ContinueOutsideLoop(Token),
}

#[derive(Debug, Clone)]
pub struct AnalyzerDiagnostic {
  pub error: AnalyzerDiagnosticError,
  pub token_line: Vec<Token>,
}

impl AnalyzerDiagnostic {
  pub fn new(error: AnalyzerDiagnosticError, token_line: Vec<Token>) -> Self {
    Self { error, token_line }
  }

  pub fn report_diagnostic(&self) -> DiagnosticReport {
    match &self.error {
      AnalyzerDiagnosticError::UndeclaredVariable(token) => DiagnosticReport::new(
        format!("Undeclared variable '{}'", token.span.literal),
        Box::new(token.clone()),
        self.token_line.clone(),
        DiagnosticLevel::Error,
        None,
        "IA0001".to_string(),
      ),
      AnalyzerDiagnosticError::InvalidUnaryOperatorForDataType(token, value) => {
        DiagnosticReport::new(
          format!(
            "Invalid unary operator '{}' for data type '{}'",
            token.span.literal, value
          ),
          Box::new(token.clone()),
          self.token_line.clone(),
          DiagnosticLevel::Error,
          None,
          "IA0002".to_string(),
        )
      }
      AnalyzerDiagnosticError::NotCallable(token) => DiagnosticReport::new(
        format!("'{}' is not callable", token.span.literal),
        Box::new(token.clone()),
        self.token_line.clone(),
        DiagnosticLevel::Error,
        None,
        "IA0003".to_string(),
      ),
      AnalyzerDiagnosticError::InvalidNumberOfArguments(max, num, token) => DiagnosticReport::new(
        format!("Expected {} arguments, but got {} arguments", max, num),
        Box::new(token.clone()),
        self.token_line.clone(),
        DiagnosticLevel::Error,
        None,
        "IA0004".to_string(),
      ),
      AnalyzerDiagnosticError::AssingInvalidType(expected, target, token) => DiagnosticReport::new(
        format!("Cannot assign '{}' to '{}'", expected, target),
        Box::new(token.clone()),
        self.token_line.clone(),
        DiagnosticLevel::Error,
        None,
        "IA0005".to_string(),
      ),
      AnalyzerDiagnosticError::InvalidArgumentType(value, token) => DiagnosticReport::new(
        format!("Invalid argument type '{}'", value),
        Box::new(token.clone()),
        self.token_line.clone(),
        DiagnosticLevel::Error,
        None,
        "IA0006".to_string(),
      ),
      AnalyzerDiagnosticError::MissingArgument(name, toke) => DiagnosticReport::new(
        format!("Missing argument '{}'", name),
        Box::new(toke.clone()),
        self.token_line.clone(),
        DiagnosticLevel::Error,
        None,
        "IA0007".to_string(),
      ),
      AnalyzerDiagnosticError::InvalidComparison(left, right, operator) => DiagnosticReport::new(
        format!("Invalid comparison between '{}' and '{}'", left, right),
        Box::new(operator.clone()),
        self.token_line.clone(),
        DiagnosticLevel::Error,
        None,
        "IA0008".to_string(),
      ),
      AnalyzerDiagnosticError::InvalidOperator(token) => DiagnosticReport::new(
        format!("Invalid operator '{}'", token.span.literal),
        Box::new(token.clone()),
        self.token_line.clone(),
        DiagnosticLevel::Error,
        None,
        "IA0009".to_string(),
      ),
      AnalyzerDiagnosticError::InvalidUnaryOperator(token) => DiagnosticReport::new(
        format!("Invalid unary operator '{}'", token.span.literal),
        Box::new(token.clone()),
        self.token_line.clone(),
        DiagnosticLevel::Error,
        None,
        "IA0010".to_string(),
      ),
      AnalyzerDiagnosticError::UndefinedVariable(token) => DiagnosticReport::new(
        format!("Undefined variable '{}'", token.span.literal),
        Box::new(token.clone()),
        self.token_line.clone(),
        DiagnosticLevel::Error,
        None,
        "IA0011".to_string(),
      ),
      AnalyzerDiagnosticError::VariableAlreadyDefined(name, token) => DiagnosticReport::new(
        format!("Variable '{}' already defined", name),
        Box::new(token.clone()),
        self.token_line.clone(),
        DiagnosticLevel::Error,
        None,
        "IA0012".to_string(),
      ),
      AnalyzerDiagnosticError::InvalidAssignmentTarget(token) => DiagnosticReport::new(
        "Invalid assignment target".to_string(),
        Box::new(token.clone()),
        self.token_line.clone(),
        DiagnosticLevel::Error,
        None,
        "IA0013".to_string(),
      ),
      AnalyzerDiagnosticError::InvalidReassignedVariable(token) => DiagnosticReport::new(
        format!("Cannot reassign variable '{}'", token.span.literal),
        Box::new(token.clone()),
        self.token_line.clone(),
        DiagnosticLevel::Error,
        None,
        "IA0014".to_string(),
      ),
      AnalyzerDiagnosticError::TypeMismatch(left, right, token) => DiagnosticReport::new(
        format!("Type mismatch between '{}' and '{}'", left, right),
        Box::new(token.clone()),
        self.token_line.clone(),
        DiagnosticLevel::Error,
        None,
        "IA0015".to_string(),
      ),
      AnalyzerDiagnosticError::TypeMismatchUnary(_, _) => todo!(),
      AnalyzerDiagnosticError::CannotSubtract(_, _, _) => todo!(),
      AnalyzerDiagnosticError::CannotMultiply(_, _, _) => todo!(),
      AnalyzerDiagnosticError::CannotDivide(_, _, _) => todo!(),
      AnalyzerDiagnosticError::CannotModulo(_, _, _) => todo!(),
      AnalyzerDiagnosticError::FunctionAlreadyDefined(_, _) => todo!(),
      AnalyzerDiagnosticError::ClassAlreadyDefined(_) => todo!(),
      AnalyzerDiagnosticError::ArgumentTypeMismatch(_, _, _) => todo!(),
      AnalyzerDiagnosticError::ImmutableVariableAsMutableParameter(_, _, _) => todo!(),
      AnalyzerDiagnosticError::ReturnOutsideFunction(_) => todo!(),
      AnalyzerDiagnosticError::NotIterable(_) => todo!(),
      AnalyzerDiagnosticError::ArrayElementTypeMismatch(_) => todo!(),
      AnalyzerDiagnosticError::ModuleNotFound(_) => todo!(),
      AnalyzerDiagnosticError::ImportedFunctionIsNotExported(_) => todo!(),
      AnalyzerDiagnosticError::BreakOutsideLoop(_) => todo!(),
      AnalyzerDiagnosticError::ContinueOutsideLoop(_) => todo!(),
    }
  }
}
