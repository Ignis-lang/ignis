use {
  lexer::{token::Token, text_span::TextSpan},
  ast::expression::variable::VariableExpression,
  enums::{data_type::DataType, token_type::TokenType},
  analyzer::analyzer_error::AnalyzerDiagnosticError,
  parser::ParserDiagnosticError,
};

use analyzer::analyzer_value::AnalyzerValue;

use super::DiagnosticList;

#[derive(Debug)]
pub enum DiagnosticError {
  // Parser
  ExpectedExpression(Token),
  ExpectedToken(TokenType, Token),
  ExpectedVariableName(Token),
  ExpectedReturnTypeAfterFunction(Token),
  ExpectedAfterExpression(TokenType, Token, Token),
  ExpectedExpressionAfter(Token),
  UnexpectedToken(TokenType, Token),
  InvalidAssignmentTarget(TextSpan),
  ExpectedTypeAfterVariable(Token),

  // Analyzer | Evaluator
  UndeclaredVariable(VariableExpression),
  InvalidUnaryOperatorForDataType(Token, AnalyzerValue),
  NotCallable(Token),
  InvalidNumberOfArguments(usize, usize, Token),
  AssingInvalidType(DataType, DataType, Token),
  InvalidArgumentType(AnalyzerValue),
  MissingArgument(String, Token),
  InvalidComparison(AnalyzerValue, AnalyzerValue, Token),
  InvalidOperator(Token),
  InvalidUnaryOperator(Token),
  UndefinedVariable(Token),
  VariableAlreadyDefined(String, DataType),
  InvalidReassignedVariable(TextSpan),
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
  BreakOutsideLoop(Token),
  ContinueOutsideLoop(Token),
  NotIterable(Token),
  ArrayElementTypeMismatch(Token),
  ModuleNotFound(Token),
  ImportedFunctionIsNotExported(Token),
  InvalidCondition(Token),
  NotAClass(Token),
  UndefinedProperty(Token),
}

impl DiagnosticError {
  pub fn from_evaluator_error(error: AnalyzerDiagnosticError) -> Self {
    match error {
      AnalyzerDiagnosticError::UndeclaredVariable(expression) => {
        DiagnosticError::UndeclaredVariable(expression)
      }
      AnalyzerDiagnosticError::InvalidUnaryOperatorForDataType(operator, right) => {
        DiagnosticError::InvalidUnaryOperatorForDataType(operator, right)
      }
      AnalyzerDiagnosticError::NotCallable(token) => DiagnosticError::NotCallable(token),
      AnalyzerDiagnosticError::InvalidNumberOfArguments(arity, arguments, token) => {
        DiagnosticError::InvalidNumberOfArguments(arity, arguments, token)
      }
      AnalyzerDiagnosticError::AssingInvalidType(argument, data_type, name) => {
        DiagnosticError::AssingInvalidType(argument, data_type, name)
      }
      AnalyzerDiagnosticError::InvalidArgumentType(argument) => {
        DiagnosticError::InvalidArgumentType(argument)
      }
      AnalyzerDiagnosticError::MissingArgument(name, token) => {
        DiagnosticError::MissingArgument(name, token)
      }
      AnalyzerDiagnosticError::InvalidComparison(left, right, token) => {
        DiagnosticError::InvalidComparison(left, right, token)
      }
      AnalyzerDiagnosticError::InvalidOperator(token) => DiagnosticError::InvalidOperator(token),
      AnalyzerDiagnosticError::InvalidUnaryOperator(token) => {
        DiagnosticError::InvalidUnaryOperator(token)
      }
      AnalyzerDiagnosticError::UndefinedVariable(token) => {
        DiagnosticError::UndefinedVariable(token)
      }
      AnalyzerDiagnosticError::VariableAlreadyDefined(name, data_type) => {
        DiagnosticError::VariableAlreadyDefined(name, data_type)
      }
      AnalyzerDiagnosticError::InvalidAssignmentTarget(span) => {
        DiagnosticError::InvalidAssignmentTarget(span)
      }
      AnalyzerDiagnosticError::InvalidReassignedVariable(span) => {
        DiagnosticError::InvalidReassignedVariable(span)
      }
      AnalyzerDiagnosticError::TypeMismatch(left, right, token) => {
        DiagnosticError::TypeMismatch(left, right, token)
      }
      AnalyzerDiagnosticError::CannotSubtract(left, right, token) => {
        DiagnosticError::CannotSubtract(left, right, token)
      }
      AnalyzerDiagnosticError::CannotMultiply(left, right, token) => {
        DiagnosticError::CannotMultiply(left, right, token)
      }
      AnalyzerDiagnosticError::CannotDivide(left, right, token) => {
        DiagnosticError::CannotDivide(left, right, token)
      }
      AnalyzerDiagnosticError::CannotModulo(left, right, token) => {
        DiagnosticError::CannotModulo(left, right, token)
      }
      AnalyzerDiagnosticError::FunctionAlreadyDefined(name, token) => {
        DiagnosticError::FunctionAlreadyDefined(name, token)
      }
      AnalyzerDiagnosticError::ClassAlreadyDefined(name) => {
        DiagnosticError::ClassAlreadyDefined(name)
      }
      AnalyzerDiagnosticError::ArgumentTypeMismatch(expected, received, token) => {
        DiagnosticError::ArgumentTypeMismatch(expected, received, token)
      }
      AnalyzerDiagnosticError::TypeMismatchUnary(right, token) => {
        DiagnosticError::TypeMismatchUnary(right, token)
      }
      AnalyzerDiagnosticError::ImmutableVariableAsMutableParameter(
        parameter_name,
        variable_name,
        token,
      ) => {
        DiagnosticError::ImmutableVariableAsMutableParameter(parameter_name, variable_name, token)
      }
      AnalyzerDiagnosticError::ReturnOutsideFunction(token) => {
        DiagnosticError::ReturnOutsideFunction(token)
      }
      AnalyzerDiagnosticError::NotIterable(token) => DiagnosticError::NotIterable(token),
      AnalyzerDiagnosticError::ArrayElementTypeMismatch(token) => {
        DiagnosticError::ArrayElementTypeMismatch(token)
      }
      AnalyzerDiagnosticError::ModuleNotFound(token) => DiagnosticError::ModuleNotFound(token),
      AnalyzerDiagnosticError::ImportedFunctionIsNotExported(token) => {
        DiagnosticError::ImportedFunctionIsNotExported(token)
      }
      AnalyzerDiagnosticError::BreakOutsideLoop(token) => DiagnosticError::BreakOutsideLoop(token),
      AnalyzerDiagnosticError::ContinueOutsideLoop(token) => {
        DiagnosticError::ContinueOutsideLoop(token)
      }
      AnalyzerDiagnosticError::InvalidCondition(token) => DiagnosticError::InvalidCondition(token),
      AnalyzerDiagnosticError::NotAClass(_) => todo!(),
      AnalyzerDiagnosticError::UndefinedProperty(_) => todo!(),
    }
  }

  pub fn from_parser_diagnostic(errors: Vec<ParserDiagnosticError>) -> Vec<Self> {
    let mut diagnostics: Vec<Self> = Vec::new();

    for error in errors {
      match error {
        ParserDiagnosticError::ExpectedExpression(token) => {
          diagnostics.push(DiagnosticError::ExpectedExpression(token));
        }
        ParserDiagnosticError::ExpectedToken(kind, token) => {
          diagnostics.push(DiagnosticError::ExpectedToken(kind, token));
        }
        ParserDiagnosticError::ExpectedVariableName(token) => {
          diagnostics.push(DiagnosticError::ExpectedVariableName(token));
        }
        ParserDiagnosticError::ExpectedReturnTypeAfterFunction(token) => {
          diagnostics.push(DiagnosticError::ExpectedReturnTypeAfterFunction(token));
        }
        ParserDiagnosticError::ExpectedAfterExpression(kind, expression, token) => {
          diagnostics.push(DiagnosticError::ExpectedAfterExpression(
            kind, expression, token,
          ));
        }
        ParserDiagnosticError::ExpectedExpressionAfter(token) => {
          diagnostics.push(DiagnosticError::ExpectedExpressionAfter(token));
        }
        ParserDiagnosticError::UnexpectedToken(kind, token) => {
          diagnostics.push(DiagnosticError::UnexpectedToken(kind, token));
        }
        ParserDiagnosticError::InvalidAssignmentTarget(span) => {
          diagnostics.push(DiagnosticError::InvalidAssignmentTarget(span));
        }
        ParserDiagnosticError::ExpectedTypeAfterVariable(token) => {
          diagnostics.push(DiagnosticError::ExpectedTypeAfterVariable(token));
        }
        ParserDiagnosticError::InvalidNumberOfArguments(arity, arguments, token) => {
          diagnostics.push(DiagnosticError::InvalidNumberOfArguments(
            arity, arguments, token,
          ));
        }
      };
    }

    diagnostics
  }

  pub fn report(&self, diagnostics: &mut DiagnosticList) {
    match self {
      DiagnosticError::ExpectedExpression(token) => {
        diagnostics.report_expected_expression(&token);
      }
      DiagnosticError::ExpectedToken(kind, token) => {
        diagnostics.report_expected_token(&kind, &token);
      }
      DiagnosticError::ExpectedVariableName(token) => {
        diagnostics.report_expected_variable_name(&token);
      }
      DiagnosticError::ExpectedReturnTypeAfterFunction(token) => {
        diagnostics.report_expected_return_type_after_function(&token);
      }
      DiagnosticError::ExpectedAfterExpression(kind, expression, token) => {
        diagnostics.report_expected_after_expression(&kind, &expression, &token);
      }
      DiagnosticError::ExpectedExpressionAfter(token) => {
        diagnostics.report_expected_expression(&token);
      }
      DiagnosticError::UnexpectedToken(kind, token) => {
        diagnostics.report_unexpected_token(&kind, &token);
      }
      DiagnosticError::InvalidAssignmentTarget(span) => {
        diagnostics.report_invalid_assignment_target(&span);
      }
      DiagnosticError::UndeclaredVariable(expression) => {
        diagnostics.report_undeclared_variable(&expression);
      }
      DiagnosticError::InvalidUnaryOperatorForDataType(operator, right) => {
        diagnostics.report_invalid_unary_operator_for_data_type(&operator, &right);
      }
      DiagnosticError::NotCallable(expression) => {
        diagnostics.report_not_callable(&expression);
      }
      DiagnosticError::InvalidNumberOfArguments(arity, arguments, token) => {
        diagnostics.report_invalid_number_of_arguments(&arity, &arguments, &token);
      }
      DiagnosticError::AssingInvalidType(argument, data_type, name) => {
        diagnostics.report_assing_invalid_type(&argument, &data_type, &name);
      }
      DiagnosticError::InvalidArgumentType(argument) => {
        diagnostics.report_invalid_argument_type(&argument);
      }
      DiagnosticError::MissingArgument(name, token) => {
        diagnostics.report_missing_argument(&name, &token.clone());
      }
      DiagnosticError::InvalidComparison(left, right, token) => {
        diagnostics.report_invalid_comparison(&left, &right, &token);
      }
      DiagnosticError::InvalidOperator(token) => {
        diagnostics.report_invalid_operator(&token);
      }
      DiagnosticError::InvalidUnaryOperator(token) => {
        diagnostics.report_invalid_unary_operator(&token);
      }
      DiagnosticError::UndefinedVariable(token) => {
        diagnostics.report_undefined_variable(&token);
      }
      DiagnosticError::VariableAlreadyDefined(name, data_type) => {
        diagnostics.report_variable_already_defined(name, data_type);
      }
      DiagnosticError::ExpectedTypeAfterVariable(token) => {
        diagnostics.report_expected_type_after_variable(&token);
      }
      DiagnosticError::InvalidReassignedVariable(span) => {
        diagnostics.report_invalid_reassigned_variable(&span);
      }
      DiagnosticError::TypeMismatch(left, right, token) => {
        diagnostics.report_type_mismatch(&left, &right, &token);
      }
      DiagnosticError::CannotSubtract(left, right, token) => {
        diagnostics.report_cannot_subtract(&left, &right, &token);
      }
      DiagnosticError::CannotMultiply(left, right, token) => {
        diagnostics.report_cannot_multiply(&left, &right, &token);
      }
      DiagnosticError::CannotDivide(left, right, token) => {
        diagnostics.report_cannot_divide(&left, &right, &token);
      }
      DiagnosticError::CannotModulo(left, right, token) => {
        diagnostics.report_cannot_modulo(&left, &right, &token);
      }
      DiagnosticError::FunctionAlreadyDefined(name, token) => {
        diagnostics.report_function_already_defined(&name, token);
      }
      DiagnosticError::ClassAlreadyDefined(name) => {
        diagnostics.report_class_already_defined(&name);
      }
      DiagnosticError::ArgumentTypeMismatch(expected, recived, token) => {
        diagnostics.report_argument_type_mismatch(&expected, &recived, &token);
      }
      DiagnosticError::TypeMismatchUnary(right, token) => {
        diagnostics.report_type_mismatch_unary(&right, &token);
      }
      DiagnosticError::ImmutableVariableAsMutableParameter(
        parameter_name,
        variable_name,
        token,
      ) => {
        diagnostics.report_immutable_variable_as_mutable_parameter(
          &parameter_name,
          &variable_name,
          &token,
        );
      }
      DiagnosticError::ReturnOutsideFunction(token) => {
        diagnostics.report_return_outside_function(&token);
      }
      DiagnosticError::NotIterable(token) => {
        diagnostics.report_not_iterable(token);
      }
      DiagnosticError::ArrayElementTypeMismatch(token) => {
        diagnostics.report_array_element_type_mismatch(token);
      }
      DiagnosticError::ModuleNotFound(token) => {
        diagnostics.report_module_not_found(token);
      }
      DiagnosticError::ImportedFunctionIsNotExported(token) => {
        diagnostics.report_imported_function_is_not_exported(token);
      }
      DiagnosticError::BreakOutsideLoop(token) => {
        diagnostics.report_break_outside_loop(token);
      }
      DiagnosticError::ContinueOutsideLoop(token) => {
        diagnostics.report_continue_outside_loop(token);
      }
      DiagnosticError::InvalidCondition(token) => {
        diagnostics.report_invalid_condition(token);
      }
        DiagnosticError::NotAClass(_) => todo!(),
        DiagnosticError::UndefinedProperty(_) => todo!(),
    }
  }
}
