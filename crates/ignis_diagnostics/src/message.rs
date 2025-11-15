use std::fmt;

use ignis_data_type::DataType;
use ignis_token::token_types::TokenType;
use ignis_type::span::Span;

use super::diagnostic_report::{Severity, Diagnostic};

#[derive(Clone, Debug, PartialEq)]
pub enum Expected {
  Token(TokenType),
  Keyword(&'static str),
  Thing(&'static str),
}

impl fmt::Display for Expected {
  fn fmt(
    &self,
    f: &mut fmt::Formatter<'_>,
  ) -> fmt::Result {
    match self {
      Expected::Token(t) => write!(f, "{:?}", t),
      Expected::Keyword(k) => write!(f, "keyword '{}'", k),
      Expected::Thing(t) => write!(f, "{}", t),
    }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum DiagnosticMessage {
  // #region Lexer
  ExpectedToken {
    expected: Expected,
    at: Span,
  },
  UnexpectedToken {
    at: Span,
  },
  InvalidToken(Span),
  UnterminatedComment(Span),
  UnterminatedCharacter(Span),
  InvalidCharacterEscapeSequence(Span),
  InvalidCharacter(Span),
  UnterminatedString(Span),
  ExpectedInteger(Span),
  ExpectedFloat(Span),
  ExpectedHex(Span),
  ExpectedBinary(Span),
  // #endregion Lexer
  // #region Parser
  ExpectedExpression(Span),
  ExpectedVariableName(Span),
  ExpectedReturnTypeAfterFunction(Span),
  ExpectedAfterExpression {
    expected: Expected,
    at: Span,
  },
  ExpectedExpressionAfter(Span),
  InvalidAssignmentTarget(Span),
  ExpectedTypeAfterVariable(Span),
  InvalidNumberOfArguments {
    expected: usize,
    got: usize,
    at: Span,
  },
  ExpectedSemicolonAfterExpression(Span),
  InvalidEnumMember(Span),
  ExpectedTypeAfterPipe(Span),
  ExpectedDelimiter(Span),
  ExpectedType(Span),
  UnexpectedKeyword(Span),
  ExpectedIdentifier(Span),
  UninitializedConstant(Span),
  ExpectedPattern(Span),
  UnexpectedGenerics(Span),
  UndefinedMeta(Span),
  InvalidMetaEntity {
    expected: Expected,
    at: Span,
  },
  MissingArgument(Span),
  InvalidArgumentType {
    expected: DataType,
    at: Span,
  },
  TypeMismatch {
    expected: DataType,
    got: DataType,
    at: Span,
  },
  InvalidProperty(Span),
  // #endregion Parser
  // #region Analyzer
  UndeclaredVariable {
    name: String,
    span: Span,
  },
  VariableAlreadyDefined {
    name: String,
    span: Span,
  },
  FunctionAlreadyDefined {
    name: String,
    span: Span,
  },
  UndefinedType {
    name: String,
    span: Span,
  },
  // #endregion Analyzer
}

impl fmt::Display for DiagnosticMessage {
  fn fmt(
    &self,
    f: &mut fmt::Formatter<'_>,
  ) -> fmt::Result {
    match self {
      // Lexer
      DiagnosticMessage::UnterminatedComment(_) => write!(f, "Unterminated comment"),
      DiagnosticMessage::UnterminatedCharacter(_) => write!(f, "Unterminated character literal"),
      DiagnosticMessage::InvalidCharacterEscapeSequence(_) => write!(f, "Invalid character escape sequence"),
      DiagnosticMessage::InvalidCharacter(_) => write!(f, "Invalid character"),
      DiagnosticMessage::UnterminatedString(_) => write!(f, "Unterminated string literal"),
      DiagnosticMessage::ExpectedInteger(_) => write!(f, "Expected integer literal"),
      DiagnosticMessage::ExpectedFloat(_) => write!(f, "Expected float literal"),
      DiagnosticMessage::ExpectedHex(_) => write!(f, "Expected hexadecimal literal"),
      DiagnosticMessage::ExpectedBinary(_) => write!(f, "Expected binary literal"),
      DiagnosticMessage::ExpectedToken { expected, .. } => write!(f, "Expected {}", expected),
      DiagnosticMessage::UnexpectedToken { .. } => write!(f, "Unexpected token"),
      DiagnosticMessage::InvalidToken(_) => write!(f, "Invalid token"),

      // Parser
      DiagnosticMessage::ExpectedExpression(_) => write!(f, "Expected expression"),
      DiagnosticMessage::ExpectedVariableName(_) => write!(f, "Expected variable name"),
      DiagnosticMessage::ExpectedReturnTypeAfterFunction(_) => write!(f, "Expected return type after function"),
      DiagnosticMessage::ExpectedAfterExpression { expected, .. } => write!(f, "Expected {} in expression", expected),
      DiagnosticMessage::ExpectedExpressionAfter(_) => write!(f, "Expected expression"),
      DiagnosticMessage::InvalidAssignmentTarget(_) => write!(f, "Invalid assignment target"),
      DiagnosticMessage::ExpectedTypeAfterVariable(_) => write!(f, "Expected type after variable"),
      DiagnosticMessage::InvalidNumberOfArguments { expected, got, .. } => {
        write!(f, "Expected {} arguments, but got {}", expected, got)
      },
      DiagnosticMessage::ExpectedSemicolonAfterExpression(_) => write!(f, "Expected ';' after expression"),
      DiagnosticMessage::InvalidEnumMember(_) => write!(f, "Invalid enum member"),
      DiagnosticMessage::ExpectedTypeAfterPipe(_) => write!(f, "Expected type after '|'"),
      DiagnosticMessage::ExpectedDelimiter(_) => write!(f, "Expected ',' or '}}'"),
      DiagnosticMessage::ExpectedType(_) => write!(f, "Expected type"),
      DiagnosticMessage::UnexpectedKeyword(_) => write!(f, "Unexpected keyword"),
      DiagnosticMessage::ExpectedIdentifier(_) => write!(f, "Expected identifier"),
      DiagnosticMessage::UninitializedConstant(_) => write!(f, "Constant must be initialized with a value"),
      DiagnosticMessage::ExpectedPattern(_) => write!(f, "Expected pattern"),
      DiagnosticMessage::UnexpectedGenerics(_) => write!(f, "Unexpected generics"),
      DiagnosticMessage::UndefinedMeta(_) => write!(f, "Undefined meta"),
      DiagnosticMessage::InvalidMetaEntity { expected, .. } => write!(f, "Invalid meta entity, expected {}", expected),
      DiagnosticMessage::MissingArgument(_) => write!(f, "Missing argument for meta"),
      DiagnosticMessage::InvalidArgumentType { expected, .. } => {
        write!(f, "Invalid argument type, expected '{}'", expected)
      },
      DiagnosticMessage::TypeMismatch { expected, got, .. } => {
        write!(f, "Type mismatch. Expected '{}', but got '{}'", expected, got)
      },
      DiagnosticMessage::InvalidProperty(_) => write!(f, "Invalid property"),

      // Analyzer
      DiagnosticMessage::UndeclaredVariable { name, .. } => write!(f, "Undeclared variable '{}'", name),
      DiagnosticMessage::VariableAlreadyDefined { name, .. } => write!(f, "Variable '{}' is already defined", name),
      DiagnosticMessage::FunctionAlreadyDefined { name, .. } => write!(f, "Function '{}' is already defined", name),
      DiagnosticMessage::UndefinedType { name, .. } => write!(f, "Undefined type '{}'", name),
    }
  }
}

impl DiagnosticMessage {
  pub fn primary_span(&self) -> Span {
    match self {
      DiagnosticMessage::ExpectedToken { at, .. }
      | DiagnosticMessage::UnexpectedToken { at, .. }
      | DiagnosticMessage::InvalidToken(at)
      | DiagnosticMessage::UnterminatedComment(at)
      | DiagnosticMessage::UnterminatedCharacter(at)
      | DiagnosticMessage::InvalidCharacterEscapeSequence(at)
      | DiagnosticMessage::InvalidCharacter(at)
      | DiagnosticMessage::UnterminatedString(at)
      | DiagnosticMessage::ExpectedInteger(at)
      | DiagnosticMessage::ExpectedFloat(at)
      | DiagnosticMessage::ExpectedHex(at)
      | DiagnosticMessage::ExpectedBinary(at)
      | DiagnosticMessage::ExpectedExpression(at)
      | DiagnosticMessage::ExpectedVariableName(at)
      | DiagnosticMessage::ExpectedReturnTypeAfterFunction(at)
      | DiagnosticMessage::ExpectedAfterExpression { at, .. }
      | DiagnosticMessage::ExpectedExpressionAfter(at)
      | DiagnosticMessage::InvalidAssignmentTarget(at)
      | DiagnosticMessage::ExpectedTypeAfterVariable(at)
      | DiagnosticMessage::InvalidNumberOfArguments { at, .. }
      | DiagnosticMessage::ExpectedSemicolonAfterExpression(at)
      | DiagnosticMessage::InvalidEnumMember(at)
      | DiagnosticMessage::ExpectedTypeAfterPipe(at)
      | DiagnosticMessage::ExpectedDelimiter(at)
      | DiagnosticMessage::ExpectedType(at)
      | DiagnosticMessage::UnexpectedKeyword(at)
      | DiagnosticMessage::ExpectedIdentifier(at)
      | DiagnosticMessage::UninitializedConstant(at)
      | DiagnosticMessage::ExpectedPattern(at)
      | DiagnosticMessage::UnexpectedGenerics(at)
      | DiagnosticMessage::UndefinedMeta(at)
      | DiagnosticMessage::InvalidMetaEntity { at, .. }
      | DiagnosticMessage::MissingArgument(at)
      | DiagnosticMessage::InvalidArgumentType { at, .. }
      | DiagnosticMessage::TypeMismatch { at, .. }
      | DiagnosticMessage::InvalidProperty(at) => at.clone(),

      DiagnosticMessage::UndeclaredVariable { span, .. }
      | DiagnosticMessage::VariableAlreadyDefined { span, .. }
      | DiagnosticMessage::FunctionAlreadyDefined { span, .. }
      | DiagnosticMessage::UndefinedType { span, .. } => span.clone(),
    }
  }

  pub fn code(&self) -> String {
    match self {
      DiagnosticMessage::ExpectedToken { .. } => "I0001",
      DiagnosticMessage::UnexpectedToken { .. } => "I0002",
      DiagnosticMessage::InvalidToken(_) => "I0003",
      DiagnosticMessage::ExpectedExpression(_) => "I0004",
      DiagnosticMessage::ExpectedVariableName(_) => "I0005",
      DiagnosticMessage::ExpectedReturnTypeAfterFunction(_) => "I0006",
      DiagnosticMessage::ExpectedAfterExpression { .. } => "I0007",
      DiagnosticMessage::ExpectedExpressionAfter(_) => "I0008",
      DiagnosticMessage::InvalidAssignmentTarget(_) => "I0009",
      DiagnosticMessage::ExpectedTypeAfterVariable(_) => "I0010",
      DiagnosticMessage::InvalidNumberOfArguments { .. } => "I0011",
      DiagnosticMessage::ExpectedSemicolonAfterExpression(_) => "I0012",
      DiagnosticMessage::InvalidEnumMember(_) => "I0013",
      DiagnosticMessage::ExpectedTypeAfterPipe(_) => "I0014",
      DiagnosticMessage::ExpectedDelimiter(_) => "I0015",
      DiagnosticMessage::ExpectedType(_) => "I0016",
      DiagnosticMessage::UnexpectedKeyword(_) => "I0017",
      DiagnosticMessage::ExpectedIdentifier(_) => "I0018",
      DiagnosticMessage::UninitializedConstant(_) => "I0019",
      DiagnosticMessage::ExpectedPattern(_) => "I0020",
      DiagnosticMessage::UnterminatedComment(_) => "I0021",
      DiagnosticMessage::UnterminatedCharacter(_) => "I0022",
      DiagnosticMessage::InvalidCharacterEscapeSequence(_) => "I0023",
      DiagnosticMessage::InvalidCharacter(_) => "I0024",
      DiagnosticMessage::UnterminatedString(_) => "I0025",
      DiagnosticMessage::UnexpectedGenerics(_) => "I0026",
      DiagnosticMessage::UndefinedMeta(_) => "I0027",
      DiagnosticMessage::InvalidMetaEntity { .. } => "I0028",
      DiagnosticMessage::MissingArgument(_) => "I0029",
      DiagnosticMessage::InvalidArgumentType { .. } => "I0030",
      DiagnosticMessage::TypeMismatch { .. } => "I0031",
      DiagnosticMessage::InvalidProperty(_) => "I0032",
      DiagnosticMessage::UndeclaredVariable { .. } => "I0033",
      DiagnosticMessage::VariableAlreadyDefined { .. } => "I0041",
      DiagnosticMessage::FunctionAlreadyDefined { .. } => "I0042",
      DiagnosticMessage::UndefinedType { .. } => "I0043",
      DiagnosticMessage::ExpectedInteger(_) => "I0044",
      DiagnosticMessage::ExpectedFloat(_) => "I0045",
      DiagnosticMessage::ExpectedHex(_) => "I0046",
      DiagnosticMessage::ExpectedBinary(_) => "I0047",
    }
    .to_string()
  }

  fn level(&self) -> Severity {
    Severity::Error
  }

  pub fn report(&self) -> Diagnostic {
    Diagnostic::new(self.level(), self.to_string(), self.code(), self.primary_span())
  }
}
