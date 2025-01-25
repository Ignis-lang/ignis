use std::fmt;

use ignis_data_type::{value::IgnisLiteralValue, DataType};
use ignis_token::{token::Token, token_types::TokenType};

use super::diagnostic_report::{DiagnosticLevel, DiagnosticReport};

#[derive(Debug, Clone, PartialEq)]
pub enum DiagnosticMessage {
  // #region Lexer
  ExpectedToken(TokenType, Token),
  UnexpectedToken(TokenType, Token),
  InvalidToken(Token),
  UntermintedComment(Token),
  UntermintedCharacter(Token),
  InvalidCharacterEscapeSequence(Token),
  InvalidCharacter(Token),
  UnterminatedString(Token),
  // #endregion Lexer
  // #region Parser
  ExpectedExpression(Token),
  ExpectedVariableName(Token),
  ExpectedReturnTypeAfterFunction(Token),
  ExpectedAfterExpression(Box<TokenType>, Token, Box<Token>),
  ExpectedExpressionAfter(Token),
  InvalidAssignmentTarget(Token),
  ExpectedTypeAfterVariable(Token),
  InvalidNumberOfArguments(usize, usize, Token),
  ExpectedSemicolonAfterExpression(Token),
  InvalidEnumMember(Token),
  ExpectedTypeAfterPipe(Token),
  ExpectedDelimiter(Token),
  ExpectedType(Token),
  UnexpectedKeyword(Token),
  ExpectedIdentifier(Token),
  UninitializedConstant(Token),
  ExpectedPattern(Token),
  UnexpectedGenerics(Token),
  UndefinedMeta(Token),
  InvalidMetaEntity(TokenType, Token),
  MissingArgument(Token),
  InvalidArgumentType(DataType, Token),
  TypeMismatch(DataType, DataType, Token),
  InvalidProperty(Token),
  // #endregion Parser
  // #region Analyzer
  UndeclaredVariable(Token),
  InvalidUnaryOperatorForDataType(Token, IgnisLiteralValue),
  NotCallable(Token),
  AssingInvalidType(DataType, DataType, Token),
  InvalidComparison(IgnisLiteralValue, IgnisLiteralValue, Token),
  InvalidOperator(Token),
  InvalidUnaryOperator(Token),
  UndefinedVariable(Token),
  VariableAlreadyDefined(String, Token),
  InvalidReassignedVariable(Token),
  TypeMismatchUnary(DataType, Token),
  CannotSubtract(IgnisLiteralValue, IgnisLiteralValue, Token),
  CannotMultiply(IgnisLiteralValue, IgnisLiteralValue, Token),
  CannotDivide(IgnisLiteralValue, IgnisLiteralValue, Token),
  CannotModulo(IgnisLiteralValue, IgnisLiteralValue, Token),
  FunctionAlreadyDefined(String, Token),
  ClassAlreadyDefined(Token),
  ArgumentTypeMismatch(DataType, DataType, Token),
  ImmutableVariableAsMutableParameter(Token, Token, Token),
  ReturnOutsideFunction(Token),
  NotIterable(Token),
  VectorElementTypeMismatch(Token),
  ModuleNotFound(Token),
  ImportedFunctionIsNotExported(Token),
  BreakOutsideLoop(Token),
  ContinueOutsideLoop(Token),
  InvalidCondition(Token),
  PropertyOutsideClass(Token),
  PropertyAlreadyDefined(Token),
  MethodOutsideClass(Token),
  MethodAlreadyDefined(Token),
  UndefinedProperty(Token),
  NotAClass(Token),
  UndefinedClass(Token, Token),
  UndefinedMethods(Token),
  ImmutableProperty(Token),
  PrivateProperty(Token),
  FunctionNotDefined(String, Token),
  VariableNeverUsed(Token),
  NotAnVector(Token),
  InvalidVectorIndex(Token),
  ThisOutsideOfClass(Token),
  ImportedClassIsNotExported(Token),
  InvalidVariableInitializer(Token),
  EnumAlreadyDefined(Token),
  EnumMemberTypeMismatch(Token, DataType, DataType),
  NotAnEnum(Token),
  UndefinedEnum(Token),
  UndefinedEnumMember(Token),
  EnumAssignmentError(Token, String),
  UndefinedImport(Token),
  PotentialTypeMismatch(DataType, DataType, Token),
  IncorrectNumberOfGenericArguments(Token, usize, usize),
  InvalidTypeArgument(DataType, Box<DataType>, Token),
  InterfaceAlreadyDefined(Token),
  UndefinedInterface(Token),
  InvalidInterfaceMethod(Token),
  MethodImplementationParameterMismatch(Token, DataType, DataType),
  MethodImplementationError(Token, Token),
  MethodImplementationReturnTypeMismatch(Token, DataType, DataType),
  UnimplementedInterfaceMethod(String, Token),
  UnimplementedInterfaceMethods(Vec<String>, Token),
  TypeAlreadyDefined(Token),
  InvalidTypeValue(Token),
  UndefinedType(String, Token),
  TupleTypeMismatch(DataType, DataType, Token),
  ReturnTypeMismatch(DataType, DataType, Token),
  ImportedEnumIsNotExported(Token),
  ImportedClassNotFound(Token, String),
  ImportedTypeIsNotExported(Token),
  ImportedTypeNotFound(Token, Box<Token>),
  InvalidCast(Token, DataType, DataType),
  RecordAlreadyDefined(Token),
  UndefinedRecord(Token),
  ConstantAlreadyDefined(Token),
  ReassignedConstant(Token),
  DecoratorAlreadyDefined(Token),
  UndefinedDecorator(Token),
  ImportedRecordIsNotExported(Token),
  ImportedDecoratorIsNotExported(Token),
  InvalidExternStatement(Token),
  ImportedExternIsNotExported(Token),
  ValueOutOfRange(IgnisLiteralValue, DataType, Token),
  InvalidThis(Token),
  MutationOnImmutableError(Token, Token),
  ExpectedReference(DataType, DataType, Token),
  UnexpectedReference(DataType, Token),
  BorrowedValueHasMoved(Token),
  InvalidPropertyType(Token),
  InvalidSpreadExpression(Token),
  InvalidParameterAfterVariadic(Token),
  InvalidParameterAfterOptional(Token),
  ImportedNamespaceIsNotExported(Token),
  NamespaceAlreadyDefined(Token),
  ExternAlreadyDefined(Token),
  EnumMemberAlreadyDefined(Token),
  // #endregion Analyzer
}

impl fmt::Display for DiagnosticMessage {
  fn fmt(
    &self,
    f: &mut fmt::Formatter<'_>,
  ) -> fmt::Result {
    match self {
      DiagnosticMessage::UntermintedComment(token) => {
        write!(f, "Unterminted comment '{}'", token.lexeme)
      },
      DiagnosticMessage::UntermintedCharacter(token) => {
        write!(f, "Unterminted character '{}'", token.lexeme)
      },
      DiagnosticMessage::InvalidCharacterEscapeSequence(token) => {
        write!(f, "Invalid character escape sequence '{}'", token.lexeme)
      },
      DiagnosticMessage::InvalidCharacter(token) => {
        write!(f, "Invalid character '{}'", token.lexeme)
      },
      DiagnosticMessage::UnterminatedString(token) => {
        write!(f, "Unterminated string '{}'", token.lexeme)
      },
      DiagnosticMessage::ExpectedToken(expected_token, token) => {
        write!(f, "Expected '{}' after '{}'", expected_token, token.lexeme)
      },
      DiagnosticMessage::UnexpectedToken(kind, token) => {
        write!(f, "Unexpected token '{}' after '{}'", kind, token.lexeme)
      },
      DiagnosticMessage::InvalidToken(token) => {
        write!(f, "Invalid token '{}'", token.lexeme)
      },
      DiagnosticMessage::ExpectedExpression(token) => {
        write!(f, "Expected expression after '{}'", token.lexeme)
      },
      DiagnosticMessage::ExpectedVariableName(token) => {
        write!(f, "Expected variable name after '{}'", token.lexeme)
      },
      DiagnosticMessage::ExpectedReturnTypeAfterFunction(token) => {
        write!(f, "Expected return type after '{}'", token.lexeme)
      },
      DiagnosticMessage::ExpectedAfterExpression(expected_token_type, token, _token2) => {
        write!(f, "Expected '{}' after '{}' in expression", expected_token_type, token.lexeme)
      },
      DiagnosticMessage::ExpectedExpressionAfter(token) => {
        write!(f, "Expected expression after '{}'", token.lexeme)
      },
      DiagnosticMessage::InvalidAssignmentTarget(_) => write!(f, "Invalid assignment target"),
      DiagnosticMessage::ExpectedTypeAfterVariable(token) => {
        write!(f, "Expected type after '{}'", token.lexeme)
      },
      DiagnosticMessage::ExpectedSemicolonAfterExpression(_) => write!(f, "Expected ';' after expression"),
      DiagnosticMessage::InvalidEnumMember(_) => write!(f, "Invalid enum member"),
      DiagnosticMessage::ExpectedTypeAfterPipe(_) => write!(f, "Expected type after '|'"),
      DiagnosticMessage::ExpectedDelimiter(_) => write!(f, "Expected ',' or '}}'"),
      DiagnosticMessage::ExpectedType(token) => write!(f, "Expected type after '{}'", token.lexeme),
      DiagnosticMessage::UnexpectedKeyword(token) => write!(f, "Unexpected keyword '{}'", token.lexeme),
      DiagnosticMessage::ExpectedIdentifier(_) => write!(f, "Expected identifier"),
      DiagnosticMessage::UninitializedConstant(token) => {
        write!(f, "Constant '{}' must be initialized with a value", token.lexeme)
      },
      DiagnosticMessage::ExpectedPattern(token) => write!(f, "Expected pattern after '{}'", token.lexeme),
      DiagnosticMessage::UnexpectedGenerics(token) => write!(f, "Unexpected generics '{}'", token.lexeme),
      DiagnosticMessage::UndefinedMeta(token) => write!(f, "Undefined meta '{}'", token.lexeme),
      DiagnosticMessage::InvalidMetaEntity(token, _) => write!(f, "Invalid meta entity '{}'", token),
      DiagnosticMessage::MissingArgument(token) => write!(f, "Missing argument for meta '{}'", token.lexeme),
      DiagnosticMessage::InvalidProperty(token) => write!(f, "Invalid property '{}'", token.lexeme),
      DiagnosticMessage::UndeclaredVariable(token) => {
        write!(f, "Undeclared variable '{}'", token.lexeme)
      },
      DiagnosticMessage::InvalidUnaryOperatorForDataType(token, value) => {
        write!(f, "Invalid unary operator '{}' for data type '{}'", token.lexeme, value)
      },
      DiagnosticMessage::NotCallable(token) => {
        write!(f, "'{}' is not callable", token.lexeme)
      },
      DiagnosticMessage::InvalidNumberOfArguments(max, num, token) => {
        write!(
          f,
          "Function '{}': expected {} arguments, but got {} arguments",
          token.lexeme, max, num
        )
      },
      DiagnosticMessage::AssingInvalidType(expected, target, _) => {
        write!(f, "Cannot assign '{}' to '{}'", expected, target)
      },
      DiagnosticMessage::InvalidArgumentType(value, type_) => {
        write!(f, "Invalid argument type '{}', exptected: '{}'", value, type_.type_)
      },
      DiagnosticMessage::InvalidComparison(left, right, _) => {
        write!(f, "Invalid comparison between '{}' and '{}'", left, right)
      },
      DiagnosticMessage::InvalidOperator(token) => {
        write!(f, "Invalid operator '{}'", token.lexeme)
      },
      DiagnosticMessage::InvalidUnaryOperator(token) => {
        write!(f, "Invalid unary operator '{}'", token.lexeme)
      },
      DiagnosticMessage::UndefinedVariable(token) => {
        write!(f, "Undefined variable '{}'", token.lexeme)
      },
      DiagnosticMessage::VariableAlreadyDefined(name, _) => {
        write!(f, "Variable '{}' already defined", name)
      },
      DiagnosticMessage::InvalidReassignedVariable(token) => {
        write!(f, "Cannot reassign variable '{}'", token.lexeme)
      },
      DiagnosticMessage::TypeMismatch(left, right, _) => {
        write!(f, "Type mismatch between '{}' and '{}'", left, right)
      },
      DiagnosticMessage::TypeMismatchUnary(kind, _) => {
        write!(f, "Type mismatch for unary operator '{}'", kind)
      },
      DiagnosticMessage::CannotSubtract(left_kind, right_kind, _) => {
        write!(f, "Cannot subtract '{}' and '{}'", left_kind, right_kind)
      },
      DiagnosticMessage::CannotMultiply(left_kind, right_kind, _) => {
        write!(f, "Cannot multiply '{}' and '{}'", left_kind, right_kind)
      },
      DiagnosticMessage::CannotDivide(left_kind, right_kind, _) => {
        write!(f, "Cannot divide '{}' and '{}'", left_kind, right_kind)
      },
      DiagnosticMessage::CannotModulo(left_kind, right_kind, _) => {
        write!(f, "Cannot modulo '{}' and '{}'", left_kind, right_kind)
      },
      DiagnosticMessage::FunctionAlreadyDefined(name, _) => {
        write!(f, "Function '{}' already defined", name)
      },
      DiagnosticMessage::ClassAlreadyDefined(token) => {
        write!(f, "Class '{}' already defined", token.lexeme)
      },
      DiagnosticMessage::ArgumentTypeMismatch(expected, gived, _) => {
        write!(f, "Expected argument type '{}', but got '{}'", expected, gived)
      },
      DiagnosticMessage::ImmutableVariableAsMutableParameter(parameter, var, _) => {
        write!(
          f,
          "Cannot use immutable variable '{}' as mutable parameter '{}'",
          var.lexeme, parameter.lexeme
        )
      },
      DiagnosticMessage::ReturnOutsideFunction(_) => {
        write!(f, "Return outside function")
      },
      DiagnosticMessage::NotIterable(token) => {
        write!(f, "'{}' is not iterable", token.lexeme)
      },
      DiagnosticMessage::VectorElementTypeMismatch(_) => {
        write!(f, "Vector element type mismatch")
      },
      DiagnosticMessage::ModuleNotFound(token) => {
        write!(f, "Module '{}' not found", token.lexeme)
      },
      DiagnosticMessage::ImportedFunctionIsNotExported(token) => {
        write!(f, "Imported function '{}' is not exported", token.lexeme)
      },
      DiagnosticMessage::BreakOutsideLoop(_) => {
        write!(f, "Break outside loop")
      },
      DiagnosticMessage::ContinueOutsideLoop(_) => {
        write!(f, "Continue outside loop")
      },
      DiagnosticMessage::InvalidCondition(_) => {
        write!(f, "Invalid condition")
      },
      DiagnosticMessage::PropertyOutsideClass(token) => {
        write!(f, "Property '{}' outside class", token.lexeme)
      },
      DiagnosticMessage::PropertyAlreadyDefined(token) => {
        write!(f, "Property '{}' already defined", token.lexeme)
      },
      DiagnosticMessage::MethodOutsideClass(token) => {
        write!(f, "Method '{}' outside class", token.lexeme)
      },
      DiagnosticMessage::MethodAlreadyDefined(token) => {
        write!(f, "Method '{}' already defined", token.lexeme)
      },
      DiagnosticMessage::UndefinedProperty(token) => {
        write!(f, "Undefined property '{}'", token.lexeme)
      },
      DiagnosticMessage::NotAClass(token) => {
        write!(f, "'{}' is not a class", token.lexeme)
      },
      DiagnosticMessage::UndefinedClass(token, _) => {
        write!(f, "Undefined class '{}'", token.lexeme)
      },
      DiagnosticMessage::UndefinedMethods(_) => {
        write!(f, "Undefined methods")
      },
      DiagnosticMessage::ImmutableProperty(token) => {
        write!(f, "The property '{}' is immutable", token.lexeme)
      },
      DiagnosticMessage::PrivateProperty(token) => {
        write!(f, "The property '{}' is private", token.lexeme)
      },
      DiagnosticMessage::FunctionNotDefined(name, _) => {
        write!(f, "Function '{}' not defined", name)
      },
      DiagnosticMessage::VariableNeverUsed(_) => {
        write!(f, "Variable never used")
      },
      DiagnosticMessage::NotAnVector(_) => {
        write!(f, "Not an vector")
      },
      DiagnosticMessage::InvalidVectorIndex(_) => {
        write!(f, "Invalid vector index")
      },
      DiagnosticMessage::ThisOutsideOfClass(_) => {
        write!(f, "Invalid 'this' outside of class")
      },
      DiagnosticMessage::ImportedClassIsNotExported(token) => {
        write!(f, "Imported class '{}' is not exported", token.lexeme)
      },
      DiagnosticMessage::InvalidVariableInitializer(token) => {
        write!(f, "Invalid variable initializer '{}'", token.lexeme)
      },
      DiagnosticMessage::EnumAlreadyDefined(token) => {
        write!(f, "Enum '{}' already defined", token.lexeme)
      },
      DiagnosticMessage::EnumMemberTypeMismatch(token, target, source) => {
        write!(
          f,
          "Enum member '{}' type mismatch. Expected '{}', but got '{}'",
          token.lexeme, target, source
        )
      },
      DiagnosticMessage::NotAnEnum(token) => {
        write!(f, "'{}' is not an enum", token.lexeme)
      },
      DiagnosticMessage::UndefinedEnum(token) => {
        write!(f, "Undefined enum '{}'", token.lexeme)
      },
      DiagnosticMessage::UndefinedEnumMember(token) => {
        write!(f, "Undefined enum member '{}'", token.lexeme)
      },
      DiagnosticMessage::EnumAssignmentError(token, name) => {
        write!(f, "Cannot assign '{}' to '{}'", token.lexeme, name)
      },
      DiagnosticMessage::UndefinedImport(name) => {
        write!(f, "Undefined import '{}'", name.lexeme)
      },
      DiagnosticMessage::PotentialTypeMismatch(expected, gived, _) => {
        write!(f, "Potential type mismatch. Expected '{}', but got '{}'", expected, gived)
      },
      DiagnosticMessage::IncorrectNumberOfGenericArguments(_, expected, gived) => {
        write!(
          f,
          "Incorrect number of generic arguments. Expected {}, but got {}",
          expected, gived
        )
      },
      DiagnosticMessage::InvalidTypeArgument(expected, gived, _) => {
        write!(f, "Invalid type argument. Expected '{}', but got '{}'", expected, gived)
      },
      DiagnosticMessage::InterfaceAlreadyDefined(token) => {
        write!(f, "Interface '{}' already defined", token.lexeme)
      },
      DiagnosticMessage::UndefinedInterface(token) => {
        write!(f, "Undefined interface '{}'", token.lexeme)
      },
      DiagnosticMessage::InvalidInterfaceMethod(token) => {
        write!(f, "Invalid interface method '{}'", token.lexeme)
      },
      DiagnosticMessage::MethodImplementationParameterMismatch(_, expected, gived) => {
        write!(
          f,
          "Method implementation parameter mismatch. Expected '{}', but got '{}'",
          expected, gived
        )
      },
      DiagnosticMessage::MethodImplementationError(_, method) => {
        write!(f, "Method implementation error. Expected '{}'", method.lexeme)
      },
      DiagnosticMessage::MethodImplementationReturnTypeMismatch(_, expected, gived) => {
        write!(
          f,
          "Method implementation return type mismatch. Expected '{}', but got '{}'",
          expected, gived
        )
      },
      DiagnosticMessage::UnimplementedInterfaceMethod(name, _) => {
        write!(f, "Unimplemented interface method '{}'", name)
      },
      DiagnosticMessage::UnimplementedInterfaceMethods(names, _) => {
        write!(
          f,
          "Unimplemented interface methods '{}'",
          names.iter().map(|x| x.to_string()).collect::<Vec<String>>().join(", ")
        )
      },
      DiagnosticMessage::TypeAlreadyDefined(token) => {
        write!(f, "Type '{}' already defined", token.lexeme)
      },
      DiagnosticMessage::InvalidTypeValue(token) => {
        write!(f, "Invalid type value '{}'", token.lexeme)
      },
      DiagnosticMessage::UndefinedType(name, _) => {
        write!(f, "Undefined type '{}'", name)
      },
      DiagnosticMessage::TupleTypeMismatch(expected, gived, _) => {
        write!(f, "Tuple type mismatch. Expected '{}', but got '{}'", expected, gived)
      },
      DiagnosticMessage::ReturnTypeMismatch(expected, gived, _) => {
        write!(f, "Return type mismatch. Expected '{}', but got '{}'", expected, gived)
      },
      DiagnosticMessage::ImportedEnumIsNotExported(token) => {
        write!(f, "Imported enum '{}' is not exported", token.lexeme)
      },
      DiagnosticMessage::ImportedClassNotFound(_, name) => {
        write!(f, "Imported class '{}' not found", name)
      },
      DiagnosticMessage::ImportedTypeIsNotExported(token) => {
        write!(f, "Imported type '{}' is not exported", token.lexeme)
      },
      DiagnosticMessage::ImportedTypeNotFound(_, name) => {
        write!(f, "Imported type '{}' not found", name.lexeme)
      },
      DiagnosticMessage::InvalidCast(_, expected, gived) => {
        write!(f, "Invalid cast. Expected '{}', but got '{}'", expected, gived)
      },
      DiagnosticMessage::RecordAlreadyDefined(token) => {
        write!(f, "Record '{}' already defined", token.lexeme)
      },
      DiagnosticMessage::UndefinedRecord(token) => {
        write!(f, "Undefined record '{}'", token.lexeme)
      },
      DiagnosticMessage::ConstantAlreadyDefined(token) => {
        write!(f, "Constant '{}' already defined", token.lexeme)
      },
      DiagnosticMessage::ReassignedConstant(token) => {
        write!(f, "Cannot reassign constant '{}'", token.lexeme)
      },
      DiagnosticMessage::DecoratorAlreadyDefined(token) => {
        write!(f, "Decorator '{}' already defined", token.lexeme)
      },
      DiagnosticMessage::UndefinedDecorator(token) => {
        write!(f, "Undefined decorator '{}'", token.lexeme)
      },
      DiagnosticMessage::ImportedRecordIsNotExported(token) => {
        write!(f, "Imported record '{}' is not exported", token.lexeme)
      },
      DiagnosticMessage::ImportedDecoratorIsNotExported(token) => {
        write!(f, "Imported decorator '{}' is not exported", token.lexeme)
      },
      DiagnosticMessage::InvalidExternStatement(token) => {
        write!(f, "Invalid extern statement '{}'", token.lexeme)
      },
      DiagnosticMessage::ImportedExternIsNotExported(token) => {
        write!(f, "Imported extern '{}' is not exported", token.lexeme)
      },
      DiagnosticMessage::ValueOutOfRange(value, type_, _) => {
        write!(
          f,
          "Value '{}' is out of range for type '{}'. The range for '{}' is '{}'",
          value,
          type_,
          type_,
          type_.get_number_range()
        )
      },
      DiagnosticMessage::InvalidThis(this) => {
        write!(f, "Invalid 'this' outside of class '{}'", this.lexeme)
      },
      DiagnosticMessage::MutationOnImmutableError(name, _) => {
        write!(
          f,
          "Attempted to call a mutable method on an immutable variable '{}'",
          name.lexeme
        )
      },
      DiagnosticMessage::ExpectedReference(expeted_type, received_type, _) => {
        write!(f, "Expected '&{}', but got '{}'", expeted_type, received_type)
      },
      DiagnosticMessage::UnexpectedReference(received_type, _) => {
        write!(f, "Unexpected '&{}'", received_type)
      },
      DiagnosticMessage::BorrowedValueHasMoved(token) => {
        write!(f, "Borrowed value has moved '{}'", token.lexeme)
      },
      DiagnosticMessage::InvalidPropertyType(token) => {
        write!(f, "Invalid property type '{}'", token.lexeme)
      },
      DiagnosticMessage::InvalidSpreadExpression(token) => {
        write!(f, "Invalid spread expression '{}'", token.lexeme)
      },
      DiagnosticMessage::InvalidParameterAfterVariadic(token) => {
        write!(f, "Invalid parameter after variadic '{}'", token.lexeme)
      },
      DiagnosticMessage::InvalidParameterAfterOptional(token) => {
        write!(f, "Invalid parameter after optional '{}'", token.lexeme)
      },
      DiagnosticMessage::ImportedNamespaceIsNotExported(token) => {
        write!(f, "Imported namespace '{}' is not exported", token.lexeme)
      },
      DiagnosticMessage::ExternAlreadyDefined(token) => {
        write!(f, "Extern '{}' already defined", token.lexeme)
      },
      DiagnosticMessage::NamespaceAlreadyDefined(token) => {
        write!(f, "Namespace '{}' already defined", token.lexeme)
      },
      DiagnosticMessage::EnumMemberAlreadyDefined(token) => {
        write!(f, "Enum member '{}' already defined", token.lexeme)
      },
    }
  }
}

impl From<&DiagnosticMessage> for Token {
  fn from(value: &DiagnosticMessage) -> Self {
    match value {
      DiagnosticMessage::ExpectedExpression(token)
      | DiagnosticMessage::ExpectedToken(_, token)
      | DiagnosticMessage::ExpectedVariableName(token)
      | DiagnosticMessage::ExpectedReturnTypeAfterFunction(token)
      | DiagnosticMessage::ExpectedAfterExpression(_, token, _)
      | DiagnosticMessage::ExpectedExpressionAfter(token)
      | DiagnosticMessage::UnexpectedToken(_, token)
      | DiagnosticMessage::InvalidAssignmentTarget(token)
      | DiagnosticMessage::ExpectedTypeAfterVariable(token)
      | DiagnosticMessage::InvalidNumberOfArguments(_, _, token)
      | DiagnosticMessage::ExpectedSemicolonAfterExpression(token)
      | DiagnosticMessage::InvalidEnumMember(token)
      | DiagnosticMessage::ExpectedTypeAfterPipe(token)
      | DiagnosticMessage::ExpectedDelimiter(token)
      | DiagnosticMessage::ExpectedType(token)
      | DiagnosticMessage::UnexpectedKeyword(token)
      | DiagnosticMessage::ExpectedIdentifier(token)
      | DiagnosticMessage::UninitializedConstant(token)
      | DiagnosticMessage::InvalidToken(token)
      | DiagnosticMessage::UntermintedComment(token)
      | DiagnosticMessage::UntermintedCharacter(token)
      | DiagnosticMessage::InvalidCharacterEscapeSequence(token)
      | DiagnosticMessage::InvalidCharacter(token)
      | DiagnosticMessage::UnterminatedString(token)
      | DiagnosticMessage::ExpectedPattern(token)
      | DiagnosticMessage::UnexpectedGenerics(token)
      | DiagnosticMessage::UndefinedMeta(token)
      | DiagnosticMessage::InvalidMetaEntity(_, token)
      | DiagnosticMessage::MissingArgument(token)
      | DiagnosticMessage::TypeMismatch(_, _, token)
      | DiagnosticMessage::InvalidProperty(token)
      | DiagnosticMessage::UndeclaredVariable(token)
      | DiagnosticMessage::InvalidUnaryOperatorForDataType(token, _)
      | DiagnosticMessage::NotCallable(token)
      | DiagnosticMessage::AssingInvalidType(_, _, token)
      | DiagnosticMessage::InvalidArgumentType(_, token)
      | DiagnosticMessage::InvalidComparison(_, _, token)
      | DiagnosticMessage::InvalidOperator(token)
      | DiagnosticMessage::InvalidUnaryOperator(token)
      | DiagnosticMessage::UndefinedVariable(token)
      | DiagnosticMessage::VariableAlreadyDefined(_, token)
      | DiagnosticMessage::InvalidReassignedVariable(token)
      | DiagnosticMessage::TypeMismatchUnary(_, token)
      | DiagnosticMessage::CannotSubtract(_, _, token)
      | DiagnosticMessage::CannotMultiply(_, _, token)
      | DiagnosticMessage::CannotDivide(_, _, token)
      | DiagnosticMessage::CannotModulo(_, _, token)
      | DiagnosticMessage::FunctionAlreadyDefined(_, token)
      | DiagnosticMessage::ClassAlreadyDefined(token)
      | DiagnosticMessage::ArgumentTypeMismatch(_, _, token)
      | DiagnosticMessage::ImmutableVariableAsMutableParameter(_, _, token)
      | DiagnosticMessage::ReturnOutsideFunction(token)
      | DiagnosticMessage::NotIterable(token)
      | DiagnosticMessage::VectorElementTypeMismatch(token)
      | DiagnosticMessage::ModuleNotFound(token)
      | DiagnosticMessage::ImportedFunctionIsNotExported(token)
      | DiagnosticMessage::BreakOutsideLoop(token)
      | DiagnosticMessage::ContinueOutsideLoop(token)
      | DiagnosticMessage::InvalidCondition(token)
      | DiagnosticMessage::PropertyOutsideClass(token)
      | DiagnosticMessage::PropertyAlreadyDefined(token)
      | DiagnosticMessage::MethodOutsideClass(token)
      | DiagnosticMessage::MethodAlreadyDefined(token)
      | DiagnosticMessage::UndefinedProperty(token)
      | DiagnosticMessage::NotAClass(token)
      | DiagnosticMessage::UndefinedClass(_, token)
      | DiagnosticMessage::UndefinedMethods(token)
      | DiagnosticMessage::ImmutableProperty(token)
      | DiagnosticMessage::PrivateProperty(token)
      | DiagnosticMessage::FunctionNotDefined(_, token)
      | DiagnosticMessage::VariableNeverUsed(token)
      | DiagnosticMessage::NotAnVector(token)
      | DiagnosticMessage::InvalidVectorIndex(token)
      | DiagnosticMessage::ThisOutsideOfClass(token)
      | DiagnosticMessage::ImportedClassIsNotExported(token)
      | DiagnosticMessage::InvalidVariableInitializer(token)
      | DiagnosticMessage::EnumAlreadyDefined(token)
      | DiagnosticMessage::EnumMemberTypeMismatch(token, _, _)
      | DiagnosticMessage::NotAnEnum(token)
      | DiagnosticMessage::UndefinedEnum(token)
      | DiagnosticMessage::UndefinedEnumMember(token)
      | DiagnosticMessage::EnumAssignmentError(token, _)
      | DiagnosticMessage::PotentialTypeMismatch(_, _, token)
      | DiagnosticMessage::IncorrectNumberOfGenericArguments(token, _, _)
      | DiagnosticMessage::InvalidTypeArgument(_, _, token)
      | DiagnosticMessage::InterfaceAlreadyDefined(token)
      | DiagnosticMessage::UndefinedInterface(token)
      | DiagnosticMessage::InvalidInterfaceMethod(token)
      | DiagnosticMessage::MethodImplementationParameterMismatch(token, _, _)
      | DiagnosticMessage::MethodImplementationError(token, _)
      | DiagnosticMessage::MethodImplementationReturnTypeMismatch(token, _, _)
      | DiagnosticMessage::UnimplementedInterfaceMethod(_, token)
      | DiagnosticMessage::UnimplementedInterfaceMethods(_, token)
      | DiagnosticMessage::TypeAlreadyDefined(token)
      | DiagnosticMessage::InvalidTypeValue(token)
      | DiagnosticMessage::UndefinedType(_, token)
      | DiagnosticMessage::TupleTypeMismatch(_, _, token)
      | DiagnosticMessage::ReturnTypeMismatch(_, _, token)
      | DiagnosticMessage::ImportedEnumIsNotExported(token)
      | DiagnosticMessage::ImportedClassNotFound(token, _)
      | DiagnosticMessage::ImportedTypeIsNotExported(token)
      | DiagnosticMessage::ImportedTypeNotFound(token, _)
      | DiagnosticMessage::InvalidCast(token, _, _)
      | DiagnosticMessage::RecordAlreadyDefined(token)
      | DiagnosticMessage::UndefinedRecord(token)
      | DiagnosticMessage::ConstantAlreadyDefined(token)
      | DiagnosticMessage::DecoratorAlreadyDefined(token)
      | DiagnosticMessage::ReassignedConstant(token)
      | DiagnosticMessage::ImportedRecordIsNotExported(token)
      | DiagnosticMessage::ImportedDecoratorIsNotExported(token)
      | DiagnosticMessage::UndefinedDecorator(token)
      | DiagnosticMessage::InvalidExternStatement(token)
      | DiagnosticMessage::ImportedExternIsNotExported(token)
      | DiagnosticMessage::InvalidThis(token)
      | DiagnosticMessage::ValueOutOfRange(_, _, token)
      | DiagnosticMessage::MutationOnImmutableError(token, _)
      | DiagnosticMessage::ExpectedReference(_, _, token)
      | DiagnosticMessage::UnexpectedReference(_, token)
      | DiagnosticMessage::BorrowedValueHasMoved(token)
      | DiagnosticMessage::UndefinedImport(token)
      | DiagnosticMessage::InvalidPropertyType(token)
      | DiagnosticMessage::InvalidSpreadExpression(token)
      | DiagnosticMessage::InvalidParameterAfterVariadic(token)
      | DiagnosticMessage::InvalidParameterAfterOptional(token)
      | DiagnosticMessage::ImportedNamespaceIsNotExported(token)
      | DiagnosticMessage::ExternAlreadyDefined(token)
      | DiagnosticMessage::NamespaceAlreadyDefined(token)
      | DiagnosticMessage::EnumMemberAlreadyDefined(token) => token.clone(),
    }
  }
}

impl DiagnosticMessage {
  fn code(&self) -> String {
    match self {
      DiagnosticMessage::ExpectedToken(_, _) => "I0001".to_string(),
      DiagnosticMessage::UnexpectedToken(_, _) => "I0002".to_string(),
      DiagnosticMessage::InvalidToken(_) => "I0003".to_string(),
      DiagnosticMessage::ExpectedExpression(_) => "I0001".to_string(),
      DiagnosticMessage::ExpectedVariableName(_) => "I0003".to_string(),
      DiagnosticMessage::ExpectedReturnTypeAfterFunction(_) => "I0004".to_string(),
      DiagnosticMessage::ExpectedAfterExpression(_, _, _) => "I0005".to_string(),
      DiagnosticMessage::ExpectedExpressionAfter(_) => "I0006".to_string(),
      DiagnosticMessage::InvalidAssignmentTarget(_) => "I0008".to_string(),
      DiagnosticMessage::ExpectedTypeAfterVariable(_) => "I0009".to_string(),
      DiagnosticMessage::InvalidNumberOfArguments(_, _, _) => "I0010".to_string(),
      DiagnosticMessage::ExpectedSemicolonAfterExpression(_) => "I0011".to_string(),
      DiagnosticMessage::InvalidEnumMember(_) => "I0012".to_string(),
      DiagnosticMessage::ExpectedTypeAfterPipe(_) => "I0013".to_string(),
      DiagnosticMessage::ExpectedDelimiter(_) => "I0014".to_string(),
      DiagnosticMessage::ExpectedType(_) => "I0015".to_string(),
      DiagnosticMessage::UnexpectedKeyword(_) => "I0016".to_string(),
      DiagnosticMessage::ExpectedIdentifier(_) => "I0017".to_string(),
      DiagnosticMessage::UninitializedConstant(_) => "I0018".to_string(),
      DiagnosticMessage::ExpectedPattern(_) => "I0019".to_string(),
      DiagnosticMessage::UntermintedComment(_) => "I0020".to_string(),
      DiagnosticMessage::UntermintedCharacter(_) => "I0021".to_string(),
      DiagnosticMessage::InvalidCharacterEscapeSequence(_) => "I0022".to_string(),
      DiagnosticMessage::InvalidCharacter(_) => "I0023".to_string(),
      DiagnosticMessage::UnterminatedString(_) => "I0024".to_string(),
      DiagnosticMessage::UnexpectedGenerics(_) => "I0025".to_string(),
      DiagnosticMessage::UndefinedMeta(_) => "I0026".to_string(),
      DiagnosticMessage::InvalidMetaEntity(_, _) => "I0027".to_string(),
      DiagnosticMessage::MissingArgument(_) => "I0028".to_string(),
      DiagnosticMessage::InvalidArgumentType(_, _) => "I0029".to_string(),
      DiagnosticMessage::TypeMismatch(_, _, _) => "I0030".to_string(),
      DiagnosticMessage::InvalidProperty(_) => "I0031".to_string(),
      DiagnosticMessage::UndeclaredVariable(_) => "I0032".to_string(),
      DiagnosticMessage::InvalidUnaryOperatorForDataType(_, _) => "I0033".to_string(),
      DiagnosticMessage::NotCallable(_) => "I0034".to_string(),
      DiagnosticMessage::AssingInvalidType(_, _, _) => "I0035".to_string(),
      DiagnosticMessage::InvalidComparison(_, _, _) => "I0036".to_string(),
      DiagnosticMessage::InvalidOperator(_) => "I0037".to_string(),
      DiagnosticMessage::InvalidUnaryOperator(_) => "I0038".to_string(),
      DiagnosticMessage::UndefinedVariable(_) => "I0039".to_string(),
      DiagnosticMessage::VariableAlreadyDefined(_, _) => "I0040".to_string(),
      DiagnosticMessage::InvalidReassignedVariable(_) => "I0041".to_string(),
      DiagnosticMessage::TypeMismatchUnary(_, _) => "I0042".to_string(),
      DiagnosticMessage::CannotSubtract(_, _, _) => "I0043".to_string(),
      DiagnosticMessage::CannotMultiply(_, _, _) => "I0044".to_string(),
      DiagnosticMessage::CannotDivide(_, _, _) => "I0045".to_string(),
      DiagnosticMessage::CannotModulo(_, _, _) => "I0046".to_string(),
      DiagnosticMessage::FunctionAlreadyDefined(_, _) => "I0047".to_string(),
      DiagnosticMessage::ClassAlreadyDefined(_) => "I0048".to_string(),
      DiagnosticMessage::ArgumentTypeMismatch(_, _, _) => "I0049".to_string(),
      DiagnosticMessage::ImmutableVariableAsMutableParameter(_, _, _) => "I0050".to_string(),
      DiagnosticMessage::ReturnOutsideFunction(_) => "I0051".to_string(),
      DiagnosticMessage::NotIterable(_) => "I0052".to_string(),
      DiagnosticMessage::VectorElementTypeMismatch(_) => "I0053".to_string(),
      DiagnosticMessage::ModuleNotFound(_) => "I0054".to_string(),
      DiagnosticMessage::ImportedFunctionIsNotExported(_) => "I0055".to_string(),
      DiagnosticMessage::BreakOutsideLoop(_) => "I0056".to_string(),
      DiagnosticMessage::ContinueOutsideLoop(_) => "I0057".to_string(),
      DiagnosticMessage::InvalidCondition(_) => "I0058".to_string(),
      DiagnosticMessage::PropertyOutsideClass(_) => "I0059".to_string(),
      DiagnosticMessage::PropertyAlreadyDefined(_) => "I0060".to_string(),
      DiagnosticMessage::MethodOutsideClass(_) => "I0061".to_string(),
      DiagnosticMessage::MethodAlreadyDefined(_) => "I0062".to_string(),
      DiagnosticMessage::UndefinedProperty(_) => "I0063".to_string(),
      DiagnosticMessage::NotAClass(_) => "I0064".to_string(),
      DiagnosticMessage::UndefinedClass(_, _1) => "I0065".to_string(),
      DiagnosticMessage::UndefinedMethods(_) => "I0066".to_string(),
      DiagnosticMessage::ImmutableProperty(_) => "I0067".to_string(),
      DiagnosticMessage::PrivateProperty(_) => "I0068".to_string(),
      DiagnosticMessage::FunctionNotDefined(_, _) => "I0069".to_string(),
      DiagnosticMessage::VariableNeverUsed(_) => "I0070".to_string(),
      DiagnosticMessage::NotAnVector(_) => "I0071".to_string(),
      DiagnosticMessage::InvalidVectorIndex(_) => "I0072".to_string(),
      DiagnosticMessage::ThisOutsideOfClass(_) => "I0073".to_string(),
      DiagnosticMessage::ImportedClassIsNotExported(_) => "I0074".to_string(),
      DiagnosticMessage::InvalidVariableInitializer(_) => "I0075".to_string(),
      DiagnosticMessage::EnumAlreadyDefined(_) => "I0076".to_string(),
      DiagnosticMessage::EnumMemberTypeMismatch(_, _, _) => "I0077".to_string(),
      DiagnosticMessage::NotAnEnum(_) => "I0078".to_string(),
      DiagnosticMessage::UndefinedEnum(_) => "I0079".to_string(),
      DiagnosticMessage::UndefinedEnumMember(_) => "I0080".to_string(),
      DiagnosticMessage::EnumAssignmentError(_, _) => "I0081".to_string(),
      DiagnosticMessage::UndefinedImport(_) => "I0082".to_string(),
      DiagnosticMessage::IncorrectNumberOfGenericArguments(_, _, _) => "I0083".to_string(),
      DiagnosticMessage::InterfaceAlreadyDefined(_) => "I0084".to_string(),
      DiagnosticMessage::UndefinedInterface(_) => "I0085".to_string(),
      DiagnosticMessage::InvalidInterfaceMethod(_) => "I0086".to_string(),
      DiagnosticMessage::MethodImplementationParameterMismatch(_, _, _) => "I0087".to_string(),
      DiagnosticMessage::MethodImplementationError(_, _) => "I0088".to_string(),
      DiagnosticMessage::MethodImplementationReturnTypeMismatch(_, _, _) => "I0089".to_string(),
      DiagnosticMessage::UnimplementedInterfaceMethod(_, _) => "I0090".to_string(),
      DiagnosticMessage::UnimplementedInterfaceMethods(_, _) => "I0091".to_string(),
      DiagnosticMessage::TypeAlreadyDefined(_) => "I0092".to_string(),
      DiagnosticMessage::InvalidTypeValue(_) => "I0093".to_string(),
      DiagnosticMessage::UndefinedType(_, _) => "I0094".to_string(),
      DiagnosticMessage::TupleTypeMismatch(_, _, _) => "I0095".to_string(),
      DiagnosticMessage::ReturnTypeMismatch(_, _, _) => "I0096".to_string(),
      DiagnosticMessage::ImportedEnumIsNotExported(_) => "I0097".to_string(),
      DiagnosticMessage::ImportedClassNotFound(_, _) => "I0098".to_string(),
      DiagnosticMessage::ImportedTypeIsNotExported(_) => "I0099".to_string(),
      DiagnosticMessage::ImportedTypeNotFound(_, _) => "I0100".to_string(),
      DiagnosticMessage::InvalidCast(_, _, _) => "I0101".to_string(),
      DiagnosticMessage::RecordAlreadyDefined(_) => "I0102".to_string(),
      DiagnosticMessage::UndefinedRecord(_) => "I0103".to_string(),
      DiagnosticMessage::ConstantAlreadyDefined(_) => "I0104".to_string(),
      DiagnosticMessage::ReassignedConstant(_) => "I0105".to_string(),
      DiagnosticMessage::DecoratorAlreadyDefined(_) => "I0106".to_string(),
      DiagnosticMessage::UndefinedDecorator(_) => "I0107".to_string(),
      DiagnosticMessage::ImportedRecordIsNotExported(_) => "I0108".to_string(),
      DiagnosticMessage::ImportedDecoratorIsNotExported(_) => "I0109".to_string(),
      DiagnosticMessage::InvalidExternStatement(_) => "I0110".to_string(),
      DiagnosticMessage::ImportedExternIsNotExported(_) => "I0111".to_string(),
      DiagnosticMessage::ValueOutOfRange(_, _, _) => "I0112".to_string(),
      DiagnosticMessage::InvalidThis(_) => "I0113".to_string(),
      DiagnosticMessage::MutationOnImmutableError(_, _) => "I0114".to_string(),
      DiagnosticMessage::ExpectedReference(_, _, _) => "I0115".to_string(),
      DiagnosticMessage::UnexpectedReference(_, _) => "I0116".to_string(),
      DiagnosticMessage::BorrowedValueHasMoved(_) => "I0117".to_string(),
      DiagnosticMessage::PotentialTypeMismatch(_, _, _) => "I0118".to_string(),
      DiagnosticMessage::InvalidTypeArgument(_, _, _) => "I0119".to_string(),
      DiagnosticMessage::InvalidPropertyType(_) => "I0120".to_string(),
      DiagnosticMessage::InvalidSpreadExpression(_) => "I0121".to_string(),
      DiagnosticMessage::InvalidParameterAfterVariadic(_) => "I0122".to_string(),
      DiagnosticMessage::InvalidParameterAfterOptional(_) => "I0123".to_string(),
      DiagnosticMessage::ImportedNamespaceIsNotExported(_) => "I0124".to_string(),
      DiagnosticMessage::ExternAlreadyDefined(_) => "I0125".to_string(),
      DiagnosticMessage::NamespaceAlreadyDefined(_) => "I0126".to_string(),
      DiagnosticMessage::EnumMemberAlreadyDefined(_) => "I0127".to_string(),
    }
  }

  fn get_hint(&self) -> Option<DiagnosticReport> {
    match self {
      DiagnosticMessage::ExpectedDelimiter(token) => Some(DiagnosticReport::new(
        DiagnosticLevel::Hint,
        "Try adding ',' | ';' | '}' | '[' | ']' | '{' ".to_string(),
        "IH0014".to_string(),
        token.clone(),
        None,
      )),
      DiagnosticMessage::UnexpectedKeyword(token) => {
        let message = format!("Try deleting '{}' or adding a '_': '{}_'", token.lexeme, token.lexeme);

        Some(DiagnosticReport::new(
          DiagnosticLevel::Hint,
          message,
          "IH0016".to_string(),
          token.clone(),
          None,
        ))
      },
      DiagnosticMessage::PotentialTypeMismatch(left, right, op) => {
        let message= format!("You are attempting to perform, {} between a generic type '{}' and a non-generic type '{}'. This might lead to unexpected behavior or runtime errors if '{}' and '{}' are not compatible. Please verify that this operation is intentional and consider enforcing type compatibility or explicit type conversion where applicable.",
          op.lexeme, left, right, left, right
        );

        Some(DiagnosticReport::new(
          DiagnosticLevel::Hint,
          message,
          "IH0017".to_string(),
          op.clone(),
          None,
        ))
      },
      DiagnosticMessage::VariableNeverUsed(token) => {
        let message = format!("Consider using `_{}` instead", token.lexeme);
        Some(DiagnosticReport::new(
          DiagnosticLevel::Hint,
          message,
          "IH0018".to_string(),
          token.clone(),
          None,
        ))
      },
      DiagnosticMessage::MutationOnImmutableError(_, declared) => {
        let message = format!(
          "Consider making '{}' mutable in '{}:{}'",
          declared.lexeme, declared.file_name, declared.line
        );

        Some(DiagnosticReport::new(
          DiagnosticLevel::Hint,
          message,
          "IH0019".to_string(),
          declared.clone(),
          None,
        ))
      },
      DiagnosticMessage::ExpectedReference(_, _, _) => {
        let message = format!("Consider using the '&' operator to create a reference");

        Some(DiagnosticReport::new(
          DiagnosticLevel::Hint,
          message,
          "IH0020".to_string(),
          self.into(),
          None,
        ))
      },
      DiagnosticMessage::BorrowedValueHasMoved(token) => {
        let message = format!("Consider using the 'clone' method to create a copy of '{}'", token.lexeme);

        Some(DiagnosticReport::new(
          DiagnosticLevel::Hint,
          message,
          "IH0020".to_string(),
          token.clone(),
          None,
        ))
      },
      _ => None,
    }
  }

  fn get_level(&self) -> DiagnosticLevel {
    match self {
      DiagnosticMessage::VariableNeverUsed(_) | DiagnosticMessage::PotentialTypeMismatch(_, _, _) => {
        DiagnosticLevel::Warning
      },
      _ => DiagnosticLevel::Error,
    }
  }
  pub fn report(&self) -> DiagnosticReport {
    DiagnosticReport::new(self.get_level(), self.to_string(), self.code(), self.into(), self.get_hint())
  }
}
