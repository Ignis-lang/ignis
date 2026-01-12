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
    previous_span: Span,
  },
  FunctionAlreadyDefined {
    name: String,
    span: Span,
    previous_span: Span,
  },
  UndefinedType {
    name: String,
    span: Span,
  },
  // Type System Errors
  ImmutableAssignment {
    var_name: String,
    span: Span,
  },
  MutableReferenceToImmutable {
    var_name: String,
    span: Span,
  },
  InvalidCast {
    from: String,
    to: String,
    span: Span,
  },
  PrecisionLossCast {
    from: String,
    to: String,
    span: Span,
  },
  ArgumentCountMismatch {
    expected: usize,
    got: usize,
    func_name: String,
    span: Span,
  },
  ArgumentTypeMismatch {
    param_idx: usize,
    expected: String,
    got: String,
    span: Span,
  },
  InvalidBinaryOperandType {
    operator: String,
    left_type: String,
    right_type: String,
    span: Span,
  },
  InvalidUnaryOperandType {
    operator: String,
    operand_type: String,
    span: Span,
  },
  DereferenceNonPointer {
    type_name: String,
    span: Span,
  },
  VectorIndexNonInteger {
    index_type: String,
    span: Span,
  },
  AccessNonVector {
    type_name: String,
    span: Span,
  },
  IndexOutOfBounds {
    index: i64,
    size: usize,
    span: Span,
  },
  NotCallable {
    type_name: String,
    span: Span,
  },
  // Control Flow & Semantic Errors
  UnreachableCode {
    span: Span,
  },
  ExternWithBody {
    name: String,
    span: Span,
  },
  ExternConstWithInitializer {
    name: String,
    span: Span,
  },
  UninitializedVariableUse {
    var_name: String,
    span: Span,
  },
  InvalidReferenceTarget {
    span: Span,
  },
  CompoundAssignmentNonNumeric {
    operator: String,
    type_name: String,
    span: Span,
  },
  ReturnTypeMismatch {
    expected: String,
    got: String,
    span: Span,
  },
  MissingReturnValue {
    expected: String,
    span: Span,
  },
  // Binder errors
  ParameterAlreadyDefined {
    name: String,
    span: Span,
    previous_span: Span,
  },
  ConstantAlreadyDefined {
    name: String,
    span: Span,
    previous_span: Span,
  },
  // Resolver errors
  UndeclaredIdentifier {
    name: String,
    span: Span,
  },
  FunctionPathNotAsCallee {
    name: String,
    span: Span,
  },
  UnsupportedPathExpression {
    name: String,
    span: Span,
  },
  // Borrow checker errors
  BorrowConflictImmWhileMutable {
    var_name: String,
    span: Span,
  },
  BorrowConflictMutWhileImmutable {
    var_name: String,
    span: Span,
  },
  BorrowConflictMutWhileMutable {
    var_name: String,
    span: Span,
  },
  MutatedWhileBorrowed {
    var_name: String,
    span: Span,
  },
  // Control flow errors
  MissingReturnStatement {
    span: Span,
  },
  BreakOutsideLoop {
    span: Span,
  },
  ContinueOutsideLoop {
    span: Span,
  },
  ReturnOutsideFunction {
    span: Span,
  },
  CannotReturnLocalReference {
    span: Span,
  },
  UndefinedIdentifier {
    name: String,
    span: Span,
  },
  // Type checker errors
  AssignmentTypeMismatch {
    expected: String,
    got: String,
    span: Span,
  },
  IntegerOverflow {
    value: i64,
    target_type: String,
    span: Span,
  },
  // Import/Module errors
  ModuleNotFound {
    path: String,
    at: Span,
  },
  SymbolNotExported {
    symbol: String,
    module: String,
    at: Span,
  },
  ImportShadowsLocal {
    name: String,
    at: Span,
    previous_span: Span,
  },
  CircularDependency {
    cycle: Vec<String>,
    at: Span,
  },
  // Ownership checker errors
  UseAfterMove {
    var_name: String,
    span: Span,
  },
  UseAfterFree {
    var_name: String,
    span: Span,
  },
  InconsistentMoveInBranches {
    var_name: String,
    span: Span,
  },
  // Ownership checker warnings
  PossibleLeakToFFI {
    var_name: String,
    span: Span,
  },
  OwnershipEscapeToGlobal {
    var_name: String,
    span: Span,
  },
  // Builtin errors
  InvalidSizeOfOperand {
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

      // Type System Errors
      DiagnosticMessage::ImmutableAssignment { var_name, .. } => {
        write!(f, "Cannot assign to immutable variable '{}'", var_name)
      },
      DiagnosticMessage::MutableReferenceToImmutable { var_name, .. } => {
        write!(f, "Cannot create mutable reference to immutable variable '{}'", var_name)
      },
      DiagnosticMessage::InvalidCast { from, to, .. } => {
        write!(f, "Cannot cast from '{}' to '{}'", from, to)
      },
      DiagnosticMessage::PrecisionLossCast { from, to, .. } => {
        write!(f, "Cast from '{}' to '{}' may lose precision", from, to)
      },
      DiagnosticMessage::ArgumentCountMismatch {
        expected,
        got,
        func_name,
        ..
      } => {
        write!(f, "Function '{}' expects {} arguments, got {}", func_name, expected, got)
      },
      DiagnosticMessage::ArgumentTypeMismatch {
        param_idx,
        expected,
        got,
        ..
      } => {
        write!(
          f,
          "Argument {} type mismatch: expected '{}', got '{}'",
          param_idx, expected, got
        )
      },
      DiagnosticMessage::InvalidBinaryOperandType {
        operator,
        left_type,
        right_type,
        ..
      } => {
        write!(
          f,
          "Binary operator '{}' cannot be applied to types '{}' and '{}'",
          operator, left_type, right_type
        )
      },
      DiagnosticMessage::InvalidUnaryOperandType {
        operator, operand_type, ..
      } => {
        write!(f, "Unary operator '{}' cannot be applied to type '{}'", operator, operand_type)
      },
      DiagnosticMessage::DereferenceNonPointer { type_name, .. } => {
        write!(f, "Cannot dereference non-pointer type '{}'", type_name)
      },
      DiagnosticMessage::VectorIndexNonInteger { index_type, .. } => {
        write!(f, "Vector index must be integer type, got '{}'", index_type)
      },
      DiagnosticMessage::AccessNonVector { type_name, .. } => {
        write!(f, "Cannot index non-vector type '{}'", type_name)
      },
      DiagnosticMessage::IndexOutOfBounds { index, size, .. } => {
        write!(f, "Index {} is out of bounds for array of size {}", index, size)
      },
      DiagnosticMessage::NotCallable { type_name, .. } => {
        write!(f, "Cannot call non-function type '{}'", type_name)
      },

      // Control Flow & Semantic Errors
      DiagnosticMessage::UnreachableCode { .. } => write!(f, "Unreachable code detected"),
      DiagnosticMessage::ExternWithBody { name, .. } => {
        write!(f, "Extern function '{}' cannot have a body", name)
      },
      DiagnosticMessage::ExternConstWithInitializer { name, .. } => {
        write!(f, "Extern constant '{}' cannot have an initializer", name)
      },
      DiagnosticMessage::UninitializedVariableUse { var_name, .. } => {
        write!(f, "Variable '{}' used before initialization", var_name)
      },
      DiagnosticMessage::InvalidReferenceTarget { .. } => {
        write!(f, "Cannot take reference of non-lvalue expression")
      },
      DiagnosticMessage::CompoundAssignmentNonNumeric {
        operator, type_name, ..
      } => {
        write!(
          f,
          "Compound assignment operator '{}' cannot be applied to type '{}'",
          operator, type_name
        )
      },
      DiagnosticMessage::ReturnTypeMismatch { expected, got, .. } => {
        write!(f, "Return type mismatch: expected '{}', got '{}'", expected, got)
      },
      DiagnosticMessage::MissingReturnValue { expected, .. } => {
        write!(f, "Missing return value: expected '{}', got void", expected)
      },

      // Binder errors
      DiagnosticMessage::ParameterAlreadyDefined { name, .. } => {
        write!(f, "Parameter '{}' is already defined", name)
      },
      DiagnosticMessage::ConstantAlreadyDefined { name, .. } => {
        write!(f, "Constant '{}' is already defined", name)
      },

      // Resolver errors
      DiagnosticMessage::UndeclaredIdentifier { name, .. } => {
        write!(f, "Undeclared identifier '{}'", name)
      },
      DiagnosticMessage::FunctionPathNotAsCallee { name, .. } => {
        write!(
          f,
          "Function path '{}' can only be used as a call target (e.g., {}(...))",
          name, name
        )
      },
      DiagnosticMessage::UnsupportedPathExpression { name, .. } => {
        write!(
          f,
          "Path expression '{}' is not supported; only constants and function calls are allowed in v0.2",
          name
        )
      },

      // Borrow checker errors
      DiagnosticMessage::BorrowConflictImmWhileMutable { var_name, .. } => {
        write!(
          f,
          "Cannot borrow '{}' as immutable because it is already borrowed as mutable",
          var_name
        )
      },
      DiagnosticMessage::BorrowConflictMutWhileImmutable { var_name, .. } => {
        write!(
          f,
          "Cannot borrow '{}' as mutable because it is already borrowed as immutable",
          var_name
        )
      },
      DiagnosticMessage::BorrowConflictMutWhileMutable { var_name, .. } => {
        write!(
          f,
          "Cannot borrow '{}' as mutable because it is already borrowed as mutable",
          var_name
        )
      },
      DiagnosticMessage::MutatedWhileBorrowed { var_name, .. } => {
        write!(f, "Cannot assign to '{}' because it is borrowed", var_name)
      },

      // Control flow errors
      DiagnosticMessage::MissingReturnStatement { .. } => {
        write!(f, "Missing return statement")
      },
      DiagnosticMessage::BreakOutsideLoop { .. } => {
        write!(f, "Break statement outside of loop")
      },
      DiagnosticMessage::ContinueOutsideLoop { .. } => {
        write!(f, "Continue statement outside of loop")
      },
      DiagnosticMessage::ReturnOutsideFunction { .. } => {
        write!(f, "Return statement outside of function")
      },
      DiagnosticMessage::CannotReturnLocalReference { .. } => {
        write!(f, "Cannot return reference to local variable")
      },
      DiagnosticMessage::UndefinedIdentifier { name, .. } => {
        write!(f, "Undefined identifier '{}'", name)
      },

      // Type checker errors
      DiagnosticMessage::AssignmentTypeMismatch { expected, got, .. } => {
        write!(f, "Type mismatch: expected '{}', found '{}'", expected, got)
      },
      DiagnosticMessage::IntegerOverflow { value, target_type, .. } => {
        write!(f, "Integer literal {} overflows type '{}'", value, target_type)
      },

      // Import/Module errors
      DiagnosticMessage::ModuleNotFound { path, .. } => {
        write!(f, "Module '{}' not found", path)
      },
      DiagnosticMessage::SymbolNotExported { symbol, module, .. } => {
        write!(f, "Symbol '{}' is not exported from module '{}'", symbol, module)
      },
      DiagnosticMessage::ImportShadowsLocal { name, .. } => {
        write!(f, "Import '{}' shadows a local definition", name)
      },
      DiagnosticMessage::CircularDependency { cycle, .. } => {
        write!(f, "Circular dependency detected: {}", cycle.join(" -> "))
      },

      // Ownership checker errors
      DiagnosticMessage::UseAfterMove { var_name, .. } => {
        write!(f, "Use of moved value '{}'", var_name)
      },
      DiagnosticMessage::UseAfterFree { var_name, .. } => {
        write!(f, "Use of freed value '{}'", var_name)
      },
      DiagnosticMessage::InconsistentMoveInBranches { var_name, .. } => {
        write!(f, "Variable '{}' is moved in one branch but not the other", var_name)
      },
      DiagnosticMessage::PossibleLeakToFFI { var_name, .. } => {
        write!(f, "Possible memory leak: ownership of '{}' escapes to FFI", var_name)
      },
      DiagnosticMessage::OwnershipEscapeToGlobal { var_name, .. } => {
        write!(f, "Ownership of '{}' escapes to global scope", var_name)
      },

      // Builtin errors
      DiagnosticMessage::InvalidSizeOfOperand { .. } => {
        write!(f, "sizeOf(unknown) requires explicit type cast")
      },
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
      | DiagnosticMessage::UndefinedType { span, .. }
      | DiagnosticMessage::ImmutableAssignment { span, .. }
      | DiagnosticMessage::MutableReferenceToImmutable { span, .. }
      | DiagnosticMessage::InvalidCast { span, .. }
      | DiagnosticMessage::PrecisionLossCast { span, .. }
      | DiagnosticMessage::ArgumentCountMismatch { span, .. }
      | DiagnosticMessage::ArgumentTypeMismatch { span, .. }
      | DiagnosticMessage::InvalidBinaryOperandType { span, .. }
      | DiagnosticMessage::InvalidUnaryOperandType { span, .. }
      | DiagnosticMessage::DereferenceNonPointer { span, .. }
      | DiagnosticMessage::VectorIndexNonInteger { span, .. }
      | DiagnosticMessage::AccessNonVector { span, .. }
      | DiagnosticMessage::IndexOutOfBounds { span, .. }
      | DiagnosticMessage::NotCallable { span, .. }
      | DiagnosticMessage::UnreachableCode { span, .. }
      | DiagnosticMessage::ExternWithBody { span, .. }
      | DiagnosticMessage::ExternConstWithInitializer { span, .. }
      | DiagnosticMessage::UninitializedVariableUse { span, .. }
      | DiagnosticMessage::InvalidReferenceTarget { span, .. }
      | DiagnosticMessage::CompoundAssignmentNonNumeric { span, .. }
      | DiagnosticMessage::ReturnTypeMismatch { span, .. }
      | DiagnosticMessage::MissingReturnValue { span, .. }
      | DiagnosticMessage::ParameterAlreadyDefined { span, .. }
      | DiagnosticMessage::ConstantAlreadyDefined { span, .. }
      | DiagnosticMessage::UndeclaredIdentifier { span, .. }
      | DiagnosticMessage::FunctionPathNotAsCallee { span, .. }
      | DiagnosticMessage::UnsupportedPathExpression { span, .. }
      | DiagnosticMessage::BorrowConflictImmWhileMutable { span, .. }
      | DiagnosticMessage::BorrowConflictMutWhileImmutable { span, .. }
      | DiagnosticMessage::BorrowConflictMutWhileMutable { span, .. }
      | DiagnosticMessage::MutatedWhileBorrowed { span, .. }
      | DiagnosticMessage::MissingReturnStatement { span, .. }
      | DiagnosticMessage::BreakOutsideLoop { span, .. }
      | DiagnosticMessage::ContinueOutsideLoop { span, .. }
      | DiagnosticMessage::ReturnOutsideFunction { span, .. }
      | DiagnosticMessage::CannotReturnLocalReference { span, .. }
      | DiagnosticMessage::UndefinedIdentifier { span, .. }
      | DiagnosticMessage::AssignmentTypeMismatch { span, .. }
      | DiagnosticMessage::IntegerOverflow { span, .. } => span.clone(),

      DiagnosticMessage::ModuleNotFound { at, .. }
      | DiagnosticMessage::SymbolNotExported { at, .. }
      | DiagnosticMessage::ImportShadowsLocal { at, .. }
      | DiagnosticMessage::CircularDependency { at, .. } => at.clone(),

      DiagnosticMessage::UseAfterMove { span, .. }
      | DiagnosticMessage::UseAfterFree { span, .. }
      | DiagnosticMessage::InconsistentMoveInBranches { span, .. }
      | DiagnosticMessage::PossibleLeakToFFI { span, .. }
      | DiagnosticMessage::OwnershipEscapeToGlobal { span, .. }
      | DiagnosticMessage::InvalidSizeOfOperand { span, .. } => span.clone(),
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
      DiagnosticMessage::ImmutableAssignment { .. } => "A0013",
      DiagnosticMessage::MutableReferenceToImmutable { .. } => "A0014",
      DiagnosticMessage::InvalidCast { .. } => "A0015",
      DiagnosticMessage::PrecisionLossCast { .. } => "A0016",
      DiagnosticMessage::ArgumentCountMismatch { .. } => "A0017",
      DiagnosticMessage::ArgumentTypeMismatch { .. } => "A0018",
      DiagnosticMessage::InvalidBinaryOperandType { .. } => "A0019",
      DiagnosticMessage::InvalidUnaryOperandType { .. } => "A0020",
      DiagnosticMessage::DereferenceNonPointer { .. } => "A0021",
      DiagnosticMessage::VectorIndexNonInteger { .. } => "A0022",
      DiagnosticMessage::AccessNonVector { .. } => "A0023",
      DiagnosticMessage::IndexOutOfBounds { .. } => "A0048",
      DiagnosticMessage::NotCallable { .. } => "A0024",
      DiagnosticMessage::UnreachableCode { .. } => "A0025",
      DiagnosticMessage::ExternWithBody { .. } => "A0026",
      DiagnosticMessage::ExternConstWithInitializer { .. } => "A0027",
      DiagnosticMessage::UninitializedVariableUse { .. } => "A0028",
      DiagnosticMessage::InvalidReferenceTarget { .. } => "A0029",
      DiagnosticMessage::CompoundAssignmentNonNumeric { .. } => "A0030",
      DiagnosticMessage::ReturnTypeMismatch { .. } => "A0031",
      DiagnosticMessage::MissingReturnValue { .. } => "A0032",
      DiagnosticMessage::ParameterAlreadyDefined { .. } => "A0033",
      DiagnosticMessage::ConstantAlreadyDefined { .. } => "A0034",
      DiagnosticMessage::UndeclaredIdentifier { .. } => "A0035",
      DiagnosticMessage::FunctionPathNotAsCallee { .. } => "A0050",
      DiagnosticMessage::UnsupportedPathExpression { .. } => "A0051",
      DiagnosticMessage::BorrowConflictImmWhileMutable { .. } => "A0036",
      DiagnosticMessage::BorrowConflictMutWhileImmutable { .. } => "A0037",
      DiagnosticMessage::BorrowConflictMutWhileMutable { .. } => "A0038",
      DiagnosticMessage::MutatedWhileBorrowed { .. } => "A0047",
      DiagnosticMessage::MissingReturnStatement { .. } => "A0039",
      DiagnosticMessage::BreakOutsideLoop { .. } => "A0040",
      DiagnosticMessage::ContinueOutsideLoop { .. } => "A0041",
      DiagnosticMessage::ReturnOutsideFunction { .. } => "A0042",
      DiagnosticMessage::CannotReturnLocalReference { .. } => "A0043",
      DiagnosticMessage::UndefinedIdentifier { .. } => "A0044",
      DiagnosticMessage::AssignmentTypeMismatch { .. } => "A0045",
      DiagnosticMessage::IntegerOverflow { .. } => "A0046",
      DiagnosticMessage::InvalidSizeOfOperand { .. } => "A0049",
      DiagnosticMessage::ModuleNotFound { .. } => "M0001",
      DiagnosticMessage::SymbolNotExported { .. } => "M0002",
      DiagnosticMessage::ImportShadowsLocal { .. } => "M0003",
      DiagnosticMessage::CircularDependency { .. } => "M0004",
      DiagnosticMessage::UseAfterMove { .. } => "O0001",
      DiagnosticMessage::UseAfterFree { .. } => "O0002",
      DiagnosticMessage::InconsistentMoveInBranches { .. } => "O0003",
      DiagnosticMessage::PossibleLeakToFFI { .. } => "O0004",
      DiagnosticMessage::OwnershipEscapeToGlobal { .. } => "O0005",
    }
    .to_string()
  }

  fn level(&self) -> Severity {
    match self {
      DiagnosticMessage::PrecisionLossCast { .. }
      | DiagnosticMessage::UnreachableCode { .. }
      | DiagnosticMessage::MissingReturnStatement { .. }
      | DiagnosticMessage::PossibleLeakToFFI { .. }
      | DiagnosticMessage::OwnershipEscapeToGlobal { .. } => Severity::Warning,
      _ => Severity::Error,
    }
  }

  fn secondary_labels(&self) -> Vec<(Span, String)> {
    match self {
      DiagnosticMessage::VariableAlreadyDefined { previous_span, .. }
      | DiagnosticMessage::FunctionAlreadyDefined { previous_span, .. }
      | DiagnosticMessage::ParameterAlreadyDefined { previous_span, .. }
      | DiagnosticMessage::ConstantAlreadyDefined { previous_span, .. }
      | DiagnosticMessage::ImportShadowsLocal { previous_span, .. } => {
        vec![(previous_span.clone(), "Previous definition here".to_string())]
      },
      DiagnosticMessage::MissingReturnStatement { span, .. } => {
        vec![(span.clone(), "Function should return a value".to_string())]
      },
      _ => vec![],
    }
  }

  pub fn report(&self) -> Diagnostic {
    let mut diagnostic = Diagnostic::new(self.level(), self.to_string(), self.code(), self.primary_span());
    for (span, message) in self.secondary_labels() {
      diagnostic = diagnostic.with_label(span, message);
    }
    diagnostic
  }
}
