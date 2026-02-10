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
  ExpectedCallAfterTypeArgs {
    span: Span,
  },
  RecursionLimitExceeded(Span),
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
  TypeAlreadyDefined {
    name: String,
    span: Span,
    previous_span: Span,
  },
  TypeAliasCycle {
    name: String,
    span: Span,
  },
  UndefinedType {
    name: String,
    span: Span,
  },
  // Generic type errors
  WrongNumberOfTypeArgs {
    expected: usize,
    got: usize,
    type_name: String,
    span: Span,
  },
  TypeParamCannotHaveArgs {
    name: String,
    span: Span,
  },
  TypeAliasCannotHaveArgs {
    name: String,
    span: Span,
  },
  TypeCannotBeParameterized {
    type_name: String,
    span: Span,
  },
  CannotInferTypeParam {
    param_name: String,
    func_name: String,
    span: Span,
  },
  // Record/Enum Type Errors
  FieldNotFound {
    field: String,
    type_name: String,
    span: Span,
  },
  MethodMustBeCalled {
    method: String,
    span: Span,
  },
  DotAccessOnEnum {
    span: Span,
  },
  DotAccessOnNonRecord {
    type_name: String,
    span: Span,
  },
  StaticAccessOnNonType {
    span: Span,
  },
  StaticMemberNotFound {
    member: String,
    type_name: String,
    span: Span,
  },
  EnumVariantRequiresPayload {
    variant: String,
    expected: usize,
    span: Span,
  },
  MemberNotFoundInNamespace {
    member: String,
    namespace: String,
    span: Span,
  },
  NotARecord {
    name: String,
    span: Span,
  },
  UnknownField {
    field: String,
    type_name: String,
    span: Span,
  },
  DuplicateFieldInit {
    field: String,
    span: Span,
  },
  MissingFieldInit {
    field: String,
    type_name: String,
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
  MutatingMethodOnImmutable {
    method: String,
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
  InvalidNullLiteral {
    span: Span,
  },
  CannotInferNullType {
    span: Span,
  },
  NullDereference {
    span: Span,
  },
  InvalidPointerArithmetic {
    operator: String,
    left_type: String,
    right_type: String,
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
  DynamicVectorsNotSupported {
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
  TypeParamAlreadyDefined {
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
  EnumNotFound {
    name: String,
    span: Span,
  },
  EnumVariantNotFound {
    enum_name: String,
    variant: String,
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
  UseAfterDrop {
    var_name: String,
    span: Span,
  },
  DoubleDrop {
    var_name: String,
    span: Span,
  },
  InconsistentDropInBranches {
    var_name: String,
    span: Span,
  },
  DoubleFree {
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
  InvalidMinMaxType {
    func_name: String,
    got: String,
    span: Span,
  },
  // Static field errors
  StaticFieldRequiresInit {
    field: String,
    type_name: String,
    span: Span,
  },
  StaticOnEnumVariant {
    variant: String,
    span: Span,
  },
  StaticFieldNotConst {
    field: String,
    type_name: String,
    span: Span,
  },
  // For-of errors
  ForOfExpectsVector {
    got: String,
    span: Span,
  },
  ForOfRequiresCopyOrRef {
    element_type: String,
    span: Span,
  },
  ForOfMutRequiresMutableIter {
    span: Span,
  },
  // Overload errors
  NoOverloadMatches {
    name: String,
    available_signatures: Vec<String>,
    arg_types: Vec<String>,
    span: Span,
  },
  AmbiguousOverload {
    name: String,
    matching_signatures: Vec<String>,
    arg_types: Vec<String>,
    span: Span,
  },
  OverloadGroupAsValue {
    name: String,
    span: Span,
  },
  DuplicateOverload {
    name: String,
    signature: String,
    span: Span,
  },
  MainFunctionCannotBeOverloaded {
    span: Span,
  },
  LibraryCannotHaveMainFunction {
    span: Span,
  },
  ExecutableMustHaveMainFunction {
    span: Span,
  },
  PrivateFieldAccess {
    field: String,
    type_name: String,
    span: Span,
  },
  PrivateMethodAccess {
    method: String,
    type_name: String,
    span: Span,
  },
  // Builtin call errors
  UnknownBuiltin {
    name: String,
    span: Span,
  },
  CompileError {
    message: String,
    span: Span,
  },
  BuiltinArgCount {
    name: String,
    expected: usize,
    got: usize,
    span: Span,
  },
  BuiltinExpectedStringLiteral {
    name: String,
    span: Span,
  },
  BuiltinTypeConstraint {
    name: String,
    constraint: String,
    span: Span,
  },
  UnknownConfigFlag {
    key: String,
    span: Span,
  },
  UnknownAttribute {
    name: String,
    target: String,
    span: Span,
  },
  AttributeArgCount {
    attr: String,
    expected: usize,
    got: usize,
    span: Span,
  },
  AttributeExpectedString {
    attr: String,
    span: Span,
  },
  AttributeExpectedInt {
    attr: String,
    span: Span,
  },
  AlignmentNotPowerOfTwo {
    value: u64,
    span: Span,
  },
  // Lint diagnostics
  UnusedVariable {
    name: String,
    span: Span,
  },
  UnusedImport {
    name: String,
    span: Span,
  },
  DeprecatedCall {
    name: String,
    message: String,
    span: Span,
  },
  UnknownLint {
    name: String,
    span: Span,
  },
  AttributeExpectedIdentifier {
    attr: String,
    span: Span,
  },
  // Lang trait diagnostics
  UnknownLangTrait {
    name: String,
    span: Span,
  },
  LangTraitDropCopyConflict {
    type_name: String,
    span: Span,
  },
  LangTraitMissingMethod {
    trait_name: String,
    method_name: String,
    type_name: String,
    span: Span,
  },
  LangTraitInvalidSignature {
    trait_name: String,
    method_name: String,
    expected: String,
    got: String,
    span: Span,
  },
  LangTraitNotApplicable {
    trait_name: String,
    span: Span,
  },
  CopyOnNonCopyField {
    type_name: String,
    field_name: String,
    field_type: String,
    span: Span,
  },
  CopyOnNonCopyVariantPayload {
    type_name: String,
    variant_name: String,
    payload_type: String,
    payload_index: usize,
    span: Span,
  },
  // Extension method diagnostics
  ExtensionInvalidTargetType {
    type_name: String,
    span: Span,
  },
  ExtensionRequiresParameter {
    span: Span,
  },
  ExtensionReceiverTypeMismatch {
    expected: String,
    got: String,
    span: Span,
  },
  ExtensionMethodOnLiteral {
    method: String,
    type_name: String,
    span: Span,
  },
  ExtensionMethodOnTemporary {
    method: String,
    type_name: String,
    span: Span,
  },
  // Trait diagnostics
  TraitMissingRequiredMethod {
    trait_name: String,
    method_name: String,
    type_name: String,
    span: Span,
  },
  TraitMethodSignatureMismatch {
    trait_name: String,
    method_name: String,
    expected: String,
    got: String,
    span: Span,
  },
  UnknownTraitInImplements {
    name: String,
    span: Span,
  },
  TraitInExternBlock {
    span: Span,
  },
  TraitMethodRequiresSelf {
    method_name: String,
    span: Span,
  },
  TraitFieldNotAllowed {
    span: Span,
  },
  TraitStaticMethodNotAllowed {
    method_name: String,
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
      DiagnosticMessage::ExpectedCallAfterTypeArgs { .. } => {
        write!(f, "Expected '(' after type arguments")
      },
      DiagnosticMessage::RecursionLimitExceeded(_) => write!(f, "Recursion limit exceeded"),

      // Analyzer
      DiagnosticMessage::UndeclaredVariable { name, .. } => write!(f, "Undeclared variable '{}'", name),
      DiagnosticMessage::VariableAlreadyDefined { name, .. } => write!(f, "Variable '{}' is already defined", name),
      DiagnosticMessage::FunctionAlreadyDefined { name, .. } => write!(f, "Function '{}' is already defined", name),
      DiagnosticMessage::TypeAlreadyDefined { name, .. } => write!(f, "Type '{}' is already defined", name),
      DiagnosticMessage::TypeAliasCycle { name, .. } => write!(f, "Type alias '{}' creates a cycle", name),
      DiagnosticMessage::UndefinedType { name, .. } => write!(f, "Undefined type '{}'", name),

      // Generic type errors
      DiagnosticMessage::WrongNumberOfTypeArgs {
        expected,
        got,
        type_name,
        ..
      } => {
        write!(
          f,
          "Wrong number of type arguments for '{}': expected {}, got {}",
          type_name, expected, got
        )
      },
      DiagnosticMessage::TypeParamCannotHaveArgs { name, .. } => {
        write!(f, "Type parameter '{}' cannot have type arguments", name)
      },
      DiagnosticMessage::TypeAliasCannotHaveArgs { name, .. } => {
        write!(f, "Type alias '{}' cannot have type arguments", name)
      },
      DiagnosticMessage::TypeCannotBeParameterized { type_name, .. } => {
        write!(f, "Type '{}' cannot be parameterized", type_name)
      },
      DiagnosticMessage::CannotInferTypeParam {
        param_name, func_name, ..
      } => {
        write!(
          f,
          "Cannot infer type parameter '{}' for '{}'; specify it explicitly",
          param_name, func_name
        )
      },

      // Record/Enum Type Errors
      DiagnosticMessage::FieldNotFound { field, type_name, .. } => {
        write!(f, "Field '{}' not found in type '{}'", field, type_name)
      },
      DiagnosticMessage::MethodMustBeCalled { method, .. } => {
        write!(f, "Method '{}' must be called", method)
      },
      DiagnosticMessage::DotAccessOnEnum { .. } => {
        write!(f, "Cannot use '.' on enum value, use '::' for static members")
      },
      DiagnosticMessage::DotAccessOnNonRecord { type_name, .. } => {
        write!(f, "Cannot access field of non-record type '{}'", type_name)
      },
      DiagnosticMessage::StaticAccessOnNonType { .. } => {
        write!(f, "'::' can only be used on types, not values")
      },
      DiagnosticMessage::StaticMemberNotFound { member, type_name, .. } => {
        write!(f, "No static member '{}' in type '{}'", member, type_name)
      },
      DiagnosticMessage::EnumVariantRequiresPayload { variant, expected, .. } => {
        write!(f, "Enum variant '{}' requires {} argument(s)", variant, expected)
      },
      DiagnosticMessage::MemberNotFoundInNamespace { member, namespace, .. } => {
        write!(f, "Member '{}' not found in namespace '{}'", member, namespace)
      },
      DiagnosticMessage::NotARecord { name, .. } => {
        write!(f, "'{}' is not a record type", name)
      },
      DiagnosticMessage::UnknownField { field, type_name, .. } => {
        write!(f, "Unknown field '{}' in record '{}'", field, type_name)
      },
      DiagnosticMessage::DuplicateFieldInit { field, .. } => {
        write!(f, "Duplicate field '{}' in initializer", field)
      },
      DiagnosticMessage::MissingFieldInit { field, type_name, .. } => {
        write!(f, "Missing field '{}' in record '{}' initializer", field, type_name)
      },

      // Type System Errors
      DiagnosticMessage::ImmutableAssignment { var_name, .. } => {
        write!(f, "Cannot assign to immutable variable '{}'", var_name)
      },
      DiagnosticMessage::MutableReferenceToImmutable { var_name, .. } => {
        write!(f, "Cannot create mutable reference to immutable variable '{}'", var_name)
      },
      DiagnosticMessage::MutatingMethodOnImmutable { method, var_name, .. } => {
        write!(
          f,
          "Cannot call mutating method '{}' on immutable variable '{}'",
          method, var_name
        )
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
      DiagnosticMessage::InvalidNullLiteral { .. } => {
        write!(f, "Null literal can only be used with pointer types")
      },
      DiagnosticMessage::CannotInferNullType { .. } => {
        write!(f, "Cannot infer pointer type for null literal")
      },
      DiagnosticMessage::NullDereference { .. } => {
        write!(f, "Cannot dereference null pointer")
      },
      DiagnosticMessage::InvalidPointerArithmetic {
        operator,
        left_type,
        right_type,
        ..
      } => {
        write!(
          f,
          "Pointer operator '{}' cannot be applied to types '{}' and '{}'",
          operator, left_type, right_type
        )
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
      DiagnosticMessage::DynamicVectorsNotSupported { .. } => {
        write!(
          f,
          "Dynamic-size vectors are not supported; use a fixed-size array `T[N]` or `Vector<T>` from std"
        )
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
      DiagnosticMessage::TypeParamAlreadyDefined { name, .. } => {
        write!(f, "Type parameter '{}' is already defined", name)
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
      DiagnosticMessage::EnumNotFound { name, .. } => {
        write!(f, "'{}' is not an enum", name)
      },
      DiagnosticMessage::EnumVariantNotFound { enum_name, variant, .. } => {
        write!(f, "Enum '{}' has no variant named '{}'", enum_name, variant)
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
      DiagnosticMessage::UseAfterDrop { var_name, .. } => {
        write!(f, "Use of dropped value '{}'", var_name)
      },
      DiagnosticMessage::DoubleDrop { var_name, .. } => {
        write!(f, "Value '{}' has already been dropped", var_name)
      },
      DiagnosticMessage::InconsistentDropInBranches { var_name, .. } => {
        write!(f, "Variable '{}' is dropped in one branch but not the other", var_name)
      },
      DiagnosticMessage::DoubleFree { var_name, .. } => {
        write!(f, "Pointer '{}' has already been freed", var_name)
      },
      DiagnosticMessage::PossibleLeakToFFI { var_name, .. } => {
        write!(f, "Possible memory leak: ownership of '{}' escapes to FFI", var_name)
      },
      DiagnosticMessage::OwnershipEscapeToGlobal { var_name, .. } => {
        write!(f, "Ownership of '{}' escapes to global scope", var_name)
      },

      // Builtin errors
      DiagnosticMessage::InvalidSizeOfOperand { .. } => {
        write!(f, "sizeOf requires an explicit cast to a concrete type")
      },
      DiagnosticMessage::InvalidMinMaxType { func_name, got, .. } => {
        write!(f, "{} requires a numeric type, got '{}'", func_name, got)
      },

      // Static field errors
      DiagnosticMessage::StaticFieldRequiresInit { field, type_name, .. } => {
        write!(f, "Static field '{}' in '{}' requires an initializer", field, type_name)
      },

      DiagnosticMessage::StaticOnEnumVariant { variant, .. } => {
        write!(f, "Enum variant '{}' cannot be declared static", variant)
      },

      DiagnosticMessage::StaticFieldNotConst { field, type_name, .. } => {
        write!(
          f,
          "Static field '{}' in '{}' must be initialized with a constant expression",
          field, type_name
        )
      },

      // For-of errors
      DiagnosticMessage::ForOfExpectsVector { got, .. } => {
        write!(f, "for-of expects vector type, got '{}'", got)
      },
      DiagnosticMessage::ForOfRequiresCopyOrRef { element_type, .. } => {
        write!(
          f,
          "for-of by value requires Copy element type; '{}' is not Copy, use '&{}'",
          element_type, element_type
        )
      },
      DiagnosticMessage::ForOfMutRequiresMutableIter { .. } => {
        write!(f, "cannot take mutable reference in for-of over immutable vector")
      },

      // Overload errors
      DiagnosticMessage::NoOverloadMatches {
        name,
        available_signatures,
        arg_types,
        ..
      } => {
        write!(
          f,
          "No overload matches call to '{}' with arguments ({})\n  Available signatures:\n    - {}",
          name,
          arg_types.join(", "),
          available_signatures.join("\n    - ")
        )
      },
      DiagnosticMessage::AmbiguousOverload {
        name,
        matching_signatures,
        arg_types,
        ..
      } => {
        write!(
          f,
          "Ambiguous overload call to '{}' with arguments ({})\n  Matching signatures:\n    - {}",
          name,
          arg_types.join(", "),
          matching_signatures.join("\n    - ")
        )
      },
      DiagnosticMessage::OverloadGroupAsValue { name, .. } => {
        write!(f, "Cannot use overload group '{}' as a value; call it with arguments", name)
      },
      DiagnosticMessage::DuplicateOverload { name, signature, .. } => {
        write!(f, "Duplicate overload signature for '{}': {}", name, signature)
      },
      DiagnosticMessage::MainFunctionCannotBeOverloaded { .. } => {
        write!(f, "Function 'main' cannot be overloaded")
      },
      DiagnosticMessage::LibraryCannotHaveMainFunction { .. } => {
        write!(f, "Library cannot have a 'main' function")
      },
      DiagnosticMessage::ExecutableMustHaveMainFunction { .. } => {
        write!(f, "Executable must have a 'main' function")
      },

      // Visibility errors
      DiagnosticMessage::PrivateFieldAccess { field, type_name, .. } => {
        write!(f, "Field '{}' of '{}' is private", field, type_name)
      },
      DiagnosticMessage::PrivateMethodAccess { method, type_name, .. } => {
        write!(f, "Method '{}' of '{}' is private", method, type_name)
      },

      // Builtin call errors
      DiagnosticMessage::UnknownBuiltin { name, .. } => {
        write!(f, "Unknown builtin '@{}'", name)
      },
      DiagnosticMessage::CompileError { message, .. } => {
        write!(f, "{}", message)
      },
      DiagnosticMessage::BuiltinArgCount {
        name, expected, got, ..
      } => {
        write!(f, "@{} expects {} argument(s), got {}", name, expected, got)
      },
      DiagnosticMessage::BuiltinExpectedStringLiteral { name, .. } => {
        write!(f, "@{} expects a string literal argument", name)
      },
      DiagnosticMessage::BuiltinTypeConstraint { name, constraint, .. } => {
        write!(f, "@{}: {}", name, constraint)
      },
      DiagnosticMessage::UnknownConfigFlag { key, .. } => {
        write!(f, "Unknown config flag '{}'", key)
      },
      DiagnosticMessage::UnknownAttribute { name, target, .. } => {
        write!(f, "unknown attribute '@{}' on {}", name, target)
      },
      DiagnosticMessage::AttributeArgCount {
        attr, expected, got, ..
      } => {
        write!(f, "@{} expects {} argument(s), got {}", attr, expected, got)
      },
      DiagnosticMessage::AttributeExpectedString { attr, .. } => {
        write!(f, "@{} expects a string argument", attr)
      },
      DiagnosticMessage::AttributeExpectedInt { attr, .. } => {
        write!(f, "@{} expects an integer argument", attr)
      },
      DiagnosticMessage::AlignmentNotPowerOfTwo { value, .. } => {
        write!(f, "alignment {} is not a power of two", value)
      },
      DiagnosticMessage::UnusedVariable { name, .. } => {
        write!(f, "unused variable '{}'", name)
      },
      DiagnosticMessage::UnusedImport { name, .. } => {
        write!(f, "unused import '{}'", name)
      },
      DiagnosticMessage::DeprecatedCall { name, message, .. } => {
        write!(f, "use of deprecated function '{}': {}", name, message)
      },
      DiagnosticMessage::UnknownLint { name, .. } => {
        write!(f, "unknown lint '{}'", name)
      },
      DiagnosticMessage::AttributeExpectedIdentifier { attr, .. } => {
        write!(f, "@{} expects an identifier argument", attr)
      },
      DiagnosticMessage::UnknownLangTrait { name, .. } => {
        write!(f, "unknown lang trait '{}' in @implements", name)
      },
      DiagnosticMessage::LangTraitDropCopyConflict { type_name, .. } => {
        write!(f, "type '{}' cannot implement both Drop and Copy", type_name)
      },
      DiagnosticMessage::LangTraitMissingMethod {
        trait_name,
        method_name,
        type_name,
        ..
      } => {
        write!(
          f,
          "type '{}' declares @implements({}) but has no '{}' method",
          type_name, trait_name, method_name
        )
      },
      DiagnosticMessage::LangTraitInvalidSignature {
        trait_name,
        method_name,
        expected,
        got,
        ..
      } => {
        write!(
          f,
          "{} method '{}' has wrong signature: expected '{}', got '{}'",
          trait_name, method_name, expected, got
        )
      },
      DiagnosticMessage::LangTraitNotApplicable { trait_name, .. } => {
        write!(
          f,
          "@implements({}) is not supported on enums (enums cannot have instance methods)",
          trait_name
        )
      },
      DiagnosticMessage::CopyOnNonCopyField {
        type_name,
        field_name,
        field_type,
        ..
      } => {
        write!(
          f,
          "type '{}' declares @implements(Copy) but field '{}' has non-Copy type '{}'",
          type_name, field_name, field_type
        )
      },
      DiagnosticMessage::CopyOnNonCopyVariantPayload {
        type_name,
        variant_name,
        payload_type,
        payload_index,
        ..
      } => {
        write!(
          f,
          "type '{}' declares @implements(Copy) but variant '{}' payload {} has non-Copy type '{}'",
          type_name, variant_name, payload_index, payload_type
        )
      },
      DiagnosticMessage::ExtensionInvalidTargetType { type_name, .. } => {
        write!(f, "@extension target type '{}' is not a valid primitive type", type_name)
      },
      DiagnosticMessage::ExtensionRequiresParameter { .. } => {
        write!(f, "@extension function must have at least one parameter (receiver)")
      },
      DiagnosticMessage::ExtensionReceiverTypeMismatch { expected, got, .. } => {
        write!(
          f,
          "first parameter type must be '{}' or a reference to it, got '{}'",
          expected, got
        )
      },
      DiagnosticMessage::ExtensionMethodOnLiteral { method, type_name, .. } => {
        write!(
          f,
          "cannot call extension method '{}' on a {} literal; assign to a variable first",
          method, type_name
        )
      },
      DiagnosticMessage::ExtensionMethodOnTemporary { method, type_name, .. } => {
        write!(
          f,
          "cannot call extension method '{}' on a temporary {}; assign to a variable first",
          method, type_name
        )
      },

      // Trait diagnostics
      DiagnosticMessage::TraitMissingRequiredMethod {
        trait_name,
        method_name,
        type_name,
        ..
      } => {
        write!(
          f,
          "trait '{}' requires method '{}' but it is not implemented on '{}'",
          trait_name, method_name, type_name
        )
      },
      DiagnosticMessage::TraitMethodSignatureMismatch {
        trait_name,
        method_name,
        expected,
        got,
        ..
      } => {
        write!(
          f,
          "method '{}' signature does not match trait '{}': expected '{}', found '{}'",
          method_name, trait_name, expected, got
        )
      },
      DiagnosticMessage::UnknownTraitInImplements { name, .. } => {
        write!(f, "unknown trait '{}' in @implements", name)
      },
      DiagnosticMessage::TraitInExternBlock { .. } => {
        write!(f, "traits cannot be declared in extern blocks")
      },
      DiagnosticMessage::TraitMethodRequiresSelf { method_name, .. } => {
        write!(
          f,
          "trait method '{}' must have a self parameter (&self or &mut self)",
          method_name
        )
      },
      DiagnosticMessage::TraitFieldNotAllowed { .. } => {
        write!(f, "fields are not allowed in trait declarations")
      },
      DiagnosticMessage::TraitStaticMethodNotAllowed { method_name, .. } => {
        write!(
          f,
          "static methods are not allowed in trait declarations; '{}' must have &self or &mut self",
          method_name
        )
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
      | DiagnosticMessage::InvalidProperty(at)
      | DiagnosticMessage::ExpectedCallAfterTypeArgs { span: at }
      | DiagnosticMessage::RecursionLimitExceeded(at) => at.clone(),

      DiagnosticMessage::UndeclaredVariable { span, .. }
      | DiagnosticMessage::VariableAlreadyDefined { span, .. }
      | DiagnosticMessage::FunctionAlreadyDefined { span, .. }
      | DiagnosticMessage::TypeAlreadyDefined { span, .. }
      | DiagnosticMessage::TypeAliasCycle { span, .. }
      | DiagnosticMessage::UndefinedType { span, .. }
      | DiagnosticMessage::WrongNumberOfTypeArgs { span, .. }
      | DiagnosticMessage::TypeParamCannotHaveArgs { span, .. }
      | DiagnosticMessage::TypeAliasCannotHaveArgs { span, .. }
      | DiagnosticMessage::TypeCannotBeParameterized { span, .. }
      | DiagnosticMessage::CannotInferTypeParam { span, .. }
      | DiagnosticMessage::FieldNotFound { span, .. }
      | DiagnosticMessage::MethodMustBeCalled { span, .. }
      | DiagnosticMessage::DotAccessOnEnum { span, .. }
      | DiagnosticMessage::DotAccessOnNonRecord { span, .. }
      | DiagnosticMessage::StaticAccessOnNonType { span, .. }
      | DiagnosticMessage::StaticMemberNotFound { span, .. }
      | DiagnosticMessage::EnumVariantRequiresPayload { span, .. }
      | DiagnosticMessage::MemberNotFoundInNamespace { span, .. }
      | DiagnosticMessage::NotARecord { span, .. }
      | DiagnosticMessage::UnknownField { span, .. }
      | DiagnosticMessage::DuplicateFieldInit { span, .. }
      | DiagnosticMessage::MissingFieldInit { span, .. }
      | DiagnosticMessage::ImmutableAssignment { span, .. }
      | DiagnosticMessage::MutableReferenceToImmutable { span, .. }
      | DiagnosticMessage::MutatingMethodOnImmutable { span, .. }
      | DiagnosticMessage::InvalidCast { span, .. }
      | DiagnosticMessage::PrecisionLossCast { span, .. }
      | DiagnosticMessage::ArgumentCountMismatch { span, .. }
      | DiagnosticMessage::ArgumentTypeMismatch { span, .. }
      | DiagnosticMessage::InvalidBinaryOperandType { span, .. }
      | DiagnosticMessage::InvalidUnaryOperandType { span, .. }
      | DiagnosticMessage::InvalidNullLiteral { span, .. }
      | DiagnosticMessage::CannotInferNullType { span, .. }
      | DiagnosticMessage::NullDereference { span, .. }
      | DiagnosticMessage::InvalidPointerArithmetic { span, .. }
      | DiagnosticMessage::DereferenceNonPointer { span, .. }
      | DiagnosticMessage::VectorIndexNonInteger { span, .. }
      | DiagnosticMessage::AccessNonVector { span, .. }
      | DiagnosticMessage::IndexOutOfBounds { span, .. }
      | DiagnosticMessage::DynamicVectorsNotSupported { span, .. }
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
      | DiagnosticMessage::TypeParamAlreadyDefined { span, .. }
      | DiagnosticMessage::ConstantAlreadyDefined { span, .. }
      | DiagnosticMessage::UndeclaredIdentifier { span, .. }
      | DiagnosticMessage::FunctionPathNotAsCallee { span, .. }
      | DiagnosticMessage::UnsupportedPathExpression { span, .. }
      | DiagnosticMessage::EnumNotFound { span, .. }
      | DiagnosticMessage::EnumVariantNotFound { span, .. }
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
      | DiagnosticMessage::UseAfterDrop { span, .. }
      | DiagnosticMessage::DoubleDrop { span, .. }
      | DiagnosticMessage::InconsistentDropInBranches { span, .. }
      | DiagnosticMessage::DoubleFree { span, .. }
      | DiagnosticMessage::PossibleLeakToFFI { span, .. }
      | DiagnosticMessage::OwnershipEscapeToGlobal { span, .. }
      | DiagnosticMessage::InvalidSizeOfOperand { span, .. }
      | DiagnosticMessage::InvalidMinMaxType { span, .. }
      | DiagnosticMessage::StaticFieldRequiresInit { span, .. }
      | DiagnosticMessage::StaticOnEnumVariant { span, .. }
      | DiagnosticMessage::StaticFieldNotConst { span, .. }
      | DiagnosticMessage::ForOfExpectsVector { span, .. }
      | DiagnosticMessage::ForOfRequiresCopyOrRef { span, .. }
      | DiagnosticMessage::ForOfMutRequiresMutableIter { span, .. }
      | DiagnosticMessage::NoOverloadMatches { span, .. }
      | DiagnosticMessage::AmbiguousOverload { span, .. }
      | DiagnosticMessage::OverloadGroupAsValue { span, .. }
      | DiagnosticMessage::DuplicateOverload { span, .. }
      | DiagnosticMessage::MainFunctionCannotBeOverloaded { span, .. }
      | DiagnosticMessage::LibraryCannotHaveMainFunction { span, .. }
      | DiagnosticMessage::ExecutableMustHaveMainFunction { span, .. }
      | DiagnosticMessage::PrivateFieldAccess { span, .. }
      | DiagnosticMessage::PrivateMethodAccess { span, .. }
      | DiagnosticMessage::UnknownBuiltin { span, .. }
      | DiagnosticMessage::CompileError { span, .. }
      | DiagnosticMessage::BuiltinArgCount { span, .. }
      | DiagnosticMessage::BuiltinExpectedStringLiteral { span, .. }
      | DiagnosticMessage::BuiltinTypeConstraint { span, .. }
      | DiagnosticMessage::UnknownConfigFlag { span, .. }
      | DiagnosticMessage::UnknownAttribute { span, .. }
      | DiagnosticMessage::AttributeArgCount { span, .. }
      | DiagnosticMessage::AttributeExpectedString { span, .. }
      | DiagnosticMessage::AttributeExpectedInt { span, .. }
      | DiagnosticMessage::AlignmentNotPowerOfTwo { span, .. }
      | DiagnosticMessage::UnusedVariable { span, .. }
      | DiagnosticMessage::UnusedImport { span, .. }
      | DiagnosticMessage::DeprecatedCall { span, .. }
      | DiagnosticMessage::UnknownLint { span, .. }
      | DiagnosticMessage::AttributeExpectedIdentifier { span, .. }
      | DiagnosticMessage::UnknownLangTrait { span, .. }
      | DiagnosticMessage::LangTraitDropCopyConflict { span, .. }
      | DiagnosticMessage::LangTraitMissingMethod { span, .. }
      | DiagnosticMessage::LangTraitInvalidSignature { span, .. }
      | DiagnosticMessage::LangTraitNotApplicable { span, .. }
      | DiagnosticMessage::CopyOnNonCopyField { span, .. }
      | DiagnosticMessage::CopyOnNonCopyVariantPayload { span, .. }
      | DiagnosticMessage::ExtensionInvalidTargetType { span, .. }
      | DiagnosticMessage::ExtensionRequiresParameter { span, .. }
      | DiagnosticMessage::ExtensionReceiverTypeMismatch { span, .. }
      | DiagnosticMessage::ExtensionMethodOnLiteral { span, .. }
      | DiagnosticMessage::ExtensionMethodOnTemporary { span, .. }
      | DiagnosticMessage::TraitMissingRequiredMethod { span, .. }
      | DiagnosticMessage::TraitMethodSignatureMismatch { span, .. }
      | DiagnosticMessage::UnknownTraitInImplements { span, .. }
      | DiagnosticMessage::TraitInExternBlock { span, .. }
      | DiagnosticMessage::TraitMethodRequiresSelf { span, .. }
      | DiagnosticMessage::TraitFieldNotAllowed { span, .. }
      | DiagnosticMessage::TraitStaticMethodNotAllowed { span, .. } => span.clone(),
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
      DiagnosticMessage::TypeAlreadyDefined { .. } => "A0052",
      DiagnosticMessage::TypeAliasCycle { .. } => "A0066",
      DiagnosticMessage::UndefinedType { .. } => "I0043",
      DiagnosticMessage::WrongNumberOfTypeArgs { .. } => "A0070",
      DiagnosticMessage::TypeParamCannotHaveArgs { .. } => "A0071",
      DiagnosticMessage::TypeAliasCannotHaveArgs { .. } => "A0072",
      DiagnosticMessage::TypeCannotBeParameterized { .. } => "A0073",
      DiagnosticMessage::CannotInferTypeParam { .. } => "A0074",
      DiagnosticMessage::FieldNotFound { .. } => "A0054",
      DiagnosticMessage::MethodMustBeCalled { .. } => "A0061",
      DiagnosticMessage::DotAccessOnEnum { .. } => "A0060",
      DiagnosticMessage::DotAccessOnNonRecord { .. } => "A0060",
      DiagnosticMessage::StaticAccessOnNonType { .. } => "A0063",
      DiagnosticMessage::StaticMemberNotFound { .. } => "A0062",
      DiagnosticMessage::EnumVariantRequiresPayload { .. } => "A0057",
      DiagnosticMessage::MemberNotFoundInNamespace { .. } => "A0062",
      DiagnosticMessage::NotARecord { .. } => "A0053",
      DiagnosticMessage::UnknownField { .. } => "A0056",
      DiagnosticMessage::DuplicateFieldInit { .. } => "A0064",
      DiagnosticMessage::MissingFieldInit { .. } => "A0055",
      DiagnosticMessage::ExpectedInteger(_) => "I0044",
      DiagnosticMessage::ExpectedFloat(_) => "I0045",
      DiagnosticMessage::ExpectedHex(_) => "I0046",
      DiagnosticMessage::ExpectedBinary(_) => "I0047",
      DiagnosticMessage::ImmutableAssignment { .. } => "A0013",
      DiagnosticMessage::MutableReferenceToImmutable { .. } => "A0014",
      DiagnosticMessage::MutatingMethodOnImmutable { .. } => "A0076",
      DiagnosticMessage::InvalidCast { .. } => "A0015",
      DiagnosticMessage::PrecisionLossCast { .. } => "A0016",
      DiagnosticMessage::ArgumentCountMismatch { .. } => "A0017",
      DiagnosticMessage::ArgumentTypeMismatch { .. } => "A0018",
      DiagnosticMessage::InvalidBinaryOperandType { .. } => "A0019",
      DiagnosticMessage::InvalidUnaryOperandType { .. } => "A0020",
      DiagnosticMessage::InvalidNullLiteral { .. } => "A0072",
      DiagnosticMessage::CannotInferNullType { .. } => "A0073",
      DiagnosticMessage::NullDereference { .. } => "A0074",
      DiagnosticMessage::InvalidPointerArithmetic { .. } => "A0075",
      DiagnosticMessage::DereferenceNonPointer { .. } => "A0021",
      DiagnosticMessage::VectorIndexNonInteger { .. } => "A0022",
      DiagnosticMessage::AccessNonVector { .. } => "A0023",
      DiagnosticMessage::IndexOutOfBounds { .. } => "A0048",
      DiagnosticMessage::DynamicVectorsNotSupported { .. } => "A0147",
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
      DiagnosticMessage::TypeParamAlreadyDefined { .. } => "A0069",
      DiagnosticMessage::ConstantAlreadyDefined { .. } => "A0034",
      DiagnosticMessage::UndeclaredIdentifier { .. } => "A0035",
      DiagnosticMessage::FunctionPathNotAsCallee { .. } => "A0050",
      DiagnosticMessage::UnsupportedPathExpression { .. } => "A0051",
      DiagnosticMessage::EnumNotFound { .. } => "A0058",
      DiagnosticMessage::EnumVariantNotFound { .. } => "A0059",
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
      DiagnosticMessage::InvalidMinMaxType { .. } => "A0072",
      DiagnosticMessage::StaticFieldRequiresInit { .. } => "A0065",
      DiagnosticMessage::StaticOnEnumVariant { .. } => "A0067",
      DiagnosticMessage::StaticFieldNotConst { .. } => "A0068",
      DiagnosticMessage::ModuleNotFound { .. } => "M0001",
      DiagnosticMessage::SymbolNotExported { .. } => "M0002",
      DiagnosticMessage::ImportShadowsLocal { .. } => "M0003",
      DiagnosticMessage::CircularDependency { .. } => "M0004",
      DiagnosticMessage::UseAfterMove { .. } => "O0001",
      DiagnosticMessage::UseAfterFree { .. } => "O0002",
      DiagnosticMessage::InconsistentMoveInBranches { .. } => "O0003",
      DiagnosticMessage::PossibleLeakToFFI { .. } => "O0004",
      DiagnosticMessage::OwnershipEscapeToGlobal { .. } => "O0005",
      DiagnosticMessage::UseAfterDrop { .. } => "O0006",
      DiagnosticMessage::DoubleDrop { .. } => "O0007",
      DiagnosticMessage::InconsistentDropInBranches { .. } => "O0008",
      DiagnosticMessage::DoubleFree { .. } => "O0009",
      DiagnosticMessage::ForOfExpectsVector { .. } => "A0069",
      DiagnosticMessage::ForOfRequiresCopyOrRef { .. } => "A0070",
      DiagnosticMessage::ForOfMutRequiresMutableIter { .. } => "A0071",
      DiagnosticMessage::ExpectedCallAfterTypeArgs { .. } => "I0048",
      DiagnosticMessage::RecursionLimitExceeded(_) => "I0049",
      DiagnosticMessage::NoOverloadMatches { .. } => "A0100",
      DiagnosticMessage::AmbiguousOverload { .. } => "A0101",
      DiagnosticMessage::OverloadGroupAsValue { .. } => "A0102",
      DiagnosticMessage::DuplicateOverload { .. } => "A0103",
      DiagnosticMessage::MainFunctionCannotBeOverloaded { .. } => "A0104",
      DiagnosticMessage::LibraryCannotHaveMainFunction { .. } => "L0101",
      DiagnosticMessage::ExecutableMustHaveMainFunction { .. } => "L0102",
      DiagnosticMessage::PrivateFieldAccess { .. } => "A0105",
      DiagnosticMessage::PrivateMethodAccess { .. } => "A0106",
      DiagnosticMessage::UnknownBuiltin { .. } => "A0110",
      DiagnosticMessage::CompileError { .. } => "A0111",
      DiagnosticMessage::BuiltinArgCount { .. } => "A0112",
      DiagnosticMessage::BuiltinExpectedStringLiteral { .. } => "A0113",
      DiagnosticMessage::BuiltinTypeConstraint { .. } => "A0116",
      DiagnosticMessage::UnknownConfigFlag { .. } => "A0115",
      DiagnosticMessage::UnknownAttribute { .. } => "A0117",
      DiagnosticMessage::AttributeArgCount { .. } => "A0118",
      DiagnosticMessage::AttributeExpectedString { .. } => "A0119",
      DiagnosticMessage::AttributeExpectedInt { .. } => "A0120",
      DiagnosticMessage::AlignmentNotPowerOfTwo { .. } => "A0121",
      DiagnosticMessage::UnusedVariable { .. } => "A0122",
      DiagnosticMessage::UnusedImport { .. } => "A0123",
      DiagnosticMessage::DeprecatedCall { .. } => "A0124",
      DiagnosticMessage::UnknownLint { .. } => "A0125",
      DiagnosticMessage::AttributeExpectedIdentifier { .. } => "A0126",
      DiagnosticMessage::UnknownLangTrait { .. } => "A0130",
      DiagnosticMessage::LangTraitDropCopyConflict { .. } => "A0131",
      DiagnosticMessage::LangTraitMissingMethod { .. } => "A0132",
      DiagnosticMessage::LangTraitInvalidSignature { .. } => "A0133",
      DiagnosticMessage::LangTraitNotApplicable { .. } => "A0134",
      DiagnosticMessage::CopyOnNonCopyField { .. } => "A0148",
      DiagnosticMessage::CopyOnNonCopyVariantPayload { .. } => "A0149",
      DiagnosticMessage::ExtensionInvalidTargetType { .. } => "A0135",
      DiagnosticMessage::ExtensionRequiresParameter { .. } => "A0136",
      DiagnosticMessage::ExtensionReceiverTypeMismatch { .. } => "A0137",
      DiagnosticMessage::ExtensionMethodOnLiteral { .. } => "A0138",
      DiagnosticMessage::ExtensionMethodOnTemporary { .. } => "A0139",
      DiagnosticMessage::TraitMissingRequiredMethod { .. } => "A0140",
      DiagnosticMessage::TraitMethodSignatureMismatch { .. } => "A0141",
      DiagnosticMessage::UnknownTraitInImplements { .. } => "A0142",
      DiagnosticMessage::TraitInExternBlock { .. } => "A0143",
      DiagnosticMessage::TraitMethodRequiresSelf { .. } => "A0144",
      DiagnosticMessage::TraitFieldNotAllowed { .. } => "A0145",
      DiagnosticMessage::TraitStaticMethodNotAllowed { .. } => "A0146",
    }
    .to_string()
  }

  fn level(&self) -> Severity {
    match self {
      DiagnosticMessage::PrecisionLossCast { .. }
      | DiagnosticMessage::UnreachableCode { .. }
      | DiagnosticMessage::MissingReturnStatement { .. }
      | DiagnosticMessage::PossibleLeakToFFI { .. }
      | DiagnosticMessage::OwnershipEscapeToGlobal { .. }
      | DiagnosticMessage::UnknownConfigFlag { .. }
      | DiagnosticMessage::UnusedVariable { .. }
      | DiagnosticMessage::UnusedImport { .. }
      | DiagnosticMessage::DeprecatedCall { .. } => Severity::Warning,
      _ => Severity::Error,
    }
  }

  fn secondary_labels(&self) -> Vec<(Span, String)> {
    match self {
      DiagnosticMessage::VariableAlreadyDefined { previous_span, .. }
      | DiagnosticMessage::FunctionAlreadyDefined { previous_span, .. }
      | DiagnosticMessage::TypeAlreadyDefined { previous_span, .. }
      | DiagnosticMessage::ParameterAlreadyDefined { previous_span, .. }
      | DiagnosticMessage::TypeParamAlreadyDefined { previous_span, .. }
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

  pub fn report_with_severity(
    &self,
    severity: Severity,
  ) -> Diagnostic {
    let mut diagnostic = Diagnostic::new(severity, self.to_string(), self.code(), self.primary_span());
    for (span, message) in self.secondary_labels() {
      diagnostic = diagnostic.with_label(span, message);
    }
    diagnostic
  }
}
