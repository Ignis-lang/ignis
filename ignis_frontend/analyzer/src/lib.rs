mod diagnostic;
use std::path::Path;
use std::{collections::HashMap, vec, fs};

use std::fmt::{Display, Formatter};

use ast::expression::array_access::ArrayAccess;
use ast::expression::this::This;
use diagnostic::{AnalyzerDiagnostic, AnalyzerDiagnosticError};
use diagnostic_report::DiagnosticReport;
use intermediate_representation::instruction_type::IRInstructionType;
use intermediate_representation::ir_array_access::IRArrayAccess;
use intermediate_representation::ir_for::IRFor;
use intermediate_representation::ir_for_of::IRForOf;
use intermediate_representation::ir_get::{IRGet, GetMetadata};
use intermediate_representation::ir_method::IRMethod;
use intermediate_representation::ir_method_call::{IRMethodCall, MethodCallMetadata};
use intermediate_representation::ir_set::IRSet;
use intermediate_representation::ir_this::IRThis;
use intermediate_representation::{
  analyzer_value::AnalyzerValue,
  IRInstruction,
  binary::IRBinary,
  logical::IRLogical,
  literal::IRLiteral,
  unary::IRUnary,
  function::{IRFunction, IRFunctionMetadata},
  variable::{IRVariable, IRVariableMetadata},
  block::IRBlock,
  assign::IRAssign,
  ternary::IRTernary,
  call::IRCall,
  ir_if::IRIf,
  ir_while::IRWhile,
  ir_return::IRReturn,
  ir_array::IRArray,
  import::IRImport,
  ir_break::IRBreak,
  ir_continue::IRContinue,
  class::IRClass,
  class_instance::IRClassInstance,
};

use lexer::Lexer;
use token::token::Token;

use ast::{
  visitor::Visitor,
  expression::{
    binary::Binary, Expression, literal::Literal, unary::Unary, grouping::Grouping,
    logical::Logical, assign::Assign, variable::VariableExpression, ternary::Ternary, call::Call,
    array::Array, new::NewExpression, get::Get, set::Set, method_call::MethodCall,
  },
  statement::{
    Statement,
    expression::ExpressionStatement,
    block::Block,
    variable::Variable,
    if_statement::IfStatement,
    while_statement::WhileStatement,
    function::{FunctionStatement, FunctionDecorator},
    return_statement::Return,
    class::Class,
    for_of::ForOf,
    import::Import,
    break_statement::BreakStatement,
    continue_statement::Continue,
    for_statement::For,
    method::MethodStatement,
    property::PropertyStatement,
  },
};
use enums::{data_type::DataType, token_type::TokenType};
use parser::Parser;

pub type AnalyzerResult = Result<IRInstruction, Box<AnalyzerDiagnostic>>;
type CheckCompatibility<T> = (bool, T);

#[derive(Debug, Clone, PartialEq)]
enum AnalyzerContext {
  Variable(Token),
  Function,
  Method,
  Class,
  Loop,
  Switch,
  Match,
}

impl Display for AnalyzerContext {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self {
      AnalyzerContext::Variable(token) => write!(f, "Variable: {}", token.span.literal),
      AnalyzerContext::Function => write!(f, "Function"),
      AnalyzerContext::Method => write!(f, "Method"),
      AnalyzerContext::Class => write!(f, "Class"),
      AnalyzerContext::Loop => write!(f, "Loop"),
      AnalyzerContext::Switch => write!(f, "Switch"),
      AnalyzerContext::Match => write!(f, "Match"),
    }
  }
}

#[derive(Debug, Clone)]
enum CalleableDeclaration {
  Function(IRFunction),
  Method(IRMethod),
  // Lambda,
}

pub struct Analyzer {
  pub irs: HashMap<String, Vec<IRInstruction>>,
  pub diagnostics: Vec<Box<AnalyzerDiagnostic>>,
  tokens: Vec<Token>,
  pub block_stack: Vec<HashMap<String, bool>>,
  scopes_variables: Vec<IRVariable>,
  current_function: Option<CalleableDeclaration>,
  current_file: String,
  current_class: Option<IRClass>,
  context: Vec<AnalyzerContext>,
}

impl Visitor<AnalyzerResult> for Analyzer {
  fn visit_binary_expression(&mut self, expression: &Binary) -> AnalyzerResult {
    let left = self.analyzer(&expression.left)?;
    let right = self.analyzer(&expression.right)?;
    let operator = expression.operator.clone();

    let left_type = self.extract_data_type(&left);
    let right_type = self.extract_data_type(&right);

    let instruction_type = if operator.kind == TokenType::Plus {
      if left_type == DataType::String && right_type == DataType::String {
        IRInstructionType::Concatenate
      } else {
        IRInstructionType::Add
      }
    } else {
      IRInstructionType::from_token_kind(&operator.kind)
    };

    let (result, data_type) = self.are_types_compatible(&left, &right, &instruction_type);
    let left_type = self.extract_data_type(&left);
    let right_type = self.extract_data_type(&right);

    if !result {
      return Err(Box::new(AnalyzerDiagnostic::new(
        AnalyzerDiagnosticError::TypeMismatch(left_type, right_type, operator.clone()),
        self.find_token_line(&operator.span.line),
      )));
    }

    let instruction = IRInstruction::Binary(IRBinary::new(
      instruction_type,
      Box::new(left),
      Box::new(right),
      data_type,
    ));

    Ok(instruction)
  }

  fn visit_grouping_expression(&mut self, expression: &Grouping) -> AnalyzerResult {
    self.analyzer(&expression.expression)
  }

  fn visit_literal_expression(&mut self, expression: &Literal) -> AnalyzerResult {
    let instruction = IRInstruction::Literal(IRLiteral::new(AnalyzerValue::from_literation_value(
      expression.value.clone(),
    )));

    Ok(instruction)
  }

  fn visit_unary_expression(&mut self, expression: &Unary) -> AnalyzerResult {
    let right = self.analyzer(&expression.right)?;
    let instruction_type = IRInstructionType::from_token_kind(&expression.operator.kind);
    let right_type = self.extract_data_type(&right);

    if !self.are_types_unary_compatible(&right, &instruction_type) {
      return Err(Box::new(AnalyzerDiagnostic::new(
        AnalyzerDiagnosticError::TypeMismatchUnary(right_type, expression.operator.clone()),
        self.find_token_line(&expression.operator.span.line),
      )));
    }

    let instruction =
      IRInstruction::Unary(IRUnary::new(instruction_type, Box::new(right), right_type));

    Ok(instruction)
  }

  fn visit_variable_expression(&mut self, variable: &VariableExpression) -> AnalyzerResult {
    if self.block_stack.is_empty() {
      return Err(Box::new(AnalyzerDiagnostic::new(
        AnalyzerDiagnosticError::UndeclaredVariable(variable.name.clone()),
        self.find_token_line(&variable.name.span.line),
      )));
    }

    let irs = &self.irs.get(&self.current_file).unwrap();
    let is_function = irs.iter().find(|ir| match ir {
      IRInstruction::Function(f) => f.name.span.literal == variable.name.span.literal,
      _ => false,
    });

    if let Some(IRInstruction::Function(f)) = is_function {
      let function = f.clone();

      let instruction = IRInstruction::Function(function);

      return Ok(instruction);
    }

    if let Some(f) = &mut self.current_function {
      match f {
        CalleableDeclaration::Function(f) => {
          if f.name.span.literal == variable.name.span.literal {
            f.metadata.is_recursive = true;

            let instruction = IRInstruction::Function(f.clone());

            return Ok(instruction);
          }
        }
        CalleableDeclaration::Method(method) => {
          if method.name.span.literal == variable.name.span.literal {
            method.metadata.is_recursive = true;
            let instruction = IRInstruction::Method(method.clone());
            return Ok(instruction);
          }
        }
      }
    }

    if let Some(c) = &self.current_class {
      let method = c
        .methods
        .iter()
        .find(|m| m.name.span.literal == variable.name.span.literal);
      if let Some(m) = method {
        let instruction = IRInstruction::Method(m.clone());
        return Ok(instruction);
      }
    }

    let env = self.block_stack.last();

    if let Some(block) = env {
      if block.get(&variable.name.span.literal).is_none() {
        return Err(Box::new(AnalyzerDiagnostic::new(
          AnalyzerDiagnosticError::UndeclaredVariable(variable.name.clone()),
          self.find_token_line(&variable.name.span.line),
        )));
      }

      let is_declared = *block.get(variable.name.span.literal.as_str()).unwrap();

      if !is_declared {
        return Err(Box::new(AnalyzerDiagnostic::new(
          AnalyzerDiagnosticError::UndeclaredVariable(variable.name.clone()),
          self.find_token_line(&variable.name.span.line),
        )));
      }

      let mut variable = match self
        .scopes_variables
        .iter()
        .find(|v| v.name == variable.name.span.literal)
      {
        Some(v) => v.clone(),
        None => {
          return Err(Box::new(AnalyzerDiagnostic::new(
            AnalyzerDiagnosticError::UndeclaredVariable(variable.name.clone()),
            self.find_token_line(&variable.name.span.line),
          )))
        }
      };

      variable.metadata.is_declaration = false;

      let instruction = IRInstruction::Variable(variable.clone());

      Ok(instruction)
    } else {
      Err(Box::new(AnalyzerDiagnostic::new(
        AnalyzerDiagnosticError::UndeclaredVariable(variable.name.clone()),
        self.find_token_line(&variable.name.span.line),
      )))
    }
  }

  fn visit_assign_expression(&mut self, expression: &Assign) -> AnalyzerResult {
    if self.block_stack.is_empty()
      || self
        .block_stack
        .last()
        .unwrap()
        .get(&expression.name.span.literal)
        .is_none()
    {
      return Err(Box::new(AnalyzerDiagnostic::new(
        AnalyzerDiagnosticError::UndefinedVariable(expression.name.clone()),
        self.find_token_line(&expression.name.span.line),
      )));
    }

    self
      .context
      .push(AnalyzerContext::Variable(expression.name.clone()));

    let value = self.analyzer(&expression.value)?;

    self.context.pop();

    let current_block = self.block_stack.last().unwrap();

    let env = current_block.iter().find(|(name, is_declared)| {
      name.as_str() == expression.name.span.literal.as_str() && **is_declared
    });

    if let Some((name, _)) = env {
      let variable = self
        .scopes_variables
        .iter()
        .find(|v| v.name == *name)
        .unwrap();

      if variable.metadata.is_mutable {
        let instruction = IRInstruction::Assign(IRAssign::new(
          expression.name.span.literal.clone(),
          Box::new(value),
        ));

        Ok(instruction)
      } else {
        Err(Box::new(AnalyzerDiagnostic::new(
          AnalyzerDiagnosticError::InvalidReassignedVariable(expression.name.clone()),
          self.find_token_line(&expression.name.span.line),
        )))
      }
    } else {
      Err(Box::new(AnalyzerDiagnostic::new(
        AnalyzerDiagnosticError::UndefinedVariable(expression.name.clone()),
        self.find_token_line(&expression.name.span.line),
      )))
    }
  }

  fn visit_logical_expression(&mut self, expression: &Logical) -> AnalyzerResult {
    let left = self.analyzer(&expression.left)?;
    let right = self.analyzer(&expression.right)?;

    let instruction_type = IRInstructionType::from_token_kind(&expression.operator.kind);

    match instruction_type {
      IRInstructionType::And | IRInstructionType::Or => {
        if !self.are_types_logical_compatibel(&left, &right, &instruction_type) {
          return Err(Box::new(AnalyzerDiagnostic::new(
            AnalyzerDiagnosticError::TypeMismatch(
              self.extract_data_type(&left),
              self.extract_data_type(&right),
              expression.operator.clone(),
            ),
            self.find_token_line(&expression.operator.span.line),
          )));
        }

        let instruction = IRInstruction::Logical(IRLogical::new(
          instruction_type,
          Box::new(left),
          Box::new(right),
        ));

        Ok(instruction)
      }
      _ => Err(Box::new(AnalyzerDiagnostic::new(
        AnalyzerDiagnosticError::InvalidOperator(expression.operator.clone()),
        self.find_token_line(&expression.operator.span.line),
      ))),
    }
  }

  fn visit_ternary_expression(&mut self, expression: &Ternary) -> AnalyzerResult {
    let condition = self.analyzer(&expression.condition)?;
    let then_branch = self.analyzer(&expression.then_branch)?;
    let else_branch = self.analyzer(&expression.else_branch)?;

    if self.extract_data_type(&condition) != DataType::Boolean {
      return Err(Box::new(AnalyzerDiagnostic::new(
        AnalyzerDiagnosticError::InvalidCondition(*expression.token.clone()),
        self.find_token_line(&expression.token.span.line),
      )));
    }

    let then_type = self.extract_data_type(&then_branch);
    let else_type = self.extract_data_type(&else_branch);

    if then_type != else_type {
      return Err(Box::new(AnalyzerDiagnostic::new(
        AnalyzerDiagnosticError::TypeMismatch(then_type, else_type, *expression.token.clone()),
        self.find_token_line(&expression.token.span.line),
      )));
    }

    Ok(IRInstruction::Ternary(IRTernary::new(
      Box::new(condition),
      Box::new(then_branch),
      Box::new(else_branch),
      then_type,
    )))
  }

  fn visit_call_expression(&mut self, expression: &Call) -> AnalyzerResult {
    let calle = self.analyzer(&expression.callee)?;

    let parameters: Vec<IRInstruction>;
    let function_name: Token;
    let return_type: DataType;

    match calle {
      IRInstruction::Function(f) => {
        parameters = f.parameters.clone();
        function_name = f.name.clone();
        return_type = f.return_type.clone();
      }
      IRInstruction::Method(m) => {
        parameters = m.parameters.clone();
        function_name = m.name.clone();
        return_type = m.return_type.clone();
      }
      _ => {
        return Err(Box::new(AnalyzerDiagnostic::new(
          AnalyzerDiagnosticError::NotCallable(expression.paren.clone()),
          self.find_token_line(&expression.paren.span.line),
        )))
      }
    };

    if parameters.len() != expression.arguments.len() {
      return Err(Box::new(AnalyzerDiagnostic::new(
        AnalyzerDiagnosticError::InvalidNumberOfArguments(
          parameters.len(),
          expression.arguments.len(),
          function_name,
        ),
        self.find_token_line(&expression.paren.span.line),
      )));
    }

    let mut arguments = Vec::<IRInstruction>::new();

    for (i, arg) in expression.arguments.iter().enumerate() {
      let parameter = if let IRInstruction::Variable(v) = &parameters[i] {
        v
      } else {
        return Err(Box::new(AnalyzerDiagnostic::new(
          AnalyzerDiagnosticError::NotCallable(expression.paren.clone()),
          self.find_token_line(&expression.paren.span.line),
        )));
      };

      let arg_type = self.analyzer(arg)?;

      let kind = match &arg_type {
        IRInstruction::Literal(l) => l.value.to_data_type(),
        IRInstruction::Variable(v) => v.data_type.clone(),
        IRInstruction::Function(f) => f.return_type.clone(),
        IRInstruction::Call(c) => c.return_type.clone(),
        IRInstruction::Return(r) => r.data_type.clone(),
        IRInstruction::Binary(b) => b.data_type.clone(),
        IRInstruction::Unary(u) => u.data_type.clone(),
        IRInstruction::Logical(_) => DataType::Boolean,
        IRInstruction::Class(c) => DataType::ClassType(c.name.clone()),
        IRInstruction::ClassInstance(c) => DataType::ClassType(c.class.name.clone()),
        IRInstruction::Array(a) => a.data_type.clone(),
        _ => DataType::Unwnown,
      };

      let data_type = self.extract_data_type(&parameters[i]);

      if kind != data_type
        && kind != DataType::Unwnown
        && data_type != DataType::Array(Box::new(DataType::Unwnown))
        && data_type != DataType::Unwnown
      {
        return Err(Box::new(AnalyzerDiagnostic::new(
          AnalyzerDiagnosticError::ArgumentTypeMismatch(
            data_type.clone(),
            kind,
            expression.paren.clone(),
          ),
          self.find_token_line(&expression.paren.span.line),
        )));
      }

      if let IRInstruction::Variable(v) = &arg_type {
        if data_type != DataType::Unwnown {
          match (v.metadata.is_mutable, parameter.metadata.is_mutable) {
            (true, true) | (false, false) => (),
            _ => {
              return Err(Box::new(AnalyzerDiagnostic::new(
                AnalyzerDiagnosticError::ImmutableVariableAsMutableParameter(
                  v.name.clone(),
                  v.name.clone(),
                  expression.paren.clone(),
                ),
                self.find_token_line(&expression.paren.span.line),
              )))
            }
          };
        }
      }

      arguments.push(arg_type);
    }

    let instruction = IRInstruction::Call(IRCall::new(function_name, arguments, return_type));

    Ok(instruction)
  }

  fn visit_array_expression(&mut self, expression: &Array) -> AnalyzerResult {
    let mut elements = Vec::new();
    let mut element_types = Vec::new();

    for elem in &expression.elements {
      let analyzed_elem = self.analyzer(elem)?;
      let elem_type = self.extract_data_type(&analyzed_elem);

      elements.push(analyzed_elem);
      element_types.push(elem_type);
    }

    let first_type = element_types.first().unwrap_or(&DataType::Unwnown);

    if !element_types.iter().all(|t| t == first_type) {
      return Err(Box::new(AnalyzerDiagnostic::new(
        AnalyzerDiagnosticError::ArrayElementTypeMismatch(expression.token.clone()),
        self.find_token_line(&expression.token.span.line),
      )));
    }

    let instruction = IRInstruction::Array(IRArray::new(
      elements,
      DataType::Array(Box::new(first_type.clone())),
    ));

    Ok(instruction)
  }

  fn visit_new_expression(&mut self, expression: &NewExpression) -> AnalyzerResult {
    let class_info = self.find_class_in_ir(&expression.name.span.literal);

    if class_info.is_none() {
      return Err(Box::new(AnalyzerDiagnostic::new(
        AnalyzerDiagnosticError::UndefinedClass(expression.name.clone()),
        self.find_token_line(&expression.name.span.line),
      )));
    }

    let class = class_info.unwrap();

    if class.methods.is_empty() {
      return Err(Box::new(AnalyzerDiagnostic::new(
        AnalyzerDiagnosticError::UndefinedMethods(expression.name.clone()),
        self.find_token_line(&expression.name.span.line),
      )));
    }

    let mut arguments = Vec::<IRInstruction>::new();

    for arg in &expression.arguments {
      let arg_type = self.analyzer(arg)?;

      arguments.push(arg_type);
    }

    let constructor = self.find_matching_constructor(&class, &arguments);

    if constructor.is_none() {
      return Err(Box::new(AnalyzerDiagnostic::new(
        AnalyzerDiagnosticError::UndefinedMethods(expression.name.clone()),
        self.find_token_line(&expression.name.span.line),
      )));
    }

    let constructor = constructor.unwrap();

    let mut instruction = IRClassInstance::new(Box::new(class), constructor.name, arguments);

    if let Some(c) = self.context.last() {
      if let AnalyzerContext::Variable(n) = c.clone() {
        instruction.var_name = n.clone()
      }
    }

    let instruction = IRInstruction::ClassInstance(instruction);

    Ok(instruction)
  }

  fn visit_get_expression(&mut self, expression: &Get) -> AnalyzerResult {
    let ir_object = self.analyzer(&expression.object)?;

    let object = match ir_object {
      IRInstruction::Variable(c) => c,
      _ => {
        return Err(Box::new(AnalyzerDiagnostic::new(
          AnalyzerDiagnosticError::NotAClass(*expression.object_token.clone()),
          self.find_token_line(&expression.name.span.line),
        )))
      }
    };

    let object_data_type = object.data_type.clone();

    let class = self.find_class_in_ir(match &object.data_type {
      DataType::ClassType(name) => name,
      DataType::Array(_) => return self.array_property(expression),
      _ => {
        return Err(Box::new(AnalyzerDiagnostic::new(
          AnalyzerDiagnosticError::NotAClass(*expression.object_token.clone()),
          self.find_token_line(&expression.object_token.span.line),
        )))
      }
    });

    if class.is_none() {
      return Err(Box::new(AnalyzerDiagnostic::new(
        AnalyzerDiagnosticError::UndefinedClass(*expression.object_token.clone()),
        self.find_token_line(&expression.name.span.line),
      )));
    }

    let class = class.unwrap();

    if class.properties.is_empty() {
      return Err(Box::new(AnalyzerDiagnostic::new(
        AnalyzerDiagnosticError::UndefinedProperty(*expression.name.clone()),
        self.find_token_line(&expression.name.span.line),
      )));
    }

    let class_binding = class.clone();

    let property = class_binding
      .properties
      .iter()
      .find(|p| p.name == expression.name.span.literal);

    if property.is_none() {
      return Err(Box::new(AnalyzerDiagnostic::new(
        AnalyzerDiagnosticError::UndefinedProperty(*expression.name.clone()),
        self.find_token_line(&expression.name.span.line),
      )));
    }

    let class_instance = self.find_class_instance(&object.name);

    if class_instance.is_none() {
      return Err(Box::new(AnalyzerDiagnostic::new(
        AnalyzerDiagnosticError::UndefinedClass(*expression.name.clone()),
        self.find_token_line(&expression.name.span.line),
      )));
    }

    let instruction = IRInstruction::Get(IRGet::new(
      expression.name.span.literal.clone(),
      Box::new(IRInstruction::ClassInstance(
        class_instance.unwrap().clone(),
      )),
      self.extract_data_type(&IRInstruction::Variable(property.unwrap().clone())),
      GetMetadata::new(object_data_type.clone()),
    ));

    Ok(instruction)
  }

  fn visit_set_expression(&mut self, set: &Set) -> AnalyzerResult {
    let value = self.analyzer(&set.value)?;

    let object = self.analyzer(&set.object)?;

    let object = match object {
      IRInstruction::Variable(c) => c,
      _ => {
        return Err(Box::new(AnalyzerDiagnostic::new(
          AnalyzerDiagnosticError::NotAClass(*set.name.clone()),
          self.find_token_line(&set.name.span.line),
        )))
      }
    };

    let class = self.find_class_in_ir(match &object.data_type {
      DataType::ClassType(name) => name,
      _ => {
        return Err(Box::new(AnalyzerDiagnostic::new(
          AnalyzerDiagnosticError::NotAClass(*set.name.clone()),
          self.find_token_line(&set.name.span.line),
        )))
      }
    });

    if class.is_none() {
      return Err(Box::new(AnalyzerDiagnostic::new(
        AnalyzerDiagnosticError::UndefinedClass(*set.name.clone()),
        self.find_token_line(&set.name.span.line),
      )));
    }

    let class = class.unwrap();

    if class.properties.is_empty() {
      return Err(Box::new(AnalyzerDiagnostic::new(
        AnalyzerDiagnosticError::UndefinedProperty(*set.name.clone()),
        self.find_token_line(&set.name.span.line),
      )));
    }

    let class_binding = class.clone();

    let property = class_binding
      .properties
      .iter()
      .find(|p| p.name == set.name.span.literal);

    if property.is_none() {
      return Err(Box::new(AnalyzerDiagnostic::new(
        AnalyzerDiagnosticError::UndefinedProperty(*set.name.clone()),
        self.find_token_line(&set.name.span.line),
      )));
    }

    if !property.unwrap().metadata.is_public {
      return Err(Box::new(AnalyzerDiagnostic::new(
        AnalyzerDiagnosticError::PrivateProperty(*set.name.clone()),
        self.find_token_line(&set.name.span.line),
      )));
    }

    if !property.unwrap().metadata.is_mutable {
      return Err(Box::new(AnalyzerDiagnostic::new(
        AnalyzerDiagnosticError::ImmutableProperty(*set.name.clone()),
        self.find_token_line(&set.name.span.line),
      )));
    }

    let class_instance = self.find_class_instance(&class.name);

    if class_instance.is_none() {
      return Err(Box::new(AnalyzerDiagnostic::new(
        AnalyzerDiagnosticError::UndefinedClass(*set.name.clone()),
        self.find_token_line(&set.name.span.line),
      )));
    }

    let instruction = IRInstruction::Set(IRSet::new(
      set.name.span.literal.clone(),
      Box::new(value),
      Box::new(class_instance.unwrap()),
      self.extract_data_type(&IRInstruction::Variable(property.unwrap().clone())),
      object.name,
    ));

    Ok(instruction)
  }

  fn visit_method_call_expression(&mut self, method_call: &MethodCall) -> AnalyzerResult {
    let instance = self.analyzer(&method_call.object)?;

    let instance_type = self.extract_data_type(&instance);

    if matches!(
      instance_type,
      DataType::Int
        | DataType::Float
        | DataType::Boolean
        | DataType::String
        | DataType::Unwnown
        | DataType::Array(_)
    ) {
      return self.type_methods(method_call);
    }

    let object = match instance {
      IRInstruction::Variable(c) => c,
      _ => {
        return Err(Box::new(AnalyzerDiagnostic::new(
          AnalyzerDiagnosticError::NotAClass(*method_call.name.clone()),
          self.find_token_line(&method_call.name.span.line),
        )))
      }
    };

    let class = self.find_class_in_ir(match &object.data_type {
      DataType::ClassType(name) | DataType::Variable(name) => name,
      _ => {
        return Err(Box::new(AnalyzerDiagnostic::new(
          AnalyzerDiagnosticError::NotAClass(*method_call.name.clone()),
          self.find_token_line(&method_call.name.span.line),
        )))
      }
    });

    if class.is_none() {
      return Err(Box::new(AnalyzerDiagnostic::new(
        AnalyzerDiagnosticError::UndefinedClass(*method_call.name.clone()),
        self.find_token_line(&method_call.name.span.line),
      )));
    }

    let class = class.unwrap();

    if class.methods.is_empty() {
      return Err(Box::new(AnalyzerDiagnostic::new(
        AnalyzerDiagnosticError::UndefinedMethods(*method_call.name.clone()),
        self.find_token_line(&method_call.name.span.line),
      )));
    }

    let class_binding = class.clone();

    let method = class_binding
      .methods
      .iter()
      .find(|p| p.name.span.literal == method_call.name.span.literal);

    if method.is_none() {
      return Err(Box::new(AnalyzerDiagnostic::new(
        AnalyzerDiagnosticError::UndefinedProperty(*method_call.name.clone()),
        self.find_token_line(&method_call.name.span.line),
      )));
    }

    let method = method.unwrap();

    if !method.metadata.is_public {
      return Err(Box::new(AnalyzerDiagnostic::new(
        AnalyzerDiagnosticError::PrivateProperty(*method_call.name.clone()),
        self.find_token_line(&method_call.name.span.line),
      )));
    }

    let class_instance = self.find_class_instance(&object.name);

    if class_instance.is_none() {
      return Err(Box::new(AnalyzerDiagnostic::new(
        AnalyzerDiagnosticError::UndefinedClass(*method_call.name.clone()),
        self.find_token_line(&method_call.name.span.line),
      )));
    }

    self.current_class = Some(class.clone());

    let calle = self.analyzer(&method_call.calle)?;

    let instruction = IRInstruction::MethodCall(IRMethodCall::new(
      method_call.name.clone(),
      Box::new(calle),
      method.return_type.clone(),
      Box::new(IRInstruction::ClassInstance(
        class_instance.clone().unwrap().clone(),
      )),
      MethodCallMetadata::new(
        method.return_type.clone(),
        DataType::ClassType(class_instance.unwrap().class.name.clone()),
      ),
    ));

    self.current_class = None;

    Ok(instruction)
  }

  fn visit_array_access_expression(&mut self, array: &ArrayAccess) -> AnalyzerResult {
    let var = self.analyzer(&array.variable)?;
    let index = self.analyzer(&array.index)?;

    let var_type = self.extract_data_type(&var);

    if !self.is_array(&var) {
      return Err(Box::new(AnalyzerDiagnostic::new(
        AnalyzerDiagnosticError::NotAnArray(*array.name.clone()),
        self.find_token_line(&array.name.span.line),
      )));
    }

    let var = match var {
      IRInstruction::Variable(v) => v,
      _ => {
        return Err(Box::new(AnalyzerDiagnostic::new(
          AnalyzerDiagnosticError::NotAnArray(*array.name.clone()),
          self.find_token_line(&array.name.span.line),
        )));
      }
    };

    match &index {
      IRInstruction::Literal(l) => {
        if !matches!(l.value, AnalyzerValue::Int(_)) {
          return Err(Box::new(AnalyzerDiagnostic::new(
            AnalyzerDiagnosticError::InvalidArrayIndex(*array.name.clone()),
            self.find_token_line(&array.name.span.line),
          )));
        }
      }
      _ => {
        let kind = self.extract_data_type(&index);

        if kind != DataType::Int {
          return Err(Box::new(AnalyzerDiagnostic::new(
            AnalyzerDiagnosticError::InvalidArrayIndex(*array.name.clone()),
            self.find_token_line(&array.name.span.line),
          )));
        }
      }
    }

    let instruction = IRInstruction::ArrayAccess(IRArrayAccess::new(
      var.name.clone(),
      Box::new(index),
      var_type,
    ));

    Ok(instruction)
  }

  fn visit_this_expression(&mut self, this: &This) -> AnalyzerResult {
    if self.current_class.is_none() {
      return Err(Box::new(AnalyzerDiagnostic::new(
        AnalyzerDiagnosticError::ThisOutsideOfClass(this.keyword.clone()),
        self.find_token_line(&this.keyword.span.line),
      )));
    }

    let mut access: Option<Box<IRInstruction>> = None;
    let mut data_type: DataType = DataType::Null;
    if this.access.is_some() {
      let result = self.analyzer(this.access.as_ref().unwrap())?;
      data_type = self.extract_data_type(&result);
      access = Some(Box::new(result))
    }

    let instruction = IRInstruction::This(IRThis::new(
      Box::new(this.keyword.clone()),
      access,
      Box::new(data_type),
    ));

    Ok(instruction)
  }

  fn visit_expression_statement(&mut self, statement: &ExpressionStatement) -> AnalyzerResult {
    self.analyzer(&statement.expression)
  }

  fn visit_variable_statement(&mut self, variable: &Variable) -> AnalyzerResult {
    if self.is_allready_declared(&variable.name.span.literal) {
      return Err(Box::new(AnalyzerDiagnostic::new(
        AnalyzerDiagnosticError::VariableAlreadyDefined(
          variable.name.span.literal.clone(),
          *variable.name.clone(),
        ),
        self.find_token_line(&variable.name.span.line),
      )));
    }

    self.declare(&variable.name.span.literal);

    let mut value = IRInstruction::Literal(IRLiteral::new(AnalyzerValue::Null));
    let data_type = variable.type_annotation.clone();

    if let Some(initializer) = &variable.initializer {
      self
        .context
        .push(AnalyzerContext::Variable(*variable.name.clone()));

      let expression = self.analyzer(initializer)?;

      value = match expression {
        IRInstruction::Literal(literal) => IRInstruction::Literal(literal),
        IRInstruction::Binary(binary) => IRInstruction::Binary(binary),
        IRInstruction::Unary(unary) => IRInstruction::Unary(unary),
        IRInstruction::Variable(variable) => IRInstruction::Variable(variable),
        IRInstruction::Ternary(ternary) => IRInstruction::Ternary(ternary),
        IRInstruction::Call(call) => IRInstruction::Call(call),
        IRInstruction::Class(class) => IRInstruction::Class(class),
        IRInstruction::Logical(logical) => IRInstruction::Logical(logical),
        IRInstruction::Array(array) => IRInstruction::Array(array),
        IRInstruction::ClassInstance(class) => IRInstruction::ClassInstance(class),
        IRInstruction::MethodCall(call) => IRInstruction::MethodCall(call),
        _ => {
          return Err(Box::new(AnalyzerDiagnostic::new(
            AnalyzerDiagnosticError::InvalidVariableInitializer(*variable.name.clone()),
            self.find_token_line(&variable.name.span.line),
          )))
        }
      };

      self.context.pop();
    }

    let mut complex_type = None;

    if let DataType::Variable(value) = &data_type {
      let class = self.find_class_in_ir(value);

      if class.is_some() {
        let class = class.unwrap();

        complex_type = Some(Box::new(IRInstruction::ClassInstance(
          IRClassInstance::new(Box::new(class), *variable.name.clone(), Vec::new()),
        )));
      }
    }

    let variable = IRVariable::new(
      variable.name.span.literal.clone(),
      data_type.clone(),
      Some(Box::new(value.clone())),
      IRVariableMetadata::new(
        variable.metadata.is_mutable,
        variable.metadata.is_reference,
        variable.metadata.is_parameter,
        false,
        false,
        true,
        variable.metadata.is_static,
        variable.metadata.is_public,
        false,
        complex_type,
      ),
    );

    self.define(&variable.name);

    self.scopes_variables.push(variable.clone());

    Ok(IRInstruction::Variable(variable.clone()))
  }

  fn visit_block(&mut self, block: &Block) -> AnalyzerResult {
    let scopes_variables = self.scopes_variables.clone();

    self.begin_scope();

    let mut ir_block = IRBlock::new(Vec::new(), Vec::new());

    for statement in &block.statements {
      let result = self.analyze_statement(statement)?;
      ir_block.instructions.push(result);
    }

    self.end_scope();

    self.scopes_variables = scopes_variables;

    Ok(IRInstruction::Block(ir_block))
  }

  fn visit_if_statement(&mut self, statement: &IfStatement) -> AnalyzerResult {
    let condition = self.analyzer(&statement.condition)?;
    let then_branch = self.analyze_statement(&statement.then_branch)?;

    let else_branch: Option<Box<IRInstruction>> = if statement.else_branch.is_some() {
      Some(Box::new(
        self.analyze_statement(statement.else_branch.as_ref().unwrap())?,
      ))
    } else {
      None
    };

    let instruction = IRInstruction::If(IRIf::new(
      Box::new(condition),
      Box::new(then_branch),
      else_branch,
    ));

    Ok(instruction)
  }

  fn visit_while_statement(&mut self, statement: &WhileStatement) -> AnalyzerResult {
    self.context.push(AnalyzerContext::Loop);
    let condition = self.analyzer(&statement.condition)?;
    let body = self.analyze_statement(&statement.body)?;

    let instruction = IRInstruction::While(IRWhile::new(Box::new(condition), Box::new(body)));

    self.context.pop();

    Ok(instruction)
  }

  fn visit_function_statement(&mut self, statement: &FunctionStatement) -> AnalyzerResult {
    self.begin_scope();
    self.context.push(AnalyzerContext::Function);
    let mut parameters = Vec::<IRInstruction>::new();

    if self.is_allready_declared(&statement.name.span.literal) {
      return Err(Box::new(AnalyzerDiagnostic::new(
        AnalyzerDiagnosticError::FunctionAlreadyDefined(
          statement.name.span.literal.clone(),
          statement.name.clone(),
        ),
        self.find_token_line(&statement.name.span.line),
      )));
    }

    self.declare(&statement.name.span.literal);
    self.define(&statement.name.span.literal);

    for param in &statement.parameters {
      let parameter = self.analyze_statement(&Statement::Variable(param.clone()))?;

      parameters.push(parameter);
    }

    let mut ir: IRBlock = IRBlock::new(Vec::new(), Vec::new());

    let is_extern = statement
      .annotations
      .clone()
      .into_iter()
      .any(|a| matches!(&a, FunctionDecorator::Extern(_)));

    let mut current_function = IRFunction::new(
      statement.name.clone(),
      parameters.clone(),
      statement.return_type.clone().unwrap_or(DataType::Void),
      None,
      IRFunctionMetadata::new(
        false,
        statement.is_exported,
        false,
        is_extern,
        false,
        false,
        false,
        false,
      ),
    );

    self.current_function = Some(CalleableDeclaration::Function(current_function.clone()));

    for body in &statement.body {
      let result = self.analyze_statement(body)?;

      match result {
        IRInstruction::Variable(v) => {
          self.scopes_variables.push(v.clone());
          ir.scopes_variables.push(v);
        }
        _ => {
          ir.instructions.push(result);
        }
      };
    }

    self.end_scope();

    current_function = match self.current_function.as_ref().unwrap() {
      CalleableDeclaration::Function(f) => f.clone(),
      _ => todo!(),
    };

    current_function.body = Some(Box::new(ir.clone()));

    let instruction = IRInstruction::Function(current_function);

    self.context.pop();
    self.current_function = None;

    Ok(instruction)
  }

  fn visit_return_statement(&mut self, statement: &Return) -> AnalyzerResult {
    if !self.context.iter().any(|context| {
      matches!(
        &context,
        AnalyzerContext::Function | AnalyzerContext::Method
      )
    }) {
      return Err(Box::new(AnalyzerDiagnostic::new(
        AnalyzerDiagnosticError::ReturnOutsideFunction(*statement.keyword.clone()),
        self.find_token_line(&statement.keyword.span.line),
      )));
    }

    let value = &statement.value;
    if value.is_none() {
      let instruction = IRInstruction::Return(IRReturn::new(
        Box::new(IRInstruction::Literal(IRLiteral::new(AnalyzerValue::Null))),
        DataType::Void,
      ));

      return Ok(instruction);
    }

    let value = self.analyzer(value.as_ref().unwrap())?;
    let data_type = self.extract_data_type(&value);

    let instruction = IRInstruction::Return(IRReturn::new(Box::new(value), data_type));

    Ok(instruction)
  }

  fn visit_class_statement(&mut self, statement: &Class) -> AnalyzerResult {
    self.context.push(AnalyzerContext::Class);
    let mut properties = Vec::<IRVariable>::new();

    if self.is_allready_declared(&statement.name.span.literal) {
      return Err(Box::new(AnalyzerDiagnostic::new(
        AnalyzerDiagnosticError::ClassAlreadyDefined(statement.name.clone()),
        self.find_token_line(&statement.name.span.line),
      )));
    }

    self.begin_scope();

    self.declare(&statement.name.span.literal);
    self.define(&statement.name.span.literal);

    for property in &statement.properties {
      let result = self.analyze_statement(property);

      match result {
        Ok(IRInstruction::Variable(v)) => {
          self.scopes_variables.push(v.clone());
          properties.push(v);
        }
        _ => {
          todo!()
        }
      };
    }

    let mut ir: Vec<IRMethod> = Vec::new();

    let mut current_class = IRClass::new(
      statement.name.span.literal.clone(),
      Vec::new(),
      properties,
      statement.metadata.is_exported,
      false,
    );

    self.current_class = Some(current_class.clone());

    for method in &statement.methods {
      let result = self.analyze_statement(method)?;

      match result {
        IRInstruction::Method(f) => {
          self.scopes_variables.push(IRVariable::new(
            f.name.span.literal.clone(),
            DataType::Unwnown,
            None,
            IRVariableMetadata::new(
              false,
              false,
              false,
              false,
              false,
              false,
              false,
              false,
              f.metadata.is_constructor,
              None,
            ),
          ));

          ir.push(f);
        }
        _ => {
          return Err(Box::new(AnalyzerDiagnostic::new(
            AnalyzerDiagnosticError::ClassAlreadyDefined(statement.name.clone()),
            self.find_token_line(&statement.name.span.line),
          )));
        }
      };
    }

    self.end_scope();
    self.declare(&statement.name.span.literal);
    self.define(&statement.name.span.literal);

    current_class = self.current_class.as_ref().unwrap().clone();

    current_class.methods = ir.clone();

    let instruction = IRInstruction::Class(current_class.clone());

    self.context.pop();
    self.current_class = None;

    Ok(instruction)
  }

  fn visit_for_statement(&mut self, _statement: &For) -> AnalyzerResult {
    let variable = self.analyze_statement(&Statement::Variable(*_statement.variable.clone()))?;
    let condition = self.analyzer(&_statement.condition)?;
    let increment = self.analyzer(&_statement.increment)?;

    let body = self.analyze_statement(&_statement.body)?;

    Ok(IRInstruction::For(IRFor::new(
      Box::new(variable),
      Box::new(condition),
      Box::new(increment),
      Box::new(body),
    )))
  }

  fn visit_for_of_statement(&mut self, statement: &ForOf) -> AnalyzerResult {
    self.declare(&statement.variable.name.span.literal);

    let iterable = self.analyzer(&statement.iterable)?;
    let data_type = self.extract_data_type(&iterable);

    if !self.is_iterable(&iterable) {
      return Err(Box::new(AnalyzerDiagnostic::new(
        AnalyzerDiagnosticError::NotIterable(statement.token.clone()),
        self.find_token_line(&statement.token.span.line),
      )));
    }

    self.begin_scope();

    self.define(&statement.variable.name.span.literal);

    let variable = IRVariable::new(
      statement.variable.name.span.literal.clone(),
      data_type,
      None,
      IRVariableMetadata::new(
        statement.variable.metadata.is_mutable,
        statement.variable.metadata.is_reference,
        false,
        false,
        false,
        false,
        false,
        false,
        false,
        None,
      ),
    );

    self.scopes_variables.push(variable.clone());

    let body = self.analyze_statement(&statement.body)?;

    self.end_scope();

    let instruction = IRInstruction::ForOf(IRForOf::new(
      variable,
      Box::new(iterable),
      Box::new(body),
      statement.token.clone(),
    ));

    Ok(instruction)
  }

  fn visit_import_statement(&mut self, statement: &Import) -> AnalyzerResult {
    let mut block_stack: HashMap<String, bool> = self.block_stack.last_mut().unwrap().clone();

    if !statement.is_std {
      self.resolve_module_import(statement, &mut block_stack)?;
    } else {
      self.resolve_std_import(statement.module_path.span.literal.clone(), &mut block_stack);
    }

    Ok(IRInstruction::Import(IRImport::new(
      statement
        .symbols
        .clone()
        .into_iter()
        .map(|i| (i.name, i.alias))
        .collect::<Vec<(Token, Option<Token>)>>(),
      statement.module_path.span.literal.clone(),
    )))
  }

  fn visit_break_statement(&mut self, statement: &BreakStatement) -> AnalyzerResult {
    let is_loop = self
      .context
      .iter()
      .find(|context| matches!(context, AnalyzerContext::Loop | AnalyzerContext::Switch));

    if is_loop.is_none() {
      return Err(Box::new(AnalyzerDiagnostic::new(
        AnalyzerDiagnosticError::BreakOutsideLoop(statement.token.clone()),
        self.find_token_line(&statement.token.span.line),
      )));
    }

    Ok(IRInstruction::Break(IRBreak::new(statement.token.clone())))
  }

  fn visit_continue_statement(&mut self, statement: &Continue) -> AnalyzerResult {
    let is_loop = self
      .context
      .iter()
      .find(|context| matches!(context, AnalyzerContext::Loop));

    if is_loop.is_none() {
      return Err(Box::new(AnalyzerDiagnostic::new(
        AnalyzerDiagnosticError::ContinueOutsideLoop(statement.token.clone()),
        self.find_token_line(&statement.token.span.line),
      )));
    }

    Ok(IRInstruction::Continue(IRContinue::new(
      statement.token.clone(),
    )))
  }

  fn visit_method_statement(&mut self, statement: &MethodStatement) -> AnalyzerResult {
    if self.is_allready_declared(&statement.name.span.literal) && !statement.metadata.is_contructor
    {
      return Err(Box::new(AnalyzerDiagnostic::new(
        AnalyzerDiagnosticError::MethodAlreadyDefined(statement.name.clone()),
        self.find_token_line(&statement.name.span.line),
      )));
    }

    if self
      .context
      .iter()
      .any(|c| !matches!(c, AnalyzerContext::Class))
    {
      return Err(Box::new(AnalyzerDiagnostic::new(
        AnalyzerDiagnosticError::MethodOutsideClass(statement.name.clone()),
        self.find_token_line(&statement.name.span.line),
      )));
    }

    self.begin_scope();
    self.context.push(AnalyzerContext::Method);
    let mut parameters = Vec::<IRInstruction>::new();

    self.declare(&statement.name.span.literal);
    self.define(&statement.name.span.literal);

    for param in &statement.parameters {
      let parameter = self.analyze_statement(&Statement::Variable(param.clone()))?;

      parameters.push(parameter);
    }

    let mut ir: IRBlock = IRBlock::new(Vec::new(), Vec::new());

    let mut current_function = IRMethod::new(
      statement.name.clone(),
      parameters.clone(),
      statement.return_type.clone().unwrap_or(DataType::Void),
      None,
      IRFunctionMetadata::new(
        false,
        false,
        false,
        false,
        statement.metadata.is_static,
        statement.metadata.is_public,
        statement.metadata.is_contructor,
        true,
      ),
      statement.class_name.clone(),
    );

    self.current_function = Some(CalleableDeclaration::Method(current_function.clone()));

    for body in &statement.body {
      let result = self.analyze_statement(body)?;

      match result {
        IRInstruction::Variable(v) => {
          self.scopes_variables.push(v.clone());
          ir.scopes_variables.push(v);
        }
        _ => {
          ir.instructions.push(result);
        }
      };
    }

    self.end_scope();

    current_function = match self.current_function.as_ref().unwrap() {
      CalleableDeclaration::Method(m) => m.clone(),
      _ => unreachable!(),
    };

    current_function.body = Some(Box::new(ir.clone()));

    let instruction = IRInstruction::Method(current_function);

    self.context.pop();
    self.current_function = None;

    Ok(instruction)
  }

  fn visit_property_statement(&mut self, statement: &PropertyStatement) -> AnalyzerResult {
    if self.is_allready_declared(&statement.name.span.literal) {
      return Err(Box::new(AnalyzerDiagnostic::new(
        AnalyzerDiagnosticError::PropertyAlreadyDefined(*statement.name.clone()),
        self.find_token_line(&statement.name.span.line),
      )));
    }

    if self
      .context
      .iter()
      .any(|c| !matches!(c, AnalyzerContext::Class))
    {
      return Err(Box::new(AnalyzerDiagnostic::new(
        AnalyzerDiagnosticError::PropertyOutsideClass(*statement.name.clone()),
        self.find_token_line(&statement.name.span.line),
      )));
    }

    self.declare(&statement.name.span.literal.clone());

    let mut value = IRInstruction::Literal(IRLiteral::new(AnalyzerValue::Null));
    let data_type = statement.type_annotation.clone();

    if let Some(initializer) = &statement.initializer {
      let expression = self.analyzer(initializer)?;
      match expression {
        IRInstruction::Literal(literal) => {
          value = IRInstruction::Literal(literal);
        }
        IRInstruction::Binary(binary) => {
          value = IRInstruction::Binary(binary);
        }
        IRInstruction::Unary(unary) => {
          value = IRInstruction::Unary(unary);
        }
        IRInstruction::Variable(variable) => {
          value = IRInstruction::Variable(variable);
        }
        IRInstruction::Ternary(ternary) => {
          value = IRInstruction::Ternary(ternary);
        }
        IRInstruction::Call(call) => {
          value = IRInstruction::Call(call);
        }
        IRInstruction::Class(class) => {
          value = IRInstruction::Class(class);
        }
        IRInstruction::Logical(logical) => {
          value = IRInstruction::Logical(logical);
        }
        IRInstruction::Array(array) => {
          value = IRInstruction::Array(array);
        }
        _ => (),
      }
    }

    let variable = IRVariable::new(
      statement.name.span.literal.clone(),
      data_type.clone(),
      Some(Box::new(value.clone())),
      IRVariableMetadata::new(
        statement.metadata.is_mutable,
        statement.metadata.is_reference,
        false,
        false,
        false,
        true,
        statement.metadata.is_static,
        statement.metadata.is_public,
        false,
        None,
      ),
    );

    self.define(&variable.name.clone());

    self.scopes_variables.push(variable.clone());

    Ok(IRInstruction::Variable(variable.clone()))
  }
}

impl Analyzer {
  pub fn new(current_file: String, tokens: Vec<Token>) -> Self {
    let mut irs = HashMap::new();
    let block_stack: HashMap<String, bool> = HashMap::new();

    irs.insert(current_file.clone(), Vec::new());

    Self {
      irs,
      tokens,
      diagnostics: Vec::new(),
      block_stack: vec![block_stack],
      scopes_variables: Vec::new(),
      current_function: None,
      current_file,
      current_class: None,
      context: Vec::new(),
    }
  }

  fn find_token_line(&self, line: &usize) -> Vec<Token> {
    self
      .tokens
      .clone()
      .into_iter()
      .filter(|t| t.span.line == *line)
      .collect::<Vec<Token>>()
  }

  pub fn analyze(&mut self, statements: &Vec<Statement>) -> Result<(), Vec<DiagnosticReport>> {
    for statement in statements {
      match self.analyze_statement(statement) {
        Ok(ir) => {
          let current_ir = self.irs.get_mut(&self.current_file).unwrap();
          current_ir.push(ir.clone());
        }
        Err(e) => self.diagnostics.push(e),
      }
    }

    if !self.diagnostics.is_empty() {
      let mut reports = Vec::<DiagnosticReport>::new();

      for diagnostic in &self.diagnostics {
        reports.push(diagnostic.report_diagnostic());
      }

      return Err(reports);
    }

    Ok(())
  }

  fn analyzer(&mut self, expression: &Expression) -> AnalyzerResult {
    expression.accept(self)
  }

  fn analyze_statement(&mut self, statement: &Statement) -> AnalyzerResult {
    statement.accept(self)
  }

  fn begin_scope(&mut self) {
    self
      .block_stack
      .push(self.block_stack.clone().last().unwrap().clone());
  }

  fn end_scope(&mut self) {
    self.block_stack.pop().unwrap();
  }

  fn declare(&mut self, name: &str) {
    if self.block_stack.is_empty() {
      return;
    }

    let current_block = self.block_stack.last_mut().unwrap();

    current_block.insert(name.to_string(), false);
  }

  fn resolve_std_import(&mut self, lib: String, block_stack: &mut HashMap<String, bool>) {
    let current_ir = self.irs.get_mut(&self.current_file).unwrap();
    let mut env = std::env::var("IGNITE_HOME");

    if env.is_err() {
      env = std::env::var("PWD");
    }

    let std_path = Path::new(&env.unwrap()).join("std");

    let file = match lib.clone().as_str() {
      "std:io" => {
        let file = fs::read(std_path.join("io").join("mod.ign"));

        if file.is_err() {
          println!("{:?}", file);
          return;
        }

        file.unwrap()
      }
      &_ => return,
    };

    let source = String::from_utf8(file).unwrap();
    let mut lexer = Lexer::new(&source, lib.clone() + ".ign");
    let _ = lexer.scan_tokens();

    let mut parser = Parser::new(lexer.tokens);
    let statements = parser.parse().0;

    let mut analyzer = Analyzer::new(lib.clone(), Vec::new());
    let _ = analyzer.analyze(&statements);

    analyzer.diagnostics.iter().for_each(|d| {
      self.diagnostics.push(Box::new(*d.clone()));
    });

    for ir in analyzer.irs.get(&lib).unwrap() {
      current_ir.push(ir.clone());

      match ir {
        IRInstruction::Function(f) => {
          block_stack.insert(f.name.span.literal.clone(), true);
        }
        _ => {}
      }
    }
  }

  fn resolve_module_import(
    &mut self,
    statement: &Import,
    block_stack: &mut HashMap<String, bool>,
  ) -> Result<(), Box<AnalyzerDiagnostic>> {
    let mut analyzer = Analyzer::new(statement.module_path.span.literal.clone(), Vec::new());
    let _ = match fs::read_to_string(format!("{}.{}", statement.module_path.span.literal, "ign")) {
      Ok(source) => {
        let mut lexer: Lexer<'_> =
          Lexer::new(&source, statement.module_path.span.literal.clone() + ".ign");
        lexer.scan_tokens();

        analyzer.tokens = lexer.tokens.clone();

        let mut parser: Parser = Parser::new(lexer.tokens);
        let statements = parser.parse();

        analyzer.analyze(&statements.0)
      }
      Err(_) => {
        return Err(Box::new(AnalyzerDiagnostic::new(
          AnalyzerDiagnosticError::ModuleNotFound(statement.module_path.clone()),
          self.find_token_line(&statement.module_path.span.line),
        )))
      }
    };

    analyzer.diagnostics.iter().for_each(|d| {
      self.diagnostics.push(Box::new(*d.clone()));
    });

    let current_ir = analyzer
      .irs
      .get(&statement.module_path.span.literal)
      .unwrap()
      .clone();

    for ir in &current_ir {
      self.define_import(statement, ir.clone(), block_stack)?;
    }

    self.irs.insert(
      statement.module_path.span.literal.clone(),
      current_ir.clone(),
    );

    Ok(())
  }

  fn define_import(
    &mut self,
    statement: &Import,
    ir: IRInstruction,
    block_stack: &mut HashMap<String, bool>,
  ) -> Result<(), Box<AnalyzerDiagnostic>> {
    let current_ir = self.irs.get_mut(&self.current_file).unwrap();

    match ir {
      IRInstruction::Function(f) => {
        for symbol in &statement.symbols {
          if symbol.name.span.literal == f.name.span.literal && !f.metadata.is_exported {
            return Err(Box::new(AnalyzerDiagnostic::new(
              AnalyzerDiagnosticError::ImportedFunctionIsNotExported(symbol.name.clone()),
              self.find_token_line(&symbol.name.span.line),
            )));
          }

          if symbol.name.span.literal == f.name.span.literal && f.metadata.is_exported {
            let mut metadata = f.metadata.clone();
            metadata.is_imported = true;
            if symbol.alias.is_some() {
              block_stack.insert(symbol.alias.as_ref().unwrap().span.literal.clone(), true);
              current_ir.push(
                IRInstruction::Function(IRFunction::new(
                  symbol.alias.as_ref().unwrap().clone(),
                  f.parameters.clone(),
                  f.return_type.clone(),
                  f.body.clone(),
                  metadata,
                ))
                .clone(),
              );
            } else {
              block_stack.insert(symbol.name.span.literal.clone(), true);
              metadata.is_exported = false;
              current_ir.push(
                IRInstruction::Function(IRFunction::new(
                  symbol.name.clone(),
                  f.parameters.clone(),
                  f.return_type.clone(),
                  f.body.clone(),
                  metadata,
                ))
                .clone(),
              );
            }
          }
        }
      }
      IRInstruction::Class(c) => {
        for symbol in &statement.symbols {
          if symbol.name.span.literal == c.name && !c.is_exported {
            return Err(Box::new(AnalyzerDiagnostic::new(
              AnalyzerDiagnosticError::ImportedClassIsNotExported(symbol.name.clone()),
              self.find_token_line(&symbol.name.span.line),
            )));
          }

          if symbol.name.span.literal == c.name && c.is_exported {
            if symbol.alias.is_some() {
              block_stack.insert(symbol.alias.as_ref().unwrap().span.literal.clone(), true);
              current_ir.push(
                IRInstruction::Class(IRClass::new(
                  symbol.alias.as_ref().unwrap().span.literal.clone(),
                  c.methods.clone(),
                  c.properties.clone(),
                  false,
                  true,
                ))
                .clone(),
              );
            } else {
              block_stack.insert(symbol.name.span.literal.clone(), true);
              current_ir.push(
                IRInstruction::Class(IRClass::new(
                  symbol.name.span.literal.clone(),
                  c.methods.clone(),
                  c.properties.clone(),
                  false,
                  true,
                ))
                .clone(),
              );
            }
          }
        }
      }
      _ => (),
    };

    Ok(())
  }

  fn is_allready_declared(&self, name: &String) -> bool {
    if self.block_stack.is_empty() {
      return false;
    }

    let current_block = self.block_stack.last().unwrap();

    current_block.get(name).is_some()
  }

  fn define(&mut self, name: &str) {
    if self.block_stack.is_empty() {
      return;
    }

    let current_block = self.block_stack.last_mut().unwrap();

    current_block.insert(name.to_string(), true);
  }

  fn define_parameter(&mut self, name: &str) {
    if self.block_stack.is_empty() {
      return;
    }

    let current_block = self.block_stack.last_mut().unwrap();

    current_block.insert(name.to_string(), true);
  }

  fn are_types_logical_compatibel(
    &self,
    left: &IRInstruction,
    right: &IRInstruction,
    operator: &IRInstructionType,
  ) -> bool {
    match operator {
      IRInstructionType::And | IRInstructionType::Or => matches!(
        (left, right),
        (
          IRInstruction::Literal(IRLiteral {
            value: AnalyzerValue::Boolean(_),
          }),
          IRInstruction::Literal(IRLiteral {
            value: AnalyzerValue::Boolean(_),
          }),
        )
      ),
      _ => false,
    }
  }

  fn are_types_unary_compatible(
    &self,
    right: &IRInstruction,
    operator: &IRInstructionType,
  ) -> bool {
    match operator {
      IRInstructionType::Sub => matches!(
        right,
        IRInstruction::Literal(IRLiteral {
          value: AnalyzerValue::Int(_),
        }) | IRInstruction::Literal(IRLiteral {
          value: AnalyzerValue::Float(_),
        })
      ),
      IRInstructionType::Not => matches!(
        right,
        IRInstruction::Literal(IRLiteral {
          value: AnalyzerValue::Boolean(_),
        }) | IRInstruction::Literal(IRLiteral {
          value: AnalyzerValue::Int(_),
        }) | IRInstruction::Literal(IRLiteral {
          value: AnalyzerValue::String(_),
        }) | IRInstruction::Literal(IRLiteral {
          value: AnalyzerValue::Float(_),
        }) | IRInstruction::Literal(IRLiteral {
          value: AnalyzerValue::Null,
        })
      ),
      IRInstructionType::Increment | IRInstructionType::Decrement => {
        matches!(
          right,
          IRInstruction::Literal(IRLiteral {
            value: AnalyzerValue::Int(_),
          }) | IRInstruction::Literal(IRLiteral {
            value: AnalyzerValue::Float(_),
          }) | IRInstruction::Variable(IRVariable {
            name: _,
            data_type: DataType::Int | DataType::Float,
            value: _,
            metadata: _
          })
        )
      }
      _ => false,
    }
  }

  fn extract_data_type(&self, instruction: &IRInstruction) -> DataType {
    match instruction {
      IRInstruction::Literal(l) => l.value.to_data_type(),
      IRInstruction::Variable(v) => v.data_type.clone(),
      IRInstruction::Function(f) => f.return_type.clone(),
      IRInstruction::Binary(b) => b.data_type.clone(),
      IRInstruction::Unary(u) => u.data_type.clone(),
      IRInstruction::Logical(_) => DataType::Boolean,
      IRInstruction::Assign(a) => self.extract_data_type(&a.value.clone()),
      IRInstruction::Call(c) => c.return_type.clone(),
      IRInstruction::Return(r) => r.data_type.clone(),
      IRInstruction::Array(array) => array.data_type.clone(),
      IRInstruction::Class(c) => DataType::ClassType(c.name.clone()),
      IRInstruction::ClassInstance(c) => DataType::ClassType(c.class.name.clone()),
      IRInstruction::MethodCall(m) => m.return_type.clone(),
      IRInstruction::Get(g) => g.data_type.clone(),
      IRInstruction::Set(s) => s.data_type.clone(),
      IRInstruction::ArrayAccess(array) => match &array.data_type {
        DataType::Array(t) => *t.clone(),
        _ => DataType::Unwnown,
      },
      IRInstruction::This(this) => *this.data_type.clone(),
      _ => DataType::Unwnown,
    }
  }

  fn check_add_compatibility(
    &self,
    left: &DataType,
    right: &DataType,
  ) -> CheckCompatibility<DataType> {
    match (left, right) {
      (DataType::Int, DataType::Int) => (true, DataType::Int),
      (DataType::Int, DataType::Unwnown) => (true, DataType::Unwnown),
      (DataType::Unwnown, DataType::Int) => (true, DataType::Unwnown),
      (DataType::Float, DataType::Float) => (true, DataType::Float),
      (DataType::Float, DataType::Unwnown) => (true, DataType::Unwnown),
      (DataType::Unwnown, DataType::Float) => (true, DataType::Unwnown),
      (DataType::String, DataType::String) => (true, DataType::String),
      (DataType::String, DataType::Unwnown) => (true, DataType::Unwnown),
      (DataType::Unwnown, DataType::String) => (true, DataType::Unwnown),
      (_, DataType::Null) => (true, left.clone()),
      (DataType::Null, _) => (true, right.clone()),
      (DataType::Unwnown, DataType::Unwnown) => (true, DataType::Unwnown),
      _ => (false, DataType::Unwnown),
    }
  }

  fn check_arithmetic_compatibility(
    &self,
    left: &DataType,
    right: &DataType,
  ) -> CheckCompatibility<DataType> {
    match (left, right) {
      (DataType::Int, DataType::Int) => (true, DataType::Int),
      (DataType::Float, DataType::Float) => (true, DataType::Float),
      (DataType::Int, DataType::Float) => (true, DataType::Float),
      (DataType::Float, DataType::Int) => (true, DataType::Float),
      (_, DataType::Null) => (true, left.clone()),
      (DataType::Null, _) => (true, right.clone()),
      _ => (false, DataType::Unwnown),
    }
  }

  fn check_comparation_compatibility(
    &self,
    left: &DataType,
    right: &DataType,
  ) -> CheckCompatibility<DataType> {
    match (left, right) {
      (DataType::Int, DataType::Int) => (true, DataType::Boolean),
      (DataType::Unwnown, DataType::Int) => (true, DataType::Boolean),
      (DataType::Int, DataType::Unwnown) => (true, DataType::Boolean),
      (DataType::Float, DataType::Float) => (true, DataType::Boolean),
      (DataType::Float, DataType::Unwnown) => (true, DataType::Boolean),
      (DataType::Unwnown, DataType::Float) => (true, DataType::Boolean),
      (DataType::Int, DataType::Float) => (true, DataType::Boolean),
      (DataType::Float, DataType::Int) => (true, DataType::Boolean),
      (_, DataType::Null) => (true, left.clone()),
      (DataType::Null, _) => (true, right.clone()),
      _ => (false, DataType::Unwnown),
    }
  }

  fn check_equal_compatibility(
    &self,
    left: &DataType,
    right: &DataType,
  ) -> CheckCompatibility<DataType> {
    match (left, right) {
      (DataType::Int, DataType::Int) => (true, DataType::Boolean),
      (DataType::Float, DataType::Float) => (true, DataType::Boolean),
      (DataType::String, DataType::String) => (true, DataType::Boolean),
      (DataType::Boolean, DataType::Boolean) => (true, DataType::Boolean),
      (_, DataType::Null) => (true, left.clone()),
      (DataType::Null, _) => (true, right.clone()),
      _ => (false, DataType::Unwnown),
    }
  }

  fn check_logical_compatibility(
    &self,
    left: &DataType,
    right: &DataType,
  ) -> CheckCompatibility<DataType> {
    match (left, right) {
      (DataType::Boolean, DataType::Boolean) => (true, DataType::Boolean),
      _ => (false, DataType::Unwnown),
    }
  }

  fn are_types_compatible(
    &self,
    left: &IRInstruction,
    right: &IRInstruction,
    operator: &IRInstructionType,
  ) -> CheckCompatibility<DataType> {
    let left_type = self.extract_data_type(left);
    let right_type = self.extract_data_type(right);

    match operator {
      IRInstructionType::Concatenate => {
        if left_type == DataType::String && right_type == DataType::String {
          (true, DataType::String)
        } else {
          (false, DataType::Unwnown)
        }
      }
      IRInstructionType::Add => self.check_add_compatibility(&left_type, &right_type),
      IRInstructionType::Sub | IRInstructionType::Mul | IRInstructionType::Div => {
        self.check_arithmetic_compatibility(&left_type, &right_type)
      }
      IRInstructionType::GreaterEqual
      | IRInstructionType::Greater
      | IRInstructionType::LessEqual
      | IRInstructionType::Less => self.check_comparation_compatibility(&left_type, &right_type),
      IRInstructionType::Equal | IRInstructionType::NotEqual => {
        self.check_equal_compatibility(&left_type, &right_type)
      }
      IRInstructionType::And | IRInstructionType::Or => {
        self.check_logical_compatibility(&left_type, &right_type)
      }
      IRInstructionType::Mod => {
        if left_type == DataType::Int && right_type == DataType::Int {
          (true, DataType::Int)
        } else {
          (false, DataType::Unwnown)
        }
      }
      _ => (false, DataType::Unwnown),
    }
  }

  fn is_iterable(&self, iterable: &IRInstruction) -> bool {
    match iterable {
      IRInstruction::Variable(var) => matches!(var.data_type, DataType::Array(_)),
      _ => false,
    }
  }

  fn find_class_in_ir(&self, name: &String) -> Option<IRClass> {
    let irs = self.irs.get(&self.current_file).unwrap();

    let class = irs.iter().find(|ir| match ir {
      IRInstruction::Class(c) => &c.name == name,
      _ => false,
    });

    match class {
      Some(IRInstruction::Class(c)) => Some(c.clone()),
      _ => None,
    }
  }

  fn find_matching_constructor(
    &self,
    class: &IRClass,
    arguments: &[IRInstruction],
  ) -> Option<IRMethod> {
    let constructors = class.methods.iter().filter(|m| m.metadata.is_constructor);
    for constructor in constructors {
      if constructor.parameters.len() != arguments.len() {
        continue;
      }

      let mut is_matching = true;

      for (i, arg) in arguments.iter().enumerate() {
        let arg_type = self.extract_data_type(arg);

        let param_type = self.extract_data_type(&constructor.parameters[i]);

        if arg_type != param_type {
          is_matching = false;
          break;
        }
      }

      if is_matching {
        return Some(constructor.clone());
      }
    }

    None
  }

  fn _verify_arguments(&self, arguments: &[IRInstruction], parameters: &[IRVariable]) -> bool {
    if arguments.len() != parameters.len() {
      return false;
    }

    for (i, arg) in arguments.iter().enumerate() {
      let arg_type = self.extract_data_type(arg);
      let param_type = parameters[i].data_type.clone();
      if arg_type != param_type {
        return false;
      }
    }

    true
  }

  fn find_class_instance(&self, instance_name: &String) -> Option<IRClassInstance> {
    if self.scopes_variables.is_empty() {
      return None;
    }

    let scopes_variables = &self.scopes_variables;

    let mut class_instance = None;

    for variable in scopes_variables {
      if variable.name != instance_name.clone() {
        continue;
      }

      if variable.metadata.complex_data_type.is_some() {
        let complex = variable
          .metadata
          .complex_data_type
          .as_ref()
          .unwrap()
          .clone();
        class_instance = match *complex {
          IRInstruction::ClassInstance(c) => Some(c.clone()),
          _ => None,
        }
      }

      let value = variable.value.clone();

      if value.is_none() {
        continue;
      }
      match value.unwrap().as_ref() {
        IRInstruction::ClassInstance(c) if &c.var_name.span.literal == instance_name => {
          class_instance = Some(c.clone());
          break;
        }
        _ => (),
      }
    }

    class_instance
  }

  fn is_array(&self, var: &IRInstruction) -> bool {
    match var {
      IRInstruction::Variable(v) => matches!(v.data_type, DataType::Array(_)),
      _ => false,
    }
  }

  fn array_property(&mut self, expression: &Get) -> AnalyzerResult {
    let ir_array = self.analyzer(&expression.object)?;

    let array = self.extract_data_type(&ir_array);

    match expression.name.span.literal.as_str() {
      "length" => {
        if !self.is_array(&ir_array) {
          return Err(Box::new(AnalyzerDiagnostic::new(
            AnalyzerDiagnosticError::UndefinedProperty(*expression.name.clone()),
            self.find_token_line(&expression.name.span.line),
          )));
        }

        Ok(IRInstruction::Get(IRGet::new(
          expression.name.span.literal.clone(),
          Box::new(ir_array),
          DataType::Int,
          GetMetadata::new(array.clone()),
        )))
      }

      _ => Err(Box::new(AnalyzerDiagnostic::new(
        AnalyzerDiagnosticError::UndefinedProperty(*expression.name.clone()),
        self.find_token_line(&expression.name.span.line),
      ))),
    }
  }

  fn type_methods(&mut self, expression: &MethodCall) -> AnalyzerResult {
    let object = self.analyzer(&expression.object)?;
    let data_type = self.extract_data_type(&object);

    match data_type {
      DataType::String => self.string_methods(expression),
      DataType::Int | DataType::Float => self.number_methods(expression, &object),
      DataType::Boolean => self.boolean_methods(expression, &object),
      DataType::Array(_) => self.array_methods(expression),
      DataType::Unwnown => todo!(),
      _ => todo!(),
    }
  }

  fn boolean_methods(&mut self, expression: &MethodCall, object: &IRInstruction) -> AnalyzerResult {
    let mut current_ir = self.irs.get_mut(&self.current_file).unwrap().clone();
    match expression.name.span.literal.as_str() {
      "toString" => {
        let current_index = current_ir.len() - 1;

        current_ir.push(IRInstruction::Function(IRFunction::new(
          *expression.name.clone(),
          vec![],
          DataType::String,
          None,
          IRFunctionMetadata::new(false, true, true, true, false, true, false, true),
        )));

        self.irs.insert(self.current_file.clone(), current_ir);

        self
          .block_stack
          .last_mut()
          .unwrap()
          .insert("toString".to_string(), true);

        let value = self.analyzer(&expression.calle)?;
        let mut current_ir = self.irs.get_mut(&self.current_file).unwrap().clone();

        current_ir.remove(current_index);
        self.block_stack.last_mut().unwrap().remove("toString");

        Ok(IRInstruction::MethodCall(IRMethodCall::new(
          expression.name.clone(),
          Box::new(value),
          DataType::String,
          Box::new(object.clone()),
          MethodCallMetadata::new(DataType::String, self.extract_data_type(object)),
        )))
      }
      _ => todo!(),
    }
  }

  fn number_methods(&mut self, expression: &MethodCall, object: &IRInstruction) -> AnalyzerResult {
    let mut current_ir = self.irs.get_mut(&self.current_file).unwrap().clone();

    match expression.name.span.literal.as_str() {
      "toString" => {
        let current_index = current_ir.len() - 1;

        current_ir.push(IRInstruction::Function(IRFunction::new(
          *expression.name.clone(),
          vec![],
          DataType::String,
          None,
          IRFunctionMetadata::new(false, true, true, true, false, true, false, true),
        )));

        self.irs.insert(self.current_file.clone(), current_ir);

        self
          .block_stack
          .last_mut()
          .unwrap()
          .insert("toString".to_string(), true);

        let value = self.analyzer(&expression.calle)?;
        let mut current_ir = self.irs.get_mut(&self.current_file).unwrap().clone();

        current_ir.remove(current_index);
        self.block_stack.last_mut().unwrap().remove("toString");

        Ok(IRInstruction::MethodCall(IRMethodCall::new(
          expression.name.clone(),
          Box::new(value),
          DataType::String,
          Box::new(object.clone()),
          MethodCallMetadata::new(DataType::String, self.extract_data_type(object)),
        )))
      }
      _ => todo!(),
    }
  }

  fn array_methods(&mut self, expression: &MethodCall) -> AnalyzerResult {
    todo!("array")
  }

  fn string_methods(&mut self, expression: &MethodCall) -> AnalyzerResult {
    todo!("string")
  }
}
