use std::{collections::HashMap, fs, path::Path, sync::Arc};

use super::{lexer::IgnisLexer, parser::IgnisParser};
use colored::*;
use ignis_ast::{
  expressions::{call::ASTCall, member_access::ASTMemberAccess, ASTExpression},
  metadata::ASTMetadataFlags,
  statements::{import::ASTImport, property, ASTStatement},
  visitor::ASTVisitor,
};
use ignis_config::IgnisConfig;
use ignis_data_type::{value::IgnisLiteralValue, DataType, GenericType};
use ignis_hir::{
  hir_assign::HIRAssign,
  hir_binary::HIRBinary,
  hir_block::HIRBlock,
  hir_call::HIRCall,
  hir_cast::HIRCast,
  hir_comment::HIRComment,
  hir_const::HIRConstant,
  hir_enum::{HIREnum, HIREnumItem},
  hir_extern::HIRExtern,
  hir_for::HIRFor,
  hir_for_of::HIRForOf,
  hir_function::HIRFunction,
  hir_function_instance::HIRFunctionInstance,
  hir_grouping::HIRGrouping,
  hir_if::HIRIf,
  hir_import::HIRImport,
  hir_literal::HIRLiteral,
  hir_logical::HIRLogical,
  hir_member_access::HIRMemberAccess,
  hir_meta::HIRMeta,
  hir_method::HIRMethod,
  hir_method_call::HIRMethodCall,
  hir_namespace::HIRNamespace,
  hir_object::HIRObjectLiteral,
  hir_record::HIRRecord,
  hir_return::HIRReturn,
  hir_spread::HIRSpread,
  hir_ternary::HIRTernary,
  hir_this::HIRThis,
  hir_type::HIRType,
  hir_unary::HIRUnary,
  hir_variable::HIRVariable,
  hir_vector::HIRVector,
  hir_vector_access::HIRVectorAccess,
  hir_while::HIRWhile,
  HIRInstruction, HIRInstructionType, HIRMetadata, HIRMetadataFlags,
};
use ignis_token::{token::Token, token_types::TokenType};

use crate::diagnostics::{diagnostic_report::DiagnosticReport, message::DiagnosticMessage, Diagnostic};

pub type AnalyzerResult = Result<HIRInstruction, Box<DiagnosticMessage>>;
type CheckCompatibility<T> = (bool, T);

#[derive(Debug, Clone, PartialEq)]
enum AnalyzerContext {
  Variable(Token),
  Function,
  Method,
  Class,
  Loop,
  Object,
  Record,
  ForCondition,
  Call,
  This,
  Extern,
  Lambda,
  Match,
  Namespace,
}

#[derive(Debug, Clone)]
enum CalleableDeclaration {
  Function(HIRFunction),
  Method(HIRMethod),
  Lambda,
}

#[derive(Debug, Clone, PartialEq)]
pub enum SymbolKind {
  Variable,
  Function,
  Class,
  Method,
  Parameter,
  Record,
  Enum,
  Interface,
  TypeAlias,
  Decorator,
  Constant,
  Extern,
  Namespace,
  Meta,
}

#[derive(Debug, Clone)]
pub struct SymbolInfo {
  pub name: Token,
  pub type_: DataType,
  pub metadata: Vec<HIRMetadataFlags>,
  pub kind: SymbolKind,
  pub hir_ref: Option<HIRInstruction>,
}

impl SymbolInfo {
  fn new(
    name: Token,
    type_: DataType,
    metadata: Vec<HIRMetadataFlags>,
    kind: SymbolKind,
    hir_ref: Option<HIRInstruction>,
  ) -> Self {
    Self {
      name,
      type_,
      metadata,
      kind,
      hir_ref,
    }
  }
}

pub struct IgnisAnalyzer {
  config: Arc<IgnisConfig>,
  programs: HashMap<String, Vec<ASTStatement>>,
  diagnostics: Vec<DiagnosticMessage>,
  hir: HashMap<String, Vec<HIRInstruction>>,
  pub primitives_std: Arc<HashMap<String, Vec<HIRInstruction>>>,
  pub primitives_symbol_stack: Arc<HashMap<String, Vec<HashMap<String, SymbolInfo>>>>,
  context: Vec<AnalyzerContext>,
  current_file: String,
  current_type: Option<DataType>,
  current_function: Option<CalleableDeclaration>,
  current_object: Option<HIRObjectLiteral>,
  symbol_stack: Vec<HashMap<String, SymbolInfo>>,
  current_block: Vec<HIRInstruction>,
}

impl ASTVisitor<AnalyzerResult> for IgnisAnalyzer {
  fn visit_binary_expression(
    &mut self,
    expression: &ignis_ast::expressions::binary::ASTBinary,
  ) -> AnalyzerResult {
    let left = self.analyzer(&expression.left)?;
    let left_type = left.extract_data_type();

    self.current_type = Some(left_type.clone());

    let right = self.analyzer(&expression.right)?;
    let right_type = right.extract_data_type();
    self.current_type = None;

    let operator = expression.operator.clone();

    let instruction_type = if operator.type_ == TokenType::Plus {
      if left_type == DataType::String && right_type == DataType::String {
        HIRInstructionType::Concatenate
      } else {
        HIRInstructionType::Add
      }
    } else {
      HIRInstructionType::from_token_kind(&operator.type_)
    };

    match (&left_type, &right_type) {
      (DataType::GenericType(left_generic), DataType::GenericType(right_generic)) => {
        if !left_generic.clone().constraints.contains(&right_type)
          && !right_generic.clone().constraints.contains(&left_type)
          && left_type != right_type
        {
          self.diagnostics.push(DiagnosticMessage::PotentialTypeMismatch(
            left_type.clone(),
            right_type.clone(),
            operator.clone(),
          ))
        }
      },
      (DataType::GenericType(left_generic), _) => {
        if !left_generic.clone().constraints.contains(&right_type) {
          self.diagnostics.push(DiagnosticMessage::PotentialTypeMismatch(
            left_type.clone(),
            right_type.clone(),
            operator.clone(),
          ))
        }
      },
      (_, DataType::GenericType(right_generic)) => {
        if !right_generic.clone().constraints.contains(&left_type) {
          self.diagnostics.push(DiagnosticMessage::PotentialTypeMismatch(
            right_type.clone(),
            left_type.clone(),
            operator.clone(),
          ))
        }
      },
      _ => (),
    };

    let (result, type_) = self.are_types_compatible(&left, &right, &instruction_type);

    if !result {
      return Err(Box::new(DiagnosticMessage::TypeMismatch(
        left_type,
        right_type,
        operator.clone(),
      )));
    }

    Ok(HIRInstruction::Binary(HIRBinary::new(
      Box::new(left),
      instruction_type,
      Box::new(right),
      type_,
    )))
  }

  fn visit_literal_expression(
    &mut self,
    expression: &ignis_ast::expressions::literal::ASTLiteral,
  ) -> AnalyzerResult {
    let mut value = expression.value.clone();

    if self.current_type.is_some() {
      let type_ = self.current_type.clone().unwrap();

      value = self.convert_generic_number_to_number(value, &type_, &expression.token)?;
    }

    let instruction = HIRInstruction::Literal(HIRLiteral::new(value, expression.token.clone()));

    Ok(instruction)
  }

  fn visit_logical_expression(
    &mut self,
    expression: &ignis_ast::expressions::logical::ASTLogical,
  ) -> AnalyzerResult {
    let left = self.analyzer(&expression.left)?;
    let right = self.analyzer(&expression.right)?;
    let instruction_type = HIRInstructionType::from_token_kind(&expression.operator.type_);
    let left_type = left.extract_data_type();
    let right_type = right.extract_data_type();

    match instruction_type {
      HIRInstructionType::And | HIRInstructionType::Or => {
        if !self.are_types_logical_compatibel(&left, &right, &instruction_type) {
          return Err(Box::new(DiagnosticMessage::TypeMismatch(
            left_type,
            right_type,
            expression.operator.clone(),
          )));
        }

        let instruction = HIRInstruction::Logical(HIRLogical::new(instruction_type, Box::new(left), Box::new(right)));

        Ok(instruction)
      },
      _ => Err(Box::new(DiagnosticMessage::InvalidOperator(expression.operator.clone()))),
    }
  }

  fn visit_ternary_expression(
    &mut self,
    expression: &ignis_ast::expressions::ternary::ASTTernary,
  ) -> AnalyzerResult {
    let condition = self.analyzer(&expression.condition)?;
    let then_branch = self.analyzer(&expression.then_branch)?;
    let else_branch = self.analyzer(&expression.else_branch)?;

    if condition.extract_data_type() != DataType::Boolean {
      return Err(Box::new(DiagnosticMessage::InvalidCondition(*expression.token.clone())));
    }

    let then_type = then_branch.extract_data_type();
    let else_type = else_branch.extract_data_type();

    if then_type != else_type {
      return Err(Box::new(DiagnosticMessage::TypeMismatch(
        then_type,
        else_type,
        *expression.token.clone(),
      )));
    }

    Ok(HIRInstruction::Ternary(HIRTernary::new(
      Box::new(condition),
      Box::new(then_branch),
      Box::new(else_branch),
      then_type,
    )))
  }

  fn visit_cast_expression(
    &mut self,
    cast: &ignis_ast::expressions::cast::ASTCast,
  ) -> AnalyzerResult {
    let value = self.analyzer(&cast.operand)?;

    let data_type = value.extract_data_type();

    if !self.is_valid_cast(&data_type, &cast.target_type) {
      return Err(Box::new(DiagnosticMessage::InvalidCast(
        cast.token.clone(),
        data_type,
        cast.target_type.clone(),
      )));
    }

    Ok(HIRInstruction::Cast(HIRCast::new(
      cast.target_type.clone(),
      Box::new(value),
      cast.token.clone(),
    )))
  }

  fn visit_unary_expression(
    &mut self,
    expression: &ignis_ast::expressions::unary::ASTUnary,
  ) -> AnalyzerResult {
    let right = self.analyzer(&expression.right)?;
    let instruction_type = HIRInstructionType::from_token_kind(&expression.operator.type_);
    let right_type = right.extract_data_type();

    if !self.are_types_unary_compatible(&right, &instruction_type) {
      return Err(Box::new(DiagnosticMessage::TypeMismatchUnary(
        right_type,
        expression.operator.clone(),
      )));
    }

    let instruction = HIRInstruction::Unary(HIRUnary::new(instruction_type, Box::new(right), right_type));

    Ok(instruction)
  }

  fn visit_variable_expression(
    &mut self,
    expression: &ignis_ast::expressions::variable::ASTVariableExpression,
  ) -> AnalyzerResult {
    if !self.is_declared(&expression.name.lexeme) {
      return Err(Box::new(DiagnosticMessage::UndefinedVariable(expression.name.clone())));
    }

    let var = self.resolve(&expression.name.lexeme);

    if var.is_none() {
      return Err(Box::new(DiagnosticMessage::UndeclaredVariable(expression.name.clone())));
    }

    let symbol = var.unwrap();

    let irs = &self.hir.get(&self.current_file).unwrap();

    match symbol.kind {
      SymbolKind::Variable => {
        if symbol.hir_ref.is_none() {
          return Err(Box::new(DiagnosticMessage::UndeclaredVariable(expression.name.clone())));
        }

        match symbol.hir_ref.as_ref().unwrap() {
          HIRInstruction::Variable(var) => {
            let mut var = var.clone();

            if var.metadata.is(HIRMetadataFlags::Moved) {
              return Err(Box::new(DiagnosticMessage::BorrowedValueHasMoved(expression.name.clone())));
            }

            var.metadata.remove_flag(HIRMetadataFlags::Declaration);

            if expression.metadata.is(ASTMetadataFlags::Reference) {
              var.metadata.remove_flag(HIRMetadataFlags::Reference);
              var.metadata.push(HIRMetadataFlags::ExplicitReference);
            }

            let instruction = HIRInstruction::Variable(var.clone());

            return Ok(instruction);
          },
          _ => {
            return Err(Box::new(DiagnosticMessage::UndeclaredVariable(expression.name.clone())));
          },
        }
      },
      SymbolKind::Function => {
        let is_function = irs.iter().find(|ir| match ir {
          HIRInstruction::Function(f) => f.name.lexeme == expression.name.lexeme,
          _ => false,
        });

        if let Some(HIRInstruction::Function(f)) = is_function {
          let mut function = f.clone();

          function.metadata.push(HIRMetadataFlags::Reference);

          let instruction = HIRInstruction::Function(function);

          return Ok(instruction);
        }

        if let Some(f) = &mut self.current_function {
          match f {
            CalleableDeclaration::Function(f) => {
              f.metadata.push(HIRMetadataFlags::Recursive);

              let instruction = HIRInstruction::Function(f.clone());

              return Ok(instruction);
            },
            CalleableDeclaration::Method(method) => {
              method.metadata.push(HIRMetadataFlags::Recursive);

              let instruction = HIRInstruction::Method(method.clone());

              return Ok(instruction);
            },
            CalleableDeclaration::Lambda => {
              let instruction = HIRInstruction::Variable(HIRVariable::new(
                expression.name.clone(),
                expression.data_type.clone(),
                None,
                HIRMetadata::new(vec![HIRMetadataFlags::Reference], None),
              ));

              return Ok(instruction);
            },
          }
        }

        unreachable!()
      },
      SymbolKind::Class => todo!(),
      SymbolKind::Method => todo!(),
      SymbolKind::Parameter => {
        let current_block = self.current_function.clone().unwrap();

        match current_block {
          CalleableDeclaration::Function(f) => {
            for var in f.parameters.iter() {
              if let HIRInstruction::Variable(v) = var {
                if v.name.lexeme == Into::<Token>::into(symbol.name.clone()).lexeme {
                  let mut var = v.clone();

                  if var.metadata.is(HIRMetadataFlags::Moved) {
                    return Err(Box::new(DiagnosticMessage::BorrowedValueHasMoved(expression.name.clone())));
                  }

                  var.metadata.remove_flag(HIRMetadataFlags::Declaration);

                  if expression.metadata.is(ASTMetadataFlags::Reference) {
                    var.metadata.remove_flag(HIRMetadataFlags::Reference);
                    var.metadata.push(HIRMetadataFlags::ExplicitReference);
                  }

                  let instruction = HIRInstruction::Variable(var.clone());

                  return Ok(instruction.clone());
                }
              }
            }

            return Err(Box::new(DiagnosticMessage::UndefinedVariable(expression.name.clone())));
          },
          CalleableDeclaration::Lambda => {
            todo!()
          },
          CalleableDeclaration::Method(method) => {
            for var in method.parameters.iter() {
              if let HIRInstruction::Variable(v) = var {
                if v.name.lexeme == Into::<Token>::into(symbol.name.clone()).lexeme {
                  let mut var = v.clone();

                  if var.metadata.is(HIRMetadataFlags::Moved) {
                    return Err(Box::new(DiagnosticMessage::BorrowedValueHasMoved(expression.name.clone())));
                  }

                  var.metadata.remove_flag(HIRMetadataFlags::Declaration);

                  if expression.metadata.is(ASTMetadataFlags::Reference) {
                    var.metadata.remove_flag(HIRMetadataFlags::Reference);
                    var.metadata.push(HIRMetadataFlags::ExplicitReference);
                  }

                  let instruction = HIRInstruction::Variable(var.clone());

                  return Ok(instruction.clone());
                }
              }
            }

            return Err(Box::new(DiagnosticMessage::UndefinedVariable(expression.name.clone())));
          },
        }
      },
      SymbolKind::Record => todo!(),
      SymbolKind::Enum => {
        let is_enum = irs.iter().find(|ir| match ir {
          HIRInstruction::Enum(e) => e.name.lexeme == expression.name.lexeme,
          _ => false,
        });

        if let Some(HIRInstruction::Enum(e)) = is_enum {
          let mut enum_ = e.clone();
          enum_.metadata.push(HIRMetadataFlags::Declaration);

          return Ok(HIRInstruction::Enum(enum_));
        }

        unreachable!()
      },
      SymbolKind::Interface => todo!(),
      SymbolKind::TypeAlias => todo!(),
      SymbolKind::Decorator => todo!(),
      SymbolKind::Constant => {
        let is_constant = irs.iter().find(|ir| match ir {
          HIRInstruction::Constant(c) => c.name.lexeme == expression.name.lexeme,
          _ => false,
        });

        if let Some(HIRInstruction::Constant(c)) = is_constant {
          let mut constant = c.clone();
          constant.metadata.push(HIRMetadataFlags::Declaration);

          return Ok(HIRInstruction::Constant(constant));
        }

        unreachable!()
      },
      SymbolKind::Namespace => {
        let is_namespace = irs.iter().find(|ir| match ir {
          HIRInstruction::Namespace(n) => n.name.lexeme == expression.name.lexeme,
          _ => false,
        });

        if let Some(HIRInstruction::Namespace(n)) = is_namespace {
          let mut namespace = n.clone();
          namespace.metadata.push(HIRMetadataFlags::Declaration);

          return Ok(HIRInstruction::Namespace(namespace));
        }

        unreachable!()
      },
      SymbolKind::Extern => {
        let is_extern = irs.iter().find(|ir| match ir {
          HIRInstruction::Extern(e) => e.name.lexeme == expression.name.lexeme,
          _ => false,
        });

        if let Some(HIRInstruction::Extern(e)) = is_extern {
          let mut extern_ = e.clone();
          extern_.metadata.push(HIRMetadataFlags::Declaration);

          return Ok(HIRInstruction::Extern(extern_));
        }

        unreachable!()
      },
      _ => Err(Box::new(DiagnosticMessage::UndeclaredVariable(expression.name.clone()))),
    }
  }

  fn visit_vector_access_expression(
    &mut self,
    vector: &ignis_ast::expressions::vector_access::ASTVectorAccess,
  ) -> AnalyzerResult {
    let var = self.analyzer(&vector.variable)?;
    let index = self.analyzer(&vector.index)?;

    let var_type = var.extract_data_type();

    if !matches!(var_type, DataType::Vector(_, _)) {
      return Err(Box::new(DiagnosticMessage::NotAnVector(*vector.name.clone())));
    }

    let var = match var {
      HIRInstruction::Variable(v) => v,
      _ => {
        return Err(Box::new(DiagnosticMessage::NotAnVector(*vector.name.clone())));
      },
    };

    match &index {
      HIRInstruction::Literal(l) => {
        if !matches!(l.value, IgnisLiteralValue::UnsignedInt32(_)) {
          return Err(Box::new(DiagnosticMessage::InvalidVectorIndex(*vector.name.clone())));
        }
      },
      _ => {
        let kind = index.extract_data_type();

        if kind != DataType::UnsignedInt32 {
          return Err(Box::new(DiagnosticMessage::InvalidVectorIndex(*vector.name.clone())));
        }
      },
    }

    let instruction: HIRInstruction =
      HIRInstruction::VectorAccess(HIRVectorAccess::new(var.name.clone(), Box::new(index), var_type));

    Ok(instruction)
  }

  fn visit_vector_expression(
    &mut self,
    expression: &ignis_ast::expressions::vector::ASTVector,
  ) -> AnalyzerResult {
    let mut elements = Vec::new();
    let mut element_types = Vec::new();

    for elem in &expression.elements {
      let analyzed_elem = self.analyzer(elem)?;
      let elem_type = analyzed_elem.extract_data_type();

      elements.push(analyzed_elem);
      element_types.push(elem_type);
    }

    let first_type = element_types.first().unwrap_or(&DataType::Unknown);

    if !element_types
      .iter()
      .all(|t| t == first_type || matches!(t, DataType::Object(_)))
    {
      return Err(Box::new(DiagnosticMessage::VectorElementTypeMismatch(expression.token.clone())));
    }

    let size = elements.len();

    let instruction = HIRInstruction::Vector(HIRVector::new(
      elements,
      DataType::Vector(Box::new(first_type.clone()), Some(size.clone())),
      Some(size),
    ));

    Ok(instruction)
  }

  fn visit_grouping_expression(
    &mut self,
    expression: &ignis_ast::expressions::grouping::ASTGrouping,
  ) -> AnalyzerResult {
    Ok(HIRInstruction::Grouping(HIRGrouping::new(Box::new(
      self.analyzer(&expression.expression)?,
    ))))
  }

  fn visit_member_access_expression(
    &mut self,
    expression: &ASTMemberAccess,
  ) -> AnalyzerResult {
    let instance: HIRInstruction = self.analyzer(expression.object.as_ref())?;
    let instance_type = instance.extract_data_type();

    if matches!(
      instance_type,
      DataType::Int8
        | DataType::Int16
        | DataType::Int32
        | DataType::Int64
        | DataType::UnsignedInt8
        | DataType::UnsignedInt16
        | DataType::UnsignedInt32
        | DataType::UnsignedInt64
        | DataType::Float32
        | DataType::Float64
        | DataType::Char
        | DataType::Boolean
        | DataType::String
        | DataType::Unknown
        | DataType::Vector(_, _)
    ) {
      return self.type_methods(&expression);
    }

    let mut object_instance: Option<HIRInstruction> = None;

    match &instance {
      HIRInstruction::This(_) => {
        let object = self
          .current_object
          .as_ref()
          .ok_or_else(|| DiagnosticMessage::InvalidThis(expression.member.as_ref().clone()))?;

        for (name, value) in object.properties.iter() {
          if name.lexeme == expression.member.lexeme {
            if let HIRInstruction::Variable(v) = value {
              object_instance = Some(HIRInstruction::Variable(v.clone()));
              break;
            }
            if let HIRInstruction::Literal(literal) = &value {
              object_instance = Some(HIRInstruction::Variable(HIRVariable::new(
                expression.member.as_ref().clone(),
                literal.value.clone().into(),
                Some(Box::new(value.clone())),
                HIRMetadata::new(vec![], None),
              )));
              break;
            } else {
              return Err(Box::new(DiagnosticMessage::InvalidPropertyType(
                expression.member.as_ref().clone(),
              )));
            }
          }
        }

        if object_instance.is_none() {
          return Err(Box::new(DiagnosticMessage::InvalidThis(expression.member.as_ref().clone())));
        }
      },
      HIRInstruction::Variable(v) => {
        if matches!(v.data_type, DataType::Pending) {
          let member_instance = self.find_hir_in_block(&v.name.lexeme);

          if let Some(member) = member_instance {
            object_instance = Some(HIRInstruction::Variable(HIRVariable::new(
              v.name.clone(),
              member.extract_data_type(),
              v.value.clone(),
              v.metadata.clone(),
            )));
          } else {
            return Err(Box::new(DiagnosticMessage::UndefinedProperty(
              expression.member.as_ref().clone(),
            )));
          }
        } else {
          if let Some(value) = &v.value {
            if let HIRInstruction::Object(object) = value.as_ref() {
              for property in &object.properties {
                if property.0.lexeme == expression.member.as_ref().lexeme {
                  object_instance = Some(HIRInstruction::Variable(HIRVariable::new(
                    property.0.clone(),
                    property.1.extract_data_type(),
                    None,
                    HIRMetadata::new(vec![], Some(Box::new(value.as_ref().clone()))),
                  )));
                }
              }

              for method in &object.methods {
                if method.name.lexeme == expression.member.as_ref().lexeme {
                  object_instance = Some(HIRInstruction::Method(method.clone()));
                }
              }
            } else {
              return Err(Box::new(DiagnosticMessage::UndefinedProperty(
                expression.member.as_ref().clone(),
              )));
            }
          } else {
            return Err(Box::new(DiagnosticMessage::UndefinedProperty(
              expression.member.as_ref().clone(),
            )));
          }
        }
      },
      HIRInstruction::Extern(extern_) => {
        for statement in &extern_.body {
          match &statement {
            HIRInstruction::Function(fun) => {
              if fun.name.lexeme == expression.member.as_ref().lexeme {
                object_instance = Some(HIRInstruction::Function(fun.clone()));
              }
            },
            _ => (),
          }
        }
      },
      HIRInstruction::Namespace(namespace) => {
        for statement in &namespace.members {
          match &statement {
            HIRInstruction::Function(fun) => {
              if fun.name.lexeme == expression.member.as_ref().lexeme {
                object_instance = Some(HIRInstruction::Function(fun.clone()));
              }
            },
            _ => (),
          }
        }
      },
      HIRInstruction::Enum(enum_) => {
        for member in &enum_.members {
          if member.name.lexeme == expression.member.as_ref().lexeme {
            object_instance = Some(HIRInstruction::Variable(HIRVariable::new(
              member.name.clone(),
              enum_.data_type.clone(),
              member.value.clone(),
              HIRMetadata::new(vec![HIRMetadataFlags::EnumMember], Some(Box::new(instance.clone()))),
            )));
          }
        }
      },
      _ => {
        return Err(Box::new(DiagnosticMessage::UndefinedProperty(
          expression.member.as_ref().clone(),
        )));
      },
    };

    if let Some(v) = object_instance {
      let ir = HIRInstruction::MemberAccess(HIRMemberAccess::new(
        Box::new(instance),
        Box::new(v),
        HIRMetadata::new(vec![], None),
      ));

      Ok(ir)
    } else {
      Err(Box::new(DiagnosticMessage::UndefinedProperty(
        expression.member.as_ref().clone(),
      )))
    }
  }

  fn visit_assignment_expression(
    &mut self,
    expression: &ignis_ast::expressions::assign::ASTAssignment,
  ) -> AnalyzerResult {
    let left = self.analyzer(&expression.left)?;
    let right = self.analyzer(&expression.right)?;
    let left_type = left.extract_data_type();
    let right_type = right.extract_data_type();

    if left_type != right_type {
      return Err(Box::new(DiagnosticMessage::TypeMismatch(
        left_type,
        right_type,
        expression.token.clone(),
      )));
    }

    Ok(HIRInstruction::Assign(HIRAssign::new(
      expression.token.clone(),
      Box::new(left),
      Box::new(right),
    )))
  }

  fn visit_call_expression(
    &mut self,
    expression: &ignis_ast::expressions::call::ASTCall,
  ) -> AnalyzerResult {
    self.context.push(AnalyzerContext::Call);
    let callee = self.analyzer(&expression.callee)?;

    let mut object: Option<HIRInstruction> = None;

    let (parameters, return_type, metadata, body, function_name, generic_parameters) = match &callee {
      HIRInstruction::Function(f) => (
        f.parameters.clone(),
        f.return_type.clone(),
        f.metadata.clone(),
        f.body.clone(),
        f.name.clone(),
        f.generic_parameters.clone(),
      ),
      HIRInstruction::Variable(var) => {
        if let DataType::Function(_, ret) = &var.data_type {
          let fun = if let HIRInstruction::Function(f) = var.metadata.complex_type.clone().unwrap().as_ref() {
            f.clone()
          } else {
            return Err(Box::new(DiagnosticMessage::NotCallable(expression.name.clone())));
          };

          (
            fun.parameters.clone(),
            (**ret).clone(),
            var.metadata.clone(),
            None,
            var.name.clone(),
            vec![],
          )
        } else {
          return Err(Box::new(DiagnosticMessage::NotCallable(expression.name.clone())));
        }
      },
      HIRInstruction::MemberAccess(member) => {
        if let HIRInstruction::Method(m) = member.member.as_ref() {
          object = Some(member.object.as_ref().clone());
          (
            m.parameters.clone(),
            m.return_type.clone(),
            m.metadata.clone(),
            m.body.clone(),
            m.name.clone(),
            vec![],
          )
        } else if let HIRInstruction::Function(f) = member.member.as_ref() {
          object = Some(member.object.as_ref().clone());
          (
            f.parameters.clone(),
            f.return_type.clone(),
            f.metadata.clone(),
            f.body.clone(),
            f.name.clone(),
            vec![],
          )
        } else {
          return Err(Box::new(DiagnosticMessage::NotCallable(expression.name.clone())));
        }
      },
      _ => {
        return Err(Box::new(DiagnosticMessage::NotCallable(expression.name.clone())));
      },
    };

    let mut parameters = parameters;
    let mut metadata = metadata;
    let mut body = body;
    let mut function_name = function_name;
    let mut generic_parameters = generic_parameters;
    let mut return_type = return_type;

    if !generic_parameters.is_empty() {
      if expression.arguments.len() < generic_parameters.len() {
        return Err(Box::new(DiagnosticMessage::IncorrectNumberOfGenericArguments(
          expression.name.clone(),
          generic_parameters.len(),
          expression.arguments.len(),
        )));
      }

      let instance_name = Token::new(
        TokenType::Identifier,
        function_name.lexeme.clone(),
        function_name.line.clone(),
        function_name.column.clone(),
        function_name.file_name.clone(),
      );

      let func: Option<HIRFunctionInstance> = self.find_function_instance(&instance_name.lexeme);

      if let Some(func) = &func {
        return_type = func.return_type.clone();
        parameters.clone_from(&func.parameters);
        body.clone_from(&func.body);
        function_name = instance_name.clone();
      } else {
        let func: HIRFunctionInstance = self.resolve_generic_params(
          expression,
          &mut parameters,
          &function_name,
          &instance_name,
          &return_type,
          &mut generic_parameters,
          &metadata,
          &body,
        )?;

        return_type = func.return_type.clone();
        parameters.clone_from(&func.parameters);
        body.clone_from(&func.body);
        function_name = instance_name.clone();
      }
    }

    let mut arguments = Vec::<HIRInstruction>::new();

    if !parameters.is_empty() {
      let last_param_instruction = parameters.last().unwrap();
      let last_params_is_variadic = last_param_instruction.get_metadata().is(HIRMetadataFlags::Variadic);

      let required_args_count = if last_params_is_variadic {
        parameters.len() - 1
      } else {
        parameters.len()
      };

      if expression.arguments.len() < required_args_count
        || (!last_params_is_variadic && expression.arguments.len() != required_args_count)
      {
        return Err(Box::new(DiagnosticMessage::InvalidNumberOfArguments(
          parameters.len(),
          expression.arguments.len(),
          expression.name.clone(),
        )));
      }

      for (i, arg) in expression.arguments.iter().enumerate() {
        let arg_hir = self.analyzer(arg)?;

        let param_instruction: &HIRInstruction = if i > parameters.len() {
          &parameters.last().unwrap()
        } else {
          &parameters[i]
        };

        let param_type = param_instruction.extract_data_type();

        let param_var = if let HIRInstruction::Variable(v) = param_instruction {
          v
        } else {
          unreachable!()
        };

        let arg_type = arg_hir.extract_data_type();

        if let HIRInstruction::Variable(v) = &arg_hir {
          if !(v.metadata.is(HIRMetadataFlags::Reference) || v.metadata.is(HIRMetadataFlags::ExplicitReference))
            && param_var.metadata.is(HIRMetadataFlags::Reference)
          {
            return Err(Box::new(DiagnosticMessage::ExpectedReference(
              v.data_type.clone(),
              param_type.clone(),
              v.name.clone(),
            )));
          }

          if (v.metadata.is(HIRMetadataFlags::Reference) || v.metadata.is(HIRMetadataFlags::ExplicitReference))
            && !param_var.metadata.is(HIRMetadataFlags::Reference)
          {
            return Err(Box::new(DiagnosticMessage::UnexpectedReference(
              v.data_type.clone(),
              v.name.clone(),
            )));
          }

          if !v.metadata.is(HIRMetadataFlags::Mutable) && param_var.metadata.is(HIRMetadataFlags::Mutable) {
            return Err(Box::new(DiagnosticMessage::ImmutableVariableAsMutableParameter(
              param_var.name.clone(),
              v.name.clone(),
              expression.name.clone(),
            )));
          }
        }

        if param_var.metadata.is(HIRMetadataFlags::Variadic) {
          let variac_data_type = param_var.data_type.clone();
          if let DataType::Vector(dt, _) = variac_data_type {
            if &arg_type != dt.as_ref() && arg_type != DataType::Unknown && dt.as_ref() != &DataType::Unknown {
              return Err(Box::new(DiagnosticMessage::ArgumentTypeMismatch(
                dt.as_ref().clone(),
                arg_type,
                arg.into(),
              )));
            }
          }
        } else if arg_type != param_type && arg_type != DataType::Unknown && param_type != DataType::Unknown {
          return Err(Box::new(DiagnosticMessage::ArgumentTypeMismatch(
            param_type,
            arg_type,
            arg.into(),
          )));
        }

        arguments.push(arg_hir);
      }
    }

    if let Some(object) = object {
      if let HIRInstruction::MemberAccess(member) = &callee {
        if matches!(member.object.as_ref(), HIRInstruction::Object(_)) {
          metadata.push(HIRMetadataFlags::ObjectMember);
        }

        return Ok(HIRInstruction::MethodCall(HIRMethodCall::new(
          Box::new(function_name),
          member.object.clone(),
          arguments,
          return_type,
          object.into(),
          metadata,
        )));
      }
    }

    Ok(HIRInstruction::Call(HIRCall::new(
      function_name,
      arguments,
      return_type,
      generic_parameters,
      metadata,
    )))
  }

  fn visit_match_expression(
    &mut self,
    expression: &ignis_ast::expressions::match_expression::ASTMatchExpression,
  ) -> AnalyzerResult {
    todo!()
  }

  fn visit_lambda_expression(
    &mut self,
    expression: &ignis_ast::expressions::lambda::ASTLambda,
  ) -> AnalyzerResult {
    todo!()
  }

  fn visit_object_expression(
    &mut self,
    object: &ignis_ast::expressions::object_literal::ASTObject,
  ) -> AnalyzerResult {
    self.context.push(AnalyzerContext::Object);
    let mut properties = Vec::<(Token, HIRInstruction)>::new();

    let mut properties_data_types = Vec::<(String, DataType)>::new();

    for (name, expression) in &object.properties {
      let value = self.analyzer(expression)?;
      let data_type = value.extract_data_type();

      properties.push((name.clone(), value));
      properties_data_types.push((name.lexeme.clone(), data_type));
    }

    let object_type = DataType::Object(properties_data_types.clone());

    let mut ir_object = HIRObjectLiteral::new(properties, Vec::new(), object_type);

    self.current_object.clone_from(&Some(ir_object.clone()));

    for method in &object.methods {
      let result = self.analyze_statement(&ASTStatement::Method(Box::new(method.clone())))?;

      if let HIRInstruction::Method(m) = result {
        ir_object.methods.push(m.clone());
        properties_data_types.push((method.name.lexeme.clone(), m.return_type.clone()));
        ir_object.data_type = DataType::Object(properties_data_types.clone());
        self.current_object.clone_from(&Some(ir_object.clone()));
      } else {
        unreachable!()
      }
    }

    self.current_object.clone_from(&None);

    self.context.pop();

    Ok(HIRInstruction::Object(ir_object))
  }

  fn visit_this_expression(
    &mut self,
    expression: &ignis_ast::expressions::this::ASTThis,
  ) -> AnalyzerResult {
    if !self
      .context
      .iter()
      .any(|c| c == &AnalyzerContext::Object || c == &AnalyzerContext::Class || c == &AnalyzerContext::Method)
    {
      return Err(Box::new(DiagnosticMessage::InvalidThis(expression.token.clone())));
    }

    let last_context = self
      .context
      .iter()
      .rfind(|&c| matches!(c, AnalyzerContext::Object | AnalyzerContext::Class | AnalyzerContext::Method));

    if last_context.is_none() {
      return Err(Box::new(DiagnosticMessage::InvalidThis(expression.token.clone())));
    }

    let last_context = last_context.unwrap();

    let data_type = match last_context {
      AnalyzerContext::Object => {
        if let Some(object) = &self.current_object {
          object.data_type.clone()
        } else {
          return Err(Box::new(DiagnosticMessage::InvalidThis(expression.token.clone())));
        }
      },
      AnalyzerContext::Class => {
        todo!()
      },
      AnalyzerContext::Method => {
        if let Some(current) = &self.current_function {
          if let CalleableDeclaration::Method(method) = current {
            if method.metadata.is(HIRMetadataFlags::ObjectMember) {
              if let Some(object) = &self.current_object {
                object.data_type.clone()
              } else {
                return Err(Box::new(DiagnosticMessage::InvalidThis(expression.token.clone())));
              }
            } else {
              todo!()
            }
          } else {
            return Err(Box::new(DiagnosticMessage::InvalidThis(expression.token.clone())));
          }
        } else {
          return Err(Box::new(DiagnosticMessage::InvalidThis(expression.token.clone())));
        }
      },
      _ => DataType::Null,
    };

    Ok(HIRInstruction::This(HIRThis::new(expression.token.clone(), data_type)))
  }

  fn visit_meta_expression(
    &mut self,
    expression: &ignis_ast::expressions::meta::ASTMeta,
  ) -> AnalyzerResult {
    todo!()
  }

  fn visit_meta_entity_expression(
    &mut self,
    expression: &ignis_ast::expressions::meta::ASTMetaEntity,
  ) -> AnalyzerResult {
    todo!()
  }

  fn visit_spread_expression(
    &mut self,
    expression: &ignis_ast::expressions::spread::ASTSpread,
  ) -> AnalyzerResult {
    let var = self.analyzer(expression.expression.as_ref())?;
    let var_type = var.extract_data_type();

    match var_type {
      DataType::Vector(_, _) | DataType::Object(_) | DataType::Record(_, _) | DataType::String => (),
      _ => return Err(Box::new(DiagnosticMessage::InvalidSpreadExpression(expression.token.clone()))),
    };

    Ok(HIRInstruction::Spread(HIRSpread::new(
      Box::new(var),
      expression.token.clone(),
      var_type,
    )))
  }

  fn visit_comment_statement(
    &mut self,
    comment: &ignis_ast::statements::comment::ASTComment,
  ) -> AnalyzerResult {
    Ok(HIRInstruction::Comment(HIRComment::new(
      comment.comment.clone(),
      (&comment.type_).into(),
      comment.token.clone(),
    )))
  }

  fn visit_expression_statement(
    &mut self,
    expression: &ignis_ast::expressions::ASTExpression,
  ) -> AnalyzerResult {
    self.analyzer(expression)
  }

  fn visit_constant_statement(
    &mut self,
    statement: &ignis_ast::statements::constant::ASTConstant,
  ) -> AnalyzerResult {
    if self.is_declared(&statement.name.lexeme) {
      return Err(Box::new(DiagnosticMessage::ConstantAlreadyDefined(statement.name.clone())));
    }

    let type_annotation = statement.type_annotation.clone();

    let mut metadata = vec![];

    for value in statement.metadata.get().iter() {
      metadata.push(value.into());
    }

    self.declare(
      statement.name.lexeme.clone(),
      SymbolInfo::new(
        statement.name.clone(),
        type_annotation.clone(),
        metadata.clone(),
        SymbolKind::Constant,
        None,
      ),
    );

    self.current_type = Some(type_annotation.clone());

    let value = self.analyzer(&statement.value)?;

    let left = type_annotation.clone();
    let right = value.extract_data_type();

    self.check_type_mismatch(&left, &right, &statement.name)?;

    let constant = HIRInstruction::Constant(HIRConstant::new(
      statement.name.clone(),
      type_annotation,
      Box::new(value),
      HIRMetadata::new(metadata, None),
    ));

    Ok(constant)
  }

  fn visit_function_statement(
    &mut self,
    expression: &ignis_ast::statements::function::ASTFunction,
  ) -> AnalyzerResult {
    if self.is_declared(&expression.name.lexeme) {
      return Err(Box::new(DiagnosticMessage::FunctionAlreadyDefined(
        expression.name.lexeme.clone(),
        expression.name.clone(),
      )));
    }

    self.context.push(AnalyzerContext::Function);

    self.declare(
      expression.name.lexeme.clone(),
      SymbolInfo::new(
        expression.name.clone(),
        expression.return_type.clone(),
        vec![HIRMetadataFlags::Function],
        SymbolKind::Function,
        None,
      ),
    );

    self.begin_scope();

    let mut parameters: Vec<HIRInstruction> = vec![];
    let mut has_variadic = false;
    let mut has_optional = false;

    for parameter in &expression.parameters {
      let hir = self.analyze_statement(&ASTStatement::Variable(Box::new(parameter.clone())))?;

      if has_variadic {
        return Err(Box::new(DiagnosticMessage::InvalidParameterAfterVariadic(
          parameter.name.clone(),
        )));
      }

      let metadata = hir.get_metadata().clone();

      if has_optional && !metadata.is(HIRMetadataFlags::Optional) {
        return Err(Box::new(DiagnosticMessage::InvalidParameterAfterOptional(
          parameter.name.clone(),
        )));
      }

      if metadata.is(HIRMetadataFlags::Variadic) {
        has_variadic = true;
      }

      if metadata.is(HIRMetadataFlags::Optional) {
        has_optional = true;
      }

      parameters.push(hir);
    }

    let mut metadata: Vec<HIRMetadataFlags> = Vec::new();

    for meta in &expression.metadata.get() {
      metadata.push(meta.into());
    }

    let mut current_function = HIRFunction::new(
      expression.name.clone(),
      parameters,
      expression.return_type.clone(),
      None,
      HIRMetadata::new(metadata, None),
      expression
        .generic_parameters
        .clone()
        .into_iter()
        .map(|g| {
          DataType::GenericType(GenericType::new(
            Box::new(DataType::Variable(g.name.lexeme.clone(), Box::new(DataType::Unknown))),
            g.constraints.clone(),
          ))
        })
        .collect(),
    );

    self.current_function = Some(CalleableDeclaration::Function(current_function));

    let mut block: Option<Box<HIRBlock>> = None;

    if expression.body.len() > 0 {
      block = Some(Box::new(HIRBlock::new(vec![], vec![])));
    }

    for statement in &expression.body {
      let block = block.as_mut().unwrap();
      let hir = self.analyze_statement(statement)?;

      block.instructions.push(hir);
    }

    self.end_scope();

    current_function = match self.current_function.as_ref().unwrap() {
      CalleableDeclaration::Function(f) => f.clone(),
      _ => todo!(),
    };

    current_function.body = block.clone();

    let instruction = HIRInstruction::Function(current_function);

    self.context.pop();
    self.current_function = None;

    Ok(instruction)
  }

  fn visit_variable_statement(
    &mut self,
    expression: &ignis_ast::statements::variable::ASTVariable,
  ) -> AnalyzerResult {
    if self.is_declared(&expression.name.lexeme) {
      return Err(Box::new(DiagnosticMessage::VariableAlreadyDefined(
        expression.name.lexeme.clone(),
        expression.name.clone(),
      )));
    }

    let mut metadata: Vec<HIRMetadataFlags> = Vec::new();

    for meta in &expression.metadata.get() {
      metadata.push(meta.into());
    }

    let mut data_type = expression.type_annotation.clone();

    if let DataType::Enum(enum_name, _) = data_type {
      let enum_type = self.find_enum_in_scope(&enum_name);

      match enum_type {
        Some(enum_type) => {
          data_type = enum_type.data_type.clone();
        },
        None => {
          let enum_type = self.find_enum_in_ir(&enum_name);

          match enum_type {
            Some(enum_type) => {
              data_type = enum_type.data_type.clone();
            },
            None => {
              unreachable!("Why is this enum not in scope?")
            },
          }
        },
      }
    }

    self.current_type = Some(data_type.clone());

    let values = self.assign_value_and_complex_data(&expression.name, &expression.initializer, &mut data_type)?;

    let value = values.0.clone();
    let complex_type = values.1.clone();

    self.current_type = None;

    let variable = HIRInstruction::Variable(HIRVariable::new(
      expression.name.clone(),
      data_type.clone(),
      Some(Box::new(value.clone())),
      HIRMetadata::new(metadata.clone(), complex_type),
    ));

    self.declare(
      expression.name.lexeme.clone(),
      SymbolInfo::new(
        expression.name.clone(),
        data_type.clone(),
        metadata.clone(),
        if metadata.contains(&HIRMetadataFlags::Parameter) {
          SymbolKind::Parameter
        } else {
          SymbolKind::Variable
        },
        Some(variable.clone()),
      ),
    );

    Ok(variable)
  }

  fn visit_block_statement(
    &mut self,
    expression: &ignis_ast::statements::block::ASTBlock,
  ) -> AnalyzerResult {
    let last_block = self.current_block.clone();
    self.current_block = vec![];
    self.begin_scope();
    let mut block: Vec<HIRInstruction> = vec![];
    let mut variables: Vec<HIRInstruction> = vec![];

    for statement in &expression.statements {
      let hir = self.analyze_statement(statement)?;

      if let HIRInstruction::Variable(_) = hir {
        variables.push(hir.clone());
      }

      self.current_block.push(hir.clone());
      block.push(hir);
    }

    self.end_scope();

    self.current_block.clone_from(&last_block);

    Ok(HIRInstruction::Block(HIRBlock::new(block, variables)))
  }

  fn visit_if_statement(
    &mut self,
    statement: &ignis_ast::statements::if_statement::ASTIf,
  ) -> AnalyzerResult {
    let condition = self.analyzer(&statement.condition)?;
    let then_branch = self.analyze_statement(&statement.then_branch)?;

    let else_branch: Option<Box<HIRInstruction>> = if statement.else_branch.is_some() {
      Some(Box::new(self.analyze_statement(statement.else_branch.as_ref().unwrap())?))
    } else {
      None
    };

    let instruction = HIRInstruction::If(HIRIf::new(Box::new(condition), Box::new(then_branch), else_branch));

    Ok(instruction)
  }

  fn visit_while_statement(
    &mut self,
    statement: &ignis_ast::statements::while_statement::ASTWhile,
  ) -> AnalyzerResult {
    self.context.push(AnalyzerContext::Loop);

    let condition = self.analyzer(&statement.condition)?;
    let body = self.analyze_statement(&statement.body)?;

    self.context.pop();

    Ok(HIRInstruction::While(HIRWhile::new(Box::new(condition), Box::new(body))))
  }

  fn visit_for_statement(
    &mut self,
    statement: &ignis_ast::statements::for_statement::ASTFor,
  ) -> AnalyzerResult {
    let variable = self.analyze_statement(&ASTStatement::Variable(statement.variable.clone()))?;

    self.context.push(AnalyzerContext::ForCondition);
    let last_current_type = self.current_type.clone();
    self.current_type = Some(DataType::UnsignedInt32);

    let condition = self.analyzer(&statement.condition)?;

    self.current_type = last_current_type.clone();
    self.context.pop();

    let increment = self.analyzer(&statement.increment)?;

    let body = self.analyze_statement(&statement.body)?;

    Ok(HIRInstruction::For(HIRFor::new(
      Box::new(variable),
      Box::new(condition),
      Box::new(increment),
      Box::new(body),
    )))
  }

  fn visit_for_of_statement(
    &mut self,
    statement: &ignis_ast::statements::for_of_statement::ASTForOf,
  ) -> AnalyzerResult {
    let iterable = self.analyzer(&statement.iterable)?;
    let data_type = iterable.extract_data_type();

    if !matches!(data_type, DataType::Vector(_, _)) {
      return Err(Box::new(DiagnosticMessage::NotIterable(statement.token.clone())));
    }

    self.begin_scope();

    let mut flags: Vec<HIRMetadataFlags> = vec![];

    for metadata in &statement.variable.metadata.get() {
      flags.push(metadata.into());
    }

    let variable = HIRVariable::new(statement.variable.name.clone(), data_type, None, HIRMetadata::new(flags, None));

    self.declare(
      statement.variable.name.lexeme.clone(),
      SymbolInfo::new(
        statement.variable.name.clone(),
        DataType::Pending,
        vec![],
        SymbolKind::Variable,
        Some(HIRInstruction::Variable(variable.clone())),
      ),
    );

    let body = self.analyze_statement(&statement.body)?;

    self.end_scope();

    let instruction = HIRInstruction::ForOf(HIRForOf::new(
      variable,
      Box::new(iterable),
      Box::new(body),
      statement.token.clone(),
    ));

    Ok(instruction)
  }

  fn visit_break_statement(
    &mut self,
    token: &ignis_token::token::Token,
  ) -> AnalyzerResult {
    if !self.context.iter().any(|c| matches!(c, AnalyzerContext::Loop)) {
      return Err(Box::new(DiagnosticMessage::BreakOutsideLoop(token.clone())));
    }

    Ok(HIRInstruction::Break(token.clone()))
  }

  fn visit_continue_statement(
    &mut self,
    token: &ignis_token::token::Token,
  ) -> AnalyzerResult {
    if !self.context.iter().any(|c| matches!(c, AnalyzerContext::Loop)) {
      return Err(Box::new(DiagnosticMessage::ContinueOutsideLoop(token.clone())));
    }

    Ok(HIRInstruction::Continue(token.clone()))
  }

  fn visit_return_statement(
    &mut self,
    return_: &ignis_ast::statements::return_::ASTReturn,
  ) -> AnalyzerResult {
    if !self.context.iter().any(|context| {
      matches!(
        &context,
        AnalyzerContext::Function | AnalyzerContext::Method | AnalyzerContext::Lambda
      )
    }) || self.current_function.is_none()
    {
      return Err(Box::new(DiagnosticMessage::ReturnOutsideFunction(return_.token.clone())));
    }

    let mut return_value = HIRInstruction::Literal(HIRLiteral::new(IgnisLiteralValue::Null, return_.token.clone()));
    let mut data_type = DataType::Null;
    let mut return_type = DataType::Null;

    match self.current_function.clone().unwrap() {
      CalleableDeclaration::Function(function) => {
        if function.return_type == DataType::Void && return_.value != None {
          return Err(Box::new(DiagnosticMessage::ReturnTypeMismatch(
            function.return_type.clone(),
            DataType::Void,
            return_.token.clone(),
          )));
        }

        let value = &return_.value;

        if value.is_none() {
          let instruction = HIRInstruction::Return(HIRReturn::new(Box::new(return_value), DataType::Void));

          return Ok(instruction);
        }

        let last_type = self.current_type.clone();
        self.current_type = Some(function.return_type.clone());

        return_value = self.analyzer(value.as_ref().unwrap())?;
        self.current_type.clone_from(&last_type);

        data_type = return_value.extract_data_type();
        return_type = function.return_type.clone();
      },
      CalleableDeclaration::Method(function) => {
        if function.return_type == DataType::Void && return_.value != None {
          return Err(Box::new(DiagnosticMessage::ReturnTypeMismatch(
            function.return_type.clone(),
            DataType::Void,
            return_.token.clone(),
          )));
        }

        let value = &return_.value;

        if value.is_none() {
          let instruction = HIRInstruction::Return(HIRReturn::new(Box::new(return_value), DataType::Void));

          return Ok(instruction);
        }

        let last_type = self.current_type.clone();
        self.current_type = Some(function.return_type.clone());

        return_value = self.analyzer(value.as_ref().unwrap())?;

        self.current_type.clone_from(&last_type);
        data_type = return_value.extract_data_type();
        return_type = function.return_type.clone();
      },
      CalleableDeclaration::Lambda => todo!(),
    };

    match &return_type {
      DataType::UnionType(types) | DataType::IntersectionType(types) => {
        if !types.contains(&data_type) {
          return Err(Box::new(DiagnosticMessage::ReturnTypeMismatch(
            return_type.clone(),
            data_type,
            return_.token.clone(),
          )));
        }
      },
      _ => {
        if return_type != data_type {
          return Err(Box::new(DiagnosticMessage::ReturnTypeMismatch(
            return_type.clone(),
            data_type,
            return_.token.clone(),
          )));
        }
      },
    };

    let instruction = HIRInstruction::Return(HIRReturn::new(Box::new(return_value), data_type));

    Ok(instruction)
  }

  fn visit_import_statement(
    &mut self,
    import: &ignis_ast::statements::import::ASTImport,
  ) -> AnalyzerResult {
    let mut block_stack: HashMap<String, SymbolInfo> = self.symbol_stack.last_mut().unwrap().clone();
    let mut path = import.module_path.lexeme.clone();

    if !import.is_std {
      self.resolve_module_import(&import, &mut block_stack)?;
    } else {
      path = format!("std/{}/mod.ign", path.split("::").last().unwrap());
      self.resolve_std_import(&import, &mut block_stack)?;
    }

    self.symbol_stack.pop();
    self.symbol_stack.push(block_stack);

    Ok(HIRInstruction::Import(HIRImport::new(
      import
        .symbols
        .clone()
        .into_iter()
        .map(|i| (i.name, i.alias))
        .collect::<Vec<(Token, Option<Token>)>>(),
      path,
    )))
  }

  fn visit_record_statement(
    &mut self,
    record: &ignis_ast::statements::record::ASTRecord,
  ) -> AnalyzerResult {
    if self.is_declared(&record.name.lexeme) {
      return Err(Box::new(DiagnosticMessage::RecordAlreadyDefined(record.name.clone())));
    }

    self.context.push(AnalyzerContext::Record);

    let mut flags: Vec<HIRMetadataFlags> = vec![];

    for flag in &record.metadata.get() {
      flags.push(flag.into());
    }

    let metadata = HIRMetadata::new(flags.clone(), None);

    self.declare(
      record.name.lexeme.clone(),
      SymbolInfo::new(
        record.name.clone(),
        DataType::Record(record.name.lexeme.clone(), vec![]),
        flags.clone(),
        SymbolKind::Record,
        None,
      ),
    );

    self.begin_scope();

    let generic_parameters: Vec<DataType> = record
      .generic_parameters
      .clone()
      .into_iter()
      .map(|g| {
        DataType::GenericType(GenericType::new(
          Box::new(DataType::Variable(g.name.lexeme.clone(), Box::new(DataType::Unknown))),
          g.constraints.clone(),
        ))
      })
      .collect();

    let mut hir_record = HIRRecord::new(
      record.name.clone(),
      vec![],
      generic_parameters,
      metadata,
      DataType::Record(record.name.lexeme.clone(), vec![]),
    );

    for property in &record.items {
      let result = self.analyze_statement(property)?;

      let symbol = self.resolve(&record.name.lexeme).unwrap();
      let data_type: DataType = if let DataType::Record(name, items) = &symbol.type_ {
        let mut items = items.clone();

        items.push(if let HIRInstruction::Variable(v) = &result {
          (v.name.lexeme.clone(), v.data_type.clone())
        } else {
          (String::new(), DataType::Unknown)
        });

        DataType::Record(name.to_string(), items)
      } else {
        unreachable!()
      };

      hir_record.data_type.clone_from(&data_type);

      self.edit_symbol(
        record.name.lexeme.clone(),
        SymbolInfo::new(
          record.name.clone(),
          data_type,
          flags.clone(),
          SymbolKind::Record,
          Some(HIRInstruction::Record(hir_record.clone())),
        ),
      );

      hir_record.items.push(result);
    }

    self.end_scope();

    self.context.pop();

    let record = HIRInstruction::Record(hir_record);

    Ok(record)
  }

  fn visit_method_statement(
    &mut self,
    method: &ignis_ast::statements::method::ASTMethod,
  ) -> AnalyzerResult {
    if self.is_declared(&method.name.lexeme) && !method.metadata.is(ASTMetadataFlags::Constructor) {
      return Err(Box::new(DiagnosticMessage::MethodAlreadyDefined(method.name.clone())));
    }

    let previous_function = self.current_function.clone();

    let last_context = self.context.last();

    if last_context.is_none()
      || !matches!(
        last_context,
        Some(AnalyzerContext::Class | AnalyzerContext::Object | AnalyzerContext::Record)
      )
    {
      return Err(Box::new(DiagnosticMessage::MethodOutsideClass(method.name.clone())));
    }

    self.begin_scope();
    self.context.push(AnalyzerContext::Method);
    let mut parameters = Vec::<HIRInstruction>::new();

    let mut metadata: Vec<HIRMetadataFlags> = vec![];

    for value in &method.metadata.get() {
      metadata.push(value.into());
    }

    self.declare(
      method.name.lexeme.clone(),
      SymbolInfo::new(
        method.name.clone(),
        method.return_type.clone(),
        metadata.clone(),
        SymbolKind::Method,
        None,
      ),
    );

    for param in &method.parameters {
      let parameter = self.analyze_statement(&ASTStatement::Variable(Box::new(param.clone())))?;

      parameters.push(parameter);
    }

    let mut ir: HIRBlock = HIRBlock::new(Vec::new(), Vec::new());

    let mut current_function = HIRMethod::new(
      method.name.clone(),
      parameters.clone(),
      method.return_type.clone(),
      None,
      HIRMetadata::new(metadata, None),
      method.class_name.clone(),
    );

    self.current_function = Some(CalleableDeclaration::Method(current_function.clone()));

    for body in &method.body.statements {
      let result = self.analyze_statement(body)?;
      ir.instructions.push(result);
    }

    self.end_scope();

    current_function = match self.current_function.as_ref().unwrap() {
      CalleableDeclaration::Method(m) => m.clone(),
      _ => unreachable!(),
    };

    current_function.body = Some(Box::new(ir.clone()));

    let instruction = HIRInstruction::Method(current_function);

    self.context.pop();
    self.current_function.clone_from(&previous_function);

    Ok(instruction)
  }

  fn visit_property_statement(
    &mut self,
    property: &ignis_ast::statements::property::ASTProperty,
  ) -> AnalyzerResult {
    todo!()
  }

  fn visit_extern_statement(
    &mut self,
    extern_: &ignis_ast::statements::r#extern::ASTExtern,
  ) -> AnalyzerResult {
    self.context.push(AnalyzerContext::Extern);
    if self.is_declared(&extern_.name.lexeme) {
      return Err(Box::new(DiagnosticMessage::ExternAlreadyDefined(extern_.name.clone())));
    }

    self.declare(
      extern_.name.lexeme.clone(),
      SymbolInfo::new(extern_.name.clone(), DataType::Null, vec![], SymbolKind::Extern, None),
    );

    self.begin_scope();

    let mut metadata: Vec<HIRMetadataFlags> = vec![];

    for value in extern_.metadata.get().iter() {
      metadata.push(value.into());
    }

    let mut body: Vec<HIRInstruction> = vec![];

    for statement in &extern_.body {
      let hir = self.analyze_statement(statement)?;

      body.push(hir);
    }

    self.end_scope();

    self.context.pop();

    let extern_ = HIRExtern::new(extern_.name.clone(), body, HIRMetadata::new(metadata, None));

    let hir = HIRInstruction::Extern(extern_.clone());

    self.edit_symbol(
      extern_.name.lexeme.clone(),
      SymbolInfo::new(
        extern_.name.clone(),
        DataType::Null,
        vec![],
        SymbolKind::Extern,
        Some(hir.clone()),
      ),
    );

    Ok(hir)
  }

  fn visit_namespace_statement(
    &mut self,
    namespace: &ignis_ast::statements::namespace::ASTNamespace,
  ) -> AnalyzerResult {
    if self.is_declared(&namespace.name.lexeme) {
      return Err(Box::new(DiagnosticMessage::NamespaceAlreadyDefined(namespace.name.clone())));
    }

    self.context.push(AnalyzerContext::Namespace);

    self.declare(
      namespace.name.lexeme.clone(),
      SymbolInfo::new(namespace.name.clone(), DataType::Pending, vec![], SymbolKind::Namespace, None),
    );

    self.begin_scope();
    let mut body: Vec<HIRInstruction> = vec![];

    for statement in &namespace.members {
      let hir = self.analyze_statement(statement)?;
      self.current_block.push(hir.clone());

      body.push(hir);
    }

    self.end_scope();

    self.context.pop();

    let metadata = HIRMetadata::new(namespace.metadata.get().iter().map(|m| m.into()).collect(), None);
    let hir = HIRInstruction::Namespace(HIRNamespace::new(namespace.name.clone(), body, metadata));

    self.edit_symbol(
      namespace.name.lexeme.clone(),
      SymbolInfo::new(
        namespace.name.clone(),
        DataType::Pending,
        vec![],
        SymbolKind::Namespace,
        Some(hir.clone()),
      ),
    );

    Ok(hir)
  }

  fn visit_type_alias_statement(
    &mut self,
    type_alias: &ignis_ast::statements::type_alias::ASTTypeAlias,
  ) -> AnalyzerResult {
    if self.is_declared(&type_alias.name.lexeme) {
      return Err(Box::new(DiagnosticMessage::TypeAlreadyDefined(type_alias.name.clone())));
    }

    let mut metadata: Vec<HIRMetadataFlags> = vec![];

    for value in type_alias.metadata.get().iter() {
      metadata.push(value.into());
    }

    self.declare(
      type_alias.name.lexeme.clone(),
      SymbolInfo::new(
        type_alias.name.clone(),
        type_alias.value.as_ref().clone(),
        metadata.clone(),
        SymbolKind::TypeAlias,
        None,
      ),
    );

    Ok(HIRInstruction::Type(HIRType::new(
      type_alias.name.clone(),
      type_alias.value.clone(),
      HIRMetadata::new(metadata.clone(), None),
      type_alias
        .generics
        .clone()
        .into_iter()
        .map(|g| {
          DataType::GenericType(GenericType::new(
            Box::new(DataType::Variable(g.name.lexeme.clone(), Box::new(DataType::Unknown))),
            g.constraints.clone(),
          ))
        })
        .collect(),
    )))
  }

  fn visitor_enum_statement(
    &mut self,
    enum_: &ignis_ast::statements::enum_statement::ASTEnum,
  ) -> AnalyzerResult {
    if self.is_declared(&enum_.name.lexeme) {
      return Err(Box::new(DiagnosticMessage::EnumAlreadyDefined(enum_.name.clone())));
    }

    self.declare(
      enum_.name.lexeme.clone(),
      SymbolInfo::new(
        enum_.name.clone(),
        DataType::Enum(enum_.name.lexeme.clone(), Box::new(DataType::Pending)),
        vec![],
        SymbolKind::Enum,
        None,
      ),
    );
    self.begin_scope();

    let mut members = Vec::<HIREnumItem>::new();
    let mut data_type = DataType::Pending;

    for member in &enum_.members {
      if members.iter().any(|m| m.name == member.name) {
        return Err(Box::new(DiagnosticMessage::EnumMemberAlreadyDefined(member.name.clone())));
      }

      let mut value = None;
      if member.value.is_some() {
        value = Some(Box::new(self.analyzer(&member.value.as_ref().unwrap())?));
      }

      if let Some(value) = &value {
        if data_type == DataType::Pending {
          data_type = value.extract_data_type();
        } else if value.extract_data_type() != data_type {
          let value_data_type = value.extract_data_type();

          return Err(Box::new(DiagnosticMessage::EnumMemberTypeMismatch(
            member.name.clone(),
            data_type,
            value_data_type,
          )));
        }
      }

      members.push(HIREnumItem::new(
        member.name.clone(),
        value,
        data_type.clone(),
        HIRMetadata::new(vec![], None),
      ));
    }

    self.end_scope();

    let mut no_initializers = false;

    if matches!(
      data_type,
      DataType::Pending | DataType::Null | DataType::Unknown | DataType::Void
    ) {
      data_type = DataType::Int32;
      no_initializers = true;
    }

    let members_cloned = members.clone();

    for member in &mut members {
      member.data_type = data_type.clone();

      if data_type == DataType::Int32 && no_initializers {
        let position: i32 = members_cloned.iter().position(|m| m.name == member.name).unwrap() as i32;

        member.value = Some(Box::new(HIRInstruction::Literal(HIRLiteral::new(
          IgnisLiteralValue::Int32(position),
          member.name.clone(),
        ))));
      }
    }

    let mut flags: Vec<HIRMetadataFlags> = vec![];

    for value in enum_.metadata.get().iter() {
      flags.push(value.into());
    }

    let _enum = HIRInstruction::Enum(HIREnum::new(
      Box::new(enum_.name.clone()),
      members,
      HIRMetadata::new(flags.clone(), None),
      enum_
        .generics
        .iter()
        .map(|g| {
          DataType::GenericType(GenericType::new(
            Box::new(DataType::Variable(g.name.lexeme.clone(), Box::new(DataType::Unknown))),
            g.constraints.clone(),
          ))
        })
        .collect(),
      DataType::Enum(enum_.name.lexeme.clone(), Box::new(data_type.clone())),
    ));

    self.edit_symbol(
      enum_.name.lexeme.clone(),
      SymbolInfo::new(
        enum_.name.clone(),
        data_type,
        flags.clone(),
        SymbolKind::Enum,
        Some(_enum.clone()),
      ),
    );

    Ok(_enum)
  }

  fn visit_meta_statement(
    &mut self,
    meta: &ignis_ast::statements::meta::ASTMetaStatement,
  ) -> AnalyzerResult {
    if self.is_declared(&meta.name.lexeme) {
      return Err(Box::new(DiagnosticMessage::MethodAlreadyDefined(meta.name.clone())));
    }

    let mut metadata = vec![];

    for value in meta.metadata.get().iter() {
      metadata.push(value.into());
    }

    self.declare(
      meta.name.lexeme.clone(),
      SymbolInfo::new(meta.name.clone(), DataType::Null, metadata.clone(), SymbolKind::Meta, None),
    );

    self.begin_scope();

    let mut parameters = vec![];

    for param in &meta.parameters {
      let hir = self.analyze_statement(&ASTStatement::Variable(Box::new(param.clone())))?;
      parameters.push(hir);
    }

    self.end_scope();

    let hir = HIRInstruction::Meta(HIRMeta::new(
      meta.name.clone(),
      parameters,
      HIRMetadata::new(metadata.clone(), None),
      meta
        .generic_parameters
        .clone()
        .into_iter()
        .map(|g| {
          DataType::GenericType(GenericType::new(
            Box::new(DataType::Variable(g.name.lexeme.clone(), Box::new(DataType::Unknown))),
            g.constraints.clone(),
          ))
        })
        .collect(),
    ));

    self.edit_symbol(
      meta.name.lexeme.clone(),
      SymbolInfo::new(meta.name.clone(), DataType::Null, metadata, SymbolKind::Meta, Some(hir.clone())),
    );

    Ok(hir)
  }
}

impl IgnisAnalyzer {
  pub fn new(
    config: Arc<IgnisConfig>,
    current_file: String,
    program: Vec<ASTStatement>,
    primitives_std: Arc<HashMap<String, Vec<HIRInstruction>>>,
    primitives_symbol_stack: Arc<HashMap<String, Vec<HashMap<String, SymbolInfo>>>>,
  ) -> Self {
    let programs = HashMap::from([(current_file.to_string(), program)]);
    let hir = HashMap::from([(current_file.to_string(), vec![])]);

    Self {
      config,
      current_type: None,
      context: Vec::new(),
      current_block: vec![],
      current_file,
      programs,
      diagnostics: Vec::new(),
      hir,
      symbol_stack: vec![HashMap::new()],
      current_function: None,
      current_object: None,
      primitives_std,
      primitives_symbol_stack,
    }
  }
  pub fn process(
    &mut self,
    std: bool,
  ) -> Result<(), Vec<DiagnosticReport>> {
    if std {
      println!(
        "{:indent$}{} Analyzing... {}",
        " ",
        "-->".bright_yellow().bold(),
        self.current_file,
        indent = 6
      );
    } else {
      println!("{:indent$}Analyzing... {}", "-->".green().bold(), self.current_file, indent = 4);
    }

    let statements = self.programs.get(&self.current_file).unwrap().clone();

    for statement in &statements {
      if let ASTStatement::Comment(_) = statement {
        continue;
      }

      match self.analyze_statement(statement) {
        Ok(ir) => {
          let current_ir = self.hir.get_mut(&self.current_file).unwrap();
          current_ir.push(ir.clone());
        },
        Err(e) => self.diagnostics.push(e.as_ref().clone()),
      }
    }

    if !self.diagnostics.is_empty() {
      let mut reports = Vec::<DiagnosticReport>::new();

      for diagnostic in &self.diagnostics {
        reports.push(diagnostic.report());
      }

      return Err(reports);
    }

    Ok(())
  }

  fn analyzer(
    &mut self,
    expression: &ASTExpression,
  ) -> AnalyzerResult {
    expression.accept(self)
  }

  fn analyze_statement(
    &mut self,
    statement: &ASTStatement,
  ) -> AnalyzerResult {
    statement.accept(self)
  }

  pub fn get_diagnostics(&self) -> Vec<DiagnosticReport> {
    let mut diagnostics = Vec::<DiagnosticReport>::new();

    for diagnostic in &self.diagnostics {
      diagnostics.push(diagnostic.report());
    }

    diagnostics
  }

  pub fn get_hir(&self) -> &HashMap<String, Vec<HIRInstruction>> {
    &self.hir
  }

  fn are_types_compatible(
    &self,
    left: &HIRInstruction,
    right: &HIRInstruction,
    operator: &HIRInstructionType,
  ) -> CheckCompatibility<DataType> {
    let left_type = left.extract_data_type();
    let right_type = right.extract_data_type();

    match operator {
      HIRInstructionType::Concatenate => {
        if left_type == DataType::String && right_type == DataType::String {
          (true, DataType::String)
        } else {
          (false, DataType::Unknown)
        }
      },
      HIRInstructionType::Add => self.check_add_compatibility(&left_type, &right_type),
      HIRInstructionType::Sub | HIRInstructionType::Mul | HIRInstructionType::Div => {
        self.check_arithmetic_compatibility(&left_type, &right_type)
      },
      HIRInstructionType::GreaterEqual
      | HIRInstructionType::Greater
      | HIRInstructionType::LessEqual
      | HIRInstructionType::Less => self.check_comparation_compatibility(&left_type, &right_type),
      HIRInstructionType::Equal | HIRInstructionType::NotEqual => {
        self.check_equal_compatibility(&left_type, &right_type)
      },
      HIRInstructionType::And | HIRInstructionType::Or => self.check_logical_compatibility(&left_type, &right_type),
      HIRInstructionType::Mod => match (left_type, right_type.clone()) {
        (
          DataType::Int8 | DataType::Int16 | DataType::Int32 | DataType::Int64,
          DataType::Int8 | DataType::Int16 | DataType::Int32 | DataType::Int64,
        )
        | (
          DataType::UnsignedInt8 | DataType::UnsignedInt16 | DataType::UnsignedInt32 | DataType::UnsignedInt64,
          DataType::UnsignedInt8 | DataType::UnsignedInt16 | DataType::UnsignedInt32 | DataType::UnsignedInt64,
        )
        | (DataType::Float32, DataType::Float32)
        | (DataType::Float64, DataType::Float64) => (true, right_type.clone()),
        _ => (false, DataType::Unknown),
      },
      _ => (false, DataType::Unknown),
    }
  }

  fn check_add_compatibility(
    &self,
    left: &DataType,
    right: &DataType,
  ) -> CheckCompatibility<DataType> {
    match (left, right.clone()) {
      (DataType::Int8, DataType::Int8)
      | (DataType::Int16, DataType::Int16)
      | (DataType::Int32, DataType::Int32)
      | (DataType::Int64, DataType::Int64)
      | (DataType::UnsignedInt8, DataType::UnsignedInt8)
      | (DataType::UnsignedInt16, DataType::UnsignedInt16)
      | (DataType::UnsignedInt32, DataType::UnsignedInt32)
      | (DataType::UnsignedInt64, DataType::UnsignedInt64)
      | (DataType::Float32, DataType::Float32)
      | (DataType::Float64, DataType::Float64)
      | (DataType::String, DataType::String) => (true, right.clone()),
      (DataType::Char, DataType::Char)
      | (DataType::Char, DataType::UnsignedInt8)
      | (DataType::UnsignedInt8, DataType::Char) => (true, DataType::String),
      (_, DataType::Unknown) | (DataType::Unknown, _) => (true, DataType::Unknown),
      (_, DataType::Null) => (true, left.clone()),
      (DataType::Null, _) => (true, right.clone()),
      (DataType::GenericType(left_generic), DataType::GenericType(right_generic)) if left_generic == &right_generic => {
        (true, DataType::GenericType(left_generic.clone()))
      },
      (DataType::GenericType(_), _) | (_, DataType::GenericType(_)) => (true, DataType::Unknown),
      _ => (false, DataType::Unknown),
    }
  }

  fn check_arithmetic_compatibility(
    &self,
    left: &DataType,
    right: &DataType,
  ) -> CheckCompatibility<DataType> {
    match (left, right.clone()) {
      (DataType::Int8, DataType::Int8)
      | (DataType::Int16, DataType::Int16)
      | (DataType::Int32, DataType::Int32)
      | (DataType::Int64, DataType::Int64)
      | (DataType::UnsignedInt8, DataType::UnsignedInt8)
      | (DataType::UnsignedInt32, DataType::UnsignedInt32)
      | (DataType::UnsignedInt64, DataType::UnsignedInt64)
      | (DataType::Float32, DataType::Float32)
      | (DataType::Float64, DataType::Float64) => (true, right.clone()),
      (_, DataType::Unknown) | (DataType::Unknown, _) => (true, DataType::Unknown),
      (_, DataType::Null) => (true, left.clone()),
      (DataType::Null, _) => (true, right.clone()),
      _ => (false, DataType::Unknown),
    }
  }

  fn check_comparation_compatibility(
    &self,
    left: &DataType,
    right: &DataType,
  ) -> CheckCompatibility<DataType> {
    match (left, right) {
      (DataType::Int8, DataType::Int8)
      | (DataType::Int16, DataType::Int16)
      | (DataType::Int32, DataType::Int32)
      | (DataType::Int64, DataType::Int64)
      | (DataType::UnsignedInt8, DataType::UnsignedInt8)
      | (DataType::UnsignedInt32, DataType::UnsignedInt32)
      | (DataType::UnsignedInt64, DataType::UnsignedInt64)
      | (DataType::Float32, DataType::Float32)
      | (DataType::Float64, DataType::Float64)
      | (DataType::Unknown, _)
      | (_, DataType::Unknown) => (true, DataType::Boolean),
      (_, DataType::Null) => (true, left.clone()),
      (DataType::Null, _) => (true, right.clone()),
      _ => (false, DataType::Unknown),
    }
  }

  fn check_equal_compatibility(
    &self,
    left: &DataType,
    right: &DataType,
  ) -> CheckCompatibility<DataType> {
    let is_left_array_char = matches!(left, DataType::Vector(v, _) if matches!(v.as_ref(), DataType::Char));
    let is_right_array_char = matches!(right, DataType::Vector(v, _) if matches!(v.as_ref(), DataType::Char));
    let is_left_array_u8 = matches!(left, DataType::Vector(v, _) if matches!(v.as_ref(), DataType::UnsignedInt8));
    let is_right_array_u8 = matches!(right, DataType::Vector(v, _) if matches!(v.as_ref(), DataType::UnsignedInt8));

    if (is_left_array_char && matches!(right, DataType::String))
      || (matches!(left, DataType::String) && is_right_array_char)
      || (is_left_array_u8 && is_right_array_char)
      || (is_left_array_char && is_right_array_u8)
    {
      return (true, DataType::Boolean);
    }

    match (left, right) {
      (DataType::Char, DataType::Char)
      | (DataType::Char, DataType::UnsignedInt8)
      | (DataType::UnsignedInt8, DataType::Char)
      | (DataType::Int8, DataType::Int8)
      | (DataType::Int16, DataType::Int16)
      | (DataType::Int32, DataType::Int32)
      | (DataType::Int64, DataType::Int64)
      | (DataType::UnsignedInt8, DataType::UnsignedInt8)
      | (DataType::UnsignedInt16, DataType::UnsignedInt16)
      | (DataType::UnsignedInt32, DataType::UnsignedInt32)
      | (DataType::UnsignedInt64, DataType::UnsignedInt64)
      | (DataType::Float32, DataType::Float32)
      | (DataType::Float64, DataType::Float64)
      | (DataType::String, DataType::String)
      | (DataType::Boolean, DataType::Boolean) => (true, DataType::Boolean),
      (_, DataType::Null) | (_, DataType::Unknown) => (true, left.clone()),
      (DataType::Null, _) | (DataType::Unknown, _) => (true, right.clone()),
      _ => (false, DataType::Unknown),
    }
  }

  fn check_logical_compatibility(
    &self,
    left: &DataType,
    right: &DataType,
  ) -> CheckCompatibility<DataType> {
    match (left, right) {
      (DataType::Boolean, DataType::Boolean) => (true, DataType::Boolean),
      _ => (false, DataType::Unknown),
    }
  }

  fn convert_generic_number_to_number(
    &self,
    value: IgnisLiteralValue,
    to: &DataType,
    token: &Token,
  ) -> Result<IgnisLiteralValue, Box<DiagnosticMessage>> {
    match to {
      DataType::Int8 => {
        let mut number: i8 = 0;
        if let IgnisLiteralValue::Int64(num) = value {
          if num > i8::MAX as i64 || num < i8::MIN as i64 {
            return Err(Box::new(DiagnosticMessage::ValueOutOfRange(
              value,
              DataType::Int8,
              token.clone(),
            )));
          }

          number = num as i8;
        }

        Ok(IgnisLiteralValue::Int8(number))
      },
      DataType::Int16 => {
        let mut number: i16 = 0;
        if let IgnisLiteralValue::Int64(num) = value {
          if num > i16::MAX as i64 || num < i16::MIN as i64 {
            return Err(Box::new(DiagnosticMessage::ValueOutOfRange(
              value,
              DataType::Int16,
              token.clone(),
            )));
          }

          number = num as i16;
        }

        Ok(IgnisLiteralValue::Int16(number))
      },
      DataType::Int32 => {
        let mut number: i32 = 0;

        if let IgnisLiteralValue::Int64(num) = value {
          if num > i32::MAX as i64 || num < i32::MIN as i64 {
            return Err(Box::new(DiagnosticMessage::ValueOutOfRange(
              value,
              DataType::Int32,
              token.clone(),
            )));
          }

          number = num as i32;
        }

        Ok(IgnisLiteralValue::Int32(number))
      },
      DataType::Int64 => {
        if self.context.contains(&AnalyzerContext::ForCondition) {
          return self.convert_generic_number_to_number(value, &DataType::UnsignedInt32, token);
        }

        Ok(value)
      },
      DataType::UnsignedInt8 => {
        let mut number: u8 = 0;
        if let IgnisLiteralValue::Int64(num) = value {
          if num > u8::MAX as i64 || num < 0 {
            return Err(Box::new(DiagnosticMessage::ValueOutOfRange(
              value,
              DataType::UnsignedInt8,
              token.clone(),
            )));
          }

          number = num as u8;
        }

        Ok(IgnisLiteralValue::UnsignedInt8(number))
      },
      DataType::UnsignedInt16 => {
        let mut number: u16 = 0;
        if let IgnisLiteralValue::Int64(num) = value {
          if num > u16::MAX as i64 || num < 0 {
            return Err(Box::new(DiagnosticMessage::ValueOutOfRange(
              value,
              DataType::UnsignedInt16,
              token.clone(),
            )));
          }

          number = num as u16;
        }

        Ok(IgnisLiteralValue::UnsignedInt16(number))
      },
      DataType::UnsignedInt32 => {
        let mut number: u32 = 0;

        if let IgnisLiteralValue::Int64(num) = value {
          if num > u32::MAX as i64 || num < 0 {
            return Err(Box::new(DiagnosticMessage::ValueOutOfRange(
              value,
              DataType::UnsignedInt32,
              token.clone(),
            )));
          }

          number = num as u32;
        }

        Ok(IgnisLiteralValue::UnsignedInt32(number))
      },
      DataType::UnsignedInt64 => {
        let mut number: u64 = 0;

        if let IgnisLiteralValue::Int64(num) = value {
          if num < 0 {
            return Err(Box::new(DiagnosticMessage::ValueOutOfRange(
              value,
              DataType::UnsignedInt64,
              token.clone(),
            )));
          }

          number = num as u64;
        }

        Ok(IgnisLiteralValue::UnsignedInt64(number))
      },
      DataType::Float32 => {
        let mut number: f32 = 0.0;

        if let IgnisLiteralValue::Float64(num) = value {
          if num > f32::MAX as f64 || num < f32::MIN as f64 {
            return Err(Box::new(DiagnosticMessage::ValueOutOfRange(
              value,
              DataType::Float32,
              token.clone(),
            )));
          }

          number = num as f32;
        }

        Ok(IgnisLiteralValue::Float32(number))
      },
      _ => Ok(value),
    }
  }

  fn begin_scope(&mut self) {
    self.symbol_stack.push(HashMap::new());
  }

  fn end_scope(&mut self) {
    let _ = self.symbol_stack.pop();
  }

  fn declare(
    &mut self,
    name: String,
    info: SymbolInfo,
  ) {
    if let Some(scope) = self.symbol_stack.last_mut() {
      scope.insert(name, info);
    }
  }

  fn edit_symbol(
    &mut self,
    name: String,
    info: SymbolInfo,
  ) {
    for symbol_stack in self.symbol_stack.iter_mut().rev() {
      if let Some(symbol) = &mut symbol_stack.get_mut(&name) {
        symbol.clone_from(&&info);
      }
    }
  }

  fn resolve(
    &self,
    name: &str,
  ) -> Option<&SymbolInfo> {
    for scope in self.symbol_stack.iter().rev() {
      if let Some(info) = scope.get(name) {
        return Some(info);
      }
    }
    None
  }

  fn is_declared(
    &self,
    name: &str,
  ) -> bool {
    self.resolve(name).is_some()
  }

  fn find_intruction(
    &self,
    name: &str,
  ) -> Option<&HIRInstruction> {
    let current_file = &self.current_file;
    let hir = self.hir.get(current_file).unwrap();

    for instruction in hir.iter() {
      match instruction {
        HIRInstruction::Variable(v) => {
          if v.name.lexeme == name {
            return Some(instruction);
          }
        },
        HIRInstruction::Function(f) => {
          if f.name.lexeme == name {
            return Some(instruction);
          }
        },
        _ => (),
      }
    }

    None
  }

  fn find_hir_in_block(
    &self,
    name: &String,
  ) -> Option<HIRInstruction> {
    let current_block = &self.current_block;
    let hir = current_block.iter().find(|ir| match ir {
      HIRInstruction::Variable(v) => &v.name.lexeme == name,
      _ => false,
    });

    if let Some(HIRInstruction::Variable(v)) = hir {
      return Some(HIRInstruction::Variable(v.clone()));
    }

    None
  }

  fn are_types_logical_compatibel(
    &self,
    left: &HIRInstruction,
    right: &HIRInstruction,
    operator: &HIRInstructionType,
  ) -> bool {
    let left = left.extract_data_type();
    let right = right.extract_data_type();

    match operator {
      HIRInstructionType::And | HIRInstructionType::Or => {
        matches!((left, right), (DataType::Boolean, DataType::Boolean,))
      },
      _ => false,
    }
  }

  fn assign_value_and_complex_data(
    &mut self,
    variable: &Token,
    initializer: &Option<Box<ASTExpression>>,
    data_type: &mut DataType,
  ) -> Result<(HIRInstruction, Option<Box<HIRInstruction>>), Box<DiagnosticMessage>> {
    let mut value = HIRInstruction::Literal(HIRLiteral::new(IgnisLiteralValue::Null, variable.clone()));
    let complex_type = None;

    if let DataType::PendingImport(_) = data_type {
      let import = self.resolve_pedding_import(variable, data_type)?;

      match import {
        HIRInstruction::Enum(_enum) => {
          *data_type = _enum.data_type.clone();
        },
        HIRInstruction::Record(_record) => {
          *data_type = _record.data_type.clone();
        },
        _ => (),
      }
    }

    if let Some(initializer) = initializer {
      let expression = self.analyzer(initializer)?;

      value = match expression {
        HIRInstruction::Literal(literal) => {
          self.check_type_mismatch(&literal.value.clone().into(), data_type, variable)?;
          HIRInstruction::Literal(literal)
        },
        HIRInstruction::Binary(binary) => {
          self.check_type_mismatch(&binary.data_type, data_type, variable)?;
          HIRInstruction::Binary(binary)
        },
        HIRInstruction::Unary(unary) => {
          self.check_type_mismatch(&unary.data_type, data_type, variable)?;
          HIRInstruction::Unary(unary)
        },
        HIRInstruction::Variable(_variable) => {
          self.check_type_mismatch(&_variable.data_type, data_type, variable)?;
          HIRInstruction::Variable(_variable)
        },
        HIRInstruction::Ternary(ternary) => {
          self.check_type_mismatch(&ternary.data_type, data_type, variable)?;
          HIRInstruction::Ternary(ternary)
        },
        HIRInstruction::Call(call) => {
          self.check_type_mismatch(&call.return_type, data_type, variable)?;
          HIRInstruction::Call(call)
        },
        HIRInstruction::Logical(logical) => {
          self.check_type_mismatch(&DataType::Boolean, data_type, variable)?;
          HIRInstruction::Logical(logical)
        },
        HIRInstruction::Vector(vector) => {
          self.check_type_mismatch(&vector.data_type, data_type, variable)?;
          let mut vector_mut = vector.clone();
          vector_mut.data_type = data_type.clone();
          HIRInstruction::Vector(vector_mut)
        },
        HIRInstruction::Enum(_enum) => {
          self.check_type_mismatch(&_enum.data_type, data_type, variable)?;
          HIRInstruction::Enum(_enum)
        },
        HIRInstruction::Record(record) => {
          self.check_type_mismatch(&record.data_type, data_type, variable)?;
          HIRInstruction::Record(record)
        },
        HIRInstruction::MemberAccess(member) => {
          self.check_type_mismatch(&member.member.extract_data_type(), data_type, variable)?;
          HIRInstruction::MemberAccess(member)
        },
        HIRInstruction::Cast(cast) => {
          self.check_type_mismatch(&cast.target_type, data_type, variable)?;
          HIRInstruction::Cast(cast)
        },
        HIRInstruction::Object(object) => {
          self.check_type_mismatch(&object.data_type, data_type, variable)?;
          HIRInstruction::Object(object)
        },
        HIRInstruction::This(this) => {
          self.check_type_mismatch(&this.data_type, data_type, variable)?;
          HIRInstruction::This(this)
        },
        HIRInstruction::Constant(constant) => {
          self.check_type_mismatch(&constant.data_type, data_type, variable)?;
          HIRInstruction::Constant(constant)
        },
        _ => {
          return Err(Box::new(DiagnosticMessage::InvalidVariableInitializer(variable.clone())));
        },
      };
    }

    Ok((value, complex_type))
  }

  fn check_type_mismatch(
    &mut self,
    left: &DataType,
    right: &DataType,
    token: &Token,
  ) -> Result<(), Box<DiagnosticMessage>> {
    match (left, right) {
      (DataType::Binary, DataType::Binary)
      | (DataType::Hex, DataType::Hex)
      | (DataType::Binary, DataType::Hex)
      | (DataType::Hex, DataType::Binary)
      | (DataType::Hex, _)
      | (_, DataType::Hex) => {
        return Ok(());
      },
      (DataType::Vector(left, l_size), DataType::Vector(right, r_size)) => {
        if self.check_type_mismatch(left, right, token).is_ok() {
          if l_size.is_some() && r_size.is_some() {
            let l_size = l_size.as_ref().unwrap();
            let r_size = r_size.as_ref().unwrap();

            if l_size != r_size {
              return Err(Box::new(DiagnosticMessage::TypeMismatch(
                *left.clone(),
                *right.clone(),
                token.clone(),
              )));
            }
          }
        } else {
          return Err(Box::new(DiagnosticMessage::TypeMismatch(
            *left.clone(),
            *right.clone(),
            token.clone(),
          )));
        }
      },
      (DataType::IntersectionType(types), _)
      | (_, DataType::IntersectionType(types))
      | (DataType::UnionType(types), _)
      | (_, DataType::UnionType(types)) => {
        if let DataType::IntersectionType(types) = left {
          return self.resolve_intersection_type(types, token);
        }

        if let DataType::UnionType(types) = left {
          return self.resolve_union_type(types, token);
        }

        if let DataType::IntersectionType(types) = right {
          return self.resolve_intersection_type(types, token);
        }

        if let DataType::UnionType(types) = right {
          return self.resolve_union_type(types, token);
        }

        if !(types.contains(left) || types.contains(right)) {
          return Err(Box::new(DiagnosticMessage::TypeMismatch(
            left.clone(),
            right.clone(),
            token.clone(),
          )));
        }

        return Ok(());
      },
      (DataType::Record(_, record), DataType::Object(object))
      | (DataType::Object(object), DataType::Record(_, record)) => {
        for r in record {
          let o = object.iter().find(|d| d.0 == r.0);

          if o.is_none() && (!matches!(r.1, DataType::Optional(_))) {
            continue;
          }

          if o.is_none() {
            return Err(Box::new(DiagnosticMessage::TypeMismatch(
              left.clone(),
              right.clone(),
              token.clone(),
            )));
          }

          let o = o.unwrap();

          match &o.1 {
            DataType::Variable(name, data_type) => {
              if let DataType::Variable(name2, data_type2) = &r.1 {
                if name == name2 && data_type == data_type2 {
                  continue;
                }
              }
            },
            _ => (),
          };

          match &r.1 {
            DataType::Variable(name, data_type) => {
              if let DataType::Variable(name2, data_type2) = &o.1 {
                if name == name2 && data_type == data_type2 {
                  continue;
                }
              }
            },
            _ => (),
          };

          if let (
            DataType::Function(left_param, left_return_type),
            DataType::Function(right_param, right_return_type),
          ) = (r.1.clone(), o.1.clone())
          {
            if left_param.len() != right_param.len() || left_return_type != right_return_type {
              return Err(Box::new(DiagnosticMessage::TypeMismatch(
                left.clone(),
                right.clone(),
                token.clone(),
              )));
            }

            for (left, right) in left_param.iter().zip(right_param) {
              if left != &right {
                return Err(Box::new(DiagnosticMessage::TypeMismatch(
                  left.clone(),
                  right.clone(),
                  token.clone(),
                )));
              }
            }

            continue;
          }

          if let (DataType::Vector(left, _), DataType::Vector(right, _)) = (r.1.clone(), o.1.clone()) {
            return self.check_type_mismatch(left.as_ref(), right.as_ref(), token);
          }

          if r.1 != o.1 {
            return Err(Box::new(DiagnosticMessage::TypeMismatch(
              left.clone(),
              right.clone(),
              token.clone(),
            )));
          }
        }
        return Ok(());
      },
      (DataType::PendingImport(import), DataType::ClassType(name))
      | (DataType::ClassType(name), DataType::PendingImport(import))
      | (DataType::PendingImport(import), DataType::Interface(name))
      | (DataType::Interface(name), DataType::PendingImport(import))
      | (DataType::PendingImport(import), DataType::Enum(name, _))
      | (DataType::Enum(name, _), DataType::PendingImport(import))
      | (DataType::PendingImport(import), DataType::AliasType(name))
      | (DataType::PendingImport(import), DataType::Record(name, _))
      | (DataType::Record(import, _), DataType::PendingImport(name))
      | (DataType::AliasType(name), DataType::PendingImport(import)) => {
        if import != name {
          return Err(Box::new(DiagnosticMessage::TypeMismatch(
            left.clone(),
            right.clone(),
            token.clone(),
          )));
        }

        return Ok(());
      },
      (DataType::GenericType(generic), _) | (_, DataType::GenericType(generic)) => {
        if generic.constraints.is_empty() {
          return Ok(());
        }

        for constraint in &generic.constraints {
          if constraint == left || constraint == right {
            return Ok(());
          }

          if let DataType::PendingImport(name) = constraint {
            let t = &self.find_type(name);
            if t.is_none() {
              return Err(Box::new(DiagnosticMessage::UndefinedType(name.clone(), token.clone())));
            }

            let ir = t.as_ref().unwrap();
            let kind = ir.extract_data_type();

            if let DataType::IntersectionType(inter) = &kind {
              if inter.contains(left) || inter.contains(right) {
                return Ok(());
              }
            }

            if let DataType::UnionType(inter) = &kind {
              if inter.contains(left) || inter.contains(right) {
                return Ok(());
              }
            }
          }

          if let DataType::IntersectionType(inter) = constraint {
            if inter.contains(left) || inter.contains(right) {
              return Ok(());
            }
          }

          if let DataType::UnionType(inter) = constraint {
            if inter.contains(left) || inter.contains(right) {
              return Ok(());
            }
          }
        }

        return Err(Box::new(DiagnosticMessage::TypeMismatch(
          left.clone(),
          right.clone(),
          token.clone(),
        )));
      },
      (DataType::Reference(left_value), DataType::Reference(right_value)) => {
        return self.check_type_mismatch(left_value.as_ref(), right_value.as_ref(), token);
      },
      (DataType::Enum(_, enum_type), _) | (_, DataType::Enum(_, enum_type)) => {
        if enum_type.as_ref() == right || enum_type.as_ref() == left {
          return Ok(());
        }
      },
      _ => (),
    };

    if left != right && !matches!(left, DataType::Unknown) && !matches!(right, DataType::Unknown) {
      return Err(Box::new(DiagnosticMessage::TypeMismatch(
        left.clone(),
        right.clone(),
        token.clone(),
      )));
    }

    Ok(())
  }

  fn find_type(
    &self,
    name: &String,
  ) -> Option<HIRInstruction> {
    let name_file = &self.current_file;
    let irs = self.hir.get(name_file).unwrap();
    let irs_std = self.primitives_std.clone();
    let type_ir = irs_std.get("std/types/mod.ign").unwrap();

    let current_ir = irs
      .iter()
      .find(|ir| match ir {
        HIRInstruction::Enum(e) => &e.name.lexeme == name,
        HIRInstruction::Record(r) => &r.name.lexeme == name,
        HIRInstruction::Type(t) => &t.name.lexeme == name,
        _ => false,
      })
      .cloned();

    if current_ir.is_some() {
      return current_ir;
    }

    let type_in_std = type_ir.iter().find(|ir| match ir {
      HIRInstruction::Enum(e) => &e.name.lexeme == name,
      HIRInstruction::Record(r) => &r.name.lexeme == name,
      HIRInstruction::Type(t) => &t.name.lexeme == name,
      _ => false,
    });

    if type_in_std.is_some() {
      return type_in_std.cloned();
    }

    None
  }

  fn check_tuple(
    &self,
    tuple: &[DataType],
    token: &Token,
  ) -> Result<(), Box<DiagnosticMessage>> {
    for (i, t) in tuple.iter().enumerate() {
      for (j, t2) in tuple.iter().enumerate() {
        if i == j {
          continue;
        }

        if t == t2 {
          return Err(Box::new(DiagnosticMessage::TupleTypeMismatch(
            t.clone(),
            t2.clone(),
            token.clone(),
          )));
        }
      }
    }

    Ok(())
  }

  fn resolve_union_type(
    &self,
    types: &[DataType],
    token: &Token,
  ) -> Result<(), Box<DiagnosticMessage>> {
    for t in types {
      match t {
        DataType::Enum(name, _)
        | DataType::ClassType(name)
        | DataType::Interface(name)
        | DataType::AliasType(name)
        | DataType::Record(name, _) => {
          if !self.is_declared(name) {
            return Err(Box::new(DiagnosticMessage::UndefinedType(name.clone(), token.clone())));
          }
        },
        _ => (),
      };
    }

    Ok(())
  }

  fn resolve_intersection_type(
    &self,
    types: &[DataType],
    token: &Token,
  ) -> Result<(), Box<DiagnosticMessage>> {
    for t in types {
      match t {
        DataType::Enum(name, _) | DataType::ClassType(name) | DataType::Interface(name) | DataType::AliasType(name) => {
          if !self.is_declared(name) {
            return Err(Box::new(DiagnosticMessage::UndefinedType(name.clone(), token.clone())));
          }
        },
        _ => (),
      };
    }
    Ok(())
  }

  fn is_valid_cast(
    &self,
    origin_type: &DataType,
    target_type: &DataType,
  ) -> bool {
    match (origin_type, target_type) {
      (
        DataType::Int8
        | DataType::Int16
        | DataType::Int32
        | DataType::Int64
        | DataType::UnsignedInt8
        | DataType::UnsignedInt16
        | DataType::UnsignedInt32
        | DataType::UnsignedInt64
        | DataType::Float32
        | DataType::Float64,
        DataType::Int8
        | DataType::Int16
        | DataType::Int32
        | DataType::Int64
        | DataType::UnsignedInt8
        | DataType::UnsignedInt16
        | DataType::UnsignedInt32
        | DataType::UnsignedInt64
        | DataType::Float32
        | DataType::Float64,
      ) => true,
      (DataType::UnionType(types), _)
      | (_, DataType::UnionType(types))
      | (DataType::IntersectionType(types), _)
      | (_, DataType::IntersectionType(types)) => types.contains(origin_type) || types.contains(target_type),
      _ => false,
    }
  }

  fn resolve_pedding_import(
    &mut self,
    token: &Token,
    import: &DataType,
  ) -> AnalyzerResult {
    if let DataType::PendingImport(name) = import {
      if let Some(function) = self.find_function_instance(name) {
        Ok(HIRInstruction::FunctionInstance(function))
      } else if let Some(enum_) = self.find_enum_in_ir(name) {
        Ok(HIRInstruction::Enum(enum_))
      } else if let Some(record) = self.find_record_in_ir(name) {
        Ok(HIRInstruction::Record(record))
      } else {
        Err(Box::new(DiagnosticMessage::UndefinedImport(token.clone())))
      }
    } else {
      Err(Box::new(DiagnosticMessage::UndefinedImport(token.clone())))
    }
  }

  // fn find_function_instance(
  //   &self,
  //   instance_name: &String,
  // ) -> Option<HIRFunctionInstance> {
  //   let name_file = &self.current_file;
  //   let function_ir = self.irs.get(name_file).unwrap();
  //
  //   if let Some(f) = function_ir
  //     .iter()
  //     .find(|ir| matches!(ir, HIRInstruction::FunctionInstance(f) if f.instance_name.span.literal == *instance_name))
  //   {
  //     if let HIRInstruction::FunctionInstance(f) = f {
  //       return Some(f.clone());
  //     }
  //
  //     return None;
  //   }
  //
  //   None
  // }

  fn find_enum_in_ir(
    &self,
    name: &String,
  ) -> Option<HIREnum> {
    let irs = self.hir.get(&self.current_file).unwrap();

    let enum_ = irs.iter().find(|ir| match ir {
      HIRInstruction::Enum(e) => &e.name.lexeme == name,
      HIRInstruction::Namespace(n) => n.members.iter().any(|m| match m {
        HIRInstruction::Enum(e) => &e.name.lexeme == name,
        _ => false,
      }),
      HIRInstruction::Extern(e) => e.body.iter().any(|m| match m {
        HIRInstruction::Enum(e) => &e.name.lexeme == name,
        _ => false,
      }),
      _ => false,
    });

    let mut enum_finded = None;

    match enum_ {
      Some(HIRInstruction::Enum(e)) => enum_finded = Some(e.clone()),
      Some(HIRInstruction::Namespace(n)) => {
        n.members.iter().map(|m| match m {
          HIRInstruction::Enum(e) if &e.name.lexeme == name => enum_finded = Some(e.clone()),
          _ => (),
        });
      },
      Some(HIRInstruction::Extern(e)) => {
        e.body.iter().map(|m| match m {
          HIRInstruction::Enum(e) if &e.name.lexeme == name => enum_finded = Some(e.clone()),
          _ => (),
        });
      },
      _ => (),
    };

    enum_finded
  }

  fn find_enum_in_scope(
    &self,
    name: &str,
  ) -> Option<HIREnum> {
    if self.symbol_stack.len() < 2 {
      return None;
    }

    let irs = self.symbol_stack[self.symbol_stack.len() - 2].clone();

    for (name2, symbol) in irs {
      if symbol.kind == SymbolKind::Enum {
        if &symbol.name.lexeme == name {
          match symbol.hir_ref.as_ref().unwrap().clone() {
            HIRInstruction::Enum(e) => return Some(e.clone()),
            _ => unreachable!("Never should reach this. If it does, I did something wrong."),
          }
        }
      }
    }

    None
  }

  fn find_record_in_ir(
    &self,
    name: &str,
  ) -> Option<HIRRecord> {
    let irs = self.hir.get(&self.current_file).unwrap();

    let record = irs.iter().find(|ir| match ir {
      HIRInstruction::Record(r) => r.name.lexeme == name,
      _ => false,
    });

    match record {
      Some(HIRInstruction::Record(r)) => Some(r.clone()),
      _ => None,
    }
  }

  fn _find_alias_in_ir(
    &self,
    name: &str,
  ) -> Option<HIRType> {
    let irs = self.hir.get(&self.current_file).unwrap();
    let alias = irs.iter().find(|ir| match ir {
      HIRInstruction::Type(a) => a.name.lexeme == name,
      _ => false,
    });
    match alias {
      Some(HIRInstruction::Type(a)) => Some(a.clone()),
      _ => None,
    }
  }

  pub fn load_primitive_std(
    config: Arc<IgnisConfig>
  ) -> (
    HashMap<String, Vec<HIRInstruction>>,
    HashMap<String, Vec<HashMap<String, SymbolInfo>>>,
  ) {
    if !config.auto_load_std || !config.std {
      return (HashMap::new(), HashMap::new());
    }

    println!("{:indent$}Loading STD...", "-->".green().bold(), indent = 4);

    let mut diagnostics: Vec<DiagnosticReport> = Vec::new();
    let names = [
      "meta".to_string(),
      "types".to_string(),
      "string".to_string(),
      "number".to_string(),
      "char".to_string(),
      // "array".to_string(),
    ]
    .to_vec();
    let mut result_std = HashMap::new();
    let mut result_scopes_variables: HashMap<String, Vec<HashMap<String, SymbolInfo>>> = HashMap::new();

    for name in names.iter() {
      let lib: String = "std/".to_owned() + name + "/mod.ign";

      if result_std.contains_key(&lib) {
        continue;
      }

      let std_path = Path::new(&config.std_path).join("std");
      let lib_path_str = format!("{}/{}", name, "mod.ign");
      let lib_path = std_path.join(&lib_path_str);
      let file_row = fs::read(lib_path);

      if file_row.is_err() {
        panic!("{:?}", file_row);
      }

      let file_row = file_row.unwrap();
      let source = String::from_utf8(file_row).unwrap();

      let mut lexer = IgnisLexer::new(&config, &source, &lib);
      lexer.scan_tokens(true);
      for diagnostic in &lexer.diagnostics {
        diagnostics.push(diagnostic.report());
      }

      let mut parser = IgnisParser::new(&config, &lexer.tokens);
      let result = parser.parse(true);

      for diagnostic in &result.1 {
        diagnostics.push(diagnostic.clone());
      }

      let statements = result.0;
      let mut analyzer = Self::new(
        config.clone(),
        lib.clone(),
        statements,
        result_std.clone().into(),
        result_scopes_variables.clone().into(),
      );

      let analyzer_report = analyzer.process(true);

      if analyzer_report.is_err() {
        for diagnostic in &analyzer_report.err().unwrap() {
          diagnostics.push(diagnostic.clone());
        }
      }

      result_std.insert(lib.clone(), analyzer.hir.get(&lib).unwrap().clone());
      result_scopes_variables.insert(lib.clone(), analyzer.symbol_stack.clone());
    }

    if !config.quiet {
      let diagnostic: Diagnostic = Diagnostic::default();

      diagnostic.report(&diagnostics);
    }

    (result_std, result_scopes_variables)
  }

  fn resolve_generic_params(
    &mut self,
    expression: &ASTCall,
    parameters: &mut [HIRInstruction],
    function_name: &Token,
    instance_name: &Token,
    return_type: &DataType,
    generic_parameter: &mut [DataType],
    metadata: &HIRMetadata,
    body: &Option<Box<HIRBlock>>,
  ) -> Result<HIRFunctionInstance, Box<DiagnosticMessage>> {
    let mut return_type: DataType = return_type.clone();

    for (i, generic) in generic_parameter.to_owned().clone().iter().enumerate() {
      if let DataType::GenericType(g) = generic {
        let provided_type: DataType = expression.arguments[i].clone().into();

        let is_valid_type = g.constraints.iter().any(|allowed_type| allowed_type == &provided_type);

        if g.constraints.is_empty() {
          generic_parameter[i] = provided_type.clone();
        } else if !is_valid_type {
          return Err(Box::new(DiagnosticMessage::InvalidTypeArgument(
            provided_type.clone(),
            g.base.clone(),
            expression.name.clone(),
          )));
        }

        parameters.iter_mut().for_each(|p| {
          if let HIRInstruction::Variable(v) = p {
            if let DataType::GenericType(ref x) = v.data_type {
              if x.base == g.base {
                v.data_type = provided_type.clone();
              }
            };
          }
        });
      }
    }

    if let DataType::GenericType(x) = &return_type {
      if let Some(lv) = expression.arguments.last() {
        let lv: DataType = lv.clone().into();

        if x.constraints.contains(&lv) || x.constraints.is_empty() {
          return_type = generic_parameter.last().unwrap().clone();
        } else {
          return Err(Box::new(DiagnosticMessage::InvalidTypeArgument(
            lv.clone(),
            x.base.clone(),
            expression.name.clone(),
          )));
        }
      }
    }

    let function_instance = HIRFunctionInstance::new(
      Box::new(function_name.clone()),
      Box::new(instance_name.clone()),
      generic_parameter.to_owned().clone(),
      parameters.to_owned().clone(),
      return_type.clone(),
      body.clone(),
      metadata.clone(),
    );

    HIRInstruction::FunctionInstance(function_instance.clone());
    let _name_file = &self.current_file;

    self.declare(
      function_name.lexeme.clone(),
      SymbolInfo::new(
        function_name.clone(),
        function_instance.return_type.clone(),
        function_instance.metadata.flags.clone(),
        SymbolKind::Function,
        None,
      ),
    );

    Ok(function_instance)
  }

  fn find_function_instance(
    &self,
    instance_name: &String,
  ) -> Option<HIRFunctionInstance> {
    let name_file = &self.current_file;
    let function_ir = self.hir.get(name_file).unwrap();

    if let Some(f) = function_ir
      .iter()
      .find(|ir| matches!(ir, HIRInstruction::FunctionInstance(f) if f.instance_name.lexeme== *instance_name))
    {
      if let HIRInstruction::FunctionInstance(f) = f {
        return Some(f.clone());
      }

      return None;
    }

    None
  }

  fn are_types_unary_compatible(
    &self,
    right: &HIRInstruction,
    operator: &HIRInstructionType,
  ) -> bool {
    match operator {
      HIRInstructionType::Sub => matches!(
        right,
        HIRInstruction::Literal(HIRLiteral {
          value: IgnisLiteralValue::Int8(_)
            | IgnisLiteralValue::Int16(_)
            | IgnisLiteralValue::Int32(_)
            | IgnisLiteralValue::Int64(_)
            | IgnisLiteralValue::Float32(_)
            | IgnisLiteralValue::Float64(_),
          ..
        })
      ),
      HIRInstructionType::Not => matches!(
        right,
        HIRInstruction::Literal(HIRLiteral {
          value: IgnisLiteralValue::Boolean(_)
            | IgnisLiteralValue::Int8(_)
            | IgnisLiteralValue::Int16(_)
            | IgnisLiteralValue::Int32(_)
            | IgnisLiteralValue::Int64(_)
            | IgnisLiteralValue::UnsignedInt8(_)
            | IgnisLiteralValue::UnsignedInt16(_)
            | IgnisLiteralValue::UnsignedInt32(_)
            | IgnisLiteralValue::UnsignedInt64(_)
            | IgnisLiteralValue::Float32(_)
            | IgnisLiteralValue::Float64(_)
            | IgnisLiteralValue::String(_)
            | IgnisLiteralValue::Char(_)
            | IgnisLiteralValue::Null,
          ..
        }) | HIRInstruction::Binary(HIRBinary {
          data_type: DataType::Boolean
            | DataType::Int8
            | DataType::Int16
            | DataType::Int32
            | DataType::Int64
            | DataType::UnsignedInt8
            | DataType::UnsignedInt16
            | DataType::UnsignedInt32
            | DataType::UnsignedInt64
            | DataType::Float32
            | DataType::Float64
            | DataType::Char
            | DataType::String
            | DataType::Null
            | DataType::Unknown,
          ..
        })
      ),
      HIRInstructionType::Increment | HIRInstructionType::Decrement => {
        matches!(
          right,
          HIRInstruction::Literal(HIRLiteral {
            value: IgnisLiteralValue::Int8(_)
              | IgnisLiteralValue::Int16(_)
              | IgnisLiteralValue::Int32(_)
              | IgnisLiteralValue::Int64(_)
              | IgnisLiteralValue::UnsignedInt8(_)
              | IgnisLiteralValue::UnsignedInt16(_)
              | IgnisLiteralValue::UnsignedInt32(_)
              | IgnisLiteralValue::UnsignedInt64(_)
              | IgnisLiteralValue::Float32(_)
              | IgnisLiteralValue::Float64(_),
            ..
          }) | HIRInstruction::Variable(HIRVariable {
            data_type: DataType::Int8
              | DataType::Int16
              | DataType::Int32
              | DataType::Int64
              | DataType::UnsignedInt8
              | DataType::UnsignedInt16
              | DataType::UnsignedInt32
              | DataType::UnsignedInt64
              | DataType::Float32
              | DataType::Float64,
            ..
          })
        )
      },
      _ => false,
    }
  }

  fn resolve_std_import(
    &mut self,
    statement: &ASTImport,
    block_stack: &mut HashMap<String, SymbolInfo>,
  ) -> Result<(), Box<DiagnosticMessage>> {
    let lib_path_str = statement.module_path.lexeme.split("::").collect::<Vec<&str>>()[1].to_owned() + "/mod.ign";
    let lib: String = format!("std/{}", lib_path_str);

    if self.primitives_std.contains_key(&lib) {
      let irs = self.primitives_std.get(&lib).unwrap().clone();
      let import_scope_variables = self.primitives_symbol_stack.get(&lib).unwrap().clone();

      for ir in &irs {
        self.define_import(&statement, ir, block_stack, &import_scope_variables)?;
      }

      return Ok(());
    }

    let mut env = std::env::var("IGNIS_HOME");

    if env.is_err() {
      env = std::env::var("PWD");
    }

    let std_path = Path::new(&env.unwrap()).join("std");
    let lib_path = std_path.join(&lib_path_str);
    let file_row = fs::read(lib_path);

    if file_row.is_err() {
      println!("{:?}", file_row);
      return Ok(());
    }

    let file = file_row.unwrap();

    let source = String::from_utf8(file).unwrap();
    let mut lexer = IgnisLexer::new(&self.config, &source, &lib);
    lexer.scan_tokens(true);

    let mut parser = IgnisParser::new(&self.config, &lexer.tokens);
    let result = parser.parse(true);
    let statements = result.0;

    let mut analyzer = Self::new(
      self.config.clone(),
      lib.clone(),
      statements,
      self.primitives_std.clone(),
      self.primitives_symbol_stack.clone(),
    );
    let _ = analyzer.process(true);

    analyzer.diagnostics.iter().for_each(|d| {
      self.diagnostics.push(d.clone());
    });

    let import_scope_variables = analyzer.symbol_stack.clone();

    for ir in analyzer.hir.get(&lib).unwrap() {
      self.define_import(statement, ir, block_stack, &import_scope_variables)?;
    }

    for ir in analyzer.hir {
      self.hir.insert(ir.0.clone(), ir.1.clone());
    }

    Ok(())
  }

  fn define_import(
    &mut self,
    statement: &ASTImport,
    ir: &HIRInstruction,
    block_stack: &mut HashMap<String, SymbolInfo>,
    import_scope_variables: &Vec<HashMap<String, SymbolInfo>>,
  ) -> Result<(), Box<DiagnosticMessage>> {
    let mut current_ir = self.hir.get(&self.current_file).unwrap().clone();

    match ir {
      HIRInstruction::Function(function) => {
        for symbol in &statement.symbols {
          if symbol.name.lexeme == function.name.lexeme && !function.metadata.is(HIRMetadataFlags::Exported) {
            return Err(Box::new(DiagnosticMessage::ImportedFunctionIsNotExported(symbol.name.clone())));
          }

          let mut variable = import_scope_variables
            .iter()
            .find(|v| v.get(&symbol.name.lexeme).is_some())
            .unwrap()
            .clone();
          let variable = variable.get_mut(&symbol.name.lexeme).unwrap();

          let mut metadata = function.metadata.clone();

          metadata.remove_flag(HIRMetadataFlags::Exported);
          metadata.push(HIRMetadataFlags::Imported);

          if symbol.name.lexeme == function.name.lexeme && function.metadata.is(HIRMetadataFlags::Exported) {
            block_stack.insert(
              symbol.name.lexeme.clone(),
              SymbolInfo::new(
                symbol.name.clone(),
                function.return_type.clone(),
                metadata.flags.clone(),
                SymbolKind::Function,
                None,
              ),
            );

            current_ir.push(HIRInstruction::Function(HIRFunction::new(
              symbol.name.clone(),
              function.parameters.clone(),
              function.return_type.clone(),
              None,
              HIRMetadata::new(metadata.flags.clone(), None),
              function.generic_parameters.clone(),
            )));

            self.declare(symbol.name.lexeme.clone(), variable.clone());
          } else {
            block_stack.insert(
              symbol.name.lexeme.clone(),
              SymbolInfo::new(
                symbol.name.clone(),
                function.return_type.clone(),
                metadata.flags.clone(),
                SymbolKind::Function,
                None,
              ),
            );

            current_ir.push(HIRInstruction::Function(HIRFunction::new(
              symbol.name.clone(),
              function.parameters.clone(),
              function.return_type.clone(),
              None,
              HIRMetadata::new(metadata.flags.clone(), None),
              function.generic_parameters.clone(),
            )));

            self.declare(symbol.name.lexeme.clone(), variable.clone());
          }
        }
      },
      HIRInstruction::Namespace(namespace) => {
        for symbol in &statement.symbols {
          if symbol.name.lexeme == namespace.name.lexeme && !namespace.metadata.is(HIRMetadataFlags::Exported) {
            return Err(Box::new(DiagnosticMessage::ImportedNamespaceIsNotExported(symbol.name.clone())));
          }

          let mut variable = import_scope_variables
            .iter()
            .find(|v| v.get(&symbol.name.lexeme).is_some())
            .unwrap()
            .clone();
          let variable = variable.get_mut(&symbol.name.lexeme).unwrap();

          let mut metadata = namespace.metadata.clone();
          metadata.remove_flag(HIRMetadataFlags::Exported);
          metadata.push(HIRMetadataFlags::Imported);

          if symbol.alias.is_some() {
            block_stack.insert(
              symbol.alias.as_ref().unwrap().lexeme.clone(),
              SymbolInfo::new(
                symbol.alias.as_ref().unwrap().clone(),
                DataType::Null,
                metadata.flags.clone(),
                SymbolKind::Namespace,
                None,
              ),
            );

            variable.name = symbol.alias.as_ref().unwrap().clone();

            current_ir.push(HIRInstruction::Namespace(HIRNamespace::new(
              symbol.alias.as_ref().unwrap().clone(),
              namespace.members.clone(),
              metadata,
            )));

            self.declare(symbol.alias.as_ref().unwrap().lexeme.clone(), variable.clone());
          } else {
            block_stack.insert(
              symbol.name.lexeme.clone(),
              SymbolInfo::new(
                symbol.name.clone(),
                DataType::Null,
                metadata.flags.clone(),
                SymbolKind::Namespace,
                None,
              ),
            );

            current_ir.push(HIRInstruction::Namespace(HIRNamespace::new(
              symbol.name.clone(),
              namespace.members.clone(),
              metadata,
            )));

            self.declare(symbol.name.lexeme.clone(), variable.clone());
          }
        }
      },
      HIRInstruction::Record(record) => {
        for symbol in &statement.symbols {
          if symbol.name.lexeme == record.name.lexeme && !record.metadata.is(HIRMetadataFlags::Exported) {
            return Err(Box::new(DiagnosticMessage::ImportedRecordIsNotExported(symbol.name.clone())));
          }

          let mut variable = import_scope_variables
            .iter()
            .find(|v| v.get(&symbol.name.lexeme).is_some())
            .unwrap()
            .clone();

          let variable = variable.get_mut(&symbol.name.lexeme).unwrap();

          let mut metadata = record.metadata.clone();
          metadata.remove_flag(HIRMetadataFlags::Exported);
          metadata.push(HIRMetadataFlags::Imported);

          if symbol.alias.is_some() {
            block_stack.insert(
              symbol.alias.as_ref().unwrap().lexeme.clone(),
              SymbolInfo::new(
                symbol.alias.as_ref().unwrap().clone(),
                record.data_type.clone(),
                metadata.flags.clone(),
                SymbolKind::Record,
                Some(HIRInstruction::Record(record.clone())),
              ),
            );

            variable.name = symbol.alias.as_ref().unwrap().clone();

            current_ir.push(HIRInstruction::Record(HIRRecord::new(
              record.name.clone(),
              record.items.clone(),
              record.generic_parameters.clone(),
              metadata,
              record.data_type.clone(),
            )));

            self.declare(symbol.alias.as_ref().unwrap().lexeme.clone(), variable.clone());
          } else {
            block_stack.insert(
              symbol.name.lexeme.clone(),
              SymbolInfo::new(
                symbol.name.clone(),
                record.data_type.clone(),
                metadata.flags.clone(),
                SymbolKind::Record,
                Some(HIRInstruction::Record(record.clone())),
              ),
            );

            current_ir.push(HIRInstruction::Record(HIRRecord::new(
              record.name.clone(),
              record.items.clone(),
              record.generic_parameters.clone(),
              metadata,
              record.data_type.clone(),
            )));

            self.declare(symbol.name.lexeme.clone(), variable.clone());
          }
        }
      },
      HIRInstruction::Enum(enum_) => {
        for symbol in &statement.symbols {
          if symbol.name.lexeme == enum_.name.lexeme && !enum_.metadata.is(HIRMetadataFlags::Exported) {
            return Err(Box::new(DiagnosticMessage::ImportedEnumIsNotExported(symbol.name.clone())));
          }

          let mut variable = import_scope_variables
            .iter()
            .find(|v| v.get(&symbol.name.lexeme).is_some())
            .unwrap()
            .clone();
          let variable = variable.get_mut(&symbol.name.lexeme).unwrap();

          let mut metadata = enum_.metadata.clone();
          metadata.remove_flag(HIRMetadataFlags::Exported);
          metadata.push(HIRMetadataFlags::Imported);

          if symbol.alias.is_some() {
            block_stack.insert(
              symbol.alias.as_ref().unwrap().lexeme.clone(),
              SymbolInfo::new(
                symbol.alias.as_ref().unwrap().clone(),
                DataType::Null,
                metadata.flags.clone(),
                SymbolKind::Enum,
                None,
              ),
            );

            variable.name = symbol.alias.as_ref().unwrap().clone();

            current_ir.push(HIRInstruction::Enum(HIREnum::new(
              Box::new(symbol.alias.as_ref().unwrap().clone()),
              enum_.members.clone(),
              metadata,
              enum_.generic.clone(),
              enum_.data_type.clone(),
            )));

            self.declare(symbol.alias.as_ref().unwrap().lexeme.clone(), variable.clone());
          } else {
            block_stack.insert(
              symbol.name.lexeme.clone(),
              SymbolInfo::new(
                symbol.name.clone(),
                DataType::Null,
                metadata.flags.clone(),
                SymbolKind::Enum,
                None,
              ),
            );

            current_ir.push(HIRInstruction::Enum(HIREnum::new(
              Box::new(symbol.name.clone()),
              enum_.members.clone(),
              metadata,
              enum_.generic.clone(),
              enum_.data_type.clone(),
            )));

            self.declare(symbol.name.lexeme.clone(), variable.clone());
          }
        }
      },
      HIRInstruction::Type(type_) => {
        for symbol in &statement.symbols {
          if symbol.name.lexeme == type_.name.lexeme && !type_.metadata.is(HIRMetadataFlags::Exported) {
            return Err(Box::new(DiagnosticMessage::ImportedTypeIsNotExported(symbol.name.clone())));
          }

          let mut variable = import_scope_variables
            .iter()
            .find(|v| v.get(&symbol.name.lexeme).is_some())
            .unwrap()
            .clone();
          let variable = variable.get_mut(&symbol.name.lexeme).unwrap();

          let mut metadata = type_.metadata.clone();
          metadata.remove_flag(HIRMetadataFlags::Exported);
          metadata.push(HIRMetadataFlags::Imported);

          if symbol.alias.is_some() {
            block_stack.insert(
              symbol.alias.as_ref().unwrap().lexeme.clone(),
              SymbolInfo::new(
                symbol.alias.as_ref().unwrap().clone(),
                DataType::Null,
                metadata.flags.clone(),
                SymbolKind::TypeAlias,
                None,
              ),
            );

            variable.name = symbol.alias.as_ref().unwrap().clone();

            current_ir.push(HIRInstruction::Type(HIRType::new(
              symbol.alias.as_ref().unwrap().clone(),
              type_.value.clone(),
              HIRMetadata::new(metadata.flags.clone(), None),
              type_.generics.clone(),
            )));

            self.declare(symbol.alias.as_ref().unwrap().lexeme.clone(), variable.clone());
          } else {
            block_stack.insert(
              symbol.name.lexeme.clone(),
              SymbolInfo::new(
                symbol.name.clone(),
                DataType::Null,
                metadata.flags.clone(),
                SymbolKind::TypeAlias,
                None,
              ),
            );

            current_ir.push(HIRInstruction::Type(HIRType::new(
              symbol.name.clone(),
              type_.value.clone(),
              HIRMetadata::new(metadata.flags.clone(), None),
              type_.generics.clone(),
            )));

            self.declare(symbol.name.lexeme.clone(), variable.clone());
          }
        }
      },
      HIRInstruction::Extern(extern_) => {
        for symbol in &statement.symbols {
          if symbol.name.lexeme == extern_.name.lexeme && !extern_.metadata.is(HIRMetadataFlags::Exported) {
            return Err(Box::new(DiagnosticMessage::ImportedExternIsNotExported(symbol.name.clone())));
          }

          let mut variable = import_scope_variables
            .iter()
            .find(|v| v.get(&symbol.name.lexeme).is_some())
            .unwrap()
            .clone();
          let variable = variable.get_mut(&symbol.name.lexeme).unwrap();

          let mut metadata = extern_.metadata.clone();
          metadata.remove_flag(HIRMetadataFlags::Exported);
          metadata.push(HIRMetadataFlags::Imported);

          if symbol.alias.is_some() {
            block_stack.insert(
              symbol.alias.as_ref().unwrap().lexeme.clone(),
              SymbolInfo::new(
                symbol.alias.as_ref().unwrap().clone(),
                DataType::Null,
                metadata.flags.clone(),
                SymbolKind::Extern,
                None,
              ),
            );

            variable.name = symbol.alias.as_ref().unwrap().clone();

            current_ir.push(HIRInstruction::Extern(HIRExtern::new(
              symbol.alias.as_ref().unwrap().clone(),
              extern_.body.clone(),
              HIRMetadata::new(metadata.flags.clone(), None),
            )));

            self.declare(symbol.alias.as_ref().unwrap().lexeme.clone(), variable.clone());
          } else {
            block_stack.insert(
              symbol.name.lexeme.clone(),
              SymbolInfo::new(
                symbol.name.clone(),
                DataType::Null,
                metadata.flags.clone(),
                SymbolKind::Extern,
                None,
              ),
            );

            current_ir.push(HIRInstruction::Extern(HIRExtern::new(
              symbol.name.clone(),
              extern_.body.clone(),
              HIRMetadata::new(metadata.flags.clone(), None),
            )));

            self.declare(symbol.name.lexeme.clone(), variable.clone());
          }
        }
      },
      _ => (),
    }

    self.hir.get_mut(&self.current_file).unwrap().clone_from(&current_ir);

    Ok(())
  }

  fn resolve_module_import(
    &mut self,
    statement: &ASTImport,
    block_stack: &mut HashMap<String, SymbolInfo>,
  ) -> Result<(), Box<DiagnosticMessage>> {
    let mut diagnostics: Vec<DiagnosticMessage> = vec![];

    let path = format!("{}.{}", statement.module_path.lexeme, "ign");
    let _ = match fs::read_to_string(&path) {
      Ok(source) => {
        let mut lexer: IgnisLexer<'_> = IgnisLexer::new(&self.config, &source, &path);
        lexer.scan_tokens(false);

        diagnostics.append(&mut lexer.diagnostics);

        let mut parser: IgnisParser = IgnisParser::new(&self.config, &lexer.tokens);
        let statements = parser.parse(false);

        diagnostics.append(&mut parser.diagnostics);

        let mut analyzer = Self::new(
          self.config.clone(),
          statement.module_path.lexeme.clone(),
          statements.0,
          self.primitives_std.clone(),
          self.primitives_symbol_stack.clone(),
        );

        let _ = analyzer.process(false);

        diagnostics.append(&mut analyzer.diagnostics);

        let current_ir = analyzer.hir.get(&statement.module_path.lexeme).unwrap().clone();
        self.diagnostics.append(&mut diagnostics);

        for ir in &current_ir {
          self.define_import(statement, ir, block_stack, &analyzer.symbol_stack)?;
        }

        for ir in analyzer.hir {
          self.hir.insert(ir.0.clone(), ir.1.clone());
        }
      },
      Err(_) => return Err(Box::new(DiagnosticMessage::ModuleNotFound(statement.module_path.clone()))),
    };

    Ok(())
  }

  fn type_methods(
    &mut self,
    expression: &ASTMemberAccess,
  ) -> AnalyzerResult {
    let object = self.analyzer(&expression.object)?;
    let data_type = object.extract_data_type();

    match data_type {
      DataType::Int8
      | DataType::Int16
      | DataType::Int32
      | DataType::Int64
      | DataType::UnsignedInt8
      | DataType::UnsignedInt16
      | DataType::UnsignedInt32
      | DataType::UnsignedInt64
      | DataType::Float32
      | DataType::Float64 => self.number_methods(expression, &object),
      DataType::String => todo!(),
      DataType::Boolean => todo!(),
      DataType::Vector(_, _) => todo!(),
      DataType::Unknown => todo!(),
      _ => todo!(),
    }
  }

  fn number_methods(
    &self,
    expression: &ASTMemberAccess,
    object: &HIRInstruction,
  ) -> Result<HIRInstruction, Box<DiagnosticMessage>> {
    if !self.config.std {
      return Err(Box::new(DiagnosticMessage::STDNotLoaded(expression.member.as_ref().clone())));
    }

    let std_ir = self.primitives_std.get("std/number/mod.ign");

    if std_ir.is_none() {
      return Err(Box::new(DiagnosticMessage::STDNotLoaded(expression.member.as_ref().clone())));
    }

    let std_ir = std_ir.unwrap();

    for statement in std_ir.iter() {
      match statement {
        HIRInstruction::Record(record) => {
          if record.name.lexeme.eq("Numbers") {
            for method in &record.items {
              if let HIRInstruction::Method(_method) = method {
                if _method.name.lexeme == expression.member.as_ref().lexeme {
                  let ir = HIRInstruction::MemberAccess(HIRMemberAccess::new(
                    Box::new(object.clone()),
                    Box::new(method.clone()),
                    HIRMetadata::new(_method.metadata.flags.clone(), None),
                  ));

                  return Ok(ir);
                }
              }
            }
          } else if record.name.lexeme.eq("Floats") {
          }
        },
        _ => (),
      };
    }

    todo!()
  }
}
