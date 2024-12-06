use diagnostics::{MetaDiagnostic, MetaDiagnosticError};
use ignis_ast::{
  expressions::{match_expression::ASTMatchCase, *},
  metadata::*,
  statements::{variable::ASTVariable, method::ASTMethod, property::ASTProperty, *},
  visitor::ASTVisitor,
};
use ignis_data_type::DataType;
use ignis_token::token::Token;
use literal::ASTLiteralValue;
use namespace::ASTNamespace;
pub mod diagnostics;

pub struct IgnisMetaProcessor {
  ast: Vec<ASTStatement>,
  pub new_ast: Vec<ASTStatement>,
  pub diagnostics: Vec<MetaDiagnostic>,
  pub current_metadata: Vec<ASTMetadataFlags>,
}

type IgnisMetaResult = Result<ASTStatement, Box<MetaDiagnostic>>;

impl ASTVisitor<IgnisMetaResult> for IgnisMetaProcessor {
  fn visit_binary_expression(
    &mut self,
    expression: &binary::ASTBinary,
  ) -> IgnisMetaResult {
    let left = expression.left.accept(self)?.as_expression().clone();
    let right = expression.right.accept(self)?.as_expression().clone();

    Ok(ASTStatement::Expression(Box::new(ASTExpression::Binary(Box::new(
      binary::ASTBinary::new(
        Box::new(left),
        expression.operator.clone(),
        Box::new(right),
        expression.data_type.clone(),
      ),
    )))))
  }

  fn visit_literal_expression(
    &mut self,
    expression: &ignis_ast::expressions::literal::ASTLiteral,
  ) -> IgnisMetaResult {
    Ok(ASTStatement::Expression(Box::new(ASTExpression::Literal(Box::new(
      literal::ASTLiteral::new(expression.value.clone(), expression.token.clone()),
    )))))
  }

  fn visit_logical_expression(
    &mut self,
    expression: &ignis_ast::expressions::logical::ASTLogical,
  ) -> IgnisMetaResult {
    let left = expression.left.accept(self)?.as_expression().clone();
    let right = expression.right.accept(self)?.as_expression().clone();

    Ok(ASTStatement::Expression(Box::new(ASTExpression::Logical(Box::new(
      ignis_ast::expressions::logical::ASTLogical::new(Box::new(left), expression.operator.clone(), Box::new(right)),
    )))))
  }

  fn visit_ternary_expression(
    &mut self,
    expression: &ignis_ast::expressions::ternary::ASTTernary,
  ) -> IgnisMetaResult {
    let condition = expression.condition.accept(self)?.as_expression().clone();
    let then_branch = expression.then_branch.accept(self)?.as_expression().clone();
    let else_branch = expression.else_branch.accept(self)?.as_expression().clone();

    Ok(ASTStatement::Expression(Box::new(ASTExpression::Ternary(Box::new(
      ternary::ASTTernary::new(
        Box::new(condition),
        Box::new(then_branch),
        Box::new(else_branch),
        expression.token.clone(),
        expression.data_type.clone(),
      ),
    )))))
  }

  fn visit_cast_expression(
    &mut self,
    expression: &ignis_ast::expressions::cast::ASTCast,
  ) -> IgnisMetaResult {
    let operand = expression.operand.accept(self)?.as_expression().clone();

    Ok(ASTStatement::Expression(Box::new(ASTExpression::Cast(Box::new(
      cast::ASTCast::new(expression.token.clone(), expression.target_type.clone(), Box::new(operand)),
    )))))
  }

  fn visit_unary_expression(
    &mut self,
    expression: &ignis_ast::expressions::unary::ASTUnary,
  ) -> IgnisMetaResult {
    let right = expression.right.accept(self)?.as_expression().clone();

    Ok(ASTStatement::Expression(Box::new(ASTExpression::Unary(Box::new(
      unary::ASTUnary::new(
        expression.operator.clone(),
        Box::new(right),
        expression.data_type.clone(),
        expression.is_prefix,
      ),
    )))))
  }

  fn visit_variable_expression(
    &mut self,
    expression: &ignis_ast::expressions::variable::ASTVariableExpression,
  ) -> IgnisMetaResult {
    Ok(ASTStatement::Expression(Box::new(ASTExpression::Variable(Box::new(
      ignis_ast::expressions::variable::ASTVariableExpression::new(
        expression.name.clone(),
        expression.data_type.clone(),
        expression.metadata.clone(),
      ),
    )))))
  }

  fn visit_vector_access_expression(
    &mut self,
    expression: &ignis_ast::expressions::vector_access::ASTVectorAccess,
  ) -> IgnisMetaResult {
    let variable = expression.variable.accept(self)?.as_expression().clone();
    let index = expression.index.accept(self)?.as_expression().clone();

    Ok(ASTStatement::Expression(Box::new(ASTExpression::VectorAccess(Box::new(
      ignis_ast::expressions::vector_access::ASTVectorAccess::new(
        expression.name.clone(),
        Box::new(variable),
        Box::new(index),
      ),
    )))))
  }

  fn visit_vector_expression(
    &mut self,
    expression: &ignis_ast::expressions::vector::ASTVector,
  ) -> IgnisMetaResult {
    let mut elements: Vec<ASTExpression> = vec![];
    for element in &expression.elements {
      elements.push(element.accept(self)?.as_expression().clone());
    }

    Ok(ASTStatement::Expression(Box::new(ASTExpression::Vector(Box::new(
      ignis_ast::expressions::vector::ASTVector::new(expression.token.clone(), elements, expression.data_type.clone()),
    )))))
  }

  fn visit_grouping_expression(
    &mut self,
    expression: &ignis_ast::expressions::grouping::ASTGrouping,
  ) -> IgnisMetaResult {
    let group = expression.expression.accept(self)?.as_expression().clone();

    Ok(ASTStatement::Expression(Box::new(ASTExpression::Grouping(Box::new(
      ignis_ast::expressions::grouping::ASTGrouping::new(Box::new(group)),
    )))))
  }

  fn visit_member_access_expression(
    &mut self,
    expression: &ignis_ast::expressions::member_access::ASTMemberAccess,
  ) -> IgnisMetaResult {
    let object = expression.object.accept(self)?.as_expression().clone();

    Ok(ASTStatement::Expression(Box::new(ASTExpression::MemberAccess(Box::new(
      ignis_ast::expressions::member_access::ASTMemberAccess::new(
        Box::new(object),
        expression.member.clone(),
        expression.metadata.clone(),
      ),
    )))))
  }

  fn visit_assignment_expression(
    &mut self,
    expression: &ignis_ast::expressions::assign::ASTAssignment,
  ) -> IgnisMetaResult {
    let left = expression.left.accept(self)?.as_expression().clone();
    let right = expression.right.accept(self)?.as_expression().clone();

    Ok(ASTStatement::Expression(Box::new(ASTExpression::Assigment(Box::new(
      ignis_ast::expressions::assign::ASTAssignment::new(Box::new(left), expression.operator.clone(), Box::new(right)),
    )))))
  }

  fn visit_call_expression(
    &mut self,
    expression: &ignis_ast::expressions::call::ASTCall,
  ) -> IgnisMetaResult {
    let mut arguments: Vec<ASTExpression> = vec![];
    for argument in &expression.arguments {
      arguments.push(argument.accept(self)?.as_expression().clone());
    }

    Ok(ASTStatement::Expression(Box::new(ASTExpression::Call(Box::new(
      ignis_ast::expressions::call::ASTCall::new(expression.callee.clone(), arguments),
    )))))
  }

  fn visit_match_expression(
    &mut self,
    expression: &ignis_ast::expressions::match_expression::ASTMatchExpression,
  ) -> IgnisMetaResult {
    let mut cases: Vec<ASTMatchCase> = vec![];
    for case in &expression.cases {
      let mut patterns: Vec<ASTExpression> = vec![];
      for pattern in &case.pattern {
        patterns.push(pattern.accept(self)?.as_expression().clone());
      }

      let when = if let Some(w) = &case.when {
        Some(w.accept(self)?.as_expression().clone())
      } else {
        None
      };

      let block = case.block.accept(self)?;

      cases.push(ASTMatchCase::new(patterns, when, Box::new(block)));
    }

    Ok(ASTStatement::Expression(Box::new(ASTExpression::Match(Box::new(
      ignis_ast::expressions::match_expression::ASTMatchExpression::new(
        expression.expression.clone(),
        expression.cases.clone(),
      ),
    )))))
  }

  fn visit_lambda_expression(
    &mut self,
    expression: &ignis_ast::expressions::lambda::ASTLambda,
  ) -> IgnisMetaResult {
    let mut parameters: Vec<ASTVariable> = vec![];

    for parameter in &expression.parameters {
      let var = ASTStatement::Variable(Box::new(parameter.clone())).accept(self)?;
      if let ASTStatement::Variable(var) = var {
        parameters.push(var.as_ref().clone());
      }
    }

    let body = expression.body.accept(self)?;

    Ok(ASTStatement::Expression(Box::new(ASTExpression::Lambda(Box::new(
      ignis_ast::expressions::lambda::ASTLambda::new(
        parameters,
        Box::new(body),
        expression.return_type.clone(),
        expression.lambda_type.clone(),
      ),
    )))))
  }

  fn visit_object_expression(
    &mut self,
    expression: &ignis_ast::expressions::object_literal::ASTObject,
  ) -> IgnisMetaResult {
    let mut properties: Vec<ASTProperty> = vec![];
    for property in &expression.properties {
      let prop = ASTStatement::Property(Box::new(property.clone())).accept(self)?;
      if let ASTStatement::Property(prop) = prop {
        properties.push(prop.as_ref().clone());
      }
    }

    let mut methods: Vec<ASTMethod> = vec![];

    for method in &expression.methods {
      let meth = ASTStatement::Method(Box::new(method.clone())).accept(self)?;
      if let ASTStatement::Method(meth) = meth {
        methods.push(meth.as_ref().clone());
      }
    }

    Ok(ASTStatement::Expression(Box::new(ASTExpression::Object(Box::new(
      ignis_ast::expressions::object_literal::ASTObject::new(
        expression.token.clone(),
        properties,
        methods,
        expression.data_type.clone(),
      ),
    )))))
  }

  fn visit_this_expression(
    &mut self,
    expression: &ignis_ast::expressions::this::ASTThis,
  ) -> IgnisMetaResult {
    Ok(ASTStatement::Expression(Box::new(ASTExpression::This(Box::new(
      ignis_ast::expressions::this::ASTThis::new(expression.token.clone()),
    )))))
  }

  fn visit_meta_expression(
    &mut self,
    expression: &ignis_ast::expressions::meta::ASTMeta,
  ) -> IgnisMetaResult {
    match expression.expression.as_ref() {
      ASTExpression::Variable(variable) => {
        let meta = self.process_variable_meta_expression(variable)?;
        self.current_metadata.push(meta);
      },
      ASTExpression::Call(call) => {
        let meta = self.process_call_meta_expression(call)?;
        self.current_metadata.push(meta);
      },
      _ => unreachable!(),
    };

    Ok(ASTStatement::Expression(Box::new(ASTExpression::Meta(Box::new(
      expression.clone(),
    )))))
  }

  fn visit_meta_entity_expression(
    &mut self,
    expression: &ignis_ast::expressions::meta::ASTMetaEntity,
  ) -> IgnisMetaResult {
    for meta in expression.metas.iter() {
      ASTExpression::Meta(Box::new(meta.clone())).accept(self)?;
    }

    if let Some(entity) = &expression.entity {
      return self.process_meta_entity(entity);
    }

    Ok(ASTStatement::Expression(Box::new(ASTExpression::MetaEntity(Box::new(
      expression.clone(),
    )))))
  }

  fn visit_comment_statement(
    &mut self,
    comment: &ignis_ast::statements::comment::ASTComment,
  ) -> IgnisMetaResult {
    Ok(ASTStatement::Comment(Box::new(comment.clone())))
  }

  fn visit_expression_statement(
    &mut self,
    expression: &ignis_ast::expressions::ASTExpression,
  ) -> IgnisMetaResult {
    expression.accept(self)
  }

  fn visit_constant_statement(
    &mut self,
    expression: &ignis_ast::statements::constant::ASTConstant,
  ) -> IgnisMetaResult {
    let value = expression.value.accept(self)?.as_expression().clone();

    Ok(ASTStatement::Constant(Box::new(
      ignis_ast::statements::constant::ASTConstant::new(
        expression.name.clone(),
        Box::new(value),
        expression.type_annotation.clone(),
        expression.metadata.clone(),
      ),
    )))
  }

  fn visit_function_statement(
    &mut self,
    expression: &ignis_ast::statements::function::ASTFunction,
  ) -> IgnisMetaResult {
    let mut parameters: Vec<ASTVariable> = vec![];

    for parameter in &expression.parameters {
      let var = ASTStatement::Variable(Box::new(parameter.clone())).accept(self)?;

      if let ASTStatement::Variable(var) = var {
        parameters.push(var.as_ref().clone());
      }
    }

    let mut body: Vec<ASTStatement> = vec![];

    for statement in &expression.body {
      body.push(statement.accept(self)?);
    }

    Ok(ASTStatement::Function(Box::new(
      ignis_ast::statements::function::ASTFunction::new(
        expression.name.clone(),
        parameters,
        body,
        expression.return_type.clone(),
        expression.metadata.clone(),
      ),
    )))
  }

  fn visit_variable_statement(
    &mut self,
    expression: &ignis_ast::statements::variable::ASTVariable,
  ) -> IgnisMetaResult {
    let initializer = if let Some(i) = &expression.initializer {
      Some(Box::new(i.accept(self)?.as_expression().clone()))
    } else {
      None
    };

    Ok(ASTStatement::Variable(Box::new(
      ignis_ast::statements::variable::ASTVariable::new(
        expression.name.clone(),
        initializer,
        expression.type_annotation.clone(),
        expression.metadata.clone(),
      ),
    )))
  }

  fn visit_block_statement(
    &mut self,
    expression: &ignis_ast::statements::block::ASTBlock,
  ) -> IgnisMetaResult {
    let mut statements: Vec<ASTStatement> = vec![];

    for statement in &expression.statements {
      statements.push(statement.accept(self)?);
    }

    Ok(ASTStatement::Block(Box::new(ignis_ast::statements::block::ASTBlock::new(
      statements,
    ))))
  }

  fn visit_if_statement(
    &mut self,
    expression: &ignis_ast::statements::if_statement::ASTIf,
  ) -> IgnisMetaResult {
    let condition = expression.condition.accept(self)?.as_expression().clone();
    let then_branch = expression.then_branch.accept(self)?;
    let else_branch = if let Some(e) = &expression.else_branch {
      Some(Box::new(e.accept(self)?))
    } else {
      None
    };

    Ok(ASTStatement::If(Box::new(ignis_ast::statements::if_statement::ASTIf::new(
      Box::new(condition),
      Box::new(then_branch),
      else_branch,
    ))))
  }

  fn visit_while_statement(
    &mut self,
    expression: &ignis_ast::statements::while_statement::ASTWhile,
  ) -> IgnisMetaResult {
    let condition = expression.condition.accept(self)?.as_expression().clone();
    let body = expression.body.accept(self)?;

    Ok(ASTStatement::While(Box::new(
      ignis_ast::statements::while_statement::ASTWhile::new(Box::new(condition), Box::new(body)),
    )))
  }

  fn visit_for_statement(
    &mut self,
    expression: &ignis_ast::statements::for_statement::ASTFor,
  ) -> IgnisMetaResult {
    let variable =
      if let ASTStatement::Variable(var) = ASTStatement::Variable(expression.variable.clone()).accept(self)? {
        var.as_ref().clone()
      } else {
        unreachable!()
      };

    let condition = expression.condition.accept(self)?.as_expression().clone();
    let increment = expression.increment.accept(self)?.as_expression().clone();
    let body = expression.body.accept(self)?;

    Ok(ASTStatement::For(Box::new(ignis_ast::statements::for_statement::ASTFor::new(
      Box::new(variable),
      Box::new(condition),
      Box::new(increment),
      Box::new(body),
    ))))
  }

  fn visit_for_of_statement(
    &mut self,
    expression: &ignis_ast::statements::for_of_statement::ASTForOf,
  ) -> IgnisMetaResult {
    let variable =
      if let ASTStatement::Variable(var) = ASTStatement::Variable(expression.variable.clone()).accept(self)? {
        var.as_ref().clone()
      } else {
        unreachable!()
      };

    let iterable = expression.iterable.accept(self)?.as_expression().clone();
    let body = expression.body.accept(self)?;

    Ok(ASTStatement::ForOf(Box::new(
      ignis_ast::statements::for_of_statement::ASTForOf::new(
        Box::new(variable),
        Box::new(iterable),
        Box::new(body),
        expression.token.clone(),
      ),
    )))
  }

  fn visit_break_statement(
    &mut self,
    token: &Token,
  ) -> IgnisMetaResult {
    Ok(ASTStatement::Break { token: token.clone() })
  }

  fn visit_continue_statement(
    &mut self,
    token: &Token,
  ) -> IgnisMetaResult {
    Ok(ASTStatement::Continue { token: token.clone() })
  }

  fn visit_return_statement(
    &mut self,
    return_: &ignis_ast::statements::return_::ASTReturn,
  ) -> IgnisMetaResult {
    let value = if let Some(v) = &return_.value {
      Some(Box::new(v.accept(self)?.as_expression().clone()))
    } else {
      None
    };

    Ok(ASTStatement::Return(Box::new(ignis_ast::statements::return_::ASTReturn::new(
      value,
      return_.token.clone(),
    ))))
  }

  fn visit_import_statement(
    &mut self,
    import: &ignis_ast::statements::import::ASTImport,
  ) -> IgnisMetaResult {
    Ok(ASTStatement::Import(Box::new(import.clone())))
  }

  fn visit_record_statement(
    &mut self,
    record: &ignis_ast::statements::record::ASTRecord,
  ) -> IgnisMetaResult {
    let mut items: Vec<ASTStatement> = vec![];
    for statement in &record.items {
      items.push(statement.accept(self)?);
    }

    Ok(ASTStatement::Record(Box::new(ignis_ast::statements::record::ASTRecord::new(
      record.name.clone(),
      items,
      record.metadata.clone(),
    ))))
  }

  fn visit_method_statement(
    &mut self,
    method: &ignis_ast::statements::method::ASTMethod,
  ) -> IgnisMetaResult {
    let mut parameters: Vec<ASTVariable> = vec![];
    for parameter in &method.parameters {
      let var = ASTStatement::Variable(Box::new(parameter.clone())).accept(self)?;
      if let ASTStatement::Variable(var) = var {
        parameters.push(var.as_ref().clone());
      }
    }

    let body = if let ASTStatement::Block(block) = ASTStatement::Block(Box::new(method.body.clone())).accept(self)? {
      block.as_ref().clone()
    } else {
      unreachable!()
    };

    Ok(ASTStatement::Method(Box::new(ignis_ast::statements::method::ASTMethod::new(
      method.name.clone(),
      parameters,
      body,
      method.return_type.clone(),
      method.metadata.clone(),
      method.class_name.clone(),
    ))))
  }

  fn visit_property_statement(
    &mut self,
    property: &ignis_ast::statements::property::ASTProperty,
  ) -> IgnisMetaResult {
    let initializer = if let Some(i) = &property.initializer {
      Some(Box::new(i.accept(self)?.as_expression().clone()))
    } else {
      None
    };

    Ok(ASTStatement::Property(Box::new(
      ignis_ast::statements::property::ASTProperty::new(
        property.name.clone(),
        initializer,
        property.type_annotation.clone(),
        property.metadata.clone(),
      ),
    )))
  }

  fn visit_extern_statement(
    &mut self,
    extern_: &ignis_ast::statements::r#extern::ASTExtern,
  ) -> IgnisMetaResult {
    let mut statements: Vec<ASTStatement> = vec![];
    for statement in &extern_.body {
      statements.push(statement.accept(self)?);
    }

    Ok(ASTStatement::Extern(Box::new(ignis_ast::statements::r#extern::ASTExtern::new(
      extern_.name.clone(),
      statements,
      extern_.metadata.clone(),
    ))))
  }

  fn visit_include_statement(
    &mut self,
    include: &Token,
  ) -> IgnisMetaResult {
    return Ok(ASTStatement::Include(Box::new(include.clone())));
  }

  fn visit_source_statement(
    &mut self,
    source: &Token,
  ) -> IgnisMetaResult {
    return Ok(ASTStatement::Source(Box::new(source.clone())));
  }

  fn visit_namespace_statement(
    &mut self,
    namespace: &ASTNamespace,
  ) -> IgnisMetaResult {
    let mut statements: Vec<ASTStatement> = vec![];

    for statement in &namespace.members {
      statements.push(statement.accept(self)?);
    }

    Ok(ASTStatement::Namespace(Box::new(ASTNamespace::new(
      namespace.name.clone(),
      statements,
      namespace.metadata.clone(),
    ))))
  }

  fn visit_spread_expression(
    &mut self,
    expression: &ignis_ast::expressions::spread::ASTSpread,
  ) -> IgnisMetaResult {
    Ok(ASTStatement::Expression(Box::new(ASTExpression::Spread(Box::new(
      ignis_ast::expressions::spread::ASTSpread::new(
        Box::new(expression.expression.accept(self)?.as_expression().clone()),
        expression.token.clone(),
      ),
    )))))
  }
}

impl IgnisMetaProcessor {
  pub fn new(ast: Vec<ASTStatement>) -> Self {
    Self {
      ast,
      new_ast: vec![],
      diagnostics: vec![],
      current_metadata: vec![],
    }
  }

  pub fn process(&mut self) {
    let ast = self.ast.clone();
    for statement in &ast {
      match statement.accept(self) {
        Ok(statement) => self.new_ast.push(statement),
        Err(error) => self.diagnostics.push(*error),
      };
    }
  }

  fn process_variable_meta_expression(
    &mut self,
    expression: &ignis_ast::expressions::variable::ASTVariableExpression,
  ) -> Result<ASTMetadataFlags, Box<MetaDiagnostic>> {
    match expression.name.lexeme.as_str() {
      "MutOnly" => Ok(ASTMetadataFlags::Meta(IgnisCompilerMeta::MutOnly)),
      "Ignore" => Ok(ASTMetadataFlags::Meta(IgnisCompilerMeta::Ignore)),
      "NotTranspile" => Ok(ASTMetadataFlags::Meta(IgnisCompilerMeta::NotTranspile)),
      "DisableLint" => Ok(ASTMetadataFlags::Meta(IgnisCompilerMeta::DisableLint)),
      "EnableLint" => Ok(ASTMetadataFlags::Meta(IgnisCompilerMeta::EnableLint)),
      "MainFunction" => Ok(ASTMetadataFlags::Meta(IgnisCompilerMeta::MainFunction)),
      "Copy" => Ok(ASTMetadataFlags::Meta(IgnisCompilerMeta::Copy)),
      "Clone" => Ok(ASTMetadataFlags::Meta(IgnisCompilerMeta::Clone)),
      _ => Err(Box::new(MetaDiagnostic::new(MetaDiagnosticError::UndefinedMeta(
        expression.name.clone(),
      )))),
    }
  }

  fn process_call_meta_expression(
    &mut self,
    call: &ignis_ast::expressions::call::ASTCall,
  ) -> Result<ASTMetadataFlags, Box<MetaDiagnostic>> {
    match call.callee.as_ref() {
      ASTExpression::Variable(variable) => match variable.name.lexeme.as_str() {
        "ToDo" => self.process_todo(&variable.name, &call.arguments),
        "Feature" => self.process_feature(&variable.name, &call.arguments),
        "Deprecated" => self.process_deprecated(&variable.name, &call.arguments),
        "PlatformSpecific" => self.process_platform_specific(&variable.name, &call.arguments),
        "Experimental" => self.process_experimental(&variable.name, &call.arguments),
        "Internal" => self.process_internal(&variable.name, &call.arguments),
        "Global" => self.process_global(&variable.name, &call.arguments),
        "FFILink" => self.process_ffi_link(&variable.name, &call.arguments),
        _ => {
          let token = variable.name.clone();

          return Err(Box::new(MetaDiagnostic::new(MetaDiagnosticError::UndefinedMeta(token))));
        },
      },
      _ => Err(Box::new(MetaDiagnostic::new(MetaDiagnosticError::UndefinedMeta(Into::into(
        &call.callee.as_ref().clone(),
      ))))),
    }
  }

  fn process_meta_entity(
    &mut self,
    entity: &ASTStatement,
  ) -> IgnisMetaResult {
    let mut statement = entity.accept(self)?;

    match &mut statement {
      ASTStatement::Function(function) => {
        let metadata = self.current_metadata.clone();
        function.metadata.push_all(metadata);
      },
      _ => {
        let kind = entity.into();

        return Err(Box::new(MetaDiagnostic::new(MetaDiagnosticError::InvalidMetaEntity(
          kind,
          entity.into(),
        ))));
      },
    };

    self.current_metadata.clear();

    return Ok(statement);
  }

  fn process_todo(
    &self,
    callee: &Token,
    arguments: &Vec<ASTExpression>,
  ) -> Result<ASTMetadataFlags, Box<MetaDiagnostic>> {
    if arguments.len() > 1 {
      return Err(Box::new(MetaDiagnostic::new(MetaDiagnosticError::InvalidNumberOfArguments(
        1,
        arguments.len(),
        callee.clone(),
      ))));
    }

    if arguments.len() == 0 {
      return Ok(ASTMetadataFlags::Meta(IgnisCompilerMeta::ToDo(None)));
    }

    let message = arguments[0].clone();

    if let ASTExpression::Literal(literal) = &message {
      if let ASTLiteralValue::String(message) = &literal.value {
        return Ok(ASTMetadataFlags::Meta(IgnisCompilerMeta::ToDo(Some(message.clone()))));
      }
    }

    Err(Box::new(MetaDiagnostic::new(MetaDiagnosticError::MissingArgument(
      callee.clone(),
    ))))
  }

  fn process_feature(
    &self,
    callee: &Token,
    arguments: &Vec<ASTExpression>,
  ) -> Result<ASTMetadataFlags, Box<MetaDiagnostic>> {
    if arguments.len() != 2 {
      return Err(Box::new(MetaDiagnostic::new(MetaDiagnosticError::InvalidNumberOfArguments(
        2,
        arguments.len(),
        callee.clone(),
      ))));
    }

    let message = arguments[1].clone();
    let name = arguments[0].clone();

    if let ASTExpression::Literal(literal) = &message {
      if let ASTLiteralValue::String(message) = &literal.value {
        if let ASTExpression::Literal(literal) = &name {
          if let ASTLiteralValue::String(name) = &literal.value {
            return Ok(ASTMetadataFlags::Meta(IgnisCompilerMeta::Feature(
              name.clone(),
              message.clone(),
            )));
          }
        }
      }
    }

    unreachable!()
  }

  fn process_deprecated(
    &self,
    callee: &Token,
    arguments: &Vec<ASTExpression>,
  ) -> Result<ASTMetadataFlags, Box<MetaDiagnostic>> {
    if arguments.len() != 2 {
      return Err(Box::new(MetaDiagnostic::new(MetaDiagnosticError::InvalidNumberOfArguments(
        2,
        arguments.len(),
        callee.clone(),
      ))));
    }

    let message = arguments[1].clone();
    let version = arguments[0].clone();

    if let ASTExpression::Literal(literal) = &message {
      if let ASTLiteralValue::String(message) = &literal.value {
        if let ASTExpression::Literal(literal) = &version {
          if let ASTLiteralValue::String(version) = &literal.value {
            return Ok(ASTMetadataFlags::Meta(IgnisCompilerMeta::Deprecated(
              version.clone(),
              message.clone(),
            )));
          }
        }
      }
    }

    unreachable!()
  }

  fn process_platform_specific(
    &self,
    callee: &Token,
    arguments: &Vec<ASTExpression>,
  ) -> Result<ASTMetadataFlags, Box<MetaDiagnostic>> {
    if arguments.len() != 2 {
      return Err(Box::new(MetaDiagnostic::new(MetaDiagnosticError::InvalidNumberOfArguments(
        2,
        arguments.len(),
        callee.clone(),
      ))));
    }

    let message = arguments[1].clone();
    let platform = arguments[0].clone();

    if let ASTExpression::Literal(literal) = &message {
      if let ASTLiteralValue::String(message) = &literal.value {
        if let ASTExpression::Literal(literal) = &platform {
          if let ASTLiteralValue::String(platform) = &literal.value {
            return Ok(ASTMetadataFlags::Meta(IgnisCompilerMeta::PlatformSpecific(
              platform.clone(),
              message.clone(),
            )));
          }
        }
      }
    }

    unreachable!()
  }

  fn process_experimental(
    &self,
    callee: &Token,
    arguments: &Vec<ASTExpression>,
  ) -> Result<ASTMetadataFlags, Box<MetaDiagnostic>> {
    if arguments.len() != 2 {
      return Err(Box::new(MetaDiagnostic::new(MetaDiagnosticError::InvalidNumberOfArguments(
        2,
        arguments.len(),
        callee.clone(),
      ))));
    }

    let message = arguments[1].clone();
    let version = arguments[0].clone();

    if let ASTExpression::Literal(literal) = &message {
      if let ASTLiteralValue::String(message) = &literal.value {
        if let ASTExpression::Literal(literal) = &version {
          if let ASTLiteralValue::String(version) = &literal.value {
            return Ok(ASTMetadataFlags::Meta(IgnisCompilerMeta::Experimental(
              version.clone(),
              message.clone(),
            )));
          }
        }
      }
    }

    unreachable!()
  }

  fn process_internal(
    &self,
    callee: &Token,
    arguments: &Vec<ASTExpression>,
  ) -> Result<ASTMetadataFlags, Box<MetaDiagnostic>> {
    if arguments.len() != 1 {
      return Err(Box::new(MetaDiagnostic::new(MetaDiagnosticError::InvalidNumberOfArguments(
        1,
        arguments.len(),
        callee.clone(),
      ))));
    }

    let message = arguments[0].clone();

    if let ASTExpression::Literal(literal) = &message {
      if let ASTLiteralValue::String(message) = &literal.value {
        return Ok(ASTMetadataFlags::Meta(IgnisCompilerMeta::Internal(message.clone())));
      }
    }

    Err(Box::new(MetaDiagnostic::new(MetaDiagnosticError::MissingArgument(
      callee.clone(),
    ))))
  }

  fn process_global(
    &self,
    callee: &Token,
    arguments: &Vec<ASTExpression>,
  ) -> Result<ASTMetadataFlags, Box<MetaDiagnostic>> {
    if arguments.len() != 1 {
      return Err(Box::new(MetaDiagnostic::new(MetaDiagnosticError::InvalidNumberOfArguments(
        1,
        arguments.len(),
        callee.clone(),
      ))));
    }

    let message = arguments[0].clone();

    if let ASTExpression::Literal(literal) = &message {
      if let ASTLiteralValue::String(message) = &literal.value {
        return Ok(ASTMetadataFlags::Meta(IgnisCompilerMeta::Global(message.clone())));
      }
    }

    Err(Box::new(MetaDiagnostic::new(MetaDiagnosticError::MissingArgument(
      callee.clone(),
    ))))
  }

  fn process_ffi_link(
    &self,
    callee: &Token,
    arguments: &Vec<ASTExpression>,
  ) -> Result<ASTMetadataFlags, Box<MetaDiagnostic>> {
    if arguments.len() != 1 {
      return Err(Box::new(MetaDiagnostic::new(MetaDiagnosticError::InvalidNumberOfArguments(
        1,
        arguments.len(),
        callee.clone(),
      ))));
    }

    for argument in arguments {
      if let ASTExpression::Literal(object) = argument {
        if let ASTLiteralValue::String(object) = &object.value {
          return Ok(ASTMetadataFlags::Meta(IgnisCompilerMeta::FFILink(object.clone())));
        }

        return Err(Box::new(MetaDiagnostic::new(MetaDiagnosticError::InvalidArgumentType(
          DataType::String,
          object.token.clone(),
        ))));
      }
    }

    return Err(Box::new(MetaDiagnostic::new(MetaDiagnosticError::MissingArgument(
      callee.clone(),
    ))));
  }
}
