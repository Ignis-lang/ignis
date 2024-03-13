pub mod parser_diagnostic;

use diagnostic_report::DiagnosticReport;
use parser_diagnostic::{ParserDiagnosticError, ParserDiagnostic};
use ast::{
  expression::{
    array::Array, array_access::ArrayAccess, get::Get, method_call::MethodCall, new::NewExpression,
    set::Set, this::This,
  },
  statement::{
    break_statement::BreakStatement,
    class::{Class, ClassMetadata},
    continue_statement::Continue,
    enum_statement::{Enum, EnumMember, EnumMemberValue},
    for_of::ForOf,
    for_statement::For,
    function::FunctionDecorator,
    import::{Import, ImportSource, ImportSymbol},
    interface_statement::InterfaceStatement,
    method::{MethodMetadata, MethodStatement},
    property::PropertyStatement,
    variable::VariableMetadata,
  },
};
use enums::{data_type::DataType, token_type::TokenType};
use {
  token::token::Token,
  ast::expression::{
    Expression, binary::Binary, unary::Unary, literal::Literal, grouping::Grouping,
    logical::Logical, assign::Assign, variable::VariableExpression, ternary, call::Call,
  },
  enums::literal_value::LiteralValue,
  ast::statement::{
    Statement, variable::Variable, expression::ExpressionStatement, if_statement::IfStatement,
    block::Block, while_statement::WhileStatement, function::FunctionStatement,
    return_statement::Return,
  },
};

type ParserResult<T> = Result<T, Box<ParserDiagnostic>>;

#[derive(Debug, Clone, PartialEq)]
enum ParserContext {
  Function,
  Class,
  ArrayAccess,
}

pub struct Parser {
  pub tokens: Vec<Token>,
  current: usize,
  pub diagnostics: Vec<ParserDiagnostic>,
  context: Vec<ParserContext>,
  class_declarations: Vec<String>,
  import_declarations: Vec<String>,
}

impl Parser {
  pub fn new(tokens: Vec<Token>) -> Self {
    Self {
      tokens,
      current: 0,
      diagnostics: Vec::new(),
      context: Vec::new(),
      class_declarations: Vec::new(),
      import_declarations: Vec::new(),
    }
  }

  fn report_error(&mut self, error: ParserDiagnostic) {
    self.diagnostics.push(error);
  }

  pub fn parse(&mut self) -> (Vec<Statement>, Vec<DiagnosticReport>) {
    let mut statements: Vec<Statement> = vec![];
    while !self.is_at_end() {
      match self.declaration() {
        Ok(result) => statements.push(result),
        Err(error) => {
          self.report_error(*error);
        }
      };
    }

    (
      statements,
      self
        .diagnostics
        .iter()
        .map(|e| e.report_diagnostic())
        .collect::<Vec<DiagnosticReport>>(),
    )
  }

  fn expression(&mut self) -> ParserResult<Expression> {
    self.assignment()
  }

  // equelity -> comparison (("!=" | "==") comparison)*;
  fn equality(&mut self) -> ParserResult<Expression> {
    let mut expression = self.comparison()?;

    while self.match_token(&[TokenType::BangEqual, TokenType::EqualEqual]) {
      let operator: Token = self.previous();
      let right = self.comparison()?;

      let data_type: DataType = DataType::Boolean;

      expression = Expression::Binary(Binary::new(
        Box::new(expression),
        operator,
        Box::new(right),
        data_type,
      ));
    }

    Ok(expression)
  }

  // comparison -> term ((">" | ">=" | "<" | "<=") term)*;
  fn comparison(&mut self) -> ParserResult<Expression> {
    let mut expression = self.term()?;

    while self.match_token(&[
      TokenType::Greater,
      TokenType::GreaterEqual,
      TokenType::Less,
      TokenType::LessEqual,
    ]) {
      let operator: Token = self.previous();
      let right = self.term()?;

      let data_type: DataType = DataType::Boolean;

      expression = Expression::Binary(Binary::new(
        Box::new(expression),
        operator,
        Box::new(right),
        data_type,
      ));
    }

    Ok(expression)
  }

  // term -> factor (("-" | "+") factor)*;
  fn term(&mut self) -> ParserResult<Expression> {
    let mut expression = self.factor()?;

    while self.match_token(&[TokenType::Minus, TokenType::Plus]) {
      let operator: Token = self.previous();
      let right = self.factor()?;

      let left_type = self.get_expression_type(&expression);
      let right_type = self.get_expression_type(&right);
      let operator_kind = operator.kind.clone();

      expression = Expression::Binary(Binary::new(
        Box::new(expression),
        operator,
        Box::new(right),
        self.get_data_type_by_operator(Some(left_type), right_type, operator_kind),
      ));
    }

    Ok(expression)
  }

  // factor -> ("!" | "-") unary | primary;
  fn factor(&mut self) -> ParserResult<Expression> {
    let mut expression: Expression = self.unary()?;

    while self.match_token(&[TokenType::Slash, TokenType::Asterisk, TokenType::Mod]) {
      let operator: Token = self.previous();
      let right: Expression = self.unary()?;

      let left_type = self.get_expression_type(&expression);
      let right_type = self.get_expression_type(&right);

      let operator_kind = operator.kind.clone();

      expression = Expression::Binary(Binary::new(
        Box::new(expression),
        operator,
        Box::new(right),
        self.get_data_type_by_operator(Some(left_type), right_type, operator_kind),
      ));
    }

    Ok(expression)
  }

  // unary -> ("!" | "-" | "++" | "++") unary | call ("++" | "--")?;
  fn unary(&mut self) -> ParserResult<Expression> {
    if self.match_token(&[
      TokenType::Bang,
      TokenType::Minus,
      TokenType::Increment,
      TokenType::Decrement,
    ]) {
      let operator = self.previous();
      let right: Expression = self.unary()?;

      let right_type = self.get_expression_type(&right);
      let operator_kind = operator.kind.clone();

      return Ok(Expression::Unary(Unary::new(
        operator,
        Box::new(right),
        self.get_data_type_by_operator(None, right_type, operator_kind),
        true,
      )));
    }

    match self.call()? {
      Expression::Variable(v) => {
        match self.match_token(&[TokenType::Increment, TokenType::Decrement]) {
          true => {
            let operator = self.previous();

            let right = Expression::Variable(v.clone());
            let right_type = self.get_expression_type(&right);

            let operator_kind = operator.kind.clone();

            Ok(Expression::Unary(Unary::new(
              operator,
              Box::new(right),
              self.get_data_type_by_operator(None, right_type, operator_kind),
              false,
            )))
          }
          false => Ok(Expression::Variable(v)),
        }
      }
      e => Ok(e),
    }
  }

  fn call(&mut self) -> ParserResult<Expression> {
    if self.match_token(&[TokenType::New]) {
      let name = self.consume(TokenType::Identifier)?;

      self.consume(TokenType::LeftParen)?;

      let mut arguments: Vec<Expression> = Vec::new();

      if !self.check(TokenType::RightParen) {
        loop {
          if arguments.len() >= 255 {
            let token = &self.peek();
            return Err(Box::new(ParserDiagnostic::new(
              ParserDiagnosticError::InvalidNumberOfArguments(255, arguments.len(), token.clone()),
              self.find_token_line(&token.span.line),
            )));
          }

          arguments.push(self.expression()?);
          if !self.match_token(&[TokenType::Comma]) {
            break;
          }
        }
      }

      self.consume(TokenType::RightParen)?;

      return Ok(Expression::New(NewExpression::new(name, arguments)));
    }

    let token = self.peek();
    let mut expression: Expression = self.primary()?;

    loop {
      if self.match_token(&[TokenType::Dot]) {
        let name = self.consume(TokenType::Identifier)?;

        if self.match_token(&[TokenType::LeftParen]) {
          let object = expression.clone();
          expression =
            Expression::Variable(VariableExpression::new(name.clone(), DataType::Pending));

          let calle = self.finish_call(expression.clone())?;

          expression = Expression::MethodCall(MethodCall::new(
            Box::new(name),
            Box::new(calle.clone()),
            self.get_expression_type(&calle),
            Box::new(object),
          ));

          continue;
        }

        expression = Expression::Get(Get::new(
          Box::new(expression),
          Box::new(token.clone()),
          Box::new(name),
        ));

        continue;
      }

      if !self.match_token(&[TokenType::LeftParen]) {
        break;
      }

      expression = self.finish_call(expression)?;
    }

    Ok(expression)
  }

  fn array_access(&mut self, var: Expression) -> ParserResult<Expression> {
    let token = self.previous();
    self.context.push(ParserContext::ArrayAccess);
    if self.match_token(&[TokenType::LeftBrack]) {
      let index = self.expression()?;

      self.consume(TokenType::RightBrack)?;

      self.context.pop();
      Ok(Expression::ArrayAccess(ArrayAccess::new(
        Box::new(token),
        Box::new(var),
        Box::new(index),
      )))
    } else {
      Err(Box::new(ParserDiagnostic::new(
        ParserDiagnosticError::ExpectedToken(TokenType::LeftBrack, token.clone()),
        self.find_token_line(&token.span.line),
      )))
    }
  }

  fn primary(&mut self) -> ParserResult<Expression> {
    let token = self.peek();

    match token.kind {
      TokenType::True
      | TokenType::False
      | TokenType::Null
      | TokenType::Int
      | TokenType::Float
      | TokenType::String => {
        self.advance();
        Ok(Expression::Literal(Literal::new(
          LiteralValue::from_token_type(token.kind.clone(), token.span.literal.clone()),
        )))
      }
      TokenType::LeftBrack => {
        self.advance();

        let mut elements = Vec::new();
        if !self.check(TokenType::RightBrack) {
          loop {
            elements.push(self.expression()?);
            if !self.match_token(&[TokenType::Comma]) {
              break;
            }
          }
        }

        self.consume(TokenType::RightBrack)?;

        let data_type = DataType::Array(Box::new(DataType::Pending));
        Ok(Expression::Array(Array::new(token, elements, data_type)))
      }
      TokenType::LeftParen => {
        self.advance();
        let expression = self.expression()?;
        self.consume(TokenType::RightParen)?;

        Ok(Expression::Grouping(Grouping::new(Box::new(expression))))
      }
      TokenType::Identifier => {
        self.advance();
        let kind = token.kind.clone();

        let var = Expression::Variable(VariableExpression::new(
          token,
          DataType::from_token_type(kind),
        ));

        if self.check(TokenType::LeftBrack) {
          let array = self.array_access(var);
          return array;
        }

        Ok(var)
      }
      TokenType::This => {
        self.advance();

        if self.peek().kind != TokenType::Dot {
          Ok(Expression::This(This::new(token, None)))
        } else {
          self.advance();
          let access = self.expression()?;

          Ok(Expression::This(This::new(token, Some(Box::new(access)))))
        }
      }
      _ => Err(Box::new(ParserDiagnostic::new(
        ParserDiagnosticError::ExpectedExpression(token.clone()),
        self.find_token_line(&token.span.line),
      ))),
    }
  }

  fn finish_call(&mut self, callee: Expression) -> ParserResult<Expression> {
    let mut arguments: Vec<Expression> = Vec::new();

    if !self.check(TokenType::RightParen) {
      loop {
        if arguments.len() >= 255 {
          let token = &self.peek();

          return Err(Box::new(ParserDiagnostic::new(
            ParserDiagnosticError::InvalidNumberOfArguments(255, arguments.len(), token.clone()),
            self.find_token_line(&token.span.line),
          )));
        }

        arguments.push(self.expression()?);

        if !self.match_token(&[TokenType::Comma]) {
          break;
        }
      }
    }

    let token = self.consume(TokenType::RightParen)?;

    Ok(Expression::Call(Call::new(
      Box::new(callee),
      token,
      arguments,
      DataType::Pending,
    )))
  }

  fn get_data_type_by_operator(
    &mut self,
    left: Option<DataType>,
    right: DataType,
    operator: TokenType,
  ) -> DataType {
    match (left, right, operator) {
      (Some(DataType::Int), DataType::Int, TokenType::Plus)
      | (Some(DataType::Int), DataType::Int, TokenType::Minus)
      | (None, DataType::Int, TokenType::Minus)
      | (Some(DataType::Int), DataType::Int, TokenType::Asterisk)
      | (Some(DataType::Int), DataType::Int, TokenType::Slash) => DataType::Int,
      (Some(DataType::Float), DataType::Float, TokenType::Plus)
      | (Some(DataType::Float), DataType::Float, TokenType::Minus)
      | (Some(DataType::Float), DataType::Float, TokenType::Slash)
      | (Some(DataType::Float), DataType::Float, TokenType::Asterisk)
      | (None, DataType::Float, TokenType::Minus) => DataType::Float,
      (Some(DataType::String), DataType::String, TokenType::Plus) => DataType::String,
      (None, DataType::Boolean, TokenType::Bang) | (None, DataType::String, TokenType::Bang) => {
        DataType::Boolean
      }
      _ => DataType::Pending,
    }
  }

  fn get_expression_type(&self, expression: &Expression) -> DataType {
    match expression {
      Expression::Binary(binary) => binary.data_type.clone(),
      Expression::Unary(unary) => unary.data_type.clone(),
      Expression::Literal(literal) => match literal.value {
        LiteralValue::Boolean(_) => DataType::Boolean,
        LiteralValue::Char(_) => DataType::Char,
        LiteralValue::Float(_) => DataType::Float,
        LiteralValue::Int(_) => DataType::Int,
        LiteralValue::String(_) => DataType::String,
        _ => DataType::Int,
      },
      Expression::Grouping(grouping) => self.get_expression_type(&grouping.expression),
      Expression::Variable(variable) => DataType::Variable(variable.name.span.literal.clone()),
      Expression::Assign(assign) => assign.data_type.clone(),
      Expression::Logical(logical) => logical.data_type.clone(),
      Expression::Ternary(ternary) => ternary.data_type.clone(),
      Expression::Call(call) => call.return_type.clone(),
      Expression::Array(a) => a.data_type.clone(),
      Expression::Get(_) => DataType::Pending,
      Expression::New(new) => DataType::ClassType(new.name.span.literal.clone()),
      Expression::Set(set) => set.data_type.clone(),
      Expression::MethodCall(method) => method.data_type.clone(),
      Expression::ArrayAccess(array) => {
        if let DataType::Array(a) = self.get_expression_type(&array.variable) {
          *a
        } else {
          DataType::Pending
        }
      }
      Expression::This(_) => DataType::Pending,
    }
  }

  fn synchronize(&mut self) {
    self.advance();

    while !self.is_at_end() {
      if self.previous().kind == TokenType::SemiColon {
        return;
      }

      match self.peek().kind {
        TokenType::Class
        | TokenType::Function
        | TokenType::Let
        | TokenType::Const
        | TokenType::For
        | TokenType::If
        | TokenType::While
        | TokenType::Import
        | TokenType::Export
        | TokenType::Return => return,
        _ => (),
      };

      self.advance();
    }
  }

  fn declaration(&mut self) -> ParserResult<Statement> {
    if self.match_token(&[TokenType::Let]) {
      return self.variable_declaration();
    }

    if self.match_token(&[TokenType::Class]) {
      self.context.push(ParserContext::Class);
      let value = self.class_declaration(false);

      self.context.pop();

      return value;
    }

    if self.match_token(&[TokenType::Interface]) {
      return self.interface_declaration();
    }

    if self.match_token(&[TokenType::Enum]) {
      return self.enum_declaration(false);
    }

    if self.match_token(&[TokenType::Function]) {
      self.context.push(ParserContext::Function);

      let value = self.function_statement(false, None);

      self.context.pop();

      return value;
    }

    if self.match_token(&[TokenType::Return]) {
      return self.return_statement();
    }

    if self.match_token(&[TokenType::While]) {
      return self.while_statement();
    }

    if self.match_token(&[TokenType::For]) {
      return self.for_statement();
    }

    if self.match_token(&[TokenType::Import]) {
      return self.import_statement();
    }

    if self.match_token(&[TokenType::Export]) {
      return self.export_statement();
    }

    if self.match_token(&[TokenType::At]) {
      return self.decoration_statement();
    }

    if self.match_token(&[TokenType::Break]) {
      return self.break_statement();
    }

    if self.match_token(&[TokenType::Continue]) {
      return self.continue_statement();
    }

    match self.statement() {
      Ok(statement) => Ok(statement),
      Err(error) => {
        self.synchronize();
        Err(error)
      }
    }
  }

  fn enum_declaration(&mut self, is_exported: bool) -> ParserResult<Statement> {
    let name: Token = self.consume(TokenType::Identifier)?;

    self.consume(TokenType::LeftBrace)?;

    let mut members: Vec<EnumMember> = Vec::new();

    while !self.check(TokenType::RightBrace) && !self.is_at_end() {
      let name = self.consume(TokenType::Identifier)?;

      if self.match_token(&[TokenType::Equal]) {
        let value = self.expression()?;

        match value {
          Expression::Literal(l) => match l.value {
            LiteralValue::Int(int) => {
              members.push(EnumMember::new(name, EnumMemberValue::Int(int)));
            }
            LiteralValue::String(str) => {
              members.push(EnumMember::new(name, EnumMemberValue::String(str.clone())));
            }
            _ => members.push(EnumMember::new(name, EnumMemberValue::None)),
          },
          _ => {
            members.push(EnumMember::new(name, EnumMemberValue::None));
          }
        }
      } else {
        members.push(EnumMember::new(name, EnumMemberValue::None));
      }

      if !self.match_token(&[TokenType::Comma]) {
        break;
      }
    }

    self.consume(TokenType::RightBrace)?;

    Ok(Statement::Enum(Enum::new(name, members, is_exported, None)))
  }

  fn interface_declaration(&mut self) -> ParserResult<Statement> {
    let name: Token = self.consume(TokenType::Identifier)?;

    self.consume(TokenType::LeftBrace)?;
    let mut methods: Vec<Statement> = Vec::new();

    while !self.check(TokenType::RightBrace) && !self.is_at_end() {
      methods.push(self.function_statement(false, None)?);
    }

    self.consume(TokenType::RightBrace)?;

    Ok(Statement::Interface(InterfaceStatement::new(name, methods)))
  }

  fn continue_statement(&mut self) -> ParserResult<Statement> {
    let token = self.previous();

    self.consume(TokenType::SemiColon)?;

    Ok(Statement::Continue(Continue::new(token)))
  }

  fn break_statement(&mut self) -> ParserResult<Statement> {
    let token = self.previous();

    self.consume(TokenType::SemiColon)?;

    Ok(Statement::Break(BreakStatement::new(token)))
  }

  fn return_statement(&mut self) -> ParserResult<Statement> {
    let keyword = self.previous();

    if self.check(TokenType::SemiColon) {
      self.advance();
      return Ok(Statement::Return(Return::new(None, Box::new(keyword))));
    }

    let value = self.expression()?;

    self.consume(TokenType::SemiColon)?;

    let result = Return::new(Some(Box::new(value)), Box::new(keyword));
    Ok(Statement::Return(result))
  }

  fn function_statement(
    &mut self,
    is_public: bool,
    decorator: Option<FunctionDecorator>,
  ) -> ParserResult<Statement> {
    let name: Token = self.consume(TokenType::Identifier)?;

    self.consume(TokenType::LeftParen)?;

    let mut parameters: Vec<Variable> = Vec::new();

    if !self.check(TokenType::RightParen) {
      loop {
        if parameters.len() >= 255 {
          return Err(Box::new(ParserDiagnostic::new(
            ParserDiagnosticError::InvalidNumberOfArguments(255, parameters.len(), name.clone()),
            self.find_token_line(&name.span.line),
          )));
        }

        let is_mut: bool = if self.peek().kind == TokenType::Mut {
          self.advance();
          true
        } else {
          false
        };

        let param = self.consume(TokenType::Identifier)?;

        self.consume(TokenType::Colon)?;
        let token = self.advance();

        let mut data_type = DataType::from_token_type(token.kind);

        if data_type == DataType::Variable("".to_string()) {
          data_type = DataType::Variable(token.span.literal.clone());
        }

        if self.check(TokenType::LeftBrack) {
          self.advance();
          self.consume(TokenType::RightBrack)?;

          data_type = DataType::Array(Box::new(data_type));
        }

        parameters.push(Variable::new(
          Box::new(param),
          None,
          data_type,
          VariableMetadata::new(is_mut, false, false, false, false, true),
        ));

        if !self.match_token(&[TokenType::Comma]) {
          break;
        }
      }
    }

    self.consume(TokenType::RightParen)?;

    self.consume(TokenType::Colon)?;

    let return_type: Option<DataType> = if self.match_token(&[
      TokenType::Void,
      TokenType::IntType,
      TokenType::FloatType,
      TokenType::StringType,
      TokenType::BooleanType,
      TokenType::CharType,
    ]) {
      let data_type: DataType = DataType::from_token_type(self.previous().kind);

      if self.check(TokenType::LeftBrack) {
        self.consume(TokenType::RightBrack)?;

        Some(DataType::Array(Box::new(data_type)))
      } else {
        Some(data_type)
      }
    } else {
      let token = &self.peek();

      return Err(Box::new(ParserDiagnostic::new(
        ParserDiagnosticError::ExpectedReturnTypeAfterFunction(token.clone()),
        self.find_token_line(&token.span.line),
      )));
    };

    let mut body: Vec<Statement> = Vec::new();

    if !self.match_token(&[TokenType::SemiColon]) {
      self.consume(TokenType::LeftBrace)?;

      body.push(self.block()?);
    }

    Ok(Statement::FunctionStatement(FunctionStatement::new(
      name,
      parameters,
      body,
      return_type,
      is_public,
      if decorator.is_some() {
        vec![decorator.unwrap()]
      } else {
        vec![]
      },
    )))
  }

  fn block(&mut self) -> ParserResult<Statement> {
    let mut statements: Vec<Statement> = Vec::new();

    while !self.check(TokenType::RightBrace) && !self.is_at_end() {
      statements.push(self.declaration()?);
    }

    self.consume(TokenType::RightBrace)?;

    Ok(Statement::Block(Block::new(statements)))
  }

  fn variable_declaration(&mut self) -> ParserResult<Statement> {
    let mutable: bool = if self.peek().kind == TokenType::Mut {
      self.advance();
      true
    } else {
      false
    };

    let name: Token = self.consume(TokenType::Identifier)?;

    let mut initializer: Option<Expression> = None;

    self.consume(TokenType::Colon)?;

    let token = self.peek();

    let mut type_annotation = DataType::from_token_type(token.kind.clone());

    if type_annotation == DataType::Pending
      && (self.class_declarations.contains(&token.span.literal)
        || self.import_declarations.contains(&token.span.literal))
    {
      type_annotation = DataType::ClassType(token.span.literal.clone());
    }

    if type_annotation == DataType::Pending {
      return Err(Box::new(ParserDiagnostic::new(
        ParserDiagnosticError::ExpectedTypeAfterVariable(token.clone()),
        self.find_token_line(&token.span.line),
      )));
    }

    self.advance();

    if self.match_token(&[TokenType::SemiColon]) {
      return Ok(Statement::Variable(Variable::new(
        Box::new(name),
        None,
        type_annotation,
        VariableMetadata::new(mutable, false, false, false, false, false),
      )));
    }

    if self.match_token(&[TokenType::LeftBrack]) {
      self.consume(TokenType::RightBrack)?;

      type_annotation = DataType::Array(Box::new(type_annotation));
    }

    if self.match_token(&[TokenType::Equal]) {
      let mut value = self.expression()?;

      if let Expression::Array(a) = value {
        value = Expression::Array(Array::new(
          a.token.clone(),
          a.elements,
          type_annotation.clone(),
        ));
      };

      initializer = Some(value);
    }

    self.consume(TokenType::SemiColon)?;

    if let Some(ini) = initializer {
      Ok(Statement::Variable(Variable::new(
        Box::new(name),
        Some(Box::new(ini)),
        type_annotation,
        VariableMetadata::new(mutable, false, false, false, false, false),
      )))
    } else {
      let token = self.peek();
      Err(Box::new(ParserDiagnostic::new(
        ParserDiagnosticError::ExpectedExpression(token.clone()),
        self.find_token_line(&token.span.line),
      )))
    }
  }

  // statement -> expressionStatement | ifStatement;
  fn statement(&mut self) -> ParserResult<Statement> {
    if self.match_token(&[TokenType::LeftBrace]) {
      return self.block();
    }

    if self.match_token(&[TokenType::If]) {
      return self.if_statement();
    }

    self.expression_statement()
  }

  // expressionStatement -> expression ";";
  fn expression_statement(&mut self) -> ParserResult<Statement> {
    let expression = self.expression()?;

    self.consume(TokenType::SemiColon)?;

    Ok(Statement::Expression(ExpressionStatement::new(Box::new(
      expression,
    ))))
  }

  fn assignment(&mut self) -> ParserResult<Expression> {
    let mut expression: Expression = self.ternary()?;

    if self.match_token(&[TokenType::Equal]) {
      let equals: Token = self.previous();
      let value: Expression = self.assignment()?;

      match expression {
        Expression::Variable(variable) => {
          expression = Expression::Assign(Assign::new(
            variable.name,
            Box::new(value),
            variable.data_type,
          ));
        }
        Expression::Get(get) => {
          expression = Expression::Set(Set::new(
            get.name,
            Box::new(value.clone()),
            get.object,
            self.get_expression_type(&value),
          ));
        }
        _ => {
          return Err(Box::new(ParserDiagnostic::new(
            ParserDiagnosticError::InvalidAssignmentTarget(equals.clone()),
            self.find_token_line(&equals.span.line),
          )))
        }
      }
    }

    Ok(expression)
  }

  fn ternary(&mut self) -> ParserResult<Expression> {
    let mut children: Vec<Expression> = Vec::new();

    children.push(self.or_expression()?);

    while self.match_token(&[TokenType::QuestionMark]) {
      children.push(self.expression()?);

      self.consume(TokenType::Colon)?;

      children.push(self.expression()?);
    }

    if children.len() == 1 {
      return Ok(children.pop().unwrap());
    }

    let else_branch = children.pop().unwrap();
    let then_branch = children.pop().unwrap();
    let condition = children.pop().unwrap();

    let mut expression: Expression = Expression::Ternary(ternary::Ternary::new(
      Box::new(condition),
      Box::new(then_branch),
      Box::new(else_branch),
      Box::new(self.peek()),
      DataType::Pending,
    ));

    while !children.is_empty() {
      expression = Expression::Ternary(ternary::Ternary::new(
        Box::new(children.pop().unwrap()),
        Box::new(expression),
        Box::new(children.pop().unwrap()),
        Box::new(self.peek()),
        DataType::Pending,
      ));
    }

    Ok(expression)
  }

  fn or_expression(&mut self) -> ParserResult<Expression> {
    let mut expression = self.and_expression()?;

    while self.match_token(&[TokenType::Or]) {
      let operator: Token = self.previous();
      let right = self.and_expression()?;

      expression = Expression::Logical(Logical::new(
        Box::new(expression),
        operator,
        Box::new(right),
      ));
    }

    Ok(expression)
  }

  fn and_expression(&mut self) -> ParserResult<Expression> {
    let mut expression = self.equality()?;

    while self.match_token(&[TokenType::And]) {
      let operator: Token = self.previous();
      let right = self.equality()?;

      expression = Expression::Logical(Logical::new(
        Box::new(expression),
        operator,
        Box::new(right),
      ));
    }

    Ok(expression)
  }

  fn while_statement(&mut self) -> ParserResult<Statement> {
    self.consume(TokenType::LeftParen)?;

    let condition: Expression = self.expression()?;

    self.consume(TokenType::RightParen)?;

    let body: Statement = self.statement()?;

    Ok(Statement::WhileStatement(WhileStatement::new(
      Box::new(condition),
      Box::new(body),
    )))
  }

  fn for_statement(&mut self) -> ParserResult<Statement> {
    self.consume(TokenType::LeftParen)?;

    self.consume(TokenType::Let)?;
    let item: Token = self.consume(TokenType::Identifier)?;

    let mut variable = Variable::new(
      Box::new(item.clone()),
      None,
      DataType::Pending,
      VariableMetadata::new(true, false, false, false, false, false),
    );

    if self.check(TokenType::Of) {
      return self.for_of_statement(variable);
    }

    self.consume(TokenType::Equal)?;

    let initializer = self.expression()?;

    variable.initializer = Some(Box::new(initializer));
    variable.type_annotation = DataType::Int;

    self.consume(TokenType::SemiColon)?;

    let condition: Expression = self.expression()?;

    self.consume(TokenType::SemiColon)?;

    let increment_decrement: Expression = self.expression()?;

    self.consume(TokenType::RightParen)?;

    self.consume(TokenType::LeftBrace)?;

    let body = self.statement()?;

    self.consume(TokenType::RightBrace)?;

    Ok(Statement::For(For::new(
      Box::new(variable),
      Box::new(condition),
      Box::new(increment_decrement),
      Box::new(body),
    )))
  }

  fn for_of_statement(&mut self, variable: Variable) -> ParserResult<Statement> {
    self.consume(TokenType::Of)?;

    let iterable: Expression = self.expression()?;

    self.consume(TokenType::RightParen)?;

    let body: Statement = self.statement()?;

    let statement = ForOf::new(variable, iterable, body, self.previous());

    Ok(Statement::ForOf(statement))
  }

  fn if_statement(&mut self) -> ParserResult<Statement> {
    self.consume(TokenType::LeftParen)?;

    let condition: Expression = self.expression()?;

    self.consume(TokenType::RightParen)?;

    let then_branch: Statement = self.statement()?;

    let else_branch: Option<Box<Statement>> = if self.match_token(&[TokenType::Else]) {
      Some(Box::new(self.statement()?))
    } else {
      None
    };

    Ok(Statement::IfStatement(IfStatement::new(
      Box::new(condition),
      Box::new(then_branch),
      else_branch,
    )))
  }

  fn consume(&mut self, kind: TokenType) -> ParserResult<Token> {
    let token: Token = self.peek();
    if token.kind == kind {
      return Ok(self.advance());
    }

    let token_previous = self.previous();
    let line = token_previous.span.line;

    let token_line = self.find_token_line(&line);
    let error = match kind {
      TokenType::SemiColon => ParserDiagnostic::new(
        ParserDiagnosticError::ExpectedSemicolonAfterExpression(token_previous.clone()),
        token_line,
      ),
      TokenType::Colon => ParserDiagnostic::new(
        ParserDiagnosticError::UnexpectedToken(TokenType::Colon, token_previous.clone()),
        token_line,
      ),
      TokenType::Identifier => ParserDiagnostic::new(
        ParserDiagnosticError::ExpectedVariableName(token_previous.clone()),
        token_line,
      ),
      TokenType::QuestionMark => ParserDiagnostic::new(
        ParserDiagnosticError::ExpectedToken(TokenType::QuestionMark, token_previous.clone()),
        token_line,
      ),
      TokenType::LeftParen | TokenType::RightParen => {
        let expression = self.previous();

        ParserDiagnostic::new(
          ParserDiagnosticError::ExpectedAfterExpression(
            Box::new(kind.clone()),
            Box::new(expression.clone()),
            Box::new(token.clone()),
          ),
          token_line,
        )
      }
      _ => ParserDiagnostic::new(
        ParserDiagnosticError::ExpectedToken(kind.clone(), token_previous.clone()),
        token_line,
      ),
    };

    Err(Box::new(error))
  }

  fn peek(&mut self) -> Token {
    self.tokens[self.current].clone()
  }

  fn is_at_end(&mut self) -> bool {
    self.peek().kind == TokenType::Eof
  }

  fn match_token(&mut self, kinds: &[TokenType]) -> bool {
    for kind in kinds {
      if self.check(kind.clone()) {
        self.advance();
        return true;
      }
    }

    false
  }

  fn check(&mut self, kind: TokenType) -> bool {
    if self.is_at_end() {
      return false;
    }

    self.peek().kind == kind
  }

  fn advance(&mut self) -> Token {
    if !self.is_at_end() {
      self.current += 1;
    }

    self.previous()
  }

  fn previous(&mut self) -> Token {
    self.tokens[self.current - 1].clone()
  }

  fn property_class_declaration(
    &mut self,
    name: Token,
    is_mutable: bool,
    is_public: bool,
  ) -> ParserResult<Statement> {
    self.consume(TokenType::Colon)?;

    let token = self.peek();

    let mut type_annotation = DataType::from_token_type(token.kind.clone());

    if type_annotation == DataType::Pending && self.class_declarations.contains(&token.span.literal)
    {
      type_annotation = DataType::ClassType(token.span.literal.clone());
    }

    if type_annotation == DataType::Pending {
      let token = self.peek();
      return Err(Box::new(ParserDiagnostic::new(
        ParserDiagnosticError::ExpectedTypeAfterVariable(self.peek()),
        self.find_token_line(&token.span.line),
      )));
    }

    self.advance();

    if self.match_token(&[TokenType::LeftBrack]) {
      self.consume(TokenType::RightBrack)?;

      type_annotation = DataType::Array(Box::new(type_annotation));
    }

    let mut initializer: Option<Box<Expression>> = None;

    if self.match_token(&[TokenType::Equal]) {
      let mut value = self.expression()?;

      if let Expression::Array(a) = value {
        value = Expression::Array(Array::new(
          a.token.clone(),
          a.elements,
          type_annotation.clone(),
        ));
      };

      initializer = Some(Box::new(value));
    }

    let _ = self.consume(TokenType::SemiColon);

    Ok(Statement::Property(PropertyStatement::new(
      Box::new(name),
      initializer,
      type_annotation,
      VariableMetadata::new(is_mutable, false, false, is_public, false, false),
    )))
  }

  fn method_declaration(
    &mut self,
    name: Token,
    is_public: bool,
    decorator: Option<FunctionDecorator>,
    class_name: &Token,
  ) -> ParserResult<Statement> {
    let mut parameters: Vec<Variable> = Vec::new();

    if !self.check(TokenType::RightParen) {
      loop {
        if parameters.len() >= 255 {
          return Err(Box::new(ParserDiagnostic::new(
            ParserDiagnosticError::InvalidNumberOfArguments(255, parameters.len(), name.clone()),
            self.find_token_line(&name.span.line),
          )));
        }

        let is_mut: bool = if self.peek().kind == TokenType::Mut {
          self.advance();
          true
        } else {
          false
        };

        let param = self.consume(TokenType::Identifier)?;

        self.consume(TokenType::Colon)?;
        let token = self.advance();

        let mut data_type = DataType::from_token_type(token.kind);

        if self.check(TokenType::LeftBrack) {
          self.advance();
          self.consume(TokenType::RightBrack)?;

          data_type = DataType::Array(Box::new(data_type));
        }

        parameters.push(Variable::new(
          Box::new(param),
          None,
          data_type,
          VariableMetadata::new(is_mut, false, false, false, false, true),
        ));

        if !self.match_token(&[TokenType::Comma]) {
          break;
        }
      }
    }

    self.consume(TokenType::RightParen)?;

    let return_type: Option<DataType>;
    if self.class_declarations.contains(&name.span.literal) {
      return_type = Some(DataType::ClassType(name.span.literal.clone()));
    } else {
      self.consume(TokenType::Colon)?;

      let token = self.peek();

      let mut data_type: DataType = DataType::from_token_type(token.kind.clone());

      if data_type == DataType::Pending && self.class_declarations.contains(&token.span.literal) {
        data_type = DataType::ClassType(token.span.literal.clone());
      }

      if data_type == DataType::Pending {
        let token = &self.peek();

        return Err(Box::new(ParserDiagnostic::new(
          ParserDiagnosticError::ExpectedReturnTypeAfterFunction(token.clone()),
          self.find_token_line(&token.span.line),
        )));
      }

      self.advance();

      if self.check(TokenType::LeftBrack) {
        self.consume(TokenType::RightBrack)?;
        data_type = DataType::Array(Box::new(data_type));
      }

      return_type = Some(data_type);
    }

    let mut body: Vec<Statement> = Vec::new();

    if !self.match_token(&[TokenType::SemiColon]) {
      self.consume(TokenType::LeftBrace)?;

      body.push(self.block()?);
    }

    let metadata = MethodMetadata::new(
      is_public,
      false,
      self.class_declarations.contains(&name.span.literal),
    );

    Ok(Statement::Method(MethodStatement::new(
      name,
      parameters,
      body,
      return_type,
      if decorator.is_some() {
        vec![decorator.unwrap()]
      } else {
        vec![]
      },
      metadata,
      class_name.clone(),
    )))
  }

  fn class_declaration(&mut self, is_public: bool) -> ParserResult<Statement> {
    let class_name: Token = self.consume(TokenType::Identifier)?;

    self
      .class_declarations
      .push(class_name.span.literal.clone());

    let mut methods: Vec<Statement> = Vec::new();
    let mut properties: Vec<Statement> = Vec::new();
    let mut interfaces: Vec<Token> = Vec::new();
    let mut extends: Option<Token> = None;

    if self.match_token(&[TokenType::Implements]) {
      loop {
        let token = self.consume(TokenType::Identifier)?;

        interfaces.push(token);

        if !self.match_token(&[TokenType::Comma]) {
          break;
        }
      }
    }

    if self.match_token(&[TokenType::Extends]) {
      extends = Some(self.consume(TokenType::Identifier)?);
    }

    self.consume(TokenType::LeftBrace)?;

    while !self.check(TokenType::RightBrace) && !self.is_at_end() {
      let is_public = self.check(TokenType::Public);

      self.match_token(&[TokenType::Public, TokenType::Private]);

      if self.match_token(&[TokenType::Mut]) {
        let name = self.consume(TokenType::Identifier)?;
        properties.push(self.property_class_declaration(name, true, is_public)?);
        continue;
      }

      let name = self.consume(TokenType::Identifier)?;

      if self.match_token(&[TokenType::LeftParen]) {
        methods.push(self.method_declaration(name, is_public, None, &class_name)?);
      } else if self.match_token(&[TokenType::Colon]) {
        properties.push(self.property_class_declaration(name, false, is_public)?);
      } else {
        let token = self.peek();
        return Err(Box::new(ParserDiagnostic::new(
          ParserDiagnosticError::ExpectedToken(TokenType::SemiColon, self.peek()),
          self.find_token_line(&token.span.line),
        )));
      }
    }

    self.consume(TokenType::RightBrace)?;

    Ok(Statement::Class(Class::new(
      class_name,
      methods,
      properties,
      ClassMetadata::new(is_public),
      interfaces,
      extends,
    )))
  }

  fn import_statement(&mut self) -> ParserResult<Statement> {
    self.consume(TokenType::LeftBrace)?;

    let mut symbols: Vec<ImportSymbol> = Vec::new();
    loop {
      if self.check(TokenType::Comma) {
        self.advance();
        continue;
      }

      if self.check(TokenType::RightBrace) {
        break;
      }

      let symbol_name = self.consume(TokenType::Identifier)?;

      let symbol = if self.check(TokenType::As) {
        self.advance();
        let alias = self.consume(TokenType::Identifier)?;
        Some(alias)
      } else {
        None
      };

      self
        .import_declarations
        .push(symbol_name.span.literal.clone());

      symbols.push(ImportSymbol::new(symbol_name, symbol));
    }

    self.consume(TokenType::RightBrace)?;

    self.consume(TokenType::From)?;
    let module_path = self.consume(TokenType::String)?;

    self.consume(TokenType::SemiColon)?;

    let is_std = module_path.span.literal.contains("std");
    let source = if is_std {
      ImportSource::StandardLibrary
    } else {
      ImportSource::FileSystem
    };

    Ok(Statement::Import(Import::new(
      module_path,
      symbols,
      is_std,
      source,
    )))
  }

  fn export_statement(&mut self) -> ParserResult<Statement> {
    if self.match_token(&[TokenType::Function]) {
      self.context.push(ParserContext::Function);

      let function = self.function_statement(true, None)?;

      self.context.pop();

      Ok(function)
    } else if self.match_token(&[TokenType::Class]) {
      self.context.push(ParserContext::Class);

      let class = self.class_declaration(true)?;

      self.context.pop();
      Ok(class)
    } else {
      let token = self.peek();
      let line = token.span.line;

      Err(Box::new(ParserDiagnostic::new(
        ParserDiagnosticError::ExpectedToken(TokenType::Function, token),
        self.find_token_line(&line),
      )))
    }
  }

  fn decoration_statement(&mut self) -> ParserResult<Statement> {
    match self.peek().kind {
      TokenType::Function => {
        self.context.push(ParserContext::Function);

        let function = self.function_statement(true, None)?;

        self.context.pop();
        Ok(function)
      }
      TokenType::Extern => {
        self.advance();
        self.consume(TokenType::LeftParen)?;

        let path = self.consume(TokenType::String)?;

        self.consume(TokenType::RightParen)?;

        let is_public = self.match_token(&[TokenType::Export]);

        self.consume(TokenType::Function)?;

        self.context.push(ParserContext::Function);

        let func = self.function_statement(is_public, Some(FunctionDecorator::Extern(path)))?;

        self.context.pop();

        Ok(func)
      }
      TokenType::Identifier => {
        todo!()
      }
      _ => todo!(),
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
}
