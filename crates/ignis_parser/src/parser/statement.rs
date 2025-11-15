use ignis_ast::{
  NodeId,
  statements::{
    ASTStatement,
    block::ASTBlock,
    break_statement::ASTBreak,
    const_statement::ASTConstant,
    continue_statement::ASTContinue,
    export_statement::ASTExport,
    extern_statement::ASTExtern,
    for_statement::ASTFor,
    function::{ASTFunction, ASTFunctionSignature, ASTParameter},
    if_statement::ASTIf,
    import_statement::ASTImport,
    return_statement::ASTReturn,
    variable::ASTVariable,
    while_statement::ASTWhile,
  },
};
use ignis_diagnostics::message::DiagnosticMessage;
use ignis_token::token_types::TokenType;
use ignis_type::span::Span;

use crate::parser::ParserResult;

impl super::IgnisParser {
  /// Entry point: parse program as a list of declarations
  /// With error recovery: continues parsing after errors to collect multiple diagnostics
  pub(crate) fn parse_program(&mut self) -> ParserResult<Vec<NodeId>> {
    let mut items = Vec::new();

    while !self.at(TokenType::Eof) {
      match self.parse_declaration() {
        Ok(node_id) => {
          items.push(node_id);
        },
        Err(diagnostic) => {
          self.diagnostics.push(diagnostic);

          if self.recovery {
            self.synchronize_after_declaration();
          } else {
            return Err(self.diagnostics.last().unwrap().clone());
          }
        },
      }
    }

    Ok(items)
  }

  /// Parse a top-level declaration
  fn parse_declaration(&mut self) -> ParserResult<NodeId> {
    match self.peek().type_ {
      TokenType::Function => self.parse_function_declaration(),
      TokenType::Import => self.parse_import_declaration(),
      TokenType::Export => self.parse_export_declaration(),
      TokenType::Const => self.parse_constant_declaration(),
      TokenType::Extern => self.parse_extern_declaration(),
      _ => Err(DiagnosticMessage::UnexpectedToken {
        at: self.peek().span.clone(),
      }),
    }
  }

  /// function name(params): type block
  fn parse_function_declaration(&mut self) -> ParserResult<NodeId> {
    let keyword = self.expect(TokenType::Function)?.clone();
    let name_token = self.expect(TokenType::Identifier)?.clone();
    let name = self.insert_symbol(&name_token);

    let parameters =
      self.parse_delimited_list(TokenType::LeftParen, TokenType::RightParen, TokenType::Comma, |parser| {
        parser.parse_parameter()
      })?;

    let right_paren = self.previous().clone();

    self.expect(TokenType::Colon)?;
    let return_type = self.parse_type_syntax()?;

    let body = self.parse_block()?;
    let _span = Span::merge(&keyword.span, self.get_span(&body));

    let signature_span = Span::merge(&name_token.span, &right_paren.span);
    let signature = ASTFunctionSignature::new(
      name,
      parameters,
      return_type,
      signature_span,
      ignis_ast::metadata::ASTMetadata::NONE,
    );

    let function = ASTFunction::new(signature, Some(body));

    Ok(self.allocate_statement(ASTStatement::Function(function)))
  }

  /// Parse a function parameter: name: type
  fn parse_parameter(&mut self) -> ParserResult<ASTParameter> {
    let name_token = self.expect(TokenType::Identifier)?.clone();
    let name = self.insert_symbol(&name_token);

    self.expect(TokenType::Colon)?;

    let type_annotation = self.parse_type_syntax()?;
    let span = name_token.span.clone();

    Ok(ASTParameter::new(
      name,
      type_annotation,
      span,
      ignis_ast::metadata::ASTMetadata::NONE,
    ))
  }

  /// Parse a block: { statements }
  /// With error recovery: continues parsing statements after errors
  fn parse_block(&mut self) -> ParserResult<NodeId> {
    let left_brace = self.expect(TokenType::LeftBrace)?.clone();
    let mut items = Vec::new();

    while !self.at(TokenType::RightBrace) && !self.at(TokenType::Eof) {
      match self.parse_statement() {
        Ok(node_id) => {
          items.push(node_id);
        },
        Err(diagnostic) => {
          self.diagnostics.push(diagnostic);

          if self.recovery {
            self.synchronize_after_statement();

            if self.at(TokenType::RightBrace) {
              break;
            }
          } else {
            return Err(self.diagnostics.last().unwrap().clone());
          }
        },
      }
    }

    let right_brace = self.expect(TokenType::RightBrace)?.clone();
    let span = Span::merge(&left_brace.span, &right_brace.span);
    let block = ASTBlock::new(items, span);

    Ok(self.allocate_statement(ASTStatement::Block(block)))
  }

  /// const name: type = expr;
  fn parse_constant_declaration(&mut self) -> ParserResult<NodeId> {
    let keyword = self.expect(TokenType::Const)?.clone();
    let name_token = self.expect(TokenType::Identifier)?.clone();
    let name = self.insert_symbol(&name_token);

    self.expect(TokenType::Colon)?;

    let type_annotation = self.parse_type_syntax()?;

    self.expect(TokenType::Equal)?;

    let value = self.parse_expression(0)?;
    let semicolon = self.expect(TokenType::SemiColon)?.clone();
    let span = Span::merge(&keyword.span, &semicolon.span);

    let constant_statement = ASTConstant::new(name, type_annotation, value, span);

    Ok(self.allocate_statement(ASTStatement::Constant(constant_statement)))
  }

  /// import a, b, c from "path";
  fn parse_import_declaration(&mut self) -> ParserResult<NodeId> {
    let keyword = self.expect(TokenType::Import)?.clone();
    let mut items = Vec::new();

    loop {
      let identifier = self.expect(TokenType::Identifier)?.clone();
      items.push(self.insert_symbol(&identifier));
      if !self.eat(TokenType::Comma) {
        break;
      }
    }

    self.expect(TokenType::From)?;
    let path_token = self.expect(TokenType::String)?.clone();
    let semicolon = self.expect(TokenType::SemiColon)?.clone();
    let span = Span::merge(&keyword.span, &semicolon.span);

    let import_statement = ASTImport::new(items, path_token.lexeme.clone(), span);
    Ok(self.allocate_statement(ASTStatement::Import(import_statement)))
  }

  /// export <declaration> | export <identifier>;
  fn parse_export_declaration(&mut self) -> ParserResult<NodeId> {
    let keyword = self.expect(TokenType::Export)?.clone();

    if self.at(TokenType::Function) || self.at(TokenType::Const) || self.at(TokenType::Extern) {
      let declaration = self.parse_declaration_inner_after_export()?;
      let span = Span::merge(&keyword.span, self.get_span(&declaration));
      let export_statement = ASTExport::declaration(declaration, span);

      Ok(self.allocate_statement(ASTStatement::Export(export_statement)))
    } else {
      let name_token = self.expect(TokenType::Identifier)?.clone();
      let semicolon = self.expect(TokenType::SemiColon)?.clone();
      let span = Span::merge(&keyword.span, &semicolon.span);
      let export_statement = ASTExport::name(self.insert_symbol(&name_token), span);

      Ok(self.allocate_statement(ASTStatement::Export(export_statement)))
    }
  }

  /// Helper: parse declaration without consuming 'export' again
  fn parse_declaration_inner_after_export(&mut self) -> ParserResult<NodeId> {
    match self.peek().type_ {
      TokenType::Function => self.parse_function_declaration(),
      TokenType::Const => self.parse_constant_declaration(),
      TokenType::Extern => self.parse_extern_declaration(),
      _ => Err(DiagnosticMessage::UnexpectedToken {
        at: self.peek().span.clone(),
      }),
    }
  }

  /// extern <function|const>
  fn parse_extern_declaration(&mut self) -> ParserResult<NodeId> {
    let keyword = self.expect(TokenType::Extern)?.clone();
    match self.peek().type_ {
      TokenType::Function => {
        let node = self.parse_function_signature_only()?;
        let span = Span::merge(&keyword.span, self.get_span(&node));
        let extern_statement = ASTExtern::new(node, span);

        Ok(self.allocate_statement(ASTStatement::Extern(extern_statement)))
      },
      TokenType::Const => {
        let node = self.parse_constant_signature_only()?;
        let span = Span::merge(&keyword.span, self.get_span(&node));
        let extern_statement = ASTExtern::new(node, span);

        Ok(self.allocate_statement(ASTStatement::Extern(extern_statement)))
      },
      _ => Err(DiagnosticMessage::UnexpectedToken {
        at: self.peek().span.clone(),
      }),
    }
  }

  /// const name: type;
  fn parse_constant_signature_only(&mut self) -> ParserResult<NodeId> {
    let keyword = self.expect(TokenType::Const)?.clone();
    let name_token = self.expect(TokenType::Identifier)?.clone();
    let name = self.insert_symbol(&name_token);

    self.expect(TokenType::Colon)?;
    let type_annotation = self.parse_type_syntax()?;
    let semicolon = self.expect(TokenType::SemiColon)?.clone();
    let span = Span::merge(&keyword.span, &semicolon.span);

    // Create a dummy value node for extern constants (will be resolved by linker)
    let dummy_value = self.nodes.alloc(ignis_ast::ASTNode::Expression(
      ignis_ast::expressions::ASTExpression::Literal(ignis_ast::expressions::literal::ASTLiteral::new(
        ignis_type::value::IgnisLiteralValue::Int64(0),
        span.clone(),
      )),
    ));

    let constant_statement = ASTConstant::new(name, type_annotation, dummy_value, span);

    Ok(self.allocate_statement(ASTStatement::Constant(constant_statement)))
  }

  /// function name(params): type;
  fn parse_function_signature_only(&mut self) -> ParserResult<NodeId> {
    let _function_keyword = self.expect(TokenType::Function)?.clone();
    let name_token = self.expect(TokenType::Identifier)?.clone();
    let name = self.insert_symbol(&name_token);

    let parameters =
      self.parse_delimited_list(TokenType::LeftParen, TokenType::RightParen, TokenType::Comma, |parser| {
        parser.parse_parameter()
      })?;

    self.expect(TokenType::Colon)?;
    let return_type = self.parse_type_syntax()?;
    let semicolon = self.expect(TokenType::SemiColon)?.clone();
    let span = Span::merge(&name_token.span, &semicolon.span);

    let signature = ASTFunctionSignature::new(
      name,
      parameters,
      return_type,
      span.clone(),
      ignis_ast::metadata::ASTMetadata::EXTERN_MEMBER,
    );

    let empty_body = self.allocate_statement(ASTStatement::Block(ASTBlock::new(vec![], semicolon.span.clone())));
    let function = ASTFunction::new(signature, Some(empty_body));

    Ok(self.allocate_statement(ASTStatement::Function(function)))
  }

  /// Parse a statement inside blocks
  pub(crate) fn parse_statement(&mut self) -> ParserResult<NodeId> {
    match self.peek().type_ {
      TokenType::Const => self.parse_constant_declaration(),
      TokenType::Let => self.parse_let_statement(),
      TokenType::If => self.parse_if_statement(),
      TokenType::While => self.parse_while_statement(),
      TokenType::Return => self.parse_return_statement(),
      TokenType::For => self.parse_for_statement(),
      TokenType::Break => {
        let token = self.bump().clone();
        let semicolon = self.expect(TokenType::SemiColon)?;
        let span = Span::merge(&token.span, &semicolon.span);
        let break_statement = ASTBreak::new(span);

        Ok(self.allocate_statement(ASTStatement::Break(break_statement)))
      },
      TokenType::Continue => {
        let token = self.bump().clone();
        let semicolon = self.expect(TokenType::SemiColon)?;
        let span = Span::merge(&token.span, &semicolon.span);
        let continue_statement = ASTContinue::new(span);

        Ok(self.allocate_statement(ASTStatement::Continue(continue_statement)))
      },
      TokenType::LeftBrace => self.parse_block(),
      _ => {
        let expression = self.parse_expression(0)?;
        self.expect(TokenType::SemiColon)?;

        Ok(
          self.allocate_statement(ASTStatement::Expression(match self.get_node(&expression) {
            ignis_ast::ASTNode::Expression(expression_node) => expression_node.clone(),
            _ => unreachable!(),
          })),
        )
      },
    }
  }

  /// let name: type (= init)?;
  fn parse_let_statement(&mut self) -> ParserResult<NodeId> {
    let keyword = self.expect(TokenType::Let)?.clone();
    let is_mutable = self.eat(TokenType::Mut);
    let name_token = self.expect(TokenType::Identifier)?.clone();
    let name = self.insert_symbol(&name_token);

    self.expect(TokenType::Colon)?;

    let type_annotation = self.parse_type_syntax()?;

    let mut initializer: Option<NodeId> = None;

    if self.eat(TokenType::Equal) {
      initializer = Some(self.parse_expression(0)?);
    }

    let semicolon = self.expect(TokenType::SemiColon)?.clone();
    let span = Span::merge(&keyword.span, &semicolon.span);

    let metadata = if is_mutable {
      ignis_ast::metadata::ASTMetadata::VARIABLE | ignis_ast::metadata::ASTMetadata::MUTABLE
    } else {
      ignis_ast::metadata::ASTMetadata::VARIABLE
    };

    let variable = ASTVariable::new(name, initializer, type_annotation, span, metadata);
    Ok(self.allocate_statement(ASTStatement::Variable(variable)))
  }

  /// if condition block (else block)?
  fn parse_if_statement(&mut self) -> ParserResult<NodeId> {
    let keyword = self.expect(TokenType::If)?.clone();

    let condition = self.parse_expression(0)?;
    let then_block = self.parse_block()?;
    let mut else_block = None;
    let mut end_span = self.get_span(&then_block).clone();

    if self.eat(TokenType::Else) {
      let else_block_node = self.parse_block()?;
      end_span = self.get_span(&else_block_node).clone();
      else_block = Some(else_block_node);
    }

    let span = Span::merge(&keyword.span, &end_span);
    let if_statement = ASTIf::new(condition, then_block, else_block, span);
    Ok(self.allocate_statement(ASTStatement::If(if_statement)))
  }

  /// while condition block
  fn parse_while_statement(&mut self) -> ParserResult<NodeId> {
    let keyword = self.expect(TokenType::While)?.clone();

    let condition = self.parse_expression(0)?;
    let body = self.parse_block()?;
    let span = Span::merge(&keyword.span, self.get_span(&body));

    let while_statement = ASTWhile::new(condition, body, span);

    Ok(self.allocate_statement(ASTStatement::While(while_statement)))
  }

  /// for (let name = expr; condition; increment) block
  fn parse_for_statement(&mut self) -> ParserResult<NodeId> {
    let keyword = self.expect(TokenType::For)?.clone();
    self.expect(TokenType::LeftParen)?;

    let initializer = self.parse_for_initializer()?;
    self.expect(TokenType::SemiColon)?;

    let condition = self.parse_expression(0)?;
    self.expect(TokenType::SemiColon)?;

    let increment = self.parse_expression(0)?;
    self.expect(TokenType::RightParen)?;

    let body = self.parse_block()?;
    let span = Span::merge(&keyword.span, self.get_span(&body));

    let for_statement = ASTFor::new(initializer, condition, increment, body, span);

    Ok(self.allocate_statement(ASTStatement::For(for_statement)))
  }

  fn parse_for_initializer(&mut self) -> ParserResult<NodeId> {
    let keyword = self.expect(TokenType::Let)?.clone();
    let name_token = self.expect(TokenType::Identifier)?.clone();
    let name = self.insert_symbol(&name_token);

    self.expect(TokenType::Equal)?;
    let initializer = self.parse_expression(0)?;

    let span = Span::merge(&keyword.span, self.get_span(&initializer));

    let metadata = ignis_ast::metadata::ASTMetadata::VARIABLE | ignis_ast::metadata::ASTMetadata::MUTABLE;

    let variable = ASTVariable::new(name, Some(initializer), ignis_ast::type_::IgnisTypeSyntax::Void, span, metadata);

    Ok(self.allocate_statement(ASTStatement::Variable(variable)))
  }

  /// return expr?;
  fn parse_return_statement(&mut self) -> ParserResult<NodeId> {
    let keyword = self.expect(TokenType::Return)?.clone();
    let mut value = None;

    if !self.at(TokenType::SemiColon) {
      let expression = self.parse_expression(0)?;
      value = Some(expression);
    }

    let semicolon = self.expect(TokenType::SemiColon)?;
    let span = Span::merge(&keyword.span, &semicolon.span);
    let return_statement = ASTReturn::new(value, span);

    Ok(self.allocate_statement(ASTStatement::Return(return_statement)))
  }
}
