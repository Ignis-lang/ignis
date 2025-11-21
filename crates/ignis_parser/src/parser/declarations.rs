use ignis_ast::{
  NodeId,
  statements::{
    ASTStatement,
    block::ASTBlock,
    const_statement::ASTConstant,
    export_statement::ASTExport,
    extern_statement::ASTExtern,
    function::{ASTFunction, ASTFunctionSignature},
    import_statement::ASTImport,
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

  /// const name: type = expr;
  pub fn parse_constant_declaration(&mut self) -> ParserResult<NodeId> {
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
    let dummy_value = self
      .nodes
      .alloc(ignis_ast::ASTNode::Expression(ignis_ast::expressions::ASTExpression::Literal(
        ignis_ast::expressions::literal::ASTLiteral::new(ignis_type::value::IgnisLiteralValue::Int64(0), span.clone()),
      )));

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
}
