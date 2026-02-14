use ignis_ast::{
  NodeId,
  statements::{
    ASTStatement, ASTForOf, ForOfBinding, block::ASTBlock, break_statement::ASTBreak, continue_statement::ASTContinue,
    for_statement::ASTFor, function::ASTParameter, if_statement::ASTIf, let_else::ASTLetElse,
    return_statement::ASTReturn, variable::ASTVariable, while_statement::ASTWhile,
  },
};
use ignis_token::token_types::TokenType;
use ignis_type::span::Span;

use crate::parser::ParserResult;

impl super::IgnisParser {
  /// Parses `[@attr] name: type`. Uses inline attribute parsing to avoid
  /// clobbering the global `pending_attrs` buffer (which holds function-level attrs).
  pub(crate) fn parse_parameter(&mut self) -> ParserResult<ASTParameter> {
    let attrs = self.parse_inline_attributes()?;

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
      attrs,
    ))
  }

  /// Parse a block: { statements }
  /// With error recovery: continues parsing statements after errors
  pub(crate) fn parse_block(&mut self) -> ParserResult<NodeId> {
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

  /// Parse a statement inside blocks
  pub(crate) fn parse_statement(&mut self) -> ParserResult<NodeId> {
    match self.peek().type_ {
      TokenType::Let => self.parse_let_statement(),
      TokenType::Const => self.parse_constant_declaration(),
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

  /// let name (: type)? (= init)?;
  /// let pattern = expr else block; (let-else)
  fn parse_let_statement(&mut self) -> ParserResult<NodeId> {
    let doc = self.take_pending_doc();

    let keyword = self.expect(TokenType::Let)?.clone();

    let is_complex_pattern = self.looks_like_complex_pattern();

    if is_complex_pattern {
      return self.parse_let_else_statement(keyword);
    }

    let is_mutable = self.eat(TokenType::Mut);
    let name_token = self.expect(TokenType::Identifier)?.clone();
    let name = self.insert_symbol(&name_token);

    let type_annotation = if self.eat(TokenType::Colon) {
      self.parse_type_syntax()?
    } else {
      ignis_ast::type_::IgnisTypeSyntax::Implicit
    };

    if self.eat(TokenType::Equal) {
      let initializer = self.parse_expression(0)?;

      if self.eat(TokenType::Else) {
        let else_block = self.parse_block()?;
        let semicolon = self.expect(TokenType::SemiColon)?.clone();
        let span = Span::merge(&keyword.span, &semicolon.span);

        let pattern = ignis_ast::pattern::ASTPattern::Path {
          segments: vec![(name, name_token.span.clone())],
          args: None,
          span: name_token.span,
        };
        let let_else = ASTLetElse::new(pattern, initializer, else_block, span);
        return Ok(self.allocate_statement(ASTStatement::LetElse(let_else)));
      }

      let semicolon = self.expect(TokenType::SemiColon)?.clone();
      let span = Span::merge(&keyword.span, &semicolon.span);

      let metadata = if is_mutable {
        ignis_ast::metadata::ASTMetadata::VARIABLE | ignis_ast::metadata::ASTMetadata::MUTABLE
      } else {
        ignis_ast::metadata::ASTMetadata::VARIABLE
      };

      let variable = ASTVariable::new(
        name,
        name_token.span.clone(),
        Some(initializer),
        type_annotation,
        span,
        metadata,
        doc,
      );
      return Ok(self.allocate_statement(ASTStatement::Variable(variable)));
    }

    let semicolon = self.expect(TokenType::SemiColon)?.clone();
    let span = Span::merge(&keyword.span, &semicolon.span);

    let metadata = if is_mutable {
      ignis_ast::metadata::ASTMetadata::VARIABLE | ignis_ast::metadata::ASTMetadata::MUTABLE
    } else {
      ignis_ast::metadata::ASTMetadata::VARIABLE
    };

    let variable = ASTVariable::new(name, name_token.span.clone(), None, type_annotation, span, metadata, doc);
    Ok(self.allocate_statement(ASTStatement::Variable(variable)))
  }

  fn looks_like_complex_pattern(&self) -> bool {
    let offset = if self.at(TokenType::Mut) { 1 } else { 0 };

    let first = &self.peek_nth(offset).type_;
    if *first != TokenType::Identifier {
      return true;
    }

    let second = &self.peek_nth(offset + 1).type_;
    *second == TokenType::DoubleColon || *second == TokenType::LeftParen
  }

  fn parse_let_else_statement(
    &mut self,
    keyword: ignis_token::token::Token,
  ) -> ParserResult<NodeId> {
    let pattern = self.parse_pattern()?;
    self.expect(TokenType::Equal)?;
    let value = self.parse_expression(0)?;
    self.expect(TokenType::Else)?;
    let else_block = self.parse_block()?;
    let semicolon = self.expect(TokenType::SemiColon)?.clone();

    let span = Span::merge(&keyword.span, &semicolon.span);
    let let_else = ASTLetElse::new(pattern, value, else_block, span);
    Ok(self.allocate_statement(ASTStatement::LetElse(let_else)))
  }

  /// if (condition) block (else block)?
  fn parse_if_statement(&mut self) -> ParserResult<NodeId> {
    let keyword = self.expect(TokenType::If)?.clone();

    self.expect(TokenType::LeftParen)?;
    let condition = self.parse_expression(0)?;
    self.expect(TokenType::RightParen)?;

    let then_block = self.parse_block()?;
    let mut else_block = None;
    let mut end_span = self.get_span(&then_block).clone();

    if self.eat(TokenType::Else) {
      let else_block_node = if self.at(TokenType::If) {
        self.parse_if_statement()?
      } else {
        self.parse_block()?
      };
      end_span = self.get_span(&else_block_node).clone();
      else_block = Some(else_block_node);
    }

    let span = Span::merge(&keyword.span, &end_span);
    let if_statement = ASTIf::new(condition, then_block, else_block, span);
    Ok(self.allocate_statement(ASTStatement::If(if_statement)))
  }

  /// while (condition) block
  fn parse_while_statement(&mut self) -> ParserResult<NodeId> {
    let keyword = self.expect(TokenType::While)?.clone();

    self.expect(TokenType::LeftParen)?;
    let condition = self.parse_expression(0)?;
    self.expect(TokenType::RightParen)?;

    let body = self.parse_block()?;
    let span = Span::merge(&keyword.span, self.get_span(&body));

    let while_statement = ASTWhile::new(condition, body, span);

    Ok(self.allocate_statement(ASTStatement::While(while_statement)))
  }

  /// for (let name = expr; condition; increment) block     -- C-style
  /// for (let name of expr) block                          -- for-of
  /// for (let name: T of expr) block                       -- for-of with type annotation
  fn parse_for_statement(&mut self) -> ParserResult<NodeId> {
    let keyword = self.expect(TokenType::For)?.clone();
    self.expect(TokenType::LeftParen)?;

    // Lookahead to detect for-of vs C-style:
    // for (let ident = ...)     => C-style (no type annotation)
    // for (let ident: T = ...)  => C-style (with type annotation)
    // for (let ident of ...)    => for-of (no type annotation)
    // for (let ident: T of ...) => for-of (with type annotation)
    //
    // Decision logic:
    // - peek(2) = '=' => C-style without type
    // - peek(2) = 'of' => for-of without type
    // - peek(2) = ':' => need to scan ahead to find '=' or 'of' after the type
    if self.at(TokenType::Let) && self.peek_nth(1).type_ == TokenType::Identifier {
      let decision_token = self.peek_nth(2).type_;

      match decision_token {
        TokenType::Equal => {
          // for C-style without type: for (let x = ...; ...; ...)
          return self.parse_for_c_style(keyword);
        },
        TokenType::Of => {
          // for-of without type: for (let x of ...)
          return self.parse_for_of(keyword);
        },
        TokenType::Colon => {
          // Has type annotation - need to scan ahead to find '=' or 'of'
          // Skip past the type to find the deciding token
          if self.is_for_of_with_type_annotation() {
            return self.parse_for_of(keyword);
          } else {
            return self.parse_for_c_style(keyword);
          }
        },
        _ => {
          // Error: expected '=', ':' or 'of' after identifier
          // Fall through to C-style parser which will give appropriate error
          return self.parse_for_c_style(keyword);
        },
      }
    }

    // Fallback to C-style
    self.parse_for_c_style(keyword)
  }

  /// Check if the current for statement with type annotation is for-of.
  /// Scans ahead from position 3 (after ':') to find 'of' before '=' or ';'.
  fn is_for_of_with_type_annotation(&self) -> bool {
    // Start scanning after the colon (position 3)
    let mut pos = 3;
    let mut paren_depth = 0;
    let mut bracket_depth = 0;

    loop {
      let token = self.peek_nth(pos);

      match token.type_ {
        TokenType::Of if paren_depth == 0 && bracket_depth == 0 => {
          // Found 'of' at top level - this is for-of
          return true;
        },
        TokenType::Equal if paren_depth == 0 && bracket_depth == 0 => {
          // Found '=' at top level - this is C-style for
          return false;
        },
        TokenType::SemiColon | TokenType::RightParen | TokenType::Eof => {
          // Reached end of initializer without finding 'of' - assume C-style
          return false;
        },
        TokenType::LeftParen => paren_depth += 1,
        TokenType::LeftBrack => bracket_depth += 1,
        TokenType::RightBrack => {
          if bracket_depth > 0 {
            bracket_depth -= 1;
          }
        },
        _ => {},
      }

      pos += 1;

      // Safety limit to avoid infinite loop
      if pos > 100 {
        return false;
      }
    }
  }

  /// Parse C-style for loop: for (let name = expr; condition; increment) block
  fn parse_for_c_style(
    &mut self,
    keyword: ignis_token::token::Token,
  ) -> ParserResult<NodeId> {
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

  /// Parse for-of loop: for (let name of expr) block
  fn parse_for_of(
    &mut self,
    keyword: ignis_token::token::Token,
  ) -> ParserResult<NodeId> {
    self.expect(TokenType::Let)?;
    let name_token = self.expect(TokenType::Identifier)?.clone();
    let name = self.insert_symbol(&name_token);

    // Optional: type annotation
    let type_ann = if self.eat(TokenType::Colon) {
      Some(self.parse_type_syntax()?)
    } else {
      None
    };

    self.expect(TokenType::Of)?;
    let iter = self.parse_expression(0)?;
    self.expect(TokenType::RightParen)?;

    let body = self.parse_block()?;
    let span = Span::merge(&keyword.span, self.get_span(&body));

    let binding = ForOfBinding::new(name, type_ann, name_token.span);
    let for_of = ASTForOf::new(binding, iter, body, span);

    Ok(self.allocate_statement(ASTStatement::ForOf(for_of)))
  }

  fn parse_for_initializer(&mut self) -> ParserResult<NodeId> {
    let keyword = self.expect(TokenType::Let)?.clone();
    let name_token = self.expect(TokenType::Identifier)?.clone();
    let name = self.insert_symbol(&name_token);

    let type_annotation = if self.eat(TokenType::Colon) {
      self.parse_type_syntax()?
    } else {
      ignis_ast::type_::IgnisTypeSyntax::Implicit
    };

    self.expect(TokenType::Equal)?;
    let initializer = self.parse_expression(0)?;

    let span = Span::merge(&keyword.span, self.get_span(&initializer));

    let metadata = ignis_ast::metadata::ASTMetadata::VARIABLE | ignis_ast::metadata::ASTMetadata::MUTABLE;

    let variable = ASTVariable::new(name, name_token.span, Some(initializer), type_annotation, span, metadata, None);

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

#[cfg(test)]
mod tests {
  use std::{cell::RefCell, rc::Rc};

  use ignis_ast::{ASTNode, NodeId, statements::ASTStatement};
  use ignis_type::{Store, file::SourceMap, symbol::SymbolTable};

  use crate::{lexer::IgnisLexer, parser::IgnisParser};

  struct ParseResult {
    nodes: Store<ASTNode>,
    roots: Vec<NodeId>,
    symbols: Rc<RefCell<SymbolTable>>,
  }

  fn parse_stmt(source: &str) -> ParseResult {
    let program = format!("function test(): void {{ {} }}", source);
    let mut sm = SourceMap::new();
    let file_id = sm.add_file("test.ign", program.clone());

    let mut lexer = IgnisLexer::new(file_id, &program);
    lexer.scan_tokens();

    let symbols = Rc::new(RefCell::new(SymbolTable::new()));
    let mut parser = IgnisParser::new(lexer.tokens, symbols.clone());
    let (nodes, roots) = parser.parse().expect("parse failed");

    ParseResult { nodes, roots, symbols }
  }

  fn get_stmt<'a>(
    result: &'a ParseResult,
    index: usize,
  ) -> &'a ASTStatement {
    let root = result.nodes.get(&result.roots[0]);
    let func = match root {
      ASTNode::Statement(ASTStatement::Function(f)) => f,
      _ => panic!("expected function"),
    };
    let body = func.body.as_ref().expect("no body");
    let block = match result.nodes.get(body) {
      ASTNode::Statement(ASTStatement::Block(b)) => b,
      _ => panic!("expected block"),
    };
    let stmt = result.nodes.get(&block.statements[index]);
    match stmt {
      ASTNode::Statement(s) => s,
      _ => panic!("expected statement"),
    }
  }

  fn symbol_name(
    result: &ParseResult,
    id: &ignis_type::symbol::SymbolId,
  ) -> String {
    result.symbols.borrow().get(id).to_string()
  }

  #[test]
  fn parses_let_without_initializer() {
    let result = parse_stmt("let x: i32;");
    let stmt = get_stmt(&result, 0);

    match stmt {
      ASTStatement::Variable(var) => {
        assert_eq!(symbol_name(&result, &var.name), "x");
        assert!(var.value.is_none());
      },
      other => panic!("expected variable, got {:?}", other),
    }
  }

  #[test]
  fn parses_let_with_initializer() {
    let result = parse_stmt("let x: i32 = 42;");
    let stmt = get_stmt(&result, 0);

    match stmt {
      ASTStatement::Variable(var) => {
        assert_eq!(symbol_name(&result, &var.name), "x");
        assert!(var.value.is_some());
      },
      other => panic!("expected variable, got {:?}", other),
    }
  }

  #[test]
  fn parses_let_mut() {
    let result = parse_stmt("let mut x: i32 = 0;");
    let stmt = get_stmt(&result, 0);

    match stmt {
      ASTStatement::Variable(var) => {
        assert!(var.metadata.is_mutable());
      },
      other => panic!("expected variable, got {:?}", other),
    }
  }

  #[test]
  fn parses_if_statement() {
    let result = parse_stmt("if (true) { }");
    let stmt = get_stmt(&result, 0);

    match stmt {
      ASTStatement::If(if_stmt) => {
        assert!(if_stmt.else_block.is_none());
      },
      other => panic!("expected if, got {:?}", other),
    }
  }

  #[test]
  fn parses_if_else_statement() {
    let result = parse_stmt("if (true) { } else { }");
    let stmt = get_stmt(&result, 0);

    match stmt {
      ASTStatement::If(if_stmt) => {
        assert!(if_stmt.else_block.is_some());
      },
      other => panic!("expected if, got {:?}", other),
    }
  }

  #[test]
  fn parses_while_statement() {
    let result = parse_stmt("while (true) { }");
    let stmt = get_stmt(&result, 0);

    match stmt {
      ASTStatement::While(_) => {},
      other => panic!("expected while, got {:?}", other),
    }
  }

  #[test]
  fn parses_for_statement() {
    let result = parse_stmt("for (let i = 0; i < 10; i++) { }");
    let stmt = get_stmt(&result, 0);

    match stmt {
      ASTStatement::For(_) => {},
      other => panic!("expected for, got {:?}", other),
    }
  }

  #[test]
  fn parses_return_void() {
    let result = parse_stmt("return;");
    let stmt = get_stmt(&result, 0);

    match stmt {
      ASTStatement::Return(ret) => {
        assert!(ret.expression.is_none());
      },
      other => panic!("expected return, got {:?}", other),
    }
  }

  #[test]
  fn parses_return_with_expression() {
    let result = parse_stmt("return 42;");
    let stmt = get_stmt(&result, 0);

    match stmt {
      ASTStatement::Return(ret) => {
        assert!(ret.expression.is_some());
      },
      other => panic!("expected return, got {:?}", other),
    }
  }

  #[test]
  fn parses_break_statement() {
    let result = parse_stmt("break;");
    let stmt = get_stmt(&result, 0);

    match stmt {
      ASTStatement::Break(_) => {},
      other => panic!("expected break, got {:?}", other),
    }
  }

  #[test]
  fn parses_continue_statement() {
    let result = parse_stmt("continue;");
    let stmt = get_stmt(&result, 0);

    match stmt {
      ASTStatement::Continue(_) => {},
      other => panic!("expected continue, got {:?}", other),
    }
  }

  #[test]
  fn parses_nested_block() {
    let result = parse_stmt("{ let x: i32 = 1; }");
    let stmt = get_stmt(&result, 0);

    match stmt {
      ASTStatement::Block(block) => {
        assert_eq!(block.statements.len(), 1);
      },
      other => panic!("expected block, got {:?}", other),
    }
  }

  #[test]
  fn parses_multiple_statements() {
    let result = parse_stmt("let x: i32 = 1; let y: i32 = 2;");
    let stmt1 = get_stmt(&result, 0);
    let stmt2 = get_stmt(&result, 1);

    match (stmt1, stmt2) {
      (ASTStatement::Variable(v1), ASTStatement::Variable(v2)) => {
        assert_eq!(symbol_name(&result, &v1.name), "x");
        assert_eq!(symbol_name(&result, &v2.name), "y");
      },
      _ => panic!("expected two variables"),
    }
  }
}
