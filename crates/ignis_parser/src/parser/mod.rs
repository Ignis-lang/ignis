use std::{cell::RefCell, rc::Rc};

mod expression;

use ignis_diagnostics::message::{DiagnosticMessage, Expected};
use ignis_token::{token::Token, token_types::TokenType};
use ignis_ast::{ASTNode, NodeId, statements::ASTStatement, expressions::ASTExpression};
use ignis_type::{
  Store,
  span::Span,
  symbol::{SymbolId, SymbolTable},
};

pub type ParserResult<T> = Result<T, DiagnosticMessage>;

type BindingPower = (u16, u16);

pub struct IgnisParser {
  diagnostics: Vec<DiagnosticMessage>,
  tokens: Vec<Token>,
  cursor: usize,

  nodes: Store<ASTNode>,
  symbols: Rc<RefCell<SymbolTable>>,
}

impl IgnisParser {
  pub fn new(
    tokens: Vec<Token>,
    symbols: Rc<RefCell<SymbolTable>>,
  ) -> Self {
    Self {
      tokens,
      diagnostics: Vec::new(),
      cursor: 0,
      nodes: Store::new(),
      symbols,
    }
  }

  pub fn parse(&mut self) -> Result<(Store<ASTNode>, Vec<NodeId>), Vec<DiagnosticMessage>> {
    let roots = self.parse_program().map_err(|e| vec![e])?;

    Ok((self.nodes.clone(), roots))
  }

  fn parse_program(&mut self) -> ParserResult<Vec<NodeId>> {
    let mut statements = Vec::new();

    while !self.at(TokenType::Eof) {
      let stmt = self.parse_statement()?;
      statements.push(stmt);

      self.eat(TokenType::SemiColon);
    }

    Ok(statements)
  }

  fn parse_statement(&mut self) -> ParserResult<NodeId> {
    self.parse_expression(0)
  }

  fn peek(&self) -> &Token {
    self.tokens.get(self.cursor).unwrap()
  }

  fn previous(&self) -> &Token {
    self.tokens.get(self.cursor - 1).unwrap()
  }

  fn at(
    &self,
    token_type: TokenType,
  ) -> bool {
    self.tokens.get(self.cursor).unwrap().type_ == token_type
  }

  fn eat(
    &mut self,
    token_type: TokenType,
  ) -> bool {
    if self.at(token_type) {
      self.bump();
      true
    } else {
      false
    }
  }

  fn expect(
    &mut self,
    token_type: TokenType,
  ) -> ParserResult<&Token> {
    if self.at(token_type) {
      return Ok(self.bump());
    }

    Err(DiagnosticMessage::ExpectedToken {
      expected: Expected::Token(token_type),
      at: self.peek().span.clone(),
    })
  }

  /// Advances the cursor to the next token.
  fn bump(&mut self) -> &Token {
    let token = self.tokens.get(self.cursor).unwrap();
    self.cursor += 1;
    token
  }

  fn allocate_statement(
    &mut self,
    statement: ASTStatement,
  ) -> NodeId {
    self.nodes.alloc(ASTNode::Statement(statement))
  }

  fn get_node(
    &self,
    id: &NodeId,
  ) -> &ASTNode {
    &self.nodes.get(id)
  }

  fn get_span(
    &self,
    id: &NodeId,
  ) -> &Span {
    self.nodes.get(id).span()
  }

  fn is_literal(
    &self,
    id: &NodeId,
  ) -> bool {
    match self.nodes.get(id) {
      ASTNode::Expression(ASTExpression::Literal(_)) => true,
      _ => false,
    }
  }

  #[inline]
  fn insert_symbol(
    &mut self,
    token: &Token,
  ) -> SymbolId {
    self.symbols.borrow_mut().intern(&token.lexeme)
  }

  #[inline]
  /// Returns the binding power of an infix operator token.
  /// If the left value is higher than the right value, the operator is left-associative.
  /// If the left value is lower than the right value, the operator is right-associative.
  pub fn binding_powers(
    &self,
    op: &TokenType,
  ) -> Option<BindingPower> {
    let p = match op {
      // unary prefix operators (right-assoc)
      TokenType::Increment | TokenType::Decrement | TokenType::Bang | TokenType::Tilde => (0, 80),

      // cast (right-assoc)
      TokenType::As => (75, 74),

      // multiplication (left)
      TokenType::Asterisk | TokenType::Slash | TokenType::Mod => (60, 61),

      // addition/subtraction (left)
      TokenType::Plus | TokenType::Minus => (50, 51),

      // shift (left)
      TokenType::LeftShift | TokenType::RightShift => (45, 46),

      // comparison (left)
      TokenType::Less | TokenType::Greater | TokenType::LessEqual | TokenType::GreaterEqual => (40, 41),

      // equality (left)
      TokenType::Equal | TokenType::BangEqual => (39, 40),

      // bitwise and/xor/or (left)
      TokenType::Ampersand => (35, 36),
      TokenType::Caret => (34, 35),
      TokenType::Pipe => (33, 34),

      // logical (left)
      TokenType::And => (30, 31),
      TokenType::Or => (29, 30),

      // assignment (right-assoc)
      TokenType::EqualEqual
      | TokenType::AddAssign
      | TokenType::SubtractAssign
      | TokenType::MulAssign
      | TokenType::DivAssign
      | TokenType::ModAssign
      | TokenType::AndAssign
      | TokenType::OrAssign
      | TokenType::NotAssign
      | TokenType::XorAssign
      | TokenType::LeftShiftAssign
      | TokenType::RightShiftAssign => (10, 9),

      _ => return None,
    };
    Some(p)
  }
}
