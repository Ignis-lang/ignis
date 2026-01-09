use std::{cell::RefCell, rc::Rc};

mod declarations;
mod expression;
mod recovery;
mod statement;
mod type_syntax;

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

  recovery: bool,
}

impl IgnisParser {
  pub fn new(
    tokens: Vec<Token>,
    symbols: Rc<RefCell<SymbolTable>>,
  ) -> Self {
    let mut parser = IgnisParser {
      tokens,
      cursor: 0,
      nodes: Store::new(),
      diagnostics: Vec::new(),
      symbols,
      recovery: true,
    };

    parser.skip_comments();
    parser
  }

  /// Parse a comma-separated list between delimiters
  /// Generic helper for parsing lists like: (item, item, item)
  ///
  /// # Usage Examples
  /// ```ignore
  /// // Function parameters: (x: int, y: int)
  /// let params = self.parse_delimited_list(
  ///   TokenType::LeftParen,
  ///   TokenType::RightParen,
  ///   TokenType::Comma,
  ///   |parser| parser.parse_param(),
  /// )?;
  ///
  /// // Function arguments: foo(1, 2, 3)
  /// let args = self.parse_delimited_list(
  ///   TokenType::LeftParen,
  ///   TokenType::RightParen,
  ///   TokenType::Comma,
  ///   |parser| parser.parse_expression(0),
  /// )?;
  ///
  /// // Vector elements: [1, 2, 3]
  /// let elements = self.parse_delimited_list(
  ///   TokenType::LeftBrack,
  ///   TokenType::RightBrack,
  ///   TokenType::Comma,
  ///   |parser| parser.parse_expression(0),
  /// )?;
  ///
  /// // Tuple elements: (1, "hello", true)
  /// let tuple = self.parse_delimited_list(
  ///   TokenType::LeftParen,
  ///   TokenType::RightParen,
  ///   TokenType::Comma,
  ///   |parser| parser.parse_expression(0),
  /// )?;
  /// ```
  pub(crate) fn parse_delimited_list<T, F>(
    &mut self,
    opening: TokenType,
    closing: TokenType,
    separator: TokenType,
    mut parse_item: F,
  ) -> ParserResult<Vec<T>>
  where
    F: FnMut(&mut Self) -> ParserResult<T>,
  {
    self.expect(opening)?;
    let mut items = Vec::new();

    if self.at(closing) {
      self.expect(closing)?;
      return Ok(items);
    }

    items.push(parse_item(self)?);

    while self.eat(separator) {
      if self.at(closing) {
        break;
      }
      items.push(parse_item(self)?);
    }

    self.expect(closing)?;
    Ok(items)
  }

  pub fn parse(&mut self) -> Result<(Store<ASTNode>, Vec<NodeId>), Vec<DiagnosticMessage>> {
    match self.parse_program() {
      Ok(statements) => {
        if self.diagnostics.is_empty() {
          Ok((self.nodes.clone(), statements))
        } else {
          Err(self.diagnostics.clone())
        }
      },
      Err(diagnostic) => {
        self.diagnostics.push(diagnostic);
        Err(self.diagnostics.clone())
      },
    }
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

  /// Advances the cursor to the next token, skipping comments.
  fn bump(&mut self) -> &Token {
    let cursor = self.cursor;
    self.cursor += 1;

    self.skip_comments();

    let token = self.tokens.get(cursor).unwrap();

    token
  }

  /// Skip over comment tokens
  fn skip_comments(&mut self) {
    while self.cursor < self.tokens.len() {
      let token_type = self.tokens.get(self.cursor).unwrap().type_;
      if matches!(
        token_type,
        TokenType::Comment | TokenType::MultiLineComment | TokenType::DocComment
      ) {
        self.cursor += 1;
      } else {
        break;
      }
    }
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
  pub(crate) fn binding_powers(
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
      TokenType::EqualEqual | TokenType::BangEqual => (39, 40),

      // bitwise and/xor/or (left)
      TokenType::Ampersand => (35, 36),
      TokenType::Caret => (34, 35),
      TokenType::Pipe => (33, 34),

      // logical (left)
      TokenType::And => (30, 31),
      TokenType::Or => (29, 30),

      // assignment (right-assoc)
      TokenType::Equal
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

#[cfg(test)]
mod tests {
  use super::*;
  use crate::lexer::IgnisLexer;
  use ignis_ast::statements::{function::ASTFunction, ASTStatement};
  use ignis_diagnostics::message::DiagnosticMessage;
  use ignis_type::file::SourceMap;

  struct ParseResult {
    nodes: Store<ASTNode>,
    roots: Vec<NodeId>,
    symbols: Rc<RefCell<SymbolTable>>,
    diagnostics: Vec<DiagnosticMessage>,
  }

  fn parse(source: &str) -> ParseResult {
    let mut sm = SourceMap::new();
    let file_id = sm.add_file("test.ign", source.to_string());

    let mut lexer = IgnisLexer::new(file_id, source);
    lexer.scan_tokens();

    let symbols = Rc::new(RefCell::new(SymbolTable::new()));
    let mut parser = IgnisParser::new(lexer.tokens, symbols.clone());

    match parser.parse() {
      Ok((nodes, roots)) => ParseResult {
        nodes,
        roots,
        symbols,
        diagnostics: Vec::new(),
      },
      Err(diagnostics) => ParseResult {
        nodes: parser.nodes.clone(),
        roots: Vec::new(),
        symbols,
        diagnostics,
      },
    }
  }

  fn first_root(result: &ParseResult) -> &ASTNode {
    result.nodes.get(&result.roots[0])
  }

  fn symbol_name(
    result: &ParseResult,
    id: &SymbolId,
  ) -> String {
    result.symbols.borrow().get(id).to_string()
  }

  fn get_function<'a>(node: &'a ASTNode) -> &'a ASTFunction {
    match node {
      ASTNode::Statement(ASTStatement::Function(func)) => func,
      _ => panic!("expected function, got {:?}", node),
    }
  }

  #[test]
  fn parses_empty_function() {
    let result = parse("function foo(): void { }");

    assert!(
      result.diagnostics.is_empty(),
      "unexpected diagnostics: {:?}",
      result.diagnostics
    );
    assert_eq!(result.roots.len(), 1);

    let func = get_function(first_root(&result));
    let name = symbol_name(&result, &func.signature.name);
    assert_eq!(name, "foo");
    assert!(func.body.is_some());
  }

  #[test]
  fn parses_function_with_parameters() {
    let result = parse("function add(a: i32, b: i32): i32 { return a; }");

    assert!(
      result.diagnostics.is_empty(),
      "unexpected diagnostics: {:?}",
      result.diagnostics
    );
    assert_eq!(result.roots.len(), 1);

    let func = get_function(first_root(&result));
    let name = symbol_name(&result, &func.signature.name);
    assert_eq!(name, "add");
    assert_eq!(func.signature.parameters.len(), 2);

    let param_a = &func.signature.parameters[0];
    let param_b = &func.signature.parameters[1];
    assert_eq!(symbol_name(&result, &param_a.name), "a");
    assert_eq!(symbol_name(&result, &param_b.name), "b");
  }

  #[test]
  fn parses_multiple_declarations() {
    let result = parse(
      r#"
        function foo(): void { }
        function bar(): void { }
      "#,
    );

    assert!(
      result.diagnostics.is_empty(),
      "unexpected diagnostics: {:?}",
      result.diagnostics
    );
    assert_eq!(result.roots.len(), 2);

    let func1 = get_function(result.nodes.get(&result.roots[0]));
    let func2 = get_function(result.nodes.get(&result.roots[1]));

    assert_eq!(symbol_name(&result, &func1.signature.name), "foo");
    assert_eq!(symbol_name(&result, &func2.signature.name), "bar");
  }

  #[test]
  fn reports_missing_function_name() {
    let result = parse("function (): void { }");

    assert!(!result.diagnostics.is_empty(), "expected diagnostic for missing function name");
  }

  #[test]
  fn reports_missing_return_type() {
    let result = parse("function foo() { }");

    assert!(!result.diagnostics.is_empty(), "expected diagnostic for missing return type");
  }
}
