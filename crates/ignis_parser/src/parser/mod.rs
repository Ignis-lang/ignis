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

  /// Pending doc comment to be attached to the next declaration.
  pending_doc: Option<String>,
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
      pending_doc: None,
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

  fn peek_nth(
    &self,
    n: usize,
  ) -> &Token {
    self.tokens.get(self.cursor + n).unwrap_or(self.tokens.last().unwrap())
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

  /// Skip over comment tokens, capturing doc comments for the next declaration.
  fn skip_comments(&mut self) {
    let mut doc_lines = Vec::new();

    while self.cursor < self.tokens.len() {
      let token = self.tokens.get(self.cursor).unwrap();
      match token.type_ {
        TokenType::Comment | TokenType::MultiLineComment => {
          // Regular comments - skip and clear any pending doc
          doc_lines.clear();
          self.cursor += 1;
        },
        TokenType::DocComment => {
          // Doc comment - collect the content
          let content = Self::extract_doc_content(&token.lexeme);
          doc_lines.push(content);
          self.cursor += 1;
        },
        _ => break,
      }
    }

    // Store the collected doc comments
    if doc_lines.is_empty() {
      self.pending_doc = None;
    } else {
      // Join with "  \n" (two spaces + newline) to create hard line breaks in Markdown
      self.pending_doc = Some(doc_lines.join("  \n"));
    }
  }

  /// Extract the actual documentation content from a doc comment.
  /// Handles both `/// comment` and `/** comment */` styles.
  fn extract_doc_content(lexeme: &str) -> String {
    if lexeme.starts_with("///") {
      // Line doc comment: /// content
      lexeme[3..].trim().to_string()
    } else if lexeme.starts_with("/**") && lexeme.ends_with("*/") {
      let inner = &lexeme[3..lexeme.len() - 2];
      // Strip leading asterisks and join with hard line breaks
      inner
        .lines()
        .map(|line| {
          let trimmed = line.trim();
          if trimmed.starts_with('*') {
            trimmed[1..].trim()
          } else {
            trimmed
          }
        })
        .collect::<Vec<_>>()
        .join("  \n")
        .trim()
        .to_string()
    } else {
      lexeme.to_string()
    }
  }

  /// Take the pending doc comment, clearing it for the next declaration.
  fn take_pending_doc(&mut self) -> Option<String> {
    self.pending_doc.take()
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

      // ternary (right-assoc)
      TokenType::QuestionMark => (20, 19),

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

  #[test]
  fn parses_line_doc_comments_with_line_breaks() {
    let source = r#"
/// Concatenates two strings into a new string.
/// @param {string} a - Prefix string.
/// @param {string} b - Suffix string.
/// @return {string} Newly allocated concatenation.
function concat(a: string, b: string): string {
  return a;
}
"#;

    let result = parse(source);

    assert!(
      result.diagnostics.is_empty(),
      "unexpected diagnostics: {:?}",
      result.diagnostics
    );
    assert_eq!(result.roots.len(), 1);

    let func = get_function(first_root(&result));
    let doc = func.signature.doc.as_ref().expect("function should have doc comment");

    // Should contain hard line breaks (two spaces + newline) to preserve line structure
    assert!(doc.contains("  \n"), "doc comment should contain hard line breaks");

    // Should contain all the expected content
    assert!(doc.contains("Concatenates two strings"));
    assert!(doc.contains("@param {string} a - Prefix string"));
    assert!(doc.contains("@param {string} b - Suffix string"));
    assert!(doc.contains("@return {string} Newly allocated"));

    // Lines should be separate (not run together)
    assert!(!doc.contains("new string.@param"), "lines should not run together");
  }

  #[test]
  fn extract_doc_content_handles_line_comments() {
    let content = IgnisParser::extract_doc_content("/// This is a doc comment");
    assert_eq!(content, "This is a doc comment");

    let content = IgnisParser::extract_doc_content("///   Whitespace should be trimmed   ");
    assert_eq!(content, "Whitespace should be trimmed");
  }

  #[test]
  fn extract_doc_content_handles_block_comments() {
    let content = IgnisParser::extract_doc_content("/** Single line block */");
    assert_eq!(content, "Single line block");

    let multiline = r#"/**
 * Line one
 * Line two
 * Line three
 */"#;
    let content = IgnisParser::extract_doc_content(multiline);
    assert_eq!(content, "Line one  \nLine two  \nLine three");

    // Should handle without leading asterisks
    let no_asterisks = r#"/**
Line one
Line two
*/"#;
    let content = IgnisParser::extract_doc_content(no_asterisks);
    assert_eq!(content, "Line one  \nLine two");
  }
}
