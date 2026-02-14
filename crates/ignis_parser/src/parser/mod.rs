use std::{cell::RefCell, rc::Rc};

mod declarations;
mod expression;
mod recovery;
mod statement;
mod type_syntax;

use ignis_diagnostics::message::{DiagnosticMessage, Expected};
use ignis_token::{token::Token, token_types::TokenType};
use ignis_ast::{ASTNode, NodeId, attribute::ASTAttribute, statements::ASTStatement, expressions::ASTExpression};
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
  recursion_depth: usize,

  /// When the parser splits a `>>` (RightShift) token to consume the first `>`
  /// in a generic context, this flag is set so that the next `>` consumption
  /// reads from the split remainder instead of advancing the cursor.
  pending_greater: bool,
  /// Span of the pending `>` half when `pending_greater` is true.
  pending_greater_span: Option<Span>,

  /// Pending outer doc comment (`///`) to be attached to the next declaration.
  pending_doc: Option<String>,
  /// Pending inner doc comment (`//!`) to be attached to the enclosing item.
  pending_inner_doc: Option<String>,
  /// Pending attributes (`@packed`, `@aligned(n)`, etc.) to be attached to the next declaration.
  pending_attrs: Vec<ASTAttribute>,
}

pub(crate) const MAX_RECURSION_DEPTH: usize = 500;

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
      recursion_depth: 0,
      pending_greater: false,
      pending_greater_span: None,
      pending_doc: None,
      pending_inner_doc: None,
      pending_attrs: Vec::new(),
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

  /// Checks whether a `>` is available, considering the pending split state
  /// and `>>` tokens that could be split.
  fn at_greater(&self) -> bool {
    if self.pending_greater {
      return true;
    }

    let ty = self.peek().type_;
    ty == TokenType::Greater || ty == TokenType::RightShift
  }

  /// Consumes a single `>` if available, handling three cases:
  /// 1. A pending `>` from a previous `>>` split
  /// 2. A normal `>` token
  /// 3. A `>>` token — consumes it and sets `pending_greater` for the second `>`
  #[allow(dead_code)]
  fn eat_greater(&mut self) -> bool {
    if self.pending_greater {
      self.pending_greater = false;
      self.pending_greater_span = None;
      return true;
    }

    if self.at(TokenType::Greater) {
      self.bump();
      return true;
    }

    if self.at(TokenType::RightShift) {
      let span = self.peek().span.clone();
      self.bump();
      self.pending_greater = true;
      self.pending_greater_span = Some(span);
      return true;
    }

    false
  }

  /// Like `expect(TokenType::Greater)` but handles `>>` splitting for nested generics.
  fn expect_greater(&mut self) -> ParserResult<Span> {
    if self.pending_greater {
      self.pending_greater = false;
      let span = self
        .pending_greater_span
        .take()
        .unwrap_or_else(|| self.peek().span.clone());
      return Ok(span);
    }

    if self.at(TokenType::Greater) {
      let span = self.peek().span.clone();
      self.bump();
      return Ok(span);
    }

    if self.at(TokenType::RightShift) {
      let span = self.peek().span.clone();
      self.bump();
      self.pending_greater = true;
      self.pending_greater_span = Some(span.clone());
      return Ok(span);
    }

    Err(DiagnosticMessage::ExpectedToken {
      expected: Expected::Token(TokenType::Greater),
      at: self.peek().span.clone(),
    })
  }

  /// Advances the cursor to the next token, skipping comments.
  fn bump(&mut self) -> &Token {
    let cursor = self.cursor;
    self.cursor += 1;

    self.skip_comments();

    (self.tokens.get(cursor).unwrap()) as _
  }

  /// Skip over comment tokens, capturing doc comments for the next declaration.
  ///
  /// - `///` outer doc comments are stored in `pending_doc`
  /// - `//!` inner doc comments are stored in `pending_inner_doc`
  ///
  /// Only updates `pending_doc`/`pending_inner_doc` when comment tokens are actually
  /// encountered. When no comments appear between tokens, pending docs are preserved
  /// so that attributes (`@extension(...)`) don't erase a preceding doc comment.
  fn skip_comments(&mut self) {
    let mut doc_lines = Vec::new();
    let mut inner_doc_lines = Vec::new();
    let mut saw_comments = false;

    while self.cursor < self.tokens.len() {
      let token = self.tokens.get(self.cursor).unwrap();
      match token.type_ {
        TokenType::Comment | TokenType::MultiLineComment => {
          saw_comments = true;
          doc_lines.clear();
          self.cursor += 1;
        },
        TokenType::DocComment => {
          saw_comments = true;
          let content = Self::extract_doc_content(&token.lexeme);
          doc_lines.push(content);
          self.cursor += 1;
        },
        TokenType::InnerDocComment => {
          saw_comments = true;
          let content = Self::extract_inner_doc_content(&token.lexeme);
          inner_doc_lines.push(content);
          self.cursor += 1;
        },
        _ => break,
      }
    }

    if !saw_comments {
      return;
    }

    if doc_lines.is_empty() {
      self.pending_doc = None;
    } else {
      self.pending_doc = Some(doc_lines.join("  \n"));
    }

    if inner_doc_lines.is_empty() {
      self.pending_inner_doc = None;
    } else {
      self.pending_inner_doc = Some(inner_doc_lines.join("  \n"));
    }
  }

  /// Extract the actual documentation content from an outer doc comment.
  /// Handles both `/// comment` and `/** comment */` styles.
  /// Preserves indentation for ASCII art diagrams in code blocks.
  fn extract_doc_content(lexeme: &str) -> String {
    if let Some(after_slashes) = lexeme.strip_prefix("///") {
      // Line doc comment: /// content
      // Strip "///" and optionally one space, but preserve remaining indentation
      if let Some(content) = after_slashes.strip_prefix(' ') {
        content.trim_end().to_string()
      } else {
        after_slashes.trim_end().to_string()
      }
    } else if lexeme.starts_with("/**") && lexeme.ends_with("*/") {
      let inner = &lexeme[3..lexeme.len() - 2];
      // Strip leading asterisks but preserve indentation after the asterisk
      inner
        .lines()
        .map(|line| {
          let trimmed = line.trim_start();
          if let Some(after_star) = trimmed.strip_prefix('*') {
            // Strip one space after *, preserve rest
            if let Some(content) = after_star.strip_prefix(' ') {
              content.trim_end()
            } else {
              after_star.trim_end()
            }
          } else {
            line.trim_end()
          }
        })
        .collect::<Vec<_>>()
        .join("\n")
        .trim()
        .to_string()
    } else {
      lexeme.to_string()
    }
  }

  /// Extract the actual documentation content from an inner doc comment.
  /// Handles both `//! comment` and `/*! comment */` styles.
  /// Preserves indentation for ASCII art diagrams in code blocks.
  fn extract_inner_doc_content(lexeme: &str) -> String {
    if let Some(after_prefix) = lexeme.strip_prefix("//!") {
      // Line inner doc comment: //! content
      // Strip "//!" and optionally one space, but preserve remaining indentation
      if let Some(content) = after_prefix.strip_prefix(' ') {
        content.trim_end().to_string()
      } else {
        after_prefix.trim_end().to_string()
      }
    } else if lexeme.starts_with("/*!") && lexeme.ends_with("*/") {
      let inner = &lexeme[3..lexeme.len() - 2];
      // Strip leading asterisks but preserve indentation after the asterisk
      inner
        .lines()
        .map(|line| {
          let trimmed = line.trim_start();
          if let Some(after_star) = trimmed.strip_prefix('*') {
            // Strip one space after *, preserve rest
            if let Some(content) = after_star.strip_prefix(' ') {
              content.trim_end()
            } else {
              after_star.trim_end()
            }
          } else {
            line.trim_end()
          }
        })
        .collect::<Vec<_>>()
        .join("\n")
        .trim()
        .to_string()
    } else {
      lexeme.to_string()
    }
  }

  /// Take the pending outer doc comment, clearing it for the next declaration.
  fn take_pending_doc(&mut self) -> Option<String> {
    self.pending_doc.take()
  }

  /// Take the pending inner doc comment, clearing it.
  fn take_pending_inner_doc(&mut self) -> Option<String> {
    self.pending_inner_doc.take()
  }

  fn take_pending_attrs(&mut self) -> Vec<ASTAttribute> {
    std::mem::take(&mut self.pending_attrs)
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
    self.nodes.get(id)
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
    matches!(self.nodes.get(id), ASTNode::Expression(ASTExpression::Literal(_)))
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
/// @param {str} a - Prefix string.
/// @param {str} b - Suffix string.
/// @return {str} Newly allocated concatenation.
function concat(a: str, b: str): str {
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
    assert!(doc.contains("@param {str} a - Prefix string"));
    assert!(doc.contains("@param {str} b - Suffix string"));
    assert!(doc.contains("@return {str} Newly allocated"));

    // Lines should be separate (not run together)
    assert!(!doc.contains("new string.@param"), "lines should not run together");
  }

  #[test]
  fn extract_doc_content_handles_line_comments() {
    let content = IgnisParser::extract_doc_content("/// This is a doc comment");
    assert_eq!(content, "This is a doc comment");

    // Leading whitespace (after first space) is preserved for ASCII art alignment
    // Only trailing whitespace is trimmed
    let content = IgnisParser::extract_doc_content("///   Indented content   ");
    assert_eq!(content, "  Indented content");

    // No space after /// - content starts immediately
    let content = IgnisParser::extract_doc_content("///No space");
    assert_eq!(content, "No space");
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
    assert_eq!(content, "Line one\nLine two\nLine three");

    // Should handle without leading asterisks
    let no_asterisks = r#"/**
Line one
Line two
*/"#;
    let content = IgnisParser::extract_doc_content(no_asterisks);
    assert_eq!(content, "Line one\nLine two");
  }

  #[test]
  fn extract_inner_doc_content_handles_line_comments() {
    let content = IgnisParser::extract_inner_doc_content("//! This is an inner doc comment");
    assert_eq!(content, "This is an inner doc comment");

    // Leading whitespace (after first space) is preserved for ASCII art alignment
    // Only trailing whitespace is trimmed
    let content = IgnisParser::extract_inner_doc_content("//!   Indented content   ");
    assert_eq!(content, "  Indented content");

    // No space after //! - content starts immediately
    let content = IgnisParser::extract_inner_doc_content("//!No space");
    assert_eq!(content, "No space");
  }

  #[test]
  fn extract_inner_doc_content_handles_block_comments() {
    let content = IgnisParser::extract_inner_doc_content("/*! Single line inner block */");
    assert_eq!(content, "Single line inner block");

    let multiline = r#"/*!
 * Module documentation
 * Second line
 */"#;
    let content = IgnisParser::extract_inner_doc_content(multiline);
    assert_eq!(content, "Module documentation\nSecond line");
  }

  #[test]
  fn doc_comment_on_export_function() {
    let source = r#"
/// Exported function documentation
export function foo(): void { }
"#;

    let result = parse(source);

    assert!(
      result.diagnostics.is_empty(),
      "unexpected diagnostics: {:?}",
      result.diagnostics
    );

    let root = first_root(&result);
    match root {
      ASTNode::Statement(ASTStatement::Export(export)) => match export {
        ignis_ast::statements::ASTExport::Declaration { decl, .. } => {
          let inner = result.nodes.get(decl);
          match inner {
            ASTNode::Statement(ASTStatement::Function(func)) => {
              let doc = func.signature.doc.as_ref().expect("exported function should have doc");
              assert!(doc.contains("Exported function documentation"));
            },
            _ => panic!("expected function inside export"),
          }
        },
        _ => panic!("expected export declaration"),
      },
      _ => panic!("expected export statement"),
    }
  }

  #[test]
  fn doc_comment_on_static_method() {
    use ignis_ast::statements::{ASTRecordItem, ASTStatement};

    let source = r#"
record Foo {
  /// Static method documentation
  static bar(): void { }
}
"#;

    let result = parse(source);

    assert!(
      result.diagnostics.is_empty(),
      "unexpected diagnostics: {:?}",
      result.diagnostics
    );

    let root = first_root(&result);
    match root {
      ASTNode::Statement(ASTStatement::Record(rec)) => {
        assert_eq!(rec.items.len(), 1);
        match &rec.items[0] {
          ASTRecordItem::Method(method) => {
            let doc = method.doc.as_ref().expect("static method should have doc");
            assert!(doc.contains("Static method documentation"));
          },
          _ => panic!("expected method"),
        }
      },
      _ => panic!("expected record statement"),
    }
  }

  #[test]
  fn doc_comment_on_record_field() {
    use ignis_ast::statements::{ASTRecordItem, ASTStatement};

    let source = r#"
record Point {
  /// X coordinate
  x: i32;
  /// Y coordinate
  y: i32;
}
"#;

    let result = parse(source);

    assert!(
      result.diagnostics.is_empty(),
      "unexpected diagnostics: {:?}",
      result.diagnostics
    );

    let root = first_root(&result);
    match root {
      ASTNode::Statement(ASTStatement::Record(rec)) => {
        assert_eq!(rec.items.len(), 2);
        match &rec.items[0] {
          ASTRecordItem::Field(field) => {
            let doc = field.doc.as_ref().expect("field x should have doc");
            assert!(doc.contains("X coordinate"));
          },
          _ => panic!("expected field"),
        }
        match &rec.items[1] {
          ASTRecordItem::Field(field) => {
            let doc = field.doc.as_ref().expect("field y should have doc");
            assert!(doc.contains("Y coordinate"));
          },
          _ => panic!("expected field"),
        }
      },
      _ => panic!("expected record statement"),
    }
  }

  #[test]
  fn doc_comment_on_enum_variant() {
    use ignis_ast::statements::{ASTEnumItem, ASTStatement};

    let source = r#"
enum Color {
  /// Red color
  Red,
  /// Green color
  Green,
  /// Blue color
  Blue
}
"#;

    let result = parse(source);

    assert!(
      result.diagnostics.is_empty(),
      "unexpected diagnostics: {:?}",
      result.diagnostics
    );

    let root = first_root(&result);
    match root {
      ASTNode::Statement(ASTStatement::Enum(en)) => {
        assert_eq!(en.items.len(), 3);
        match &en.items[0] {
          ASTEnumItem::Variant(variant) => {
            let doc = variant.doc.as_ref().expect("Red variant should have doc");
            assert!(doc.contains("Red color"));
          },
          _ => panic!("expected variant"),
        }
      },
      _ => panic!("expected enum statement"),
    }
  }

  #[test]
  fn doc_comment_survives_attributes() {
    use ignis_ast::statements::ASTStatement;

    let source = r#"
/// Documented function with attribute
@deprecated("use bar instead")
function foo(): void { }
"#;

    let result = parse(source);

    assert!(
      result.diagnostics.is_empty(),
      "unexpected diagnostics: {:?}",
      result.diagnostics
    );

    let root = first_root(&result);
    match root {
      ASTNode::Statement(ASTStatement::Function(func)) => {
        let doc = func
          .signature
          .doc
          .as_ref()
          .expect("function with attribute should have doc");
        assert!(doc.contains("Documented function with attribute"));
      },
      _ => panic!("expected function statement"),
    }
  }

  #[test]
  fn doc_comment_survives_multiple_attributes() {
    use ignis_ast::statements::ASTStatement;

    let source = r#"
namespace Ns {
  /// Documented with two attributes
  @deprecated
  @cold
  function baz(): void { }
}
"#;

    let result = parse(source);

    assert!(
      result.diagnostics.is_empty(),
      "unexpected diagnostics: {:?}",
      result.diagnostics
    );

    let root = first_root(&result);
    match root {
      ASTNode::Statement(ASTStatement::Namespace(ns)) => {
        let func_node = result.nodes.get(&ns.items[0]);
        match func_node {
          ASTNode::Statement(ASTStatement::Function(func)) => {
            let doc = func
              .signature
              .doc
              .as_ref()
              .expect("function with multiple attributes should have doc");
            assert!(doc.contains("Documented with two attributes"));
          },
          _ => panic!("expected function inside namespace"),
        }
      },
      _ => panic!("expected namespace statement"),
    }
  }

  #[test]
  fn inner_doc_comment_on_namespace() {
    use ignis_ast::statements::ASTStatement;

    let source = r#"
/// Outer doc for namespace
namespace Math {
  //! This module provides mathematical functions.
  //! It includes basic arithmetic operations.

  function add(a: i32, b: i32): i32 { return a + b; }
}
"#;

    let result = parse(source);

    assert!(
      result.diagnostics.is_empty(),
      "unexpected diagnostics: {:?}",
      result.diagnostics
    );

    let root = first_root(&result);
    match root {
      ASTNode::Statement(ASTStatement::Namespace(ns)) => {
        // Check outer doc
        let outer_doc = ns.doc.as_ref().expect("namespace should have outer doc");
        assert!(outer_doc.contains("Outer doc for namespace"));

        // Check inner doc
        let inner_doc = ns.inner_doc.as_ref().expect("namespace should have inner doc");
        assert!(inner_doc.contains("This module provides mathematical functions"));
        assert!(inner_doc.contains("basic arithmetic operations"));
      },
      _ => panic!("expected namespace statement"),
    }
  }
}
