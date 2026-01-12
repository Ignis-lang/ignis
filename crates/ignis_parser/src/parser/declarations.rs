use ignis_ast::{
  NodeId,
  statements::{
    ASTStatement,
    const_statement::ASTConstant,
    export_statement::ASTExport,
    extern_statement::ASTExtern,
    function::{ASTFunction, ASTFunctionSignature},
    import_statement::ASTImport,
    namespace_statement::ASTNamespace,
  },
};
use ignis_diagnostics::message::DiagnosticMessage;
use ignis_token::token_types::TokenType;
use ignis_type::{span::Span, symbol::SymbolId};

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
      TokenType::Namespace => self.parse_namespace_declaration(),
      _ => Err(DiagnosticMessage::UnexpectedToken {
        at: self.peek().span.clone(),
      }),
    }
  }

  /// Parsea `foo` o `foo::bar::baz` → (Vec<SymbolId>, Span)
  fn parse_qualified_identifier(&mut self) -> ParserResult<(Vec<SymbolId>, Span)> {
    let first = self.expect(TokenType::Identifier)?.clone();
    let start = first.span.clone();
    let mut segments = vec![self.insert_symbol(&first)];
    let mut end = first.span.clone();

    while self.eat(TokenType::DoubleColon) {
      let ident = self.expect(TokenType::Identifier)?.clone();
      end = ident.span.clone();
      segments.push(self.insert_symbol(&ident));
    }

    let span = Span::merge(&start, &end);
    Ok((segments, span))
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

    let constant_statement = ASTConstant::new(name, type_annotation, Some(value), span);

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
      if self.at(TokenType::From) {
        break;
      }
    }

    self.expect(TokenType::From)?;
    let path_token = self.expect(TokenType::String)?.clone();
    let semicolon = self.expect(TokenType::SemiColon)?.clone();
    let span = Span::merge(&keyword.span, &semicolon.span);

    let import_path = path_token.lexeme.trim_matches('"').to_string();
    let import_statement = ASTImport::new(items, import_path, span);
    Ok(self.allocate_statement(ASTStatement::Import(import_statement)))
  }

  /// export <declaration> | export <identifier>;
  fn parse_export_declaration(&mut self) -> ParserResult<NodeId> {
    let keyword = self.expect(TokenType::Export)?.clone();

    if self.at(TokenType::Function)
      || self.at(TokenType::Const)
      || self.at(TokenType::Extern)
      || self.at(TokenType::Namespace)
    {
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
      TokenType::Namespace => self.parse_namespace_declaration(),
      _ => Err(DiagnosticMessage::UnexpectedToken {
        at: self.peek().span.clone(),
      }),
    }
  }

  /// extern path { function ...; const ...; }
  fn parse_extern_declaration(&mut self) -> ParserResult<NodeId> {
    let keyword = self.expect(TokenType::Extern)?.clone();
    let (path, _) = self.parse_qualified_identifier()?;
    self.expect(TokenType::LeftBrace)?;

    let mut items = Vec::new();
    while !self.at(TokenType::RightBrace) && !self.at(TokenType::Eof) {
      let item = match self.peek().type_ {
        TokenType::Function => self.parse_function_signature_only()?,
        TokenType::Const => self.parse_constant_signature_only()?,
        _ => {
          return Err(DiagnosticMessage::UnexpectedToken {
            at: self.peek().span.clone(),
          });
        },
      };
      items.push(item);
    }

    let close = self.expect(TokenType::RightBrace)?.clone();
    let span = Span::merge(&keyword.span, &close.span);

    let extern_block = ASTExtern::new(path, items, span);
    Ok(self.allocate_statement(ASTStatement::Extern(extern_block)))
  }

  /// namespace path { declarations... }
  fn parse_namespace_declaration(&mut self) -> ParserResult<NodeId> {
    let keyword = self.expect(TokenType::Namespace)?.clone();
    let (path, _) = self.parse_qualified_identifier()?;
    self.expect(TokenType::LeftBrace)?;

    let mut items = Vec::new();
    while !self.at(TokenType::RightBrace) && !self.at(TokenType::Eof) {
      let item = self.parse_namespace_item()?;
      items.push(item);
    }

    let close = self.expect(TokenType::RightBrace)?.clone();
    let span = Span::merge(&keyword.span, &close.span);

    let ns = ASTNamespace::new(path, items, span);
    Ok(self.allocate_statement(ASTStatement::Namespace(ns)))
  }

  fn parse_namespace_item(&mut self) -> ParserResult<NodeId> {
    match self.peek().type_ {
      TokenType::Function => self.parse_function_declaration(),
      TokenType::Const => self.parse_constant_declaration(),
      TokenType::Namespace => self.parse_namespace_declaration(),
      TokenType::Extern => self.parse_extern_declaration(),
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

    let constant_statement = ASTConstant::new(name, type_annotation, None, span);

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

    let function = ASTFunction::new(signature, None);

    Ok(self.allocate_statement(ASTStatement::Function(function)))
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

  fn parse(source: &str) -> ParseResult {
    let mut sm = SourceMap::new();
    let file_id = sm.add_file("test.ign", source.to_string());

    let mut lexer = IgnisLexer::new(file_id, source);
    lexer.scan_tokens();

    let symbols = Rc::new(RefCell::new(SymbolTable::new()));
    let mut parser = IgnisParser::new(lexer.tokens, symbols.clone());
    let (nodes, roots) = parser.parse().expect("parse failed");

    ParseResult { nodes, roots, symbols }
  }

  fn first_root(result: &ParseResult) -> &ASTStatement {
    match result.nodes.get(&result.roots[0]) {
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
  fn parses_import_single() {
    let result = parse("import foo from \"module\";");
    let stmt = first_root(&result);

    match stmt {
      ASTStatement::Import(imp) => {
        assert_eq!(imp.items.len(), 1);
        assert_eq!(symbol_name(&result, &imp.items[0]), "foo");
      },
      other => panic!("expected import, got {:?}", other),
    }
  }

  #[test]
  fn parses_import_multiple() {
    let result = parse("import foo, bar, baz from \"module\";");
    let stmt = first_root(&result);

    match stmt {
      ASTStatement::Import(imp) => {
        assert_eq!(imp.items.len(), 3);
        assert_eq!(symbol_name(&result, &imp.items[0]), "foo");
        assert_eq!(symbol_name(&result, &imp.items[1]), "bar");
        assert_eq!(symbol_name(&result, &imp.items[2]), "baz");
      },
      other => panic!("expected import, got {:?}", other),
    }
  }

  #[test]
  fn parses_import_trailing_comma() {
    let result = parse("import foo, bar, from \"module\";");
    let stmt = first_root(&result);

    match stmt {
      ASTStatement::Import(imp) => {
        assert_eq!(imp.items.len(), 2);
        assert_eq!(symbol_name(&result, &imp.items[0]), "foo");
        assert_eq!(symbol_name(&result, &imp.items[1]), "bar");
      },
      other => panic!("expected import, got {:?}", other),
    }
  }

  #[test]
  fn parses_export_function() {
    use ignis_ast::statements::ASTExport;
    let result = parse("export function foo(): void { }");
    let stmt = first_root(&result);

    match stmt {
      ASTStatement::Export(ASTExport::Declaration { .. }) => {},
      other => panic!("expected export declaration, got {:?}", other),
    }
  }

  #[test]
  fn parses_export_identifier() {
    use ignis_ast::statements::ASTExport;
    let result = parse("export foo;");
    let stmt = first_root(&result);

    match stmt {
      ASTStatement::Export(ASTExport::Name { name, .. }) => {
        assert_eq!(symbol_name(&result, name), "foo");
      },
      other => panic!("expected export name, got {:?}", other),
    }
  }

  #[test]
  fn parses_extern_block_with_function() {
    let result = parse("extern libc { function printf(fmt: string): void; }");
    let stmt = first_root(&result);

    match stmt {
      ASTStatement::Extern(ext) => {
        assert_eq!(ext.path.len(), 1);
        assert_eq!(symbol_name(&result, &ext.path[0]), "libc");
        assert_eq!(ext.items.len(), 1);

        let inner = result.nodes.get(&ext.items[0]);
        match inner {
          ASTNode::Statement(ASTStatement::Function(func)) => {
            assert_eq!(symbol_name(&result, &func.signature.name), "printf");
            assert!(func.body.is_none());
          },
          other => panic!("expected function inside extern, got {:?}", other),
        }
      },
      other => panic!("expected extern, got {:?}", other),
    }
  }

  #[test]
  fn parses_extern_block_with_const() {
    let result = parse("extern libc { const BUFFER_SIZE: i32; }");
    let stmt = first_root(&result);

    match stmt {
      ASTStatement::Extern(ext) => {
        assert_eq!(ext.path.len(), 1);
        assert_eq!(symbol_name(&result, &ext.path[0]), "libc");
        assert_eq!(ext.items.len(), 1);

        let inner = result.nodes.get(&ext.items[0]);
        match inner {
          ASTNode::Statement(ASTStatement::Constant(c)) => {
            assert_eq!(symbol_name(&result, &c.name), "BUFFER_SIZE");
            assert!(c.value.is_none());
          },
          other => panic!("expected constant inside extern, got {:?}", other),
        }
      },
      other => panic!("expected extern, got {:?}", other),
    }
  }

  #[test]
  fn parses_extern_block_multiple_items() {
    let result = parse("extern libc { function printf(fmt: string): i32; const BUFSIZ: i32; }");
    let stmt = first_root(&result);

    match stmt {
      ASTStatement::Extern(ext) => {
        assert_eq!(ext.items.len(), 2);
      },
      other => panic!("expected extern, got {:?}", other),
    }
  }

  #[test]
  fn parses_extern_qualified_path() {
    let result = parse("extern std::io { function read(): i32; }");
    let stmt = first_root(&result);

    match stmt {
      ASTStatement::Extern(ext) => {
        assert_eq!(ext.path.len(), 2);
        assert_eq!(symbol_name(&result, &ext.path[0]), "std");
        assert_eq!(symbol_name(&result, &ext.path[1]), "io");
      },
      other => panic!("expected extern, got {:?}", other),
    }
  }

  #[test]
  fn parses_namespace_basic() {
    let result = parse("namespace Math { function add(a: i32, b: i32): i32 { return a + b; } }");
    let stmt = first_root(&result);

    match stmt {
      ASTStatement::Namespace(ns) => {
        assert_eq!(ns.path.len(), 1);
        assert_eq!(symbol_name(&result, &ns.path[0]), "Math");
        assert_eq!(ns.items.len(), 1);
      },
      other => panic!("expected namespace, got {:?}", other),
    }
  }

  #[test]
  fn parses_namespace_nested() {
    let result = parse("namespace A { namespace B { function f(): void {} } }");
    let stmt = first_root(&result);

    match stmt {
      ASTStatement::Namespace(ns) => {
        assert_eq!(ns.path.len(), 1);
        assert_eq!(symbol_name(&result, &ns.path[0]), "A");
        assert_eq!(ns.items.len(), 1);

        let inner = result.nodes.get(&ns.items[0]);
        match inner {
          ASTNode::Statement(ASTStatement::Namespace(inner_ns)) => {
            assert_eq!(symbol_name(&result, &inner_ns.path[0]), "B");
          },
          other => panic!("expected namespace inside namespace, got {:?}", other),
        }
      },
      other => panic!("expected namespace, got {:?}", other),
    }
  }

  #[test]
  fn parses_namespace_qualified_path() {
    let result = parse("namespace Foo::Bar { function baz(): void {} }");
    let stmt = first_root(&result);

    match stmt {
      ASTStatement::Namespace(ns) => {
        assert_eq!(ns.path.len(), 2);
        assert_eq!(symbol_name(&result, &ns.path[0]), "Foo");
        assert_eq!(symbol_name(&result, &ns.path[1]), "Bar");
      },
      other => panic!("expected namespace, got {:?}", other),
    }
  }

  #[test]
  fn parses_const_declaration() {
    let result = parse("const PI: f64 = 3.14;");
    let stmt = first_root(&result);

    match stmt {
      ASTStatement::Constant(c) => {
        assert_eq!(symbol_name(&result, &c.name), "PI");
        assert!(c.value.is_some());
      },
      other => panic!("expected constant, got {:?}", other),
    }
  }

  #[test]
  fn parses_function_trailing_param_comma() {
    let result = parse("function foo(a: i32,): void { }");
    let stmt = first_root(&result);

    match stmt {
      ASTStatement::Function(func) => {
        assert_eq!(func.signature.parameters.len(), 1);
      },
      other => panic!("expected function, got {:?}", other),
    }
  }
}
