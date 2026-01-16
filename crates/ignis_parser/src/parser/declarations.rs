use ignis_ast::{
  NodeId,
  generics::{ASTGenericParam, ASTGenericParams},
  metadata::ASTMetadata,
  statements::{
    ASTStatement, ASTEnum, ASTEnumField, ASTEnumItem, ASTEnumVariant, ASTMethod, ASTRecord, ASTRecordField,
    ASTRecordItem, ASTTypeAlias,
    const_statement::ASTConstant,
    export_statement::ASTExport,
    extern_statement::ASTExtern,
    function::{ASTFunction, ASTFunctionSignature},
    import_statement::ASTImport,
    namespace_statement::ASTNamespace,
  },
  type_::IgnisTypeSyntax,
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
      TokenType::Type => self.parse_type_alias_declaration(),
      TokenType::Record => self.parse_record_declaration(),
      TokenType::Enum => self.parse_enum_declaration(),
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

  // =========================================================================
  // GENERIC PARAMETERS PARSING
  // =========================================================================

  /// Parse optional generic parameters: `<T, U, V>`
  ///
  /// Returns `None` if no `<` is found.
  /// Returns `Some(ASTGenericParams)` if generic params are present.
  ///
  /// Grammar: `<` identifier (`,` identifier)* `>`
  fn parse_optional_generic_params(&mut self) -> ParserResult<Option<ASTGenericParams>> {
    if !self.at(TokenType::Less) {
      return Ok(None);
    }

    let start = self.expect(TokenType::Less)?.span.clone();
    let mut params = Vec::new();

    // Parse first parameter (required if < was consumed)
    let first_token = self.expect(TokenType::Identifier)?.clone();
    params.push(ASTGenericParam::new(self.insert_symbol(&first_token), first_token.span.clone()));

    // Parse remaining parameters
    while self.eat(TokenType::Comma) {
      // Allow trailing comma
      if self.at(TokenType::Greater) {
        break;
      }
      let param_token = self.expect(TokenType::Identifier)?.clone();
      params.push(ASTGenericParam::new(self.insert_symbol(&param_token), param_token.span.clone()));
    }

    let end = self.expect(TokenType::Greater)?.span.clone();
    let span = Span::merge(&start, &end);

    Ok(Some(ASTGenericParams::new(params, span)))
  }

  /// Parse optional type arguments for calls and type instantiation: `<i32, bool>`
  ///
  /// Returns `None` if no `<` is found.
  /// Returns `Some(Vec<IgnisTypeSyntax>)` if type args are present.
  ///
  /// Grammar: `<` type (`,` type)* `>`
  pub(crate) fn parse_optional_type_args(&mut self) -> ParserResult<Option<Vec<IgnisTypeSyntax>>> {
    if !self.at(TokenType::Less) {
      return Ok(None);
    }

    let _start = self.expect(TokenType::Less)?.span.clone();
    let mut args = Vec::new();

    // Parse first type argument (required if < was consumed)
    args.push(self.parse_type_syntax()?);

    // Parse remaining type arguments
    while self.eat(TokenType::Comma) {
      // Allow trailing comma
      if self.at(TokenType::Greater) {
        break;
      }
      args.push(self.parse_type_syntax()?);
    }

    self.expect(TokenType::Greater)?;

    Ok(Some(args))
  }

  // =========================================================================
  // FUNCTION DECLARATIONS
  // =========================================================================

  /// function name<T, U>(params): type block
  fn parse_function_declaration(&mut self) -> ParserResult<NodeId> {
    let keyword = self.expect(TokenType::Function)?.clone();
    let name_token = self.expect(TokenType::Identifier)?.clone();
    let name = self.insert_symbol(&name_token);

    // Parse optional generic parameters: <T, U>
    let type_params = self.parse_optional_generic_params()?;

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
      type_params,
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
      || self.at(TokenType::Type)
      || self.at(TokenType::Record)
      || self.at(TokenType::Enum)
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
      TokenType::Type => self.parse_type_alias_declaration(),
      TokenType::Record => self.parse_record_declaration(),
      TokenType::Enum => self.parse_enum_declaration(),
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
      TokenType::Type => self.parse_type_alias_declaration(),
      TokenType::Record => self.parse_record_declaration(),
      TokenType::Enum => self.parse_enum_declaration(),
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
      None, // TODO: parse type_params
      parameters,
      return_type,
      span.clone(),
      ignis_ast::metadata::ASTMetadata::EXTERN_MEMBER,
    );

    let function = ASTFunction::new(signature, None);

    Ok(self.allocate_statement(ASTStatement::Function(function)))
  }

  // =========================================================================
  // TYPE ALIAS, RECORD, ENUM DECLARATIONS
  // =========================================================================

  /// type Name<T, U> = <type>;
  fn parse_type_alias_declaration(&mut self) -> ParserResult<NodeId> {
    let start = self.expect(TokenType::Type)?.span.clone();
    let name_token = self.expect(TokenType::Identifier)?.clone();
    let name = self.insert_symbol(&name_token);

    let type_params = self.parse_optional_generic_params()?;

    self.expect(TokenType::Equal)?;

    let target = self.parse_type_syntax()?;
    let end = self.expect(TokenType::SemiColon)?.clone();

    let span = Span::merge(&start, &end.span);
    Ok(self.allocate_statement(ASTStatement::TypeAlias(ASTTypeAlias::new(name, type_params, target, span))))
  }

  /// Parse member modifiers: static, public, private
  fn parse_member_modifiers(&mut self) -> ASTMetadata {
    let mut meta = ASTMetadata::NONE;

    loop {
      match self.peek().type_ {
        TokenType::Static => {
          meta |= ASTMetadata::STATIC;
          self.bump();
        },
        TokenType::Public => {
          meta |= ASTMetadata::PUBLIC;
          self.bump();
        },
        TokenType::Private => {
          meta |= ASTMetadata::PRIVATE;
          self.bump();
        },
        _ => break,
      }
    }

    meta
  }

  /// record Name<T, U> { fields, methods }
  fn parse_record_declaration(&mut self) -> ParserResult<NodeId> {
    let start = self.expect(TokenType::Record)?.span.clone();
    let name_token = self.expect(TokenType::Identifier)?.clone();
    let name = self.insert_symbol(&name_token);

    // Parse optional generic parameters: <T, U>
    let type_params = self.parse_optional_generic_params()?;

    self.expect(TokenType::LeftBrace)?;

    let mut items = Vec::new();

    while !self.at(TokenType::RightBrace) && !self.at(TokenType::Eof) {
      let modifiers = self.parse_member_modifiers();

      if !self.at(TokenType::Identifier) {
        return Err(DiagnosticMessage::UnexpectedToken {
          at: self.peek().span.clone(),
        });
      }

      // Lookahead to distinguish method vs field:
      // - identifier ( ... -> method
      // - identifier < ... -> method with generics
      // - identifier : ... -> field
      let next = self.peek_nth(1).type_;
      if next == TokenType::LeftParen || next == TokenType::Less {
        let method = self.parse_method(modifiers)?;
        items.push(ASTRecordItem::Method(method));
      } else {
        let field = self.parse_record_field(modifiers)?;
        items.push(ASTRecordItem::Field(field));
      }
    }

    let end = self.expect(TokenType::RightBrace)?.clone();
    let span = Span::merge(&start, &end.span);

    Ok(self.allocate_statement(ASTStatement::Record(ASTRecord::new(name, type_params, items, span))))
  }

  /// Method declaration (without 'function' keyword)
  /// name<U>(params): returnType { body }
  fn parse_method(
    &mut self,
    modifiers: ASTMetadata,
  ) -> ParserResult<ASTMethod> {
    let name_token = self.expect(TokenType::Identifier)?.clone();
    let name = self.insert_symbol(&name_token);

    // Parse optional generic parameters: <U, V>
    let type_params = self.parse_optional_generic_params()?;

    let parameters =
      self.parse_delimited_list(TokenType::LeftParen, TokenType::RightParen, TokenType::Comma, |parser| {
        parser.parse_parameter()
      })?;

    // Optional return type: : Type
    let return_type = if self.eat(TokenType::Colon) {
      self.parse_type_syntax()?
    } else {
      IgnisTypeSyntax::Void
    };

    let body = self.parse_block()?;

    let span = Span::merge(&name_token.span, self.get_span(&body));

    Ok(ASTMethod::new(
      name,
      type_params,
      parameters,
      return_type,
      body,
      modifiers,
      span,
    ))
  }

  /// Field declaration: name: type (= expr)?;
  fn parse_record_field(
    &mut self,
    modifiers: ASTMetadata,
  ) -> ParserResult<ASTRecordField> {
    let name_token = self.expect(TokenType::Identifier)?.clone();
    let name = self.insert_symbol(&name_token);

    self.expect(TokenType::Colon)?;

    let type_ = self.parse_type_syntax()?;

    // Optional initializer (required for static fields)
    let value = if self.eat(TokenType::Equal) {
      Some(self.parse_expression(0)?)
    } else {
      None
    };

    let end = self.expect(TokenType::SemiColon)?.clone();
    let span = Span::merge(&name_token.span, &end.span);

    Ok(ASTRecordField::new(name, type_, value, modifiers, span))
  }

  /// enum Name<T> { variants, methods, fields }
  fn parse_enum_declaration(&mut self) -> ParserResult<NodeId> {
    let start = self.expect(TokenType::Enum)?.span.clone();
    let name_token = self.expect(TokenType::Identifier)?.clone();
    let name = self.insert_symbol(&name_token);

    // Parse optional generic parameters: <T>
    let type_params = self.parse_optional_generic_params()?;

    self.expect(TokenType::LeftBrace)?;

    let mut items = Vec::new();

    while !self.at(TokenType::RightBrace) && !self.at(TokenType::Eof) {
      let modifiers = self.parse_member_modifiers();

      if !self.at(TokenType::Identifier) {
        return Err(DiagnosticMessage::UnexpectedToken {
          at: self.peek().span.clone(),
        });
      }

      // Lookahead to distinguish variant vs method vs field:
      // - identifier : ... -> field
      // - identifier ( ... followed by : or { -> method
      // - identifier < ... ( ... -> generic method
      // - identifier or identifier ( types ) -> variant
      let next = self.peek_nth(1).type_;

      if next == TokenType::Colon {
        // Field: name: type = expr;
        let field = self.parse_enum_field(modifiers)?;
        items.push(ASTEnumItem::Field(field));
      } else if next == TokenType::Less && self.is_generic_method_ahead() {
        // Generic method: name<T>(...): type { }
        let method = self.parse_method(modifiers)?;
        items.push(ASTEnumItem::Method(method));
      } else if next == TokenType::LeftParen && self.is_method_ahead() {
        // Method: name(...): type { }
        let method = self.parse_method(modifiers)?;
        items.push(ASTEnumItem::Method(method));
      } else {
        // Variant: Name or Name(Type, Type)
        // Check for invalid static modifier on variant
        if modifiers.contains(ASTMetadata::STATIC) {
          let variant = self.parse_enum_variant()?;
          self.diagnostics.push(DiagnosticMessage::StaticOnEnumVariant {
            variant: self.symbols.borrow().get(&variant.name).to_string(),
            span: variant.span.clone(),
          });
          items.push(ASTEnumItem::Variant(variant));
        } else {
          let variant = self.parse_enum_variant()?;
          items.push(ASTEnumItem::Variant(variant));
        }
      }
    }

    let end = self.expect(TokenType::RightBrace)?.clone();
    let span = Span::merge(&start, &end.span);

    Ok(self.allocate_statement(ASTStatement::Enum(ASTEnum::new(name, type_params, items, span))))
  }

  /// Distinguish method from variant with payload:
  /// - name(Type, Type) -> variant (types, no params with names)
  /// - name(x: Type): Type { } -> method (params with names, has body)
  ///
  /// Heuristic: if after ) comes { or :, it's a method
  fn is_method_ahead(&self) -> bool {
    // We know peek_nth(1) is '(' - find its matching ')'
    let mut depth = 1;
    let mut i = 2; // start after the opening (

    loop {
      let tok = self.peek_nth(i).type_;

      match tok {
        TokenType::LeftParen => depth += 1,
        TokenType::RightParen => {
          depth -= 1;
          if depth == 0 {
            // Found the matching ) - see what follows
            let after = self.peek_nth(i + 1).type_;
            return after == TokenType::LeftBrace || after == TokenType::Colon;
          }
        },
        TokenType::Eof => return false,
        _ => {},
      }

      i += 1;
      if i > 100 {
        return false; // safety limit
      }
    }
  }

  /// Check if we have a generic method: name<...>(...): ... { }
  /// We know peek_nth(1) is '<'
  fn is_generic_method_ahead(&self) -> bool {
    // Skip past the generic parameters <...>
    let mut depth = 1;
    let mut i = 2; // start after <

    loop {
      let tok = self.peek_nth(i).type_;

      match tok {
        TokenType::Less => depth += 1,
        TokenType::Greater => {
          depth -= 1;
          if depth == 0 {
            // Found the matching > - next should be (
            let after = self.peek_nth(i + 1).type_;
            if after == TokenType::LeftParen {
              // Now scan from ( to see if it looks like a method
              return self.is_method_ahead_from(i + 1);
            }
            return false;
          }
        },
        TokenType::Eof => return false,
        _ => {},
      }

      i += 1;
      if i > 100 {
        return false;
      }
    }
  }

  /// Helper: check if we have a method starting from a specific position where ( is located
  fn is_method_ahead_from(
    &self,
    paren_pos: usize,
  ) -> bool {
    let mut depth = 1;
    let mut i = paren_pos + 1; // start after (

    loop {
      let tok = self.peek_nth(i).type_;

      match tok {
        TokenType::LeftParen => depth += 1,
        TokenType::RightParen => {
          depth -= 1;
          if depth == 0 {
            let after = self.peek_nth(i + 1).type_;
            return after == TokenType::LeftBrace || after == TokenType::Colon;
          }
        },
        TokenType::Eof => return false,
        _ => {},
      }

      i += 1;
      if i > 100 {
        return false;
      }
    }
  }

  /// Enum variant: Name or Name(Type, Type)
  fn parse_enum_variant(&mut self) -> ParserResult<ASTEnumVariant> {
    let name_token = self.expect(TokenType::Identifier)?.clone();
    let name = self.insert_symbol(&name_token);
    let start_span = name_token.span.clone();

    let mut payload = Vec::new();

    let end_span = if self.eat(TokenType::LeftParen) {
      // Payload types (no names, just types)
      if !self.at(TokenType::RightParen) {
        payload.push(self.parse_type_syntax()?);
        while self.eat(TokenType::Comma) {
          if self.at(TokenType::RightParen) {
            break;
          }
          payload.push(self.parse_type_syntax()?);
        }
      }
      self.expect(TokenType::RightParen)?.span.clone()
    } else {
      start_span.clone()
    };

    // Consume optional trailing comma
    self.eat(TokenType::Comma);

    let span = Span::merge(&start_span, &end_span);
    Ok(ASTEnumVariant::new(name, payload, span))
  }

  /// Enum field: name: type = expr;
  fn parse_enum_field(
    &mut self,
    modifiers: ASTMetadata,
  ) -> ParserResult<ASTEnumField> {
    let name_token = self.expect(TokenType::Identifier)?.clone();
    let name = self.insert_symbol(&name_token);

    self.expect(TokenType::Colon)?;

    let type_ = self.parse_type_syntax()?;

    let value = if self.eat(TokenType::Equal) {
      Some(self.parse_expression(0)?)
    } else {
      None
    };

    let end = self.expect(TokenType::SemiColon)?.clone();
    let span = Span::merge(&name_token.span, &end.span);

    Ok(ASTEnumField::new(name, type_, value, modifiers, span))
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

  // =========================================================================
  // GENERIC SYNTAX TESTS
  // =========================================================================

  #[test]
  fn parses_generic_function_single_param() {
    let result = parse("function identity<T>(x: T): T { return x; }");
    let stmt = first_root(&result);

    match stmt {
      ASTStatement::Function(func) => {
        assert_eq!(symbol_name(&result, &func.signature.name), "identity");

        let type_params = func.signature.type_params.as_ref().expect("should have type params");
        assert_eq!(type_params.len(), 1);
        assert_eq!(symbol_name(&result, &type_params.params[0].name), "T");

        assert_eq!(func.signature.parameters.len(), 1);
        assert_eq!(symbol_name(&result, &func.signature.parameters[0].name), "x");
      },
      other => panic!("expected function, got {:?}", other),
    }
  }

  #[test]
  fn parses_generic_function_multiple_params() {
    let result = parse("function map<T, U, V>(a: T, b: U): V { return a; }");
    let stmt = first_root(&result);

    match stmt {
      ASTStatement::Function(func) => {
        assert_eq!(symbol_name(&result, &func.signature.name), "map");

        let type_params = func.signature.type_params.as_ref().expect("should have type params");
        assert_eq!(type_params.len(), 3);
        assert_eq!(symbol_name(&result, &type_params.params[0].name), "T");
        assert_eq!(symbol_name(&result, &type_params.params[1].name), "U");
        assert_eq!(symbol_name(&result, &type_params.params[2].name), "V");
      },
      other => panic!("expected function, got {:?}", other),
    }
  }

  #[test]
  fn parses_generic_function_trailing_comma() {
    let result = parse("function foo<T,>(x: T): T { return x; }");
    let stmt = first_root(&result);

    match stmt {
      ASTStatement::Function(func) => {
        let type_params = func.signature.type_params.as_ref().expect("should have type params");
        assert_eq!(type_params.len(), 1);
        assert_eq!(symbol_name(&result, &type_params.params[0].name), "T");
      },
      other => panic!("expected function, got {:?}", other),
    }
  }

  #[test]
  fn parses_non_generic_function_has_no_type_params() {
    let result = parse("function foo(x: i32): i32 { return x; }");
    let stmt = first_root(&result);

    match stmt {
      ASTStatement::Function(func) => {
        assert!(func.signature.type_params.is_none());
      },
      other => panic!("expected function, got {:?}", other),
    }
  }

  #[test]
  fn parses_generic_record_single_param() {
    let result = parse("record Box<T> { value: T; }");
    let stmt = first_root(&result);

    match stmt {
      ASTStatement::Record(rec) => {
        assert_eq!(symbol_name(&result, &rec.name), "Box");

        let type_params = rec.type_params.as_ref().expect("should have type params");
        assert_eq!(type_params.len(), 1);
        assert_eq!(symbol_name(&result, &type_params.params[0].name), "T");

        assert_eq!(rec.items.len(), 1);
      },
      other => panic!("expected record, got {:?}", other),
    }
  }

  #[test]
  fn parses_generic_record_multiple_params() {
    let result = parse("record Pair<K, V> { key: K; value: V; }");
    let stmt = first_root(&result);

    match stmt {
      ASTStatement::Record(rec) => {
        assert_eq!(symbol_name(&result, &rec.name), "Pair");

        let type_params = rec.type_params.as_ref().expect("should have type params");
        assert_eq!(type_params.len(), 2);
        assert_eq!(symbol_name(&result, &type_params.params[0].name), "K");
        assert_eq!(symbol_name(&result, &type_params.params[1].name), "V");
      },
      other => panic!("expected record, got {:?}", other),
    }
  }

  #[test]
  fn parses_non_generic_record_has_no_type_params() {
    let result = parse("record Point { x: i32; y: i32; }");
    let stmt = first_root(&result);

    match stmt {
      ASTStatement::Record(rec) => {
        assert!(rec.type_params.is_none());
      },
      other => panic!("expected record, got {:?}", other),
    }
  }

  #[test]
  fn parses_generic_record_with_method() {
    use ignis_ast::statements::ASTRecordItem;

    let result = parse(
      r#"record Container<T> {
        data: *T;
        get(index: i32): T { return *data; }
      }"#,
    );
    let stmt = first_root(&result);

    match stmt {
      ASTStatement::Record(rec) => {
        assert_eq!(symbol_name(&result, &rec.name), "Container");

        let type_params = rec.type_params.as_ref().expect("should have type params");
        assert_eq!(type_params.len(), 1);
        assert_eq!(symbol_name(&result, &type_params.params[0].name), "T");

        assert_eq!(rec.items.len(), 2);
        match &rec.items[1] {
          ASTRecordItem::Method(method) => {
            assert_eq!(symbol_name(&result, &method.name), "get");
            assert!(method.type_params.is_none());
          },
          other => panic!("expected method, got {:?}", other),
        }
      },
      other => panic!("expected record, got {:?}", other),
    }
  }

  #[test]
  fn parses_generic_method_on_record() {
    use ignis_ast::statements::ASTRecordItem;

    let result = parse(
      r#"record Container<T> {
        data: *T;
        map<U>(f: (T) -> U): U { return f(*data); }
      }"#,
    );
    let stmt = first_root(&result);

    match stmt {
      ASTStatement::Record(rec) => {
        assert_eq!(rec.items.len(), 2);
        match &rec.items[1] {
          ASTRecordItem::Method(method) => {
            assert_eq!(symbol_name(&result, &method.name), "map");

            let method_type_params = method.type_params.as_ref().expect("method should have type params");
            assert_eq!(method_type_params.len(), 1);
            assert_eq!(symbol_name(&result, &method_type_params.params[0].name), "U");
          },
          other => panic!("expected method, got {:?}", other),
        }
      },
      other => panic!("expected record, got {:?}", other),
    }
  }

  #[test]
  fn parses_generic_method_multiple_params() {
    use ignis_ast::statements::ASTRecordItem;

    let result = parse(
      r#"record Container<T> {
        transform<U, V>(f: (T) -> U, g: (U) -> V): V { return g(f(*data)); }
      }"#,
    );
    let stmt = first_root(&result);

    match stmt {
      ASTStatement::Record(rec) => match &rec.items[0] {
        ASTRecordItem::Method(method) => {
          assert_eq!(symbol_name(&result, &method.name), "transform");

          let method_type_params = method.type_params.as_ref().expect("method should have type params");
          assert_eq!(method_type_params.len(), 2);
          assert_eq!(symbol_name(&result, &method_type_params.params[0].name), "U");
          assert_eq!(symbol_name(&result, &method_type_params.params[1].name), "V");
        },
        other => panic!("expected method, got {:?}", other),
      },
      other => panic!("expected record, got {:?}", other),
    }
  }

  #[test]
  fn parses_generic_enum_single_param() {
    let result = parse(
      r#"enum Option<T> {
        Some(T),
        None
      }"#,
    );
    let stmt = first_root(&result);

    match stmt {
      ASTStatement::Enum(en) => {
        assert_eq!(symbol_name(&result, &en.name), "Option");

        let type_params = en.type_params.as_ref().expect("should have type params");
        assert_eq!(type_params.len(), 1);
        assert_eq!(symbol_name(&result, &type_params.params[0].name), "T");

        assert_eq!(en.items.len(), 2);
      },
      other => panic!("expected enum, got {:?}", other),
    }
  }

  #[test]
  fn parses_generic_enum_multiple_params() {
    let result = parse(
      r#"enum Result<T, E> {
        Ok(T),
        Err(E)
      }"#,
    );
    let stmt = first_root(&result);

    match stmt {
      ASTStatement::Enum(en) => {
        assert_eq!(symbol_name(&result, &en.name), "Result");

        let type_params = en.type_params.as_ref().expect("should have type params");
        assert_eq!(type_params.len(), 2);
        assert_eq!(symbol_name(&result, &type_params.params[0].name), "T");
        assert_eq!(symbol_name(&result, &type_params.params[1].name), "E");
      },
      other => panic!("expected enum, got {:?}", other),
    }
  }

  #[test]
  fn parses_non_generic_enum_has_no_type_params() {
    let result = parse(
      r#"enum Direction {
        North,
        South,
        East,
        West
      }"#,
    );
    let stmt = first_root(&result);

    match stmt {
      ASTStatement::Enum(en) => {
        assert!(en.type_params.is_none());
      },
      other => panic!("expected enum, got {:?}", other),
    }
  }

  #[test]
  fn parses_generic_enum_with_method() {
    use ignis_ast::statements::ASTEnumItem;

    let result = parse(
      r#"enum Option<T> {
        Some(T),
        None,
        map<U>(f: (T) -> U): U { return f(self); }
      }"#,
    );
    let stmt = first_root(&result);

    match stmt {
      ASTStatement::Enum(en) => {
        assert_eq!(en.items.len(), 3);
        match &en.items[2] {
          ASTEnumItem::Method(method) => {
            assert_eq!(symbol_name(&result, &method.name), "map");

            let method_type_params = method.type_params.as_ref().expect("method should have type params");
            assert_eq!(method_type_params.len(), 1);
            assert_eq!(symbol_name(&result, &method_type_params.params[0].name), "U");
          },
          other => panic!("expected method, got {:?}", other),
        }
      },
      other => panic!("expected enum, got {:?}", other),
    }
  }
}
