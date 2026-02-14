use ignis_ast::type_::IgnisTypeSyntax;
use ignis_diagnostics::message::DiagnosticMessage;
use ignis_token::token_types::TokenType;

use crate::parser::ParserResult;

impl super::IgnisParser {
  #[inline]
  fn base_type(&mut self) -> ParserResult<IgnisTypeSyntax> {
    if self.at(TokenType::Identifier) {
      return self.parse_type_path();
    }

    if self.at(TokenType::LeftParen) {
      return self.parse_function_type();
    }

    if self.is_primitive_type(&self.peek().type_) {
      return self.parse_primitive_type();
    }

    Err(DiagnosticMessage::UnexpectedToken {
      at: self.peek().span.clone(),
    })
  }

  /// Parse type syntax with modifiers
  pub(crate) fn parse_type_syntax(&mut self) -> ParserResult<IgnisTypeSyntax> {
    let mut by_ref = false;
    let mut ptr = false;
    let mut mutable = false;

    loop {
      if self.eat(TokenType::Ampersand) {
        by_ref = true;
        continue;
      }
      if self.eat(TokenType::Asterisk) {
        ptr = true;
        continue;
      }
      if self.eat(TokenType::Mut) {
        mutable = true;
        continue;
      }
      break;
    }

    let mut base = self.base_type()?;

    // Check for vector suffix: T[size?]
    while self.at(TokenType::LeftBrack) {
      base = self.parse_vector_suffix(base)?;
    }

    // Apply modifiers if any
    if by_ref {
      Ok(IgnisTypeSyntax::Reference {
        inner: Box::new(base),
        mutable,
      })
    } else if ptr {
      Ok(IgnisTypeSyntax::Pointer {
        inner: Box::new(base),
        mutable,
      })
    } else {
      Ok(base)
    }
  }

  /// Parse qualified path with optional type arguments
  /// Examples: Writer, io::Writer, Vec<i32>, Map<string, i32>, io::Reader<T>
  fn parse_type_path(&mut self) -> ParserResult<IgnisTypeSyntax> {
    let first = self.expect(TokenType::Identifier)?.clone();
    let first_span = first.span.clone();
    let mut segments = vec![(self.insert_symbol(&first), first.span.clone())];
    let mut end_span = first_span.clone();

    while self.eat(TokenType::DoubleColon) {
      let identifier = self.expect(TokenType::Identifier)?.clone();
      end_span = identifier.span.clone();
      segments.push((self.insert_symbol(&identifier), identifier.span.clone()));
    }

    // Parse optional type arguments: <T, U, V>
    let args = if self.at(TokenType::Less) {
      self.parse_type_args()?
    } else {
      Vec::new()
    };

    if segments.len() == 1 && args.is_empty() {
      Ok(IgnisTypeSyntax::Named {
        symbol: segments[0].0,
        span: segments[0].1.clone(),
      })
    } else if segments.len() == 1 {
      // Simple name with type args: Vec<i32>
      Ok(IgnisTypeSyntax::Applied {
        base: Box::new(IgnisTypeSyntax::Named {
          symbol: segments[0].0,
          span: segments[0].1.clone(),
        }),
        args,
      })
    } else {
      let span = ignis_type::span::Span::merge(&first_span, &end_span);
      Ok(IgnisTypeSyntax::Path { segments, args, span })
    }
  }

  /// Parse type arguments: <T, U, V>
  fn parse_type_args(&mut self) -> ParserResult<Vec<IgnisTypeSyntax>> {
    self.expect(TokenType::Less)?;
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
    Ok(args)
  }

  /// Parse vector suffix: T[size?]
  /// This is called after parsing the base type T
  fn parse_vector_suffix(
    &mut self,
    element_type: IgnisTypeSyntax,
  ) -> ParserResult<IgnisTypeSyntax> {
    self.expect(TokenType::LeftBrack)?;
    let mut size = None;

    // Optional size: T[10] or just T[]
    if self.at(TokenType::Int) {
      let token = self.bump().clone();
      size = token.lexeme.parse::<usize>().ok();
    }

    self.expect(TokenType::RightBrack)?;

    Ok(IgnisTypeSyntax::Vector(Box::new(element_type), size))
  }

  /// Parse function type: (T, U) -> R
  fn parse_function_type(&mut self) -> ParserResult<IgnisTypeSyntax> {
    self.expect(TokenType::LeftParen)?;
    let mut parameters = Vec::new();

    if !self.at(TokenType::RightParen) {
      parameters.push(self.parse_type_syntax()?);
      while self.eat(TokenType::Comma) {
        if self.at(TokenType::RightParen) {
          break;
        }
        parameters.push(self.parse_type_syntax()?);
      }
    }

    self.expect(TokenType::RightParen)?;
    self.expect(TokenType::Arrow)?;
    let return_type = self.parse_type_syntax()?;

    Ok(IgnisTypeSyntax::Callable(parameters, Box::new(return_type)))
  }

  /// Parse primitive type
  fn parse_primitive_type(&mut self) -> ParserResult<IgnisTypeSyntax> {
    let token = self.bump().clone();
    Ok(match token.type_ {
      TokenType::Int8Type => IgnisTypeSyntax::I8,
      TokenType::Int16Type => IgnisTypeSyntax::I16,
      TokenType::Int32Type => IgnisTypeSyntax::I32,
      TokenType::Int64Type => IgnisTypeSyntax::I64,
      TokenType::UnsignedInt8Type => IgnisTypeSyntax::U8,
      TokenType::UnsignedInt16Type => IgnisTypeSyntax::U16,
      TokenType::UnsignedInt32Type => IgnisTypeSyntax::U32,
      TokenType::UnsignedInt64Type => IgnisTypeSyntax::U64,
      TokenType::Float32Type => IgnisTypeSyntax::F32,
      TokenType::Float64Type => IgnisTypeSyntax::F64,
      TokenType::BooleanType => IgnisTypeSyntax::Boolean,
      TokenType::StrType => IgnisTypeSyntax::Str,
      TokenType::StringType => IgnisTypeSyntax::String,
      TokenType::AtomType => IgnisTypeSyntax::Atom,
      TokenType::CharType => IgnisTypeSyntax::Char,
      TokenType::Void => IgnisTypeSyntax::Void,
      _ => IgnisTypeSyntax::Void,
    })
  }

  /// Check if token is a primitive type
  fn is_primitive_type(
    &self,
    token_type: &TokenType,
  ) -> bool {
    matches!(
      token_type,
      TokenType::Int8Type
        | TokenType::Int16Type
        | TokenType::Int32Type
        | TokenType::Int64Type
        | TokenType::UnsignedInt8Type
        | TokenType::UnsignedInt16Type
        | TokenType::UnsignedInt32Type
        | TokenType::UnsignedInt64Type
        | TokenType::Float32Type
        | TokenType::Float64Type
        | TokenType::BooleanType
        | TokenType::AtomType
        | TokenType::StrType
        | TokenType::StringType
        | TokenType::CharType
        | TokenType::Void
    )
  }
}

#[cfg(test)]
mod tests {
  use std::{cell::RefCell, rc::Rc};

  use ignis_ast::{ASTNode, statements::ASTStatement, type_::IgnisTypeSyntax};
  use ignis_type::{file::SourceMap, symbol::SymbolTable};

  use crate::{lexer::IgnisLexer, parser::IgnisParser};

  fn parse_type(type_str: &str) -> IgnisTypeSyntax {
    let program = format!("function test(): {} {{ }}", type_str);
    let mut sm = SourceMap::new();
    let file_id = sm.add_file("test.ign", program.clone());

    let mut lexer = IgnisLexer::new(file_id, &program);
    lexer.scan_tokens();

    let symbols = Rc::new(RefCell::new(SymbolTable::new()));
    let mut parser = IgnisParser::new(lexer.tokens, symbols.clone());
    let (nodes, roots) = parser.parse().expect("parse failed");

    let root = nodes.get(&roots[0]);
    match root {
      ASTNode::Statement(ASTStatement::Function(func)) => func.signature.return_type.clone(),
      _ => panic!("expected function"),
    }
  }

  #[test]
  fn parses_primitive_i32() {
    let ty = parse_type("i32");
    assert_eq!(ty, IgnisTypeSyntax::I32);
  }

  #[test]
  fn parses_primitive_i64() {
    let ty = parse_type("i64");
    assert_eq!(ty, IgnisTypeSyntax::I64);
  }

  #[test]
  fn parses_primitive_f32() {
    let ty = parse_type("f32");
    assert_eq!(ty, IgnisTypeSyntax::F32);
  }

  #[test]
  fn parses_primitive_f64() {
    let ty = parse_type("f64");
    assert_eq!(ty, IgnisTypeSyntax::F64);
  }

  #[test]
  fn parses_primitive_bool() {
    let ty = parse_type("boolean");
    assert_eq!(ty, IgnisTypeSyntax::Boolean);
  }

  #[test]
  fn parses_type_keyword_string() {
    let ty = parse_type("string");
    assert_eq!(ty, IgnisTypeSyntax::String);
  }

  #[test]
  fn parses_primitive_void() {
    let ty = parse_type("void");
    assert_eq!(ty, IgnisTypeSyntax::Void);
  }

  #[test]
  fn parses_vector_type() {
    let ty = parse_type("i32[]");
    match ty {
      IgnisTypeSyntax::Vector(inner, size) => {
        assert_eq!(*inner, IgnisTypeSyntax::I32);
        assert!(size.is_none());
      },
      other => panic!("expected vector, got {:?}", other),
    }
  }

  #[test]
  fn parses_sized_vector_type() {
    let ty = parse_type("i32[10]");
    match ty {
      IgnisTypeSyntax::Vector(inner, size) => {
        assert_eq!(*inner, IgnisTypeSyntax::I32);
        assert_eq!(size, Some(10));
      },
      other => panic!("expected vector, got {:?}", other),
    }
  }

  #[test]
  fn parses_nested_vector_type() {
    let ty = parse_type("i32[][]");
    match ty {
      IgnisTypeSyntax::Vector(inner, _) => match *inner {
        IgnisTypeSyntax::Vector(inner_inner, _) => {
          assert_eq!(*inner_inner, IgnisTypeSyntax::I32);
        },
        other => panic!("expected nested vector, got {:?}", other),
      },
      other => panic!("expected vector, got {:?}", other),
    }
  }

  #[test]
  fn parses_reference_type() {
    let ty = parse_type("&i32");
    match ty {
      IgnisTypeSyntax::Reference { inner, mutable } => {
        assert_eq!(*inner, IgnisTypeSyntax::I32);
        assert!(!mutable);
      },
      other => panic!("expected reference, got {:?}", other),
    }
  }

  #[test]
  fn parses_mutable_reference_type() {
    let ty = parse_type("&mut i32");
    match ty {
      IgnisTypeSyntax::Reference { inner, mutable } => {
        assert_eq!(*inner, IgnisTypeSyntax::I32);
        assert!(mutable);
      },
      other => panic!("expected mutable reference, got {:?}", other),
    }
  }

  #[test]
  fn parses_pointer_type() {
    let ty = parse_type("*i32");
    match ty {
      IgnisTypeSyntax::Pointer { inner, mutable } => {
        assert_eq!(*inner, IgnisTypeSyntax::I32);
        assert!(!mutable);
      },
      other => panic!("expected pointer, got {:?}", other),
    }
  }

  #[test]
  fn parses_mutable_pointer_type() {
    let ty = parse_type("*mut i32");
    match ty {
      IgnisTypeSyntax::Pointer { inner, mutable } => {
        assert_eq!(*inner, IgnisTypeSyntax::I32);
        assert!(mutable);
      },
      other => panic!("expected mutable pointer, got {:?}", other),
    }
  }

  #[test]
  fn parses_function_type() {
    let ty = parse_type("(i32, i32) -> i32");
    match ty {
      IgnisTypeSyntax::Callable(params, ret) => {
        assert_eq!(params.len(), 2);
        assert_eq!(params[0], IgnisTypeSyntax::I32);
        assert_eq!(params[1], IgnisTypeSyntax::I32);
        assert_eq!(*ret, IgnisTypeSyntax::I32);
      },
      other => panic!("expected callable, got {:?}", other),
    }
  }

  #[test]
  fn parses_named_type() {
    let ty = parse_type("MyType");
    match ty {
      IgnisTypeSyntax::Named { .. } => {},
      other => panic!("expected named type, got {:?}", other),
    }
  }

  #[test]
  fn parses_path_type() {
    let ty = parse_type("io::Writer");
    match ty {
      IgnisTypeSyntax::Path { segments, .. } => {
        assert_eq!(segments.len(), 2);
      },
      other => panic!("expected path type, got {:?}", other),
    }
  }

  // =========================================================================
  // GENERIC TYPE SYNTAX TESTS
  // =========================================================================

  #[test]
  fn parses_applied_type_single_arg() {
    // Box<i32> is a user-defined generic record, not built-in
    let ty = parse_type("Box<i32>");
    match ty {
      IgnisTypeSyntax::Applied { args, .. } => {
        assert_eq!(args.len(), 1);
        assert_eq!(args[0], IgnisTypeSyntax::I32);
      },
      other => panic!("expected applied type, got {:?}", other),
    }
  }

  #[test]
  fn parses_applied_type_multiple_args() {
    // Map<K, V> is a user-defined generic record; `string` is a type keyword alias for `String`
    let ty = parse_type("Map<string, i32>");
    match ty {
      IgnisTypeSyntax::Applied { args, .. } => {
        assert_eq!(args.len(), 2);
        assert_eq!(args[0], IgnisTypeSyntax::String);
        assert_eq!(args[1], IgnisTypeSyntax::I32);
      },
      other => panic!("expected applied type, got {:?}", other),
    }
  }

  #[test]
  fn parses_applied_type_with_pointer_arg() {
    let ty = parse_type("Box<*i32>");
    match ty {
      IgnisTypeSyntax::Applied { args, .. } => {
        assert_eq!(args.len(), 1);
        match &args[0] {
          IgnisTypeSyntax::Pointer { inner, mutable } => {
            assert_eq!(**inner, IgnisTypeSyntax::I32);
            assert!(!*mutable);
          },
          other => panic!("expected pointer, got {:?}", other),
        }
      },
      other => panic!("expected applied type, got {:?}", other),
    }
  }

  #[test]
  fn parses_applied_type_with_reference_arg() {
    let ty = parse_type("Container<&mut i32>");
    match ty {
      IgnisTypeSyntax::Applied { args, .. } => {
        assert_eq!(args.len(), 1);
        match &args[0] {
          IgnisTypeSyntax::Reference { mutable, .. } => {
            assert!(*mutable);
          },
          other => panic!("expected reference, got {:?}", other),
        }
      },
      other => panic!("expected applied type, got {:?}", other),
    }
  }

  #[test]
  fn parses_path_type_with_args() {
    let ty = parse_type("io::Reader<i32>");
    match ty {
      IgnisTypeSyntax::Path { segments, args, .. } => {
        assert_eq!(segments.len(), 2);
        assert_eq!(args.len(), 1);
        assert_eq!(args[0], IgnisTypeSyntax::I32);
      },
      other => panic!("expected path type with args, got {:?}", other),
    }
  }

  #[test]
  fn parses_applied_type_trailing_comma() {
    let ty = parse_type("Box<i32,>");
    match ty {
      IgnisTypeSyntax::Applied { args, .. } => {
        assert_eq!(args.len(), 1);
      },
      other => panic!("expected applied type, got {:?}", other),
    }
  }

  #[test]
  fn parses_named_type_param() {
    // T without arguments is just Named (a type parameter)
    let ty = parse_type("T");
    match ty {
      IgnisTypeSyntax::Named { .. } => {},
      other => panic!("expected named type, got {:?}", other),
    }
  }

  #[test]
  fn parses_pointer_to_applied_type() {
    let ty = parse_type("*Box<i32>");
    match ty {
      IgnisTypeSyntax::Pointer { inner, mutable } => {
        assert!(!mutable);
        match *inner {
          IgnisTypeSyntax::Applied { args, .. } => {
            assert_eq!(args.len(), 1);
          },
          other => panic!("expected applied type inside pointer, got {:?}", other),
        }
      },
      other => panic!("expected pointer, got {:?}", other),
    }
  }

  #[test]
  fn parses_reference_to_applied_type() {
    let ty = parse_type("&Box<i32>");
    match ty {
      IgnisTypeSyntax::Reference { inner, mutable } => {
        assert!(!mutable);
        match *inner {
          IgnisTypeSyntax::Applied { args, .. } => {
            assert_eq!(args.len(), 1);
          },
          other => panic!("expected applied type inside reference, got {:?}", other),
        }
      },
      other => panic!("expected reference, got {:?}", other),
    }
  }

  #[test]
  fn parses_vector_of_applied_type() {
    // Box<i32>[] is an array of Box<i32>
    let ty = parse_type("Box<i32>[]");
    match ty {
      IgnisTypeSyntax::Vector(inner, size) => {
        assert!(size.is_none());
        match *inner {
          IgnisTypeSyntax::Applied { args, .. } => {
            assert_eq!(args.len(), 1);
          },
          other => panic!("expected applied type inside vector, got {:?}", other),
        }
      },
      other => panic!("expected vector, got {:?}", other),
    }
  }

  #[test]
  fn parses_function_type_with_generic_param() {
    let ty = parse_type("(Box<i32>) -> boolean");
    match ty {
      IgnisTypeSyntax::Callable(params, ret) => {
        assert_eq!(params.len(), 1);
        match &params[0] {
          IgnisTypeSyntax::Applied { args, .. } => {
            assert_eq!(args.len(), 1);
          },
          other => panic!("expected applied type, got {:?}", other),
        }
        assert_eq!(*ret, IgnisTypeSyntax::Boolean);
      },
      other => panic!("expected callable, got {:?}", other),
    }
  }

  #[test]
  fn parses_applied_type_with_array_arg() {
    // Container<i32[]> - generic with array type argument
    let ty = parse_type("Container<i32[]>");
    match ty {
      IgnisTypeSyntax::Applied { args, .. } => {
        assert_eq!(args.len(), 1);
        match &args[0] {
          IgnisTypeSyntax::Vector(inner, size) => {
            assert_eq!(**inner, IgnisTypeSyntax::I32);
            assert!(size.is_none());
          },
          other => panic!("expected vector, got {:?}", other),
        }
      },
      other => panic!("expected applied type, got {:?}", other),
    }
  }

  #[test]
  fn parses_applied_type_with_sized_array_arg() {
    // Container<i32[10]> - generic with fixed-size array
    let ty = parse_type("Container<i32[10]>");
    match ty {
      IgnisTypeSyntax::Applied { args, .. } => {
        assert_eq!(args.len(), 1);
        match &args[0] {
          IgnisTypeSyntax::Vector(inner, size) => {
            assert_eq!(**inner, IgnisTypeSyntax::I32);
            assert_eq!(*size, Some(10));
          },
          other => panic!("expected vector, got {:?}", other),
        }
      },
      other => panic!("expected applied type, got {:?}", other),
    }
  }
}
