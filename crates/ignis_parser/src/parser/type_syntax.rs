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
      Ok(IgnisTypeSyntax::Pointer(Box::new(base)))
    } else {
      Ok(base)
    }
  }

  /// Parse qualified path
  /// Example: io::Writer
  fn parse_type_path(&mut self) -> ParserResult<IgnisTypeSyntax> {
    let first = self.expect(TokenType::Identifier)?.clone();
    let mut segments = vec![self.insert_symbol(&first)];

    while self.eat(TokenType::DoubleColon) {
      let identifier = self.expect(TokenType::Identifier)?.clone();
      segments.push(self.insert_symbol(&identifier));
    }

    // For now, use the last segment as the type name
    // TODO: Implement proper path resolution
    Ok(IgnisTypeSyntax::Named(segments.last().unwrap().clone()))
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
      TokenType::StringType => IgnisTypeSyntax::String,
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
        | TokenType::StringType
        | TokenType::CharType
        | TokenType::Void
    )
  }

}
