use ignis_ast::{
  ASTNode, NodeId,
  expressions::{
    ASTExpression, ASTAccessOp, ASTMemberAccess, ASTRecordInit, ASTRecordInitField,
    assignment::{ASTAssignment, ASTAssignmentOperator},
    binary::{ASTBinary, ASTBinaryOperator},
    call::ASTCallExpression,
    cast::ASTCast,
    dereference::ASTDereference,
    grouped::ASTGrouped,
    literal::ASTLiteral,
    path::{ASTPath, ASTPathSegment},
    reference::ASTReference,
    ternary::ASTTernary,
    unary::{ASTUnary, UnaryOperator},
    variable::ASTVariableExpression,
    vector::ASTVector,
    vector_access::ASTVectorAccess,
  },
};
use ignis_diagnostics::message::DiagnosticMessage;
use ignis_token::{token::Token, token_types::TokenType};
use ignis_type::{span::Span, symbol::SymbolId, value::IgnisLiteralValue};

use super::{IgnisParser, ParserResult};

impl IgnisParser {
  pub(crate) fn parse_expression(
    &mut self,
    min_bp: u16,
  ) -> ParserResult<NodeId> {
    let mut left: NodeId = self.parse_prefix()?;

    loop {
      // Handle calls with optional type arguments: foo<T, U>(args) or foo(args)
      // Check for < only after identifiers/paths (not after literals)
      if self.at(TokenType::Less) && !self.is_literal(&left) && self.looks_like_type_args() {
        // This is a generic call: foo<i32>(x)
        let type_args = self.parse_optional_type_args()?;
        // Now expect the call
        if self.at(TokenType::LeftParen) {
          left = self.parse_call_with_type_args(left, type_args)?;
          continue;
        } else {
          // Error: type args without call - could recover or error
          return Err(DiagnosticMessage::ExpectedCallAfterTypeArgs {
            span: self.peek().span.clone(),
          });
        }
      }

      if self.at(TokenType::LeftParen) {
        if self.is_literal(&left) {
          let right = self.parse_prefix()?;
          let span = Span::merge(self.get_span(&left), self.get_span(&right));
          left = self.allocate_expression(ASTExpression::Binary(ASTBinary::new(
            left,
            ASTBinaryOperator::Multiply,
            right,
            span,
          )));
          continue;
        } else {
          left = self.parse_call_with_type_args(left, None)?;
          continue;
        }
      }

      if self.at(TokenType::LeftBrack) {
        left = self.parse_index(left)?;
        continue;
      }

      // Member access with . (instance)
      if self.at(TokenType::Dot) {
        left = self.parse_member_access(left, ASTAccessOp::Dot)?;
        continue;
      }

      if self.at(TokenType::Increment) || self.at(TokenType::Decrement) {
        left = self.parse_postfix(left)?;
        continue;
      }

      let op = self.peek().type_.clone();
      let Some((lbp, rbp)) = self.binding_powers(&op) else {
        break;
      };
      if lbp < min_bp {
        break;
      }

      self.bump();

      if op == TokenType::As {
        let type_start = self.peek().span.clone();
        let target_type = self.parse_type_syntax()?;
        let span = Span::merge(self.get_span(&left), &type_start);
        left = self.allocate_expression(ASTExpression::Cast(ASTCast::new(span, target_type, left)));
        continue;
      }

      if op == TokenType::QuestionMark {
        let then_expr = self.parse_expression(0)?;
        let _ = self.expect(TokenType::Colon)?;
        let else_expr = self.parse_expression(0)?;
        let span = Span::merge(self.get_span(&left), self.get_span(&else_expr));

        left = self.allocate_expression(ASTExpression::Ternary(ASTTernary::new(left, then_expr, else_expr, span)));
        continue;
      }

      let right = self.parse_expression(rbp)?;
      let span = Span::merge(self.get_span(&left), self.get_span(&right));

      if let Some(assign_op) = Self::to_assignment_operator(&op) {
        left = self.allocate_expression(ASTExpression::Assignment(ASTAssignment::new(left, right, assign_op, span)));
      } else {
        left = self.allocate_expression(ASTExpression::Binary(ASTBinary::new(
          left,
          ASTBinaryOperator::from(&op),
          right,
          span,
        )));
      }
    }

    Ok(left)
  }

  /// Heuristic to determine if `<` starts type arguments for record init or enum variant.
  ///
  /// We look ahead to see if the pattern matches: `< type (, type)* > {` or `< type (, type)* > ::`
  /// This handles: `Box<i32> { ... }` and `Option<i32>::Some(...)`
  fn looks_like_type_args_for_init(&self) -> bool {
    // We're at '<'
    // Look for pattern: < ... > { or < ... > ::
    let mut depth = 1;
    let mut i = 1;

    loop {
      let tok = self.peek_nth(i).type_;

      match tok {
        TokenType::Less => depth += 1,
        TokenType::Greater => {
          depth -= 1;
          if depth == 0 {
            // Found matching >, check if followed by { or ::
            let next = self.peek_nth(i + 1).type_;
            return next == TokenType::LeftBrace || next == TokenType::DoubleColon;
          }
        },
        // These can appear in type arguments
        TokenType::Identifier
        | TokenType::Comma
        | TokenType::Ampersand
        | TokenType::Asterisk
        | TokenType::Mut
        | TokenType::DoubleColon
        | TokenType::LeftBrack
        | TokenType::RightBrack
        | TokenType::Int
        | TokenType::Arrow
        | TokenType::LeftParen
        | TokenType::RightParen => {},
        // Primitive types
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
        | TokenType::Void => {},
        // If we hit these, it's likely a comparison not type args
        TokenType::Eof | TokenType::SemiColon | TokenType::RightBrace => return false,
        // Operators that wouldn't appear in type args suggest comparison
        TokenType::Plus
        | TokenType::Minus
        | TokenType::Slash
        | TokenType::Mod
        | TokenType::EqualEqual
        | TokenType::BangEqual
        | TokenType::LessEqual
        | TokenType::GreaterEqual
        | TokenType::And
        | TokenType::Or
        | TokenType::Equal => return false,
        _ => {},
      }

      i += 1;
      if i > 50 {
        return false;
      }
    }
  }

  /// Heuristic to determine if `<` starts type arguments rather than a comparison.
  ///
  /// We look ahead to see if the pattern matches: `< type (, type)* > (`
  /// This is a simplification - we check if after < we have identifier/primitive
  /// followed eventually by > and (.
  fn looks_like_type_args(&self) -> bool {
    // We're at '<'
    // Look for pattern: < ... > (
    // Simple heuristic: scan for balanced <> and check if followed by (
    let mut depth = 1;
    let mut i = 1; // start after <

    loop {
      let tok = self.peek_nth(i).type_;

      match tok {
        TokenType::Less => depth += 1,
        TokenType::Greater => {
          depth -= 1;
          if depth == 0 {
            // Found matching >, check if followed by (
            return self.peek_nth(i + 1).type_ == TokenType::LeftParen;
          }
        },
        // These can appear in type arguments
        TokenType::Identifier
        | TokenType::Comma
        | TokenType::Ampersand
        | TokenType::Asterisk
        | TokenType::Mut
        | TokenType::DoubleColon
        | TokenType::LeftBrack
        | TokenType::RightBrack
        | TokenType::Int
        | TokenType::Arrow
        | TokenType::LeftParen
        | TokenType::RightParen => {},
        // Primitive types
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
        | TokenType::Void => {},
        // If we hit these, it's likely a comparison not type args
        TokenType::Eof | TokenType::SemiColon | TokenType::RightBrace => return false,
        // Operators that wouldn't appear in type args suggest comparison
        TokenType::Plus
        | TokenType::Minus
        | TokenType::Slash
        | TokenType::Mod
        | TokenType::EqualEqual
        | TokenType::BangEqual
        | TokenType::LessEqual
        | TokenType::GreaterEqual
        | TokenType::And
        | TokenType::Or
        | TokenType::Equal => return false,
        _ => {},
      }

      i += 1;
      if i > 50 {
        // Safety limit to avoid infinite scanning
        return false;
      }
    }
  }

  fn parse_call_with_type_args(
    &mut self,
    callee: NodeId,
    type_args: Option<Vec<ignis_ast::type_::IgnisTypeSyntax>>,
  ) -> ParserResult<NodeId> {
    let _ = self.expect(TokenType::LeftParen)?;
    let arguments = self.parse_arguments()?;
    let right_paren = self.expect(TokenType::RightParen)?.clone();
    let callee_span = self.get_span(&callee);
    let span = Span::merge(callee_span, &right_paren.span);

    Ok(self.allocate_expression(ASTExpression::Call(ASTCallExpression::new(callee, type_args, span, arguments))))
  }

  fn parse_arguments(&mut self) -> ParserResult<Vec<NodeId>> {
    if self.at(TokenType::RightParen) {
      return Ok(Vec::new());
    }

    let mut arguments = Vec::new();
    arguments.push(self.parse_expression(0)?);

    while self.eat(TokenType::Comma) {
      if self.at(TokenType::RightParen) {
        break;
      }
      arguments.push(self.parse_expression(0)?);
    }

    Ok(arguments)
  }

  fn parse_index(
    &mut self,
    base: NodeId,
  ) -> ParserResult<NodeId> {
    let _ = self.expect(TokenType::LeftBrack)?;
    let index = self.parse_expression(0)?;
    let right_bracket = self.expect(TokenType::RightBrack)?.clone();

    let left_span = self.get_span(&base).clone();
    let span = Span::merge(&left_span, &right_bracket.span);

    Ok(self.allocate_expression(ASTExpression::VectorAccess(ASTVectorAccess::new(base, span, index))))
  }

  fn parse_postfix(
    &mut self,
    left: NodeId,
  ) -> ParserResult<NodeId> {
    let operator = self.bump().clone();
    let left_span = self.get_span(&left).clone();
    let span = Span::merge(&left_span, &operator.span);

    Ok(self.allocate_expression(match operator.type_ {
      TokenType::Increment => ASTExpression::PostfixIncrement { expr: left, span },
      TokenType::Decrement => ASTExpression::PostfixDecrement { expr: left, span },
      _ => unreachable!(),
    }))
  }

  fn parse_prefix(&mut self) -> ParserResult<NodeId> {
    let token: Token = self.bump().clone();

    match token.type_ {
      TokenType::Int
      | TokenType::Float
      | TokenType::String
      | TokenType::Char
      | TokenType::Hex
      | TokenType::Binary
      | TokenType::True
      | TokenType::False
      | TokenType::Null => self.parse_literal(&token),
      TokenType::Identifier => {
        let first = token.clone();
        let start = first.span.clone();
        let mut segments: Vec<(SymbolId, Span)> = vec![(self.insert_symbol(&first), first.span.clone())];

        while self.eat(TokenType::DoubleColon) {
          let ident = self.expect(TokenType::Identifier)?.clone();
          segments.push((self.insert_symbol(&ident), ident.span.clone()));
        }

        // Check for generic record init: Type<Args> { field: value, ... }
        if self.at(TokenType::Less) && self.looks_like_type_args_for_init() {
          let type_args = self.parse_optional_type_args()?;
          if self.lookahead_record_init() {
            return self.parse_record_init(segments, type_args);
          }
          // Could be Type<Args>::Variant - continue as path and let :: handling proceed
          // For now, if no { follows, fall through to path handling
        }

        // Check for record init: Type { field: value, ... }
        if self.lookahead_record_init() {
          return self.parse_record_init(segments, None);
        }

        if segments.len() == 1 {
          Ok(self.allocate_expression(ASTExpression::Variable(ASTVariableExpression::new(segments[0].0, start))))
        } else {
          let end = self.previous().span.clone();
          let span = Span::merge(&start, &end);
          let path_segments: Vec<ASTPathSegment> = segments
            .into_iter()
            .map(|(id, seg_span)| ASTPathSegment::new(id, seg_span))
            .collect();
          Ok(self.allocate_expression(ASTExpression::Path(ASTPath::new(path_segments, span))))
        }
      },
      TokenType::Self_ => {
        // `self` keyword as variable reference
        let span = token.span.clone();
        let sym = self.insert_symbol(&token);
        Ok(self.allocate_expression(ASTExpression::Variable(ASTVariableExpression::new(sym, span))))
      },
      TokenType::LeftParen => {
        let expression = self.parse_expression(0)?;
        let left_span = self.get_span(&expression).clone();
        let right_paren = self.expect(TokenType::RightParen)?;
        let span = Span::merge(&left_span, &right_paren.span);

        Ok(self.allocate_expression(ASTExpression::Grouped(ASTGrouped::new(expression, span))))
      },
      TokenType::LeftBrack => {
        let lb_span = token.span.clone();

        let items = if self.at(TokenType::RightBrack) {
          Vec::new()
        } else {
          let mut items = Vec::new();
          items.push(self.parse_expression(0)?);

          while self.eat(TokenType::Comma) {
            if self.at(TokenType::RightBrack) {
              break;
            }
            items.push(self.parse_expression(0)?);
          }
          items
        };

        let rb = self.expect(TokenType::RightBrack)?.clone();
        let span = Span::merge(&lb_span, &rb.span);

        Ok(self.allocate_expression(ASTExpression::Vector(ASTVector::new(span, items))))
      },
      TokenType::Ampersand => {
        let ampersand_span = token.span.clone();
        let is_mutable = self.eat(TokenType::Mut);

        let (_, binding_power_right) = self.binding_powers(&TokenType::Ampersand).unwrap();
        let inner = self.parse_expression(binding_power_right)?;
        let span = Span::merge(&ampersand_span, self.get_span(&inner));

        Ok(self.allocate_expression(ASTExpression::Reference(ASTReference::new(inner, is_mutable, span))))
      },
      TokenType::Asterisk => {
        let (_, binding_power_right) = self.binding_powers(&TokenType::Asterisk).unwrap();
        let inner = self.parse_expression(binding_power_right)?;
        let span = Span::merge(&token.span, self.get_span(&inner));

        Ok(self.allocate_expression(ASTExpression::Dereference(ASTDereference::new(inner, span))))
      },
      TokenType::Increment | TokenType::Decrement | TokenType::Minus | TokenType::Bang | TokenType::Tilde => {
        let (_, binding_power_right) = self.binding_powers(&token.type_).unwrap();
        let right = self.parse_expression(binding_power_right)?;
        let span = Span::merge(&token.span, self.get_span(&right));

        Ok(self.allocate_expression(ASTExpression::Unary(ASTUnary::new(
          UnaryOperator::from(&token.type_),
          right,
          span,
        ))))
      },
      _ => Err(DiagnosticMessage::ExpectedExpression(token.span.clone())),
    }
  }

  fn parse_literal(
    &mut self,
    token: &Token,
  ) -> ParserResult<NodeId> {
    let value: IgnisLiteralValue = token.into();
    Ok(self.allocate_expression(ASTExpression::Literal(ASTLiteral::new(value, token.span.clone()))))
  }

  fn to_assignment_operator(token: &TokenType) -> Option<ASTAssignmentOperator> {
    match token {
      TokenType::Equal => Some(ASTAssignmentOperator::Assign),
      TokenType::AddAssign => Some(ASTAssignmentOperator::AddAssign),
      TokenType::SubtractAssign => Some(ASTAssignmentOperator::SubAssign),
      TokenType::MulAssign => Some(ASTAssignmentOperator::MulAssign),
      TokenType::DivAssign => Some(ASTAssignmentOperator::DivAssign),
      TokenType::ModAssign => Some(ASTAssignmentOperator::ModAssign),
      TokenType::AndAssign => Some(ASTAssignmentOperator::BitAndAssign),
      TokenType::OrAssign => Some(ASTAssignmentOperator::BitOrAssign),
      TokenType::XorAssign => Some(ASTAssignmentOperator::BitXorAssign),
      TokenType::NotAssign => Some(ASTAssignmentOperator::NotAssign),
      TokenType::LeftShiftAssign => Some(ASTAssignmentOperator::ShiftLeftAssign),
      TokenType::RightShiftAssign => Some(ASTAssignmentOperator::ShiftRightAssign),
      _ => None,
    }
  }

  fn allocate_expression(
    &mut self,
    expression: ASTExpression,
  ) -> NodeId {
    self.nodes.alloc(ASTNode::Expression(expression))
  }

  /// Parse member access: object.member or Type::member
  fn parse_member_access(
    &mut self,
    object: NodeId,
    op: ASTAccessOp,
  ) -> ParserResult<NodeId> {
    self.bump(); // consume . or ::

    let member_token = self.expect(TokenType::Identifier)?.clone();
    let member = self.insert_symbol(&member_token);
    let member_span = member_token.span.clone();

    let span = Span::merge(self.get_span(&object), &member_token.span);
    Ok(self.allocate_expression(ASTExpression::MemberAccess(ASTMemberAccess::new(
      object,
      op,
      member,
      member_span,
      span,
    ))))
  }

  /// Check if current position looks like a record init: { } or { identifier :
  fn lookahead_record_init(&self) -> bool {
    if !self.at(TokenType::LeftBrace) {
      return false;
    }

    let next = self.peek_nth(1);
    if next.type_ == TokenType::RightBrace {
      return true; // Empty: Foo {}
    }
    if next.type_ != TokenType::Identifier {
      return false;
    }
    self.peek_nth(2).type_ == TokenType::Colon
  }

  /// Parse record init expression: Type { field: value, ... } or Type<Args> { field: value, ... }
  fn parse_record_init(
    &mut self,
    path: Vec<(SymbolId, Span)>,
    type_args: Option<Vec<ignis_ast::type_::IgnisTypeSyntax>>,
  ) -> ParserResult<NodeId> {
    let start = path
      .first()
      .map(|p| p.1.clone())
      .unwrap_or_else(|| self.peek().span.clone());
    self.expect(TokenType::LeftBrace)?;

    let mut fields = Vec::new();

    while !self.at(TokenType::RightBrace) && !self.at(TokenType::Eof) {
      let field_name_token = self.expect(TokenType::Identifier)?.clone();
      let field_name = self.insert_symbol(&field_name_token);
      self.expect(TokenType::Colon)?;
      let value = self.parse_expression(0)?;

      let field_span = Span::merge(&field_name_token.span, self.get_span(&value));
      fields.push(ASTRecordInitField::new(field_name, value, field_span));

      if !self.eat(TokenType::Comma) {
        break;
      }
    }

    let end = self.expect(TokenType::RightBrace)?.clone();
    let span = Span::merge(&start, &end.span);

    Ok(self.allocate_expression(ASTExpression::RecordInit(ASTRecordInit::new(path, type_args, fields, span))))
  }
}

#[cfg(test)]
mod tests {
  use std::{cell::RefCell, rc::Rc};

  use ignis_ast::{
    ASTNode, NodeId,
    expressions::{ASTExpression, binary::ASTBinaryOperator, unary::UnaryOperator},
    statements::ASTStatement,
  };
  use ignis_type::{Store, file::SourceMap, symbol::SymbolTable, value::IgnisLiteralValue};

  use crate::{lexer::IgnisLexer, parser::IgnisParser};

  struct ParseResult {
    nodes: Store<ASTNode>,
    roots: Vec<NodeId>,
  }

  fn parse_expr(source: &str) -> ParseResult {
    let program = format!("function test(): void {{ {}; }}", source);
    let mut sm = SourceMap::new();
    let file_id = sm.add_file("test.ign", program.clone());

    let mut lexer = IgnisLexer::new(file_id, &program);
    lexer.scan_tokens();

    let symbols = Rc::new(RefCell::new(SymbolTable::new()));
    let mut parser = IgnisParser::new(lexer.tokens, symbols.clone());
    let (nodes, roots) = parser.parse().expect("parse failed");

    ParseResult { nodes, roots }
  }

  fn get_expr<'a>(result: &'a ParseResult) -> &'a ASTExpression {
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
    let stmt = result.nodes.get(&block.statements[0]);
    match stmt {
      ASTNode::Statement(ASTStatement::Expression(e)) => e,
      _ => panic!("expected expression statement"),
    }
  }

  #[test]
  fn parses_integer_literal() {
    let result = parse_expr("42");
    let expr = get_expr(&result);

    match expr {
      ASTExpression::Literal(lit) => match &lit.value {
        IgnisLiteralValue::Int32(v) => assert_eq!(*v, 42),
        other => panic!("expected Int32, got {:?}", other),
      },
      other => panic!("expected literal, got {:?}", other),
    }
  }

  #[test]
  fn parses_float_literal() {
    let result = parse_expr("3.14");
    let expr = get_expr(&result);

    match expr {
      ASTExpression::Literal(lit) => match &lit.value {
        IgnisLiteralValue::Float64(v) => assert!((v.into_inner() - 3.14).abs() < 0.001),
        other => panic!("expected Float64, got {:?}", other),
      },
      other => panic!("expected literal, got {:?}", other),
    }
  }

  #[test]
  fn parses_string_literal() {
    let result = parse_expr("\"hello\"");
    let expr = get_expr(&result);

    match expr {
      ASTExpression::Literal(lit) => match &lit.value {
        IgnisLiteralValue::String(s) => assert_eq!(s, "hello"),
        other => panic!("expected String, got {:?}", other),
      },
      other => panic!("expected literal, got {:?}", other),
    }
  }

  #[test]
  fn parses_boolean_literals() {
    let result = parse_expr("true");
    let expr = get_expr(&result);
    match expr {
      ASTExpression::Literal(lit) => match &lit.value {
        IgnisLiteralValue::Boolean(v) => assert!(*v),
        other => panic!("expected Boolean, got {:?}", other),
      },
      other => panic!("expected literal, got {:?}", other),
    }

    let result = parse_expr("false");
    let expr = get_expr(&result);
    match expr {
      ASTExpression::Literal(lit) => match &lit.value {
        IgnisLiteralValue::Boolean(v) => assert!(!*v),
        other => panic!("expected Boolean, got {:?}", other),
      },
      other => panic!("expected literal, got {:?}", other),
    }
  }

  #[test]
  fn parses_null_literal() {
    let result = parse_expr("null");
    let expr = get_expr(&result);

    match expr {
      ASTExpression::Literal(lit) => match &lit.value {
        IgnisLiteralValue::Null => {},
        other => panic!("expected Null, got {:?}", other),
      },
      other => panic!("expected literal, got {:?}", other),
    }
  }

  #[test]
  fn parses_binary_addition() {
    let result = parse_expr("1 + 2");
    let expr = get_expr(&result);

    match expr {
      ASTExpression::Binary(bin) => {
        assert_eq!(bin.operator, ASTBinaryOperator::Add);

        let left = result.nodes.get(&bin.left);
        match left {
          ASTNode::Expression(ASTExpression::Literal(lit)) => match &lit.value {
            IgnisLiteralValue::Int32(v) => assert_eq!(*v, 1),
            _ => panic!("expected Int32"),
          },
          _ => panic!("expected literal"),
        }

        let right = result.nodes.get(&bin.right);
        match right {
          ASTNode::Expression(ASTExpression::Literal(lit)) => match &lit.value {
            IgnisLiteralValue::Int32(v) => assert_eq!(*v, 2),
            _ => panic!("expected Int32"),
          },
          _ => panic!("expected literal"),
        }
      },
      other => panic!("expected binary, got {:?}", other),
    }
  }

  #[test]
  fn parses_binary_subtraction() {
    let result = parse_expr("5 - 3");
    let expr = get_expr(&result);

    match expr {
      ASTExpression::Binary(bin) => assert_eq!(bin.operator, ASTBinaryOperator::Subtract),
      other => panic!("expected binary, got {:?}", other),
    }
  }

  #[test]
  fn parses_binary_multiplication() {
    let result = parse_expr("4 * 2");
    let expr = get_expr(&result);

    match expr {
      ASTExpression::Binary(bin) => assert_eq!(bin.operator, ASTBinaryOperator::Multiply),
      other => panic!("expected binary, got {:?}", other),
    }
  }

  #[test]
  fn parses_binary_division() {
    let result = parse_expr("10 / 2");
    let expr = get_expr(&result);

    match expr {
      ASTExpression::Binary(bin) => assert_eq!(bin.operator, ASTBinaryOperator::Divide),
      other => panic!("expected binary, got {:?}", other),
    }
  }

  #[test]
  fn parses_comparison_operators() {
    let cases = [
      ("a < b", ASTBinaryOperator::LessThan),
      ("a > b", ASTBinaryOperator::GreaterThan),
      ("a <= b", ASTBinaryOperator::LessThanOrEqual),
      ("a >= b", ASTBinaryOperator::GreaterThanOrEqual),
      ("a == b", ASTBinaryOperator::Equal),
      ("a != b", ASTBinaryOperator::NotEqual),
    ];

    for (source, expected_op) in cases {
      let result = parse_expr(source);
      let expr = get_expr(&result);

      match expr {
        ASTExpression::Binary(bin) => assert_eq!(bin.operator, expected_op, "failed for: {}", source),
        other => panic!("expected binary for {}, got {:?}", source, other),
      }
    }
  }

  #[test]
  fn parses_logical_operators() {
    let result = parse_expr("a && b");
    let expr = get_expr(&result);
    match expr {
      ASTExpression::Binary(bin) => assert_eq!(bin.operator, ASTBinaryOperator::And),
      other => panic!("expected binary, got {:?}", other),
    }

    let result = parse_expr("a || b");
    let expr = get_expr(&result);
    match expr {
      ASTExpression::Binary(bin) => assert_eq!(bin.operator, ASTBinaryOperator::Or),
      other => panic!("expected binary, got {:?}", other),
    }
  }

  #[test]
  fn parses_ternary_expression() {
    let result = parse_expr("flag ? 1 : 2");
    let expr = get_expr(&result);

    match expr {
      ASTExpression::Ternary(ternary) => {
        let condition = result.nodes.get(&ternary.condition);
        match condition {
          ASTNode::Expression(ASTExpression::Variable(_)) => {},
          other => panic!("expected variable condition, got {:?}", other),
        }

        let then_expr = result.nodes.get(&ternary.then_expr);
        match then_expr {
          ASTNode::Expression(ASTExpression::Literal(lit)) => match &lit.value {
            IgnisLiteralValue::Int32(v) => assert_eq!(*v, 1),
            other => panic!("expected Int32, got {:?}", other),
          },
          other => panic!("expected literal then branch, got {:?}", other),
        }

        let else_expr = result.nodes.get(&ternary.else_expr);
        match else_expr {
          ASTNode::Expression(ASTExpression::Literal(lit)) => match &lit.value {
            IgnisLiteralValue::Int32(v) => assert_eq!(*v, 2),
            other => panic!("expected Int32, got {:?}", other),
          },
          other => panic!("expected literal else branch, got {:?}", other),
        }
      },
      other => panic!("expected ternary, got {:?}", other),
    }
  }

  #[test]
  fn parses_unary_negation() {
    let result = parse_expr("-x");
    let expr = get_expr(&result);

    match expr {
      ASTExpression::Unary(un) => assert_eq!(un.operator, UnaryOperator::Negate),
      other => panic!("expected unary, got {:?}", other),
    }
  }

  #[test]
  fn parses_unary_not() {
    let result = parse_expr("!flag");
    let expr = get_expr(&result);

    match expr {
      ASTExpression::Unary(un) => assert_eq!(un.operator, UnaryOperator::Not),
      other => panic!("expected unary, got {:?}", other),
    }
  }

  #[test]
  fn parses_grouped_expression() {
    let result = parse_expr("(1 + 2)");
    let expr = get_expr(&result);

    match expr {
      ASTExpression::Grouped(g) => {
        let inner = result.nodes.get(&g.expression);
        match inner {
          ASTNode::Expression(ASTExpression::Binary(bin)) => {
            assert_eq!(bin.operator, ASTBinaryOperator::Add);
          },
          other => panic!("expected binary inside grouped, got {:?}", other),
        }
      },
      other => panic!("expected grouped, got {:?}", other),
    }
  }

  #[test]
  fn precedence_multiplication_over_addition() {
    // 1 + 2 * 3 should parse as 1 + (2 * 3)
    let result = parse_expr("1 + 2 * 3");
    let expr = get_expr(&result);

    match expr {
      ASTExpression::Binary(bin) => {
        assert_eq!(bin.operator, ASTBinaryOperator::Add);
        // Right should be multiplication
        let right = result.nodes.get(&bin.right);
        match right {
          ASTNode::Expression(ASTExpression::Binary(inner)) => {
            assert_eq!(inner.operator, ASTBinaryOperator::Multiply);
          },
          other => panic!("expected multiplication on right, got {:?}", other),
        }
      },
      other => panic!("expected binary, got {:?}", other),
    }
  }

  #[test]
  fn precedence_parentheses_override() {
    // (1 + 2) * 3 should parse with addition first
    let result = parse_expr("(1 + 2) * 3");
    let expr = get_expr(&result);

    match expr {
      ASTExpression::Binary(bin) => {
        assert_eq!(bin.operator, ASTBinaryOperator::Multiply);
        // Left should be grouped containing addition
        let left = result.nodes.get(&bin.left);
        match left {
          ASTNode::Expression(ASTExpression::Grouped(g)) => {
            let inner = result.nodes.get(&g.expression);
            match inner {
              ASTNode::Expression(ASTExpression::Binary(inner_bin)) => {
                assert_eq!(inner_bin.operator, ASTBinaryOperator::Add);
              },
              other => panic!("expected addition inside grouped, got {:?}", other),
            }
          },
          other => panic!("expected grouped on left, got {:?}", other),
        }
      },
      other => panic!("expected binary, got {:?}", other),
    }
  }

  #[test]
  fn left_associativity_addition() {
    // 1 + 2 + 3 should parse as (1 + 2) + 3
    let result = parse_expr("1 + 2 + 3");
    let expr = get_expr(&result);

    match expr {
      ASTExpression::Binary(bin) => {
        assert_eq!(bin.operator, ASTBinaryOperator::Add);
        // Left should be another addition
        let left = result.nodes.get(&bin.left);
        match left {
          ASTNode::Expression(ASTExpression::Binary(inner)) => {
            assert_eq!(inner.operator, ASTBinaryOperator::Add);
          },
          other => panic!("expected addition on left, got {:?}", other),
        }
      },
      other => panic!("expected binary, got {:?}", other),
    }
  }

  #[test]
  fn parses_function_call_no_args() {
    let result = parse_expr("foo()");
    let expr = get_expr(&result);

    match expr {
      ASTExpression::Call(call) => {
        assert_eq!(call.arguments.len(), 0);
      },
      other => panic!("expected call, got {:?}", other),
    }
  }

  #[test]
  fn parses_function_call_with_args() {
    let result = parse_expr("foo(1, 2, 3)");
    let expr = get_expr(&result);

    match expr {
      ASTExpression::Call(call) => {
        assert_eq!(call.arguments.len(), 3);
      },
      other => panic!("expected call, got {:?}", other),
    }
  }

  #[test]
  fn parses_empty_vector() {
    let result = parse_expr("[]");
    let expr = get_expr(&result);

    match expr {
      ASTExpression::Vector(vec) => {
        assert_eq!(vec.items.len(), 0);
      },
      other => panic!("expected vector, got {:?}", other),
    }
  }

  #[test]
  fn parses_vector_with_elements() {
    let result = parse_expr("[1, 2, 3]");
    let expr = get_expr(&result);

    match expr {
      ASTExpression::Vector(vec) => {
        assert_eq!(vec.items.len(), 3);
      },
      other => panic!("expected vector, got {:?}", other),
    }
  }

  #[test]
  fn parses_vector_access() {
    let result = parse_expr("arr[0]");
    let expr = get_expr(&result);

    match expr {
      ASTExpression::VectorAccess(_) => {},
      other => panic!("expected vector access, got {:?}", other),
    }
  }

  #[test]
  fn parses_reference() {
    let result = parse_expr("&x");
    let expr = get_expr(&result);

    match expr {
      ASTExpression::Reference(r) => {
        assert!(!r.mutable);
      },
      other => panic!("expected reference, got {:?}", other),
    }
  }

  #[test]
  fn parses_mutable_reference() {
    let result = parse_expr("&mut x");
    let expr = get_expr(&result);

    match expr {
      ASTExpression::Reference(r) => {
        assert!(r.mutable);
      },
      other => panic!("expected mutable reference, got {:?}", other),
    }
  }

  #[test]
  fn parses_dereference() {
    let result = parse_expr("*ptr");
    let expr = get_expr(&result);

    match expr {
      ASTExpression::Dereference(_) => {},
      other => panic!("expected dereference, got {:?}", other),
    }
  }

  #[test]
  fn parses_prefix_increment() {
    let result = parse_expr("++x");
    let expr = get_expr(&result);

    match expr {
      ASTExpression::Unary(un) => {
        assert_eq!(un.operator, UnaryOperator::Increment);
      },
      other => panic!("expected unary increment, got {:?}", other),
    }
  }

  #[test]
  fn parses_postfix_increment() {
    let result = parse_expr("x++");
    let expr = get_expr(&result);

    match expr {
      ASTExpression::PostfixIncrement { .. } => {},
      other => panic!("expected postfix increment, got {:?}", other),
    }
  }

  #[test]
  fn parses_path_expression() {
    let result = parse_expr("Math::add(1, 2)");
    let expr = get_expr(&result);

    match expr {
      ASTExpression::Call(call) => {
        let callee = result.nodes.get(&call.callee);
        match callee {
          ASTNode::Expression(ASTExpression::Path(path)) => {
            assert_eq!(path.segments.len(), 2);
          },
          other => panic!("expected path as callee, got {:?}", other),
        }
      },
      other => panic!("expected call, got {:?}", other),
    }
  }

  #[test]
  fn parses_simple_identifier_not_path() {
    let result = parse_expr("foo()");
    let expr = get_expr(&result);

    match expr {
      ASTExpression::Call(call) => {
        let callee = result.nodes.get(&call.callee);
        match callee {
          ASTNode::Expression(ASTExpression::Variable(_)) => {},
          other => panic!("expected variable as callee (not path), got {:?}", other),
        }
      },
      other => panic!("expected call, got {:?}", other),
    }
  }

  #[test]
  fn parses_nested_path() {
    let result = parse_expr("Std::Io::read()");
    let expr = get_expr(&result);

    match expr {
      ASTExpression::Call(call) => {
        let callee = result.nodes.get(&call.callee);
        match callee {
          ASTNode::Expression(ASTExpression::Path(path)) => {
            assert_eq!(path.segments.len(), 3);
          },
          other => panic!("expected path as callee, got {:?}", other),
        }
      },
      other => panic!("expected call, got {:?}", other),
    }
  }

  // =========================================================================
  // GENERIC CALL EXPRESSION TESTS
  // =========================================================================

  #[test]
  fn parses_generic_call_single_type_arg() {
    let result = parse_expr("identity<i32>(42)");
    let expr = get_expr(&result);

    match expr {
      ASTExpression::Call(call) => {
        let type_args = call.type_args.as_ref().expect("should have type args");
        assert_eq!(type_args.len(), 1);
        assert_eq!(call.arguments.len(), 1);
      },
      other => panic!("expected call, got {:?}", other),
    }
  }

  #[test]
  fn parses_generic_call_multiple_type_args() {
    let result = parse_expr("map<i32, bool>(x, f)");
    let expr = get_expr(&result);

    match expr {
      ASTExpression::Call(call) => {
        let type_args = call.type_args.as_ref().expect("should have type args");
        assert_eq!(type_args.len(), 2);
        assert_eq!(call.arguments.len(), 2);
      },
      other => panic!("expected call, got {:?}", other),
    }
  }

  #[test]
  fn parses_generic_call_with_pointer_type_arg() {
    let result = parse_expr("create<*i32>()");
    let expr = get_expr(&result);

    match expr {
      ASTExpression::Call(call) => {
        let type_args = call.type_args.as_ref().expect("should have type args");
        assert_eq!(type_args.len(), 1);
      },
      other => panic!("expected call, got {:?}", other),
    }
  }

  #[test]
  fn parses_generic_call_with_reference_type_arg() {
    let result = parse_expr("process<&mut i32>(x)");
    let expr = get_expr(&result);

    match expr {
      ASTExpression::Call(call) => {
        let type_args = call.type_args.as_ref().expect("should have type args");
        assert_eq!(type_args.len(), 1);
      },
      other => panic!("expected call, got {:?}", other),
    }
  }

  #[test]
  fn parses_generic_call_with_function_type_arg() {
    let result = parse_expr("transform<(i32) -> bool>(x, f)");
    let expr = get_expr(&result);

    match expr {
      ASTExpression::Call(call) => {
        let type_args = call.type_args.as_ref().expect("should have type args");
        assert_eq!(type_args.len(), 1);
      },
      other => panic!("expected call, got {:?}", other),
    }
  }

  #[test]
  fn parses_non_generic_call_has_no_type_args() {
    let result = parse_expr("foo(1, 2)");
    let expr = get_expr(&result);

    match expr {
      ASTExpression::Call(call) => {
        assert!(call.type_args.is_none());
      },
      other => panic!("expected call, got {:?}", other),
    }
  }

  #[test]
  fn parses_comparison_not_as_type_args() {
    // a < b should parse as comparison, not type args (b is followed by semicolon, not >)
    let result = parse_expr("a < b");
    let expr = get_expr(&result);

    match expr {
      ASTExpression::Binary(bin) => {
        assert_eq!(bin.operator, ASTBinaryOperator::LessThan);
      },
      other => panic!("expected binary comparison, got {:?}", other),
    }
  }

  #[test]
  fn parses_less_than_comparison_with_literal() {
    // 1 < 2 should be comparison, not type args
    let result = parse_expr("1 < 2");
    let expr = get_expr(&result);

    match expr {
      ASTExpression::Binary(bin) => {
        assert_eq!(bin.operator, ASTBinaryOperator::LessThan);
      },
      other => panic!("expected binary comparison, got {:?}", other),
    }
  }

  #[test]
  fn parses_comparison_chain_not_as_type_args() {
    // a < b should still be a comparison
    let result = parse_expr("a < b");
    let expr = get_expr(&result);

    match expr {
      ASTExpression::Binary(bin) => {
        assert_eq!(bin.operator, ASTBinaryOperator::LessThan);
      },
      other => panic!("expected binary comparison, got {:?}", other),
    }
  }

  #[test]
  fn parses_generic_call_on_path() {
    let result = parse_expr("Container::new<i32>()");
    let expr = get_expr(&result);

    match expr {
      ASTExpression::Call(call) => {
        let type_args = call.type_args.as_ref().expect("should have type args");
        assert_eq!(type_args.len(), 1);

        let callee = result.nodes.get(&call.callee);
        match callee {
          ASTNode::Expression(ASTExpression::Path(path)) => {
            assert_eq!(path.segments.len(), 2);
          },
          other => panic!("expected path as callee, got {:?}", other),
        }
      },
      other => panic!("expected call, got {:?}", other),
    }
  }

  #[test]
  fn parses_generic_call_with_trailing_comma() {
    let result = parse_expr("foo<i32,>(x)");
    let expr = get_expr(&result);

    match expr {
      ASTExpression::Call(call) => {
        let type_args = call.type_args.as_ref().expect("should have type args");
        assert_eq!(type_args.len(), 1);
      },
      other => panic!("expected call, got {:?}", other),
    }
  }

  #[test]
  fn parses_generic_call_with_nested_generic_type() {
    // Note: nested generics like Box<Box<i32>> would require >> splitting in the lexer
    // For now, we test with a single level of nesting that doesn't produce >>
    let result = parse_expr("process<*Box>(x)");
    let expr = get_expr(&result);

    match expr {
      ASTExpression::Call(call) => {
        let type_args = call.type_args.as_ref().expect("should have type args");
        assert_eq!(type_args.len(), 1);
      },
      other => panic!("expected call, got {:?}", other),
    }
  }

  #[test]
  fn parses_generic_call_with_array_type_arg() {
    // Ignis uses T[10] syntax for fixed arrays, not [T; 10]
    let result = parse_expr("foo<i32[10]>(arr)");
    let expr = get_expr(&result);

    match expr {
      ASTExpression::Call(call) => {
        let type_args = call.type_args.as_ref().expect("should have type args");
        assert_eq!(type_args.len(), 1);
      },
      other => panic!("expected call, got {:?}", other),
    }
  }
}
