use ignis_ast::{
  ASTNode, NodeId,
  expressions::{
    ASTExpression,
    assignment::{ASTAssignment, ASTAssignmentOperator},
    binary::{ASTBinary, ASTBinaryOperator},
    call::ASTCallExpression,
    cast::ASTCast,
    dereference::ASTDereference,
    grouped::ASTGrouped,
    literal::ASTLiteral,
    reference::ASTReference,
    unary::{ASTUnary, UnaryOperator},
    variable::ASTVariableExpression,
    vector::ASTVector,
    vector_access::ASTVectorAccess,
  },
};
use ignis_diagnostics::message::DiagnosticMessage;
use ignis_token::{token::Token, token_types::TokenType};
use ignis_type::{span::Span, value::IgnisLiteralValue};

use super::{IgnisParser, ParserResult};

impl IgnisParser {
  pub(crate) fn parse_expression(
    &mut self,
    min_bp: u16,
  ) -> ParserResult<NodeId> {
    let mut left: NodeId = self.parse_prefix()?;

    loop {
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
          left = self.parse_call(left)?;
          continue;
        }
      }

      if self.at(TokenType::LeftBrack) {
        left = self.parse_index(left)?;
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
        left = self.allocate_expression(ASTExpression::Cast(ASTCast::new(
          span,
          target_type,
          left,
        )));
        continue;
      }

      let right = self.parse_expression(rbp)?;
      let span = Span::merge(self.get_span(&left), self.get_span(&right));

      if let Some(assign_op) = Self::to_assignment_operator(&op) {
        left = self.allocate_expression(ASTExpression::Assignment(ASTAssignment::new(
          left,
          right,
          assign_op,
          span,
        )));
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

  fn parse_call(
    &mut self,
    callee: NodeId,
  ) -> ParserResult<NodeId> {
    let _ = self.expect(TokenType::LeftParen)?;
    let arguments = self.parse_arguments()?;
    let right_paren = self.expect(TokenType::RightParen)?.clone();
    let callee_span = self.get_span(&callee);
    let span = Span::merge(callee_span, &right_paren.span);

    Ok(self.allocate_expression(ASTExpression::Call(ASTCallExpression::new(callee, span, arguments))))
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
        let symbol = self.insert_symbol(&token);

        Ok(self.allocate_expression(ASTExpression::Variable(ASTVariableExpression::new(symbol, token.span.clone()))))
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
}

#[cfg(test)]
mod tests {
  use std::{cell::RefCell, rc::Rc};

  use ignis_ast::{
    ASTNode, NodeId,
    expressions::{
      ASTExpression,
      binary::ASTBinaryOperator,
      unary::UnaryOperator,
    },
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
        IgnisLiteralValue::String(s) => assert_eq!(s, "\"hello\""),
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
}
