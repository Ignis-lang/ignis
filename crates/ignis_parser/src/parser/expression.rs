use ignis_ast::{
  ASTNode, NodeId,
  expressions::{
    ASTExpression,
    binary::{ASTBinary, ASTBinaryOperator},
    call::ASTCallExpression,
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
      let right = self.parse_expression(rbp)?;
      let span = Span::merge(self.get_span(&left), self.get_span(&right));
      left = self.allocate_expression(ASTExpression::Binary(ASTBinary::new(
        left,
        ASTBinaryOperator::from(&op),
        right,
        span,
      )));
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

  fn allocate_expression(
    &mut self,
    expression: ASTExpression,
  ) -> NodeId {
    self.nodes.alloc(ASTNode::Expression(expression))
  }
}
