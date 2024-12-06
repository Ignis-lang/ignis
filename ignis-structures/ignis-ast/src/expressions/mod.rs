pub mod assign;
pub mod binary;
pub mod call;
pub mod cast;
pub mod decorator;
pub mod grouping;
pub mod lambda;
pub mod literal;
pub mod logical;
pub mod match_expression;
pub mod member_access;
pub mod meta;
pub mod method_call;
pub mod new;
pub mod object_literal;
pub mod set;
pub mod ternary;
pub mod this;
pub mod unary;
pub mod variable;
pub mod vector;
pub mod vector_access;
pub mod spread;

use assign::ASTAssignment;
use call::ASTCall;
use lambda::ASTLambda;
use match_expression::ASTMatchExpression;
use member_access::ASTMemberAccess;
use meta::{ASTMeta, ASTMetaEntity};
use object_literal::ASTObject;
use spread::ASTSpread;
use this::ASTThis;
use vector::ASTVector;
use vector_access::ASTVectorAccess;
use cast::ASTCast;
use grouping::ASTGrouping;
use ignis_data_type::DataType;
use ignis_token::token::Token;
use serde_json::json;
use ternary::ASTTernary;
use unary::ASTUnary;
use variable::ASTVariableExpression;

use self::{binary::ASTBinary, literal::ASTLiteral, logical::ASTLogical};
use crate::{statements::ASTStatement, visitor::ASTVisitor};

#[derive(Debug, Clone, PartialEq)]
pub enum ASTExpression {
  Binary(Box<ASTBinary>),
  Literal(Box<ASTLiteral>),
  Logical(Box<ASTLogical>),
  Ternary(Box<ASTTernary>),
  Grouping(Box<ASTGrouping>),
  Cast(Box<ASTCast>),
  Unary(Box<ASTUnary>),
  Variable(Box<ASTVariableExpression>),
  VectorAccess(Box<ASTVectorAccess>),
  Vector(Box<ASTVector>),
  MemberAccess(Box<ASTMemberAccess>),
  Call(Box<ASTCall>),
  Assigment(Box<ASTAssignment>),
  Match(Box<ASTMatchExpression>),
  Lambda(Box<ASTLambda>),
  Object(Box<ASTObject>),
  This(Box<ASTThis>),
  Meta(Box<ASTMeta>),
  MetaEntity(Box<ASTMetaEntity>),
  Spread(Box<ASTSpread>),
}

impl From<ASTExpression> for DataType {
  fn from(value: ASTExpression) -> Self {
    match value {
      ASTExpression::Binary(binary) => binary.data_type,
      ASTExpression::Literal(literal) => literal.value.into(),
      ASTExpression::Logical(logical) => logical.data_type,
      ASTExpression::Ternary(ternary) => ternary.data_type,
      ASTExpression::Cast(cast) => cast.target_type,
      ASTExpression::Unary(unary) => unary.data_type,
      ASTExpression::Variable(variable) => variable.data_type,
      ASTExpression::VectorAccess(_) => DataType::Pending,
      ASTExpression::Vector(vector) => vector.data_type,
      ASTExpression::Grouping(grouping) => (*grouping.expression).into(),
      ASTExpression::MemberAccess(member_access) => (*member_access.object).into(),
      ASTExpression::Call(call) => (*call.callee).into(),
      ASTExpression::Assigment(assignment) => (*assignment.left).into(),
      ASTExpression::Match(match_) => (*match_.expression).into(),
      ASTExpression::Lambda(lambda) => lambda.lambda_type.clone(),
      ASTExpression::Object(object_literal) => object_literal.data_type.clone(),
      ASTExpression::This(_) => DataType::Pending,
      ASTExpression::Meta(_) | ASTExpression::MetaEntity(_) => DataType::Pending,
      ASTExpression::Spread(spread) => (*spread.expression).into(),
    }
  }
}

impl Into<Token> for &ASTExpression {
  fn into(self) -> Token {
    match self {
      ASTExpression::Binary(binary) => binary.operator.clone(),
      ASTExpression::Literal(literal) => literal.token.clone(),
      ASTExpression::Logical(logical) => logical.operator.clone(),
      ASTExpression::Ternary(ternary) => ternary.token.as_ref().clone(),
      ASTExpression::Cast(cast) => cast.token.clone(),
      ASTExpression::Unary(unary) => unary.operator.clone(),
      ASTExpression::Variable(variable) => variable.name.clone(),
      ASTExpression::VectorAccess(vector_access) => vector_access.name.as_ref().clone(),
      ASTExpression::Vector(vector) => vector.token.clone(),
      ASTExpression::Grouping(grouping) => (&grouping.expression.as_ref().clone()).into(),
      ASTExpression::MemberAccess(member_access) => member_access.member.as_ref().clone(),
      ASTExpression::Call(call) => (&call.callee.as_ref().clone()).into(),
      ASTExpression::Assigment(assignment) => assignment.operator.as_ref().clone(),
      ASTExpression::Match(match_) => (&match_.expression.as_ref().clone()).into(),
      ASTExpression::Object(object) => object.token.clone(),
      ASTExpression::Lambda(_) => todo!(),
      ASTExpression::This(this) => this.token.clone(),
      ASTExpression::Meta(meta) => meta.expression.clone().as_ref().into(),
      ASTExpression::MetaEntity(meta) => meta.metas.first().unwrap().expression.clone().as_ref().into(),
      ASTExpression::Spread(spread) => spread.token.clone(),
    }
  }
}

impl ASTExpression {
  pub fn accept<R>(
    &self,
    visitor: &mut dyn ASTVisitor<R>,
  ) -> R {
    match self {
      ASTExpression::Binary(binary) => visitor.visit_binary_expression(binary),
      ASTExpression::Literal(literal) => visitor.visit_literal_expression(literal),
      ASTExpression::Logical(logical) => visitor.visit_logical_expression(logical),
      ASTExpression::Ternary(ternary) => visitor.visit_ternary_expression(ternary),
      ASTExpression::Cast(cast) => visitor.visit_cast_expression(cast),
      ASTExpression::Unary(unary) => visitor.visit_unary_expression(unary),
      ASTExpression::Variable(variable) => visitor.visit_variable_expression(variable),
      ASTExpression::VectorAccess(vector_access) => visitor.visit_vector_access_expression(vector_access),
      ASTExpression::Vector(vector) => visitor.visit_vector_expression(vector),
      ASTExpression::Grouping(grouping) => visitor.visit_grouping_expression(grouping),
      ASTExpression::MemberAccess(member_access) => visitor.visit_member_access_expression(member_access),
      ASTExpression::Call(call) => visitor.visit_call_expression(call),
      ASTExpression::Assigment(assignment) => visitor.visit_assignment_expression(assignment),
      ASTExpression::Match(match_) => visitor.visit_match_expression(match_),
      ASTExpression::Lambda(lambda) => visitor.visit_lambda_expression(lambda),
      ASTExpression::Object(object) => visitor.visit_object_expression(object),
      ASTExpression::This(this) => visitor.visit_this_expression(this),
      ASTExpression::Meta(meta) => visitor.visit_meta_expression(meta),
      ASTExpression::MetaEntity(meta) => visitor.visit_meta_entity_expression(meta),
      ASTExpression::Spread(spread) => visitor.visit_spread_expression(spread),
    }
  }

  pub fn to_json(&self) -> serde_json::Value {
    match self {
      ASTExpression::Binary(binary) => {
        json!({
          "type": "BINARY",
          "left": binary.left.to_json(),
          "operator": binary.operator.lexeme,
          "right": binary.right.to_json(),
        })
      },
      ASTExpression::Literal(literal) => {
        json!({
          "type": "LITERAL",
          "value": literal.value,
          "token": literal.token.lexeme,
        })
      },
      ASTExpression::Logical(logical) => {
        json!({
          "type": "LOGICAL",
          "left": logical.left.to_json(),
          "operator": logical.operator.lexeme,
          "right": logical.right.to_json(),
        })
      },
      ASTExpression::Ternary(ternary) => {
        json!({
          "type": "TERNARY",
          "condition": ternary.condition.to_json(),
          "then_branch": ternary.then_branch.to_json(),
          "else_branch": ternary.else_branch.to_json(),
          "token": ternary.token.lexeme,
        })
      },
      ASTExpression::Cast(cast) => {
        json!({
          "type": "CAST",
          "token": cast.token.lexeme,
          "target_type": cast.target_type,
          "operand": cast.operand.to_json(),
        })
      },
      ASTExpression::Unary(unary) => {
        json!({
          "type": "UNARY",
          "operator": unary.operator.lexeme,
          "operand": unary.operator,
        })
      },
      ASTExpression::Variable(variable) => {
        json!({
          "type": "VARIABLE",
          "name": variable.name.lexeme,
          "data_type": variable.data_type,
          "metadata": variable.metadata.to_json(),
        })
      },
      ASTExpression::VectorAccess(vector_access) => {
        json!({
          "type": "VECTOR_ACCESS",
          "name": vector_access.name.lexeme,
          "variable": vector_access.variable.to_json(),
          "index": vector_access.index.to_json(),
        })
      },
      ASTExpression::Vector(vector) => {
        json!({
          "type": "VECTOR",
          "token": vector.token.lexeme,
          "elements": vector.elements.iter().map(|e| e.to_json()).collect::<Vec<serde_json::Value>>(),
          "data_type": vector.data_type,
        })
      },
      ASTExpression::Grouping(grouping) => {
        json!({
          "type": "GROUPING",
          "expression": grouping.expression.to_json(),
        })
      },
      ASTExpression::MemberAccess(member_access) => {
        json!({
          "type": "MEMBER_ACCESS",
          "object": member_access.object.to_json(),
          "member": member_access.member,
        })
      },
      ASTExpression::Call(call) => {
        json!({
          "type": "CALL",
          "callee": call.callee.to_json(),
          "arguments": call.arguments.iter().map(|a| a.to_json()).collect::<Vec<serde_json::Value>>(),
        })
      },
      ASTExpression::Assigment(assignment) => {
        json!({
          "type": "ASSIGNMENT",
          "left": assignment.left.to_json(),
          "operator": assignment.operator.lexeme,
          "right": assignment.right.to_json(),
        })
      },
      ASTExpression::Match(match_) => {
        json!({
          "type": "MATCH",
          "expression": match_.expression.to_json(),
          "cases": match_.cases.iter().map(|c| c.to_json()).collect::<Vec<serde_json::Value>>(),
        })
      },
      ASTExpression::Lambda(lambda) => {
        json!({
          "type": "LAMBDA",
          "parameters": lambda.parameters.iter().map(|p| ASTStatement::Variable(Box::new(p.clone())).to_json()).collect::<Vec<serde_json::Value>>(),
          "body": lambda.body.to_json(),
          "return_type": lambda.return_type,
        })
      },
      ASTExpression::Object(object) => {
        json!({
          "type": "OBJECT",
          "properties": object.properties.iter().map(|p|ASTStatement::Property(Box::new(p.clone())).to_json()).collect::<Vec<serde_json::Value>>(),
          "methods": object.methods.iter().map(|m|ASTStatement::Method(Box::new(m.clone())).to_json()).collect::<Vec<serde_json::Value>>(),
          "data_type": object.data_type,
        })
      },
      ASTExpression::This(this) => {
        json!({
          "type": "THIS",
          "token": this.token.lexeme,
        })
      },
      ASTExpression::Meta(meta) => {
        json!({
          "type": "META",
          "expression": meta.expression.to_json(),
        })
      },
      ASTExpression::MetaEntity(meta) => {
        json!({
          "type": "META_ENTITY",
          "metas": meta.metas.iter().map(|m| ASTExpression::Meta(Box::new(m.clone())).to_json()).collect::<Vec<serde_json::Value>>(),
          "entity": meta.entity.as_ref().map(|e| e.to_json()),
        })
      },
      ASTExpression::Spread(spread) => {
        json!({
          "type": "SPREAD",
          "expression": spread.expression.to_json(),
          "token": spread.token.lexeme,
        })
      },
    }
  }
}
