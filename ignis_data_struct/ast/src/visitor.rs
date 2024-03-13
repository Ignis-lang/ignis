use crate::{
  expression::{
    array::Array, array_access::ArrayAccess, assign::Assign, binary::Binary, call::Call, get::Get, grouping::Grouping,
    literal::Literal, logical::Logical, method_call::MethodCall, new::NewExpression, set::Set, ternary::Ternary,
    this::This, unary::Unary, variable::VariableExpression,
  },
  statement::{
    block::Block, break_statement::BreakStatement, class::Class, continue_statement::Continue, enum_statement::Enum,
    expression::ExpressionStatement, for_of::ForOf, for_statement::For, function::FunctionStatement,
    if_statement::IfStatement, import::Import, interface_statement::InterfaceStatement, method::MethodStatement,
    property::PropertyStatement, return_statement::Return, variable::Variable, while_statement::WhileStatement,
  },
};

pub trait Visitor<R> {
  // Expression
  fn visit_binary_expression(
    &mut self,
    expression: &Binary,
  ) -> R;
  fn visit_grouping_expression(
    &mut self,
    expression: &Grouping,
  ) -> R;
  fn visit_literal_expression(
    &mut self,
    expression: &Literal,
  ) -> R;
  fn visit_unary_expression(
    &mut self,
    expression: &Unary,
  ) -> R;
  fn visit_variable_expression(
    &mut self,
    variable: &VariableExpression,
  ) -> R;
  fn visit_assign_expression(
    &mut self,
    expression: &Assign,
  ) -> R;
  fn visit_logical_expression(
    &mut self,
    expression: &Logical,
  ) -> R;
  fn visit_ternary_expression(
    &mut self,
    expression: &Ternary,
  ) -> R;
  fn visit_call_expression(
    &mut self,
    expression: &Call,
  ) -> R;
  fn visit_array_expression(
    &mut self,
    expression: &Array,
  ) -> R;
  fn visit_new_expression(
    &mut self,
    expression: &NewExpression,
  ) -> R;
  fn visit_get_expression(
    &mut self,
    expression: &Get,
  ) -> R;
  fn visit_set_expression(
    &mut self,
    set: &Set,
  ) -> R;
  fn visit_method_call_expression(
    &mut self,
    method: &MethodCall,
  ) -> R;
  fn visit_array_access_expression(
    &mut self,
    array: &ArrayAccess,
  ) -> R;
  fn visit_this_expression(
    &mut self,
    this: &This,
  ) -> R;

  // Statements
  fn visit_expression_statement(
    &mut self,
    statement: &ExpressionStatement,
  ) -> R;
  fn visit_variable_statement(
    &mut self,
    variable: &Variable,
  ) -> R;
  fn visit_block(
    &mut self,
    block: &Block,
  ) -> R;
  fn visit_if_statement(
    &mut self,
    statement: &IfStatement,
  ) -> R;
  fn visit_while_statement(
    &mut self,
    statement: &WhileStatement,
  ) -> R;
  fn visit_function_statement(
    &mut self,
    statement: &FunctionStatement,
  ) -> R;
  fn visit_return_statement(
    &mut self,
    statement: &Return,
  ) -> R;
  fn visit_class_statement(
    &mut self,
    statement: &Class,
  ) -> R;
  fn visit_for_statement(
    &mut self,
    statement: &For,
  ) -> R;
  fn visit_for_of_statement(
    &mut self,
    statement: &ForOf,
  ) -> R;
  fn visit_import_statement(
    &mut self,
    statement: &Import,
  ) -> R;
  fn visit_break_statement(
    &mut self,
    statement: &BreakStatement,
  ) -> R;
  fn visit_continue_statement(
    &mut self,
    statement: &Continue,
  ) -> R;
  fn visit_method_statement(
    &mut self,
    statement: &MethodStatement,
  ) -> R;
  fn visit_property_statement(
    &mut self,
    statement: &PropertyStatement,
  ) -> R;
  fn visit_interface_statement(
    &mut self,
    statement: &InterfaceStatement,
  ) -> R;
  fn visit_enum_statement(
    &mut self,
    statement: &Enum,
  ) -> R;
}
