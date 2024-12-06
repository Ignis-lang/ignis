use ignis_token::token::Token;

use crate::expressions::binary::ASTBinary;

pub trait ASTVisitor<R> {
  // #region Expressions
  fn visit_binary_expression(
    &mut self,
    expression: &ASTBinary,
  ) -> R;
  fn visit_literal_expression(
    &mut self,
    expression: &crate::expressions::literal::ASTLiteral,
  ) -> R;
  fn visit_logical_expression(
    &mut self,
    expression: &crate::expressions::logical::ASTLogical,
  ) -> R;
  fn visit_ternary_expression(
    &mut self,
    expression: &crate::expressions::ternary::ASTTernary,
  ) -> R;
  fn visit_cast_expression(
    &mut self,
    expression: &crate::expressions::cast::ASTCast,
  ) -> R;
  fn visit_unary_expression(
    &mut self,
    expression: &crate::expressions::unary::ASTUnary,
  ) -> R;
  fn visit_variable_expression(
    &mut self,
    expression: &crate::expressions::variable::ASTVariableExpression,
  ) -> R;
  fn visit_vector_access_expression(
    &mut self,
    expression: &crate::expressions::vector_access::ASTVectorAccess,
  ) -> R;
  fn visit_vector_expression(
    &mut self,
    expression: &crate::expressions::vector::ASTVector,
  ) -> R;
  fn visit_grouping_expression(
    &mut self,
    expression: &crate::expressions::grouping::ASTGrouping,
  ) -> R;
  fn visit_member_access_expression(
    &mut self,
    expression: &crate::expressions::member_access::ASTMemberAccess,
  ) -> R;
  fn visit_assignment_expression(
    &mut self,
    expression: &crate::expressions::assign::ASTAssignment,
  ) -> R;
  fn visit_call_expression(
    &mut self,
    expression: &crate::expressions::call::ASTCall,
  ) -> R;
  fn visit_match_expression(
    &mut self,
    expression: &crate::expressions::match_expression::ASTMatchExpression,
  ) -> R;
  fn visit_lambda_expression(
    &mut self,
    expression: &crate::expressions::lambda::ASTLambda,
  ) -> R;
  fn visit_object_expression(
    &mut self,
    expression: &crate::expressions::object_literal::ASTObject,
  ) -> R;
  fn visit_this_expression(
    &mut self,
    expression: &crate::expressions::this::ASTThis,
  ) -> R;
  fn visit_meta_expression(
    &mut self,
    expression: &crate::expressions::meta::ASTMeta,
  ) -> R;
  fn visit_meta_entity_expression(
    &mut self,
    expression: &crate::expressions::meta::ASTMetaEntity,
  ) -> R;
  fn visit_spread_expression(
    &mut self,
    expression: &crate::expressions::spread::ASTSpread,
  ) -> R;
  // #endregion

  // #region Statements
  fn visit_comment_statement(
    &mut self,
    comment: &crate::statements::comment::ASTComment,
  ) -> R;
  fn visit_expression_statement(
    &mut self,
    expression: &crate::expressions::ASTExpression,
  ) -> R;
  fn visit_constant_statement(
    &mut self,
    expression: &crate::statements::constant::ASTConstant,
  ) -> R;
  fn visit_function_statement(
    &mut self,
    expression: &crate::statements::function::ASTFunction,
  ) -> R;
  fn visit_variable_statement(
    &mut self,
    expression: &crate::statements::variable::ASTVariable,
  ) -> R;
  fn visit_block_statement(
    &mut self,
    expression: &crate::statements::block::ASTBlock,
  ) -> R;
  fn visit_if_statement(
    &mut self,
    expression: &crate::statements::if_statement::ASTIf,
  ) -> R;
  fn visit_while_statement(
    &mut self,
    expression: &crate::statements::while_statement::ASTWhile,
  ) -> R;
  fn visit_for_statement(
    &mut self,
    expression: &crate::statements::for_statement::ASTFor,
  ) -> R;
  fn visit_for_of_statement(
    &mut self,
    expression: &crate::statements::for_of_statement::ASTForOf,
  ) -> R;
  fn visit_break_statement(
    &mut self,
    token: &Token,
  ) -> R;
  fn visit_continue_statement(
    &mut self,
    token: &Token,
  ) -> R;
  fn visit_return_statement(
    &mut self,
    return_: &crate::statements::return_::ASTReturn,
  ) -> R;
  fn visit_import_statement(
    &mut self,
    import: &crate::statements::import::ASTImport,
  ) -> R;
  fn visit_record_statement(
    &mut self,
    record: &crate::statements::record::ASTRecord,
  ) -> R;
  fn visit_method_statement(
    &mut self,
    method: &crate::statements::method::ASTMethod,
  ) -> R;
  fn visit_property_statement(
    &mut self,
    property: &crate::statements::property::ASTProperty,
  ) -> R;
  fn visit_extern_statement(
    &mut self,
    extern_: &crate::statements::r#extern::ASTExtern,
  ) -> R;
  fn visit_include_statement(
    &mut self,
    include: &Token,
  ) -> R;
  fn visit_source_statement(
    &mut self,
    source: &Token,
  ) -> R;
  fn visit_namespace_statement(
    &mut self,
    namespace: &crate::statements::namespace::ASTNamespace,
  ) -> R;
  fn visit_type_alias_statement(
    &mut self,
    type_alias: &crate::statements::type_alias::ASTTypeAlias,
  ) -> R;
  // #endregion
}
