//! AST Display Module - Lisp-style S-expression Formatter
//!
//! This module provides functionality to display the Abstract Syntax Tree (AST)
//! in a Lisp-style S-expression format. It handles arena-allocated nodes (NodeId),
//! symbol resolution (SymbolId), and metadata formatting.
//!
//! # Example Usage
//!
//! ```rust,ignore
//! use std::{cell::RefCell, rc::Rc};
//! use ignis_ast::display::{format_ast, format_ast_nodes};
//! use ignis_type::{Store, symbol::SymbolTable};
//! use ignis_ast::{ASTNode, NodeId};
//!
//! // Assuming you have:
//! // - nodes: Store<ASTNode> (arena allocator with all AST nodes)
//! // - symbols: Rc<RefCell<SymbolTable>> (symbol table)
//! // - root: NodeId (root node of the AST)
//!
//! // Format a single AST node
//! let lisp_output = format_ast(nodes.clone(), symbols.clone(), root);
//! println!("{}", lisp_output);
//!
//! // Format multiple root nodes (e.g., a program)
//! let roots = vec![node1, node2, node3];
//! let program_output = format_ast_nodes(nodes, symbols, &roots);
//! println!("{}", program_output);
//! ```
//!
//! # Output Format Examples
//!
//! Binary expression: `(+ 42 (Var "x"))`
//!
//! Function definition:
//! ```lisp
//! (Function "factorial"
//!   ((Param "n" I32 []))
//!   I32
//!   [pub]
//!   (Block
//!     (If (== (Var "n") 0)
//!       (Return 1)
//!       (Return (* (Var "n") (Call (Var "factorial") (- (Var "n") 1)))))))
//! ```

use std::{cell::RefCell, rc::Rc};

use ignis_type::{Store, symbol::SymbolTable};

use crate::{
  ASTNode, NodeId,
  expressions::{
    ASTExpression,
    assignment::{ASTAssignment, ASTAssignmentOperator},
    binary::{ASTBinary, ASTBinaryOperator},
    call::ASTCallExpression,
    cast::ASTCast,
    grouped::ASTGrouped,
    literal::ASTLiteral,
    path::ASTPath,
    unary::{ASTUnary, UnaryOperator},
    variable::ASTVariableExpression,
    vector::ASTVector,
    vector_access::ASTVectorAccess,
  },
  statements::{
    ASTStatement,
    block::ASTBlock,
    break_statement::ASTBreak,
    comment_statement::ASTComment,
    continue_statement::ASTContinue,
    extern_statement::{ASTExternItem, ASTExternModule},
    for_statement::ASTFor,
    function::{ASTFunction, ASTFunctionSignature, ASTParameter},
    if_statement::ASTIf,
    import_statement::ASTImport,
    return_statement::ASTReturn,
    variable::ASTVariable,
    while_statement::ASTWhile,
  },
  type_::IgnisTypeSyntax,
  metadata::ASTMetadata,
};

/// Trait for converting AST nodes to Lisp-style S-expression format
pub trait DisplayLisp {
  fn to_lisp(
    &self,
    formatter: &ASTFormatter,
  ) -> String;
}

/// Formatter that holds the arena allocator and symbol table
/// for resolving NodeId and SymbolId references
pub struct ASTFormatter {
  nodes: Store<ASTNode>,
  symbols: Rc<RefCell<SymbolTable>>,
  indent_level: usize,
}

impl ASTFormatter {
  pub fn new(
    nodes: Store<ASTNode>,
    symbols: Rc<RefCell<SymbolTable>>,
  ) -> Self {
    Self {
      nodes,
      symbols,
      indent_level: 0,
    }
  }

  /// Resolve a NodeId to its corresponding ASTNode
  pub fn resolve_node(
    &self,
    node_id: &NodeId,
  ) -> &ASTNode {
    self.nodes.get(node_id)
  }

  /// Resolve a SymbolId to its string name
  pub fn resolve_symbol(
    &self,
    symbol_id: &ignis_type::symbol::SymbolId,
  ) -> String {
    self.symbols.borrow().get(symbol_id).to_string()
  }

  /// Get current indentation string
  pub fn indent(&self) -> String {
    "  ".repeat(self.indent_level)
  }

  /// Increase indentation level
  pub fn increase_indent(&mut self) {
    self.indent_level += 1;
  }

  /// Decrease indentation level
  pub fn decrease_indent(&mut self) {
    if self.indent_level > 0 {
      self.indent_level -= 1;
    }
  }

  /// Format a node by its NodeId
  pub fn format_node(
    &self,
    node_id: &NodeId,
  ) -> String {
    let node = self.resolve_node(node_id);
    node.to_lisp(self)
  }
}

/// Public API to format an entire AST starting from a root node
pub fn format_ast(
  nodes: Store<ASTNode>,
  symbols: Rc<RefCell<SymbolTable>>,
  root: NodeId,
) -> String {
  let formatter = ASTFormatter::new(nodes, symbols);
  formatter.format_node(&root)
}

/// Public API to format multiple root nodes (e.g., a program with multiple statements)
pub fn format_ast_nodes(
  nodes: Store<ASTNode>,
  symbols: Rc<RefCell<SymbolTable>>,
  roots: &[NodeId],
) -> String {
  let formatter = ASTFormatter::new(nodes, symbols);
  let formatted: Vec<String> = roots
    .iter()
    .map(|node_id| {
      let node_str = formatter.format_node(node_id);
      format!("  {}", node_str)
    })
    .collect();

  format!("(Program\n{})", formatted.join("\n"))
}

/// Public API to format all nodes in a Store (useful when you don't have specific root NodeIds)
pub fn format_all_nodes(
  nodes: Store<ASTNode>,
  symbols: Rc<RefCell<SymbolTable>>,
) -> String {
  let formatter = ASTFormatter::new(nodes.clone(), symbols);
  let all_nodes = nodes.get_all();

  if all_nodes.is_empty() {
    return "(Program)".to_string();
  }

  // Format each node directly without using NodeId
  let formatted: Vec<String> = all_nodes.iter().map(|node| node.to_lisp(&formatter)).collect();

  format!("(Program\n  {})", formatted.join("\n  "))
}

// Implementation for ASTNode
impl DisplayLisp for ASTNode {
  fn to_lisp(
    &self,
    formatter: &ASTFormatter,
  ) -> String {
    match self {
      ASTNode::Expression(expr) => expr.to_lisp(formatter),
      ASTNode::Statement(stmt) => stmt.to_lisp(formatter),
    }
  }
}

// Implementation for ASTExpression
impl DisplayLisp for ASTExpression {
  fn to_lisp(
    &self,
    formatter: &ASTFormatter,
  ) -> String {
    match self {
      ASTExpression::Assignment(expr) => expr.to_lisp(formatter),
      ASTExpression::Binary(expr) => expr.to_lisp(formatter),
      ASTExpression::Cast(expr) => expr.to_lisp(formatter),
      ASTExpression::Call(expr) => expr.to_lisp(formatter),
      ASTExpression::Grouped(expr) => expr.to_lisp(formatter),
      ASTExpression::Unary(expr) => expr.to_lisp(formatter),
      ASTExpression::Literal(expr) => expr.to_lisp(formatter),
      ASTExpression::Variable(expr) => expr.to_lisp(formatter),
      ASTExpression::Vector(expr) => expr.to_lisp(formatter),
      ASTExpression::VectorAccess(expr) => expr.to_lisp(formatter),
      ASTExpression::Path(expr) => expr.to_lisp(formatter),
      ASTExpression::PostfixInc { expr, .. } => {
        let operand = formatter.format_node(expr);
        format!("(Postfix++ {})", operand)
      },
      ASTExpression::PostfixDec { expr, .. } => {
        let operand = formatter.format_node(expr);
        format!("(Postfix-- {})", operand)
      },
    }
  }
}

// Implementation for ASTStatement
impl DisplayLisp for ASTStatement {
  fn to_lisp(
    &self,
    formatter: &ASTFormatter,
  ) -> String {
    match self {
      ASTStatement::Expression(expr) => expr.to_lisp(formatter),
      ASTStatement::Variable(stmt) => stmt.to_lisp(formatter),
      ASTStatement::Function(stmt) => stmt.to_lisp(formatter),
      ASTStatement::Block(stmt) => stmt.to_lisp(formatter),
      ASTStatement::If(stmt) => stmt.to_lisp(formatter),
      ASTStatement::While(stmt) => stmt.to_lisp(formatter),
      ASTStatement::For(stmt) => stmt.to_lisp(formatter),
      ASTStatement::Return(stmt) => stmt.to_lisp(formatter),
      ASTStatement::Continue(stmt) => stmt.to_lisp(formatter),
      ASTStatement::Break(stmt) => stmt.to_lisp(formatter),
      ASTStatement::Import(stmt) => stmt.to_lisp(formatter),
      ASTStatement::Extern(stmt) => stmt.to_lisp(formatter),
      ASTStatement::Comment(stmt) => stmt.to_lisp(formatter),
    }
  }
}

// ============================================================================
// EXPRESSION IMPLEMENTATIONS
// ============================================================================

// Binary Expression
impl DisplayLisp for ASTBinary {
  fn to_lisp(
    &self,
    formatter: &ASTFormatter,
  ) -> String {
    let op = match self.operator {
      ASTBinaryOperator::Add => "+",
      ASTBinaryOperator::Subtract => "-",
      ASTBinaryOperator::Multiply => "*",
      ASTBinaryOperator::Divide => "/",
      ASTBinaryOperator::Modulo => "%",
      ASTBinaryOperator::Equal => "==",
      ASTBinaryOperator::NotEqual => "!=",
      ASTBinaryOperator::GreaterThan => ">",
      ASTBinaryOperator::GreaterThanOrEqual => ">=",
      ASTBinaryOperator::LessThan => "<",
      ASTBinaryOperator::LessThanOrEqual => "<=",
      ASTBinaryOperator::And => "and",
      ASTBinaryOperator::Or => "or",
      ASTBinaryOperator::BitAnd => "&",
      ASTBinaryOperator::BitOr => "|",
      ASTBinaryOperator::BitXor => "^",
      ASTBinaryOperator::ShiftLeft => "<<",
      ASTBinaryOperator::ShiftRight => ">>",
    };

    let left = formatter.format_node(&self.left);
    let right = formatter.format_node(&self.right);

    format!("({} {} {})", op, left, right)
  }
}

// Unary Expression
impl DisplayLisp for ASTUnary {
  fn to_lisp(
    &self,
    formatter: &ASTFormatter,
  ) -> String {
    let op = match self.operator {
      UnaryOperator::Not => "!",
      UnaryOperator::Negate => "-",
      UnaryOperator::Increment => "++",
      UnaryOperator::Decrement => "--",
      UnaryOperator::BitNot => "~",
    };

    let operand = formatter.format_node(&self.operand);
    format!("({} {})", op, operand)
  }
}

// Literal Expression
impl DisplayLisp for ASTLiteral {
  fn to_lisp(
    &self,
    _formatter: &ASTFormatter,
  ) -> String {
    format!("{}", self.value)
  }
}

// Variable Expression
impl DisplayLisp for ASTVariableExpression {
  fn to_lisp(
    &self,
    formatter: &ASTFormatter,
  ) -> String {
    let name = formatter.resolve_symbol(&self.name);
    format!("(Variable \"{}\")", name)
  }
}

// Assignment Expression
impl DisplayLisp for ASTAssignment {
  fn to_lisp(
    &self,
    formatter: &ASTFormatter,
  ) -> String {
    let op = match self.operator {
      ASTAssignmentOperator::Assign => "=",
      ASTAssignmentOperator::AddAssign => "+=",
      ASTAssignmentOperator::SubAssign => "-=",
      ASTAssignmentOperator::MulAssign => "*=",
      ASTAssignmentOperator::DivAssign => "/=",
      ASTAssignmentOperator::ModAssign => "%=",
      ASTAssignmentOperator::ShiftLeftAssign => "<<=",
      ASTAssignmentOperator::ShiftRightAssign => ">>=",
      ASTAssignmentOperator::BitAndAssign => "&=",
      ASTAssignmentOperator::BitOrAssign => "|=",
      ASTAssignmentOperator::BitXorAssign => "^=",
      ASTAssignmentOperator::NotAssign => "!=",
      ASTAssignmentOperator::AndAssign => "and=",
      ASTAssignmentOperator::OrAssign => "or=",
    };

    let target = formatter.format_node(&self.target);
    let value = formatter.format_node(&self.value);

    format!("({} {} {})", op, target, value)
  }
}

// Call Expression
impl DisplayLisp for ASTCallExpression {
  fn to_lisp(
    &self,
    formatter: &ASTFormatter,
  ) -> String {
    let callee = formatter.format_node(&self.callee);
    let args: Vec<String> = self.arguments.iter().map(|arg| formatter.format_node(arg)).collect();

    if args.is_empty() {
      format!("(Call {})", callee)
    } else {
      format!("(Call {} {})", callee, args.join(" "))
    }
  }
}

// Cast Expression
impl DisplayLisp for ASTCast {
  fn to_lisp(
    &self,
    formatter: &ASTFormatter,
  ) -> String {
    let expr = formatter.format_node(&self.expression);
    let type_str = self.target_type.to_lisp(formatter);

    format!("(Cast {} {})", expr, type_str)
  }
}

// Grouped Expression
impl DisplayLisp for ASTGrouped {
  fn to_lisp(
    &self,
    formatter: &ASTFormatter,
  ) -> String {
    formatter.format_node(&self.expression)
  }
}

// Vector Expression
impl DisplayLisp for ASTVector {
  fn to_lisp(
    &self,
    formatter: &ASTFormatter,
  ) -> String {
    let items: Vec<String> = self.items.iter().map(|item| formatter.format_node(item)).collect();

    if items.is_empty() {
      "(Vector)".to_string()
    } else {
      format!("(Vector {})", items.join(" "))
    }
  }
}

// Vector Access Expression
impl DisplayLisp for ASTVectorAccess {
  fn to_lisp(
    &self,
    formatter: &ASTFormatter,
  ) -> String {
    let name = formatter.format_node(&self.name);
    let index = formatter.format_node(&self.index);

    format!("(VectorAccess {} {})", name, index)
  }
}

// Path Expression
impl DisplayLisp for ASTPath {
  fn to_lisp(
    &self,
    formatter: &ASTFormatter,
  ) -> String {
    let segments: Vec<String> = self.segments.iter().map(|seg| formatter.resolve_symbol(seg)).collect();

    format!("(Path \"{}\")", segments.join("::"))
  }
}

// ============================================================================
// STATEMENT IMPLEMENTATIONS
// ============================================================================

// Block Statement
impl DisplayLisp for ASTBlock {
  fn to_lisp(
    &self,
    formatter: &ASTFormatter,
  ) -> String {
    let statements: Vec<String> = self
      .statements
      .iter()
      .map(|stmt| {
        let indent = formatter.indent();
        format!("{}{}", indent, formatter.format_node(stmt))
      })
      .collect();

    if statements.is_empty() {
      "(Block)".to_string()
    } else {
      format!("(Block\n{})", statements.join("\n"))
    }
  }
}

// Variable Statement
impl DisplayLisp for ASTVariable {
  fn to_lisp(
    &self,
    formatter: &ASTFormatter,
  ) -> String {
    let name = formatter.resolve_symbol(&self.name);
    let type_str = self.type_.to_lisp(formatter);
    let metadata_str = format_metadata(&self.metadata);

    match &self.value {
      Some(value_id) => {
        let value = formatter.format_node(value_id);
        format!("(Var \"{}\" {} {} {})", name, type_str, metadata_str, value)
      },
      None => {
        format!("(Var \"{}\" {} {})", name, type_str, metadata_str)
      },
    }
  }
}

// Function Statement
impl DisplayLisp for ASTFunction {
  fn to_lisp(
    &self,
    formatter: &ASTFormatter,
  ) -> String {
    let sig = &self.signature;
    let name = formatter.resolve_symbol(&sig.name);
    let params: Vec<String> = sig.parameters.iter().map(|p| p.to_lisp(formatter)).collect();
    let return_type = sig.return_type.to_lisp(formatter);
    let metadata_str = format_metadata(&sig.metadata);

    let params_str = if params.is_empty() {
      "()".to_string()
    } else {
      format!("({})", params.join(" "))
    };

    match &self.body {
      Some(body_id) => {
        let body = formatter.format_node(body_id);
        format!(
          "(Function \"{}\" {} {} {}\n  {})",
          name, params_str, return_type, metadata_str, body
        )
      },
      None => {
        format!("(Function \"{}\" {} {} {})", name, params_str, return_type, metadata_str)
      },
    }
  }
}

// Function Signature (helper)
impl DisplayLisp for ASTFunctionSignature {
  fn to_lisp(
    &self,
    formatter: &ASTFormatter,
  ) -> String {
    let name = formatter.resolve_symbol(&self.name);
    let params: Vec<String> = self.parameters.iter().map(|p| p.to_lisp(formatter)).collect();
    let return_type = self.return_type.to_lisp(formatter);
    let metadata_str = format_metadata(&self.metadata);

    let params_str = if params.is_empty() {
      "()".to_string()
    } else {
      format!("({})", params.join(" "))
    };

    format!("(Signature \"{}\" {} {} {})", name, params_str, return_type, metadata_str)
  }
}

// Parameter (helper)
impl DisplayLisp for ASTParameter {
  fn to_lisp(
    &self,
    formatter: &ASTFormatter,
  ) -> String {
    let name = formatter.resolve_symbol(&self.name);
    let type_str = self.type_.to_lisp(formatter);
    let metadata_str = format_metadata(&self.metadata);

    format!("(Param \"{}\" {} {})", name, type_str, metadata_str)
  }
}

// If Statement
impl DisplayLisp for ASTIf {
  fn to_lisp(
    &self,
    formatter: &ASTFormatter,
  ) -> String {
    let condition = formatter.format_node(&self.condition);
    let then_block = formatter.format_node(&self.then_block);

    match &self.else_block {
      Some(else_id) => {
        let else_block = formatter.format_node(else_id);
        format!("(If {} {} {})", condition, then_block, else_block)
      },
      None => {
        format!("(If {} {})", condition, then_block)
      },
    }
  }
}

// While Statement
impl DisplayLisp for ASTWhile {
  fn to_lisp(
    &self,
    formatter: &ASTFormatter,
  ) -> String {
    let condition = formatter.format_node(&self.condition);
    let body = formatter.format_node(&self.body);

    format!("(While {} {})", condition, body)
  }
}

// For Statement
impl DisplayLisp for ASTFor {
  fn to_lisp(
    &self,
    formatter: &ASTFormatter,
  ) -> String {
    let initializer = formatter.format_node(&self.initializer);
    let condition = formatter.format_node(&self.condition);
    let increment = formatter.format_node(&self.increment);
    let body = formatter.format_node(&self.body);

    format!("(For {} {} {} {})", initializer, condition, increment, body)
  }
}

// Return Statement
impl DisplayLisp for ASTReturn {
  fn to_lisp(
    &self,
    formatter: &ASTFormatter,
  ) -> String {
    let expr = formatter.format_node(&self.expression);
    format!("(Return {})", expr)
  }
}

// Continue Statement
impl DisplayLisp for ASTContinue {
  fn to_lisp(
    &self,
    _formatter: &ASTFormatter,
  ) -> String {
    "(Continue)".to_string()
  }
}

// Break Statement
impl DisplayLisp for ASTBreak {
  fn to_lisp(
    &self,
    _formatter: &ASTFormatter,
  ) -> String {
    "(Break)".to_string()
  }
}

// Import Statement
impl DisplayLisp for ASTImport {
  fn to_lisp(
    &self,
    formatter: &ASTFormatter,
  ) -> String {
    let module = formatter.resolve_symbol(&self.module);

    match &self.alias {
      Some(alias_id) => {
        let alias = formatter.resolve_symbol(alias_id);
        format!("(Import \"{}\" as \"{}\")", module, alias)
      },
      None => {
        format!("(Import \"{}\")", module)
      },
    }
  }
}

// Extern Statement
impl DisplayLisp for ASTExternModule {
  fn to_lisp(
    &self,
    formatter: &ASTFormatter,
  ) -> String {
    let name = formatter.resolve_symbol(&self.name);
    let items: Vec<String> = self.items.iter().map(|item| item.to_lisp(formatter)).collect();

    if items.is_empty() {
      format!("(Extern \"{}\")", name)
    } else {
      format!("(Extern \"{}\" {})", name, items.join(" "))
    }
  }
}

// Extern Item
impl DisplayLisp for ASTExternItem {
  fn to_lisp(
    &self,
    formatter: &ASTFormatter,
  ) -> String {
    match self {
      ASTExternItem::Function(sig) => sig.to_lisp(formatter),
    }
  }
}

// Comment Statement
impl DisplayLisp for ASTComment {
  fn to_lisp(
    &self,
    _formatter: &ASTFormatter,
  ) -> String {
    format!("(Comment \"{}\")", self.content.replace("\"", "\\\""))
  }
}

// ============================================================================
// TYPE IMPLEMENTATIONS
// ============================================================================

impl DisplayLisp for IgnisTypeSyntax {
  fn to_lisp(
    &self,
    formatter: &ASTFormatter,
  ) -> String {
    match self {
      IgnisTypeSyntax::I8 => "I8".to_string(),
      IgnisTypeSyntax::I16 => "I16".to_string(),
      IgnisTypeSyntax::I32 => "I32".to_string(),
      IgnisTypeSyntax::I64 => "I64".to_string(),
      IgnisTypeSyntax::U8 => "U8".to_string(),
      IgnisTypeSyntax::U16 => "U16".to_string(),
      IgnisTypeSyntax::U32 => "U32".to_string(),
      IgnisTypeSyntax::U64 => "U64".to_string(),
      IgnisTypeSyntax::F32 => "F32".to_string(),
      IgnisTypeSyntax::F64 => "F64".to_string(),
      IgnisTypeSyntax::String => "String".to_string(),
      IgnisTypeSyntax::Boolean => "Bool".to_string(),
      IgnisTypeSyntax::Void => "Void".to_string(),
      IgnisTypeSyntax::Null => "Null".to_string(),
      IgnisTypeSyntax::Char => "Char".to_string(),

      IgnisTypeSyntax::Vector(inner, size) => match size {
        Some(s) => format!("(Vector {} {})", inner.to_lisp(formatter), s),
        None => format!("(Vector {})", inner.to_lisp(formatter)),
      },

      IgnisTypeSyntax::Tuple(types) => {
        let types_str: Vec<String> = types.iter().map(|t| t.to_lisp(formatter)).collect();
        format!("(Tuple {})", types_str.join(" "))
      },

      IgnisTypeSyntax::Callable(params, return_type) => {
        let params_str: Vec<String> = params.iter().map(|p| p.to_lisp(formatter)).collect();
        let return_str = return_type.to_lisp(formatter);

        if params_str.is_empty() {
          format!("(Fn () -> {})", return_str)
        } else {
          format!("(Fn ({}) -> {})", params_str.join(" "), return_str)
        }
      },

      IgnisTypeSyntax::Pointer(inner) => {
        format!("(Ptr {})", inner.to_lisp(formatter))
      },

      IgnisTypeSyntax::Reference(inner) => {
        format!("(Ref {})", inner.to_lisp(formatter))
      },

      IgnisTypeSyntax::Named(symbol_id) => {
        let name = formatter.resolve_symbol(symbol_id);
        format!("(Named \"{}\")", name)
      },

      IgnisTypeSyntax::Applied { base, args } => {
        let base_str = base.to_lisp(formatter);
        let args_str: Vec<String> = args.iter().map(|a| a.to_lisp(formatter)).collect();

        if args_str.is_empty() {
          base_str
        } else {
          format!("(Applied {} {})", base_str, args_str.join(" "))
        }
      },

      IgnisTypeSyntax::Path { segments, args, .. } => {
        let path_parts: Vec<String> = segments.iter().map(|(sym, _)| formatter.resolve_symbol(sym)).collect();

        let path_str = path_parts.join("::");

        if args.is_empty() {
          format!("(Path \"{}\")", path_str)
        } else {
          let args_str: Vec<String> = args.iter().map(|a| a.to_lisp(formatter)).collect();
          format!("(Path \"{}\" {})", path_str, args_str.join(" "))
        }
      },

      IgnisTypeSyntax::Union(types) => {
        let types_str: Vec<String> = types.iter().map(|t| t.to_lisp(formatter)).collect();
        format!("(Union {})", types_str.join(" "))
      },

      IgnisTypeSyntax::Intersection(types) => {
        let types_str: Vec<String> = types.iter().map(|t| t.to_lisp(formatter)).collect();
        format!("(Intersection {})", types_str.join(" "))
      },
    }
  }
}

// ============================================================================
// HELPER FUNCTIONS
// ============================================================================

/// Helper function to format metadata flags
fn format_metadata(metadata: &ASTMetadata) -> String {
  let mut flags = Vec::new();

  if metadata.contains(ASTMetadata::CONSTANT) {
    flags.push("const");
  }
  if metadata.contains(ASTMetadata::MUTABLE) {
    flags.push("mut");
  }
  if metadata.contains(ASTMetadata::PUBLIC) {
    flags.push("pub");
  }
  if metadata.contains(ASTMetadata::PRIVATE) {
    flags.push("priv");
  }
  if metadata.contains(ASTMetadata::STATIC) {
    flags.push("static");
  }
  if metadata.contains(ASTMetadata::INLINE) {
    flags.push("inline");
  }
  if metadata.contains(ASTMetadata::EXPORT) {
    flags.push("export");
  }
  if metadata.contains(ASTMetadata::EXTERN_MEMBER) {
    flags.push("extern");
  }
  if metadata.contains(ASTMetadata::REFERENCE) {
    flags.push("ref");
  }
  if metadata.contains(ASTMetadata::POINTER) {
    flags.push("ptr");
  }
  if metadata.contains(ASTMetadata::OPTIONAL) {
    flags.push("opt");
  }
  if metadata.contains(ASTMetadata::VARIADIC) {
    flags.push("variadic");
  }

  if flags.is_empty() {
    "[]".to_string()
  } else {
    format!("[{}]", flags.join(" "))
  }
}
