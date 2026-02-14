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

use std::{
  cell::{Cell, RefCell},
  rc::Rc,
};

use ignis_type::{Store, symbol::SymbolTable};

use crate::{
  ASTNode, NodeId,
  expressions::{
    ASTExpression, ASTAccessOp, ASTMemberAccess, ASTRecordInit,
    assignment::{ASTAssignment, ASTAssignmentOperator},
    binary::{ASTBinary, ASTBinaryOperator},
    builtin_call::ASTBuiltinCall,
    call::ASTCallExpression,
    cast::ASTCast,
    grouped::ASTGrouped,
    literal::ASTLiteral,
    match_expression::ASTMatch,
    path::ASTPath,
    ternary::ASTTernary,
    unary::{ASTUnary, UnaryOperator},
    variable::ASTVariableExpression,
    vector::ASTVector,
    vector_access::ASTVectorAccess,
  },
  statements::{
    ASTStatement, ASTEnum, ASTEnumItem, ASTForOf, ASTMethod, ASTRecord, ASTRecordItem, ASTTypeAlias,
    block::ASTBlock,
    break_statement::ASTBreak,
    comment_statement::ASTComment,
    const_statement::ASTConstant,
    continue_statement::ASTContinue,
    export_statement::ASTExport,
    extern_statement::ASTExtern,
    for_statement::ASTFor,
    function::{ASTFunction, ASTFunctionSignature, ASTParameter},
    if_statement::ASTIf,
    import_statement::ASTImport,
    namespace_statement::ASTNamespace,
    return_statement::ASTReturn,
    variable::ASTVariable,
    while_statement::ASTWhile,
  },
  type_::IgnisTypeSyntax,
  metadata::ASTMetadata,
  pattern::ASTPattern,
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
  indent_level: Cell<usize>,
}

impl ASTFormatter {
  pub fn new(
    nodes: Store<ASTNode>,
    symbols: Rc<RefCell<SymbolTable>>,
  ) -> Self {
    Self {
      nodes,
      symbols,
      indent_level: Cell::new(0),
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
    "  ".repeat(self.indent_level.get())
  }

  /// Increase indentation level
  pub fn increase_indent(&self) {
    self.indent_level.set(self.indent_level.get() + 1);
  }

  /// Decrease indentation level
  pub fn decrease_indent(&self) {
    let current = self.indent_level.get();
    if current > 0 {
      self.indent_level.set(current - 1);
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

  if roots.is_empty() {
    return "(Program)".to_string();
  }

  formatter.increase_indent();
  let formatted: Vec<String> = roots
    .iter()
    .map(|node_id| format!("{}{}", formatter.indent(), formatter.format_node(node_id)))
    .collect();
  formatter.decrease_indent();

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

  formatter.increase_indent();
  let formatted: Vec<String> = all_nodes
    .iter()
    .map(|node| format!("{}{}", formatter.indent(), node.to_lisp(&formatter)))
    .collect();
  formatter.decrease_indent();

  format!("(Program\n{})", formatted.join("\n"))
}

// Implementation for ASTNode
impl DisplayLisp for ASTNode {
  fn to_lisp(
    &self,
    formatter: &ASTFormatter,
  ) -> String {
    match self {
      ASTNode::Expression(expression) => expression.to_lisp(formatter),
      ASTNode::Statement(statement) => statement.to_lisp(formatter),
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
      ASTExpression::Ternary(expr) => expr.to_lisp(formatter),
      ASTExpression::Cast(expr) => expr.to_lisp(formatter),
      ASTExpression::Call(expr) => expr.to_lisp(formatter),
      ASTExpression::Dereference(expr) => {
        let inner = formatter.format_node(&expr.inner);
        format!("(*{})", inner)
      },
      ASTExpression::Grouped(expr) => expr.to_lisp(formatter),
      ASTExpression::Reference(expr) => {
        let inner = formatter.format_node(&expr.inner);
        if expr.mutable {
          format!("(&mut {})", inner)
        } else {
          format!("(&{})", inner)
        }
      },
      ASTExpression::Unary(expr) => expr.to_lisp(formatter),
      ASTExpression::Literal(expr) => expr.to_lisp(formatter),
      ASTExpression::Match(expr) => expr.to_lisp(formatter),
      ASTExpression::Variable(expr) => expr.to_lisp(formatter),
      ASTExpression::Vector(expr) => expr.to_lisp(formatter),
      ASTExpression::VectorAccess(expr) => expr.to_lisp(formatter),
      ASTExpression::Path(expr) => expr.to_lisp(formatter),
      ASTExpression::PostfixIncrement { expr, .. } => {
        let operand = formatter.format_node(expr);
        format!("(PostfixIncrement {})", operand)
      },
      ASTExpression::PostfixDecrement { expr, .. } => {
        let operand = formatter.format_node(expr);
        format!("(PostfixDecrement {})", operand)
      },
      ASTExpression::MemberAccess(expr) => expr.to_lisp(formatter),
      ASTExpression::RecordInit(expr) => expr.to_lisp(formatter),
      ASTExpression::BuiltinCall(expr) => expr.to_lisp(formatter),
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
      ASTStatement::Expression(expression) => expression.to_lisp(formatter),
      ASTStatement::Variable(statement) => statement.to_lisp(formatter),
      ASTStatement::Function(statement) => statement.to_lisp(formatter),
      ASTStatement::Block(statement) => statement.to_lisp(formatter),
      ASTStatement::If(statement) => statement.to_lisp(formatter),
      ASTStatement::While(statement) => statement.to_lisp(formatter),
      ASTStatement::For(statement) => statement.to_lisp(formatter),
      ASTStatement::ForOf(statement) => statement.to_lisp(formatter),
      ASTStatement::Return(statement) => statement.to_lisp(formatter),
      ASTStatement::Continue(statement) => statement.to_lisp(formatter),
      ASTStatement::Break(statement) => statement.to_lisp(formatter),
      ASTStatement::Import(statement) => statement.to_lisp(formatter),
      ASTStatement::Extern(statement) => statement.to_lisp(formatter),
      ASTStatement::Constant(statement) => statement.to_lisp(formatter),
      ASTStatement::Export(statement) => statement.to_lisp(formatter),
      ASTStatement::Comment(statement) => statement.to_lisp(formatter),
      ASTStatement::Namespace(statement) => statement.to_lisp(formatter),
      ASTStatement::TypeAlias(statement) => statement.to_lisp(formatter),
      ASTStatement::Record(statement) => statement.to_lisp(formatter),
      ASTStatement::Enum(statement) => statement.to_lisp(formatter),
      ASTStatement::Trait(_) => "(trait)".to_string(),
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

// Ternary Expression
impl DisplayLisp for ASTTernary {
  fn to_lisp(
    &self,
    formatter: &ASTFormatter,
  ) -> String {
    let condition = formatter.format_node(&self.condition);
    let then_expr = formatter.format_node(&self.then_expr);
    let else_expr = formatter.format_node(&self.else_expr);

    format!("(?: {} {} {})", condition, then_expr, else_expr)
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
    format!("(VariableExpression \"{}\")", name)
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
    let segments: Vec<String> = self
      .segments
      .iter()
      .map(|seg| formatter.resolve_symbol(&seg.name))
      .collect();

    format!("(Path \"{}\")", segments.join("::"))
  }
}

impl DisplayLisp for ASTMatch {
  fn to_lisp(
    &self,
    formatter: &ASTFormatter,
  ) -> String {
    let scrutinee = formatter.format_node(&self.scrutinee);

    if self.arms.is_empty() {
      return format!("(Match {})", scrutinee);
    }

    let arms: Vec<String> = self
      .arms
      .iter()
      .map(|arm| {
        let pattern = arm.pattern.to_lisp(formatter);

        let guard = match arm.guard {
          Some(ref guard) => format!(" if {}", formatter.format_node(guard)),
          None => String::new(),
        };

        let body = formatter.format_node(&arm.body);
        format!("(Arm {}{} -> {})", pattern, guard, body)
      })
      .collect();

    format!("(Match {} {})", scrutinee, arms.join(" "))
  }
}

impl DisplayLisp for ASTPattern {
  fn to_lisp(
    &self,
    formatter: &ASTFormatter,
  ) -> String {
    match self {
      ASTPattern::Wildcard { .. } => "_".to_string(),
      ASTPattern::Literal { value, .. } => value.to_string(),
      ASTPattern::Path { segments, args, .. } => {
        let path: Vec<String> = segments.iter().map(|(sym, _)| formatter.resolve_symbol(sym)).collect();
        let path = path.join("::");

        match args {
          Some(args) => {
            let args: Vec<String> = args.iter().map(|arg| arg.to_lisp(formatter)).collect();
            format!("(PathPattern \"{}\" ({}))", path, args.join(" "))
          },
          None => format!("(PathPattern \"{}\")", path),
        }
      },
      ASTPattern::Tuple { elements, .. } => {
        let elements: Vec<String> = elements.iter().map(|element| element.to_lisp(formatter)).collect();
        format!("(TuplePattern {})", elements.join(" "))
      },
      ASTPattern::Or { patterns, .. } => {
        let patterns: Vec<String> = patterns.iter().map(|pattern| pattern.to_lisp(formatter)).collect();
        format!("(OrPattern {})", patterns.join(" "))
      },
    }
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
    if self.statements.is_empty() {
      return "(Block)".to_string();
    }

    formatter.increase_indent();
    let statements: Vec<String> = self
      .statements
      .iter()
      .map(|statement_id| format!("{}{}", formatter.indent(), formatter.format_node(statement_id)))
      .collect();
    formatter.decrease_indent();

    format!("(Block\n{})", statements.join("\n"))
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
        format!("(Variable \"{}\" {} {} {})", name, type_str, metadata_str, value)
      },
      None => {
        format!("(Variable \"{}\" {} {})", name, type_str, metadata_str)
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
          "(Function \"{}\" {} {} {}\n{}{})",
          name,
          params_str,
          return_type,
          metadata_str,
          formatter.indent(),
          body
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

// For-Of Statement
impl DisplayLisp for ASTForOf {
  fn to_lisp(
    &self,
    formatter: &ASTFormatter,
  ) -> String {
    let name = formatter.resolve_symbol(&self.binding.name);
    let type_str = match &self.binding.type_annotation {
      Some(ty) => ty.to_lisp(formatter),
      None => "inferred".to_string(),
    };
    let iter = formatter.format_node(&self.iter);
    let body = formatter.format_node(&self.body);

    format!("(ForOf \"{}\" {} {} {})", name, type_str, iter, body)
  }
}

// Return Statement
impl DisplayLisp for ASTReturn {
  fn to_lisp(
    &self,
    formatter: &ASTFormatter,
  ) -> String {
    match &self.expression {
      Some(expr) => {
        let expr_str = formatter.format_node(expr);
        format!("(Return {})", expr_str)
      },
      None => "(Return)".to_string(),
    }
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
    let items: Vec<String> = self
      .items
      .iter()
      .map(|item| formatter.resolve_symbol(&item.name))
      .collect();
    // Remove surrounding quotes from the path string if present
    let path = self.from.trim_matches('"');
    format!("(Import [{}] from \"{}\")", items.join(", "), path)
  }
}

// Extern Statement
impl DisplayLisp for ASTExtern {
  fn to_lisp(
    &self,
    formatter: &ASTFormatter,
  ) -> String {
    let path_str: Vec<String> = self.path.iter().map(|s| formatter.resolve_symbol(s)).collect();
    let items_display: Vec<String> = self.items.iter().map(|i| formatter.format_node(i)).collect();
    format!("(Extern {} [{}])", path_str.join("::"), items_display.join(" "))
  }
}

// Constant Statement
impl DisplayLisp for ASTConstant {
  fn to_lisp(
    &self,
    formatter: &ASTFormatter,
  ) -> String {
    let name = formatter.resolve_symbol(&self.name);
    let type_display = self.ty.to_lisp(formatter);

    if let Some(value_id) = &self.value {
      let value_display = formatter.format_node(value_id);
      format!("(Constant {} : {} = {})", name, type_display, value_display)
    } else {
      format!("(Constant {} : {})", name, type_display)
    }
  }
}

// Export Statement
impl DisplayLisp for ASTExport {
  fn to_lisp(
    &self,
    formatter: &ASTFormatter,
  ) -> String {
    match self {
      ASTExport::Declaration { decl, .. } => {
        let declaration_display = formatter.format_node(decl);
        format!("(Export {})", declaration_display)
      },
      ASTExport::Name { name, .. } => {
        let name_string = formatter.resolve_symbol(name);
        format!("(Export \"{}\")", name_string)
      },
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

// Namespace Statement
impl DisplayLisp for ASTNamespace {
  fn to_lisp(
    &self,
    formatter: &ASTFormatter,
  ) -> String {
    let path_str: Vec<String> = self.path.iter().map(|s| formatter.resolve_symbol(s)).collect();
    let items_display: Vec<String> = self.items.iter().map(|i| formatter.format_node(i)).collect();
    format!("(Namespace {} [{}])", path_str.join("::"), items_display.join(" "))
  }
}

// Type Alias Statement
impl DisplayLisp for ASTTypeAlias {
  fn to_lisp(
    &self,
    formatter: &ASTFormatter,
  ) -> String {
    let name = formatter.resolve_symbol(&self.name);
    let target = self.target.to_lisp(formatter);
    format!("(TypeAlias \"{}\" {})", name, target)
  }
}

// Record Statement
impl DisplayLisp for ASTRecord {
  fn to_lisp(
    &self,
    formatter: &ASTFormatter,
  ) -> String {
    let name = formatter.resolve_symbol(&self.name);

    if self.items.is_empty() {
      return format!("(Record \"{}\")", name);
    }

    formatter.increase_indent();
    let items: Vec<String> = self
      .items
      .iter()
      .map(|item| {
        let item_str = match item {
          ASTRecordItem::Field(field) => {
            let field_name = formatter.resolve_symbol(&field.name);
            let type_str = field.type_.to_lisp(formatter);
            let metadata_str = format_metadata(&field.metadata);
            match &field.value {
              Some(value_id) => {
                let value = formatter.format_node(value_id);
                format!("(Field \"{}\" {} {} {})", field_name, type_str, metadata_str, value)
              },
              None => {
                format!("(Field \"{}\" {} {})", field_name, type_str, metadata_str)
              },
            }
          },
          ASTRecordItem::Method(method) => method.to_lisp(formatter),
        };
        format!("{}{}", formatter.indent(), item_str)
      })
      .collect();
    formatter.decrease_indent();

    format!("(Record \"{}\"\n{})", name, items.join("\n"))
  }
}

// Method (used by Record and Enum)
impl DisplayLisp for ASTMethod {
  fn to_lisp(
    &self,
    formatter: &ASTFormatter,
  ) -> String {
    let name = formatter.resolve_symbol(&self.name);
    let params: Vec<String> = self.parameters.iter().map(|p| p.to_lisp(formatter)).collect();
    let return_type = self.return_type.to_lisp(formatter);
    let metadata_str = format_metadata(&self.metadata);
    let body = formatter.format_node(&self.body);

    let params_str = if params.is_empty() {
      "()".to_string()
    } else {
      format!("({})", params.join(" "))
    };

    format!(
      "(Method \"{}\" {} {} {}\n{}{})",
      name,
      params_str,
      return_type,
      metadata_str,
      formatter.indent(),
      body
    )
  }
}

// Enum Statement
impl DisplayLisp for ASTEnum {
  fn to_lisp(
    &self,
    formatter: &ASTFormatter,
  ) -> String {
    let name = formatter.resolve_symbol(&self.name);

    if self.items.is_empty() {
      return format!("(Enum \"{}\")", name);
    }

    formatter.increase_indent();
    let items: Vec<String> = self
      .items
      .iter()
      .map(|item| {
        let item_str = match item {
          ASTEnumItem::Variant(variant) => {
            let variant_name = formatter.resolve_symbol(&variant.name);
            if variant.payload.is_empty() {
              format!("(Variant \"{}\")", variant_name)
            } else {
              let payload: Vec<String> = variant.payload.iter().map(|t| t.to_lisp(formatter)).collect();
              format!("(Variant \"{}\" ({}))", variant_name, payload.join(" "))
            }
          },
          ASTEnumItem::Method(method) => method.to_lisp(formatter),
          ASTEnumItem::Field(field) => {
            let field_name = formatter.resolve_symbol(&field.name);
            let type_str = field.type_.to_lisp(formatter);
            let metadata_str = format_metadata(&field.metadata);
            match &field.value {
              Some(value_id) => {
                let value = formatter.format_node(value_id);
                format!("(Field \"{}\" {} {} {})", field_name, type_str, metadata_str, value)
              },
              None => {
                format!("(Field \"{}\" {} {})", field_name, type_str, metadata_str)
              },
            }
          },
        };
        format!("{}{}", formatter.indent(), item_str)
      })
      .collect();
    formatter.decrease_indent();

    format!("(Enum \"{}\"\n{})", name, items.join("\n"))
  }
}

// Member Access Expression
impl DisplayLisp for ASTMemberAccess {
  fn to_lisp(
    &self,
    formatter: &ASTFormatter,
  ) -> String {
    let object = formatter.format_node(&self.object);
    let member = formatter.resolve_symbol(&self.member);
    let op_str = match self.op {
      ASTAccessOp::Dot => ".",
      ASTAccessOp::DoubleColon => "::",
    };
    format!("(MemberAccess {} \"{}\" \"{}\")", object, op_str, member)
  }
}

// Record Init Expression
impl DisplayLisp for ASTRecordInit {
  fn to_lisp(
    &self,
    formatter: &ASTFormatter,
  ) -> String {
    let path: Vec<String> = self.path.iter().map(|(sym, _)| formatter.resolve_symbol(sym)).collect();
    let path_str = path.join("::");

    if self.fields.is_empty() {
      return format!("(RecordInit \"{}\")", path_str);
    }

    let fields: Vec<String> = self
      .fields
      .iter()
      .map(|field| {
        let name = formatter.resolve_symbol(&field.name);
        let value = formatter.format_node(&field.value);
        format!("(\"{}\": {})", name, value)
      })
      .collect();

    format!("(RecordInit \"{}\" {})", path_str, fields.join(" "))
  }
}

// Builtin Call Expression
impl DisplayLisp for ASTBuiltinCall {
  fn to_lisp(
    &self,
    formatter: &ASTFormatter,
  ) -> String {
    let name = formatter.resolve_symbol(&self.name);

    let type_args_str = match &self.type_args {
      Some(tas) => {
        let types: Vec<String> = tas.iter().map(|t| t.to_lisp(formatter)).collect();
        format!("<{}>", types.join(", "))
      },
      None => String::new(),
    };

    let args: Vec<String> = self.args.iter().map(|arg| formatter.format_node(arg)).collect();

    if args.is_empty() {
      format!("(@{}{})", name, type_args_str)
    } else {
      format!("(@{}{} {})", name, type_args_str, args.join(" "))
    }
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
      IgnisTypeSyntax::Str => "Str".to_string(),
      IgnisTypeSyntax::String => "String".to_string(),
      IgnisTypeSyntax::Boolean => "Bool".to_string(),
      IgnisTypeSyntax::Atom => "Atom".to_string(),
      IgnisTypeSyntax::Void => "Void".to_string(),
      IgnisTypeSyntax::Null => "Null".to_string(),
      IgnisTypeSyntax::Char => "Char".to_string(),
      IgnisTypeSyntax::Implicit => "Implicit".to_string(),

      IgnisTypeSyntax::Vector(inner, size) => match size {
        Some(s) => format!("({}[{}])", inner.to_lisp(formatter), s),
        None => format!("({}[])", inner.to_lisp(formatter)),
      },

      IgnisTypeSyntax::Tuple(types) => {
        let types_str: Vec<String> = types.iter().map(|t| t.to_lisp(formatter)).collect();
        format!("({})", types_str.join(" "))
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

      IgnisTypeSyntax::Pointer { inner, mutable } => {
        let inner_str = inner.to_lisp(formatter);
        if *mutable {
          format!("(*mut {})", inner_str)
        } else {
          format!("(*{})", inner_str)
        }
      },

      IgnisTypeSyntax::Reference { inner, mutable } => {
        let inner_str = inner.to_lisp(formatter);
        if *mutable {
          format!("(&mut {})", inner_str)
        } else {
          format!("(&{})", inner_str)
        }
      },

      IgnisTypeSyntax::Named { symbol, .. } => {
        let name = formatter.resolve_symbol(symbol);
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
        format!("({})", types_str.join("&"))
      },

      IgnisTypeSyntax::Intersection(types) => {
        let types_str: Vec<String> = types.iter().map(|t| t.to_lisp(formatter)).collect();
        format!("({})", types_str.join("|"))
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
    flags.push("constant");
  }
  if metadata.contains(ASTMetadata::MUTABLE) {
    flags.push("mutable");
  }
  if metadata.contains(ASTMetadata::PUBLIC) {
    flags.push("public");
  }
  if metadata.contains(ASTMetadata::PRIVATE) {
    flags.push("private");
  }
  if metadata.contains(ASTMetadata::STATIC) {
    flags.push("static");
  }
  if metadata.contains(ASTMetadata::EXPORT) {
    flags.push("export");
  }
  if metadata.contains(ASTMetadata::EXTERN_MEMBER) {
    flags.push("extern");
  }
  if metadata.contains(ASTMetadata::REFERENCE) {
    flags.push("reference");
  }
  if metadata.contains(ASTMetadata::POINTER) {
    flags.push("pointer");
  }
  if metadata.contains(ASTMetadata::OPTIONAL) {
    flags.push("optional");
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
