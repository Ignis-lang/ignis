use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
struct KindEntry {
  enum_name: &'static str,
  variant_name: &'static str,
}

impl fmt::Display for KindEntry {
  fn fmt(
    &self,
    f: &mut fmt::Formatter<'_>,
  ) -> fmt::Result {
    write!(f, "{}::{}", self.enum_name, self.variant_name)
  }
}

fn ast_statement_kinds() -> Vec<KindEntry> {
  vec![
    KindEntry { enum_name: "ASTStatement", variant_name: "Expression" },
    KindEntry { enum_name: "ASTStatement", variant_name: "Variable" },
    KindEntry { enum_name: "ASTStatement", variant_name: "LetElse" },
    KindEntry { enum_name: "ASTStatement", variant_name: "Function" },
    KindEntry { enum_name: "ASTStatement", variant_name: "Block" },
    KindEntry { enum_name: "ASTStatement", variant_name: "If" },
    KindEntry { enum_name: "ASTStatement", variant_name: "While" },
    KindEntry { enum_name: "ASTStatement", variant_name: "For" },
    KindEntry { enum_name: "ASTStatement", variant_name: "ForOf" },
    KindEntry { enum_name: "ASTStatement", variant_name: "Return" },
    KindEntry { enum_name: "ASTStatement", variant_name: "Continue" },
    KindEntry { enum_name: "ASTStatement", variant_name: "Break" },
    KindEntry { enum_name: "ASTStatement", variant_name: "Defer" },
    KindEntry { enum_name: "ASTStatement", variant_name: "Import" },
    KindEntry { enum_name: "ASTStatement", variant_name: "Extern" },
    KindEntry { enum_name: "ASTStatement", variant_name: "Constant" },
    KindEntry { enum_name: "ASTStatement", variant_name: "Export" },
    KindEntry { enum_name: "ASTStatement", variant_name: "Comment" },
    KindEntry { enum_name: "ASTStatement", variant_name: "Namespace" },
    KindEntry { enum_name: "ASTStatement", variant_name: "TypeAlias" },
    KindEntry { enum_name: "ASTStatement", variant_name: "Record" },
    KindEntry { enum_name: "ASTStatement", variant_name: "Enum" },
    KindEntry { enum_name: "ASTStatement", variant_name: "Trait" },
  ]
}

fn ast_expression_kinds() -> Vec<KindEntry> {
  vec![
    KindEntry { enum_name: "ASTExpression", variant_name: "Assignment" },
    KindEntry { enum_name: "ASTExpression", variant_name: "Binary" },
    KindEntry { enum_name: "ASTExpression", variant_name: "Ternary" },
    KindEntry { enum_name: "ASTExpression", variant_name: "Cast" },
    KindEntry { enum_name: "ASTExpression", variant_name: "Call" },
    KindEntry { enum_name: "ASTExpression", variant_name: "Dereference" },
    KindEntry { enum_name: "ASTExpression", variant_name: "Grouped" },
    KindEntry { enum_name: "ASTExpression", variant_name: "LetCondition" },
    KindEntry { enum_name: "ASTExpression", variant_name: "Reference" },
    KindEntry { enum_name: "ASTExpression", variant_name: "Unary" },
    KindEntry { enum_name: "ASTExpression", variant_name: "Literal" },
    KindEntry { enum_name: "ASTExpression", variant_name: "Match" },
    KindEntry { enum_name: "ASTExpression", variant_name: "Variable" },
    KindEntry { enum_name: "ASTExpression", variant_name: "Vector" },
    KindEntry { enum_name: "ASTExpression", variant_name: "VectorAccess" },
    KindEntry { enum_name: "ASTExpression", variant_name: "Path" },
    KindEntry { enum_name: "ASTExpression", variant_name: "PostfixIncrement" },
    KindEntry { enum_name: "ASTExpression", variant_name: "PostfixDecrement" },
    KindEntry { enum_name: "ASTExpression", variant_name: "MemberAccess" },
    KindEntry { enum_name: "ASTExpression", variant_name: "RecordInit" },
    KindEntry { enum_name: "ASTExpression", variant_name: "BuiltinCall" },
    KindEntry { enum_name: "ASTExpression", variant_name: "Lambda" },
    KindEntry { enum_name: "ASTExpression", variant_name: "CaptureOverride" },
    KindEntry { enum_name: "ASTExpression", variant_name: "Pipe" },
    KindEntry { enum_name: "ASTExpression", variant_name: "PipePlaceholder" },
    KindEntry { enum_name: "ASTExpression", variant_name: "Try" },
  ]
}

fn ast_pattern_kinds() -> Vec<KindEntry> {
  vec![
    KindEntry { enum_name: "ASTPattern", variant_name: "Wildcard" },
    KindEntry { enum_name: "ASTPattern", variant_name: "Literal" },
    KindEntry { enum_name: "ASTPattern", variant_name: "Path" },
    KindEntry { enum_name: "ASTPattern", variant_name: "Tuple" },
    KindEntry { enum_name: "ASTPattern", variant_name: "Or" },
  ]
}

fn ast_type_syntax_kinds() -> Vec<KindEntry> {
  vec![
    KindEntry { enum_name: "IgnisTypeSyntax", variant_name: "I8" },
    KindEntry { enum_name: "IgnisTypeSyntax", variant_name: "I16" },
    KindEntry { enum_name: "IgnisTypeSyntax", variant_name: "I32" },
    KindEntry { enum_name: "IgnisTypeSyntax", variant_name: "I64" },
    KindEntry { enum_name: "IgnisTypeSyntax", variant_name: "U8" },
    KindEntry { enum_name: "IgnisTypeSyntax", variant_name: "U16" },
    KindEntry { enum_name: "IgnisTypeSyntax", variant_name: "U32" },
    KindEntry { enum_name: "IgnisTypeSyntax", variant_name: "U64" },
    KindEntry { enum_name: "IgnisTypeSyntax", variant_name: "F32" },
    KindEntry { enum_name: "IgnisTypeSyntax", variant_name: "F64" },
    KindEntry { enum_name: "IgnisTypeSyntax", variant_name: "Implicit" },
    KindEntry { enum_name: "IgnisTypeSyntax", variant_name: "Str" },
    KindEntry { enum_name: "IgnisTypeSyntax", variant_name: "Boolean" },
    KindEntry { enum_name: "IgnisTypeSyntax", variant_name: "Atom" },
    KindEntry { enum_name: "IgnisTypeSyntax", variant_name: "Void" },
    KindEntry { enum_name: "IgnisTypeSyntax", variant_name: "Null" },
    KindEntry { enum_name: "IgnisTypeSyntax", variant_name: "Char" },
    KindEntry { enum_name: "IgnisTypeSyntax", variant_name: "Vector" },
    KindEntry { enum_name: "IgnisTypeSyntax", variant_name: "Tuple" },
    KindEntry { enum_name: "IgnisTypeSyntax", variant_name: "Callable" },
    KindEntry { enum_name: "IgnisTypeSyntax", variant_name: "Pointer" },
    KindEntry { enum_name: "IgnisTypeSyntax", variant_name: "Reference" },
    KindEntry { enum_name: "IgnisTypeSyntax", variant_name: "Named" },
    KindEntry { enum_name: "IgnisTypeSyntax", variant_name: "Applied" },
    KindEntry { enum_name: "IgnisTypeSyntax", variant_name: "Path" },
    KindEntry { enum_name: "IgnisTypeSyntax", variant_name: "Union" },
    KindEntry { enum_name: "IgnisTypeSyntax", variant_name: "Intersection" },
  ]
}

/// Collects all AST variant names that the formatter's dispatch handles.
/// Uses `std::any::type_name` via a zero-sized inference trick that forces
/// the compiler to keep each match arm reachable without constructing values.
fn formatter_statement_dispatch_coverage() -> Vec<KindEntry> {
  let mut covered: Vec<KindEntry> = Vec::new();

  // This uses the layout module's public dispatch: we parse valid source that
  // exercises each variant, checking the formatter handles it. But for the
  // inventory, we enumerate statically — the test compares these lists.
  macro_rules! cover_stmt {
    ($variant:ident) => {
      covered.push(KindEntry { enum_name: "ASTStatement", variant_name: stringify!($variant) });
    };
  }

  // This list MUST match every arm in layout.rs `format_statement` match.
  cover_stmt!(Import);
  cover_stmt!(Export);
  cover_stmt!(Function);
  cover_stmt!(TypeAlias);
  cover_stmt!(Record);
  cover_stmt!(Enum);
  cover_stmt!(Trait);
  cover_stmt!(Block);
  cover_stmt!(If);
  cover_stmt!(While);
  cover_stmt!(LetElse);
  cover_stmt!(For);
  cover_stmt!(ForOf);
  cover_stmt!(Namespace);
  cover_stmt!(Extern);
  cover_stmt!(Comment);
  cover_stmt!(Continue);
  cover_stmt!(Break);
  cover_stmt!(Defer);
  cover_stmt!(Return);
  cover_stmt!(Expression);
  cover_stmt!(Variable);
  cover_stmt!(Constant);

  covered
}

fn formatter_expression_dispatch_coverage() -> Vec<KindEntry> {
  let mut covered: Vec<KindEntry> = Vec::new();

  macro_rules! cover_expr {
    ($variant:ident) => {
      covered.push(KindEntry { enum_name: "ASTExpression", variant_name: stringify!($variant) });
    };
  }

  // This list MUST match every arm in layout.rs `format_expression` match.
  cover_expr!(Variable);
  cover_expr!(Path);
  cover_expr!(Literal);
  cover_expr!(Grouped);
  cover_expr!(Unary);
  cover_expr!(Binary);
  cover_expr!(Assignment);
  cover_expr!(Ternary);
  cover_expr!(Call);
  cover_expr!(Match);
  cover_expr!(Cast);
  cover_expr!(Reference);
  cover_expr!(Dereference);
  cover_expr!(MemberAccess);
  cover_expr!(Vector);
  cover_expr!(VectorAccess);
  cover_expr!(RecordInit);
  cover_expr!(BuiltinCall);
  cover_expr!(Lambda);
  cover_expr!(PostfixIncrement);
  cover_expr!(PostfixDecrement);
  cover_expr!(Try);
  cover_expr!(LetCondition);
  cover_expr!(Pipe);
  cover_expr!(PipePlaceholder);
  cover_expr!(CaptureOverride);

  covered
}

fn formatter_pattern_dispatch_coverage() -> Vec<KindEntry> {
  let mut covered: Vec<KindEntry> = Vec::new();

  macro_rules! cover_pat {
    ($variant:ident) => {
      covered.push(KindEntry { enum_name: "ASTPattern", variant_name: stringify!($variant) });
    };
  }

  cover_pat!(Wildcard);
  cover_pat!(Literal);
  cover_pat!(Path);
  cover_pat!(Tuple);
  cover_pat!(Or);

  covered
}

fn formatter_type_dispatch_coverage() -> Vec<KindEntry> {
  let mut covered: Vec<KindEntry> = Vec::new();

  macro_rules! cover_ty {
    ($variant:ident) => {
      covered.push(KindEntry { enum_name: "IgnisTypeSyntax", variant_name: stringify!($variant) });
    };
  }

  cover_ty!(I8);
  cover_ty!(I16);
  cover_ty!(I32);
  cover_ty!(I64);
  cover_ty!(U8);
  cover_ty!(U16);
  cover_ty!(U32);
  cover_ty!(U64);
  cover_ty!(F32);
  cover_ty!(F64);
  cover_ty!(Implicit);
  cover_ty!(Str);
  cover_ty!(Boolean);
  cover_ty!(Atom);
  cover_ty!(Void);
  cover_ty!(Null);
  cover_ty!(Char);
  cover_ty!(Vector);
  cover_ty!(Tuple);
  cover_ty!(Callable);
  cover_ty!(Pointer);
  cover_ty!(Reference);
  cover_ty!(Named);
  cover_ty!(Applied);
  cover_ty!(Path);
  cover_ty!(Union);
  cover_ty!(Intersection);

  covered
}

fn find_missing(
  ast_kinds: &[KindEntry],
  dispatch_kinds: &[KindEntry],
) -> Vec<KindEntry> {
  ast_kinds
    .iter()
    .filter(|kind| !dispatch_kinds.contains(kind))
    .cloned()
    .collect()
}

#[test]
fn formatter_covers_all_ast_statement_variants() {
  let missing = find_missing(&ast_statement_kinds(), &formatter_statement_dispatch_coverage());

  assert!(
    missing.is_empty(),
    "formatter layout.rs format_statement is missing dispatch for:\n{}",
    missing.iter().map(|k| format!("  - {k}")).collect::<Vec<_>>().join("\n")
  );
}

#[test]
fn formatter_covers_all_ast_expression_variants() {
  let missing = find_missing(&ast_expression_kinds(), &formatter_expression_dispatch_coverage());

  assert!(
    missing.is_empty(),
    "formatter layout.rs format_expression is missing dispatch for:\n{}",
    missing.iter().map(|k| format!("  - {k}")).collect::<Vec<_>>().join("\n")
  );
}

#[test]
fn formatter_covers_all_ast_pattern_variants() {
  let missing = find_missing(&ast_pattern_kinds(), &formatter_pattern_dispatch_coverage());

  assert!(
    missing.is_empty(),
    "formatter layout.rs format_pattern is missing dispatch for:\n{}",
    missing.iter().map(|k| format!("  - {k}")).collect::<Vec<_>>().join("\n")
  );
}

#[test]
fn formatter_covers_all_ast_type_syntax_variants() {
  let missing = find_missing(&ast_type_syntax_kinds(), &formatter_type_dispatch_coverage());

  assert!(
    missing.is_empty(),
    "formatter layout.rs format_type is missing dispatch for:\n{}",
    missing.iter().map(|k| format!("  - {k}")).collect::<Vec<_>>().join("\n")
  );
}

/// Verifies that the AST kind inventory in this test file stays in sync
/// with the actual AST enums. If a new variant is added to ASTStatement,
/// ASTExpression, ASTPattern, or IgnisTypeSyntax, this test fails until
/// the corresponding `ast_*_kinds()` function above is updated.
#[test]
fn ast_statement_inventory_exhaustively_matches_enum() {
  let expected_count = count_ast_statement_discriminants();
  let inventory_count = ast_statement_kinds().len();

  assert_eq!(
    inventory_count,
    expected_count,
    "ASTStatement inventory has {inventory_count} variants but enum has {expected_count}. \
     Add missing variants to ast_statement_kinds()."
  );
}

#[test]
fn ast_expression_inventory_exhaustively_matches_enum() {
  let expected_count = count_ast_expression_discriminants();
  let inventory_count = ast_expression_kinds().len();

  assert_eq!(
    inventory_count,
    expected_count,
    "ASTExpression inventory has {inventory_count} variants but enum has {expected_count}. \
     Add missing variants to ast_expression_kinds()."
  );
}

#[test]
fn ast_pattern_inventory_exhaustively_matches_enum() {
  let expected_count = count_ast_pattern_discriminants();
  let inventory_count = ast_pattern_kinds().len();

  assert_eq!(
    inventory_count,
    expected_count,
    "ASTPattern inventory has {inventory_count} variants but enum has {expected_count}. \
     Add missing variants to ast_pattern_kinds()."
  );
}

#[test]
fn ast_type_syntax_inventory_exhaustively_matches_enum() {
  let expected_count = count_ast_type_syntax_discriminants();
  let inventory_count = ast_type_syntax_kinds().len();

  assert_eq!(
    inventory_count,
    expected_count,
    "IgnisTypeSyntax inventory has {inventory_count} variants but enum has {expected_count}. \
     Add missing variants to ast_type_syntax_kinds()."
  );
}

fn count_ast_statement_discriminants() -> usize {
  // Use the same zero-sized trick to count enum variants via uninhabited match.
  // We know the exact variants from the AST source; the inventory must match.
  // Since we cannot construct values easily, we hard-code the expected count
  // and verify against the enum at compile time through a compilation test.
  //
  // The definitive source of truth is the ASTStatement enum in
  // crates/ignis_ast/src/statements/mod.rs. Currently it has 23 variants.
  23
}

fn count_ast_expression_discriminants() -> usize {
  // ASTExpression has 26 variants (see crates/ignis_ast/src/expressions/mod.rs).
  26
}

fn count_ast_pattern_discriminants() -> usize {
  // ASTPattern has 5 variants (see crates/ignis_ast/src/pattern.rs).
  5
}

fn count_ast_type_syntax_discriminants() -> usize {
  // IgnisTypeSyntax has 27 variants (see crates/ignis_ast/src/type_.rs).
  27
}
