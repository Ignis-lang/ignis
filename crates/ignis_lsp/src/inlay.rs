//! Inlay hints implementation.
//!
//! Provides:
//! - Parameter name hints at call sites
//! - Type hints for match scrutinees
//! - Type hints for variables with inferred types

use std::collections::HashMap;

use ignis_ast::expressions::{ASTAccessOp, ASTExpression};
use ignis_ast::pattern::ASTPattern;
use ignis_ast::statements::{ASTEnumItem, ASTRecordItem, ASTStatement};
use ignis_ast::type_::IgnisTypeSyntax;
use ignis_ast::{ASTNode, NodeId};
use ignis_driver::PerFileAnalysis;
use ignis_type::definition::{DefinitionId, DefinitionKind, DefinitionStore};
use ignis_type::span::Span;
use ignis_type::symbol::SymbolId;
use ignis_type::types::{Type, TypeStore};
use tower_lsp::lsp_types::{InlayHint, InlayHintKind, InlayHintLabel, Position, Range};

use crate::convert::LineIndex;

struct CallInfo {
  argument_ids: Vec<NodeId>,
  argument_spans: Vec<ignis_type::span::Span>,
  def_id: DefinitionId,
  is_instance_call: bool,
}

struct MatchScrutineeInfo {
  node_id: NodeId,
  span: ignis_type::span::Span,
}

struct InferredVariableInfo {
  def_id: DefinitionId,
}

fn collect_calls(file: &PerFileAnalysis) -> Vec<CallInfo> {
  let mut calls = Vec::new();
  let mut stack: Vec<NodeId> = file.roots.clone();

  while let Some(node_id) = stack.pop() {
    let node = file.nodes.get(&node_id);

    match &node {
      ASTNode::Expression(ASTExpression::Call(call)) => {
        let def_id = file
          .resolved_calls
          .get(&node_id)
          .or_else(|| file.node_defs.get(&node_id))
          .cloned();

        if let Some(def_id) = def_id {
          let argument_ids = call.arguments.to_vec();

          let argument_spans = call
            .arguments
            .iter()
            .map(|arg_id| file.nodes.get(arg_id).span().clone())
            .collect();

          let is_instance_call = matches!(
            file.nodes.get(&call.callee),
            ASTNode::Expression(ASTExpression::MemberAccess(member_access)) if member_access.op == ASTAccessOp::Dot
          );

          calls.push(CallInfo {
            argument_ids,
            argument_spans,
            def_id,
            is_instance_call,
          });
        }

        stack.push(call.callee);
        stack.extend(call.arguments.iter().cloned());
      },
      ASTNode::Expression(expr) => {
        collect_expression_children(expr, &mut stack);
      },
      ASTNode::Statement(stmt) => {
        collect_statement_children(stmt, &mut stack);
      },
    }
  }

  calls
}

fn collect_match_scrutinees(file: &PerFileAnalysis) -> Vec<MatchScrutineeInfo> {
  let mut scrutinees = Vec::new();
  let mut stack: Vec<NodeId> = file.roots.clone();

  while let Some(node_id) = stack.pop() {
    let node = file.nodes.get(&node_id);

    match &node {
      ASTNode::Expression(ASTExpression::Match(match_expr)) => {
        let scrutinee_span = file.nodes.get(&match_expr.scrutinee).span().clone();
        scrutinees.push(MatchScrutineeInfo {
          node_id: match_expr.scrutinee,
          span: scrutinee_span,
        });

        stack.push(match_expr.scrutinee);
        for arm in &match_expr.arms {
          if let Some(guard) = arm.guard {
            stack.push(guard);
          }
          stack.push(arm.body);
        }
      },
      ASTNode::Expression(expr) => {
        collect_expression_children(expr, &mut stack);
      },
      ASTNode::Statement(stmt) => {
        collect_statement_children(stmt, &mut stack);
      },
    }
  }

  scrutinees
}

fn collect_inferred_variables(
  file: &PerFileAnalysis,
  defs: &DefinitionStore,
) -> Vec<InferredVariableInfo> {
  let mut results = Vec::new();
  let mut stack: Vec<NodeId> = file.roots.clone();

  while let Some(node_id) = stack.pop() {
    let node = file.nodes.get(&node_id);

    match &node {
      ASTNode::Statement(ASTStatement::Variable(var)) => {
        if matches!(var.type_, IgnisTypeSyntax::Implicit)
          && let Some(def_id) = file.node_defs.get(&node_id)
        {
          results.push(InferredVariableInfo { def_id: *def_id });
        }

        if let Some(value) = &var.value {
          stack.push(*value);
        }
      },
      // `let-else` binds names through refutable patterns. The analyzer does
      // not map pattern nodes to definitions, so binders are resolved against
      // the global definition store by (name, name_span). Only exactly one
      // binding leaf in a refutable pattern (e.g. `Option::SOME(value)`) is
      // hinted; multi-binder/tuple/or patterns are deferred to avoid
      // misleading shared hints. Explicit `binding_type` (which includes the
      // shorthand implicit single-name form) suppresses the inferred hint,
      // mirroring `ASTVariable::type_ != Implicit`.
      ASTNode::Statement(ASTStatement::LetElse(let_else)) => {
        if let_else.binding_type.is_none()
          && let Some((name, span)) = single_binder_let_else(&let_else.pattern)
          && let Some(def_id) = resolve_variable_def(defs, name, &span)
        {
          results.push(InferredVariableInfo { def_id });
        }

        stack.push(let_else.value);
        stack.push(let_else.else_block);
      },
      ASTNode::Expression(expr) => {
        collect_expression_children(expr, &mut stack);
      },
      ASTNode::Statement(stmt) => {
        collect_statement_children(stmt, &mut stack);
      },
    }
  }

  results
}

/// Returns the single binding leaf of a refutable `let-else` pattern when it is
/// safe to anchor exactly one type hint, otherwise `None`.
///
/// Only `Path` patterns with sub-pattern arguments (i.e. enum variant patterns
/// such as `Option::SOME(value)`) are considered: simple identifier let-else is
/// handled via the shorthand path with an explicit `binding_type` (suppressed),
/// and `Tuple`/`Or`/`Wildcard`/`Literal` patterns are deferred per the design.
fn single_binder_let_else(pattern: &ASTPattern) -> Option<(SymbolId, Span)> {
  match pattern {
    ASTPattern::Path { args: Some(args), .. } => {
      let mut leaves = Vec::new();
      for arg in args {
        collect_pattern_binder_leaves(arg, &mut leaves);
      }
      if leaves.len() == 1 { leaves.pop() } else { None }
    },
    _ => None,
  }
}

/// Recursively collects irrefutable identifier bindings (`Path` with a single
/// segment and no arguments) reachable from a pattern. Wildcards and literals
/// bind nothing.
fn collect_pattern_binder_leaves(
  pattern: &ASTPattern,
  out: &mut Vec<(SymbolId, Span)>,
) {
  match pattern {
    ASTPattern::Path { segments, args, .. } => {
      if args.is_none() && segments.len() == 1 {
        out.push((segments[0].0, segments[0].1.clone()));
      } else if let Some(args) = args {
        for arg in args {
          collect_pattern_binder_leaves(arg, out);
        }
      }
    },
    ASTPattern::Tuple { elements, .. } => {
      for element in elements {
        collect_pattern_binder_leaves(element, out);
      }
    },
    ASTPattern::Or { patterns, .. } => {
      for member in patterns {
        collect_pattern_binder_leaves(member, out);
      }
    },
    ASTPattern::Wildcard { .. } | ASTPattern::Literal { .. } => {},
  }
}

/// Resolves a pattern binder to its `DefinitionId` by matching the analyzer-
/// created variable definition on `(name, name_span)`, mirroring the analyzer's
/// own lookup in `define_pattern_binding_if_absent`. The span's embedded file id
/// keeps the match scoped to the current module.
fn resolve_variable_def(
  defs: &DefinitionStore,
  name: SymbolId,
  span: &Span,
) -> Option<DefinitionId> {
  defs.iter().find_map(|(id, def)| {
    if def.name == name && def.name_span == *span && matches!(def.kind, DefinitionKind::Variable(_)) {
      Some(id)
    } else {
      None
    }
  })
}

fn collect_expression_children(
  expr: &ASTExpression,
  stack: &mut Vec<NodeId>,
) {
  match expr {
    ASTExpression::Assignment(a) => {
      stack.push(a.target);
      stack.push(a.value);
    },
    ASTExpression::Binary(b) => {
      stack.push(b.left);
      stack.push(b.right);
    },
    ASTExpression::Ternary(t) => {
      stack.push(t.condition);
      stack.push(t.then_expr);
      stack.push(t.else_expr);
    },
    ASTExpression::Cast(c) => {
      stack.push(c.expression);
    },
    ASTExpression::Call(c) => {
      stack.push(c.callee);
      stack.extend(c.arguments.iter().cloned());
    },
    ASTExpression::Dereference(d) => {
      stack.push(d.inner);
    },
    ASTExpression::Grouped(g) => {
      stack.push(g.expression);
    },
    ASTExpression::Reference(r) => {
      stack.push(r.inner);
    },
    ASTExpression::Unary(u) => {
      stack.push(u.operand);
    },
    ASTExpression::Unit { .. } => {},
    ASTExpression::Literal(_) => {},
    ASTExpression::Variable(_) => {},
    ASTExpression::Vector(v) => {
      stack.extend(v.items.iter().cloned());
    },
    ASTExpression::Tuple(tuple) => {
      stack.extend(tuple.elements.iter().cloned());
    },
    ASTExpression::VectorAccess(va) => {
      stack.push(va.name);
      stack.push(va.index);
    },
    ASTExpression::Path(_) => {},
    ASTExpression::PostfixIncrement { expr, .. } => {
      stack.push(*expr);
    },
    ASTExpression::PostfixDecrement { expr, .. } => {
      stack.push(*expr);
    },
    ASTExpression::MemberAccess(m) => {
      stack.push(m.object);
    },
    ASTExpression::RecordInit(r) => {
      for field in &r.fields {
        stack.push(field.value);
      }
    },
    ASTExpression::BuiltinCall(bc) => {
      for arg_id in &bc.args {
        stack.push(*arg_id);
      }
    },
    ASTExpression::LetCondition(let_condition) => {
      stack.push(let_condition.value);
    },
    ASTExpression::Match(match_expr) => {
      stack.push(match_expr.scrutinee);
      for arm in &match_expr.arms {
        if let Some(guard) = arm.guard {
          stack.push(guard);
        }
        stack.push(arm.body);
      }
    },
    ASTExpression::Lambda(lambda) => match &lambda.body {
      ignis_ast::expressions::lambda::LambdaBody::Expression(id) => stack.push(*id),
      ignis_ast::expressions::lambda::LambdaBody::Block(id) => stack.push(*id),
    },
    ASTExpression::CaptureOverride(co) => {
      stack.push(co.inner);
    },
    ASTExpression::Pipe { lhs, rhs, .. } => {
      stack.push(*lhs);
      stack.push(*rhs);
    },
    ASTExpression::PipePlaceholder { .. } => {},
    ASTExpression::Try { expr, .. } => {
      stack.push(*expr);
    },
  }
}

fn collect_statement_children(
  stmt: &ASTStatement,
  stack: &mut Vec<NodeId>,
) {
  match stmt {
    ASTStatement::Variable(v) => {
      if let Some(value) = &v.value {
        stack.push(*value);
      }
    },
    ASTStatement::LetElse(let_else) => {
      stack.push(let_else.value);
      stack.push(let_else.else_block);
    },
    ASTStatement::Expression(e) => {
      collect_expression_children(e, stack);
    },
    ASTStatement::Block(b) => {
      stack.extend(b.statements.iter().cloned());
    },
    ASTStatement::If(i) => {
      stack.push(i.condition);
      stack.push(i.then_block);
      if let Some(else_block) = &i.else_block {
        stack.push(*else_block);
      }
    },
    ASTStatement::While(w) => {
      stack.push(w.condition);
      stack.push(w.body);
    },
    ASTStatement::For(f) => {
      stack.push(f.initializer);
      stack.push(f.condition);
      stack.push(f.increment);
      stack.push(f.body);
    },
    ASTStatement::ForOf(fo) => {
      stack.push(fo.iter);
      stack.push(fo.body);
    },
    ASTStatement::Return(r) => {
      if let Some(expr) = &r.expression {
        stack.push(*expr);
      }
    },
    ASTStatement::Function(f) => {
      if let Some(body) = &f.body {
        stack.push(*body);
      }
    },
    ASTStatement::Namespace(n) => {
      stack.extend(n.items.iter().cloned());
    },
    ASTStatement::Record(r) => {
      for item in &r.items {
        match item {
          ASTRecordItem::Method(method) => {
            stack.push(method.body);
          },
          ASTRecordItem::Field(field) => {
            if let Some(value) = &field.value {
              stack.push(*value);
            }
          },
        }
      }
    },
    ASTStatement::Enum(e) => {
      for item in &e.items {
        match item {
          ASTEnumItem::Method(method) => {
            stack.push(method.body);
          },
          ASTEnumItem::Field(field) => {
            if let Some(value) = &field.value {
              stack.push(*value);
            }
          },
          ASTEnumItem::Variant(_) => {},
        }
      }
    },
    ASTStatement::Constant(c) => {
      if let Some(value) = &c.value {
        stack.push(*value);
      }
    },

    ASTStatement::Defer(d) => {
      stack.push(d.expression);
    },

    ASTStatement::Import(_)
    | ASTStatement::Export(_)
    | ASTStatement::Extern(_)
    | ASTStatement::TypeAlias(_)
    | ASTStatement::Break(_)
    | ASTStatement::Continue(_)
    | ASTStatement::Comment(_)
    | ASTStatement::Trait(_) => {},
  }
}

pub fn generate_hints(
  file: &PerFileAnalysis,
  defs: &DefinitionStore,
  types: &TypeStore,
  symbol_names: &HashMap<SymbolId, String>,
  line_index: &LineIndex,
) -> Vec<InlayHint> {
  let mut hints = generate_parameter_hints(file, defs, symbol_names, line_index);
  hints.extend(generate_match_scrutinee_type_hints(file, defs, types, symbol_names, line_index));
  hints.extend(generate_inferred_variable_type_hints(
    file,
    defs,
    types,
    symbol_names,
    line_index,
  ));
  hints
}

fn generate_parameter_hints(
  file: &PerFileAnalysis,
  defs: &DefinitionStore,
  symbol_names: &HashMap<SymbolId, String>,
  line_index: &LineIndex,
) -> Vec<InlayHint> {
  let calls = collect_calls(file);
  let mut hints = Vec::new();

  for call in calls {
    let def = defs.get(&call.def_id);

    let (param_ids, param_offset, is_variadic): (&[DefinitionId], usize, bool) = match &def.kind {
      DefinitionKind::Function(func) => (&func.params, 0, func.is_variadic),
      DefinitionKind::Method(method) => {
        let offset = if !method.is_static && call.is_instance_call {
          1
        } else {
          0
        };
        (&method.params, offset, false)
      },
      _ => continue,
    };

    for (arg_idx, (arg_id, arg_span)) in call.argument_ids.iter().zip(call.argument_spans.iter()).enumerate() {
      let param_idx = arg_idx + param_offset;

      if param_idx >= param_ids.len() {
        if is_variadic {
          continue;
        }
        break;
      }

      let param_def = defs.get(&param_ids[param_idx]);
      let param_name = match symbol_names.get(&param_def.name) {
        Some(name) => name.as_str(),
        None => continue,
      };

      if param_name.len() <= 1 || param_name.starts_with('_') {
        continue;
      }

      if let ASTNode::Expression(ASTExpression::Variable(var)) = file.nodes.get(arg_id)
        && let Some(arg_name) = symbol_names.get(&var.name)
        && arg_name == param_name
      {
        continue;
      }

      let (line, col) = line_index.line_col_utf16(arg_span.start);

      hints.push(InlayHint {
        position: Position { line, character: col },
        label: InlayHintLabel::String(format!("{}: ", param_name)),
        kind: Some(InlayHintKind::PARAMETER),
        text_edits: None,
        tooltip: None,
        padding_left: None,
        padding_right: None,
        data: None,
      });
    }
  }

  hints
}

fn generate_match_scrutinee_type_hints(
  file: &PerFileAnalysis,
  defs: &DefinitionStore,
  types: &TypeStore,
  symbol_names: &HashMap<SymbolId, String>,
  line_index: &LineIndex,
) -> Vec<InlayHint> {
  let scrutinees = collect_match_scrutinees(file);
  let mut hints = Vec::new();

  for scrutinee in scrutinees {
    let Some(type_id) = file.node_types.get(&scrutinee.node_id) else {
      continue;
    };

    let ty = types.get(type_id);
    if matches!(ty, Type::Error | Type::Infer) {
      continue;
    }

    let type_label = crate::type_format::format_type(types, defs, symbol_names, type_id);
    let (line, col) = line_index.line_col_utf16(scrutinee.span.end);

    hints.push(InlayHint {
      position: Position { line, character: col },
      label: InlayHintLabel::String(format!(": {}", type_label)),
      kind: Some(InlayHintKind::TYPE),
      text_edits: None,
      tooltip: None,
      padding_left: Some(true),
      padding_right: None,
      data: None,
    });
  }

  hints
}

fn generate_inferred_variable_type_hints(
  file: &PerFileAnalysis,
  defs: &DefinitionStore,
  types: &TypeStore,
  symbol_names: &HashMap<SymbolId, String>,
  line_index: &LineIndex,
) -> Vec<InlayHint> {
  let variables = collect_inferred_variables(file, defs);
  let mut hints = Vec::new();

  for var_info in variables {
    let def = defs.get(&var_info.def_id);

    let type_id = match &def.kind {
      DefinitionKind::Variable(v) => &v.type_id,
      _ => continue,
    };

    let ty = types.get(type_id);
    if matches!(ty, Type::Error | Type::Infer | Type::InferVar(_) | Type::Unknown) {
      continue;
    }

    let type_label = crate::type_format::format_type(types, defs, symbol_names, type_id);
    let (line, col) = line_index.line_col_utf16(def.name_span.end);

    hints.push(InlayHint {
      position: Position { line, character: col },
      label: InlayHintLabel::String(format!(": {}", type_label)),
      kind: Some(InlayHintKind::TYPE),
      text_edits: None,
      tooltip: None,
      padding_left: None,
      padding_right: None,
      data: None,
    });
  }

  hints
}

pub fn filter_hints_by_range(
  hints: &[InlayHint],
  range: &Range,
) -> Vec<InlayHint> {
  hints
    .iter()
    .filter(|hint| {
      let pos = hint.position;
      let starts_after_or_at =
        pos.line > range.start.line || (pos.line == range.start.line && pos.character >= range.start.character);
      let ends_before_or_at =
        pos.line < range.end.line || (pos.line == range.end.line && pos.character < range.end.character);
      starts_after_or_at && ends_before_or_at
    })
    .cloned()
    .collect()
}

#[cfg(test)]
#[allow(clippy::items_after_test_module)]
mod tests {
  use super::{filter_hints_by_range, generate_hints};
  use std::path::PathBuf;
  use std::time::{SystemTime, UNIX_EPOCH};

  use ignis_config::IgnisConfig;
  use ignis_driver::{AnalysisOptions, AnalyzeProjectOutput};
  use ignis_type::file::FileId;
  use tower_lsp::lsp_types::{InlayHint, InlayHintKind, InlayHintLabel, Position, Range};

  use crate::convert::LineIndex;

  fn unique_temp_dir(name: &str) -> PathBuf {
    let timestamp = SystemTime::now()
      .duration_since(UNIX_EPOCH)
      .expect("system time should be after epoch")
      .as_nanos();

    std::env::temp_dir().join(format!("ignis_lsp_inlay_{name}_{}_{}", std::process::id(), timestamp))
  }

  /// Analyze a temp source, asserting it analyzes cleanly (no diagnostics).
  fn analyze_clean(
    name: &str,
    source: &str,
  ) -> (AnalyzeProjectOutput, FileId, LineIndex) {
    let (output, file_id, line_index) = analyze_raw(name, source);
    assert!(
      output.diagnostics.is_empty(),
      "test source should analyze cleanly: {:#?}",
      output.diagnostics
    );
    (output, file_id, line_index)
  }

  /// Analyze a temp source without asserting clean diagnostics (used for cases
  /// that intentionally exercise the error/infer skip branch).
  fn analyze_lenient(
    name: &str,
    source: &str,
  ) -> (AnalyzeProjectOutput, FileId, LineIndex) {
    analyze_raw(name, source)
  }

  fn analyze_raw(
    name: &str,
    source: &str,
  ) -> (AnalyzeProjectOutput, FileId, LineIndex) {
    let root = unique_temp_dir(name);
    let file_path = root.join("main.ign");
    std::fs::create_dir_all(&root).expect("test root should be created");
    std::fs::write(&file_path, source).expect("test source should be written");

    let config = IgnisConfig::default();
    let output =
      ignis_driver::analyze_project_with_options(&config, &file_path.to_string_lossy(), AnalysisOptions::default());
    let file_id = output
      .source_map
      .lookup_by_path(&file_path)
      .expect("analyzed file should be in source map");
    let line_index = LineIndex::new(output.source_map.get(&file_id).text.clone());
    (output, file_id, line_index)
  }

  fn hints(
    output: &AnalyzeProjectOutput,
    file_id: FileId,
    line_index: &LineIndex,
  ) -> Vec<InlayHint> {
    let file = output.file_analysis(&file_id).expect("file analysis should exist");
    generate_hints(file, &output.defs, &output.types, &output.symbol_names, line_index)
  }

  fn type_hint_at(
    hints: &[InlayHint],
    pos: Position,
  ) -> Option<String> {
    hints.iter().find_map(|hint| {
      if hint.position == pos && hint.kind == Some(InlayHintKind::TYPE) {
        match &hint.label {
          InlayHintLabel::String(label) => Some(label.clone()),
          _ => None,
        }
      } else {
        None
      }
    })
  }

  fn name_end_pos(
    source: &str,
    name: &str,
    line_index: &LineIndex,
  ) -> Position {
    let byte = source
      .find(name)
      .unwrap_or_else(|| panic!("name {name:?} should exist in source"))
      + name.len();
    let (line, col) = line_index.line_col_utf16(ignis_type::BytePosition(byte as u32));
    Position { line, character: col }
  }

  /// Inferred simple `let` binding emits a single TYPE hint at the binding
  /// name end with the formatted inferred type.
  #[test]
  fn inferred_simple_let_emits_type_hint() {
    let source = "function main(): i32 {\n    let value = 1;\n    return value;\n}\n";
    let (output, file_id, line_index) = analyze_clean("simple_let", source);

    let pos = name_end_pos(source, "value", &line_index);
    let all = hints(&output, file_id, &line_index);
    assert_eq!(type_hint_at(&all, pos).as_deref(), Some(": i32"));
    assert_eq!(all.iter().filter(|h| h.kind == Some(InlayHintKind::TYPE)).count(), 1);
  }

  /// Explicit type annotation on a `let` binding suppresses the inferred
  /// TYPE hint.
  #[test]
  fn explicit_annotation_suppresses_let_type_hint() {
    let source = "function main(): i32 {\n    let value: i32 = 1;\n    return value;\n}\n";
    let (output, file_id, line_index) = analyze_clean("explicit_let", source);

    let all = hints(&output, file_id, &line_index);
    assert!(
      all.iter().all(|h| h.kind != Some(InlayHintKind::TYPE)),
      "explicitly annotated let should not produce a type hint, got {all:?}"
    );
  }

  /// Type hint positions use UTF-16 code units: a non-BMP character earlier on
  /// the same line shifts the column differently than a raw byte offset would.
  #[test]
  fn let_type_hint_uses_utf16_positioning() {
    // `U+1D11E` is 4 UTF-8 bytes but 2 UTF-16 code units. It sits in an inline
    // block comment before `let value`, forcing UTF-16 != byte offset.
    let source = "function main(): i32 {\n    /* \u{1D11E} */ let value = 1;\n    return value;\n}\n";
    let (output, file_id, line_index) = analyze_clean("utf16_let", source);

    let pos = name_end_pos(source, "value", &line_index);
    assert_eq!(pos.line, 1, "hint should be on the let line");
    assert_eq!(pos.character, 22, "UTF-16 column should account for the non-BMP char");

    let all = hints(&output, file_id, &line_index);
    assert_eq!(type_hint_at(&all, pos).as_deref(), Some(": i32"));
  }

  /// A binding whose inferred type is unresolved/error must not produce a type
  /// hint. This exercises the Error/Infer skip branch through real analyzer
  /// output (lenient diagnostics are expected).
  #[test]
  fn unresolved_error_type_is_skipped() {
    let source = "function main(): i32 {\n    let value = undefinedName;\n    return 0;\n}\n";
    let (output, file_id, line_index) = analyze_lenient("error_let", source);
    assert!(
      !output.diagnostics.is_empty(),
      "this source is expected to produce an unresolved-name diagnostic"
    );

    let all = hints(&output, file_id, &line_index);
    assert!(
      type_hint_at(&all, name_end_pos(source, "value", &line_index)).is_none(),
      "error-typed binding should not get a type hint, got {all:?}"
    );
    assert!(
      all
        .iter()
        .all(|h| !matches!(&h.label, InlayHintLabel::String(label) if label == ": error")),
      "no hint should emit the synthetic `: error` label"
    );
  }

  /// Parameter-name hints still appear for call arguments after hardening.
  #[test]
  fn parameter_hints_non_regression() {
    let source = "\
function main(): i32 {
    return add(2, 3);
}

function add(left: i32, right: i32): i32 {
    return left + right;
}
";
    let (output, file_id, line_index) = analyze_clean("param_hints", source);
    let all = hints(&output, file_id, &line_index);

    let param_hints: Vec<&InlayHint> = all
      .iter()
      .filter(|h| h.kind == Some(InlayHintKind::PARAMETER))
      .collect();
    assert_eq!(param_hints.len(), 2, "both call arguments should get parameter hints");

    let labels: Vec<String> = param_hints
      .iter()
      .map(|h| match &h.label {
        InlayHintLabel::String(s) => s.clone(),
        _ => String::new(),
      })
      .collect();
    assert!(labels.contains(&"left: ".to_string()), "missing left label: {labels:?}");
    assert!(labels.contains(&"right: ".to_string()), "missing right label: {labels:?}");
  }

  /// Match scrutinee type hints still appear after hardening.
  #[test]
  fn match_scrutinee_hints_non_regression() {
    let source = "function main(): i32 {\n    let value = 1;\n    return match (value) { 0 -> 0, _ -> value, };\n}\n";
    let (output, file_id, line_index) = analyze_clean("scrutinee", source);

    // Scrutinee `value` ends right after the `value` token inside `match (...)`.
    let match_pos = source.find("match (value)").expect("match scrutinee should exist");
    let scrutinee_end = match_pos + "match (".len() + "value".len();
    let (line, col) = line_index.line_col_utf16(ignis_type::BytePosition(scrutinee_end as u32));
    let pos = Position { line, character: col };

    let all = hints(&output, file_id, &line_index);
    assert_eq!(
      type_hint_at(&all, pos).as_deref(),
      Some(": i32"),
      "scrutinee type hint missing in {all:?}"
    );
  }

  /// `filter_hints_by_range` keeps hints inside the range and drops hints
  /// outside it, including boundary behavior (end is exclusive).
  #[test]
  fn filter_hints_by_range_boundaries() {
    let make = |line, character| InlayHint {
      position: Position { line, character },
      label: InlayHintLabel::String(": i32".to_string()),
      kind: Some(InlayHintKind::TYPE),
      text_edits: None,
      tooltip: None,
      padding_left: None,
      padding_right: None,
      data: None,
    };
    let hints_vec = vec![
      make(0, 0),  // before range start (excluded)
      make(0, 5),  // at range start (included)
      make(2, 0),  // inside range (included)
      make(3, 10), // at range end (excluded: end is exclusive)
      make(4, 0),  // after range end (excluded)
    ];
    let range = Range {
      start: Position { line: 0, character: 5 },
      end: Position { line: 3, character: 10 },
    };
    let filtered = filter_hints_by_range(&hints_vec, &range);
    assert_eq!(filtered.len(), 2, "only in-range hints should remain, got {filtered:?}");
    assert_eq!(filtered[0].position.character, 5);
    assert_eq!(filtered[1].position.line, 2);
  }

  /// A refutable `let-else` with exactly one binding leaf (a single-binder
  /// enum variant) emits a TYPE hint for the binder at its name end.
  #[test]
  fn let_else_single_binder_variant_emits_hint() {
    let source = "\
enum Maybe {
    SOME(i32),
    NONE,
}

function main(): i32 {
    let opt = Maybe::SOME(42);
    let Maybe::SOME(value) = opt else {
        return 1;
    };
    return value;
}
";
    let (output, file_id, line_index) = analyze_clean("letelse_variant", source);

    let all = hints(&output, file_id, &line_index);
    assert_eq!(
      type_hint_at(&all, name_end_pos(source, "value", &line_index)).as_deref(),
      Some(": i32"),
      "single-binder variant let-else should emit a type hint, got {all:?}"
    );
  }

  /// An explicit `binding_type` on a shorthand `let-else` suppresses the type
  /// hint, mirroring an explicit `let` annotation.
  #[test]
  fn let_else_explicit_binding_type_is_suppressed() {
    let source = "\
@lang(try)
enum Try {
    OK(i32),
    ERR(i32),
}

function main(): i32 {
    let res = Try::OK(42);
    let value: i32 = res else {
        return 1;
    };
    return value;
}
";
    let (output, file_id, line_index) = analyze_clean("letelse_explicit", source);

    let all = hints(&output, file_id, &line_index);
    assert!(
      type_hint_at(&all, name_end_pos(source, "value", &line_index)).is_none(),
      "explicit binding_type let-else should not produce a type hint, got {all:?}"
    );
    assert!(
      all
        .iter()
        .all(|h| !matches!(&h.label, InlayHintLabel::String(label) if label == ": i32")),
      "no inferred type hint should be emitted for the explicit let-else binder, got {all:?}"
    );
  }

  /// A multi-binder (tuple destructuring) `let-else` does not produce a shared
  /// or per-binder type hint — complex patterns are deferred.
  #[test]
  fn let_else_multi_binder_emits_no_hint() {
    let source = "\
enum Pair {
    P(i32, i32),
    N,
}

function main(): i32 {
    let p = Pair::P(1, 2);
    let Pair::P(first, second) = p else {
        return 1;
    };
    return first + second;
}
";
    let (output, file_id, line_index) = analyze_clean("letelse_tuple", source);

    let all = hints(&output, file_id, &line_index);
    assert!(
      type_hint_at(&all, name_end_pos(source, "first", &line_index)).is_none(),
      "multi-binder `first` should not get a shared hint, got {all:?}"
    );
    assert!(
      type_hint_at(&all, name_end_pos(source, "second", &line_index)).is_none(),
      "multi-binder `second` should not get a shared hint, got {all:?}"
    );
  }

  /// A wildcard-payload `let-else` has no binder to hint: no TYPE hint is
  /// anchored at the wildcard position (the only type hint comes from the
  /// inferred `let opt` binding).
  #[test]
  fn let_else_wildcard_payload_emits_no_hint() {
    let source = "\
enum Maybe {
    SOME(i32),
    NONE,
}

function main(): i32 {
    let opt = Maybe::SOME(42);
    let Maybe::SOME(_) = opt else {
        return 1;
    };
    return 0;
}
";
    let (output, file_id, line_index) = analyze_clean("letelse_wildcard", source);
    let all = hints(&output, file_id, &line_index);

    // The inferred `let opt` binding legitimately gets a hint; the wildcard
    // let-else must not add a hint anchored at the `_` position.
    assert!(
      type_hint_at(&all, name_end_pos(source, "opt", &line_index)).is_some(),
      "the inferred `opt` binding should still get its own hint, got {all:?}"
    );
    let wildcard_end = source
      .find("SOME(_) = opt")
      .map(|i| i + "SOME(_".len())
      .expect("wildcard let-else should exist");
    let (wildcard_line, wildcard_col) = line_index.line_col_utf16(ignis_type::BytePosition(wildcard_end as u32));
    let wildcard_pos = Position {
      line: wildcard_line,
      character: wildcard_col,
    };
    assert!(
      type_hint_at(&all, wildcard_pos).is_none(),
      "wildcard-payload let-else should not anchor a type hint at the wildcard, got {all:?}"
    );
  }
}
