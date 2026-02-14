//! LSP completion support for Ignis.
//!
//! This module provides autocompletion by analyzing tokens and cached analysis
//! without depending on a valid AST.

use std::collections::{HashMap, HashSet};
use std::io::Write;

use crate::at_items::AtItemKind;

/// Log to ignis_lsp.log for debugging
pub fn log(msg: &str) {
  let _ = std::panic::catch_unwind(|| {
    eprintln!("[IgnisLSP] {}", msg);
    if let Ok(mut f) = std::fs::OpenOptions::new()
      .create(true)
      .append(true)
      .open("ignis_lsp.log")
    {
      let _ = writeln!(f, "{}", msg);
    }
  });
}

use ignis_ast::statements::{ASTExport, ASTStatement};
use ignis_ast::ASTNode;
use ignis_driver::AnalyzeProjectOutput;
use ignis_token::token::Token;
use ignis_token::token_types::TokenType;
use ignis_type::span::Span;
use ignis_type::definition::{Definition, DefinitionId, DefinitionKind, DefinitionStore, SymbolEntry, Visibility};
use ignis_type::file::FileId;
use ignis_type::symbol::SymbolId;
use ignis_type::types::{Type, TypeId, TypeStore};
use tower_lsp::lsp_types::{CompletionItem, CompletionItemKind, InsertTextFormat};

/// Completion context detected from tokens.
#[derive(Debug)]
pub enum CompletionContext {
  /// After `.` - instance member access.
  AfterDot {
    /// Byte offset of the `.` token.
    dot_offset: u32,
    /// Partial identifier typed after the dot.
    prefix: String,
  },

  /// After `::` - static member access or namespace path.
  AfterDoubleColon {
    /// Byte offset of the `::` token (reserved for future use).
    #[allow(dead_code)]
    double_colon_offset: u32,
    /// Path segments before `::` (e.g., `["Math"]` for `Math::`).
    path_segments: Vec<String>,
    /// Partial identifier typed after `::`.
    prefix: String,
  },

  /// Typing an identifier (not after `.` or `::`).
  Identifier {
    /// Partial identifier typed.
    prefix: String,
    /// Offset where the identifier starts.
    start_offset: u32,
  },

  /// Inside import path string: `from "std::..."`.
  ImportPath {
    /// Partial path typed.
    prefix: String,
  },

  /// Inside record initializer: `RecordName { field1: val, | }`.
  RecordInit {
    /// Name of the record being initialized.
    record_name: String,
    /// Fields already assigned in the initializer.
    assigned_fields: Vec<String>,
    /// Partial field name typed.
    prefix: String,
  },

  /// After `@` — builtin call or directive attribute.
  AfterAt {
    /// Partial identifier typed after `@`.
    prefix: String,
  },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CompletionScope {
  Global,
  TypeBody,
  CallableBody,
}

#[derive(Debug, Clone, Copy)]
struct ScopeFrame {
  scope: CompletionScope,
  start: u32,
}

/// Internal representation of a completion candidate.
pub struct CompletionCandidate {
  pub label: String,
  pub kind: CompletionKind,
  pub detail: Option<String>,
  pub documentation: Option<String>,
  pub insert_text: Option<String>,
  pub insert_text_format: Option<InsertTextFormat>,
  /// Sort priority (lower = higher priority).
  pub sort_priority: u8,
}

/// Kind of completion item.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CompletionKind {
  Function,
  Method,
  Field,
  Variable,
  Constant,
  Record,
  Enum,
  EnumVariant,
  Namespace,
  TypeAlias,
  Keyword,
  Module,
}

impl From<CompletionKind> for CompletionItemKind {
  fn from(kind: CompletionKind) -> Self {
    match kind {
      CompletionKind::Function => CompletionItemKind::FUNCTION,
      CompletionKind::Method => CompletionItemKind::METHOD,
      CompletionKind::Field => CompletionItemKind::FIELD,
      CompletionKind::Variable => CompletionItemKind::VARIABLE,
      CompletionKind::Constant => CompletionItemKind::CONSTANT,
      CompletionKind::Record => CompletionItemKind::STRUCT,
      CompletionKind::Enum => CompletionItemKind::ENUM,
      CompletionKind::EnumVariant => CompletionItemKind::ENUM_MEMBER,
      CompletionKind::Namespace => CompletionItemKind::MODULE,
      CompletionKind::TypeAlias => CompletionItemKind::INTERFACE,
      CompletionKind::Keyword => CompletionItemKind::KEYWORD,
      CompletionKind::Module => CompletionItemKind::MODULE,
    }
  }
}

/// Detect completion context from tokens and cursor position.
///
/// This is token-based (not AST-based) to work even when the parser fails.
pub fn detect_context(
  tokens: &[Token],
  cursor_offset: u32,
  source_text: &str,
) -> Option<CompletionContext> {
  // Filter out whitespace and comments
  let meaningful: Vec<&Token> = tokens
    .iter()
    .filter(|t| {
      !matches!(
        t.type_,
        TokenType::Whitespace | TokenType::Comment | TokenType::MultiLineComment | TokenType::DocComment
      )
    })
    .collect();

  // Check if inside a string literal after `from` keyword (import path)
  if let Some(ctx) = detect_import_path_context(&meaningful, cursor_offset, source_text) {
    return Some(ctx);
  }

  // Fallback: detect import context from raw text (robust against lexer failures)
  if let Some(ctx) = detect_import_path_fallback(source_text, cursor_offset) {
    return Some(ctx);
  }

  // Find tokens near cursor
  let (prev_token, current_token) = find_adjacent_tokens(&meaningful, cursor_offset);

  // Check for `@` context (builtins and directives)
  if let Some(ctx) = detect_at_context(prev_token, current_token, &meaningful, cursor_offset, source_text) {
    return Some(ctx);
  }

  // Check for `.` context
  if let Some(prev) = prev_token {
    if prev.type_ == TokenType::Dot {
      let prefix = extract_prefix_at_cursor(current_token, cursor_offset);
      return Some(CompletionContext::AfterDot {
        dot_offset: prev.span.start.0,
        prefix,
      });
    }

    // Check for `::` context
    if prev.type_ == TokenType::DoubleColon {
      let path_segments = collect_path_segments_backwards(&meaningful, prev);
      let prefix = extract_prefix_at_cursor(current_token, cursor_offset);
      return Some(CompletionContext::AfterDoubleColon {
        double_colon_offset: prev.span.start.0,
        path_segments,
        prefix,
      });
    }

    // Check for record initializer context: `RecordName { field: val, |`
    if (prev.type_ == TokenType::Comma || prev.type_ == TokenType::LeftBrace)
      && let Some(ctx) = detect_record_init_context(&meaningful, prev, current_token, cursor_offset, source_text)
    {
      return Some(ctx);
    }
  }

  // Default: identifier context
  //
  // Find the identifier token being typed. Two cases:
  //   1. current_token is an Identifier → cursor is inside it
  //   2. prev_token is an Identifier ending exactly at cursor → cursor at end
  //
  // Case 2 matters when the next token starts at cursor (e.g., `result.t|)`)
  // — find_adjacent_tokens returns prev=t, current=), and we must not miss `t`.
  let ident_token = current_token
    .filter(|tok| tok.type_ == TokenType::Identifier)
    .or_else(|| prev_token.filter(|tok| tok.type_ == TokenType::Identifier && tok.span.end.0 == cursor_offset));

  let (prefix, start_offset) = if let Some(tok) = ident_token {
    let prefix_end = cursor_offset.min(tok.span.end.0);
    let prefix_start = tok.span.start.0;

    if prefix_end > prefix_start {
      let prefix = source_text
        .get(prefix_start as usize..prefix_end as usize)
        .unwrap_or("")
        .to_string();
      (prefix, prefix_start)
    } else {
      (String::new(), cursor_offset)
    }
  } else {
    (String::new(), cursor_offset)
  };

  // Check if the identifier we're about to return is preceded by `.` or `::`
  // in the meaningful token list. This handles `foo.ba|` where prev_token is
  // the identifier being typed, not the dot.
  if !prefix.is_empty()
    && let Some(before_ident) = find_token_before_offset(&meaningful, start_offset)
  {
    if before_ident.type_ == TokenType::Dot {
      return Some(CompletionContext::AfterDot {
        dot_offset: before_ident.span.start.0,
        prefix,
      });
    }

    if before_ident.type_ == TokenType::DoubleColon {
      let path_segments = collect_path_segments_backwards(&meaningful, before_ident);
      return Some(CompletionContext::AfterDoubleColon {
        double_colon_offset: before_ident.span.start.0,
        path_segments,
        prefix,
      });
    }
  }

  Some(CompletionContext::Identifier { prefix, start_offset })
}

/// Fallback import path detection using raw text.
/// Handles unclosed quotes or desynchronized tokens by searching for `from "` or `import "`.
fn detect_import_path_fallback(
  text: &str,
  cursor_offset: u32,
) -> Option<CompletionContext> {
  let cursor = (cursor_offset as usize).min(text.len());
  let pre_cursor = text.get(..cursor)?;

  // Limit search window to avoid scanning large files
  let window_start = pre_cursor.len().saturating_sub(200);
  let window_start = (window_start..)
    .take(4)
    .find(|&i| text.is_char_boundary(i))
    .unwrap_or(pre_cursor.len());

  let window = pre_cursor.get(window_start..)?;

  for pattern in ["from \"", "import \""] {
    if let Some(idx) = window.rfind(pattern) {
      let content = window.get(idx + pattern.len()..)?;
      if content.contains('\n') {
        return None;
      }
      return Some(CompletionContext::ImportPath {
        prefix: content.to_string(),
      });
    }
  }

  None
}

/// Detect if cursor is inside an import path string.
/// Clamps offsets to handle editor/server position mismatches.
fn detect_import_path_context(
  tokens: &[&Token],
  cursor_offset: u32,
  source_text: &str,
) -> Option<CompletionContext> {
  let text_len = source_text.len();

  for (i, tok) in tokens.iter().enumerate() {
    if tok.type_ != TokenType::From {
      continue;
    }

    let Some(string_tok) = tokens.get(i + 1) else { continue };
    if string_tok.type_ != TokenType::String {
      continue;
    }

    let token_start = (string_tok.span.start.0 as usize).min(text_len);
    let token_end = (string_tok.span.end.0 as usize).min(text_len);
    let cursor = (cursor_offset as usize).min(text_len);

    if cursor < token_start || cursor > token_end {
      continue;
    }

    // Skip opening quote
    let content_start = (token_start + 1).min(token_end);
    let clamped_cursor = cursor.clamp(content_start, token_end);

    // Snap to valid UTF-8 boundaries
    let content_start = find_char_boundary(source_text, content_start);
    let clamped_cursor = find_char_boundary(source_text, clamped_cursor);

    let prefix = source_text.get(content_start..clamped_cursor).unwrap_or("").to_string();
    return Some(CompletionContext::ImportPath { prefix });
  }

  None
}

/// Find the token immediately before and at/after the cursor.
fn find_adjacent_tokens<'a>(
  tokens: &[&'a Token],
  cursor_offset: u32,
) -> (Option<&'a Token>, Option<&'a Token>) {
  let mut prev: Option<&Token> = None;
  let mut current: Option<&Token> = None;

  for tok in tokens {
    if tok.span.end.0 <= cursor_offset {
      prev = Some(*tok);
    } else if tok.span.start.0 <= cursor_offset && cursor_offset <= tok.span.end.0 {
      current = Some(*tok);
      break;
    } else {
      // Token is after cursor
      break;
    }
  }

  (prev, current)
}

/// Find the meaningful token immediately before a given byte offset.
fn find_token_before_offset<'a>(
  tokens: &[&'a Token],
  offset: u32,
) -> Option<&'a Token> {
  let mut result = None;

  for tok in tokens {
    if tok.span.end.0 <= offset {
      result = Some(*tok);
    } else {
      break;
    }
  }

  result
}

/// Extract prefix being typed at cursor position.
fn extract_prefix_at_cursor(
  current_token: Option<&Token>,
  cursor_offset: u32,
) -> String {
  if let Some(tok) = current_token
    && tok.type_ == TokenType::Identifier
    && tok.span.start.0 <= cursor_offset
  {
    let end = (cursor_offset - tok.span.start.0) as usize;
    return tok.lexeme.chars().take(end).collect();
  }
  String::new()
}

/// Collect path segments backwards from a `::` token.
///
/// For `Math::Vector::`, returns `["Math", "Vector"]`.
fn collect_path_segments_backwards(
  tokens: &[&Token],
  double_colon: &Token,
) -> Vec<String> {
  let mut segments = Vec::new();
  let mut expect_ident = true;

  // Find index of double_colon
  let Some(dc_idx) = tokens.iter().position(|t| std::ptr::eq(*t, double_colon)) else {
    return segments;
  };

  // Walk backwards
  for i in (0..dc_idx).rev() {
    let tok = tokens[i];

    if expect_ident {
      if tok.type_ == TokenType::Identifier {
        segments.push(tok.lexeme.clone());
        expect_ident = false;
      } else {
        break;
      }
    } else if tok.type_ == TokenType::DoubleColon {
      expect_ident = true;
    } else {
      break;
    }
  }

  segments.reverse();
  segments
}

/// Detect record initializer context: `RecordName { field1: val, |`
fn detect_record_init_context(
  tokens: &[&Token],
  trigger_token: &Token,
  current_token: Option<&Token>,
  cursor_offset: u32,
  _source_text: &str,
) -> Option<CompletionContext> {
  let trigger_idx = tokens.iter().position(|t| std::ptr::eq(*t, trigger_token))?;

  // Scan backwards to find opening `{`
  let mut brace_depth = 0i32;
  let mut open_brace_idx = None;

  for i in (0..=trigger_idx).rev() {
    let tok = tokens[i];

    match tok.type_ {
      TokenType::RightBrace => brace_depth += 1,
      TokenType::LeftBrace => {
        if brace_depth == 0 {
          open_brace_idx = Some(i);
          break;
        }
        brace_depth -= 1;
      },
      _ => {},
    }
  }

  let open_brace_idx = open_brace_idx?;

  if open_brace_idx == 0 {
    return None;
  }

  let name_token = tokens[open_brace_idx - 1];
  if name_token.type_ != TokenType::Identifier {
    return None;
  }

  // Filter out block statements like `if (cond) {` or `else {`
  if open_brace_idx >= 2 {
    let before_name = tokens[open_brace_idx - 2];
    if matches!(before_name.type_, TokenType::RightParen | TokenType::Else) {
      return None;
    }
  }

  let record_name = name_token.lexeme.clone();

  // Collect assigned fields (pattern: `name:`)
  let mut assigned_fields = Vec::new();
  let mut i = open_brace_idx + 1;
  while i < tokens.len() && tokens[i].span.start.0 < cursor_offset {
    let tok = tokens[i];
    if tok.type_ == TokenType::RightBrace {
      break;
    }
    if tok.type_ == TokenType::Identifier && i + 1 < tokens.len() && tokens[i + 1].type_ == TokenType::Colon {
      assigned_fields.push(tok.lexeme.clone());
    }
    i += 1;
  }

  let prefix = extract_prefix_at_cursor(current_token, cursor_offset);

  Some(CompletionContext::RecordInit {
    record_name,
    assigned_fields,
    prefix,
  })
}

/// Detect `@` context for builtin/directive completion.
///
/// Handles:
/// - `@|` — cursor right after `@`
/// - `@pa|` — cursor typing identifier after `@`
/// - `@[fo|` or `@[foo, ba|` — cursor inside attribute list
fn detect_at_context(
  prev_token: Option<&Token>,
  current_token: Option<&Token>,
  meaningful: &[&Token],
  cursor_offset: u32,
  source_text: &str,
) -> Option<CompletionContext> {
  if let Some(prev) = prev_token
    && prev.type_ == TokenType::At
  {
    let prefix = extract_prefix_at_cursor(current_token, cursor_offset);
    return Some(CompletionContext::AfterAt { prefix });
  }

  // Identifier at cursor preceded by `@`
  if let Some(curr) = current_token
    && curr.type_ == TokenType::Identifier
  {
    let at_before = find_token_before_offset(meaningful, curr.span.start.0);
    if let Some(at_tok) = at_before
      && at_tok.type_ == TokenType::At
    {
      let prefix_end = cursor_offset.min(curr.span.end.0);
      let prefix_start = curr.span.start.0;
      let prefix = source_text
        .get(prefix_start as usize..prefix_end as usize)
        .unwrap_or("")
        .to_string();
      return Some(CompletionContext::AfterAt { prefix });
    }
  }

  // Identifier ending at cursor — check what precedes it
  if let Some(prev) = prev_token
    && prev.type_ == TokenType::Identifier
    && prev.span.end.0 == cursor_offset
  {
    let before_ident = find_token_before_offset(meaningful, prev.span.start.0);
    if let Some(before) = before_ident {
      if before.type_ == TokenType::At {
        let prefix = prev.lexeme.clone();
        return Some(CompletionContext::AfterAt { prefix });
      }

      if (before.type_ == TokenType::LeftBrack || before.type_ == TokenType::Comma)
        && let Some(ctx) = check_at_bracket_list(meaningful, before, &prev.lexeme)
      {
        return Some(ctx);
      }
    }
  }

  // Inside `@[...]` with no identifier started yet
  if let Some(prev) = prev_token
    && (prev.type_ == TokenType::LeftBrack || prev.type_ == TokenType::Comma)
    && let Some(ctx) = check_at_bracket_list(meaningful, prev, &extract_prefix_at_cursor(current_token, cursor_offset))
  {
    return Some(ctx);
  }

  None
}

/// Check if a `[` or `,` token is part of an `@[...]` attribute list.
///
/// Returns `AfterAt` context with the given prefix if the bracket/comma
/// is preceded by the `@[` pattern.
fn check_at_bracket_list(
  meaningful: &[&Token],
  bracket_or_comma: &Token,
  prefix: &str,
) -> Option<CompletionContext> {
  let trigger_idx = meaningful.iter().position(|t| std::ptr::eq(*t, bracket_or_comma))?;

  if bracket_or_comma.type_ == TokenType::LeftBrack
    && trigger_idx > 0
    && meaningful[trigger_idx - 1].type_ == TokenType::At
  {
    return Some(CompletionContext::AfterAt {
      prefix: prefix.to_string(),
    });
  }

  if bracket_or_comma.type_ == TokenType::Comma {
    let mut depth = 0i32;
    for i in (0..trigger_idx).rev() {
      match meaningful[i].type_ {
        TokenType::RightBrack => depth += 1,
        TokenType::LeftBrack => {
          if depth == 0 {
            if i > 0 && meaningful[i - 1].type_ == TokenType::At {
              return Some(CompletionContext::AfterAt {
                prefix: prefix.to_string(),
              });
            }
            break;
          }
          depth -= 1;
        },
        _ => {},
      }
    }
  }

  None
}

/// Build completion candidates for `@`-items (builtins and directives).
pub fn complete_at_items(prefix: &str) -> Vec<CompletionCandidate> {
  use crate::at_items;

  let matches = at_items::completions_matching(prefix);
  let mut candidates = Vec::new();

  for item in matches {
    let (kind, detail_prefix) = match item.kind {
      at_items::AtItemKind::Builtin => (CompletionKind::Function, "builtin"),
      at_items::AtItemKind::Directive => (CompletionKind::Keyword, "directive"),
    };

    let (insert_text, insert_format) = match at_items::snippet_for(item) {
      Some(snippet) => (Some(snippet), Some(InsertTextFormat::SNIPPET)),
      None => (Some(item.name.to_string()), None),
    };

    candidates.push(CompletionCandidate {
      label: item.name.to_string(),
      kind,
      detail: Some(format!("({}) {}", detail_prefix, item.summary)),
      documentation: item.doc.map(|d| d.to_string()),
      insert_text,
      insert_text_format: insert_format,
      sort_priority: 15,
    });
  }

  candidates
}

/// Complete unassigned fields in a record initializer.
pub fn complete_record_init(
  record_name: &str,
  assigned_fields: &[String],
  prefix: &str,
  output: &AnalyzeProjectOutput,
  file_id: &FileId,
) -> Vec<CompletionCandidate> {
  let mut candidates = Vec::new();

  let Some(def_id) = output
    .defs
    .iter()
    .find(|(_, def)| {
      matches!(&def.kind, DefinitionKind::Record(_))
        && output.symbol_names.get(&def.name).is_some_and(|n| n == record_name)
        && is_visible(def, file_id)
    })
    .map(|(id, _)| id)
  else {
    return candidates;
  };

  let def = output.defs.get(&def_id);
  let DefinitionKind::Record(record_def) = &def.kind else {
    return candidates;
  };

  for field in &record_def.fields {
    let Some(field_name) = output.symbol_names.get(&field.name) else {
      continue;
    };

    if assigned_fields.contains(field_name) {
      continue;
    }

    if !matches_prefix(field_name, prefix) {
      continue;
    }

    let field_def = output.defs.get(&field.def_id);
    if !is_visible(field_def, file_id) {
      continue;
    }

    let type_str = format_type_brief(&output.types, &output.defs, &output.symbol_names, &field.type_id);
    candidates.push(CompletionCandidate {
      label: field_name.clone(),
      kind: CompletionKind::Field,
      detail: Some(type_str),
      documentation: field_def.doc.clone(),
      insert_text: Some(format!("{}: $0", field_name)),
      insert_text_format: Some(InsertTextFormat::SNIPPET),
      sort_priority: 10,
    });
  }

  candidates
}

/// Complete members after `.` (instance access).
///
/// This function uses node_spans when available, and falls back to token-based
/// lookup when node_spans is empty (e.g., due to parse errors).
pub fn complete_dot(
  dot_offset: u32,
  prefix: &str,
  output: &AnalyzeProjectOutput,
  file_id: &FileId,
  tokens: &[Token],
  _source_text: &str,
) -> Vec<CompletionCandidate> {
  let mut candidates = Vec::new();

  // Try token-based lookup first (more reliable when code has been edited)
  // This looks up the identifier before the dot by name, not position
  let type_id = if !tokens.is_empty() {
    find_receiver_type_by_token(dot_offset, tokens, output, file_id)
  } else {
    None
  };

  // Fall back to span-based lookup if token-based didn't work
  let type_id = type_id.or_else(|| find_receiver_type_by_span(dot_offset, output, file_id));

  let Some(type_id) = type_id else {
    return candidates;
  };

  // Get members based on type
  add_instance_members(
    &type_id,
    prefix,
    &output.types,
    &output.defs,
    &output.symbol_names,
    file_id,
    &mut candidates,
  );

  // Add extension methods for this type
  add_extension_methods(
    &type_id,
    prefix,
    &output.types,
    &output.defs,
    &output.symbol_names,
    &output.extension_methods,
    &mut candidates,
  );

  candidates
}

/// Find receiver type by looking up node_spans.
fn find_receiver_type_by_span(
  dot_offset: u32,
  output: &AnalyzeProjectOutput,
  file_id: &FileId,
) -> Option<TypeId> {
  let file_analysis = output.file_analysis(file_id)?;

  if file_analysis.node_spans.is_empty() {
    return None;
  }

  // Find receiver node: span.end <= dot_offset, pick max by (end, -span_size)
  let receiver_node = file_analysis
    .node_spans
    .iter()
    .filter(|(_, span)| span.end.0 <= dot_offset)
    .max_by(|(_, a), (_, b)| {
      a.end
        .0
        .cmp(&b.end.0)
        .then_with(|| (b.end.0 - b.start.0).cmp(&(a.end.0 - a.start.0)))
    });

  let (node_id, _receiver_span) = receiver_node?;

  // Get type of receiver
  let type_id = file_analysis.node_types.get(node_id)?;

  Some(*type_id)
}

/// Find receiver type by looking up the identifier token before the dot.
fn find_receiver_type_by_token(
  dot_offset: u32,
  tokens: &[Token],
  output: &AnalyzeProjectOutput,
  file_id: &FileId,
) -> Option<TypeId> {
  // Find the identifier token that ends right before the dot
  let receiver_token = tokens
    .iter()
    .filter(|t| t.type_ == TokenType::Identifier && t.span.end.0 <= dot_offset)
    .max_by_key(|t| t.span.end.0)?;

  // Check that the token ends right at the dot position
  if receiver_token.span.end.0 != dot_offset {
    return None;
  }

  let var_name = &receiver_token.lexeme;

  // Find the symbol ID for this name
  let symbol_id = output
    .symbol_names
    .iter()
    .find(|(_, name)| *name == var_name)
    .map(|(id, _)| *id)?;

  // Find variable/parameter in current file
  for (_def_id, def) in output.defs.iter() {
    if def.name != symbol_id {
      continue;
    }

    if def.span.file != *file_id {
      continue;
    }

    match &def.kind {
      DefinitionKind::Variable(var_def) => {
        return Some(var_def.type_id);
      },
      DefinitionKind::Parameter(param_def) => {
        return Some(param_def.type_id);
      },
      _ => continue,
    }
  }

  None
}

/// Add instance members (fields and instance methods) for a type.
fn add_instance_members(
  type_id: &TypeId,
  prefix: &str,
  types: &TypeStore,
  defs: &DefinitionStore,
  symbol_names: &HashMap<SymbolId, String>,
  current_file: &FileId,
  candidates: &mut Vec<CompletionCandidate>,
) {
  let ty = types.get(type_id);

  // Get the definition ID for Record/Enum types
  let def_id = match ty {
    Type::Record(id) => Some(id),
    Type::Enum(id) => Some(id),
    Type::Instance { generic, .. } => Some(generic),
    Type::Reference { inner, .. } | Type::Pointer { inner, .. } => {
      // Dereference and recurse
      add_instance_members(inner, prefix, types, defs, symbol_names, current_file, candidates);
      return;
    },
    _ => None,
  };

  let Some(def_id) = def_id else {
    return;
  };

  let def = defs.get(def_id);

  match &def.kind {
    DefinitionKind::Record(record_def) => {
      // Add fields
      for field in &record_def.fields {
        let Some(name) = symbol_names.get(&field.name) else {
          continue;
        };

        if !matches_prefix(name, prefix) {
          continue;
        }

        let field_def = defs.get(&field.def_id);
        if !is_visible(field_def, current_file) {
          continue;
        }

        let detail = format_type_brief(types, defs, symbol_names, &field.type_id);

        candidates.push(CompletionCandidate {
          label: name.clone(),
          kind: CompletionKind::Field,
          detail: Some(detail),
          documentation: None,
          insert_text: None,
          insert_text_format: None,
          sort_priority: 10,
        });
      }

      // Add instance methods
      for (sym_id, entry) in &record_def.instance_methods {
        add_method_candidates(
          sym_id,
          entry,
          prefix,
          defs,
          symbol_names,
          types,
          current_file,
          false,
          candidates,
        );
      }
    },

    DefinitionKind::Enum(enum_def) => {
      for (sym_id, entry) in &enum_def.instance_methods {
        add_method_candidates(
          sym_id,
          entry,
          prefix,
          defs,
          symbol_names,
          types,
          current_file,
          false,
          candidates,
        );
      }
    },

    _ => {},
  }
}

/// Add extension methods for a type (e.g., `toString` on primitives).
fn add_extension_methods(
  type_id: &TypeId,
  prefix: &str,
  types: &TypeStore,
  defs: &DefinitionStore,
  symbol_names: &HashMap<SymbolId, String>,
  extension_methods: &HashMap<TypeId, HashMap<SymbolId, Vec<DefinitionId>>>,
  candidates: &mut Vec<CompletionCandidate>,
) {
  // Dereference through references/pointers
  let ty = types.get(type_id);
  let lookup_id = match ty {
    Type::Reference { inner, .. } | Type::Pointer { inner, .. } => inner,
    _ => type_id,
  };

  let Some(methods_by_name) = extension_methods.get(lookup_id) else {
    return;
  };

  for (sym_id, def_ids) in methods_by_name {
    let Some(name) = symbol_names.get(sym_id) else {
      continue;
    };

    if !matches_prefix(name, prefix) {
      continue;
    }

    for def_id in def_ids {
      let def = defs.get(def_id);

      let detail = if let DefinitionKind::Function(func_def) = &def.kind {
        let params: Vec<String> = func_def
          .params
          .iter()
          .skip(1) // skip the `self` parameter
          .filter_map(|p_id| {
            let p_def = defs.get(p_id);
            let p_name = symbol_names.get(&p_def.name)?;
            if let DefinitionKind::Parameter(param) = &p_def.kind {
              Some(format!(
                "{}: {}",
                p_name,
                format_type_brief(types, defs, symbol_names, &param.type_id)
              ))
            } else {
              None
            }
          })
          .collect();

        let ret = format_type_brief(types, defs, symbol_names, &func_def.return_type);
        Some(format!("fn({}) -> {}", params.join(", "), ret))
      } else {
        None
      };

      candidates.push(CompletionCandidate {
        label: name.clone(),
        kind: CompletionKind::Method,
        detail,
        documentation: def.doc.clone(),
        insert_text: Some(format!("{}($0)", name)),
        insert_text_format: Some(InsertTextFormat::SNIPPET),
        sort_priority: 15,
      });
    }
  }
}

/// Complete members after `::` (static access or namespace).
pub fn complete_double_colon(
  path_segments: &[String],
  prefix: &str,
  output: &AnalyzeProjectOutput,
  file_id: &FileId,
) -> Vec<CompletionCandidate> {
  let mut candidates = Vec::new();

  if path_segments.is_empty() {
    return candidates;
  }

  // Resolve the path to a definition
  let target_def_id = resolve_path_to_def(path_segments, output, file_id);

  let Some(def_id) = target_def_id else {
    // Try resolving as namespace
    if let Some(ns_id) = resolve_path_to_namespace(path_segments, output) {
      add_namespace_members(&ns_id, prefix, output, file_id, &mut candidates);
    }
    return candidates;
  };

  let def = output.defs.get(&def_id);

  match &def.kind {
    DefinitionKind::Record(record_def) => {
      // Static methods
      for (sym_id, entry) in &record_def.static_methods {
        add_method_candidates(
          sym_id,
          entry,
          prefix,
          &output.defs,
          &output.symbol_names,
          &output.types,
          file_id,
          true,
          &mut candidates,
        );
      }

      // Static fields (constants)
      for (sym_id, const_def_id) in &record_def.static_fields {
        let Some(name) = output.symbol_names.get(sym_id) else {
          continue;
        };

        if !matches_prefix(name, prefix) {
          continue;
        }

        let const_def = output.defs.get(const_def_id);
        if !is_visible(const_def, file_id) {
          continue;
        }

        candidates.push(CompletionCandidate {
          label: name.clone(),
          kind: CompletionKind::Constant,
          detail: None,
          documentation: None,
          insert_text: None,
          insert_text_format: None,
          sort_priority: 20,
        });
      }
    },

    DefinitionKind::Enum(enum_def) => {
      // Variants
      for variant in &enum_def.variants {
        let Some(name) = output.symbol_names.get(&variant.name) else {
          continue;
        };

        if !matches_prefix(name, prefix) {
          continue;
        }

        let detail = if variant.payload.is_empty() {
          None
        } else {
          let payload: Vec<String> = variant
            .payload
            .iter()
            .map(|t| format_type_brief(&output.types, &output.defs, &output.symbol_names, t))
            .collect();
          Some(format!("({})", payload.join(", ")))
        };

        let insert_text = if variant.payload.is_empty() {
          None
        } else {
          Some(format!("{}($0)", name))
        };

        let insert_format = if variant.payload.is_empty() {
          None
        } else {
          Some(InsertTextFormat::SNIPPET)
        };

        candidates.push(CompletionCandidate {
          label: name.clone(),
          kind: CompletionKind::EnumVariant,
          detail,
          documentation: None,
          insert_text,
          insert_text_format: insert_format,
          sort_priority: 10,
        });
      }

      // Static methods
      for (sym_id, entry) in &enum_def.static_methods {
        add_method_candidates(
          sym_id,
          entry,
          prefix,
          &output.defs,
          &output.symbol_names,
          &output.types,
          file_id,
          true,
          &mut candidates,
        );
      }

      // Static fields
      for (sym_id, const_def_id) in &enum_def.static_fields {
        let Some(name) = output.symbol_names.get(sym_id) else {
          continue;
        };

        if !matches_prefix(name, prefix) {
          continue;
        }

        let const_def = output.defs.get(const_def_id);
        if !is_visible(const_def, file_id) {
          continue;
        }

        candidates.push(CompletionCandidate {
          label: name.clone(),
          kind: CompletionKind::Constant,
          detail: None,
          documentation: None,
          insert_text: None,
          insert_text_format: None,
          sort_priority: 20,
        });
      }
    },

    DefinitionKind::Namespace(ns_def) => {
      add_namespace_members(&ns_def.namespace_id, prefix, output, file_id, &mut candidates);
    },

    _ => {},
  }

  candidates
}

/// Complete identifier (not after `.` or `::`).
pub fn complete_identifier(
  prefix: &str,
  start_offset: u32,
  tokens: &[Token],
  output: Option<&AnalyzeProjectOutput>,
  file_id: &FileId,
) -> Vec<CompletionCandidate> {
  let mut candidates = Vec::new();

  // Check if next token is `(` to avoid inserting `foo(()` for functions
  let next_is_paren = is_next_token_paren(tokens, start_offset);

  let (scope, local_scope_start) = analyze_completion_scope(tokens, start_offset);

  let mut seen_names = HashSet::new();

  if let Some(output) = output {
    let file_analysis = output.file_analysis(file_id);
    let mut visible_defs: HashMap<DefinitionId, u8> = HashMap::new();

    let mut register_visible_def = |def_id: DefinitionId, priority: u8| {
      visible_defs
        .entry(def_id)
        .and_modify(|existing| {
          if priority < *existing {
            *existing = priority;
          }
        })
        .or_insert(priority);
    };

    // Current-file symbols (excluding lexical locals).
    for (def_id, def) in output.defs.iter() {
      if def.span.file != *file_id {
        continue;
      }

      if !include_in_file_identifier_completion(def) {
        continue;
      }

      register_visible_def(def_id, 20);
    }

    // Explicitly imported symbols in this file.
    if let Some(file_analysis) = file_analysis {
      let import_item_spans = collect_explicit_import_item_spans(file_analysis);

      for (span, def_id) in &file_analysis.import_item_defs {
        if !import_item_spans.contains(span) {
          continue;
        }

        register_visible_def(*def_id, 30);
      }
    }

    // Public type-like symbols from other files (e.g. Option/Result).
    if !prefix.is_empty() {
      for (def_id, def) in output.defs.iter() {
        if def.span.file == *file_id {
          continue;
        }

        if !include_cross_file_identifier_completion(def) {
          continue;
        }

        if !is_visible(def, file_id) {
          continue;
        }

        register_visible_def(def_id, 40);
      }
    }

    let mut visible_defs: Vec<(DefinitionId, u8)> = visible_defs.into_iter().collect();
    visible_defs.sort_by(|(left_id, left_priority), (right_id, right_priority)| {
      left_priority.cmp(right_priority).then_with(|| {
        let left_name = output
          .symbol_names
          .get(&output.defs.get(left_id).name)
          .map(String::as_str)
          .unwrap_or_default();
        let right_name = output
          .symbol_names
          .get(&output.defs.get(right_id).name)
          .map(String::as_str)
          .unwrap_or_default();

        left_name.cmp(right_name)
      })
    });

    for (def_id, source_priority) in visible_defs {
      let def = output.defs.get(&def_id);

      // Skip internal definitions
      match &def.kind {
        DefinitionKind::TypeParam(_)
        | DefinitionKind::Field(_)
        | DefinitionKind::Variant(_)
        | DefinitionKind::Method(_)
        | DefinitionKind::Placeholder => continue,
        _ => {},
      }

      let Some(name) = output.symbol_names.get(&def.name) else {
        continue;
      };

      if !matches_prefix(name, prefix) {
        continue;
      }

      if !is_visible(def, file_id) {
        continue;
      }

      let (kind, detail, insert_text, insert_format) =
        def_to_completion_info(def, name, &output.types, &output.defs, &output.symbol_names, next_is_paren);

      seen_names.insert(name.clone());

      candidates.push(CompletionCandidate {
        label: name.clone(),
        kind,
        detail,
        documentation: def.doc.clone(),
        insert_text,
        insert_text_format: insert_format,
        sort_priority: source_priority,
      });
    }
  }

  if scope == CompletionScope::CallableBody {
    add_heuristic_locals(tokens, start_offset, prefix, local_scope_start, &seen_names, &mut candidates);
  }

  add_at_item_completions(prefix, scope, &mut candidates);

  // Add keywords
  add_snippet_completions(tokens, start_offset, prefix, scope, &mut candidates);

  candidates
}

fn include_in_file_identifier_completion(def: &Definition) -> bool {
  if def.owner_namespace.is_some() {
    return false;
  }

  matches!(
    &def.kind,
    DefinitionKind::Function(_)
      | DefinitionKind::Constant(_)
      | DefinitionKind::Record(_)
      | DefinitionKind::Enum(_)
      | DefinitionKind::Namespace(_)
      | DefinitionKind::TypeAlias(_)
  )
}

fn include_cross_file_identifier_completion(def: &Definition) -> bool {
  if def.owner_namespace.is_some() {
    return false;
  }

  matches!(
    &def.kind,
    DefinitionKind::Record(_) | DefinitionKind::Enum(_) | DefinitionKind::Namespace(_) | DefinitionKind::TypeAlias(_)
  )
}

fn collect_explicit_import_item_spans(file: &ignis_driver::PerFileAnalysis) -> HashSet<Span> {
  let mut spans = HashSet::new();
  let mut stack = file.roots.clone();

  while let Some(node_id) = stack.pop() {
    match file.nodes.get(&node_id) {
      ASTNode::Statement(ASTStatement::Import(import_stmt)) => {
        for item in &import_stmt.items {
          spans.insert(item.span.clone());
        }
      },
      ASTNode::Statement(ASTStatement::Namespace(ns)) => {
        stack.extend(ns.items.iter().copied());
      },
      ASTNode::Statement(ASTStatement::Export(ASTExport::Declaration { decl, .. })) => {
        stack.push(*decl);
      },
      _ => {},
    }
  }

  spans
}

fn add_at_item_completions(
  prefix: &str,
  scope: CompletionScope,
  candidates: &mut Vec<CompletionCandidate>,
) {
  let wanted_kind = match scope {
    CompletionScope::CallableBody => AtItemKind::Builtin,
    CompletionScope::Global => AtItemKind::Directive,
    CompletionScope::TypeBody => return,
  };

  for item in crate::at_items::completions_matching(prefix) {
    if item.kind != wanted_kind {
      continue;
    }

    match item.kind {
      AtItemKind::Builtin => {
        let (insert_text, insert_text_format) = if let Some(snippet) = crate::at_items::snippet_for(item) {
          (Some(format!("@{}", snippet)), Some(InsertTextFormat::SNIPPET))
        } else {
          (Some(format!("@{}", item.name)), None)
        };

        candidates.push(CompletionCandidate {
          label: item.name.to_string(),
          kind: CompletionKind::Function,
          detail: Some("builtin".to_string()),
          documentation: Some(crate::at_items::format_hover(item)),
          insert_text,
          insert_text_format,
          sort_priority: 35,
        });
      },
      AtItemKind::Directive => {
        let (insert_text, insert_text_format) = directive_insert_text(item.name);

        candidates.push(CompletionCandidate {
          label: item.name.to_string(),
          kind: CompletionKind::Keyword,
          detail: Some("directive".to_string()),
          documentation: Some(crate::at_items::format_hover(item)),
          insert_text,
          insert_text_format,
          sort_priority: 45,
        });
      },
    }
  }
}

fn directive_insert_text(name: &str) -> (Option<String>, Option<InsertTextFormat>) {
  let plain = || (Some(format!("@{}", name)), None);

  let snippet = match name {
    "aligned" => "@aligned(${1:N})",
    "implements" => "@implements(${1:Trait})",
    "externName" => "@externName(\"${1:symbol}\")",
    "deprecated" => "@deprecated(${1:\"message\"})",
    "extension" => "@extension(${1:TypeName})",
    "inline" => "@inline(${1:always})",
    "takes" => "@takes",
    "allow" => "@allow(${1:lint_name})",
    "warn" => "@warn(${1:lint_name})",
    "deny" => "@deny(${1:lint_name})",
    "langHook" => "@langHook(\"${1:hook_name}\")",
    "packed" | "cold" => return plain(),
    _ => return plain(),
  };

  (Some(snippet.to_string()), Some(InsertTextFormat::SNIPPET))
}

fn analyze_completion_scope(
  tokens: &[Token],
  cursor_offset: u32,
) -> (CompletionScope, Option<u32>) {
  let meaningful: Vec<&Token> = tokens
    .iter()
    .filter(|token| {
      !matches!(
        token.type_,
        TokenType::Whitespace | TokenType::Comment | TokenType::MultiLineComment | TokenType::DocComment
      )
    })
    .collect();

  let mut stack: Vec<ScopeFrame> = Vec::new();

  for (index, token) in meaningful.iter().enumerate() {
    if token.span.start.0 >= cursor_offset {
      break;
    }

    match token.type_ {
      TokenType::LeftBrace => {
        let parent_scope = stack.last().map(|frame| frame.scope);
        let scope = classify_left_brace_scope(&meaningful, index, parent_scope);
        stack.push(ScopeFrame {
          scope,
          start: token.span.start.0,
        });
      },
      TokenType::RightBrace => {
        stack.pop();
      },
      _ => {},
    }
  }

  let scope = stack.last().map(|frame| frame.scope).unwrap_or(CompletionScope::Global);

  let local_scope_start = if scope == CompletionScope::CallableBody {
    stack
      .iter()
      .rfind(|frame| frame.scope == CompletionScope::CallableBody)
      .map(|frame| frame.start)
  } else {
    None
  };

  (scope, local_scope_start)
}

fn classify_left_brace_scope(
  tokens: &[&Token],
  brace_index: usize,
  parent_scope: Option<CompletionScope>,
) -> CompletionScope {
  if parent_scope == Some(CompletionScope::CallableBody) {
    return CompletionScope::CallableBody;
  }

  let mut segment_start = 0usize;
  for index in (0..brace_index).rev() {
    if matches!(
      tokens[index].type_,
      TokenType::SemiColon | TokenType::LeftBrace | TokenType::RightBrace
    ) {
      segment_start = index + 1;
      break;
    }
  }

  let segment = &tokens[segment_start..brace_index];

  let has_type_keyword = segment
    .iter()
    .any(|token| matches!(token.type_, TokenType::Record | TokenType::Enum));
  if has_type_keyword {
    return CompletionScope::TypeBody;
  }

  let has_function_keyword = segment.iter().any(|token| token.type_ == TokenType::Function);
  if has_function_keyword {
    return CompletionScope::CallableBody;
  }

  let has_control_keyword = segment.iter().any(|token| {
    matches!(
      token.type_,
      TokenType::If | TokenType::While | TokenType::For | TokenType::Else | TokenType::Match
    )
  });
  if has_control_keyword {
    return parent_scope.unwrap_or(CompletionScope::Global);
  }

  let has_right_paren = segment.iter().any(|token| token.type_ == TokenType::RightParen);
  let has_colon = segment.iter().any(|token| token.type_ == TokenType::Colon);
  if has_right_paren && has_colon {
    return CompletionScope::CallableBody;
  }

  parent_scope.unwrap_or(CompletionScope::Global)
}

/// Find the previous meaningful token before the cursor.
/// Skips whitespace, comments, and the identifier currently being typed.
fn find_prev_meaningful_token(
  tokens: &[Token],
  start_offset: u32,
) -> Option<&Token> {
  // Find token index at start_offset
  let cursor_idx = tokens
    .iter()
    .position(|t| t.span.start.0 >= start_offset)
    .unwrap_or(tokens.len());

  // Scan backwards
  for i in (0..cursor_idx).rev() {
    let tok = &tokens[i];

    // Skip the token we are currently typing (if any)
    if tok.span.start.0 == start_offset {
      continue;
    }

    match tok.type_ {
      TokenType::Whitespace | TokenType::Comment | TokenType::MultiLineComment | TokenType::DocComment => continue,
      _ => return Some(tok),
    }
  }

  None
}

/// Add contextual snippet completions (replacing raw keywords).
fn add_snippet_completions(
  tokens: &[Token],
  start_offset: u32,
  prefix: &str,
  scope: CompletionScope,
  candidates: &mut Vec<CompletionCandidate>,
) {
  let prev_token = find_prev_meaningful_token(tokens, start_offset);

  // Check context
  let is_start_of_stmt = match prev_token {
    None => true, // Start of file
    Some(t) => matches!(
      t.type_,
      TokenType::SemiColon
        | TokenType::LeftBrace
        | TokenType::RightBrace
        | TokenType::LeftParen
        | TokenType::RightParen // e.g. if (...) {
        | TokenType::LeftBrack
        | TokenType::RightBrack
        | TokenType::Comma
        | TokenType::Equal
        | TokenType::Arrow // match arm
    ),
  };

  let is_after_right_brace = match prev_token {
    Some(t) => t.type_ == TokenType::RightBrace,
    None => false,
  };

  // Helper to add a snippet
  let mut add = |label: &str, detail: &str, snippet: &str, priority: u8| {
    if matches_prefix(label, prefix) {
      candidates.push(CompletionCandidate {
        label: label.to_string(),
        kind: CompletionKind::Keyword,
        detail: Some(detail.to_string()),
        documentation: None,
        insert_text: Some(snippet.to_string()),
        insert_text_format: Some(InsertTextFormat::SNIPPET),
        sort_priority: priority,
      });
    }
  };

  // Restrict snippet triggers to empty or exact keyword match to avoid noise
  let is_alphabetic = prefix.chars().next().is_some_and(|c| c.is_alphabetic());
  let exact_keyword = matches!(
    prefix,
    "if"
      | "else"
      | "while"
      | "for"
      | "match"
      | "return"
      | "let"
      | "const"
      | "function"
      | "record"
      | "enum"
      | "namespace"
      | "type"
      | "import"
      | "from"
      | "public"
      | "private"
      | "static"
      | "extern"
      | "mut"
  );
  let wants_snippets = prefix.is_empty() || !is_alphabetic || exact_keyword;

  // Priority 80 for snippets (lower than locals/defs/imports), 60 for literals
  let snip_prio = 80;
  let lit_prio = 60;

  if is_start_of_stmt && wants_snippets {
    match scope {
      CompletionScope::CallableBody => {
        add("if", "if (...) { ... }", "if ($1) {\n\t$0\n}", snip_prio);
        add(
          "if else",
          "if (...) { ... } else { ... }",
          "if ($1) {\n\t$2\n} else {\n\t$0\n}",
          snip_prio,
        );
        add("while", "while (...) { ... }", "while ($1) {\n\t$0\n}", snip_prio);
        add("for", "for { ... }", "for (let $1 = $2; $3; $4) {\n\t$0\n}", snip_prio);
        add("for of", "for { ... }", "for (let $1 of $2) {\n\t$0\n}", snip_prio);
        add("match", "match { ... }", "match ($1) {\n\t$0\n}", snip_prio);
        add("return", "return val", "return $0;", snip_prio);
        add("let", "let var: type = ...", "let $1: $2 = $0;", snip_prio);
        add("const", "const var: type = ...", "const $1: $2 = $0;", snip_prio);
      },
      CompletionScope::TypeBody => {
        add("public", "public modifier", "public ", snip_prio);
        add("private", "private modifier", "private ", snip_prio);
        add("static", "static modifier", "static ", snip_prio);
        add("inline", "inline modifier", "inline ", snip_prio);
      },
      CompletionScope::Global => {
        add("const", "const var: type = ...", "const $1: $2 = $0;", snip_prio);
        add(
          "function",
          "function name(): type { ... }",
          "function $1($2): $3 {\n\t$0\n}",
          snip_prio,
        );
        add("record", "record Name { ... }", "record $1 {\n\t$0\n}", snip_prio);
        add("enum", "enum Name { ... }", "enum $1 {\n\t$0\n}", snip_prio);
        add("namespace", "namespace Name { ... }", "namespace $1 {\n\t$0\n}", snip_prio);
        add("type", "type Alias = ...", "type $1 = $0;", snip_prio);
        add("extern", "extern path { ... }", "extern $1 {\n\t$0\n}", snip_prio);
        add("import", "import ... from ...", "import $0 from \"$1\"", snip_prio);
      },
    }
  }

  // Special case: else (only valid after '}')
  if scope == CompletionScope::CallableBody && is_after_right_brace && wants_snippets {
    add("else", "else { ... }", "else {\n\t$0\n}", snip_prio);
  }

  // Keywords valid in almost any expression context
  if scope == CompletionScope::CallableBody
    && (is_start_of_stmt
      || matches!(
        prev_token.map(|t| t.type_),
        Some(TokenType::Colon | TokenType::Equal | TokenType::LeftParen | TokenType::Comma)
      ))
  {
    add("true", "boolean true", "true", lit_prio);
    add("false", "boolean false", "false", lit_prio);
    add("null", "null value", "null", lit_prio);
    add("self", "self reference", "self", lit_prio);
  }
}

/// Scan tokens backwards to find local variable declarations.
/// Names in `already_seen` are skipped to avoid duplicates.
fn add_heuristic_locals(
  tokens: &[Token],
  cursor_offset: u32,
  prefix: &str,
  local_scope_start: Option<u32>,
  already_seen: &HashSet<String>,
  candidates: &mut Vec<CompletionCandidate>,
) {
  // Find token index at cursor
  let cursor_idx = tokens
    .iter()
    .position(|t| t.span.start.0 >= cursor_offset)
    .unwrap_or(tokens.len());

  let mut seen = HashSet::new();

  // Scan backwards from cursor
  for i in (0..cursor_idx).rev() {
    let tok = &tokens[i];

    if let Some(local_scope_start) = local_scope_start
      && tok.span.start.0 < local_scope_start
    {
      break;
    }

    // Look for: let/const <IDENT>
    if tok.type_ == TokenType::Identifier {
      let name = &tok.lexeme;

      if !matches_prefix(name, prefix) {
        continue;
      }

      // Skip names already added from the definition-based path
      if already_seen.contains(name) {
        continue;
      }

      // Check previous token
      if i > 0 {
        let prev = &tokens[i - 1];
        match prev.type_ {
          TokenType::Let | TokenType::Const => {
            if seen.insert(name.clone()) {
              candidates.push(CompletionCandidate {
                label: name.clone(),
                kind: CompletionKind::Variable,
                detail: Some("(local)".to_string()),
                documentation: None,
                insert_text: None,
                insert_text_format: None,
                sort_priority: 10, // Highest priority
              });
            }
          },
          _ => {},
        }
      }
    }

    // Stop scanning if we go too far back (e.g. 500 tokens) to avoid perf hit
    if cursor_idx - i > 500 {
      break;
    }
  }
}

pub fn complete_import_path(
  prefix: &str,
  config: &ignis_config::IgnisConfig,
) -> Vec<CompletionCandidate> {
  let mut candidates = Vec::new();

  // Suggest "std::" as a root module
  if prefix.is_empty() || "std".starts_with(prefix) {
    candidates.push(CompletionCandidate {
      label: "std::".to_string(),
      kind: CompletionKind::Module,
      detail: Some("Standard library".to_string()),
      documentation: None,
      insert_text: Some("std::".to_string()),
      insert_text_format: None,
      sort_priority: 10,
    });
  }

  if let Some(subprefix) = prefix.strip_prefix("std::") {
    let mut modules: Vec<_> = config.manifest.modules.keys().collect();
    modules.sort();

    for name in modules {
      if name.starts_with(subprefix) {
        candidates.push(CompletionCandidate {
          label: format!("std::{}", name),
          kind: CompletionKind::Module,
          detail: Some(format!("Module: {}", name)),
          documentation: None,
          // Only insert the module name, not the full path (user already typed "std::")
          insert_text: Some(name.clone()),
          insert_text_format: None,
          sort_priority: 10,
        });
      }
    }
  }

  candidates
}

/// Convert internal candidates to LSP CompletionItems.
pub fn to_completion_items(candidates: Vec<CompletionCandidate>) -> Vec<CompletionItem> {
  let mut unique_candidates: Vec<CompletionCandidate> = Vec::new();
  let mut keys: HashMap<(String, CompletionKind, Option<String>), usize> = HashMap::new();

  for candidate in candidates {
    let key = (candidate.label.clone(), candidate.kind, candidate.detail.clone());

    if let Some(index) = keys.get(&key) {
      let current = &mut unique_candidates[*index];

      if candidate.sort_priority < current.sort_priority {
        *current = candidate;
      }

      continue;
    }

    keys.insert(key, unique_candidates.len());
    unique_candidates.push(candidate);
  }

  let mut items: Vec<CompletionItem> = unique_candidates
    .into_iter()
    .enumerate()
    .map(|(i, c)| {
      let sort_text = Some(format!("{:02}{:04}", c.sort_priority, i));

      CompletionItem {
        label: c.label,
        kind: Some(c.kind.into()),
        detail: c.detail,
        documentation: c.documentation.map(|d| {
          tower_lsp::lsp_types::Documentation::MarkupContent(tower_lsp::lsp_types::MarkupContent {
            kind: tower_lsp::lsp_types::MarkupKind::Markdown,
            value: d,
          })
        }),
        insert_text: c.insert_text,
        insert_text_format: c.insert_text_format,
        sort_text,
        ..Default::default()
      }
    })
    .collect();

  items.sort_by(|a, b| a.sort_text.cmp(&b.sort_text));
  items
}

// --- Helper functions ---

/// Advance to the next valid UTF-8 char boundary at or after `offset`.
fn find_char_boundary(
  text: &str,
  offset: usize,
) -> usize {
  if offset >= text.len() {
    return text.len();
  }
  if text.is_char_boundary(offset) {
    return offset;
  }
  for i in 1..=3 {
    let next = offset + i;
    if next >= text.len() || text.is_char_boundary(next) {
      return next.min(text.len());
    }
  }
  text.len()
}

/// Check if a name matches the given prefix (case-insensitive).
fn matches_prefix(
  name: &str,
  prefix: &str,
) -> bool {
  if prefix.is_empty() {
    return true;
  }
  name.to_lowercase().starts_with(&prefix.to_lowercase())
}

/// Check if a definition is visible from the current file.
fn is_visible(
  def: &Definition,
  current_file: &FileId,
) -> bool {
  // Synthetic definitions (compiler builtins) are never visible to users
  if def.span.file == FileId::SYNTHETIC {
    return false;
  }

  def.span.file == *current_file || def.visibility == Visibility::Public
}

/// Resolve a path (like `["Math", "Vector"]`) to a DefinitionId.
fn resolve_path_to_def(
  path: &[String],
  output: &AnalyzeProjectOutput,
  current_file: &FileId,
) -> Option<DefinitionId> {
  if path.is_empty() {
    return None;
  }

  // Find the first segment as a definition
  let first_name = &path[0];
  let mut current_def_id: Option<DefinitionId> = resolve_imported_path_head(first_name, output, current_file);

  // Search for definition by name, prefer same file
  for (def_id, def) in output.defs.iter() {
    let Some(name) = output.symbol_names.get(&def.name) else {
      continue;
    };

    if name != first_name {
      continue;
    }

    // Skip non-type definitions for path resolution
    match &def.kind {
      DefinitionKind::Record(_)
      | DefinitionKind::Enum(_)
      | DefinitionKind::Namespace(_)
      | DefinitionKind::TypeAlias(_) => {},
      _ => continue,
    }

    // Allow extern namespaces to be resolved even if they are synthetic/hidden
    let is_extern_ns = matches!(&def.kind, DefinitionKind::Namespace(ns) if ns.is_extern);

    if !is_extern_ns && !is_visible(def, current_file) {
      continue;
    }

    // Prefer same file
    if def.span.file == *current_file {
      current_def_id = Some(def_id);
      break;
    } else if current_def_id.is_none() {
      current_def_id = Some(def_id);
    }
  }

  // Navigate remaining segments
  for segment in &path[1..] {
    let def_id = current_def_id?;

    let def = output.defs.get(&def_id);

    current_def_id = match &def.kind {
      DefinitionKind::Namespace(ns_def) => {
        let ns = output.namespaces.get(&ns_def.namespace_id);
        let mut next_def_id: Option<DefinitionId> = None;

        // Look for child namespace
        for (sym_id, child_ns_id) in &ns.children {
          if let Some(name) = output.symbol_names.get(sym_id)
            && name == segment
          {
            next_def_id = find_namespace_def_id(child_ns_id, output, current_file);
            break;
          }
        }

        if next_def_id.is_some() {
          next_def_id
        } else {
          // Look for definition in namespace
          for (sym_id, entry) in &ns.definitions {
            if let Some(name) = output.symbol_names.get(sym_id)
              && name == segment
            {
              next_def_id = match entry {
                SymbolEntry::Single(id) => Some(*id),
                SymbolEntry::Overload(ids) => ids.first().copied(),
              };
              break;
            }
          }

          next_def_id
        }
      },
      _ => None,
    };
  }

  current_def_id
}

fn resolve_imported_path_head(
  head: &str,
  output: &AnalyzeProjectOutput,
  current_file: &FileId,
) -> Option<DefinitionId> {
  let file_analysis = output.file_analysis(current_file)?;
  let import_item_spans = collect_explicit_import_item_spans(file_analysis);
  let source = &output.source_map.get(current_file).text;

  for (span, def_id) in &file_analysis.import_item_defs {
    if !import_item_spans.contains(span) {
      continue;
    }

    let start = span.start.0 as usize;
    let end = span.end.0 as usize;

    if start >= source.len() || end > source.len() || start >= end {
      continue;
    }

    let name = source[start..end].trim();
    if name == head {
      return Some(*def_id);
    }
  }

  None
}

fn find_namespace_def_id(
  namespace_id: &ignis_type::namespace::NamespaceId,
  output: &AnalyzeProjectOutput,
  current_file: &FileId,
) -> Option<DefinitionId> {
  let mut fallback: Option<DefinitionId> = None;

  for (def_id, def) in output.defs.iter() {
    let DefinitionKind::Namespace(ns_def) = &def.kind else {
      continue;
    };

    if ns_def.namespace_id != *namespace_id {
      continue;
    }

    if !ns_def.is_extern && !is_visible(def, current_file) {
      continue;
    }

    if def.span.file == *current_file {
      return Some(def_id);
    }

    if fallback.is_none() {
      fallback = Some(def_id);
    }
  }

  fallback
}

/// Resolve a path to a namespace ID.
fn resolve_path_to_namespace(
  path: &[String],
  output: &AnalyzeProjectOutput,
) -> Option<ignis_type::namespace::NamespaceId> {
  if path.is_empty() {
    return None;
  }

  // Fast path: resolve by symbol IDs if all segments are known.
  let mut sym_path = Vec::with_capacity(path.len());
  let mut all_segments_known = true;
  for segment in path {
    if let Some(sym_id) = output
      .symbol_names
      .iter()
      .find(|(_, n)| *n == segment)
      .map(|(id, _)| *id)
    {
      sym_path.push(sym_id);
    } else {
      all_segments_known = false;
      break;
    }
  }

  if all_segments_known && let Some(ns_id) = output.namespaces.lookup(&sym_path) {
    return Some(ns_id);
  }

  // Fallback: match namespace full paths by names.
  for (_, def) in output.defs.iter() {
    let DefinitionKind::Namespace(ns_def) = &def.kind else {
      continue;
    };

    let full_path = output.namespaces.full_path(ns_def.namespace_id);
    if full_path.len() != path.len() {
      continue;
    }

    let is_match = full_path
      .iter()
      .zip(path.iter())
      .all(|(sym_id, segment)| output.symbol_names.get(sym_id).is_some_and(|name| name == segment));

    if is_match {
      return Some(ns_def.namespace_id);
    }
  }

  None
}

/// Add members of a namespace to candidates.
fn add_namespace_members(
  ns_id: &ignis_type::namespace::NamespaceId,
  prefix: &str,
  output: &AnalyzeProjectOutput,
  current_file: &FileId,
  candidates: &mut Vec<CompletionCandidate>,
) {
  let ns = output.namespaces.get(ns_id);

  // Child namespaces
  for (sym_id, child_ns_id) in &ns.children {
    let Some(name) = output.symbol_names.get(sym_id) else {
      continue;
    };

    if !matches_prefix(name, prefix) {
      continue;
    }

    if find_namespace_def_id(child_ns_id, output, current_file).is_none() {
      continue;
    }

    candidates.push(CompletionCandidate {
      label: name.clone(),
      kind: CompletionKind::Namespace,
      detail: None,
      documentation: None,
      insert_text: None,
      insert_text_format: None,
      sort_priority: 10,
    });
  }

  // Definitions in namespace
  for (sym_id, entry) in &ns.definitions {
    let Some(name) = output.symbol_names.get(sym_id) else {
      continue;
    };

    if !matches_prefix(name, prefix) {
      continue;
    }

    let def_ids: Vec<DefinitionId> = match entry {
      SymbolEntry::Single(id) => vec![*id],
      SymbolEntry::Overload(ids) => ids.clone(),
    };

    for def_id in def_ids {
      let def = output.defs.get(&def_id);

      let (kind, detail, insert_text, insert_format) =
        def_to_completion_info(def, name, &output.types, &output.defs, &output.symbol_names, false);

      candidates.push(CompletionCandidate {
        label: name.clone(),
        kind,
        detail,
        documentation: def.doc.clone(),
        insert_text,
        insert_text_format: insert_format,
        sort_priority: 20,
      });
    }
  }
}

/// Add method candidates from a SymbolEntry.
fn add_method_candidates(
  sym_id: &SymbolId,
  entry: &SymbolEntry,
  prefix: &str,
  defs: &DefinitionStore,
  symbol_names: &HashMap<SymbolId, String>,
  types: &TypeStore,
  current_file: &FileId,
  is_static: bool,
  candidates: &mut Vec<CompletionCandidate>,
) {
  let Some(name) = symbol_names.get(sym_id) else {
    return;
  };

  if !matches_prefix(name, prefix) {
    return;
  }

  let def_ids: Vec<DefinitionId> = match entry {
    SymbolEntry::Single(id) => vec![*id],
    SymbolEntry::Overload(ids) => ids.clone(),
  };

  for def_id in def_ids {
    let def = defs.get(&def_id);

    if !is_visible(def, current_file) {
      continue;
    }

    let detail = if let DefinitionKind::Method(method_def) = &def.kind {
      let params: Vec<String> = method_def
        .params
        .iter()
        .filter_map(|p_id| {
          let p_def = defs.get(p_id);
          let p_name = symbol_names.get(&p_def.name)?;
          if let DefinitionKind::Parameter(param) = &p_def.kind {
            Some(format!(
              "{}: {}",
              p_name,
              format_type_brief(types, defs, symbol_names, &param.type_id)
            ))
          } else {
            None
          }
        })
        .collect();

      let self_param = if is_static {
        String::new()
      } else if method_def.self_mutable {
        "&mut self".to_string()
      } else {
        "&self".to_string()
      };

      let all_params = if self_param.is_empty() {
        params.join(", ")
      } else if params.is_empty() {
        self_param
      } else {
        format!("{}, {}", self_param, params.join(", "))
      };

      let ret = format_type_brief(types, defs, symbol_names, &method_def.return_type);
      Some(format!("fn({}) -> {}", all_params, ret))
    } else {
      None
    };

    candidates.push(CompletionCandidate {
      label: name.clone(),
      kind: CompletionKind::Method,
      detail,
      documentation: def.doc.clone(),
      insert_text: Some(format!("{}($0)", name)),
      insert_text_format: Some(InsertTextFormat::SNIPPET),
      sort_priority: 15,
    });
  }
}

/// Convert a definition to completion info.
fn def_to_completion_info(
  def: &Definition,
  name: &str,
  types: &TypeStore,
  defs: &DefinitionStore,
  symbol_names: &HashMap<SymbolId, String>,
  next_is_paren: bool,
) -> (CompletionKind, Option<String>, Option<String>, Option<InsertTextFormat>) {
  match &def.kind {
    DefinitionKind::Function(func_def) => {
      let mut param_details = Vec::new();
      let mut snippet_args = Vec::new();
      let mut idx = 1;

      for p_id in &func_def.params {
        let p_def = defs.get(p_id);
        if let Some(p_name) = symbol_names.get(&p_def.name)
          && let DefinitionKind::Parameter(param) = &p_def.kind
        {
          let type_str = format_type_brief(types, defs, symbol_names, &param.type_id);
          param_details.push(format!("{}: {}", p_name, type_str));
          snippet_args.push(format!("${{{}:{}}}", idx, p_name));
          idx += 1;
        }
      }

      let variadic = if func_def.is_variadic { ", ..." } else { "" };
      let ret = format_type_brief(types, defs, symbol_names, &func_def.return_type);
      let detail = Some(format!("function({}{}): {}", param_details.join(", "), variadic, ret));

      let (insert_text, insert_format) = if next_is_paren {
        (Some(name.to_string()), None)
      } else {
        let args_str = snippet_args.join(", ");
        let snippet = if snippet_args.is_empty() {
          format!("{}()", name)
        } else {
          format!("{}({})", name, args_str)
        };
        (Some(snippet), Some(InsertTextFormat::SNIPPET))
      };

      (CompletionKind::Function, detail, insert_text, insert_format)
    },

    DefinitionKind::Record(record_def) => {
      let type_params = if record_def.type_params.is_empty() {
        String::new()
      } else {
        let params: Vec<String> = record_def
          .type_params
          .iter()
          .filter_map(|p| symbol_names.get(&defs.get(p).name).cloned())
          .collect();
        format!("<{}>", params.join(", "))
      };
      let detail = Some(format!("record{}", type_params));
      (CompletionKind::Record, detail, None, None)
    },

    DefinitionKind::Enum(enum_def) => {
      let type_params = if enum_def.type_params.is_empty() {
        String::new()
      } else {
        let params: Vec<String> = enum_def
          .type_params
          .iter()
          .filter_map(|p| symbol_names.get(&defs.get(p).name).cloned())
          .collect();
        format!("<{}>", params.join(", "))
      };
      let detail = Some(format!("enum{}", type_params));
      (CompletionKind::Enum, detail, None, None)
    },

    DefinitionKind::Namespace(ns) => {
      let detail = if ns.is_extern { "extern" } else { "namespace" };
      (CompletionKind::Namespace, Some(detail.to_string()), None, None)
    },

    DefinitionKind::TypeAlias(alias_def) => {
      let target = format_type_brief(types, defs, symbol_names, &alias_def.target);
      let detail = Some(format!("type = {}", target));
      (CompletionKind::TypeAlias, detail, None, None)
    },

    DefinitionKind::Constant(const_def) => {
      let ty = format_type_brief(types, defs, symbol_names, &const_def.type_id);
      let detail = Some(format!("const: {}", ty));
      (CompletionKind::Constant, detail, None, None)
    },

    DefinitionKind::Variable(var_def) => {
      let ty = format_type_brief(types, defs, symbol_names, &var_def.type_id);
      (CompletionKind::Variable, Some(ty), None, None)
    },

    DefinitionKind::Parameter(param_def) => {
      let ty = format_type_brief(types, defs, symbol_names, &param_def.type_id);
      (CompletionKind::Variable, Some(ty), None, None)
    },

    _ => (CompletionKind::Variable, None, None, None),
  }
}

/// Check if the next token after the given offset is `(`.
fn is_next_token_paren(
  tokens: &[Token],
  start_offset: u32,
) -> bool {
  for tok in tokens {
    if tok.span.start.0 > start_offset {
      return tok.type_ == TokenType::LeftParen;
    }
  }
  false
}

/// Format a type briefly for completion detail.
fn format_type_brief(
  types: &TypeStore,
  defs: &DefinitionStore,
  symbol_names: &HashMap<SymbolId, String>,
  type_id: &TypeId,
) -> String {
  let ty = types.get(type_id);

  match ty {
    Type::I8 => "i8".to_string(),
    Type::I16 => "i16".to_string(),
    Type::I32 => "i32".to_string(),
    Type::I64 => "i64".to_string(),
    Type::U8 => "u8".to_string(),
    Type::U16 => "u16".to_string(),
    Type::U32 => "u32".to_string(),
    Type::U64 => "u64".to_string(),
    Type::F32 => "f32".to_string(),
    Type::F64 => "f64".to_string(),
    Type::Boolean => "bool".to_string(),
    Type::Char => "char".to_string(),
    Type::Str => "str".to_string(),
    Type::Atom => "atom".to_string(),
    Type::Void => "void".to_string(),
    Type::Never => "never".to_string(),
    Type::Infer => "infer".to_string(),
    Type::NullPtr => "null".to_string(),
    Type::Error => "error".to_string(),

    Type::Pointer { inner, mutable } => {
      let inner_str = format_type_brief(types, defs, symbol_names, inner);
      if *mutable {
        format!("*mut {}", inner_str)
      } else {
        format!("*{}", inner_str)
      }
    },

    Type::Reference { inner, mutable } => {
      let inner_str = format_type_brief(types, defs, symbol_names, inner);
      if *mutable {
        format!("&mut {}", inner_str)
      } else {
        format!("&{}", inner_str)
      }
    },

    Type::Vector { element, size } => {
      let elem_str = format_type_brief(types, defs, symbol_names, element);
      format!("{}[{}]", elem_str, size)
    },

    Type::Tuple(elements) => {
      let elem_strs: Vec<String> = elements
        .iter()
        .map(|e| format_type_brief(types, defs, symbol_names, e))
        .collect();
      format!("({})", elem_strs.join(", "))
    },

    Type::Function {
      params,
      ret,
      is_variadic,
    } => {
      let param_strs: Vec<String> = params
        .iter()
        .map(|p| format_type_brief(types, defs, symbol_names, p))
        .collect();
      let variadic = if *is_variadic { ", ..." } else { "" };
      let ret_str = format_type_brief(types, defs, symbol_names, ret);
      format!("function({}{}): {}", param_strs.join(", "), variadic, ret_str)
    },

    Type::Record(def_id) | Type::Enum(def_id) => symbol_names
      .get(&defs.get(def_id).name)
      .cloned()
      .unwrap_or_else(|| "?".to_string()),

    Type::Param { index, .. } => format!("T{}", index),

    Type::Instance { generic, args } => {
      let base = symbol_names
        .get(&defs.get(generic).name)
        .cloned()
        .unwrap_or_else(|| "?".to_string());
      let arg_strs: Vec<String> = args
        .iter()
        .map(|a| format_type_brief(types, defs, symbol_names, a))
        .collect();
      format!("{}<{}>", base, arg_strs.join(", "))
    },
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use std::path::PathBuf;

  use ignis_config::IgnisConfig;
  use ignis_parser::IgnisLexer;
  use ignis_type::file::FileId;

  fn lex(source: &str) -> Vec<Token> {
    let mut lexer = IgnisLexer::new(FileId::default(), source);
    lexer.scan_tokens();
    // Filter out EOF token
    lexer.tokens.into_iter().filter(|t| t.type_ != TokenType::Eof).collect()
  }

  fn labels(candidates: &[CompletionCandidate]) -> Vec<String> {
    candidates.iter().map(|candidate| candidate.label.clone()).collect()
  }

  #[test]
  fn test_scope_detection_inside_record_body() {
    let source = "record Rc {\n  \n}";
    let cursor = source.find("\n  ").unwrap() as u32 + 2;
    let tokens = lex(source);

    let (scope, _) = analyze_completion_scope(&tokens, cursor);
    assert_eq!(scope, CompletionScope::TypeBody);
  }

  #[test]
  fn test_scope_detection_inside_function_body() {
    let source = "function f(): void {\n  \n}";
    let cursor = source.find("\n  ").unwrap() as u32 + 2;
    let tokens = lex(source);

    let (scope, local_scope_start) = analyze_completion_scope(&tokens, cursor);
    assert_eq!(scope, CompletionScope::CallableBody);
    assert!(local_scope_start.is_some());
  }

  #[test]
  fn test_scope_detection_at_global_level() {
    let source = "const value: i32 = 1;\n";
    let tokens = lex(source);
    let cursor = source.len() as u32;

    let (scope, local_scope_start) = analyze_completion_scope(&tokens, cursor);
    assert_eq!(scope, CompletionScope::Global);
    assert!(local_scope_start.is_none());
  }

  #[test]
  fn test_type_body_snippets_only_include_member_modifiers() {
    let source = "record Rc {\n  \n}";
    let cursor = source.find("\n  ").unwrap() as u32 + 2;
    let tokens = lex(source);
    let mut candidates = Vec::new();

    add_snippet_completions(&tokens, cursor, "", CompletionScope::TypeBody, &mut candidates);
    let names = labels(&candidates);

    assert!(names.contains(&"public".to_string()));
    assert!(names.contains(&"private".to_string()));
    assert!(names.contains(&"static".to_string()));
    assert!(!names.contains(&"if".to_string()));
    assert!(!names.contains(&"import".to_string()));
  }

  #[test]
  fn test_callable_snippets_exclude_global_and_member_keywords() {
    let source = "function f(): void {\n  \n}";
    let cursor = source.find("\n  ").unwrap() as u32 + 2;
    let tokens = lex(source);
    let mut candidates = Vec::new();

    add_snippet_completions(&tokens, cursor, "", CompletionScope::CallableBody, &mut candidates);
    let names = labels(&candidates);

    assert!(names.contains(&"if".to_string()));
    assert!(names.contains(&"let".to_string()));
    assert!(!names.contains(&"public".to_string()));
    assert!(!names.contains(&"namespace".to_string()));
    assert!(!names.contains(&"import".to_string()));
  }

  #[test]
  fn test_callable_scope_offers_builtins_without_at() {
    let mut candidates = Vec::new();
    add_at_item_completions("si", CompletionScope::CallableBody, &mut candidates);
    let names = labels(&candidates);

    assert!(names.contains(&"sizeOf".to_string()));
    assert!(!names.contains(&"extension".to_string()));
  }

  #[test]
  fn test_global_scope_offers_directives_without_at() {
    let mut candidates = Vec::new();
    add_at_item_completions("ex", CompletionScope::Global, &mut candidates);
    let names = labels(&candidates);

    assert!(names.contains(&"extension".to_string()));
    assert!(!names.contains(&"sizeOf".to_string()));
  }

  #[test]
  fn test_type_body_scope_does_not_offer_at_items_without_at() {
    let mut candidates = Vec::new();
    add_at_item_completions("", CompletionScope::TypeBody, &mut candidates);
    assert!(candidates.is_empty());
  }

  #[test]
  fn test_detect_context_after_dot() {
    let source = "foo.";
    let tokens = lex(source);

    // Cursor right after the dot (position 4)
    let context = detect_context(&tokens, 4, source);

    match context {
      Some(CompletionContext::AfterDot { dot_offset, prefix }) => {
        assert_eq!(dot_offset, 3, "dot should start at position 3");
        assert_eq!(prefix, "", "no prefix typed yet");
      },
      other => panic!("Expected AfterDot context, got {:?}", other),
    }
  }

  #[test]
  fn test_detect_context_after_double_colon() {
    let source = "Foo::";
    let tokens = lex(source);

    // Cursor right after the ::
    let context = detect_context(&tokens, 5, source);

    match context {
      Some(CompletionContext::AfterDoubleColon {
        path_segments, prefix, ..
      }) => {
        assert_eq!(path_segments, vec!["Foo"]);
        assert_eq!(prefix, "");
      },
      other => panic!("Expected AfterDoubleColon context, got {:?}", other),
    }
  }

  #[test]
  fn test_detect_context_identifier() {
    let source = "let x = fo";
    let tokens = lex(source);

    // Cursor at end of "fo"
    let context = detect_context(&tokens, 10, source);

    match context {
      Some(CompletionContext::Identifier { prefix, .. }) => {
        assert_eq!(prefix, "fo");
      },
      other => panic!("Expected Identifier context, got {:?}", other),
    }
  }

  #[test]
  fn test_detect_context_dot_then_identifier_at_end() {
    // Cursor at end of `t` in `foo.t` (no trailing space)
    let source = "foo.t";
    let tokens = lex(source);

    let context = detect_context(&tokens, 5, source);

    match context {
      Some(CompletionContext::AfterDot { dot_offset, prefix }) => {
        assert_eq!(dot_offset, 3, "dot should start at position 3");
        assert_eq!(prefix, "t", "prefix should be 't'");
      },
      other => panic!("Expected AfterDot context, got {:?}", other),
    }
  }

  #[test]
  fn test_detect_context_dot_then_identifier_before_paren() {
    // Cursor at end of `t` in `foo.t)` — next token starts at cursor
    let source = "println(foo.t)";
    let tokens = lex(source);

    // Cursor at position 13 (end of `t`, right before `)`)
    let context = detect_context(&tokens, 13, source);

    match context {
      Some(CompletionContext::AfterDot { dot_offset, prefix }) => {
        assert_eq!(dot_offset, 11, "dot should start at position 11");
        assert_eq!(prefix, "t", "prefix should be 't'");
      },
      other => panic!("Expected AfterDot context, got {:?}", other),
    }
  }

  #[test]
  fn test_detect_context_double_colon_then_identifier_before_paren() {
    // Cursor at end of `b` in `Foo::b)` — next token starts at cursor
    let source = "call(Foo::b)";
    let tokens = lex(source);

    // Cursor at position 11 (end of `b`, right before `)`)
    let context = detect_context(&tokens, 11, source);

    match context {
      Some(CompletionContext::AfterDoubleColon {
        path_segments, prefix, ..
      }) => {
        assert_eq!(path_segments, vec!["Foo"]);
        assert_eq!(prefix, "b");
      },
      other => panic!("Expected AfterDoubleColon context, got {:?}", other),
    }
  }

  #[test]
  fn test_detect_context_after_nested_double_colon() {
    let source = "Math::Linear::";
    let tokens = lex(source);

    let context = detect_context(&tokens, source.len() as u32, source);

    match context {
      Some(CompletionContext::AfterDoubleColon {
        path_segments, prefix, ..
      }) => {
        assert_eq!(path_segments, vec!["Math", "Linear"]);
        assert_eq!(prefix, "");
      },
      other => panic!("Expected AfterDoubleColon context, got {:?}", other),
    }
  }

  #[test]
  fn test_detect_context_after_nested_double_colon_with_prefix() {
    let source = "Math::Linear::Ve";
    let tokens = lex(source);

    let context = detect_context(&tokens, source.len() as u32, source);

    match context {
      Some(CompletionContext::AfterDoubleColon {
        path_segments, prefix, ..
      }) => {
        assert_eq!(path_segments, vec!["Math", "Linear"]);
        assert_eq!(prefix, "Ve");
      },
      other => panic!("Expected AfterDoubleColon context, got {:?}", other),
    }
  }

  #[test]
  fn test_complete_double_colon_resolves_imported_namespace_head() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../..");
    let entry_path = repo_root.join("example/hello-world.ign");
    let source = std::fs::read_to_string(&entry_path).expect("example/hello-world.ign should exist");

    let mut config = IgnisConfig::default();
    config.std = true;
    config.auto_load_std = true;
    config.std_path = repo_root.join("std").to_string_lossy().to_string();

    let output = ignis_driver::analyze_project_with_text(
      &config,
      entry_path.to_str().expect("entry path should be valid UTF-8"),
      Some(source),
    );

    let current_file = output
      .source_map
      .lookup_by_path(&entry_path)
      .expect("entry file should be present in source map");

    let path = vec!["Io".to_string()];
    assert!(resolve_path_to_def(&path, &output, &current_file).is_some());

    let candidates = complete_double_colon(&path, "", &output, &current_file);
    assert!(candidates.iter().any(|c| c.label == "println"));
  }

  #[test]
  fn test_detect_import_path_context_clamps_cursor() {
    // Simulate import with string token
    let source = r#"import foo from "std::io""#;
    let tokens = lex(source);

    // Cursor way past the end of the string token (simulates editor desync)
    let cursor_past_end = 100u32;
    let context = detect_context(&tokens, cursor_past_end, source);

    // Should either return None or not panic
    // The function should not panic even with out-of-bounds cursor
    match context {
      None => {}, // This is acceptable - cursor is outside all tokens
      Some(CompletionContext::ImportPath { prefix }) => {
        // If it returns ImportPath, the prefix should be valid
        assert!(prefix.len() <= source.len(), "prefix should not exceed source length");
      },
      other => {
        // Other contexts might be detected depending on token positions
        // This is also acceptable
        let _ = other;
      },
    }
  }

  #[test]
  fn test_detect_import_path_context_cursor_at_quote() {
    // Cursor exactly at the opening quote position
    let source = r#"import foo from "std::io""#;
    let tokens = lex(source);

    // Find the position of the opening quote
    let quote_pos = source.find('"').unwrap() as u32;

    let context = detect_context(&tokens, quote_pos, source);

    // Should not panic and should handle this edge case
    match context {
      Some(CompletionContext::ImportPath { prefix }) => {
        // Cursor at quote start means empty or very short prefix
        assert!(prefix.len() <= 10, "prefix at quote start should be short");
      },
      _ => {}, // Other contexts are acceptable
    }
  }

  #[test]
  fn test_detect_import_path_fallback_ignores_previous_string() {
    // This tests that the fallback doesn't match quotes from previous unrelated strings
    let source = r#"let msg = "hello"; import foo from "std::s"#;
    //                                              cursor here ^

    // Cursor inside the import string, after "std::s"
    let cursor_pos = source.rfind("std::s").unwrap() + "std::s".len();

    let context = detect_import_path_fallback(source, cursor_pos as u32);

    match context {
      Some(CompletionContext::ImportPath { prefix }) => {
        // Should extract "std::s", not "hello" or anything from the previous string
        assert_eq!(
          prefix, "std::s",
          "should extract prefix from the import string, not previous string"
        );
      },
      _ => panic!("Expected ImportPath context from fallback"),
    }
  }

  #[test]
  fn test_detect_import_path_fallback_rejects_multiline() {
    // Fallback should reject multiline content
    let source = "import foo from \"std::\nio\"";

    // Cursor after the newline
    let cursor_pos = source.len();

    let context = detect_import_path_fallback(source, cursor_pos as u32);

    // Should return None because there's a newline in the content
    assert!(context.is_none(), "fallback should reject multiline strings");
  }

  #[test]
  fn test_detect_import_path_fallback_window_limit() {
    // Test that fallback only searches within a reasonable window
    // Create a source with a `from "` pattern far back (> 200 chars)
    let filler = "x".repeat(250);
    let source = format!(r#"import old from "old_module"; {} import new from "new::"#, filler);

    // Cursor at the end
    let cursor_pos = source.len();

    let context = detect_import_path_fallback(&source, cursor_pos as u32);

    match context {
      Some(CompletionContext::ImportPath { prefix }) => {
        // Should find "new::", not "old_module"
        assert_eq!(prefix, "new::", "should find the recent import, not the distant one");
      },
      _ => panic!("Expected ImportPath context from fallback"),
    }
  }

  #[test]
  fn test_detect_import_path_fallback_utf8_safety() {
    // Test with emoji and multi-byte UTF-8 characters
    let source = r#"let emoji = "🔥🚀"; import foo from "std::io"#;

    // Cursor at end
    let cursor_pos = source.len();
    let context = detect_import_path_fallback(source, cursor_pos as u32);

    match context {
      Some(CompletionContext::ImportPath { prefix }) => {
        assert_eq!(prefix, "std::io", "should handle UTF-8 correctly");
      },
      _ => panic!("Expected ImportPath context"),
    }

    // Test with cursor at a byte position that would be mid-character
    // The emoji 🔥 is 4 bytes, so position 14 is after "let emoji = \""
    // and before the emoji ends
    let mid_emoji_cursor = 14u32; // This is in the middle of 🔥
    let context = detect_import_path_fallback(source, mid_emoji_cursor);

    // Should not panic, and should return None (no from/import pattern in window)
    assert!(context.is_none(), "should not panic on mid-UTF8 cursor");
  }

  #[test]
  fn test_find_char_boundary() {
    use super::find_char_boundary;

    let text = "hello 🔥 world";
    // "hello " = 6 bytes, 🔥 = 4 bytes, " world" = 6 bytes

    // Position 6 is start of emoji - valid boundary
    assert_eq!(find_char_boundary(text, 6), 6);

    // Position 7 is in the middle of the emoji - should find next boundary at 10
    assert_eq!(find_char_boundary(text, 7), 10);

    // Position 8 is also in the middle of the emoji
    assert_eq!(find_char_boundary(text, 8), 10);

    // Position 10 is after the emoji - valid boundary
    assert_eq!(find_char_boundary(text, 10), 10);

    // Position past end should return text.len()
    assert_eq!(find_char_boundary(text, 100), text.len());
  }

  #[test]
  fn test_detect_context_after_at() {
    let source = "@";
    let tokens = lex(source);
    let context = detect_context(&tokens, 1, source);

    match context {
      Some(CompletionContext::AfterAt { prefix }) => {
        assert_eq!(prefix, "");
      },
      other => panic!("Expected AfterAt context, got {:?}", other),
    }
  }

  #[test]
  fn test_detect_context_after_at_with_prefix() {
    let source = "@pa";
    let tokens = lex(source);
    let context = detect_context(&tokens, 3, source);

    match context {
      Some(CompletionContext::AfterAt { prefix }) => {
        assert_eq!(prefix, "pa");
      },
      other => panic!("Expected AfterAt context, got {:?}", other),
    }
  }

  #[test]
  fn test_detect_context_after_at_full_name() {
    let source = "@sizeOf";
    let tokens = lex(source);
    let context = detect_context(&tokens, 7, source);

    match context {
      Some(CompletionContext::AfterAt { prefix }) => {
        assert_eq!(prefix, "sizeOf");
      },
      other => panic!("Expected AfterAt context, got {:?}", other),
    }
  }

  #[test]
  fn test_detect_context_at_bracket_list() {
    let source = "@[fo";
    let tokens = lex(source);
    let context = detect_context(&tokens, 4, source);

    match context {
      Some(CompletionContext::AfterAt { prefix }) => {
        assert_eq!(prefix, "fo");
      },
      other => panic!("Expected AfterAt context, got {:?}", other),
    }
  }

  #[test]
  fn test_detect_context_at_bracket_list_comma() {
    let source = "@[foo, ba";
    let tokens = lex(source);
    let context = detect_context(&tokens, 9, source);

    match context {
      Some(CompletionContext::AfterAt { prefix }) => {
        assert_eq!(prefix, "ba");
      },
      other => panic!("Expected AfterAt context, got {:?}", other),
    }
  }

  #[test]
  fn test_complete_at_items_returns_all() {
    let candidates = complete_at_items("");
    assert!(candidates.len() > 20, "expected all items, got {}", candidates.len());
    assert!(candidates.iter().any(|c| c.label == "sizeOf"));
    assert!(candidates.iter().any(|c| c.label == "packed"));
  }

  #[test]
  fn test_complete_at_items_filters_by_prefix() {
    let candidates = complete_at_items("si");
    assert_eq!(candidates.len(), 1);
    assert_eq!(candidates[0].label, "sizeOf");
  }

  #[test]
  fn test_complete_at_items_builtin_has_snippet() {
    let candidates = complete_at_items("sizeOf");
    assert_eq!(candidates.len(), 1);
    assert_eq!(candidates[0].insert_text_format, Some(InsertTextFormat::SNIPPET));
    assert!(candidates[0].insert_text.as_deref().unwrap().contains("${1:T}"));
  }

  #[test]
  fn test_complete_at_items_directive_no_snippet() {
    let candidates = complete_at_items("packed");
    assert_eq!(candidates.len(), 1);
    assert_eq!(candidates[0].insert_text_format, None);
    assert_eq!(candidates[0].insert_text.as_deref(), Some("packed"));
  }
}
