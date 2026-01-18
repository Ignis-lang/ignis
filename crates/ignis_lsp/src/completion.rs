//! LSP completion support for Ignis.
//!
//! This module provides autocompletion by analyzing tokens and cached analysis
//! without depending on a valid AST.

use std::collections::HashMap;
use std::io::Write;

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

use ignis_driver::AnalyzeProjectOutput;
use ignis_token::token::Token;
use ignis_token::token_types::TokenType;
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
#[derive(Debug, Clone, Copy)]
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
  }

  // Default: identifier context
  // Check current_token first (cursor inside token), then prev_token (cursor at end of token)
  let (prefix, start_offset) = if let Some(tok) = current_token {
    if tok.type_ == TokenType::Identifier {
      // Cursor is inside an identifier
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
    }
  } else if let Some(tok) = prev_token {
    // Cursor might be at the exact end of an identifier
    if tok.type_ == TokenType::Identifier && tok.span.end.0 == cursor_offset {
      let prefix = source_text
        .get(tok.span.start.0 as usize..tok.span.end.0 as usize)
        .unwrap_or("")
        .to_string();
      (prefix, tok.span.start.0)
    } else {
      (String::new(), cursor_offset)
    }
  } else {
    (String::new(), cursor_offset)
  };

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

/// Extract prefix being typed at cursor position.
fn extract_prefix_at_cursor(
  current_token: Option<&Token>,
  cursor_offset: u32,
) -> String {
  if let Some(tok) = current_token {
    if tok.type_ == TokenType::Identifier && tok.span.start.0 <= cursor_offset {
      let end = (cursor_offset - tok.span.start.0) as usize;
      return tok.lexeme.chars().take(end).collect();
    }
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
    } else {
      if tok.type_ == TokenType::DoubleColon {
        expect_ident = true;
      } else {
        break;
      }
    }
  }

  segments.reverse();
  segments
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

  candidates
}

/// Find receiver type by looking up node_spans.
fn find_receiver_type_by_span(
  dot_offset: u32,
  output: &AnalyzeProjectOutput,
  file_id: &FileId,
) -> Option<TypeId> {
  if output.node_spans.is_empty() {
    return None;
  }

  // Find receiver node: span.end <= dot_offset, pick max by (end, -span_size)
  let receiver_node = output
    .node_spans
    .iter()
    .filter(|(_, span)| span.file == *file_id && span.end.0 <= dot_offset)
    .max_by(|(_, a), (_, b)| {
      a.end
        .0
        .cmp(&b.end.0)
        .then_with(|| (b.end.0 - b.start.0).cmp(&(a.end.0 - a.start.0)))
    });

  let (node_id, _receiver_span) = receiver_node?;

  // Get type of receiver
  let type_id = output.node_types.get(node_id)?;

  Some(type_id.clone())
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
        return Some(var_def.type_id.clone());
      },
      DefinitionKind::Parameter(param_def) => {
        return Some(param_def.type_id.clone());
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
      // Enums might have instance methods
      // (in this implementation, they don't have fields accessible via `.`)
      for (sym_id, entry) in &enum_def.static_methods {
        // Note: For instance access on enum values, we'd need instance_methods
        // This is a placeholder for future extension
        let _ = (sym_id, entry);
      }
    },

    _ => {},
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

  // Add heuristic locals (Phase 0 scope analysis)
  add_heuristic_locals(tokens, start_offset, prefix, &mut candidates);

  if let Some(output) = output {
    let mut visible_defs = std::collections::HashSet::new();

    // 1. Collect definitions from the current file
    for (def_id, def) in output.defs.iter() {
      if def.span.file == *file_id {
        visible_defs.insert(def_id);
      }
    }

    // 2. Collect imported definitions
    // output.import_item_defs maps import usage span -> definition
    for (span, def_id) in &output.import_item_defs {
      if span.file == *file_id {
        visible_defs.insert(*def_id);
      }
    }

    // Add collected definitions
    for def_id in visible_defs {
      let def = output.defs.get(&def_id);

      // Skip internal definitions
      match &def.kind {
        DefinitionKind::Variable(_)
        | DefinitionKind::Parameter(_)
        | DefinitionKind::TypeParam(_)
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

      // Final visibility check (redundant for imports but safe)
      if !is_visible(def, file_id) {
        continue;
      }

      let (kind, detail, insert_text, insert_format) =
        def_to_completion_info(def, name, &output.types, &output.defs, &output.symbol_names, next_is_paren);

      // Determine sort priority
      // Locals (10) > Same file (20) > Imports (30) > Keywords (50)
      let sort_priority = if def.span.file == *file_id { 20 } else { 30 };

      candidates.push(CompletionCandidate {
        label: name.clone(),
        kind,
        detail,
        documentation: def.doc.clone(),
        insert_text,
        insert_text_format: insert_format,
        sort_priority,
      });
    }
  }

  // Add keywords
  add_snippet_completions(tokens, start_offset, prefix, &mut candidates);

  candidates
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
  let is_alphabetic = prefix.chars().next().map_or(false, |c| c.is_alphabetic());
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
    // Control flow
    add("if", "if { ... }", "if ($1) {\n\t$0\n}", snip_prio);
    add(
      "if else",
      "if { ... } else { ... }",
      "if ($1) {\n\t$2\n} else {\n\t$0\n}",
      snip_prio,
    );
    add("while", "while { ... }", "while ($1) {\n\t$0\n}", snip_prio);
    add("for", "for { ... }", "for (let $1 = $2; $3; $4) {\n\t$0\n}", snip_prio);
    add("for of", "for { ... }", "for (let $1 of $2) {\n\t$0\n}", snip_prio);
    add("match", "match { ... }", "match ($1) {\n\t$0\n}", snip_prio);
    add("return", "return val", "return $0;", snip_prio);

    // Declarations
    add("let", "let var: type = ...", "let $1: $2 = $0;", snip_prio);
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

    // Imports
    add("import", "import ... from ...", "import $0 from \"$1\"", snip_prio);
    add("from", "import ... from ...", "import $0 from \"$1\"", snip_prio);

    // Modifiers
    add("public", "public modifier", "public ", snip_prio);
    add("private", "private modifier", "private ", snip_prio);
    add("static", "static modifier", "static ", snip_prio);
    add("extern", "extern modifier", "extern ", snip_prio);
    add("mut", "mut modifier", "mut ", snip_prio);
  }

  // Special case: else (only valid after '}')
  if is_after_right_brace && wants_snippets {
    add("else", "else { ... }", "else {\n\t$0\n}", snip_prio);
  }

  // Keywords valid in almost any expression context
  if is_start_of_stmt
    || matches!(
      prev_token.map(|t| t.type_),
      Some(TokenType::Colon | TokenType::Equal | TokenType::LeftParen | TokenType::Comma)
    )
  {
    add("true", "boolean true", "true", lit_prio);
    add("false", "boolean false", "false", lit_prio);
    add("null", "null value", "null", lit_prio);
    add("self", "self reference", "self", lit_prio);
  }
}

/// Scan tokens backwards to find local variable declarations.
fn add_heuristic_locals(
  tokens: &[Token],
  cursor_offset: u32,
  prefix: &str,
  candidates: &mut Vec<CompletionCandidate>,
) {
  // Find token index at cursor
  let cursor_idx = tokens
    .iter()
    .position(|t| t.span.start.0 >= cursor_offset)
    .unwrap_or(tokens.len());

  let mut seen = std::collections::HashSet::new();

  // Scan backwards from cursor
  for i in (0..cursor_idx).rev() {
    let tok = &tokens[i];

    // Look for: let/const/mut <IDENT>
    if tok.type_ == TokenType::Identifier {
      let name = &tok.lexeme;

      if !matches_prefix(name, prefix) {
        continue;
      }

      // Check previous token
      if i > 0 {
        let prev = &tokens[i - 1];
        match prev.type_ {
          TokenType::Let | TokenType::Const | TokenType::Mut => {
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
          // Basic parameter detection: Function <Name> ( <Param>
          // Or: <Param> : <Type> inside parens (harder without parsing)
          // Simple heuristic: Identifiers followed by Colon or Comma inside function?
          // For now, let/const/mut is the safest bet without a parser.
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
  let mut items: Vec<CompletionItem> = candidates
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
  let mut current_def_id: Option<DefinitionId> = None;

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
    let Some(def_id) = current_def_id else {
      return None;
    };

    let def = output.defs.get(&def_id);

    current_def_id = match &def.kind {
      DefinitionKind::Namespace(ns_def) => {
        let ns = output.namespaces.get(&ns_def.namespace_id);

        // Look for child namespace
        for (sym_id, child_ns_id) in &ns.children {
          if let Some(name) = output.symbol_names.get(sym_id) {
            if name == segment {
              // Find the definition for this namespace
              for (d_id, d) in output.defs.iter() {
                if let DefinitionKind::Namespace(child_ns_def) = &d.kind {
                  if child_ns_def.namespace_id == *child_ns_id {
                    return Some(d_id);
                  }
                }
              }
            }
          }
        }

        // Look for definition in namespace
        for (sym_id, entry) in &ns.definitions {
          if let Some(name) = output.symbol_names.get(sym_id) {
            if name == segment {
              match entry {
                SymbolEntry::Single(id) => return Some(*id),
                SymbolEntry::Overload(ids) => return ids.first().copied(),
              }
            }
          }
        }

        None
      },
      _ => None,
    };
  }

  current_def_id
}

/// Resolve a path to a namespace ID.
fn resolve_path_to_namespace(
  path: &[String],
  output: &AnalyzeProjectOutput,
) -> Option<ignis_type::namespace::NamespaceId> {
  if path.is_empty() {
    return None;
  }

  // Build symbol IDs for path
  let mut sym_path = Vec::new();
  for segment in path {
    let sym_id = output
      .symbol_names
      .iter()
      .find(|(_, n)| *n == segment)
      .map(|(id, _)| *id)?;
    sym_path.push(sym_id);
  }

  output.namespaces.lookup(&sym_path)
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
  for (sym_id, _child_ns_id) in &ns.children {
    let Some(name) = output.symbol_names.get(sym_id) else {
      continue;
    };

    if !matches_prefix(name, prefix) {
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

      if !is_visible(def, current_file) {
        continue;
      }

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
        if let Some(p_name) = symbol_names.get(&p_def.name) {
          if let DefinitionKind::Parameter(param) = &p_def.kind {
            let type_str = format_type_brief(types, defs, symbol_names, &param.type_id);
            param_details.push(format!("{}: {}", p_name, type_str));
            snippet_args.push(format!("${{{}:{}}}", idx, p_name));
            idx += 1;
          }
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
    Type::String => "string".to_string(),
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
      match size {
        Some(s) => format!("{}[{}]", elem_str, s),
        None => format!("[{}]", elem_str),
      }
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
  use ignis_parser::IgnisLexer;
  use ignis_type::file::FileId;

  fn lex(source: &str) -> Vec<Token> {
    let mut lexer = IgnisLexer::new(FileId::default(), source);
    lexer.scan_tokens();
    // Filter out EOF token
    lexer.tokens.into_iter().filter(|t| t.type_ != TokenType::Eof).collect()
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
}
