//! LSP server implementation.

use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use ignis_driver::AnalyzeProjectOutput;
use ignis_type::file::FileId;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};
use url::Url;

use crate::convert::{convert_diagnostic, LineIndex};
use crate::state::LspState;

/// The Ignis Language Server.
pub struct Server {
  /// LSP client for sending notifications.
  client: Client,

  /// Shared server state.
  state: Arc<LspState>,
}

impl Server {
  /// Create a new server instance.
  pub fn new(
    client: Client,
    state: Arc<LspState>,
  ) -> Self {
    Self { client, state }
  }

  /// Analyze a document and publish diagnostics.
  ///
  /// Uses project-level analysis to resolve imports properly.
  /// Publishes diagnostics for ALL files affected by the analysis,
  /// and clears diagnostics from files that no longer have errors.
  async fn analyze_and_publish(
    &self,
    uri: &Url,
  ) {
    // Extract data from state (behind RwLock)
    let (path_str, text, version) = {
      let guard = self.state.open_files.read().await;

      let Some(doc) = guard.get(uri) else {
        return;
      };

      (doc.path.to_string_lossy().to_string(), doc.text.clone(), doc.version)
    };

    // Clone config for use in sync block
    let config = (*self.state.config).clone();

    // Run project-level analysis with in-memory text.
    let output = Arc::new(ignis_driver::analyze_project_with_text(&config, &path_str, Some(text)));

    // Cache the analysis result for future requests
    {
      let mut guard = self.state.open_files.write().await;

      if let Some(doc) = guard.get_mut(uri) {
        doc.set_cached_analysis(version, Arc::clone(&output));
      }
    }

    // Group diagnostics by file URI, caching LineIndex per file_id
    let diagnostics_by_uri = {
      let mut by_uri: HashMap<Url, Vec<Diagnostic>> = HashMap::new();
      let mut line_indexes: HashMap<FileId, LineIndex> = HashMap::new();

      for diag in &output.diagnostics {
        let file_id = &diag.primary_span.file;
        let file_path = output.source_map.get(file_id).path.to_string_lossy().to_string();

        let file_uri = match Url::from_file_path(&file_path) {
          Ok(u) => u,
          Err(_) => {
            // Skip files with non-convertible paths (e.g., Windows UNC paths, virtual files).
            // This is rare in practice on Unix systems.
            continue;
          },
        };

        // Cache LineIndex per file_id to avoid O(diags) constructions
        let line_index = line_indexes
          .entry(file_id.clone())
          .or_insert_with(|| LineIndex::new(output.source_map.get(file_id).text.clone()));

        let lsp_diag = convert_diagnostic(diag, line_index);

        by_uri.entry(file_uri).or_default().push(lsp_diag);
      }

      by_uri
    };

    // Get the set of URIs that had diagnostics before
    let previous_uris: HashSet<Url> = {
      let guard = self.state.previous_diagnostic_uris.read().await;
      guard.clone()
    };

    // Collect URIs that have diagnostics now
    let current_uris: HashSet<Url> = diagnostics_by_uri.keys().cloned().collect();

    // Publish diagnostics for all affected files
    for (file_uri, diags) in &diagnostics_by_uri {
      // Use version only for the file that triggered the analysis
      let diag_version = if file_uri == uri { Some(version) } else { None };

      self
        .client
        .publish_diagnostics(file_uri.clone(), diags.clone(), diag_version)
        .await;
    }

    // Clear diagnostics for files that previously had errors but now don't
    for stale_uri in previous_uris.difference(&current_uris) {
      self.client.publish_diagnostics(stale_uri.clone(), vec![], None).await;
    }

    // Update the set of URIs with diagnostics
    {
      let mut guard = self.state.previous_diagnostic_uris.write().await;
      *guard = current_uris;
    }
  }

  /// Run analysis for a document, using cache if available.
  ///
  /// Returns the analysis output and the current document version.
  async fn get_analysis(
    &self,
    uri: &Url,
  ) -> Option<(Arc<AnalyzeProjectOutput>, String, i32)> {
    let guard = self.state.open_files.read().await;

    let doc = guard.get(uri)?;
    let path_str = doc.path.to_string_lossy().to_string();
    let version = doc.version;

    // Check if we have a cached analysis for the current version
    if let Some(cached) = doc.get_cached_analysis() {
      return Some((cached, path_str, version));
    }

    // No cache available - run analysis
    let text = doc.text.clone();
    drop(guard); // Release read lock before analysis

    let config = (*self.state.config).clone();
    let output = Arc::new(ignis_driver::analyze_project_with_text(&config, &path_str, Some(text)));

    // Cache the result
    {
      let mut guard = self.state.open_files.write().await;

      if let Some(doc) = guard.get_mut(uri) {
        // Only cache if version hasn't changed
        if doc.version == version {
          doc.set_cached_analysis(version, Arc::clone(&output));
        }
      }
    }

    Some((output, path_str, version))
  }

  /// Invalidate cached analysis for all open documents and re-analyze.
  async fn invalidate_and_reanalyze(&self) {
    let uris: Vec<Url> = {
      let guard = self.state.open_files.read().await;
      guard.keys().cloned().collect()
    };

    if uris.is_empty() {
      return;
    }

    {
      let mut guard = self.state.open_files.write().await;
      for doc in guard.values_mut() {
        doc.cached_analysis = None;
      }
    }

    // Any open doc triggers full project analysis
    self.analyze_and_publish(&uris[0]).await;
  }
}

#[tower_lsp::async_trait]
impl LanguageServer for Server {
  async fn initialize(
    &self,
    params: InitializeParams,
  ) -> Result<InitializeResult> {
    // Store workspace root
    if let Some(root_uri) = params.root_uri {
      if let Ok(path) = root_uri.to_file_path() {
        self.state.set_root(Some(path)).await;
      }
    }

    Ok(InitializeResult {
      capabilities: ServerCapabilities {
        // Full text sync - client sends entire document on change
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),

        // Go to definition
        definition_provider: Some(OneOf::Left(true)),

        // Hover (type info)
        hover_provider: Some(HoverProviderCapability::Simple(true)),

        // Semantic tokens (semantic highlighting)
        semantic_tokens_provider: Some(SemanticTokensServerCapabilities::SemanticTokensOptions(
          SemanticTokensOptions {
            legend: crate::semantic::build_legend(),
            full: Some(SemanticTokensFullOptions::Bool(true)),
            range: None,
            work_done_progress_options: WorkDoneProgressOptions::default(),
          },
        )),

        // Inlay hints (parameter names at call sites)
        inlay_hint_provider: Some(OneOf::Right(InlayHintServerCapabilities::Options(InlayHintOptions {
          work_done_progress_options: WorkDoneProgressOptions::default(),
          resolve_provider: Some(false),
        }))),

        // TODO: Add more capabilities as we implement features:
        // - references_provider
        // - completion_provider
        ..Default::default()
      },
      server_info: Some(ServerInfo {
        name: "ignis-lsp".to_string(),
        version: Some(env!("CARGO_PKG_VERSION").to_string()),
      }),
    })
  }

  async fn initialized(
    &self,
    _: InitializedParams,
  ) {
    let registration = Registration {
      id: "ignis-file-watcher".to_string(),
      method: "workspace/didChangeWatchedFiles".to_string(),
      register_options: Some(
        serde_json::to_value(DidChangeWatchedFilesRegistrationOptions {
          watchers: vec![FileSystemWatcher {
            glob_pattern: GlobPattern::String("**/*.ign".to_string()),
            kind: Some(WatchKind::all()),
          }],
        })
        .expect("valid JSON"),
      ),
    };

    if let Err(e) = self.client.register_capability(vec![registration]).await {
      self
        .client
        .log_message(MessageType::WARNING, format!("Failed to register file watcher: {}", e))
        .await;
    }

    self
      .client
      .log_message(MessageType::INFO, "Ignis LSP initialized")
      .await;
  }

  async fn shutdown(&self) -> Result<()> {
    Ok(())
  }

  async fn did_open(
    &self,
    params: DidOpenTextDocumentParams,
  ) {
    let uri = params.text_document.uri;
    let version = params.text_document.version;
    let text = params.text_document.text;

    self.state.open_document(uri.clone(), version, text).await;
    self.analyze_and_publish(&uri).await;
  }

  async fn did_change(
    &self,
    params: DidChangeTextDocumentParams,
  ) {
    let uri = params.text_document.uri;
    let version = params.text_document.version;

    // With FULL sync, there's exactly one change containing the entire text
    if let Some(change) = params.content_changes.into_iter().next() {
      self.state.update_document(&uri, version, change.text).await;
      self.analyze_and_publish(&uri).await;
    }
  }

  async fn did_close(
    &self,
    params: DidCloseTextDocumentParams,
  ) {
    let uri = params.text_document.uri;

    self.state.close_document(&uri).await;

    // Clear diagnostics for closed file
    self.client.publish_diagnostics(uri, vec![], None).await;
  }

  async fn did_change_watched_files(
    &self,
    params: DidChangeWatchedFilesParams,
  ) {
    let mut should_reanalyze = false;

    for change in params.changes {
      match change.typ {
        FileChangeType::CREATED | FileChangeType::CHANGED => {
          // Open files are handled by didChange, skip them
          let is_open = {
            let guard = self.state.open_files.read().await;
            guard.contains_key(&change.uri)
          };

          if !is_open {
            should_reanalyze = true;
          }
        },
        FileChangeType::DELETED => {
          should_reanalyze = true;
        },
        _ => {},
      }
    }

    if should_reanalyze {
      self.invalidate_and_reanalyze().await;
    }
  }

  async fn goto_definition(
    &self,
    params: GotoDefinitionParams,
  ) -> Result<Option<GotoDefinitionResponse>> {
    let uri = &params.text_document_position_params.text_document.uri;
    let position = params.text_document_position_params.position;

    // Get cached analysis or run new analysis
    let Some((output, path_str, _)) = self.get_analysis(uri).await else {
      return Ok(None);
    };

    // Get line_index from open document (use cached instance)
    let line_index = {
      let guard = self.state.open_files.read().await;

      let Some(doc) = guard.get(uri) else {
        return Ok(None);
      };

      doc.line_index.clone()
    };

    // Get the FileId for the current file
    let Some(file_id) = output.source_map.lookup_by_path(&path_str) else {
      return Ok(None);
    };

    // Convert LSP position to byte offset
    let byte_offset = line_index.offset(position);

    // Find which node's span contains this position (only from this file)
    let mut found_node = None;
    let mut smallest_span_size = u32::MAX;

    for (node_id, span) in &output.node_spans {
      // Only consider spans from the current file
      if span.file != file_id {
        continue;
      }

      // Check if this span contains the cursor position
      if span.start.0 <= byte_offset && byte_offset <= span.end.0 {
        // Prefer the smallest span that contains the position
        let span_size = span.end.0 - span.start.0;
        if span_size < smallest_span_size {
          smallest_span_size = span_size;
          found_node = Some(node_id.clone());
        }
      }
    }

    // If we found a node, look up its definition
    let Some(node_id) = found_node else {
      return Ok(None);
    };

    // Check node_defs first, then resolved_calls (for overloaded function calls)
    let def_id = output
      .node_defs
      .get(&node_id)
      .or_else(|| output.resolved_calls.get(&node_id));

    let Some(def_id) = def_id else {
      return Ok(None);
    };

    let def = output.defs.get(def_id);

    // Get the file path and text for the definition's file
    let def_file_path = output.source_map.get(&def.span.file).path.clone();
    let def_text = output.source_map.get(&def.span.file).text.clone();

    let def_uri = match Url::from_file_path(&def_file_path) {
      Ok(u) => u,
      Err(_) => return Ok(None),
    };

    let def_line_index = LineIndex::new(def_text);
    let range = def_line_index.span_to_range(&def.span);

    Ok(Some(GotoDefinitionResponse::Scalar(Location { uri: def_uri, range })))
  }

  async fn hover(
    &self,
    params: HoverParams,
  ) -> Result<Option<Hover>> {
    let uri = &params.text_document_position_params.text_document.uri;
    let position = params.text_document_position_params.position;

    // Get cached analysis or run new analysis
    let Some((output, path_str, _)) = self.get_analysis(uri).await else {
      return Ok(None);
    };

    // Get line_index from open document (use cached instance)
    let line_index = {
      let guard = self.state.open_files.read().await;

      let Some(doc) = guard.get(uri) else {
        return Ok(None);
      };

      doc.line_index.clone()
    };

    // Get the FileId for the current file
    let Some(file_id) = output.source_map.lookup_by_path(&path_str) else {
      return Ok(None);
    };

    // Convert LSP position to byte offset
    let byte_offset = line_index.offset(position);

    // Find which node's span contains this position (only from this file)
    let mut found_node = None;
    let mut smallest_span_size = u32::MAX;

    for (node_id, span) in &output.node_spans {
      // Only consider spans from the current file
      if span.file != file_id {
        continue;
      }

      if span.start.0 <= byte_offset && byte_offset <= span.end.0 {
        let span_size = span.end.0 - span.start.0;
        if span_size < smallest_span_size {
          smallest_span_size = span_size;
          found_node = Some(node_id.clone());
        }
      }
    }

    // First, check if cursor is on an import item (more specific than the import statement node)
    let import_hover = {
      let mut found_import = None;
      let mut smallest_size = u32::MAX;

      for (span, def_id) in &output.import_item_defs {
        if span.file != file_id {
          continue;
        }

        if span.start.0 <= byte_offset && byte_offset <= span.end.0 {
          let size = span.end.0 - span.start.0;
          if size < smallest_size {
            smallest_size = size;
            found_import = Some((span.clone(), def_id.clone(), size));
          }
        }
      }

      found_import
    };

    // Build hover content based on definition and/or type
    // Import items take priority when cursor is on them (they're more specific than the import statement)
    let (content, hover_span) = if let Some((import_span, import_def_id, import_size)) = import_hover {
      // Check if we also have a node, and if so, whether the import item is more specific
      let use_import = if let Some(ref node_id) = found_node {
        if let Some(node_span) = output.node_spans.get(node_id) {
          let node_size = node_span.end.0 - node_span.start.0;
          import_size <= node_size // Prefer import item if it's same size or smaller
        } else {
          true // No span for node, use import
        }
      } else {
        true // No node found, use import
      };

      if use_import {
        let content = format_definition_hover(
          &output.defs,
          &output.types,
          &output.symbol_names,
          &output.source_map,
          &import_def_id,
        );
        (content, Some(import_span))
      } else {
        // Fall through to node-based lookup
        (String::new(), None)
      }
    } else {
      (String::new(), None)
    };

    // If no import item found (or node was more specific), try node-based lookups
    let (content, hover_span) = if content.is_empty() {
      if let Some(node_id) = found_node {
        // Try node_defs first, then resolved_calls (for overloaded function calls), then types
        let content = if let Some(def_id) = output.node_defs.get(&node_id) {
          format_definition_hover(&output.defs, &output.types, &output.symbol_names, &output.source_map, def_id)
        } else if let Some(def_id) = output.resolved_calls.get(&node_id) {
          // This is a call to an overloaded function - show the resolved overload
          format_definition_hover(&output.defs, &output.types, &output.symbol_names, &output.source_map, def_id)
        } else if let Some(type_id) = output.node_types.get(&node_id) {
          // No definition, just show the type
          format!(
            "```ignis\n{}\n```",
            format_type(&output.types, &output.defs, &output.symbol_names, type_id)
          )
        } else {
          String::new()
        };

        let span = output.node_spans.get(&node_id).cloned();
        (content, span)
      } else {
        (String::new(), None)
      }
    } else {
      (content, hover_span)
    };

    if content.is_empty() {
      return Ok(None);
    }

    // Get the range of the hovered element for highlighting
    let range = hover_span.as_ref().map(|s| line_index.span_to_range(s));

    Ok(Some(Hover {
      contents: HoverContents::Markup(MarkupContent {
        kind: MarkupKind::Markdown,
        value: content,
      }),
      range,
    }))
  }

  async fn semantic_tokens_full(
    &self,
    params: SemanticTokensParams,
  ) -> Result<Option<SemanticTokensResult>> {
    let uri = &params.text_document.uri;

    // Get cached analysis or run new analysis
    let Some((output, path_str, _)) = self.get_analysis(uri).await else {
      return Ok(None);
    };

    // Get text and line_index from open document (use cached line_index)
    let (text, line_index) = {
      let guard = self.state.open_files.read().await;

      let Some(doc) = guard.get(uri) else {
        return Ok(None);
      };

      (doc.text.clone(), doc.line_index.clone())
    };

    let Some(file_id) = output.source_map.lookup_by_path(&path_str) else {
      return Ok(None);
    };

    // Lex the source to get tokens
    let mut lexer = ignis_parser::IgnisLexer::new(file_id, &text);
    lexer.scan_tokens();

    // Classify tokens using semantic analysis
    let tokens =
      crate::semantic::classify_tokens(&lexer.tokens, &output.import_item_defs, &output.defs, &file_id, &line_index);

    Ok(Some(SemanticTokensResult::Tokens(tokens)))
  }

  async fn inlay_hint(
    &self,
    params: InlayHintParams,
  ) -> Result<Option<Vec<InlayHint>>> {
    let uri = &params.text_document.uri;

    // Get cached analysis or run new analysis
    let Some((output, path_str, _)) = self.get_analysis(uri).await else {
      return Ok(None);
    };

    // Get line_index from open document (use cached instance)
    let line_index = {
      let guard = self.state.open_files.read().await;

      let Some(doc) = guard.get(uri) else {
        return Ok(None);
      };

      doc.line_index.clone()
    };

    let Some(file_id) = output.source_map.lookup_by_path(&path_str) else {
      return Ok(None);
    };

    let hints = crate::inlay::generate_parameter_hints(
      &output.nodes,
      &output.roots,
      &output.resolved_calls,
      &output.node_defs,
      &output.defs,
      &output.symbol_names,
      &file_id,
      &line_index,
    );

    Ok(Some(hints))
  }
}

/// Format a type as a human-readable string.
fn format_type(
  types: &ignis_type::types::TypeStore,
  defs: &ignis_type::definition::DefinitionStore,
  symbol_names: &std::collections::HashMap<ignis_type::symbol::SymbolId, String>,
  type_id: &ignis_type::types::TypeId,
) -> String {
  use ignis_type::types::Type;

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
      if *mutable {
        format!("*mut {}", format_type(types, defs, symbol_names, inner))
      } else {
        format!("*{}", format_type(types, defs, symbol_names, inner))
      }
    },
    Type::Reference { inner, mutable } => {
      if *mutable {
        format!("&mut {}", format_type(types, defs, symbol_names, inner))
      } else {
        format!("&{}", format_type(types, defs, symbol_names, inner))
      }
    },
    Type::Vector { element, size } => {
      if let Some(s) = size {
        format!("[{}; {}]", format_type(types, defs, symbol_names, element), s)
      } else {
        format!("[{}]", format_type(types, defs, symbol_names, element))
      }
    },
    Type::Tuple(elements) => {
      let elem_strs: Vec<_> = elements
        .iter()
        .map(|e| format_type(types, defs, symbol_names, e))
        .collect();
      format!("({})", elem_strs.join(", "))
    },
    Type::Function {
      params,
      ret,
      is_variadic,
    } => {
      let param_strs: Vec<_> = params
        .iter()
        .map(|p| format_type(types, defs, symbol_names, p))
        .collect();
      let variadic = if *is_variadic { ", ..." } else { "" };
      format!(
        "fn({}{}) -> {}",
        param_strs.join(", "),
        variadic,
        format_type(types, defs, symbol_names, ret)
      )
    },
    Type::Record(def_id) => {
      let name = symbol_names
        .get(&defs.get(def_id).name)
        .cloned()
        .unwrap_or_else(|| "?".to_string());
      name
    },
    Type::Enum(def_id) => {
      let name = symbol_names
        .get(&defs.get(def_id).name)
        .cloned()
        .unwrap_or_else(|| "?".to_string());
      name
    },
    Type::Param { index, .. } => {
      format!("T{}", index)
    },
    Type::Instance { generic, args } => {
      let base_name = symbol_names
        .get(&defs.get(generic).name)
        .cloned()
        .unwrap_or_else(|| "?".to_string());
      let arg_strs: Vec<_> = args.iter().map(|a| format_type(types, defs, symbol_names, a)).collect();
      format!("{}<{}>", base_name, arg_strs.join(", "))
    },
  }
}

/// Format a definition for hover display with full signature.
fn format_definition_hover(
  defs: &ignis_type::definition::DefinitionStore,
  types: &ignis_type::types::TypeStore,
  symbol_names: &std::collections::HashMap<ignis_type::symbol::SymbolId, String>,
  source_map: &ignis_type::file::SourceMap,
  def_id: &ignis_type::definition::DefinitionId,
) -> String {
  use ignis_type::definition::DefinitionKind;

  let def = defs.get(def_id);
  let name = symbol_names.get(&def.name).cloned().unwrap_or_else(|| "?".to_string());

  let file_info = {
    let file = source_map.get(&def.span.file);
    let path = &file.path;
    path.file_name().map(|n| n.to_string_lossy().to_string())
  };

  let signature = match &def.kind {
    DefinitionKind::Function(func_def) => {
      format_function_signature(&name, func_def, defs, types, symbol_names, func_def.is_extern)
    },

    DefinitionKind::Method(method_def) => format_method_signature(&name, method_def, defs, types, symbol_names),

    DefinitionKind::Variable(var_def) => {
      let ty = format_type(types, defs, symbol_names, &var_def.type_id);
      let mutability = if var_def.mutable { "let mut" } else { "let" };
      format!("```ignis\n{} {}: {}\n```", mutability, name, ty)
    },

    DefinitionKind::Constant(const_def) => {
      let ty = format_type(types, defs, symbol_names, &const_def.type_id);
      format!("```ignis\nconst {}: {}\n```", name, ty)
    },

    DefinitionKind::Parameter(param_def) => {
      let ty = format_type(types, defs, symbol_names, &param_def.type_id);
      let mutability = if param_def.mutable { "mut " } else { "" };
      format!("```ignis\n{}{}: {}\n```\n\n(parameter)", mutability, name, ty)
    },

    DefinitionKind::Record(record_def) => {
      let type_params = format_type_params(&record_def.type_params, defs, symbol_names);
      let fields: Vec<String> = record_def
        .fields
        .iter()
        .map(|f| {
          let field_name = symbol_names.get(&f.name).cloned().unwrap_or_else(|| "?".to_string());
          let field_ty = format_type(types, defs, symbol_names, &f.type_id);
          format!("  {}: {}", field_name, field_ty)
        })
        .collect();
      let fields_str = if fields.is_empty() {
        String::new()
      } else {
        format!("\n{}\n", fields.join(",\n"))
      };
      format!("```ignis\nrecord {}{} {{{}}}\n```", name, type_params, fields_str)
    },

    DefinitionKind::Enum(enum_def) => {
      let type_params = format_type_params(&enum_def.type_params, defs, symbol_names);
      let variants: Vec<String> = enum_def
        .variants
        .iter()
        .map(|v| {
          let variant_name = symbol_names.get(&v.name).cloned().unwrap_or_else(|| "?".to_string());
          if v.payload.is_empty() {
            format!("  {}", variant_name)
          } else {
            let payload: Vec<String> = v
              .payload
              .iter()
              .map(|t| format_type(types, defs, symbol_names, t))
              .collect();
            format!("  {}({})", variant_name, payload.join(", "))
          }
        })
        .collect();
      let variants_str = if variants.is_empty() {
        String::new()
      } else {
        format!("\n{}\n", variants.join(",\n"))
      };
      format!("```ignis\nenum {}{} {{{}}}\n```", name, type_params, variants_str)
    },

    DefinitionKind::TypeAlias(alias_def) => {
      let type_params = format_type_params(&alias_def.type_params, defs, symbol_names);
      let target = format_type(types, defs, symbol_names, &alias_def.target);
      format!("```ignis\ntype {}{} = {}\n```", name, type_params, target)
    },

    DefinitionKind::Namespace(_) => {
      format!("```ignis\nnamespace {}\n```", name)
    },

    DefinitionKind::TypeParam(_) => {
      format!("```ignis\n{}\n```\n\n(type parameter)", name)
    },

    DefinitionKind::Field(field_def) => {
      let ty = format_type(types, defs, symbol_names, &field_def.type_id);
      let owner_name = symbol_names
        .get(&defs.get(&field_def.owner_type).name)
        .cloned()
        .unwrap_or_else(|| "?".to_string());
      format!("```ignis\n{}: {}\n```\n\n(field of `{}`)", name, ty, owner_name)
    },

    DefinitionKind::Variant(variant_def) => {
      let owner_name = symbol_names
        .get(&defs.get(&variant_def.owner_enum).name)
        .cloned()
        .unwrap_or_else(|| "?".to_string());

      if variant_def.payload.is_empty() {
        format!("```ignis\n{}::{}\n```\n\n(enum variant)", owner_name, name)
      } else {
        let payload_types: Vec<String> = variant_def
          .payload
          .iter()
          .map(|ty| format_type(types, defs, symbol_names, ty))
          .collect();
        format!(
          "```ignis\n{}::{}({})\n```\n\n(enum variant)",
          owner_name,
          name,
          payload_types.join(", ")
        )
      }
    },

    DefinitionKind::Placeholder => {
      format!("`{}`", name)
    },
  };

  // Build the final hover content
  let mut result = signature;

  // Add documentation if present
  if let Some(doc) = &def.doc {
    result.push_str("\n\n---\n\n");
    result.push_str(doc);
  }

  // Add file info if available
  if let Some(filename) = file_info {
    result.push_str(&format!("\n\n*Defined in `{}`*", filename));
  }

  result
}

/// Format a function signature.
fn format_function_signature(
  name: &str,
  func_def: &ignis_type::definition::FunctionDefinition,
  defs: &ignis_type::definition::DefinitionStore,
  types: &ignis_type::types::TypeStore,
  symbol_names: &std::collections::HashMap<ignis_type::symbol::SymbolId, String>,
  is_extern: bool,
) -> String {
  let type_params = format_type_params(&func_def.type_params, defs, symbol_names);

  let params: Vec<String> = func_def
    .params
    .iter()
    .map(|param_id| {
      let param_def = defs.get(param_id);
      let param_name = symbol_names
        .get(&param_def.name)
        .cloned()
        .unwrap_or_else(|| "?".to_string());
      if let ignis_type::definition::DefinitionKind::Parameter(p) = &param_def.kind {
        let ty = format_type(types, defs, symbol_names, &p.type_id);
        format!("{}: {}", param_name, ty)
      } else {
        param_name
      }
    })
    .collect();

  let variadic = if func_def.is_variadic { ", ..." } else { "" };
  let ret = format_type(types, defs, symbol_names, &func_def.return_type);
  let extern_prefix = if is_extern || func_def.is_extern { "extern " } else { "" };

  format!(
    "```ignis\n{}function {}{}({}{}): {}\n```",
    extern_prefix,
    name,
    type_params,
    params.join(", "),
    variadic,
    ret
  )
}

/// Format a method signature.
fn format_method_signature(
  name: &str,
  method_def: &ignis_type::definition::MethodDefinition,
  defs: &ignis_type::definition::DefinitionStore,
  types: &ignis_type::types::TypeStore,
  symbol_names: &std::collections::HashMap<ignis_type::symbol::SymbolId, String>,
) -> String {
  let type_params = format_type_params(&method_def.type_params, defs, symbol_names);

  // Get owner type name
  let owner_name = symbol_names
    .get(&defs.get(&method_def.owner_type).name)
    .cloned()
    .unwrap_or_else(|| "?".to_string());

  let params: Vec<String> = method_def
    .params
    .iter()
    .map(|param_id| {
      let param_def = defs.get(param_id);
      let param_name = symbol_names
        .get(&param_def.name)
        .cloned()
        .unwrap_or_else(|| "?".to_string());
      if let ignis_type::definition::DefinitionKind::Parameter(p) = &param_def.kind {
        let ty = format_type(types, defs, symbol_names, &p.type_id);
        format!("{}: {}", param_name, ty)
      } else {
        param_name
      }
    })
    .collect();

  let ret = format_type(types, defs, symbol_names, &method_def.return_type);

  if method_def.is_static {
    format!(
      "```ignis\n{}::{}{}({}): {}\n```\n\n(static method)",
      owner_name,
      name,
      type_params,
      params.join(", "),
      ret
    )
  } else {
    let self_ref = if method_def.self_mutable { "&mut self" } else { "&self" };
    let params_with_self = if params.is_empty() {
      self_ref.to_string()
    } else {
      format!("{}, {}", self_ref, params.join(", "))
    };
    format!(
      "```ignis\n{}::{}{}({}): {}\n```\n\n(instance method)",
      owner_name, name, type_params, params_with_self, ret
    )
  }
}

/// Format type parameters for display.
fn format_type_params(
  type_params: &[ignis_type::definition::DefinitionId],
  defs: &ignis_type::definition::DefinitionStore,
  symbol_names: &std::collections::HashMap<ignis_type::symbol::SymbolId, String>,
) -> String {
  if type_params.is_empty() {
    return String::new();
  }

  let names: Vec<String> = type_params
    .iter()
    .map(|id| {
      symbol_names
        .get(&defs.get(id).name)
        .cloned()
        .unwrap_or_else(|| "?".to_string())
    })
    .collect();

  format!("<{}>", names.join(", "))
}
