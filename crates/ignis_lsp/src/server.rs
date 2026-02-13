//! LSP server implementation.

use std::collections::{HashMap, HashSet};
use std::panic::{AssertUnwindSafe, catch_unwind};
use std::path::{Path, PathBuf};
use std::sync::Arc;

use ignis_driver::{AnalysisOptions, AnalyzeProjectOutput};
use ignis_type::file::FileId;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};
use url::Url;

use crate::completion::{
  CompletionContext, complete_at_items, complete_dot, complete_double_colon, complete_identifier, complete_import_path,
  complete_record_init, detect_context, log, to_completion_items,
};
use crate::convert::{convert_diagnostic, LineIndex};
use crate::project::ProjectContext;
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
    let (path_str, version) = {
      let guard = self.state.open_files.read().await;

      let Some(doc) = guard.get(uri) else {
        return;
      };

      (doc.path.to_string_lossy().to_string(), doc.version)
    };

    let path = PathBuf::from(&path_str);

    // Get project context for this file
    let project_ctx = self.state.project_manager.project_for_file(&path).await;

    // Collect file overrides from all open .ign files
    let file_overrides = self.collect_file_overrides().await;

    // Build config and analysis options based on project context
    let (config, entry_path, project_opt) = match &project_ctx {
      ProjectContext::Project(resolved) => {
        let config = (*resolved.config).clone();
        let editing_std_file = is_file_inside_std_path(&path, &config.std_path);

        if editing_std_file {
          (config, path_str.clone(), None)
        } else {
          (
            config,
            resolved.project.entry.to_string_lossy().to_string(),
            Some(resolved.project.clone()),
          )
        }
      },
      ProjectContext::NoProject => ((*self.state.config).clone(), path_str.clone(), None),
      ProjectContext::Error { root, error } => {
        // Publish TOML diagnostic
        self.publish_toml_diagnostics(root).await;
        log(&format!("[analyze] Project error: {}", error));
        // Continue with fallback config
        ((*self.state.config).clone(), path_str.clone(), None)
      },
    };

    let options = AnalysisOptions {
      file_overrides,
      project: project_opt,
    };

    // Run project-level analysis, catching panics
    let analysis_result = catch_unwind(AssertUnwindSafe(|| {
      ignis_driver::analyze_project_with_options(&config, &entry_path, options)
    }));

    let output = match analysis_result {
      Ok(o) => Arc::new(o),
      Err(panic_info) => {
        let panic_msg = if let Some(s) = panic_info.downcast_ref::<&str>() {
          s.to_string()
        } else if let Some(s) = panic_info.downcast_ref::<String>() {
          s.clone()
        } else {
          "unknown panic".to_string()
        };
        log(&format!("[analyze] PANIC: {} in {}", panic_msg, path_str));
        return;
      },
    };

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
          .entry(*file_id)
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

    // Check if the document version changed during analysis (abort if stale)
    {
      let guard = self.state.open_files.read().await;
      if let Some(doc) = guard.get(uri)
        && doc.version != version
      {
        return;
      }
    }

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
    let (path_str, path, version, cached) = {
      let guard = self.state.open_files.read().await;
      let doc = guard.get(uri)?;
      let path_str = doc.path.to_string_lossy().to_string();
      let path = doc.path.clone();
      let version = doc.version;
      let cached = doc.get_cached_analysis();
      (path_str, path, version, cached)
    };

    // Check if we have a cached analysis for the current version
    if let Some(cached) = cached {
      return Some((cached, path_str, version));
    }

    // No cache available - run analysis with project-aware configuration
    let project_ctx = self.state.project_manager.project_for_file(&path).await;
    let file_overrides = self.collect_file_overrides().await;

    let (config, entry_path, project_opt) = match &project_ctx {
      ProjectContext::Project(resolved) => {
        let config = (*resolved.config).clone();
        let editing_std_file = is_file_inside_std_path(&path, &config.std_path);

        if editing_std_file {
          (config, path_str.clone(), None)
        } else {
          (
            config,
            resolved.project.entry.to_string_lossy().to_string(),
            Some(resolved.project.clone()),
          )
        }
      },
      _ => ((*self.state.config).clone(), path_str.clone(), None),
    };

    let options = AnalysisOptions {
      file_overrides,
      project: project_opt,
    };

    let output = Arc::new(ignis_driver::analyze_project_with_options(&config, &entry_path, options));

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

  /// Collect text from all open .ign files for analysis.
  async fn collect_file_overrides(&self) -> HashMap<PathBuf, String> {
    let guard = self.state.open_files.read().await;

    guard
      .iter()
      .filter(|(uri, _)| uri.path().ends_with(".ign"))
      .filter_map(|(uri, doc)| uri.to_file_path().ok().map(|path| (path, doc.text.clone())))
      .collect()
  }

  /// Publish diagnostics for ignis.toml errors.
  async fn publish_toml_diagnostics(
    &self,
    root: &std::path::Path,
  ) {
    let ctx = self.state.project_manager.project_for_root(root).await;

    let toml_path = root.join("ignis.toml");
    let Ok(uri) = Url::from_file_path(&toml_path) else {
      return;
    };

    let diagnostics = match ctx {
      ProjectContext::Project(_) | ProjectContext::NoProject => vec![],
      ProjectContext::Error { error, .. } => {
        vec![Diagnostic {
          range: Range::default(), // (0,0)-(0,0)
          severity: Some(DiagnosticSeverity::ERROR),
          code: Some(NumberOrString::String("P0001".to_string())),
          source: Some("ignis".to_string()),
          message: error.to_string(),
          ..Default::default()
        }]
      },
    };

    self.client.publish_diagnostics(uri, diagnostics, None).await;
  }

  /// Re-analyze all open .ign files in a project.
  async fn reanalyze_project_files(
    &self,
    _root: &std::path::Path,
  ) {
    // For now, just re-analyze all open files.
    // A more sophisticated approach would filter by project root.
    self.invalidate_and_reanalyze().await;
  }
}

fn is_file_inside_std_path(
  file_path: &Path,
  std_path: &str,
) -> bool {
  if std_path.is_empty() {
    return false;
  }

  let std_canon = match Path::new(std_path).canonicalize() {
    Ok(path) => path,
    Err(_) => return false,
  };

  let file_canon = match file_path.canonicalize() {
    Ok(path) => path,
    Err(_) => return false,
  };

  file_canon.starts_with(std_canon)
}

#[tower_lsp::async_trait]
impl LanguageServer for Server {
  async fn initialize(
    &self,
    params: InitializeParams,
  ) -> Result<InitializeResult> {
    log("[initialize] Server starting...");
    if let Some(client_info) = &params.client_info {
      log(&format!("[initialize] Client: {} {:?}", client_info.name, client_info.version));
    }

    // Store workspace root
    if let Some(root_uri) = params.root_uri {
      log(&format!("[initialize] Root URI: {}", root_uri));
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

        // Inlay hints
        inlay_hint_provider: Some(OneOf::Right(InlayHintServerCapabilities::Options(InlayHintOptions {
          work_done_progress_options: WorkDoneProgressOptions::default(),
          resolve_provider: Some(false),
        }))),

        // Workspace symbol search (Ctrl+T / Cmd+T)
        workspace_symbol_provider: Some(OneOf::Left(true)),

        // Document symbols (outline view, breadcrumbs)
        document_symbol_provider: Some(OneOf::Right(DocumentSymbolOptions {
          label: Some("Ignis Outline".to_string()),
          work_done_progress_options: WorkDoneProgressOptions {
            work_done_progress: Some(false),
          },
        })),

        // Find all references
        references_provider: Some(OneOf::Left(true)),

        // Autocompletion
        completion_provider: Some(CompletionOptions {
          trigger_characters: Some(vec![".".to_string(), ":".to_string()]),
          resolve_provider: Some(false),
          work_done_progress_options: WorkDoneProgressOptions::default(),
          all_commit_characters: None,
          completion_item: None,
        }),

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
          watchers: vec![
            FileSystemWatcher {
              glob_pattern: GlobPattern::String("**/*.ign".to_string()),
              kind: Some(WatchKind::all()),
            },
            FileSystemWatcher {
              glob_pattern: GlobPattern::String("**/ignis.toml".to_string()),
              kind: Some(WatchKind::all()),
            },
          ],
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

    // Handle ignis.toml specially
    if uri.path().ends_with("ignis.toml") {
      if let Ok(path) = uri.to_file_path()
        && let Some(root) = path.parent()
      {
        self.state.project_manager.set_toml_override(root, text).await;
        self.publish_toml_diagnostics(root).await;
        self.reanalyze_project_files(root).await;
      }
      return;
    }

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
    let Some(change) = params.content_changes.into_iter().next() else {
      return;
    };

    // Handle ignis.toml specially
    if uri.path().ends_with("ignis.toml") {
      if let Ok(path) = uri.to_file_path()
        && let Some(root) = path.parent()
      {
        self.state.project_manager.set_toml_override(root, change.text).await;
        self.publish_toml_diagnostics(root).await;
        self.reanalyze_project_files(root).await;
      }
      return;
    }

    self.state.update_document(&uri, version, change.text).await;
    self.analyze_and_publish(&uri).await;
  }

  async fn did_close(
    &self,
    params: DidCloseTextDocumentParams,
  ) {
    let uri = params.text_document.uri;

    // Handle ignis.toml specially
    if uri.path().ends_with("ignis.toml") {
      if let Ok(path) = uri.to_file_path()
        && let Some(root) = path.parent()
      {
        self.state.project_manager.clear_toml_override(root).await;
        // Clear TOML diagnostics and re-analyze with disk version
        self.client.publish_diagnostics(uri, vec![], None).await;
        self.reanalyze_project_files(root).await;
      }
      return;
    }

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
      // Handle ignis.toml changes
      if change.uri.path().ends_with("ignis.toml") {
        if let Ok(path) = change.uri.to_file_path()
          && let Some(root) = path.parent()
        {
          // Only invalidate cache if TOML is not open in editor
          // (open files use in-memory override)
          let has_override = self.state.project_manager.has_toml_override(root).await;

          if !has_override {
            self.state.project_manager.invalidate(root).await;
            self.publish_toml_diagnostics(root).await;
            should_reanalyze = true;
          }
        }
        continue;
      }

      // Handle .ign files
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
    log(&format!("[goto_definition] Request for {}", uri));
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

    let Some(file_analysis) = output.file_analysis(&file_id) else {
      return Ok(None);
    };

    // Convert LSP position to byte offset
    let byte_offset = line_index.offset(position);

    // Check import path strings (navigates to module file)
    for (span, target_file_id) in &file_analysis.import_module_files {
      if span.start.0 <= byte_offset && byte_offset <= span.end.0 {
        let target_file = output.source_map.get(target_file_id);
        let target_uri = match Url::from_file_path(&target_file.path) {
          Ok(u) => u,
          Err(_) => continue,
        };

        let range = Range {
          start: Position { line: 0, character: 0 },
          end: Position { line: 0, character: 0 },
        };

        return Ok(Some(GotoDefinitionResponse::Scalar(Location { uri: target_uri, range })));
      }
    }

    // Check import items
    let import_def = {
      let mut found = None;
      let mut smallest_size = u32::MAX;

      for (span, def_id) in &file_analysis.import_item_defs {
        if span.start.0 <= byte_offset && byte_offset <= span.end.0 {
          let size = span.end.0 - span.start.0;
          if size < smallest_size {
            smallest_size = size;
            found = Some(*def_id);
          }
        }
      }

      found
    };

    // Find which node's span contains this position (only from this file)
    let mut found_node = None;
    let mut smallest_span_size = u32::MAX;

    for (node_id, span) in &file_analysis.node_spans {
      // Check if this span contains the cursor position
      if span.start.0 <= byte_offset && byte_offset <= span.end.0 {
        // Prefer the smallest span that contains the position
        let span_size = span.end.0 - span.start.0;
        if span_size < smallest_span_size {
          smallest_span_size = span_size;
          found_node = Some(*node_id);
        }
      }
    }

    // Prefer import_item_defs if found, otherwise use node_defs/resolved_calls
    let def_id = if let Some(id) = import_def {
      Some(id)
    } else if let Some(node_id) = found_node {
      file_analysis
        .node_defs
        .get(&node_id)
        .or_else(|| file_analysis.resolved_calls.get(&node_id))
        .cloned()
    } else {
      None
    };

    let Some(def_id) = def_id else {
      return Ok(None);
    };

    let def = output.defs.get(&def_id);

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
    log(&format!("[hover] Request for {}", uri));
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

    let Some(file_analysis) = output.file_analysis(&file_id) else {
      return Ok(None);
    };

    // Convert LSP position to byte offset
    let byte_offset = line_index.offset(position);

    // Check if cursor is on a comment token - if so, don't show hover
    // This prevents showing hover for the enclosing item when hovering over doc comments
    {
      let source_text = output.source_map.get(&file_id).text.as_str();
      let mut lexer = ignis_parser::IgnisLexer::new(file_id, source_text);
      lexer.scan_tokens();

      for token in &lexer.tokens {
        if token.span.file != file_id {
          continue;
        }

        let is_comment = matches!(
          token.type_,
          ignis_token::token_types::TokenType::Comment
            | ignis_token::token_types::TokenType::MultiLineComment
            | ignis_token::token_types::TokenType::DocComment
            | ignis_token::token_types::TokenType::InnerDocComment
        );

        if is_comment && token.span.start.0 <= byte_offset && byte_offset <= token.span.end.0 {
          return Ok(None);
        }
      }
    }

    // Check if cursor is on a `@builtin` or `@directive` name
    {
      let source_text = output.source_map.get(&file_id).text.as_str();
      let mut lexer = ignis_parser::IgnisLexer::new(file_id, source_text);
      lexer.scan_tokens();

      if let Some(hover) = resolve_at_item_hover(&lexer.tokens, byte_offset, &file_id, &line_index) {
        return Ok(Some(hover));
      }
    }

    // First, check if cursor is on an import path string (e.g., "std::io")
    for (span, target_file_id) in &file_analysis.import_module_files {
      if span.start.0 <= byte_offset && byte_offset <= span.end.0 {
        let target_file = output.source_map.get(target_file_id);
        let file_path = target_file.path.to_string_lossy();

        // Extract the module path from the source text (the string content)
        let source_text = output.source_map.get(&file_id).text.as_str();
        let path_text = &source_text[span.start.0 as usize..span.end.0 as usize];

        let content = format!("```ignis\nmodule {}\n```\n\n*File: `{}`*", path_text, file_path);
        let range = line_index.span_to_range(span);

        return Ok(Some(Hover {
          contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: content,
          }),
          range: Some(range),
        }));
      }
    }

    // Find which node's span contains this position (only from this file)
    let mut found_node = None;
    let mut smallest_span_size = u32::MAX;

    for (node_id, span) in &file_analysis.node_spans {
      if span.start.0 <= byte_offset && byte_offset <= span.end.0 {
        let span_size = span.end.0 - span.start.0;
        if span_size < smallest_span_size {
          smallest_span_size = span_size;
          found_node = Some(*node_id);
        }
      }
    }

    // Second, check if cursor is on an import item (more specific than the import statement node)
    let import_hover = {
      let mut found_import = None;
      let mut smallest_size = u32::MAX;

      for (span, def_id) in &file_analysis.import_item_defs {
        if span.start.0 <= byte_offset && byte_offset <= span.end.0 {
          let size = span.end.0 - span.start.0;
          if size < smallest_size {
            smallest_size = size;
            found_import = Some((span.clone(), *def_id, size));
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
        if let Some(node_span) = file_analysis.node_spans.get(node_id) {
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
          &output.namespaces,
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
        let content = if let Some(def_id) = file_analysis.node_defs.get(&node_id) {
          format_definition_hover(
            &output.defs,
            &output.types,
            &output.symbol_names,
            &output.source_map,
            &output.namespaces,
            def_id,
          )
        } else if let Some(def_id) = file_analysis.resolved_calls.get(&node_id) {
          // This is a call to an overloaded function - show the resolved overload
          format_definition_hover(
            &output.defs,
            &output.types,
            &output.symbol_names,
            &output.source_map,
            &output.namespaces,
            def_id,
          )
        } else if let Some(type_id) = file_analysis.node_types.get(&node_id) {
          // No definition, just show the type
          format!(
            "```ignis\n{}\n```",
            format_type(&output.types, &output.defs, &output.symbol_names, type_id)
          )
        } else {
          String::new()
        };

        let span = file_analysis.node_spans.get(&node_id).cloned();
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
    log(&format!("[semantic_tokens] request for {}", uri));

    // Get cached analysis or run new analysis
    let Some((output, path_str, _)) = self.get_analysis(uri).await else {
      log("[semantic_tokens] no analysis available");
      return Ok(None);
    };

    // Get text and line_index from open document (use cached line_index)
    let (text, line_index) = {
      let guard = self.state.open_files.read().await;

      let Some(doc) = guard.get(uri) else {
        log("[semantic_tokens] document not open");
        return Ok(None);
      };

      (doc.text.clone(), doc.line_index.clone())
    };

    let Some(file_id) = output.source_map.lookup_by_path(&path_str) else {
      log("[semantic_tokens] file_id not found");
      return Ok(None);
    };

    let Some(file_analysis) = output.file_analysis(&file_id) else {
      log("[semantic_tokens] per-file analysis not found");
      return Ok(None);
    };

    // Wrap sync computation in catch_unwind to prevent panics from killing the server
    let uri_str = uri.to_string();
    let result = catch_unwind(AssertUnwindSafe(|| {
      // Lex the source to get tokens
      let mut lexer = ignis_parser::IgnisLexer::new(file_id, &text);
      lexer.scan_tokens();

      // Classify tokens using semantic analysis
      crate::semantic::classify_tokens(
        &lexer.tokens,
        &file_analysis.import_item_defs,
        &output.defs,
        &file_id,
        &line_index,
      )
    }));

    match result {
      Ok(tokens) => {
        log(&format!("[semantic_tokens] returning {} tokens", tokens.data.len()));
        Ok(Some(SemanticTokensResult::Tokens(tokens)))
      },
      Err(panic_info) => {
        let panic_msg = if let Some(s) = panic_info.downcast_ref::<&str>() {
          s.to_string()
        } else if let Some(s) = panic_info.downcast_ref::<String>() {
          s.clone()
        } else {
          "unknown panic".to_string()
        };
        log(&format!("[semantic_tokens] PANIC: {} | uri={}", panic_msg, uri_str));
        Ok(None)
      },
    }
  }

  async fn inlay_hint(
    &self,
    params: InlayHintParams,
  ) -> Result<Option<Vec<InlayHint>>> {
    let uri = &params.text_document.uri;

    // Check for cached hints first
    {
      let guard = self.state.open_files.read().await;
      if let Some(doc) = guard.get(uri)
        && let Some(hints) = doc.get_cached_hints()
      {
        let range_hints = crate::inlay::filter_hints_by_range(hints, &params.range);
        return Ok(Some(range_hints));
      }
    }

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

    let Some(file) = output.file_analysis(&file_id) else {
      return Ok(None);
    };

    let hints = crate::inlay::generate_hints(file, &output.defs, &output.types, &output.symbol_names, &line_index);

    // Cache the computed hints
    {
      let mut guard = self.state.open_files.write().await;
      if let Some(doc) = guard.get_mut(uri) {
        doc.set_cached_hints(hints.clone());
      }
    }

    let range_hints = crate::inlay::filter_hints_by_range(&hints, &params.range);
    Ok(Some(range_hints))
  }

  async fn symbol(
    &self,
    params: WorkspaceSymbolParams,
  ) -> Result<Option<Vec<SymbolInformation>>> {
    use ignis_type::definition::DefinitionKind;

    let query = &params.query;

    // Empty query = no results (avoids freezing on Ctrl+T with thousands of symbols)
    if query.is_empty() {
      return Ok(Some(vec![]));
    }

    // Get analysis from any open document (all share the same project defs)
    let output = {
      let guard = self.state.open_files.read().await;

      let mut cached = None;
      for doc in guard.values() {
        if let Some(analysis) = doc.get_cached_analysis() {
          cached = Some(analysis);
          break;
        }
      }
      cached
    };

    let Some(output) = output else {
      return Ok(Some(vec![]));
    };

    // Build namespace_id -> name map for container_name
    let namespace_names: HashMap<ignis_type::namespace::NamespaceId, String> = output
      .defs
      .iter()
      .filter_map(|(_, def)| {
        if let DefinitionKind::Namespace(ns_def) = &def.kind {
          let name = output.symbol_names.get(&def.name)?.clone();
          Some((ns_def.namespace_id, name))
        } else {
          None
        }
      })
      .collect();

    // Cache LineIndex per FileId to avoid recreating for each definition
    let mut line_indexes: HashMap<FileId, LineIndex> = HashMap::new();

    // Collect matching symbols with their scores
    let mut matches: Vec<(u8, SymbolInformation)> = output
      .defs
      .iter()
      .filter_map(|(_, def)| {
        let kind = definition_to_symbol_kind(&def.kind)?;
        let name = output.symbol_names.get(&def.name)?;
        let score = match_score(name, query)?;

        // Get file path and build location
        let file = output.source_map.get(&def.span.file);
        let uri = Url::from_file_path(&file.path).ok()?;

        let line_index = line_indexes
          .entry(def.span.file)
          .or_insert_with(|| LineIndex::new(file.text.clone()));
        let range = line_index.span_to_range(&def.span);

        let container_name = def
          .owner_namespace
          .and_then(|ns_id| namespace_names.get(&ns_id).cloned());

        #[allow(deprecated)]
        let symbol = SymbolInformation {
          name: name.clone(),
          kind,
          tags: None,
          deprecated: None,
          location: Location { uri, range },
          container_name,
        };

        Some((score, symbol))
      })
      .collect();

    // Sort by score (lower = better match), then by name
    matches
      .sort_by(|(score_a, sym_a), (score_b, sym_b)| score_a.cmp(score_b).then_with(|| sym_a.name.cmp(&sym_b.name)));

    // Take top 200 results
    let symbols: Vec<SymbolInformation> = matches.into_iter().take(200).map(|(_, s)| s).collect();

    Ok(Some(symbols))
  }

  async fn document_symbol(
    &self,
    params: DocumentSymbolParams,
  ) -> Result<Option<DocumentSymbolResponse>> {
    use ignis_type::definition::DefinitionKind;
    use ignis_type::namespace::NamespaceId;

    let uri = &params.text_document.uri;
    log(&format!("[document_symbol] Request for URI: {}", uri));

    let Some((output, path_str, _)) = self.get_analysis(uri).await else {
      log("[document_symbol] No analysis available");
      return Ok(None);
    };

    let line_index = {
      let guard = self.state.open_files.read().await;

      let Some(doc) = guard.get(uri) else {
        log("[document_symbol] Document not found in open_files");
        return Ok(None);
      };

      doc.line_index.clone()
    };

    let Some(file_id) = output.source_map.lookup_by_path(&path_str) else {
      log(&format!("[document_symbol] Could not resolve path to FileId: {}", path_str));
      return Ok(None);
    };

    log(&format!("[document_symbol] FileId: {:?}", file_id));

    // Build mapping: NamespaceId -> DefinitionId (for namespace definitions in this file)
    let mut ns_to_def: HashMap<NamespaceId, ignis_type::definition::DefinitionId> = HashMap::new();

    for (def_id, def) in output.defs.iter() {
      if def.span.file != file_id {
        continue;
      }

      if let DefinitionKind::Namespace(ns_def) = &def.kind {
        ns_to_def.insert(ns_def.namespace_id, def_id);
      }
    }

    // Group definitions by their owner namespace (only for defs in this file)
    let mut defs_by_namespace: HashMap<Option<NamespaceId>, Vec<ignis_type::definition::DefinitionId>> = HashMap::new();

    for (def_id, def) in output.defs.iter() {
      if def.span.file != file_id {
        continue;
      }

      // Skip internal definitions that shouldn't appear in outline
      match &def.kind {
        DefinitionKind::Variable(_)
        | DefinitionKind::Parameter(_)
        | DefinitionKind::TypeParam(_)
        | DefinitionKind::Placeholder => {
          continue;
        },
        // Fields and variants are handled as children of their parent type
        DefinitionKind::Field(_) | DefinitionKind::Variant(_) => continue,
        // Methods are handled as children of their owner type
        DefinitionKind::Method(_) => continue,
        _ => {},
      }

      defs_by_namespace.entry(def.owner_namespace).or_default().push(def_id);
    }

    // Build document symbols recursively
    let top_level_defs = defs_by_namespace.remove(&None).unwrap_or_default();
    log(&format!(
      "[document_symbol] Found {} top-level definitions",
      top_level_defs.len()
    ));
    let mut visited_namespaces = HashSet::new();

    let symbols: Vec<DocumentSymbol> = top_level_defs
      .into_iter()
      .filter_map(|def_id| {
        build_document_symbol(
          &def_id,
          &output,
          &file_id,
          &line_index,
          &defs_by_namespace,
          &ns_to_def,
          &mut visited_namespaces,
        )
      })
      .collect();

    log(&format!("[document_symbol] Returning {} top-level symbols", symbols.len()));
    Ok(Some(DocumentSymbolResponse::Nested(symbols)))
  }

  async fn references(
    &self,
    params: ReferenceParams,
  ) -> Result<Option<Vec<Location>>> {
    let uri = &params.text_document_position.text_document.uri;
    let position = params.text_document_position.position;
    let include_declaration = params.context.include_declaration;

    let Some((output, path_str, _)) = self.get_analysis(uri).await else {
      return Ok(None);
    };

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

    let Some(file_analysis) = output.file_analysis(&file_id) else {
      return Ok(None);
    };

    let byte_offset = line_index.offset(position);

    // Find the definition at cursor position
    // First check import_item_defs
    let target_def_id = {
      let mut found = None;
      let mut smallest_size = u32::MAX;

      for (span, def_id) in &file_analysis.import_item_defs {
        if span.start.0 <= byte_offset && byte_offset <= span.end.0 {
          let size = span.end.0 - span.start.0;
          if size < smallest_size {
            smallest_size = size;
            found = Some(*def_id);
          }
        }
      }

      found
    };

    // If not found in imports, check node_defs
    let target_def_id = target_def_id.or_else(|| {
      let mut found_node = None;
      let mut smallest_span_size = u32::MAX;

      for (node_id, span) in &file_analysis.node_spans {
        if span.start.0 <= byte_offset && byte_offset <= span.end.0 {
          let span_size = span.end.0 - span.start.0;
          if span_size < smallest_span_size {
            smallest_span_size = span_size;
            found_node = Some(*node_id);
          }
        }
      }

      found_node.and_then(|node_id| {
        file_analysis
          .node_defs
          .get(&node_id)
          .or_else(|| file_analysis.resolved_calls.get(&node_id))
          .cloned()
      })
    });

    let Some(target_def_id) = target_def_id else {
      return Ok(None);
    };

    // Collect all references to this definition
    let mut locations: Vec<Location> = Vec::new();

    // Cache LineIndex per FileId
    let mut line_indexes: HashMap<ignis_type::file::FileId, LineIndex> = HashMap::new();

    // Include the declaration itself if requested
    if include_declaration {
      let def = output.defs.get(&target_def_id);
      let file = output.source_map.get(&def.span.file);

      if let Ok(def_uri) = Url::from_file_path(&file.path) {
        let li = line_indexes
          .entry(def.span.file)
          .or_insert_with(|| LineIndex::new(file.text.clone()));
        let range = li.span_to_range(&def.name_span);
        locations.push(Location { uri: def_uri, range });
      }
    }

    for file_analysis in output.files.values() {
      for (node_id, def_id) in &file_analysis.node_defs {
        if *def_id != target_def_id {
          continue;
        }

        if let Some(span) = file_analysis.node_spans.get(node_id) {
          let file = output.source_map.get(&span.file);

          if let Ok(ref_uri) = Url::from_file_path(&file.path) {
            let li = line_indexes
              .entry(span.file)
              .or_insert_with(|| LineIndex::new(file.text.clone()));
            let range = li.span_to_range(span);
            locations.push(Location { uri: ref_uri, range });
          }
        }
      }

      for (node_id, def_id) in &file_analysis.resolved_calls {
        if *def_id != target_def_id {
          continue;
        }

        if let Some(span) = file_analysis.node_spans.get(node_id) {
          let file = output.source_map.get(&span.file);

          if let Ok(ref_uri) = Url::from_file_path(&file.path) {
            let li = line_indexes
              .entry(span.file)
              .or_insert_with(|| LineIndex::new(file.text.clone()));
            let range = li.span_to_range(span);
            locations.push(Location { uri: ref_uri, range });
          }
        }
      }

      for (span, def_id) in &file_analysis.import_item_defs {
        if *def_id != target_def_id {
          continue;
        }

        let file = output.source_map.get(&span.file);

        if let Ok(ref_uri) = Url::from_file_path(&file.path) {
          let li = line_indexes
            .entry(span.file)
            .or_insert_with(|| LineIndex::new(file.text.clone()));
          let range = li.span_to_range(span);
          locations.push(Location { uri: ref_uri, range });
        }
      }
    }

    if locations.is_empty() {
      Ok(None)
    } else {
      Ok(Some(locations))
    }
  }

  async fn completion(
    &self,
    params: CompletionParams,
  ) -> Result<Option<CompletionResponse>> {
    let uri = &params.text_document_position.text_document.uri;
    let position = params.text_document_position.position;

    // Get document info (text, path, line_index, version)
    let (text, path_str, line_index, version) = {
      let guard = self.state.open_files.read().await;

      let Some(doc) = guard.get(uri) else {
        return Ok(None);
      };

      (
        doc.text.clone(),
        doc.path.to_string_lossy().to_string(),
        doc.line_index.clone(),
        doc.version,
      )
    };

    // Convert position to byte offset, clamped to text length
    let cursor_offset = line_index.offset(position).min(text.len() as u32);

    // Get or compute tokens
    let tokens = {
      let mut guard = self.state.open_files.write().await;

      let Some(doc) = guard.get_mut(uri) else {
        return Ok(None);
      };

      // Check for version mismatch (race condition between did_change and completion)
      if doc.version != version {
        return Ok(None);
      }

      // Get file_id for token computation
      let config = (*self.state.config).clone();
      let temp_output = ignis_driver::analyze_project_with_text(&config, &path_str, Some(doc.text.clone()));

      let Some(file_id) = temp_output.source_map.lookup_by_path(&path_str) else {
        return Ok(None);
      };

      doc.get_or_compute_tokens(&file_id).to_vec()
    };

    // Get analysis for completions (use cache if available, fallback to last good)
    let output = {
      let guard = self.state.open_files.read().await;
      guard.get(uri).and_then(|doc| doc.get_completion_analysis())
    };

    // If no cached analysis, run a fresh one
    let output = match output {
      Some(o) => o,
      None => {
        let config = (*self.state.config).clone();
        let fresh = Arc::new(ignis_driver::analyze_project_with_text(&config, &path_str, Some(text.clone())));

        // Cache it
        {
          let mut guard = self.state.open_files.write().await;
          if let Some(doc) = guard.get_mut(uri)
            && doc.version == version
          {
            doc.set_cached_analysis(version, Arc::clone(&fresh));
          }
        }

        fresh
      },
    };

    // Get file_id for completion
    let Some(file_id) = output.source_map.lookup_by_path(&path_str) else {
      return Ok(None);
    };

    // Clone config for use in sync closure
    let config = (*self.state.config).clone();

    // Wrap sync computation in catch_unwind to prevent panics from killing the server
    let uri_str = uri.to_string();
    let result = catch_unwind(AssertUnwindSafe(|| {
      let context = detect_context(&tokens, cursor_offset, &text)?;

      let candidates = match &context {
        CompletionContext::AfterDot { dot_offset, prefix } => {
          complete_dot(*dot_offset, prefix, &output, &file_id, &tokens, &text)
        },
        CompletionContext::AfterDoubleColon {
          path_segments, prefix, ..
        } => complete_double_colon(path_segments, prefix, &output, &file_id),
        CompletionContext::Identifier { prefix, start_offset } => {
          complete_identifier(prefix, *start_offset, &tokens, Some(&output), &file_id)
        },
        CompletionContext::ImportPath { prefix } => complete_import_path(prefix, &config),
        CompletionContext::RecordInit {
          record_name,
          assigned_fields,
          prefix,
        } => complete_record_init(record_name, assigned_fields, prefix, &output, &file_id),
        CompletionContext::AfterAt { prefix } => complete_at_items(prefix),
      };

      if candidates.is_empty() {
        return None;
      }

      Some(to_completion_items(candidates))
    }));

    match result {
      Ok(Some(items)) => Ok(Some(CompletionResponse::Array(items))),
      Ok(None) => Ok(None),
      Err(panic_info) => {
        let panic_msg = if let Some(s) = panic_info.downcast_ref::<&str>() {
          s.to_string()
        } else if let Some(s) = panic_info.downcast_ref::<String>() {
          s.clone()
        } else {
          "unknown panic".to_string()
        };
        log(&format!(
          "[completion] PANIC: {} | uri={} pos={:?}",
          panic_msg, uri_str, position
        ));
        Ok(None)
      },
    }
  }
}

/// Build a DocumentSymbol for a definition, including children recursively.
///
/// The `visited_namespaces` parameter prevents infinite recursion if namespace
/// hierarchy contains cycles (defensive programming).
#[allow(deprecated)] // DocumentSymbol.deprecated field
fn build_document_symbol(
  def_id: &ignis_type::definition::DefinitionId,
  output: &ignis_driver::AnalyzeProjectOutput,
  file_id: &ignis_type::file::FileId,
  line_index: &crate::convert::LineIndex,
  defs_by_namespace: &HashMap<Option<ignis_type::namespace::NamespaceId>, Vec<ignis_type::definition::DefinitionId>>,
  ns_to_def: &HashMap<ignis_type::namespace::NamespaceId, ignis_type::definition::DefinitionId>,
  visited_namespaces: &mut HashSet<ignis_type::namespace::NamespaceId>,
) -> Option<DocumentSymbol> {
  use ignis_type::definition::DefinitionKind;

  let def = output.defs.get(def_id);
  let name = output.symbol_names.get(&def.name)?.clone();
  let kind = definition_to_doc_symbol_kind(&def.kind)?;
  let range = line_index.span_to_range(&def.span);
  let selection_range = line_index.span_to_range(&def.name_span);

  // Build children based on definition kind
  let children = match &def.kind {
    DefinitionKind::Namespace(ns_def) => {
      // Cycle detection: skip if already visiting this namespace
      if visited_namespaces.contains(&ns_def.namespace_id) {
        return None;
      }
      visited_namespaces.insert(ns_def.namespace_id);

      let mut children = Vec::new();

      // Add sub-namespaces (recursively)
      let ns = output.namespaces.get(&ns_def.namespace_id);
      for child_ns_id in ns.children.values() {
        if let Some(child_def_id) = ns_to_def.get(child_ns_id)
          && let Some(child_sym) = build_document_symbol(
            child_def_id,
            output,
            file_id,
            line_index,
            defs_by_namespace,
            ns_to_def,
            visited_namespaces,
          )
        {
          children.push(child_sym);
        }
      }

      // Add direct definitions in this namespace
      if let Some(ns_defs) = defs_by_namespace.get(&Some(ns_def.namespace_id)) {
        for child_def_id in ns_defs {
          // Skip namespace definitions (handled above via children)
          let child_def = output.defs.get(child_def_id);
          if matches!(child_def.kind, DefinitionKind::Namespace(_)) {
            continue;
          }

          if let Some(child_sym) = build_document_symbol(
            child_def_id,
            output,
            file_id,
            line_index,
            defs_by_namespace,
            ns_to_def,
            visited_namespaces,
          ) {
            children.push(child_sym);
          }
        }
      }

      visited_namespaces.remove(&ns_def.namespace_id);

      if children.is_empty() { None } else { Some(children) }
    },

    DefinitionKind::Record(record_def) => {
      let mut children = Vec::new();

      // Add fields
      for field in &record_def.fields {
        if let Some(field_sym) = build_field_symbol(&field.def_id, output, file_id, line_index) {
          children.push(field_sym);
        }
      }

      // Add instance methods
      for entry in record_def.instance_methods.values() {
        for method_def_id in symbol_entry_to_def_ids(entry) {
          if let Some(method_sym) = build_method_symbol(&method_def_id, output, file_id, line_index) {
            children.push(method_sym);
          }
        }
      }

      // Add static methods
      for entry in record_def.static_methods.values() {
        for method_def_id in symbol_entry_to_def_ids(entry) {
          if let Some(method_sym) = build_method_symbol(&method_def_id, output, file_id, line_index) {
            children.push(method_sym);
          }
        }
      }

      // Add static fields (constants)
      for const_def_id in record_def.static_fields.values() {
        if let Some(const_sym) = build_constant_symbol(const_def_id, output, file_id, line_index) {
          children.push(const_sym);
        }
      }

      if children.is_empty() { None } else { Some(children) }
    },

    DefinitionKind::Enum(enum_def) => {
      let mut children = Vec::new();

      // Add variants
      for variant in &enum_def.variants {
        if let Some(variant_sym) = build_variant_symbol(&variant.def_id, output, file_id, line_index) {
          children.push(variant_sym);
        }
      }

      // Add static methods
      for entry in enum_def.static_methods.values() {
        for method_def_id in symbol_entry_to_def_ids(entry) {
          if let Some(method_sym) = build_method_symbol(&method_def_id, output, file_id, line_index) {
            children.push(method_sym);
          }
        }
      }

      // Add static fields (constants)
      for const_def_id in enum_def.static_fields.values() {
        if let Some(const_sym) = build_constant_symbol(const_def_id, output, file_id, line_index) {
          children.push(const_sym);
        }
      }

      if children.is_empty() { None } else { Some(children) }
    },

    _ => None,
  };

  Some(DocumentSymbol {
    name,
    detail: None,
    kind,
    tags: None,
    deprecated: None,
    range,
    selection_range,
    children,
  })
}

/// Build a DocumentSymbol for a field.
#[allow(deprecated)]
fn build_field_symbol(
  def_id: &ignis_type::definition::DefinitionId,
  output: &ignis_driver::AnalyzeProjectOutput,
  file_id: &ignis_type::file::FileId,
  line_index: &crate::convert::LineIndex,
) -> Option<DocumentSymbol> {
  let def = output.defs.get(def_id);

  // Only include symbols from the current file
  if def.span.file != *file_id {
    return None;
  }

  let name = output.symbol_names.get(&def.name)?.clone();
  let range = line_index.span_to_range(&def.span);
  let selection_range = line_index.span_to_range(&def.name_span);

  Some(DocumentSymbol {
    name,
    detail: None,
    kind: SymbolKind::FIELD,
    tags: None,
    deprecated: None,
    range,
    selection_range,
    children: None,
  })
}

/// Build a DocumentSymbol for a method.
#[allow(deprecated)]
fn build_method_symbol(
  def_id: &ignis_type::definition::DefinitionId,
  output: &ignis_driver::AnalyzeProjectOutput,
  file_id: &ignis_type::file::FileId,
  line_index: &crate::convert::LineIndex,
) -> Option<DocumentSymbol> {
  let def = output.defs.get(def_id);

  // Only include symbols from the current file
  if def.span.file != *file_id {
    return None;
  }

  let name = output.symbol_names.get(&def.name)?.clone();
  let range = line_index.span_to_range(&def.span);
  let selection_range = line_index.span_to_range(&def.name_span);

  Some(DocumentSymbol {
    name,
    detail: None,
    kind: SymbolKind::METHOD,
    tags: None,
    deprecated: None,
    range,
    selection_range,
    children: None,
  })
}

/// Build a DocumentSymbol for an enum variant.
#[allow(deprecated)]
fn build_variant_symbol(
  def_id: &ignis_type::definition::DefinitionId,
  output: &ignis_driver::AnalyzeProjectOutput,
  file_id: &ignis_type::file::FileId,
  line_index: &crate::convert::LineIndex,
) -> Option<DocumentSymbol> {
  let def = output.defs.get(def_id);

  // Only include symbols from the current file
  if def.span.file != *file_id {
    return None;
  }

  let name = output.symbol_names.get(&def.name)?.clone();
  let range = line_index.span_to_range(&def.span);
  let selection_range = line_index.span_to_range(&def.name_span);

  Some(DocumentSymbol {
    name,
    detail: None,
    kind: SymbolKind::ENUM_MEMBER,
    tags: None,
    deprecated: None,
    range,
    selection_range,
    children: None,
  })
}

/// Build a DocumentSymbol for a constant (static field).
#[allow(deprecated)]
fn build_constant_symbol(
  def_id: &ignis_type::definition::DefinitionId,
  output: &ignis_driver::AnalyzeProjectOutput,
  file_id: &ignis_type::file::FileId,
  line_index: &crate::convert::LineIndex,
) -> Option<DocumentSymbol> {
  let def = output.defs.get(def_id);

  // Only include symbols from the current file
  if def.span.file != *file_id {
    return None;
  }

  let name = output.symbol_names.get(&def.name)?.clone();
  let range = line_index.span_to_range(&def.span);
  let selection_range = line_index.span_to_range(&def.name_span);

  Some(DocumentSymbol {
    name,
    detail: None,
    kind: SymbolKind::CONSTANT,
    tags: None,
    deprecated: None,
    range,
    selection_range,
    children: None,
  })
}

/// Convert a SymbolEntry to a list of DefinitionIds.
fn symbol_entry_to_def_ids(entry: &ignis_type::definition::SymbolEntry) -> Vec<ignis_type::definition::DefinitionId> {
  match entry {
    ignis_type::definition::SymbolEntry::Single(id) => vec![*id],
    ignis_type::definition::SymbolEntry::Overload(ids) => ids.clone(),
  }
}

/// Convert DefinitionKind to LSP SymbolKind for document symbols.
fn definition_to_doc_symbol_kind(kind: &ignis_type::definition::DefinitionKind) -> Option<SymbolKind> {
  use ignis_type::definition::DefinitionKind;

  match kind {
    DefinitionKind::Function(_) => Some(SymbolKind::FUNCTION),
    DefinitionKind::Method(_) => Some(SymbolKind::METHOD),
    DefinitionKind::Record(_) => Some(SymbolKind::STRUCT),
    DefinitionKind::Enum(_) => Some(SymbolKind::ENUM),
    DefinitionKind::Namespace(_) => Some(SymbolKind::NAMESPACE),
    DefinitionKind::TypeAlias(_) => Some(SymbolKind::INTERFACE),
    DefinitionKind::Trait(_) => Some(SymbolKind::INTERFACE),
    DefinitionKind::Constant(_) => Some(SymbolKind::CONSTANT),
    DefinitionKind::Field(_) => Some(SymbolKind::FIELD),
    DefinitionKind::Variant(_) => Some(SymbolKind::ENUM_MEMBER),
    // These don't appear in document outline
    DefinitionKind::Variable(_)
    | DefinitionKind::Parameter(_)
    | DefinitionKind::TypeParam(_)
    | DefinitionKind::Placeholder => None,
  }
}

/// Format a type as a human-readable string.
fn format_type(
  types: &ignis_type::types::TypeStore,
  defs: &ignis_type::definition::DefinitionStore,
  symbol_names: &std::collections::HashMap<ignis_type::symbol::SymbolId, String>,
  type_id: &ignis_type::types::TypeId,
) -> String {
  crate::type_format::format_type(types, defs, symbol_names, type_id)
}

/// Check if the cursor is on an `@`-item name and return hover if so.
///
/// Scans tokens for pattern: `@` followed by `Identifier` where cursor is on
/// either the `@` or the identifier.
fn resolve_at_item_hover(
  tokens: &[ignis_token::token::Token],
  byte_offset: u32,
  file_id: &ignis_type::file::FileId,
  line_index: &crate::convert::LineIndex,
) -> Option<Hover> {
  use ignis_token::token_types::TokenType;

  let meaningful: Vec<&ignis_token::token::Token> = tokens
    .iter()
    .filter(|t| {
      t.span.file == *file_id
        && !matches!(
          t.type_,
          TokenType::Whitespace | TokenType::Comment | TokenType::MultiLineComment | TokenType::DocComment
        )
    })
    .collect();

  for (i, tok) in meaningful.iter().enumerate() {
    if tok.type_ == TokenType::Identifier
      && tok.span.start.0 <= byte_offset
      && byte_offset <= tok.span.end.0
      && i > 0
      && meaningful[i - 1].type_ == TokenType::At
      && let Some(item) = crate::at_items::lookup(&tok.lexeme)
    {
      let hover_text = crate::at_items::format_hover(item);
      let at_span = ignis_type::span::Span {
        file: *file_id,
        start: meaningful[i - 1].span.start,
        end: tok.span.end,
      };
      let range = line_index.span_to_range(&at_span);

      return Some(Hover {
        contents: HoverContents::Markup(MarkupContent {
          kind: MarkupKind::Markdown,
          value: hover_text,
        }),
        range: Some(range),
      });
    }

    if tok.type_ == TokenType::At
      && tok.span.start.0 <= byte_offset
      && byte_offset <= tok.span.end.0
      && i + 1 < meaningful.len()
      && meaningful[i + 1].type_ == TokenType::Identifier
    {
      let name_tok = meaningful[i + 1];
      if let Some(item) = crate::at_items::lookup(&name_tok.lexeme) {
        let hover_text = crate::at_items::format_hover(item);
        let at_span = ignis_type::span::Span {
          file: *file_id,
          start: tok.span.start,
          end: name_tok.span.end,
        };
        let range = line_index.span_to_range(&at_span);

        return Some(Hover {
          contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: hover_text,
          }),
          range: Some(range),
        });
      }
    }
  }

  None
}

/// Format a definition for hover display with full signature.
fn format_definition_hover(
  defs: &ignis_type::definition::DefinitionStore,
  types: &ignis_type::types::TypeStore,
  symbol_names: &std::collections::HashMap<ignis_type::symbol::SymbolId, String>,
  source_map: &ignis_type::file::SourceMap,
  namespaces: &ignis_type::namespace::NamespaceStore,
  def_id: &ignis_type::definition::DefinitionId,
) -> String {
  use ignis_type::definition::DefinitionKind;

  let def = defs.get(def_id);
  let name = symbol_names.get(&def.name).cloned().unwrap_or_else(|| "?".to_string());

  // Build the location path for display at the top of hover
  // Prefer namespace path if available, otherwise use file path
  let file = source_map.get(&def.span.file);
  let file_path = &file.path;

  let location_path = if let Some(ns_id) = def.owner_namespace {
    // Use explicit namespace path
    let path_symbols = namespaces.full_path(ns_id);
    let ns_path = path_symbols
      .iter()
      .filter_map(|sym| symbol_names.get(sym))
      .cloned()
      .collect::<Vec<_>>()
      .join("::");
    if ns_path.is_empty() { None } else { Some(ns_path) }
  } else {
    // Fall back to file path - extract meaningful parts
    // For std files like "std/memory/allocator.ign", show "memory/allocator"
    // For project files, show the filename without extension
    let path_str = file_path.to_string_lossy();
    if let Some(std_idx) = path_str.find("/std/") {
      // Extract path after /std/ and remove .ign extension
      let after_std = &path_str[std_idx + 5..];
      let without_ext = after_std.strip_suffix(".ign").unwrap_or(after_std);
      Some(without_ext.replace('/', "::"))
    } else {
      // Just use file stem for project files
      file_path.file_stem().map(|s| s.to_string_lossy().to_string())
    }
  };

  let file_info = file_path.file_name().map(|n| n.to_string_lossy().to_string());

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

    DefinitionKind::Trait(trait_def) => {
      let type_params = format_type_params(&trait_def.type_params, defs, symbol_names);
      let method_count = trait_def.methods.len();
      format!("```ignis\ntrait {}{}\n```\n\n({} methods)", name, type_params, method_count)
    },

    DefinitionKind::Placeholder => {
      format!("`{}`", name)
    },
  };

  // Build the final hover content
  let mut result = String::new();

  // Add location path at the top (like rust-analyzer shows module path)
  if let Some(ref loc_path) = location_path
    && !loc_path.is_empty()
  {
    result.push_str(loc_path);
    result.push_str("\n\n");
  }

  result.push_str(&signature);

  // Add documentation if present
  if let Some(doc) = &def.doc {
    let trimmed = doc.trim();
    if !trimmed.is_empty() {
      result.push_str("\n\n---\n\n");
      result.push_str(trimmed);
    }
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

/// Match score for workspace symbol search.
/// Returns 0 for starts_with, 1 for contains, None for no match.
fn match_score(
  name: &str,
  query: &str,
) -> Option<u8> {
  if query.is_empty() {
    return Some(10);
  }

  let name_lower = name.to_lowercase();
  let query_lower = query.to_lowercase();

  if name_lower.starts_with(&query_lower) {
    Some(0)
  } else if name_lower.contains(&query_lower) {
    Some(1)
  } else {
    None
  }
}

/// Convert DefinitionKind to LSP SymbolKind.
/// Returns None for definitions that shouldn't appear in workspace symbol search.
fn definition_to_symbol_kind(kind: &ignis_type::definition::DefinitionKind) -> Option<SymbolKind> {
  use ignis_type::definition::DefinitionKind;

  match kind {
    DefinitionKind::Function(_) => Some(SymbolKind::FUNCTION),
    DefinitionKind::Method(_) => Some(SymbolKind::METHOD),
    DefinitionKind::Record(_) => Some(SymbolKind::STRUCT),
    DefinitionKind::Enum(_) => Some(SymbolKind::ENUM),
    DefinitionKind::Namespace(_) => Some(SymbolKind::NAMESPACE),
    DefinitionKind::TypeAlias(_) => Some(SymbolKind::INTERFACE),
    DefinitionKind::Constant(_) => Some(SymbolKind::CONSTANT),
    _ => None,
  }
}
