//! LSP server implementation.

use std::collections::{HashMap, HashSet};
use std::panic::{AssertUnwindSafe, catch_unwind};
use std::path::{Path, PathBuf};
use std::sync::Arc;

use ignis_config::{IgnisConfig, IgnisSTDManifest};
use ignis_driver::project::{Project, find_project_root};
use ignis_driver::{AnalysisOptions, AnalyzeProjectOutput};
use ignis_formatter::{
  FormatOptions, FormatterCliOverrides, FormatterConfig, FormatterConfigError, FormatterConfigPaths, format_text,
  load_formatter_config,
};
use ignis_token::token_types::TokenType;
use ignis_type::definition::{DefinitionId, DefinitionKind, Visibility};
use ignis_type::file::FileId;
use ignis_type::span::Span;
use ignis_type::BytePosition;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};
use url::Url;

use crate::completion::{
  CompletionContext, complete_after_pipe, complete_at_items, complete_dot, complete_double_colon, complete_identifier,
  complete_import_path, complete_record_init, detect_context, log, to_completion_items,
};
use crate::convert::{convert_diagnostic, LineIndex};
use crate::project::ProjectContext;
use crate::state::LspState;

#[derive(Clone)]
struct LspFormatDocument {
  text: String,
  path: PathBuf,
  line_index: LineIndex,
}

#[derive(Clone)]
struct LspCodeActionDocument {
  text: String,
  line_index: LineIndex,
}

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

  fn build_analysis_inputs(
    &self,
    path: &Path,
    path_str: &str,
    project_ctx: &ProjectContext,
  ) -> (IgnisConfig, String, Option<Project>) {
    let mut config;
    let mut entry_path = path_str.to_string();
    let mut project_opt: Option<Project> = None;

    match project_ctx {
      ProjectContext::Project(resolved) => {
        config = (*resolved.config).clone();
        entry_path = resolved.project.entry.to_string_lossy().to_string();
        project_opt = Some(resolved.project.clone());
      },
      ProjectContext::NoProject | ProjectContext::Error { .. } => {
        config = (*self.state.config).clone();
      },
    }

    if config.std_path.is_empty()
      && let Some(inferred_std_path) = infer_std_path_from_file(path)
    {
      config.std_path = inferred_std_path;
      config.std = true;
      config.auto_load_std = true;
      config.manifest = load_std_manifest(&config.std_path);
    }

    if is_file_inside_std_path(path, &config.std_path) {
      entry_path = std_module_entry_path(path, &config.std_path).unwrap_or_else(|| path_str.to_string());
      project_opt = None;
    }

    (config, entry_path, project_opt)
  }

  async fn analyze_completion_without_current_override(
    &self,
    current_uri: &Url,
    path: &Path,
    path_str: &str,
  ) -> Option<Arc<AnalyzeProjectOutput>> {
    let project_ctx = self.state.project_manager.project_for_file(path).await;

    let mut file_overrides = self.collect_file_overrides().await;
    file_overrides.retain(|candidate_path, _| {
      if candidate_path == path {
        return false;
      }

      match (candidate_path.canonicalize(), path.canonicalize()) {
        (Ok(candidate), Ok(current)) => candidate != current,
        _ => true,
      }
    });

    if let ProjectContext::Error { error, .. } = &project_ctx {
      log(&format!(
        "[completion fallback] project error while recovering {}: {}",
        current_uri, error
      ));
    }

    let (config, entry_path, project_opt) = self.build_analysis_inputs(path, path_str, &project_ctx);

    let options = AnalysisOptions {
      file_overrides,
      project: project_opt,
    };

    Some(Arc::new(ignis_driver::analyze_project_with_options(
      &config,
      &entry_path,
      options,
    )))
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

    if let ProjectContext::Error { root, error } = &project_ctx {
      self.publish_toml_diagnostics(root).await;
      log(&format!("[analyze] Project error: {}", error));
    }

    let (config, entry_path, project_opt) = self.build_analysis_inputs(&path, &path_str, &project_ctx);

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

    self.state.set_global_last_good_analysis(Arc::clone(&output)).await;

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

    if let ProjectContext::Error { error, .. } = &project_ctx {
      log(&format!("[analysis cache miss] Project error: {}", error));
    }

    let (config, entry_path, project_opt) = self.build_analysis_inputs(&path, &path_str, &project_ctx);

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

    self.state.set_global_last_good_analysis(Arc::clone(&output)).await;

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

fn lsp_server_capabilities() -> ServerCapabilities {
  ServerCapabilities {
    // Full text sync - client sends entire document on change
    text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),

    // Go to definition
    definition_provider: Some(OneOf::Left(true)),

    // Hover (type info)
    hover_provider: Some(HoverProviderCapability::Simple(true)),

    // Full-document formatting only. Range formatting remains unsupported.
    document_formatting_provider: Some(OneOf::Left(true)),

    // QuickFix-only code actions. Currently limited to safe unused single-item import removal.
    code_action_provider: Some(CodeActionProviderCapability::Options(CodeActionOptions {
      code_action_kinds: Some(vec![CodeActionKind::QUICKFIX]),
      resolve_provider: Some(false),
      work_done_progress_options: WorkDoneProgressOptions::default(),
    })),

    // Semantic tokens (semantic highlighting)
    semantic_tokens_provider: Some(SemanticTokensServerCapabilities::SemanticTokensOptions(SemanticTokensOptions {
      legend: crate::semantic::build_legend(),
      full: Some(SemanticTokensFullOptions::Bool(true)),
      range: None,
      work_done_progress_options: WorkDoneProgressOptions::default(),
    })),

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

    // Conservative rename for locals and parameters
    rename_provider: Some(OneOf::Left(true)),

    // Autocompletion
    completion_provider: Some(CompletionOptions {
      trigger_characters: Some(vec![".".to_string(), ":".to_string()]),
      resolve_provider: Some(false),
      work_done_progress_options: WorkDoneProgressOptions::default(),
      all_commit_characters: None,
      completion_item: None,
    }),

    ..Default::default()
  }
}

fn resolve_formatter_config_for_lsp_file(path: &Path) -> std::result::Result<FormatterConfig, FormatterConfigError> {
  let parent = path.parent().unwrap_or_else(|| Path::new("."));
  let project_root = find_project_root(parent).unwrap_or_else(|| parent.to_path_buf());
  let ignis_toml = project_root.join("ignis.toml");
  let dedicated_config = project_root.join("ignisfmt.toml");

  let paths = FormatterConfigPaths {
    project_root,
    ignis_toml: ignis_toml.is_file().then_some(ignis_toml),
    dedicated_config: dedicated_config.is_file().then_some(dedicated_config),
    explicit_config: None,
  };

  load_formatter_config(&paths, &FormatterCliOverrides::default())
}

fn formatting_edits_for_open_document(document: Option<LspFormatDocument>) -> Option<Vec<TextEdit>> {
  let Some(document) = document else {
    return Some(vec![]);
  };

  let result: std::result::Result<std::result::Result<Vec<TextEdit>, String>, _> =
    catch_unwind(AssertUnwindSafe(|| {
      let config = resolve_formatter_config_for_lsp_file(&document.path).map_err(|error| error.to_string())?;
      let formatted =
        format_text(&document.text, &FormatOptions { check: false, config }).map_err(|error| error.to_string())?;

      if formatted == document.text {
        return Ok(vec![]);
      }

      Ok(vec![TextEdit {
        range: document.line_index.full_range(),
        new_text: formatted,
      }])
    }));

  match result {
    Ok(Ok(edits)) => Some(edits),
    Ok(Err(error)) => {
      log(&format!("[formatting] {}", error));
      Some(vec![])
    },
    Err(panic_info) => {
      let panic_msg = if let Some(s) = panic_info.downcast_ref::<&str>() {
        s.to_string()
      } else if let Some(s) = panic_info.downcast_ref::<String>() {
        s.clone()
      } else {
        "unknown panic".to_string()
      };
      log(&format!("[formatting] PANIC: {}", panic_msg));
      Some(vec![])
    },
  }
}

fn code_actions_for_open_document(
  uri: &Url,
  document: Option<LspCodeActionDocument>,
  diagnostics: &[Diagnostic],
) -> Vec<CodeActionOrCommand> {
  if uri
    .to_file_path()
    .ok()
    .and_then(|path| path.extension().map(|extension| extension == "ign"))
    .unwrap_or(false)
    == false
  {
    return vec![];
  }

  let Some(document) = document else {
    return vec![];
  };

  diagnostics
    .iter()
    .filter_map(|diagnostic| remove_unused_import_action(uri, &document, diagnostic))
    .collect()
}

fn remove_unused_import_action(
  uri: &Url,
  document: &LspCodeActionDocument,
  diagnostic: &Diagnostic,
) -> Option<CodeActionOrCommand> {
  if diagnostic.code.as_ref()? != &NumberOrString::String("A0123".to_string()) {
    return None;
  }

  let diagnostic_start = document.line_index.offset(diagnostic.range.start) as usize;
  let diagnostic_end = document.line_index.offset(diagnostic.range.end) as usize;
  if diagnostic_start >= diagnostic_end || diagnostic_end > document.text.len() {
    return None;
  }

  let (line_start, line_end_without_newline, line_end_with_newline) =
    containing_line_bounds(&document.text, diagnostic_start)?;
  if diagnostic_end > line_end_with_newline {
    return None;
  }

  let line = &document.text[line_start..line_end_without_newline];
  let leading_whitespace = line.len() - line.trim_start().len();
  let trimmed = line.trim();
  let trimmed_start = line_start + leading_whitespace;
  if !trimmed.starts_with("import ") || !trimmed.ends_with(';') {
    return None;
  }

  let from_matches: Vec<_> = trimmed.match_indices(" from ").collect();
  if from_matches.len() != 1 {
    return None;
  }

  let from_index = from_matches[0].0;
  let imported_item = trimmed["import ".len()..from_index].trim();
  if imported_item.is_empty()
    || imported_item.contains(',')
    || imported_item.contains('{')
    || imported_item.contains('}')
    || imported_item.split_whitespace().count() != 1
  {
    return None;
  }

  let statement_start = trimmed_start;
  let statement_end = statement_start + trimmed.len();
  if diagnostic_start < statement_start || diagnostic_end > statement_end {
    return None;
  }

  let (start_line, start_character) = document.line_index.line_col_utf16(BytePosition(line_start as u32));
  let (end_line, end_character) = document
    .line_index
    .line_col_utf16(BytePosition(line_end_with_newline as u32));
  let delete_range = Range {
    start: Position {
      line: start_line,
      character: start_character,
    },
    end: Position {
      line: end_line,
      character: end_character,
    },
  };

  let mut changes = HashMap::new();
  changes.insert(
    uri.clone(),
    vec![TextEdit {
      range: delete_range,
      new_text: String::new(),
    }],
  );

  Some(CodeActionOrCommand::CodeAction(CodeAction {
    title: "Remove unused import".to_string(),
    kind: Some(CodeActionKind::QUICKFIX),
    diagnostics: Some(vec![diagnostic.clone()]),
    edit: Some(WorkspaceEdit {
      changes: Some(changes),
      document_changes: None,
      change_annotations: None,
    }),
    is_preferred: None,
    disabled: None,
    command: None,
    data: None,
  }))
}

fn containing_line_bounds(
  text: &str,
  byte_offset: usize,
) -> Option<(usize, usize, usize)> {
  if byte_offset > text.len() || !text.is_char_boundary(byte_offset) {
    return None;
  }

  let line_start = text[..byte_offset].rfind('\n').map_or(0, |index| index + 1);
  let relative_line_end = text[byte_offset..].find('\n');
  let line_end_without_newline = relative_line_end.map_or(text.len(), |index| byte_offset + index);
  let line_end_with_newline = relative_line_end.map_or(line_end_without_newline, |index| byte_offset + index + 1);

  Some((line_start, line_end_without_newline, line_end_with_newline))
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

fn std_module_entry_path(
  file_path: &Path,
  std_path: &str,
) -> Option<String> {
  let std_canon = Path::new(std_path).canonicalize().ok()?;
  let file_canon = file_path.canonicalize().ok()?;

  if !file_canon.starts_with(&std_canon) {
    return None;
  }

  if file_canon.file_name().and_then(|n| n.to_str()) == Some("mod.ign") {
    return Some(file_canon.to_string_lossy().to_string());
  }

  let mut current = file_canon.parent();
  while let Some(dir) = current {
    if !dir.starts_with(&std_canon) {
      break;
    }

    let candidate = dir.join("mod.ign");
    if candidate.exists() {
      return Some(candidate.to_string_lossy().to_string());
    }

    current = dir.parent();
  }

  Some(file_canon.to_string_lossy().to_string())
}

fn lookup_file_id_best_effort(
  output: &AnalyzeProjectOutput,
  path: &Path,
  path_str: &str,
) -> Option<FileId> {
  if let Some(file_id) = output.source_map.lookup_by_path(path_str) {
    return Some(file_id);
  }

  if let Some(file_id) = output.source_map.lookup_by_path(path) {
    return Some(file_id);
  }

  if let Ok(canon_path) = path.canonicalize()
    && let Some(file_id) = output.source_map.lookup_by_path(&canon_path)
  {
    return Some(file_id);
  }

  let target_name = path.file_name().and_then(|name| name.to_str());
  target_name?;

  let mut by_name_matches: Vec<FileId> = output
    .source_map
    .iter_paths()
    .filter_map(|(candidate, file_id)| {
      candidate
        .file_name()
        .and_then(|name| name.to_str())
        .filter(|candidate_name| Some(*candidate_name) == target_name)
        .map(|_| *file_id)
    })
    .collect();

  by_name_matches.sort_by_key(|file_id| file_id.index());
  by_name_matches.dedup();

  if by_name_matches.len() == 1 {
    return by_name_matches.first().copied();
  }

  None
}

fn infer_std_path_from_file(file_path: &Path) -> Option<String> {
  let file_canon = file_path.canonicalize().ok()?;
  let mut current = file_canon.parent();

  while let Some(dir) = current {
    let has_manifest = dir.join("manifest.toml").is_file();
    let has_runtime_header = dir.join("runtime").join("ignis_rt.h").is_file();

    if has_manifest && has_runtime_header {
      return Some(dir.to_string_lossy().to_string());
    }

    current = dir.parent();
  }

  None
}

fn load_std_manifest(std_path: &str) -> IgnisSTDManifest {
  if std_path.is_empty() {
    return IgnisSTDManifest::default();
  }

  let manifest_path = Path::new(std_path).join("manifest.toml");

  std::fs::read_to_string(&manifest_path)
    .ok()
    .and_then(|content| toml::from_str(&content).ok())
    .unwrap_or_default()
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct SymbolOccurrence {
  def_id: DefinitionId,
  span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ReferenceOccurrence {
  def_id: DefinitionId,
  span: Span,
  is_declaration: bool,
}

fn span_contains_offset(
  span: &Span,
  byte_offset: u32,
) -> bool {
  span.start.0 <= byte_offset && byte_offset <= span.end.0
}

fn same_span_range(
  left: &Span,
  right: &Span,
) -> bool {
  left.file == right.file && left.start == right.start && left.end == right.end
}

fn is_identifier_boundary(byte: Option<u8>) -> bool {
  !matches!(byte, Some(b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_'))
}

fn identifier_span_in_candidate(
  output: &AnalyzeProjectOutput,
  candidate: &Span,
  name: &str,
) -> Span {
  if name.is_empty() || candidate.is_empty() {
    return candidate.clone();
  }

  let file = output.source_map.get(&candidate.file);
  let bytes = file.text.as_bytes();
  let start = candidate.start.0 as usize;
  let end = candidate.end.0 as usize;

  if start > end || end > bytes.len() {
    return candidate.clone();
  }

  let name_bytes = name.as_bytes();
  let mut cursor = start;

  while cursor + name_bytes.len() <= end {
    if &bytes[cursor..cursor + name_bytes.len()] == name_bytes {
      let before = cursor.checked_sub(1).and_then(|index| bytes.get(index).copied());
      let after = bytes.get(cursor + name_bytes.len()).copied();

      if is_identifier_boundary(before) && is_identifier_boundary(after) {
        return Span {
          file: candidate.file,
          start: ignis_type::BytePosition(cursor as u32),
          end: ignis_type::BytePosition((cursor + name_bytes.len()) as u32),
        };
      }
    }

    cursor += 1;
  }

  candidate.clone()
}

fn exact_reference_span(
  output: &AnalyzeProjectOutput,
  target: DefinitionId,
  candidate: &Span,
) -> Span {
  let def = output.defs.get(&target);

  if same_span_range(candidate, &def.name_span) {
    return candidate.clone();
  }

  let Some(name) = output.symbol_names.get(&def.name) else {
    return candidate.clone();
  };

  identifier_span_in_candidate(output, candidate, name)
}

fn symbol_at_position(
  output: &AnalyzeProjectOutput,
  file_analysis: &ignis_driver::PerFileAnalysis,
  byte_offset: u32,
) -> Option<SymbolOccurrence> {
  let mut declaration_match = None;
  let mut declaration_size = u32::MAX;

  for (def_id, def) in output.defs.iter() {
    if span_contains_offset(&def.name_span, byte_offset) {
      let size = def.name_span.end.0 - def.name_span.start.0;
      if size < declaration_size {
        declaration_size = size;
        declaration_match = Some(SymbolOccurrence {
          def_id,
          span: def.name_span.clone(),
        });
      }
    }
  }

  if declaration_match.is_some() {
    return declaration_match;
  }

  let mut import_match = None;
  let mut import_size = u32::MAX;

  for (span, def_id) in &file_analysis.import_item_defs {
    if span_contains_offset(span, byte_offset) {
      let size = span.end.0 - span.start.0;
      if size < import_size {
        import_size = size;
        import_match = Some(SymbolOccurrence {
          def_id: *def_id,
          span: span.clone(),
        });
      }
    }
  }

  if import_match.is_some() {
    return import_match;
  }

  let mut node_match = None;
  let mut node_size = u32::MAX;

  for (node_id, span) in &file_analysis.node_spans {
    if span_contains_offset(span, byte_offset) {
      let size = span.end.0 - span.start.0;
      if size < node_size {
        let def_id = file_analysis
          .node_defs
          .get(node_id)
          .or_else(|| file_analysis.resolved_calls.get(node_id))
          .copied();

        if let Some(def_id) = def_id {
          node_size = size;
          node_match = Some(SymbolOccurrence {
            def_id,
            span: exact_reference_span(output, def_id, span),
          });
        }
      }
    }
  }

  node_match
}

fn collect_reference_spans(
  output: &AnalyzeProjectOutput,
  target: DefinitionId,
  include_declaration: bool,
) -> Vec<ReferenceOccurrence> {
  let def = output.defs.get(&target);
  let mut occurrences = Vec::new();

  if include_declaration {
    occurrences.push(ReferenceOccurrence {
      def_id: target,
      span: def.name_span.clone(),
      is_declaration: true,
    });
  }

  for file_analysis in output.files.values() {
    for (span, def_id) in &file_analysis.import_item_defs {
      if *def_id != target {
        continue;
      }

      let exact_span = exact_reference_span(output, target, span);
      let is_declaration = same_span_range(&exact_span, &def.name_span);
      if !include_declaration && is_declaration {
        continue;
      }

      occurrences.push(ReferenceOccurrence {
        def_id: target,
        span: exact_span,
        is_declaration,
      });
    }

    for (node_id, def_id) in file_analysis
      .node_defs
      .iter()
      .chain(file_analysis.resolved_calls.iter())
    {
      if *def_id != target {
        continue;
      }

      let Some(span) = file_analysis.node_spans.get(node_id) else {
        continue;
      };

      let exact_span = exact_reference_span(output, target, span);
      let is_declaration = same_span_range(&exact_span, &def.name_span);
      if !include_declaration && is_declaration {
        continue;
      }

      occurrences.push(ReferenceOccurrence {
        def_id: target,
        span: exact_span,
        is_declaration,
      });
    }
  }

  occurrences.sort_by(|left, right| {
    let left_path = output.source_map.get(&left.span.file).path.clone();
    let right_path = output.source_map.get(&right.span.file).path.clone();

    left_path
      .cmp(&right_path)
      .then_with(|| left.span.start.0.cmp(&right.span.start.0))
      .then_with(|| left.span.end.0.cmp(&right.span.end.0))
  });
  occurrences.dedup_by(|left, right| same_span_range(&left.span, &right.span));

  occurrences
}

fn location_for_span(
  output: &AnalyzeProjectOutput,
  span: &Span,
  line_indexes: &mut HashMap<FileId, LineIndex>,
) -> Option<Location> {
  let file = output.source_map.get(&span.file);
  let uri = Url::from_file_path(&file.path).ok()?;
  let line_index = line_indexes
    .entry(span.file)
    .or_insert_with(|| LineIndex::new(file.text.clone()));

  Some(Location {
    uri,
    range: line_index.span_to_range(span),
  })
}

fn collect_reference_locations(
  output: &AnalyzeProjectOutput,
  target: DefinitionId,
  include_declaration: bool,
) -> Vec<Location> {
  let mut line_indexes = HashMap::new();

  collect_reference_spans(output, target, include_declaration)
    .into_iter()
    .filter_map(|occurrence| location_for_span(output, &occurrence.span, &mut line_indexes))
    .collect()
}

fn is_real_file_span(span: &Span) -> bool {
  span.file != FileId::SYNTHETIC
}

fn is_std_or_virtual_file_path(path: &Path) -> bool {
  if path.to_string_lossy().starts_with('<') {
    return true;
  }

  path.components().any(|component| component.as_os_str() == "std")
}

fn can_rename_definition(
  output: &AnalyzeProjectOutput,
  def_id: DefinitionId,
) -> bool {
  let def = output.defs.get(&def_id);

  if def.visibility != Visibility::Private || !is_real_file_span(&def.name_span) || def.name_span.is_empty() {
    return false;
  }

  if !matches!(def.kind, DefinitionKind::Variable(_) | DefinitionKind::Parameter(_)) {
    return false;
  }

  let file = output.source_map.get(&def.name_span.file);
  !is_std_or_virtual_file_path(&file.path)
}

fn validate_rename_name(name: &str) -> bool {
  let mut chars = name.chars();

  let Some(first) = chars.next() else {
    return false;
  };

  if !(first == '_' || first.is_ascii_alphabetic()) {
    return false;
  }

  if !chars.all(|character| character == '_' || character.is_ascii_alphanumeric()) {
    return false;
  }

  TokenType::get_keyword_from_string(name).is_none()
}

fn span_text_matches_name(
  file_text: &str,
  span: &Span,
  name: &str,
) -> bool {
  let start = span.start.0 as usize;
  let end = span.end.0 as usize;

  start <= end && end <= file_text.len() && file_text.get(start..end) == Some(name)
}

fn rename_workspace_edit(
  output: &AnalyzeProjectOutput,
  target: DefinitionId,
  new_name: &str,
) -> Option<WorkspaceEdit> {
  if !validate_rename_name(new_name) || !can_rename_definition(output, target) {
    return None;
  }

  let def = output.defs.get(&target);
  let current_name = output.symbol_names.get(&def.name)?;
  let mut line_indexes = HashMap::new();
  let mut changes: HashMap<Url, Vec<TextEdit>> = HashMap::new();

  for occurrence in collect_reference_spans(output, target, true) {
    if !is_real_file_span(&occurrence.span) {
      return None;
    }

    let file = output.source_map.get(&occurrence.span.file);
    if is_std_or_virtual_file_path(&file.path) || !span_text_matches_name(&file.text, &occurrence.span, current_name) {
      return None;
    }

    let uri = Url::from_file_path(&file.path).ok()?;
    let line_index = line_indexes
      .entry(occurrence.span.file)
      .or_insert_with(|| LineIndex::new(file.text.clone()));

    changes.entry(uri).or_default().push(TextEdit {
      range: line_index.span_to_range(&occurrence.span),
      new_text: new_name.to_string(),
    });
  }

  if changes.is_empty() {
    return None;
  }

  Some(WorkspaceEdit {
    changes: Some(changes),
    document_changes: None,
    change_annotations: None,
  })
}

#[cfg(test)]
#[allow(clippy::items_after_test_module)]
mod tests {
  use super::{
    LspCodeActionDocument, LspFormatDocument, can_rename_definition, code_actions_for_open_document,
    collect_reference_locations, collect_reference_spans, document_symbols_for_analysis,
    formatting_edits_for_open_document, infer_std_path_from_file, load_std_manifest, lsp_server_capabilities,
    rename_workspace_edit, std_module_entry_path, symbol_at_position, validate_rename_name,
    workspace_symbols_for_analysis,
  };
  use std::collections::{HashMap, HashSet};
  use std::path::PathBuf;
  use std::time::{SystemTime, UNIX_EPOCH};

  use ignis_config::IgnisConfig;
  use ignis_driver::{AnalysisOptions, AnalyzeProjectOutput};
  use ignis_type::file::FileId;
  use tower_lsp::lsp_types::{
    CodeActionOrCommand, CodeActionProviderCapability, Diagnostic, Location, NumberOrString, OneOf, Position, Range,
    SymbolKind, TextEdit, WorkspaceEdit,
  };
  use url::Url;

  use crate::convert::LineIndex;

  fn unique_temp_dir(name: &str) -> PathBuf {
    let timestamp = SystemTime::now()
      .duration_since(UNIX_EPOCH)
      .expect("system time should be after epoch")
      .as_nanos();

    std::env::temp_dir().join(format!("ignis_lsp_{}_{}_{}", name, std::process::id(), timestamp))
  }

  fn analyze_temp_source(
    name: &str,
    source: &str,
  ) -> (PathBuf, PathBuf, AnalyzeProjectOutput, FileId) {
    let root = unique_temp_dir(name);
    let file_path = root.join("main.ign");
    std::fs::create_dir_all(&root).expect("test root should be created");
    std::fs::write(&file_path, source).expect("test source should be written");

    let config = IgnisConfig::default();
    let output =
      ignis_driver::analyze_project_with_options(&config, &file_path.to_string_lossy(), AnalysisOptions::default());
    assert!(output.diagnostics.is_empty(), "test source should analyze cleanly");

    let file_id = output
      .source_map
      .lookup_by_path(&file_path)
      .expect("analyzed file should be in source map");

    (root, file_path, output, file_id)
  }

  fn analyze_temp_sources(
    name: &str,
    files: &[(&str, &str)],
    entry: &str,
  ) -> (PathBuf, HashMap<String, PathBuf>, AnalyzeProjectOutput) {
    let root = unique_temp_dir(name);
    std::fs::create_dir_all(&root).expect("test root should be created");

    let mut paths = HashMap::new();
    for (relative, source) in files {
      let file_path = root.join(relative);
      if let Some(parent) = file_path.parent() {
        std::fs::create_dir_all(parent).expect("test file parent should be created");
      }
      std::fs::write(&file_path, source).expect("test source should be written");
      paths.insert((*relative).to_string(), file_path);
    }

    let entry_path = root.join(entry);
    let config = IgnisConfig::default();
    let output =
      ignis_driver::analyze_project_with_options(&config, &entry_path.to_string_lossy(), AnalysisOptions::default());
    assert!(output.diagnostics.is_empty(), "test sources should analyze cleanly");

    (root, paths, output)
  }

  fn repo_std_path() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
      .parent()
      .and_then(|crates_dir| crates_dir.parent())
      .expect("ignis_lsp should live under crates/ignis_lsp")
      .join("std")
  }

  fn analyze_temp_source_with_std(
    name: &str,
    source: &str,
  ) -> (PathBuf, PathBuf, AnalyzeProjectOutput, FileId) {
    let root = unique_temp_dir(name);
    let file_path = root.join("main.ign");
    std::fs::create_dir_all(&root).expect("test root should be created");
    std::fs::write(&file_path, source).expect("test source should be written");

    let std_path = repo_std_path();
    let mut config = IgnisConfig {
      std_path: std_path.to_string_lossy().to_string(),
      std: true,
      auto_load_std: true,
      ..IgnisConfig::default()
    };
    config.manifest = load_std_manifest(&config.std_path);

    let output =
      ignis_driver::analyze_project_with_options(&config, &file_path.to_string_lossy(), AnalysisOptions::default());
    assert!(output.diagnostics.is_empty(), "std import source should analyze cleanly");

    let file_id = output
      .source_map
      .lookup_by_path(&file_path)
      .expect("analyzed file should be in source map");

    (root, file_path, output, file_id)
  }

  fn analyze_temp_source_with_override(
    name: &str,
    disk_source: &str,
    override_source: &str,
  ) -> (PathBuf, PathBuf, AnalyzeProjectOutput, FileId) {
    let root = unique_temp_dir(name);
    let file_path = root.join("main.ign");
    std::fs::create_dir_all(&root).expect("test root should be created");
    std::fs::write(&file_path, disk_source).expect("disk source should be written");

    let mut file_overrides = HashMap::new();
    file_overrides.insert(file_path.clone(), override_source.to_string());

    let config = IgnisConfig::default();
    let output = ignis_driver::analyze_project_with_options(
      &config,
      &file_path.to_string_lossy(),
      AnalysisOptions {
        file_overrides,
        project: None,
      },
    );
    assert!(output.diagnostics.is_empty(), "override source should analyze cleanly");

    let file_id = output
      .source_map
      .lookup_by_path(&file_path)
      .expect("analyzed override file should be in source map");

    (root, file_path, output, file_id)
  }

  fn references_at_marker(
    output: &AnalyzeProjectOutput,
    file_id: FileId,
    marker_source: &str,
    marker: &str,
    include_declaration: bool,
  ) -> Vec<Location> {
    let byte_offset = marker_source
      .find(marker)
      .unwrap_or_else(|| panic!("marker {marker:?} should exist")) as u32;
    let file_analysis = output.file_analysis(&file_id).expect("file analysis should exist");
    let symbol = symbol_at_position(output, file_analysis, byte_offset).expect("symbol should resolve at marker");
    collect_reference_locations(output, symbol.def_id, include_declaration)
  }

  fn reference_texts(
    output: &AnalyzeProjectOutput,
    file_id: FileId,
    marker_source: &str,
    marker: &str,
    include_declaration: bool,
  ) -> Vec<String> {
    let byte_offset = marker_source
      .find(marker)
      .unwrap_or_else(|| panic!("marker {marker:?} should exist")) as u32;
    let file_analysis = output.file_analysis(&file_id).expect("file analysis should exist");
    let symbol = symbol_at_position(output, file_analysis, byte_offset).expect("symbol should resolve at marker");

    collect_reference_spans(output, symbol.def_id, include_declaration)
      .into_iter()
      .map(|occurrence| {
        let file = output.source_map.get(&occurrence.span.file);
        file.text[occurrence.span.start.0 as usize..occurrence.span.end.0 as usize].to_string()
      })
      .collect()
  }

  fn assert_unique_locations(locations: &[Location]) {
    let mut seen = HashSet::new();

    for location in locations {
      let key = (
        location.uri.clone(),
        location.range.start.line,
        location.range.start.character,
        location.range.end.line,
        location.range.end.character,
      );
      assert!(seen.insert(key), "reference locations should be deduped");
    }
  }

  fn rename_edit_at_marker(
    output: &AnalyzeProjectOutput,
    file_id: FileId,
    marker_source: &str,
    marker: &str,
    new_name: &str,
  ) -> Option<WorkspaceEdit> {
    let byte_offset = marker_source
      .find(marker)
      .unwrap_or_else(|| panic!("marker {marker:?} should exist")) as u32;
    let file_analysis = output.file_analysis(&file_id).expect("file analysis should exist");
    let symbol = symbol_at_position(output, file_analysis, byte_offset)?;
    rename_workspace_edit(output, symbol.def_id, new_name)
  }

  fn edits_for_file(
    edit: &WorkspaceEdit,
    file_path: &PathBuf,
  ) -> Vec<TextEdit> {
    let uri = Url::from_file_path(file_path).expect("test path should convert to URI");
    edit
      .changes
      .as_ref()
      .and_then(|changes| changes.get(&uri))
      .cloned()
      .unwrap_or_default()
  }

  #[test]
  fn test_references_helpers_honor_include_declaration_for_local_variables_and_parameters() {
    let source = "function add(value: i32): i32 {\n  let total: i32 = value + value;\n  return total;\n}\n";
    let (root, _, output, file_id) = analyze_temp_source("references_include_declaration", source);

    let local_without_declaration = reference_texts(&output, file_id, source, "total;", false);
    assert_eq!(local_without_declaration, vec!["total"]);

    let local_with_declaration = reference_texts(&output, file_id, source, "total;", true);
    assert_eq!(local_with_declaration, vec!["total", "total"]);

    let parameter_without_declaration = reference_texts(&output, file_id, source, "value +", false);
    assert_eq!(parameter_without_declaration, vec!["value", "value"]);

    let parameter_with_declaration = reference_texts(&output, file_id, source, "value +", true);
    assert_eq!(parameter_with_declaration, vec!["value", "value", "value"]);

    let _ = std::fs::remove_dir_all(root);
  }

  #[test]
  fn test_references_helpers_dedupe_and_return_exact_identifier_locations() {
    let source = "function echo(value: i32): i32 {\n  let valueCopy: i32 = value;\n  return echo(valueCopy);\n}\n";
    let (root, file_path, output, file_id) = analyze_temp_source("references_exact_ranges", source);

    let locations = references_at_marker(&output, file_id, source, "valueCopy);", true);
    assert_eq!(
      locations.len(),
      2,
      "declaration and call argument should be returned exactly once"
    );
    assert_unique_locations(&locations);

    let uri = Url::from_file_path(&file_path).expect("test path should convert to URI");
    assert_eq!(locations[0].uri, uri);
    assert_eq!(locations[0].range, Range::new(Position::new(1, 6), Position::new(1, 15)));
    assert_eq!(locations[1].range, Range::new(Position::new(2, 14), Position::new(2, 23)));

    let function_locations = references_at_marker(&output, file_id, source, "echo(valueCopy", false);
    assert_eq!(
      function_locations.len(),
      1,
      "call reference should be returned without declaration"
    );
    assert_eq!(
      function_locations[0].range,
      Range::new(Position::new(2, 9), Position::new(2, 13))
    );

    let _ = std::fs::remove_dir_all(root);
  }

  #[test]
  fn test_references_helpers_use_open_buffer_file_overrides() {
    let disk_source = "function main(): i32 {\n  let value: i32 = 1;\n  return value;\n}\n";
    let override_source = "function main(): i32 {\n  let value: i32 = 1;\n  return value + value;\n}\n";
    let (root, _, output, file_id) =
      analyze_temp_source_with_override("references_open_buffer_override", disk_source, override_source);

    let texts = reference_texts(&output, file_id, override_source, "value +", true);
    assert_eq!(texts, vec!["value", "value", "value"]);

    let _ = std::fs::remove_dir_all(root);
  }

  #[test]
  fn test_references_helpers_collect_multi_file_workspace_references() {
    let helper_source = "export function helper(value: i32): i32 {\n  return value;\n}\n";
    let main_source =
      "import helper from \"./helper\";\n\nfunction main(): i32 {\n  return helper(1) + helper(2);\n}\n";
    let (root, paths, output) = analyze_temp_sources(
      "references_multi_file",
      &[("helper.ign", helper_source), ("main.ign", main_source)],
      "main.ign",
    );
    let main_path = paths.get("main.ign").expect("main path should exist");
    let helper_path = paths.get("helper.ign").expect("helper path should exist");
    let main_file_id = output
      .source_map
      .lookup_by_path(main_path)
      .expect("main file should be in source map");

    let locations = references_at_marker(&output, main_file_id, main_source, "helper(1)", true);
    assert_eq!(locations.len(), 4, "declaration, import, and two calls should be returned");
    assert_unique_locations(&locations);

    let helper_uri = Url::from_file_path(helper_path).expect("helper path should convert to URI");
    let main_uri = Url::from_file_path(main_path).expect("main path should convert to URI");
    assert_eq!(locations[0].uri, helper_uri);
    assert_eq!(locations[0].range, Range::new(Position::new(0, 16), Position::new(0, 22)));
    assert_eq!(locations[1].uri, main_uri);
    assert_eq!(locations[1].range, Range::new(Position::new(0, 7), Position::new(0, 13)));
    assert_eq!(locations[2].range, Range::new(Position::new(3, 9), Position::new(3, 15)));
    assert_eq!(locations[3].range, Range::new(Position::new(3, 21), Position::new(3, 27)));

    let without_declaration = references_at_marker(&output, main_file_id, main_source, "helper(1)", false);
    assert_eq!(
      without_declaration.len(),
      3,
      "import and two calls should remain without declaration"
    );

    let _ = std::fs::remove_dir_all(root);
  }

  #[test]
  fn test_rename_helpers_return_exact_edits_for_local_variables_and_parameters() {
    let source = "function add(value: i32): i32 {\n  let total: i32 = value + value;\n  return total;\n}\n";
    let (root, file_path, output, file_id) = analyze_temp_source("rename_locals_parameters", source);

    let local_edit = rename_edit_at_marker(&output, file_id, source, "total;", "sum")
      .expect("local variable rename should produce an edit");
    let local_edits = edits_for_file(&local_edit, &file_path);
    assert_eq!(local_edits.len(), 2, "local declaration and reference should be edited");
    assert_eq!(local_edits[0].range, Range::new(Position::new(1, 6), Position::new(1, 11)));
    assert_eq!(local_edits[1].range, Range::new(Position::new(2, 9), Position::new(2, 14)));
    assert!(local_edits.iter().all(|edit| edit.new_text == "sum"));

    let parameter_edit = rename_edit_at_marker(&output, file_id, source, "value +", "input")
      .expect("parameter rename should produce an edit");
    let parameter_edits = edits_for_file(&parameter_edit, &file_path);
    assert_eq!(
      parameter_edits.len(),
      3,
      "parameter declaration and both references should be edited"
    );
    assert_eq!(parameter_edits[0].range, Range::new(Position::new(0, 13), Position::new(0, 18)));
    assert_eq!(parameter_edits[1].range, Range::new(Position::new(1, 19), Position::new(1, 24)));
    assert_eq!(parameter_edits[2].range, Range::new(Position::new(1, 27), Position::new(1, 32)));
    assert!(parameter_edits.iter().all(|edit| edit.new_text == "input"));

    let _ = std::fs::remove_dir_all(root);
  }

  #[test]
  fn test_rename_helpers_reject_unsupported_targets_without_edits() {
    let source = "record User {\n  public name: i32;\n}\n\nenum Status {\n  READY,\n}\n\nexport function identity(value: i32): i32 {\n  return value;\n}\n";
    let (root, _, output, file_id) = analyze_temp_source("rename_reject_unsupported", source);
    let file_analysis = output.file_analysis(&file_id).expect("file analysis should exist");

    for marker in ["User", "name", "READY", "identity"] {
      let byte_offset = source
        .find(marker)
        .unwrap_or_else(|| panic!("marker {marker:?} should exist")) as u32;
      let symbol = symbol_at_position(&output, file_analysis, byte_offset).expect("unsupported symbol should resolve");
      assert!(
        !can_rename_definition(&output, symbol.def_id),
        "{marker:?} should be rejected by conservative rename policy"
      );
      assert_eq!(rename_workspace_edit(&output, symbol.def_id, "renamed"), None);
    }

    assert_eq!(rename_edit_at_marker(&output, file_id, source, "record", "renamed"), None);

    let _ = std::fs::remove_dir_all(root);
  }

  #[test]
  fn test_rename_helpers_reject_imports_and_std_symbols_without_edits() {
    let helper_source = "export function helper(value: i32): i32 {\n  return value;\n}\n";
    let main_source = "import helper from \"./helper\";\n\nfunction main(): i32 {\n  return helper(1);\n}\n";
    let (root, paths, output) = analyze_temp_sources(
      "rename_reject_imports",
      &[("helper.ign", helper_source), ("main.ign", main_source)],
      "main.ign",
    );
    let main_path = paths.get("main.ign").expect("main path should exist");
    let main_file_id = output
      .source_map
      .lookup_by_path(main_path)
      .expect("main file should be in source map");

    assert_eq!(
      rename_edit_at_marker(&output, main_file_id, main_source, "helper from", "renamed"),
      None,
      "import items should not be renamed by the conservative rename policy"
    );

    let _ = std::fs::remove_dir_all(root);

    let std_source = "import Io from \"std::io\";\n\nfunction main(): void {\n  Io::println(\"hello\");\n}\n";
    let (std_root, _, std_output, std_file_id) = analyze_temp_source_with_std("rename_reject_std", std_source);
    assert_eq!(
      rename_edit_at_marker(&std_output, std_file_id, std_source, "Io from", "Output"),
      None,
      "std import symbols should not be renamed"
    );
    assert_eq!(
      rename_edit_at_marker(&std_output, std_file_id, std_source, "println", "printLine"),
      None,
      "std member references should not be renamed"
    );

    let _ = std::fs::remove_dir_all(std_root);
  }

  #[test]
  fn test_rename_helpers_reject_invalid_names_and_leave_same_spelling_symbols_unrelated() {
    assert!(validate_rename_name("renamed_value"));
    for invalid in ["", "1value", "value-name", "function"] {
      assert!(
        !validate_rename_name(invalid),
        "{invalid:?} should not be a valid rename target"
      );
    }

    let source = "function first(value: i32): i32 {\n  return value;\n}\n\nfunction second(value: i32): i32 {\n  return value;\n}\n";
    let (root, file_path, output, file_id) = analyze_temp_source("rename_same_spelling", source);

    let edit = rename_edit_at_marker(&output, file_id, source, "value;\n}\n\nfunction", "left")
      .expect("parameter rename should produce an edit");
    let edits = edits_for_file(&edit, &file_path);
    assert_eq!(edits.len(), 2, "only the first parameter declaration and use should be edited");
    assert_eq!(edits[0].range, Range::new(Position::new(0, 15), Position::new(0, 20)));
    assert_eq!(edits[1].range, Range::new(Position::new(1, 9), Position::new(1, 14)));
    assert!(edits.iter().all(|edit| edit.new_text == "left"));

    assert_eq!(
      rename_edit_at_marker(&output, file_id, source, "value;\n}\n\nfunction", "1value"),
      None
    );

    let _ = std::fs::remove_dir_all(root);
  }

  #[test]
  fn test_infer_std_path_from_file_finds_manifest_root() {
    let root = unique_temp_dir("infer_std");
    let std_root = root.join("std");
    let file_path = std_root.join("fs").join("metadata.ign");

    std::fs::create_dir_all(std_root.join("runtime")).expect("runtime directory should be created");
    std::fs::create_dir_all(file_path.parent().expect("file path should have parent"))
      .expect("fs directory should be created");
    std::fs::write(std_root.join("manifest.toml"), "[modules]\nfs = \"fs/mod.ign\"\n")
      .expect("manifest should be written");
    std::fs::write(std_root.join("runtime").join("ignis_rt.h"), "// rt").expect("runtime header should be written");
    std::fs::write(std_root.join("fs").join("mod.ign"), "").expect("mod file should be written");
    std::fs::write(&file_path, "").expect("metadata file should be written");

    let inferred = infer_std_path_from_file(&file_path);
    assert_eq!(inferred, Some(std_root.to_string_lossy().to_string()));

    let _ = std::fs::remove_dir_all(root);
  }

  #[test]
  fn test_std_module_entry_path_returns_nearest_mod_file() {
    let root = unique_temp_dir("std_entry");
    let std_root = root.join("std");
    let fs_dir = std_root.join("fs");
    let file_path = fs_dir.join("metadata.ign");
    let mod_path = fs_dir.join("mod.ign");

    std::fs::create_dir_all(&fs_dir).expect("fs directory should be created");
    std::fs::write(&mod_path, "").expect("mod file should be written");
    std::fs::write(&file_path, "").expect("metadata file should be written");

    let entry = std_module_entry_path(&file_path, &std_root.to_string_lossy());
    assert_eq!(entry, Some(mod_path.to_string_lossy().to_string()));

    let _ = std::fs::remove_dir_all(root);
  }

  #[test]
  fn test_lsp_capabilities_enable_full_document_formatting_only() {
    let capabilities = lsp_server_capabilities();

    assert_eq!(capabilities.document_formatting_provider, Some(OneOf::Left(true)));
    assert_eq!(capabilities.document_range_formatting_provider, None);
    assert_eq!(capabilities.rename_provider, Some(OneOf::Left(true)));
    assert!(capabilities.document_symbol_provider.is_some());
    assert_eq!(capabilities.workspace_symbol_provider, Some(OneOf::Left(true)));

    let code_action_options = match capabilities.code_action_provider {
      Some(CodeActionProviderCapability::Options(options)) => options,
      other => panic!("expected quickfix code action options, got {other:?}"),
    };
    assert_eq!(code_action_options.resolve_provider, Some(false));
    assert_eq!(
      code_action_options.code_action_kinds,
      Some(vec![tower_lsp::lsp_types::CodeActionKind::QUICKFIX])
    );
  }

  fn diagnostic_with_code(
    code: &str,
    range: Range,
  ) -> Diagnostic {
    Diagnostic {
      range,
      code: Some(NumberOrString::String(code.to_string())),
      message: "unused import".to_string(),
      ..Diagnostic::default()
    }
  }

  fn code_action_document(text: &str) -> LspCodeActionDocument {
    LspCodeActionDocument {
      text: text.to_string(),
      line_index: LineIndex::new(text.to_string()),
    }
  }

  fn extract_single_code_action(actions: Vec<CodeActionOrCommand>) -> tower_lsp::lsp_types::CodeAction {
    assert_eq!(actions.len(), 1, "expected exactly one code action");
    match actions.into_iter().next().expect("action should exist") {
      CodeActionOrCommand::CodeAction(action) => action,
      CodeActionOrCommand::Command(command) => panic!("expected code action, got command {command:?}"),
    }
  }

  #[test]
  fn test_code_action_removes_safe_single_item_unused_import() {
    let uri = Url::parse("file:///tmp/main.ign").expect("test URI should parse");
    let text = "import Foo from \"./foo\";\nfunction main(): void {\n  return;\n}\n";
    let diagnostics = vec![diagnostic_with_code(
      "A0123",
      Range {
        start: Position { line: 0, character: 7 },
        end: Position { line: 0, character: 10 },
      },
    )];

    let action = extract_single_code_action(code_actions_for_open_document(
      &uri,
      Some(code_action_document(text)),
      &diagnostics,
    ));

    assert_eq!(action.title, "Remove unused import");
    assert_eq!(action.kind, Some(tower_lsp::lsp_types::CodeActionKind::QUICKFIX));
    assert_eq!(action.diagnostics, Some(diagnostics));
    let changes = action
      .edit
      .expect("quickfix should include an edit")
      .changes
      .expect("quickfix should use workspace changes");
    assert_eq!(changes.len(), 1);
    let edits = changes.get(&uri).expect("edit should target requested URI");
    assert_eq!(edits.len(), 1);
    assert_eq!(
      edits[0].range,
      Range {
        start: Position { line: 0, character: 0 },
        end: Position { line: 1, character: 0 },
      }
    );
    assert_eq!(edits[0].new_text, "");
  }

  #[test]
  fn test_code_action_returns_no_action_for_unsafe_cases() {
    let uri = Url::parse("file:///tmp/main.ign").expect("test URI should parse");
    let single_item_text = "import Foo from \"./foo\";\nfunction main(): void {}\n";
    let multi_item_text = "import Foo, Bar from \"./foo\";\nfunction main(): void {}\n";
    let matching_a0123 = diagnostic_with_code(
      "A0123",
      Range {
        start: Position { line: 0, character: 7 },
        end: Position { line: 0, character: 10 },
      },
    );

    let unsafe_cases = vec![
      (
        "multi-item imports are ambiguous",
        Some(code_action_document(multi_item_text)),
        vec![matching_a0123.clone()],
      ),
      (
        "stale ranges outside the import are ignored",
        Some(code_action_document(single_item_text)),
        vec![diagnostic_with_code(
          "A0123",
          Range {
            start: Position { line: 1, character: 9 },
            end: Position { line: 1, character: 13 },
          },
        )],
      ),
      ("missing open document is ignored", None, vec![matching_a0123.clone()]),
      (
        "missing request diagnostics are ignored",
        Some(code_action_document(single_item_text)),
        vec![],
      ),
      (
        "non-A0123 diagnostics are ignored",
        Some(code_action_document(single_item_text)),
        vec![diagnostic_with_code(
          "A0456",
          Range {
            start: Position { line: 0, character: 7 },
            end: Position { line: 0, character: 10 },
          },
        )],
      ),
    ];

    for (name, document, diagnostics) in unsafe_cases {
      let actions = code_actions_for_open_document(&uri, document, &diagnostics);
      assert!(actions.is_empty(), "{name} should not produce code actions");
    }

    let unsupported_uri = Url::parse("file:///tmp/main.txt").expect("test URI should parse");
    let unsupported_actions = code_actions_for_open_document(
      &unsupported_uri,
      Some(code_action_document(single_item_text)),
      &[matching_a0123],
    );
    assert!(
      unsupported_actions.is_empty(),
      "unsupported file extensions should not produce code actions"
    );
  }

  #[test]
  fn test_document_symbols_return_nested_symbols_in_source_order() {
    let source = "namespace Tools {\n  record Point {\n    public x: i32;\n    public y: i32;\n  }\n\n  enum Mode {\n    FAST,\n    SLOW,\n  }\n\n  function make(): i32 {\n    return 1;\n  }\n}\n\nfunction main(): i32 {\n  return Tools::make();\n}\n";
    let (root, file_path, output, _) = analyze_temp_source("document_symbols_nested", source);
    let line_index = LineIndex::new(source.to_string());

    let symbols = document_symbols_for_analysis(&output, &file_path.to_string_lossy(), &line_index)
      .expect("analyzed document should produce symbols");

    let top_level: Vec<_> = symbols
      .iter()
      .map(|symbol| (symbol.name.as_str(), symbol.kind))
      .collect();
    assert_eq!(
      top_level,
      vec![("Tools", SymbolKind::NAMESPACE), ("main", SymbolKind::FUNCTION)]
    );

    let tools_children = symbols[0]
      .children
      .as_ref()
      .expect("namespace should expose nested declarations");
    let child_names: Vec<_> = tools_children
      .iter()
      .map(|symbol| (symbol.name.as_str(), symbol.kind))
      .collect();
    assert_eq!(
      child_names,
      vec![
        ("Point", SymbolKind::STRUCT),
        ("Mode", SymbolKind::ENUM),
        ("make", SymbolKind::FUNCTION),
      ]
    );

    let point_children = tools_children[0]
      .children
      .as_ref()
      .expect("record fields should be nested under the record");
    let point_child_names: Vec<_> = point_children
      .iter()
      .map(|symbol| (symbol.name.as_str(), symbol.kind))
      .collect();
    assert_eq!(point_child_names, vec![("x", SymbolKind::FIELD), ("y", SymbolKind::FIELD)]);

    let mode_children = tools_children[1]
      .children
      .as_ref()
      .expect("enum variants should be nested under the enum");
    let mode_child_names: Vec<_> = mode_children
      .iter()
      .map(|symbol| (symbol.name.as_str(), symbol.kind))
      .collect();
    assert_eq!(
      mode_child_names,
      vec![("FAST", SymbolKind::ENUM_MEMBER), ("SLOW", SymbolKind::ENUM_MEMBER)]
    );

    let _ = std::fs::remove_dir_all(root);
  }

  #[test]
  fn test_document_symbols_are_deterministic_across_repeated_calls() {
    let source = "namespace Outer {\n  namespace Inner {\n    function beta(): i32 { return 2; }\n    function alpha(): i32 { return 1; }\n  }\n}\n\nfunction main(): i32 {\n  return Outer::Inner::alpha();\n}\n";
    let (root, file_path, output, _) = analyze_temp_source("document_symbols_stable", source);
    let line_index = LineIndex::new(source.to_string());

    let first = document_symbols_for_analysis(&output, &file_path.to_string_lossy(), &line_index)
      .expect("first symbol request should succeed");
    let second = document_symbols_for_analysis(&output, &file_path.to_string_lossy(), &line_index)
      .expect("second symbol request should succeed");

    assert_eq!(first, second);

    let _ = std::fs::remove_dir_all(root);
  }

  #[test]
  fn test_workspace_symbols_filter_queries_and_empty_states_safely() {
    let source = "namespace Math {\n  function add(a: i32, b: i32): i32 { return a + b; }\n  function subtract(a: i32, b: i32): i32 { return a - b; }\n}\n\nfunction main(): i32 {\n  return Math::add(1, 2);\n}\n";
    let (root, _, output, _) = analyze_temp_source("workspace_symbols_filter", source);

    let add_symbols = workspace_symbols_for_analysis(Some(&output), "ADD");
    let add_names: Vec<_> = add_symbols.iter().map(|symbol| symbol.name.as_str()).collect();
    assert_eq!(add_names, vec!["add"]);
    assert_eq!(add_symbols[0].kind, SymbolKind::FUNCTION);

    let math_symbols = workspace_symbols_for_analysis(Some(&output), "math");
    let math_names: Vec<_> = math_symbols.iter().map(|symbol| symbol.name.as_str()).collect();
    assert_eq!(math_names, vec!["Math"]);

    assert!(workspace_symbols_for_analysis(Some(&output), "missing").is_empty());
    assert!(workspace_symbols_for_analysis(Some(&output), "").is_empty());
    assert!(workspace_symbols_for_analysis(None, "add").is_empty());

    let _ = std::fs::remove_dir_all(root);
  }

  #[test]
  fn test_workspace_symbols_are_deterministic_and_tie_break_by_location() {
    let source =
      "function alpha(): i32 { return 1; }\nfunction alpine(): i32 { return 2; }\nfunction beta(): i32 { return 3; }\n";
    let (root, _, output, _) = analyze_temp_source("workspace_symbols_stable", source);

    let first = workspace_symbols_for_analysis(Some(&output), "alp");
    let second = workspace_symbols_for_analysis(Some(&output), "alp");

    let first_names: Vec<_> = first.iter().map(|symbol| symbol.name.as_str()).collect();
    assert_eq!(first_names, vec!["alpha", "alpine"]);
    assert_eq!(first, second);
    assert!(first[0].location.range.start.line < first[1].location.range.start.line);

    let _ = std::fs::remove_dir_all(root);
  }

  #[test]
  fn test_formatting_changed_text_returns_one_full_document_edit() {
    let root = unique_temp_dir("format_changed");
    let file_path = root.join("main.ign");
    std::fs::create_dir_all(&root).expect("root directory should be created");
    std::fs::write(&file_path, "function main(): void {\n  return;\n}\n")
      .expect("disk file should not affect open-buffer formatting");

    let open_buffer_text = "function main(): void { return; }\n".to_string();
    let edits = formatting_edits_for_open_document(Some(LspFormatDocument {
      line_index: LineIndex::new(open_buffer_text.clone()),
      path: file_path,
      text: open_buffer_text,
    }))
    .expect("formatting should return an edit list");

    assert_eq!(edits.len(), 1);
    assert_eq!(
      edits[0].range,
      Range {
        start: Position { line: 0, character: 0 },
        end: Position { line: 1, character: 0 },
      }
    );
    assert_eq!(edits[0].new_text, "function main(): void {\n  return;\n}\n");

    let _ = std::fs::remove_dir_all(root);
  }

  #[test]
  fn test_formatting_unchanged_text_returns_empty_edits() {
    let root = unique_temp_dir("format_unchanged");
    let file_path = root.join("main.ign");
    std::fs::create_dir_all(&root).expect("root directory should be created");

    let open_buffer_text = "function main(): void {\n  return;\n}\n".to_string();
    let edits = formatting_edits_for_open_document(Some(LspFormatDocument {
      line_index: LineIndex::new(open_buffer_text.clone()),
      path: file_path,
      text: open_buffer_text,
    }))
    .expect("formatting should return an edit list");

    assert!(edits.is_empty(), "unchanged formatted text should not produce edits");

    let _ = std::fs::remove_dir_all(root);
  }

  #[test]
  fn test_formatting_missing_open_document_returns_empty_edits() {
    let edits = formatting_edits_for_open_document(None).expect("missing documents should be successful no-ops");

    assert!(edits.is_empty(), "missing open documents should not produce edits");
  }

  #[test]
  fn test_formatting_invalid_input_returns_empty_edits() {
    let root = unique_temp_dir("format_invalid_input");
    let file_path = root.join("main.ign");
    std::fs::create_dir_all(&root).expect("root directory should be created");

    let open_buffer_text = "function main(: void {\n".to_string();
    let edits = formatting_edits_for_open_document(Some(LspFormatDocument {
      line_index: LineIndex::new(open_buffer_text.clone()),
      path: file_path,
      text: open_buffer_text,
    }))
    .expect("formatter errors should be successful no-ops");

    assert!(edits.is_empty(), "invalid formatter input should not interrupt the editor");

    let _ = std::fs::remove_dir_all(root);
  }

  #[test]
  fn test_formatting_config_failure_returns_empty_edits() {
    let root = unique_temp_dir("format_config_error");
    let file_path = root.join("main.ign");
    std::fs::create_dir_all(&root).expect("root directory should be created");
    std::fs::write(root.join("ignisfmt.toml"), "unknown_key = true\n")
      .expect("invalid formatter config should be written");

    let open_buffer_text = "function main(): void { return; }\n".to_string();
    let edits = formatting_edits_for_open_document(Some(LspFormatDocument {
      line_index: LineIndex::new(open_buffer_text.clone()),
      path: file_path,
      text: open_buffer_text,
    }))
    .expect("config errors should be successful no-ops");

    assert!(edits.is_empty(), "formatter config errors should not interrupt the editor");

    let _ = std::fs::remove_dir_all(root);
  }
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
      capabilities: lsp_server_capabilities(),
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

  async fn formatting(
    &self,
    params: DocumentFormattingParams,
  ) -> Result<Option<Vec<TextEdit>>> {
    let document = {
      let guard = self.state.open_files.read().await;
      guard.get(&params.text_document.uri).map(|doc| LspFormatDocument {
        text: doc.text.clone(),
        path: doc.path.clone(),
        line_index: doc.line_index.clone(),
      })
    };

    Ok(formatting_edits_for_open_document(document))
  }

  async fn code_action(
    &self,
    params: CodeActionParams,
  ) -> Result<Option<CodeActionResponse>> {
    let uri = params.text_document.uri;
    let document = {
      let guard = self.state.open_files.read().await;
      guard.get(&uri).map(|doc| LspCodeActionDocument {
        text: doc.text.clone(),
        line_index: doc.line_index.clone(),
      })
    };

    Ok(Some(code_actions_for_open_document(
      &uri,
      document,
      &params.context.diagnostics,
    )))
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
    let query = &params.query;

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

    Ok(Some(workspace_symbols_for_analysis(output.as_deref(), query)))
  }

  async fn document_symbol(
    &self,
    params: DocumentSymbolParams,
  ) -> Result<Option<DocumentSymbolResponse>> {
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

    let Some(symbols) = document_symbols_for_analysis(&output, &path_str, &line_index) else {
      log(&format!("[document_symbol] Could not resolve path to FileId: {}", path_str));
      return Ok(None);
    };

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

    let Some(symbol) = symbol_at_position(&output, file_analysis, byte_offset) else {
      return Ok(None);
    };

    Ok(Some(collect_reference_locations(&output, symbol.def_id, include_declaration)))
  }

  async fn rename(
    &self,
    params: RenameParams,
  ) -> Result<Option<WorkspaceEdit>> {
    let uri = &params.text_document_position.text_document.uri;
    let position = params.text_document_position.position;

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

    let Some(symbol) = symbol_at_position(&output, file_analysis, byte_offset) else {
      return Ok(None);
    };

    Ok(rename_workspace_edit(&output, symbol.def_id, &params.new_name))
  }

  async fn completion(
    &self,
    params: CompletionParams,
  ) -> Result<Option<CompletionResponse>> {
    let uri = &params.text_document_position.text_document.uri;
    let position = params.text_document_position.position;

    // Get document info (text, path, line_index, version)
    let (text, path, path_str, line_index, version) = {
      let guard = self.state.open_files.read().await;

      let Some(doc) = guard.get(uri) else {
        return Ok(None);
      };

      (
        doc.text.clone(),
        doc.path.clone(),
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

      doc.get_or_compute_tokens().to_vec()
    };

    // Get analysis for completions (prefer current doc cache, then global, then fresh)
    let mut analysis_source = "doc_cache";
    let mut output = {
      let guard = self.state.open_files.read().await;
      guard.get(uri).and_then(|doc| doc.get_completion_analysis())
    };

    if output.is_none() {
      analysis_source = "global_last_good";
      output = self.state.get_global_last_good_analysis().await;
    }

    if output.is_none() {
      analysis_source = "fresh";
      output = self.get_analysis(uri).await.map(|(fresh, _, _)| fresh);
    }

    let mut output = match output {
      Some(output) => output,
      None => return Ok(None),
    };

    let mut file_id = lookup_file_id_best_effort(&output, &path, &path_str);

    if file_id.is_none()
      && let Some((fresh, _, _)) = self.get_analysis(uri).await
      && let Some(resolved_file_id) = lookup_file_id_best_effort(&fresh, &path, &path_str)
    {
      analysis_source = "fresh_relookup";
      output = fresh;
      file_id = Some(resolved_file_id);
    }

    if file_id.is_none()
      && let Some(recovered) = self
        .analyze_completion_without_current_override(uri, &path, &path_str)
        .await
      && let Some(resolved_file_id) = lookup_file_id_best_effort(&recovered, &path, &path_str)
    {
      analysis_source = "disk_fallback";
      output = recovered;
      file_id = Some(resolved_file_id);
    }

    if file_id.is_none()
      && let Some(global) = self.state.get_global_last_good_analysis().await
      && let Some(resolved_file_id) = lookup_file_id_best_effort(&global, &path, &path_str)
    {
      analysis_source = "global_relookup";
      output = global;
      file_id = Some(resolved_file_id);
    }

    let Some(file_id) = file_id else {
      log(&format!(
        "[completion] no file_id for path '{}' (source={})",
        path_str, analysis_source
      ));
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
        CompletionContext::AfterPipe { prefix, start_offset } => {
          complete_after_pipe(prefix, *start_offset, &tokens, Some(&output), &file_id)
        },
        CompletionContext::ImportPath { prefix } => complete_import_path(prefix, &config),
        CompletionContext::RecordInit {
          record_name,
          assigned_fields,
          prefix,
        } => complete_record_init(record_name, assigned_fields, prefix, &output, &file_id),
        CompletionContext::AfterAt { prefix } => complete_at_items(prefix),
      };

      log(&format!(
        "[completion] uri={} context={:?} source={} file={:?} candidates={}",
        uri_str,
        context,
        analysis_source,
        file_id,
        candidates.len()
      ));

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

fn document_symbols_for_analysis(
  output: &AnalyzeProjectOutput,
  path_str: &str,
  line_index: &LineIndex,
) -> Option<Vec<DocumentSymbol>> {
  use ignis_type::definition::DefinitionKind;
  use ignis_type::namespace::NamespaceId;

  let file_id = output.source_map.lookup_by_path(path_str)?;

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

  for defs in defs_by_namespace.values_mut() {
    sort_def_ids_by_source(output, defs);
  }

  // Build document symbols recursively
  let top_level_defs = defs_by_namespace.remove(&None).unwrap_or_default();
  let mut visited_namespaces = HashSet::new();

  let mut symbols: Vec<DocumentSymbol> = top_level_defs
    .into_iter()
    .filter_map(|def_id| {
      build_document_symbol(
        &def_id,
        output,
        &file_id,
        line_index,
        &defs_by_namespace,
        &ns_to_def,
        &mut visited_namespaces,
      )
    })
    .collect();

  sort_document_symbols_recursive(&mut symbols);
  Some(symbols)
}

fn workspace_symbols_for_analysis(
  output: Option<&AnalyzeProjectOutput>,
  query: &str,
) -> Vec<SymbolInformation> {
  use ignis_type::definition::DefinitionKind;

  // Empty query = no results (avoids freezing on Ctrl+T with thousands of symbols)
  if query.is_empty() {
    return vec![];
  }

  let Some(output) = output else {
    return vec![];
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

  matches.sort_by(|(score_a, sym_a), (score_b, sym_b)| {
    score_a
      .cmp(score_b)
      .then_with(|| sym_a.name.to_lowercase().cmp(&sym_b.name.to_lowercase()))
      .then_with(|| sym_a.name.cmp(&sym_b.name))
      .then_with(|| sym_a.location.uri.as_str().cmp(sym_b.location.uri.as_str()))
      .then_with(|| compare_ranges(&sym_a.location.range, &sym_b.location.range))
      .then_with(|| symbol_kind_rank(sym_a.kind).cmp(&symbol_kind_rank(sym_b.kind)))
      .then_with(|| sym_a.container_name.cmp(&sym_b.container_name))
  });

  matches.into_iter().take(200).map(|(_, symbol)| symbol).collect()
}

fn sort_def_ids_by_source(
  output: &AnalyzeProjectOutput,
  def_ids: &mut [DefinitionId],
) {
  def_ids.sort_by(|left, right| {
    let left_def = output.defs.get(left);
    let right_def = output.defs.get(right);
    left_def
      .span
      .start
      .cmp(&right_def.span.start)
      .then_with(|| left_def.span.end.cmp(&right_def.span.end))
      .then_with(|| {
        output
          .symbol_names
          .get(&left_def.name)
          .cmp(&output.symbol_names.get(&right_def.name))
      })
      .then_with(|| format!("{:?}", left).cmp(&format!("{:?}", right)))
  });
}

fn sort_document_symbols_recursive(symbols: &mut [DocumentSymbol]) {
  for symbol in symbols.iter_mut() {
    if let Some(children) = &mut symbol.children {
      sort_document_symbols_recursive(children);
    }
  }

  symbols.sort_by(|left, right| {
    compare_ranges(&left.range, &right.range)
      .then_with(|| left.name.cmp(&right.name))
      .then_with(|| symbol_kind_rank(left.kind).cmp(&symbol_kind_rank(right.kind)))
  });
}

fn symbol_kind_rank(kind: SymbolKind) -> u32 {
  if kind == SymbolKind::FILE {
    1
  } else if kind == SymbolKind::MODULE {
    2
  } else if kind == SymbolKind::NAMESPACE {
    3
  } else if kind == SymbolKind::PACKAGE {
    4
  } else if kind == SymbolKind::CLASS {
    5
  } else if kind == SymbolKind::METHOD {
    6
  } else if kind == SymbolKind::PROPERTY {
    7
  } else if kind == SymbolKind::FIELD {
    8
  } else if kind == SymbolKind::CONSTRUCTOR {
    9
  } else if kind == SymbolKind::ENUM {
    10
  } else if kind == SymbolKind::INTERFACE {
    11
  } else if kind == SymbolKind::FUNCTION {
    12
  } else if kind == SymbolKind::VARIABLE {
    13
  } else if kind == SymbolKind::CONSTANT {
    14
  } else if kind == SymbolKind::STRING {
    15
  } else if kind == SymbolKind::NUMBER {
    16
  } else if kind == SymbolKind::BOOLEAN {
    17
  } else if kind == SymbolKind::ARRAY {
    18
  } else if kind == SymbolKind::OBJECT {
    19
  } else if kind == SymbolKind::KEY {
    20
  } else if kind == SymbolKind::NULL {
    21
  } else if kind == SymbolKind::ENUM_MEMBER {
    22
  } else if kind == SymbolKind::STRUCT {
    23
  } else if kind == SymbolKind::EVENT {
    24
  } else if kind == SymbolKind::OPERATOR {
    25
  } else if kind == SymbolKind::TYPE_PARAMETER {
    26
  } else {
    0
  }
}

fn compare_ranges(
  left: &Range,
  right: &Range,
) -> std::cmp::Ordering {
  left
    .start
    .line
    .cmp(&right.start.line)
    .then_with(|| left.start.character.cmp(&right.start.character))
    .then_with(|| left.end.line.cmp(&right.end.line))
    .then_with(|| left.end.character.cmp(&right.end.character))
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
