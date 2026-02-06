//! LSP server state management.

use std::collections::{HashMap, HashSet};
use std::path::PathBuf;
use std::sync::Arc;

use ignis_config::IgnisConfig;
use ignis_driver::AnalyzeProjectOutput;
use ignis_parser::IgnisLexer;
use ignis_token::token::Token;
use ignis_type::file::FileId;
use tokio::sync::RwLock;
use tower_lsp::lsp_types::InlayHint;
use url::Url;

use crate::convert::LineIndex;
use crate::project::ProjectManager;

/// Cached analysis result for a document.
pub struct CachedAnalysis {
  /// Version of the document when this analysis was performed.
  pub version: i32,

  /// The analysis output.
  pub output: Arc<AnalyzeProjectOutput>,

  /// Cached inlay hints (computed lazily from analysis).
  pub hints: Option<Vec<InlayHint>>,
}

/// Cached tokens for a document.
pub struct CachedTokens {
  /// Version of the document when tokens were computed.
  pub version: i32,

  /// The lexed tokens.
  pub tokens: Vec<Token>,
}

/// An open document in the editor.
pub struct OpenDoc {
  /// LSP document version.
  pub version: i32,

  /// Current text content.
  pub text: String,

  /// File path (for analysis).
  pub path: PathBuf,

  /// Line index for position conversions.
  pub line_index: LineIndex,

  /// Cached analysis result (if available and up-to-date).
  pub cached_analysis: Option<CachedAnalysis>,

  /// Last successful analysis (stage >= Parsed).
  /// Used for completions when current analysis fails.
  pub last_good_analysis: Option<Arc<AnalyzeProjectOutput>>,

  /// Cached tokens for completion context detection.
  pub cached_tokens: Option<CachedTokens>,
}

impl OpenDoc {
  /// Create a new open document.
  pub fn new(
    version: i32,
    text: String,
    path: PathBuf,
  ) -> Self {
    let line_index = LineIndex::new(text.clone());

    Self {
      version,
      text,
      path,
      line_index,
      cached_analysis: None,
      last_good_analysis: None,
      cached_tokens: None,
    }
  }

  /// Update the document with new content.
  pub fn update(
    &mut self,
    version: i32,
    text: String,
  ) {
    self.version = version;
    self.text = text;
    // Rebuild line index from the stored text (avoids extra clone)
    self.line_index = LineIndex::new(self.text.clone());
    // Invalidate cached analysis and tokens on text change
    self.cached_analysis = None;
    self.cached_tokens = None;
  }

  /// Set the cached analysis for this document.
  ///
  /// Also updates `last_good_analysis` if the analysis has usable type information.
  /// We only update last_good_analysis if node_types is non-empty, which indicates
  /// that the analyzer actually processed the code (not just returned an empty AST
  /// due to parse errors).
  pub fn set_cached_analysis(
    &mut self,
    version: i32,
    output: Arc<AnalyzeProjectOutput>,
  ) {
    // Update last_good_analysis only if analysis has type information
    // This ensures we don't lose good analysis when the user types incomplete code
    if !output.node_types.is_empty() {
      self.last_good_analysis = Some(Arc::clone(&output));
    }

    self.cached_analysis = Some(CachedAnalysis {
      version,
      output,
      hints: None,
    });
  }

  /// Get the cached analysis if it matches the current version.
  pub fn get_cached_analysis(&self) -> Option<Arc<AnalyzeProjectOutput>> {
    self.cached_analysis.as_ref().and_then(|cache| {
      if cache.version == self.version {
        Some(Arc::clone(&cache.output))
      } else {
        None
      }
    })
  }

  /// Get cached inlay hints if available.
  pub fn get_cached_hints(&self) -> Option<&Vec<InlayHint>> {
    self.cached_analysis.as_ref().and_then(|cache| {
      if cache.version == self.version {
        cache.hints.as_ref()
      } else {
        None
      }
    })
  }

  /// Set cached inlay hints.
  pub fn set_cached_hints(
    &mut self,
    hints: Vec<InlayHint>,
  ) {
    if let Some(cache) = &mut self.cached_analysis
      && cache.version == self.version
    {
      cache.hints = Some(hints);
    }
  }

  /// Get analysis for completion.
  ///
  /// Prefers cached analysis if it has type information, otherwise falls back
  /// to last_good_analysis. This ensures we use good type info even when the
  /// user is typing incomplete code.
  pub fn get_completion_analysis(&self) -> Option<Arc<AnalyzeProjectOutput>> {
    // Try cached analysis first
    if let Some(cached) = self.get_cached_analysis() {
      // If it has type info, use it
      if !cached.node_types.is_empty() {
        return Some(cached);
      }
      // Otherwise fall through to last_good_analysis
    }

    // Fall back to last good analysis
    self.last_good_analysis.clone()
  }

  /// Get or compute tokens for the current document version.
  ///
  /// Tokens are cached and reused if the version matches.
  pub fn get_or_compute_tokens(
    &mut self,
    file_id: &FileId,
  ) -> &[Token] {
    // Check if we have cached tokens for the current version
    if self.cached_tokens.as_ref().map(|c| c.version) == Some(self.version) {
      return &self.cached_tokens.as_ref().unwrap().tokens;
    }

    // Compute tokens
    let mut lexer = IgnisLexer::new(*file_id, &self.text);
    lexer.scan_tokens();

    self.cached_tokens = Some(CachedTokens {
      version: self.version,
      tokens: lexer.tokens,
    });

    &self.cached_tokens.as_ref().unwrap().tokens
  }
}

/// Shared state for the LSP server.
pub struct LspState {
  /// Compiler configuration (fallback when no project).
  pub config: Arc<IgnisConfig>,

  /// Project manager for ignis.toml discovery and caching.
  pub project_manager: ProjectManager,

  /// Currently open documents.
  pub open_files: RwLock<HashMap<Url, OpenDoc>>,

  /// Workspace root path.
  pub root: RwLock<Option<PathBuf>>,

  /// URIs that had diagnostics in the last publish cycle.
  /// Used to clear stale diagnostics when files no longer have errors.
  pub previous_diagnostic_uris: RwLock<HashSet<Url>>,
}

impl LspState {
  /// Create new LSP state with the given configuration.
  pub fn new(config: Arc<IgnisConfig>) -> Self {
    Self {
      project_manager: ProjectManager::new(Arc::clone(&config)),
      config,
      open_files: RwLock::new(HashMap::new()),
      root: RwLock::new(None),
      previous_diagnostic_uris: RwLock::new(HashSet::new()),
    }
  }

  /// Set the workspace root.
  pub async fn set_root(
    &self,
    root: Option<PathBuf>,
  ) {
    let mut guard = self.root.write().await;
    *guard = root;
  }

  /// Insert or update an open document.
  pub async fn open_document(
    &self,
    uri: Url,
    version: i32,
    text: String,
  ) {
    let path = uri_to_path(&uri);
    let doc = OpenDoc::new(version, text, path);

    let mut guard = self.open_files.write().await;
    guard.insert(uri, doc);
  }

  /// Update an existing document.
  pub async fn update_document(
    &self,
    uri: &Url,
    version: i32,
    text: String,
  ) {
    let mut guard = self.open_files.write().await;

    if let Some(doc) = guard.get_mut(uri) {
      doc.update(version, text);
    }
  }

  /// Close a document.
  pub async fn close_document(
    &self,
    uri: &Url,
  ) {
    let mut guard = self.open_files.write().await;
    guard.remove(uri);
  }
}

/// Convert a URL to a file path.
///
/// Uses the same normalization as SourceMap to ensure consistent FileId lookups.
/// Falls back to a virtual path if the URL cannot be converted.
fn uri_to_path(uri: &Url) -> PathBuf {
  let path = uri.to_file_path().unwrap_or_else(|_| {
    // Virtual file - use the URI path as-is
    PathBuf::from(uri.path())
  });

  // Normalize using the same function as SourceMap for consistent path comparison
  ignis_type::file::normalize_path(path)
}
