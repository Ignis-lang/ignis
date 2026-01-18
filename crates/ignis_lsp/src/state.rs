//! LSP server state management.

use std::collections::{HashMap, HashSet};
use std::path::PathBuf;
use std::sync::Arc;

use ignis_config::IgnisConfig;
use ignis_driver::AnalyzeProjectOutput;
use tokio::sync::RwLock;
use url::Url;

use crate::convert::LineIndex;

/// Cached analysis result for a document.
pub struct CachedAnalysis {
  /// Version of the document when this analysis was performed.
  pub version: i32,

  /// The analysis output.
  pub output: Arc<AnalyzeProjectOutput>,
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
    // Invalidate cached analysis on text change
    self.cached_analysis = None;
  }

  /// Set the cached analysis for this document.
  pub fn set_cached_analysis(
    &mut self,
    version: i32,
    output: Arc<AnalyzeProjectOutput>,
  ) {
    self.cached_analysis = Some(CachedAnalysis { version, output });
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
}

/// Shared state for the LSP server.
pub struct LspState {
  /// Compiler configuration.
  pub config: Arc<IgnisConfig>,

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
/// Falls back to a virtual path if the URL cannot be converted.
fn uri_to_path(uri: &Url) -> PathBuf {
  uri.to_file_path().unwrap_or_else(|_| {
    // Virtual file - use the URI path as-is
    PathBuf::from(uri.path())
  })
}
