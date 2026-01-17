//! LSP server state management.

use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;

use ignis_config::IgnisConfig;
use tokio::sync::RwLock;
use url::Url;

use crate::convert::LineIndex;

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
    }
  }

  /// Update the document with new content.
  pub fn update(
    &mut self,
    version: i32,
    text: String,
  ) {
    self.version = version;
    self.line_index = LineIndex::new(text.clone());
    self.text = text;
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
}

impl LspState {
  /// Create new LSP state with the given configuration.
  pub fn new(config: Arc<IgnisConfig>) -> Self {
    Self {
      config,
      open_files: RwLock::new(HashMap::new()),
      root: RwLock::new(None),
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
