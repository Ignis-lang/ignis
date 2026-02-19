//! Error types for project loading and resolution.

use std::fmt;
use std::path::PathBuf;

/// Errors that can occur when loading or resolving a project.
#[derive(Debug)]
pub enum ProjectError {
  /// No ignis.toml found when searching upward from the given directory.
  NotFound { searched_from: PathBuf },

  /// I/O error while reading a file.
  IoError { path: PathBuf, source: std::io::Error },

  /// Failed to parse ignis.toml.
  TomlParseError { path: PathBuf, message: String },

  /// The source_dir specified in the TOML does not exist.
  SourceDirNotFound { path: PathBuf },

  /// The entry file specified in the TOML does not exist.
  EntryNotFound { path: PathBuf },

  /// std_path does not exist when std=true.
  StdPathNotFound { path: PathBuf },

  /// runtime_path does not exist.
  RuntimePathNotFound { path: PathBuf },

  /// opt_level is out of valid range (0-3).
  InvalidOptLevel { value: u8, max: u8 },

  /// Unsupported target backend.
  UnsupportedTarget { value: String },

  /// Invalid emit value.
  InvalidEmit { value: String },

  /// Alias key is reserved (e.g., "std").
  ReservedAlias { name: String },

  /// Alias path does not exist on disk.
  AliasPathNotFound { alias: String, path: PathBuf },
}

impl fmt::Display for ProjectError {
  fn fmt(
    &self,
    f: &mut fmt::Formatter<'_>,
  ) -> fmt::Result {
    match self {
      ProjectError::NotFound { searched_from } => {
        write!(f, "no ignis.toml found (searched from '{}')", searched_from.display())
      },

      ProjectError::IoError { path, source } => {
        write!(f, "failed to read '{}': {}", path.display(), source)
      },

      ProjectError::TomlParseError { path, message } => {
        write!(f, "failed to parse '{}': {}", path.display(), message)
      },

      ProjectError::SourceDirNotFound { path } => {
        write!(f, "source directory not found: '{}'", path.display())
      },

      ProjectError::EntryNotFound { path } => {
        write!(f, "entry file not found: '{}'", path.display())
      },

      ProjectError::StdPathNotFound { path } => {
        write!(f, "std_path not found: '{}'", path.display())
      },

      ProjectError::RuntimePathNotFound { path } => {
        write!(f, "runtime_path not found: '{}'", path.display())
      },

      ProjectError::InvalidOptLevel { value, max } => {
        write!(f, "opt_level {} is out of range (max: {})", value, max)
      },

      ProjectError::UnsupportedTarget { value } => {
        write!(
          f,
          "unsupported target/backend: '{}' (supported: 'c', 'qbe', 'iir', 'none')",
          value
        )
      },

      ProjectError::InvalidEmit { value } => {
        write!(f, "invalid emit value: '{}' (supported: 'c', 'qbe', 'obj')", value)
      },

      ProjectError::ReservedAlias { name } => {
        write!(f, "alias '{}' is reserved and cannot be defined in [aliases]", name)
      },

      ProjectError::AliasPathNotFound { alias, path } => {
        write!(f, "alias '{}' path not found: '{}'", alias, path.display())
      },
    }
  }
}

impl std::error::Error for ProjectError {
  fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
    match self {
      ProjectError::IoError { source, .. } => Some(source),
      _ => None,
    }
  }
}
