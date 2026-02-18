//! Project discovery and caching for the LSP.
//!
//! Discovers `ignis.toml` by searching upward, caches resolved projects
//! with mtime-based invalidation, and supports in-memory overrides for
//! unsaved editor content.

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::SystemTime;

use ignis_config::{IgnisConfig, IgnisSTDManifest};
use ignis_driver::project::{find_project_root, resolve_project, CliOverrides, Project, ProjectError, ProjectToml};
use ignis_type::file::normalize_path;
use tokio::sync::RwLock;

/// Manages project discovery and caching for the LSP.
pub struct ProjectManager {
  /// Cached projects by root directory.
  cache: RwLock<HashMap<PathBuf, CachedProject>>,

  /// In-memory TOML overrides (when ignis.toml is open unsaved).
  open_toml_overrides: RwLock<HashMap<PathBuf, String>>,

  /// Fallback config when no project found.
  fallback_config: Arc<IgnisConfig>,
}

struct CachedProject {
  resolved: ResolvedProject,
  mtime: SystemTime,
}

/// A resolved project with its associated config.
pub struct ResolvedProject {
  pub project: Project,
  pub config: Arc<IgnisConfig>,
}

impl Clone for ResolvedProject {
  fn clone(&self) -> Self {
    Self {
      project: self.project.clone(),
      config: Arc::clone(&self.config),
    }
  }
}

/// Result of project discovery for a file.
pub enum ProjectContext {
  /// Project found and resolved successfully.
  Project(ResolvedProject),

  /// No ignis.toml found (single-file mode).
  NoProject,

  /// Error loading/parsing ignis.toml.
  Error { root: PathBuf, error: ProjectError },
}

impl ProjectManager {
  /// Create a new ProjectManager with the given fallback config.
  pub fn new(fallback_config: Arc<IgnisConfig>) -> Self {
    Self {
      cache: RwLock::new(HashMap::new()),
      open_toml_overrides: RwLock::new(HashMap::new()),
      fallback_config,
    }
  }

  /// Get project context for a file inside a project.
  ///
  /// Searches upward from `file_path` for `ignis.toml`.
  pub async fn project_for_file(
    &self,
    file_path: &Path,
  ) -> ProjectContext {
    let Some(parent) = file_path.parent() else {
      return ProjectContext::NoProject;
    };

    let Some(root) = find_project_root(parent) else {
      return ProjectContext::NoProject;
    };

    self.project_for_root(&root).await
  }

  /// Get project context for a project root directory.
  ///
  /// Expects `root` to be the directory containing ignis.toml.
  pub async fn project_for_root(
    &self,
    root: &Path,
  ) -> ProjectContext {
    let root = normalize_path(root.to_path_buf());

    // Check in-memory override first
    {
      let overrides = self.open_toml_overrides.read().await;
      if let Some(toml_text) = overrides.get(&root) {
        return self.resolve_from_text(&root, toml_text);
      }
    }

    // Check cache
    {
      let cache = self.cache.read().await;
      if let Some(cached) = cache.get(&root)
        && self.is_cache_fresh(&root, cached.mtime)
      {
        return ProjectContext::Project(cached.resolved.clone());
      }
    }

    // Load from disk and cache
    self.load_and_cache(&root).await
  }

  /// Set in-memory TOML override (when ignis.toml is open/edited).
  pub async fn set_toml_override(
    &self,
    root: &Path,
    text: String,
  ) {
    let root = normalize_path(root.to_path_buf());
    let mut overrides = self.open_toml_overrides.write().await;
    overrides.insert(root, text);
  }

  /// Clear in-memory TOML override (when ignis.toml is closed).
  pub async fn clear_toml_override(
    &self,
    root: &Path,
  ) {
    let root = normalize_path(root.to_path_buf());
    let mut overrides = self.open_toml_overrides.write().await;
    overrides.remove(&root);
  }

  /// Check if there's an in-memory override for a root.
  pub async fn has_toml_override(
    &self,
    root: &Path,
  ) -> bool {
    let root = normalize_path(root.to_path_buf());
    let overrides = self.open_toml_overrides.read().await;
    overrides.contains_key(&root)
  }

  /// Invalidate disk cache for a root (on file watcher events).
  pub async fn invalidate(
    &self,
    root: &Path,
  ) {
    let root = normalize_path(root.to_path_buf());
    let mut cache = self.cache.write().await;
    cache.remove(&root);
  }

  /// Get config for a file (project config if found, otherwise fallback).
  pub async fn config_for_file(
    &self,
    file_path: &Path,
  ) -> Arc<IgnisConfig> {
    match self.project_for_file(file_path).await {
      ProjectContext::Project(resolved) => resolved.config,
      _ => Arc::clone(&self.fallback_config),
    }
  }

  /// Resolve a project from TOML text.
  fn resolve_from_text(
    &self,
    root: &Path,
    toml_text: &str,
  ) -> ProjectContext {
    let toml: ProjectToml = match toml::from_str(toml_text) {
      Ok(t) => t,
      Err(e) => {
        return ProjectContext::Error {
          root: root.to_path_buf(),
          error: ProjectError::TomlParseError {
            path: root.join("ignis.toml"),
            message: e.to_string(),
          },
        };
      },
    };

    match resolve_project(root.to_path_buf(), toml, &CliOverrides::default()) {
      Ok(project) => {
        let config = self.build_config_from_project(&project);
        ProjectContext::Project(ResolvedProject { project, config })
      },
      Err(e) => ProjectContext::Error {
        root: root.to_path_buf(),
        error: e,
      },
    }
  }

  /// Load project from disk and cache it.
  async fn load_and_cache(
    &self,
    root: &Path,
  ) -> ProjectContext {
    let toml_path = root.join("ignis.toml");

    let mtime = match std::fs::metadata(&toml_path) {
      Ok(meta) => meta.modified().unwrap_or(SystemTime::UNIX_EPOCH),
      Err(_) => return ProjectContext::NoProject,
    };

    let toml_text = match std::fs::read_to_string(&toml_path) {
      Ok(text) => text,
      Err(e) => {
        return ProjectContext::Error {
          root: root.to_path_buf(),
          error: ProjectError::IoError {
            path: toml_path,
            source: e,
          },
        };
      },
    };

    let ctx = self.resolve_from_text(root, &toml_text);

    // Cache if successful
    if let ProjectContext::Project(ref resolved) = ctx {
      let mut cache = self.cache.write().await;
      cache.insert(
        root.to_path_buf(),
        CachedProject {
          resolved: resolved.clone(),
          mtime,
        },
      );
    }

    ctx
  }

  /// Check if cached project is still fresh (mtime hasn't changed).
  fn is_cache_fresh(
    &self,
    root: &Path,
    cached_mtime: SystemTime,
  ) -> bool {
    let toml_path = root.join("ignis.toml");

    match std::fs::metadata(&toml_path) {
      Ok(meta) => meta.modified().ok() == Some(cached_mtime),
      Err(_) => false,
    }
  }

  /// Build an IgnisConfig from a resolved Project.
  fn build_config_from_project(
    &self,
    project: &Project,
  ) -> Arc<IgnisConfig> {
    let mut config = (*self.fallback_config).clone();

    if let Some(ref std_path) = project.std_path {
      config.std_path = std_path.to_string_lossy().to_string();
    }

    config.std = project.std_path.is_some();
    config.auto_load_std = project.std_path.is_some();
    config.manifest = load_manifest(&config.std_path);

    if let Some(ref triple) = project.target_triple {
      config.target_triple = triple.clone();
    }

    let mut features: std::collections::HashSet<String> = project.default_features.iter().cloned().collect();
    features.extend(config.enabled_features.drain());
    config.enabled_features = features;

    if !project.known_features.is_empty() {
      config.known_features = Some(project.known_features.iter().cloned().collect());
    }

    Arc::new(config)
  }
}

/// Load std manifest from std_path/manifest.toml.
fn load_manifest(std_path: &str) -> IgnisSTDManifest {
  if std_path.is_empty() {
    return IgnisSTDManifest::default();
  }

  let manifest_path = Path::new(std_path).join("manifest.toml");

  std::fs::read_to_string(&manifest_path)
    .ok()
    .and_then(|content| toml::from_str(&content).ok())
    .unwrap_or_default()
}
