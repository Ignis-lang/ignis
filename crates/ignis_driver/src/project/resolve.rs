//! Resolution of raw TOML config into a validated Project.
//!
//! This module takes the raw `ProjectToml` from parsing and converts it into
//! a `Project` with all paths resolved to absolute, canonicalized forms and
//! all values validated.

use std::collections::HashMap;
use std::path::{Path, PathBuf};

use ignis_config::TargetBackend;

use crate::project::config::ProjectToml;
use crate::project::errors::ProjectError;

/// Maximum allowed optimization level.
const MAX_OPT_LEVEL: u8 = 3;

/// A fully resolved and validated project configuration.
///
/// All paths are absolute and canonicalized where the target exists.
/// All values have been validated.
#[derive(Debug, Clone)]
pub struct Project {
  /// Path to the ignis.toml file.
  pub toml_path: PathBuf,

  /// Project root directory (parent of ignis.toml), canonicalized.
  pub root: PathBuf,

  /// Project name from `[package]`.
  pub name: String,

  /// Project version from `[package]`.
  pub version: String,

  /// Absolute path to the entry file.
  pub entry: PathBuf,

  /// Absolute path to the source directory.
  pub source_dir: PathBuf,

  /// Absolute path to the output directory.
  pub out_dir: PathBuf,

  /// Absolute path to the standard library (None if std=false).
  pub std_path: Option<PathBuf>,

  /// Absolute path to the runtime directory.
  pub runtime_path: Option<PathBuf>,

  /// Whether to produce an executable (vs library).
  pub bin: bool,

  /// Optimization level (0-3).
  pub opt_level: u8,

  /// Include debug information.
  pub debug: bool,

  /// Selected compilation backend.
  pub backend: TargetBackend,

  /// Target triple used for compile-time directives.
  pub target_triple: Option<String>,

  /// Declared feature catalog for strict unknown-feature validation.
  pub known_features: Vec<String>,

  /// Features enabled by default in this project.
  pub default_features: Vec<String>,

  /// C compiler to use.
  pub cc: String,

  /// Additional C compiler flags.
  pub cflags: Vec<String>,

  /// Extra artifacts to emit.
  pub emit: EmitSet,

  /// Import path aliases: first segment -> absolute directory path.
  pub aliases: HashMap<String, PathBuf>,
}

fn resolve_backend(
  toml: &ProjectToml,
  overrides: &CliOverrides,
) -> Result<TargetBackend, ProjectError> {
  if let Some(backend) = overrides.backend {
    return Ok(backend);
  }

  let raw = toml
    .build
    .backend
    .as_deref()
    .unwrap_or(toml.build.target.as_str())
    .to_ascii_lowercase();

  match raw.as_str() {
    "c" => Ok(TargetBackend::C),
    "qbe" => Ok(TargetBackend::Qbe),
    "iir" => Ok(TargetBackend::Iir),
    "none" => Ok(TargetBackend::None),
    _ => Err(ProjectError::UnsupportedTarget { value: raw }),
  }
}

/// Set of extra artifacts to emit during compilation.
///
/// These are *extra* outputs beyond the default behavior:
/// - `build` mode always produces a binary (if bin=true) regardless of this set.
/// - `check` mode never produces a binary regardless of this set.
/// - `emit.c = true` means also save the generated C files.
/// - `emit.obj = true` means also save the object files (only in build mode).
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct EmitSet {
  /// Save generated C code to out_dir/c/.
  pub c: bool,

  /// Save object files to out_dir/obj/ (build mode only).
  pub obj: bool,
}

/// CLI overrides that take precedence over TOML values.
#[derive(Debug, Default)]
pub struct CliOverrides {
  pub opt_level: Option<u8>,
  pub debug: Option<bool>,
  pub backend: Option<TargetBackend>,
  pub out_dir: Option<PathBuf>,
  pub std_path: Option<PathBuf>,
  pub target_triple: Option<String>,
  pub cc: Option<String>,
  pub cflags: Option<Vec<String>>,
  pub emit: Option<Vec<String>>,
}

/// Resolve a `ProjectToml` into a fully validated `Project`.
///
/// # Arguments
/// * `root` - Directory containing ignis.toml (will be canonicalized).
/// * `toml` - Parsed TOML configuration.
/// * `overrides` - CLI overrides that take precedence.
///
/// # Errors
/// Returns `ProjectError` if:
/// - Paths don't exist (source_dir, entry, std_path, runtime_path)
/// - Values are out of range (opt_level)
/// - Target is unsupported
/// - emit values are invalid
pub fn resolve_project(
  root: PathBuf,
  toml: ProjectToml,
  overrides: &CliOverrides,
) -> Result<Project, ProjectError> {
  // Canonicalize root
  let root = root.canonicalize().map_err(|e| ProjectError::IoError {
    path: root.clone(),
    source: e,
  })?;

  let toml_path = root.join(crate::project::find::PROJECT_FILE);

  // Resolve source_dir
  let source_dir = root.join(&toml.build.source_dir);
  if !source_dir.exists() {
    return Err(ProjectError::SourceDirNotFound { path: source_dir });
  }
  let source_dir = source_dir.canonicalize().map_err(|e| ProjectError::IoError {
    path: source_dir.clone(),
    source: e,
  })?;

  // Resolve entry
  let entry = source_dir.join(&toml.build.entry);
  if !entry.exists() {
    return Err(ProjectError::EntryNotFound { path: entry });
  }
  let entry = entry.canonicalize().map_err(|e| ProjectError::IoError {
    path: entry.clone(),
    source: e,
  })?;

  // Resolve out_dir (may not exist yet, that's ok)
  let out_dir = overrides
    .out_dir
    .clone()
    .map(|p| resolve_path(&root, &p))
    .unwrap_or_else(|| root.join(&toml.build.out_dir));

  // Resolve std_path
  let std_path = if toml.ignis.std {
    let path = if let Some(override_path) = &overrides.std_path {
      override_path.clone()
    } else if let Some(toml_path) = &toml.ignis.std_path {
      resolve_path(&root, toml_path)
    } else {
      // Try environment variable
      std::env::var("IGNIS_STD_PATH")
        .map(PathBuf::from)
        .map_err(|_| ProjectError::StdPathNotFound {
          path: PathBuf::from("$IGNIS_STD_PATH (not set)"),
        })?
    };

    if !path.exists() {
      return Err(ProjectError::StdPathNotFound { path });
    }

    Some(path.canonicalize().map_err(|e| ProjectError::IoError {
      path: path.clone(),
      source: e,
    })?)
  } else {
    None
  };

  // Resolve runtime_path
  let runtime_path = if toml.ignis.std {
    let path = if let Some(toml_path) = &toml.ignis.runtime_path {
      resolve_path(&root, toml_path)
    } else if let Some(ref sp) = std_path {
      sp.join("runtime")
    } else {
      // std=true but no std_path, this shouldn't happen (caught above)
      return Err(ProjectError::RuntimePathNotFound {
        path: PathBuf::from("(no std_path)"),
      });
    };

    if !path.exists() {
      return Err(ProjectError::RuntimePathNotFound { path });
    }

    Some(path.canonicalize().map_err(|e| ProjectError::IoError {
      path: path.clone(),
      source: e,
    })?)
  } else {
    None
  };

  // Resolve opt_level
  let opt_level = overrides.opt_level.unwrap_or(toml.build.opt_level.unwrap_or(0));
  if opt_level > MAX_OPT_LEVEL {
    return Err(ProjectError::InvalidOptLevel {
      value: opt_level,
      max: MAX_OPT_LEVEL,
    });
  }

  // Resolve debug
  let debug = overrides.debug.unwrap_or(toml.build.debug);

  let backend = resolve_backend(&toml, overrides)?;

  // Resolve target triple
  let target_triple = overrides.target_triple.clone().or(toml.build.target_triple.clone());

  // Feature sets from project config
  let known_features = toml.build.known_features.clone();
  let default_features = toml.build.default_features.clone();

  // Resolve cc
  let cc = overrides.cc.clone().unwrap_or(toml.build.cc);

  // Resolve cflags
  let cflags = overrides.cflags.clone().unwrap_or(toml.build.cflags);

  // Resolve emit
  let emit_values = overrides.emit.as_ref().unwrap_or(&toml.build.emit);
  let emit = parse_emit_set(emit_values)?;

  // Resolve aliases
  let mut aliases = HashMap::new();
  for (key, value) in &toml.aliases {
    if key == "std" {
      return Err(ProjectError::ReservedAlias { name: key.clone() });
    }
    let path = resolve_path(&root, value);
    if !path.exists() {
      return Err(ProjectError::AliasPathNotFound {
        alias: key.clone(),
        path,
      });
    }
    let canonical = path.canonicalize().map_err(|e| ProjectError::IoError {
      path: path.clone(),
      source: e,
    })?;
    aliases.insert(key.clone(), canonical);
  }

  Ok(Project {
    toml_path,
    root,
    name: toml.package.name,
    version: toml.package.version,
    entry,
    source_dir,
    out_dir,
    std_path,
    runtime_path,
    bin: toml.build.bin,
    opt_level,
    debug,
    backend,
    target_triple,
    known_features,
    default_features,
    cc,
    cflags,
    emit,
    aliases,
  })
}

/// Resolve a path that may be relative to the project root.
fn resolve_path<P: AsRef<Path>>(
  root: &Path,
  path: P,
) -> PathBuf {
  let path = path.as_ref();
  if path.is_absolute() {
    path.to_path_buf()
  } else {
    root.join(path)
  }
}

/// Parse emit values into an EmitSet.
fn parse_emit_set(values: &[String]) -> Result<EmitSet, ProjectError> {
  let mut set = EmitSet::default();

  for value in values {
    match value.to_lowercase().as_str() {
      "c" => set.c = true,
      "obj" => set.obj = true,
      other => {
        return Err(ProjectError::InvalidEmit {
          value: other.to_string(),
        });
      },
    }
  }

  Ok(set)
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::project::config::{BuildTomlConfig, IgnisTomlConfig, PackageConfig, ProjectToml};
  use std::fs;

  fn minimal_toml(name: &str) -> ProjectToml {
    ProjectToml {
      package: PackageConfig {
        name: name.to_string(),
        version: "0.1.0".to_string(),
        authors: vec![],
        description: String::new(),
        keywords: vec![],
        license: String::new(),
        repository: String::new(),
      },
      ignis: IgnisTomlConfig {
        std: false, // Disable std for simpler tests
        std_path: None,
        runtime_path: None,
      },
      build: BuildTomlConfig {
        bin: true,
        source_dir: "src".to_string(),
        entry: "main.ign".to_string(),
        out_dir: "build".to_string(),
        opt_level: None,
        debug: false,
        backend: None,
        target: "c".to_string(),
        target_triple: None,
        known_features: Vec::new(),
        default_features: Vec::new(),
        cc: "cc".to_string(),
        cflags: vec![],
        emit: vec![],
      },
      aliases: HashMap::new(),
    }
  }

  fn setup_project_dir(name: &str) -> PathBuf {
    let temp_dir = std::env::temp_dir().join(format!("ignis_resolve_test_{}", name));
    let _ = fs::remove_dir_all(&temp_dir);
    let src_dir = temp_dir.join("src");
    fs::create_dir_all(&src_dir).unwrap();
    fs::write(src_dir.join("main.ign"), "fn main() {}").unwrap();
    temp_dir
  }

  #[test]
  fn test_resolve_minimal_project() {
    let temp_dir = setup_project_dir("minimal");
    let toml = minimal_toml("test");
    let overrides = CliOverrides::default();

    let project = resolve_project(temp_dir.clone(), toml, &overrides).unwrap();

    assert_eq!(project.name, "test");
    assert_eq!(project.version, "0.1.0");
    assert!(project.entry.ends_with("main.ign"));
    assert!(project.source_dir.ends_with("src"));
    assert!(project.out_dir.ends_with("build"));
    assert!(project.bin);
    assert_eq!(project.opt_level, 0);
    assert!(!project.debug);
    assert_eq!(project.backend, TargetBackend::C);
    assert!(project.target_triple.is_none());
    assert!(project.known_features.is_empty());
    assert!(project.default_features.is_empty());
    assert_eq!(project.cc, "cc");
    assert!(project.cflags.is_empty());
    assert!(!project.emit.c);
    assert!(!project.emit.obj);

    fs::remove_dir_all(&temp_dir).unwrap();
  }

  #[test]
  fn test_resolve_backend_from_backend_field() {
    let temp_dir = setup_project_dir("backend_field_qbe");
    let mut toml = minimal_toml("test");
    toml.build.backend = Some("qbe".to_string());

    let overrides = CliOverrides::default();

    let project = resolve_project(temp_dir.clone(), toml, &overrides).unwrap();
    assert_eq!(project.backend, TargetBackend::Qbe);

    fs::remove_dir_all(&temp_dir).unwrap();
  }

  #[test]
  fn test_resolve_backend_override_takes_precedence() {
    let temp_dir = setup_project_dir("backend_override_priority");
    let mut toml = minimal_toml("test");
    toml.build.backend = Some("qbe".to_string());

    let overrides = CliOverrides {
      backend: Some(TargetBackend::C),
      ..Default::default()
    };

    let project = resolve_project(temp_dir.clone(), toml, &overrides).unwrap();
    assert_eq!(project.backend, TargetBackend::C);

    fs::remove_dir_all(&temp_dir).unwrap();
  }

  #[test]
  fn test_resolve_with_cli_overrides() {
    let temp_dir = setup_project_dir("overrides");
    let toml = minimal_toml("test");
    let overrides = CliOverrides {
      opt_level: Some(2),
      debug: Some(true),
      out_dir: Some(temp_dir.join("custom_build")),
      cc: Some("gcc".to_string()),
      cflags: Some(vec!["-Wall".to_string()]),
      emit: Some(vec!["c".to_string(), "obj".to_string()]),
      ..Default::default()
    };

    let project = resolve_project(temp_dir.clone(), toml, &overrides).unwrap();

    assert_eq!(project.opt_level, 2);
    assert!(project.debug);
    assert!(project.out_dir.ends_with("custom_build"));
    assert_eq!(project.cc, "gcc");
    assert_eq!(project.cflags, vec!["-Wall"]);
    assert!(project.emit.c);
    assert!(project.emit.obj);

    fs::remove_dir_all(&temp_dir).unwrap();
  }

  #[test]
  fn test_resolve_entry_not_found() {
    let temp_dir = setup_project_dir("entry_not_found");
    fs::remove_file(temp_dir.join("src/main.ign")).unwrap();

    let toml = minimal_toml("test");
    let overrides = CliOverrides::default();

    let result = resolve_project(temp_dir.clone(), toml, &overrides);
    assert!(matches!(result, Err(ProjectError::EntryNotFound { .. })));

    fs::remove_dir_all(&temp_dir).unwrap();
  }

  #[test]
  fn test_resolve_source_dir_not_found() {
    let temp_dir = setup_project_dir("source_not_found");
    fs::remove_dir_all(temp_dir.join("src")).unwrap();

    let toml = minimal_toml("test");
    let overrides = CliOverrides::default();

    let result = resolve_project(temp_dir.clone(), toml, &overrides);
    assert!(matches!(result, Err(ProjectError::SourceDirNotFound { .. })));

    fs::remove_dir_all(&temp_dir).unwrap();
  }

  #[test]
  fn test_resolve_invalid_opt_level() {
    let temp_dir = setup_project_dir("invalid_opt");
    let mut toml = minimal_toml("test");
    toml.build.opt_level = Some(5);

    let overrides = CliOverrides::default();

    let result = resolve_project(temp_dir.clone(), toml, &overrides);
    assert!(matches!(result, Err(ProjectError::InvalidOptLevel { value: 5, max: 3 })));

    fs::remove_dir_all(&temp_dir).unwrap();
  }

  #[test]
  fn test_resolve_unsupported_target() {
    let temp_dir = setup_project_dir("unsupported_target");
    let mut toml = minimal_toml("test");
    toml.build.target = "wasm".to_string();

    let overrides = CliOverrides::default();

    let result = resolve_project(temp_dir.clone(), toml, &overrides);
    assert!(matches!(result, Err(ProjectError::UnsupportedTarget { .. })));

    fs::remove_dir_all(&temp_dir).unwrap();
  }

  #[test]
  fn test_resolve_invalid_emit() {
    let temp_dir = setup_project_dir("invalid_emit");
    let mut toml = minimal_toml("test");
    toml.build.emit = vec!["invalid".to_string()];

    let overrides = CliOverrides::default();

    let result = resolve_project(temp_dir.clone(), toml, &overrides);
    assert!(matches!(result, Err(ProjectError::InvalidEmit { .. })));

    fs::remove_dir_all(&temp_dir).unwrap();
  }

  #[test]
  fn test_resolve_paths_are_absolute() {
    let temp_dir = setup_project_dir("absolute_paths");
    let toml = minimal_toml("test");
    let overrides = CliOverrides::default();

    let project = resolve_project(temp_dir.clone(), toml, &overrides).unwrap();

    assert!(project.root.is_absolute());
    assert!(project.entry.is_absolute());
    assert!(project.source_dir.is_absolute());
    assert!(project.out_dir.is_absolute());
    assert!(project.toml_path.is_absolute());

    fs::remove_dir_all(&temp_dir).unwrap();
  }

  #[test]
  fn test_parse_emit_set() {
    assert_eq!(parse_emit_set(&[]).unwrap(), EmitSet::default());

    let set = parse_emit_set(&["c".to_string()]).unwrap();
    assert!(set.c);
    assert!(!set.obj);

    let set = parse_emit_set(&["C".to_string(), "OBJ".to_string()]).unwrap();
    assert!(set.c);
    assert!(set.obj);
  }

  #[test]
  fn test_resolve_with_std() {
    let temp_dir = setup_project_dir("with_std");

    // Create a fake std directory
    let std_dir = temp_dir.join("fake_std");
    let runtime_dir = std_dir.join("runtime");
    fs::create_dir_all(&runtime_dir).unwrap();
    fs::write(runtime_dir.join("dummy.c"), "// dummy").unwrap();

    let mut toml = minimal_toml("test");
    toml.ignis.std = true;
    toml.ignis.std_path = Some("fake_std".to_string());

    let overrides = CliOverrides::default();

    let project = resolve_project(temp_dir.clone(), toml, &overrides).unwrap();

    assert!(project.std_path.is_some());
    assert!(project.runtime_path.is_some());
    assert!(project.std_path.unwrap().ends_with("fake_std"));
    assert!(project.runtime_path.unwrap().ends_with("runtime"));

    fs::remove_dir_all(&temp_dir).unwrap();
  }

  #[test]
  fn test_resolve_std_path_not_found() {
    let temp_dir = setup_project_dir("std_not_found");

    let mut toml = minimal_toml("test");
    toml.ignis.std = true;
    toml.ignis.std_path = Some("nonexistent_std".to_string());

    let overrides = CliOverrides::default();

    let result = resolve_project(temp_dir.clone(), toml, &overrides);
    assert!(matches!(result, Err(ProjectError::StdPathNotFound { .. })));

    fs::remove_dir_all(&temp_dir).unwrap();
  }

  #[test]
  fn test_resolve_alias_reserved_std_rejected() {
    let temp_dir = setup_project_dir("alias_reserved");

    let mut toml = minimal_toml("test");
    toml.aliases.insert("std".to_string(), "libs/std".to_string());

    let overrides = CliOverrides::default();

    let result = resolve_project(temp_dir.clone(), toml, &overrides);
    assert!(matches!(result, Err(ProjectError::ReservedAlias { .. })));

    fs::remove_dir_all(&temp_dir).unwrap();
  }

  #[test]
  fn test_resolve_alias_path_not_found() {
    let temp_dir = setup_project_dir("alias_not_found");

    let mut toml = minimal_toml("test");
    toml.aliases.insert("mylib".to_string(), "nonexistent_dir".to_string());

    let overrides = CliOverrides::default();

    let result = resolve_project(temp_dir.clone(), toml, &overrides);
    assert!(matches!(result, Err(ProjectError::AliasPathNotFound { .. })));

    fs::remove_dir_all(&temp_dir).unwrap();
  }

  #[test]
  fn test_resolve_alias_valid() {
    let temp_dir = setup_project_dir("alias_valid");

    // Create the alias target directory
    let libs_dir = temp_dir.join("libs");
    fs::create_dir_all(&libs_dir).unwrap();

    let mut toml = minimal_toml("test");
    toml.aliases.insert("mylib".to_string(), "libs".to_string());

    let overrides = CliOverrides::default();

    let project = resolve_project(temp_dir.clone(), toml, &overrides).unwrap();
    assert_eq!(project.aliases.len(), 1);
    assert!(project.aliases.contains_key("mylib"));
    assert!(project.aliases["mylib"].is_absolute());

    fs::remove_dir_all(&temp_dir).unwrap();
  }
}
