use std::collections::HashMap;

use serde::{Deserialize, Serialize};

/// Linking information for a std module (header and object file)
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct StdLinkingInfo {
  /// Path to the C header file (optional)
  pub header: Option<String>,
  /// Path to the object file for linking (optional)
  pub object: Option<String>,
}

/// Configuration for auto-loading modules
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct StdAutoLoad {
  /// List of module names to auto-load (e.g., ["prelude"])
  pub modules: Vec<String>,
}

/// Manifest for the Ignis standard library
///
/// Expected format in manifest.toml:
/// ```toml
/// [modules]
/// io = "io/mod.ign"
/// math = "math/mod.ign"
///
/// [linking.io]
/// header = "runtime/io.h"
/// object = "runtime/libio.o"
///
/// [auto_load]
/// modules = ["prelude"]
/// ```
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct IgnisSTDManifest {
  /// Module name -> relative path to .ign file
  #[serde(default)]
  pub modules: HashMap<String, String>,
  /// Module name -> linking info (header and object file)
  #[serde(default)]
  pub linking: HashMap<String, StdLinkingInfo>,
  /// Modules to auto-load
  #[serde(default)]
  pub auto_load: Option<StdAutoLoad>,
}

impl IgnisSTDManifest {
  /// Get the relative path to a module's .ign file
  pub fn get_module_path(
    &self,
    name: &str,
  ) -> Option<&String> {
    self.modules.get(name)
  }

  /// Get linking info for a module
  pub fn get_linking_info(
    &self,
    name: &str,
  ) -> Option<&StdLinkingInfo> {
    self.linking.get(name)
  }

  /// Check if a module should be auto-loaded
  pub fn is_auto_load(
    &self,
    name: &str,
  ) -> bool {
    self
      .auto_load
      .as_ref()
      .map(|a| a.modules.contains(&name.to_string()))
      .unwrap_or(false)
  }

  /// Get all auto-load module names
  pub fn get_auto_load_modules(&self) -> Vec<&str> {
    self
      .auto_load
      .as_ref()
      .map(|a| a.modules.iter().map(|s| s.as_str()).collect())
      .unwrap_or_default()
  }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum DebugPrint {
  None,
  Lexer,
  Ast,
  Analyzer,
  Hir,
  Ir,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Default, Serialize, Deserialize)]
pub enum TargetBackend {
  #[default]
  C,
  Iir,
  None,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct IgnisProjectBuildConfig {
  pub source_dir: String,
  pub main_file: String,
  pub target: TargetBackend,
  pub optimize: bool,
  pub output_dir: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct IgnisProjectIgnisConfig {
  pub std_path: String,
  pub std: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IgnisProjectConfig {
  pub name: String,
  pub version: String,
  pub authors: Vec<String>,
  pub description: String,
  pub keywords: Vec<String>,
  pub license: String,
  pub repository: String,
  pub build: IgnisProjectBuildConfig,
  pub ignis: IgnisProjectIgnisConfig,
  // TODO: dependencies
}

impl IgnisProjectConfig {
  pub fn new(
    name: String,
    version: String,
    authors: Vec<String>,
    description: String,
    keywords: Vec<String>,
    license: String,
    repository: String,
    target: TargetBackend,
    main_file: String,
    output_dir: String,
    source_dir: String,
    optimize: bool,
    std_path: String,
    std: bool,
  ) -> Self {
    Self {
      name,
      version,
      authors,
      description,
      keywords,
      license,
      repository,
      build: IgnisProjectBuildConfig {
        source_dir,
        main_file,
        target,
        optimize,
        output_dir,
      },
      ignis: IgnisProjectIgnisConfig { std_path, std },
    }
  }
}

#[derive(Debug, Clone)]
pub struct IgnisBuildConfig {
  pub file: Option<String>,
  pub is_project: bool,
  pub target: TargetBackend,
  pub optimize: bool,
  pub output_dir: String,
  pub dump_types: bool,
  pub dump_defs: bool,
  pub dump_hir: Option<String>,
  pub dump_hir_summary: bool,
  pub dump_lir: bool,
  pub emit_c: Option<String>,
}

impl IgnisBuildConfig {
  pub fn new(
    file: Option<String>,
    target: TargetBackend,
    is_project: bool,
    optimize: bool,
    output_dir: String,
    dump_types: bool,
    dump_defs: bool,
    dump_hir: Option<String>,
    dump_hir_summary: bool,
    dump_lir: bool,
    emit_c: Option<String>,
  ) -> Self {
    Self {
      file,
      is_project,
      target,
      optimize,
      output_dir,
      dump_types,
      dump_defs,
      dump_hir,
      dump_hir_summary,
      dump_lir,
      emit_c,
    }
  }
}

#[derive(Debug, Clone)]
pub struct IgnisInitConfig {
  pub name: String,
  pub version: String,
  pub authors: Vec<String>,
  pub description: String,
  pub keywords: Vec<String>,
  pub license: String,
  pub repository: String,
  pub git: bool,
  pub target: TargetBackend,
}

impl IgnisInitConfig {
  pub fn new(
    name: String,
    version: String,
    authors: Vec<String>,
    description: String,
    keywords: Vec<String>,
    license: String,
    repository: String,
    git: bool,
    target: TargetBackend,
  ) -> Self {
    Self {
      name,
      version,
      authors,
      description,
      keywords,
      license,
      repository,
      git,
      target,
    }
  }
}

#[derive(Debug, Clone, Default)]
pub struct IgnisConfig {
  pub project_config: Option<IgnisProjectConfig>,
  pub build_config: Option<IgnisBuildConfig>,
  pub init_config: Option<IgnisInitConfig>,
  pub debug: Vec<DebugPrint>,
  pub quiet: bool,
  pub verbose: u8,
  pub build: bool,
  pub test: bool,
  pub init: bool,
  pub std_path: String,
  pub std: bool,
  pub auto_load_std: bool,
  pub manifest: IgnisSTDManifest,
}

impl IgnisConfig {
  pub fn new(
    project_config: Option<IgnisProjectConfig>,
    build_config: Option<IgnisBuildConfig>,
    init_config: Option<IgnisInitConfig>,
    debug: Vec<DebugPrint>,
    quiet: bool,
    verbose: u8,
    build: bool,
    test: bool,
    init: bool,
    std_path: String,
    std: bool,
    auto_load_std: bool,
    manifest: IgnisSTDManifest,
  ) -> Self {
    Self {
      project_config,
      build_config,
      init_config,
      debug,
      quiet,
      verbose,
      build,
      test,
      init,
      std_path,
      std,
      auto_load_std,
      manifest,
    }
  }

  pub fn new_basic(
    debug: Vec<DebugPrint>,
    quiet: bool,
    verbose: u8,
  ) -> Self {
    Self {
      debug,
      quiet,
      verbose,
      ..Self::default()
    }
  }
}
