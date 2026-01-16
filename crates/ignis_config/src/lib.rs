use std::collections::HashMap;

use serde::{Deserialize, Serialize};

/// Header to include in generated C code with style info
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CHeader {
  /// The path/name of the header
  pub path: String,
  /// true = #include "...", false = #include <...>
  pub quoted: bool,
}

/// Toolchain configuration for the standard library
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct StdToolchainConfig {
  /// Base header providing fundamental types (e.g., "runtime/types/types.h")
  pub base_header: Option<String>,
  /// Whether base_header uses quoted includes (default: true)
  pub base_header_quoted: Option<bool>,
  /// Include directories relative to std_root (default: ["."])
  #[serde(default)]
  pub include_dirs: Vec<String>,
}

/// Linking information for a std module (header and object file)
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct StdLinkingInfo {
  /// Path to the C header file (optional)
  pub header: Option<String>,
  /// Whether header uses quoted includes (default: false for system headers)
  pub header_quoted: Option<bool>,
  /// Path to the object file for linking (optional)
  pub object: Option<String>,
  /// External library to link (e.g., "m" for -lm)
  pub lib: Option<String>,
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
  /// Toolchain configuration
  #[serde(default)]
  pub toolchain: StdToolchainConfig,
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
  /// Get the base header as a CHeader if configured
  pub fn get_base_header(&self) -> Option<CHeader> {
    self.toolchain.base_header.as_ref().map(|path| CHeader {
      path: path.clone(),
      quoted: self.toolchain.base_header_quoted.unwrap_or(true),
    })
  }

  /// Get include directories (defaults to ["."] if empty)
  pub fn get_include_dirs(&self) -> Vec<&str> {
    if self.toolchain.include_dirs.is_empty() {
      vec!["."]
    } else {
      self.toolchain.include_dirs.iter().map(|s| s.as_str()).collect()
    }
  }

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
pub enum DumpKind {
  Lexer,
  Ast,
  Defs,
  Types,
  Hir,
  HirSummary,
  Lir,
  Ir,
  C,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum DebugTrace {
  Analyzer,
  Parser,
  Lexer,
  Mono,
  Ownership,
  Lir,
  Codegen,
  Link,
  Std,
}

/// Controls the verbosity level of CLI output.
///
/// - `Quiet`: No output except errors
/// - `Detailed`: Structured progress output (default)
/// - `Verbose`: Detailed output with internal phases
#[derive(Debug, Copy, Clone, PartialEq, Eq, Default)]
pub enum OutputLevel {
  Quiet,
  #[default]
  Detailed,
  Verbose,
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
  pub dump: Vec<DumpKind>,
  pub dump_dir: Option<String>,
  pub dump_hir: Option<String>,
  pub emit_c: Option<String>,
  pub emit_obj: Option<String>,
  pub emit_bin: Option<String>,
  pub rebuild_std: bool,
  pub bin: bool,
  pub lib: bool,
  pub check_mode: bool,
  pub analyze_only: bool,
}

impl IgnisBuildConfig {
  pub fn new(
    file: Option<String>,
    target: TargetBackend,
    is_project: bool,
    optimize: bool,
    output_dir: String,
    dump: Vec<DumpKind>,
    dump_dir: Option<String>,
    dump_hir: Option<String>,
    emit_c: Option<String>,
    emit_obj: Option<String>,
    emit_bin: Option<String>,
    rebuild_std: bool,
    bin: bool,
    lib: bool,
    check_mode: bool,
    analyze_only: bool,
  ) -> Self {
    Self {
      file,
      is_project,
      target,
      optimize,
      output_dir,
      dump,
      dump_dir,
      dump_hir,
      emit_c,
      emit_obj,
      emit_bin,
      rebuild_std,
      bin,
      lib,
      check_mode,
      analyze_only,
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
  pub debug: bool,
  pub debug_trace: Vec<DebugTrace>,
  pub quiet: bool,
  pub verbose: u8,
  pub output_level: OutputLevel,
  pub build: bool,
  pub test: bool,
  pub init: bool,
  pub build_std: bool,
  pub build_std_output_dir: Option<String>,
  pub std_path: String,
  pub std: bool,
  pub auto_load_std: bool,
  pub manifest: IgnisSTDManifest,
  pub check_std: bool,
  pub check_runtime: bool,
  pub runtime_path_override: Option<String>,
}

impl IgnisConfig {
  pub fn new(
    project_config: Option<IgnisProjectConfig>,
    build_config: Option<IgnisBuildConfig>,
    init_config: Option<IgnisInitConfig>,
    debug: bool,
    debug_trace: Vec<DebugTrace>,
    quiet: bool,
    verbose: u8,
    output_level: OutputLevel,
    build: bool,
    test: bool,
    init: bool,
    build_std: bool,
    build_std_output_dir: Option<String>,
    std_path: String,
    std: bool,
    auto_load_std: bool,
    manifest: IgnisSTDManifest,
    check_std: bool,
    check_runtime: bool,
    runtime_path_override: Option<String>,
  ) -> Self {
    Self {
      project_config,
      build_config,
      init_config,
      debug,
      debug_trace,
      quiet,
      verbose,
      output_level,
      build,
      test,
      init,
      build_std,
      build_std_output_dir,
      std_path,
      std,
      auto_load_std,
      manifest,
      check_std,
      check_runtime,
      runtime_path_override,
    }
  }

  pub fn new_basic(
    debug: bool,
    debug_trace: Vec<DebugTrace>,
    quiet: bool,
    verbose: u8,
  ) -> Self {
    let output_level = if quiet {
      OutputLevel::Quiet
    } else if verbose > 0 {
      OutputLevel::Verbose
    } else {
      OutputLevel::Detailed
    };

    Self {
      debug,
      debug_trace,
      quiet,
      verbose,
      output_level,
      ..Self::default()
    }
  }
}
