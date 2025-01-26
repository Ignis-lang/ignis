use serde::{Deserialize, Serialize};

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
  Bytecode,
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
}

impl IgnisBuildConfig {
  pub fn new(
    file: Option<String>,
    target: TargetBackend,
    is_project: bool,
    optimize: bool,
    output_dir: String,
  ) -> Self {
    Self {
      file,
      is_project,
      target,
      optimize,
      output_dir,
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
