#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum DebugPrint {
  None,
  Lexer,
  Ast,
  Analyzer,
  Hir,
  Ir,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum TargetBackend {
  C,
  Bytecode,
  Iir,
  None,
}

#[derive(Debug, Clone)]
pub struct IgnisProjectConfig {
  pub name: String,
  pub version: String,
  pub authors: Vec<String>,
  pub description: String,
  pub keywords: Vec<String>,
  pub license: String,
  pub repository: String,
  pub target: TargetBackend,
  pub main_file: String,
  pub output_dir: String,
  pub source_dir: String,
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
  ) -> Self {
    Self {
      name,
      version,
      authors,
      description,
      keywords,
      license,
      repository,
      target,
      main_file,
      output_dir,
      source_dir,
    }
  }
}

#[derive(Debug, Clone)]
pub struct IgnisBuildConfig {
  pub file: Option<String>,
  pub isProject: bool,
  pub target: TargetBackend,
  pub optimize: bool,
  pub output_dir: String,
}

impl IgnisBuildConfig {
  pub fn new(
    file: Option<String>,
    target: TargetBackend,
    isProject: bool,
    optimize: bool,
    output_dir: String,
  ) -> Self {
    Self {
      file,
      isProject,
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
