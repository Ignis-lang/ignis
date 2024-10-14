use clap::{Parser, ValueEnum, Subcommand, ColorChoice};
use ignis_config::{DebugPrint, TargetBackend};

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
pub enum DebugPrintCli {
  /// Default value. Don't print anything
  None,
  /// Print the lexer output
  Lexer,
  /// Print the AST struct
  Ast,
  /// Print the analyzer output
  Analyzer,
  /// Print the HIR struct
  Hir,
  /// Print the IR struct
  Ir,
}

impl Into<DebugPrint> for DebugPrintCli {
  fn into(self) -> DebugPrint {
    match self {
      DebugPrintCli::None => DebugPrint::None,
      DebugPrintCli::Lexer => DebugPrint::Lexer,
      DebugPrintCli::Ast => DebugPrint::Ast,
      DebugPrintCli::Analyzer => DebugPrint::Analyzer,
      DebugPrintCli::Hir => DebugPrint::Hir,
      DebugPrintCli::Ir => DebugPrint::Ir,
    }
  }
}

impl Into<DebugPrint> for &DebugPrintCli {
  fn into(self) -> DebugPrint {
    match self {
      DebugPrintCli::None => DebugPrint::None,
      DebugPrintCli::Lexer => DebugPrint::Lexer,
      DebugPrintCli::Ast => DebugPrint::Ast,
      DebugPrintCli::Analyzer => DebugPrint::Analyzer,
      DebugPrintCli::Hir => DebugPrint::Hir,
      DebugPrintCli::Ir => DebugPrint::Ir,
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
pub enum Target {
  /// Transpile to C an compile to native code
  C,
  /// Transpile to bytecode for the IVM (TODO)
  Bytecode,
  /// Export Intermediate Representation
  Iir,
  /// No target
  None,
}

impl Into<TargetBackend> for Target {
  fn into(self) -> TargetBackend {
    match self {
      Target::C => TargetBackend::C,
      Target::Bytecode => TargetBackend::Bytecode,
      Target::Iir => TargetBackend::Iir,
      Target::None => TargetBackend::None,
    }
  }
}

#[derive(Parser, Debug, Clone, PartialEq)]
pub struct BuildCommand {
  /// The file to build or nothing to build the project
  pub file_path: Option<String>,

  /// The target to compile to
  #[arg(short, long, value_enum, default_value = "c")]
  pub target: Target,

  /// Optimize the output
  #[arg(short = 'O', long)]
  pub optimize: bool,

  /// Output directory
  #[arg(short = 'o', long, default_value = "build")]
  pub output_dir: String,
}

#[derive(Parser, Debug, Clone, PartialEq)]
pub struct InitCommand {
  /// The name of the project
  pub name: String,

  /// Initialize Git repository
  #[arg(short, long, default_value = "false")]
  pub git: bool,

  /// The author of the project
  #[arg(short, long)]
  pub authors: Vec<String>,

  /// The email of the author
  #[arg(short, long, default_value = "ignis@example.com")]
  pub email: String,

  /// The description of the project
  #[arg(short, long, default_value = "A Ignis project")]
  pub description: String,

  /// The license of the project
  #[arg(short, long, default_value = "MIT")]
  pub license: String,

  /// The version of the project
  #[arg(short, long, default_value = "0.1.0")]
  pub project_version: String,

  /// The target to compile to
  #[arg(short, long, value_enum, default_value = "c")]
  pub target: Target,

  /// Main file of the project
  #[arg(short, long, default_value = "main.ign")]
  pub main_file: String,

  /// Output directory
  #[arg(short, long, default_value = "build")]
  pub output_dir: String,

  /// Source directory
  #[arg(short, long, default_value = "src")]
  pub source_dir: String,

  /// Git repository
  #[arg(short, long, default_value = "")]
  pub repository: String,

  /// Keywords of the project
  #[arg(short, long)]
  pub keywords: Vec<String>,
}

#[derive(Subcommand, Clone, PartialEq)]
pub enum SubCommand {
  /// Build file or project
  Build(BuildCommand),
  /// Initialize a new Ignis project
  Init(InitCommand),
}

#[derive(Parser)]
#[command(author, version, about = "The Ignis compiler", long_about = None)]
#[command(propagate_version = true)]
#[command(color = ColorChoice::Always)]
pub struct Cli {
  #[command(subcommand)]
  pub subcommand: SubCommand,

  /// Print debug information
  #[arg(short, long, value_enum, action = clap::ArgAction::Append)]
  pub debug: Vec<DebugPrintCli>,

  /// Don't print any output
  #[arg(long, default_value = "false")]
  pub quiet: bool,

  /// Use verbose output
  #[arg(long, short, action = clap::ArgAction::Count, global = true)]
  pub verbose: u8,
}
