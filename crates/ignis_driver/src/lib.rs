#![allow(clippy::result_unit_err)]

mod api;
mod build_layout;
mod codegen_backend;
mod context;
pub mod link;
mod pipeline;
pub mod project;

pub use api::{
  analyze_project, analyze_project_with_options, analyze_project_with_text, analyze_text, AnalysisOptions,
  AnalyzeProjectOutput, AnalyzeTextOutput, PerFileAnalysis,
};
pub use build_layout::BuildLayout;
pub use context::CompilationContext;
pub use link::LinkPlan;
pub use pipeline::{build_std, check_runtime, check_std, compile_file, compile_project};
pub use project::{
  find_project_root, load_project_toml, resolve_project, CliOverrides, EmitSet, Project, ProjectError, ProjectToml,
};
