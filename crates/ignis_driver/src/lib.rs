#![allow(clippy::result_unit_err)]

mod api;
pub mod backend;
mod build_layout;
mod context;
mod doc;
mod fixture_tests;
pub mod jobs;
pub mod link;
mod pipeline;
mod process;
pub mod project;
pub mod stages;

pub use api::{
  analyze_project, analyze_project_with_options, analyze_project_with_text, analyze_text, AnalysisOptions,
  AnalyzeProjectOutput, AnalyzeTextOutput, PerFileAnalysis,
};
pub use build_layout::BuildLayout;
pub use context::CompilationContext;
pub use doc::{document_project, DocItem, DocKind, DocMember, DocModule, DocPackage};
pub use link::LinkPlan;
pub use pipeline::{
  build_std, check_runtime, check_std, compile_file, compile_project, run_project_tests,
  run_project_tests_with_options, run_single_file_tests, run_single_file_tests_with_options, run_std_tests,
  parse_partition_spec, TestRunOptions,
};
pub use project::{
  find_project_root, load_project_toml, resolve_project, CliOverrides, EmitSet, Project, ProjectError, ProjectToml,
};
