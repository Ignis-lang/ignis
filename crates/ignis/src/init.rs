use std::fs::OpenOptions;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::Command;

use colored::*;
use serde::Serialize;

use crate::cli::{InitCommand, Target};

#[derive(Serialize)]
struct ProjectTomlFile {
  package: PackageSection,
  ignis: IgnisSection,
  build: BuildSection,
}

#[derive(Serialize)]
struct PackageSection {
  name: String,
  version: String,
  authors: Vec<String>,
  description: String,
  keywords: Vec<String>,
  license: String,
  repository: String,
}

#[derive(Serialize)]
struct IgnisSection {
  std: bool,

  #[serde(skip_serializing_if = "Option::is_none")]
  std_path: Option<String>,

  #[serde(skip_serializing_if = "Option::is_none")]
  runtime_path: Option<String>,
}

#[derive(Serialize)]
struct BuildSection {
  bin: bool,
  source_dir: String,
  entry: String,
  out_dir: String,
  opt_level: u8,
  debug: bool,
  target: String,
  cc: String,
  cflags: Vec<String>,
  emit: Vec<String>,
}

struct DetectedStdPaths {
  std_path: Option<String>,
  runtime_path: Option<String>,
}

pub fn run_init(
  cmd: &InitCommand,
  quiet: bool,
) -> Result<(), ()> {
  let project_root = resolve_project_root(&cmd.name)?;

  if project_root.exists() {
    if !project_root.is_dir() {
      eprintln!(
        "{} Path exists but is not a directory: '{}'",
        "Error:".red().bold(),
        project_root.display()
      );
      return Err(());
    }
  } else {
    std::fs::create_dir_all(&project_root).map_err(|error| {
      eprintln!(
        "{} Failed to create project directory '{}': {}",
        "Error:".red().bold(),
        project_root.display(),
        error
      );
    })?;
  }

  let package_name = match package_name_from_root(&project_root) {
    Some(name) => name,
    None => {
      eprintln!(
        "{} Could not determine project name from path '{}'",
        "Error:".red().bold(),
        project_root.display()
      );
      return Err(());
    },
  };

  let source_dir_path = project_root.join(&cmd.source_dir);
  std::fs::create_dir_all(&source_dir_path).map_err(|error| {
    eprintln!(
      "{} Failed to create source directory '{}': {}",
      "Error:".red().bold(),
      source_dir_path.display(),
      error
    );
  })?;

  let entry_file = resolve_entry_file(cmd);
  let entry_path = source_dir_path.join(&entry_file);
  let manifest_path = project_root.join("ignis.toml");

  if manifest_path.exists() {
    eprintln!(
      "{} Refusing to overwrite existing file '{}'",
      "Error:".red().bold(),
      manifest_path.display()
    );
    return Err(());
  }

  if entry_path.exists() {
    eprintln!(
      "{} Refusing to overwrite existing file '{}'",
      "Error:".red().bold(),
      entry_path.display()
    );
    return Err(());
  }

  let detected_std_paths = detect_std_paths(&project_root);

  let project_toml = ProjectTomlFile {
    package: PackageSection {
      name: package_name.clone(),
      version: cmd.project_version.clone(),
      authors: build_authors(&cmd.authors, &cmd.email),
      description: cmd.description.clone(),
      keywords: trim_and_filter(&cmd.keywords),
      license: cmd.license.clone(),
      repository: cmd.repository.clone(),
    },
    ignis: IgnisSection {
      std: true,
      std_path: detected_std_paths.std_path,
      runtime_path: detected_std_paths.runtime_path,
    },
    build: BuildSection {
      bin: !cmd.lib,
      source_dir: cmd.source_dir.clone(),
      entry: entry_file.clone(),
      out_dir: cmd.output_dir.clone(),
      opt_level: 0,
      debug: false,
      target: target_to_toml_value(&cmd.target),
      cc: "cc".to_string(),
      cflags: Vec::new(),
      emit: Vec::new(),
    },
  };

  let mut toml_content = toml::to_string_pretty(&project_toml).map_err(|error| {
    eprintln!("{} Failed to serialize ignis.toml: {}", "Error:".red().bold(), error);
  })?;
  toml_content.push('\n');

  let source_content = if cmd.lib { lib_template() } else { main_template() };

  write_new_file(&manifest_path, &toml_content)?;
  write_new_file(&entry_path, source_content)?;

  if !cmd.no_git {
    init_git_repository(&project_root)?;
  }

  if !quiet {
    let kind = if cmd.lib { "library" } else { "binary" };
    println!(
      "Initialized Ignis {} project '{}' at {}",
      kind,
      package_name,
      project_root.display()
    );
  }

  Ok(())
}

fn resolve_project_root(name: &str) -> Result<PathBuf, ()> {
  let path = PathBuf::from(name);

  if path.is_absolute() {
    return Ok(path);
  }

  let cwd = std::env::current_dir().map_err(|error| {
    eprintln!("{} Failed to resolve current directory: {}", "Error:".red().bold(), error);
  })?;

  let root = cwd.join(path);
  if root.exists() {
    return root.canonicalize().map_err(|error| {
      eprintln!(
        "{} Failed to canonicalize project path '{}': {}",
        "Error:".red().bold(),
        root.display(),
        error
      );
    });
  }

  Ok(root)
}

fn package_name_from_root(project_root: &Path) -> Option<String> {
  project_root
    .file_name()
    .and_then(|name| name.to_str())
    .map(|name| name.trim())
    .filter(|name| !name.is_empty())
    .map(ToString::to_string)
}

fn resolve_entry_file(cmd: &InitCommand) -> String {
  if let Some(file_name) = &cmd.main_file {
    return file_name.clone();
  }

  if cmd.lib {
    "lib.ign".to_string()
  } else {
    "main.ign".to_string()
  }
}

fn detect_std_paths(project_root: &Path) -> DetectedStdPaths {
  if let Some(from_env) = detect_std_paths_from_env() {
    return from_env;
  }

  if let Some(parent) = project_root.parent() {
    let std_candidate = parent.join("std");
    if std_candidate.is_dir() {
      let runtime_candidate = std_candidate.join("runtime");

      return DetectedStdPaths {
        std_path: Some("../std".to_string()),
        runtime_path: runtime_candidate.is_dir().then(|| "../std/runtime".to_string()),
      };
    }
  }

  DetectedStdPaths {
    std_path: None,
    runtime_path: None,
  }
}

fn detect_std_paths_from_env() -> Option<DetectedStdPaths> {
  let std_path = std::env::var("IGNIS_STD_PATH").ok()?;
  let trimmed = std_path.trim();

  if trimmed.is_empty() {
    return None;
  }

  let std_path = PathBuf::from(trimmed);
  if !std_path.exists() {
    return None;
  }

  let std_path = std_path.canonicalize().ok()?;

  let runtime_path = std_path.join("runtime");

  Some(DetectedStdPaths {
    std_path: Some(std_path.to_string_lossy().to_string()),
    runtime_path: runtime_path
      .is_dir()
      .then(|| runtime_path.to_string_lossy().to_string()),
  })
}

fn build_authors(
  authors: &[String],
  email: &str,
) -> Vec<String> {
  let cleaned_authors = trim_and_filter(authors);
  let cleaned_email = email.trim();

  if cleaned_authors.is_empty() {
    return Vec::new();
  }

  if cleaned_email.is_empty() {
    return cleaned_authors;
  }

  cleaned_authors
    .into_iter()
    .map(|author| format!("{} <{}>", author, cleaned_email))
    .collect()
}

fn trim_and_filter(values: &[String]) -> Vec<String> {
  values
    .iter()
    .map(|value| value.trim())
    .filter(|value| !value.is_empty())
    .map(ToString::to_string)
    .collect()
}

fn target_to_toml_value(target: &Target) -> String {
  match target {
    Target::C => "c".to_string(),
    Target::Iir => "iir".to_string(),
    Target::None => "none".to_string(),
  }
}

fn write_new_file(
  path: &Path,
  content: &str,
) -> Result<(), ()> {
  if let Some(parent) = path.parent() {
    std::fs::create_dir_all(parent).map_err(|error| {
      eprintln!(
        "{} Failed to create parent directory for '{}': {}",
        "Error:".red().bold(),
        path.display(),
        error
      );
    })?;
  }

  let mut file = OpenOptions::new()
    .write(true)
    .create_new(true)
    .open(path)
    .map_err(|error| {
      eprintln!(
        "{} Failed to create file '{}': {}",
        "Error:".red().bold(),
        path.display(),
        error
      );
    })?;

  file.write_all(content.as_bytes()).map_err(|error| {
    eprintln!("{} Failed to write file '{}': {}", "Error:".red().bold(), path.display(), error);
  })
}

fn init_git_repository(project_root: &Path) -> Result<(), ()> {
  if project_root.join(".git").exists() {
    return Ok(());
  }

  let status = Command::new("git")
    .arg("init")
    .current_dir(project_root)
    .status()
    .map_err(|error| {
      eprintln!(
        "{} Failed to initialize git repository at '{}': {}",
        "Error:".red().bold(),
        project_root.display(),
        error
      );
    })?;

  if status.success() {
    Ok(())
  } else {
    eprintln!("{} 'git init' failed in '{}'", "Error:".red().bold(), project_root.display());
    Err(())
  }
}

fn main_template() -> &'static str {
  "import Io from \"std::io\";\n\nfunction main(): i32 {\n  Io::println(\"Hello, Ignis!\");\n  return 0;\n}\n"
}

fn lib_template() -> &'static str {
  "export function add(a: i32, b: i32): i32 {\n  return a + b;\n}\n"
}

#[cfg(test)]
mod tests {
  use super::*;

  fn base_command() -> InitCommand {
    InitCommand {
      name: "demo".to_string(),
      no_git: false,
      lib: false,
      authors: Vec::new(),
      email: "ignis@example.com".to_string(),
      description: "A Ignis project".to_string(),
      license: "MIT".to_string(),
      project_version: "0.1.0".to_string(),
      target: Target::C,
      main_file: None,
      output_dir: "build".to_string(),
      source_dir: "src".to_string(),
      repository: String::new(),
      keywords: Vec::new(),
    }
  }

  #[test]
  fn resolve_entry_file_uses_main_by_default() {
    let cmd = base_command();
    assert_eq!(resolve_entry_file(&cmd), "main.ign");
  }

  #[test]
  fn resolve_entry_file_uses_lib_for_library_projects() {
    let mut cmd = base_command();
    cmd.lib = true;

    assert_eq!(resolve_entry_file(&cmd), "lib.ign");
  }

  #[test]
  fn resolve_entry_file_honors_custom_value() {
    let mut cmd = base_command();
    cmd.main_file = Some("entry.ign".to_string());

    assert_eq!(resolve_entry_file(&cmd), "entry.ign");
  }

  #[test]
  fn templates_match_expected_shape() {
    assert!(main_template().contains("Hello, Ignis!"));
    assert!(lib_template().contains("export function add"));
  }
}
