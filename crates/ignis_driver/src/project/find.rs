//! Functions for locating and loading ignis.toml.

use std::path::{Path, PathBuf};

use crate::project::config::ProjectToml;
use crate::project::errors::ProjectError;

/// The project manifest filename.
pub const PROJECT_FILE: &str = "ignis.toml";

/// Search upward from `start` to find a directory containing ignis.toml.
///
/// Returns the directory containing ignis.toml (project root), not the file itself.
/// Returns `None` if no ignis.toml is found before reaching the filesystem root.
pub fn find_project_root(start: &Path) -> Option<PathBuf> {
  let mut current = if start.is_file() {
    start.parent()?.to_path_buf()
  } else {
    start.to_path_buf()
  };

  loop {
    let candidate = current.join(PROJECT_FILE);

    if candidate.is_file() {
      return Some(current);
    }

    if !current.pop() {
      return None;
    }
  }
}

/// Load and parse an ignis.toml file.
///
/// # Arguments
/// * `toml_path` - Path to the ignis.toml file (not the directory).
///
/// # Errors
/// - `IoError` if the file cannot be read.
/// - `TomlParseError` if the file is not valid TOML or doesn't match the schema.
pub fn load_project_toml(toml_path: &Path) -> Result<ProjectToml, ProjectError> {
  let content = std::fs::read_to_string(toml_path).map_err(|e| ProjectError::IoError {
    path: toml_path.to_path_buf(),
    source: e,
  })?;

  toml::from_str(&content).map_err(|e| ProjectError::TomlParseError {
    path: toml_path.to_path_buf(),
    message: e.to_string(),
  })
}

#[cfg(test)]
mod tests {
  use super::*;
  use std::fs;

  #[test]
  fn test_find_project_root_in_current_dir() {
    let temp_dir = std::env::temp_dir().join("ignis_find_test_current");
    let _ = fs::remove_dir_all(&temp_dir);
    fs::create_dir_all(&temp_dir).unwrap();

    let toml_path = temp_dir.join(PROJECT_FILE);
    fs::write(
      &toml_path,
      r#"
[package]
name = "test"
version = "0.1.0"
"#,
    )
    .unwrap();

    let result = find_project_root(&temp_dir);
    assert_eq!(result, Some(temp_dir.clone()));

    fs::remove_dir_all(&temp_dir).unwrap();
  }

  #[test]
  fn test_find_project_root_in_parent() {
    let temp_dir = std::env::temp_dir().join("ignis_find_test_parent");
    let _ = fs::remove_dir_all(&temp_dir);
    let sub_dir = temp_dir.join("src").join("nested");
    fs::create_dir_all(&sub_dir).unwrap();

    let toml_path = temp_dir.join(PROJECT_FILE);
    fs::write(
      &toml_path,
      r#"
[package]
name = "test"
version = "0.1.0"
"#,
    )
    .unwrap();

    let result = find_project_root(&sub_dir);
    assert_eq!(result, Some(temp_dir.clone()));

    fs::remove_dir_all(&temp_dir).unwrap();
  }

  #[test]
  fn test_find_project_root_not_found() {
    let temp_dir = std::env::temp_dir().join("ignis_find_test_notfound");
    let _ = fs::remove_dir_all(&temp_dir);
    fs::create_dir_all(&temp_dir).unwrap();

    // No ignis.toml created
    let result = find_project_root(&temp_dir);
    assert!(result.is_none());

    fs::remove_dir_all(&temp_dir).unwrap();
  }

  #[test]
  fn test_find_project_root_from_file() {
    let temp_dir = std::env::temp_dir().join("ignis_find_test_fromfile");
    let _ = fs::remove_dir_all(&temp_dir);
    let src_dir = temp_dir.join("src");
    fs::create_dir_all(&src_dir).unwrap();

    let toml_path = temp_dir.join(PROJECT_FILE);
    fs::write(
      &toml_path,
      r#"
[package]
name = "test"
version = "0.1.0"
"#,
    )
    .unwrap();

    let file_path = src_dir.join("main.ign");
    fs::write(&file_path, "// main").unwrap();

    // Start from a file, should find project in parent
    let result = find_project_root(&file_path);
    assert_eq!(result, Some(temp_dir.clone()));

    fs::remove_dir_all(&temp_dir).unwrap();
  }

  #[test]
  fn test_load_project_toml_success() {
    let temp_dir = std::env::temp_dir().join("ignis_load_test_success");
    let _ = fs::remove_dir_all(&temp_dir);
    fs::create_dir_all(&temp_dir).unwrap();

    let toml_path = temp_dir.join(PROJECT_FILE);
    fs::write(
      &toml_path,
      r#"
[package]
name = "myproject"
version = "1.2.3"

[ignis]
std = true
std_path = "/path/to/std"

[build]
entry = "app.ign"
opt_level = 3
"#,
    )
    .unwrap();

    let result = load_project_toml(&toml_path).unwrap();
    assert_eq!(result.package.name, "myproject");
    assert_eq!(result.package.version, "1.2.3");
    assert!(result.ignis.std);
    assert_eq!(result.ignis.std_path, Some("/path/to/std".to_string()));
    assert_eq!(result.build.entry, "app.ign");
    assert_eq!(result.build.opt_level, Some(3));

    fs::remove_dir_all(&temp_dir).unwrap();
  }

  #[test]
  fn test_load_project_toml_io_error() {
    let result = load_project_toml(Path::new("/nonexistent/path/ignis.toml"));
    assert!(matches!(result, Err(ProjectError::IoError { .. })));
  }

  #[test]
  fn test_load_project_toml_parse_error() {
    let temp_dir = std::env::temp_dir().join("ignis_load_test_parse_error");
    let _ = fs::remove_dir_all(&temp_dir);
    fs::create_dir_all(&temp_dir).unwrap();

    let toml_path = temp_dir.join(PROJECT_FILE);
    fs::write(&toml_path, "this is not valid toml {{{{").unwrap();

    let result = load_project_toml(&toml_path);
    assert!(matches!(result, Err(ProjectError::TomlParseError { .. })));

    fs::remove_dir_all(&temp_dir).unwrap();
  }

  #[test]
  fn test_load_project_toml_missing_required_field() {
    let temp_dir = std::env::temp_dir().join("ignis_load_test_missing_field");
    let _ = fs::remove_dir_all(&temp_dir);
    fs::create_dir_all(&temp_dir).unwrap();

    let toml_path = temp_dir.join(PROJECT_FILE);
    // Missing [package] section
    fs::write(
      &toml_path,
      r#"
[build]
entry = "main.ign"
"#,
    )
    .unwrap();

    let result = load_project_toml(&toml_path);
    assert!(matches!(result, Err(ProjectError::TomlParseError { .. })));

    fs::remove_dir_all(&temp_dir).unwrap();
  }
}
