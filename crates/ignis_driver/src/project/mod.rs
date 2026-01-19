//! Project discovery, loading, and resolution.
//!
//! This module provides functionality to:
//! - Find `ignis.toml` by searching upward from a directory
//! - Parse the TOML configuration
//! - Resolve relative paths to absolute paths
//! - Validate all configuration values
//!
//! # Example
//!
//! ```ignore
//! use ignis_driver::project::{find_project_root, load_project_toml, resolve_project, CliOverrides};
//!
//! let cwd = std::env::current_dir()?;
//! if let Some(root) = find_project_root(&cwd) {
//!     let toml = load_project_toml(&root.join("ignis.toml"))?;
//!     let project = resolve_project(root, toml, &CliOverrides::default())?;
//!     println!("Project: {}", project.name);
//! }
//! ```

pub mod config;
pub mod errors;
pub mod find;
pub mod resolve;

// Re-export commonly used types
pub use config::ProjectToml;
pub use errors::ProjectError;
pub use find::{find_project_root, load_project_toml, PROJECT_FILE};
pub use resolve::{resolve_project, CliOverrides, EmitSet, Project};
