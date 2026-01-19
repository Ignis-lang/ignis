use std::path::{Path, PathBuf};
use std::process::Command;

use colored::*;
use ignis_analyzer::modules::ModuleGraph;
use ignis_config::{CHeader, IgnisSTDManifest, StdLinkingInfo};
use ignis_type::module::ModuleId;

/// Get the linkable path from StdLinkingInfo, preferring archive over object.
fn get_linkable_path(info: &StdLinkingInfo) -> Option<&String> {
  info.archive.as_ref().or(info.object.as_ref())
}

/// Get headers from StdLinkingInfo, preferring `headers` over `header`.
fn get_headers(info: &StdLinkingInfo) -> Vec<CHeader> {
  let quoted = info.header_quoted.unwrap_or(false);
  if !info.headers.is_empty() {
    info
      .headers
      .iter()
      .map(|h| CHeader {
        path: h.clone(),
        quoted,
      })
      .collect()
  } else if let Some(h) = &info.header {
    vec![CHeader {
      path: h.clone(),
      quoted,
    }]
  } else {
    vec![]
  }
}

/// Plan for linking the final executable.
#[derive(Debug, Default, Clone)]
pub struct LinkPlan {
  /// Headers to #include in the generated C (ordered, base first)
  pub headers: Vec<CHeader>,
  /// Object files to link (.o from runtime)
  pub objects: Vec<PathBuf>,
  /// Path to the precompiled std archive (libignis_std.a)
  pub std_archive: Option<PathBuf>,
  /// Path to the user code archive (libignis_user.a)
  pub user_archive: Option<PathBuf>,
  /// External libraries to link (-l flags)
  pub libs: Vec<String>,
  /// Include directories (-I flags)
  pub include_dirs: Vec<PathBuf>,
}

impl LinkPlan {
  pub fn from_modules(
    used_modules: &[ModuleId],
    module_graph: &ModuleGraph,
    std_path: &Path,
    build_dir: &Path,
    manifest: Option<&IgnisSTDManifest>,
  ) -> Self {
    let mut plan = Self::default();

    // Add include directories from manifest toolchain
    if let Some(m) = manifest {
      for dir in m.get_include_dirs() {
        let inc_path = std_path.join(dir);
        if !plan.include_dirs.contains(&inc_path) {
          plan.include_dirs.push(inc_path);
        }
      }

      // Add base header first
      if let Some(base) = m.get_base_header() {
        plan.headers.push(base);
      }
    } else {
      // Fallback: just add std_path as include dir
      plan.include_dirs.push(std_path.to_path_buf());
    }

    // Check for precompiled std library in new layout: build/std/
    let std_build_dir = build_dir.join("std");
    let std_include_dir = std_build_dir.join("include");
    let std_header = std_include_dir.join("ignis_std.h");
    let std_archive = std_build_dir.join("lib/libignis_std.a");

    if std_header.exists() {
      if !plan.include_dirs.contains(&std_include_dir) {
        plan.include_dirs.push(std_include_dir);
      }
      plan.headers.push(CHeader {
        path: "ignis_std.h".to_string(),
        quoted: true,
      });
    }

    if std_archive.exists() {
      plan.std_archive = Some(std_archive);

      // When using precompiled std, we need ALL runtime objects because
      // libignis_std.a depends on them. Add all objects/libs from manifest.
      if let Some(m) = manifest {
        // First add ignis_rt (core runtime) - it's not a module but always needed
        if let Some(info) = m.get_linking_info("ignis_rt") {
          if let Some(p) = get_linkable_path(info) {
            let link_path = std_path.join(p);
            if !plan.objects.contains(&link_path) {
              plan.objects.push(link_path);
            }
          }
        }

        // Then add all module objects/libs
        for module_name in m.modules.keys() {
          if let Some(info) = m.get_linking_info(module_name) {
            if let Some(p) = get_linkable_path(info) {
              let link_path = std_path.join(p);
              if !plan.objects.contains(&link_path) {
                plan.objects.push(link_path);
              }
            }
            if let Some(l) = &info.lib {
              if !plan.libs.contains(l) {
                plan.libs.push(l.clone());
              }
            }
          }
        }
      }
    }

    for module_id in used_modules {
      let module = module_graph.modules.get(module_id);
      if let Some(name) = module.path.std_module_name() {
        if let Some(info) = module_graph.get_linking_info(name) {
          for header in get_headers(info) {
            if !plan.headers.iter().any(|existing| existing.path == header.path) {
              plan.headers.push(header);
            }
          }
          if let Some(p) = get_linkable_path(info) {
            let link_path = std_path.join(p);
            if !plan.objects.contains(&link_path) {
              plan.objects.push(link_path);
            }
          }
          if let Some(l) = &info.lib {
            if !plan.libs.contains(l) {
              plan.libs.push(l.clone());
            }
          }
        }
      }
    }

    plan
  }

  /// Build a link plan from ALL modules in the manifest (for build-std).
  /// Unlike from_modules, this uses the manifest module names directly
  /// rather than relying on ModulePath::std_module_name().
  pub fn from_manifest(
    manifest: &IgnisSTDManifest,
    std_path: &Path,
  ) -> Self {
    let mut plan = Self::default();

    // Add include directories from manifest toolchain
    for dir in manifest.get_include_dirs() {
      let inc_path = std_path.join(dir);
      if !plan.include_dirs.contains(&inc_path) {
        plan.include_dirs.push(inc_path);
      }
    }

    // Add base header first
    if let Some(base) = manifest.get_base_header() {
      plan.headers.push(base);
    }

    // Add headers/objects/libs for all modules in manifest
    for module_name in manifest.modules.keys() {
      if let Some(info) = manifest.get_linking_info(module_name) {
        for header in get_headers(info) {
          // Dedupe by path
          if !plan.headers.iter().any(|existing| existing.path == header.path) {
            plan.headers.push(header);
          }
        }
        if let Some(p) = get_linkable_path(info) {
          let link_path = std_path.join(p);
          if !plan.objects.contains(&link_path) {
            plan.objects.push(link_path);
          }
        }
        if let Some(l) = &info.lib {
          if !plan.libs.contains(l) {
            plan.libs.push(l.clone());
          }
        }
      }
    }

    // Also add ignis_rt linking info (not in modules list)
    if let Some(info) = manifest.get_linking_info("ignis_rt") {
      if let Some(p) = get_linkable_path(info) {
        let link_path = std_path.join(p);
        if !plan.objects.contains(&link_path) {
          plan.objects.push(link_path);
        }
      }
    }

    plan
  }
}

pub fn format_tool_error(
  tool: &str,
  action: &str,
  stderr: &str,
) -> String {
  format!(
    "{} {} {} failed:\n{}",
    "Error:".red().bold(),
    tool.yellow(),
    action,
    stderr.trim()
  )
}

/// Compile a C file to an object file using gcc.
pub fn compile_to_object(
  c_path: &Path,
  obj_path: &Path,
  link_plan: &LinkPlan,
  quiet: bool,
) -> Result<(), String> {
  let mut cmd = Command::new("gcc");
  cmd.arg("-c").arg(c_path).arg("-o").arg(obj_path);

  for inc_dir in &link_plan.include_dirs {
    cmd.arg("-I").arg(inc_dir);
  }

  if !quiet {
    eprintln!(
      "    {} Compiling {} -> {}",
      "-->".bright_green().bold(),
      c_path.display(),
      obj_path.display()
    );
  }

  let output = cmd.output().map_err(|e| format!("Failed to run gcc: {}", e))?;

  if !output.status.success() {
    let stderr = String::from_utf8_lossy(&output.stderr);
    return Err(format_tool_error("gcc", "compilation", &stderr));
  }

  Ok(())
}

/// Link object files into an executable using gcc.
pub fn link_executable(
  obj_path: &Path,
  bin_path: &Path,
  link_plan: &LinkPlan,
  quiet: bool,
) -> Result<(), String> {
  let mut cmd = Command::new("gcc");
  cmd.arg(obj_path);

  // Link std archive if present (libignis_std.a)
  if let Some(std_archive) = &link_plan.std_archive {
    cmd.arg(std_archive);
  }

  for obj in &link_plan.objects {
    cmd.arg(obj);
  }

  cmd.arg("-o").arg(bin_path);

  for lib in &link_plan.libs {
    cmd.arg(format!("-l{}", lib));
  }

  if !quiet {
    eprintln!(
      "    {} Linking {} -> {}",
      "-->".bright_green().bold(),
      obj_path.display(),
      bin_path.display()
    );
  }

  let output = cmd.output().map_err(|e| format!("Failed to run gcc: {}", e))?;

  if !output.status.success() {
    let stderr = String::from_utf8_lossy(&output.stderr);
    return Err(format_tool_error("gcc", "linking", &stderr));
  }

  Ok(())
}

/// Link multiple object files into an executable using gcc.
pub fn link_executable_multi(
  obj_paths: &[PathBuf],
  bin_path: &Path,
  link_plan: &LinkPlan,
  quiet: bool,
) -> Result<(), String> {
  let mut cmd = Command::new("gcc");

  for obj in obj_paths {
    cmd.arg(obj);
  }

  // Link user archive if present (libignis_user.a)
  if let Some(user_archive) = &link_plan.user_archive {
    cmd.arg(user_archive);
  }

  // Link std archive if present (libignis_std.a)
  if let Some(std_archive) = &link_plan.std_archive {
    cmd.arg(std_archive);
  }

  for obj in &link_plan.objects {
    cmd.arg(obj);
  }

  cmd.arg("-o").arg(bin_path);

  for lib in &link_plan.libs {
    cmd.arg(format!("-l{}", lib));
  }

  if !quiet {
    let archive_count = link_plan.user_archive.is_some() as usize + link_plan.std_archive.is_some() as usize;
    eprintln!(
      "    {} Linking {} objects + {} archives -> {}",
      "-->".bright_green().bold(),
      obj_paths.len(),
      archive_count,
      bin_path.display()
    );
  }

  let output = cmd.output().map_err(|e| format!("Failed to run gcc: {}", e))?;

  if !output.status.success() {
    let stderr = String::from_utf8_lossy(&output.stderr);
    return Err(format_tool_error("gcc", "linking", &stderr));
  }

  Ok(())
}

/// Rebuild the std runtime by running make in std/runtime.
pub fn rebuild_std_runtime(
  std_path: &Path,
  quiet: bool,
) -> Result<(), String> {
  let runtime_path = std_path.join("runtime");

  if !runtime_path.exists() {
    return Err(format!("Runtime directory not found: {}", runtime_path.display()));
  }

  if !quiet {
    println!("{} Rebuilding std runtime...", "-->".bright_cyan().bold());
  }

  let output = Command::new("make")
    .arg("-C")
    .arg(&runtime_path)
    .arg("all")
    .output()
    .map_err(|e| format!("Failed to run make: {}", e))?;

  if !output.status.success() {
    let stderr = String::from_utf8_lossy(&output.stderr);
    return Err(format_tool_error("make", "rebuild", &stderr));
  }

  Ok(())
}

#[cfg(test)]
mod tests {
  use super::*;
  use ignis_config::{IgnisSTDManifest, StdLinkingInfo, StdToolchainConfig};
  use ignis_type::file::{FileId, SourceMap};
  use ignis_type::module::{Module, ModulePath};
  use std::collections::HashMap;

  fn dummy_file_id() -> FileId {
    let mut sm = SourceMap::new();
    sm.add_file("test.ign", "".to_string())
  }

  fn create_test_manifest() -> IgnisSTDManifest {
    let toolchain = StdToolchainConfig {
      base_header: Some("runtime/ignis_rt.h".to_string()),
      base_header_quoted: Some(true),
      include_dirs: vec![".".to_string()],
    };

    let mut modules = HashMap::new();
    modules.insert("io".to_string(), "io/mod.ign".to_string());
    modules.insert("math".to_string(), "math/mod.ign".to_string());
    modules.insert("string".to_string(), "string/mod.ign".to_string());

    let mut linking = HashMap::new();
    linking.insert(
      "ignis_rt".to_string(),
      StdLinkingInfo {
        header: Some("runtime/ignis_rt.h".to_string()),
        headers: vec![],
        header_quoted: Some(true),
        object: None,
        archive: Some("runtime/libignis_rt.a".to_string()),
        lib: None,
      },
    );
    linking.insert(
      "math".to_string(),
      StdLinkingInfo {
        header: Some("math.h".to_string()),
        headers: vec![],
        header_quoted: Some(false),
        object: None,
        archive: None,
        lib: Some("m".to_string()),
      },
    );

    IgnisSTDManifest {
      toolchain,
      modules,
      linking,
      auto_load: None,
    }
  }

  #[test]
  fn test_link_plan_empty_modules_no_manifest() {
    let graph = ModuleGraph::new(None, PathBuf::from("/std"));
    let plan = LinkPlan::from_modules(&[], &graph, Path::new("/std"), Path::new("/build"), None);

    assert!(plan.headers.is_empty());
    assert!(plan.objects.is_empty());
    assert!(plan.libs.is_empty());
    assert_eq!(plan.include_dirs, vec![PathBuf::from("/std")]);
  }

  #[test]
  fn test_link_plan_with_manifest_includes_base_header() {
    let manifest = create_test_manifest();
    let graph = ModuleGraph::with_manifest(None, PathBuf::from("/std"), manifest.clone());
    let plan = LinkPlan::from_modules(&[], &graph, Path::new("/std"), Path::new("/build"), Some(&manifest));

    // Base header should be included even with no modules
    assert_eq!(plan.headers.len(), 1);
    assert_eq!(plan.headers[0].path, "runtime/ignis_rt.h");
    assert!(plan.headers[0].quoted);
    assert_eq!(plan.include_dirs, vec![PathBuf::from("/std/.")]);
  }

  #[test]
  fn test_link_plan_single_std_module() {
    let manifest = create_test_manifest();
    let mut graph = ModuleGraph::with_manifest(None, PathBuf::from("/std"), manifest.clone());

    let io_module = Module::new(dummy_file_id(), ModulePath::Std("io".to_string()));
    let io_id = graph.register(io_module);

    let plan = LinkPlan::from_modules(&[io_id], &graph, Path::new("/std"), Path::new("/build"), Some(&manifest));

    // base header only (io has no separate linking info now)
    assert_eq!(plan.headers.len(), 1);
    assert_eq!(plan.headers[0].path, "runtime/ignis_rt.h");
    assert!(plan.headers[0].quoted);
    // No module-specific objects (all in libignis_rt.a)
    assert!(plan.objects.is_empty());
    assert!(plan.libs.is_empty());
  }

  #[test]
  fn test_link_plan_multiple_std_modules() {
    let manifest = create_test_manifest();
    let mut graph = ModuleGraph::with_manifest(None, PathBuf::from("/std"), manifest.clone());

    let io_module = Module::new(dummy_file_id(), ModulePath::Std("io".to_string()));
    let io_id = graph.register(io_module);

    let string_module = Module::new(dummy_file_id(), ModulePath::Std("string".to_string()));
    let string_id = graph.register(string_module);

    let plan = LinkPlan::from_modules(
      &[io_id, string_id],
      &graph,
      Path::new("/std"),
      Path::new("/build"),
      Some(&manifest),
    );

    // base header only (modules have no separate headers in new layout)
    assert_eq!(plan.headers.len(), 1);
    assert_eq!(plan.headers[0].path, "runtime/ignis_rt.h");

    // No module-specific objects (all consolidated in libignis_rt.a)
    assert!(plan.objects.is_empty());
  }

  #[test]
  fn test_link_plan_with_external_lib() {
    let manifest = create_test_manifest();
    let mut graph = ModuleGraph::with_manifest(None, PathBuf::from("/std"), manifest.clone());

    let math_module = Module::new(dummy_file_id(), ModulePath::Std("math".to_string()));
    let math_id = graph.register(math_module);

    let plan = LinkPlan::from_modules(&[math_id], &graph, Path::new("/std"), Path::new("/build"), Some(&manifest));

    // base header + math header
    assert_eq!(plan.headers.len(), 2);
    assert_eq!(plan.headers[0].path, "runtime/ignis_rt.h");
    assert_eq!(plan.headers[1].path, "math.h");
    assert!(!plan.headers[1].quoted); // system header
    assert!(plan.objects.is_empty());
    assert_eq!(plan.libs, vec!["m"]);
  }

  #[test]
  fn test_link_plan_project_module_ignored() {
    let manifest = create_test_manifest();
    let mut graph =
      ModuleGraph::with_manifest(Some(PathBuf::from("/project")), PathBuf::from("/std"), manifest.clone());

    let project_module = Module::new(dummy_file_id(), ModulePath::Project(PathBuf::from("/project/utils.ign")));
    let project_id = graph.register(project_module);

    let plan = LinkPlan::from_modules(&[project_id], &graph, Path::new("/std"), Path::new("/build"), Some(&manifest));

    // Only base header, no module headers
    assert_eq!(plan.headers.len(), 1);
    assert_eq!(plan.headers[0].path, "runtime/ignis_rt.h");
    assert!(plan.objects.is_empty());
    assert!(plan.libs.is_empty());
  }

  #[test]
  fn test_link_plan_no_duplicates() {
    let manifest = create_test_manifest();
    let mut graph = ModuleGraph::with_manifest(None, PathBuf::from("/std"), manifest.clone());

    let io_module = Module::new(dummy_file_id(), ModulePath::Std("io".to_string()));
    let io_id = graph.register(io_module);

    // Same module twice should not duplicate entries
    let plan = LinkPlan::from_modules(&[io_id, io_id], &graph, Path::new("/std"), Path::new("/build"), Some(&manifest));

    // base header only (no dupe)
    assert_eq!(plan.headers.len(), 1);
    assert!(plan.objects.is_empty());
  }
}
