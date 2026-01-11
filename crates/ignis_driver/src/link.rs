use std::path::{Path, PathBuf};
use std::process::Command;

use colored::*;
use ignis_analyzer::modules::ModuleGraph;
use ignis_config::{CHeader, IgnisSTDManifest};
use ignis_type::module::ModuleId;

/// Plan for linking the final executable.
#[derive(Debug, Default)]
pub struct LinkPlan {
  /// Headers to #include in the generated C (ordered, base first)
  pub headers: Vec<CHeader>,
  /// Object files to link (.o from runtime)
  pub objects: Vec<PathBuf>,
  /// Path to the precompiled std archive (libignis_std.a)
  pub std_archive: Option<PathBuf>,
  /// External libraries to link (-l flags)
  pub libs: Vec<String>,
  /// Include directories (-I flags)
  pub include_dirs: Vec<PathBuf>,
}

impl LinkPlan {
  /// Build a link plan from the used modules and manifest.
  pub fn from_modules(
    used_modules: &[ModuleId],
    module_graph: &ModuleGraph,
    std_path: &Path,
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

    // Check for precompiled std library
    let std_build_dir = std_path.join("build");
    let std_header = std_build_dir.join("ignis_std.h");
    let std_archive = std_build_dir.join("libignis_std.a");

    if std_header.exists() {
      if !plan.include_dirs.contains(&std_build_dir) {
        plan.include_dirs.push(std_build_dir);
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
          if let Some(o) = &info.object {
            let obj_path = std_path.join(o);
            if !plan.objects.contains(&obj_path) {
              plan.objects.push(obj_path);
            }
          }
        }

        // Then add all module objects/libs
        for module_name in m.modules.keys() {
          if let Some(info) = m.get_linking_info(module_name) {
            if let Some(o) = &info.object {
              let obj_path = std_path.join(o);
              if !plan.objects.contains(&obj_path) {
                plan.objects.push(obj_path);
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
          if let Some(h) = &info.header {
            let header = CHeader {
              path: h.clone(),
              quoted: info.header_quoted.unwrap_or(false),
            };
            if !plan.headers.iter().any(|existing| existing.path == header.path) {
              plan.headers.push(header);
            }
          }
          if let Some(o) = &info.object {
            let obj_path = std_path.join(o);
            if !plan.objects.contains(&obj_path) {
              plan.objects.push(obj_path);
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
        if let Some(h) = &info.header {
          let header = CHeader {
            path: h.clone(),
            quoted: info.header_quoted.unwrap_or(false),
          };
          // Dedupe by path
          if !plan.headers.iter().any(|existing| existing.path == header.path) {
            plan.headers.push(header);
          }
        }
        if let Some(o) = &info.object {
          let obj_path = std_path.join(o);
          if !plan.objects.contains(&obj_path) {
            plan.objects.push(obj_path);
          }
        }
        if let Some(l) = &info.lib {
          if !plan.libs.contains(l) {
            plan.libs.push(l.clone());
          }
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
    println!(
      "{} Compiling {} -> {}",
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
    println!(
      "{} Linking {} -> {}",
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
      base_header: Some("runtime/types/types.h".to_string()),
      base_header_quoted: Some(true),
      include_dirs: vec![".".to_string()],
    };

    let mut modules = HashMap::new();
    modules.insert("io".to_string(), "io/mod.ign".to_string());
    modules.insert("math".to_string(), "math/mod.ign".to_string());
    modules.insert("string".to_string(), "string/mod.ign".to_string());

    let mut linking = HashMap::new();
    linking.insert(
      "io".to_string(),
      StdLinkingInfo {
        header: Some("runtime/io/io.h".to_string()),
        header_quoted: Some(true),
        object: Some("runtime/io/libignis_io.o".to_string()),
        lib: None,
      },
    );
    linking.insert(
      "math".to_string(),
      StdLinkingInfo {
        header: Some("math.h".to_string()),
        header_quoted: Some(false),
        object: None,
        lib: Some("m".to_string()),
      },
    );
    linking.insert(
      "string".to_string(),
      StdLinkingInfo {
        header: Some("runtime/string/string.h".to_string()),
        header_quoted: Some(true),
        object: Some("runtime/string/libignis_string.o".to_string()),
        lib: None,
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
    let plan = LinkPlan::from_modules(&[], &graph, Path::new("/std"), None);

    assert!(plan.headers.is_empty());
    assert!(plan.objects.is_empty());
    assert!(plan.libs.is_empty());
    assert_eq!(plan.include_dirs, vec![PathBuf::from("/std")]);
  }

  #[test]
  fn test_link_plan_with_manifest_includes_base_header() {
    let manifest = create_test_manifest();
    let graph = ModuleGraph::with_manifest(None, PathBuf::from("/std"), manifest.clone());
    let plan = LinkPlan::from_modules(&[], &graph, Path::new("/std"), Some(&manifest));

    // Base header should be included even with no modules
    assert_eq!(plan.headers.len(), 1);
    assert_eq!(plan.headers[0].path, "runtime/types/types.h");
    assert!(plan.headers[0].quoted);
    assert_eq!(plan.include_dirs, vec![PathBuf::from("/std/.")]);
  }

  #[test]
  fn test_link_plan_single_std_module() {
    let manifest = create_test_manifest();
    let mut graph = ModuleGraph::with_manifest(None, PathBuf::from("/std"), manifest.clone());

    let io_module = Module::new(dummy_file_id(), ModulePath::Std("io".to_string()));
    let io_id = graph.register(io_module);

    let plan = LinkPlan::from_modules(&[io_id], &graph, Path::new("/std"), Some(&manifest));

    // base header first, then io header
    assert_eq!(plan.headers.len(), 2);
    assert_eq!(plan.headers[0].path, "runtime/types/types.h");
    assert!(plan.headers[0].quoted);
    assert_eq!(plan.headers[1].path, "runtime/io/io.h");
    assert!(plan.headers[1].quoted);
    assert_eq!(plan.objects, vec![PathBuf::from("/std/runtime/io/libignis_io.o")]);
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

    let plan = LinkPlan::from_modules(&[io_id, string_id], &graph, Path::new("/std"), Some(&manifest));

    // base header + 2 module headers
    assert_eq!(plan.headers.len(), 3);
    assert_eq!(plan.headers[0].path, "runtime/types/types.h");
    assert!(plan.headers.iter().any(|h| h.path == "runtime/io/io.h" && h.quoted));
    assert!(
      plan
        .headers
        .iter()
        .any(|h| h.path == "runtime/string/string.h" && h.quoted)
    );

    assert_eq!(plan.objects.len(), 2);
    assert!(plan.objects.contains(&PathBuf::from("/std/runtime/io/libignis_io.o")));
    assert!(
      plan
        .objects
        .contains(&PathBuf::from("/std/runtime/string/libignis_string.o"))
    );
  }

  #[test]
  fn test_link_plan_with_external_lib() {
    let manifest = create_test_manifest();
    let mut graph = ModuleGraph::with_manifest(None, PathBuf::from("/std"), manifest.clone());

    let math_module = Module::new(dummy_file_id(), ModulePath::Std("math".to_string()));
    let math_id = graph.register(math_module);

    let plan = LinkPlan::from_modules(&[math_id], &graph, Path::new("/std"), Some(&manifest));

    // base header + math header
    assert_eq!(plan.headers.len(), 2);
    assert_eq!(plan.headers[0].path, "runtime/types/types.h");
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

    let plan = LinkPlan::from_modules(&[project_id], &graph, Path::new("/std"), Some(&manifest));

    // Only base header, no module headers
    assert_eq!(plan.headers.len(), 1);
    assert_eq!(plan.headers[0].path, "runtime/types/types.h");
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
    let plan = LinkPlan::from_modules(&[io_id, io_id], &graph, Path::new("/std"), Some(&manifest));

    // base header + 1 module header (no dupe)
    assert_eq!(plan.headers.len(), 2);
    assert_eq!(plan.objects.len(), 1);
  }
}
