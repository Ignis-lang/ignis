use std::collections::BTreeMap;
use std::fmt::Write as FmtWrite;
use std::io;
use std::path::{Path, PathBuf};

/// Build output directory structure.
///
/// ```text
/// {base}/
///   std/
///     include/   .h files (ignis_std.h, std_io.h, ...)
///     src/       .c files
///     obj/       .o files
///     lib/       libignis_std.a
///     .stamp     cache invalidation
///   user/
///     include/   .h files (preserves source structure)
///     src/       .c files
///     obj/       .o files
///     lib/       libignis_user.a
///     .stamps/   per-module stamp files
///   bin/         final executables
/// ```
#[derive(Debug, Clone)]
pub struct BuildLayout {
  base: PathBuf,
  project_name: String,
  /// Project root directory. Used to make source paths relative.
  /// If None, source paths are used as-is (assumed relative).
  project_root: Option<PathBuf>,
}

impl BuildLayout {
  pub fn new(
    project_name: &str,
    out_dir: &Path,
  ) -> Self {
    Self {
      base: out_dir.to_path_buf(),
      project_name: project_name.to_string(),
      project_root: None,
    }
  }

  /// Create a BuildLayout with a project root for relativizing source paths.
  pub fn with_project_root(
    project_name: &str,
    out_dir: &Path,
    project_root: &Path,
  ) -> Self {
    Self {
      base: out_dir.to_path_buf(),
      project_name: project_name.to_string(),
      project_root: Some(project_root.to_path_buf()),
    }
  }

  /// Strip project_root prefix from source_path if present, otherwise return as-is.
  pub fn relativize(
    &self,
    source_path: &Path,
  ) -> PathBuf {
    if let Some(root) = &self.project_root
      && let Ok(rel) = source_path.strip_prefix(root)
    {
      return rel.to_path_buf();
    }
    source_path.to_path_buf()
  }

  pub fn base(&self) -> &Path {
    &self.base
  }

  pub fn project_name(&self) -> &str {
    &self.project_name
  }

  // -- Std paths --

  pub fn std_dir(&self) -> PathBuf {
    self.base.join("std")
  }

  pub fn std_include_dir(&self) -> PathBuf {
    self.std_dir().join("include")
  }

  pub fn std_src_dir(&self) -> PathBuf {
    self.std_dir().join("src")
  }

  pub fn std_obj_dir(&self) -> PathBuf {
    self.std_dir().join("obj")
  }

  pub fn std_lib_dir(&self) -> PathBuf {
    self.std_dir().join("lib")
  }

  pub fn std_lib_path(&self) -> PathBuf {
    self.std_lib_dir().join("libignis_std.a")
  }

  pub fn std_umbrella_header(&self) -> PathBuf {
    self.std_include_dir().join("ignis_std.h")
  }

  pub fn std_module_header(
    &self,
    module: &str,
  ) -> PathBuf {
    self.std_include_dir().join(format!("std_{}.h", module))
  }

  pub fn std_module_src(
    &self,
    module: &str,
  ) -> PathBuf {
    self.std_src_dir().join(format!("std_{}.c", module))
  }

  pub fn std_module_obj(
    &self,
    module: &str,
  ) -> PathBuf {
    self.std_obj_dir().join(format!("std_{}.o", module))
  }

  pub fn std_stamp_path(&self) -> PathBuf {
    self.std_dir().join(".stamp")
  }

  // -- User paths --

  pub fn user_dir(&self) -> PathBuf {
    self.base.join("user")
  }

  pub fn user_include_dir(&self) -> PathBuf {
    self.user_dir().join("include")
  }

  pub fn user_src_dir(&self) -> PathBuf {
    self.user_dir().join("src")
  }

  pub fn user_obj_dir(&self) -> PathBuf {
    self.user_dir().join("obj")
  }

  pub fn user_lib_dir(&self) -> PathBuf {
    self.user_dir().join("lib")
  }

  pub fn user_lib_path(&self) -> PathBuf {
    self.user_lib_dir().join("libignis_user.a")
  }

  pub fn user_umbrella_header(&self) -> PathBuf {
    self.user_include_dir().join("ignis_user.h")
  }

  pub fn user_stamps_dir(&self) -> PathBuf {
    self.user_dir().join(".stamps")
  }

  /// Per-module stamp: `build/user/.stamps/src/main.stamp`
  pub fn user_module_stamp_path(
    &self,
    source_path: &Path,
  ) -> PathBuf {
    let rel = self.relativize(source_path);
    self.user_stamps_dir().join(rel.with_extension("stamp"))
  }

  /// Preserves source path structure: `src/math/utils.ign` -> `include/src/math/utils.h`
  pub fn user_module_header(
    &self,
    source_path: &Path,
  ) -> PathBuf {
    let rel = self.relativize(source_path);
    self.user_include_dir().join(rel.with_extension("h"))
  }

  /// Preserves source path structure: `src/math/utils.ign` -> `src/src/math/utils.c`
  pub fn user_module_src(
    &self,
    source_path: &Path,
  ) -> PathBuf {
    let rel = self.relativize(source_path);
    self.user_src_dir().join(rel.with_extension("c"))
  }

  /// Preserves source path structure: `src/math/utils.ign` -> `obj/src/math/utils.o`
  pub fn user_module_obj(
    &self,
    source_path: &Path,
  ) -> PathBuf {
    let rel = self.relativize(source_path);
    self.user_obj_dir().join(rel.with_extension("o"))
  }

  #[deprecated(note = "Use per-module stamps via user_module_stamp_path")]
  pub fn user_stamp_path(&self) -> PathBuf {
    self.user_dir().join(".stamp")
  }

  // -- Output paths --

  pub fn bin_dir(&self) -> PathBuf {
    self.base.join("bin")
  }

  pub fn bin_path(&self) -> PathBuf {
    self.bin_dir().join(&self.project_name)
  }

  // -- Directory creation --

  pub fn create_all_dirs(&self) -> io::Result<()> {
    std::fs::create_dir_all(self.std_include_dir())?;
    std::fs::create_dir_all(self.std_src_dir())?;
    std::fs::create_dir_all(self.std_obj_dir())?;
    std::fs::create_dir_all(self.std_lib_dir())?;
    std::fs::create_dir_all(self.user_include_dir())?;
    std::fs::create_dir_all(self.user_src_dir())?;
    std::fs::create_dir_all(self.user_obj_dir())?;
    std::fs::create_dir_all(self.user_lib_dir())?;
    std::fs::create_dir_all(self.user_stamps_dir())?;
    std::fs::create_dir_all(self.bin_dir())?;
    Ok(())
  }

  pub fn create_std_dirs(&self) -> io::Result<()> {
    std::fs::create_dir_all(self.std_include_dir())?;
    std::fs::create_dir_all(self.std_src_dir())?;
    std::fs::create_dir_all(self.std_obj_dir())?;
    std::fs::create_dir_all(self.std_lib_dir())?;
    Ok(())
  }

  pub fn create_user_dirs(&self) -> io::Result<()> {
    std::fs::create_dir_all(self.user_include_dir())?;
    std::fs::create_dir_all(self.user_src_dir())?;
    std::fs::create_dir_all(self.user_obj_dir())?;
    std::fs::create_dir_all(self.user_lib_dir())?;
    std::fs::create_dir_all(self.user_stamps_dir())?;
    std::fs::create_dir_all(self.bin_dir())?;
    Ok(())
  }

  pub fn ensure_user_module_dirs(
    &self,
    source_path: &Path,
  ) -> io::Result<()> {
    for path in [
      self.user_module_header(source_path),
      self.user_module_src(source_path),
      self.user_module_obj(source_path),
      self.user_module_stamp_path(source_path),
    ] {
      if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)?;
      }
    }
    Ok(())
  }
}

// =============================================================================
// Stamp v2: content-hash based cache invalidation
// =============================================================================
//
// Format (key=value, sorted):
//   version=2
//   compiler_version=0.1.0
//   codegen_abi_version=1
//   target=
//   file_count=N
//   file.0.path=...
//   file.0.hash=...
//   ...
//
// For per-module stamps (user modules with deps):
//   version=2
//   compiler_version=...
//   codegen_abi_version=...
//   target=
//   self_path=...
//   self_hash=...
//   dep_count=N
//   dep.0.path=...
//   dep.0.hash=...
//   ...

const STAMP_VERSION: &str = "2";

/// Build configuration that affects output.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BuildFingerprint {
  pub compiler_version: String,
  pub codegen_abi_version: u32,
  pub target: String,
}

impl BuildFingerprint {
  #[allow(dead_code)]
  pub fn new(
    compiler_version: &str,
    codegen_abi_version: u32,
  ) -> Self {
    Self {
      compiler_version: compiler_version.to_string(),
      codegen_abi_version,
      target: String::new(), // placeholder for future target triple
    }
  }
}

/// A file entry with its content hash.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FileEntry {
  pub path: PathBuf,
  pub hash: String,
}

/// Stamp for std library (all files in a directory).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StdStamp {
  pub fingerprint: BuildFingerprint,
  pub files: Vec<FileEntry>,
}

/// Stamp for a single user module with its dependencies.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleStamp {
  pub fingerprint: BuildFingerprint,
  pub self_path: PathBuf,
  pub self_hash: String,
  pub deps: Vec<FileEntry>,
}

// -- Hashing --

pub fn hash_file(path: &Path) -> io::Result<String> {
  let contents = std::fs::read(path)?;
  Ok(blake3::hash(&contents).to_hex().to_string())
}

#[allow(dead_code)]
pub fn hash_bytes(data: &[u8]) -> String {
  blake3::hash(data).to_hex().to_string()
}

// -- Write if changed --

/// Write content only if it differs from existing file.
/// Returns true if file was written, false if unchanged.
pub fn write_if_changed(
  path: &Path,
  content: &str,
) -> io::Result<bool> {
  if path.exists()
    && let Ok(existing) = std::fs::read_to_string(path)
    && existing == content
  {
    return Ok(false);
  }
  if let Some(parent) = path.parent() {
    std::fs::create_dir_all(parent)?;
  }
  std::fs::write(path, content)?;
  Ok(true)
}

// -- Std stamp --

impl StdStamp {
  pub fn compute(
    std_path: &Path,
    fingerprint: BuildFingerprint,
  ) -> io::Result<Self> {
    let mut files = Vec::new();
    collect_source_hashes(std_path, &mut files)?;
    files.sort_by(|a, b| a.path.cmp(&b.path));
    Ok(Self { fingerprint, files })
  }

  pub fn serialize(&self) -> String {
    let mut out = String::new();
    writeln!(out, "version={}", STAMP_VERSION).unwrap();
    writeln!(out, "compiler_version={}", self.fingerprint.compiler_version).unwrap();
    writeln!(out, "codegen_abi_version={}", self.fingerprint.codegen_abi_version).unwrap();
    writeln!(out, "target={}", self.fingerprint.target).unwrap();
    writeln!(out, "file_count={}", self.files.len()).unwrap();
    for (i, f) in self.files.iter().enumerate() {
      writeln!(out, "file.{}.path={}", i, f.path.display()).unwrap();
      writeln!(out, "file.{}.hash={}", i, f.hash).unwrap();
    }
    out
  }

  pub fn deserialize(s: &str) -> Option<Self> {
    let map = parse_kv(s);

    if map.get("version")? != STAMP_VERSION {
      return None;
    }

    let fingerprint = BuildFingerprint {
      compiler_version: map.get("compiler_version")?.to_string(),
      codegen_abi_version: map.get("codegen_abi_version")?.parse().ok()?,
      target: map.get("target").map(|s| s.to_string()).unwrap_or_default(),
    };

    let file_count: usize = map.get("file_count")?.parse().ok()?;
    let mut files = Vec::with_capacity(file_count);

    for i in 0..file_count {
      let path = PathBuf::from(map.get(&format!("file.{}.path", i))?);
      let hash = map.get(&format!("file.{}.hash", i))?.to_string();
      files.push(FileEntry { path, hash });
    }

    Some(Self { fingerprint, files })
  }
}

fn collect_source_hashes(
  dir: &Path,
  files: &mut Vec<FileEntry>,
) -> io::Result<()> {
  if !dir.is_dir() {
    return Ok(());
  }

  for entry in std::fs::read_dir(dir)? {
    let entry = entry?;
    let path = entry.path();

    if path.is_dir() {
      collect_source_hashes(&path, files)?;
    } else if path.extension().map(|e| e == "ign").unwrap_or(false) {
      let hash = hash_file(&path)?;
      files.push(FileEntry { path, hash });
    }
  }

  Ok(())
}

pub fn write_std_stamp(
  stamp_path: &Path,
  stamp: &StdStamp,
) -> io::Result<bool> {
  write_if_changed(stamp_path, &stamp.serialize())
}

pub fn is_std_stamp_valid(
  stamp_path: &Path,
  std_path: &Path,
  fingerprint: &BuildFingerprint,
) -> bool {
  let saved = match std::fs::read_to_string(stamp_path) {
    Ok(s) => s,
    Err(_) => return false,
  };

  let saved_stamp = match StdStamp::deserialize(&saved) {
    Some(s) => s,
    None => return false,
  };

  if saved_stamp.fingerprint != *fingerprint {
    return false;
  }

  let current = match StdStamp::compute(std_path, fingerprint.clone()) {
    Ok(s) => s,
    Err(_) => return false,
  };

  saved_stamp.files == current.files
}

// -- Module stamp (user modules with deps) --

impl ModuleStamp {
  /// Create a stamp for a user module.
  /// `deps` should be the transitive dependencies with their content hashes.
  pub fn new(
    fingerprint: BuildFingerprint,
    self_path: PathBuf,
    self_hash: String,
    deps: Vec<FileEntry>,
  ) -> Self {
    let mut deps = deps;
    deps.sort_by(|a, b| a.path.cmp(&b.path));
    Self {
      fingerprint,
      self_path,
      self_hash,
      deps,
    }
  }

  pub fn serialize(&self) -> String {
    let mut out = String::new();
    writeln!(out, "version={}", STAMP_VERSION).unwrap();
    writeln!(out, "compiler_version={}", self.fingerprint.compiler_version).unwrap();
    writeln!(out, "codegen_abi_version={}", self.fingerprint.codegen_abi_version).unwrap();
    writeln!(out, "target={}", self.fingerprint.target).unwrap();
    writeln!(out, "self_path={}", self.self_path.display()).unwrap();
    writeln!(out, "self_hash={}", self.self_hash).unwrap();
    writeln!(out, "dep_count={}", self.deps.len()).unwrap();
    for (i, d) in self.deps.iter().enumerate() {
      writeln!(out, "dep.{}.path={}", i, d.path.display()).unwrap();
      writeln!(out, "dep.{}.hash={}", i, d.hash).unwrap();
    }
    out
  }

  pub fn deserialize(s: &str) -> Option<Self> {
    let map = parse_kv(s);

    if map.get("version")? != STAMP_VERSION {
      return None;
    }

    let fingerprint = BuildFingerprint {
      compiler_version: map.get("compiler_version")?.to_string(),
      codegen_abi_version: map.get("codegen_abi_version")?.parse().ok()?,
      target: map.get("target").map(|s| s.to_string()).unwrap_or_default(),
    };

    let self_path = PathBuf::from(map.get("self_path")?);
    let self_hash = map.get("self_hash")?.to_string();

    let dep_count: usize = map.get("dep_count")?.parse().ok()?;
    let mut deps = Vec::with_capacity(dep_count);

    for i in 0..dep_count {
      let path = PathBuf::from(map.get(&format!("dep.{}.path", i))?);
      let hash = map.get(&format!("dep.{}.hash", i))?.to_string();
      deps.push(FileEntry { path, hash });
    }

    Some(Self {
      fingerprint,
      self_path,
      self_hash,
      deps,
    })
  }
}

pub fn write_module_stamp(
  stamp_path: &Path,
  stamp: &ModuleStamp,
) -> io::Result<bool> {
  write_if_changed(stamp_path, &stamp.serialize())
}

/// Check if a module stamp is still valid.
/// `current_self_hash` is the current hash of the module's source file.
/// `current_deps` are the current hashes of all transitive dependencies.
pub fn is_module_stamp_valid(
  stamp_path: &Path,
  fingerprint: &BuildFingerprint,
  current_self_hash: &str,
  current_deps: &[FileEntry],
) -> bool {
  let saved = match std::fs::read_to_string(stamp_path) {
    Ok(s) => s,
    Err(_) => return false,
  };

  let saved_stamp = match ModuleStamp::deserialize(&saved) {
    Some(s) => s,
    None => return false,
  };

  if saved_stamp.fingerprint != *fingerprint {
    return false;
  }

  if saved_stamp.self_hash != current_self_hash {
    return false;
  }

  // Compare deps (both are sorted)
  let mut current_sorted = current_deps.to_vec();
  current_sorted.sort_by(|a, b| a.path.cmp(&b.path));

  saved_stamp.deps == current_sorted
}

// -- Helpers --

fn parse_kv(s: &str) -> BTreeMap<String, String> {
  let mut map = BTreeMap::new();
  for line in s.lines() {
    if let Some((k, v)) = line.split_once('=') {
      map.insert(k.to_string(), v.to_string());
    }
  }
  map
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
  use super::*;

  // -- BuildLayout tests --

  #[test]
  fn test_std_paths() {
    let layout = BuildLayout::new("myapp", Path::new("/tmp/build"));

    assert_eq!(layout.std_dir(), PathBuf::from("/tmp/build/std"));
    assert_eq!(layout.std_include_dir(), PathBuf::from("/tmp/build/std/include"));
    assert_eq!(layout.std_src_dir(), PathBuf::from("/tmp/build/std/src"));
    assert_eq!(layout.std_obj_dir(), PathBuf::from("/tmp/build/std/obj"));
    assert_eq!(layout.std_lib_dir(), PathBuf::from("/tmp/build/std/lib"));
    assert_eq!(layout.std_lib_path(), PathBuf::from("/tmp/build/std/lib/libignis_std.a"));
    assert_eq!(layout.std_stamp_path(), PathBuf::from("/tmp/build/std/.stamp"));
  }

  #[test]
  fn test_std_module_paths() {
    let layout = BuildLayout::new("myapp", Path::new("/tmp/build"));

    assert_eq!(layout.std_module_header("io"), PathBuf::from("/tmp/build/std/include/std_io.h"));
    assert_eq!(
      layout.std_module_header("string"),
      PathBuf::from("/tmp/build/std/include/std_string.h")
    );
    assert_eq!(layout.std_module_src("io"), PathBuf::from("/tmp/build/std/src/std_io.c"));
    assert_eq!(layout.std_module_obj("io"), PathBuf::from("/tmp/build/std/obj/std_io.o"));
  }

  #[test]
  fn test_user_paths() {
    let layout = BuildLayout::new("myapp", Path::new("/tmp/build"));

    assert_eq!(layout.user_dir(), PathBuf::from("/tmp/build/user"));
    assert_eq!(layout.user_include_dir(), PathBuf::from("/tmp/build/user/include"));
    assert_eq!(layout.user_src_dir(), PathBuf::from("/tmp/build/user/src"));
    assert_eq!(layout.user_obj_dir(), PathBuf::from("/tmp/build/user/obj"));
    assert_eq!(layout.user_lib_dir(), PathBuf::from("/tmp/build/user/lib"));
    assert_eq!(layout.user_lib_path(), PathBuf::from("/tmp/build/user/lib/libignis_user.a"));
    assert_eq!(
      layout.user_umbrella_header(),
      PathBuf::from("/tmp/build/user/include/ignis_user.h")
    );
    assert_eq!(layout.user_stamps_dir(), PathBuf::from("/tmp/build/user/.stamps"));
  }

  #[test]
  fn test_user_module_paths_preserves_structure() {
    let layout = BuildLayout::new("myapp", Path::new("/tmp/build"));

    let main = Path::new("main.ign");
    assert_eq!(layout.user_module_header(main), PathBuf::from("/tmp/build/user/include/main.h"));
    assert_eq!(layout.user_module_src(main), PathBuf::from("/tmp/build/user/src/main.c"));
    assert_eq!(layout.user_module_obj(main), PathBuf::from("/tmp/build/user/obj/main.o"));
    assert_eq!(
      layout.user_module_stamp_path(main),
      PathBuf::from("/tmp/build/user/.stamps/main.stamp")
    );

    let nested = Path::new("src/math/utils.ign");
    assert_eq!(
      layout.user_module_header(nested),
      PathBuf::from("/tmp/build/user/include/src/math/utils.h")
    );
    assert_eq!(
      layout.user_module_src(nested),
      PathBuf::from("/tmp/build/user/src/src/math/utils.c")
    );
    assert_eq!(
      layout.user_module_obj(nested),
      PathBuf::from("/tmp/build/user/obj/src/math/utils.o")
    );
    assert_eq!(
      layout.user_module_stamp_path(nested),
      PathBuf::from("/tmp/build/user/.stamps/src/math/utils.stamp")
    );
  }

  #[test]
  fn test_bin_paths() {
    let layout = BuildLayout::new("myapp", Path::new("/tmp/build"));

    assert_eq!(layout.bin_dir(), PathBuf::from("/tmp/build/bin"));
    assert_eq!(layout.bin_path(), PathBuf::from("/tmp/build/bin/myapp"));
  }

  #[test]
  fn test_user_module_paths_with_absolute_source() {
    // When project_root is set, absolute source paths are relativized
    let layout = BuildLayout::with_project_root(
      "allocator",
      Path::new("/home/user/allocator/build"),
      Path::new("/home/user/allocator"),
    );

    // Absolute path that starts with project_root
    let abs_main = Path::new("/home/user/allocator/src/main.ign");
    assert_eq!(
      layout.user_module_header(abs_main),
      PathBuf::from("/home/user/allocator/build/user/include/src/main.h")
    );
    assert_eq!(
      layout.user_module_src(abs_main),
      PathBuf::from("/home/user/allocator/build/user/src/src/main.c")
    );
    assert_eq!(
      layout.user_module_obj(abs_main),
      PathBuf::from("/home/user/allocator/build/user/obj/src/main.o")
    );
    assert_eq!(
      layout.user_module_stamp_path(abs_main),
      PathBuf::from("/home/user/allocator/build/user/.stamps/src/main.stamp")
    );

    // Nested module
    let abs_block = Path::new("/home/user/allocator/src/block.ign");
    assert_eq!(
      layout.user_module_header(abs_block),
      PathBuf::from("/home/user/allocator/build/user/include/src/block.h")
    );
  }

  #[test]
  fn test_user_module_paths_without_project_root() {
    // Without project_root, absolute paths are used as-is (bug case we're preventing)
    let layout = BuildLayout::new("allocator", Path::new("/home/user/allocator/build"));

    // With no project_root, an absolute source path would go to wrong location
    // This test documents the behavior (though it's not ideal)
    let rel_main = Path::new("src/main.ign");
    assert_eq!(
      layout.user_module_header(rel_main),
      PathBuf::from("/home/user/allocator/build/user/include/src/main.h")
    );
  }

  #[test]
  fn test_relativize_with_non_matching_path() {
    // If source path doesn't start with project_root, it's returned as-is
    let layout = BuildLayout::with_project_root("myapp", Path::new("/tmp/build"), Path::new("/home/user/myproject"));

    // Path outside project root - returned as-is (will likely cause issues, but that's a usage error)
    let outside = Path::new("/other/path/file.ign");
    // join with absolute path replaces base, so this is the "broken" behavior
    // but relativize returns it as-is since it doesn't match project_root
    assert_eq!(layout.relativize(outside), PathBuf::from("/other/path/file.ign"));
  }

  // -- Stamp v2 tests --

  #[test]
  fn test_std_stamp_roundtrip() {
    let fp = BuildFingerprint::new("0.1.0", 1);
    let stamp = StdStamp {
      fingerprint: fp,
      files: vec![
        FileEntry {
          path: PathBuf::from("a.ign"),
          hash: "abc123".to_string(),
        },
        FileEntry {
          path: PathBuf::from("b.ign"),
          hash: "def456".to_string(),
        },
      ],
    };

    let serialized = stamp.serialize();
    let deserialized = StdStamp::deserialize(&serialized).unwrap();

    assert_eq!(stamp, deserialized);
  }

  #[test]
  fn test_std_stamp_stability() {
    let fp = BuildFingerprint::new("0.1.0", 1);
    let stamp = StdStamp {
      fingerprint: fp,
      files: vec![
        FileEntry {
          path: PathBuf::from("z.ign"),
          hash: "zzz".to_string(),
        },
        FileEntry {
          path: PathBuf::from("a.ign"),
          hash: "aaa".to_string(),
        },
      ],
    };

    let s1 = stamp.serialize();
    let s2 = stamp.serialize();
    assert_eq!(s1, s2);
  }

  #[test]
  fn test_module_stamp_roundtrip() {
    let fp = BuildFingerprint::new("0.1.0", 1);
    let stamp = ModuleStamp::new(
      fp,
      PathBuf::from("src/main.ign"),
      "self_hash_123".to_string(),
      vec![
        FileEntry {
          path: PathBuf::from("src/utils.ign"),
          hash: "dep1".to_string(),
        },
        FileEntry {
          path: PathBuf::from("src/math.ign"),
          hash: "dep2".to_string(),
        },
      ],
    );

    let serialized = stamp.serialize();
    let deserialized = ModuleStamp::deserialize(&serialized).unwrap();

    assert_eq!(stamp, deserialized);
  }

  #[test]
  fn test_module_stamp_validity_self_changed() {
    let fp = BuildFingerprint::new("0.1.0", 1);
    let stamp = ModuleStamp::new(fp.clone(), PathBuf::from("main.ign"), "old_hash".to_string(), vec![]);

    let temp_dir = std::env::temp_dir().join("ignis_stamp_v2_self_test");
    let _ = std::fs::remove_dir_all(&temp_dir);
    std::fs::create_dir_all(&temp_dir).unwrap();

    let stamp_path = temp_dir.join("main.stamp");
    std::fs::write(&stamp_path, stamp.serialize()).unwrap();

    // Same hash -> valid
    assert!(is_module_stamp_valid(&stamp_path, &fp, "old_hash", &[]));

    // Different hash -> invalid
    assert!(!is_module_stamp_valid(&stamp_path, &fp, "new_hash", &[]));

    std::fs::remove_dir_all(&temp_dir).unwrap();
  }

  #[test]
  fn test_module_stamp_validity_dep_changed() {
    let fp = BuildFingerprint::new("0.1.0", 1);
    let deps = vec![FileEntry {
      path: PathBuf::from("dep.ign"),
      hash: "dep_old".to_string(),
    }];
    let stamp = ModuleStamp::new(fp.clone(), PathBuf::from("main.ign"), "self".to_string(), deps.clone());

    let temp_dir = std::env::temp_dir().join("ignis_stamp_v2_dep_test");
    let _ = std::fs::remove_dir_all(&temp_dir);
    std::fs::create_dir_all(&temp_dir).unwrap();

    let stamp_path = temp_dir.join("main.stamp");
    std::fs::write(&stamp_path, stamp.serialize()).unwrap();

    // Same deps -> valid
    assert!(is_module_stamp_valid(&stamp_path, &fp, "self", &deps));

    // Changed dep hash -> invalid
    let new_deps = vec![FileEntry {
      path: PathBuf::from("dep.ign"),
      hash: "dep_new".to_string(),
    }];
    assert!(!is_module_stamp_valid(&stamp_path, &fp, "self", &new_deps));

    std::fs::remove_dir_all(&temp_dir).unwrap();
  }

  #[test]
  fn test_module_stamp_validity_fingerprint_changed() {
    let fp1 = BuildFingerprint::new("0.1.0", 1);
    let fp2 = BuildFingerprint::new("0.2.0", 1);
    let stamp = ModuleStamp::new(fp1, PathBuf::from("main.ign"), "hash".to_string(), vec![]);

    let temp_dir = std::env::temp_dir().join("ignis_stamp_v2_fp_test");
    let _ = std::fs::remove_dir_all(&temp_dir);
    std::fs::create_dir_all(&temp_dir).unwrap();

    let stamp_path = temp_dir.join("main.stamp");
    std::fs::write(&stamp_path, stamp.serialize()).unwrap();

    // Different compiler version -> invalid
    assert!(!is_module_stamp_valid(&stamp_path, &fp2, "hash", &[]));

    std::fs::remove_dir_all(&temp_dir).unwrap();
  }

  #[test]
  fn test_write_if_changed_same_content() {
    let temp_dir = std::env::temp_dir().join("ignis_write_if_changed_test");
    let _ = std::fs::remove_dir_all(&temp_dir);
    std::fs::create_dir_all(&temp_dir).unwrap();

    let path = temp_dir.join("test.txt");
    let content = "hello world";

    // First write -> true
    assert!(write_if_changed(&path, content).unwrap());

    // Get mtime
    let mtime1 = std::fs::metadata(&path).unwrap().modified().unwrap();

    // Small delay to ensure mtime would change if written
    std::thread::sleep(std::time::Duration::from_millis(10));

    // Same content -> false, file not written
    assert!(!write_if_changed(&path, content).unwrap());

    let mtime2 = std::fs::metadata(&path).unwrap().modified().unwrap();
    assert_eq!(mtime1, mtime2);

    // Different content -> true
    assert!(write_if_changed(&path, "different").unwrap());

    std::fs::remove_dir_all(&temp_dir).unwrap();
  }

  #[test]
  fn test_hash_bytes_deterministic() {
    let h1 = hash_bytes(b"hello");
    let h2 = hash_bytes(b"hello");
    assert_eq!(h1, h2);

    let h3 = hash_bytes(b"world");
    assert_ne!(h1, h3);
  }

  #[test]
  fn test_std_stamp_with_real_files() {
    let temp_dir = std::env::temp_dir().join("ignis_std_stamp_v2_test");
    let _ = std::fs::remove_dir_all(&temp_dir);
    std::fs::create_dir_all(&temp_dir).unwrap();

    let std_dir = temp_dir.join("std");
    std::fs::create_dir_all(&std_dir).unwrap();
    std::fs::write(std_dir.join("io.ign"), "// io module").unwrap();
    std::fs::write(std_dir.join("math.ign"), "// math module").unwrap();

    let fp = BuildFingerprint::new("0.1.0", 1);
    let stamp = StdStamp::compute(&std_dir, fp.clone()).unwrap();

    assert_eq!(stamp.files.len(), 2);

    let stamp_path = temp_dir.join(".stamp");
    write_std_stamp(&stamp_path, &stamp).unwrap();

    // Should be valid
    assert!(is_std_stamp_valid(&stamp_path, &std_dir, &fp));

    // Modify a file
    std::fs::write(std_dir.join("io.ign"), "// io module modified").unwrap();

    // Should be invalid
    assert!(!is_std_stamp_valid(&stamp_path, &std_dir, &fp));

    std::fs::remove_dir_all(&temp_dir).unwrap();
  }

  #[test]
  fn test_std_stamp_invalid_version() {
    let s = "version=1\ncompiler_version=0.1.0\n";
    assert!(StdStamp::deserialize(s).is_none());
  }

  #[test]
  fn test_module_stamp_invalid_version() {
    let s = "version=1\ncompiler_version=0.1.0\n";
    assert!(ModuleStamp::deserialize(s).is_none());
  }
}
