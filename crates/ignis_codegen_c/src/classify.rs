use std::collections::HashMap;

use ignis_type::definition::{Definition, DefinitionKind};
use ignis_type::module::{ModuleId, ModulePath};

/// Classification of a definition's origin.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DefKind {
  User,
  Std(String),
  Runtime, // extern functions from C headers
}

/// Target for code emission.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EmitTarget {
  User,
  UserModule(ModuleId),
  StdModule(String),
  All, // legacy
}

impl EmitTarget {
  pub fn should_emit_def(
    &self,
    kind: &DefKind,
  ) -> bool {
    match self {
      EmitTarget::User | EmitTarget::UserModule(_) => kind.is_user(),
      EmitTarget::StdModule(target_mod) => {
        matches!(kind, DefKind::Std(mod_name) if mod_name == target_mod)
      },
      EmitTarget::All => !kind.is_runtime(),
    }
  }

  pub fn target_user_module(&self) -> Option<ModuleId> {
    match self {
      EmitTarget::UserModule(id) => Some(*id),
      _ => None,
    }
  }

  /// Returns false for all modern targets; only All (legacy) emits externs for Runtime.
  pub fn should_emit_extern(
    &self,
    kind: &DefKind,
  ) -> bool {
    matches!(self, EmitTarget::All) && kind.is_runtime()
  }
}

impl DefKind {
  pub fn is_user(&self) -> bool {
    matches!(self, DefKind::User)
  }

  pub fn is_std(&self) -> bool {
    matches!(self, DefKind::Std(_))
  }

  pub fn is_runtime(&self) -> bool {
    matches!(self, DefKind::Runtime)
  }

  pub fn std_module(&self) -> Option<&str> {
    match self {
      DefKind::Std(name) => Some(name),
      _ => None,
    }
  }
}

/// Classify based on owner module and extern status.
/// extern -> Runtime, Std(_) -> Std, Project(_) -> User.
pub fn classify_def(
  def: &Definition,
  module_paths: &HashMap<ModuleId, ModulePath>,
) -> DefKind {
  classify_def_with_std_path(def, module_paths, None)
}

/// Classify based on owner module and extern status, with std_path awareness.
///
/// If `std_path` is provided, `ModulePath::Project` paths inside that directory
/// are classified as `DefKind::Std` with the module name extracted from the path.
pub fn classify_def_with_std_path(
  def: &Definition,
  module_paths: &HashMap<ModuleId, ModulePath>,
  std_path: Option<&std::path::Path>,
) -> DefKind {
  let is_extern = match &def.kind {
    DefinitionKind::Function(fd) => fd.is_extern,
    DefinitionKind::Namespace(nd) => nd.is_extern,
    _ => false,
  };

  if is_extern {
    return DefKind::Runtime;
  }

  match module_paths.get(&def.owner_module) {
    Some(ModulePath::Std(name)) => DefKind::Std(name.clone()),
    Some(ModulePath::Project(path)) => {
      if let Some(std_dir) = std_path
        && path.starts_with(std_dir)
      {
        let module_path = module_paths.get(&def.owner_module).unwrap();
        return DefKind::Std(module_path.module_name());
      }
      DefKind::User
    },
    None => DefKind::User,
  }
}

pub fn build_module_paths<'a, I>(modules: I) -> HashMap<ModuleId, ModulePath>
where
  I: IntoIterator<Item = (ModuleId, &'a ModulePath)>,
{
  modules.into_iter().map(|(id, path)| (id, path.clone())).collect()
}

#[cfg(test)]
mod tests {
  use super::*;
  use ignis_type::definition::{FunctionDefinition, InlineMode, Visibility};
  use ignis_type::span::Span;
  use ignis_type::symbol::SymbolId;
  use ignis_type::types::TypeId;
  use std::path::PathBuf;

  fn make_function_def(
    owner_module: ModuleId,
    is_extern: bool,
  ) -> Definition {
    Definition {
      kind: DefinitionKind::Function(FunctionDefinition {
        type_params: vec![],
        params: vec![],
        return_type: TypeId::new(0),
        is_extern,
        is_variadic: false,
        inline_mode: InlineMode::None,
        attrs: vec![],
      }),
      name: SymbolId::new(0),
      span: Span::default(),
      name_span: Span::default(),
      visibility: Visibility::Public,
      owner_module,
      owner_namespace: None,
      doc: None,
    }
  }

  #[test]
  fn test_classify_extern_as_runtime() {
    let module_id = ModuleId::new(0);
    let def = make_function_def(module_id, true);

    let mut paths = HashMap::new();
    paths.insert(module_id, ModulePath::Project(PathBuf::from("/project/main.ign")));

    let kind = classify_def(&def, &paths);
    assert_eq!(kind, DefKind::Runtime);
    assert!(kind.is_runtime());
  }

  #[test]
  fn test_classify_std_module() {
    let module_id = ModuleId::new(0);
    let def = make_function_def(module_id, false);

    let mut paths = HashMap::new();
    paths.insert(module_id, ModulePath::Std("io".to_string()));

    let kind = classify_def(&def, &paths);
    assert_eq!(kind, DefKind::Std("io".to_string()));
    assert!(kind.is_std());
    assert_eq!(kind.std_module(), Some("io"));
  }

  #[test]
  fn test_classify_project_as_user() {
    let module_id = ModuleId::new(0);
    let def = make_function_def(module_id, false);

    let mut paths = HashMap::new();
    paths.insert(module_id, ModulePath::Project(PathBuf::from("/project/main.ign")));

    let kind = classify_def(&def, &paths);
    assert_eq!(kind, DefKind::User);
    assert!(kind.is_user());
  }

  #[test]
  fn test_classify_unknown_module_defaults_to_user() {
    let module_id = ModuleId::new(999);
    let def = make_function_def(module_id, false);

    let paths = HashMap::new(); // empty - module not found

    let kind = classify_def(&def, &paths);
    assert_eq!(kind, DefKind::User);
  }

  #[test]
  fn test_extern_takes_precedence_over_module_path() {
    // Even if module is std, extern functions are runtime
    let module_id = ModuleId::new(0);
    let def = make_function_def(module_id, true);

    let mut paths = HashMap::new();
    paths.insert(module_id, ModulePath::Std("io".to_string()));

    let kind = classify_def(&def, &paths);
    assert_eq!(kind, DefKind::Runtime);
  }

  #[test]
  fn test_build_module_paths() {
    let id1 = ModuleId::new(0);
    let id2 = ModuleId::new(1);
    let path1 = ModulePath::Std("io".to_string());
    let path2 = ModulePath::Project(PathBuf::from("/project/main.ign"));

    let paths = build_module_paths([(id1, &path1), (id2, &path2)]);

    assert_eq!(paths.get(&id1), Some(&path1));
    assert_eq!(paths.get(&id2), Some(&path2));
  }

  // EmitTarget tests

  #[test]
  fn test_emit_target_user_emits_only_user() {
    let target = EmitTarget::User;

    assert!(target.should_emit_def(&DefKind::User));
    assert!(!target.should_emit_def(&DefKind::Std("io".to_string())));
    assert!(!target.should_emit_def(&DefKind::Runtime));
  }

  #[test]
  fn test_emit_target_std_module_emits_only_matching_module() {
    let target = EmitTarget::StdModule("io".to_string());

    assert!(!target.should_emit_def(&DefKind::User));
    assert!(target.should_emit_def(&DefKind::Std("io".to_string())));
    assert!(!target.should_emit_def(&DefKind::Std("string".to_string())));
    assert!(!target.should_emit_def(&DefKind::Runtime));
  }

  #[test]
  fn test_emit_target_all_emits_user_and_std() {
    let target = EmitTarget::All;

    assert!(target.should_emit_def(&DefKind::User));
    assert!(target.should_emit_def(&DefKind::Std("io".to_string())));
    assert!(!target.should_emit_def(&DefKind::Runtime)); // Runtime comes from headers
  }

  #[test]
  fn test_emit_target_user_never_emits_extern() {
    let target = EmitTarget::User;

    assert!(!target.should_emit_extern(&DefKind::User));
    assert!(!target.should_emit_extern(&DefKind::Std("io".to_string())));
    assert!(!target.should_emit_extern(&DefKind::Runtime));
  }

  #[test]
  fn test_emit_target_std_module_never_emits_extern() {
    let target = EmitTarget::StdModule("io".to_string());

    assert!(!target.should_emit_extern(&DefKind::User));
    assert!(!target.should_emit_extern(&DefKind::Std("io".to_string())));
    assert!(!target.should_emit_extern(&DefKind::Runtime));
  }

  #[test]
  fn test_emit_target_all_emits_extern_for_runtime_only() {
    let target = EmitTarget::All;

    assert!(!target.should_emit_extern(&DefKind::User));
    assert!(!target.should_emit_extern(&DefKind::Std("io".to_string())));
    assert!(target.should_emit_extern(&DefKind::Runtime)); // Legacy behavior
  }

  #[test]
  fn test_emit_target_user_module_emits_user_defs() {
    let module_id = ModuleId::new(42);
    let target = EmitTarget::UserModule(module_id);

    // UserModule target emits user definitions (filtering by module is done separately)
    assert!(target.should_emit_def(&DefKind::User));
    assert!(!target.should_emit_def(&DefKind::Std("io".to_string())));
    assert!(!target.should_emit_def(&DefKind::Runtime));
  }

  #[test]
  fn test_emit_target_user_module_returns_module_id() {
    let module_id = ModuleId::new(42);
    let target = EmitTarget::UserModule(module_id);

    assert_eq!(target.target_user_module(), Some(module_id));
    assert_eq!(EmitTarget::User.target_user_module(), None);
    assert_eq!(EmitTarget::All.target_user_module(), None);
  }

  #[test]
  fn test_emit_target_user_module_never_emits_extern() {
    let module_id = ModuleId::new(42);
    let target = EmitTarget::UserModule(module_id);

    assert!(!target.should_emit_extern(&DefKind::User));
    assert!(!target.should_emit_extern(&DefKind::Std("io".to_string())));
    assert!(!target.should_emit_extern(&DefKind::Runtime));
  }
}
