use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;
use std::fmt::Write;

use ignis_config::CHeader;
use ignis_hir::operation::{BinaryOperation, UnaryOperation};
use ignis_lir::{Block, ConstValue, FunctionLir, Instr, LirProgram, Operand, TempId, Terminator, instr::BuiltinEqKind};
use ignis_type::{
  attribute::{FieldAttr, FunctionAttr, RecordAttr},
  definition::{
    DefinitionId, DefinitionKind, DefinitionStore, EnumDefinition, FunctionDefinition, InlineMode, RecordDefinition,
    Visibility,
  },
  file::{FileId, SourceMap},
  module::{ModuleId, ModulePath},
  namespace::NamespaceStore,
  symbol::SymbolTable,
  types::{Type, TypeId, TypeStore},
};

use crate::classify::{DefKind, EmitTarget};
use crate::EmitInput;

const USER_MAIN_SYMBOL: &str = "__ignis_user_main";
const GENERATED_C_LINE_FILE: &str = "<generated-c>";

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TestHarnessEntry {
  pub def_id: DefinitionId,
  pub fq_name: String,
}

#[derive(Clone, Copy)]
enum EntryMainReturn {
  I32,
  Void,
  TryI32 {
    ok_tag: u32,
    err_tag: u32,
    error_display: ErrorDisplay,
  },
}

#[derive(Clone, Copy)]
enum ErrorDisplay {
  Str,
  RecordStrField { field_index: u32 },
  Opaque,
}

#[derive(Clone, Copy)]
enum EntryMainArgs {
  None,
  ArgcArgv,
}

/// C code emitter for LIR programs.
pub struct CEmitter<'a> {
  program: &'a LirProgram,
  types: &'a TypeStore,
  defs: &'a DefinitionStore,
  namespaces: &'a NamespaceStore,
  symbols: &'a SymbolTable,
  headers: &'a [CHeader],
  output: String,
  source_map: Option<&'a SourceMap>,
  current_fn_id: Option<DefinitionId>,
  /// Emit target for filtering definitions (None = emit all, legacy mode)
  target: Option<EmitTarget>,
  /// Module paths for classifying definitions (required when target is Some)
  module_paths: Option<&'a HashMap<ModuleId, ModulePath>>,
  /// Std path for classifying std internal files as Std instead of User
  std_path: Option<&'a std::path::Path>,
  /// Extra function definitions that must be emitted in user module mode
  /// (typically monomorphized generic callees owned by other modules).
  forced_emit_defs: HashSet<DefinitionId>,
  /// Symbols already provided by the precompiled std archive. Used to emit only
  /// missing std generic specializations into user objects.
  std_defined_symbols: Option<&'a HashSet<String>>,

  /// Maps `Type::Function` TypeId → emitted closure struct name.
  /// Populated during `emit_closure_types()`, used by `format_type()`.
  closure_struct_names: HashMap<TypeId, String>,

  /// Maps thunk DefinitionId → env struct name. Populated during `emit_closure_types()`.
  closure_env_names: HashMap<DefinitionId, String>,

  /// Optional native test harness dispatch table.
  test_harness: Option<&'a [TestHarnessEntry]>,
}

impl<'a> CEmitter<'a> {
  pub fn new(
    program: &'a LirProgram,
    types: &'a TypeStore,
    defs: &'a DefinitionStore,
    namespaces: &'a NamespaceStore,
    symbols: &'a SymbolTable,
    headers: &'a [CHeader],
  ) -> Self {
    Self {
      program,
      types,
      defs,
      namespaces,
      symbols,
      headers,
      output: String::new(),
      source_map: None,
      current_fn_id: None,
      target: None,
      module_paths: None,
      std_path: None,
      forced_emit_defs: HashSet::new(),
      std_defined_symbols: None,
      closure_struct_names: HashMap::new(),
      closure_env_names: HashMap::new(),
      test_harness: None,
    }
  }

  /// Create an emitter with a specific emit target for filtering.
  #[allow(clippy::too_many_arguments)]
  pub fn with_target(
    program: &'a LirProgram,
    types: &'a TypeStore,
    defs: &'a DefinitionStore,
    namespaces: &'a NamespaceStore,
    symbols: &'a SymbolTable,
    headers: &'a [CHeader],
    target: EmitTarget,
    module_paths: &'a HashMap<ModuleId, ModulePath>,
  ) -> Self {
    Self {
      program,
      types,
      defs,
      namespaces,
      symbols,
      headers,
      output: String::new(),
      source_map: None,
      current_fn_id: None,
      target: Some(target),
      module_paths: Some(module_paths),
      std_path: None,
      forced_emit_defs: HashSet::new(),
      std_defined_symbols: None,
      closure_struct_names: HashMap::new(),
      closure_env_names: HashMap::new(),
      test_harness: None,
    }
  }

  /// Create an emitter for a specific user module with std_path awareness.
  #[allow(clippy::too_many_arguments)]
  pub fn with_user_module_target(
    program: &'a LirProgram,
    types: &'a TypeStore,
    defs: &'a DefinitionStore,
    namespaces: &'a NamespaceStore,
    symbols: &'a SymbolTable,
    headers: &'a [CHeader],
    module_id: ModuleId,
    module_paths: &'a HashMap<ModuleId, ModulePath>,
    std_path: &'a std::path::Path,
    std_defined_symbols: Option<&'a HashSet<String>>,
  ) -> Self {
    Self {
      program,
      types,
      defs,
      namespaces,
      symbols,
      headers,
      output: String::new(),
      source_map: None,
      current_fn_id: None,
      target: Some(EmitTarget::UserModule(module_id)),
      module_paths: Some(module_paths),
      std_path: Some(std_path),
      forced_emit_defs: HashSet::new(),
      std_defined_symbols,
      closure_struct_names: HashMap::new(),
      closure_env_names: HashMap::new(),
      test_harness: None,
    }
  }

  /// Create an emitter for std module emission with std_path awareness.
  #[allow(clippy::too_many_arguments)]
  pub fn with_std_target(
    program: &'a LirProgram,
    types: &'a TypeStore,
    defs: &'a DefinitionStore,
    namespaces: &'a NamespaceStore,
    symbols: &'a SymbolTable,
    headers: &'a [CHeader],
    target: EmitTarget,
    module_paths: &'a HashMap<ModuleId, ModulePath>,
    std_path: &'a std::path::Path,
  ) -> Self {
    Self {
      program,
      types,
      defs,
      namespaces,
      symbols,
      headers,
      output: String::new(),
      source_map: None,
      current_fn_id: None,
      target: Some(target),
      module_paths: Some(module_paths),
      std_path: Some(std_path),
      forced_emit_defs: HashSet::new(),
      std_defined_symbols: None,
      closure_struct_names: HashMap::new(),
      closure_env_names: HashMap::new(),
      test_harness: None,
    }
  }

  #[allow(clippy::too_many_arguments)]
  pub fn with_test_harness_target(
    program: &'a LirProgram,
    types: &'a TypeStore,
    defs: &'a DefinitionStore,
    namespaces: &'a NamespaceStore,
    symbols: &'a SymbolTable,
    headers: &'a [CHeader],
    _module_paths: &'a HashMap<ModuleId, ModulePath>,
    test_harness: &'a [TestHarnessEntry],
  ) -> Self {
    Self {
      program,
      types,
      defs,
      namespaces,
      symbols,
      headers,
      output: String::new(),
      source_map: None,
      current_fn_id: None,
      target: None,
      module_paths: None,
      std_path: None,
      forced_emit_defs: HashSet::new(),
      std_defined_symbols: None,
      closure_struct_names: HashMap::new(),
      closure_env_names: HashMap::new(),
      test_harness: Some(test_harness),
    }
  }

  pub fn with_source_map(
    mut self,
    source_map: Option<&'a SourceMap>,
  ) -> Self {
    self.source_map = source_map;
    self
  }

  pub fn emit(mut self) -> String {
    if let Some(module_id) = self.target.as_ref().and_then(|target| target.target_user_module()) {
      self.forced_emit_defs = self.collect_forced_emit_defs(module_id);
    }

    self.emit_implicit_headers();
    self.emit_headers();

    let uses_module_headers = matches!(self.target, Some(EmitTarget::UserModule(_)) | Some(EmitTarget::StdModule(_)));

    if !uses_module_headers {
      self.emit_slice_types();
      self.emit_type_forward_declarations();
      self.emit_type_definitions();
    } else if let Some(module_id) = self.target.as_ref().and_then(|target| target.target_user_module()) {
      self.emit_forced_external_type_definitions(module_id);
    }

    self.emit_closure_types();
    self.emit_static_constants();
    self.emit_extern_declarations();
    self.emit_drop_glue_helpers();
    self.emit_forward_declarations();
    self.emit_functions();
    self.emit_entry_wrapper();
    self.output
  }

  pub fn emit_test_harness_only(
    mut self,
    test_harness: &'a [TestHarnessEntry],
  ) -> String {
    self.test_harness = Some(test_harness);

    self.emit_implicit_headers();
    self.emit_headers();

    for test in test_harness {
      writeln!(self.output, "extern void {}(void);", self.def_name(test.def_id)).unwrap();
    }

    if !test_harness.is_empty() {
      writeln!(self.output).unwrap();
    }

    self.emit_test_harness_wrapper(test_harness);
    self.output
  }

  /// Emit C headers implied by instructions in the program (e.g. stdio for panic, math for pow).
  fn emit_implicit_headers(&mut self) {
    let mut needs_stdio = false;
    let mut needs_math = false;
    let mut needs_string = false;

    for func in self.program.functions.values() {
      for (_, block) in func.blocks.iter() {
        for instr in &block.instructions {
          match instr {
            Instr::PanicMessage { .. } => needs_stdio = true,
            Instr::BinOp {
              op: BinaryOperation::Pow,
              ..
            } => needs_math = true,
            Instr::BitCast { .. } => needs_string = true,
            Instr::BuiltinEq { ty, .. } if matches!(self.types.get(ty), Type::Str) => needs_string = true,
            _ => {},
          }
        }
      }
    }

    if matches!(self.entry_main_return_kind(), Some(EntryMainReturn::TryI32 { .. })) {
      needs_stdio = true;
    }

    if self.test_harness.is_some() {
      needs_stdio = true;
      needs_string = true;
    }

    if needs_stdio {
      writeln!(self.output, "#include <stdio.h>").unwrap();
      writeln!(self.output, "#include <stdlib.h>").unwrap();
    }

    if needs_math {
      writeln!(self.output, "#include <math.h>").unwrap();
    }

    if needs_string {
      writeln!(self.output, "#include <string.h>").unwrap();
    }
  }

  /// Classify a definition based on its owner module.
  fn classify(
    &self,
    def_id: DefinitionId,
  ) -> DefKind {
    use crate::classify::classify_def_with_std_path;

    let def = self.defs.get(&def_id);
    if let Some(module_paths) = self.module_paths {
      classify_def_with_std_path(def, module_paths, self.std_path)
    } else {
      // Legacy mode: no classification, treat everything as "should emit"
      // For extern functions, return Runtime; otherwise User
      let is_extern = match &def.kind {
        DefinitionKind::Function(fd) => fd.is_extern,
        DefinitionKind::Namespace(nd) => nd.is_extern,
        _ => false,
      };
      if is_extern { DefKind::Runtime } else { DefKind::User }
    }
  }

  /// Check if a definition should be emitted based on the current target.
  fn should_emit(
    &self,
    def_id: DefinitionId,
  ) -> bool {
    match &self.target {
      Some(target) => {
        let kind = self.classify(def_id);

        if let Some(target_module_id) = target.target_user_module() {
          if self.forced_emit_defs.contains(&def_id) {
            return true;
          }

          let def = self.defs.get(&def_id);

          if def.owner_module == target_module_id {
            if should_defer_owner_module_emit_to_entry(
              self.is_entry_user_module(target_module_id),
              self.is_monomorphized_generic_def(def_id),
              self.definition_depends_on_user_type(def_id),
            ) {
              return false;
            }

            return true;
          }

          return false;
        }

        if matches!(target, EmitTarget::StdModule(_))
          && should_defer_std_module_emit_for_user_specialization(
            self.is_monomorphized_generic_def(def_id),
            self.definition_depends_on_user_type(def_id),
          )
        {
          return false;
        }

        target.should_emit_def(&kind)
      },
      None => true, // Legacy mode: emit everything
    }
  }

  fn collect_forced_emit_defs(
    &self,
    target_module_id: ModuleId,
  ) -> HashSet<DefinitionId> {
    let mut reachable: HashSet<DefinitionId> = HashSet::new();
    let mut queue: VecDeque<DefinitionId> = VecDeque::new();

    for (def_id, function) in &self.program.functions {
      if function.is_extern {
        continue;
      }

      if self.defs.get(def_id).owner_module == target_module_id {
        reachable.insert(*def_id);
        queue.push_back(*def_id);
      }
    }

    while let Some(current) = queue.pop_front() {
      let Some(function) = self.program.functions.get(&current) else {
        continue;
      };

      for (_, block) in function.blocks.iter() {
        for instruction in &block.instructions {
          for dependency in self.forced_emit_instruction_dependencies(function, instruction) {
            if reachable.insert(dependency) {
              queue.push_back(dependency);
            }
          }
        }
      }
    }

    let external_reachable: HashSet<_> = reachable
      .into_iter()
      .filter(|def_id| self.is_force_emit_candidate(*def_id, target_module_id))
      .collect();

    let mut forced = HashSet::new();
    let mut queue: VecDeque<DefinitionId> = external_reachable
      .iter()
      .copied()
      .filter(|def_id| self.is_force_emit_seed(*def_id))
      .collect();

    while let Some(current) = queue.pop_front() {
      if !forced.insert(current) {
        continue;
      }

      let Some(function) = self.program.functions.get(&current) else {
        continue;
      };

      for (_, block) in function.blocks.iter() {
        for instruction in &block.instructions {
          for dependency in self.forced_emit_instruction_dependencies(function, instruction) {
            if (external_reachable.contains(&dependency)
              || self.is_missing_std_archive_generic_specialization(dependency))
              && self.should_recurse_forced_emit(dependency)
            {
              queue.push_back(dependency);
            }
          }
        }
      }
    }

    // Every entry in `forced` is a cross-module std definition reachable from this
    // module and is emitted with internal (`static`) linkage when it is missing from
    // the std archive (see the linkage handling in `emit_function`). A specialization
    // that std never instantiated -- e.g. `Vector<u8>::toSlice` over a primitive
    // element type -- is absent from the archive, so it must be emitted locally in
    // every user module that reaches it, regardless of whether its signature mentions
    // a user type. Dropping such defs here previously produced linker errors like
    // `undefined reference to 'Vector____u8____toSlice'` from non-entry user modules.

    forced
  }

  fn forced_emit_instruction_dependencies(
    &self,
    function: &FunctionLir,
    instruction: &Instr,
  ) -> Vec<DefinitionId> {
    let mut dependencies = Vec::new();

    match instruction {
      Instr::Call { callee, .. } => dependencies.push(*callee),
      Instr::Drop { local } => {
        self.push_drop_method_dependency(&mut dependencies, function.locals.get(local).ty);
      },
      Instr::DropInPlace { ty, .. } => {
        self.push_drop_method_dependency(&mut dependencies, *ty);
      },
      Instr::BuiltinEq {
        kind: BuiltinEqKind::Method(method_def_id),
        ..
      } => {
        dependencies.push(*method_def_id);
      },
      Instr::BuiltinHash { ty, .. } => {
        self.push_hash_method_dependency(&mut dependencies, *ty);
      },
      _ => {},
    }

    dependencies
  }

  fn push_drop_method_dependency(
    &self,
    dependencies: &mut Vec<DefinitionId>,
    ty: TypeId,
  ) {
    let mut visited_types = HashSet::new();
    self.push_drop_dependencies(dependencies, ty, &mut visited_types);
  }

  fn push_drop_dependencies(
    &self,
    dependencies: &mut Vec<DefinitionId>,
    ty: TypeId,
    visited_types: &mut HashSet<TypeId>,
  ) {
    if !visited_types.insert(ty) {
      return;
    }

    match self.types.get(&ty) {
      Type::Record(def_id) | Type::Enum(def_id) => {
        if let Some(drop_method_def_id) = self.find_drop_method(*def_id) {
          dependencies.push(drop_method_def_id);
          return;
        }

        self.push_structural_drop_dependencies(dependencies, *def_id, visited_types);
      },
      _ => {},
    }
  }

  fn push_structural_drop_dependencies(
    &self,
    dependencies: &mut Vec<DefinitionId>,
    def_id: DefinitionId,
    visited_types: &mut HashSet<TypeId>,
  ) {
    match &self.defs.get(&def_id).kind {
      DefinitionKind::Record(record_definition) => {
        for field in &record_definition.fields {
          if self.types.needs_drop_with_defs(&field.type_id, self.defs) {
            self.push_drop_dependencies(dependencies, field.type_id, visited_types);
          }
        }
      },
      DefinitionKind::Enum(enum_definition) => {
        for variant in &enum_definition.variants {
          for payload_type in &variant.payload {
            if self.types.needs_drop_with_defs(payload_type, self.defs) {
              self.push_drop_dependencies(dependencies, *payload_type, visited_types);
            }
          }
        }
      },
      _ => {},
    }
  }

  fn push_hash_method_dependency(
    &self,
    dependencies: &mut Vec<DefinitionId>,
    ty: TypeId,
  ) {
    let owner_def_id = match self.types.get(&ty) {
      Type::Record(def_id) | Type::Enum(def_id) => Some(*def_id),
      _ => None,
    };

    if let Some(owner_def_id) = owner_def_id
      && let Some(hash_method_def_id) = self.find_named_instance_method(owner_def_id, "hash")
    {
      dependencies.push(hash_method_def_id);
    }
  }

  fn is_force_emit_candidate(
    &self,
    def_id: DefinitionId,
    target_module_id: ModuleId,
  ) -> bool {
    if self.defs.get(&def_id).owner_module == target_module_id {
      return false;
    }

    let Some(function) = self.program.functions.get(&def_id) else {
      return false;
    };

    if function.is_extern || !self.is_function_signature_monomorphized(function) {
      return false;
    }

    let is_public = self.is_effectively_public(def_id);

    if is_public && matches!(function.inline_mode, InlineMode::Inline | InlineMode::Always) {
      return false;
    }

    !matches!(self.classify(def_id), DefKind::Runtime)
  }

  fn is_effectively_public(
    &self,
    def_id: DefinitionId,
  ) -> bool {
    let def = self.defs.get(&def_id);

    if matches!(&def.kind, DefinitionKind::Function(function) if function_is_extension(function)) {
      return true;
    }

    if def.visibility == Visibility::Public {
      return true;
    }

    if self.is_monomorphized_generic_def(def_id)
      && (self.is_std_test_def(def_id)
        || self.definition_depends_on_user_type(def_id)
        || self.definition_mentions_std_test_user_type(def_id))
    {
      return false;
    }

    let mut current_namespace = def.owner_namespace;

    while let Some(namespace_id) = current_namespace {
      let Some(namespace_def_id) = self.namespace_definition_id(namespace_id) else {
        return false;
      };

      let namespace_def = self.defs.get(&namespace_def_id);
      if namespace_def.visibility == Visibility::Public {
        return true;
      }

      // Walk the namespace tree parent, not the definition's owner_namespace.
      // The two differ when a nested namespace is declared as a top-level item
      // (e.g. `namespace LibC::Memory {}`), where the definition has
      // owner_namespace = None but the tree parent is `LibC`. Following the
      // tree parent matches what is_effectively_public_for_header does, so the
      // C source and the emitted header agree on linkage.
      current_namespace = self.namespaces.get(&namespace_id).parent;
    }

    false
  }

  fn namespace_definition_id(
    &self,
    namespace_id: ignis_type::namespace::NamespaceId,
  ) -> Option<DefinitionId> {
    self.defs.iter().find_map(|(def_id, def)| match &def.kind {
      DefinitionKind::Namespace(namespace_def) if namespace_def.namespace_id == namespace_id => Some(def_id),
      _ => None,
    })
  }

  fn is_force_emit_seed(
    &self,
    def_id: DefinitionId,
  ) -> bool {
    let kind = self.classify(def_id);

    (kind.is_std() && self.is_std_test_option_presence_method(def_id))
      || (kind.is_std() && self.is_missing_std_archive_generic_specialization(def_id))
      || (kind.is_std()
        && self.is_monomorphized_generic_def(def_id)
        && (self.definition_depends_on_user_type(def_id)
          || self.definition_mentions_std_test_user_type(def_id)
          || self.is_std_test_def(def_id)))
  }

  fn is_missing_std_archive_generic_specialization(
    &self,
    def_id: DefinitionId,
  ) -> bool {
    if !self.is_monomorphized_generic_def(def_id) {
      return false;
    }

    self
      .std_defined_symbols
      .is_some_and(|symbols| !symbols.contains(&self.def_name(def_id)))
  }

  fn should_recurse_forced_emit(
    &self,
    def_id: DefinitionId,
  ) -> bool {
    if self.is_force_emit_seed(def_id) {
      return true;
    }

    let raw_name = self.symbols.get(&self.defs.get(&def_id).name);
    let kind = self.classify(def_id);

    kind.is_std() && raw_name.starts_with('_') && self.is_force_emit_candidate_private_std_helper(def_id)
  }

  fn is_force_emit_candidate_private_std_helper(
    &self,
    def_id: DefinitionId,
  ) -> bool {
    let Some(function) = self.program.functions.get(&def_id) else {
      return false;
    };

    self.is_function_signature_monomorphized(function)
  }

  fn is_entry_user_module(
    &self,
    target_module_id: ModuleId,
  ) -> bool {
    if let Some(entry_id) = self.program.entry_point {
      return self.defs.get(&entry_id).owner_module == target_module_id;
    }

    self.is_primary_user_module_without_entry(target_module_id)
  }

  fn is_primary_user_module_without_entry(
    &self,
    target_module_id: ModuleId,
  ) -> bool {
    let Some(module_paths) = self.module_paths else {
      return false;
    };

    module_paths
      .iter()
      .filter(|(_, module_path)| match module_path {
        ModulePath::Project(path) => !self.std_path.is_some_and(|std_path| path.starts_with(std_path)),
        ModulePath::Std(_) => false,
      })
      .map(|(module_id, _)| *module_id)
      .min_by_key(|module_id| module_id.index())
      .is_some_and(|module_id| module_id == target_module_id)
  }

  fn is_monomorphized_generic_def(
    &self,
    def_id: DefinitionId,
  ) -> bool {
    let raw_name = self.symbols.get(&self.defs.get(&def_id).name);
    raw_name.contains("__") && !raw_name.starts_with("__")
  }

  fn is_std_test_def(
    &self,
    def_id: DefinitionId,
  ) -> bool {
    self
      .module_paths
      .and_then(|module_paths| module_paths.get(&self.defs.get(&def_id).owner_module))
      .is_some_and(|module_path| matches!(module_path, ModulePath::Std(name) if name == "test"))
  }

  fn is_std_test_option_presence_method(
    &self,
    def_id: DefinitionId,
  ) -> bool {
    let symbol_name = self.def_name(def_id);

    symbol_name.starts_with("Option____")
      && (symbol_name.ends_with("____isNone") || symbol_name.ends_with("____isSome"))
  }

  fn definition_depends_on_user_type(
    &self,
    def_id: DefinitionId,
  ) -> bool {
    let mut visited_types = HashSet::new();
    let def = self.defs.get(&def_id);

    match &def.kind {
      DefinitionKind::Function(function_definition) => {
        self.type_contains_user_definition(function_definition.return_type, &mut visited_types)
          || function_definition.params.iter().any(|param_id| {
            if let DefinitionKind::Parameter(parameter_definition) = &self.defs.get(param_id).kind {
              self.type_contains_user_definition(parameter_definition.type_id, &mut visited_types)
            } else {
              false
            }
          })
      },
      DefinitionKind::Method(method_definition) => {
        self.definition_tree_contains_user_definition(method_definition.owner_type, &mut visited_types)
          || self.type_contains_user_definition(method_definition.return_type, &mut visited_types)
          || method_definition.params.iter().any(|param_id| {
            if let DefinitionKind::Parameter(parameter_definition) = &self.defs.get(param_id).kind {
              self.type_contains_user_definition(parameter_definition.type_id, &mut visited_types)
            } else {
              false
            }
          })
      },
      _ => false,
    }
  }

  fn definition_mentions_std_test_user_type(
    &self,
    def_id: DefinitionId,
  ) -> bool {
    let raw_name = self.symbols.get(&self.defs.get(&def_id).name);

    self
      .defs
      .iter()
      .filter_map(|(candidate_id, candidate)| match &candidate.kind {
        DefinitionKind::Record(_) | DefinitionKind::Enum(_) => Some((candidate_id, candidate)),
        _ => None,
      })
      .filter(|(_, candidate)| self.is_std_test_local_user_type(candidate.owner_module))
      .map(|(_, candidate)| self.symbols.get(&candidate.name))
      .any(|candidate_name| raw_name.contains(candidate_name))
  }

  fn is_std_test_local_user_type(
    &self,
    module_id: ModuleId,
  ) -> bool {
    self.is_std_test_local_user_module(module_id)
  }

  fn is_std_test_local_user_module(
    &self,
    module_id: ModuleId,
  ) -> bool {
    self
      .module_paths
      .and_then(|module_paths| module_paths.get(&module_id))
      .is_some_and(|module_path| match module_path {
        ModulePath::Project(path) => self.std_path.is_some_and(|std_path| {
          path.starts_with(std_path) && path.file_name().is_some_and(|name| name == "tests.ign")
        }),
        ModulePath::Std(_) => false,
      })
  }

  fn definition_tree_contains_user_definition(
    &self,
    def_id: DefinitionId,
    visited_types: &mut HashSet<TypeId>,
  ) -> bool {
    if self.classify(def_id).is_user() {
      return true;
    }

    match &self.defs.get(&def_id).kind {
      DefinitionKind::Record(record_definition) => record_definition
        .fields
        .iter()
        .any(|field| self.type_contains_user_definition(field.type_id, visited_types)),
      DefinitionKind::Enum(enum_definition) => enum_definition.variants.iter().any(|variant| {
        variant
          .payload
          .iter()
          .any(|payload_type| self.type_contains_user_definition(*payload_type, visited_types))
      }),
      _ => false,
    }
  }

  fn type_contains_user_definition(
    &self,
    ty: TypeId,
    visited_types: &mut HashSet<TypeId>,
  ) -> bool {
    if !visited_types.insert(ty) {
      return false;
    }

    match self.types.get(&ty) {
      Type::Record(def_id) | Type::Enum(def_id) => {
        self.definition_tree_contains_user_definition(*def_id, visited_types)
      },
      Type::Pointer { inner, .. } | Type::Reference { inner, .. } => {
        self.type_contains_user_definition(*inner, visited_types)
      },
      Type::Slice { element, .. } => self.type_contains_user_definition(*element, visited_types),
      Type::FixedArray { element, .. } => self.type_contains_user_definition(*element, visited_types),
      Type::Tuple(elements) => elements
        .iter()
        .any(|element_type| self.type_contains_user_definition(*element_type, visited_types)),
      Type::Function { params, ret, .. } => {
        params
          .iter()
          .any(|param_type| self.type_contains_user_definition(*param_type, visited_types))
          || self.type_contains_user_definition(*ret, visited_types)
      },
      Type::Instance { generic, args } => {
        self.classify(*generic).is_user()
          || args
            .iter()
            .any(|arg_type| self.type_contains_user_definition(*arg_type, visited_types))
      },
      _ => false,
    }
  }

  fn emit_forced_external_type_definitions(
    &mut self,
    target_module_id: ModuleId,
  ) {
    if self.forced_emit_defs.is_empty() {
      return;
    }

    let module_type_names: std::collections::HashSet<String> = self
      .defs
      .iter()
      .filter_map(|(def_id, def)| match &def.kind {
        DefinitionKind::Record(record_definition)
          if def.owner_module == target_module_id && record_definition.type_params.is_empty() =>
        {
          Some(build_mangled_name_standalone(
            def_id,
            self.defs,
            self.namespaces,
            self.symbols,
            self.types,
          ))
        },
        DefinitionKind::Enum(enum_definition)
          if def.owner_module == target_module_id && enum_definition.type_params.is_empty() =>
        {
          Some(build_mangled_name_standalone(
            def_id,
            self.defs,
            self.namespaces,
            self.symbols,
            self.types,
          ))
        },
        _ => None,
      })
      .collect();

    let mut referenced_struct_names: std::collections::HashSet<String> = std::collections::HashSet::new();

    for def_id in &self.forced_emit_defs {
      let def = self.defs.get(def_id);
      collect_struct_types_from_def(
        *def_id,
        def,
        self.defs,
        self.types,
        self.symbols,
        self.namespaces,
        &mut referenced_struct_names,
      );
    }

    referenced_struct_names.retain(|name| name.contains("__"));

    let external_definition_ids = collect_external_type_definition_ids(
      self.defs,
      self.types,
      self.symbols,
      self.namespaces,
      &module_type_names,
      &referenced_struct_names,
    );

    if external_definition_ids.is_empty() {
      return;
    }

    for def_id in external_definition_ids {
      let name = build_mangled_name_standalone(def_id, self.defs, self.namespaces, self.symbols, self.types);
      let type_guard = format!("IGNIS_TYPE_DEF_{}", sanitize_macro_name(&name));

      writeln!(self.output, "#ifndef {}", type_guard).unwrap();
      writeln!(self.output, "#define {}", type_guard).unwrap();

      match &self.defs.get(&def_id).kind {
        DefinitionKind::Record(record_definition) => {
          emit_record_definition_standalone(
            def_id,
            record_definition,
            self.defs,
            self.types,
            self.symbols,
            self.namespaces,
            &mut self.output,
          );
        },
        DefinitionKind::Enum(enum_definition) => {
          emit_enum_definition_standalone(
            def_id,
            enum_definition,
            self.defs,
            self.symbols,
            self.namespaces,
            self.types,
            &mut self.output,
          );
        },
        _ => continue,
      }

      writeln!(self.output, "#endif // {}", type_guard).unwrap();
    }

    writeln!(self.output).unwrap();
  }

  /// Check if an extern declaration should be emitted for a definition.
  fn should_emit_extern(
    &self,
    def_id: DefinitionId,
  ) -> bool {
    match &self.target {
      Some(target) => {
        let kind = self.classify(def_id);
        target.should_emit_extern(&kind)
      },
      None => true, // Legacy mode: emit all externs
    }
  }

  /// Emit forward declarations for all record and enum types.
  /// This allows structs to reference each other (e.g., for recursive types).
  /// Skips generic definitions (those with non-empty type_params) since they
  /// are only templates - concrete instantiations are emitted instead.
  fn emit_type_forward_declarations(&mut self) {
    let mut type_defs: Vec<_> = self
      .defs
      .iter()
      .filter(|(def_id, def)| {
        // Filter by target if set
        if !self.should_emit(*def_id) {
          return false;
        }
        match &def.kind {
          DefinitionKind::Record(rd) => rd.type_params.is_empty(),
          DefinitionKind::Enum(ed) => ed.type_params.is_empty(),
          _ => false,
        }
      })
      .collect();

    if type_defs.is_empty() {
      return;
    }

    type_defs.sort_by_key(|(id, _)| id.index());

    writeln!(self.output, "// Type forward declarations").unwrap();
    for (def_id, _) in &type_defs {
      let name = self.type_struct_name(*def_id);
      writeln!(self.output, "typedef struct {} {};", name, name).unwrap();
    }
    writeln!(self.output).unwrap();
  }

  fn emit_slice_types(&mut self) {
    let mut slice_type_ids: Vec<TypeId> = self
      .types
      .iter()
      .filter_map(|(type_id, ty)| match ty {
        Type::Slice { element, .. } if self.is_fully_monomorphized(*element) => Some(type_id),
        _ => None,
      })
      .collect();

    if slice_type_ids.is_empty() {
      return;
    }

    slice_type_ids.sort_by_key(|type_id| type_id.index());
    slice_type_ids.dedup();

    writeln!(self.output, "// Slice types").unwrap();
    for slice_type_id in slice_type_ids {
      self.emit_slice_type_definition(slice_type_id);
    }
    writeln!(self.output).unwrap();
  }

  fn emit_slice_type_definition(
    &mut self,
    slice_type_id: TypeId,
  ) {
    let Type::Slice { element, .. } = self.types.get(&slice_type_id) else {
      return;
    };

    let struct_name = self.slice_struct_name(slice_type_id);
    let data_type = self.format_type(*element);

    writeln!(self.output, "struct {} {{", struct_name).unwrap();
    writeln!(self.output, "    {}* data;", data_type).unwrap();
    writeln!(self.output, "    u64 len;").unwrap();
    writeln!(self.output, "}};").unwrap();
  }

  /// Emit struct definitions for all record and enum types.
  /// Skips generic definitions (those with non-empty type_params) since they
  /// are only templates - concrete instantiations are emitted instead.
  fn emit_type_definitions(&mut self) {
    let mut type_defs: Vec<_> = self
      .defs
      .iter()
      .filter(|(def_id, def)| {
        // Filter by target if set
        if !self.should_emit(*def_id) {
          return false;
        }

        match &def.kind {
          DefinitionKind::Record(rd) => {
            // Skip generic records and records with unresolved types in fields
            rd.type_params.is_empty() && rd.fields.iter().all(|f| self.is_fully_monomorphized(f.type_id))
          },
          DefinitionKind::Enum(ed) => {
            // Skip generic enums and enums with unresolved types in payloads
            ed.type_params.is_empty()
              && ed
                .variants
                .iter()
                .all(|v| v.payload.iter().all(|&ty| self.is_fully_monomorphized(ty)))
          },
          _ => false,
        }
      })
      .collect();

    if type_defs.is_empty() {
      return;
    }

    type_defs.sort_by_key(|(id, _)| id.index());

    writeln!(self.output, "// Type definitions").unwrap();
    for (def_id, def) in type_defs {
      match &def.kind {
        DefinitionKind::Record(rd) => {
          self.emit_record_definition(def_id, rd);
        },
        DefinitionKind::Enum(ed) => {
          self.emit_enum_definition(def_id, ed);
        },
        _ => {},
      }
    }
    writeln!(self.output).unwrap();
  }

  /// Scan LIR for closure instructions and emit the required C type definitions:
  /// - An env struct per thunk (holding captured values)
  /// - A closure struct per unique function signature
  fn emit_closure_types(&mut self) {
    let mut env_infos: Vec<(DefinitionId, Vec<TypeId>, TypeId)> = Vec::new();
    let mut seen_thunks: HashSet<DefinitionId> = HashSet::new();
    let mut seen_sigs: HashSet<TypeId> = HashSet::new();

    // Collect TypeIds that are raw function pointers (from DropGlue destinations).
    // These must NOT be registered as closure structs — they represent bare C
    // function pointers, not Ignis closures with environment.
    let mut raw_fn_ptr_types: HashSet<TypeId> = HashSet::new();

    for func in self.program.functions.values() {
      for block in func.blocks.get_all() {
        for instr in &block.instructions {
          if let Instr::DropGlue { dest, .. } = instr {
            let ty = func.temp_type(*dest);
            raw_fn_ptr_types.insert(ty);
          }
        }
      }
    }

    for func in self.program.functions.values() {
      // Scan MakeClosure instructions for thunk env types and closure signatures
      for block in func.blocks.get_all() {
        for instr in &block.instructions {
          if let Instr::MakeClosure {
            thunk,
            captures,
            closure_type,
            ..
          } = instr
          {
            if seen_thunks.insert(*thunk) {
              let cap_types: Vec<TypeId> = captures
                .iter()
                .map(|op| match op {
                  Operand::Temp(t) => func.temp_type(*t),
                  Operand::Local(l) => func.locals.get(l).ty,
                  Operand::Const(c) => c.type_id(),
                  Operand::FuncRef(_) | Operand::GlobalRef(_) => self.types.void(),
                })
                .collect();
              env_infos.push((*thunk, cap_types, *closure_type));
            }

            seen_sigs.insert(*closure_type);
          }
        }
      }

      // Scan non-extern function parameters and locals for closure types. Functions
      // that *receive* closures as parameters (e.g. forEach, map) don't have
      // MakeClosure instructions — the caller creates the closure. We still need the
      // struct definition to emit the correct parameter type in the function signature.
      //
      // Extern functions are skipped: their Type::Function parameters represent raw C
      // function pointers (e.g. IgnisDropFn), not Ignis closures with environment.
      if !func.is_extern {
        for param_id in func.params.iter() {
          if let DefinitionKind::Parameter(param) = &self.defs.get(param_id).kind
            && matches!(self.types.get(&param.type_id), Type::Function { .. })
            && !raw_fn_ptr_types.contains(&param.type_id)
          {
            seen_sigs.insert(param.type_id);
          }
        }
      }

      for local in func.locals.get_all() {
        if matches!(self.types.get(&local.ty), Type::Function { .. }) && !raw_fn_ptr_types.contains(&local.ty) {
          seen_sigs.insert(local.ty);
        }
      }
    }

    if env_infos.is_empty() && seen_sigs.is_empty() {
      return;
    }

    writeln!(self.output, "// Closure types").unwrap();

    for (thunk_id, cap_types, _) in &env_infos {
      let env_name = format!("__closure_env_{}", thunk_id.index());
      writeln!(self.output, "struct {} {{", env_name).unwrap();

      if cap_types.is_empty() {
        writeln!(self.output, "    char _empty;").unwrap();
      } else {
        for (i, &ty) in cap_types.iter().enumerate() {
          let ty_str = self.format_type(ty);
          writeln!(self.output, "    {} field_{};", ty_str, i).unwrap();
        }
      }

      writeln!(self.output, "}};").unwrap();
      self.closure_env_names.insert(*thunk_id, env_name);
    }

    for &sig_type_id in &seen_sigs {
      if let Type::Function { params, ret, .. } = self.types.get(&sig_type_id) {
        let struct_name = self.closure_struct_name(sig_type_id);

        let ret_str = self.format_type(*ret);
        let mut call_params = vec!["u8*".to_string()];
        for &p in params {
          call_params.push(self.format_type(p));
        }

        writeln!(self.output, "struct {} {{", struct_name).unwrap();
        writeln!(self.output, "    {} (*call)({});", ret_str, call_params.join(", ")).unwrap();
        writeln!(self.output, "    void (*drop_fn)(u8*);").unwrap();
        writeln!(self.output, "    u8* env;").unwrap();
        writeln!(self.output, "}};").unwrap();

        self.closure_struct_names.insert(sig_type_id, struct_name);
      }
    }

    writeln!(self.output).unwrap();
  }

  /// Build a deterministic name for a closure struct based on function signature type.
  fn closure_struct_name(
    &self,
    sig_type_id: TypeId,
  ) -> String {
    if let Type::Function { params, ret, .. } = self.types.get(&sig_type_id) {
      let mut parts = Vec::new();
      for &p in params {
        parts.push(self.format_type_for_name(p));
      }
      parts.push(self.format_type_for_name(*ret));
      format!("__ignis_closure_{}", parts.join("_"))
    } else {
      format!("__ignis_closure_{}", sig_type_id.index())
    }
  }

  /// Format a type as a simple identifier fragment (for building closure struct names).
  fn format_type_for_name(
    &self,
    ty: TypeId,
  ) -> String {
    match self.types.get(&ty) {
      Type::I8 => "i8".to_string(),
      Type::I16 => "i16".to_string(),
      Type::I32 => "i32".to_string(),
      Type::I64 => "i64".to_string(),
      Type::U8 => "u8".to_string(),
      Type::U16 => "u16".to_string(),
      Type::U32 => "u32".to_string(),
      Type::U64 => "u64".to_string(),
      Type::F32 => "f32".to_string(),
      Type::F64 => "f64".to_string(),
      Type::Boolean => "bool".to_string(),
      Type::Char => "char".to_string(),
      Type::Str => "str".to_string(),
      Type::Void => "void".to_string(),
      Type::Never => "never".to_string(),
      Type::Pointer { inner, .. } => format!("ptr_{}", self.format_type_for_name(*inner)),
      Type::Reference { inner, .. } => format!("ref_{}", self.format_type_for_name(*inner)),
      Type::Slice { element, .. } => format!("slice_{}", self.format_type_for_name(*element)),
      Type::Record(def_id) | Type::Enum(def_id) => self.build_mangled_name(*def_id),
      _ => format!("ty{}", ty.index()),
    }
  }

  fn slice_struct_name(
    &self,
    slice_type_id: TypeId,
  ) -> String {
    let Type::Slice { element, .. } = self.types.get(&slice_type_id) else {
      return format!("__ignis_slice_{}", slice_type_id.index());
    };

    format!("__ignis_slice_{}", self.format_type_for_name(*element))
  }

  /// Check if a type is fully monomorphized (no Type::Param or Type::Instance).
  fn is_fully_monomorphized(
    &self,
    type_id: TypeId,
  ) -> bool {
    match self.types.get(&type_id) {
      Type::Param { .. } | Type::Instance { .. } | Type::Infer => false,
      Type::Pointer { inner, .. } | Type::Reference { inner, .. } => self.is_fully_monomorphized(*inner),
      Type::Slice { element, .. } => self.is_fully_monomorphized(*element),
      Type::FixedArray { element, .. } => self.is_fully_monomorphized(*element),
      Type::Function { params, ret, .. } => {
        params.iter().all(|&p| self.is_fully_monomorphized(p)) && self.is_fully_monomorphized(*ret)
      },
      _ => true,
    }
  }

  /// Check if a function's signature is fully monomorphized.
  /// Returns false if any parameter type or return type contains Type::Param/Instance.
  fn is_function_signature_monomorphized(
    &self,
    func: &FunctionLir,
  ) -> bool {
    // Check return type
    if !self.is_fully_monomorphized(func.return_type) {
      return false;
    }

    // Check all parameter types
    for &param_def_id in &func.params {
      let param_type = *self.defs.type_of(&param_def_id);
      if !self.is_fully_monomorphized(param_type) {
        return false;
      }
    }

    true
  }

  /// Emit a struct definition for a record type.
  fn emit_record_definition(
    &mut self,
    def_id: DefinitionId,
    rd: &RecordDefinition,
  ) {
    let name = self.type_struct_name(def_id);

    writeln!(self.output, "struct {} {{", name).unwrap();

    if rd.fields.is_empty() && !rd.lang_traits.drop {
      // C doesn't allow empty structs, add a dummy field
      writeln!(self.output, "    char _empty;").unwrap();
    } else {
      for field in &rd.fields {
        let field_ty = self.format_type(field.type_id);
        let field_attr_str = self.format_field_attrs(&field.attrs);
        writeln!(self.output, "    {} field_{}{};", field_ty, field.index, field_attr_str).unwrap();
      }

      if rd.lang_traits.drop {
        writeln!(self.output, "    uint8_t __ignis_drop_state;").unwrap();
      }
    }

    write!(self.output, "}}").unwrap();
    self.emit_record_attrs(&rd.attrs);
    writeln!(self.output, ";").unwrap();
  }

  /// Emit a tagged union definition for an enum type.
  fn emit_enum_definition(
    &mut self,
    def_id: DefinitionId,
    ed: &EnumDefinition,
  ) {
    let name = self.type_struct_name(def_id);

    writeln!(self.output, "struct {} {{", name).unwrap();

    // Tag field
    let tag_ty = self.format_type(ed.tag_type);
    writeln!(self.output, "    {} tag;", tag_ty).unwrap();

    // Check if any variant has payload
    let has_payload = ed.variants.iter().any(|variant| {
      variant
        .payload
        .iter()
        .any(|payload_ty| !matches!(self.types.get(payload_ty), Type::Void))
    });

    if has_payload {
      writeln!(self.output, "    union {{").unwrap();

      for variant in &ed.variants {
        let payload_fields: Vec<_> = variant
          .payload
          .iter()
          .copied()
          .filter(|payload_ty| !matches!(self.types.get(payload_ty), Type::Void))
          .collect();

        if payload_fields.is_empty() {
          continue;
        }

        writeln!(self.output, "        struct {{").unwrap();
        for (i, payload_ty) in payload_fields.iter().enumerate() {
          let ty = self.format_type(*payload_ty);
          writeln!(self.output, "            {} field_{};", ty, i).unwrap();
        }
        writeln!(self.output, "        }} variant_{};", variant.tag_value).unwrap();
      }

      writeln!(self.output, "    }} payload;").unwrap();
    }

    if ed.lang_traits.drop {
      writeln!(self.output, "    uint8_t __ignis_drop_state;").unwrap();
    }

    write!(self.output, "}}").unwrap();
    self.emit_record_attrs(&ed.attrs);
    writeln!(self.output, ";").unwrap();

    // Emit tag constants as #defines
    for variant in &ed.variants {
      let variant_name = self.symbols.get(&variant.name);
      writeln!(self.output, "#define {}_{} {}", name, variant_name, variant.tag_value).unwrap();
    }
  }

  /// Get the C struct name for a type definition (record or enum).
  fn type_struct_name(
    &self,
    def_id: DefinitionId,
  ) -> String {
    self.build_mangled_name(def_id)
  }

  fn emit_record_attrs(
    &mut self,
    attrs: &[RecordAttr],
  ) {
    for attr in attrs {
      match attr {
        RecordAttr::Packed => write!(self.output, " __attribute__((packed))").unwrap(),
        RecordAttr::Aligned(n) => write!(self.output, " __attribute__((aligned({})))", n).unwrap(),
        RecordAttr::LangTry => {},
      }
    }
  }

  fn format_field_attrs(
    &self,
    attrs: &[FieldAttr],
  ) -> String {
    let mut result = String::new();
    for attr in attrs {
      match attr {
        FieldAttr::Aligned(n) => {
          write!(result, " __attribute__((aligned({})))", n).unwrap();
        },
      }
    }
    result
  }

  fn get_function_attrs(
    &self,
    def_id: DefinitionId,
  ) -> &[FunctionAttr] {
    match &self.defs.get(&def_id).kind {
      DefinitionKind::Function(f) => &f.attrs,
      DefinitionKind::Method(m) => &m.attrs,
      _ => &[],
    }
  }

  fn emit_headers(&mut self) {
    for header in self.headers {
      if header.quoted {
        writeln!(self.output, "#include \"{}\"", header.path).unwrap();
      } else {
        writeln!(self.output, "#include <{}>", header.path).unwrap();
      }
    }

    if !self.headers.is_empty() {
      writeln!(self.output).unwrap();
    }
  }

  /// Emit static constants from records and enums.
  fn emit_static_constants(&mut self) {
    let mut constants: Vec<(DefinitionId, String, String)> = Vec::new();

    // Collect static fields from records
    for (def_id, def) in self.defs.iter() {
      // Filter by target
      if !self.should_emit(def_id) {
        continue;
      }

      if let DefinitionKind::Record(rd) = &def.kind {
        let type_name = self.build_mangled_name(def_id);
        for (&field_name_sym, &const_def_id) in &rd.static_fields {
          if let DefinitionKind::Constant(const_def) = &self.defs.get(&const_def_id).kind
            && let Some(value) = &const_def.value
          {
            let field_name = self.symbols.get(&field_name_sym);
            let c_type = self.format_type(const_def.type_id);
            let c_value = self.const_value_to_c(value, &const_def.type_id);
            let full_name = format!("{}_{}", type_name, Self::escape_ident(field_name));
            constants.push((
              const_def_id,
              format!("static const {} {} = {};", c_type, full_name, c_value),
              full_name,
            ));
          }
        }
      }
      if let DefinitionKind::Enum(ed) = &def.kind {
        let type_name = self.build_mangled_name(def_id);
        for (&field_name_sym, &const_def_id) in &ed.static_fields {
          if let DefinitionKind::Constant(const_def) = &self.defs.get(&const_def_id).kind
            && let Some(value) = &const_def.value
          {
            let field_name = self.symbols.get(&field_name_sym);
            let c_type = self.format_type(const_def.type_id);
            let c_value = self.const_value_to_c(value, &const_def.type_id);
            let full_name = format!("{}_{}", type_name, Self::escape_ident(field_name));
            constants.push((
              const_def_id,
              format!("static const {} {} = {};", c_type, full_name, c_value),
              full_name,
            ));
          }
        }
      }
    }

    if constants.is_empty() {
      return;
    }

    constants.sort_by_key(|(id, _, _)| id.index());

    writeln!(self.output, "// Static constants").unwrap();
    for (_, decl, _) in &constants {
      writeln!(self.output, "{}", decl).unwrap();
    }
    writeln!(self.output).unwrap();
  }

  #[allow(clippy::only_used_in_recursion)]
  fn const_value_to_c(
    &self,
    value: &ignis_type::definition::ConstValue,
    _type_id: &TypeId,
  ) -> String {
    use ignis_type::definition::ConstValue;
    match value {
      ConstValue::Int(i) => i.to_string(),
      ConstValue::Float(f) => format!("{:.}", f.into_inner()),
      ConstValue::Bool(b) => if *b { "true" } else { "false" }.to_string(),
      ConstValue::Char(c) => format!("((ignis_char_t){})", c),
      ConstValue::String(s) => format!("\"{}\"", s.escape_default()),
      ConstValue::Null => "NULL".to_string(),
      ConstValue::Array(arr) => {
        let elements: Vec<String> = arr.iter().map(|v| self.const_value_to_c(v, _type_id)).collect();
        format!("{{{}}}", elements.join(", "))
      },
      ConstValue::Tuple(elems) => {
        let elements: Vec<String> = elems.iter().map(|v| self.const_value_to_c(v, _type_id)).collect();
        format!("{{{}}}", elements.join(", "))
      },
    }
  }

  fn emit_extern_declarations(&mut self) {
    let mut funcs: Vec<_> = self.program.functions.iter().collect();
    funcs.sort_by_key(|(def_id, _)| def_id.index());

    // Filter extern functions based on target's extern hygiene rules
    let extern_funcs: Vec<_> = funcs
      .iter()
      .filter(|(def_id, func)| func.is_extern && self.should_emit_extern(**def_id))
      .collect();

    if extern_funcs.is_empty() {
      return;
    }

    writeln!(self.output, "// Extern declarations").unwrap();
    for (def_id, func) in extern_funcs {
      write!(self.output, "extern ").unwrap();
      self.emit_function_signature(**def_id, func);
      writeln!(self.output, ";").unwrap();
    }
    writeln!(self.output).unwrap();
  }

  fn drop_glue_name(
    &self,
    ty: TypeId,
  ) -> String {
    format!("ignis_drop_glue_{}", self.format_type_for_mangling(&ty))
  }

  fn emit_drop_glue_helpers(&mut self) {
    let mut glue_types = Vec::new();

    for (def_id, func) in &self.program.functions {
      if !self.should_emit(*def_id) {
        continue;
      }

      for (_, block) in func.blocks.iter() {
        for instr in &block.instructions {
          if let Instr::DropGlue { ty, .. } = instr {
            glue_types.push(*ty);
          }
        }
      }
    }

    if glue_types.is_empty() {
      return;
    }

    glue_types.sort_by_key(|ty| ty.index());
    glue_types.dedup();

    for ty in glue_types {
      self.emit_drop_glue_helper(ty);
      writeln!(self.output).unwrap();
    }
  }

  fn emit_drop_glue_helper(
    &mut self,
    ty: TypeId,
  ) {
    let helper_name = self.drop_glue_name(ty);

    writeln!(self.output, "static void {}(u8* payload) {{", helper_name).unwrap();

    if !self.types.needs_drop_with_defs(&ty, self.defs) {
      writeln!(self.output, "    (void)payload;").unwrap();
      writeln!(self.output, "}}").unwrap();
      return;
    }

    let c_type = self.format_type(ty);
    writeln!(self.output, "    {}* value = ({}*)payload;", c_type, c_type).unwrap();
    write!(self.output, "    ").unwrap();
    self.emit_field_drops("(*value)", ty);
    writeln!(self.output, "}}").unwrap();
  }

  fn emit_forward_declarations(&mut self) {
    let mut funcs: Vec<_> = self.program.functions.iter().collect();
    funcs.sort_by_key(|(def_id, _)| def_id.index());

    // Only emit forward declarations for non-extern functions that match the target.
    // Extern functions are declared separately via emit_extern_declarations().
    // Skip functions with unmonomorphized signatures (generic functions that weren't instantiated).
    let mut emitted_any = false;
    for (def_id, func) in &funcs {
      if func.is_extern {
        continue;
      }

      // static inline functions don't need forward declarations.
      // Exported inline functions are not static, so they still need one.
      if matches!(func.inline_mode, InlineMode::Inline | InlineMode::Always) {
        let is_public = self.is_effectively_public(**def_id);
        if !is_public {
          continue;
        }
      }

      let should_emit = self.should_emit(**def_id);
      let should_forward_declare_external = !should_emit && self.should_forward_declare_external(**def_id, func);

      if !should_emit && !should_forward_declare_external {
        continue;
      }

      // Skip functions with Type::Param or Type::Instance in their signature
      if !self.is_function_signature_monomorphized(func) {
        continue;
      }

      self.emit_function_signature(**def_id, func);
      writeln!(self.output, ";").unwrap();
      emitted_any = true;
    }

    if emitted_any {
      writeln!(self.output).unwrap();
    }
  }

  fn should_forward_declare_external(
    &self,
    def_id: DefinitionId,
    func: &FunctionLir,
  ) -> bool {
    if func.is_extern || !self.is_function_signature_monomorphized(func) {
      return false;
    }

    let def = self.defs.get(&def_id);
    let kind = self.classify(def_id);

    if kind.is_runtime() {
      return false;
    }

    kind.is_user() || self.definition_depends_on_user_type(def_id) || matches!(def.kind, DefinitionKind::Method(_))
  }

  fn emit_functions(&mut self) {
    let mut funcs: Vec<_> = self.program.functions.iter().collect();
    funcs.sort_by_key(|(def_id, _)| def_id.index());

    for (def_id, func) in funcs {
      if func.is_extern {
        continue;
      }

      // Filter by target
      if !self.should_emit(*def_id) {
        continue;
      }

      // Skip functions with Type::Param or Type::Instance in their signature
      if !self.is_function_signature_monomorphized(func) {
        continue;
      }

      self.emit_function(*def_id, func);
    }
  }

  fn emit_entry_wrapper(&mut self) {
    if let Some(test_harness) = self.test_harness {
      self.emit_test_harness_wrapper(test_harness);
      return;
    }

    let Some((entry_def_id, entry_return_type)) = self
      .entry_main_def_and_func()
      .map(|(def_id, entry_func)| (def_id, entry_func.return_type))
    else {
      return;
    };

    let Some(entry_args) = self.entry_main_args_kind() else {
      writeln!(self.output, "int main(int argc, char** argv) {{").unwrap();
      writeln!(self.output, "    ignis_runtime_init((i32)argc, argv);").unwrap();
      writeln!(self.output, "    (void)argc;").unwrap();
      writeln!(self.output, "    (void)argv;").unwrap();
      writeln!(self.output, "    return 1;").unwrap();
      writeln!(self.output, "}}\n").unwrap();
      return;
    };

    let user_main_name = self.def_name(entry_def_id);
    let user_main_call = match entry_args {
      EntryMainArgs::None => {
        format!("{}()", user_main_name)
      },
      EntryMainArgs::ArgcArgv => {
        format!("{}((i32)argc, (const char**)argv)", user_main_name)
      },
    };

    writeln!(self.output, "int main(int argc, char** argv) {{").unwrap();
    writeln!(self.output, "    ignis_runtime_init((i32)argc, argv);").unwrap();

    if matches!(entry_args, EntryMainArgs::None) {
      writeln!(self.output, "    (void)argc;").unwrap();
      writeln!(self.output, "    (void)argv;").unwrap();
    }

    match self.entry_main_return_kind() {
      Some(EntryMainReturn::I32) => {
        writeln!(self.output, "    return {};", user_main_call).unwrap();
      },
      Some(EntryMainReturn::Void) => {
        writeln!(self.output, "    {};", user_main_call).unwrap();
        writeln!(self.output, "    return 0;").unwrap();
      },
      Some(EntryMainReturn::TryI32 {
        ok_tag,
        err_tag,
        error_display,
      }) => {
        let result_ty = self.format_type(entry_return_type);

        writeln!(self.output, "    {} __ignis_main_result = {};", result_ty, user_main_call).unwrap();
        writeln!(self.output, "    if (__ignis_main_result.tag == {}) {{", ok_tag).unwrap();
        writeln!(
          self.output,
          "        return __ignis_main_result.payload.variant_{}.field_0;",
          ok_tag
        )
        .unwrap();
        writeln!(self.output, "    }}").unwrap();

        let err_access = format!("__ignis_main_result.payload.variant_{}.field_0", err_tag);
        let err_msg_expr = match error_display {
          ErrorDisplay::Str => err_access.clone(),
          ErrorDisplay::RecordStrField { field_index } => format!("{}.field_{}", err_access, field_index),
          ErrorDisplay::Opaque => "\"(error)\"".to_string(),
        };

        writeln!(self.output, "    fprintf(stderr, \"Error: %s\\n\", {});", err_msg_expr).unwrap();
        writeln!(self.output, "    exit(101);").unwrap();
      },
      None => {
        writeln!(self.output, "    return 1;").unwrap();
      },
    }

    writeln!(self.output, "}}\n").unwrap();
  }

  fn emit_test_harness_wrapper(
    &mut self,
    test_harness: &[TestHarnessEntry],
  ) {
    writeln!(self.output, "int main(int argc, char** argv) {{").unwrap();
    writeln!(self.output, "    ignis_runtime_init((i32)argc, argv);").unwrap();
    writeln!(self.output, "    if (argc != 3 || strcmp(argv[1], \"--ignis-test\") != 0) {{").unwrap();
    writeln!(
      self.output,
      "        fprintf(stderr, \"Error: expected --ignis-test <name>\\n\");"
    )
    .unwrap();
    writeln!(self.output, "        return 2;").unwrap();
    writeln!(self.output, "    }}").unwrap();

    for test in test_harness {
      writeln!(self.output, "    if (strcmp(argv[2], {:?}) == 0) {{", test.fq_name).unwrap();
      writeln!(self.output, "        {}();", self.def_name(test.def_id)).unwrap();
      writeln!(self.output, "        return 0;").unwrap();
      writeln!(self.output, "    }}").unwrap();
    }

    writeln!(self.output, "    fprintf(stderr, \"Error: unknown test %s\\n\", argv[2]);").unwrap();
    writeln!(self.output, "    return 2;").unwrap();
    writeln!(self.output, "}}\n").unwrap();
  }

  fn entry_main_def_and_func(&self) -> Option<(DefinitionId, &FunctionLir)> {
    let entry_def_id = self.program.entry_point?;
    let entry_func = self.program.functions.get(&entry_def_id)?;

    if entry_func.is_extern || !self.should_emit(entry_def_id) || !self.is_function_signature_monomorphized(entry_func)
    {
      return None;
    }

    Some((entry_def_id, entry_func))
  }

  fn entry_main_return_kind(&self) -> Option<EntryMainReturn> {
    let (_, entry_func) = self.entry_main_def_and_func()?;

    match self.types.get(&entry_func.return_type) {
      Type::I32 => Some(EntryMainReturn::I32),
      Type::Void => Some(EntryMainReturn::Void),
      Type::Enum(enum_def_id) => self
        .entry_try_i32_layout(*enum_def_id)
        .map(|(ok_tag, err_tag, error_display)| EntryMainReturn::TryI32 {
          ok_tag,
          err_tag,
          error_display,
        }),
      _ => None,
    }
  }

  fn entry_main_args_kind(&self) -> Option<EntryMainArgs> {
    let (_, entry_func) = self.entry_main_def_and_func()?;

    match entry_func.params.as_slice() {
      [] => Some(EntryMainArgs::None),
      [argc_param, argv_param] => {
        let argc_type = *self.defs.type_of(argc_param);
        if argc_type != self.types.i32() {
          return None;
        }

        let argv_type = *self.defs.type_of(argv_param);
        match self.types.get(&argv_type) {
          Type::Pointer { inner, .. } if matches!(self.types.get(inner), Type::Str) => Some(EntryMainArgs::ArgcArgv),
          _ => None,
        }
      },
      _ => None,
    }
  }

  fn entry_try_i32_layout(
    &self,
    enum_def_id: DefinitionId,
  ) -> Option<(u32, u32, ErrorDisplay)> {
    let DefinitionKind::Enum(enum_def) = &self.defs.get(&enum_def_id).kind else {
      return None;
    };

    let try_capability = enum_def.try_capable?;
    let ok_variant = enum_def
      .variants
      .iter()
      .find(|variant| variant.tag_value == try_capability.ok_variant)?;
    let err_variant = enum_def
      .variants
      .iter()
      .find(|variant| variant.tag_value == try_capability.err_variant)?;

    if ok_variant.payload.len() != 1 || ok_variant.payload[0] != self.types.i32() {
      return None;
    }

    let error_display = if err_variant.payload.len() == 1 {
      self.error_display_for_type(err_variant.payload[0])
    } else {
      ErrorDisplay::Opaque
    };

    Some((try_capability.ok_variant, try_capability.err_variant, error_display))
  }

  fn error_display_for_type(
    &self,
    ty: TypeId,
  ) -> ErrorDisplay {
    match self.types.get(&ty) {
      Type::Str => ErrorDisplay::Str,

      Type::Record(def_id) => {
        let DefinitionKind::Record(rd) = &self.defs.get(def_id).kind else {
          return ErrorDisplay::Opaque;
        };

        let message_sym = self.symbols.map.get("message");
        if let Some(&sym) = message_sym
          && let Some(field) = rd
            .fields
            .iter()
            .find(|f| f.name == sym && f.type_id == self.types.str())
        {
          return ErrorDisplay::RecordStrField {
            field_index: field.index,
          };
        }

        ErrorDisplay::Opaque
      },

      _ => ErrorDisplay::Opaque,
    }
  }

  fn emit_function_signature(
    &mut self,
    def_id: DefinitionId,
    func: &FunctionLir,
  ) {
    let name = self.def_name(def_id);
    let ret_ty = self.format_type(func.return_type);
    let is_internal_closure_helper = self.is_internal_closure_helper(def_id);

    let params: Vec<_> = func
      .params
      .iter()
      .map(|&p| {
        let pname = self.def_name(p);
        let pty = self.format_type(*self.defs.type_of(&p));
        format!("{} {}", pty, pname)
      })
      .collect();

    let params_str = if params.is_empty() {
      "void".to_string()
    } else if func.is_variadic {
      format!("{}, ...", params.join(", "))
    } else {
      params.join(", ")
    };

    // Public functions must not be `static` -- conflicts with the header declaration.
    // Module-private free functions are translation-unit-local: without `static`,
    // identically-named private functions across modules collide at link time
    // even after the overload-suffix mangling, because two `function readFixture(path: str): String`
    // in different files share the same signature and therefore the same mangled
    // name. Marking them `static` gives each TU its own copy and resolves the
    // collision.
    //
    // Two exceptions to the !is_public -> static rule:
    //
    // - `@test` functions: the generated test harness lives in a separate
    //   translation unit and references them via `extern void`, so they must
    //   keep external linkage even when their owning namespace is private.
    //
    // - Methods (DefinitionKind::Method): methods of an exported record can be
    //   invoked from another module even when the method itself is not marked
    //   public. The caller's translation unit emits a forward declaration and
    //   relies on external linkage at the definition; marking the definition
    //   `static` would break the cross-module call.
    if !func.is_extern {
      let mut is_public = self.is_effectively_public(def_id);
      let def_kind = &self.defs.get(&def_id).kind;
      let is_method = matches!(def_kind, DefinitionKind::Method(_));
      let is_test_function = matches!(
        def_kind,
        DefinitionKind::Function(function_def) if function_def.has_test_attr()
      );
      let mut needs_internal_linkage = !is_public && !is_method && !is_test_function;
      let is_forced_cross_module_helper = self.forced_emit_defs.contains(&def_id)
        && self
          .target
          .as_ref()
          .and_then(|target| target.target_user_module())
          .is_some_and(|target_module_id| self.defs.get(&def_id).owner_module != target_module_id);

      // In per-user-module emission, helper definitions forced from other modules
      // are implementation details of this translation unit.
      if is_forced_cross_module_helper
        && (self.is_missing_std_archive_generic_specialization(def_id)
          || cross_module_emit_needs_internal_linkage(is_public))
      {
        is_public = false;
        needs_internal_linkage = true;
      }

      if is_internal_closure_helper {
        is_public = false;
        needs_internal_linkage = true;

        if !matches!(func.inline_mode, InlineMode::Inline | InlineMode::Always) {
          needs_internal_linkage = true;
        }
      }

      if needs_internal_linkage && !matches!(func.inline_mode, InlineMode::Inline | InlineMode::Always) {
        write!(self.output, "static ").unwrap();
      }

      let func_attrs = self.get_function_attrs(def_id);
      if func_attrs.iter().any(|a| matches!(a, FunctionAttr::Cold)) {
        write!(self.output, "__attribute__((cold)) ").unwrap();
      }

      match func.inline_mode {
        InlineMode::Inline if is_public => write!(self.output, "inline ").unwrap(),
        InlineMode::Inline => write!(self.output, "static inline ").unwrap(),
        InlineMode::Always if is_public => write!(self.output, "__attribute__((always_inline)) inline ").unwrap(),
        InlineMode::Always => write!(self.output, "__attribute__((always_inline)) static inline ").unwrap(),
        InlineMode::Never => write!(self.output, "__attribute__((noinline)) ").unwrap(),
        InlineMode::None => {},
      }
    }

    write!(self.output, "{} {}({})", ret_ty, name, params_str).unwrap();
  }

  fn current_output_line(&self) -> usize {
    self.output.bytes().filter(|byte| *byte == b'\n').count() + 1
  }

  fn format_line_filename(path: &std::path::Path) -> String {
    path.display().to_string().replace('\\', "\\\\").replace('"', "\\\"")
  }

  fn emit_line_directive_for_span(
    &mut self,
    span: &ignis_type::span::Span,
  ) -> bool {
    let Some(source_map) = self.source_map else {
      return false;
    };

    if span.file == FileId::SYNTHETIC {
      return false;
    }

    let file = source_map.get(&span.file);
    let (line, _) = source_map.line_col(&span.file, span.start);
    writeln!(self.output, "#line {} \"{}\"", line, Self::format_line_filename(&file.path)).unwrap();
    true
  }

  fn emit_generated_line_reset(&mut self) {
    let next_generated_line = self.current_output_line() + 2;
    writeln!(self.output, "#line {} \"{}\"", next_generated_line, GENERATED_C_LINE_FILE).unwrap();
  }

  fn emit_function(
    &mut self,
    def_id: DefinitionId,
    func: &FunctionLir,
  ) {
    self.current_fn_id = Some(def_id);
    let emitted_source_mapping = if self.classify(def_id).is_user() {
      self.emit_line_directive_for_span(&func.span)
    } else {
      false
    };
    self.emit_function_signature(def_id, func);
    writeln!(self.output, " {{").unwrap();

    self.emit_locals(func);
    self.emit_temps(func);

    for (idx, block) in func.blocks.get_all().iter().enumerate() {
      if idx > 0 {
        writeln!(self.output).unwrap();
      }
      self.emit_block(func, block);
    }

    writeln!(self.output, "}}\n").unwrap();
    if emitted_source_mapping {
      self.emit_generated_line_reset();
      writeln!(self.output).unwrap();
    }
    self.current_fn_id = None;
  }

  fn emit_locals(
    &mut self,
    func: &FunctionLir,
  ) {
    let locals = func.locals.get_all();
    if locals.is_empty() {
      return;
    }

    writeln!(self.output, "    // Locals").unwrap();
    let drop_scheduled_locals: HashSet<_> = func
      .blocks
      .get_all()
      .iter()
      .flat_map(|block| block.instructions.iter())
      .filter_map(|instruction| match instruction {
        Instr::Drop { local } => Some(local.index()),
        _ => None,
      })
      .collect();
    let mut droppable_locals = Vec::new();

    for (idx, local) in locals.iter().enumerate() {
      let name = local.name.as_deref().unwrap_or("_");
      if let Type::FixedArray { element, size: n } = self.types.get(&local.ty) {
        let elem_ty = self.format_type(*element);
        writeln!(self.output, "    {} l{}[{}]; // {}", elem_ty, idx, n, name).unwrap();
      } else {
        let ty = self.format_var_type(local.ty);
        writeln!(self.output, "    {} l{}; // {}", ty, idx, name).unwrap();

        if !local.borrowed_alias
          && (drop_scheduled_locals.contains(&(idx as u32)) || self.types.needs_drop_with_defs(&local.ty, self.defs))
        {
          droppable_locals.push((idx, local.ty));
        }
      }
    }

    if !droppable_locals.is_empty() {
      writeln!(self.output).unwrap();
      writeln!(
        self.output,
        "    // Mark droppable locals as uninitialized until their first store"
      )
      .unwrap();
      for (idx, ty) in droppable_locals {
        writeln!(self.output, "    __builtin_memset((void*)&l{}, 0, sizeof(l{}));", idx, idx).unwrap();
        self.emit_uninitialized_drop_state(&format!("l{}", idx), ty);
      }
    }

    writeln!(self.output).unwrap();
  }

  fn emit_uninitialized_drop_state(
    &mut self,
    expr: &str,
    ty: TypeId,
  ) {
    match self.types.get(&ty).clone() {
      Type::Record(def_id) => {
        if self.record_has_drop_trait(def_id) {
          writeln!(self.output, "    {}.__ignis_drop_state = 1;", expr).unwrap();
          return;
        }

        let field_info: Vec<(u32, TypeId)> = {
          let def = self.defs.get(&def_id);
          match &def.kind {
            DefinitionKind::Record(record_definition) => record_definition
              .fields
              .iter()
              .map(|field| (field.index, field.type_id))
              .collect(),
            _ => vec![],
          }
        };

        for (index, field_ty) in field_info {
          if self.types.needs_drop_with_defs(&field_ty, self.defs) {
            self.emit_uninitialized_drop_state(&format!("{}.field_{}", expr, index), field_ty);
          }
        }
      },
      Type::Enum(def_id) => {
        if self.record_has_drop_trait(def_id) {
          writeln!(self.output, "    {}.__ignis_drop_state = 1;", expr).unwrap();
        }
      },
      _ => {},
    }
  }

  fn emit_temps(
    &mut self,
    func: &FunctionLir,
  ) {
    let temps = func.temps.get_all();
    if temps.is_empty() {
      return;
    }

    writeln!(self.output, "    // Temporaries").unwrap();
    for (idx, temp) in temps.iter().enumerate() {
      let ty = self.format_var_type(temp.ty);
      writeln!(self.output, "    {} t{};", ty, idx).unwrap();
    }
    writeln!(self.output).unwrap();
  }

  fn format_var_type(
    &self,
    ty: TypeId,
  ) -> String {
    match self.types.get(&ty) {
      Type::Void | Type::Never => "int".to_string(),
      _ => self.format_type(ty),
    }
  }

  fn emit_block(
    &mut self,
    func: &FunctionLir,
    block: &Block,
  ) {
    writeln!(self.output, "{}:", block.label).unwrap();

    for instr in &block.instructions {
      write!(self.output, "    ").unwrap();
      self.emit_instr(func, instr);
    }

    write!(self.output, "    ").unwrap();
    self.emit_terminator(func, &block.terminator);
  }

  fn emit_instr(
    &mut self,
    func: &FunctionLir,
    instr: &Instr,
  ) {
    match instr {
      Instr::Load { dest, source } => {
        writeln!(self.output, "t{} = l{};", dest.index(), source.index()).unwrap();
      },
      Instr::Store { dest, value } => {
        let local_info = func.locals.get(dest);
        let target_ty = local_info.ty;

        if let Type::FixedArray { size: n, element } = self.types.get(&target_ty) {
          let val = self.format_operand(func, value);
          let elem_size = self.sizeof_type(*element);
          writeln!(self.output, "memcpy(l{}, {}, {} * {});", dest.index(), val, n, elem_size).unwrap();
        } else {
          let val = self.format_operand_coerced(func, value, target_ty);
          writeln!(self.output, "l{} = {};", dest.index(), val).unwrap();
        }
      },
      Instr::LoadPtr { dest, ptr } => {
        let p = self.format_operand(func, ptr);

        writeln!(self.output, "t{} = *{};", dest.index(), p).unwrap();
      },
      Instr::StorePtr { ptr, value } => {
        let p = self.format_operand(func, ptr);

        // Resolve pointee type so format_operand_coerced can apply future coercions.
        let v = if let Some(ptr_ty) = self.operand_type(func, ptr) {
          if let Type::Pointer { inner, .. } = self.types.get(&ptr_ty) {
            self.format_operand_coerced(func, value, *inner)
          } else {
            self.format_operand(func, value)
          }
        } else {
          self.format_operand(func, value)
        };

        writeln!(self.output, "*{} = {};", p, v).unwrap();
      },
      Instr::BuiltinLoad { dest, ptr, ty } => {
        let p = self.format_operand(func, ptr);
        let c_type = self.format_type(*ty);

        writeln!(self.output, "t{} = *({}*)({});", dest.index(), c_type, p).unwrap();
      },
      Instr::BuiltinStore { ptr, value, ty } => {
        let p = self.format_operand(func, ptr);
        let v = self.format_operand(func, value);
        let c_type = self.format_type(*ty);

        writeln!(self.output, "*({}*)({}) = {};", c_type, p, v).unwrap();
      },
      Instr::BuiltinHash { value, hasher, ty } => {
        self.emit_builtin_hash(func, value, hasher, *ty);
      },
      Instr::BuiltinEq {
        dest,
        left,
        right,
        ty,
        kind,
      } => {
        self.emit_builtin_eq(func, *dest, left, right, *ty, *kind);
      },
      Instr::Copy { dest, source } => {
        let s = self.format_operand(func, source);

        writeln!(self.output, "t{} = {};", dest.index(), s).unwrap();
      },
      Instr::BinOp { dest, op, left, right } => {
        if self.emit_pointer_binop(func, *dest, op, left, right) {
          return;
        }

        if matches!(op, BinaryOperation::Equal | BinaryOperation::NotEqual)
          && self.emit_enum_comparison(func, *dest, op, left, right)
        {
          return;
        }

        let l = self.format_operand(func, left);
        let r = self.format_operand(func, right);

        if matches!(op, BinaryOperation::Pow) {
          writeln!(self.output, "t{} = pow({}, {});", dest.index(), l, r).unwrap();
        } else {
          let op_str = self.format_binop(op);
          writeln!(self.output, "t{} = {} {} {};", dest.index(), l, op_str, r).unwrap();
        }
      },
      Instr::UnaryOp { dest, op, operand } => {
        let o = self.format_operand(func, operand);
        let op_str = self.format_unaryop(op);

        writeln!(self.output, "t{} = {}{};", dest.index(), op_str, o).unwrap();
      },
      Instr::Call { dest, callee, args } => {
        self.emit_method_receiver_drop_guard(func, *callee, args);

        let name = self.def_name(*callee);

        // Get callee parameter types and extern flag
        let (param_types, callee_is_extern): (Vec<TypeId>, bool) = {
          let def = self.defs.get(callee);
          let params = match &def.kind {
            DefinitionKind::Function(fd) => fd.params.iter().map(|p| *self.defs.type_of(p)).collect(),
            DefinitionKind::Method(md) => md.params.iter().map(|p| *self.defs.type_of(p)).collect(),
            _ => Vec::new(),
          };
          let is_ext = match &def.kind {
            DefinitionKind::Function(fd) => fd.is_extern,
            _ => false,
          };
          (params, is_ext)
        };

        let args_str: Vec<_> = args
          .iter()
          .enumerate()
          .map(|(i, a)| {
            let formatted = if let Some(&pt) = param_types.get(i) {
              self.format_operand_coerced(func, a, pt)
            } else {
              self.format_operand(func, a)
            };

            // Extern functions may use C-level types (e.g. IgnisString*) that are
            // structurally identical to compiler-generated record structs but have a
            // different C tag.  Cast pointer/reference args through void* so C
            // doesn't warn about incompatible pointer types.
            if callee_is_extern && let Some(op_ty) = self.operand_type(func, a) {
              match self.types.get(&op_ty) {
                Type::Reference { .. } | Type::Pointer { .. } => {
                  return format!("(void*){}", formatted);
                },
                _ => {},
              }
            }

            formatted
          })
          .collect();

        if let Some(d) = dest {
          writeln!(self.output, "t{} = {}({});", d.index(), name, args_str.join(", ")).unwrap();
        } else {
          writeln!(self.output, "{}({});", name, args_str.join(", ")).unwrap();
        }

        self.emit_manual_drop_state_update(func, *callee, args);
      },
      Instr::Cast {
        dest,
        source,
        target_type,
      } => {
        let s = self.format_operand(func, source);
        let ty = self.format_type(*target_type);

        if self.type_contains_infer(*target_type) {
          panic!("ICE: cast to inferred type reached C codegen after implicit type removal");
        }

        if self.operand_contains_infer(func, source) {
          panic!("ICE: cast from inferred type reached C codegen after implicit type removal");
        }

        // Check for pointer <-> integer casts and use uintptr_t for C standards compliance
        let source_ty = self.operand_type(func, source);
        let target_ty_info = self.types.get(target_type);

        let source_is_ptr = source_ty.is_some_and(|t| matches!(self.types.get(&t), Type::Pointer { .. }));
        let target_is_ptr = matches!(target_ty_info, Type::Pointer { .. });
        let target_is_int = self.types.is_integer(target_type);
        let source_is_int = source_ty.is_some_and(|t| self.types.is_integer(&t));

        if source_is_ptr && target_is_int {
          // Pointer -> integer: use (target_type)(uintptr_t)(ptr)
          writeln!(self.output, "t{} = ({})(uintptr_t)({});", dest.index(), ty, s).unwrap();
        } else if source_is_int && target_is_ptr {
          // Integer -> pointer: use (target_type)(uintptr_t)(int_val)
          writeln!(self.output, "t{} = ({})(uintptr_t)({});", dest.index(), ty, s).unwrap();
        } else {
          // Regular cast
          writeln!(self.output, "t{} = ({})({});", dest.index(), ty, s).unwrap();
        }
      },
      Instr::BitCast {
        dest,
        source,
        target_type,
      } => {
        let s = self.format_operand(func, source);
        let target_ty_str = self.format_type(*target_type);
        let source_ty = self.operand_type(func, source);
        let source_ty_str = source_ty
          .map(|t| self.format_type(t))
          .unwrap_or_else(|| "void".to_string());

        writeln!(
          self.output,
          "_Static_assert(sizeof({}) == sizeof({}), \"bitCast: size mismatch\");",
          target_ty_str, source_ty_str
        )
        .unwrap();
        writeln!(self.output, "memcpy(&t{}, &{}, sizeof({}));", dest.index(), s, target_ty_str).unwrap();
      },
      Instr::AddrOfLocal { dest, local, .. } => {
        // For array locals, the name already decays to a pointer in C
        let local_info = func.locals.get(local);
        if matches!(self.types.get(&local_info.ty), Type::FixedArray { .. }) {
          writeln!(self.output, "t{} = l{};", dest.index(), local.index()).unwrap();
        } else {
          writeln!(self.output, "t{} = &l{};", dest.index(), local.index()).unwrap();
        }
      },
      Instr::GetElementPtr { dest, base, index, .. } => {
        let b = self.format_operand(func, base);
        let i = self.format_operand(func, index);

        let base_slice = self.operand_type(func, base).and_then(|ty| match self.types.get(&ty) {
          Type::Slice { .. } => Some(false),
          Type::Pointer { inner, .. } | Type::Reference { inner, .. } => {
            matches!(self.types.get(inner), Type::Slice { .. }).then_some(true)
          },
          _ => None,
        });

        if let Some(pointer_to_slice) = base_slice {
          if pointer_to_slice {
            writeln!(self.output, "t{} = &(({})->data[{}]);", dest.index(), b, i).unwrap();
          } else {
            writeln!(self.output, "t{} = &(({}).data[{}]);", dest.index(), b, i).unwrap();
          }
          return;
        }

        writeln!(self.output, "t{} = &{}[{}];", dest.index(), b, i).unwrap();
      },
      Instr::MakeSlice {
        dest,
        data,
        len,
        element_type: _,
      } => {
        let data_operand = self.format_operand(func, data);
        let len_operand = self.format_operand(func, len);
        let slice_type = self.format_type(func.temp_type(*dest));
        writeln!(
          self.output,
          "t{} = ({}){{ .data = {}, .len = {} }};",
          dest.index(),
          slice_type,
          data_operand,
          len_operand
        )
        .unwrap();
      },
      Instr::InitVector { dest_ptr, elements, .. } => {
        let p = self.format_operand(func, dest_ptr);

        for (i, elem) in elements.iter().enumerate() {
          if i > 0 {
            write!(self.output, "    ").unwrap();
          }

          let e = self.format_operand(func, elem);

          writeln!(self.output, "{}[{}] = {};", p, i, e).unwrap();
        }
      },
      Instr::Nop => {
        writeln!(self.output, "/* nop */").unwrap();
      },
      Instr::RuntimeCall { name, args } => {
        let args_str: Vec<_> = args.iter().map(|a| self.format_operand(func, a)).collect();
        writeln!(self.output, "{}({});", name, args_str.join(", ")).unwrap();
      },
      Instr::TypeIdOf { dest, source } => {
        let _ = (dest, source);
        panic!("ICE: typeIdOf reached C codegen after implicit type removal");
      },
      Instr::SizeOf { dest, ty } => {
        let c_type = self.format_type(*ty);
        writeln!(self.output, "t{} = sizeof({});", dest.index(), c_type).unwrap();
      },
      Instr::AlignOf { dest, ty } => {
        let c_type = self.format_type(*ty);
        writeln!(self.output, "t{} = _Alignof({});", dest.index(), c_type).unwrap();
      },
      Instr::MaxOf { dest, ty } => {
        let c_max = self.type_max_value(*ty);
        writeln!(self.output, "t{} = {};", dest.index(), c_max).unwrap();
      },
      Instr::MinOf { dest, ty } => {
        let c_min = self.type_min_value(*ty);
        writeln!(self.output, "t{} = {};", dest.index(), c_min).unwrap();
      },
      Instr::Drop { local } => {
        let local_data = func.locals.get(local);
        if local_data.borrowed_alias {
          writeln!(self.output, "/* drop l{}: borrowed alias */", local.index()).unwrap();
          return;
        }
        let ty = local_data.ty;

        match self.types.get(&ty) {
          Type::Record(_) => {
            self.emit_field_drops(&format!("l{}", local.index()), ty);
          },
          Type::Enum(_) => {
            self.emit_field_drops(&format!("l{}", local.index()), ty);
          },
          Type::Infer => {
            panic!("ICE: drop of inferred type reached C codegen after implicit type removal");
          },
          _ => {
            // Should not happen if LIR verification passed
            writeln!(self.output, "/* drop l{}: non-droppable type */", local.index()).unwrap();
          },
        }
      },
      Instr::GetFieldPtr {
        dest,
        base,
        field_index,
        ..
      } => {
        let b = self.format_operand(func, base);
        let base_ty = self.operand_type(func, base);
        let is_pointer = base_ty
          .as_ref()
          .is_some_and(|ty| matches!(self.types.get(ty), Type::Pointer { .. } | Type::Reference { .. }));

        if let Some(base_ty) = base_ty
          && self.resolve_droppable_record(base_ty).is_some()
        {
          let access = if is_pointer {
            format!("({})->", b)
          } else {
            format!("({}).", b)
          };
          writeln!(
            self.output,
            "if ({}__ignis_drop_state) {{ fprintf(stderr, \"panic: use of dropped value\\n\"); exit(101); }}",
            access
          )
          .unwrap();
          write!(self.output, "    ").unwrap();
        }

        // If the base is a u8* env pointer in a thunk, cast to the env struct type.
        let env_cast_base = self
          .current_fn_id
          .and_then(|fn_id| self.closure_env_names.get(&fn_id).cloned());

        let is_env_ptr = is_pointer && base_ty.is_some_and(|ty| {
          matches!(self.types.get(&ty), Type::Pointer { inner, .. } if matches!(self.types.get(inner), Type::U8))
        });

        if is_pointer {
          if is_env_ptr && let Some(env_name) = &env_cast_base {
            writeln!(
              self.output,
              "t{} = &(((struct {}*){}))->field_{};",
              dest.index(),
              env_name,
              b,
              field_index
            )
            .unwrap();
          } else {
            writeln!(self.output, "t{} = &(({})->field_{});", dest.index(), b, field_index).unwrap();
          }
        } else {
          writeln!(self.output, "t{} = &(({}).field_{});", dest.index(), b, field_index).unwrap();
        }
      },
      Instr::InitRecord {
        dest_ptr,
        fields,
        record_type,
      } => {
        let p = self.format_operand(func, dest_ptr);

        writeln!(self.output, "__builtin_memset((void*){}, 0, sizeof(*{}));", p, p).unwrap();

        // Resolve field types for format_operand_coerced.
        let field_types: Vec<(u32, TypeId)> = match self.types.get(record_type) {
          Type::Record(def_id) => {
            let def = self.defs.get(def_id);
            match &def.kind {
              DefinitionKind::Record(rd) => rd.fields.iter().map(|f| (f.index, f.type_id)).collect(),
              _ => Vec::new(),
            }
          },
          _ => Vec::new(),
        };

        for (field_idx, field_value) in fields {
          if field_idx > &0 {
            write!(self.output, "    ").unwrap();
          }
          let field_ty = field_types.iter().find(|(idx, _)| idx == field_idx).map(|(_, ty)| *ty);

          let v = if let Some(ft) = field_ty {
            self.format_operand_coerced(func, field_value, ft)
          } else {
            self.format_operand(func, field_value)
          };
          writeln!(self.output, "({})->field_{} = {};", p, field_idx, v).unwrap();
        }

        if self.resolve_droppable_record(*record_type).is_some() {
          if !fields.is_empty() {
            write!(self.output, "    ").unwrap();
          }
          writeln!(self.output, "({})->__ignis_drop_state = 0;", p).unwrap();
        }
      },
      Instr::InitEnumVariant {
        dest_ptr,
        variant_tag,
        payload,
        enum_type,
      } => {
        let p = self.format_operand(func, dest_ptr);

        writeln!(self.output, "__builtin_memset((void*){}, 0, sizeof(*{}));", p, p).unwrap();

        // Resolve payload types for format_operand_coerced.
        let payload_types: Vec<TypeId> = match self.types.get(enum_type) {
          Type::Enum(def_id) => {
            let def = self.defs.get(def_id);
            match &def.kind {
              DefinitionKind::Enum(ed) => {
                if let Some(variant) = ed.variants.iter().find(|v| v.tag_value == *variant_tag) {
                  variant
                    .payload
                    .iter()
                    .copied()
                    .filter(|payload_ty| !matches!(self.types.get(payload_ty), Type::Void))
                    .collect()
                } else {
                  Vec::new()
                }
              },
              _ => Vec::new(),
            }
          },
          _ => Vec::new(),
        };

        // Set the tag
        writeln!(self.output, "({})->tag = {};", p, variant_tag).unwrap();

        // Set payload fields
        for (i, payload_value) in payload.iter().enumerate() {
          write!(self.output, "    ").unwrap();
          let v = if let Some(&pt) = payload_types.get(i) {
            self.format_operand_coerced(func, payload_value, pt)
          } else {
            self.format_operand(func, payload_value)
          };
          writeln!(self.output, "({})->payload.variant_{}.field_{} = {};", p, variant_tag, i, v).unwrap();
        }

        if self.resolve_droppable_record(*enum_type).is_some() {
          write!(self.output, "    ").unwrap();
          writeln!(self.output, "({})->__ignis_drop_state = 0;", p).unwrap();
        }
      },
      Instr::EnumGetTag { dest, source } => {
        let s = self.format_operand(func, source);
        writeln!(self.output, "t{} = {}.tag;", dest.index(), s).unwrap();
      },
      Instr::EnumGetPayloadField {
        dest,
        source,
        variant_tag,
        field_index,
      } => {
        let s = self.format_operand(func, source);
        writeln!(
          self.output,
          "t{} = {}.payload.variant_{}.field_{};",
          dest.index(),
          s,
          variant_tag,
          field_index
        )
        .unwrap();
      },
      Instr::Trap { .. } => {
        writeln!(self.output, "__builtin_trap();").unwrap();
      },
      Instr::PanicMessage { message, .. } => {
        writeln!(
          self.output,
          "fprintf(stderr, \"panic: %s\\n\", \"{}\");",
          Self::escape_string(message)
        )
        .unwrap();
        writeln!(self.output, "exit(101);").unwrap();
      },
      Instr::DropInPlace { ptr, ty } => {
        if self.types.needs_drop_with_defs(ty, self.defs) {
          let p = self.format_operand(func, ptr);
          let c_type = self.format_type(*ty);
          writeln!(self.output, "{{ {}* __dip = ({}*){}; (void)__dip;", c_type, c_type, p).unwrap();
          write!(self.output, "    ").unwrap();
          self.emit_field_drops("(*__dip)", *ty);
          writeln!(self.output, "}}").unwrap();
        } else {
          let p = self.format_operand(func, ptr);
          writeln!(self.output, "(void){}; /* drop_in_place: no-op */", p).unwrap();
        }
      },

      Instr::MarkMoved { ptr, ty } => {
        if self.types.needs_drop_with_defs(ty, self.defs) {
          let p = self.format_operand(func, ptr);
          let c_type = self.format_type(*ty);
          writeln!(self.output, "{{ {}* __moved = ({}*){}; (void)__moved;", c_type, c_type, p).unwrap();
          write!(self.output, "    ").unwrap();
          self.emit_uninitialized_drop_state("(*__moved)", *ty);
          writeln!(self.output, "}}").unwrap();
        } else {
          let p = self.format_operand(func, ptr);
          writeln!(self.output, "(void){}; /* mark_moved: no-op */", p).unwrap();
        }
      },

      Instr::DropGlue { dest, ty } => {
        let glue_name = self.drop_glue_name(*ty);
        // Cast to void* — drop glue functions are raw C function pointers passed
        // through opaque void* parameters to extern runtime functions (e.g.
        // ignis_rc_alloc). The runtime internally casts back to IgnisDropFn.
        writeln!(self.output, "t{} = (void*){};", dest.index(), glue_name).unwrap();
      },

      Instr::MakeClosure {
        dest,
        thunk,
        drop_fn,
        captures,
        closure_type,
        heap_allocate,
      } => {
        let env_name = self
          .closure_env_names
          .get(thunk)
          .cloned()
          .unwrap_or_else(|| format!("__closure_env_{}", thunk.index()));
        let closure_ty = self.format_type(*closure_type);
        let thunk_name = self.def_name(*thunk);

        let env_ptr_expr = if *heap_allocate {
          writeln!(
            self.output,
            "struct {}* __envp_t{} = (struct {}*)malloc(sizeof(struct {}));",
            env_name,
            dest.index(),
            env_name,
            env_name,
          )
          .unwrap();

          for (i, cap_op) in captures.iter().enumerate() {
            let v = self.format_operand(func, cap_op);
            write!(self.output, "    ").unwrap();
            writeln!(self.output, "__envp_t{}->field_{} = {};", dest.index(), i, v).unwrap();
          }

          format!("(u8*)__envp_t{}", dest.index())
        } else {
          writeln!(self.output, "struct {} __env_t{};", env_name, dest.index()).unwrap();

          for (i, cap_op) in captures.iter().enumerate() {
            let v = self.format_operand(func, cap_op);
            write!(self.output, "    ").unwrap();
            writeln!(self.output, "__env_t{}.field_{} = {};", dest.index(), i, v).unwrap();
          }

          format!("(u8*)&__env_t{}", dest.index())
        };

        let call_cast = if let Type::Function { params, ret, .. } = self.types.get(closure_type) {
          let ret_str = self.format_type(*ret);
          let mut param_strs = vec!["u8*".to_string()];
          for &p in params {
            param_strs.push(self.format_type(p));
          }
          format!("({}(*)({}))", ret_str, param_strs.join(", "))
        } else {
          String::new()
        };

        let drop_expr = match drop_fn {
          Some(drop_id) => format!("(void(*)(u8*)){}", self.def_name(*drop_id)),
          None => "NULL".to_string(),
        };

        write!(self.output, "    ").unwrap();
        writeln!(
          self.output,
          "t{} = ({}){{ {}{}, {}, {} }};",
          dest.index(),
          closure_ty,
          call_cast,
          thunk_name,
          drop_expr,
          env_ptr_expr,
        )
        .unwrap();
      },

      Instr::CallClosure {
        dest, closure, args, ..
      } => {
        let c = self.format_operand(func, closure);
        let arg_strs: Vec<String> = args.iter().map(|a| self.format_operand(func, a)).collect();

        let mut all_args = vec![format!("{}.env", c)];
        all_args.extend(arg_strs);

        if let Some(d) = dest {
          writeln!(self.output, "t{} = {}.call({});", d.index(), c, all_args.join(", ")).unwrap();
        } else {
          writeln!(self.output, "{}.call({});", c, all_args.join(", ")).unwrap();
        }
      },

      Instr::DropClosure {
        closure,
        heap_allocated,
        ..
      } => {
        let c = self.format_operand(func, closure);
        writeln!(self.output, "if ({}.drop_fn) {}.drop_fn({}.env);", c, c, c).unwrap();

        if *heap_allocated {
          write!(self.output, "    ").unwrap();
          writeln!(self.output, "if ({}.env) free({}.env);", c, c).unwrap();
        }
      },
    }
  }

  fn emit_terminator(
    &mut self,
    func: &FunctionLir,
    term: &Terminator,
  ) {
    match term {
      Terminator::Goto(target) => {
        let label = &func.blocks.get(target).label;
        writeln!(self.output, "goto {};", label).unwrap();
      },
      Terminator::Branch {
        condition,
        then_block,
        else_block,
      } => {
        let c = self.format_operand(func, condition);
        let then_label = &func.blocks.get(then_block).label;
        let else_label = &func.blocks.get(else_block).label;

        writeln!(self.output, "if ({}) goto {}; else goto {};", c, then_label, else_label).unwrap();
      },
      Terminator::Return(value) => {
        if let Some(v) = value {
          let val = self.format_operand(func, v);
          writeln!(self.output, "return {};", val).unwrap();
        } else {
          writeln!(self.output, "return;").unwrap();
        }
      },
      Terminator::Unreachable => {
        writeln!(self.output, "__builtin_unreachable();").unwrap();
      },
    }
  }

  fn format_operand(
    &self,
    func: &FunctionLir,
    op: &Operand,
  ) -> String {
    match op {
      Operand::Temp(t) => {
        let idx = t.index() as usize;
        // First N temps are function parameters
        if idx < func.params.len() {
          self.def_name(func.params[idx])
        } else {
          format!("t{}", idx)
        }
      },
      Operand::Local(l) => format!("l{}", l.index()),
      Operand::Const(c) => self.format_const(c),
      Operand::FuncRef(def) => self.def_name(*def),
      Operand::GlobalRef(def) => self.def_name(*def),
    }
  }

  /// Placeholder: will apply implicit coercions (e.g. str -> String) when implemented.
  fn format_operand_coerced(
    &self,
    func: &FunctionLir,
    op: &Operand,
    _target_ty: TypeId,
  ) -> String {
    self.format_operand(func, op)
  }

  fn operand_type(
    &self,
    func: &FunctionLir,
    op: &Operand,
  ) -> Option<TypeId> {
    match op {
      Operand::Temp(t) => Some(func.temp_type(*t)),
      Operand::Local(l) => Some(func.locals.get(l).ty),
      Operand::Const(c) => Some(c.type_id()),
      Operand::FuncRef(_) | Operand::GlobalRef(_) => None,
    }
  }

  fn type_contains_infer(
    &self,
    ty: TypeId,
  ) -> bool {
    match self.types.get(&ty) {
      Type::Infer => true,
      Type::Reference { inner, .. } | Type::Pointer { inner, .. } => self.type_contains_infer(*inner),
      _ => false,
    }
  }

  fn operand_contains_infer(
    &self,
    func: &FunctionLir,
    op: &Operand,
  ) -> bool {
    self.operand_type(func, op).is_some_and(|t| self.type_contains_infer(t))
  }

  fn format_const(
    &self,
    c: &ConstValue,
  ) -> String {
    match c {
      ConstValue::Int(v, ty) => {
        let suffix = match self.types.get(ty) {
          Type::I64 => "LL",
          _ => "",
        };
        format!("{}{}", v, suffix)
      },
      ConstValue::UInt(v, ty) => {
        let suffix = match self.types.get(ty) {
          Type::U64 => "ULL",
          Type::U32 => "U",
          _ => "U",
        };
        format!("{}{}", v, suffix)
      },
      ConstValue::Float(v, ty) => {
        let suffix = match self.types.get(ty) {
          Type::F32 => "f",
          _ => "",
        };
        let s = v.to_string();
        // C requires decimal point: 0.0f not 0f
        if s.contains('.') || s.contains('e') || s.contains('E') {
          format!("{}{}", s, suffix)
        } else {
          format!("{}.0{}", s, suffix)
        }
      },
      ConstValue::Bool(v, _) => format!("{}", v),
      ConstValue::Char(v, _) => format!("((ignis_char_t){})", *v),
      ConstValue::String(v, _ty) => {
        format!("\"{}\"", Self::escape_string(v))
      },
      ConstValue::Atom(id, _) => format!("{}U", id),
      ConstValue::Null(ty) => format!("({})NULL", self.format_type(*ty)),
      ConstValue::Undef(_) => "/* undef */ 0".to_string(),
    }
  }

  fn escape_string(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    for c in s.chars() {
      match c {
        '\n' => result.push_str("\\n"),
        '\r' => result.push_str("\\r"),
        '\t' => result.push_str("\\t"),
        '\0' => result.push_str("\\0"),
        '\\' => result.push_str("\\\\"),
        '"' => result.push_str("\\\""),
        c if c.is_ascii_graphic() || c == ' ' => result.push(c),
        c => result.push_str(&format!("\\x{:02x}", c as u32)),
      }
    }
    result
  }

  fn emit_pointer_binop(
    &mut self,
    func: &FunctionLir,
    dest: TempId,
    op: &BinaryOperation,
    left: &Operand,
    right: &Operand,
  ) -> bool {
    let left_ty = self.operand_type(func, left);
    let right_ty = self.operand_type(func, right);
    let dest_ty = func.temp_type(dest);

    let is_left_ptr = left_ty.is_some_and(|ty| matches!(self.types.get(&ty), Type::Pointer { .. }));
    let is_right_ptr = right_ty.is_some_and(|ty| matches!(self.types.get(&ty), Type::Pointer { .. }));
    let is_right_i64 = right_ty.is_some_and(|ty| matches!(self.types.get(&ty), Type::I64));

    let l = self.format_operand(func, left);
    let r = self.format_operand(func, right);

    match op {
      BinaryOperation::Add | BinaryOperation::Sub if is_left_ptr && is_right_i64 => {
        let op_str = self.format_binop(op);
        writeln!(self.output, "t{} = {} {} {};", dest.index(), l, op_str, r).unwrap();
        true
      },
      BinaryOperation::Sub if is_left_ptr && is_right_ptr && matches!(self.types.get(&dest_ty), Type::I64) => {
        writeln!(self.output, "t{} = (i64)({} - {});", dest.index(), l, r).unwrap();
        true
      },
      _ => false,
    }
  }

  /// Emit comparison for unit enums by comparing their tags.
  /// Returns true if this was an enum comparison, false otherwise.
  fn emit_enum_comparison(
    &mut self,
    func: &FunctionLir,
    dest: TempId,
    op: &BinaryOperation,
    left: &Operand,
    right: &Operand,
  ) -> bool {
    let Some(left_ty) = self.operand_type(func, left) else {
      return false;
    };
    let Some(right_ty) = self.operand_type(func, right) else {
      return false;
    };

    if !self.is_unit_enum_type(left_ty) || !self.is_unit_enum_type(right_ty) {
      return false;
    }

    let l = self.format_operand(func, left);
    let r = self.format_operand(func, right);
    let op_str = self.format_binop(op);

    writeln!(self.output, "t{} = {}.tag {} {}.tag;", dest.index(), l, op_str, r).unwrap();
    true
  }

  /// Returns true if the type is an enum where all variants have no payload.
  fn is_unit_enum_type(
    &self,
    ty: TypeId,
  ) -> bool {
    match self.types.get(&ty) {
      Type::Enum(def_id) => {
        let def = self.defs.get(def_id);
        if let DefinitionKind::Enum(ed) = &def.kind {
          return ed.variants.iter().all(|v| v.payload.is_empty());
        }
        false
      },
      _ => false,
    }
  }

  fn format_binop(
    &self,
    op: &BinaryOperation,
  ) -> &'static str {
    match op {
      BinaryOperation::Add => "+",
      BinaryOperation::Sub => "-",
      BinaryOperation::Mul => "*",
      BinaryOperation::Div => "/",
      BinaryOperation::Mod => "%",
      BinaryOperation::Pow => unreachable!(),
      BinaryOperation::And => "&&",
      BinaryOperation::Or => "||",
      BinaryOperation::Equal => "==",
      BinaryOperation::NotEqual => "!=",
      BinaryOperation::LessThan => "<",
      BinaryOperation::LessEqual => "<=",
      BinaryOperation::GreaterThan => ">",
      BinaryOperation::GreaterEqual => ">=",
      BinaryOperation::BitAnd => "&",
      BinaryOperation::BitOr => "|",
      BinaryOperation::BitXor => "^",
      BinaryOperation::BitShiftLeft => "<<",
      BinaryOperation::BitShiftRight => ">>",
    }
  }

  fn format_unaryop(
    &self,
    op: &UnaryOperation,
  ) -> &'static str {
    match op {
      UnaryOperation::Not => "!",
      UnaryOperation::Neg => "-",
      UnaryOperation::BitNot => "~",
    }
  }

  /// Get C expression for the maximum value of a numeric type.
  fn type_max_value(
    &self,
    ty: TypeId,
  ) -> String {
    match self.types.get(&ty) {
      Type::I8 => "INT8_MAX".to_string(),
      Type::I16 => "INT16_MAX".to_string(),
      Type::I32 => "INT32_MAX".to_string(),
      Type::I64 => "INT64_MAX".to_string(),
      Type::U8 => "UINT8_MAX".to_string(),
      Type::U16 => "UINT16_MAX".to_string(),
      Type::U32 => "UINT32_MAX".to_string(),
      Type::U64 => "UINT64_MAX".to_string(),
      Type::F32 => "FLT_MAX".to_string(),
      Type::F64 => "DBL_MAX".to_string(),
      _ => panic!("ICE: maxOf called with non-numeric type"),
    }
  }

  /// Get C expression for the minimum value of a numeric type.
  fn type_min_value(
    &self,
    ty: TypeId,
  ) -> String {
    match self.types.get(&ty) {
      Type::I8 => "INT8_MIN".to_string(),
      Type::I16 => "INT16_MIN".to_string(),
      Type::I32 => "INT32_MIN".to_string(),
      Type::I64 => "INT64_MIN".to_string(),
      Type::U8 => "((u8)0)".to_string(),
      Type::U16 => "((u16)0)".to_string(),
      Type::U32 => "((u32)0)".to_string(),
      Type::U64 => "((u64)0)".to_string(),
      Type::F32 => "(-FLT_MAX)".to_string(),
      Type::F64 => "(-DBL_MAX)".to_string(),
      _ => panic!("ICE: minOf called with non-numeric type"),
    }
  }

  fn format_type(
    &self,
    ty: TypeId,
  ) -> String {
    // Use runtime type aliases from types.h for compatibility
    match self.types.get(&ty) {
      Type::I8 => "i8".to_string(),
      Type::I16 => "i16".to_string(),
      Type::I32 => "i32".to_string(),
      Type::I64 => "i64".to_string(),
      Type::U8 => "u8".to_string(),
      Type::U16 => "u16".to_string(),
      Type::U32 => "u32".to_string(),
      Type::U64 => "u64".to_string(),
      Type::F32 => "f32".to_string(),
      Type::F64 => "f64".to_string(),
      Type::Boolean => "boolean".to_string(),
      Type::Char => "ignis_char_t".to_string(),
      Type::Str => "const char*".to_string(),
      Type::Atom => "ignis_atom_t".to_string(),
      Type::Void => "void".to_string(),
      Type::Never => "void".to_string(),
      Type::NullPtr => "void*".to_string(),
      Type::Error => "/* error */ void*".to_string(),
      Type::Pointer { inner, .. } => format!("{}*", self.format_type(*inner)),
      Type::Reference { inner, .. } => format!("{}*", self.format_type(*inner)),
      Type::Slice { .. } => format!("struct {}", self.slice_struct_name(ty)),
      Type::FixedArray { element, .. } => {
        format!("{}*", self.format_type(*element))
      },
      Type::Tuple(_) => "/* tuple */ void*".to_string(),
      Type::Function { .. } => {
        if let Some(name) = self.closure_struct_names.get(&ty) {
          format!("struct {}", name)
        } else {
          "/* fn */ void*".to_string()
        }
      },
      Type::Record(def_id) => {
        // Use build_mangled_name for consistency with type_struct_name
        let name = self.build_mangled_name(*def_id);
        format!("struct {}", name)
      },
      Type::Enum(def_id) => {
        // Use build_mangled_name for consistency with type_struct_name
        let name = self.build_mangled_name(*def_id);
        format!("struct {}", name)
      },
      Type::Param { .. } => {
        // Type::Param should never reach codegen (Invariant A)
        panic!("ICE: Type::Param reached C codegen - monomorphization failed")
      },
      Type::Instance { .. } => {
        // Type::Instance should never reach codegen (Invariant D)
        panic!("ICE: Type::Instance reached C codegen - monomorphization failed")
      },
      Type::Infer => {
        panic!("ICE: Type::Infer reached C codegen after implicit type removal")
      },
      Type::InferVar(_) => {
        panic!("ICE: Type::InferVar reached C codegen - zonking failed")
      },
      Type::Unknown => {
        panic!("ICE: Type::Unknown reached C codegen")
      },
    }
  }

  fn sizeof_type(
    &self,
    ty: TypeId,
  ) -> String {
    let ty_str = self.format_type(ty);
    format!("sizeof({})", ty_str)
  }

  /// Find the `drop` instance method on a record definition.
  ///
  /// For non-generic records, the method is looked up via `instance_methods`.
  /// For monomorphized records (where `instance_methods` is intentionally empty
  /// post-mono), we fall back to scanning the LIR program's compiled functions
  /// for a `Method` whose `owner_type` matches this record.
  fn find_drop_method(
    &self,
    def_id: DefinitionId,
  ) -> Option<DefinitionId> {
    let def = self.defs.get(&def_id);

    let (instance_methods, has_drop) = match &def.kind {
      DefinitionKind::Record(rd) => (&rd.instance_methods, rd.lang_traits.drop),
      DefinitionKind::Enum(ed) => (&ed.instance_methods, ed.lang_traits.drop),
      _ => return None,
    };

    for (sym_id, entry) in instance_methods {
      if self.symbols.get(sym_id) == "drop" {
        return match entry {
          ignis_type::definition::SymbolEntry::Single(id) => Some(*id),
          ignis_type::definition::SymbolEntry::Overload(ids) => ids.first().copied(),
        };
      }
    }

    if has_drop && instance_methods.is_empty() {
      for (method_def_id, method_def) in self.defs.iter() {
        if let DefinitionKind::Method(md) = &method_def.kind
          && md.owner_type == def_id
        {
          let method_name = self.symbols.get(&method_def.name);
          if method_name == "drop" || method_name.ends_with("__drop") {
            return Some(method_def_id);
          }
        }
      }
    }

    None
  }

  fn find_named_instance_method(
    &self,
    def_id: DefinitionId,
    method_name: &str,
  ) -> Option<DefinitionId> {
    let def = self.defs.get(&def_id);

    let instance_methods = match &def.kind {
      DefinitionKind::Record(rd) => &rd.instance_methods,
      DefinitionKind::Enum(ed) => &ed.instance_methods,
      _ => return None,
    };

    for (sym_id, entry) in instance_methods {
      if self.symbols.get(sym_id) == method_name {
        return match entry {
          ignis_type::definition::SymbolEntry::Single(id) => Some(*id),
          ignis_type::definition::SymbolEntry::Overload(ids) => ids.first().copied(),
        };
      }
    }

    for (method_def_id, method_def) in self.defs.iter() {
      if let DefinitionKind::Method(md) = &method_def.kind
        && md.owner_type == def_id
      {
        let candidate_name = self.symbols.get(&method_def.name);
        if candidate_name == method_name || candidate_name.ends_with(&format!("__{}", method_name)) {
          return Some(method_def_id);
        }
      }
    }

    None
  }

  fn emit_builtin_hash(
    &mut self,
    func: &FunctionLir,
    value: &Operand,
    hasher: &Operand,
    ty: TypeId,
  ) {
    let value_expr = self.format_operand(func, value);
    let hasher_expr = self.format_operand(func, hasher);

    match self.types.get(&ty).clone() {
      Type::Boolean => {
        self.emit_hasher_write_call("writeBoolean", &hasher_expr, &self.format_builtin_ref_deref(&value_expr, ty))
      },
      Type::Char => {
        self.emit_hasher_write_call("writeChar", &hasher_expr, &self.format_builtin_ref_deref(&value_expr, ty))
      },
      Type::I8 => self.emit_hasher_write_call("writeI8", &hasher_expr, &self.format_builtin_ref_deref(&value_expr, ty)),
      Type::I16 => {
        self.emit_hasher_write_call("writeI16", &hasher_expr, &self.format_builtin_ref_deref(&value_expr, ty))
      },
      Type::I32 => {
        self.emit_hasher_write_call("writeI32", &hasher_expr, &self.format_builtin_ref_deref(&value_expr, ty))
      },
      Type::I64 => {
        self.emit_hasher_write_call("writeI64", &hasher_expr, &self.format_builtin_ref_deref(&value_expr, ty))
      },
      Type::U8 => {
        self.emit_hasher_write_call("writeByte", &hasher_expr, &self.format_builtin_ref_deref(&value_expr, ty))
      },
      Type::U16 => {
        self.emit_hasher_write_call("writeU16", &hasher_expr, &self.format_builtin_ref_deref(&value_expr, ty))
      },
      Type::U32 => {
        self.emit_hasher_write_call("writeU32", &hasher_expr, &self.format_builtin_ref_deref(&value_expr, ty))
      },
      Type::U64 => {
        self.emit_hasher_write_call("writeU64", &hasher_expr, &self.format_builtin_ref_deref(&value_expr, ty))
      },
      Type::Str => {
        self.emit_hasher_write_call("writeStr", &hasher_expr, &self.format_builtin_ref_deref(&value_expr, ty))
      },
      Type::Record(def_id) | Type::Enum(def_id) => {
        let Some(method_def_id) = self.find_named_instance_method(def_id, "hash") else {
          panic!("ICE: missing hash method for builtin hash on {:?}", self.types.get(&ty));
        };
        let method_name = self.def_name(method_def_id);
        writeln!(self.output, "{}({}, {});", method_name, value_expr, hasher_expr).unwrap();
      },
      other => panic!("ICE: builtin hash unsupported for {:?}", other),
    }
  }

  fn emit_builtin_eq(
    &mut self,
    func: &FunctionLir,
    dest: TempId,
    left: &Operand,
    right: &Operand,
    ty: TypeId,
    kind: BuiltinEqKind,
  ) {
    let left_expr = self.format_operand(func, left);
    let right_expr = self.format_operand(func, right);

    match kind {
      BuiltinEqKind::Primitive
        if matches!(
          self.types.get(&ty),
          Type::Boolean
            | Type::Char
            | Type::I8
            | Type::I16
            | Type::I32
            | Type::I64
            | Type::U8
            | Type::U16
            | Type::U32
            | Type::U64
        ) =>
      {
        let left_value = self.format_builtin_ref_deref(&left_expr, ty);
        let right_value = self.format_builtin_ref_deref(&right_expr, ty);
        writeln!(self.output, "t{} = {} == {};", dest.index(), left_value, right_value).unwrap();
      },
      BuiltinEqKind::Str if matches!(self.types.get(&ty), Type::Str) => {
        let left_value = self.format_builtin_ref_deref(&left_expr, ty);
        let right_value = self.format_builtin_ref_deref(&right_expr, ty);
        writeln!(self.output, "t{} = strcmp({}, {}) == 0;", dest.index(), left_value, right_value).unwrap();
      },
      BuiltinEqKind::Method(method_def_id)
        if matches!(self.defs.get(&method_def_id).kind, DefinitionKind::Method(_)) =>
      {
        let method_name = self.def_name(method_def_id);
        writeln!(
          self.output,
          "t{} = {}({}, {});",
          dest.index(),
          method_name,
          left_expr,
          right_expr
        )
        .unwrap();
      },
      BuiltinEqKind::Primitive => {
        self.emit_invalid_builtin_eq(
          dest,
          &format!("primitive eq kind on unsupported type {:?}", self.types.get(&ty)),
        );
      },
      BuiltinEqKind::Str => {
        self.emit_invalid_builtin_eq(dest, &format!("str eq kind on unsupported type {:?}", self.types.get(&ty)));
      },
      BuiltinEqKind::Method(method_def_id) => {
        self.emit_invalid_builtin_eq(dest, &format!("eq method {:?} missing from lowered defs", method_def_id));
      },
    }
  }

  fn emit_invalid_builtin_eq(
    &mut self,
    dest: TempId,
    message: &str,
  ) {
    eprintln!("ICE: {}", message);
    writeln!(self.output, "/* {} */", message).unwrap();
    writeln!(self.output, "t{} = false;", dest.index()).unwrap();
  }

  fn emit_hasher_write_call(
    &mut self,
    method_name: &str,
    hasher_expr: &str,
    value_expr: &str,
  ) {
    let hasher_def_id = self
      .find_hasher_def_id()
      .expect("ICE: missing std::hash::Hasher definition");
    let method_def_id = self
      .find_named_instance_method(hasher_def_id, method_name)
      .unwrap_or_else(|| panic!("ICE: missing Hasher::{} method", method_name));
    let method = self.def_name(method_def_id);
    writeln!(self.output, "{}({}, {});", method, hasher_expr, value_expr).unwrap();
  }

  fn find_hasher_def_id(&self) -> Option<DefinitionId> {
    for (def_id, def) in self.defs.iter() {
      if matches!(def.kind, DefinitionKind::Record(_)) && self.symbols.get(&def.name) == "Hasher" {
        return Some(def_id);
      }
    }
    None
  }

  fn format_builtin_ref_deref(
    &self,
    expr: &str,
    ty: TypeId,
  ) -> String {
    let c_type = self.format_type(ty);
    format!("*({}*)({})", c_type, expr)
  }

  /// Recursively emit drop calls for each droppable field of a value.
  ///
  /// For types with an explicit `drop` method, calls that method.
  /// For types without one but with droppable fields, recurses into each field.
  fn emit_field_drops(
    &mut self,
    expr: &str,
    ty: TypeId,
  ) {
    match self.types.get(&ty).clone() {
      Type::Record(def_id) => {
        let has_drop_trait = self.record_has_drop_trait(def_id);

        if let Some(method_def_id) = self.find_drop_method(def_id) {
          if has_drop_trait {
            writeln!(self.output, "if (!({}).__ignis_drop_state) {{", expr).unwrap();
            write!(self.output, "    ").unwrap();
          }

          let name = self.def_name(method_def_id);
          writeln!(self.output, "{}(&{});", name, expr).unwrap();

          if has_drop_trait {
            write!(self.output, "    ").unwrap();
            writeln!(self.output, "({}).__ignis_drop_state = 1;", expr).unwrap();
            writeln!(self.output, "}}").unwrap();
          }
        } else {
          let field_info: Vec<(u32, TypeId)> = {
            let def = self.defs.get(&def_id);
            match &def.kind {
              DefinitionKind::Record(rd) => rd.fields.iter().map(|f| (f.index, f.type_id)).collect(),
              _ => vec![],
            }
          };

          for (index, field_ty) in field_info {
            if self.types.needs_drop_with_defs(&field_ty, self.defs) {
              self.emit_field_drops(&format!("{}.field_{}", expr, index), field_ty);
            }
          }
        }
      },

      Type::Enum(def_id) => {
        let has_drop_trait = self.record_has_drop_trait(def_id);

        if let Some(method_def_id) = self.find_drop_method(def_id) {
          if has_drop_trait {
            writeln!(self.output, "if (!({}).__ignis_drop_state) {{", expr).unwrap();
            write!(self.output, "    ").unwrap();
          }

          let name = self.def_name(method_def_id);
          writeln!(self.output, "{}(&{});", name, expr).unwrap();

          if has_drop_trait {
            write!(self.output, "    ").unwrap();
            writeln!(self.output, "({}).__ignis_drop_state = 1;", expr).unwrap();
            writeln!(self.output, "}}").unwrap();
          }
        } else {
          self.emit_enum_variant_drops(expr, def_id);
        }
      },

      _ => {},
    }
  }

  /// Emit drop calls for an enum's variant payloads via switch-on-tag.
  ///
  /// Generates a `switch` statement that dispatches on the tag field, then
  /// recursively drops each droppable payload field within the active variant.
  fn emit_enum_variant_drops(
    &mut self,
    expr: &str,
    def_id: DefinitionId,
  ) {
    let variants: Vec<_> = {
      let def = self.defs.get(&def_id);
      match &def.kind {
        DefinitionKind::Enum(ed) => ed.variants.clone(),
        _ => return,
      }
    };

    let has_droppable_variant = variants.iter().any(|v| {
      v.payload
        .iter()
        .any(|payload_ty| self.types.needs_drop_with_defs(payload_ty, self.defs))
    });

    if !has_droppable_variant {
      return;
    }

    writeln!(self.output, "switch (({}).tag) {{", expr).unwrap();

    for variant in &variants {
      let droppable_fields: Vec<(usize, TypeId)> = variant
        .payload
        .iter()
        .enumerate()
        .filter(|(_, payload_ty)| self.types.needs_drop_with_defs(payload_ty, self.defs))
        .map(|(i, ty)| (i, *ty))
        .collect();

      if droppable_fields.is_empty() {
        continue;
      }

      writeln!(self.output, "case {}:", variant.tag_value).unwrap();

      for (field_idx, field_ty) in droppable_fields {
        let field_expr = format!("{}.payload.variant_{}.field_{}", expr, variant.tag_value, field_idx);
        write!(self.output, "    ").unwrap();
        self.emit_field_drops(&field_expr, field_ty);
      }

      writeln!(self.output, "    break;").unwrap();
    }

    writeln!(self.output, "}}").unwrap();
  }

  fn def_name(
    &self,
    def_id: DefinitionId,
  ) -> String {
    let def = self.defs.get(&def_id);
    let raw_name = self.symbols.get(&def.name).to_string();

    // Check for @externName override
    if let DefinitionKind::Function(f) = &def.kind {
      for attr in &f.attrs {
        if let FunctionAttr::ExternName(name) = attr {
          return name.clone();
        }
      }
    }
    if let DefinitionKind::Method(m) = &def.kind {
      for attr in &m.attrs {
        if let FunctionAttr::ExternName(name) = attr {
          return name.clone();
        }
      }
    }

    let is_extern = match &def.kind {
      DefinitionKind::Function(f) => f.is_extern,
      DefinitionKind::Constant(c) => c.value.is_none(), // extern const has no value
      _ => false,
    };

    if is_extern {
      return raw_name;
    }

    if Some(def_id) == self.program.entry_point && def.owner_namespace.is_none() && raw_name == "main" {
      return USER_MAIN_SYMBOL.to_string();
    }

    self.build_mangled_name(def_id)
  }

  fn is_internal_closure_helper(
    &self,
    def_id: DefinitionId,
  ) -> bool {
    let raw_name = self.symbols.get(&self.defs.get(&def_id).name);
    raw_name.starts_with("__closure_thunk_") || raw_name.starts_with("__closure_drop_")
  }

  /// Emit a receiver drop-state guard for non-static method calls.
  fn emit_method_receiver_drop_guard(
    &mut self,
    func: &FunctionLir,
    callee: DefinitionId,
    args: &[Operand],
  ) {
    let callee_def = self.defs.get(&callee);
    let owner_def_id = match &callee_def.kind {
      DefinitionKind::Method(md) if !md.is_static => md.owner_type,
      _ => return,
    };

    if !self.record_has_drop_trait(owner_def_id) {
      return;
    }

    if let Some(recv_op) = args.first() {
      let recv = self.format_operand(func, recv_op);
      writeln!(
        self.output,
        "if (({})->__ignis_drop_state) {{ fprintf(stderr, \"panic: use of dropped value\\n\"); exit(101); }}",
        recv
      )
      .unwrap();
      write!(self.output, "    ").unwrap();
    }
  }

  /// Mark the receiver as dropped after a manual `.drop()` call.
  fn emit_manual_drop_state_update(
    &mut self,
    func: &FunctionLir,
    callee: DefinitionId,
    args: &[Operand],
  ) {
    let callee_def = self.defs.get(&callee);
    let owner_def_id = match &callee_def.kind {
      DefinitionKind::Method(md) if !md.is_static => md.owner_type,
      _ => return,
    };

    if !self.record_has_drop_trait(owner_def_id) {
      return;
    }

    let method_name = self.symbols.get(&callee_def.name);
    if method_name != "drop" && !method_name.ends_with("__drop") {
      return;
    }

    if let Some(recv_op) = args.first() {
      let recv = self.format_operand(func, recv_op);
      write!(self.output, "    ").unwrap();
      writeln!(self.output, "({})->__ignis_drop_state = 1;", recv).unwrap();
    }
  }

  /// Check if a record or enum DefinitionId has `@implements(Drop)`.
  fn record_has_drop_trait(
    &self,
    def_id: DefinitionId,
  ) -> bool {
    let def = self.defs.get(&def_id);
    match &def.kind {
      DefinitionKind::Record(rd) => rd.lang_traits.drop,
      DefinitionKind::Enum(ed) => ed.lang_traits.drop,
      _ => false,
    }
  }

  /// Resolve a type (possibly behind pointer/reference) to a droppable record or enum.
  fn resolve_droppable_record(
    &self,
    ty: TypeId,
  ) -> Option<DefinitionId> {
    match self.types.get(&ty).clone() {
      Type::Record(def_id) | Type::Enum(def_id) => {
        if self.record_has_drop_trait(def_id) {
          Some(def_id)
        } else {
          None
        }
      },
      Type::Pointer { inner, .. } | Type::Reference { inner, .. } => self.resolve_droppable_record(inner),
      _ => None,
    }
  }

  fn escape_ident(name: &str) -> String {
    name.replace('_', "__")
  }

  fn build_mangled_name(
    &self,
    def_id: DefinitionId,
  ) -> String {
    let def = self.defs.get(&def_id);
    let raw_name = self.symbols.get(&def.name).to_string();

    // Mirror build_mangled_name_standalone: every Function gets a param suffix
    // unconditionally so that bare names like `abs` do not clash with libc /
    // runtime symbols. Methods only get the suffix when actually overloaded
    // (they already carry an owner_type prefix and any monomorphization
    // suffix, so adding an unconditional param tail bloats them without
    // disambiguation benefit).
    let needs_param_suffix = match &def.kind {
      DefinitionKind::Function(_) => true,
      DefinitionKind::Method(_) => self.has_overloads(def_id) || self.original_generic_method_has_overloads(def_id),
      _ => false,
    };
    let param_suffix = if needs_param_suffix {
      self.format_param_types_for_mangling(def_id)
    } else {
      String::new()
    };

    // For methods and static fields, include owner type in mangled name to avoid collisions.
    // e.g., User.getName() -> User_getName, Admin.getName() -> Admin_getName
    // e.g., Config::MAX_SIZE -> Config_MAX__SIZE
    // For monomorphized methods, the name already includes the owner type (e.g., Box__i32__get),
    // so we don't add the owner prefix to avoid duplication (e.g., Box__i32_Box__i32__get)
    let owner_type_prefix = match &def.kind {
      DefinitionKind::Method(md) => {
        let owner_def = self.defs.get(&md.owner_type);
        let owner_name = self.symbols.get(&owner_def.name).to_string();
        // Check if the method name already starts with the owner name (monomorphized method)
        if raw_name.starts_with(&owner_name) {
          None
        } else {
          Some(Self::escape_ident(&owner_name))
        }
      },
      DefinitionKind::Constant(cd) => cd.owner_type.as_ref().map(|owner_id| {
        let owner_def = self.defs.get(owner_id);
        Self::escape_ident(self.symbols.get(&owner_def.name))
      }),
      _ => None,
    };

    match def.owner_namespace {
      Some(ns_id) => {
        let ns_path = self.namespaces.full_path(ns_id);
        let mut parts: Vec<String> = ns_path
          .iter()
          .map(|s| Self::escape_ident(self.symbols.get(s)))
          .collect();

        if let Some(type_prefix) = owner_type_prefix {
          parts.push(type_prefix);
        }

        parts.push(Self::escape_ident(&raw_name));
        if !param_suffix.is_empty() {
          parts.push(param_suffix);
        }
        parts.join("_")
      },
      None => {
        let mut name = if let Some(type_prefix) = owner_type_prefix {
          format!("{}_{}", type_prefix, Self::escape_ident(&raw_name))
        } else {
          Self::escape_ident(&raw_name)
        };
        if !param_suffix.is_empty() {
          name = format!("{}_{}", name, param_suffix);
        }
        name
      },
    }
  }

  fn has_overloads(
    &self,
    target_def_id: DefinitionId,
  ) -> bool {
    let target_def = self.defs.get(&target_def_id);
    let target_name = target_def.name;
    let target_ns = target_def.owner_namespace;

    // Only functions and methods can be overloaded
    if !matches!(target_def.kind, DefinitionKind::Function(_) | DefinitionKind::Method(_)) {
      return false;
    }

    let target_owner_type = match &target_def.kind {
      DefinitionKind::Method(md) => Some(md.owner_type),
      _ => None,
    };

    let mut count = 0;
    for (_, def) in self.defs.iter() {
      if def.name == target_name {
        // Check if it's the right kind (Function/Method)
        if !matches!(def.kind, DefinitionKind::Function(_) | DefinitionKind::Method(_)) {
          continue;
        }

        let def_owner_type = match &def.kind {
          DefinitionKind::Method(md) => Some(md.owner_type),
          _ => None,
        };

        if target_owner_type.is_some() {
          // Target is a method: check if other is method of same type
          if target_owner_type == def_owner_type {
            count += 1;
          }
        } else {
          // Target is a function: check if other is function in same namespace
          // AND ensure other is NOT a method (def_owner_type is None)
          if def.owner_namespace == target_ns && def_owner_type.is_none() {
            count += 1;
          }
        }

        if count > 1 {
          return true;
        }
      }
    }
    false
  }

  fn original_generic_method_has_overloads(
    &self,
    target_def_id: DefinitionId,
  ) -> bool {
    let target_def = self.defs.get(&target_def_id);
    let DefinitionKind::Method(target_method) = &target_def.kind else {
      return false;
    };

    let target_owner_def = self.defs.get(&target_method.owner_type);
    let target_owner_name = self.symbols.get(&target_owner_def.name);
    let raw_name = self.symbols.get(&target_def.name);
    let Some(original_method_name) = raw_name.strip_prefix(&format!("{}__", target_owner_name)) else {
      return false;
    };

    let Some(original_owner_name) = target_owner_name.split("__").next() else {
      return false;
    };

    let Some(original_owner_def_id) = self.defs.iter().find_map(|(candidate_id, candidate)| {
      if candidate.owner_module == target_owner_def.owner_module
        && matches!(candidate.kind, DefinitionKind::Record(_) | DefinitionKind::Enum(_))
        && self.symbols.get(&candidate.name) == original_owner_name
      {
        Some(candidate_id)
      } else {
        None
      }
    }) else {
      return false;
    };

    let Some(&method_symbol) = self.symbols.map.get(original_method_name) else {
      return false;
    };

    let overload_count = self
      .defs
      .iter()
      .filter(|(_, candidate)| {
        candidate.name == method_symbol
          && matches!(candidate.kind, DefinitionKind::Method(_))
          && matches!(&candidate.kind, DefinitionKind::Method(method) if method.owner_type == original_owner_def_id)
      })
      .take(2)
      .count();

    overload_count > 1
  }

  fn format_param_types_for_mangling(
    &self,
    def_id: DefinitionId,
  ) -> String {
    match &self.defs.get(&def_id).kind {
      DefinitionKind::Function(fd) => {
        let mut parts: Vec<String> = fd
          .params
          .iter()
          .map(|p| self.format_type_for_mangling(self.defs.type_of(p)))
          .collect();

        parts.push(self.format_type_for_mangling(&fd.return_type));

        parts.join("_")
      },
      DefinitionKind::Method(md) => {
        let start = if md.is_static { 0 } else { 1 };
        let mut parts: Vec<String> = md.params[start..]
          .iter()
          .map(|p| self.format_type_for_mangling(self.defs.type_of(p)))
          .collect();

        parts.push(self.format_type_for_mangling(&md.return_type));

        parts.join("_")
      },
      _ => String::new(),
    }
  }

  fn type_definition_mangling_name(
    &self,
    def_id: DefinitionId,
  ) -> String {
    let def = self.defs.get(&def_id);

    match def.owner_namespace {
      Some(_) => self.build_mangled_name(def_id),
      None => self.symbols.get(&def.name).to_string(),
    }
  }

  fn format_type_for_mangling(
    &self,
    ty: &TypeId,
  ) -> String {
    match self.types.get(ty) {
      Type::I8 => "i8".to_string(),
      Type::I16 => "i16".to_string(),
      Type::I32 => "i32".to_string(),
      Type::I64 => "i64".to_string(),
      Type::U8 => "u8".to_string(),
      Type::U16 => "u16".to_string(),
      Type::U32 => "u32".to_string(),
      Type::U64 => "u64".to_string(),
      Type::F32 => "f32".to_string(),
      Type::F64 => "f64".to_string(),
      Type::Boolean => "bool".to_string(),
      Type::Char => "char".to_string(),
      Type::Str => "str".to_string(),
      Type::Atom => "atom".to_string(),
      Type::Void => "void".to_string(),
      Type::Never => "never".to_string(),
      Type::Pointer { inner, .. } => format!("ptr_{}", self.format_type_for_mangling(inner)),
      Type::Reference { inner, mutable: true } => format!("mutref_{}", self.format_type_for_mangling(inner)),
      Type::Reference { inner, mutable: false } => format!("ref_{}", self.format_type_for_mangling(inner)),
      Type::Slice { element, .. } => format!("slice_{}", self.format_type_for_mangling(element)),
      Type::FixedArray { element, .. } => {
        format!("vec_{}", self.format_type_for_mangling(element))
      },
      Type::Tuple(elems) => {
        let parts: Vec<_> = elems.iter().map(|e| self.format_type_for_mangling(e)).collect();
        format!("tup_{}", parts.join("_"))
      },
      Type::Record(def_id) | Type::Enum(def_id) => self.type_definition_mangling_name(*def_id),
      Type::Instance { generic, args } => {
        let base_name = self.type_definition_mangling_name(*generic);
        if args.is_empty() {
          base_name
        } else {
          let args_str = args
            .iter()
            .map(|a| self.format_type_for_mangling(a))
            .collect::<Vec<_>>()
            .join("_");
          format!("{}_{}", base_name, args_str)
        }
      },
      Type::Infer => {
        panic!("ICE: Type::Infer reached C codegen after implicit type removal")
      },
      _ => "opaque".to_string(),
    }
  }
}

fn should_defer_owner_module_emit_to_entry(
  is_entry_user_module: bool,
  is_monomorphized_generic_def: bool,
  depends_on_user_type: bool,
) -> bool {
  let _ = is_entry_user_module;
  let _ = is_monomorphized_generic_def;
  let _ = depends_on_user_type;

  false
}

fn should_defer_std_module_emit_for_user_specialization(
  is_monomorphized_generic_def: bool,
  depends_on_user_type: bool,
) -> bool {
  is_monomorphized_generic_def && depends_on_user_type
}

/// Legacy: emits everything (all modules combined).
pub fn emit_c(
  program: &LirProgram,
  types: &TypeStore,
  defs: &DefinitionStore,
  namespaces: &NamespaceStore,
  symbols: &SymbolTable,
  headers: &[CHeader],
) -> String {
  emit_c_from_input(EmitInput::new(program, types, defs), namespaces, symbols, headers)
}

pub fn emit_c_from_input(
  input: EmitInput<'_>,
  namespaces: &NamespaceStore,
  symbols: &SymbolTable,
  headers: &[CHeader],
) -> String {
  CEmitter::new(input.program, input.types, input.defs, namespaces, symbols, headers)
    .with_source_map(input.source_map)
    .emit()
}

/// Emit C for user definitions only (excludes std and runtime).
pub fn emit_user_c(
  program: &LirProgram,
  types: &TypeStore,
  defs: &DefinitionStore,
  namespaces: &NamespaceStore,
  symbols: &SymbolTable,
  headers: &[CHeader],
  module_paths: &HashMap<ModuleId, ModulePath>,
) -> String {
  emit_user_c_from_input(EmitInput::new(program, types, defs), namespaces, symbols, headers, module_paths)
}

pub fn emit_user_c_from_input(
  input: EmitInput<'_>,
  namespaces: &NamespaceStore,
  symbols: &SymbolTable,
  headers: &[CHeader],
  module_paths: &HashMap<ModuleId, ModulePath>,
) -> String {
  CEmitter::with_target(
    input.program,
    input.types,
    input.defs,
    namespaces,
    symbols,
    headers,
    EmitTarget::User,
    module_paths,
  )
  .with_source_map(input.source_map)
  .emit()
}

pub fn emit_user_test_harness_from_input(
  input: EmitInput<'_>,
  namespaces: &NamespaceStore,
  symbols: &SymbolTable,
  headers: &[CHeader],
  module_paths: &HashMap<ModuleId, ModulePath>,
  test_harness: &[TestHarnessEntry],
) -> String {
  CEmitter::with_test_harness_target(
    input.program,
    input.types,
    input.defs,
    namespaces,
    symbols,
    headers,
    module_paths,
    test_harness,
  )
  .with_source_map(input.source_map)
  .emit_test_harness_only(test_harness)
}

/// Emit C for a specific std module. Prepends umbrella header if provided.
#[allow(clippy::too_many_arguments)]
pub fn emit_std_module_c(
  module_name: &str,
  program: &LirProgram,
  types: &TypeStore,
  defs: &DefinitionStore,
  namespaces: &NamespaceStore,
  symbols: &SymbolTable,
  headers: &[CHeader],
  module_paths: &HashMap<ModuleId, ModulePath>,
  umbrella_header_path: Option<&str>,
  std_path: &std::path::Path,
) -> String {
  emit_std_module_c_from_input(
    module_name,
    EmitInput::new(program, types, defs),
    namespaces,
    symbols,
    headers,
    module_paths,
    umbrella_header_path,
    std_path,
  )
}

#[allow(clippy::too_many_arguments)]
pub fn emit_std_module_c_from_input(
  module_name: &str,
  input: EmitInput<'_>,
  namespaces: &NamespaceStore,
  symbols: &SymbolTable,
  headers: &[CHeader],
  module_paths: &HashMap<ModuleId, ModulePath>,
  umbrella_header_path: Option<&str>,
  std_path: &std::path::Path,
) -> String {
  let headers = prepend_umbrella_header(headers, umbrella_header_path);
  CEmitter::with_std_target(
    input.program,
    input.types,
    input.defs,
    namespaces,
    symbols,
    &headers,
    EmitTarget::StdModule(module_name.to_string()),
    module_paths,
    std_path,
  )
  .with_source_map(input.source_map)
  .emit()
}

/// Emit header for a std module (function prototypes).
pub fn emit_std_module_h(
  module_name: &str,
  defs: &DefinitionStore,
  types: &TypeStore,
  symbols: &SymbolTable,
  namespaces: &NamespaceStore,
  module_paths: &HashMap<ModuleId, ModulePath>,
) -> String {
  emit_std_module_h_from_input(
    module_name,
    EmitInput::new(&LirProgram::new(), types, defs),
    symbols,
    namespaces,
    module_paths,
  )
}

pub fn emit_std_module_h_from_input(
  module_name: &str,
  input: EmitInput<'_>,
  symbols: &SymbolTable,
  namespaces: &NamespaceStore,
  module_paths: &HashMap<ModuleId, ModulePath>,
) -> String {
  let guard_name = format!("STD_{}_H", module_name.to_uppercase());
  let comment = format!("std::{}", module_name);

  let filter = |_def_id: DefinitionId, def: &ignis_type::definition::Definition| -> bool {
    match module_paths.get(&def.owner_module) {
      Some(ModulePath::Std(name)) => name.replace("::", "_") == module_name,
      Some(ModulePath::Project(_)) => {
        // For std internal files (e.g., memory/layout.ign), check if module_name matches
        let path = module_paths.get(&def.owner_module).unwrap();
        path.module_name() == module_name
      },
      None => false,
    }
  };

  emit_module_header(&guard_name, &comment, input, symbols, namespaces, filter)
}

/// Emit C for a specific user module.
///
/// `user_module_headers` should contain the module's own header plus headers of its
/// transitive user module dependencies. These are prepended to the standard headers.
#[allow(clippy::too_many_arguments)]
pub fn emit_user_module_c(
  module_id: ModuleId,
  program: &LirProgram,
  types: &TypeStore,
  defs: &DefinitionStore,
  namespaces: &NamespaceStore,
  symbols: &SymbolTable,
  headers: &[CHeader],
  module_paths: &HashMap<ModuleId, ModulePath>,
  user_module_headers: &[CHeader],
  std_path: &std::path::Path,
) -> String {
  emit_user_module_c_from_input(
    module_id,
    EmitInput::new(program, types, defs),
    namespaces,
    symbols,
    headers,
    module_paths,
    user_module_headers,
    std_path,
    None,
  )
}

#[allow(clippy::too_many_arguments)]
pub fn emit_user_module_c_from_input(
  module_id: ModuleId,
  input: EmitInput<'_>,
  namespaces: &NamespaceStore,
  symbols: &SymbolTable,
  headers: &[CHeader],
  module_paths: &HashMap<ModuleId, ModulePath>,
  user_module_headers: &[CHeader],
  std_path: &std::path::Path,
  std_defined_symbols: Option<&HashSet<String>>,
) -> String {
  let mut all_headers = user_module_headers.to_vec();
  all_headers.extend(headers.iter().cloned());

  CEmitter::with_user_module_target(
    input.program,
    input.types,
    input.defs,
    namespaces,
    symbols,
    &all_headers,
    module_id,
    module_paths,
    std_path,
    std_defined_symbols,
  )
  .with_source_map(input.source_map)
  .emit()
}

/// Emit header for a user module (function prototypes).
pub fn emit_user_module_h(
  module_id: ModuleId,
  source_path: &std::path::Path,
  defs: &DefinitionStore,
  types: &TypeStore,
  symbols: &SymbolTable,
  namespaces: &NamespaceStore,
) -> String {
  emit_user_module_h_from_input(
    module_id,
    source_path,
    EmitInput::new(&LirProgram::new(), types, defs),
    symbols,
    namespaces,
  )
}

pub fn emit_user_module_h_from_input(
  module_id: ModuleId,
  source_path: &std::path::Path,
  input: EmitInput<'_>,
  symbols: &SymbolTable,
  namespaces: &NamespaceStore,
) -> String {
  let guard_name = source_path
    .with_extension("")
    .to_string_lossy()
    .to_uppercase()
    .replace(['/', '\\', '.', '-'], "_")
    + "_H";
  let comment = source_path.display().to_string();

  let filter =
    |_def_id: DefinitionId, def: &ignis_type::definition::Definition| -> bool { def.owner_module == module_id };

  emit_module_header(&guard_name, &comment, input, symbols, namespaces, filter)
}

fn prepend_umbrella_header(
  headers: &[CHeader],
  umbrella: Option<&str>,
) -> Vec<CHeader> {
  match umbrella {
    Some(path) => {
      let mut h = vec![CHeader {
        path: path.to_string(),
        quoted: true,
      }];
      h.extend(headers.iter().cloned());
      h
    },
    None => headers.to_vec(),
  }
}

/// Common header generation for both std and user modules.
fn emit_module_header<F>(
  guard_name: &str,
  comment: &str,
  input: EmitInput<'_>,
  symbols: &SymbolTable,
  namespaces: &NamespaceStore,
  filter: F,
) -> String
where
  F: Fn(DefinitionId, &ignis_type::definition::Definition) -> bool,
{
  use std::collections::HashSet;

  let program = input.program;
  let defs = input.defs;
  let types = input.types;

  let mut output = String::new();

  writeln!(output, "#ifndef {}", guard_name).unwrap();
  writeln!(output, "#define {}", guard_name).unwrap();
  writeln!(output).unwrap();
  writeln!(output, "#include \"runtime/ignis_rt.h\"").unwrap();
  writeln!(output).unwrap();
  writeln!(output, "// Auto-generated header for {}", comment).unwrap();
  writeln!(output).unwrap();

  // Collect records and enums defined in this module
  let mut module_records: HashSet<DefinitionId> = HashSet::new();
  let mut module_enums: HashSet<DefinitionId> = HashSet::new();
  for (def_id, def) in defs.iter() {
    if !filter(def_id, def) {
      continue;
    }
    match &def.kind {
      DefinitionKind::Record(rd)
        if rd.type_params.is_empty()
          && rd
            .fields
            .iter()
            .all(|field| is_type_fully_monomorphized_standalone(field.type_id, types)) =>
      {
        module_records.insert(def_id);
      },
      DefinitionKind::Enum(ed)
        if ed.type_params.is_empty()
          && ed.variants.iter().all(|variant| {
            variant
              .payload
              .iter()
              .all(|payload_type| is_type_fully_monomorphized_standalone(*payload_type, types))
          }) =>
      {
        module_enums.insert(def_id);
      },
      _ => {},
    }
  }

  // Collect all struct types used in function signatures AND record fields
  let mut struct_forward_decls: HashSet<String> = HashSet::new();
  for (def_id, def) in defs.iter() {
    if !filter(def_id, def) {
      continue;
    }
    collect_struct_types_from_def(def_id, def, defs, types, symbols, namespaces, &mut struct_forward_decls);

    // Also collect types used in record fields
    if let DefinitionKind::Record(rd) = &def.kind {
      for field in &rd.fields {
        collect_struct_types_from_type(&field.type_id, types, defs, symbols, namespaces, &mut struct_forward_decls);
      }
    }

    // Also collect types used in enum variant payloads
    if let DefinitionKind::Enum(ed) = &def.kind {
      for variant in &ed.variants {
        for payload_ty in &variant.payload {
          collect_struct_types_from_type(payload_ty, types, defs, symbols, namespaces, &mut struct_forward_decls);
        }
      }
    }

    if let Some(func) = program.functions.get(&def_id) {
      collect_struct_types_from_function_body(func, types, defs, symbols, namespaces, &mut struct_forward_decls);
    }
  }

  // Get names of types defined in this module
  let module_type_names: HashSet<String> = module_records
    .iter()
    .chain(module_enums.iter())
    .map(|&def_id| build_mangled_name_standalone(def_id, defs, namespaces, symbols, types))
    .collect();

  // Separate closure structs from regular external structs.
  // Closure structs need full definitions (for by-value passing); others get forward decls.
  let mut external_forward_decls: Vec<&String> = Vec::new();
  let mut slice_struct_type_ids: Vec<TypeId> = Vec::new();
  let mut closure_struct_type_ids: Vec<TypeId> = Vec::new();

  for name in &struct_forward_decls {
    if module_type_names.contains(name) {
      continue;
    }
    if name.starts_with("__ignis_slice_") {
      for (type_id, ty) in types.iter() {
        if matches!(ty, Type::Slice { .. }) {
          let slice_name = slice_struct_name_standalone(type_id, types, defs, symbols, namespaces);
          if &slice_name == name && !slice_struct_type_ids.contains(&type_id) {
            slice_struct_type_ids.push(type_id);
            break;
          }
        }
      }
    } else if name.starts_with("__ignis_closure_") {
      // Find the matching TypeId for this closure struct name
      for (type_id, ty) in types.iter() {
        if let Type::Function { params, ret, .. } = ty {
          if !params
            .iter()
            .all(|param_type| is_type_fully_monomorphized_standalone(*param_type, types))
            || !is_type_fully_monomorphized_standalone(*ret, types)
          {
            continue;
          }

          let cname = closure_struct_name_standalone(params, ret, types, defs, symbols, namespaces);
          if &cname == name && !closure_struct_type_ids.contains(&type_id) {
            closure_struct_type_ids.push(type_id);
            break;
          }
        }
      }
    } else {
      external_forward_decls.push(name);
    }
  }

  external_forward_decls.sort();

  if !external_forward_decls.is_empty() {
    for name in &external_forward_decls {
      writeln!(output, "typedef struct {} {};", name, name).unwrap();
    }
    writeln!(output).unwrap();
  }

  slice_struct_type_ids.sort_by_key(|id| id.index());

  for slice_type_id in &slice_struct_type_ids {
    if let Type::Slice { element, .. } = types.get(slice_type_id) {
      let struct_name = slice_struct_name_standalone(*slice_type_id, types, defs, symbols, namespaces);
      let guard = format!("IGNIS_TYPE_DEF_{}", sanitize_macro_name(&struct_name));
      let data_type = format_c_type(types.get(element), types, defs, symbols, namespaces);

      writeln!(output, "#ifndef {}", guard).unwrap();
      writeln!(output, "#define {}", guard).unwrap();
      writeln!(output, "struct {} {{", struct_name).unwrap();
      writeln!(output, "    {}* data;", data_type).unwrap();
      writeln!(output, "    u64 len;").unwrap();
      writeln!(output, "}};").unwrap();
      writeln!(output, "#endif // {}", guard).unwrap();
    }
  }

  if !slice_struct_type_ids.is_empty() {
    writeln!(output).unwrap();
  }

  // Emit full closure struct definitions (guarded to avoid redefinition across headers).
  closure_struct_type_ids.sort_by_key(|id| id.index());

  for sig_type_id in &closure_struct_type_ids {
    if let Type::Function { params, ret, .. } = types.get(sig_type_id) {
      let struct_name = closure_struct_name_standalone(params, ret, types, defs, symbols, namespaces);
      let guard = format!("IGNIS_TYPE_DEF_{}", sanitize_macro_name(&struct_name));

      let ret_str = format_c_type(types.get(ret), types, defs, symbols, namespaces);
      let mut call_params = vec!["u8*".to_string()];
      for &p in params {
        call_params.push(format_c_type(types.get(&p), types, defs, symbols, namespaces));
      }

      writeln!(output, "#ifndef {}", guard).unwrap();
      writeln!(output, "#define {}", guard).unwrap();
      writeln!(output, "struct {} {{", struct_name).unwrap();
      writeln!(output, "    {} (*call)({});", ret_str, call_params.join(", ")).unwrap();
      writeln!(output, "    void (*drop_fn)(u8*);").unwrap();
      writeln!(output, "    u8* env;").unwrap();
      writeln!(output, "}};").unwrap();
      writeln!(output, "#endif // {}", guard).unwrap();
    }
  }

  if !closure_struct_type_ids.is_empty() {
    writeln!(output).unwrap();
  }

  // Collect external type definitions needed by this module (by-value usage).
  let external_definition_ids =
    collect_external_type_definition_ids(defs, types, symbols, namespaces, &module_type_names, &struct_forward_decls);

  // Build a unified set of all type definitions: module enums + records + external deps.
  // Then topologically sort them so that if type A contains type B by-value, B comes first.
  let mut all_type_defs: std::collections::HashMap<String, DefinitionId> = std::collections::HashMap::new();

  for &def_id in module_enums.iter().chain(module_records.iter()) {
    let name = build_mangled_name_standalone(def_id, defs, namespaces, symbols, types);
    all_type_defs.insert(name, def_id);
  }

  for def_id in &external_definition_ids {
    let name = build_mangled_name_standalone(*def_id, defs, namespaces, symbols, types);
    all_type_defs.insert(name, *def_id);
  }

  // Topological sort: emit types whose by-value dependencies are already resolved first.
  let mut unresolved: std::collections::HashSet<String> = all_type_defs.keys().cloned().collect();
  let mut resolved: std::collections::HashSet<String> = std::collections::HashSet::new();
  let mut ordered_type_defs: Vec<DefinitionId> = Vec::new();

  while !unresolved.is_empty() {
    let mut progressed = false;
    let mut names: Vec<String> = unresolved.iter().cloned().collect();
    names.sort();

    for name in names {
      let Some(def_id) = all_type_defs.get(&name).copied() else {
        continue;
      };

      let def = defs.get(&def_id);
      let mut dependencies = std::collections::HashSet::new();

      match &def.kind {
        DefinitionKind::Record(rd) => {
          for field in &rd.fields {
            collect_struct_dependencies_from_type(field.type_id, types, defs, symbols, namespaces, &mut dependencies);
          }
        },
        DefinitionKind::Enum(ed) => {
          for variant in &ed.variants {
            for payload in &variant.payload {
              collect_struct_dependencies_from_type(*payload, types, defs, symbols, namespaces, &mut dependencies);
            }
          }
        },
        _ => {},
      }

      dependencies.remove(&name);

      if dependencies
        .iter()
        .all(|dep| resolved.contains(dep) || !unresolved.contains(dep))
      {
        ordered_type_defs.push(def_id);
        unresolved.remove(&name);
        resolved.insert(name);
        progressed = true;
      }
    }

    if !progressed {
      let mut remaining: Vec<String> = unresolved.into_iter().collect();
      remaining.sort();
      for name in remaining {
        if let Some(def_id) = all_type_defs.get(&name).copied() {
          ordered_type_defs.push(def_id);
        }
      }
      break;
    }
  }

  // Emit all type definitions in topological order (each guarded).
  for def_id in &ordered_type_defs {
    let name = build_mangled_name_standalone(*def_id, defs, namespaces, symbols, types);
    let type_guard = format!("IGNIS_TYPE_DEF_{}", sanitize_macro_name(&name));

    writeln!(output, "#ifndef {}", type_guard).unwrap();
    writeln!(output, "#define {}", type_guard).unwrap();

    let def = defs.get(def_id);
    match &def.kind {
      DefinitionKind::Record(rd) => {
        emit_record_definition_standalone(*def_id, rd, defs, types, symbols, namespaces, &mut output);
      },
      DefinitionKind::Enum(ed) => {
        emit_enum_definition_standalone(*def_id, ed, defs, symbols, namespaces, types, &mut output);
      },
      _ => {},
    }

    writeln!(output, "#endif // {}", type_guard).unwrap();
  }

  if !ordered_type_defs.is_empty() {
    writeln!(output).unwrap();
  }

  let mut emitted: HashSet<String> = HashSet::new();

  for (def_id, def) in defs.iter() {
    if !filter(def_id, def) {
      continue;
    }

    if let Some(proto) = emit_func_prototype(def_id, def, defs, types, symbols, namespaces, &mut emitted) {
      writeln!(output, "{}", proto).unwrap();
    }
  }

  writeln!(output).unwrap();
  writeln!(output, "#endif // {}", guard_name).unwrap();

  output
}

/// Emit a struct definition for a record type (standalone version for headers).
fn emit_record_definition_standalone(
  def_id: DefinitionId,
  rd: &RecordDefinition,
  defs: &DefinitionStore,
  types: &TypeStore,
  symbols: &SymbolTable,
  namespaces: &NamespaceStore,
  output: &mut String,
) {
  let name = build_mangled_name_standalone(def_id, defs, namespaces, symbols, types);

  writeln!(output, "struct {} {{", name).unwrap();

  for (index, field) in rd.fields.iter().enumerate() {
    let field_type = format_c_type(types.get(&field.type_id), types, defs, symbols, namespaces);
    let field_attr_str = format_field_attrs_standalone(&field.attrs);
    writeln!(output, "    {} field_{}{};", field_type, index, field_attr_str).unwrap();
  }

  if rd.lang_traits.drop {
    writeln!(output, "    uint8_t __ignis_drop_state;").unwrap();
  }

  write!(output, "}}").unwrap();
  emit_record_attrs_standalone(&rd.attrs, output);
  writeln!(output, ";").unwrap();
}

/// Emit a struct definition for an enum type (standalone version for headers).
fn emit_enum_definition_standalone(
  def_id: DefinitionId,
  ed: &EnumDefinition,
  defs: &DefinitionStore,
  symbols: &SymbolTable,
  namespaces: &NamespaceStore,
  types: &TypeStore,
  output: &mut String,
) {
  let name = build_mangled_name_standalone(def_id, defs, namespaces, symbols, types);

  // Keep header enum layout identical to source emission (`payload.variant_<tag>`)
  // so generated .c can include the module header without type redefinition.
  writeln!(output, "struct {} {{", name).unwrap();
  let tag_type = format_c_type(types.get(&ed.tag_type), types, defs, symbols, namespaces);
  writeln!(output, "    {} tag;", tag_type).unwrap();

  // Check if any variant has payload
  let has_payloads = ed.variants.iter().any(|variant| {
    variant
      .payload
      .iter()
      .any(|payload_ty| !matches!(types.get(payload_ty), Type::Void))
  });

  if has_payloads {
    writeln!(output, "    union {{").unwrap();
    for variant in &ed.variants {
      let payload_fields: Vec<_> = variant
        .payload
        .iter()
        .copied()
        .filter(|payload_ty| !matches!(types.get(payload_ty), Type::Void))
        .collect();

      if !payload_fields.is_empty() {
        writeln!(output, "        struct {{").unwrap();
        for (i, payload_type_id) in payload_fields.iter().enumerate() {
          let payload_type = format_c_type(types.get(payload_type_id), types, defs, symbols, namespaces);
          writeln!(output, "            {} field_{};", payload_type, i).unwrap();
        }
        writeln!(output, "        }} variant_{};", variant.tag_value).unwrap();
      }
    }
    writeln!(output, "    }} payload;").unwrap();
  }

  write!(output, "}}").unwrap();
  emit_record_attrs_standalone(&ed.attrs, output);
  writeln!(output, ";").unwrap();

  // Emit variant tag constants
  for (i, variant) in ed.variants.iter().enumerate() {
    let variant_name = symbols.get(&variant.name);
    writeln!(output, "#define {}_{} {}", name, variant_name, i).unwrap();
  }
}

fn emit_record_attrs_standalone(
  attrs: &[RecordAttr],
  output: &mut String,
) {
  for attr in attrs {
    match attr {
      RecordAttr::Packed => write!(output, " __attribute__((packed))").unwrap(),
      RecordAttr::Aligned(n) => write!(output, " __attribute__((aligned({})))", n).unwrap(),
      RecordAttr::LangTry => {},
    }
  }
}

fn format_field_attrs_standalone(attrs: &[FieldAttr]) -> String {
  let mut result = String::new();
  for attr in attrs {
    match attr {
      FieldAttr::Aligned(n) => {
        write!(result, " __attribute__((aligned({})))", n).unwrap();
      },
    }
  }
  result
}

fn sanitize_macro_name(name: &str) -> String {
  name
    .chars()
    .map(|character| {
      if character.is_ascii_alphanumeric() {
        character.to_ascii_uppercase()
      } else {
        '_'
      }
    })
    .collect()
}

fn is_type_fully_monomorphized_standalone(
  type_id: TypeId,
  types: &TypeStore,
) -> bool {
  match types.get(&type_id) {
    Type::Param { .. } | Type::Infer => false,
    Type::Instance { args, .. } => args
      .iter()
      .all(|arg| is_type_fully_monomorphized_standalone(*arg, types)),
    Type::Pointer { inner, .. } | Type::Reference { inner, .. } => {
      is_type_fully_monomorphized_standalone(*inner, types)
    },
    Type::Slice { element, .. } => is_type_fully_monomorphized_standalone(*element, types),
    Type::FixedArray { element, .. } => is_type_fully_monomorphized_standalone(*element, types),
    Type::Function { params, ret, .. } => {
      params
        .iter()
        .all(|parameter| is_type_fully_monomorphized_standalone(*parameter, types))
        && is_type_fully_monomorphized_standalone(*ret, types)
    },
    _ => true,
  }
}

fn collect_struct_dependencies_from_type(
  type_id: TypeId,
  types: &TypeStore,
  defs: &DefinitionStore,
  symbols: &SymbolTable,
  namespaces: &NamespaceStore,
  out: &mut std::collections::HashSet<String>,
) {
  match types.get(&type_id) {
    Type::Record(def_id) | Type::Enum(def_id) => {
      out.insert(build_mangled_name_standalone(*def_id, defs, namespaces, symbols, types));
    },
    Type::Instance { args, .. } => {
      out.insert(monomorphized_instance_type_name_standalone(
        type_id, types, defs, symbols, namespaces,
      ));
      for arg in args {
        collect_struct_dependencies_from_type(*arg, types, defs, symbols, namespaces, out);
      }
    },
    Type::Slice { element, .. } => {
      out.insert(slice_struct_name_standalone(type_id, types, defs, symbols, namespaces));
      collect_struct_dependencies_from_type(*element, types, defs, symbols, namespaces, out);
    },
    Type::Tuple(elements) => {
      for element in elements {
        collect_struct_dependencies_from_type(*element, types, defs, symbols, namespaces, out);
      }
    },
    Type::Pointer { .. } | Type::Reference { .. } | Type::FixedArray { .. } | Type::Function { .. } => {
      // Pointers/function pointers do not require complete by-value definitions.
    },
    _ => {},
  }
}

fn collect_external_type_definition_ids(
  defs: &DefinitionStore,
  types: &TypeStore,
  symbols: &SymbolTable,
  namespaces: &NamespaceStore,
  module_type_names: &std::collections::HashSet<String>,
  referenced_struct_names: &std::collections::HashSet<String>,
) -> Vec<DefinitionId> {
  // Build a map of ALL monomorphized record/enum definitions (name → def_id).
  // This serves as the universe from which transitive dependencies can be discovered.
  let mut all_candidates: std::collections::HashMap<String, DefinitionId> = std::collections::HashMap::new();

  for (def_id, def) in defs.iter() {
    let is_candidate = match &def.kind {
      DefinitionKind::Record(record_definition) => {
        record_definition.type_params.is_empty()
          && record_definition
            .fields
            .iter()
            .all(|field| is_type_fully_monomorphized_standalone(field.type_id, types))
      },
      DefinitionKind::Enum(enum_definition) => {
        enum_definition.type_params.is_empty()
          && enum_definition.variants.iter().all(|variant| {
            variant
              .payload
              .iter()
              .all(|payload| is_type_fully_monomorphized_standalone(*payload, types))
          })
      },
      _ => false,
    };

    if !is_candidate {
      continue;
    }

    let name = build_mangled_name_standalone(def_id, defs, namespaces, symbols, types);

    if !module_type_names.contains(&name) {
      all_candidates.insert(name, def_id);
    }
  }

  // Start with directly referenced types, then transitively expand.
  let mut referenced_definitions: std::collections::HashMap<String, DefinitionId> = std::collections::HashMap::new();

  for (name, &def_id) in &all_candidates {
    if referenced_struct_names.contains(name) {
      referenced_definitions.insert(name.clone(), def_id);
    }
  }

  // Transitively discover by-value dependencies.
  // Each iteration scans fields/payloads of newly-added types and adds any
  // new dependencies from the candidate universe.
  loop {
    let mut newly_discovered: Vec<(String, DefinitionId)> = Vec::new();

    for (_, &def_id) in referenced_definitions.iter() {
      let def = defs.get(&def_id);
      let mut deps = std::collections::HashSet::new();

      match &def.kind {
        DefinitionKind::Record(rd) => {
          for field in &rd.fields {
            collect_struct_dependencies_from_type(field.type_id, types, defs, symbols, namespaces, &mut deps);
          }
        },
        DefinitionKind::Enum(ed) => {
          for variant in &ed.variants {
            for payload in &variant.payload {
              collect_struct_dependencies_from_type(*payload, types, defs, symbols, namespaces, &mut deps);
            }
          }
        },
        _ => {},
      }

      for dep_name in deps {
        if !referenced_definitions.contains_key(&dep_name)
          && let Some(&dep_def_id) = all_candidates.get(&dep_name)
        {
          newly_discovered.push((dep_name, dep_def_id));
        }
      }
    }

    if newly_discovered.is_empty() {
      break;
    }

    for (name, def_id) in newly_discovered {
      referenced_definitions.insert(name, def_id);
    }
  }

  if referenced_definitions.is_empty() {
    return Vec::new();
  }

  // Topological sort: emit types whose by-value dependencies are already resolved first.
  let mut unresolved: std::collections::HashSet<String> = referenced_definitions.keys().cloned().collect();
  let mut resolved: std::collections::HashSet<String> = module_type_names.clone();
  let mut ordered = Vec::new();

  while !unresolved.is_empty() {
    let mut progressed = false;
    let mut names: Vec<String> = unresolved.iter().cloned().collect();
    names.sort();

    for name in names {
      let Some(def_id) = referenced_definitions.get(&name).copied() else {
        continue;
      };

      let def = defs.get(&def_id);
      let mut dependencies = std::collections::HashSet::new();

      match &def.kind {
        DefinitionKind::Record(record_definition) => {
          for field in &record_definition.fields {
            collect_struct_dependencies_from_type(field.type_id, types, defs, symbols, namespaces, &mut dependencies);
          }
        },
        DefinitionKind::Enum(enum_definition) => {
          for variant in &enum_definition.variants {
            for payload in &variant.payload {
              collect_struct_dependencies_from_type(*payload, types, defs, symbols, namespaces, &mut dependencies);
            }
          }
        },
        _ => {},
      }

      dependencies.remove(&name);

      if dependencies
        .iter()
        .all(|dependency| resolved.contains(dependency) || !unresolved.contains(dependency))
      {
        ordered.push(def_id);
        unresolved.remove(&name);
        resolved.insert(name);
        progressed = true;
      }
    }

    if !progressed {
      let mut remaining: Vec<String> = unresolved.into_iter().collect();
      remaining.sort();
      for name in remaining {
        if let Some(def_id) = referenced_definitions.get(&name).copied() {
          ordered.push(def_id);
        }
      }
      break;
    }
  }

  ordered
}

/// Collect struct type names used in a function/method signature.
fn collect_struct_types_from_def(
  _def_id: DefinitionId,
  def: &ignis_type::definition::Definition,
  defs: &DefinitionStore,
  types: &TypeStore,
  symbols: &SymbolTable,
  namespaces: &NamespaceStore,
  out: &mut std::collections::HashSet<String>,
) {
  let (params_ids, return_type, is_extern, type_params_empty) = match &def.kind {
    DefinitionKind::Function(fd) => (&fd.params, &fd.return_type, fd.is_extern, fd.type_params.is_empty()),
    DefinitionKind::Method(md) => {
      // Skip methods of generic owners
      let owner_is_generic = match &defs.get(&md.owner_type).kind {
        DefinitionKind::Record(rd) => !rd.type_params.is_empty(),
        DefinitionKind::Enum(ed) => !ed.type_params.is_empty(),
        _ => false,
      };
      if owner_is_generic {
        return;
      }
      (&md.params, &md.return_type, false, md.type_params.is_empty())
    },
    _ => return,
  };

  if is_extern || !type_params_empty {
    return;
  }

  // Collect struct types from return type
  collect_struct_types_from_type(return_type, types, defs, symbols, namespaces, out);

  // Collect struct types from parameters
  for param_id in params_ids {
    let param_def = defs.get(param_id);
    if let DefinitionKind::Parameter(param) = &param_def.kind {
      collect_struct_types_from_type(&param.type_id, types, defs, symbols, namespaces, out);
    }
  }
}

fn collect_struct_types_from_function_body(
  func: &FunctionLir,
  types: &TypeStore,
  defs: &DefinitionStore,
  symbols: &SymbolTable,
  namespaces: &NamespaceStore,
  out: &mut std::collections::HashSet<String>,
) {
  for local in func.locals.get_all() {
    collect_struct_types_from_type(&local.ty, types, defs, symbols, namespaces, out);
  }

  for temp in func.temps.get_all() {
    collect_struct_types_from_type(&temp.ty, types, defs, symbols, namespaces, out);
  }
}

/// Recursively collect struct type names from a type.
fn collect_struct_types_from_type(
  type_id: &TypeId,
  types: &TypeStore,
  defs: &DefinitionStore,
  symbols: &SymbolTable,
  namespaces: &NamespaceStore,
  out: &mut std::collections::HashSet<String>,
) {
  match types.get(type_id) {
    Type::Record(def_id) | Type::Enum(def_id) => {
      let name = build_mangled_name_standalone(*def_id, defs, namespaces, symbols, types);
      out.insert(name);
    },
    Type::Instance { args, .. } => {
      out.insert(monomorphized_instance_type_name_standalone(
        *type_id, types, defs, symbols, namespaces,
      ));
      for arg in args {
        collect_struct_types_from_type(arg, types, defs, symbols, namespaces, out);
      }
    },
    Type::Slice { element, .. } => {
      out.insert(slice_struct_name_standalone(*type_id, types, defs, symbols, namespaces));
      collect_struct_types_from_type(element, types, defs, symbols, namespaces, out);
    },
    Type::Pointer { inner, .. } | Type::Reference { inner, .. } => {
      collect_struct_types_from_type(inner, types, defs, symbols, namespaces, out);
    },
    Type::FixedArray { element, .. } => {
      collect_struct_types_from_type(element, types, defs, symbols, namespaces, out);
    },
    Type::Function { params, ret, .. } => {
      let name = closure_struct_name_standalone(params, ret, types, defs, symbols, namespaces);
      out.insert(name);
    },
    _ => {},
  }
}

/// Emit a function/method prototype if applicable. Returns None if skipped.
fn emit_func_prototype(
  def_id: DefinitionId,
  def: &ignis_type::definition::Definition,
  defs: &DefinitionStore,
  types: &TypeStore,
  symbols: &SymbolTable,
  namespaces: &NamespaceStore,
  emitted: &mut std::collections::HashSet<String>,
) -> Option<String> {
  let (params_ids, return_type, is_extern, is_variadic, type_params_empty) = match &def.kind {
    DefinitionKind::Function(fd) => (
      &fd.params,
      &fd.return_type,
      fd.is_extern,
      fd.is_variadic,
      fd.type_params.is_empty(),
    ),
    DefinitionKind::Method(md) => {
      // Skip methods of generic owners - they can't be emitted without instantiation
      let owner_is_generic = match &defs.get(&md.owner_type).kind {
        DefinitionKind::Record(rd) => !rd.type_params.is_empty(),
        DefinitionKind::Enum(ed) => !ed.type_params.is_empty(),
        _ => false,
      };
      if owner_is_generic {
        return None;
      }
      (&md.params, &md.return_type, false, false, md.type_params.is_empty())
    },
    _ => return None,
  };

  let raw_name = symbols.get(&def.name);
  if raw_name.starts_with("__closure_thunk_") || raw_name.starts_with("__closure_drop_") {
    return None;
  }

  if !is_effectively_public_for_header(def, defs, symbols, namespaces) {
    return None;
  }

  if is_extern || !type_params_empty {
    return None;
  }

  // Skip definitions whose signature still contains Type::Param. This happens when
  // the monomorphizer creates "concrete" instantiations using Type::Param as the
  // type argument (parasitic definitions from generic contexts that weren't fully
  // resolved). The type_params field is empty but the actual types aren't concrete.
  if !is_type_fully_monomorphized_standalone(*return_type, types) {
    return None;
  }
  let has_unresolved_param = params_ids.iter().any(|param_id| {
    if let DefinitionKind::Parameter(param) = &defs.get(param_id).kind {
      !is_type_fully_monomorphized_standalone(param.type_id, types)
    } else {
      false
    }
  });
  if has_unresolved_param {
    return None;
  }

  let name = build_mangled_name_standalone(def_id, defs, namespaces, symbols, types);
  if emitted.contains(&name) {
    return None;
  }
  emitted.insert(name.clone());

  let return_ty = format_c_type(types.get(return_type), types, defs, symbols, namespaces);

  let mut param_strs: Vec<String> = params_ids
    .iter()
    .filter_map(|param_id| {
      let param_def = defs.get(param_id);
      if let DefinitionKind::Parameter(param) = &param_def.kind {
        let pname = symbols.get(&param_def.name);
        let pty = format_c_type(types.get(&param.type_id), types, defs, symbols, namespaces);
        Some(format!("{} {}", pty, pname))
      } else {
        None
      }
    })
    .collect();

  if is_variadic {
    param_strs.push("...".to_string());
  }

  let params = if param_strs.is_empty() {
    "void".to_string()
  } else {
    param_strs.join(", ")
  };

  Some(format!("{} {}({});", return_ty, name, params))
}

fn is_effectively_public_for_header(
  def: &ignis_type::definition::Definition,
  defs: &DefinitionStore,
  symbols: &SymbolTable,
  namespaces: &NamespaceStore,
) -> bool {
  let raw_name = symbols.get(&def.name);

  if raw_name.starts_with("__") {
    return false;
  }

  if matches!(&def.kind, DefinitionKind::Function(function) if function_is_extension(function)) {
    return true;
  }

  if def.visibility == Visibility::Public {
    return true;
  }

  let mut current_namespace = def.owner_namespace;

  while let Some(namespace_id) = current_namespace {
    let Some(namespace_def_id) = defs.iter().find_map(|(candidate_id, candidate)| match &candidate.kind {
      DefinitionKind::Namespace(namespace_def) if namespace_def.namespace_id == namespace_id => Some(candidate_id),
      _ => None,
    }) else {
      return false;
    };

    let namespace_def = defs.get(&namespace_def_id);
    if namespace_def.visibility == Visibility::Public {
      return true;
    }

    current_namespace = namespaces.get(&namespace_id).parent;
  }

  false
}

fn function_is_extension(function: &FunctionDefinition) -> bool {
  function
    .attrs
    .iter()
    .any(|attr| matches!(attr, FunctionAttr::Extension { .. }))
}

fn build_mangled_name_standalone(
  def_id: DefinitionId,
  defs: &DefinitionStore,
  namespaces: &NamespaceStore,
  symbols: &SymbolTable,
  types: &TypeStore,
) -> String {
  let def = defs.get(&def_id);
  let raw_name = symbols.get(&def.name).to_string();

  let is_extern = match &def.kind {
    DefinitionKind::Function(f) => f.is_extern,
    // Methods are never extern in Ignis
    DefinitionKind::Method(_) => false,
    _ => false,
  };

  if is_extern {
    return raw_name;
  }

  if def.owner_namespace.is_none() && raw_name == "main" && matches!(def.kind, DefinitionKind::Function(_)) {
    return USER_MAIN_SYMBOL.to_string();
  }

  // Ignis supports function overloading, so the C symbol for every non-extern
  // function must encode the parameter and return types. Doing it unconditionally
  // (rather than only when a same-name sibling is detected) keeps user functions
  // from accidentally clashing with libc / runtime symbols of the same bare
  // name (e.g. `function abs(n: i32): i32` vs the C library `abs`). Methods
  // already carry an owner-type prefix and a generic specialization suffix, so
  // the param suffix is appended to the inner method name in the namespaced
  // branch below.
  let needs_param_suffix = match &def.kind {
    DefinitionKind::Function(_) => true,
    DefinitionKind::Method(_) => {
      has_overloads_standalone(def_id, defs) || original_generic_method_has_overloads_standalone(def_id, defs, symbols)
    },
    _ => false,
  };
  let param_suffix = if needs_param_suffix {
    format_param_types_for_mangling_standalone(def_id, defs, namespaces, types, symbols)
  } else {
    String::new()
  };

  // For methods, include owner type prefix
  let owner_type_prefix = match &def.kind {
    DefinitionKind::Method(md) => {
      let owner_def = defs.get(&md.owner_type);
      let owner_name = symbols.get(&owner_def.name).to_string();
      if raw_name.starts_with(&owner_name) {
        None
      } else {
        Some(escape_ident(&owner_name))
      }
    },
    _ => None,
  };

  match def.owner_namespace {
    Some(ns_id) => {
      let ns_path = namespaces.full_path(ns_id);
      let mut parts: Vec<String> = ns_path.iter().map(|s| escape_ident(symbols.get(s))).collect();

      if let Some(type_prefix) = owner_type_prefix {
        parts.push(type_prefix);
      }

      let name_with_suffix = if param_suffix.is_empty() {
        escape_ident(&raw_name)
      } else {
        format!("{}_{}", escape_ident(&raw_name), param_suffix)
      };
      parts.push(name_with_suffix);
      parts.join("_")
    },
    None => {
      let name_with_suffix = if param_suffix.is_empty() {
        escape_ident(&raw_name)
      } else {
        format!("{}_{}", escape_ident(&raw_name), param_suffix)
      };
      if let Some(type_prefix) = owner_type_prefix {
        format!("{}_{}", type_prefix, name_with_suffix)
      } else {
        name_with_suffix
      }
    },
  }
}

fn has_overloads_standalone(
  target_def_id: DefinitionId,
  defs: &DefinitionStore,
) -> bool {
  let target_def = defs.get(&target_def_id);
  let target_name = target_def.name;
  let target_ns = target_def.owner_namespace;

  // Only functions and methods can be overloaded
  if !matches!(target_def.kind, DefinitionKind::Function(_) | DefinitionKind::Method(_)) {
    return false;
  }

  let target_owner_type = match &target_def.kind {
    DefinitionKind::Method(md) => Some(md.owner_type),
    _ => None,
  };

  let mut count = 0;
  for (_, def) in defs.iter() {
    if def.name == target_name {
      if !matches!(def.kind, DefinitionKind::Function(_) | DefinitionKind::Method(_)) {
        continue;
      }

      let def_owner_type = match &def.kind {
        DefinitionKind::Method(md) => Some(md.owner_type),
        _ => None,
      };

      if target_owner_type.is_some() {
        if target_owner_type == def_owner_type {
          count += 1;
        }
      } else if def.owner_namespace == target_ns && def_owner_type.is_none() {
        count += 1;
      }

      if count > 1 {
        return true;
      }
    }
  }
  false
}

fn original_generic_method_has_overloads_standalone(
  target_def_id: DefinitionId,
  defs: &DefinitionStore,
  symbols: &SymbolTable,
) -> bool {
  let target_def = defs.get(&target_def_id);
  let DefinitionKind::Method(target_method) = &target_def.kind else {
    return false;
  };

  let target_owner_def = defs.get(&target_method.owner_type);
  let target_owner_name = symbols.get(&target_owner_def.name);
  let raw_name = symbols.get(&target_def.name);
  let Some(original_method_name) = raw_name.strip_prefix(&format!("{}__", target_owner_name)) else {
    return false;
  };

  let Some(original_owner_name) = target_owner_name.split("__").next() else {
    return false;
  };

  let Some(original_owner_def_id) = defs.iter().find_map(|(candidate_id, candidate)| {
    if candidate.owner_module == target_owner_def.owner_module
      && matches!(candidate.kind, DefinitionKind::Record(_) | DefinitionKind::Enum(_))
      && symbols.get(&candidate.name) == original_owner_name
    {
      Some(candidate_id)
    } else {
      None
    }
  }) else {
    return false;
  };

  let Some(&method_symbol) = symbols.map.get(original_method_name) else {
    return false;
  };

  let overload_count = defs
    .iter()
    .filter(|(_, candidate)| {
      candidate.name == method_symbol
        && matches!(candidate.kind, DefinitionKind::Method(_))
        && matches!(&candidate.kind, DefinitionKind::Method(method) if method.owner_type == original_owner_def_id)
    })
    .take(2)
    .count();

  overload_count > 1
}

fn format_param_types_for_mangling_standalone(
  def_id: DefinitionId,
  defs: &DefinitionStore,
  namespaces: &NamespaceStore,
  types: &TypeStore,
  symbols: &SymbolTable,
) -> String {
  match &defs.get(&def_id).kind {
    DefinitionKind::Function(fd) => {
      let mut parts: Vec<String> = fd
        .params
        .iter()
        .map(|p| format_type_for_mangling_standalone(defs.type_of(p), types, defs, namespaces, symbols))
        .collect();

      parts.push(format_type_for_mangling_standalone(
        &fd.return_type,
        types,
        defs,
        namespaces,
        symbols,
      ));

      parts.join("_")
    },
    DefinitionKind::Method(md) => {
      let start = if md.is_static { 0 } else { 1 };
      let mut parts: Vec<String> = md.params[start..]
        .iter()
        .map(|p| format_type_for_mangling_standalone(defs.type_of(p), types, defs, namespaces, symbols))
        .collect();

      parts.push(format_type_for_mangling_standalone(
        &md.return_type,
        types,
        defs,
        namespaces,
        symbols,
      ));

      parts.join("_")
    },
    _ => String::new(),
  }
}

fn type_definition_mangling_name_standalone(
  def_id: DefinitionId,
  defs: &DefinitionStore,
  namespaces: &NamespaceStore,
  symbols: &SymbolTable,
  types: &TypeStore,
) -> String {
  let def = defs.get(&def_id);

  match def.owner_namespace {
    Some(_) => build_mangled_name_standalone(def_id, defs, namespaces, symbols, types),
    None => symbols.get(&def.name).to_string(),
  }
}

fn format_type_for_mangling_standalone(
  ty: &TypeId,
  types: &TypeStore,
  defs: &DefinitionStore,
  namespaces: &NamespaceStore,
  symbols: &SymbolTable,
) -> String {
  match types.get(ty) {
    Type::I8 => "i8".to_string(),
    Type::I16 => "i16".to_string(),
    Type::I32 => "i32".to_string(),
    Type::I64 => "i64".to_string(),
    Type::U8 => "u8".to_string(),
    Type::U16 => "u16".to_string(),
    Type::U32 => "u32".to_string(),
    Type::U64 => "u64".to_string(),
    Type::F32 => "f32".to_string(),
    Type::F64 => "f64".to_string(),
    Type::Boolean => "bool".to_string(),
    Type::Char => "char".to_string(),
    Type::Str => "str".to_string(),
    Type::Atom => "atom".to_string(),
    Type::Void => "void".to_string(),
    Type::Pointer { inner, .. } => {
      format!(
        "ptr_{}",
        format_type_for_mangling_standalone(inner, types, defs, namespaces, symbols)
      )
    },
    Type::Reference { inner, mutable: true } => {
      format!(
        "mutref_{}",
        format_type_for_mangling_standalone(inner, types, defs, namespaces, symbols)
      )
    },
    Type::Reference { inner, mutable: false } => {
      format!(
        "ref_{}",
        format_type_for_mangling_standalone(inner, types, defs, namespaces, symbols)
      )
    },
    Type::Slice { element, .. } => {
      format!(
        "slice_{}",
        format_type_for_mangling_standalone(element, types, defs, namespaces, symbols)
      )
    },
    Type::FixedArray { element, .. } => {
      format!(
        "vec_{}",
        format_type_for_mangling_standalone(element, types, defs, namespaces, symbols)
      )
    },
    Type::Tuple(elems) => {
      let parts: Vec<_> = elems
        .iter()
        .map(|e| format_type_for_mangling_standalone(e, types, defs, namespaces, symbols))
        .collect();
      format!("tup_{}", parts.join("_"))
    },
    Type::Record(def_id) | Type::Enum(def_id) => {
      type_definition_mangling_name_standalone(*def_id, defs, namespaces, symbols, types)
    },
    Type::Instance { generic, args } => {
      let base = type_definition_mangling_name_standalone(*generic, defs, namespaces, symbols, types);
      if args.is_empty() {
        base
      } else {
        let args_str = args
          .iter()
          .map(|a| format_type_for_mangling_standalone(a, types, defs, namespaces, symbols))
          .collect::<Vec<_>>()
          .join("_");
        format!("{}_{}", base, args_str)
      }
    },
    _ => "unknown".to_string(),
  }
}

fn escape_ident(name: &str) -> String {
  name.replace('_', "__")
}

fn monomorphized_instance_type_name_standalone(
  type_id: TypeId,
  types: &TypeStore,
  defs: &DefinitionStore,
  symbols: &SymbolTable,
  namespaces: &NamespaceStore,
) -> String {
  match types.get(&type_id) {
    Type::Instance { generic, args } => {
      monomorphized_instance_name_from_parts_standalone(*generic, args, types, defs, symbols, namespaces)
    },
    _ => escape_ident(&format_type_for_monomorphized_name_standalone(
      type_id, types, defs, symbols, namespaces,
    )),
  }
}

fn monomorphized_instance_name_from_parts_standalone(
  generic: DefinitionId,
  args: &[TypeId],
  types: &TypeStore,
  defs: &DefinitionStore,
  symbols: &SymbolTable,
  namespaces: &NamespaceStore,
) -> String {
  let base = type_definition_mangling_name_standalone(generic, defs, namespaces, symbols, types);

  if args.is_empty() {
    return escape_ident(&base);
  }

  let args_str = args
    .iter()
    .map(|arg| format_type_for_monomorphized_name_standalone(*arg, types, defs, symbols, namespaces))
    .collect::<Vec<_>>()
    .join("__");

  escape_ident(&format!("{}__{}", base, args_str))
}

fn format_type_for_monomorphized_name_standalone(
  type_id: TypeId,
  types: &TypeStore,
  defs: &DefinitionStore,
  symbols: &SymbolTable,
  namespaces: &NamespaceStore,
) -> String {
  match types.get(&type_id) {
    Type::I8 => "i8".to_string(),
    Type::I16 => "i16".to_string(),
    Type::I32 => "i32".to_string(),
    Type::I64 => "i64".to_string(),
    Type::U8 => "u8".to_string(),
    Type::U16 => "u16".to_string(),
    Type::U32 => "u32".to_string(),
    Type::U64 => "u64".to_string(),
    Type::F32 => "f32".to_string(),
    Type::F64 => "f64".to_string(),
    Type::Boolean => "bool".to_string(),
    Type::Char => "char".to_string(),
    Type::Str => "str".to_string(),
    Type::Atom => "atom".to_string(),
    Type::Void => "void".to_string(),
    Type::Never => "never".to_string(),
    Type::Pointer { inner, mutable } => {
      let prefix = if *mutable { "ptrmut" } else { "ptr" };
      format!(
        "{}_{}",
        prefix,
        format_type_for_monomorphized_name_standalone(*inner, types, defs, symbols, namespaces)
      )
    },
    Type::Reference { inner, mutable } => {
      let prefix = if *mutable { "refmut" } else { "ref" };
      format!(
        "{}_{}",
        prefix,
        format_type_for_monomorphized_name_standalone(*inner, types, defs, symbols, namespaces)
      )
    },
    Type::Slice { element, mutable } => {
      let prefix = if *mutable { "slicemut" } else { "slice" };
      format!(
        "{}_{}",
        prefix,
        format_type_for_monomorphized_name_standalone(*element, types, defs, symbols, namespaces)
      )
    },
    Type::FixedArray { element, size } => {
      format!(
        "arr{}_{}",
        size,
        format_type_for_monomorphized_name_standalone(*element, types, defs, symbols, namespaces)
      )
    },
    Type::Instance { generic, args } => {
      let generic_def = defs.get(generic);
      let base = symbols.get(&generic_def.name).to_string();
      let args_str = args
        .iter()
        .map(|arg| format_type_for_monomorphized_name_standalone(*arg, types, defs, symbols, namespaces))
        .collect::<Vec<_>>()
        .join("__");
      format!("{}__{}", base, args_str)
    },
    Type::Record(def_id) | Type::Enum(def_id) => {
      type_definition_mangling_name_standalone(*def_id, defs, namespaces, symbols, types)
    },
    Type::Tuple(elements) => {
      let elements_str = elements
        .iter()
        .map(|element| format_type_for_monomorphized_name_standalone(*element, types, defs, symbols, namespaces))
        .collect::<Vec<_>>()
        .join("_");
      format!("tuple_{}", elements_str)
    },
    _ => "unknown".to_string(),
  }
}

/// Build a deterministic closure struct name from a function signature (standalone version).
fn closure_struct_name_standalone(
  params: &[TypeId],
  ret: &TypeId,
  types: &TypeStore,
  defs: &DefinitionStore,
  symbols: &SymbolTable,
  namespaces: &NamespaceStore,
) -> String {
  let mut parts = Vec::new();
  for &p in params {
    parts.push(format_type_for_name_standalone(p, types, defs, symbols, namespaces));
  }
  parts.push(format_type_for_name_standalone(*ret, types, defs, symbols, namespaces));
  format!("__ignis_closure_{}", parts.join("_"))
}

/// Format a type as a simple identifier fragment for closure struct naming (standalone version).
fn format_type_for_name_standalone(
  ty: TypeId,
  types: &TypeStore,
  defs: &DefinitionStore,
  symbols: &SymbolTable,
  namespaces: &NamespaceStore,
) -> String {
  match types.get(&ty) {
    Type::I8 => "i8".to_string(),
    Type::I16 => "i16".to_string(),
    Type::I32 => "i32".to_string(),
    Type::I64 => "i64".to_string(),
    Type::U8 => "u8".to_string(),
    Type::U16 => "u16".to_string(),
    Type::U32 => "u32".to_string(),
    Type::U64 => "u64".to_string(),
    Type::F32 => "f32".to_string(),
    Type::F64 => "f64".to_string(),
    Type::Boolean => "bool".to_string(),
    Type::Char => "char".to_string(),
    Type::Str => "str".to_string(),
    Type::Void => "void".to_string(),
    Type::Never => "never".to_string(),
    Type::Pointer { inner, .. } => format!(
      "ptr_{}",
      format_type_for_name_standalone(*inner, types, defs, symbols, namespaces)
    ),
    Type::Reference { inner, .. } => format!(
      "ref_{}",
      format_type_for_name_standalone(*inner, types, defs, symbols, namespaces)
    ),
    Type::Slice { element, .. } => format!(
      "slice_{}",
      format_type_for_name_standalone(*element, types, defs, symbols, namespaces)
    ),
    Type::Record(def_id) | Type::Enum(def_id) => {
      build_mangled_name_standalone(*def_id, defs, namespaces, symbols, types)
    },
    Type::Instance { generic, args } => {
      monomorphized_instance_name_from_parts_standalone(*generic, args, types, defs, symbols, namespaces)
    },
    _ => format!("ty{}", ty.index()),
  }
}

fn slice_struct_name_from_element_standalone(
  element: TypeId,
  types: &TypeStore,
  defs: &DefinitionStore,
  symbols: &SymbolTable,
  namespaces: &NamespaceStore,
) -> String {
  format!(
    "__ignis_slice_{}",
    format_type_for_name_standalone(element, types, defs, symbols, namespaces)
  )
}

fn slice_struct_name_standalone(
  slice_type_id: TypeId,
  types: &TypeStore,
  defs: &DefinitionStore,
  symbols: &SymbolTable,
  namespaces: &NamespaceStore,
) -> String {
  let Type::Slice { element, .. } = types.get(&slice_type_id) else {
    return format!("__ignis_slice_{}", slice_type_id.index());
  };

  slice_struct_name_from_element_standalone(*element, types, defs, symbols, namespaces)
}

/// Format a type as C type string using runtime type aliases.
pub fn format_c_type(
  ty: &Type,
  types: &TypeStore,
  defs: &DefinitionStore,
  symbols: &SymbolTable,
  namespaces: &NamespaceStore,
) -> String {
  match ty {
    Type::I8 => "i8".to_string(),
    Type::I16 => "i16".to_string(),
    Type::I32 => "i32".to_string(),
    Type::I64 => "i64".to_string(),
    Type::U8 => "u8".to_string(),
    Type::U16 => "u16".to_string(),
    Type::U32 => "u32".to_string(),
    Type::U64 => "u64".to_string(),
    Type::F32 => "f32".to_string(),
    Type::F64 => "f64".to_string(),
    Type::Boolean => "boolean".to_string(),
    Type::Char => "ignis_char_t".to_string(),
    Type::Str => "const char*".to_string(),
    Type::Atom => "ignis_atom_t".to_string(),
    Type::Void => "void".to_string(),
    Type::Never => "void".to_string(),
    Type::NullPtr => "void*".to_string(),
    Type::Error => "void*".to_string(),
    Type::Pointer { inner, .. } => {
      format!("{}*", format_c_type(types.get(inner), types, defs, symbols, namespaces))
    },
    Type::Reference { inner, .. } => {
      format!("{}*", format_c_type(types.get(inner), types, defs, symbols, namespaces))
    },
    Type::Slice { element, .. } => {
      format!(
        "struct {}",
        slice_struct_name_from_element_standalone(*element, types, defs, symbols, namespaces)
      )
    },
    Type::FixedArray { element, .. } => {
      format!("{}*", format_c_type(types.get(element), types, defs, symbols, namespaces))
    },
    Type::Tuple(_) => "void*".to_string(),
    Type::Function { params, ret, .. } => {
      let name = closure_struct_name_standalone(params, ret, types, defs, symbols, namespaces);
      format!("struct {}", name)
    },
    Type::Record(def_id) => {
      let name = build_mangled_name_standalone(*def_id, defs, namespaces, symbols, types);
      format!("struct {}", name)
    },
    Type::Enum(def_id) => {
      let name = build_mangled_name_standalone(*def_id, defs, namespaces, symbols, types);
      format!("struct {}", name)
    },
    Type::Param { .. } => {
      // Type::Param should never reach codegen (Invariant A)
      panic!("ICE: Type::Param reached C codegen - monomorphization failed")
    },
    Type::Instance { generic, args } => {
      let name = monomorphized_instance_name_from_parts_standalone(*generic, args, types, defs, symbols, namespaces);
      format!("struct {}", name)
    },
    Type::Infer => {
      panic!("ICE: Type::Infer reached C codegen after implicit type removal")
    },
    Type::InferVar(_) => {
      panic!("ICE: Type::InferVar reached C codegen - zonking failed")
    },
    Type::Unknown => {
      panic!("ICE: Type::Unknown reached C codegen")
    },
  }
}

/// Emit a C header file with function prototypes for public definitions.
pub fn emit_std_header(
  defs: &DefinitionStore,
  types: &TypeStore,
  symbols: &SymbolTable,
  namespaces: &NamespaceStore,
) -> String {
  emit_std_header_from_input(EmitInput::new(&LirProgram::new(), types, defs), symbols, namespaces)
}

pub fn emit_std_header_from_input(
  input: EmitInput<'_>,
  symbols: &SymbolTable,
  namespaces: &NamespaceStore,
) -> String {
  use ignis_type::definition::DefinitionKind;
  use std::collections::HashSet;

  let mut output = String::new();
  let mut emitted_names: HashSet<String> = HashSet::new();

  writeln!(output, "#ifndef IGNIS_STD_H").unwrap();
  writeln!(output, "#define IGNIS_STD_H").unwrap();
  writeln!(output).unwrap();
  writeln!(output, "#include \"runtime/ignis_rt.h\"").unwrap();
  writeln!(output).unwrap();
  writeln!(output, "// Auto-generated standard library prototypes").unwrap();
  writeln!(output).unwrap();

  for (def_id, def) in input.defs.iter() {
    if !matches!(def.kind, DefinitionKind::Function(_)) {
      continue;
    }

    let Some(prototype) =
      emit_func_prototype(def_id, def, input.defs, input.types, symbols, namespaces, &mut emitted_names)
    else {
      continue;
    };

    writeln!(output, "{}", prototype).unwrap();
  }

  writeln!(output).unwrap();
  writeln!(output, "#endif // IGNIS_STD_H").unwrap();

  output
}

fn cross_module_emit_needs_internal_linkage(is_effectively_public: bool) -> bool {
  !is_effectively_public
}

#[cfg(test)]
mod tests {
  use super::*;
  use ignis_type::BytePosition;
  use ignis_type::attribute::FunctionAttr;
  use ignis_type::definition::{
    Definition, DefinitionKind, FieldDefinition, FunctionDefinition, LangTraitSet, MethodDefinition,
    ParameterDefinition, NamespaceDefinition, RecordDefinition, RecordFieldDef, Visibility,
  };
  use ignis_type::file::FileId;
  use ignis_type::module::ModuleId;
  use ignis_type::namespace::NamespaceId;
  use ignis_type::span::Span;
  use ignis_type::symbol::SymbolTable;
  use std::cell::RefCell;
  use std::rc::Rc;

  #[test]
  fn test_owner_module_emission_keeps_user_specializations_in_owner_module() {
    assert!(!should_defer_owner_module_emit_to_entry(false, true, true));
    assert!(!should_defer_owner_module_emit_to_entry(true, true, true));
    assert!(!should_defer_owner_module_emit_to_entry(false, false, true));
    assert!(!should_defer_owner_module_emit_to_entry(false, true, false));
  }

  #[test]
  fn test_std_module_emission_skips_user_specializations() {
    assert!(should_defer_std_module_emit_for_user_specialization(true, true));
    assert!(!should_defer_std_module_emit_for_user_specialization(false, true));
    assert!(!should_defer_std_module_emit_for_user_specialization(true, false));
  }

  #[test]
  fn test_cross_module_emit_keeps_exported_linkage_external() {
    assert!(!cross_module_emit_needs_internal_linkage(true));
    assert!(cross_module_emit_needs_internal_linkage(false));
  }

  fn empty_program() -> (LirProgram, TypeStore, DefinitionStore, NamespaceStore, Rc<RefCell<SymbolTable>>) {
    let program = LirProgram::new();
    let types = TypeStore::new();
    let defs = DefinitionStore::new();
    let namespaces = NamespaceStore::new();
    let symbols = Rc::new(RefCell::new(SymbolTable::new()));
    (program, types, defs, namespaces, symbols)
  }

  fn synthetic_span() -> Span {
    Span::new(FileId::SYNTHETIC, BytePosition(0), BytePosition(0))
  }

  fn alloc_function(
    defs: &mut DefinitionStore,
    symbols: &mut SymbolTable,
    name: &str,
    visibility: Visibility,
    owner_namespace: Option<NamespaceId>,
    return_type: TypeId,
    attrs: Vec<FunctionAttr>,
  ) -> DefinitionId {
    defs.alloc(Definition {
      kind: DefinitionKind::Function(FunctionDefinition {
        type_params: Vec::new(),
        params: Vec::new(),
        return_type,
        is_extern: false,
        is_variadic: false,
        inline_mode: InlineMode::None,
        attrs,
      }),
      name: symbols.intern(name),
      span: synthetic_span(),
      name_span: synthetic_span(),
      visibility,
      owner_module: ModuleId::new(0),
      owner_namespace,
      doc: None,
    })
  }

  fn alloc_namespace(
    defs: &mut DefinitionStore,
    symbols: &mut SymbolTable,
    name: &str,
    visibility: Visibility,
    namespace_id: NamespaceId,
  ) -> DefinitionId {
    defs.alloc(Definition {
      kind: DefinitionKind::Namespace(NamespaceDefinition {
        namespace_id,
        is_extern: false,
        attrs: Vec::new(),
      }),
      name: symbols.intern(name),
      span: synthetic_span(),
      name_span: synthetic_span(),
      visibility,
      owner_module: ModuleId::new(0),
      owner_namespace: None,
      doc: None,
    })
  }

  fn alloc_record(
    defs: &mut DefinitionStore,
    symbols: &mut SymbolTable,
    name: &str,
    owner_module: ModuleId,
  ) -> DefinitionId {
    defs.alloc(Definition {
      kind: DefinitionKind::Record(RecordDefinition {
        type_params: Vec::new(),
        type_id: TypeId::new(0),
        fields: Vec::new(),
        instance_methods: std::collections::HashMap::new(),
        static_methods: std::collections::HashMap::new(),
        static_fields: std::collections::HashMap::new(),
        attrs: Vec::new(),
        lang_traits: LangTraitSet::default(),
        implemented_traits: Vec::new(),
      }),
      name: symbols.intern(name),
      span: synthetic_span(),
      name_span: synthetic_span(),
      visibility: Visibility::Public,
      owner_module,
      owner_namespace: None,
      doc: None,
    })
  }

  fn alloc_method(
    defs: &mut DefinitionStore,
    symbols: &mut SymbolTable,
    name: &str,
    owner_type: DefinitionId,
    owner_module: ModuleId,
    return_type: TypeId,
  ) -> DefinitionId {
    defs.alloc(Definition {
      kind: DefinitionKind::Method(MethodDefinition {
        type_params: Vec::new(),
        owner_type,
        params: Vec::new(),
        return_type,
        is_static: true,
        self_mutable: false,
        inline_mode: InlineMode::None,
        attrs: Vec::new(),
      }),
      name: symbols.intern(name),
      span: synthetic_span(),
      name_span: synthetic_span(),
      visibility: Visibility::Public,
      owner_module,
      owner_namespace: None,
      doc: None,
    })
  }

  #[test]
  fn test_monomorphized_method_uses_original_overload_suffix_for_archive_abi() {
    let (program, types, mut defs, namespaces, symbols_rc) = empty_program();
    let mut symbols = symbols_rc.borrow_mut();

    let vector_def = alloc_record(&mut defs, &mut symbols, "Vector", ModuleId::new(1));
    let vector_string_def = alloc_record(&mut defs, &mut symbols, "Vector__String", ModuleId::new(1));
    let bool_type = types.boolean();

    alloc_method(&mut defs, &mut symbols, "init", vector_def, ModuleId::new(1), bool_type);
    alloc_method(&mut defs, &mut symbols, "init", vector_def, ModuleId::new(1), bool_type);
    let concrete_init = alloc_method(
      &mut defs,
      &mut symbols,
      "Vector__String__init",
      vector_string_def,
      ModuleId::new(1),
      bool_type,
    );

    let emitter = CEmitter::new(&program, &types, &defs, &namespaces, &symbols, &[]);

    assert_eq!(emitter.build_mangled_name(concrete_init), "Vector____String____init_bool");
  }

  #[test]
  fn test_missing_std_archive_generic_specialization_is_force_emit_seed() {
    let (program, types, mut defs, namespaces, symbols_rc) = empty_program();
    let mut symbols = symbols_rc.borrow_mut();

    let vector_string_def = alloc_record(&mut defs, &mut symbols, "Vector__String", ModuleId::new(1));
    let bool_type = types.boolean();
    let concrete_is_empty = alloc_method(
      &mut defs,
      &mut symbols,
      "Vector__String__isEmpty",
      vector_string_def,
      ModuleId::new(1),
      bool_type,
    );

    let module_paths = HashMap::new();
    let missing_symbols = HashSet::new();
    let emitter = CEmitter::with_user_module_target(
      &program,
      &types,
      &defs,
      &namespaces,
      &symbols,
      &[],
      ModuleId::new(0),
      &module_paths,
      std::path::Path::new("std"),
      Some(&missing_symbols),
    );

    assert!(emitter.is_missing_std_archive_generic_specialization(concrete_is_empty));

    let mut present_symbols = HashSet::new();
    present_symbols.insert("Vector____String____isEmpty".to_string());
    let emitter = CEmitter::with_user_module_target(
      &program,
      &types,
      &defs,
      &namespaces,
      &symbols,
      &[],
      ModuleId::new(0),
      &module_paths,
      std::path::Path::new("std"),
      Some(&present_symbols),
    );

    assert!(!emitter.is_missing_std_archive_generic_specialization(concrete_is_empty));
  }

  #[test]
  fn test_non_entry_user_module_force_emits_missing_std_test_generic_specialization() {
    let (mut program, types, mut defs, namespaces, symbols_rc) = empty_program();
    let mut symbols = symbols_rc.borrow_mut();

    let user_module = ModuleId::new(0);
    let std_test_module = ModuleId::new(1);
    let void_type = types.void();

    let caller = alloc_function(
      &mut defs,
      &mut symbols,
      "callsAssertEq",
      Visibility::Private,
      None,
      void_type,
      Vec::new(),
    );

    let assert_eq = defs.alloc(Definition {
      kind: DefinitionKind::Function(FunctionDefinition {
        type_params: Vec::new(),
        params: Vec::new(),
        return_type: void_type,
        is_extern: false,
        is_variadic: false,
        inline_mode: InlineMode::None,
        attrs: Vec::new(),
      }),
      name: symbols.intern("assertEq__str"),
      span: synthetic_span(),
      name_span: synthetic_span(),
      visibility: Visibility::Private,
      owner_module: std_test_module,
      owner_namespace: None,
      doc: None,
    });

    let mut caller_blocks = ignis_type::Store::new();
    let mut caller_block = Block::new("entry".to_string());
    caller_block.instructions.push(Instr::Call {
      dest: None,
      callee: assert_eq,
      args: Vec::new(),
    });
    caller_block.terminator = Terminator::Return(None);
    let caller_entry = caller_blocks.alloc(caller_block);

    program.functions.insert(
      caller,
      FunctionLir {
        def_id: caller,
        params: Vec::new(),
        return_type: void_type,
        locals: ignis_type::Store::new(),
        temps: ignis_type::Store::new(),
        blocks: caller_blocks,
        entry_block: caller_entry,
        is_extern: false,
        is_variadic: false,
        inline_mode: InlineMode::None,
        span: synthetic_span(),
      },
    );

    let mut assert_blocks = ignis_type::Store::new();
    let mut assert_block = Block::new("entry".to_string());
    assert_block.terminator = Terminator::Return(None);
    let assert_entry = assert_blocks.alloc(assert_block);

    program.functions.insert(
      assert_eq,
      FunctionLir {
        def_id: assert_eq,
        params: Vec::new(),
        return_type: void_type,
        locals: ignis_type::Store::new(),
        temps: ignis_type::Store::new(),
        blocks: assert_blocks,
        entry_block: assert_entry,
        is_extern: false,
        is_variadic: false,
        inline_mode: InlineMode::None,
        span: synthetic_span(),
      },
    );

    let module_paths = HashMap::from([
      (user_module, ModulePath::Project(std::path::PathBuf::from("src/tests.ign"))),
      (std_test_module, ModulePath::Std("test".to_string())),
    ]);
    let missing_symbols = HashSet::new();

    let c_code = CEmitter::with_user_module_target(
      &program,
      &types,
      &defs,
      &namespaces,
      &symbols,
      &[],
      user_module,
      &module_paths,
      std::path::Path::new("std"),
      Some(&missing_symbols),
    )
    .emit();

    assert!(
      c_code.contains("void assertEq____str_void(void);"),
      "expected a prototype for the forced std::test generic specialization"
    );
    assert!(
      c_code.contains("static void assertEq____str_void(void)"),
      "expected the forced std::test generic specialization to be emitted with internal linkage"
    );
  }

  #[test]
  fn test_non_entry_user_module_force_emits_missing_std_generic_user_type_specialization() {
    let (mut program, mut types, mut defs, namespaces, symbols_rc) = empty_program();
    let mut symbols = symbols_rc.borrow_mut();

    let entry_module = ModuleId::new(0);
    let std_module = ModuleId::new(1);
    let user_module = ModuleId::new(2);

    let void_type = types.void();
    let token_def = alloc_record(&mut defs, &mut symbols, "Token", user_module);
    let token_type = types.record(token_def);
    let vector_token_def = alloc_record(&mut defs, &mut symbols, "Vector__Token", std_module);

    let push_param = defs.alloc(Definition {
      kind: DefinitionKind::Parameter(ParameterDefinition {
        type_id: token_type,
        mutable: false,
        attrs: Vec::new(),
      }),
      name: symbols.intern("value"),
      span: synthetic_span(),
      name_span: synthetic_span(),
      visibility: Visibility::Private,
      owner_module: std_module,
      owner_namespace: None,
      doc: None,
    });

    let vector_push = defs.alloc(Definition {
      kind: DefinitionKind::Method(MethodDefinition {
        type_params: Vec::new(),
        owner_type: vector_token_def,
        params: vec![push_param],
        return_type: void_type,
        is_static: false,
        self_mutable: true,
        inline_mode: InlineMode::None,
        attrs: Vec::new(),
      }),
      name: symbols.intern("Vector__Token__push"),
      span: synthetic_span(),
      name_span: synthetic_span(),
      visibility: Visibility::Public,
      owner_module: std_module,
      owner_namespace: None,
      doc: None,
    });

    let entry = alloc_function(&mut defs, &mut symbols, "main", Visibility::Public, None, void_type, Vec::new());
    let caller = alloc_function(
      &mut defs,
      &mut symbols,
      "usesVectorToken",
      Visibility::Private,
      None,
      void_type,
      Vec::new(),
    );
    defs.get_mut(&caller).owner_module = user_module;

    let mut entry_blocks = ignis_type::Store::new();
    let mut entry_block = Block::new("entry".to_string());
    entry_block.terminator = Terminator::Return(None);
    let entry_block_id = entry_blocks.alloc(entry_block);
    program.entry_point = Some(entry);
    program.functions.insert(
      entry,
      FunctionLir {
        def_id: entry,
        params: Vec::new(),
        return_type: void_type,
        locals: ignis_type::Store::new(),
        temps: ignis_type::Store::new(),
        blocks: entry_blocks,
        entry_block: entry_block_id,
        is_extern: false,
        is_variadic: false,
        inline_mode: InlineMode::None,
        span: synthetic_span(),
      },
    );

    let mut caller_blocks = ignis_type::Store::new();
    let mut caller_block = Block::new("entry".to_string());
    caller_block.instructions.push(Instr::Call {
      dest: None,
      callee: vector_push,
      args: Vec::new(),
    });
    caller_block.terminator = Terminator::Return(None);
    let caller_entry = caller_blocks.alloc(caller_block);
    program.functions.insert(
      caller,
      FunctionLir {
        def_id: caller,
        params: Vec::new(),
        return_type: void_type,
        locals: ignis_type::Store::new(),
        temps: ignis_type::Store::new(),
        blocks: caller_blocks,
        entry_block: caller_entry,
        is_extern: false,
        is_variadic: false,
        inline_mode: InlineMode::None,
        span: synthetic_span(),
      },
    );

    let mut push_blocks = ignis_type::Store::new();
    let mut push_block = Block::new("entry".to_string());
    push_block.terminator = Terminator::Return(None);
    let push_entry = push_blocks.alloc(push_block);
    program.functions.insert(
      vector_push,
      FunctionLir {
        def_id: vector_push,
        params: vec![push_param],
        return_type: void_type,
        locals: ignis_type::Store::new(),
        temps: ignis_type::Store::new(),
        blocks: push_blocks,
        entry_block: push_entry,
        is_extern: false,
        is_variadic: false,
        inline_mode: InlineMode::None,
        span: synthetic_span(),
      },
    );

    let module_paths = HashMap::from([
      (entry_module, ModulePath::Project(std::path::PathBuf::from("ignis/main.ign"))),
      (std_module, ModulePath::Std("vector".to_string())),
      (
        user_module,
        ModulePath::Project(std::path::PathBuf::from("ignis/syntax/stream.ign")),
      ),
    ]);
    let missing_symbols = HashSet::new();

    let c_code = CEmitter::with_user_module_target(
      &program,
      &types,
      &defs,
      &namespaces,
      &symbols,
      &[],
      user_module,
      &module_paths,
      std::path::Path::new("std"),
      Some(&missing_symbols),
    )
    .emit();

    assert!(
      c_code.contains("void Vector____Token____push("),
      "expected a prototype for the missing Vector<Token> specialization"
    );
    assert!(
      c_code.contains("static void Vector____Token____push("),
      "expected non-entry user module to force-emit missing Vector<Token> specialization with internal linkage"
    );
  }

  #[test]
  fn test_user_module_header_emits_record_with_generic_instance_field() {
    let (program, mut types, mut defs, namespaces, symbols_rc) = empty_program();
    let mut symbols = symbols_rc.borrow_mut();

    let vector_def = defs.alloc(Definition {
      kind: DefinitionKind::Record(RecordDefinition {
        type_params: Vec::new(),
        type_id: types.void(),
        fields: Vec::new(),
        instance_methods: std::collections::HashMap::new(),
        static_methods: std::collections::HashMap::new(),
        static_fields: std::collections::HashMap::new(),
        attrs: Vec::new(),
        lang_traits: LangTraitSet::default(),
        implemented_traits: Vec::new(),
      }),
      name: symbols.intern("Vector"),
      span: synthetic_span(),
      name_span: synthetic_span(),
      visibility: Visibility::Public,
      owner_module: ModuleId::new(1),
      owner_namespace: None,
      doc: None,
    });

    let string_def = defs.alloc(Definition {
      kind: DefinitionKind::Record(RecordDefinition {
        type_params: Vec::new(),
        type_id: types.void(),
        fields: Vec::new(),
        instance_methods: std::collections::HashMap::new(),
        static_methods: std::collections::HashMap::new(),
        static_fields: std::collections::HashMap::new(),
        attrs: Vec::new(),
        lang_traits: LangTraitSet::default(),
        implemented_traits: Vec::new(),
      }),
      name: symbols.intern("String"),
      span: synthetic_span(),
      name_span: synthetic_span(),
      visibility: Visibility::Public,
      owner_module: ModuleId::new(1),
      owner_namespace: None,
      doc: None,
    });

    let string_type = types.record(string_def);
    let vector_string_type = types.instance(vector_def, vec![string_type]);

    let interner_def = defs.alloc(Definition {
      kind: DefinitionKind::Record(RecordDefinition {
        type_params: Vec::new(),
        type_id: types.void(),
        fields: Vec::new(),
        instance_methods: std::collections::HashMap::new(),
        static_methods: std::collections::HashMap::new(),
        static_fields: std::collections::HashMap::new(),
        attrs: Vec::new(),
        lang_traits: LangTraitSet::default(),
        implemented_traits: Vec::new(),
      }),
      name: symbols.intern("StringInterner"),
      span: synthetic_span(),
      name_span: synthetic_span(),
      visibility: Visibility::Public,
      owner_module: ModuleId::new(0),
      owner_namespace: None,
      doc: None,
    });

    let field_def = defs.alloc(Definition {
      kind: DefinitionKind::Field(FieldDefinition {
        type_id: vector_string_type,
        owner_type: interner_def,
        index: 0,
      }),
      name: symbols.intern("texts"),
      span: synthetic_span(),
      name_span: synthetic_span(),
      visibility: Visibility::Private,
      owner_module: ModuleId::new(0),
      owner_namespace: None,
      doc: None,
    });

    let interner_type = types.record(interner_def);
    if let DefinitionKind::Record(record) = &mut defs.get_mut(&interner_def).kind {
      record.type_id = interner_type;
      record.fields.push(RecordFieldDef {
        name: symbols.intern("texts"),
        type_id: vector_string_type,
        index: 0,
        span: synthetic_span(),
        def_id: field_def,
        attrs: Vec::new(),
      });
    }

    let header = emit_user_module_h_from_input(
      ModuleId::new(0),
      std::path::Path::new("ignis/symbols/mod.ign"),
      EmitInput::new(&program, &types, &defs),
      &symbols,
      &namespaces,
    );

    assert!(header.contains("struct StringInterner {"), "{header}");
    assert!(header.contains("struct Vector____String field_0;"), "{header}");
  }

  #[test]
  fn test_emit_headers_no_includes() {
    let (program, types, defs, namespaces, symbols) = empty_program();
    let sym = symbols.borrow();
    let output = emit_c(&program, &types, &defs, &namespaces, &sym, &[]);

    // No hardcoded headers - emitter outputs only what it's given
    assert!(!output.contains("#include"));
  }

  #[test]
  fn test_emit_headers_with_quoted_include() {
    let (program, types, defs, namespaces, symbols) = empty_program();
    let sym = symbols.borrow();
    let headers = vec![CHeader {
      path: "runtime/io/io.h".to_string(),
      quoted: true,
    }];
    let output = emit_c(&program, &types, &defs, &namespaces, &sym, &headers);

    assert!(output.contains("#include \"runtime/io/io.h\""));
  }

  #[test]
  fn test_emit_headers_with_system_include() {
    let (program, types, defs, namespaces, symbols) = empty_program();
    let sym = symbols.borrow();
    let headers = vec![CHeader {
      path: "math.h".to_string(),
      quoted: false,
    }];
    let output = emit_c(&program, &types, &defs, &namespaces, &sym, &headers);

    assert!(output.contains("#include <math.h>"));
  }

  #[test]
  fn test_emit_headers_mixed_includes() {
    let (program, types, defs, namespaces, symbols) = empty_program();
    let sym = symbols.borrow();
    let headers = vec![
      CHeader {
        path: "runtime/ignis_rt.h".to_string(),
        quoted: true,
      },
      CHeader {
        path: "math.h".to_string(),
        quoted: false,
      },
    ];
    let output = emit_c(&program, &types, &defs, &namespaces, &sym, &headers);

    assert!(output.contains("#include \"runtime/ignis_rt.h\""));
    assert!(output.contains("#include <math.h>"));
  }

  #[test]
  fn test_header_visibility_skips_private_free_functions() {
    let mut defs = DefinitionStore::new();
    let types = TypeStore::new();
    let mut namespaces = NamespaceStore::new();
    let mut symbols = SymbolTable::new();

    let private_function = alloc_function(
      &mut defs,
      &mut symbols,
      "hashMapStorageBytes",
      Visibility::Private,
      None,
      types.void(),
      Vec::new(),
    );

    let libc = symbols.intern("LibC");
    let file = symbols.intern("File");
    let public_namespace_id = namespaces.get_or_create(&[libc], false);
    let nested_namespace_id = namespaces.get_or_create(&[libc, file], false);

    alloc_namespace(&mut defs, &mut symbols, "LibC", Visibility::Public, public_namespace_id);
    alloc_namespace(&mut defs, &mut symbols, "File", Visibility::Private, nested_namespace_id);

    let exported_namespace_function = alloc_function(
      &mut defs,
      &mut symbols,
      "write",
      Visibility::Private,
      Some(nested_namespace_id),
      types.void(),
      Vec::new(),
    );
    let extension_function = alloc_function(
      &mut defs,
      &mut symbols,
      "toString",
      Visibility::Private,
      None,
      types.void(),
      vec![FunctionAttr::Extension {
        type_name: "i32".to_string(),
        mutable: false,
      }],
    );

    assert!(
      !is_effectively_public_for_header(defs.get(&private_function), &defs, &symbols, &namespaces),
      "private free functions must stay out of generated headers"
    );
    assert!(
      is_effectively_public_for_header(defs.get(&exported_namespace_function), &defs, &symbols, &namespaces),
      "functions inside exported namespace chains must keep external linkage"
    );
    assert!(
      is_effectively_public_for_header(defs.get(&extension_function), &defs, &symbols, &namespaces),
      "extension functions must stay visible across module headers"
    );
  }

  #[test]
  fn test_emit_std_header_skips_generic_function_prototypes() {
    let program = LirProgram::new();
    let mut types = TypeStore::new();
    let mut defs = DefinitionStore::new();
    let namespaces = NamespaceStore::new();
    let mut symbols = SymbolTable::new();

    let type_param_owner = DefinitionId::new(99);
    let generic_param = types.param(type_param_owner, 0);

    defs.alloc(Definition {
      kind: DefinitionKind::Function(FunctionDefinition {
        type_params: vec![DefinitionId::new(100)],
        params: Vec::new(),
        return_type: generic_param,
        is_extern: false,
        is_variadic: false,
        inline_mode: InlineMode::None,
        attrs: vec![FunctionAttr::Extension {
          type_name: "T".to_string(),
          mutable: false,
        }],
      }),
      name: symbols.intern("toString"),
      span: synthetic_span(),
      name_span: synthetic_span(),
      visibility: Visibility::Private,
      owner_module: ModuleId::new(0),
      owner_namespace: None,
      doc: None,
    });

    let output = emit_std_header_from_input(EmitInput::new(&program, &types, &defs), &symbols, &namespaces);

    assert!(
      !output.contains("toString"),
      "generic functions with unresolved type params must be skipped in std headers"
    );
  }
}
