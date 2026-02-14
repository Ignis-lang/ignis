use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;
use std::fmt::Write;

use ignis_config::CHeader;
use ignis_hir::operation::{BinaryOperation, UnaryOperation};
use ignis_lir::{Block, ConstValue, FunctionLir, Instr, LirProgram, Operand, TempId, Terminator};
use ignis_type::{
  attribute::{FieldAttr, FunctionAttr, RecordAttr},
  definition::{
    DefinitionId, DefinitionKind, DefinitionStore, EnumDefinition, InlineMode, RecordDefinition, Visibility,
  },
  module::{ModuleId, ModulePath},
  namespace::NamespaceStore,
  symbol::SymbolTable,
  types::{Type, TypeId, TypeStore},
};

use crate::classify::{DefKind, EmitTarget};

/// C code emitter for LIR programs.
pub struct CEmitter<'a> {
  program: &'a LirProgram,
  types: &'a TypeStore,
  defs: &'a DefinitionStore,
  namespaces: &'a NamespaceStore,
  symbols: &'a SymbolTable,
  headers: &'a [CHeader],
  output: String,
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
      current_fn_id: None,
      target: None,
      module_paths: None,
      std_path: None,
      forced_emit_defs: HashSet::new(),
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
      current_fn_id: None,
      target: Some(target),
      module_paths: Some(module_paths),
      std_path: None,
      forced_emit_defs: HashSet::new(),
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
      current_fn_id: None,
      target: Some(target),
      module_paths: Some(module_paths),
      std_path: Some(std_path),
      forced_emit_defs: HashSet::new(),
    }
  }

  pub fn emit(mut self) -> String {
    if let Some(module_id) = self.target.as_ref().and_then(|target| target.target_user_module()) {
      self.forced_emit_defs = self.collect_forced_emit_defs(module_id);
    }

    self.emit_implicit_headers();
    self.emit_headers();

    let uses_module_headers = matches!(self.target, Some(EmitTarget::UserModule(_)) | Some(EmitTarget::StdModule(_)));

    if !uses_module_headers {
      self.emit_type_forward_declarations();
      self.emit_type_definitions();
    }

    self.emit_static_constants();
    self.emit_extern_declarations();
    self.emit_drop_glue_helpers();
    self.emit_forward_declarations();
    self.emit_functions();
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
            _ => {},
          }
        }
      }
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
            return true;
          }

          // Monomorphized functions (mangled names with __) are emitted only in the entry module
          // to avoid duplicate definitions across user modules.
          let name = self.symbols.get(&def.name);
          if name.contains("__")
            && !name.starts_with("__")
            && let Some(entry_id) = self.program.entry_point
          {
            let entry_def = self.defs.get(&entry_id);
            if entry_def.owner_module == target_module_id {
              return true;
            }
          }

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
          if let Instr::Call { callee, .. } = instruction
            && reachable.insert(*callee)
          {
            queue.push_back(*callee);
          }
        }
      }
    }

    reachable
      .into_iter()
      .filter(|def_id| {
        if self.defs.get(def_id).owner_module == target_module_id {
          return false;
        }

        let Some(function) = self.program.functions.get(def_id) else {
          return false;
        };

        if function.is_extern {
          return false;
        }

        if !self.is_function_signature_monomorphized(function) {
          return false;
        }

        // Non-user definitions (Std/Runtime) are only force-emitted when they
        // are monomorphized specializations (name contains `__`). Regular std
        // functions already exist in precompiled std object files.
        let kind = self.classify(*def_id);
        if !kind.is_user() {
          let name = self.symbols.get(&self.defs.get(def_id).name);
          return name.contains("__") && !name.starts_with("__");
        }

        true
      })
      .collect()
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

  /// Check if a type is fully monomorphized (no Type::Param or Type::Instance).
  fn is_fully_monomorphized(
    &self,
    type_id: TypeId,
  ) -> bool {
    match self.types.get(&type_id) {
      Type::Param { .. } | Type::Instance { .. } | Type::Infer => false,
      Type::Pointer { inner, .. } | Type::Reference { inner, .. } => self.is_fully_monomorphized(*inner),
      Type::Vector { element, .. } => self.is_fully_monomorphized(*element),
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
    let has_payload = ed.variants.iter().any(|v| !v.payload.is_empty());

    if has_payload {
      writeln!(self.output, "    union {{").unwrap();

      for variant in &ed.variants {
        if variant.payload.is_empty() {
          continue;
        }

        writeln!(self.output, "        struct {{").unwrap();
        for (i, &payload_ty) in variant.payload.iter().enumerate() {
          let ty = self.format_type(payload_ty);
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
      ConstValue::Char(c) => format!("'{}'", c.escape_default()),
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

    if std::env::var("IGNIS_VERBOSE").is_ok() {
      eprintln!("[EMIT] emit_forward_declarations: {} functions in LIR", funcs.len());
      for (def_id, _) in &funcs {
        let name = self.def_name(**def_id);
        if name.contains("init") || name.contains("Vector") {
          eprintln!("[EMIT]   - {} (should_emit={})", name, self.should_emit(**def_id));
        }
      }
    }

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
        let is_public = self.defs.get(def_id).visibility == Visibility::Public;
        if !is_public {
          continue;
        }
      }

      // Filter by target
      if !self.should_emit(**def_id) {
        continue;
      }

      // Skip functions with Type::Param or Type::Instance in their signature
      if !self.is_function_signature_monomorphized(func) {
        if std::env::var("IGNIS_VERBOSE").is_ok() {
          let name = self.def_name(**def_id);
          eprintln!("[EMIT] Skipping forward decl for {} - signature not monomorphized", name);
        }
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

  fn emit_function_signature(
    &mut self,
    def_id: DefinitionId,
    func: &FunctionLir,
  ) {
    let name = self.def_name(def_id);

    // C requires int main(void) for the entry point
    let is_entry_main = Some(def_id) == self.program.entry_point && name == "main";
    let ret_ty = if is_entry_main {
      "int".to_string()
    } else {
      self.format_type(func.return_type)
    };

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
    if !is_entry_main && !func.is_extern {
      let mut is_public = self.defs.get(&def_id).visibility == Visibility::Public;

      // In per-user-module emission, helper definitions forced from other modules
      // are implementation details of this translation unit.
      if let Some(target_module_id) = self.target.as_ref().and_then(|target| target.target_user_module())
        && self.defs.get(&def_id).owner_module != target_module_id
      {
        is_public = false;
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

  fn emit_function(
    &mut self,
    def_id: DefinitionId,
    func: &FunctionLir,
  ) {
    self.current_fn_id = Some(def_id);
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
    for (idx, local) in locals.iter().enumerate() {
      let name = local.name.as_deref().unwrap_or("_");
      if let Type::Vector { element, size: n } = self.types.get(&local.ty) {
        let elem_ty = self.format_type(*element);
        writeln!(self.output, "    {} l{}[{}]; // {}", elem_ty, idx, n, name).unwrap();
      } else {
        let ty = self.format_var_type(local.ty);
        writeln!(self.output, "    {} l{}; // {}", ty, idx, name).unwrap();
      }
    }
    writeln!(self.output).unwrap();
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

        if let Type::Vector { size: n, element } = self.types.get(&target_ty) {
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
        if matches!(self.types.get(&local_info.ty), Type::Vector { .. }) {
          writeln!(self.output, "t{} = l{};", dest.index(), local.index()).unwrap();
        } else {
          writeln!(self.output, "t{} = &l{};", dest.index(), local.index()).unwrap();
        }
      },
      Instr::GetElementPtr { dest, base, index, .. } => {
        let b = self.format_operand(func, base);
        let i = self.format_operand(func, index);

        writeln!(self.output, "t{} = &{}[{}];", dest.index(), b, i).unwrap();
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

        if is_pointer {
          writeln!(self.output, "t{} = &(({})->field_{});", dest.index(), b, field_index).unwrap();
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

        // Resolve payload types for format_operand_coerced.
        let payload_types: Vec<TypeId> = match self.types.get(enum_type) {
          Type::Enum(def_id) => {
            let def = self.defs.get(def_id);
            match &def.kind {
              DefinitionKind::Enum(ed) => {
                if let Some(variant) = ed.variants.iter().find(|v| v.tag_value == *variant_tag) {
                  variant.payload.clone()
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

      Instr::DropGlue { dest, ty } => {
        let glue_name = self.drop_glue_name(*ty);
        writeln!(self.output, "t{} = (void(*)(u8*)){};", dest.index(), glue_name).unwrap();
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
          // C main must return int - if we're in main with void return, emit return 0
          let is_main = self
            .current_fn_id
            .is_some_and(|id| Some(id) == self.program.entry_point && self.def_name(id) == "main");
          if is_main {
            writeln!(self.output, "return 0;").unwrap();
          } else {
            writeln!(self.output, "return;").unwrap();
          }
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
      ConstValue::Char(v, _) => format!("{}", *v as u32),
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
      Type::Char => "char".to_string(),
      Type::Str => "const char*".to_string(),
      Type::Atom => "ignis_atom_t".to_string(),
      Type::Void => "void".to_string(),
      Type::Never => "void".to_string(),
      Type::NullPtr => "void*".to_string(),
      Type::Error => "/* error */ void*".to_string(),
      Type::Pointer { inner, .. } => format!("{}*", self.format_type(*inner)),
      Type::Reference { inner, .. } => format!("{}*", self.format_type(*inner)),
      Type::Vector { element, .. } => {
        format!("{}*", self.format_type(*element))
      },
      Type::Tuple(_) => "/* tuple */ void*".to_string(),
      Type::Function { .. } => "/* fn */ void*".to_string(),
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

    if def.owner_namespace.is_none() && raw_name == "main" {
      return raw_name;
    }

    self.build_mangled_name(def_id)
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

    let has_overloads = self.has_overloads(def_id);

    let param_suffix = if has_overloads {
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

  fn format_param_types_for_mangling(
    &self,
    def_id: DefinitionId,
  ) -> String {
    match &self.defs.get(&def_id).kind {
      DefinitionKind::Function(fd) => fd
        .params
        .iter()
        .map(|p| self.format_type_for_mangling(self.defs.type_of(p)))
        .collect::<Vec<_>>()
        .join("_"),
      DefinitionKind::Method(md) => {
        let start = if md.is_static { 0 } else { 1 };
        md.params[start..]
          .iter()
          .map(|p| self.format_type_for_mangling(self.defs.type_of(p)))
          .collect::<Vec<_>>()
          .join("_")
      },
      _ => String::new(),
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
      Type::Pointer { inner, .. } => format!("ptr_{}", self.format_type_for_mangling(inner)),
      Type::Reference { inner, mutable: true } => format!("mutref_{}", self.format_type_for_mangling(inner)),
      Type::Reference { inner, mutable: false } => format!("ref_{}", self.format_type_for_mangling(inner)),
      Type::Record(def_id) => {
        let def = self.defs.get(def_id);
        self.symbols.get(&def.name).to_string()
      },
      Type::Enum(def_id) => {
        let def = self.defs.get(def_id);
        self.symbols.get(&def.name).to_string()
      },
      Type::Instance { generic, args } => {
        let generic_def = self.defs.get(generic);
        let base_name = self.symbols.get(&generic_def.name).to_string();
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

/// Legacy: emits everything (all modules combined).
pub fn emit_c(
  program: &LirProgram,
  types: &TypeStore,
  defs: &DefinitionStore,
  namespaces: &NamespaceStore,
  symbols: &SymbolTable,
  headers: &[CHeader],
) -> String {
  CEmitter::new(program, types, defs, namespaces, symbols, headers).emit()
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
  CEmitter::with_target(
    program,
    types,
    defs,
    namespaces,
    symbols,
    headers,
    EmitTarget::User,
    module_paths,
  )
  .emit()
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
  let headers = prepend_umbrella_header(headers, umbrella_header_path);
  CEmitter::with_std_target(
    program,
    types,
    defs,
    namespaces,
    symbols,
    &headers,
    EmitTarget::StdModule(module_name.to_string()),
    module_paths,
    std_path,
  )
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
  let guard_name = format!("STD_{}_H", module_name.to_uppercase());
  let comment = format!("std::{}", module_name);

  let filter = |_def_id: DefinitionId, def: &ignis_type::definition::Definition| -> bool {
    match module_paths.get(&def.owner_module) {
      Some(ModulePath::Std(name)) => name == module_name,
      Some(ModulePath::Project(_)) => {
        // For std internal files (e.g., memory/layout.ign), check if module_name matches
        let path = module_paths.get(&def.owner_module).unwrap();
        path.module_name() == module_name
      },
      None => false,
    }
  };

  emit_module_header(&guard_name, &comment, defs, types, symbols, namespaces, filter)
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
) -> String {
  let mut all_headers = user_module_headers.to_vec();
  all_headers.extend(headers.iter().cloned());

  CEmitter::with_target(
    program,
    types,
    defs,
    namespaces,
    symbols,
    &all_headers,
    EmitTarget::UserModule(module_id),
    module_paths,
  )
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
  let guard_name = source_path
    .with_extension("")
    .to_string_lossy()
    .to_uppercase()
    .replace(['/', '\\', '.', '-'], "_")
    + "_H";
  let comment = source_path.display().to_string();

  let filter =
    |_def_id: DefinitionId, def: &ignis_type::definition::Definition| -> bool { def.owner_module == module_id };

  emit_module_header(&guard_name, &comment, defs, types, symbols, namespaces, filter)
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
  defs: &DefinitionStore,
  types: &TypeStore,
  symbols: &SymbolTable,
  namespaces: &NamespaceStore,
  filter: F,
) -> String
where
  F: Fn(DefinitionId, &ignis_type::definition::Definition) -> bool,
{
  use std::collections::HashSet;

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
      DefinitionKind::Record(rd) if rd.type_params.is_empty() => {
        module_records.insert(def_id);
      },
      DefinitionKind::Enum(ed) if ed.type_params.is_empty() => {
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
  }

  // Get names of types defined in this module
  let module_type_names: HashSet<String> = module_records
    .iter()
    .chain(module_enums.iter())
    .map(|&def_id| build_mangled_name_standalone(def_id, defs, namespaces, symbols, types))
    .collect();

  // Emit forward declarations for external struct types (not defined in this module)
  let mut external_structs: Vec<_> = struct_forward_decls
    .iter()
    .filter(|name| !module_type_names.contains(*name))
    .collect();
  external_structs.sort();

  if !external_structs.is_empty() {
    for name in external_structs {
      writeln!(output, "typedef struct {} {};", name, name).unwrap();
    }
    writeln!(output).unwrap();
  }

  // Emit full struct definitions for enums first (they may be used as fields in records).
  // Each definition is wrapped in an include guard so that if another module header
  // also emits the same type (as an external dependency), the second definition is skipped.
  if !module_enums.is_empty() {
    let mut sorted_enums: Vec<_> = module_enums.iter().collect();
    sorted_enums.sort_by_key(|id| id.index());

    for &def_id in sorted_enums {
      let def = defs.get(&def_id);
      if let DefinitionKind::Enum(ed) = &def.kind {
        let name = build_mangled_name_standalone(def_id, defs, namespaces, symbols, types);
        let type_guard = format!("IGNIS_TYPE_DEF_{}", sanitize_macro_name(&name));
        writeln!(output, "#ifndef {}", type_guard).unwrap();
        writeln!(output, "#define {}", type_guard).unwrap();
        emit_enum_definition_standalone(def_id, ed, defs, symbols, namespaces, types, &mut output);
        writeln!(output, "#endif // {}", type_guard).unwrap();
      }
    }
  }

  // Emit full struct definitions for records defined in this module (also guarded).
  if !module_records.is_empty() {
    let mut sorted_records: Vec<_> = module_records.iter().collect();
    sorted_records.sort_by_key(|id| id.index());

    for &def_id in sorted_records {
      let def = defs.get(&def_id);
      if let DefinitionKind::Record(rd) = &def.kind {
        let name = build_mangled_name_standalone(def_id, defs, namespaces, symbols, types);
        let type_guard = format!("IGNIS_TYPE_DEF_{}", sanitize_macro_name(&name));
        writeln!(output, "#ifndef {}", type_guard).unwrap();
        writeln!(output, "#define {}", type_guard).unwrap();
        emit_record_definition_standalone(def_id, rd, defs, types, symbols, namespaces, &mut output);
        writeln!(output, "#endif // {}", type_guard).unwrap();
      }
    }
  }

  if !module_enums.is_empty() || !module_records.is_empty() {
    writeln!(output).unwrap();
  }

  // Emit complete definitions for external, concrete struct types used by this module.
  // This is required for by-value usage in signatures (e.g. Option<TokenType>), where
  // forward declarations alone are insufficient.
  let external_definition_ids =
    collect_external_type_definition_ids(defs, types, symbols, namespaces, &module_type_names, &struct_forward_decls);

  for def_id in external_definition_ids {
    let name = build_mangled_name_standalone(def_id, defs, namespaces, symbols, types);
    let guard_name = format!("IGNIS_TYPE_DEF_{}", sanitize_macro_name(&name));

    writeln!(output, "#ifndef {}", guard_name).unwrap();
    writeln!(output, "#define {}", guard_name).unwrap();

    let def = defs.get(&def_id);
    match &def.kind {
      DefinitionKind::Record(rd) => {
        emit_record_definition_standalone(def_id, rd, defs, types, symbols, namespaces, &mut output);
      },
      DefinitionKind::Enum(ed) => {
        emit_enum_definition_standalone(def_id, ed, defs, symbols, namespaces, types, &mut output);
      },
      _ => {},
    }

    writeln!(output, "#endif // {}", guard_name).unwrap();
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
  let has_payloads = ed.variants.iter().any(|v| !v.payload.is_empty());

  if has_payloads {
    writeln!(output, "    union {{").unwrap();
    for variant in &ed.variants {
      if !variant.payload.is_empty() {
        writeln!(output, "        struct {{").unwrap();
        for (i, &payload_type_id) in variant.payload.iter().enumerate() {
          let payload_type = format_c_type(types.get(&payload_type_id), types, defs, symbols, namespaces);
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
    Type::Param { .. } | Type::Instance { .. } | Type::Infer => false,
    Type::Pointer { inner, .. } | Type::Reference { inner, .. } => {
      is_type_fully_monomorphized_standalone(*inner, types)
    },
    Type::Vector { element, .. } => is_type_fully_monomorphized_standalone(*element, types),
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
    Type::Tuple(elements) => {
      for element in elements {
        collect_struct_dependencies_from_type(*element, types, defs, symbols, namespaces, out);
      }
    },
    Type::Pointer { .. } | Type::Reference { .. } | Type::Vector { .. } | Type::Function { .. } => {
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
  let mut referenced_definitions: std::collections::HashMap<String, DefinitionId> = std::collections::HashMap::new();

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

    if referenced_struct_names.contains(&name) && !module_type_names.contains(&name) {
      referenced_definitions.insert(name, def_id);
    }
  }

  if referenced_definitions.is_empty() {
    return Vec::new();
  }

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
    Type::Pointer { inner, .. } | Type::Reference { inner, .. } => {
      collect_struct_types_from_type(inner, types, defs, symbols, namespaces, out);
    },
    Type::Vector { element, .. } => {
      collect_struct_types_from_type(element, types, defs, symbols, namespaces, out);
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

  if is_extern || !type_params_empty {
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

  // Check for overloads to determine if we need a parameter suffix
  let has_overloads = has_overloads_standalone(def_id, defs);
  let param_suffix = if has_overloads {
    format_param_types_for_mangling_standalone(def_id, defs, types, symbols)
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

fn format_param_types_for_mangling_standalone(
  def_id: DefinitionId,
  defs: &DefinitionStore,
  types: &TypeStore,
  symbols: &SymbolTable,
) -> String {
  match &defs.get(&def_id).kind {
    DefinitionKind::Function(fd) => fd
      .params
      .iter()
      .map(|p| format_type_for_mangling_standalone(defs.type_of(p), types, defs, symbols))
      .collect::<Vec<_>>()
      .join("_"),
    DefinitionKind::Method(md) => {
      let start = if md.is_static { 0 } else { 1 };
      md.params[start..]
        .iter()
        .map(|p| format_type_for_mangling_standalone(defs.type_of(p), types, defs, symbols))
        .collect::<Vec<_>>()
        .join("_")
    },
    _ => String::new(),
  }
}

fn format_type_for_mangling_standalone(
  ty: &TypeId,
  types: &TypeStore,
  defs: &DefinitionStore,
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
      format!("ptr_{}", format_type_for_mangling_standalone(inner, types, defs, symbols))
    },
    Type::Reference { inner, mutable: true } => {
      format!("mutref_{}", format_type_for_mangling_standalone(inner, types, defs, symbols))
    },
    Type::Reference { inner, mutable: false } => {
      format!("ref_{}", format_type_for_mangling_standalone(inner, types, defs, symbols))
    },
    Type::Vector { element, .. } => {
      format!("vec_{}", format_type_for_mangling_standalone(element, types, defs, symbols))
    },
    Type::Tuple(elems) => {
      let parts: Vec<_> = elems
        .iter()
        .map(|e| format_type_for_mangling_standalone(e, types, defs, symbols))
        .collect();
      format!("tup_{}", parts.join("_"))
    },
    Type::Record(def_id) | Type::Enum(def_id) => {
      let def = defs.get(def_id);
      symbols.get(&def.name).to_string()
    },
    Type::Instance { generic, args } => {
      let def = defs.get(generic);
      let base = symbols.get(&def.name).to_string();
      if args.is_empty() {
        base
      } else {
        let args_str = args
          .iter()
          .map(|a| format_type_for_mangling_standalone(a, types, defs, symbols))
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
    Type::Char => "char".to_string(),
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
    Type::Vector { element, .. } => {
      format!("{}*", format_c_type(types.get(element), types, defs, symbols, namespaces))
    },
    Type::Tuple(_) => "void*".to_string(),
    Type::Function { .. } => "void*".to_string(),
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
    Type::Instance { .. } => {
      // Type::Instance should never reach codegen (Invariant D)
      panic!("ICE: Type::Instance reached C codegen - monomorphization failed")
    },
    Type::Infer => {
      panic!("ICE: Type::Infer reached C codegen after implicit type removal")
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
  use ignis_type::definition::{DefinitionKind, Visibility};
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

  for def in defs.get_all() {
    if def.visibility != Visibility::Public {
      continue;
    }

    if let DefinitionKind::Function(func_def) = &def.kind {
      // Skip extern functions - they're declared in runtime headers
      if func_def.is_extern {
        continue;
      }

      let name = symbols.get(&def.name);

      // Skip duplicates (imported functions may appear multiple times)
      if emitted_names.contains(name) {
        continue;
      }
      emitted_names.insert(name.to_string());

      let return_ty = format_c_type(types.get(&func_def.return_type), types, defs, symbols, namespaces);

      let mut param_strs = Vec::new();
      for param_id in &func_def.params {
        let param_def = defs.get(param_id);
        if let DefinitionKind::Parameter(param) = &param_def.kind {
          let param_name = symbols.get(&param_def.name);
          let param_ty = format_c_type(types.get(&param.type_id), types, defs, symbols, namespaces);
          param_strs.push(format!("{} {}", param_ty, param_name));
        }
      }

      if func_def.is_variadic {
        param_strs.push("...".to_string());
      }

      let params = if param_strs.is_empty() {
        "void".to_string()
      } else {
        param_strs.join(", ")
      };

      writeln!(output, "{} {}({});", return_ty, name, params).unwrap();
    }
  }

  writeln!(output).unwrap();
  writeln!(output, "#endif // IGNIS_STD_H").unwrap();

  output
}

#[cfg(test)]
mod tests {
  use super::*;
  use ignis_type::symbol::SymbolTable;
  use std::cell::RefCell;
  use std::rc::Rc;

  fn empty_program() -> (LirProgram, TypeStore, DefinitionStore, NamespaceStore, Rc<RefCell<SymbolTable>>) {
    let program = LirProgram::new();
    let types = TypeStore::new();
    let defs = DefinitionStore::new();
    let namespaces = NamespaceStore::new();
    let symbols = Rc::new(RefCell::new(SymbolTable::new()));
    (program, types, defs, namespaces, symbols)
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
}
