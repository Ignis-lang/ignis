use std::fmt::Write;

use ignis_config::CHeader;
use ignis_hir::operation::{BinaryOperation, UnaryOperation};
use ignis_lir::{Block, ConstValue, FunctionLir, Instr, LirProgram, Operand, TempId, Terminator};
use ignis_type::{
  definition::{DefinitionId, DefinitionKind, DefinitionStore, EnumDefinition, RecordDefinition},
  namespace::{NamespaceId, NamespaceStore},
  symbol::{SymbolId, SymbolTable},
  types::{Type, TypeId, TypeStore},
};

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
    }
  }

  pub fn emit(mut self) -> String {
    self.emit_headers();
    self.emit_type_forward_declarations();
    self.emit_type_definitions();
    self.emit_static_constants();
    self.emit_extern_declarations();
    self.emit_forward_declarations();
    self.emit_functions();
    self.output
  }

  /// Emit forward declarations for all record and enum types.
  /// This allows structs to reference each other (e.g., for recursive types).
  /// Skips generic definitions (those with non-empty type_params) since they
  /// are only templates - concrete instantiations are emitted instead.
  fn emit_type_forward_declarations(&mut self) {
    let mut type_defs: Vec<_> = self
      .defs
      .iter()
      .filter(|(_, def)| match &def.kind {
        DefinitionKind::Record(rd) => rd.type_params.is_empty(),
        DefinitionKind::Enum(ed) => ed.type_params.is_empty(),
        _ => false,
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
      .filter(|(_, def)| match &def.kind {
        DefinitionKind::Record(rd) => rd.type_params.is_empty(),
        DefinitionKind::Enum(ed) => ed.type_params.is_empty(),
        _ => false,
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
          // Skip generic records - they should have been monomorphized
          if rd.type_params.is_empty() {
            self.emit_record_definition(def_id, rd);
          }
        },
        DefinitionKind::Enum(ed) => {
          // Skip generic enums - they should have been monomorphized
          if ed.type_params.is_empty() {
            self.emit_enum_definition(def_id, ed);
          }
        },
        _ => {},
      }
    }
    writeln!(self.output).unwrap();
  }

  /// Emit a struct definition for a record type.
  fn emit_record_definition(
    &mut self,
    def_id: DefinitionId,
    rd: &RecordDefinition,
  ) {
    let name = self.type_struct_name(def_id);

    writeln!(self.output, "struct {} {{", name).unwrap();

    if rd.fields.is_empty() {
      // C doesn't allow empty structs, add a dummy field
      writeln!(self.output, "    char _empty;").unwrap();
    } else {
      for field in &rd.fields {
        let field_ty = self.format_type(field.type_id);
        writeln!(self.output, "    {} field_{};", field_ty, field.index).unwrap();
      }
    }

    writeln!(self.output, "}};").unwrap();
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

    writeln!(self.output, "}};").unwrap();

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
      if let DefinitionKind::Record(rd) = &def.kind {
        let type_name = self.build_mangled_name(def_id);
        for (&field_name_sym, &const_def_id) in &rd.static_fields {
          if let DefinitionKind::Constant(const_def) = &self.defs.get(&const_def_id).kind {
            if let Some(value) = &const_def.value {
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
      if let DefinitionKind::Enum(ed) = &def.kind {
        let type_name = self.build_mangled_name(def_id);
        for (&field_name_sym, &const_def_id) in &ed.static_fields {
          if let DefinitionKind::Constant(const_def) = &self.defs.get(&const_def_id).kind {
            if let Some(value) = &const_def.value {
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

    let extern_funcs: Vec<_> = funcs.iter().filter(|(_, func)| func.is_extern).collect();

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

  fn emit_forward_declarations(&mut self) {
    let mut funcs: Vec<_> = self.program.functions.iter().collect();
    funcs.sort_by_key(|(def_id, _)| def_id.index());

    // Only emit forward declarations for non-extern functions.
    // Extern functions are declared separately via emit_extern_declarations().
    for (def_id, func) in &funcs {
      if func.is_extern {
        continue;
      }
      self.emit_function_signature(**def_id, func);
      writeln!(self.output, ";").unwrap();
    }

    writeln!(self.output).unwrap();
  }

  fn emit_functions(&mut self) {
    let mut funcs: Vec<_> = self.program.functions.iter().collect();
    funcs.sort_by_key(|(def_id, _)| def_id.index());

    for (def_id, func) in funcs {
      if !func.is_extern {
        self.emit_function(*def_id, func);
      }
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
      if let Type::Vector { element, size: Some(n) } = self.types.get(&local.ty) {
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
        let val = self.format_operand(func, value);

        if let Type::Vector { size: Some(n), element } = self.types.get(&local_info.ty) {
          let elem_size = self.sizeof_type(*element);
          writeln!(self.output, "memcpy(l{}, {}, {} * {});", dest.index(), val, n, elem_size).unwrap();
        } else {
          writeln!(self.output, "l{} = {};", dest.index(), val).unwrap();
        }
      },
      Instr::LoadPtr { dest, ptr } => {
        let p = self.format_operand(func, ptr);

        writeln!(self.output, "t{} = *{};", dest.index(), p).unwrap();
      },
      Instr::StorePtr { ptr, value } => {
        let p = self.format_operand(func, ptr);
        let v = self.format_operand(func, value);

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
        let name = self.def_name(*callee);
        let args_str: Vec<_> = args.iter().map(|a| self.format_operand(func, a)).collect();

        if let Some(d) = dest {
          writeln!(self.output, "t{} = {}({});", d.index(), name, args_str.join(", ")).unwrap();
        } else {
          writeln!(self.output, "{}({});", name, args_str.join(", ")).unwrap();
        }
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

        writeln!(self.output, "t{} = ({})({});", dest.index(), ty, s).unwrap();
      },
      Instr::AddrOfLocal { dest, local, .. } => {
        // For array locals, the name already decays to a pointer in C
        let local_info = func.locals.get(local);
        if matches!(self.types.get(&local_info.ty), Type::Vector { size: Some(_), .. }) {
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
      Instr::Drop { local } => {
        let local_data = func.locals.get(local);
        let ty = local_data.ty;

        match self.types.get(&ty) {
          Type::String => {
            writeln!(self.output, "ignis_string_drop(l{});", local.index()).unwrap();
          },
          Type::Vector { size: None, .. } => {
            writeln!(self.output, "ignis_buf_drop(l{});", local.index()).unwrap();
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
        // Check if base is a pointer type
        let is_pointer = if let Some(base_ty) = self.operand_type(func, base) {
          matches!(self.types.get(&base_ty), Type::Pointer(_) | Type::Reference { .. })
        } else {
          false
        };

        // Use -> for pointers, . for values
        if is_pointer {
          writeln!(self.output, "t{} = &(({})->field_{});", dest.index(), b, field_index).unwrap();
        } else {
          writeln!(self.output, "t{} = &(({}).field_{});", dest.index(), b, field_index).unwrap();
        }
      },
      Instr::InitRecord { dest_ptr, fields, .. } => {
        let p = self.format_operand(func, dest_ptr);

        for (field_idx, field_value) in fields {
          if field_idx > &0 {
            write!(self.output, "    ").unwrap();
          }
          let v = self.format_operand(func, field_value);
          writeln!(self.output, "({})->field_{} = {};", p, field_idx, v).unwrap();
        }
      },
      Instr::InitEnumVariant {
        dest_ptr,
        variant_tag,
        payload,
        ..
      } => {
        let p = self.format_operand(func, dest_ptr);

        // Set the tag
        writeln!(self.output, "({})->tag = {};", p, variant_tag).unwrap();

        // Set payload fields
        for (i, payload_value) in payload.iter().enumerate() {
          write!(self.output, "    ").unwrap();
          let v = self.format_operand(func, payload_value);
          writeln!(self.output, "({})->payload.variant_{}.field_{} = {};", p, variant_tag, i, v).unwrap();
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
          // C main must return int - if we're in main with void return, emit return 0
          let is_main = self
            .current_fn_id
            .map_or(false, |id| Some(id) == self.program.entry_point && self.def_name(id) == "main");
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
      Operand::Const(c) => self.format_const(c),
      Operand::FuncRef(def) => self.def_name(*def),
      Operand::GlobalRef(def) => self.def_name(*def),
    }
  }

  fn operand_type(
    &self,
    func: &FunctionLir,
    op: &Operand,
  ) -> Option<TypeId> {
    match op {
      Operand::Temp(t) => Some(func.temp_type(*t)),
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
      Type::Reference { inner, .. } | Type::Pointer(inner) => self.type_contains_infer(*inner),
      _ => false,
    }
  }

  fn operand_contains_infer(
    &self,
    func: &FunctionLir,
    op: &Operand,
  ) -> bool {
    self.operand_type(func, op).map_or(false, |t| self.type_contains_infer(t))
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
        format!("{}{}", v, suffix)
      },
      ConstValue::Bool(v, _) => format!("{}", v),
      ConstValue::Char(v, _) => format!("{}", *v as u32),
      ConstValue::String(v, _) => format!("ignis_string_from_cstr(\"{}\")", Self::escape_string(v)),
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

    let is_left_ptr = left_ty.map_or(false, |ty| matches!(self.types.get(&ty), Type::Pointer(_)));
    let is_right_ptr = right_ty.map_or(false, |ty| matches!(self.types.get(&ty), Type::Pointer(_)));
    let is_right_i64 = right_ty.map_or(false, |ty| matches!(self.types.get(&ty), Type::I64));

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
      Type::String => "string".to_string(),
      Type::Void => "void".to_string(),
      Type::Never => "void".to_string(),
      Type::NullPtr => "void*".to_string(),
      Type::Error => "/* error */ void*".to_string(),
      Type::Pointer(inner) => format!("{}*", self.format_type(*inner)),
      Type::Reference { inner, .. } => format!("{}*", self.format_type(*inner)),
      Type::Vector { element, size } => {
        if size.is_some() {
          // Fixed-size array decays to pointer in most contexts
          format!("{}*", self.format_type(*element))
        } else {
          // Dynamic buffer
          "IgnisBuffer*".to_string()
        }
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

  fn def_name(
    &self,
    def_id: DefinitionId,
  ) -> String {
    let def = self.defs.get(&def_id);
    let raw_name = self.symbols.get(&def.name).to_string();

    let is_extern = match &def.kind {
      DefinitionKind::Function(f) => f.is_extern,
      DefinitionKind::Constant(c) => c.value.is_none(), // extern const has no value
      _ => false,
    };

    if is_extern {
      return Self::escape_ident(&raw_name);
    }

    if def.owner_namespace.is_none() && raw_name == "main" {
      return raw_name;
    }

    self.build_mangled_name(def_id)
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
      Type::String => "str".to_string(),
      Type::Pointer(inner) => format!("ptr_{}", self.format_type_for_mangling(inner)),
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

/// Emit C code from a LIR program.
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

/// Format a type as C type string using runtime type aliases.
pub fn format_c_type(
  ty: &Type,
  types: &TypeStore,
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
    Type::Char => "u32".to_string(),
    Type::String => "string".to_string(),
    Type::Void => "void".to_string(),
    Type::Never => "void".to_string(),
    Type::NullPtr => "void*".to_string(),
    Type::Error => "void*".to_string(),
    Type::Pointer(inner) => format!("{}*", format_c_type(types.get(inner), types)),
    Type::Reference { inner, .. } => format!("{}*", format_c_type(types.get(inner), types)),
    Type::Vector { element, size } => {
      if size.is_some() {
        format!("{}*", format_c_type(types.get(element), types))
      } else {
        "IgnisBuffer*".to_string()
      }
    },
    Type::Tuple(_) => "void*".to_string(),
    Type::Function { .. } => "void*".to_string(),
    Type::Record(_) => {
      // TODO: emit proper struct name (requires DefinitionStore)
      "void*".to_string()
    },
    Type::Enum(_) => {
      // TODO: emit proper tagged union name (requires DefinitionStore)
      "void*".to_string()
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
) -> String {
  use ignis_type::definition::{DefinitionKind, Visibility};
  use std::collections::HashSet;

  let mut output = String::new();
  let mut emitted_names: HashSet<String> = HashSet::new();

  writeln!(output, "#ifndef IGNIS_STD_H").unwrap();
  writeln!(output, "#define IGNIS_STD_H").unwrap();
  writeln!(output).unwrap();
  writeln!(output, "#include \"runtime/types/types.h\"").unwrap();
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

      let return_ty = format_c_type(types.get(&func_def.return_type), types);

      let mut param_strs = Vec::new();
      for param_id in &func_def.params {
        let param_def = defs.get(param_id);
        if let DefinitionKind::Parameter(param) = &param_def.kind {
          let param_name = symbols.get(&param_def.name);
          let param_ty = format_c_type(types.get(&param.type_id), types);
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
        path: "runtime/types/types.h".to_string(),
        quoted: true,
      },
      CHeader {
        path: "runtime/io/io.h".to_string(),
        quoted: true,
      },
      CHeader {
        path: "math.h".to_string(),
        quoted: false,
      },
    ];
    let output = emit_c(&program, &types, &defs, &namespaces, &sym, &headers);

    assert!(output.contains("#include \"runtime/types/types.h\""));
    assert!(output.contains("#include \"runtime/io/io.h\""));
    assert!(output.contains("#include <math.h>"));
  }
}
