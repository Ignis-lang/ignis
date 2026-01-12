use std::fmt::Write;

use ignis_config::CHeader;
use ignis_hir::operation::{BinaryOperation, UnaryOperation};
use ignis_lir::{Block, ConstValue, FunctionLir, Instr, LirProgram, Operand, Terminator};
use ignis_type::{
  definition::{DefinitionId, DefinitionKind, DefinitionStore},
  namespace::NamespaceStore,
  symbol::SymbolTable,
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
    self.emit_extern_declarations();
    self.emit_forward_declarations();
    self.emit_functions();
    self.output
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
      Instr::Copy { dest, source } => {
        let s = self.format_operand(func, source);

        writeln!(self.output, "t{} = {};", dest.index(), s).unwrap();
      },
      Instr::BinOp { dest, op, left, right } => {
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
        let target = self.types.get(target_type);

        // Boxing to `unknown` requires calling runtime boxing function
        if matches!(target, Type::Unknown) {
          let source_type = self.operand_type(func, source);
          let boxing_fn = self.unknown_boxing_function(&source_type);
          writeln!(self.output, "t{} = {}({});", dest.index(), boxing_fn, s).unwrap();
        // Unboxing from `any` requires dereference since C can't cast pointer to float
        } else if self.is_any_type(func, source) {
          writeln!(self.output, "t{} = *({}*)({});", dest.index(), ty, s).unwrap();
        } else {
          writeln!(self.output, "t{} = ({})({});", dest.index(), ty, s).unwrap();
        }
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
        let s = self.format_operand(func, source);
        writeln!(self.output, "t{} = ({}).type_id;", dest.index(), s).unwrap();
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
          Type::Unknown => {
            writeln!(self.output, "ignis_drop_unknown(l{});", local.index()).unwrap();
          },
          _ => {
            // Should not happen if LIR verification passed
            writeln!(self.output, "/* drop l{}: non-droppable type */", local.index()).unwrap();
          },
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

  fn is_any_type(
    &self,
    func: &FunctionLir,
    op: &Operand,
  ) -> bool {
    self.operand_type(func, op).map_or(false, |t| match self.types.get(&t) {
      Type::Unknown => true,
      Type::Reference { inner, .. } | Type::Pointer(inner) => {
        matches!(self.types.get(inner), Type::Unknown)
      },
      _ => false,
    })
  }

  fn unknown_boxing_function(
    &self,
    source_type: &Option<TypeId>,
  ) -> &'static str {
    match source_type {
      Some(ty) => match self.types.get(ty) {
        Type::I8 => "ignis_unknown_i8",
        Type::I16 => "ignis_unknown_i16",
        Type::I32 => "ignis_unknown_i32",
        Type::I64 => "ignis_unknown_i64",
        Type::U8 => "ignis_unknown_u8",
        Type::U16 => "ignis_unknown_u16",
        Type::U32 => "ignis_unknown_u32",
        Type::U64 => "ignis_unknown_u64",
        Type::F32 => "ignis_unknown_f32",
        Type::F64 => "ignis_unknown_f64",
        Type::Boolean => "ignis_unknown_bool",
        Type::String => "ignis_unknown_obj",
        Type::Pointer(_) | Type::Reference { .. } => "ignis_unknown_rawptr",
        _ => "ignis_unknown_obj",
      },
      None => "ignis_unknown_obj",
    }
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
      ConstValue::Null(_) => "NULL".to_string(),
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
      Type::Unknown => "IgnisUnknown".to_string(),
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

    match def.owner_namespace {
      Some(ns_id) => {
        let ns_path = self.namespaces.full_path(ns_id);
        let mut parts: Vec<String> = ns_path
          .iter()
          .map(|s| Self::escape_ident(self.symbols.get(s)))
          .collect();
        parts.push(Self::escape_ident(&raw_name));
        parts.join("_")
      },
      None => Self::escape_ident(&raw_name),
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
    Type::Unknown => "IgnisUnknown".to_string(),
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
