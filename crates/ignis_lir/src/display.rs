use std::fmt::Write;

use ignis_type::{
  definition::DefinitionStore,
  symbol::SymbolTable,
  types::{Type, TypeStore},
};

use crate::{Block, ConstValue, FunctionLir, Instr, LirProgram, Operand, Terminator};

/// Pretty printer for LIR programs.
pub struct LirPrinter<'a> {
  program: &'a LirProgram,
  types: &'a TypeStore,
  defs: &'a DefinitionStore,
  symbols: &'a SymbolTable,
  output: String,
}

impl<'a> LirPrinter<'a> {
  pub fn new(
    program: &'a LirProgram,
    types: &'a TypeStore,
    defs: &'a DefinitionStore,
    symbols: &'a SymbolTable,
  ) -> Self {
    Self {
      program,
      types,
      defs,
      symbols,
      output: String::new(),
    }
  }

  pub fn print(mut self) -> String {
    writeln!(self.output, "=== LIR ===\n").unwrap();

    if let Some(entry) = self.program.entry_point {
      let name = self.def_name(entry);
      writeln!(self.output, "Entry: {}\n", name).unwrap();
    }

    // Sort functions by definition ID for stable output
    let mut funcs: Vec<_> = self.program.functions.iter().collect();
    funcs.sort_by_key(|(def_id, _)| def_id.index());

    for (def_id, func) in funcs {
      self.print_function(*def_id, func);
    }

    self.output
  }

  fn print_function(
    &mut self,
    def_id: ignis_type::definition::DefinitionId,
    func: &FunctionLir,
  ) {
    let name = self.def_name(def_id);
    let ret_ty = self.format_type(func.return_type);

    let params: Vec<_> = func
      .params
      .iter()
      .map(|&p| {
        let pname = self.def_name(p);
        let pty = self.format_type(*self.defs.type_of(&p));
        format!("{}: {}", pname, pty)
      })
      .collect();

    let mut flags = Vec::new();
    if func.is_extern {
      flags.push("extern");
    }
    if func.is_variadic {
      flags.push("variadic");
    }
    let flags_str = if flags.is_empty() {
      String::new()
    } else {
      format!(" [{}]", flags.join(", "))
    };

    writeln!(self.output, "fn {}({}): {}{} {{", name, params.join(", "), ret_ty, flags_str).unwrap();

    // Print locals
    if !func.locals.get_all().is_empty() {
      writeln!(self.output, "  locals:").unwrap();
      for (idx, local) in func.locals.get_all().iter().enumerate() {
        let lname = local.name.as_deref().unwrap_or("_");
        let lty = self.format_type(local.ty);
        let mut_str = if local.mutable { " [mut]" } else { "" };
        writeln!(self.output, "    %{}: {} = {}{}", idx, lty, lname, mut_str).unwrap();
      }
    }

    // Print blocks
    for block in func.blocks.get_all().iter() {
      writeln!(self.output).unwrap();
      self.print_block(func, block);
    }

    writeln!(self.output, "}}\n").unwrap();
  }

  fn print_block(
    &mut self,
    func: &FunctionLir,
    block: &Block,
  ) {
    writeln!(self.output, "  {}:", block.label).unwrap();

    for instr in &block.instructions {
      write!(self.output, "    ").unwrap();
      self.print_instr(func, instr);
    }

    write!(self.output, "    ").unwrap();
    self.print_terminator(func, &block.terminator);
  }

  fn print_instr(
    &mut self,
    func: &FunctionLir,
    instr: &Instr,
  ) {
    match instr {
      Instr::Load { dest, source } => {
        let ty = self.format_type(func.temp_type(*dest));
        writeln!(self.output, "t{} = load %{} : {}", dest.index(), source.index(), ty).unwrap();
      },
      Instr::Store { dest, value } => {
        let val = self.format_operand(func, value);
        writeln!(self.output, "store %{}, {}", dest.index(), val).unwrap();
      },
      Instr::LoadPtr { dest, ptr } => {
        let p = self.format_operand(func, ptr);
        let ty = self.format_type(func.temp_type(*dest));
        writeln!(self.output, "t{} = load_ptr {} : {}", dest.index(), p, ty).unwrap();
      },
      Instr::StorePtr { ptr, value } => {
        let p = self.format_operand(func, ptr);
        let v = self.format_operand(func, value);
        writeln!(self.output, "store_ptr {}, {}", p, v).unwrap();
      },
      Instr::BuiltinLoad { dest, ptr, ty } => {
        let p = self.format_operand(func, ptr);
        let ty_str = self.format_type(*ty);
        writeln!(self.output, "t{} = builtin_load {} : {}", dest.index(), p, ty_str).unwrap();
      },
      Instr::BuiltinStore { ptr, value, ty } => {
        let p = self.format_operand(func, ptr);
        let v = self.format_operand(func, value);
        let ty_str = self.format_type(*ty);
        writeln!(self.output, "builtin_store {} <- {} : {}", p, v, ty_str).unwrap();
      },
      Instr::Copy { dest, source } => {
        let s = self.format_operand(func, source);
        let ty = self.format_type(func.temp_type(*dest));
        writeln!(self.output, "t{} = {} : {}", dest.index(), s, ty).unwrap();
      },
      Instr::BinOp { dest, op, left, right } => {
        let l = self.format_operand(func, left);
        let r = self.format_operand(func, right);
        let ty = self.format_type(func.temp_type(*dest));
        writeln!(self.output, "t{} = {:?} {}, {} : {}", dest.index(), op, l, r, ty).unwrap();
      },
      Instr::UnaryOp { dest, op, operand } => {
        let o = self.format_operand(func, operand);
        let ty = self.format_type(func.temp_type(*dest));
        writeln!(self.output, "t{} = {:?} {} : {}", dest.index(), op, o, ty).unwrap();
      },
      Instr::Call { dest, callee, args } => {
        let name = self.def_name(*callee);
        let args_str: Vec<_> = args.iter().map(|a| self.format_operand(func, a)).collect();

        if let Some(d) = dest {
          let ty = self.format_type(func.temp_type(*d));
          writeln!(self.output, "t{} = call {}({}) : {}", d.index(), name, args_str.join(", "), ty).unwrap();
        } else {
          writeln!(self.output, "call {}({})", name, args_str.join(", ")).unwrap();
        }
      },
      Instr::Cast {
        dest,
        source,
        target_type,
      } => {
        let s = self.format_operand(func, source);
        let ty = self.format_type(*target_type);
        writeln!(self.output, "t{} = cast {} as {}", dest.index(), s, ty).unwrap();
      },
      Instr::BitCast {
        dest,
        source,
        target_type,
      } => {
        let s = self.format_operand(func, source);
        let ty = self.format_type(*target_type);
        writeln!(self.output, "t{} = bitcast {} as {}", dest.index(), s, ty).unwrap();
      },
      Instr::AddrOfLocal { dest, local, mutable } => {
        let ty = self.format_type(func.temp_type(*dest));
        let ref_kind = if *mutable { "&mut" } else { "&" };
        writeln!(self.output, "t{} = {} %{} : {}", dest.index(), ref_kind, local.index(), ty).unwrap();
      },
      Instr::GetElementPtr {
        dest,
        base,
        index,
        element_type,
      } => {
        let b = self.format_operand(func, base);
        let i = self.format_operand(func, index);
        let ty = self.format_type(*element_type);
        writeln!(self.output, "t{} = gep {}[{}] : *{}", dest.index(), b, i, ty).unwrap();
      },
      Instr::InitVector {
        dest_ptr,
        elements,
        element_type,
      } => {
        let p = self.format_operand(func, dest_ptr);
        let elems: Vec<_> = elements.iter().map(|e| self.format_operand(func, e)).collect();
        let ty = self.format_type(*element_type);
        writeln!(self.output, "init_vec {}, [{}] : {}", p, elems.join(", "), ty).unwrap();
      },
      Instr::Nop => {
        writeln!(self.output, "nop").unwrap();
      },
      Instr::RuntimeCall { name, args } => {
        let args_str: Vec<_> = args.iter().map(|a| self.format_operand(func, a)).collect();
        writeln!(self.output, "runtime_call {}({})", name, args_str.join(", ")).unwrap();
      },
      Instr::TypeIdOf { dest, source } => {
        let s = self.format_operand(func, source);
        writeln!(self.output, "t{} = type_id_of {} : u32", dest.index(), s).unwrap();
      },
      Instr::SizeOf { dest, ty } => {
        let ty_str = self.format_type(*ty);
        writeln!(self.output, "t{} = sizeof({}) : u64", dest.index(), ty_str).unwrap();
      },
      Instr::AlignOf { dest, ty } => {
        let ty_str = self.format_type(*ty);
        writeln!(self.output, "t{} = alignof({}) : u64", dest.index(), ty_str).unwrap();
      },
      Instr::MaxOf { dest, ty } => {
        let ty_str = self.format_type(*ty);
        writeln!(self.output, "t{} = maxof({}) : {}", dest.index(), ty_str, ty_str).unwrap();
      },
      Instr::MinOf { dest, ty } => {
        let ty_str = self.format_type(*ty);
        writeln!(self.output, "t{} = minof({}) : {}", dest.index(), ty_str, ty_str).unwrap();
      },
      Instr::Drop { local } => {
        writeln!(self.output, "drop l{}", local.index()).unwrap();
      },
      Instr::GetFieldPtr {
        dest,
        base,
        field_index,
        field_type,
      } => {
        let b = self.format_operand(func, base);
        let ty = self.format_type(*field_type);
        writeln!(self.output, "t{} = get_field_ptr {}.{} : *{}", dest.index(), b, field_index, ty).unwrap();
      },
      Instr::InitRecord {
        dest_ptr,
        fields,
        record_type,
      } => {
        let p = self.format_operand(func, dest_ptr);
        let fields_str: Vec<_> = fields
          .iter()
          .map(|(idx, op)| format!("{}: {}", idx, self.format_operand(func, op)))
          .collect();
        let ty = self.format_type(*record_type);
        writeln!(self.output, "init_record {}, {{ {} }} : {}", p, fields_str.join(", "), ty).unwrap();
      },
      Instr::InitEnumVariant {
        dest_ptr,
        enum_type,
        variant_tag,
        payload,
      } => {
        let p = self.format_operand(func, dest_ptr);
        let payload_str: Vec<_> = payload.iter().map(|op| self.format_operand(func, op)).collect();
        let ty = self.format_type(*enum_type);
        writeln!(
          self.output,
          "init_enum_variant {}, tag={}, payload=[{}] : {}",
          p,
          variant_tag,
          payload_str.join(", "),
          ty
        )
        .unwrap();
      },
      Instr::EnumGetTag { dest, source } => {
        let s = self.format_operand(func, source);
        writeln!(self.output, "t{} = enum_get_tag {}", dest.index(), s).unwrap();
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
          "t{} = enum_get_payload {} [tag={}, field={}]",
          dest.index(),
          s,
          variant_tag,
          field_index
        )
        .unwrap();
      },
      Instr::Trap { .. } => {
        writeln!(self.output, "    trap").unwrap();
      },
      Instr::PanicMessage { message, .. } => {
        writeln!(self.output, "    panic \"{}\"", message).unwrap();
      },
      Instr::DropInPlace { ptr, ty } => {
        let p = self.format_operand(func, ptr);
        let ty_str = self.format_type(*ty);
        writeln!(self.output, "    drop_in_place {} : {}", p, ty_str).unwrap();
      },
      Instr::DropGlue { dest, ty } => {
        let ty_str = self.format_type(*ty);
        writeln!(self.output, "    t{} = drop_glue<{}>()", dest.index(), ty_str).unwrap();
      },
    }
  }

  fn print_terminator(
    &mut self,
    func: &FunctionLir,
    term: &Terminator,
  ) {
    match term {
      Terminator::Goto(target) => {
        let label = &func.blocks.get(target).label;
        writeln!(self.output, "goto {}", label).unwrap();
      },
      Terminator::Branch {
        condition,
        then_block,
        else_block,
      } => {
        let c = self.format_operand(func, condition);
        let then_label = &func.blocks.get(then_block).label;
        let else_label = &func.blocks.get(else_block).label;
        writeln!(self.output, "br {}, {}, {}", c, then_label, else_label).unwrap();
      },
      Terminator::Return(value) => {
        if let Some(v) = value {
          let val = self.format_operand(func, v);
          writeln!(self.output, "ret {}", val).unwrap();
        } else {
          writeln!(self.output, "ret void").unwrap();
        }
      },
      Terminator::Unreachable => {
        writeln!(self.output, "unreachable").unwrap();
      },
    }
  }

  fn format_operand(
    &self,
    _func: &FunctionLir,
    op: &Operand,
  ) -> String {
    match op {
      Operand::Temp(t) => format!("t{}", t.index()),
      Operand::Local(l) => format!("l{}", l.index()),
      Operand::Const(c) => self.format_const(c),
      Operand::FuncRef(def) => format!("@{}", self.def_name(*def)),
      Operand::GlobalRef(def) => format!("${}", self.def_name(*def)),
    }
  }

  fn format_const(
    &self,
    c: &ConstValue,
  ) -> String {
    match c {
      ConstValue::Int(v, _) => format!("{}", v),
      ConstValue::UInt(v, _) => format!("{}u", v),
      ConstValue::Float(v, _) => format!("{}", v),
      ConstValue::Bool(v, _) => format!("{}", v),
      ConstValue::Char(v, _) => format!("'{}'", escape_char(*v)),
      ConstValue::String(v, _) => format!("\"{}\"", escape_string(v)),
      ConstValue::Atom(id, _) => format!(":atom#{}", id),
      ConstValue::Null(_) => "null".to_string(),
      ConstValue::Undef(_) => "undef".to_string(),
    }
  }

  fn format_type(
    &self,
    ty: ignis_type::types::TypeId,
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
      Type::String => "string".to_string(),
      Type::Atom => "atom".to_string(),
      Type::Void => "void".to_string(),
      Type::Never => "!".to_string(),
      Type::Infer => "?".to_string(),
      Type::NullPtr => "null".to_string(),
      Type::Error => "error".to_string(),
      Type::Pointer { inner, mutable } => {
        if *mutable {
          format!("*mut {}", self.format_type(*inner))
        } else {
          format!("*{}", self.format_type(*inner))
        }
      },
      Type::Reference { inner, mutable } => {
        if *mutable {
          format!("&mut {}", self.format_type(*inner))
        } else {
          format!("&{}", self.format_type(*inner))
        }
      },
      Type::Vector { element, size } => {
        format!("[{}; {}]", self.format_type(*element), size)
      },
      Type::Tuple(elems) => {
        let parts: Vec<_> = elems.iter().map(|e| self.format_type(*e)).collect();
        format!("({})", parts.join(", "))
      },
      Type::Function {
        params,
        ret,
        is_variadic,
      } => {
        let parts: Vec<_> = params.iter().map(|p| self.format_type(*p)).collect();
        let var = if *is_variadic { ", ..." } else { "" };
        format!("({}{}): {}", parts.join(", "), var, self.format_type(*ret))
      },
      Type::Record(def_id) => {
        // TODO: look up record name from definitions
        format!("record#{}", def_id.index())
      },
      Type::Enum(def_id) => {
        // TODO: look up enum name from definitions
        format!("enum#{}", def_id.index())
      },
      Type::Param { owner, index } => {
        // Type::Param should not reach LIR (Invariant A)
        format!("T{}@{}", index, owner.index())
      },
      Type::Instance { generic, args } => {
        // Type::Instance should not reach LIR (Invariant D)
        let arg_strs: Vec<_> = args.iter().map(|a| self.format_type(*a)).collect();
        format!("{}#{}<{}>", "generic", generic.index(), arg_strs.join(", "))
      },
    }
  }

  fn def_name(
    &self,
    def_id: ignis_type::definition::DefinitionId,
  ) -> String {
    let def = self.defs.get(&def_id);
    self.symbols.get(&def.name).to_string()
  }
}

/// Print a LIR program to a string.
pub fn print_lir(
  program: &LirProgram,
  types: &TypeStore,
  defs: &DefinitionStore,
  symbols: &SymbolTable,
) -> String {
  LirPrinter::new(program, types, defs, symbols).print()
}

/// Print a single function.
pub fn print_function(
  func: &FunctionLir,
  types: &TypeStore,
  defs: &DefinitionStore,
  symbols: &SymbolTable,
) -> String {
  let mut program = LirProgram::new();
  program.functions.insert(func.def_id, func.clone());
  LirPrinter::new(&program, types, defs, symbols).print()
}

fn escape_char(c: char) -> String {
  match c {
    '\n' => "\\n".to_string(),
    '\r' => "\\r".to_string(),
    '\t' => "\\t".to_string(),
    '\0' => "\\0".to_string(),
    '\\' => "\\\\".to_string(),
    '\'' => "\\'".to_string(),
    '"' => "\\\"".to_string(),
    c if c.is_ascii_graphic() || c == ' ' => c.to_string(),
    c => format!("\\x{:02x}", c as u32),
  }
}

fn escape_string(s: &str) -> String {
  let mut result = String::with_capacity(s.len());
  for c in s.chars() {
    result.push_str(&escape_char(c));
  }
  result
}
