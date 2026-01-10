use std::collections::HashSet;
use std::fmt::Write;

use ignis_hir::operation::{BinaryOperation, UnaryOperation};
use ignis_lir::{Block, ConstValue, FunctionLir, Instr, LirProgram, Operand, Terminator};
use ignis_type::{
  definition::{DefinitionId, DefinitionKind, DefinitionStore},
  symbol::SymbolTable,
  types::{Type, TypeId, TypeStore},
};

/// C code emitter for LIR programs.
pub struct CEmitter<'a> {
  program: &'a LirProgram,
  types: &'a TypeStore,
  defs: &'a DefinitionStore,
  symbols: &'a SymbolTable,
  output: String,
}

impl<'a> CEmitter<'a> {
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

  pub fn emit(mut self) -> String {
    self.emit_headers();
    self.emit_forward_declarations();
    self.emit_functions();
    self.output
  }

  fn emit_headers(&mut self) {
    writeln!(self.output, "#include <stdint.h>").unwrap();
    writeln!(self.output, "#include <stdbool.h>").unwrap();
    writeln!(self.output, "#include <math.h>").unwrap();
    writeln!(self.output).unwrap();
  }

  fn emit_forward_declarations(&mut self) {
    let extern_calls = self.collect_extern_calls();
    if !extern_calls.is_empty() {
      writeln!(self.output, "// Extern functions").unwrap();
      let mut extern_list: Vec<_> = extern_calls.iter().collect();
      extern_list.sort_by_key(|def_id| def_id.index());

      for def_id in extern_list {
        self.emit_extern_prototype(*def_id);
      }
      writeln!(self.output).unwrap();
    }

    let mut funcs: Vec<_> = self.program.functions.iter().collect();
    funcs.sort_by_key(|(def_id, _)| def_id.index());

    for (def_id, func) in &funcs {
      self.emit_function_signature(**def_id, func);
      writeln!(self.output, ";").unwrap();
    }

    writeln!(self.output).unwrap();
  }

  fn collect_extern_calls(&self) -> HashSet<DefinitionId> {
    let mut externs = HashSet::new();
    for func in self.program.functions.values() {
      for block in func.blocks.get_all() {
        for instr in &block.instructions {
          if let Instr::Call { callee, .. } = instr {
            if !self.program.functions.contains_key(callee) {
              externs.insert(*callee);
            }
          }
        }
      }
    }
    externs
  }

  fn emit_extern_prototype(
    &mut self,
    def_id: DefinitionId,
  ) {
    let def = self.defs.get(&def_id);
    let name = self.def_name(def_id);

    if let DefinitionKind::Function(func_def) = &def.kind {
      let ret_ty = self.format_type(func_def.return_type);

      let params: Vec<_> = func_def
        .params
        .iter()
        .map(|&p| self.format_type(*self.defs.type_of(&p)))
        .collect();

      let params_str = if params.is_empty() {
        "void".to_string()
      } else if func_def.is_variadic {
        format!("{}, ...", params.join(", "))
      } else {
        params.join(", ")
      };

      writeln!(self.output, "{} {}({});", ret_ty, name, params_str).unwrap();
    }
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
    let ret_ty = self.format_type(func.return_type);

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
      let ty = self.format_var_type(local.ty);
      let name = local.name.as_deref().unwrap_or("_");
      writeln!(self.output, "    {} l{}; // {}", ty, idx, name).unwrap();
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
      self.emit_instr(instr);
    }

    write!(self.output, "    ").unwrap();
    self.emit_terminator(func, &block.terminator);
  }

  fn emit_instr(
    &mut self,
    instr: &Instr,
  ) {
    match instr {
      Instr::Load { dest, source } => {
        writeln!(self.output, "t{} = l{};", dest.index(), source.index()).unwrap();
      },
      Instr::Store { dest, value } => {
        let val = self.format_operand(value);
        writeln!(self.output, "l{} = {};", dest.index(), val).unwrap();
      },
      Instr::LoadPtr { dest, ptr } => {
        let p = self.format_operand(ptr);
        writeln!(self.output, "t{} = *{};", dest.index(), p).unwrap();
      },
      Instr::StorePtr { ptr, value } => {
        let p = self.format_operand(ptr);
        let v = self.format_operand(value);
        writeln!(self.output, "*{} = {};", p, v).unwrap();
      },
      Instr::Copy { dest, source } => {
        let s = self.format_operand(source);
        writeln!(self.output, "t{} = {};", dest.index(), s).unwrap();
      },
      Instr::BinOp { dest, op, left, right } => {
        let l = self.format_operand(left);
        let r = self.format_operand(right);
        if matches!(op, BinaryOperation::Pow) {
          writeln!(self.output, "t{} = pow({}, {});", dest.index(), l, r).unwrap();
        } else {
          let op_str = self.format_binop(op);
          writeln!(self.output, "t{} = {} {} {};", dest.index(), l, op_str, r).unwrap();
        }
      },
      Instr::UnaryOp { dest, op, operand } => {
        let o = self.format_operand(operand);
        let op_str = self.format_unaryop(op);
        writeln!(self.output, "t{} = {}{};", dest.index(), op_str, o).unwrap();
      },
      Instr::Call { dest, callee, args } => {
        let name = self.def_name(*callee);
        let args_str: Vec<_> = args.iter().map(|a| self.format_operand(a)).collect();

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
        let s = self.format_operand(source);
        let ty = self.format_type(*target_type);
        writeln!(self.output, "t{} = ({})({});", dest.index(), ty, s).unwrap();
      },
      Instr::AddrOfLocal { dest, local, .. } => {
        writeln!(self.output, "t{} = &l{};", dest.index(), local.index()).unwrap();
      },
      Instr::GetElementPtr { dest, base, index, .. } => {
        let b = self.format_operand(base);
        let i = self.format_operand(index);
        writeln!(self.output, "t{} = &{}[{}];", dest.index(), b, i).unwrap();
      },
      Instr::InitVector { dest_ptr, elements, .. } => {
        let p = self.format_operand(dest_ptr);
        for (i, elem) in elements.iter().enumerate() {
          if i > 0 {
            write!(self.output, "    ").unwrap();
          }
          let e = self.format_operand(elem);
          writeln!(self.output, "{}[{}] = {};", p, i, e).unwrap();
        }
      },
      Instr::Nop => {
        writeln!(self.output, "/* nop */").unwrap();
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
        let c = self.format_operand(condition);
        let then_label = &func.blocks.get(then_block).label;
        let else_label = &func.blocks.get(else_block).label;
        writeln!(self.output, "if ({}) goto {}; else goto {};", c, then_label, else_label).unwrap();
      },
      Terminator::Return(value) => {
        if let Some(v) = value {
          let val = self.format_operand(v);
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
    op: &Operand,
  ) -> String {
    match op {
      Operand::Temp(t) => format!("t{}", t.index()),
      Operand::Const(c) => self.format_const(c),
      Operand::FuncRef(def) => self.def_name(*def),
      Operand::GlobalRef(def) => self.def_name(*def),
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
      ConstValue::Char(v, _) => format!("'{}'", v.escape_default()),
      ConstValue::String(v, _) => format!("\"{}\"", v.escape_default()),
      ConstValue::Null(_) => "NULL".to_string(),
      ConstValue::Undef(_) => "/* undef */ 0".to_string(),
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
    match self.types.get(&ty) {
      Type::I8 => "int8_t".to_string(),
      Type::I16 => "int16_t".to_string(),
      Type::I32 => "int32_t".to_string(),
      Type::I64 => "int64_t".to_string(),
      Type::U8 => "uint8_t".to_string(),
      Type::U16 => "uint16_t".to_string(),
      Type::U32 => "uint32_t".to_string(),
      Type::U64 => "uint64_t".to_string(),
      Type::F32 => "float".to_string(),
      Type::F64 => "double".to_string(),
      Type::Boolean => "bool".to_string(),
      Type::Char => "char".to_string(),
      Type::String => "const char*".to_string(),
      Type::Void => "void".to_string(),
      Type::Never => "void".to_string(),
      Type::Unknown | Type::Error => "/* unknown */ void*".to_string(),
      Type::Pointer(inner) => format!("{}*", self.format_type(*inner)),
      Type::Reference { inner, .. } => format!("{}*", self.format_type(*inner)),
      Type::Vector { element, .. } => format!("{}*", self.format_type(*element)),
      Type::Tuple(_) => "/* tuple */ void*".to_string(),
      Type::Function { .. } => "/* fn */ void*".to_string(),
    }
  }

  fn def_name(
    &self,
    def_id: DefinitionId,
  ) -> String {
    let def = self.defs.get(&def_id);
    let name = self.symbols.get(&def.name).to_string();
    name.replace("::", "_")
  }
}

/// Emit C code from a LIR program.
pub fn emit_c(
  program: &LirProgram,
  types: &TypeStore,
  defs: &DefinitionStore,
  symbols: &SymbolTable,
) -> String {
  CEmitter::new(program, types, defs, symbols).emit()
}
