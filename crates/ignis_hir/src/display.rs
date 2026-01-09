use std::fmt::Write;

use ignis_type::{
  definition::{DefinitionKind, DefinitionStore, Visibility},
  symbol::SymbolTable,
  types::TypeStore,
};

use crate::{HIR, HIRId, HIRKind, statement::LoopKind};

pub struct HIRPrinter<'a> {
  hir: &'a HIR,
  types: &'a TypeStore,
  defs: &'a DefinitionStore,
  symbols: &'a SymbolTable,
  indent: usize,
  output: String,
}

impl<'a> HIRPrinter<'a> {
  pub fn new(
    hir: &'a HIR,
    types: &'a TypeStore,
    defs: &'a DefinitionStore,
    symbols: &'a SymbolTable,
  ) -> Self {
    Self {
      hir,
      types,
      defs,
      symbols,
      indent: 0,
      output: String::new(),
    }
  }

  pub fn print(mut self) -> String {
    writeln!(self.output, "=== HIR ===").unwrap();
    writeln!(self.output).unwrap();

    if let Some(entry) = self.hir.entry_point {
      let def = self.defs.get(&entry);
      let name = self.symbols.get(&def.name);
      writeln!(self.output, "Entry Point: {}", name).unwrap();
    }

    self.print_declarations();
    self.print_definitions();
    self.print_variable_initializers();

    self.output
  }

  fn print_declarations(&mut self) {
    let declarations: Vec<_> = self
      .hir
      .items
      .iter()
      .filter(|def_id| {
        let def = self.defs.get(def_id);
        match &def.kind {
          DefinitionKind::Function(func_def) => func_def.is_extern && !self.hir.function_bodies.contains_key(def_id),
          DefinitionKind::Constant(_) => !self.hir.variables_inits.contains_key(def_id),
          _ => false,
        }
      })
      .collect();

    if declarations.is_empty() {
      return;
    }

    writeln!(self.output).unwrap();
    writeln!(self.output, "--- Declarations ---").unwrap();

    for def_id in declarations {
      let def = self.defs.get(def_id);
      let name = self.symbols.get(&def.name);

      match &def.kind {
        DefinitionKind::Function(func_def) => {
          let params = self.format_params(&func_def.params);
          let ret_type = self.format_type(&func_def.return_type);
          let variadic = if func_def.is_variadic { ", ..." } else { "" };

          let mut flags = Vec::new();
          if func_def.is_extern {
            flags.push("extern");
          }
          if func_def.is_variadic {
            flags.push("variadic");
          }
          if def.visibility == Visibility::Public {
            flags.push("export");
          }

          let flags_str = if flags.is_empty() {
            "".to_string()
          } else {
            format!("  [{}]", flags.join(", "))
          };

          writeln!(self.output).unwrap();
          writeln!(self.output, "fn {}({}{}): {}{}", name, params, variadic, ret_type, flags_str).unwrap();
        },
        DefinitionKind::Constant(const_def) => {
          let type_str = self.format_type(&const_def.type_id);

          writeln!(self.output).unwrap();
          writeln!(self.output, "extern const {}: {}", name, type_str).unwrap();
        },
        _ => {},
      }
    }
  }

  fn print_definitions(&mut self) {
    if self.hir.function_bodies.is_empty() {
      return;
    }

    writeln!(self.output).unwrap();
    writeln!(self.output, "--- Definitions ---").unwrap();

    let mut bodies: Vec<_> = self.hir.function_bodies.iter().collect();
    bodies.sort_by_key(|(def_id, _)| {
      let def = self.defs.get(def_id);
      self.symbols.get(&def.name).to_string()
    });

    for (def_id, body_id) in bodies {
      let def = self.defs.get(def_id);
      let name = self.symbols.get(&def.name);

      if let DefinitionKind::Function(func_def) = &def.kind {
        let params = self.format_params(&func_def.params);
        let ret_type = self.format_type(&func_def.return_type);
        let variadic = if func_def.is_variadic { ", ..." } else { "" };

        let mut flags = Vec::new();
        if func_def.is_extern {
          flags.push("extern");
        }
        if func_def.is_variadic {
          flags.push("variadic");
        }
        if def.visibility == Visibility::Public {
          flags.push("export");
        }

        let flags_str = if flags.is_empty() {
          "".to_string()
        } else {
          format!("  [{}]", flags.join(", "))
        };

        writeln!(self.output).unwrap();
        writeln!(self.output, "fn {}({}{}): {}{}", name, params, variadic, ret_type, flags_str).unwrap();
        writeln!(self.output, "  body:").unwrap();
        self.indent = 2;
        self.print_node(*body_id);
      }
    }
  }

  fn print_variable_initializers(&mut self) {
    if self.hir.variables_inits.is_empty() {
      return;
    }

    writeln!(self.output).unwrap();
    writeln!(self.output, "--- Variable Initializers ---").unwrap();
    writeln!(self.output).unwrap();

    let mut inits: Vec<_> = self.hir.variables_inits.iter().collect();
    inits.sort_by_key(|(def_id, _)| {
      let def = self.defs.get(def_id);
      self.symbols.get(&def.name).to_string()
    });

    for (def_id, init_id) in inits {
      let def = self.defs.get(def_id);
      let name = self.symbols.get(&def.name);

      if let DefinitionKind::Variable(var_def) = &def.kind {
        let mut_str = if var_def.mutable { " [mut]" } else { "" };
        let type_str = self.format_type(&var_def.type_id);

        let init_str = self.format_node_compact(*init_id);

        writeln!(self.output, "{}: {}{} = {}", name, type_str, mut_str, init_str).unwrap();
      }
    }
  }

  fn format_node_compact(
    &self,
    id: HIRId,
  ) -> String {
    let node = self.hir.get(id);
    let type_str = self.format_type(&node.type_id);

    match &node.kind {
      HIRKind::Literal(lit) => format!("Literal({:?})", lit),
      HIRKind::Variable(def_id) => {
        let def = self.defs.get(def_id);
        let name = self.symbols.get(&def.name);
        format!("Variable({})", name)
      },
      HIRKind::Call { callee, args } => {
        let def = self.defs.get(callee);
        let name = self.symbols.get(&def.name);
        let args_str: Vec<_> = args.iter().map(|a| self.format_node_compact(*a)).collect();
        format!("Call({}) {{ {} }}", name, args_str.join(", "))
      },
      HIRKind::Binary { operation, left, right } => {
        let left_str = self.format_node_compact(*left);
        let right_str = self.format_node_compact(*right);
        format!("Binary({:?}, {}, {})", operation, left_str, right_str)
      },
      HIRKind::Unary { operation, operand } => {
        let operand_str = self.format_node_compact(*operand);
        format!("Unary({:?}, {})", operation, operand_str)
      },
      HIRKind::Reference { expression, mutable } => {
        let ref_kind = if *mutable { "&mut" } else { "&" };
        let expr_str = self.format_node_compact(*expression);
        format!("{}({})", ref_kind, expr_str)
      },
      HIRKind::Dereference(expr) => {
        let expr_str = self.format_node_compact(*expr);
        format!("*({})", expr_str)
      },
      HIRKind::Cast { expression, target } => {
        let expr_str = self.format_node_compact(*expression);
        let target_str = self.format_type(target);
        format!("Cast({} as {})", expr_str, target_str)
      },
      _ => format!("<complex: {}>", type_str),
    }
  }

  fn format_params(
    &self,
    params: &[ignis_type::definition::DefinitionId],
  ) -> String {
    params
      .iter()
      .map(|param_id| {
        let param_def = self.defs.get(param_id);
        let param_name = self.symbols.get(&param_def.name);
        let param_type = self.format_type(&self.defs.type_of(param_id));

        if let DefinitionKind::Parameter(p) = &param_def.kind {
          if p.mutable {
            format!("mut {}: {}", param_name, param_type)
          } else {
            format!("{}: {}", param_name, param_type)
          }
        } else {
          format!("{}: {}", param_name, param_type)
        }
      })
      .collect::<Vec<_>>()
      .join(", ")
  }

  fn print_node(
    &mut self,
    id: HIRId,
  ) {
    let node = self.hir.get(id);
    let type_str = self.format_type(&node.type_id);

    self.write_indent();

    match &node.kind {
      HIRKind::Literal(lit) => {
        writeln!(self.output, "Literal({:?}) : {}", lit, type_str).unwrap();
      },
      HIRKind::Variable(def_id) => {
        let def = self.defs.get(def_id);
        let name = self.symbols.get(&def.name);

        let metadata = match &def.kind {
          DefinitionKind::Variable(var_def) => {
            if var_def.mutable {
              " [mut]"
            } else {
              ""
            }
          },
          DefinitionKind::Parameter(param_def) => {
            if param_def.mutable {
              " [param, mut]"
            } else {
              " [param]"
            }
          },
          DefinitionKind::Constant(_) => " [const]",
          _ => "",
        };

        writeln!(self.output, "Variable({}{}) : {}", name, metadata, type_str).unwrap();
      },
      HIRKind::Binary { operation, left, right } => {
        writeln!(self.output, "Binary({:?}) : {}", operation, type_str).unwrap();
        self.indent += 1;
        self.write_indent();
        writeln!(self.output, "left:").unwrap();
        self.indent += 1;
        self.print_node(*left);
        self.indent -= 1;
        self.write_indent();
        writeln!(self.output, "right:").unwrap();
        self.indent += 1;
        self.print_node(*right);
        self.indent -= 2;
      },
      HIRKind::Unary { operation, operand } => {
        writeln!(self.output, "Unary({:?}) : {}", operation, type_str).unwrap();
        self.indent += 1;
        self.print_node(*operand);
        self.indent -= 1;
      },
      HIRKind::Call { callee, args } => {
        let def = self.defs.get(callee);
        let name = self.symbols.get(&def.name);

        let metadata = match &def.kind {
          DefinitionKind::Function(func_def) => {
            let mut parts = Vec::new();
            if func_def.is_extern {
              parts.push("extern");
            }
            if func_def.is_variadic {
              parts.push("variadic");
            }
            if !parts.is_empty() {
              format!(" [{}]", parts.join(", "))
            } else {
              "".to_string()
            }
          },
          _ => "".to_string(),
        };

        writeln!(self.output, "Call({}{}) : {}", name, metadata, type_str).unwrap();
        if !args.is_empty() {
          self.indent += 1;
          self.write_indent();
          writeln!(self.output, "args:").unwrap();
          self.indent += 1;
          for (i, arg) in args.iter().enumerate() {
            self.write_indent();
            writeln!(self.output, "[{}]:", i).unwrap();
            self.indent += 1;
            self.print_node(*arg);
            self.indent -= 1;
          }
          self.indent -= 2;
        }
      },
      HIRKind::Cast { expression, target } => {
        let target_str = self.format_type(target);
        writeln!(self.output, "Cast(-> {}) : {}", target_str, type_str).unwrap();
        self.indent += 1;
        self.print_node(*expression);
        self.indent -= 1;
      },
      HIRKind::Reference { expression, mutable } => {
        let ref_kind = if *mutable { "&mut" } else { "&" };
        writeln!(self.output, "Reference({}) : {}", ref_kind, type_str).unwrap();
        self.indent += 1;
        self.print_node(*expression);
        self.indent -= 1;
      },
      HIRKind::Dereference(expr) => {
        writeln!(self.output, "Dereference(*) : {}", type_str).unwrap();
        self.indent += 1;
        self.print_node(*expr);
        self.indent -= 1;
      },
      HIRKind::Index { base, index } => {
        writeln!(self.output, "Index : {}", type_str).unwrap();
        self.indent += 1;
        self.write_indent();
        writeln!(self.output, "base:").unwrap();
        self.indent += 1;
        self.print_node(*base);
        self.indent -= 1;
        self.write_indent();
        writeln!(self.output, "index:").unwrap();
        self.indent += 1;
        self.print_node(*index);
        self.indent -= 2;
      },
      HIRKind::VectorLiteral { elements } => {
        writeln!(self.output, "VectorLiteral : {}", type_str).unwrap();
        if !elements.is_empty() {
          self.indent += 1;
          for (i, elem) in elements.iter().enumerate() {
            self.write_indent();
            writeln!(self.output, "[{}]:", i).unwrap();
            self.indent += 1;
            self.print_node(*elem);
            self.indent -= 1;
          }
          self.indent -= 1;
        }
      },
      HIRKind::Let { name, value } => {
        let def = self.defs.get(name);
        let var_name = self.symbols.get(&def.name);

        let mut_str = match &def.kind {
          DefinitionKind::Variable(var_def) if var_def.mutable => " [mut]",
          _ => "",
        };

        writeln!(self.output, "Let({}{}) : {}", var_name, mut_str, type_str).unwrap();
        if let Some(val) = value {
          self.indent += 1;
          self.write_indent();
          writeln!(self.output, "init:").unwrap();
          self.indent += 1;
          self.print_node(*val);
          self.indent -= 2;
        }
      },
      HIRKind::Assign {
        target,
        value,
        operation,
      } => {
        let op_str = if let Some(op) = operation {
          format!("{:?}=", op)
        } else {
          "=".to_string()
        };
        writeln!(self.output, "Assign({}) : {}", op_str, type_str).unwrap();
        self.indent += 1;
        self.write_indent();
        writeln!(self.output, "target:").unwrap();
        self.indent += 1;
        self.print_node(*target);
        self.indent -= 1;
        self.write_indent();
        writeln!(self.output, "value:").unwrap();
        self.indent += 1;
        self.print_node(*value);
        self.indent -= 2;
      },
      HIRKind::Block { statements, expression } => {
        writeln!(self.output, "Block : {}", type_str).unwrap();
        self.indent += 1;
        if !statements.is_empty() {
          self.write_indent();
          writeln!(self.output, "statements:").unwrap();
          self.indent += 1;
          for stmt in statements {
            self.print_node(*stmt);
          }
          self.indent -= 1;
        }
        if let Some(expr) = expression {
          self.write_indent();
          writeln!(self.output, "expr:").unwrap();
          self.indent += 1;
          self.print_node(*expr);
          self.indent -= 1;
        }
        self.indent -= 1;
      },
      HIRKind::If {
        condition,
        then_branch,
        else_branch,
      } => {
        writeln!(self.output, "If : {}", type_str).unwrap();
        self.indent += 1;
        self.write_indent();
        writeln!(self.output, "cond:").unwrap();
        self.indent += 1;
        self.print_node(*condition);
        self.indent -= 1;
        self.write_indent();
        writeln!(self.output, "then:").unwrap();
        self.indent += 1;
        self.print_node(*then_branch);
        self.indent -= 1;
        if let Some(else_br) = else_branch {
          self.write_indent();
          writeln!(self.output, "else:").unwrap();
          self.indent += 1;
          self.print_node(*else_br);
          self.indent -= 1;
        }
        self.indent -= 1;
      },
      HIRKind::Loop { condition, body } => {
        let loop_kind = match condition {
          LoopKind::Infinite => "infinite",
          LoopKind::While { .. } => "while",
          LoopKind::For { .. } => "for",
        };
        writeln!(self.output, "Loop({}) : {}", loop_kind, type_str).unwrap();
        self.indent += 1;

        match condition {
          LoopKind::While { condition } => {
            self.write_indent();
            writeln!(self.output, "cond:").unwrap();
            self.indent += 1;
            self.print_node(*condition);
            self.indent -= 1;
          },
          LoopKind::For {
            init,
            condition,
            update,
          } => {
            if let Some(init_id) = init {
              self.write_indent();
              writeln!(self.output, "init:").unwrap();
              self.indent += 1;
              self.print_node(*init_id);
              self.indent -= 1;
            }
            if let Some(cond_id) = condition {
              self.write_indent();
              writeln!(self.output, "cond:").unwrap();
              self.indent += 1;
              self.print_node(*cond_id);
              self.indent -= 1;
            }
            if let Some(update_id) = update {
              self.write_indent();
              writeln!(self.output, "update:").unwrap();
              self.indent += 1;
              self.print_node(*update_id);
              self.indent -= 1;
            }
          },
          LoopKind::Infinite => {},
        }

        self.write_indent();
        writeln!(self.output, "body:").unwrap();
        self.indent += 1;
        self.print_node(*body);
        self.indent -= 2;
      },
      HIRKind::Break => {
        writeln!(self.output, "Break : {}", type_str).unwrap();
      },
      HIRKind::Continue => {
        writeln!(self.output, "Continue : {}", type_str).unwrap();
      },
      HIRKind::Return(value) => {
        writeln!(self.output, "Return : {}", type_str).unwrap();
        if let Some(val) = value {
          self.indent += 1;
          self.print_node(*val);
          self.indent -= 1;
        }
      },
      HIRKind::ExpressionStatement(expr) => {
        writeln!(self.output, "ExprStmt : {}", type_str).unwrap();
        self.indent += 1;
        self.print_node(*expr);
        self.indent -= 1;
      },
      HIRKind::Error => {
        writeln!(self.output, "Error : {}", type_str).unwrap();
      },
    }
  }

  fn write_indent(&mut self) {
    for _ in 0..self.indent {
      write!(self.output, "  ").unwrap();
    }
  }

  fn format_type(
    &self,
    type_id: &ignis_type::types::TypeId,
  ) -> String {
    use ignis_type::types::Type;

    let ty = self.types.get(type_id);
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
      Type::Boolean => "bool".to_string(),
      Type::Char => "char".to_string(),
      Type::String => "string".to_string(),
      Type::Void => "void".to_string(),
      Type::Never => "never".to_string(),
      Type::Unknown => "unknown".to_string(),
      Type::Error => "error".to_string(),
      Type::Pointer(inner) => format!("*{}", self.format_type(inner)),
      Type::Reference { inner, mutable } => {
        if *mutable {
          format!("&mut {}", self.format_type(inner))
        } else {
          format!("&{}", self.format_type(inner))
        }
      },
      Type::Vector { element, size } => {
        if let Some(s) = size {
          format!("[{}; {}]", self.format_type(element), s)
        } else {
          format!("[{}]", self.format_type(element))
        }
      },
      Type::Tuple(elements) => {
        let elem_strs: Vec<_> = elements.iter().map(|e| self.format_type(e)).collect();
        format!("({})", elem_strs.join(", "))
      },
      Type::Function {
        params,
        ret,
        is_variadic,
      } => {
        let param_strs: Vec<_> = params.iter().map(|p| self.format_type(p)).collect();
        let variadic = if *is_variadic { ", ..." } else { "" };
        format!("fn({}{}) -> {}", param_strs.join(", "), variadic, self.format_type(ret))
      },
    }
  }
}

pub fn print_hir(
  hir: &HIR,
  types: &TypeStore,
  defs: &DefinitionStore,
  symbols: &SymbolTable,
) -> String {
  HIRPrinter::new(hir, types, defs, symbols).print()
}
