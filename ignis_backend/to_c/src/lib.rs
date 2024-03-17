use std::{
  collections::HashMap,
  process::{Command, Stdio},
  fs,
  io::Write,
};

use code_result::CodeResult;
use intermediate_representation::{
  IRInstruction, variable::IRVariable, function::IRFunction, call::IRCall, ir_if::IRIf, ir_while::IRWhile,
  instruction_type::IRInstructionType, analyzer_value::AnalyzerValue,
};
use enums::data_type::DataType;

#[derive(Debug, Clone, PartialEq)]
enum TranspilerContext {
  For,
  While,
  Continue(Box<TranspilerContext>),
  Call,
  DontNeedSemicolon,
  Condition,
}

pub struct TranspilerToC {
  pub code: String,
  pub statement_exported: Vec<(String, String)>,
  pub statement_imported: HashMap<String, String>,
  context: Vec<TranspilerContext>,
  irs: HashMap<String, Vec<IRInstruction>>,
}

impl TranspilerToC {
  pub fn new(irs: HashMap<String, Vec<IRInstruction>>) -> Self {
    Self {
      code: String::new(),
      statement_exported: Vec::new(),
      statement_imported: HashMap::new(),
      context: Vec::new(),
      irs,
    }
  }

  fn transpile_operator_to_c(
    &self,
    operator: &IRInstructionType,
  ) -> String {
    match operator {
      IRInstructionType::Add => "+",
      IRInstructionType::Sub => "-",
      IRInstructionType::Mul => "*",
      IRInstructionType::Div => "/",
      IRInstructionType::GreaterEqual => ">=",
      IRInstructionType::Greater => ">",
      IRInstructionType::LessEqual => "<=",
      IRInstructionType::Less => "<",
      IRInstructionType::Equal => "==",
      IRInstructionType::NotEqual => "!=",
      IRInstructionType::And => "&&",
      IRInstructionType::Or => "||",
      IRInstructionType::Not => "!",
      IRInstructionType::Assign => "=",
      IRInstructionType::AssignAdd => "+=",
      IRInstructionType::AssignSub => "-=",
      IRInstructionType::Mod => "%",
      IRInstructionType::Concatenate => "+",
      IRInstructionType::Increment => "++",
      IRInstructionType::Decrement => "--",
    }
    .to_string()
  }

  fn get_format_string_from_data_type(
    &self,
    data_type: &DataType,
  ) -> String {
    match data_type {
      DataType::Int => "%d",
      DataType::Float => "%f",
      DataType::String => "%s",
      DataType::Boolean => "%d",
      _ => "%p",
    }
    .to_string()
  }

  fn get_format_string_from_analyzer_value(
    &self,
    analyzer_value: &AnalyzerValue,
  ) -> String {
    match analyzer_value {
      AnalyzerValue::String(_) => "%s".to_string(),
      AnalyzerValue::Int(_) => "%d".to_string(),
      AnalyzerValue::Float(_) => "%f".to_string(),
      AnalyzerValue::Boolean(_) => "%d".to_string(),
      AnalyzerValue::Return(r) => self.get_format_string_from_analyzer_value(r).to_string(),
      _ => "%p".to_string(),
    }
  }

  fn transpile_variable_to_c(
    &mut self,
    variable: &IRVariable,
    indent_level: usize,
  ) -> String {
    let var_value = if let Some(value) = &variable.value {
      match **value {
        IRInstruction::Call(_) => {
          self.context.push(TranspilerContext::Call);
          let result = self.transpile_ir_to_c(value, 0);
          self.context.pop();

          result
        },
        _ => self.transpile_ir_to_c(value, 0),
      }
    } else {
      "".to_string()
    };

    if variable.metadata.is_declaration {
      match variable.data_type {
        DataType::Array(_) => {
          format!(
            "{}{} {}[] = {};\n",
            " ".repeat(indent_level),
            variable.data_type.to_c_type(variable.metadata.is_mutable),
            variable.name,
            var_value
          )
        },
        _ => {
          format!(
            "{}{} {} = {};\n",
            " ".repeat(indent_level),
            variable.data_type.to_c_type(variable.metadata.is_mutable),
            variable.name,
            var_value
          )
        },
      }
    } else {
      variable.name.to_string()
    }
  }

  fn transpile_function_to_c(
    &mut self,
    func: &IRFunction,
    indent_level: usize,
  ) -> String {
    let mut code = String::new();

    if func.metadata.is_extern {
      return code;
    }

    if let Some(body) = &func.body {
      // let parameters = func
      //   .parameters
      //   .iter()
      //   .map(|x| {
      //     format!(
      //       "{} {}",
      //       x.data_type.to_c_type(x.metadata.is_mutable),
      //       x.name.clone()
      //     )
      //   })
      //   .collect::<Vec<String>>()
      //   .join(", ");

      if func.metadata.is_exported {
        self
          .statement_exported
          .push((func.name.span.literal.clone(), String::new()));
      }

      if func.metadata.is_imported {
        return code;
      }

      // code.push_str(&format!(
      //   "{}{} {}({}) {{\n",
      //   " ".repeat(indent_level),
      //   func.return_type.to_c_type(true),
      //   func.name,
      //   parameters
      // ));

      for instr in &body.instructions {
        code.push_str(&self.transpile_ir_to_c(instr, indent_level + 2));
      }

      code.push_str(format!("{}}}\n", " ".repeat(indent_level)).as_str());
    }

    code
  }

  fn transpile_print_to_c(
    &mut self,
    call: &IRCall,
    indent_level: usize,
  ) -> String {
    let mut code: String = String::new();
    let mut format_string: String = String::new();
    let mut args: String = String::new();

    for arg in &call.arguments {
      match arg {
        IRInstruction::Binary(b) => {
          format_string.push_str(&self.get_format_string_from_data_type(&b.data_type));
          args.push_str(&self.transpile_ir_to_c(arg, 0));
        },
        IRInstruction::Block(_) => todo!(),
        IRInstruction::Literal(l) => {
          format_string.push_str(&self.get_format_string_from_analyzer_value(&l.value));
          args.push_str(&self.transpile_ir_to_c(arg, 0));
        },
        IRInstruction::Unary(u) => {
          format_string.push_str(&self.get_format_string_from_data_type(&u.data_type));
          args.push_str(&self.transpile_ir_to_c(arg, 0));
        },
        IRInstruction::Variable(v) => {
          format_string.push_str(&self.get_format_string_from_data_type(&v.data_type));
          args.push_str(&self.transpile_ir_to_c(arg, 0));
        },
        IRInstruction::Logical(_) => {
          format_string.push_str(&self.get_format_string_from_data_type(&DataType::Boolean));
          args.push_str(&self.transpile_ir_to_c(arg, 0));
        },
        IRInstruction::If(_) => todo!(),
        IRInstruction::While(_) => todo!(),
        IRInstruction::Function(_) => todo!(),
        IRInstruction::Call(c) => {
          format_string.push_str(&self.get_format_string_from_data_type(&c.return_type));
          args.push_str(&self.transpile_ir_to_c(arg, 0));
        },
        IRInstruction::Return(_) => todo!(),
        IRInstruction::Assign(_) => todo!(),
        IRInstruction::Class(_) => todo!(),
        IRInstruction::Ternary(ternary) => {
          format_string.push_str(&self.get_format_string_from_data_type(&ternary.data_type));
          args.push_str(&self.transpile_ir_to_c(arg, 0));
        },
        IRInstruction::ForOf(_) => todo!(),
        IRInstruction::Array(_) => todo!(),
        IRInstruction::Import(_) => todo!(),
        IRInstruction::Break(_) => todo!(),
        IRInstruction::Continue(_) => todo!(),
        IRInstruction::Get(_) => todo!(),
        IRInstruction::ClassInstance(_) => todo!(),
        IRInstruction::Set(_) => todo!(),
        IRInstruction::For(_) => todo!(),
        IRInstruction::ArrayAccess(_) => todo!(),
        IRInstruction::MethodCall(_) => todo!(),
        IRInstruction::Method(_) => todo!(),
        IRInstruction::This(_) => todo!(),
        IRInstruction::Enum(_) => todo!(),
        IRInstruction::FunctionInstance(_) => todo!(),
        IRInstruction::Interface(_) => todo!(),
      };

      args.push(',');
    }

    args.pop();

    code.push_str(format!("{}printf(\"{}\\n\", {});\n", " ".repeat(indent_level), format_string, args).as_str());

    code
  }

  fn transpile_call_to_c(
    &mut self,
    call: &IRCall,
    indent_level: usize,
  ) -> String {
    let mut code = String::new();

    let name = match call.name.span.literal.as_str() {
      "println" => {
        return self.transpile_print_to_c(call, indent_level);
      },
      "toString" => "toString",
      _ => &call.name.span.literal,
    };

    let args = call
      .arguments
      .iter()
      .map(|x| match x {
        IRInstruction::Call(_) => {
          self.context.push(TranspilerContext::Call);
          let value = self.transpile_ir_to_c(x, 0);

          self.context.pop();
          value
        },
        _ => self.transpile_ir_to_c(x, 0),
      })
      .collect::<Vec<String>>()
      .join(", ");

    code.push_str(&format!("{}{}({})", " ".repeat(indent_level), name, args));

    if let Some(x) = self.context.last() {
      match x {
        TranspilerContext::Call | TranspilerContext::Condition | TranspilerContext::DontNeedSemicolon => {},
        _ => {
          code.push_str(";\n");
        },
      };
    } else {
      code.push_str(";\n");
    }

    code
  }

  fn transpile_if_to_c(
    &mut self,
    if_instruction: &IRIf,
    indent_level: usize,
  ) -> String {
    let mut code: String = String::new();

    self.context.push(TranspilerContext::Condition);
    let condition = self.transpile_ir_to_c(&if_instruction.condition, 0);
    self.context.pop();

    let mut if_block = String::new();
    let mut else_block = String::new();

    if_block.push_str(&self.transpile_ir_to_c(&if_instruction.then_branch, indent_level + 2));

    if let Some(else_block_instructions) = &if_instruction.else_branch {
      else_block.push_str(&self.transpile_ir_to_c(else_block_instructions, indent_level + 2));
    }

    code.push_str(&format!(
      "{}if ({}) {{\n{}{}}}",
      " ".repeat(indent_level),
      condition,
      if_block,
      " ".repeat(indent_level)
    ));

    if !else_block.is_empty() {
      code.push_str(&format!(" else {{\n{}{}}}", else_block, " ".repeat(indent_level)))
    }

    code.push('\n');

    code
  }

  fn transpile_while_to_c(
    &mut self,
    while_instruction: &IRWhile,
    indent_level: usize,
  ) -> String {
    let mut code: String = String::new();

    self.context.push(TranspilerContext::Condition);
    let condition = self.transpile_ir_to_c(&while_instruction.condition, 0);
    self.context.pop();

    let block = &self.transpile_ir_to_c(&while_instruction.body, indent_level + 2);

    code.push_str(&format!(
      "{}while ({}) {{\n{}{}}}\n",
      " ".repeat(indent_level),
      condition,
      block,
      " ".repeat(indent_level),
    ));

    code
  }

  fn transpile_ir_to_c(
    &mut self,
    instruction: &IRInstruction,
    indent_level: usize,
  ) -> String {
    let mut code = String::new();
    match instruction {
      IRInstruction::Literal(literal) => code.push_str(&match &literal.value {
        AnalyzerValue::Int(num) => num.to_string(),
        AnalyzerValue::Float(num) => num.to_string(),
        AnalyzerValue::String(s) => format!("\"{}\"", s),
        AnalyzerValue::Boolean(boolean) => if *boolean { "0" } else { "1" }.to_string(),
        AnalyzerValue::Null | AnalyzerValue::Unknown => "NULL".to_string(),
        AnalyzerValue::Return(_) => todo!(),
        AnalyzerValue::Function(_) => todo!(),
        AnalyzerValue::Class(_) => todo!(),
      }),
      IRInstruction::Binary(binary) => {
        let left = self.transpile_ir_to_c(&binary.left, indent_level);
        let right = self.transpile_ir_to_c(&binary.right, indent_level);
        let op = self.transpile_operator_to_c(&binary.instruction_type);

        code.push_str(&format!("{} {} {}", left, op, right));
      },
      IRInstruction::Block(block) => {
        for instr in &block.instructions {
          code.push_str(&self.transpile_ir_to_c(instr, indent_level));
        }
      },
      IRInstruction::Unary(unary) => {
        let value = self.transpile_ir_to_c(&unary.right, indent_level);
        let op = self.transpile_operator_to_c(&unary.instruction_type);

        code.push_str(&format!("{} {}", op, value));
      },
      IRInstruction::Variable(var) => code.push_str(self.transpile_variable_to_c(var, indent_level).as_str()),
      IRInstruction::Logical(logical) => {
        let left = self.transpile_ir_to_c(&logical.left, indent_level);
        let right = self.transpile_ir_to_c(&logical.right, indent_level);
        let op = self.transpile_operator_to_c(&logical.instruction_type);

        code.push_str(&format!("{} {} {}", left, op, right));
      },
      IRInstruction::If(i) => code.push_str(self.transpile_if_to_c(&i, indent_level).as_str()),
      IRInstruction::While(w) => code.push_str(self.transpile_while_to_c(&w, indent_level).as_str()),
      IRInstruction::Function(fun) => code.push_str(self.transpile_function_to_c(fun, indent_level).as_str()),
      IRInstruction::Call(call) => {
        code.push_str(&self.transpile_call_to_c(call, indent_level));
      },
      IRInstruction::Return(ir_return) => {
        self.context.push(TranspilerContext::DontNeedSemicolon);
        let value = self.transpile_ir_to_c(&ir_return.value, indent_level);
        self.context.pop();

        code.push_str(&format!("{}return {};\n", " ".repeat(indent_level), value))
      },
      IRInstruction::Assign(assign) => {
        let value = self.transpile_ir_to_c(&assign.value, indent_level);

        code.push_str(&format!("{}{} = {};\n", " ".repeat(indent_level), assign.name, value))
      },
      IRInstruction::Class(_) => todo!(),
      IRInstruction::Ternary(ternary) => {
        self.context.push(TranspilerContext::Condition);
        let condition = self.transpile_ir_to_c(&ternary.condition, indent_level);
        self.context.pop();

        let then_branch = self.transpile_ir_to_c(&ternary.then_branch, indent_level);
        let else_branch = self.transpile_ir_to_c(&ternary.else_branch, indent_level);

        code.push_str(&format!("{} ? {} : {}", condition, then_branch, else_branch))
      },
      IRInstruction::ForOf(_) => todo!(),
      IRInstruction::Array(array) => {
        let mut elements = String::new();

        for element in &array.elements {
          elements.push_str(&self.transpile_ir_to_c(element, indent_level));
          elements.push_str(", ");
        }

        elements.pop();
        elements.pop();

        code.push_str(&format!("{{ {} }}", elements))
      },
      IRInstruction::Import(import) => {
        if import.path.contains("std:") {
          match import.path.as_str() {
            "std:io" => {
              code.push_str("#include <stdio.h>\n");
            },
            _ => (),
          }
        }
      },
      IRInstruction::Break(_) => todo!(),
      IRInstruction::Continue(_) => todo!(),
      IRInstruction::Get(_) => todo!(),
      IRInstruction::ClassInstance(_) => todo!(),
      IRInstruction::Set(_) => todo!(),
      IRInstruction::For(_) => todo!(),
      IRInstruction::ArrayAccess(_) => todo!(),
      IRInstruction::MethodCall(_) => todo!(),
      IRInstruction::Method(_) => todo!(),
      IRInstruction::This(_) => todo!(),
      IRInstruction::Enum(_) => todo!(),
        IRInstruction::FunctionInstance(_) => todo!(),
        IRInstruction::Interface(_) => todo!(),
    };

    code
  }

  fn transpile(
    &mut self,
    ir: &Vec<IRInstruction>,
  ) {
    self.statement_exported = vec![];
    self.code = String::new();
    self.statement_imported = HashMap::new();

    for instruction in ir {
      let code = self.transpile_ir_to_c(instruction, 0).clone();

      self.code.push_str(code.as_str());
    }
  }

  pub fn process(&mut self) {
    let mut code_results = Vec::new();
    let irs = self.irs.clone();

    for result in irs.iter() {
      self.transpile(result.1);
      code_results.push(CodeResult::new(self.code.clone(), result.0.clone()));
    }

    self.create_files(code_results);
  }

  fn create_files(
    &self,
    code_results: Vec<CodeResult>,
  ) {
    for code_result in code_results {
      let mut path = code_result.file_name.split('/').collect::<Vec<&str>>();
      let code = code_result.code.clone();

      let name = path.last().unwrap().replace(r".ign", "");

      path.pop();

      fs::create_dir_all(format!("build/{}", path.join("/"))).unwrap();

      let build_path = "build/".to_string() + path.join("/").as_str();

      fs::write(format!("{}/{}.c", &build_path, &name), &code).unwrap();

      let child = Command::new("gcc")
        .args(["-x", "c", "-", "-o", &format!("{}/{}", &build_path, &name)])
        .stdin(Stdio::piped())
        .spawn();

      if child.is_err() {
        eprintln!("Error spawning gcc");
        return;
      }

      let mut child = child.unwrap();
      {
        let stdin = child.stdin.as_mut().ok_or("Error getting stdin").unwrap();
        stdin.write_all(code.as_bytes()).unwrap();
      }

      let output = child.wait_with_output();

      if output.is_err() {
        eprintln!("Error waiting for gcc");
        return;
      }

      let output = output.unwrap();

      if !output.status.success() {
        eprintln!("Compilation error: {}", String::from_utf8_lossy(&output.stderr));

        return;
      }
    }
  }
}
