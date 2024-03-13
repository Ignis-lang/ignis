use std::{vec, collections::HashMap};
use colored::*;

use code_result::CodeResult;
use enums::data_type::DataType;
use intermediate_representation::{
  IRInstruction, function::IRFunction, call::IRCall, variable::IRVariable, instruction_type::IRInstructionType,
  analyzer_value::AnalyzerValue, ir_get::IRGet, ir_method_call::IRMethodCall, class::IRClass, ir_this::IRThis,
};

#[derive(Debug, Clone, PartialEq)]
enum TranspilerContext {
  For,
  While,
  Continue(Box<TranspilerContext>),
  ArrayAccess,
}

pub struct TranspilerToLua {
  pub code: String,
  pub statement_exported: Vec<(String, String)>,
  pub statement_imported: HashMap<String, String>,
  context: Vec<TranspilerContext>,
  irs: HashMap<String, Vec<IRInstruction>>,
}

impl TranspilerToLua {
  pub fn new(irs: HashMap<String, Vec<IRInstruction>>) -> Self {
    println!("{}: Transpiling to Lua...", "[Transpiler]".green().bold());
    Self {
      code: String::new(),
      statement_exported: vec![],
      statement_imported: HashMap::new(),
      context: vec![],
      irs,
    }
  }

  fn transpile(
    &mut self,
    ir: &Vec<IRInstruction>,
  ) {
    self.statement_exported = vec![];
    self.code = String::new();
    self.statement_imported = HashMap::new();

    for instruction in ir {
      let code = self.transpile_ir_to_lua(instruction, 0).clone();

      self.code.push_str(code.as_str());
    }

    if !self.statement_exported.is_empty() {
      self.code.push_str("local M = {}\n");

      for statement in &self.statement_exported {
        self
          .code
          .push_str(format!("{}M.{} = {}\n", " ".repeat(2), statement.0.as_str(), statement.0).as_str());
      }

      self.code.push_str(&format!("{}return M\n", " ".repeat(0)));
    }
  }

  fn transpile_ir_to_lua(
    &mut self,
    instruction: &IRInstruction,
    indent_level: usize,
  ) -> String {
    let mut code = String::new();

    match instruction {
      IRInstruction::Literal(literal) => code.push_str(&match &literal.value {
        AnalyzerValue::Int(num) => {
          if let Some(c) = self.context.last() {
            if c == &TranspilerContext::ArrayAccess {
              if *num == 0 {
                return "1".to_string();
              } else {
                return num.to_string();
              }
            }
          }
          return num.to_string();
        },
        AnalyzerValue::String(s) => format!("\"{}\"", s),
        AnalyzerValue::Float(num) => num.to_string(),
        AnalyzerValue::Boolean(boolean) => boolean.to_string(),
        AnalyzerValue::Return(r) => r.to_string(),
        AnalyzerValue::Function(f) => f.name.span.literal.clone(),
        AnalyzerValue::Null | AnalyzerValue::Unknown => "nil".to_string(),
        AnalyzerValue::Class(_) => todo!(),
      }),
      IRInstruction::Binary(binary) => {
        let left = self.transpile_ir_to_lua(&binary.left, indent_level);
        let right = self.transpile_ir_to_lua(&binary.right, indent_level);
        let op = self.transpile_opeartor_to_lua(&binary.instruction_type);

        if let Some(c) = self.context.last() {
          if c == &TranspilerContext::For {
            return right.to_string();
          }
        }

        code.push_str(&format!("{} {} {}", left, op, right));
      },
      IRInstruction::Block(block) => {
        for instr in &block.instructions {
          code.push_str(&self.transpile_ir_to_lua(instr, indent_level));
        }
      },
      IRInstruction::Function(func) => {
        code.push_str(&self.transpile_function_to_lua(func, indent_level));
      },
      IRInstruction::Unary(unary) => {
        let value = self.transpile_ir_to_lua(&unary.right, indent_level);
        let op = self.transpile_opeartor_to_lua(&unary.instruction_type);

        if let Some(c) = self.context.last() {
          if c == &TranspilerContext::For {
            return op.to_string();
          }
        }

        if unary.instruction_type == IRInstructionType::Increment {
          return format!("{}{} = {} + {}\n", " ".repeat(indent_level), value, value, op);
        }

        if unary.instruction_type == IRInstructionType::Decrement {
          return format!("{}{} = {} - {}\n", " ".repeat(indent_level), value, value, op);
        }

        code.push_str(&format!("{} {}", op, value));
      },
      IRInstruction::Variable(var) => code.push_str(&self.transpile_variable_to_lua(var, indent_level)),
      IRInstruction::Logical(logical) => {
        let left = self.transpile_ir_to_lua(&logical.left, indent_level);
        let right = self.transpile_ir_to_lua(&logical.right, indent_level);
        let op = self.transpile_opeartor_to_lua(&logical.instruction_type);

        code.push_str(&format!("{} {} {}", left, op, right));
      },
      IRInstruction::If(if_instruction) => {
        code.push_str(&format!(
          "{}if {} then\n",
          " ".repeat(indent_level),
          &self.transpile_ir_to_lua(&if_instruction.condition, indent_level)
        ));

        code.push_str(&self.transpile_ir_to_lua(&if_instruction.then_branch, indent_level));

        if let Some(else_branch) = &if_instruction.else_branch {
          code.push_str(format!("{}else\n", " ".repeat(indent_level)).as_str());

          code.push_str(&self.transpile_ir_to_lua(else_branch, indent_level));
        }

        code.push_str(format!("{} end\n", " ".repeat(indent_level)).as_str());
      },
      IRInstruction::While(ir_while) => {
        let condition = self.transpile_ir_to_lua(&ir_while.condition, indent_level);

        self.context.push(TranspilerContext::While);

        code.push_str(&format!("{}while ({}) do\n", " ".repeat(indent_level), condition));

        code.push_str(&self.transpile_ir_to_lua(&ir_while.body, indent_level + 2));

        if let TranspilerContext::Continue(l) = self.context.last().unwrap() {
          if **l == TranspilerContext::While {
            code.push_str(&format!("{}::continue::\n", " ".repeat(indent_level)));
            self.context.pop();
          }
        };

        code.push_str(format!("{}end\n", " ".repeat(indent_level)).as_str());

        self.context.pop();
      },
      IRInstruction::Call(call) => code.push_str(&self.transpile_call_to_lua(call, indent_level)),
      IRInstruction::Return(r) => {
        let value = self.transpile_ir_to_lua(&r.value, indent_level);
        code.push_str(&format!("{}return {}\n", " ".repeat(indent_level), value));
      },
      IRInstruction::Assign(assign) => {
        let value = self.transpile_ir_to_lua(&assign.value, 0);
        code.push_str(&format!("{}{} = {}\n", " ".repeat(indent_level), assign.name, value));
      },
      IRInstruction::Class(class) => code.push_str(&self.transpile_class_to_lua(class, indent_level)),
      IRInstruction::Ternary(ternary) => {
        let condition = self.transpile_ir_to_lua(&ternary.condition, indent_level);

        let then_branch = self.transpile_ir_to_lua(&ternary.then_branch, indent_level);
        let else_branch = self.transpile_ir_to_lua(&ternary.else_branch, indent_level);

        code.push_str(&format!("{} and {} or {}", condition, then_branch, else_branch));
      },
      IRInstruction::ForOf(for_of) => {
        self.context.push(TranspilerContext::For);

        code.push_str(&format!(
          "{}for _, {} in pairs({}) do\n",
          " ".repeat(indent_level),
          for_of.variable.name,
          self.transpile_ir_to_lua(&for_of.iterable, indent_level)
        ));

        code.push_str(&self.transpile_ir_to_lua(&for_of.body, indent_level + 2));

        if let TranspilerContext::Continue(l) = self.context.last().unwrap() {
          if **l == TranspilerContext::For {
            code.push_str(&format!("{}::continue::\n", " ".repeat(indent_level)));
            self.context.pop();
          }
        };

        self.context.pop();

        code.push_str(format!("{}end\n", " ".repeat(indent_level)).as_str());
      },
      IRInstruction::Array(array) => {
        code.push_str(&format!(
          "{}{{ {}",
          " ".repeat(indent_level),
          array
            .elements
            .iter()
            .map(|x| self.transpile_ir_to_lua(x, indent_level + 2))
            .collect::<Vec<String>>()
            .join(", ")
        ));

        code.push_str(" }\n");
      },
      IRInstruction::Import(import) => {
        if !import.path.contains("std:") {
          let module_path = import.path.split('/').collect::<Vec<&str>>();
          let mut module_name = module_path.last().unwrap().to_string();

          for (name, alias) in &import.name {
            let value = if alias.is_some() {
              alias.clone().unwrap().span.literal.clone()
            } else {
              name.span.literal.clone()
            };

            module_name.clone_from(&value);

            self.statement_imported.insert(module_name.clone(), value);
          }

          code.push_str(&format!(
            "{}local {} = require(\"build.{}\")\n",
            " ".repeat(indent_level),
            module_name,
            module_path.join("."),
          ));
        }
      },
      IRInstruction::Break(_) => {
        code.push_str(&format!("{}break\n", " ".repeat(indent_level)));
      },
      IRInstruction::Continue(_) => {
        let context = self.context.pop().unwrap();

        self.context.push(TranspilerContext::Continue(Box::new(context)));

        code.push_str(&format!("{}goto continue\n", " ".repeat(indent_level)));
      },
      IRInstruction::Get(get) => {
        let result = match get.metadata.object_data_type {
          DataType::Array(_) => self.transpile_array_property(get),
          _ => self.transpile_get_to_lua(get, indent_level),
        };

        code.push_str(&result);
      },
      IRInstruction::ClassInstance(class) => {
        let mut name = class.class.name.clone();

        if class.class.is_imported {
          if let Some(n) = self.statement_imported.get(&name.clone()) {
            name = n.clone() + "." + &name;
          }
        }

        code.push_str(&format!(
          "{}{}:new({})",
          " ".repeat(indent_level),
          name,
          class
            .constructor_args
            .iter()
            .map(|a| { self.transpile_ir_to_lua(a, indent_level) })
            .collect::<Vec<String>>()
            .join(", ")
        ));
      },
      IRInstruction::Set(set) => {
        let value = self.transpile_ir_to_lua(&set.value, indent_level);
        code.push_str(&format!(
          "{}{}.{} = {}\n",
          " ".repeat(indent_level),
          set.var_name,
          set.name,
          value
        ));
      },
      IRInstruction::For(_for) => {
        self.context.push(TranspilerContext::For);
        code.push_str(&format!(
          "{}for {}, {}, {} do\n",
          " ".repeat(indent_level),
          self.transpile_ir_to_lua(&_for.initializer, 0),
          self.transpile_ir_to_lua(&_for.condition, indent_level),
          self.transpile_ir_to_lua(&_for.increment, 0)
        ));

        self.context.pop();
        code.push_str(&self.transpile_ir_to_lua(&_for.body, indent_level + 2));

        if let Some(l) = self.context.last() {
          if l == &TranspilerContext::Continue(Box::new(TranspilerContext::For)) {
            self.context.pop();
            code.push_str(&format!("{}::continue::\n", " ".repeat(indent_level)));
          }
        };

        self.context.pop();

        code.push_str(format!("{}end\n", " ".repeat(indent_level)).as_str());
      },
      IRInstruction::ArrayAccess(array) => {
        self.context.push(TranspilerContext::ArrayAccess);

        code.push_str(&format!("{}[{}]", array.name, self.transpile_ir_to_lua(&array.index, 0)));

        self.context.pop();
      },
      IRInstruction::MethodCall(call) => match call.metadata.object_data_type {
        DataType::Int | DataType::Float => code.push_str(&self.transpile_number_methods(call, indent_level)),
        DataType::Boolean => code.push_str(&self.transpile_boolean_methods(call, indent_level)),
        _ => code.push_str(&self.transpile_method_call(call, indent_level)),
      },
      IRInstruction::Method(_) => todo!(),
      IRInstruction::This(this) => code.push_str(&self.transpile_this_to_lua(this, indent_level)),
      IRInstruction::Enum(_) => todo!(),
    };

    code
  }

  fn transpile_this_to_lua(
    &mut self,
    this: &IRThis,
    indent_level: usize,
  ) -> String {
    let mut code = String::new();

    code.push_str(&format!("{}self", " ".repeat(indent_level)));

    if let Some(access) = &this.access {
      let access = *access.clone();
      match &access {
        IRInstruction::MethodCall(_) | IRInstruction::Call(_) => {
          code.push(':');
          code.push_str(&self.transpile_ir_to_lua(&access, 0));
        },
        _ => {
          code.push('.');
          code.push_str(&self.transpile_ir_to_lua(&access, 0));
        },
      };
    }

    code
  }

  fn transpile_get_to_lua(
    &mut self,
    get: &IRGet,
    indent_level: usize,
  ) -> String {
    let mut code = String::new();

    match *get.object.clone() {
      IRInstruction::ClassInstance(inst) => {
        code.push_str(&format!("{}{}", " ".repeat(indent_level), inst.var_name.span.literal));
      },
      _ => todo!(),
    };

    code.push_str(format!(".{}", get.name).as_str());

    code
  }

  fn transpile_class_to_lua(
    &mut self,
    class: &IRClass,
    indent_level: usize,
  ) -> String {
    let mut code = String::new();

    if class.is_exported {
      self.statement_exported.push((class.name.clone(), String::new()));
    }

    if class.is_imported {
      return code;
    }

    code.push_str(&format!("{}{} = {{\n", " ".repeat(indent_level), class.name));

    for property in &class.properties {
      code.push_str(&format!("{}{} = ", " ".repeat(indent_level + 2), property.name,));

      if let Some(value) = &property.value {
        code.push_str(&self.transpile_ir_to_lua(value, 0));
      } else {
        code.push_str("nil");
      }

      code.push(',');
      code.push('\n');
    }

    code.push_str("}\n");
    code.push('\n');

    code.push_str(&format!(
      "{}{}.__index = {}\n",
      " ".repeat(indent_level),
      class.name,
      class.name
    ));

    for method in &class.methods {
      let parameters = self.transpile_function_params(&method.parameters);

      let name = if class.name == method.name.span.literal {
        "new".to_string()
      } else {
        method.name.span.literal.clone()
      };

      code.push_str(&format!(
        "{}function {}:{}({})\n",
        " ".repeat(indent_level),
        class.name,
        name,
        parameters
      ));

      if let Some(body) = &method.body {
        if name == "new" {
          code.push_str(format!("{}{}", " ".repeat(indent_level + 2), "local instance = {}\n").as_str());
          code.push_str(format!("{}setmetatable(instance, {})\n", " ".repeat(indent_level + 2), class.name,).as_str());
        }
        for instr in &body.instructions {
          let result = &self.transpile_ir_to_lua(instr, indent_level + 2);
          if name == "new" {
            code.push_str(result.replace("self", "instance").as_str());
          } else {
            code.push_str(result);
          }
        }
      }

      if name == "new" {
        code.push_str(format!("{}{}", " ".repeat(indent_level + 2), "return instance\n").as_str());
      }

      code.push_str("end\n");
    }

    code
  }

  fn transpile_method_call(
    &mut self,
    call: &IRMethodCall,
    indent_level: usize,
  ) -> String {
    let mut code = String::new();

    let calle = self.transpile_ir_to_lua(&call.calle, indent_level);

    code.push_str(&format!(
      "{}{}:{}",
      " ".repeat(indent_level),
      match *call.object.clone() {
        IRInstruction::ClassInstance(inst) => inst.var_name.span.literal,
        _ => todo!(),
      },
      calle,
    ));

    code
  }

  fn transpile_boolean_methods(
    &mut self,
    call: &IRMethodCall,
    indent_level: usize,
  ) -> String {
    let mut code = String::new();
    match call.name.span.literal.as_str() {
      "toString" => {
        code.push_str(format!("tostring({})", &self.transpile_ir_to_lua(&call.object, indent_level)).as_str());
      },
      _ => todo!(),
    }
    code
  }

  fn transpile_number_methods(
    &mut self,
    call: &IRMethodCall,
    indent_level: usize,
  ) -> String {
    let mut code = String::new();
    match call.name.span.literal.as_str() {
      "toString" => {
        code.push_str(format!("tostring({})", &self.transpile_ir_to_lua(&call.object, indent_level)).as_str());
      },
      _ => todo!(),
    }
    code
  }

  fn transpile_function_to_lua(
    &mut self,
    func: &IRFunction,
    indent_level: usize,
  ) -> String {
    let mut code = String::new();

    if func.metadata.is_extern {
      return code;
    }

    if func.metadata.is_exported {
      self
        .statement_exported
        .push((func.name.span.literal.clone(), String::new()));
    }

    let parameters = self.transpile_function_params(&func.parameters);
    if let Some(body) = &func.body {
      if func.metadata.is_imported {
        return code;
      }

      if func.metadata.is_recursive {
        code.push_str(&format!("{}local {}\n", " ".repeat(indent_level), func.name.span.literal));

        code.push_str(&format!(
          "{}{} = function({})\n",
          " ".repeat(indent_level),
          func.name.span.literal,
          parameters
        ));
      } else {
        code.push_str(&format!(
          "{}local {} = function({})\n",
          " ".repeat(indent_level),
          func.name.span.literal,
          parameters
        ));
      }

      for instr in &body.instructions {
        code.push_str(&self.transpile_ir_to_lua(instr, indent_level + 2));
      }

      code.push_str(format!("{}end\n", " ".repeat(indent_level)).as_str());

      if func.name.span.literal == "main" {
        code.push_str(&format!("{}{}()\n", " ".repeat(indent_level), func.name.span.literal));
      }
    }

    code
  }

  fn transpile_function_params(
    &mut self,
    params: &Vec<IRInstruction>,
  ) -> String {
    let mut code = String::new();

    for param in params {
      code.push_str(&self.transpile_ir_to_lua(param, 0));
      code.push(',');
    }

    code.pop();

    code
  }

  fn transpile_array_property(
    &mut self,
    array: &IRGet,
  ) -> String {
    match array.name.as_str() {
      "length" => {
        format!("#{}", self.transpile_ir_to_lua(&array.object, 0))
      },
      _ => todo!(),
    }
  }

  fn transpile_opeartor_to_lua(
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
      IRInstructionType::NotEqual => "~=",
      IRInstructionType::And => "and",
      IRInstructionType::Or => "or",
      IRInstructionType::Not => "not",
      IRInstructionType::Assign => "=",
      IRInstructionType::AssignAdd => "+=",
      IRInstructionType::AssignSub => "-=",
      IRInstructionType::Mod => "%",
      IRInstructionType::Concatenate => "..",
      IRInstructionType::Increment => "1",
      IRInstructionType::Decrement => "-1",
    }
    .to_string()
  }

  fn transpile_call_to_lua(
    &mut self,
    call: &IRCall,
    indent_level: usize,
  ) -> String {
    let mut code = String::new();

    let name = match call.name.span.literal.as_str() {
      "println" => "print".to_string(),
      _ => call.name.span.literal.clone(),
    };

    if self.statement_imported.contains_key(&name) {
      let module_name = self.statement_imported.get(&name).unwrap().clone();

      code.push_str(&format!(
        "{}{}.{}({})",
        " ".repeat(indent_level),
        module_name,
        name,
        call
          .arguments
          .iter()
          .map(|f| self.transpile_ir_to_lua(f, indent_level))
          .collect::<Vec<String>>()
          .join(", ")
      ));

      return code;
    }

    code.push_str(&format!(
      "{}{}({})",
      " ".repeat(indent_level),
      name,
      call
        .arguments
        .iter()
        .map(|f| self.transpile_ir_to_lua(f, indent_level))
        .collect::<Vec<String>>()
        .join(", ")
    ));

    code.push('\n');

    code
  }

  fn transpile_variable_to_lua(
    &mut self,
    variable: &IRVariable,
    indent_level: usize,
  ) -> String {
    let var_value = if let Some(value) = &variable.value {
      self.transpile_ir_to_lua(value, 0)
    } else {
      "".to_string()
    };

    if variable.metadata.is_parameter {
      return format!("{}{}", " ".repeat(indent_level), variable.name);
    }

    if variable.metadata.is_declaration {
      if let Some(l) = self.context.last() {
        let value = var_value.parse::<i32>().unwrap();

        match l {
          TranspilerContext::For => {
            return format!(
              "{}{} = {}",
              " ".repeat(indent_level),
              variable.name,
              if value == 0 { 1 } else { value }
            );
          },
          TranspilerContext::While => todo!(),
          TranspilerContext::Continue(_) => todo!(),
          TranspilerContext::ArrayAccess => todo!(),
        }
      }

      format!("{}local {} = {}\n", " ".repeat(indent_level), variable.name, var_value)
    } else {
      variable.name.to_string()
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
      let path = code_result.file_name.split('/').collect::<Vec<&str>>();
      let code = code_result.code.clone();

      let mut name = path.last().unwrap().replace(r".ign", "");

      name.push_str(".lua");

      let mut build_output = path.clone();
      build_output.pop();

      let mut build_path = "build/".to_string() + build_output.join("/").as_str();

      std::fs::create_dir_all(build_path.clone()).unwrap();

      build_path.push_str(format!("/{}", &name).as_str());

      std::fs::write(build_path, code).unwrap();

      println!("{}: {}", "[Done]".blue().bold(), code_result.file_name);
    }
  }
}
