use std::{vec, collections::HashMap};

use intermediate_representation::{
  {
    IRInstruction, function::IRFunction, call::IRCall, variable::IRVariable,
    instruction_type::IRInstructionType,
  },
  analyzer_value::AnalyzerValue,
};

#[derive(Debug, Clone, PartialEq)]
enum TranspilerContext {
  For,
  While,
  Continue(Box<TranspilerContext>),
}

pub struct TranspilerToLua {
  pub code: String,
  pub statement_exported: Vec<(String, String)>,
  pub statement_imported: HashMap<String, String>,
  context: Vec<TranspilerContext>,
}

impl TranspilerToLua {
  pub fn new() -> Self {
    Self {
      code: String::new(),
      statement_exported: vec![],
      statement_imported: HashMap::new(),
      context: vec![],
    }
  }

  pub fn transpile(&mut self, ir: &Vec<IRInstruction>) {
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
        self.code.push_str(
          format!(
            "{}M.{} = {}\n",
            " ".repeat(2),
            statement.0.as_str(),
            statement.0
          )
          .as_str(),
        );
      }

      self.code.push_str(&format!("{}return M\n", " ".repeat(0)));
    }
  }

  fn transpile_ir_to_lua(&mut self, instruction: &IRInstruction, indent_level: usize) -> String {
    let mut code = String::new();

    match instruction {
      IRInstruction::Literal(literal) => code.push_str(&match &literal.value {
        AnalyzerValue::Int(num) => num.to_string(),
        AnalyzerValue::String(s) => format!("\"{}\"", s),
        AnalyzerValue::Float(num) => num.to_string(),
        AnalyzerValue::Boolean(boolean) => boolean.to_string(),
        AnalyzerValue::Return(r) => r.to_string(),
        AnalyzerValue::Function(f) => f.name.clone(),
        AnalyzerValue::Null | AnalyzerValue::None => "nil".to_string(),
      }),
      IRInstruction::Binary(binary) => {
        let left = self.transpile_ir_to_lua(&binary.left, indent_level);
        let right = self.transpile_ir_to_lua(&binary.right, indent_level);
        let op = self.transpile_opeartor_to_lua(&binary.instruction_type);

        code.push_str(&format!("{} {} {}", left, op, right));
      }
      IRInstruction::Block(block) => {
        for instr in &block.instructions {
          code.push_str(&self.transpile_ir_to_lua(instr, indent_level));
        }
      }
      IRInstruction::Function(func) => {
        code.push_str(&self.transpile_function_to_lua(func, indent_level));
      }
      IRInstruction::Unary(unary) => {
        let value = self.transpile_ir_to_lua(&unary.right, indent_level);
        let op = self.transpile_opeartor_to_lua(&unary.instruction_type);

        code.push_str(&format!("{} {}", op, value));
      }
      IRInstruction::Variable(var) => {
        code.push_str(&self.transpile_variable_to_lua(var, indent_level))
      }
      IRInstruction::Logical(logical) => {
        let left = self.transpile_ir_to_lua(&logical.left, indent_level);
        let right = self.transpile_ir_to_lua(&logical.right, indent_level);
        let op = self.transpile_opeartor_to_lua(&logical.instruction_type);

        code.push_str(&format!("{} {} {}", left, op, right));
      }
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
      }
      IRInstruction::While(ir_while) => {
        let condition = self.transpile_ir_to_lua(&ir_while.condition, indent_level);

        self.context.push(TranspilerContext::While);

        code.push_str(&format!(
          "{}while {} do\n",
          " ".repeat(indent_level),
          condition
        ));

        code.push_str(&self.transpile_ir_to_lua(&ir_while.body, indent_level + 2));

        if let TranspilerContext::Continue(l) = self.context.last().unwrap() {
          if **l == TranspilerContext::While {
            code.push_str(&format!("{}::continue::\n", " ".repeat(indent_level)));
            self.context.pop();
          }
        };

        code.push_str(format!("{}end\n", " ".repeat(indent_level)).as_str());

        self.context.pop();
      }
      IRInstruction::Call(call) => code.push_str(&self.transpile_call_to_lua(call, indent_level)),
      IRInstruction::Return(r) => {
        let value = self.transpile_ir_to_lua(&r.value, indent_level);
        code.push_str(&format!("{}return {}\n", " ".repeat(indent_level), value));
      }
      IRInstruction::Assign(assign) => {
        let value = self.transpile_ir_to_lua(&assign.value, 0);
        code.push_str(&format!(
          "{}{} = {}\n",
          " ".repeat(indent_level),
          assign.name,
          value
        ));
      }
      IRInstruction::Class(_) => todo!(),
      IRInstruction::Ternary(ternary) => {
        let condition = self.transpile_ir_to_lua(&ternary.condition, indent_level);

        let then_branch = self.transpile_ir_to_lua(&ternary.then_branch, indent_level);
        let else_branch = self.transpile_ir_to_lua(&ternary.else_branch, indent_level);

        code.push_str(&format!(
          "{} and {} or {}",
          condition, then_branch, else_branch
        ));
      }
      IRInstruction::ForIn(for_in) => {
        self.context.push(TranspilerContext::For);

        code.push_str(&format!(
          "{}for _, {} in pairs({}) do\n",
          " ".repeat(indent_level),
          for_in.variable.name,
          self.transpile_ir_to_lua(&for_in.iterable, indent_level)
        ));

        code.push_str(&self.transpile_ir_to_lua(&for_in.body, indent_level + 2));

        if let TranspilerContext::Continue(l) = self.context.last().unwrap() {
          if **l == TranspilerContext::While {
            code.push_str(&format!("{}::continue::\n", " ".repeat(indent_level)));
            self.context.pop();
          }
        };

        code.push_str(format!("{}end\n", " ".repeat(indent_level)).as_str());
      }
      IRInstruction::Array(array) => {
        code.push_str(&format!(
          "{}{{{}",
          " ".repeat(indent_level),
          array
            .elements
            .iter()
            .map(|x| self.transpile_ir_to_lua(x, indent_level + 2))
            .collect::<Vec<String>>()
            .join(", ")
        ));

        code.push_str("}\n");
      }
      IRInstruction::Import(import) => {
        if !import.path.contains("std:") {
          let module_path = import.path.split('/').collect::<Vec<&str>>();
          let module_name = module_path.last().unwrap().to_string();

          for (name, alias) in &import.name {
            let value = if alias.is_some() {
              alias.clone().unwrap().span.literal.clone()
            } else {
              name.span.literal.clone()
            };

            self.statement_imported.insert(value, module_name.clone());
          }

          code.push_str(&format!(
            "{}local {} = require(\"build.{}\")\n",
            " ".repeat(indent_level),
            module_name,
            module_path.join("."),
          ));
        }
      }
      IRInstruction::Break(_) => {
        code.push_str(&format!("{}break\n", " ".repeat(indent_level)));
      }
      IRInstruction::Continue(_) => {
        let context = self.context.pop().unwrap();

        self
          .context
          .push(TranspilerContext::Continue(Box::new(context)));

        code.push_str(&format!("{}goto continue\n", " ".repeat(indent_level)));
      }
    };

    code
  }

  fn transpile_function_to_lua(&mut self, func: &IRFunction, indent_level: usize) -> String {
    let mut code = String::new();

    if func.metadata.is_extern {
      return code;
    }

    if let Some(body) = &func.body {
      let parameters = func
        .parameters
        .iter()
        .map(|x| x.name.clone())
        .collect::<Vec<String>>()
        .join(", ");

      if func.metadata.is_exported {
        self
          .statement_exported
          .push((func.name.clone(), String::new()));
      }

      if func.metadata.is_imported {
        return code;
      }

      if func.metadata.is_recursive {
        code.push_str(&format!(
          "{}local {}\n",
          " ".repeat(indent_level),
          func.name
        ));

        code.push_str(&format!(
          "{}{} = function({})\n",
          " ".repeat(indent_level),
          func.name,
          parameters
        ));
      } else {
        code.push_str(&format!(
          "{}local {} = function({})\n",
          " ".repeat(indent_level),
          func.name,
          parameters
        ));
      }

      for instr in &body.instructions {
        code.push_str(&self.transpile_ir_to_lua(instr, indent_level + 2));
      }

      code.push_str(format!("{}end\n", " ".repeat(indent_level)).as_str());

      if func.name == "main" {
        code.push_str(&format!("{}{}()\n", " ".repeat(indent_level), func.name));
      }
    }

    code
  }

  fn transpile_opeartor_to_lua(&self, operator: &IRInstructionType) -> String {
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
    }
    .to_string()
  }

  fn transpile_call_to_lua(&mut self, call: &IRCall, indent_level: usize) -> String {
    let mut code = String::new();

    let name = match call.name.as_str() {
      "println" => "print".to_string(),
      "toString" => "toString".to_string(),
      _ => call.name.clone(),
    };

    if name == "toString" {
      code.push_str(&self.transpile_ir_to_lua(&call.arguments[0], indent_level));

      return code;
    }

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

  fn transpile_variable_to_lua(&mut self, variable: &IRVariable, indent_level: usize) -> String {
    let var_value = if let Some(value) = &variable.value {
      self.transpile_ir_to_lua(value, 0)
    } else {
      "".to_string()
    };

    if variable.metadata.is_declaration {
      format!(
        "{}local {} = {}\n",
        " ".repeat(indent_level),
        variable.name,
        var_value
      )
    } else {
      variable.name.clone()
    }
  }
}

impl Default for TranspilerToLua {
    fn default() -> Self {
        Self::new()
    }
}
