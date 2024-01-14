use std::{collections::HashMap, fmt::Display};

pub mod debug;

use intermediate_representation ::{instruction::IRInstruction, analyzer_value::AnalyzerValue};

#[derive(Debug, Clone, Copy)]
pub enum OpCode {
  ConstantInt = 0,
  ConstantFloat,
  Add,
  Subtract,
  Multiply,
  Divide,
  Negate,
  Return,
}

impl From<u8> for OpCode {
  fn from(byte: u8) -> Self {
    match byte {
      0 => OpCode::ConstantInt,
      1 => OpCode::ConstantFloat,
      2 => OpCode::Add,
      3 => OpCode::Subtract,
      4 => OpCode::Multiply,
      5 => OpCode::Divide,
      6 => OpCode::Negate,
      7 => OpCode::Return,
      _ => panic!("Invalid OpCode {}", byte),
    }
  }
}

#[derive(Debug, Clone, Copy)]
pub enum Value {
  Int(i64),
  Float(f64),
}

impl Display for Value {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Value::Int(int) => write!(f, "{}", int),
      Value::Float(float) => write!(f, "{}", float),
    }
  }
}

pub struct Bytecode {
  pub code: Vec<u8>,
  pub capacity: usize,
  pub count: usize,
  pub lines: Vec<usize>,
  pub constants: Vec<Value>,
}

impl Bytecode {
  pub fn new() -> Self {
    Self {
      code: Vec::new(),
      capacity: 0,
      count: 0,
      lines: Vec::new(),
      constants: Vec::new(),
    }
  }

  pub fn write(&mut self, byte: u8, line: usize) {
    self.code.push(byte);
    self.lines.push(line);
  }

  pub fn add_constant(&mut self, value: Value) -> usize {
    self.constants.push(value);
    self.constants.len() - 1
  }
}

pub struct BytecodeGenerator {
  pub bytecodes: HashMap<String, Bytecode>,
  pub irs: HashMap<String, Vec<IRInstruction>>,
}

impl BytecodeGenerator {
  pub fn new() -> Self {
    Self {
      bytecodes: HashMap::new(),
      irs: HashMap::new(),
    }
  }

  pub fn generate(&mut self, irs: HashMap<String, Vec<IRInstruction>>) {
    for (file_name, ir) in irs.into_iter() {
      self.bytecodes.insert(file_name.clone(), Bytecode::new());
      let bytecode = self.bytecodes.get_mut(file_name.as_str()).unwrap();

      for code in ir {
        if bytecode.capacity < bytecode.count + 1 {
          bytecode.capacity = bytecode.count + 1;
        }

        match code {
          IRInstruction::Literal(literal) => {
            match literal.value {
              AnalyzerValue::String(_) => todo!(),
              AnalyzerValue::Int(int) => {
                bytecode.write(OpCode::ConstantInt as u8, 123);
                bytecode.add_constant(Value::Int(int));
              }
              AnalyzerValue::Float(float) => {
                bytecode.write(OpCode::ConstantFloat as u8, 123);
                bytecode.add_constant(Value::Float(float));
              }
              AnalyzerValue::Boolean(_) => todo!(),
              AnalyzerValue::Return(_) => todo!(),
              AnalyzerValue::Function(_) => todo!(),
              AnalyzerValue::Null => todo!(),
              AnalyzerValue::None => todo!(),
            };
          }
          _ => todo!(),
        }

        bytecode.count += 1;
      }
    }
  }
}
