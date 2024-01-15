use std::collections::HashMap;

use crate::{Bytecode, OpCode};

pub struct BytecodeDebug {
  pub bytecodes: HashMap<String, Bytecode>,
}

impl BytecodeDebug {
  pub fn new(bytecodes: HashMap<String, Bytecode>) -> Self {
    Self { bytecodes }
  }

  pub fn print_bytecode(&self) {
    for (file_name, code) in self.bytecodes.iter() {
      println!("Bytecode for {}", file_name);

      let mut i = 0;

      for byte in code.code.iter() {
        let op = OpCode::from(byte.clone());
        match op {
          OpCode::ConstantInt | OpCode::ConstantFloat => {
            let constant = code.constants[i];
            println!("{:05}\t{:?}\t{}", i, op, constant);
          }
          OpCode::Add => todo!(),
          OpCode::Subtract => todo!(),
          OpCode::Multiply => todo!(),
          OpCode::Divide => todo!(),
          OpCode::Negate => todo!(),
          OpCode::Return => todo!(),
        };

        i += 1;

        if i == code.count {
          break;
        }
      }
    }
  }
}
