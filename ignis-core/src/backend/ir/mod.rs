use std::collections::HashMap;

use ignis_data_type::{value::IgnisLiteralValue, DataType};
use ignis_hir::{
  hir_binary::HIRBinary, hir_call::HIRCall, hir_for::HIRFor, hir_for_of::HIRForOf, hir_function::HIRFunction, hir_if::HIRIf, hir_ternary::HIRTernary, hir_while::HIRWhile, HIRInstruction, HIRMetadataFlags
};
use ir::{IRFunction, IRImport, IRInstruction, IRProgramInstruction, IRStruct, IRTypeDefinition};
use ir_flags::{IRFlag, IRFlags};
use ir_operation::{IROperation, IROperationValue};

pub mod ir;
pub mod ir_flags;
pub mod ir_operation;

#[derive(Debug, Clone, PartialEq)]
enum IRContext {
  If,
  Condition,
}

pub struct IRGenerator {
  pub programs_by_file: HashMap<String, Vec<IRProgramInstruction>>,
  programs: Vec<IRProgramInstruction>,
  temp_count: u32,
  label_count: u32,
  return_count: u32,
  current_block: Vec<IRInstruction>,
  // current_ffi_data: Vec<IgnisFFIOptions>,
  context: Vec<IRContext>,
}

impl IRGenerator {
  pub fn new() -> Self {
    Self {
      programs_by_file: HashMap::new(),
      programs: vec![],
      temp_count: 0,
      label_count: 0,
      return_count: 0,
      current_block: vec![],
      context: vec![],
    }
  }

  pub fn process(
    &mut self,
    input: &HashMap<String, Vec<HIRInstruction>>,
  ) {
    for (file, ir) in input {
      self.generate_tac(ir);
      let program = self.programs.clone();

      self.programs_by_file.insert(file.clone(), program);
      self.programs.clone_from(&Vec::new());
    }
  }

  fn generate_tac(
    &mut self,
    hir: &Vec<HIRInstruction>,
  ) {
    for instruction in hir {
      self.process_hir(instruction);
    }
  }

  fn process_hir(
    &mut self,
    hir: &HIRInstruction,
  ) {
    match hir {
      HIRInstruction::Function(function) => {
        if function.metadata.is(HIRMetadataFlags::Imported) {
          return;
        }

        self.process_hir_function(function.clone());
      },
      HIRInstruction::Block(block) => {
        for instruction in &block.instructions {
          self.process_hir(instruction);
        }
      },
      HIRInstruction::For(for_) => self.process_hir_for(for_),
      HIRInstruction::ForOf(for_) => self.process_hir_for_of(for_),
      HIRInstruction::If(hir) => self.process_hir_if(hir),
      HIRInstruction::Variable(var) => {
        let mut var_value = IROperationValue::None;
        if let Some(value) = &var.value {
          var_value.clone_from(&self.process_hir_expression_value(value));
        }

        let mut flags: IRFlags = vec![];

        for flag in &var.metadata.flags {
          flags.push(IRFlag::from(flag));
        }

        if let DataType::Vector(_, _) = var.data_type {
          flags.push(IRFlag::Vector);
        }

        let ir = IRInstruction {
          op: IROperation::Assign,
          dest: var.name.lexeme.clone(),
          type_: var.data_type.clone(),
          left: var_value,
          right: IROperationValue::None,
          flags,
        };

        self.current_block.push(ir);
      },
      HIRInstruction::Call(call) => self.process_hir_call(call),

      HIRInstruction::While(while_) => self.process_while(while_),
      _ => {
        let ir = self.process_hir_expression(hir);
        self.current_block.push(ir);
      },
    };
  }

  fn process_while(
    &mut self,
    while_: &HIRWhile,
  ) {
    let label = self.new_label();
    let return_label = self.new_label();

    self.current_block.push(IRInstruction {
      op: IROperation::Label,
      dest: label.clone(),
      type_: DataType::Unknown,
      left: IROperationValue::None,
      right: IROperationValue::None,
      flags: vec![IRFlag::Label],
    });

    self.process_hir(&while_.body);

    let condition_tmp: (IROperationValue, String) = self.new_temp();
    self.context.push(IRContext::Condition);
    let condition: IROperationValue = self.process_hir_expression_value(&while_.condition);
    self.context.pop();

    let mut flags: IRFlags = vec![IRFlag::Temporary];

    self.current_block.push(IRInstruction {
      op: IROperation::Assign,
      dest: condition_tmp.1.clone(),
      type_: DataType::Boolean,
      left: condition,
      right: IROperationValue::None,
      flags,
    });

    self.current_block.push(IRInstruction {
      op: IROperation::If,
      dest: label.clone(),
      type_: DataType::Boolean,
      left: condition_tmp.0.clone(),
      right: IROperationValue::None,
      flags: vec![],
    });

    self.current_block.push(IRInstruction {
      op: IROperation::Goto,
      dest: return_label.clone(),
      type_: DataType::Unknown,
      left: IROperationValue::None,
      right: IROperationValue::None,
      flags: vec![],
    });

    self.current_block.push(IRInstruction {
      op: IROperation::Label,
      dest: return_label.clone(),
      type_: DataType::Unknown,
      left: IROperationValue::None,
      right: IROperationValue::None,
      flags: vec![IRFlag::Label],
    });
  }

  fn process_hir_call(
    &mut self,
    call: &HIRCall,
  ) {
    let mut arguments: Vec<IROperationValue> = vec![];

    for argument in &call.arguments {
      arguments.push(self.process_hir_expression_value(argument));
    }

    let mut return_tmp = self.new_return();
    let mut flags: IRFlags = vec![IRFlag::Call, IRFlag::Temporary];

    if matches!(&call.return_type, DataType::Void) {
      return_tmp = String::new();
      flags.clone_from(&vec![IRFlag::Call]);
    }

    if call.metadata.is(HIRMetadataFlags::Extern) {
      flags.push(IRFlag::Extern);
    }

    let ir = IRInstruction {
      op: IROperation::Call,
      dest: return_tmp,
      type_: call.return_type.clone(),
      left: IROperationValue::Register {
        name: call.name.lexeme.clone(),
        type_: call.return_type.clone(),
        flags: vec![IRFlag::Function],
      },
      right: IROperationValue::Arguments { values: arguments },
      flags,
    };

    self.current_block.push(ir);
  }

  fn process_hir_if(
    &mut self,
    hir: &HIRIf,
  ) {
    self.context.push(IRContext::If);
    let condition = self.process_hir_expression_value(&hir.condition);
    self.context.pop();

    let then_label = self.new_label();
    let end_label = self.new_label();

    // Instruction 'if' that jumps to the 'then' block if the condition is true
    self.current_block.push(IRInstruction {
      op: IROperation::If,
      dest: then_label.clone(),
      type_: DataType::Boolean,
      left: condition,
      right: IROperationValue::None,
      flags: vec![],
    });

    if let Some(else_branch) = &hir.else_branch {
      // Process the 'else' block
      self.current_block.push(IRInstruction {
        op: IROperation::StartBlock,
        dest: "".to_string(),
        type_: DataType::Unknown,
        left: IROperationValue::None,
        right: IROperationValue::None,
        flags: vec![],
      });

      self.process_hir(else_branch);

      // Jump to the end of the 'else' block
      self.current_block.push(IRInstruction {
        op: IROperation::Goto,
        dest: end_label.clone(),
        type_: DataType::Unknown,
        left: IROperationValue::None,
        right: IROperationValue::None,
        flags: vec![],
      });

      self.current_block.push(IRInstruction {
        op: IROperation::EndBlock,
        dest: "".to_string(),
        type_: DataType::Unknown,
        left: IROperationValue::None,
        right: IROperationValue::None,
        flags: vec![],
      });
    } else {
      // If the condition is false, jump to the end of the 'then' block
      self.current_block.push(IRInstruction {
        op: IROperation::Goto,
        dest: end_label.clone(),
        type_: DataType::Unknown,
        left: IROperationValue::None,
        right: IROperationValue::None,
        flags: vec![],
      });
    }

    // Label for the 'then' block
    self.current_block.push(IRInstruction {
      op: IROperation::Label,
      dest: then_label.clone(),
      type_: DataType::Unknown,
      left: IROperationValue::None,
      right: IROperationValue::None,
      flags: vec![],
    });

    self.current_block.push(IRInstruction {
      op: IROperation::StartBlock,
      dest: "".to_string(),
      type_: DataType::Unknown,
      left: IROperationValue::None,
      right: IROperationValue::None,
      flags: vec![],
    });

    self.process_hir(&hir.then_branch);

    self.current_block.push(IRInstruction {
      op: IROperation::EndBlock,
      dest: "".to_string(),
      type_: DataType::Unknown,
      left: IROperationValue::None,
      right: IROperationValue::None,
      flags: vec![],
    });

    // Final label for the 'then' block
    self.current_block.push(IRInstruction {
      op: IROperation::Label,
      dest: end_label.clone(),
      type_: DataType::Unknown,
      left: IROperationValue::None,
      right: IROperationValue::None,
      flags: vec![],
    });
  }

  fn process_hir_for_of(
    &mut self,
    for_: &HIRForOf,
  ) {
    let collection = self.process_hir_expression_value(&for_.iterable);

    let index_temp = self.new_temp();
    let iterator_temp = self.new_temp();
    self.current_block.push(IRInstruction {
      op: IROperation::Label,
      dest: iterator_temp.1.clone(),
      type_: DataType::Unknown,
      left: collection.clone(),
      right: IROperationValue::None,
      flags: vec![IRFlag::Iterator],
    });

    let loop_label = self.new_label();
    let exit_label = self.new_label();

    self.current_block.push(IRInstruction {
      op: IROperation::Label,
      dest: loop_label.clone(),
      type_: DataType::Unknown,
      left: IROperationValue::None,
      right: IROperationValue::None,
      flags: vec![IRFlag::Label],
    });

    let element_temp = self.new_temp();
    self.current_block.push(IRInstruction {
      op: IROperation::VectorAccess,
      dest: element_temp.1.clone(),
      type_: DataType::Unknown,
      left: IROperationValue::Register {
        name: collection.get_name(),
        type_: collection.get_type(),
        flags: vec![],
      },
      right: IROperationValue::Register {
        name: index_temp.1.clone(),
        type_: DataType::UnsignedInt32,
        flags: vec![],
      },
      flags: vec![],
    });
  }

  fn process_hir_for(
    &mut self,
    for_: &HIRFor,
  ) {
    self.process_hir(&for_.initializer);

    let last_instruction = self.current_block.last_mut().unwrap();

    if let IROperationValue::Constant { flags, .. } = &mut last_instruction.left {
      flags.push(IRFlag::Iterator);
    }

    let label = self.new_label();
    let return_label = self.new_label();

    self.current_block.push(IRInstruction {
      op: IROperation::Label,
      dest: label.clone(),
      type_: DataType::Unknown,
      left: IROperationValue::None,
      right: IROperationValue::None,
      flags: vec![IRFlag::Label],
    });

    self.current_block.push(IRInstruction {
      op: IROperation::StartBlock,
      dest: "".to_string(),
      type_: DataType::Unknown,
      left: IROperationValue::None,
      right: IROperationValue::None,
      flags: vec![],
    });

    let condition_tmp: (IROperationValue, String) = self.new_temp();
    self.context.push(IRContext::Condition);
    let condition: IROperationValue = self.process_hir_expression_value(&for_.condition);
    self.context.pop();

    let mut flags: IRFlags = vec![IRFlag::Temporary, IRFlag::Negate];

    self.current_block.push(IRInstruction {
      op: IROperation::Assign,
      dest: condition_tmp.1.clone(),
      type_: DataType::Boolean,
      left: condition,
      right: IROperationValue::None,
      flags,
    });

    self.current_block.push(IRInstruction {
      op: IROperation::If,
      dest: return_label.clone(),
      type_: DataType::Boolean,
      left: condition_tmp.0.clone(),
      right: IROperationValue::None,
      flags: vec![],
    });

    self.process_hir(&for_.body);

    self.process_hir(&for_.increment);

    self.current_block.push(IRInstruction {
      op: IROperation::Goto,
      dest: label.clone(),
      type_: DataType::Unknown,
      left: IROperationValue::None,
      right: IROperationValue::None,
      flags: vec![],
    });

    self.current_block.push(IRInstruction {
      op: IROperation::EndBlock,
      dest: "".to_string(),
      type_: DataType::Unknown,
      left: IROperationValue::None,
      right: IROperationValue::None,
      flags: vec![],
    });

    self.current_block.push(IRInstruction {
      op: IROperation::Label,
      dest: return_label.clone(),
      type_: DataType::Unknown,
      left: IROperationValue::None,
      right: IROperationValue::None,
      flags: vec![IRFlag::Label],
    });

    self.current_block.push(IRInstruction {
      op: IROperation::StartBlock,
      dest: "".to_string(),
      type_: DataType::Unknown,
      left: IROperationValue::None,
      right: IROperationValue::None,
      flags: vec![],
    });

    self.current_block.push(IRInstruction {
      op: IROperation::EndBlock,
      dest: "".to_string(),
      type_: DataType::Unknown,
      left: IROperationValue::None,
      right: IROperationValue::None,
      flags: vec![],
    });
  }

  fn process_hir_function(
    &mut self,
    function: HIRFunction,
  ) {
    let mut flags: IRFlags = vec![];

    for flag in &function.metadata.flags {
      flags.push(IRFlag::from(flag));
    }

    let mut ir_function = IRFunction {
      name: function.name.lexeme.clone(),
      body: vec![],
      return_type: function.return_type,
      flags,
      parameters: vec![],
      // ffi_data: self.current_ffi_data.clone(),
    };

    self.current_block.clone_from(&Vec::new());
    let mut parameters: Vec<IRInstruction> = vec![];

    for parameter in &function.parameters {
      if let HIRInstruction::Variable(parameter) = parameter {
        let mut flags: IRFlags = vec![];

        for flag in &parameter.metadata.flags {
          flags.push(IRFlag::from(flag));
        }

        parameters.push(IRInstruction {
          op: IROperation::Parameter,
          dest: parameter.name.lexeme.clone(),
          type_: parameter.data_type.clone(),
          left: IROperationValue::None,
          right: IROperationValue::None,
          flags,
        });
      }
    }

    if let Some(body) = &function.body {
      for instruction in &body.instructions {
        self.process_hir(instruction);
      }
    }

    ir_function.body.clone_from(&self.current_block);
    ir_function.parameters.clone_from(&parameters);

    self.programs.push(IRProgramInstruction::Function(ir_function));
  }

  fn process_hir_expression(
    &mut self,
    ir: &HIRInstruction,
  ) -> IRInstruction {
    match ir {
      HIRInstruction::Variable(var) => {
        let mut var_value = IROperationValue::None;
        if let Some(value) = &var.value {
          var_value.clone_from(&self.process_hir_expression_value(value));
        }

        let mut flags: IRFlags = vec![];

        for flag in &var.metadata.flags {
          flags.push(IRFlag::from(flag));
        }

        if let DataType::Vector(_, _) = var.data_type {
          flags.push(IRFlag::Vector);
        }

        let ir = IRInstruction {
          op: IROperation::Assign,
          dest: var.name.lexeme.clone(),
          type_: var.data_type.clone(),
          left: var_value,
          right: IROperationValue::None,
          flags,
        };

        ir
      },
      HIRInstruction::Binary(binary) => self.process_hir_binary(binary),
      HIRInstruction::Ternary(ternary) => self.process_hir_ternary(ternary),
      _ => {
        todo!("Expression TODO: {ir:#?}")
      },
    }
  }

  fn process_hir_ternary(
    &mut self,
    ternary: &HIRTernary,
  ) -> IRInstruction {
    self.context.push(IRContext::If);
    let condition = self.process_hir_expression_value(&ternary.condition);
    self.context.pop();

    let then_label = self.new_label();
    let end_label = self.new_label();

    self.current_block.push(IRInstruction {
      op: IROperation::If,
      dest: then_label.clone(),
      type_: DataType::Boolean,
      left: condition.clone(),
      right: IROperationValue::None,
      flags: vec![],
    });

    self.process_hir(&ternary.else_branch);

    self.current_block.push(IRInstruction {
      op: IROperation::Goto,
      dest: end_label.clone(),
      type_: DataType::Unknown,
      left: IROperationValue::None,
      right: IROperationValue::None,
      flags: vec![],
    });

    self.current_block.push(IRInstruction {
      op: IROperation::Label,
      dest: then_label.clone(),
      type_: DataType::Unknown,
      left: IROperationValue::None,
      right: IROperationValue::None,
      flags: vec![IRFlag::Label],
    });

    self.process_hir(&ternary.then_branch);

    IRInstruction {
      op: IROperation::Label,
      dest: end_label.clone(),
      type_: DataType::Unknown,
      left: IROperationValue::None,
      right: IROperationValue::None,
      flags: vec![IRFlag::Label],
    }
  }

  fn process_hir_binary(
    &mut self,
    binary: &HIRBinary,
  ) -> IRInstruction {
    let left = self.process_hir_expression_value(&binary.left);
    let right = self.process_hir_expression_value(&binary.right);
    let result = self.new_temp();
    let op = IROperation::from(&binary.operator);
    let mut flags: IRFlags = vec![];

    if self.context.iter().any(|c| c == &IRContext::If) {
      flags.push(IRFlag::Condition);
    }

    flags.push(IRFlag::Temporary);

    IRInstruction {
      op,
      dest: result.1.clone(),
      type_: binary.data_type.clone(),
      left,
      right,
      flags,
    }
  }

  fn process_hir_expression_value(
    &mut self,
    ir: &HIRInstruction,
  ) -> IROperationValue {
    match ir {
      HIRInstruction::Literal(literal) => {
        let mut flags = vec![];

        if self.context.contains(&IRContext::Condition) {
          flags.push(IRFlag::Condition);
        }

        IROperationValue::Constant {
          value: literal.value.clone(),
          type_: literal.value.clone().into(),
          flags,
        }
      },
      HIRInstruction::Variable(var) => {
        let mut flags: IRFlags = vec![];

        for flag in &var.metadata.flags {
          flags.push(IRFlag::from(flag));
        }

        IROperationValue::Register {
          name: var.name.lexeme.clone(),
          type_: var.data_type.clone(),
          flags,
        }
      },
      HIRInstruction::Vector(vector) => {
        let mut values = vec![];

        for value in &vector.elements {
          values.push(self.process_hir_expression_value(value));
        }

        IROperationValue::Vector {
          type_: vector.data_type.clone(),
          length: vector.length.unwrap_or(values.len()),
          values,
        }
      },
      HIRInstruction::Binary(binary) => self.process_hir_binary_value(binary),
      HIRInstruction::Call(call) => {
        let mut arguments: Vec<IROperationValue> = vec![];
        for argument in &call.arguments {
          arguments.push(self.process_hir_expression_value(argument));
        }

        let return_tmp = self.new_return();

        let mut flags: IRFlags = vec![IRFlag::Call];

        if call.metadata.is(HIRMetadataFlags::Extern) {
          flags.push(IRFlag::Extern);
        }

        self.current_block.push(IRInstruction {
          op: IROperation::Call,
          dest: return_tmp.clone(),
          type_: call.return_type.clone(),
          left: IROperationValue::Register {
            name: call.name.lexeme.clone(),
            type_: call.return_type.clone(),
            flags: vec![],
          },
          right: IROperationValue::Arguments { values: arguments },
          flags,
        });

        IROperationValue::Register {
          name: return_tmp,
          type_: DataType::Unknown,
          flags: vec![IRFlag::Temporary, IRFlag::Call],
        }
      },
      HIRInstruction::Ternary(ternary) => self.process_hir_ternary_expression(ternary),
      _ => {
        println!("Expression value TODO: {:#?}", ir);
        todo!()
      },
    }
  }

  fn process_hir_ternary_expression(
    &mut self,
    hir: &HIRTernary,
  ) -> IROperationValue {
    self.context.push(IRContext::If);
    let condition = self.process_hir_expression_value(&hir.condition);
    self.context.pop();

    let then_label = self.new_label();
    let end_label = self.new_label();

    let result = self.new_temp();

    self.current_block.push(IRInstruction {
      op: IROperation::Assign,
      dest: result.1.clone(),
      type_: hir.data_type.clone(),
      left: IROperationValue::Constant {
        value: IgnisLiteralValue::Null,
        type_: hir.data_type.clone(),
        flags: vec![],
      },
      right: IROperationValue::None,
      flags: vec![IRFlag::Temporary],
    });

    self.current_block.push(IRInstruction {
      op: IROperation::If,
      dest: then_label.clone(),
      type_: DataType::Boolean,
      left: condition.clone(),
      right: IROperationValue::None,
      flags: vec![],
    });

    let else_branch = self.process_hir_expression_value(&hir.else_branch);

    self.current_block.push(IRInstruction {
      op: IROperation::Assign,
      dest: result.1.clone(),
      type_: hir.data_type.clone(),
      left: else_branch,
      right: IROperationValue::None,
      flags: vec![],
    });

    self.current_block.push(IRInstruction {
      op: IROperation::Goto,
      dest: end_label.clone(),
      type_: DataType::Unknown,
      left: IROperationValue::None,
      right: IROperationValue::None,
      flags: vec![],
    });

    self.current_block.push(IRInstruction {
      op: IROperation::Label,
      dest: then_label.clone(),
      type_: DataType::Unknown,
      left: IROperationValue::None,
      right: IROperationValue::None,
      flags: vec![IRFlag::Label],
    });

    let then_branch = self.process_hir_expression_value(&hir.then_branch);

    self.current_block.push(IRInstruction {
      op: IROperation::Assign,
      dest: result.1.clone(),
      type_: hir.data_type.clone(),
      left: then_branch,
      right: IROperationValue::None,
      flags: vec![],
    });

    self.current_block.push(IRInstruction {
      op: IROperation::Label,
      dest: end_label.clone(),
      type_: DataType::Unknown,
      left: IROperationValue::None,
      right: IROperationValue::None,
      flags: vec![IRFlag::Label],
    });

    result.0
  }

  fn process_hir_binary_value(
    &mut self,
    binary: &HIRBinary,
  ) -> IROperationValue {
    let left = self.process_hir_expression_value(&binary.left);
    let right = self.process_hir_expression_value(&binary.right);
    let result = self.new_temp();
    let op = IROperation::from(&binary.operator);
    let mut flags: IRFlags = vec![];

    if self.context.iter().any(|c| c == &IRContext::If) {
      flags.push(IRFlag::Condition);
    }

    flags.push(IRFlag::Temporary);

    self.current_block.push(IRInstruction {
      op,
      dest: result.1.clone(),
      type_: binary.data_type.clone(),
      left,
      right,
      flags,
    });

    result.0
  }

  fn new_temp(&mut self) -> (IROperationValue, String) {
    let name = format!("_t{}", self.temp_count);
    self.temp_count += 1;

    (
      IROperationValue::Register {
        name: name.clone(),
        type_: DataType::Unknown,
        flags: vec![IRFlag::Temporary],
      },
      name,
    )
  }

  fn new_return(&mut self) -> String {
    let return_ = format!("_ret{}", self.return_count);

    self.return_count += 1;

    return_
  }

  fn new_label(&mut self) -> String {
    let label = format!("L{}", self.label_count);

    self.label_count += 1;

    label
  }

  pub fn print(&self) {
    for (file, program) in self.programs_by_file.iter() {
      println!("======================");
      println!("File: {}", file);
      println!("======================");
      for instruction in program {
        match &instruction {
          IRProgramInstruction::Function(function) | IRProgramInstruction::Method(function) => {
            self.print_function(function)
          },
          IRProgramInstruction::Global(global) => self.print_global(global),
          IRProgramInstruction::Import(import) => self.print_import(import),
          IRProgramInstruction::Struct(s) => self.print_struct(s),
          IRProgramInstruction::Type(t) => self.print_type(t),
        }
      }
    }
  }

  pub fn programs_to_string(&self) -> Vec<(String, String)> {
    let mut programs = vec![];

    for (file, program) in self.programs_by_file.iter() {
      let mut program_string = "".to_string();
      for instruction in program {
        match &instruction {
          IRProgramInstruction::Function(function) | IRProgramInstruction::Method(function) => {
            program_string.push_str(&self.function_to_string(function))
          },
          IRProgramInstruction::Global(global) => program_string.push_str(&self.global_to_string(global)),
          IRProgramInstruction::Import(import) => program_string.push_str(&self.import_to_string(import)),
          IRProgramInstruction::Struct(st) => program_string.push_str(&self.struct_to_string(st)),
          IRProgramInstruction::Type(t) => program_string.push_str(&self.ir_type_to_string(t)),
        }
        program_string.push('\n');
      }
      programs.push((file.clone(), program_string));
    }

    programs
  }

  fn ir_type_to_string(
    &self,
    type_: &IRTypeDefinition,
  ) -> String {
    let metadata = format!(" flags: {:?}", type_.flags);
    format!("{{{{\n{}\n}}}}\ntype {} = {};", metadata, type_.name, type_.type_)
  }

  fn print_type(
    &self,
    type_: &IRTypeDefinition,
  ) {
    println!("{}", self.ir_type_to_string(type_));
  }

  fn import_to_string(
    &self,
    import: &IRImport,
  ) -> String {
    format!(
      "import {} from {}\n",
      import
        .modules
        .iter()
        .map(|(name, alias)| {
          if let Some(alias) = alias {
            format!("{} as {}", name, alias)
          } else {
            name.clone()
          }
        })
        .collect::<Vec<String>>()
        .join(","),
      import.path
    )
  }

  fn print_import(
    &self,
    import: &IRImport,
  ) {
    println!("{}", self.import_to_string(import));
  }

  fn struct_to_string(
    &self,
    st: &IRStruct,
  ) -> String {
    let metadata = format!(" flags: {:?} ", st.flags);
    let mut generics = String::new();

    if !st.generics.is_empty() {
      generics = format!(
        "<{}>",
        st.generics
          .iter()
          .map(|g| format!("{g}"))
          .collect::<Vec<String>>()
          .join(", ")
      );
    }

    format!(
      "{{{{\n{}\n}}}}\nstruct{} {} {{\n{}\n}};\n",
      metadata,
      generics,
      st.name,
      st.fields
        .iter()
        .map(|field| self.instruction_to_string(field, 2))
        .collect::<Vec<String>>()
        .join("\n"),
    )
  }

  fn print_struct(
    &self,
    st: &IRStruct,
  ) {
    let struct_ = self.struct_to_string(st);
    println!("{}", struct_);
  }

  fn function_to_string(
    &self,
    function: &IRFunction,
  ) -> String {
    let parameters = function
      .parameters
      .iter()
      .map(|param| self.instruction_to_string(param, 0))
      .collect::<Vec<String>>()
      .join(", ");

    let mut metadata = format!("\n flags: {:?},\n return_type: {}", function.flags, function.return_type);

    // if !function.ffi_data.is_empty() {
    //   metadata.push_str(&format!(
    //     ",\n ffi_data: [\n{}\n ]",
    //     function
    //       .ffi_data
    //       .iter()
    //       .map(|data| data.to_string())
    //       .collect::<Vec<String>>()
    //       .join(", ")
    //   ));
    // } else {
    metadata.push('\n');
    // }

    let header = format!(
      "# function: {}\n# signature: {} {}({})\n{{{{{}}}}}",
      function.name, function.return_type, function.name, parameters, metadata
    );

    let mut body = "".to_string();

    for instruction in &function.body {
      body.push_str(&self.instruction_to_string(instruction, 2));
      body.push('\n');
    }

    format!(
      "{}\n{} {}({}) {{\n{}}}",
      header, function.return_type, function.name, parameters, body
    )
  }

  fn print_function(
    &self,
    function: &IRFunction,
  ) {
    let function = self.function_to_string(function);
    println!("{}", function);
  }

  fn print_global(
    &self,
    global: &IRInstruction,
  ) {
    println!("{}", self.global_to_string(global))
  }

  fn global_to_string(
    &self,
    global: &IRInstruction,
  ) -> String {
    self.instruction_to_string(global, 0)
  }

  fn instruction_to_string(
    &self,
    instruction: &IRInstruction,
    indent: usize,
  ) -> String {
    let mut indent = indent;

    let expression = match &instruction.op {
      IROperation::Add => format!(
        "{} {} = {} + {}",
        instruction.type_, instruction.dest, instruction.left, instruction.right
      ),
      IROperation::Subtract => format!(
        "{} {} = {} - {}",
        instruction.type_, instruction.dest, instruction.left, instruction.right
      ),
      IROperation::Multiply => format!(
        "{} {} = {} * {}",
        instruction.type_, instruction.dest, instruction.left, instruction.right
      ),
      IROperation::Divide => format!(
        "{} {} = {} / {}",
        instruction.type_, instruction.dest, instruction.left, instruction.right
      ),
      IROperation::Mod => format!(
        "{} {} = {} % {}",
        instruction.type_, instruction.dest, instruction.left, instruction.right
      ),
      IROperation::And => format!(
        "{} {} = {} && {}",
        instruction.type_, instruction.dest, instruction.left, instruction.right
      ),
      IROperation::Or => format!(
        "{} {} = {} || {}",
        instruction.type_, instruction.dest, instruction.left, instruction.right
      ),
      IROperation::Not => format!("{} {} = !{}", instruction.type_, instruction.dest, instruction.left),
      IROperation::Equal => format!(
        "{} {} = {} == {}",
        instruction.type_, instruction.dest, instruction.left, instruction.right
      ),
      IROperation::NotEqual => format!(
        "{} {} = {} != {}",
        instruction.type_, instruction.dest, instruction.left, instruction.right
      ),
      IROperation::Greater => format!(
        "{} {} = {} > {}",
        instruction.type_, instruction.dest, instruction.left, instruction.right
      ),
      IROperation::GreaterEqual => format!(
        "{} {} = {} >= {}",
        instruction.type_, instruction.dest, instruction.left, instruction.right
      ),
      IROperation::Less => format!(
        "{} {} = {} < {}",
        instruction.type_, instruction.dest, instruction.left, instruction.right
      ),
      IROperation::LessEqual => format!(
        "{} {} = {} <= {}",
        instruction.type_, instruction.dest, instruction.left, instruction.right
      ),
      IROperation::Cast => format!(
        "{} {} = {} as {}",
        instruction.type_, instruction.dest, instruction.left, instruction.type_
      ),
      IROperation::Call => {
        if instruction.dest.is_empty() {
          format!("call {}({})", instruction.left, instruction.right)
        } else {
          format!(
            "{} {} = call {}({})",
            instruction.type_, instruction.dest, instruction.left, instruction.right
          )
        }
      },
      IROperation::Return => format!("return {}", instruction.left),
      IROperation::VectorAccess => format!(
        "{} {} = {}[{}]",
        instruction.type_, instruction.dest, instruction.left, instruction.right
      ),
      IROperation::Label => {
        indent = 0;
        format!("{}:", instruction.dest)
      },
      IROperation::Parameter => {
        return self.parameter_to_string(instruction);
      },
      IROperation::If => format!("if {} goto {}", instruction.left, instruction.dest),
      IROperation::Assign => self.format_assignment(instruction),
      IROperation::Allocate => format!("{} {} = alloc {}", instruction.type_, instruction.dest, instruction.left),
      IROperation::Declare => todo!(),
      IROperation::Constant => {
        let metadata = format!(" {:?}", instruction.flags);

        format!(
          "{} {} = {} {{{{{}}}}}",
          instruction.type_, instruction.dest, instruction.left, metadata
        )
      },
      IROperation::AssignAdd => format!("{} {} += {}", instruction.type_, instruction.dest, instruction.right),
      IROperation::AssignSub => format!("{} {} -= {}", instruction.type_, instruction.dest, instruction.right),
      IROperation::Concatenate => format!(
        "{} {} = {} + {}",
        instruction.type_, instruction.dest, instruction.left, instruction.right
      ),
      IROperation::Increment => format!("{} {} = {} + 1", instruction.type_, instruction.dest, instruction.left),
      IROperation::Decrement => format!("{} {} = {} - 1", instruction.type_, instruction.dest, instruction.left),
      IROperation::Goto => format!("goto {}", instruction.dest),
      IROperation::StartBlock => "# Start Block".to_string(),
      IROperation::EndBlock => "# End Block".to_string(),
      IROperation::This => {
        if instruction.flags.contains(&IRFlag::Call) {
          format!("this->{}({})", instruction.left, instruction.right)
        } else if matches!(&instruction.left, IROperationValue::None) {
          "this".to_string()
        } else {
          format!("this->{} = {}", instruction.dest, instruction.left)
        }
      },
      IROperation::Callable => {
        if instruction.flags.contains(&IRFlag::Pointer) {
          format!("{} (*{})({})", instruction.type_, instruction.dest, instruction.left)
        } else {
          String::new()
        }
      },
      IROperation::MethodCall => {
        if let IROperationValue::FieldAccess { base, field, .. } = &instruction.left {
          if instruction.dest.is_empty() {
            format!("{}->{}({})", base, field, instruction.right)
          } else {
            format!("{} = {}->{}({})", instruction.dest, base, field, instruction.right)
          }
        } else {
          String::new()
        }
      },
    };

    let mut metadata = format!(" flags: {:?}, type: {} ", instruction.flags, instruction.type_);

    // if !instruction.ffi_data.is_empty() {
    //   metadata.push_str(&format!(
    //     ", ffi_data: [\n{}\n ]",
    //     instruction
    //       .ffi_data
    //       .iter()
    //       .map(|data| data.to_string())
    //       .collect::<Vec<String>>()
    //       .join(", ")
    //   ));
    // }

    format!("{:indent$}{} {{{{{}}}}}", "", expression, metadata, indent = indent)
  }

  fn parameter_to_string(
    &self,
    param: &IRInstruction,
  ) -> String {
    let mut modifier = String::new();

    if param.flags.contains(&IRFlag::Field) {
      return self.format_field(param);
    }

    if param.flags.contains(&IRFlag::Variadic) {
      modifier.push_str("...");
    }

    if param.flags.contains(&IRFlag::Reference) {
      modifier.push('&');
    }

    if param.flags.contains(&IRFlag::Mutable) {
      modifier.push_str("mut ");
    }

    modifier.push_str(&param.type_.to_string());

    if param.flags.contains(&IRFlag::Pointer) {
      modifier.push('*');
    }

    format!("{} {}", modifier, param.dest)
  }

  fn _print_instruction(
    &self,
    instruction: &IRInstruction,
    indent: usize,
  ) {
    let expression = self.instruction_to_string(instruction, indent);

    println!("{}", expression);
  }

  fn format_assignment(
    &self,
    assignment: &IRInstruction,
  ) -> String {
    if assignment.flags.contains(&IRFlag::Field) {
      return self.format_field(assignment);
    }

    let mut modifier = String::new();
    if assignment.flags.contains(&IRFlag::Reference) {
      modifier.push('&');
    }

    if assignment.flags.contains(&IRFlag::Mutable) {
      modifier.push_str("mut ");
    }

    if assignment.flags.contains(&IRFlag::Pointer) {
      modifier.push('*');
    }

    format!("{} {}{} = {}", assignment.type_, modifier, assignment.dest, assignment.left)
  }

  fn format_field(
    &self,
    field: &IRInstruction,
  ) -> String {
    let mut metadata = String::new();

    if field.flags.contains(&IRFlag::Static) {
      metadata.push_str("static ");
    }

    if field.flags.contains(&IRFlag::Reference) {
      metadata.push('&');
    }

    if field.flags.contains(&IRFlag::Mutable) {
      metadata.push_str("mut ");
    }

    if field.flags.contains(&IRFlag::Pointer) {
      metadata.push('*');
    }

    format!("{}{} {};", metadata, field.type_, field.dest)
  }
}
