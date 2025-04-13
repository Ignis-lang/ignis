use std::{collections::HashMap, fmt::format};

use ignis_data_type::{value::IgnisLiteralValue, DataType};
use ignis_hir::{
  hir_binary::HIRBinary, hir_call::HIRCall, hir_enum::HIREnum, hir_extern::HIRExtern, hir_for::HIRFor,
  hir_for_of::HIRForOf, hir_function::HIRFunction, hir_if::HIRIf, hir_import::HIRImport, hir_literal::HIRLiteral,
  hir_member_access::HIRMemberAccess, hir_method::HIRMethod, hir_namespace::HIRNamespace, hir_object::HIRObjectLiteral,
  hir_record::HIRRecord, hir_return::HIRReturn, hir_ternary::HIRTernary, hir_type::HIRType, hir_while::HIRWhile,
  HIRInstruction, HIRMetadataFlags,
};
use ir::{IREnum, IREnumValue, IRFunction, IRImport, IRInstruction, IRProgramInstruction, IRStruct, IRTypeDefinition};
use ir_flags::{IRFlag, IRFlags};
use ir_operation::{IROperation, IROperationValue};

pub mod ir;
pub mod ir_flags;
pub mod ir_operation;

#[derive(Debug, Clone, PartialEq)]
enum IRContext {
  If,
  Condition,
  Namespace(String),
}

pub struct IRGenerator {
  pub programs_by_file: HashMap<String, Vec<IRProgramInstruction>>,
  programs: Vec<IRProgramInstruction>,
  temp_count: u32,
  label_count: u32,
  return_count: u32,
  object_count: u32,
  current_block: Vec<IRInstruction>,
  // current_ffi_data: Vec<IgnisFFIOptions>,
  context: Vec<IRContext>,
  rename_map: HashMap<String, String>,
}

impl IRGenerator {
  pub fn new() -> Self {
    Self {
      programs_by_file: HashMap::new(),
      programs: vec![],
      temp_count: 0,
      label_count: 0,
      return_count: 0,
      object_count: 0,
      current_block: vec![],
      context: vec![],
      rename_map: HashMap::new(),
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
      HIRInstruction::Record(record) => self.process_hir_record(record),
      HIRInstruction::While(while_) => self.process_while(while_),
      HIRInstruction::Enum(enum_) => {
        if enum_.metadata.is(HIRMetadataFlags::Imported) {
          return;
        }

        self.process_enum(enum_);
      },
      HIRInstruction::Extern(extern_) => self.process_extern(extern_),
      HIRInstruction::Namespace(namespace) => self.process_hir_namespace(namespace),
      HIRInstruction::Import(import) => self.process_import(import),
      HIRInstruction::Type(type_) => self.process_hir_type(type_),
      HIRInstruction::Meta(_) => {},
      _ => {
        let ir = self.process_hir_expression(hir);
        self.current_block.push(ir);
      },
    };
  }

  fn process_hir_type(
    &mut self,
    type_: &HIRType,
  ) {
    if type_.metadata.is(HIRMetadataFlags::Imported) {
      return;
    }

    if let DataType::UnionType(values) = type_.value.as_ref() {}

    // The statement `type` is a type alias, so when we process it we don't need
    // make any changes to the IR.
    // TODO: Implement remplacement of type aliases

    return;
  }

  fn process_hir_namespace(
    &mut self,
    namespace: &HIRNamespace,
  ) {
    if namespace.metadata.is(HIRMetadataFlags::Imported) {
      return;
    }

    self.context.push(IRContext::Namespace(namespace.name.lexeme.clone()));

    for statement in &namespace.members {
      self.process_hir(statement);
    }
  }

  fn process_extern(
    &mut self,
    extern_: &HIRExtern,
  ) {
    if extern_.metadata.is(HIRMetadataFlags::Imported) {
      return;
    }

    for statement in &extern_.body {
      self.process_hir(statement);
    }
  }

  fn process_import(
    &mut self,
    import: &HIRImport,
  ) {
    self.programs.push(IRProgramInstruction::Import(IRImport {
      modules: import
        .name
        .iter()
        .map(|(name, alias)| {
          if let Some(alias) = alias {
            (name.lexeme.clone(), Some(alias.lexeme.clone()))
          } else {
            (name.lexeme.clone(), None)
          }
        })
        .collect(),
      path: import.path.clone(),
      flags: vec![],
      extra_path: None,
    }));
  }

  fn process_enum(
    &mut self,
    _enum: &HIREnum,
  ) {
    let name = _enum.name.lexeme.to_uppercase();
    let mut enum_members: Vec<IREnumValue> = vec![];

    for var in &_enum.members {
      if var.metadata.is(HIRMetadataFlags::Complex) {
        todo!()
      }

      enum_members.push(IREnumValue::Simple {
        name: var.name.lexeme.clone(),
        discriminant: var.simple(),
      });
    }

    self.programs.push(IRProgramInstruction::Enum(IREnum {
      name,
      values: enum_members,
      flags: _enum.metadata.flags.iter().map(IRFlag::from).collect(),
    }));
  }

  fn process_hir_record(
    &mut self,
    record: &HIRRecord,
  ) {
    if record.metadata.is(HIRMetadataFlags::Imported) {
      return;
    }

    let mut name = record.name.lexeme.clone();

    if record.metadata.is(HIRMetadataFlags::NamespaceMember) {
      if let IRContext::Namespace(namespace) = self.context.last().unwrap() {
        name = format!("{}__{}", namespace, name);
      }
    }

    let generics = record.generic_parameters.clone();

    let mut fields = vec![];

    for field in &record.items {
      if let HIRInstruction::Variable(var) = field {
        let mut flags: IRFlags = vec![];

        for flag in &var.metadata.flags {
          flags.push(IRFlag::from(flag));
        }

        let ir = IRInstruction {
          op: IROperation::Assign,
          dest: var.name.lexeme.clone(),
          type_: var.data_type.clone(),
          left: IROperationValue::None,
          right: IROperationValue::None,
          flags,
        };

        fields.push(ir);
      }

      if let HIRInstruction::Method(m) = field {
        let mut flags: IRFlags = vec![IRFlag::Pointer];

        for flag in &m.metadata.flags {
          flags.push(IRFlag::from(flag));
        }

        let mut parameters: Vec<IROperationValue> = vec![];

        let this_flag: IRFlags = vec![IRFlag::Mutable, IRFlag::Pointer];

        parameters.push(IROperationValue::Register {
          name: "this".to_string(),
          type_: record.data_type.clone(),
          flags: this_flag,
        });

        for parameter in &m.parameters {
          if let HIRInstruction::Variable(parameter) = parameter {
            let flags: IRFlags = parameter.metadata.flags.iter().map(IRFlag::from).collect();

            parameters.push(IROperationValue::Register {
              name: parameter.name.lexeme.clone(),
              type_: parameter.data_type.clone(),
              flags,
            });
          }
        }

        let ir = IRInstruction {
          op: IROperation::Callable,
          dest: m.name.lexeme.clone(),
          type_: m.return_type.clone(),
          left: IROperationValue::Arguments { values: parameters },
          right: IROperationValue::None,
          flags,
        };

        fields.push(ir)
      }
    }

    let flags: IRFlags = record.metadata.flags.iter().map(IRFlag::from).collect();

    let struct_ = IRStruct {
      name,
      fields,
      generics,
      flags,
    };

    self.programs.push(IRProgramInstruction::Struct(struct_));
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

    let flags: IRFlags = vec![IRFlag::Temporary];

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
    let _exit_label = self.new_label();

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

    let flags: IRFlags = vec![IRFlag::Temporary, IRFlag::Negate];

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

    let mut name = function.name.lexeme.clone();

    if function.metadata.is(HIRMetadataFlags::NamespaceMember) {
      if let IRContext::Namespace(namespace) = self.context.last().unwrap() {
        name = format!("{}__{}", namespace, name);
      }
    }

    let mut ir_function = IRFunction {
      name,
      body: vec![],
      return_type: function.return_type,
      flags,
      parameters: vec![],
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
      HIRInstruction::Return(_return) => self.process_hir_return(_return),
      HIRInstruction::MethodCall(call) => {
        let mut arguments: Vec<IROperationValue> = vec![];
        for argument in &call.arguments {
          arguments.push(self.process_hir_expression_value(argument));
        }

        let mut flags: IRFlags = vec![IRFlag::Call, IRFlag::Method];

        if call.metadata.is(HIRMetadataFlags::ExternMember) {
          flags.push(IRFlag::ExternMember);

          return IRInstruction {
            op: IROperation::Call,
            dest: String::new(),
            type_: call.return_type.clone(),
            left: IROperationValue::Register {
              name: call.name.lexeme.clone(),
              type_: call.return_type.clone(),
              flags: vec![IRFlag::Function],
            },
            right: IROperationValue::Arguments { values: arguments },
            flags,
          };
        }

        if call.metadata.is(HIRMetadataFlags::NamespaceMember) {
          flags.push(IRFlag::NamespaceMember);
        }

        IRInstruction {
          op: IROperation::MethodCall,
          dest: String::new(),
          type_: call.return_type.clone(),
          left: IROperationValue::FieldAccess {
            base: call.calle.get_name().lexeme.clone(),
            field: call.name.lexeme.clone(),
            type_: call.return_type.clone(),
            flags: vec![IRFlag::Function],
            base_type: call.calle.extract_data_type(),
          },
          right: IROperationValue::Arguments { values: arguments },
          flags,
        }
      },
      _ => {
        todo!("Expression TODO: {ir:#?}")
      },
    }
  }

  fn process_hir_member_access(
    &mut self,
    member: &HIRMemberAccess,
  ) -> IROperationValue {
    IROperationValue::FieldAccess {
      base: member.object.get_name().lexeme.clone(),
      field: member.member.get_name().lexeme.clone(),
      type_: member.member.extract_data_type(),
      flags: vec![IRFlag::Field],
      base_type: member.object.extract_data_type(),
    }
  }

  fn process_literal(
    &mut self,
    literal: &HIRLiteral,
  ) -> IROperationValue {
    let value = literal.value.clone();
    let type_ = literal.value.clone().into();
    let mut flags: IRFlags = vec![];

    if self.context.contains(&IRContext::Condition) {
      flags.push(IRFlag::Condition);
    }

    IROperationValue::Constant { value, type_, flags }
  }

  fn process_hir_return(
    &mut self,
    _return: &HIRReturn,
  ) -> IRInstruction {
    let value = self.process_hir_expression_value(&_return.value);

    IRInstruction {
      op: IROperation::Return,
      dest: "".to_string(),
      type_: _return.data_type.clone(),
      left: value,
      right: IROperationValue::None,
      flags: vec![],
    }
  }

  fn process_hir_object(
    &mut self,
    object: &HIRObjectLiteral,
  ) -> IROperationValue {
    let mut properties = Vec::<(String, IROperationValue)>::new();

    for (name, value) in &object.properties {
      let value = self.process_hir_expression_value(value);

      properties.push((name.lexeme.clone(), value));
    }

    for method in &object.methods {
      let result = self.process_hir_method(method);

      properties.push((method.name.lexeme.clone(), result));
    }

    IROperationValue::ObjectLiteral {
      properties,
      type_: object.data_type.clone(),
      flags: vec![],
    }
  }

  fn process_hir_method(
    &mut self,
    method: &HIRMethod,
  ) -> IROperationValue {
    let mut flags: IRFlags = vec![];

    for flag in &method.metadata.flags {
      flags.push(IRFlag::from(flag));
    }

    let name = if method.name.lexeme == method.object.lexeme {
      format!("__object{}_{}", self.object_count, method.name.lexeme)
    } else {
      format!("{}_{}", method.object.lexeme, method.name.lexeme)
    };

    let mut ir_function = IRFunction {
      name,
      body: vec![],
      return_type: method.return_type.clone(),
      flags,
      parameters: vec![],
    };

    self.current_block.clone_from(&Vec::new());
    let mut parameters: Vec<IRInstruction> = vec![];
    let mut parameters_data_type = vec![];

    parameters.push(IRInstruction {
      op: IROperation::Parameter,
      dest: String::from("this"),
      type_: DataType::StructType(method.object.lexeme.clone()),
      left: IROperationValue::None,
      right: IROperationValue::None,
      flags: vec![IRFlag::Pointer, IRFlag::Mutable],
    });

    for parameter in &method.parameters {
      if let HIRInstruction::Variable(parameter) = parameter {
        let mut flags: IRFlags = vec![];

        for flag in &parameter.metadata.flags {
          flags.push(IRFlag::from(flag));
        }
        parameters_data_type.push(parameter.data_type.clone());

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

    if let Some(body) = &method.body {
      for instruction in &body.instructions {
        self.process_hir(instruction);
      }
    }

    ir_function.body.clone_from(&self.current_block);
    ir_function.parameters.clone_from(&parameters);

    self.programs.push(IRProgramInstruction::Function(ir_function.clone()));

    IROperationValue::Function {
      name: ir_function.name.clone(),
      parameters: parameters_data_type,
      return_type: method.return_type.clone(),
      flags: vec![],
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
      HIRInstruction::Literal(literal) => self.process_literal(literal),
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
      HIRInstruction::Object(object) => self.process_hir_object(object),
      HIRInstruction::MemberAccess(member) => self.process_hir_member_access(member),
      HIRInstruction::MethodCall(call) => {
        let mut arguments: Vec<IROperationValue> = vec![];

        for argument in &call.arguments {
          arguments.push(self.process_hir_expression_value(argument));
        }

        let return_tmp = self.new_return();

        let mut flags = vec![IRFlag::Call, IRFlag::Method];

        for flag in &call.metadata.flags {
          flags.push(IRFlag::from(flag));
        }

        if flags.contains(&IRFlag::ExternMember) {
          self.current_block.push(IRInstruction {
            op: IROperation::Call,
            dest: return_tmp.clone(),
            type_: call.return_type.clone(),
            left: IROperationValue::Register {
              name: call.name.lexeme.clone(),
              type_: call.return_type.clone(),
              flags: vec![IRFlag::Function],
            },
            right: IROperationValue::Arguments { values: arguments },
            flags,
          });
        } else {
          self.current_block.push(IRInstruction {
            op: IROperation::MethodCall,
            dest: return_tmp.clone(),
            type_: call.return_type.clone(),
            left: IROperationValue::FieldAccess {
              base: call.calle.get_name().lexeme.clone(),
              field: call.name.lexeme.clone(),
              type_: call.return_type.clone(),
              flags: vec![IRFlag::Function],
              base_type: call.calle.extract_data_type(),
            },
            right: IROperationValue::Arguments { values: arguments },
            flags,
          });
        }

        let flags: IRFlags = vec![IRFlag::Temporary, IRFlag::Call];

        IROperationValue::Register {
          name: return_tmp,
          type_: DataType::Unknown,
          flags,
        }
      },
      HIRInstruction::Spread(spread) => {
        unimplemented!()
      },
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
          IRProgramInstruction::Enum(e) => self.print_enum(e),
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
          IRProgramInstruction::Enum(e) => program_string.push_str(&self.enum_to_string(e)),
        }
        program_string.push('\n');
      }
      programs.push((file.clone(), program_string));
    }

    programs
  }

  fn enum_to_string(
    &self,
    _enum_: &IREnum,
  ) -> String {
    let mut metadata = format!(" flags: {:?}", _enum_.flags);

    if !_enum_.values.is_empty() {
      metadata.push_str("\n");
    }

    let mut values = String::new();

    for (index, value) in _enum_.values.iter().enumerate() {
      match value {
        IREnumValue::Simple { name, discriminant } => {
          values.push_str(&format!("{}{} = {},n", "  ".repeat(2), name, discriminant))
        },
        IREnumValue::Complex { .. } => {
          todo!()
        },
      }

      if index != _enum_.values.len() - 1 {
        values.push('\n');
      }
    }

    format!("{{{{\n{}\n}}}}\nenum {} {{\n{}\n}};\n", metadata, _enum_.name, values)
  }

  fn ir_type_to_string(
    &self,
    type_: &IRTypeDefinition,
  ) -> String {
    let metadata = format!(" flags: {:?}", type_.flags);
    format!(
      "{{{{\n{}\n}}}}\ntype {} = {};",
      metadata,
      type_.name,
      type_.type_.to_ir_format()
    )
  }

  pub fn print_enum(
    &self,
    enum_: &IREnum,
  ) {
    println!("{}", self.enum_to_string(enum_));
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

    let mut metadata = format!(
      "\n flags: {:?},\n return_type: {}",
      function.flags,
      function.return_type.to_ir_format()
    );

    metadata.push('\n');

    let header = format!("# {{{{{}}}}}", metadata);

    let mut body = "".to_string();

    for instruction in &function.body {
      body.push_str(&self.instruction_to_string(instruction, 2));
      body.push('\n');
    }

    let mut str_body = if function.flags.contains(&IRFlag::ExternMember) {
      ";\n".to_string()
    } else {
      format!(" {{\n{}}}", body)
    };

    format!(
      "{}\n{}{} {}({}){}",
      header,
      if function.flags.contains(&IRFlag::ExternMember) {
        "extern "
      } else {
        ""
      },
      function.return_type.to_ir_format(),
      function.name,
      parameters,
      str_body
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

    let type_ = instruction.type_.to_ir_format();

    let expression = match &instruction.op {
      IROperation::Add => format!("{} {} = {} + {}", type_, instruction.dest, instruction.left, instruction.right),
      IROperation::Subtract => format!("{} {} = {} - {}", type_, instruction.dest, instruction.left, instruction.right),
      IROperation::Multiply => format!("{} {} = {} * {}", type_, instruction.dest, instruction.left, instruction.right),
      IROperation::Divide => format!("{} {} = {} / {}", type_, instruction.dest, instruction.left, instruction.right),
      IROperation::Mod => format!("{} {} = {} % {}", type_, instruction.dest, instruction.left, instruction.right),
      IROperation::And => format!("{} {} = {} && {}", type_, instruction.dest, instruction.left, instruction.right),
      IROperation::Or => format!("{} {} = {} || {}", type_, instruction.dest, instruction.left, instruction.right),
      IROperation::Not => format!("{} {} = !{}", type_, instruction.dest, instruction.left),
      IROperation::Equal => format!("{} {} = {} == {}", type_, instruction.dest, instruction.left, instruction.right),
      IROperation::NotEqual => {
        format!("{} {} = {} != {}", type_, instruction.dest, instruction.left, instruction.right)
      },
      IROperation::Greater => format!("{} {} = {} > {}", type_, instruction.dest, instruction.left, instruction.right),
      IROperation::GreaterEqual => {
        format!("{} {} = {} >= {}", type_, instruction.dest, instruction.left, instruction.right)
      },
      IROperation::Less => format!("{} {} = {} < {}", type_, instruction.dest, instruction.left, instruction.right),
      IROperation::LessEqual => {
        format!("{} {} = {} <= {}", type_, instruction.dest, instruction.left, instruction.right)
      },
      IROperation::Cast => format!("{} {} = {} as {}", type_, instruction.dest, instruction.left, type_),
      IROperation::Call => {
        if instruction.dest.is_empty() {
          format!("call {}({})", instruction.left, instruction.right)
        } else {
          format!(
            "{} {} = call {}({})",
            type_, instruction.dest, instruction.left, instruction.right
          )
        }
      },
      IROperation::Return => format!("return {}", instruction.left),
      IROperation::VectorAccess => {
        format!("{} {} = {}[{}]", type_, instruction.dest, instruction.left, instruction.right)
      },
      IROperation::Label => {
        indent = 0;
        format!("{}:", instruction.dest)
      },
      IROperation::Parameter => {
        return self.parameter_to_string(instruction);
      },
      IROperation::If => format!("if {} goto {}", instruction.left, instruction.dest),
      IROperation::Assign => self.format_assignment(instruction),
      IROperation::Allocate => format!("{} {} = alloc {}", type_, instruction.dest, instruction.left),
      IROperation::Declare => todo!(),
      IROperation::Constant => {
        let metadata = format!(" {:?}", instruction.flags);

        format!(
          "const {} {} = {} {{{{{}}}}}",
          type_, instruction.dest, instruction.left, metadata
        )
      },
      IROperation::AssignAdd => format!("{} {} += {}", type_, instruction.dest, instruction.right),
      IROperation::AssignSub => format!("{} {} -= {}", type_, instruction.dest, instruction.right),
      IROperation::Concatenate => {
        format!("{} {} = {} + {}", type_, instruction.dest, instruction.left, instruction.right)
      },
      IROperation::Increment => format!("{} {} = {} + 1", type_, instruction.dest, instruction.left),
      IROperation::Decrement => format!("{} {} = {} - 1", type_, instruction.dest, instruction.left),
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
          format!("{} (*{})({})", type_, instruction.dest, instruction.left)
        } else {
          String::new()
        }
      },
      IROperation::MethodCall => {
        if let IROperationValue::FieldAccess { base, field, .. } = &instruction.left {
          let operator = if instruction.flags.contains(&IRFlag::ObjectMember) {
            "."
          } else if instruction.flags.contains(&IRFlag::NamespaceMember) {
            "__"
          } else {
            "->"
          };

          if instruction.dest.is_empty() {
            format!("{}{}{}({})", base, operator, field, instruction.right)
          } else {
            format!("{} = {}{}{}({})", instruction.dest, base, operator, field, instruction.right)
          }
        } else {
          String::new()
        }
      },
    };

    let metadata = format!(" flags: {:?}, type: {} ", instruction.flags, type_);

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

    modifier.push_str(&param.type_.to_ir_format());

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
    if assignment.flags.contains(&IRFlag::Property) {
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

    format!(
      "{} {}{} = {}",
      assignment.type_.to_ir_format(),
      modifier,
      assignment.dest,
      assignment.left
    )
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

    format!("{}{} {};", metadata, field.type_.to_ir_format(), field.dest)
  }
}
