use crate::ir::instruction::{IRInstruction, block::IRBlock, variable::IRVariable, literal::IRLiteral};

pub fn display_ir(instruction: &IRInstruction, indent_level: usize) {
  let indent = "  ".repeat(indent_level);
  let indent_subtext = indent.repeat(2);
  let indent_level = indent_level + 1;

  match instruction {
    IRInstruction::Binary(b) => {
      println!("{}Binary:", indent);
      println!("{}Left:", indent_subtext);
      display_ir(&b.left, indent_level);
      println!("{}Right:", indent_subtext);
      display_ir(&b.right, indent_level);
      println!(
        "{}Instructin Type: {:?}",
        indent_subtext, b.instruction_type
      );
    }
    IRInstruction::Block(b) => {
      display_block(b, "Block", indent_level);
    }
    IRInstruction::Literal(l) => {
      display_literal(l, indent_level);
    }
    IRInstruction::Unary(u) => {
      println!("{}Unary:", indent);
      println!("{}Right:", indent_subtext);
      display_ir(&u.right, indent_level);
      println!(
        "{}Instructin Type: {:?}",
        indent_subtext, u.instruction_type
      );
    }
    IRInstruction::Variable(v) => {
      display_variable(v, indent_level);
    }
    IRInstruction::Logical(l) => {
      println!("{}Logical:", indent);
      println!("{}Left:", indent_subtext);
      display_ir(&l.left, indent_level);
      println!("{}Right:", indent_subtext);
      display_ir(&l.right, indent_level);
      println!(
        "{}Instructin Type: {:?}",
        indent_subtext, l.instruction_type
      );
    }
    IRInstruction::If(i) => {
      println!("{}If:", indent);

      println!("{}Condition:", indent_subtext);
      display_ir(&i.condition, indent_level);

      println!("{}Then:", indent_subtext);
      display_ir(&i.then_branch, indent_level);

      if let Some(else_branch) = &i.else_branch {
        println!("{}Else:", indent_subtext);
        display_ir(else_branch, indent_level);
      }
    }
    IRInstruction::While(w) => {
      println!("{}While:", indent);
      println!("{}Condition:", indent_subtext);

      display_ir(&w.condition, indent_level);

      println!("{}Body:", indent_subtext);
      display_ir(&w.body, indent_level);
    }
    IRInstruction::Function(f) => {
      println!("{}Function:", indent);
      println!("{}Name: {}", indent_subtext, f.name);

      println!("{}Parameters:", indent_subtext);
      let indent_parameter = format!("{}  ", indent_subtext);
      for parameter in &f.parameters {
        println!("{}Name: {}", indent_parameter, parameter.name);
        println!("{}DataType: {:?}", indent_parameter, parameter.data_type);
        println!(
          "{}Is mutable: {:?}",
          indent_parameter, parameter.metadata.is_mutable
        );
        println!(
          "{}Is reference: {:?}",
          indent_parameter, parameter.metadata.is_reference
        );
      }

      println!("{}Return type: {:?}", indent_subtext, f.return_type);

      if let Some(body) = &f.body {
        display_block(&body.clone(), &f.name, indent_level);
      }
    }
    IRInstruction::Call(c) => {
      println!("{}Call:", indent);
      println!("{}Name: {}", indent_subtext, c.name);
      println!("{}Arguments:", indent_subtext);

      if c.arguments.is_empty() {
        println!("{}Empty", indent_subtext.repeat(2));
      } else {
        for argument in &c.arguments {
          display_ir(argument, indent_level);
        }
      }
    }
    IRInstruction::Return(r) => {
      println!("{}Return:", indent);
      println!("{}Value:", indent_subtext);
      display_ir(&r.value, indent_level);
    }
    IRInstruction::Assign(a) => {
      println!("{}Assign:", indent);
      println!("{}Name: {}", indent_subtext, a.name);

      println!("{}Value:", indent_subtext);
      display_ir(&a.value, indent_level);
    }
    IRInstruction::Class(class) => {
      println!("{}Class:", indent);
      println!("{}Name: {}", indent_subtext, class.name);
      println!("{}Methods:", indent_subtext);
      if class.methods.is_empty() {
        println!("{}Empty", indent_subtext.repeat(2));
      } else {
        for method in &class.methods {
          display_ir(&IRInstruction::Function(method.clone()), indent_level);
        }
      }
      println!("{}Properties:", indent_subtext);
      if class.properties.is_empty() {
        println!("{}Empty", indent_subtext.repeat(2));
      } else {
        for property in &class.properties {
          display_ir(&IRInstruction::Variable(property.clone()), indent_level);
        }
      }
    }
    IRInstruction::Ternary(t) => {
      println!("{}Ternary:", indent);
      println!("{}Condition:", indent_subtext);
      display_ir(&t.condition, indent_level);

      println!("{}Then:", indent_subtext);
      display_ir(&t.then_branch, indent_level);

      println!("{}Else:", indent_level);
      display_ir(&t.else_branch, indent_level);
    }
    IRInstruction::ForIn(for_in) => {
      println!("{}ForIn:", indent);
      println!("{}Variable:", indent_subtext);
      display_variable(&for_in.variable, indent_level);

      println!("{}Iterable:", indent_subtext);
      display_ir(&for_in.iterable, indent_level);

      println!("{}Body:", indent_subtext);
      display_ir(&for_in.body, indent_level);
    }
    IRInstruction::Array(array) => {
      println!("{}Array:", indent);
      println!("{}Elements:", indent_subtext);

      if array.elements.is_empty() {
        println!("{}Empty", indent_subtext.repeat(2));
      } else {
        for element in &array.elements {
          display_ir(element, indent_level);
        }
      }
    }
    IRInstruction::Import(import) => {
      println!(
        "{}Import: modules: {} | path: {}",
        indent,
        import
          .name
          .iter()
          .map(|(name, _)| name.span.literal.clone())
          .collect::<Vec<String>>()
          .join("."),
        import.path
      );
    }
    IRInstruction::Break(_) => {
      println!("{}Break", indent);
    }
    IRInstruction::Continue(_) => {
      println!("{}Continue", indent);
    }
    IRInstruction::Get(get) => {
      println!("{}Get:", indent);
      println!("{}Object:", indent_subtext);
      // display_ir(&*get.object, indent_level);
      println!("{}Property: {}", indent_subtext, get.name);
      println!("{}DataType: {}", indent_subtext, get.data_type);
    }
  };
}

pub fn display_block(block: &IRBlock, owner: &str, indent_level: usize) {
  let indent = "  ".repeat(indent_level);
  let indent_subtext = indent.clone();

  println!(
    "{}Block, owner {}:",
    if indent_level > 1 {
      indent
    } else {
      "".to_string()
    },
    owner
  );

  println!("{}Instructions:", indent_subtext);
  if block.instructions.is_empty() {
    println!("{}Empty", indent_subtext.repeat(2));
  } else {
    for instruction in &block.instructions {
      display_ir(instruction, indent_level + 1);
    }
  }

  println!("{}Scopes variables:", indent_subtext);
  if block.scopes_variables.is_empty() {
    println!("{}Empty", indent_subtext.repeat(2));
  } else {
    for var in &block.scopes_variables {
      display_variable(var, indent_level + 1);
    }
  }
}

pub fn display_variable(variable: &IRVariable, indent_level: usize) {
  let indent = " ".repeat(indent_level);
  let indent_subtext = indent.repeat(2);

  println!("{}Variable:", indent);
  println!("{}Name: {}", indent_subtext, variable.name);
  println!("{}DataType: {:?}", indent_subtext, variable.data_type);
  println!(
    "{}Is mutable: {:?}",
    indent_subtext, variable.metadata.is_mutable
  );
  println!(
    "{}Is reference: {:?}",
    indent_subtext, variable.metadata.is_reference
  );

  if let Some(value) = &variable.value {
    println!("{}Value:", indent_subtext);
    display_ir(value, indent_level + 1);
  }
}

pub fn display_literal(literal: &IRLiteral, indent_level: usize) {
  let indent = " ".repeat(indent_level);
  let indent_subtext = indent.repeat(2);

  println!("{}Literal:", indent);
  println!("{}Value: {:?}", indent_subtext, literal.value);
}
