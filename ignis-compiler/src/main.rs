mod ast;

use std::{
  io::{self, Write, BufRead},
  env,
  process::exit,
  fs,
};

use ast::{
  parser::Parser,
  lexer::Lexer,
  expression,
  statement::{self, Statement},
  evaluator::{Evaluator, self},
  visitor::Visitor,
  Ast,
};

fn run_file(path: &str) -> Result<(), String> {
  let mut evaluator = Evaluator::new();

  match fs::read_to_string(path) {
    Ok(content) => run(content, &mut evaluator),
    Err(message) => Err(message.to_string()),
  }
}

fn run(source: String, evaluator: &mut Evaluator) -> Result<(), String> {
  if source.trim() == String::from("exit") {
    println!("Bye!");
    exit(0);
  }

  let mut lexer: Lexer<'_> = Lexer::new(&source);
  lexer.scan_tokens();

  // for token in &lexer.tokens {
  //   println!("{:?}", token);
  // }

  let mut parser = Parser::new(lexer.tokens);
  let expressions = parser.parse();
  let mut ast = Ast::new();

  match expressions {
    Ok(statements) => {
      for statement in statements {
        ast.add(statement);
      }
      ast.visit(evaluator);
    }
    Err(errors) => {
      for error in errors {
        println!("{:?}", error);
      }
    }
  }

  Ok(())
}

fn run_prompt() -> Result<(), String> {
  let mut evaluator = Evaluator::new();

  loop {
    print!("(ignis) > ");

    match io::stdout().flush() {
      Ok(_) => (),
      Err(_) => return Err("Could not flush stdout".to_string()),
    }
    let mut buffer = String::new();
    let mut handler = io::stdin().lock();

    match handler.read_line(&mut buffer) {
      Ok(n) => {
        if n == 0 {
          println!("");
          return Ok(());
        } else if n == 1 {
          continue;
        }
      }
      Err(_) => return Err("Clound not read line".to_string()),
    }

    match run(buffer, &mut evaluator) {
      Ok(_) => (),
      Err(message) => println!("{}", message),
    }
  }
}

fn main() {
  let args: Vec<String> = env::args().collect();

  if args.len() == 2 {
    match run_file(&args[1]) {
      Ok(_) => exit(0),
      Err(msg) => {
        println!("ERROR\n{}", msg);
        exit(1);
      }
    }
  } else {
    match run_prompt() {
      Ok(_) => exit(0),
      Err(msg) => {
        println!("ERROR\n{}", msg);
        exit(1);
      }
    }
  }
}
