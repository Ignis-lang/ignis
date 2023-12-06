use std::{
  io::{self, Write, BufRead},
  process::{exit, Command, Stdio},
  fs,
  collections::HashMap,
};

mod cli;

use analyzer::{Analyzer, debug::display_ir, ir::instruction::IRInstruction};
use clap::Parser as ClapParser;
use cli::{Cli, DebugPrint, Backend, SubCommand};
use parser::Parser;
use lexer::Lexer;
use ast::Ast;
use to_c::TranspilerToC;
use to_lua::TranspilerToLua;
use diagnostic::{DiagnosticList, error::DiagnosticError};

struct CodeResult {
  pub code: String,
  pub file_name: String,
}

impl CodeResult {
  pub fn new(code: String, file_name: String) -> Self {
    Self { code, file_name }
  }
}

struct App {
  pub args: Cli,
  pub file_path: String,
  pub build: bool,
  pub relp: bool,
  pub source: String,
}

impl App {
  pub fn new(args: Cli) -> Self {
    let file_path: String;
    let build: bool;

    match &args.subcommand {
      SubCommand::Build(b) => {
        file_path = b.file_path.clone();
        build = true;
      }
    };

    Self {
      args,
      file_path,
      build,
      relp: false,
      source: String::new(),
    }
  }

  pub fn display_diagnostic(&mut self, diagnostics: &DiagnosticList) {
    for diagnostic in diagnostics.diagnostics.iter() {
      println!("- {}", diagnostic.module_path.as_ref().unwrap());
      println!("{}: {}", diagnostic.code, diagnostic.hint.as_ref().unwrap());

      if !self.relp {
        println!("{} | {}", diagnostic.span.line, diagnostic.span.literal);
        println!("Column: {}", diagnostic.span.end - diagnostic.span.start);
      }
    }
  }

  pub fn create_lua_files(&self, code_results: Vec<CodeResult>) {
    for code_result in code_results {
      let mut path = code_result.file_name.split("/").collect::<Vec<&str>>();
      let code = code_result.code.clone();

      let mut name = path.last().unwrap().replace(r".ign", "");

      name.push_str(".lua");
      path.pop();
      fs::create_dir_all(format!("build/{}", path.join("/"))).unwrap();

      let mut build_path = "build/".to_string() + path.join("/").as_str();

      fs::create_dir_all(build_path.clone()).unwrap();

      build_path.push_str(format!("/{}", &name).as_str());

      fs::write(build_path, code).unwrap();
    }
  }

  pub fn create_c_files(
    &self,
    code_results: Vec<CodeResult>,
  ) -> Result<(), Box<dyn std::error::Error>> {
    for code_result in code_results {
      let mut path = code_result.file_name.split("/").collect::<Vec<&str>>();
      let code = code_result.code.clone();

      let mut name = path.last().unwrap().replace(r".ign", "");

      path.pop();

      fs::create_dir_all(format!("build/{}", path.join("/"))).unwrap();

      let mut build_path = "build/".to_string() + path.join("/").as_str();

      fs::write(format!("{}/{}.c", &build_path, &name), &code).unwrap();

      let mut child = Command::new("gcc")
        .args(&["-x", "c", "-", "-o", &format!("{}/{}", &build_path, &name)]) // -x c indica que el input viene de stdin, "-" significa stdin
        .stdin(Stdio::piped())
        .spawn()?;

      {
        let stdin = child.stdin.as_mut().ok_or("Error al obtener stdin")?;
        stdin.write_all(code.as_bytes())?;
      }

      let output = child.wait_with_output()?;

      if !output.status.success() {
        eprintln!(
          "Compilation error: {}",
          String::from_utf8_lossy(&output.stderr)
        );
        return Err("Failed compilation".into());
      }
    }

    Ok(())
  }

  pub fn run_file(&mut self) -> Result<(), ()> {
    match fs::read_to_string(self.file_path.clone()) {
      Ok(content) => {
        self.source = content;

        let result = self.run()?;

        Ok(())
      }
      Err(e) => {
        println!("{:?}", e);
        Err(())
      }
    }
  }

  fn transpile(&mut self, irs: &HashMap<String, Vec<IRInstruction>>) {
    match self.args.backend {
      Backend::Lua => {
        let mut transpiler = TranspilerToLua::new();
        let mut code_results: Vec<CodeResult> = vec![];

        for result in irs.iter() {
          println!("Transpiling {}", result.0);
          transpiler.transpile(result.1);

          code_results.push(CodeResult::new(transpiler.code.clone(), result.0.clone()));
        }

        self.create_lua_files(code_results);
        
        println!("Done!");
      }
      Backend::C => {
        let mut transpiler = TranspilerToC::new();
        let mut code_results: Vec<CodeResult> = vec![];

        for result in irs.into_iter() {
          println!("Compiling {}", result.0);
          transpiler.transpile(result.1);

          code_results.push(CodeResult::new(transpiler.code.clone(), result.0.clone()));
        }

        self.create_c_files(code_results);

        println!("Done!");
      }
      Backend::Bytecode => todo!(),
      Backend::LLVM => todo!(),
      _ => {
        println!("Backend not implemented");
      }
    }
  }

  fn run(&mut self) -> Result<(), ()> {
    let mut lexer: Lexer<'_> = Lexer::new(&self.source, self.file_path.clone());
    lexer.scan_tokens();

    if self.args.debug.contains(&DebugPrint::Lexer) {
      lexer.display_lexer();
    }

    let mut parser = Parser::new(lexer.tokens);
    let parser_result = parser.parse();

    let mut diagnostics = DiagnosticList::new();

    let mut ast: Ast = match parser_result {
      Ok(statements) => Ast::new(statements),
      Err(_) => {
        DiagnosticError::from_parser_diagnostic(parser.diagnostics)
          .iter()
          .for_each(|error| {
            error.report(&mut diagnostics);
          });

        self.display_diagnostic(&diagnostics);

        return Err(());
      }
    };

    if self.args.debug.contains(&DebugPrint::Ast) {
      let pretty_string = serde_json::to_string_pretty(&ast.to_json()).unwrap();
      println!("{}", pretty_string);
    }

    let mut analyzer = Analyzer::new(self.file_path.clone());

    analyzer.analyze(&mut ast.statements);

    for diagnostic in &analyzer.diagnostics {
      DiagnosticError::report(
        &DiagnosticError::from_evaluator_error(diagnostic.clone()),
        &mut diagnostics,
      );
    }

    if self.args.debug.contains(&DebugPrint::Ir) {
      for result in &analyzer.irs {
        println!("IR for {}", result.0);
        for ir in result.1 {
          display_ir(ir, 1);
        }
      }
    }

    // visit(ast.statements, &mut diagnostics, evaluator);

    if diagnostics.diagnostics.len() > 0 {
      self.display_diagnostic(&diagnostics);

      if !self.relp {
        exit(1);
      }
    }

    diagnostics.clean_diagnostic();

    self.transpile(&analyzer.irs);

    Ok(())
  }

  pub fn _run_prompt(&mut self) -> Result<(), String> {
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

      if buffer.trim() == String::from("exit") {
        println!("Bye!");
        exit(0);
      }

      if buffer.contains("load") == true {
        let path = buffer.split("load").collect::<Vec<&str>>()[1]
          .trim()
          .to_string();

        self.file_path = path;

        match self.run_file() {
          Ok(_) => (),
          Err(_) => println!("Could not import file"),
        }
        continue;
      }

      self.source = buffer.clone();

      match self.run() {
        Ok(_) => (),
        Err(()) => (),
      }
    }
  }
}

fn main() {
  let mut cli = Cli::parse();

  let mut app = App::new(cli);

  let _ = app.run_file();
}
