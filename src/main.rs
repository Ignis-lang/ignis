use std::{
  io::{self, Write, BufRead},
  process::exit,
  fs,
};

mod cli;

use clap::Parser as ClapParser;
use cli::{Cli, DebugPrint, Backend, SubCommand};
use diagnostic::Diagnostic;
use diagnostic_report::DiagnosticReport;
use ignis_frontend::{IgnisFrontend, FrontendDebugPrint};
use ignis_backend::{IgnisBackend, CodeResult, BackendTarget};

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

  pub fn create_lua_files(&self, code_results: Vec<CodeResult>) {
    for code_result in code_results {
      let mut path = code_result.file_name.split('/').collect::<Vec<&str>>();
      let code = code_result.code.clone();

      let mut name = path.last().unwrap().replace(r".ign", "");

      name.push_str(".lua");
      path.pop();

      let mut build_path = "build/".to_string() + path.join("/").as_str();

      fs::create_dir_all(build_path.clone()).unwrap();

      build_path.push_str(format!("/{}", &name).as_str());

      fs::write(build_path, code).unwrap();
    }
  }

  pub fn run_file(&mut self) -> Result<(), Vec<DiagnosticReport>> {
    match fs::read_to_string(self.file_path.clone()) {
      Ok(content) => {
        self.source = content;

        let result = self.run()?;

        match self.args.backend {
          Backend::Lua => {
            self.create_lua_files(result);
          }
          _ => {
            println!("Backend not implemented");
          }
        };

        Ok(())
      }
      Err(e) => {
        println!("{:?}", e);
        Err(vec![])
      }
    }
  }

  fn run(&mut self) -> Result<Vec<CodeResult>, Vec<DiagnosticReport>> {
    if self.source.is_empty() {
      println!("No source code to run");
      exit(1);
    }

    let mut debug_frontend: Vec<FrontendDebugPrint> = vec![];
    let mut debug_backend: Vec<DebugPrint> = vec![];

    for debug in self.args.debug.clone() {
      match debug {
        DebugPrint::Lexer => {
          debug_frontend.push(FrontendDebugPrint::Lexer);
        }
        DebugPrint::Parser => (),
        DebugPrint::Ast => {
          debug_frontend.push(FrontendDebugPrint::Ast);
        }
        DebugPrint::Analyzer => (),
        DebugPrint::Ir => {
          debug_frontend.push(FrontendDebugPrint::IR);
        }
        _ => (),
      }
    }

    let mut frontend =
      IgnisFrontend::new(self.source.clone(), self.file_path.clone(), debug_frontend);

    let result = frontend.process()?;

    let backend = IgnisBackend::new(BackendTarget::Lua);
    let code_results: Vec<CodeResult> = backend.process(result)?;

    Ok(code_results)
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
            println!();
            return Ok(());
          } else if n == 1 {
            continue;
          }
        }
        Err(_) => return Err("Clound not read line".to_string()),
      }

      if buffer.trim() == "exit" {
        println!("Bye!");
        exit(0);
      }

      if buffer.contains("load") {
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

      if let Err(errors) = self.run() {
        for error in errors {
          let diagnostic = Diagnostic::new();
          diagnostic.print(&error);
        }
      }
    }
  }
}

fn main() {
  let mut cli = Cli::parse();

  cli.backend = Backend::Lua;

  let mut app = App::new(cli);

  if let Err(errors) = app.run_file() {
    for error in errors {
      let diagnostic = Diagnostic::new();
      diagnostic.print(&error);
    }
  }
}
