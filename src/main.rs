use std::{
  collections::HashMap,
  fs,
  io::{self, BufRead, Write},
  process::exit,
};

mod cli;

use clap::Parser as ClapParser;
use cli::{Cli, DebugPrint, Target, SubCommand};
use diagnostic::Diagnostic;
use diagnostic_report::DiagnosticReport;
use ignis_frontend::{IgnisFrontend, FrontendDebugPrint};
use ignis_backend::IgnisBackend;
use backend_trait::BackendTrait;

struct App {
  pub args: Cli,
  pub file_path: String,
  pub target: Target,
  pub build: bool,
  pub relp: bool,
  pub source: String,
}

impl App {
  pub fn new(args: Cli) -> Self {
    let file_path: String;
    let build: bool;
    let target: Target;

    match &args.subcommand {
      SubCommand::Build(b) => {
        file_path = b.file_path.clone();
        build = true;
        target = b.target;
      }
      SubCommand::Run(_) => todo!(),
    };

    Self {
      args,
      file_path,
      build,
      target,
      relp: false,
      source: String::new(),
    }
  }

  pub fn run_file(&mut self) -> Result<(), Vec<DiagnosticReport>> {
    match fs::read_to_string(self.file_path.clone()) {
      Ok(content) => {
        self.source = content;

        self.run()?;

        Ok(())
      }
      Err(e) => {
        println!("{:?}", e);
        Err(vec![])
      }
    }
  }

  fn run(&mut self) -> Result<(), Vec<DiagnosticReport>> {
    if self.source.is_empty() {
      println!("No source code to run");
      exit(1);
    }

    let mut debug_frontend: Vec<FrontendDebugPrint> = vec![];
    let _debug_backend: Vec<DebugPrint> = vec![];

    for debug in self.args.debug.clone() {
      match debug {
        DebugPrint::Lexer => {
          debug_frontend.push(FrontendDebugPrint::Lexer);
        }
        DebugPrint::Ast => {
          debug_frontend.push(FrontendDebugPrint::Ast);
        }
        DebugPrint::Ir => {
          debug_frontend.push(FrontendDebugPrint::IR);
        }
        _ => (),
      }
    }

    let mut frontend =
      IgnisFrontend::new(self.source.clone(), self.file_path.clone(), debug_frontend);

    let result = frontend.process()?;

    let mut backend: IgnisBackend = IgnisBackend::new(self.target.to_backend(), result);
    backend.process()?;

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
            println!();
            return Ok(());
          }

          if n == 1 {
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

      self.source.clone_from(&buffer);

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
  let cli = Cli::parse();

  let mut app = App::new(cli);

  if let Err(errors) = app.run_file() {
    if app.args.no_diagnostic {
      return;
    }

    for error in errors {
      let diagnostic = Diagnostic::new();
      diagnostic.print(&error);
    }
  }
}
