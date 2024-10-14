use std::io::Read;

use ignis_config::{DebugPrint, IgnisConfig};
use ignis_token::token::Token;
use crate::lexer::Lexer;

fn get_file_content(path: &String) -> String {
  let file = std::fs::File::open(path);

  if file.is_err() {
    return String::new();
  }

  let mut file = file.unwrap();
  let mut content = String::new();

  let _ = file.read_to_string(&mut content);

  return content;
}

pub struct IgnisFrontend {
  config: Box<IgnisConfig>,
}

impl IgnisFrontend {
  pub fn new(config: Box<IgnisConfig>) -> Self {
    Self { config }
  }

  pub fn process(&mut self) {
    let build_config = self.config.build_config.clone().unwrap();

    let file = build_config.file.unwrap();

    let source = get_file_content(&file);

    let mut lexer = Lexer::new(self.config.clone(), source.as_str(), file);
    lexer.scan_tokens(false);
    let tokens = lexer.tokens;

    if self.config.debug.contains(&DebugPrint::Lexer) && !self.config.quiet {
      if self.config.verbose == 0 {
        Token::print_ascii_table(tokens);
      } else {
        println!("{:#?}", tokens);
      }
    }
  }
}
