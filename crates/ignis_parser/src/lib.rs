mod lexer;
mod parser;

use ignis_config::IgnisConfig;
use ignis_token::source::IgnisSourceFile;
use std::sync::Arc;

use crate::lexer::IgnisLexer;

pub fn new_paser_from_source(
  config: Arc<IgnisConfig>,
  source: Arc<IgnisSourceFile>,
) {
  let mut lexer = IgnisLexer::new(config, &source.source, &source.name);
  lexer.scan_tokens(false);

  for token in lexer.tokens {
    println!("{:?}", token);
  }
}
