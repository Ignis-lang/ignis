use ignis_token::token::Token;
use ignis_token::token_types::TokenType;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CommentPlacement {
  Leading,
  Trailing,
  Detached,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CommentTrivia {
  pub raw: String,
  pub placement: CommentPlacement,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CommentBlock {
  pub lines: Vec<String>,
  pub placement: CommentPlacement,
}

impl CommentBlock {
  pub fn flatten(&self) -> Vec<CommentTrivia> {
    self
      .lines
      .iter()
      .cloned()
      .map(|raw| CommentTrivia {
        raw,
        placement: self.placement.clone(),
      })
      .collect()
  }
}

pub(crate) fn classify_comment_placement(
  source: &str,
  tokens: &[Token],
  comment_index: usize,
  start_index: usize,
  end_index: usize,
) -> CommentPlacement {
  let comment = &tokens[comment_index];
  let comment_line = line_number(source, comment.span.start.0 as usize);

  let previous = tokens[start_index..comment_index]
    .iter()
    .rev()
    .find(|token| !token.is_comment_trivia());

  if let Some(previous) = previous {
    let previous_line = line_number(source, previous.span.end.0 as usize);
    if previous_line == comment_line && previous.type_ != TokenType::LeftBrace {
      return CommentPlacement::Trailing;
    }
  }

  let next = tokens[comment_index + 1..end_index]
    .iter()
    .find(|token| !token.is_comment_trivia());

  if let Some(next) = next {
    let next_line = line_number(source, next.span.start.0 as usize);
    if comment_line + 1 >= next_line {
      CommentPlacement::Leading
    } else {
      CommentPlacement::Detached
    }
  } else {
    CommentPlacement::Detached
  }
}

fn line_number(
  source: &str,
  byte_index: usize,
) -> usize {
  source[..byte_index.min(source.len())]
    .bytes()
    .filter(|byte| *byte == b'\n')
    .count()
}

#[cfg(test)]
mod tests {
  use ignis_parser::IgnisLexer;
  use ignis_type::file::SourceMap;

  use crate::comments::{CommentPlacement, classify_comment_placement};

  #[test]
  fn classifies_detached_and_trailing_comments_by_line_position() {
    let source = "// detached\n\nfunction main(): void {} // trailing\n";
    let mut source_map = SourceMap::new();
    let file_id = source_map.add_file("comments.ign", source.to_string());

    let mut lexer = IgnisLexer::new(file_id, source);
    lexer.scan_tokens();

    let detached = classify_comment_placement(source, &lexer.tokens, 0, 0, lexer.tokens.len() - 1);
    let trailing = classify_comment_placement(source, &lexer.tokens, 6, 0, lexer.tokens.len() - 1);

    assert_eq!(detached, CommentPlacement::Detached);
    assert_eq!(trailing, CommentPlacement::Trailing);
  }
}
