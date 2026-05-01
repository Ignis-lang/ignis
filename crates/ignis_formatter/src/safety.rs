use ignis_parser::IgnisLexer;
use ignis_token::token_types::TokenType;
use ignis_type::file::SourceMap;

use crate::comments::classify_comment_placement;
use crate::{DirectiveBlock, FormatError, FormatFile, FormatItem};

pub fn validate_formatted_output(
  source: &str,
  formatted: &str,
  sort_imports: bool,
) -> Result<(), FormatError> {
  validate_parse_back(formatted)?;
  validate_no_trailing_whitespace(formatted)?;
  validate_final_newline(formatted)?;

  let source_file = FormatFile::from_source(source)?;
  let formatted_file = FormatFile::from_source(formatted)?;

  let source_shape = file_shape(source, &source_file)?;
  let formatted_shape = file_shape(formatted, &formatted_file)?;

  let source_tokens = normalize_module_item_groups(&normalize_optional_trailing_commas(&source_shape.tokens));
  let formatted_tokens = normalize_module_item_groups(&normalize_optional_trailing_commas(&formatted_shape.tokens));

  let tokens_match = if sort_imports {
    let mut source_keys: Vec<String> = source_tokens.iter().map(|(_, text)| text.clone()).collect();
    let mut formatted_keys: Vec<String> = formatted_tokens.iter().map(|(_, text)| text.clone()).collect();
    source_keys.sort();
    formatted_keys.sort();
    source_keys == formatted_keys
  } else {
    source_tokens == formatted_tokens
  };

  if !tokens_match {
    return Err(FormatError::Safety {
      message: "formatted output changed token shape or preserved trivia structure".to_string(),
    });
  }

  if source_shape.comments != formatted_shape.comments {
    return Err(FormatError::Safety {
      message: "formatted output changed comment ownership or preserved trivia structure".to_string(),
    });
  }

  if source_shape.directives != formatted_shape.directives {
    return Err(FormatError::Safety {
      message: "formatted output changed directive structure".to_string(),
    });
  }

  Ok(())
}

/// Verifies that the formatted output can be lexed without errors (parse-back check).
/// This catches cases where the formatter produces syntactically invalid output.
pub fn validate_parse_back(formatted: &str) -> Result<(), FormatError> {
  let mut source_map = SourceMap::new();
  let file_id = source_map.add_file("formatter_parseback.ign", formatted.to_string());
  let mut lexer = IgnisLexer::new(file_id, formatted);
  lexer.scan_tokens();

  if !lexer.diagnostics.is_empty() {
    return Err(FormatError::Safety {
      message: format!(
        "parse-back validation failed: formatted output does not lex cleanly: {}",
        lexer
          .diagnostics
          .iter()
          .map(ToString::to_string)
          .collect::<Vec<_>>()
          .join("; ")
      ),
    });
  }

  Ok(())
}

/// Verifies that the formatted output has no trailing whitespace on any line.
pub fn validate_no_trailing_whitespace(formatted: &str) -> Result<(), FormatError> {
  for (line_number, line) in formatted.lines().enumerate() {
    if line != line.trim_end() {
      return Err(FormatError::Safety {
        message: format!("trailing whitespace on line {}: {:?}", line_number + 1, line),
      });
    }
  }

  Ok(())
}

/// Verifies that the formatted output ends with a final newline character.
pub fn validate_final_newline(formatted: &str) -> Result<(), FormatError> {
  if !formatted.is_empty() && !formatted.ends_with('\n') {
    return Err(FormatError::Safety {
      message: "formatted output is missing a final newline".to_string(),
    });
  }

  Ok(())
}

pub fn validate_idempotence<F>(
  formatted: &str,
  reformat: F,
) -> Result<(), FormatError>
where
  F: FnOnce(&str) -> Result<String, FormatError>,
{
  let reformatted = reformat(formatted)?;

  if reformatted != formatted {
    return Err(FormatError::Safety {
      message: "formatted output is not idempotent on a second formatter pass".to_string(),
    });
  }

  Ok(())
}

#[derive(Debug, Clone, PartialEq)]
struct FileShape {
  tokens: Vec<(TokenType, String)>,
  comments: Vec<CommentShape>,
  directives: Vec<String>,
}

#[derive(Debug, Clone, PartialEq)]
struct CommentShape {
  raw: String,
  placement: crate::CommentPlacement,
  previous_token: Option<(TokenType, String)>,
  next_token: Option<(TokenType, String)>,
}

fn file_shape(
  source: &str,
  file: &FormatFile,
) -> Result<FileShape, FormatError> {
  let mut shape = FileShape {
    tokens: Vec::new(),
    comments: comment_shape(source)?,
    directives: Vec::new(),
  };

  collect_shape(&file.items, &mut shape)?;

  Ok(shape)
}

fn collect_shape(
  items: &[FormatItem],
  shape: &mut FileShape,
) -> Result<(), FormatError> {
  for item in items {
    match item {
      FormatItem::Code(region) => shape.tokens.extend(token_shape(&region.raw)?),
      FormatItem::DetachedComment(_) => {},
      FormatItem::Directive(DirectiveBlock {
        leading: _,
        header,
        then_items,
        else_header,
        else_items,
        trailing: _,
        braced: _,
      }) => {
        shape.directives.push(header.clone());
        if let Some(else_header) = else_header {
          shape.directives.push(else_header.clone());
        }
        collect_shape(then_items, shape)?;
        collect_shape(else_items, shape)?;
      },
    }
  }

  Ok(())
}

fn comment_shape(source: &str) -> Result<Vec<CommentShape>, FormatError> {
  let mut source_map = SourceMap::new();
  let file_id = source_map.add_file("formatter_comments.ign", source.to_string());
  let mut lexer = IgnisLexer::new(file_id, source);
  lexer.scan_tokens();

  if !lexer.diagnostics.is_empty() {
    return Err(FormatError::Lex {
      diagnostics: lexer.diagnostics.iter().map(ToString::to_string).collect(),
    });
  }

  let eof_index = lexer.tokens.len().saturating_sub(1);
  let mut comments = Vec::new();

  for (index, token) in lexer.tokens.iter().enumerate() {
    if !token.is_comment_trivia() {
      continue;
    }

    comments.push(CommentShape {
      raw: token.source_text(source).to_string(),
      placement: classify_comment_placement(source, &lexer.tokens, index, 0, eof_index),
      previous_token: lexer.tokens[..index]
        .iter()
        .rev()
        .find(|candidate| !candidate.is_comment_trivia() && candidate.type_ != TokenType::Eof)
        .map(|candidate| (candidate.type_, candidate.source_text(source).to_string())),
      next_token: lexer.tokens[index + 1..]
        .iter()
        .find(|candidate| !candidate.is_comment_trivia() && candidate.type_ != TokenType::Eof)
        .map(|candidate| (candidate.type_, candidate.source_text(source).to_string())),
    });
  }

  Ok(comments)
}

fn token_shape(source: &str) -> Result<Vec<(TokenType, String)>, FormatError> {
  raw_token_shape(source)
}

fn raw_token_shape(source: &str) -> Result<Vec<(TokenType, String)>, FormatError> {
  let mut source_map = SourceMap::new();
  let file_id = source_map.add_file("formatter_safety.ign", source.to_string());
  let mut lexer = IgnisLexer::new(file_id, source);
  lexer.scan_tokens();

  if !lexer.diagnostics.is_empty() {
    return Err(FormatError::Lex {
      diagnostics: lexer.diagnostics.iter().map(ToString::to_string).collect(),
    });
  }

  let mut normalized = Vec::new();

  for token in lexer
    .tokens
    .iter()
    .filter(|token| !token.is_comment_trivia() && token.type_ != TokenType::Eof)
  {
    let raw = token.source_text(source);

    if matches!(token.type_, TokenType::Int | TokenType::Float)
      && let Some(sign) = raw.chars().next()
      && matches!(sign, '+' | '-')
      && normalized_ends_expression(&normalized)
    {
      normalized.push((if sign == '+' { TokenType::Plus } else { TokenType::Minus }, sign.to_string()));
      normalized.push((token.type_, raw[sign.len_utf8()..].to_string()));
      continue;
    }

    normalized.push((token.type_, raw.to_string()));
  }

  Ok(normalized)
}

fn normalized_ends_expression(tokens: &[(TokenType, String)]) -> bool {
  matches!(
    tokens.last().map(|(type_, _)| type_),
    Some(
      TokenType::Identifier
        | TokenType::Int
        | TokenType::Float
        | TokenType::String
        | TokenType::Char
        | TokenType::True
        | TokenType::False
        | TokenType::Null
        | TokenType::RightParen
        | TokenType::RightBrack
        | TokenType::RightBrace
    )
  )
}

fn normalize_optional_trailing_commas(tokens: &[(TokenType, String)]) -> Vec<(TokenType, String)> {
  let mut normalized = Vec::with_capacity(tokens.len());

  for (index, token) in tokens.iter().enumerate() {
    if token.0 == TokenType::Comma
      && let Some((next_type, _)) = tokens.get(index + 1)
      && matches!(next_type, TokenType::RightParen | TokenType::RightBrace | TokenType::From)
    {
      continue;
    }

    normalized.push(token.clone());
  }

  normalized
}

fn normalize_module_item_groups(tokens: &[(TokenType, String)]) -> Vec<(TokenType, String)> {
  let mut normalized = Vec::with_capacity(tokens.len());
  let mut index = 0;

  while index < tokens.len() {
    let Some(first_statement) = parse_module_item_statement(tokens, index) else {
      normalized.push(tokens[index].clone());
      index += 1;
      continue;
    };

    let mut grouped_items = first_statement.items;
    let mut next_index = first_statement.end_index;

    while let Some(next_statement) = parse_module_item_statement(tokens, next_index) {
      if next_statement.keyword != first_statement.keyword || next_statement.from != first_statement.from {
        break;
      }

      grouped_items.extend(next_statement.items);
      next_index = next_statement.end_index;
    }

    normalized.push((first_statement.keyword, first_statement.keyword_text));
    for (item_index, item) in grouped_items.iter().enumerate() {
      if item_index > 0 {
        normalized.push((TokenType::Comma, ",".to_string()));
      }

      normalized.extend(item.iter().cloned());
    }

    normalized.push((TokenType::From, "from".to_string()));
    normalized.push((TokenType::String, first_statement.from));
    normalized.push((TokenType::SemiColon, ";".to_string()));
    index = next_index;
  }

  normalized
}

struct ModuleItemStatement {
  keyword: TokenType,
  keyword_text: String,
  items: Vec<Vec<(TokenType, String)>>,
  from: String,
  end_index: usize,
}

fn parse_module_item_statement(
  tokens: &[(TokenType, String)],
  start: usize,
) -> Option<ModuleItemStatement> {
  let (keyword, keyword_text) = tokens.get(start)?.clone();
  if !matches!(keyword, TokenType::Import | TokenType::Export) {
    return None;
  }

  let mut items = Vec::new();
  let mut current_item = Vec::new();
  let mut cursor = start + 1;

  while let Some((token_type, token_text)) = tokens.get(cursor) {
    if *token_type == TokenType::From {
      break;
    }

    if *token_type == TokenType::Comma {
      if !current_item.is_empty() {
        items.push(std::mem::take(&mut current_item));
      }
    } else {
      current_item.push((*token_type, token_text.clone()));
    }

    cursor += 1;
  }

  if !current_item.is_empty() {
    items.push(current_item);
  }

  if items.is_empty() || tokens.get(cursor)?.0 != TokenType::From {
    return None;
  }

  let (from_type, from) = tokens.get(cursor + 1)?.clone();
  if from_type != TokenType::String || tokens.get(cursor + 2)?.0 != TokenType::SemiColon {
    return None;
  }

  Some(ModuleItemStatement {
    keyword,
    keyword_text,
    items,
    from,
    end_index: cursor + 3,
  })
}
