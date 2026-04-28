use ignis_parser::IgnisLexer;
use ignis_token::token::Token;
use ignis_type::file::SourceMap;

use crate::{CommentBlock, CommentTrivia, FormatError, comments::classify_comment_placement};

/// Describes the normalized gap between two sibling items in a sequence.
///
/// The formatter preserves author intent by keeping single blank lines but
/// collapsing runs of 2+ consecutive blank lines down to exactly one.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SiblingGap {
  /// Number of blank lines to emit (0 = adjacent, 1 = one blank line).
  /// Values greater than 1 are normalized down to 1.
  pub blank_lines: usize,
}

impl SiblingGap {
  /// Creates a `SiblingGap` from a raw whitespace gap between two items.
  /// Normalizes any run of 2+ blank lines to exactly 1.
  pub fn from_source_gap(
    source: &str,
    start: usize,
    end: usize,
  ) -> Self {
    let blank_count = count_blank_lines_in_gap(source, start, end);
    Self {
      blank_lines: blank_count.min(1),
    }
  }

  /// Creates a gap with no blank lines (items are adjacent).
  pub fn none() -> Self {
    Self { blank_lines: 0 }
  }

  /// Creates a gap with exactly one blank line.
  pub fn single() -> Self {
    Self { blank_lines: 1 }
  }
}

/// Counts the number of blank lines (empty lines) in the whitespace gap
/// between byte offsets `start` and `end` in `source`.
fn count_blank_lines_in_gap(
  source: &str,
  start: usize,
  end: usize,
) -> usize {
  if start >= end {
    return 0;
  }

  let slice = &source[start.min(source.len())..end.min(source.len())];
  let mut blank_count = 0usize;

  for line in slice.lines() {
    if line.trim().is_empty() {
      blank_count += 1;
    }
  }

  blank_count
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FormatFile {
  pub items: Vec<FormatItem>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FormatItem {
  Code(CodeRegion),
  DetachedComment(CommentBlock),
  Directive(DirectiveBlock),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DirectiveBlock {
  pub leading: Vec<CommentBlock>,
  pub header: String,
  pub then_items: Vec<FormatItem>,
  pub else_header: Option<String>,
  pub else_items: Vec<FormatItem>,
  pub trailing: Vec<CommentBlock>,
  /// False for `@configFlag(cond)\nstatement` (annotation form, no braces).
  pub braced: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CodeRegion {
  pub raw: String,
  pub leading: Vec<CommentBlock>,
  pub trailing: Vec<CommentBlock>,
  /// True when the leading comments appeared in the source AFTER an attribute
  /// annotation prefix (e.g. `@implements(Drop)\n/// doc\nexport record`).
  /// False means the comments appeared BEFORE any attribute annotations.
  pub leading_after_attr_prefix: bool,
}

impl FormatFile {
  pub fn from_source(source: &str) -> Result<Self, FormatError> {
    let mut source_map = SourceMap::new();
    let file_id = source_map.add_file("formatter.ign", source.to_string());

    let mut lexer = IgnisLexer::new(file_id, source);
    lexer.scan_tokens();

    if !lexer.diagnostics.is_empty() {
      return Err(FormatError::Lex {
        diagnostics: lexer.diagnostics.iter().map(ToString::to_string).collect(),
      });
    }

    let eof_index = lexer.tokens.len().saturating_sub(1);
    let items = FormatModelBuilder::new(source, &lexer.tokens).parse_region(0, eof_index, 0, source.len())?;

    Ok(Self { items })
  }

  pub fn comments(&self) -> Vec<CommentTrivia> {
    let mut comments = Vec::new();
    collect_comments(&self.items, &mut comments);
    comments
  }

  pub fn comment_blocks(&self) -> Vec<&CommentBlock> {
    let mut blocks = Vec::new();
    collect_comment_blocks(&self.items, &mut blocks);
    blocks
  }
}

fn collect_comments(
  items: &[FormatItem],
  output: &mut Vec<CommentTrivia>,
) {
  for item in items {
    match item {
      FormatItem::Code(region) => {
        output.extend(region.leading.iter().flat_map(CommentBlock::flatten));
        output.extend(region.trailing.iter().flat_map(CommentBlock::flatten));
      },
      FormatItem::DetachedComment(comment) => output.extend(comment.flatten()),
      FormatItem::Directive(block) => {
        output.extend(block.leading.iter().flat_map(CommentBlock::flatten));
        collect_comments(&block.then_items, output);
        collect_comments(&block.else_items, output);
        output.extend(block.trailing.iter().flat_map(CommentBlock::flatten));
      },
    }
  }
}

fn collect_comment_blocks<'a>(
  items: &'a [FormatItem],
  output: &mut Vec<&'a CommentBlock>,
) {
  for item in items {
    match item {
      FormatItem::Code(region) => {
        output.extend(region.leading.iter());
        output.extend(region.trailing.iter());
      },
      FormatItem::DetachedComment(comment) => output.push(comment),
      FormatItem::Directive(block) => {
        output.extend(block.leading.iter());
        collect_comment_blocks(&block.then_items, output);
        collect_comment_blocks(&block.else_items, output);
        output.extend(block.trailing.iter());
      },
    }
  }
}

struct FormatModelBuilder<'a> {
  source: &'a str,
  tokens: &'a [Token],
}

impl<'a> FormatModelBuilder<'a> {
  fn new(
    source: &'a str,
    tokens: &'a [Token],
  ) -> Self {
    Self { source, tokens }
  }

  fn parse_region(
    &self,
    start_index: usize,
    end_index: usize,
    region_start_byte: usize,
    region_end_byte: usize,
  ) -> Result<Vec<FormatItem>, FormatError> {
    let mut items = Vec::new();
    let mut cursor = start_index;
    let mut code_start_byte = region_start_byte;
    let mut brace_depth = 0usize;
    let mut pending_leading = Vec::new();

    // Byte position after the last complete top-level statement (`;` or `}` at depth 0).
    // Used to detect attribute-only code between a complete statement and an upcoming comment.
    let mut last_stmt_end_byte = region_start_byte;

    // Attribute text from before the current comment that belongs to the NEXT declaration.
    // When a `///` comment falls between `@attribute` tokens and their declaration,
    // the model builder would otherwise create an unparseable chunk. This string carries
    // the orphaned attribute prefix so it gets prepended to the following code region.
    let mut pending_attr_prefix = String::new();

    while cursor < end_index {
      let token = &self.tokens[cursor];

      if brace_depth == 0 && token.is_comment_trivia() {
        let comment_start = token.span.start.0 as usize;

        // Check if the code accumulated since the last complete statement is attribute-only.
        // If so, splitting here would leave the attributes without their declaration in the
        // current chunk (which would fail to parse). Instead, push only the complete code
        // and carry the attribute prefix over to the next chunk.
        let trailing_since_stmt = &self.source[last_stmt_end_byte..comment_start];
        if trailing_since_stmt.trim().starts_with('@') {
          self.push_code_item(
            &mut items,
            &mut pending_leading,
            &mut pending_attr_prefix,
            code_start_byte,
            last_stmt_end_byte,
          );
          pending_attr_prefix = trailing_since_stmt.to_string();

          let (lines, next_index, next_code_start_byte) = self.consume_comment_block(cursor);
          let block = CommentBlock {
            lines,
            placement: classify_comment_placement(
              self.source,
              self.tokens,
              next_index.saturating_sub(1),
              start_index,
              end_index,
            ),
          };
          self.attach_comment_block(&mut items, &mut pending_leading, block)?;
          code_start_byte = next_code_start_byte;
          cursor = next_index;
          continue;
        }

        self.push_code_item(
          &mut items,
          &mut pending_leading,
          &mut pending_attr_prefix,
          code_start_byte,
          comment_start,
        );

        let (lines, next_index, next_code_start_byte) = self.consume_comment_block(cursor);
        let block = CommentBlock {
          lines,
          placement: classify_comment_placement(
            self.source,
            self.tokens,
            next_index.saturating_sub(1),
            start_index,
            end_index,
          ),
        };

        self.attach_comment_block(&mut items, &mut pending_leading, block)?;

        code_start_byte = next_code_start_byte;
        cursor = next_index;
        continue;
      }

      if brace_depth == 0 && self.is_directive_start(cursor) {
        self.push_code_item(
          &mut items,
          &mut pending_leading,
          &mut pending_attr_prefix,
          code_start_byte,
          token.span.start.0 as usize,
        );

        let (mut block, next_index, next_code_start_byte) = self.parse_directive_block(cursor)?;
        block.leading = std::mem::take(&mut pending_leading);
        items.push(FormatItem::Directive(block));

        cursor = next_index;
        code_start_byte = next_code_start_byte;
        last_stmt_end_byte = next_code_start_byte;
        continue;
      }

      match token.type_ {
        ignis_token::token_types::TokenType::LeftBrace => brace_depth += 1,
        ignis_token::token_types::TokenType::RightBrace => {
          brace_depth = brace_depth.saturating_sub(1);
          if brace_depth == 0 {
            last_stmt_end_byte = token.span.end.0 as usize;
          }
        },
        ignis_token::token_types::TokenType::SemiColon if brace_depth == 0 => {
          last_stmt_end_byte = token.span.end.0 as usize;
        },
        _ => {},
      }

      cursor += 1;
    }

    self.push_code_item(
      &mut items,
      &mut pending_leading,
      &mut pending_attr_prefix,
      code_start_byte,
      region_end_byte,
    );

    for comment in pending_leading {
      items.push(FormatItem::DetachedComment(CommentBlock {
        placement: comment.placement,
        lines: comment.lines,
      }));
    }

    Ok(items)
  }

  fn attach_comment_block(
    &self,
    items: &mut Vec<FormatItem>,
    pending_leading: &mut Vec<CommentBlock>,
    block: CommentBlock,
  ) -> Result<(), FormatError> {
    match block.placement {
      crate::CommentPlacement::Leading => pending_leading.push(block),
      crate::CommentPlacement::Detached => items.push(FormatItem::DetachedComment(block)),
      crate::CommentPlacement::Trailing => {
        let previous = items.last_mut().ok_or_else(|| FormatError::Model {
          message: "trailing comment is missing an owning formatter region".to_string(),
        })?;

        match previous {
          FormatItem::Code(region) => region.trailing.push(block),
          FormatItem::Directive(directive) => directive.trailing.push(block),
          FormatItem::DetachedComment(_) => {
            return Err(FormatError::Model {
              message: "trailing comment cannot attach to detached trivia".to_string(),
            });
          },
        }
      },
    }

    Ok(())
  }

  fn consume_comment_block(
    &self,
    start_index: usize,
  ) -> (Vec<String>, usize, usize) {
    let mut lines = Vec::new();
    let mut cursor = start_index;
    let mut last_end = self.tokens[start_index].span.end.0 as usize;

    while cursor < self.tokens.len() && self.tokens[cursor].is_comment_trivia() {
      lines.push(self.tokens[cursor].source_text(self.source).to_string());
      last_end = self.tokens[cursor].span.end.0 as usize;

      let next_index = cursor + 1;
      if next_index >= self.tokens.len() || !self.tokens[next_index].is_comment_trivia() {
        cursor = next_index;
        break;
      }

      if !self.comments_are_contiguous(cursor, next_index) {
        cursor = next_index;
        break;
      }

      cursor = next_index;
    }

    (lines, cursor, last_end)
  }

  fn comments_are_contiguous(
    &self,
    left_index: usize,
    right_index: usize,
  ) -> bool {
    let between =
      &self.source[self.tokens[left_index].span.end.0 as usize..self.tokens[right_index].span.start.0 as usize];

    between.chars().all(char::is_whitespace) && between.chars().filter(|ch| *ch == '\n').count() <= 1
  }

  fn push_code_item(
    &self,
    items: &mut Vec<FormatItem>,
    pending_leading: &mut Vec<CommentBlock>,
    attr_prefix: &mut String,
    start_byte: usize,
    end_byte: usize,
  ) {
    let raw_bytes = if start_byte < end_byte {
      &self.source[start_byte..end_byte]
    } else {
      ""
    };

    let (raw, leading_after_attr_prefix) = if attr_prefix.is_empty() {
      if raw_bytes.trim().is_empty() {
        return;
      }
      (raw_bytes.to_string(), false)
    } else {
      let combined = format!("{}{}", attr_prefix, raw_bytes);
      attr_prefix.clear();
      if combined.trim().is_empty() {
        return;
      }
      (combined, true)
    };

    items.push(FormatItem::Code(CodeRegion {
      raw,
      leading: std::mem::take(pending_leading),
      trailing: Vec::new(),
      leading_after_attr_prefix,
    }));
  }

  fn parse_directive_block(
    &self,
    start_index: usize,
  ) -> Result<(DirectiveBlock, usize, usize), FormatError> {
    let header_end_index = self.find_header_end(start_index)?;
    let header = self.source
      [self.tokens[start_index].span.start.0 as usize..self.tokens[header_end_index].span.end.0 as usize]
      .to_string();

    let after_header = header_end_index + 1;
    let is_braced = after_header < self.tokens.len()
      && self.tokens[after_header].type_ == ignis_token::token_types::TokenType::LeftBrace;

    if !is_braced {
      // Brace-less form: `@configFlag(cond)\nstatement` — the next single top-level
      // declaration is the then-item. Scan for its end at brace depth 0.
      let stmt_start_token = after_header;
      if stmt_start_token >= self.tokens.len() {
        return Err(FormatError::Model {
          message: format!("Directive header '{}' is missing a following statement", header),
        });
      }

      let (stmt_end_token, stmt_end_byte) = self.find_single_declaration_end(stmt_start_token)?;
      let stmt_start_byte = self.tokens[stmt_start_token].span.start.0 as usize;
      let then_items = self.parse_region(stmt_start_token, stmt_end_token, stmt_start_byte, stmt_end_byte)?;
      let next_code_start_byte = stmt_end_byte;

      return Ok((
        DirectiveBlock {
          leading: Vec::new(),
          header,
          then_items,
          else_header: None,
          else_items: Vec::new(),
          trailing: Vec::new(),
          braced: false,
        },
        stmt_end_token,
        next_code_start_byte,
      ));
    }

    let then_left_brace = after_header;
    let then_right_brace = self.find_matching_right_brace(then_left_brace)?;
    let then_items = self.parse_region(
      then_left_brace + 1,
      then_right_brace,
      self.tokens[then_left_brace].span.end.0 as usize,
      self.tokens[then_right_brace].span.start.0 as usize,
    )?;

    let mut next_index = then_right_brace + 1;
    let mut else_header = None;
    let mut else_items = Vec::new();

    if self.is_else_directive_start(next_index) {
      else_header = Some(
        self.source[self.tokens[next_index].span.start.0 as usize..self.tokens[next_index + 1].span.end.0 as usize]
          .to_string(),
      );

      let else_left_brace = next_index + 2;
      if else_left_brace >= self.tokens.len()
        || self.tokens[else_left_brace].type_ != ignis_token::token_types::TokenType::LeftBrace
      {
        return Err(FormatError::Model {
          message: "@else directive is missing a branch body".to_string(),
        });
      }

      let else_right_brace = self.find_matching_right_brace(else_left_brace)?;
      else_items = self.parse_region(
        else_left_brace + 1,
        else_right_brace,
        self.tokens[else_left_brace].span.end.0 as usize,
        self.tokens[else_right_brace].span.start.0 as usize,
      )?;

      next_index = else_right_brace + 1;
    }

    let next_code_start_byte = if next_index < self.tokens.len() {
      self.tokens[next_index - 1].span.end.0 as usize
    } else {
      self.source.len()
    };

    Ok((
      DirectiveBlock {
        leading: Vec::new(),
        header,
        then_items,
        else_header,
        else_items,
        trailing: Vec::new(),
        braced: true,
      },
      next_index,
      next_code_start_byte,
    ))
  }

  /// Finds the end of a single top-level declaration starting at `start_index`.
  /// Returns `(end_token_index, end_byte)` where `end_token_index` is the index
  /// of the first token AFTER the declaration, and `end_byte` is the byte position
  /// of the declaration's end (after `;` or `}`).
  fn find_single_declaration_end(
    &self,
    start_index: usize,
  ) -> Result<(usize, usize), FormatError> {
    let mut cursor = start_index;
    let mut depth = 0usize;

    while cursor < self.tokens.len() {
      match self.tokens[cursor].type_ {
        ignis_token::token_types::TokenType::LeftBrace => depth += 1,
        ignis_token::token_types::TokenType::RightBrace => {
          depth = depth.saturating_sub(1);
          if depth == 0 {
            let end_byte = self.tokens[cursor].span.end.0 as usize;
            return Ok((cursor + 1, end_byte));
          }
        },
        ignis_token::token_types::TokenType::SemiColon if depth == 0 => {
          let end_byte = self.tokens[cursor].span.end.0 as usize;
          return Ok((cursor + 1, end_byte));
        },
        _ => {},
      }
      cursor += 1;
    }

    let start_byte = self.tokens[start_index].span.start.0 as usize;
    let context_end = (start_byte + 160).min(self.source.len());
    let context = self.source[start_byte..context_end].replace('\n', "\\n");

    Err(FormatError::Model {
      message: format!("brace-less directive statement has no clear end near: '{context}'"),
    })
  }

  fn find_header_end(
    &self,
    start_index: usize,
  ) -> Result<usize, FormatError> {
    let mut cursor = start_index + 1;
    while cursor < self.tokens.len() && self.tokens[cursor].type_ != ignis_token::token_types::TokenType::LeftParen {
      cursor += 1;
    }

    if cursor >= self.tokens.len() {
      return Err(FormatError::Model {
        message: "Directive header is missing '('".to_string(),
      });
    }

    let mut depth = 0usize;
    while cursor < self.tokens.len() {
      match self.tokens[cursor].type_ {
        ignis_token::token_types::TokenType::LeftParen => depth += 1,
        ignis_token::token_types::TokenType::RightParen => {
          depth -= 1;
          if depth == 0 {
            return Ok(cursor);
          }
        },
        _ => {},
      }

      cursor += 1;
    }

    Err(FormatError::Model {
      message: "Directive header has unbalanced parentheses".to_string(),
    })
  }

  fn find_matching_right_brace(
    &self,
    left_brace_index: usize,
  ) -> Result<usize, FormatError> {
    let mut depth = 0usize;
    for cursor in left_brace_index..self.tokens.len() {
      match self.tokens[cursor].type_ {
        ignis_token::token_types::TokenType::LeftBrace => depth += 1,
        ignis_token::token_types::TokenType::RightBrace => {
          depth -= 1;
          if depth == 0 {
            return Ok(cursor);
          }
        },
        _ => {},
      }
    }

    Err(FormatError::Model {
      message: "Directive branch has unbalanced braces".to_string(),
    })
  }

  fn is_directive_start(
    &self,
    index: usize,
  ) -> bool {
    if index + 1 >= self.tokens.len() {
      return false;
    }

    let token = &self.tokens[index];
    let next = &self.tokens[index + 1];

    token.type_ == ignis_token::token_types::TokenType::At
      && (next.type_ == ignis_token::token_types::TokenType::If
        || (next.type_ == ignis_token::token_types::TokenType::Identifier
          && matches!(next.lexeme.as_str(), "ifelse" | "configFlag")))
  }

  fn is_else_directive_start(
    &self,
    index: usize,
  ) -> bool {
    index + 1 < self.tokens.len()
      && self.tokens[index].type_ == ignis_token::token_types::TokenType::At
      && self.tokens[index + 1].type_ == ignis_token::token_types::TokenType::Else
  }
}

#[cfg(test)]
mod tests {
  use crate::{CodeRegion, CommentPlacement, FormatFile, FormatItem};

  #[test]
  fn nests_directives_inside_branch_regions() {
    let file =
      FormatFile::from_source("@if(feature) {\n    @if(other) {\n        function nested(): void {}\n    }\n}\n")
        .expect("build format file");

    match &file.items[0] {
      FormatItem::Directive(block) => {
        assert!(matches!(block.then_items[0], FormatItem::Directive(_)));
      },
      other => panic!("expected directive, got {:?}", other),
    }
  }

  #[test]
  fn collects_comment_placements_recursively() {
    let file =
      FormatFile::from_source("@if(flag) {\n    /// docs\n    function main(): void {}\n}\n").expect("build file");
    let placements: Vec<CommentPlacement> = file
      .comments()
      .iter()
      .map(|comment| comment.placement.clone())
      .collect();

    assert_eq!(placements, vec![CommentPlacement::Leading]);
  }

  #[test]
  fn keeps_top_level_leading_comments_on_owned_code_regions() {
    let file = FormatFile::from_source("/// docs\nfunction main(): void {}\n").expect("build file");

    assert!(matches!(
      &file.items[0],
      FormatItem::Code(CodeRegion {
        leading,
        trailing,
        ..
      }) if leading.len() == 1 && trailing.is_empty()
    ));
  }
}
