use crate::{CommentPlacement, DirectiveBlock, FormatError, FormatFile, FormatItem, FormatterConfig};

pub(crate) fn print_file(
  file: &FormatFile,
  config: &FormatterConfig,
) -> Result<String, FormatError> {
  let mut output = String::new();
  render_items(&mut output, &file.items, 0, false, config)?;

  if !output.ends_with('\n') {
    output.push('\n');
  }

  Ok(output)
}

fn render_items(
  output: &mut String,
  items: &[FormatItem],
  indent_level: usize,
  in_directive_branch: bool,
  config: &FormatterConfig,
) -> Result<(), FormatError> {
  let mut previous_was_import = false;
  let mut previous_is_complex = false;

  for (index, item) in items.iter().enumerate() {
    match item {
      FormatItem::Code(region) => {
        let trimmed_raw = region.raw.trim_start();
        let is_import = trimmed_raw.starts_with("import ")
          || (trimmed_raw.starts_with("export ") && region.raw.contains(" from \""));

        // A code item is "complex" (block-level declaration) if its trimmed content spans
        // multiple lines. Raw includes preceding whitespace so we must trim first.
        let is_complex = region.raw.trim().contains('\n');

        write_item_separator(
          output,
          previous_was_import,
          is_import,
          index > 0 && (is_complex || previous_is_complex),
          in_directive_branch,
        );

        previous_is_complex = is_complex;

        if !output.is_empty() && !output.ends_with('\n') {
          output.push('\n');
        }

        let formatted = indent_block(region.raw.trim_end(), indent_level, config);
        if formatted.is_empty() {
          continue;
        }

        // When leading doc comments appeared after attribute annotations in the
        // source (e.g. `@implements(Drop)\n/// doc\nexport record Foo`),
        // emit the attribute lines first so the comment's token neighbors stay
        // identical between source and formatted output.
        let attr_prefix_len = if region.leading_after_attr_prefix {
          leading_attribute_prefix_len(&formatted)
        } else {
          0
        };

        if attr_prefix_len > 0 {
          output.push_str(&formatted[..attr_prefix_len]);
          for comment in &region.leading {
            write_comment_block(output, comment, indent_level, false, false, config);
          }
          output.push_str(&formatted[attr_prefix_len..]);
        } else {
          for comment in &region.leading {
            write_comment_block(output, comment, indent_level, false, index > 0, config);
          }
          output.push_str(&formatted);
        }

        for comment in &region.trailing {
          write_comment_block(output, comment, indent_level, true, false, config);
        }

        if !formatted.ends_with('\n') {
          output.push('\n');
        }

        previous_was_import = is_import;
      },
      FormatItem::DetachedComment(comment) => {
        write_comment_block(output, comment, indent_level, false, index > 0, config);
        previous_was_import = false;
      },
      FormatItem::Directive(block) => {
        // Directives are always block-level (multi-line).
        write_item_separator(output, previous_was_import, false, index > 0, in_directive_branch);

        if !output.is_empty() && !output.ends_with('\n') {
          output.push('\n');
        }

        for comment in &block.leading {
          write_comment_block(output, comment, indent_level, false, index > 0, config);
        }

        render_directive_block(output, block, indent_level, config)?;

        for comment in &block.trailing {
          write_comment_block(output, comment, indent_level, true, false, config);
        }

        previous_was_import = false;
        previous_is_complex = true;
      },
    }
  }

  Ok(())
}

fn write_item_separator(
  output: &mut String,
  previous_was_import: bool,
  current_is_import: bool,
  has_previous_item: bool,
  in_directive_branch: bool,
) {
  if previous_was_import && !current_is_import {
    if !output.is_empty() && !output.ends_with('\n') {
      output.push('\n');
    }

    if !output.ends_with("\n\n") {
      output.push('\n');
    }

    return;
  }

  if has_previous_item && !output.ends_with("\n\n") && !in_directive_branch {
    output.push('\n');
  }
}

fn render_directive_block(
  output: &mut String,
  block: &DirectiveBlock,
  indent_level: usize,
  config: &FormatterConfig,
) -> Result<(), FormatError> {
  if !block.braced {
    output.push_str(&indent(indent_level, config));
    output.push_str(block.header.trim());
    output.push('\n');
    render_items(output, &block.then_items, indent_level, true, config)?;
    if !output.ends_with('\n') {
      output.push('\n');
    }
    return Ok(());
  }

  output.push_str(&indent(indent_level, config));
  output.push_str(block.header.trim());
  output.push_str(" {\n");

  render_items(output, &block.then_items, indent_level + 1, true, config)?;
  if !output.ends_with('\n') {
    output.push('\n');
  }

  output.push_str(&indent(indent_level, config));
  output.push_str("}\n");

  if let Some(else_header) = &block.else_header {
    output.push_str(&indent(indent_level, config));
    output.push_str(else_header.trim());
    output.push_str(" {\n");

    render_items(output, &block.else_items, indent_level + 1, true, config)?;
    if !output.ends_with('\n') {
      output.push('\n');
    }

    output.push_str(&indent(indent_level, config));
    output.push_str("}\n");
  }

  Ok(())
}

fn indent(
  level: usize,
  config: &FormatterConfig,
) -> String {
  config.indent_string(level)
}

fn indent_block(
  code: &str,
  indent_level: usize,
  config: &FormatterConfig,
) -> String {
  if indent_level == 0 {
    return code.to_string();
  }

  code
    .lines()
    .map(|line| {
      if line.is_empty() {
        String::new()
      } else {
        format!("{}{}", indent(indent_level, config), line)
      }
    })
    .collect::<Vec<_>>()
    .join("\n")
}

/// Returns the byte length of the leading `@attribute` lines in `code`.
///
/// When a doc comment appears after attributes in the source (e.g.
/// `@implements(Drop)\n/// doc\nexport record Foo`), the printer must output
/// the attribute lines before the doc comments so the comment's token neighbors
/// stay identical between source and formatted output.
fn leading_attribute_prefix_len(code: &str) -> usize {
  let mut len = 0usize;

  for line in code.lines() {
    let trimmed = line.trim();

    if trimmed.starts_with('@') && !is_formatter_directive(trimmed) {
      len += line.len() + 1; // +1 accounts for the '\n' that lines() strips
    } else {
      break;
    }
  }

  len.min(code.len())
}

fn is_formatter_directive(line: &str) -> bool {
  line.starts_with("@if(")
    || line.starts_with("@else")
    || line.starts_with("@ifelse(")
    || line.starts_with("@configFlag(")
}

fn write_comment_block(
  output: &mut String,
  comment: &crate::CommentBlock,
  indent_level: usize,
  trailing: bool,
  allow_leading_gap: bool,
  config: &FormatterConfig,
) {
  if trailing && comment.lines.len() == 1 {
    output.push(' ');
    output.push_str(comment.lines[0].trim());
    return;
  }

  if !output.is_empty() && !output.ends_with('\n') {
    output.push('\n');
  }

  if allow_leading_gap
    && !output.is_empty()
    && matches!(comment.placement, CommentPlacement::Leading | CommentPlacement::Detached)
    && !output.ends_with("\n\n")
  {
    output.push('\n');
  }

  for line in &comment.lines {
    output.push_str(&indent(indent_level, config));
    output.push_str(line.trim());
    output.push('\n');
  }
}
