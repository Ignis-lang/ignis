use crate::FormatterConfig;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Doc {
  Text(String),
  Line,
  Indent(Box<Doc>),
  List(Vec<Doc>),
}

impl Doc {
  pub(crate) fn text(value: impl Into<String>) -> Self {
    Self::Text(value.into())
  }

  pub(crate) fn line() -> Self {
    Self::Line
  }

  pub(crate) fn indent(doc: Doc) -> Self {
    Self::Indent(Box::new(doc))
  }

  pub(crate) fn list(items: Vec<Doc>) -> Self {
    Self::List(items)
  }
}

pub(crate) fn render_doc(
  doc: &Doc,
  config: &FormatterConfig,
  base_indent: usize,
) -> String {
  let mut output = String::new();
  render_into(doc, config, base_indent, &mut output);
  output
}

pub(crate) fn format_inline_or_multiline(
  prefix: &str,
  items: &[String],
  suffix: &str,
  leading_offset: usize,
  config: &FormatterConfig,
  multiline_trailing_comma: bool,
  reserved_trailing: usize,
) -> Doc {
  let joined = items.join(", ");
  let flat = format!("{prefix}{joined}{suffix}");

  // `leading_offset` is the column at which `prefix` begins. Callers whose
  // `prefix` already embeds its own indentation pass 0; callers that splice
  // the result mid-line pass the column where that splice starts.
  //
  // `reserved_trailing` reserves columns for content appended on the same line
  // after the suffix (e.g. ` {` for a body, `;` for a declaration), so the
  // combined first line never exceeds `line_width`.
  if leading_offset + flat.len() + reserved_trailing <= config.line_width {
    return Doc::text(flat);
  }

  // Multiline: each param on its own line, indented one level deeper than the
  // declaration. The closing suffix (`): ReturnType`) sits on its own line at
  // the declaration level. `render_doc` must be called with `base_indent =
  // indent_level` so that `Doc::line()` at the top level produces the correct
  // indentation for the suffix, and `Doc::Indent` produces params at level+1.
  let mut param_docs: Vec<Doc> = Vec::new();
  for (index, item) in items.iter().enumerate() {
    param_docs.push(Doc::line());
    if index + 1 < items.len() || multiline_trailing_comma {
      param_docs.push(Doc::text(format!("{item},")));
    } else {
      param_docs.push(Doc::text(item.clone()));
    }
  }

  Doc::list(vec![
    Doc::text(prefix.to_string()),
    Doc::indent(Doc::list(param_docs)),
    Doc::line(),
    Doc::text(suffix.to_string()),
  ])
}

fn render_into(
  doc: &Doc,
  config: &FormatterConfig,
  indent_level: usize,
  output: &mut String,
) {
  match doc {
    Doc::Text(text) => output.push_str(text),
    Doc::Line => {
      output.push('\n');
      output.push_str(&config.indent_string(indent_level));
    },
    Doc::Indent(child) => render_into(child, config, indent_level + 1, output),
    Doc::List(items) => {
      for item in items {
        render_into(item, config, indent_level, output);
      }
    },
  }
}
