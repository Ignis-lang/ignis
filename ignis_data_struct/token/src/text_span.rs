#[derive(Debug, Clone, PartialEq)]
pub struct TextSpan {
  pub start: usize,
  pub end: usize,
  pub literal: String,
  pub line: usize,
  pub column: usize,
  pub file: String,
}

impl TextSpan {
  pub fn new(
    start: usize,
    end: usize,
    line: usize,
    literal: String,
    column: usize,
    file: String,
  ) -> Self {
    Self {
      start,
      end,
      line,
      literal,
      column,
      file,
    }
  }
}
