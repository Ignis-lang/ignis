#[derive(Debug)]
pub struct TextSpan {
  pub start: usize,
  pub end: usize,
  pub literal: String,
  pub line: usize,
}

impl TextSpan {
  pub fn new(start: usize, end: usize, line: usize, literal: String) -> Self {
    Self {
      start,
      end,
      line,
      literal,
    }
  }

  pub fn lenght(&self) -> usize {
    self.end - self.start
  }
}
