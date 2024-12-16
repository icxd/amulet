#[derive(Debug, Clone, Copy, Default)]
pub struct Span {
  pub(crate) start: usize,
  pub(crate) end: usize,
}

impl Span {
  pub fn new(start: usize, end: usize) -> Self {
    Self { start, end }
  }

  pub fn extend(&mut self, other: &Self) {
    self.start = self.start.min(other.start);
    self.end = self.end.max(other.end);
  }

  pub fn start(&self) -> usize {
    self.start
  }

  pub fn end(&self) -> usize {
    self.end
  }
}
