use crate::compiler::FileId;

#[derive(Debug, Clone, Copy, Default, PartialEq)]
pub struct Span {
  pub(crate) file_id: FileId,
  pub(crate) start: usize,
  pub(crate) end: usize,
}

impl Span {
  pub fn new(file_id: FileId, start: usize, end: usize) -> Self {
    Self {
      file_id,
      start,
      end,
    }
  }

  pub fn extend(&mut self, other: &Self) {
    assert!(self.file_id == other.file_id);
    self.start = self.start.min(other.start);
    self.end = self.end.max(other.end);
  }

  pub fn join(&self, other: &Self) -> Self {
    assert!(self.file_id == other.file_id);
    Self {
      file_id: self.file_id,
      start: self.start.min(other.start),
      end: self.end.max(other.end),
    }
  }
}
