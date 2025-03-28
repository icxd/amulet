use crate::compiler::FileId;
use serde::{ser::SerializeStruct, Serialize};

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

  pub fn contains(&self, position: usize) -> bool {
    self.start <= position && position <= self.end
  }
}

impl Serialize for Span {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: serde::Serializer,
  {
    let mut s = serializer.serialize_struct("Span", 2)?;
    s.serialize_field("start", &self.start)?;
    s.serialize_field("end", &self.end)?;
    s.end()
  }
}
