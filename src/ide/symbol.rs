use crate::span::Span;
use serde::{ser::SerializeStruct, Serialize};

#[derive(Debug, Clone, PartialEq)]
pub enum SymbolKind {
  Function,
  Variable,
  Class,
  Interface,
  Namespace,
}

impl ToString for SymbolKind {
  fn to_string(&self) -> String {
    match self {
      SymbolKind::Function => "function".to_string(),
      SymbolKind::Variable => "variable".to_string(),
      SymbolKind::Class => "class".to_string(),
      SymbolKind::Interface => "interface".to_string(),
      SymbolKind::Namespace => "namespace".to_string(),
    }
  }
}

#[derive(Debug, Clone)]
pub struct Symbol {
  pub(crate) name: String,
  pub(crate) detail: Option<String>,
  pub(crate) kind: SymbolKind,
  pub(crate) range: Span,
  pub(crate) selection_range: Span,
  pub(crate) children: Vec<Symbol>,
}

impl Serialize for Symbol {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: serde::Serializer,
  {
    let mut s = serializer.serialize_struct("Symbol", 6)?;
    s.serialize_field("name", &self.name)?;
    if self.detail.is_some() {
      s.serialize_field("detail", &self.detail)?;
    }
    s.serialize_field("kind", &self.kind.to_string())?;
    s.serialize_field("range", &self.range)?;
    s.serialize_field("selection_range", &self.selection_range)?;
    s.serialize_field("children", &self.children)?;
    s.end()
  }
}
