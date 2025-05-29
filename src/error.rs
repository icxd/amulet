use crate::span::Span;
use colored::{Color, Colorize};
use lyneate::Report;
use serde::{ser::SerializeStruct, Serialize};

pub type Result<T> = std::result::Result<T, Diagnostic>;

#[derive(Debug, Clone)]
pub struct DiagnosticHint {
  pub(crate) span: Span,
  pub(crate) message: String,
}

impl Serialize for DiagnosticHint {
  fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
  where
    S: serde::Serializer,
  {
    let mut s = serializer.serialize_struct("DiagnosticHint", 2)?;
    s.serialize_field("span", &self.span)?;
    s.serialize_field("message", &self.message)?;
    s.end()
  }
}

#[derive(Debug, Clone, Copy)]
pub enum Severity {
  Error,
  Warning,
}

impl Serialize for Severity {
  fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
  where
    S: serde::Serializer,
  {
    serializer.serialize_str(match self {
      Severity::Error => "error",
      Severity::Warning => "warning",
    })
  }
}

impl Severity {
  fn display(&self) -> String {
    use Severity::*;
    match self {
      Error => "Error:".bright_red(),
      Warning => "Warning:".bright_yellow(),
    }
    .to_string()
  }

  fn color(&self) -> (u8, u8, u8) {
    use Severity::*;
    match self {
      Error => (255, 64, 112),
      Warning => (255, 159, 64),
    }
  }
}

#[derive(Debug, Clone)]
pub struct Diagnostic {
  pub(crate) severity: Severity,
  pub(crate) span: Span,
  pub(crate) message: String,
  pub(crate) hints: Vec<DiagnosticHint>,
}

impl Diagnostic {
  pub fn error(span: Span, message: String) -> Self {
    Self {
      severity: Severity::Error,
      span,
      message,
      hints: vec![],
    }
  }

  pub fn warning(span: Span, message: String) -> Self {
    Self {
      severity: Severity::Warning,
      span,
      message,
      hints: vec![],
    }
  }

  pub fn with_hint(&mut self, span: Span, message: String) -> Self {
    self.hints.push(DiagnosticHint { span, message });
    self.clone()
  }

  pub fn display(&self, source: &str) {
    let mut messages = vec![(
      self.span.start..self.span.end,
      self.message.custom_color(self.severity.color()).to_string(),
      self.severity.color(),
    )];
    for hint in &self.hints {
      messages.push((
        hint.span.start..hint.span.end,
        hint.message.custom_color((64, 140, 255)).to_string(),
        (64, 140, 255),
      ));
    }

    eprintln!("{} {}", self.severity.display(), self.message);
    Report::new_char_spanned(&source, messages).display();
  }
}

impl Serialize for Diagnostic {
  fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
  where
    S: serde::Serializer,
  {
    let mut s = serializer.serialize_struct("Diagnostic", 4)?;
    s.serialize_field("severity", &self.severity)?;
    s.serialize_field("span", &self.span)?;
    s.serialize_field("message", &self.message)?;
    s.serialize_field("hints", &self.hints)?;
    s.end()
  }
}
