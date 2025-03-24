use crate::span::Span;
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

fn display_error_hint(hint: &DiagnosticHint, source: &str, filepath: &str) {
  let (mut line_n, mut col) = (1, 1);
  for i in 0..hint.span.start {
    if source.chars().nth(i).unwrap() == '\n' {
      line_n += 1;
      col = 1;
    } else {
      col += 1;
    }
  }
  let lines = source.lines().collect::<Vec<&str>>();
  let line = lines.get(line_n - 1).unwrap();

  eprintln!("\x1b[36;1mhint: \x1b[0;1m{}\x1b[0m", hint.message);
  eprintln!(
    "\x1b[34;1m{}--> \x1b[0m{}:{}:{}",
    " ".repeat(line_n.to_string().len()),
    filepath,
    line_n,
    col
  );
  eprintln!("\x1b[34;1m{} |", " ".repeat(line_n.to_string().len()),);
  eprintln!("\x1b[34;1m{line_n} |\x1b[0m {line}");
  eprintln!(
    "\x1b[34;1m{} | \x1b[36;1m{}^{}\x1b[0m",
    " ".repeat(line_n.to_string().len()),
    " ".repeat(col - 1),
    "~".repeat(hint.span.end - hint.span.start - 1)
  );
}

pub fn display_error(err: &Diagnostic, source: &str, filepath: &str) {
  let (mut line_n, mut col) = (1, 1);
  for i in 0..err.span.start {
    if source.chars().nth(i).unwrap() == '\n' {
      line_n += 1;
      col = 1;
    } else {
      col += 1;
    }
  }

  let lines = source.lines().collect::<Vec<&str>>();
  let line = lines.get(line_n - 1).unwrap();

  eprintln!(
    "\x1b[{};1m{}: \x1b[0;1m{}\x1b[0m",
    match err.severity {
      Severity::Error => "31",
      Severity::Warning => "33",
    },
    match err.severity {
      Severity::Error => "error",
      Severity::Warning => "warning",
    },
    err.message
  );
  eprintln!(
    "\x1b[34;1m{}--> \x1b[0m{}:{}:{}",
    " ".repeat(line_n.to_string().len()),
    filepath,
    line_n,
    col
  );
  eprintln!("\x1b[34;1m{} |", " ".repeat(line_n.to_string().len()),);
  eprintln!("\x1b[34;1m{line_n} |\x1b[0m {line}");
  eprintln!(
    "\x1b[34;1m{} | \x1b[{};1m{}^{}\x1b[0m",
    " ".repeat(line_n.to_string().len()),
    match err.severity {
      Severity::Error => "31",
      Severity::Warning => "33",
    },
    " ".repeat(col - 1),
    "~".repeat(err.span.end - err.span.start - 1)
  );

  for hint in &err.hints {
    display_error_hint(hint, source, filepath);
  }
}
