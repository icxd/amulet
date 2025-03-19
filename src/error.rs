use crate::span::Span;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Clone)]
pub struct ErrorHint {
  pub(crate) span: Span,
  pub(crate) message: String,
}

#[derive(Debug, Clone)]
pub struct Error {
  pub(crate) span: Span,
  pub(crate) message: String,
  pub(crate) hints: Vec<ErrorHint>,
}

impl Error {
  pub fn new(span: Span, message: String) -> Self {
    Self {
      span,
      message,
      hints: vec![],
    }
  }

  pub fn with_hint(&mut self, span: Span, message: String) -> Self {
    self.hints.push(ErrorHint { span, message });
    self.clone()
  }
}

fn display_error_hint(hint: &ErrorHint, source: &str, filepath: &str) {
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

pub fn display_error(err: &Error, source: &str, filepath: &str) {
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

  eprintln!("\x1b[31;1merror: \x1b[0;1m{}\x1b[0m", err.message);
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
    "\x1b[34;1m{} | \x1b[31;1m{}^{}\x1b[0m",
    " ".repeat(line_n.to_string().len()),
    " ".repeat(col - 1),
    "~".repeat(err.span.end - err.span.start - 1)
  );

  for hint in &err.hints {
    display_error_hint(hint, source, filepath);
  }
}
