pub mod ast;
pub mod checker;
pub mod error;
pub mod parser;
pub mod span;
pub mod tokenizer;

fn main() {
  env_logger::init();

  let mut args = std::env::args();
  args.next();
  let path = args.next().unwrap();
  let source = std::fs::read_to_string(path.clone()).unwrap();
  let mut tokenizer = tokenizer::Tokenizer::new(source.clone());
  match tokenizer.tokenize() {
    Ok(tokens) => {
      let mut parser = parser::Parser::new(tokens);
      let namespace = parser.parse();
      if let Ok(namespace) = namespace {
        println!("{:#?}", namespace);
      } else {
        let err = namespace.unwrap_err();
        error::display_error(&err, &source, &path);
        std::process::exit(1);
      }
    }
    Err(err) => {
      error::display_error(&err, &source, &path);
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use tokenizer::{TokenKind, Tokenizer};

  #[test]
  fn test_tokenize() {
    let source = "
      fn main() {
        println(\"Hello, World!\");
      }
    ";
    let mut tokenizer = Tokenizer::new(source.to_string());
    let tokens = tokenizer.tokenize().unwrap();
    let tokens = tokens.iter().map(|t| t.kind.clone()).collect::<Vec<_>>();
    assert_eq!(
      tokens,
      vec![
        TokenKind::KwFn,
        TokenKind::Identifier,
        TokenKind::OpenParen,
        TokenKind::CloseParen,
        TokenKind::OpenBrace,
        TokenKind::Identifier,
        TokenKind::OpenParen,
        TokenKind::String,
        TokenKind::CloseParen,
        TokenKind::Semicolon,
        TokenKind::CloseBrace,
      ]
    );
  }
}
