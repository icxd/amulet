use crate::{
  compiler::FileId,
  error::{Error, Result},
  span::Span,
};

#[derive(Debug, Clone, PartialEq, Eq, Default)]
#[repr(u8)]
#[rustfmt::skip]
pub enum TokenKind {
  #[default]
  Identifier,
  Integer,
  Float,
  String,

  KwAs,
  KwAsm,
  KwBreak, 
  KwContinue,
  KwCallConv, 
  KwClass,
  KwConst, 
  KwElse,
  KwFn,
  KwFor,
  KwIf, 
  KwInterface, 
  KwLet, 
  KwLoop,
  KwMut,
  KwNative, 
  KwNoReturn,
  KwPkg,
  KwReturn,
  KwStatic,
  KwTodo,
  KwType,
  KwUnreachable, 
  KwWhile,

  OpenParen, CloseParen,
  OpenBracket, CloseBracket,
  OpenBrace, CloseBrace,
  Period, Comma,
  Colon, Semicolon,
  Arrow,

  Plus, PlusEqual,
  Minus, MinusEqual,
  Asterisk, AsteriskEqual,
  Slash, SlashEqual,
  Percent, PercentEqual,
  Equal, EqualEqual,
  Bang, BangEqual,
  Less, LessEqual,
  Greater, GreaterEqual,
  Ampersand, AmpersandEqual,
  Pipe, PipeEqual,
  Question,
}

impl std::fmt::Display for TokenKind {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "{}",
      match self {
        TokenKind::Identifier => "identifier",
        TokenKind::Integer => "integer",
        TokenKind::Float => "float",
        TokenKind::String => "string",
        TokenKind::KwAs => "as",
        TokenKind::KwAsm => "asm",
        TokenKind::KwBreak => "break",
        TokenKind::KwCallConv => "callconv",
        TokenKind::KwClass => "class",
        TokenKind::KwConst => "const",
        TokenKind::KwContinue => "continue",
        TokenKind::KwElse => "else",
        TokenKind::KwFn => "fn",
        TokenKind::KwFor => "for",
        TokenKind::KwIf => "if",
        TokenKind::KwInterface => "interface",
        TokenKind::KwLet => "let",
        TokenKind::KwLoop => "loop",
        TokenKind::KwMut => "mut",
        TokenKind::KwNative => "native",
        TokenKind::KwNoReturn => "noreturn",
        TokenKind::KwPkg => "pkg",
        TokenKind::KwReturn => "return",
        TokenKind::KwStatic => "static",
        TokenKind::KwTodo => "todo",
        TokenKind::KwType => "type",
        TokenKind::KwUnreachable => "unreachable",
        TokenKind::KwWhile => "while",
        TokenKind::OpenParen => "(",
        TokenKind::CloseParen => ")",
        TokenKind::OpenBracket => "[",
        TokenKind::CloseBracket => "]",
        TokenKind::OpenBrace => "{",
        TokenKind::CloseBrace => "}",
        TokenKind::Period => ".",
        TokenKind::Comma => ",",
        TokenKind::Colon => ":",
        TokenKind::Semicolon => ";",
        TokenKind::Arrow => "->",
        TokenKind::Plus => "+",
        TokenKind::PlusEqual => "+=",
        TokenKind::Minus => "-",
        TokenKind::MinusEqual => "-=",
        TokenKind::Asterisk => "*",
        TokenKind::AsteriskEqual => "*=",
        TokenKind::Slash => "/",
        TokenKind::SlashEqual => "/=",
        TokenKind::Percent => "%",
        TokenKind::PercentEqual => "%=",
        TokenKind::Equal => "=",
        TokenKind::EqualEqual => "==",
        TokenKind::Bang => "!",
        TokenKind::BangEqual => "!=",
        TokenKind::Less => "<",
        TokenKind::LessEqual => "<=",
        TokenKind::Greater => ">",
        TokenKind::GreaterEqual => ">=",
        TokenKind::Ampersand => "&",
        TokenKind::AmpersandEqual => "&=",
        TokenKind::Pipe => "|",
        TokenKind::PipeEqual => "|=",
        TokenKind::Question => "?",
      }
    )
  }
}

#[derive(Debug, Clone, Default)]
pub struct Token {
  pub(crate) kind: TokenKind,
  pub(crate) span: Span,
  pub(crate) literal: String,
}

pub struct Tokenizer {
  file_id: FileId,
  source: String,
  pos: usize,
}

impl Tokenizer {
  pub fn new(file_id: FileId, source: String) -> Self {
    Self {
      file_id,
      source,
      pos: 0,
    }
  }

  pub fn tokenize(&mut self) -> Result<Vec<Token>> {
    let mut tokens = vec![];
    while self.pos < self.source.len() {
      let c = self.source.chars().nth(self.pos);

      match c {
        Some(' ') | Some('\t') | Some('\n') => {
          self.pos += 1;
        }

        Some('a'..='z') | Some('A'..='Z') | Some('_') => {
          let start = self.pos;
          while self.pos < self.source.len()
            && (self.source.chars().nth(self.pos).unwrap().is_alphanumeric()
              || self.source.chars().nth(self.pos).unwrap() == '_')
          {
            self.pos += 1;
          }
          let end = self.pos;
          let literal = self.source[start..end].to_string();
          tokens.push(Token {
            kind: match literal.as_str() {
              "as" => TokenKind::KwAs,
              "asm" => TokenKind::KwAsm,
              "break" => TokenKind::KwBreak,
              "callconv" => TokenKind::KwCallConv,
              "class" => TokenKind::KwClass,
              "const" => TokenKind::KwConst,
              "continue" => TokenKind::KwContinue,
              "else" => TokenKind::KwElse,
              "fn" => TokenKind::KwFn,
              "for" => TokenKind::KwFor,
              "if" => TokenKind::KwIf,
              "interface" => TokenKind::KwInterface,
              "let" => TokenKind::KwLet,
              "loop" => TokenKind::KwLoop,
              "mut" => TokenKind::KwMut,
              "native" => TokenKind::KwNative,
              "noreturn" => TokenKind::KwNoReturn,
              "pkg" => TokenKind::KwPkg,
              "return" => TokenKind::KwReturn,
              "static" => TokenKind::KwStatic,
              "todo" => TokenKind::KwTodo,
              "type" => TokenKind::KwType,
              "unreachable" => TokenKind::KwUnreachable,
              "while" => TokenKind::KwWhile,
              _ => TokenKind::Identifier,
            },
            span: Span::new(self.file_id, start, end),
            literal,
          });
        }

        Some('0'..='9') => {
          let start = self.pos;
          if self.source.chars().nth(self.pos).unwrap() == '0'
            && self.pos + 1 < self.source.len()
            && self.source.chars().nth(self.pos + 1).unwrap() == 'x'
          {
            self.pos += 2;
            while self.pos < self.source.len()
              && self.source.chars().nth(self.pos).unwrap().is_digit(16)
            {
              self.pos += 1;
            }
            let end = self.pos;
            let literal = self.source[start..end].to_string();
            let literal = literal.trim_start_matches("0x");
            tokens.push(Token {
              kind: TokenKind::Integer,
              span: Span::new(self.file_id, start, end),
              literal: match i128::from_str_radix(literal, 16) {
                Ok(value) => value.to_string(),
                Err(_) => panic!("internal error: unable to parse integer literal"),
              },
            });
          } else {
            while self.pos < self.source.len()
              && self.source.chars().nth(self.pos).unwrap().is_digit(10)
            {
              self.pos += 1;
            }
            if self.source.chars().nth(self.pos).unwrap() == '.' {
              self.pos += 1;
              while self.pos < self.source.len()
                && self.source.chars().nth(self.pos).unwrap().is_digit(10)
              {
                self.pos += 1;
              }
              let end = self.pos;
              let literal = self.source[start..end].to_string();
              tokens.push(Token {
                kind: TokenKind::Float,
                span: Span::new(self.file_id, start, end),
                literal,
              });
            } else {
              let end = self.pos;
              let literal = self.source[start..end].to_string();
              tokens.push(Token {
                kind: TokenKind::Integer,
                span: Span::new(self.file_id, start, end),
                literal,
              });
            }
          }
        }

        Some('"') => {
          self.pos += 1;
          let start = self.pos;
          while self.pos < self.source.len() && self.source.chars().nth(self.pos).unwrap() != '"' {
            self.pos += 1;
          }
          let end = self.pos;
          let literal = self.source[start..end].to_string();
          tokens.push(Token {
            kind: TokenKind::String,
            span: Span::new(self.file_id, start, end),
            literal,
          });
          self.pos += 1;
        }

        Some('(') => {
          tokens.push(Token {
            kind: TokenKind::OpenParen,
            span: Span::new(self.file_id, self.pos, self.pos + 1),
            literal: c.unwrap().to_string(),
          });
          self.pos += 1;
        }
        Some(')') => {
          tokens.push(Token {
            kind: TokenKind::CloseParen,
            span: Span::new(self.file_id, self.pos, self.pos + 1),
            literal: c.unwrap().to_string(),
          });
          self.pos += 1;
        }

        Some('[') => {
          tokens.push(Token {
            kind: TokenKind::OpenBracket,
            span: Span::new(self.file_id, self.pos, self.pos + 1),
            literal: c.unwrap().to_string(),
          });
          self.pos += 1;
        }
        Some(']') => {
          tokens.push(Token {
            kind: TokenKind::CloseBracket,
            span: Span::new(self.file_id, self.pos, self.pos + 1),
            literal: c.unwrap().to_string(),
          });
          self.pos += 1;
        }

        Some('{') => {
          tokens.push(Token {
            kind: TokenKind::OpenBrace,
            span: Span::new(self.file_id, self.pos, self.pos + 1),
            literal: c.unwrap().to_string(),
          });
          self.pos += 1;
        }
        Some('}') => {
          tokens.push(Token {
            kind: TokenKind::CloseBrace,
            span: Span::new(self.file_id, self.pos, self.pos + 1),
            literal: c.unwrap().to_string(),
          });
          self.pos += 1;
        }

        Some('.') => {
          tokens.push(Token {
            kind: TokenKind::Period,
            span: Span::new(self.file_id, self.pos, self.pos + 1),
            literal: c.unwrap().to_string(),
          });
          self.pos += 1;
        }

        Some(',') => {
          tokens.push(Token {
            kind: TokenKind::Comma,
            span: Span::new(self.file_id, self.pos, self.pos + 1),
            literal: c.unwrap().to_string(),
          });
          self.pos += 1;
        }

        Some(':') => {
          tokens.push(Token {
            kind: TokenKind::Colon,
            span: Span::new(self.file_id, self.pos, self.pos + 1),
            literal: c.unwrap().to_string(),
          });
          self.pos += 1;
        }
        Some(';') => {
          tokens.push(Token {
            kind: TokenKind::Semicolon,
            span: Span::new(self.file_id, self.pos, self.pos + 1),
            literal: c.unwrap().to_string(),
          });
          self.pos += 1;
        }

        Some('+') => {
          if self.source.chars().nth(self.pos + 1).unwrap() == '=' {
            tokens.push(Token {
              kind: TokenKind::PlusEqual,
              span: Span::new(self.file_id, self.pos, self.pos + 2),
              literal: format!("{}=", c.unwrap()),
            });
            self.pos += 2;
          } else {
            tokens.push(Token {
              kind: TokenKind::Plus,
              span: Span::new(self.file_id, self.pos, self.pos + 1),
              literal: c.unwrap().to_string(),
            });
            self.pos += 1;
          }
        }

        Some('-') => {
          if self.source.chars().nth(self.pos + 1).unwrap() == '=' {
            tokens.push(Token {
              kind: TokenKind::MinusEqual,
              span: Span::new(self.file_id, self.pos, self.pos + 2),
              literal: format!("{}=", c.unwrap()),
            });
            self.pos += 2;
          } else if self.source.chars().nth(self.pos + 1).unwrap() == '>' {
            tokens.push(Token {
              kind: TokenKind::Arrow,
              span: Span::new(self.file_id, self.pos, self.pos + 2),
              literal: format!("{}>", c.unwrap()),
            });
            self.pos += 2;
          } else {
            tokens.push(Token {
              kind: TokenKind::Minus,
              span: Span::new(self.file_id, self.pos, self.pos + 1),
              literal: c.unwrap().to_string(),
            });
            self.pos += 1;
          }
        }

        Some('*') => {
          if self.source.chars().nth(self.pos + 1).unwrap() == '=' {
            tokens.push(Token {
              kind: TokenKind::AsteriskEqual,
              span: Span::new(self.file_id, self.pos, self.pos + 2),
              literal: format!("{}=", c.unwrap()),
            });
            self.pos += 2;
          } else {
            tokens.push(Token {
              kind: TokenKind::Asterisk,
              span: Span::new(self.file_id, self.pos, self.pos + 1),
              literal: c.unwrap().to_string(),
            });
            self.pos += 1;
          }
        }

        Some('/') => {
          if self.source.chars().nth(self.pos + 1).unwrap() == '=' {
            tokens.push(Token {
              kind: TokenKind::SlashEqual,
              span: Span::new(self.file_id, self.pos, self.pos + 2),
              literal: format!("{}=", c.unwrap()),
            });
            self.pos += 2;
          } else if self.source.chars().nth(self.pos + 1).unwrap() == '/' {
            self.pos += 2;
            while self.pos < self.source.len() && self.source.chars().nth(self.pos).unwrap() != '\n'
            {
              self.pos += 1;
            }
          } else {
            tokens.push(Token {
              kind: TokenKind::Slash,
              span: Span::new(self.file_id, self.pos, self.pos + 1),
              literal: c.unwrap().to_string(),
            });
            self.pos += 1;
          }
        }

        Some('%') => {
          if self.source.chars().nth(self.pos + 1).unwrap() == '=' {
            tokens.push(Token {
              kind: TokenKind::PercentEqual,
              span: Span::new(self.file_id, self.pos, self.pos + 2),
              literal: format!("{}=", c.unwrap()),
            });
            self.pos += 2;
          } else {
            tokens.push(Token {
              kind: TokenKind::Percent,
              span: Span::new(self.file_id, self.pos, self.pos + 1),
              literal: c.unwrap().to_string(),
            });
            self.pos += 1;
          }
        }

        Some('=') => {
          if self.source.chars().nth(self.pos + 1).unwrap() == '=' {
            tokens.push(Token {
              kind: TokenKind::EqualEqual,
              span: Span::new(self.file_id, self.pos, self.pos + 2),
              literal: format!("{}=", c.unwrap()),
            });
            self.pos += 2;
          } else {
            tokens.push(Token {
              kind: TokenKind::Equal,
              span: Span::new(self.file_id, self.pos, self.pos + 1),
              literal: c.unwrap().to_string(),
            });
            self.pos += 1;
          }
        }

        Some('!') => {
          if self.source.chars().nth(self.pos + 1).unwrap() == '=' {
            tokens.push(Token {
              kind: TokenKind::BangEqual,
              span: Span::new(self.file_id, self.pos, self.pos + 2),
              literal: format!("{}=", c.unwrap()),
            });
            self.pos += 2;
          } else {
            tokens.push(Token {
              kind: TokenKind::Bang,
              span: Span::new(self.file_id, self.pos, self.pos + 1),
              literal: c.unwrap().to_string(),
            });
            self.pos += 1;
          }
        }

        Some('<') => {
          if self.source.chars().nth(self.pos + 1).unwrap() == '=' {
            tokens.push(Token {
              kind: TokenKind::LessEqual,
              span: Span::new(self.file_id, self.pos, self.pos + 2),
              literal: format!("{}=", c.unwrap()),
            });
            self.pos += 2;
          } else {
            tokens.push(Token {
              kind: TokenKind::Less,
              span: Span::new(self.file_id, self.pos, self.pos + 1),
              literal: c.unwrap().to_string(),
            });
            self.pos += 1;
          }
        }

        Some('>') => {
          if self.source.chars().nth(self.pos + 1).unwrap() == '=' {
            tokens.push(Token {
              kind: TokenKind::GreaterEqual,
              span: Span::new(self.file_id, self.pos, self.pos + 2),
              literal: format!("{}=", c.unwrap()),
            });
            self.pos += 2;
          } else {
            tokens.push(Token {
              kind: TokenKind::Greater,
              span: Span::new(self.file_id, self.pos, self.pos + 1),
              literal: c.unwrap().to_string(),
            });
            self.pos += 1;
          }
        }

        Some('?') => {
          tokens.push(Token {
            kind: TokenKind::Question,
            span: Span::new(self.file_id, self.pos, self.pos + 1),
            literal: c.unwrap().to_string(),
          });
          self.pos += 1;
        }

        Some('&') => {
          if self.source.chars().nth(self.pos + 1).unwrap() == '=' {
            tokens.push(Token {
              kind: TokenKind::AmpersandEqual,
              span: Span::new(self.file_id, self.pos, self.pos + 2),
              literal: format!("{}=", c.unwrap()),
            });
            self.pos += 2;
          } else {
            tokens.push(Token {
              kind: TokenKind::Ampersand,
              span: Span::new(self.file_id, self.pos, self.pos + 1),
              literal: c.unwrap().to_string(),
            });
            self.pos += 1;
          }
        }
        Some('|') => {
          if self.source.chars().nth(self.pos + 1).unwrap() == '=' {
            tokens.push(Token {
              kind: TokenKind::PipeEqual,
              span: Span::new(self.file_id, self.pos, self.pos + 2),
              literal: format!("{}=", c.unwrap()),
            });
            self.pos += 2;
          } else {
            tokens.push(Token {
              kind: TokenKind::Pipe,
              span: Span::new(self.file_id, self.pos, self.pos + 1),
              literal: c.unwrap().to_string(),
            });
            self.pos += 1;
          }
        }

        _ => {
          return Err(Error::new(
            Span::new(self.file_id, self.pos, self.pos + 1),
            format!("unexpected character '{}'", c.unwrap()),
          ));
        }
      }
    }
    Ok(tokens)
  }
}
