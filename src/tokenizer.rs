use crate::{
  ast::NumericConstant,
  compiler::FileId,
  error::{Diagnostic, Result},
  span::Span,
};
use std::str::FromStr;

#[derive(Debug, Clone, PartialEq, Eq, Default)]
#[repr(u8)]
#[rustfmt::skip]
pub enum TokenKind {
  #[default]
  Identifier,
  Integer,
  Float,
  String,
  Char,

  KwAs,
  KwAsm,
  KwBind,
  KwBreak, 
  KwCallConv, 
  KwClass,
  KwClobber,
  KwConst, 
  KwContinue,
  KwElse,
  KwFn,
  KwFor,
  KwIf, 
  KwIn,
  KwInterface, 
  KwLet, 
  KwLoop,
  KwMut,
  KwNative, 
  KwNoReturn,
  KwOut,
  KwPkg,
  KwReg,
  KwReturn,
  KwStatic,
  KwTodo,
  KwType,
  KwUnreachable, 
  KwVolatile,
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
        TokenKind::Char => "char",
        TokenKind::KwAs => "as",
        TokenKind::KwAsm => "asm",
        TokenKind::KwBind => "bind",
        TokenKind::KwBreak => "break",
        TokenKind::KwCallConv => "callconv",
        TokenKind::KwClass => "class",
        TokenKind::KwClobber => "clobber",
        TokenKind::KwConst => "const",
        TokenKind::KwContinue => "continue",
        TokenKind::KwElse => "else",
        TokenKind::KwFn => "fn",
        TokenKind::KwFor => "for",
        TokenKind::KwIf => "if",
        TokenKind::KwIn => "in",
        TokenKind::KwInterface => "interface",
        TokenKind::KwLet => "let",
        TokenKind::KwLoop => "loop",
        TokenKind::KwMut => "mut",
        TokenKind::KwNative => "native",
        TokenKind::KwNoReturn => "noreturn",
        TokenKind::KwOut => "out",
        TokenKind::KwPkg => "pkg",
        TokenKind::KwReg => "reg",
        TokenKind::KwReturn => "return",
        TokenKind::KwStatic => "static",
        TokenKind::KwTodo => "todo",
        TokenKind::KwType => "type",
        TokenKind::KwUnreachable => "unreachable",
        TokenKind::KwVolatile => "volatile",
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NumericSuffix {
  U8,
  U16,
  U32,
  U64,
  U128,
  Usz,
  I8,
  I16,
  I32,
  I64,
  I128,
  Isz,
  F32,
  F64,
}

#[derive(Debug, Clone, Default)]
pub struct Token {
  pub(crate) kind: TokenKind,
  pub(crate) span: Span,
  pub(crate) literal: String,
  pub(crate) constant: Option<NumericConstant>,
}

impl Token {
  pub fn new(kind: TokenKind, span: Span, literal: String) -> Self {
    Self {
      kind,
      span,
      literal,
      constant: None,
    }
  }

  pub fn integer(span: Span, suffix: NumericConstant) -> Self {
    Self {
      kind: TokenKind::Integer,
      span,
      literal: String::new(),
      constant: Some(suffix),
    }
  }

  pub fn float(span: Span, suffix: NumericConstant) -> Self {
    Self {
      kind: TokenKind::Float,
      span,
      literal: String::new(),
      constant: Some(suffix),
    }
  }
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
          tokens.push(Token::new(
            match literal.as_str() {
              "as" => TokenKind::KwAs,
              "asm" => TokenKind::KwAsm,
              "bind" => TokenKind::KwBind,
              "break" => TokenKind::KwBreak,
              "callconv" => TokenKind::KwCallConv,
              "class" => TokenKind::KwClass,
              "clobber" => TokenKind::KwClobber,
              "const" => TokenKind::KwConst,
              "continue" => TokenKind::KwContinue,
              "else" => TokenKind::KwElse,
              "fn" => TokenKind::KwFn,
              "for" => TokenKind::KwFor,
              "if" => TokenKind::KwIf,
              "in" => TokenKind::KwIn,
              "interface" => TokenKind::KwInterface,
              "let" => TokenKind::KwLet,
              "loop" => TokenKind::KwLoop,
              "mut" => TokenKind::KwMut,
              "native" => TokenKind::KwNative,
              "noreturn" => TokenKind::KwNoReturn,
              "out" => TokenKind::KwOut,
              "pkg" => TokenKind::KwPkg,
              "reg" => TokenKind::KwReg,
              "return" => TokenKind::KwReturn,
              "static" => TokenKind::KwStatic,
              "todo" => TokenKind::KwTodo,
              "type" => TokenKind::KwType,
              "unreachable" => TokenKind::KwUnreachable,
              "volatile" => TokenKind::KwVolatile,
              "while" => TokenKind::KwWhile,
              _ => TokenKind::Identifier,
            },
            Span::new(self.file_id, start, end),
            literal,
          ));
        }

        Some('\'') => {
          self.pos += 1;
          let start = self.pos;
          while self.pos < self.source.len() && self.source.chars().nth(self.pos).unwrap() != '\'' {
            match self.source.chars().nth(self.pos).unwrap() {
              '\\' => {
                self.pos += 1;
                if self.pos >= self.source.len() {
                  return Err(Diagnostic::error(
                    Span::new(self.file_id, start, self.pos),
                    "unexpected end of input".into(),
                  ));
                }
                match self.source.chars().nth(self.pos).unwrap() {
                  'n' | 't' | 'r' | 'b' | 'f' | '\\' | '\'' | '0' => {}
                  _ => {
                    return Err(Diagnostic::error(
                      Span::new(self.file_id, start, self.pos),
                      format!(
                        "invalid escape sequence '\\{}'",
                        self.source.chars().nth(self.pos).unwrap()
                      ),
                    ));
                  }
                }
              }
              _ => {}
            }
            self.pos += 1;
          }
          let end = self.pos;
          let length = end - start;
          let literal = self.source[start..end].to_string();
          if literal.chars().nth(0).unwrap() != '\\' && length != 1 {
            return Err(Diagnostic::error(
              Span::new(self.file_id, start, end),
              "character literal must be a single character".into(),
            ));
          }
          tokens.push(Token::new(
            TokenKind::Char,
            Span::new(self.file_id, start, end),
            literal,
          ));
          self.pos += 1;
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
            let suffix = self.consume_numeric_literal_suffix();
            let end = self.pos;
            let literal = self.source[start..end].to_string();
            let literal = literal.trim_start_matches("0x");
            let span = Span::new(self.file_id, start, end);
            tokens.push(match i128::from_str_radix(literal, 16) {
              Ok(value) => self.make_number_token(value, suffix, span)?,
              Err(_) => {
                return Err(Diagnostic::error(
                  span,
                  format!("invalid hex literal '{}'", literal),
                ));
              }
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
              let suffix = self.consume_numeric_literal_suffix();
              let span = Span::new(self.file_id, start, end);
              tokens.push(match f64::from_str(&literal) {
                Ok(value) => self.make_number_token(value as i128, suffix, span)?,
                Err(_) => {
                  return Err(Diagnostic::error(
                    span,
                    format!("invalid float literal '{}'", literal),
                  ));
                }
              });
            } else {
              let end = self.pos;
              let literal = self.source[start..end].to_string();
              let suffix = self.consume_numeric_literal_suffix();
              let span = Span::new(self.file_id, start, end);
              tokens.push(match i128::from_str_radix(&literal, 10) {
                Ok(value) => self.make_number_token(value, suffix, span)?,
                Err(_) => {
                  return Err(Diagnostic::error(
                    span,
                    format!("invalid integer literal '{}'", literal),
                  ));
                }
              });
            }
          }
        }

        Some('"') => {
          self.pos += 1;
          let start = self.pos;
          while self.pos < self.source.len() && self.source.chars().nth(self.pos).unwrap() != '"' {
            let ch = self.source.chars().nth(self.pos).unwrap();
            if ch == '\\' {
              self.pos += 1;
              match self.source.chars().nth(self.pos).unwrap() {
                'n' => self.pos += 1,
                't' => self.pos += 1,
                'r' => self.pos += 1,
                'b' => self.pos += 1,
                'f' => self.pos += 1,
                '\\' => self.pos += 1,
                '"' => self.pos += 1,
                '\'' => self.pos += 1,
                _ => {
                  return Err(Diagnostic::error(
                    Span::new(self.file_id, start, self.pos),
                    format!(
                      "invalid escape sequence '\\{}'",
                      self.source.chars().nth(self.pos).unwrap()
                    ),
                  ));
                }
              }
            } else {
              self.pos += 1;
            }
          }
          let end = self.pos;
          let literal = self.source[start..end].to_string();
          tokens.push(Token::new(
            TokenKind::String,
            Span::new(self.file_id, start - 1, end + 1),
            literal,
          ));
          self.pos += 1;
        }

        Some('(') => {
          tokens.push(Token::new(
            TokenKind::OpenParen,
            Span::new(self.file_id, self.pos, self.pos + 1),
            c.unwrap().to_string(),
          ));
          self.pos += 1;
        }
        Some(')') => {
          tokens.push(Token::new(
            TokenKind::CloseParen,
            Span::new(self.file_id, self.pos, self.pos + 1),
            c.unwrap().to_string(),
          ));
          self.pos += 1;
        }

        Some('[') => {
          tokens.push(Token::new(
            TokenKind::OpenBracket,
            Span::new(self.file_id, self.pos, self.pos + 1),
            c.unwrap().to_string(),
          ));
          self.pos += 1;
        }
        Some(']') => {
          tokens.push(Token::new(
            TokenKind::CloseBracket,
            Span::new(self.file_id, self.pos, self.pos + 1),
            c.unwrap().to_string(),
          ));
          self.pos += 1;
        }

        Some('{') => {
          tokens.push(Token::new(
            TokenKind::OpenBrace,
            Span::new(self.file_id, self.pos, self.pos + 1),
            c.unwrap().to_string(),
          ));
          self.pos += 1;
        }
        Some('}') => {
          tokens.push(Token::new(
            TokenKind::CloseBrace,
            Span::new(self.file_id, self.pos, self.pos + 1),
            c.unwrap().to_string(),
          ));
          self.pos += 1;
        }

        Some('.') => {
          tokens.push(Token::new(
            TokenKind::Period,
            Span::new(self.file_id, self.pos, self.pos + 1),
            c.unwrap().to_string(),
          ));
          self.pos += 1;
        }

        Some(',') => {
          tokens.push(Token::new(
            TokenKind::Comma,
            Span::new(self.file_id, self.pos, self.pos + 1),
            c.unwrap().to_string(),
          ));
          self.pos += 1;
        }

        Some(':') => {
          tokens.push(Token::new(
            TokenKind::Colon,
            Span::new(self.file_id, self.pos, self.pos + 1),
            c.unwrap().to_string(),
          ));
          self.pos += 1;
        }
        Some(';') => {
          tokens.push(Token::new(
            TokenKind::Semicolon,
            Span::new(self.file_id, self.pos, self.pos + 1),
            c.unwrap().to_string(),
          ));
          self.pos += 1;
        }

        Some('+') => {
          if self.source.chars().nth(self.pos + 1).unwrap() == '=' {
            tokens.push(Token::new(
              TokenKind::PlusEqual,
              Span::new(self.file_id, self.pos, self.pos + 2),
              format!("{}=", c.unwrap()),
            ));
            self.pos += 2;
          } else {
            tokens.push(Token::new(
              TokenKind::Plus,
              Span::new(self.file_id, self.pos, self.pos + 1),
              c.unwrap().to_string(),
            ));
            self.pos += 1;
          }
        }

        Some('-') => {
          if self.source.chars().nth(self.pos + 1).unwrap() == '=' {
            tokens.push(Token::new(
              TokenKind::MinusEqual,
              Span::new(self.file_id, self.pos, self.pos + 2),
              format!("{}=", c.unwrap()),
            ));
            self.pos += 2;
          } else if self.source.chars().nth(self.pos + 1).unwrap() == '>' {
            tokens.push(Token::new(
              TokenKind::Arrow,
              Span::new(self.file_id, self.pos, self.pos + 2),
              format!("{}>", c.unwrap()),
            ));
            self.pos += 2;
          } else {
            tokens.push(Token::new(
              TokenKind::Minus,
              Span::new(self.file_id, self.pos, self.pos + 1),
              c.unwrap().to_string(),
            ));
            self.pos += 1;
          }
        }

        Some('*') => {
          if self.source.chars().nth(self.pos + 1).unwrap() == '=' {
            tokens.push(Token::new(
              TokenKind::AsteriskEqual,
              Span::new(self.file_id, self.pos, self.pos + 2),
              format!("{}=", c.unwrap()),
            ));
            self.pos += 2;
          } else {
            tokens.push(Token::new(
              TokenKind::Asterisk,
              Span::new(self.file_id, self.pos, self.pos + 1),
              c.unwrap().to_string(),
            ));
            self.pos += 1;
          }
        }

        Some('/') => {
          if self.source.chars().nth(self.pos + 1).unwrap() == '=' {
            tokens.push(Token::new(
              TokenKind::SlashEqual,
              Span::new(self.file_id, self.pos, self.pos + 2),
              format!("{}=", c.unwrap()),
            ));
            self.pos += 2;
          } else if self.source.chars().nth(self.pos + 1).unwrap() == '/' {
            self.pos += 2;
            while self.pos < self.source.len() && self.source.chars().nth(self.pos).unwrap() != '\n'
            {
              self.pos += 1;
            }
          } else {
            tokens.push(Token::new(
              TokenKind::Slash,
              Span::new(self.file_id, self.pos, self.pos + 1),
              c.unwrap().to_string(),
            ));
            self.pos += 1;
          }
        }

        Some('%') => {
          if self.source.chars().nth(self.pos + 1).unwrap() == '=' {
            tokens.push(Token::new(
              TokenKind::PercentEqual,
              Span::new(self.file_id, self.pos, self.pos + 2),
              format!("{}=", c.unwrap()),
            ));
            self.pos += 2;
          } else {
            tokens.push(Token::new(
              TokenKind::Percent,
              Span::new(self.file_id, self.pos, self.pos + 1),
              c.unwrap().to_string(),
            ));
            self.pos += 1;
          }
        }

        Some('=') => {
          if self.source.chars().nth(self.pos + 1).unwrap() == '=' {
            tokens.push(Token::new(
              TokenKind::EqualEqual,
              Span::new(self.file_id, self.pos, self.pos + 2),
              format!("{}=", c.unwrap()),
            ));
            self.pos += 2;
          } else {
            tokens.push(Token::new(
              TokenKind::Equal,
              Span::new(self.file_id, self.pos, self.pos + 1),
              c.unwrap().to_string(),
            ));
            self.pos += 1;
          }
        }

        Some('!') => {
          if self.source.chars().nth(self.pos + 1).unwrap() == '=' {
            tokens.push(Token::new(
              TokenKind::BangEqual,
              Span::new(self.file_id, self.pos, self.pos + 2),
              format!("{}=", c.unwrap()),
            ));
            self.pos += 2;
          } else {
            tokens.push(Token::new(
              TokenKind::Bang,
              Span::new(self.file_id, self.pos, self.pos + 1),
              c.unwrap().to_string(),
            ));
            self.pos += 1;
          }
        }

        Some('<') => {
          if self.source.chars().nth(self.pos + 1).unwrap() == '=' {
            tokens.push(Token::new(
              TokenKind::LessEqual,
              Span::new(self.file_id, self.pos, self.pos + 2),
              format!("{}=", c.unwrap()),
            ));
            self.pos += 2;
          } else {
            tokens.push(Token::new(
              TokenKind::Less,
              Span::new(self.file_id, self.pos, self.pos + 1),
              c.unwrap().to_string(),
            ));
            self.pos += 1;
          }
        }

        Some('>') => {
          if self.source.chars().nth(self.pos + 1).unwrap() == '=' {
            tokens.push(Token::new(
              TokenKind::GreaterEqual,
              Span::new(self.file_id, self.pos, self.pos + 2),
              format!("{}=", c.unwrap()),
            ));
            self.pos += 2;
          } else {
            tokens.push(Token::new(
              TokenKind::Greater,
              Span::new(self.file_id, self.pos, self.pos + 1),
              c.unwrap().to_string(),
            ));
            self.pos += 1;
          }
        }

        Some('?') => {
          tokens.push(Token::new(
            TokenKind::Question,
            Span::new(self.file_id, self.pos, self.pos + 1),
            c.unwrap().to_string(),
          ));
          self.pos += 1;
        }

        Some('&') => {
          if self.source.chars().nth(self.pos + 1).unwrap() == '=' {
            tokens.push(Token::new(
              TokenKind::AmpersandEqual,
              Span::new(self.file_id, self.pos, self.pos + 2),
              format!("{}=", c.unwrap()),
            ));
            self.pos += 2;
          } else {
            tokens.push(Token::new(
              TokenKind::Ampersand,
              Span::new(self.file_id, self.pos, self.pos + 1),
              c.unwrap().to_string(),
            ));
            self.pos += 1;
          }
        }
        Some('|') => {
          if self.source.chars().nth(self.pos + 1).unwrap() == '=' {
            tokens.push(Token::new(
              TokenKind::PipeEqual,
              Span::new(self.file_id, self.pos, self.pos + 2),
              format!("{}=", c.unwrap()),
            ));
            self.pos += 2;
          } else {
            tokens.push(Token::new(
              TokenKind::Pipe,
              Span::new(self.file_id, self.pos, self.pos + 1),
              c.unwrap().to_string(),
            ));
            self.pos += 1;
          }
        }

        _ => {
          return Err(Diagnostic::error(
            Span::new(self.file_id, self.pos, self.pos + 1),
            format!("unexpected character '{}'", c.unwrap()),
          ));
        }
      }
    }
    Ok(tokens)
  }

  fn consume_numeric_literal_suffix(&mut self) -> Option<NumericSuffix> {
    match self.source.chars().nth(self.pos).unwrap() {
      'u' | 'i' | 'f' => {}
      _ => return None,
    }

    match self.source.chars().nth(self.pos) {
      Some('u') => {
        if self.pos + 1 >= self.source.len() {
          return None;
        }
        self.pos += 1;

        match self.source.chars().nth(self.pos) {
          Some('s') => {
            if self.pos + 2 >= self.source.len() {
              return None;
            }
            self.pos += 2;

            Some(NumericSuffix::Usz)
          }
          Some('8') => {
            if self.pos + 1 >= self.source.len() {
              return None;
            }
            self.pos += 2;

            Some(NumericSuffix::U8)
          }
          Some('1') => {
            if self.pos + 1 >= self.source.len() {
              return None;
            }
            self.pos += 1;

            match self.source.chars().nth(self.pos) {
              Some('6') => {
                self.pos += 1;
                Some(NumericSuffix::U16)
              }
              Some('2') => {
                self.pos += 2;
                Some(NumericSuffix::U128)
              }
              _ => return None,
            }
          }
          Some('3') => {
            if self.pos + 1 >= self.source.len() {
              return None;
            }
            self.pos += 2;

            Some(NumericSuffix::U32)
          }
          Some('6') => {
            if self.pos + 1 >= self.source.len() {
              return None;
            }
            self.pos += 2;

            Some(NumericSuffix::U64)
          }
          _ => return None,
        }
      }
      Some('i') => {
        if self.pos + 1 >= self.source.len() {
          return None;
        }
        self.pos += 1;

        match self.source.chars().nth(self.pos) {
          Some('s') => {
            if self.pos + 2 >= self.source.len() {
              return None;
            }
            self.pos += 2;

            Some(NumericSuffix::Isz)
          }
          Some('8') => {
            if self.pos + 1 >= self.source.len() {
              return None;
            }
            self.pos += 2;

            Some(NumericSuffix::I8)
          }
          Some('1') => {
            if self.pos + 1 >= self.source.len() {
              return None;
            }
            self.pos += 1;

            match self.source.chars().nth(self.pos) {
              Some('6') => {
                self.pos += 1;
                Some(NumericSuffix::I16)
              }
              Some('2') => {
                self.pos += 2;
                Some(NumericSuffix::I128)
              }
              _ => return None,
            }
          }
          Some('3') => {
            if self.pos + 1 >= self.source.len() {
              return None;
            }
            self.pos += 2;

            Some(NumericSuffix::I32)
          }
          Some('6') => {
            if self.pos + 1 >= self.source.len() {
              return None;
            }
            self.pos += 2;

            Some(NumericSuffix::I64)
          }
          _ => return None,
        }
      }
      Some('f') => {
        if self.pos + 1 >= self.source.len() {
          return None;
        }
        self.pos += 1;

        match self.source.chars().nth(self.pos) {
          Some('3') => {
            if self.pos + 1 >= self.source.len() {
              return None;
            }
            self.pos += 2;

            Some(NumericSuffix::F32)
          }
          Some('6') => {
            if self.pos + 1 >= self.source.len() {
              return None;
            }
            self.pos += 2;

            Some(NumericSuffix::F64)
          }
          _ => return None,
        }
      }
      _ => unreachable!(),
    }
  }

  fn make_number_token(
    &self,
    number: i128,
    suffix: Option<NumericSuffix>,
    span: Span,
  ) -> Result<Token> {
    let token = match suffix {
      Some(NumericSuffix::U8) => Token::integer(span, NumericConstant::U8(number as u8)),
      Some(NumericSuffix::U16) => Token::integer(span, NumericConstant::U16(number as u16)),
      Some(NumericSuffix::U32) => Token::integer(span, NumericConstant::U32(number as u32)),
      Some(NumericSuffix::U64) => Token::integer(span, NumericConstant::U64(number as u64)),
      Some(NumericSuffix::U128) => Token::integer(span, NumericConstant::U128(number as u128)),
      Some(NumericSuffix::Usz) => Token::integer(span, NumericConstant::Usz(number as usize)),

      Some(NumericSuffix::I8) => Token::integer(span, NumericConstant::I8(number as i8)),
      Some(NumericSuffix::I16) => Token::integer(span, NumericConstant::I16(number as i16)),
      Some(NumericSuffix::I32) => Token::integer(span, NumericConstant::I32(number as i32)),
      Some(NumericSuffix::I64) => Token::integer(span, NumericConstant::I64(number as i64)),
      Some(NumericSuffix::I128) => Token::integer(span, NumericConstant::I128(number as i128)),
      Some(NumericSuffix::Isz) => Token::integer(span, NumericConstant::Isz(number as isize)),

      Some(NumericSuffix::F32) => Token::float(span, NumericConstant::F32(number as f32)),
      Some(NumericSuffix::F64) => Token::float(span, NumericConstant::F64(number as f64)),

      _ => {
        if number > i128::MAX.into() {
          if number <= i128::MAX.into() {
            Token::integer(span, NumericConstant::U128(number as u128))
          } else {
            return Err(Diagnostic::error(
              span,
              format!("Integer literal {} too large", number),
            ));
          }
        } else if number >= i128::MIN.into() {
          Token::integer(span, NumericConstant::I128(number as i128))
        } else {
          return Err(Diagnostic::error(
            span,
            format!("Integer literal {} too small", number),
          ));
        }
      }
    };
    Ok(token)
  }
}
