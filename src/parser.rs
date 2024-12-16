use log::trace;

use crate::{
  ast::{
    BinaryOperator, NumericConstant, ParsedBlock, ParsedCall, ParsedExpression, ParsedFunction,
    ParsedNamespace, ParsedStatement, ParsedType, ParsedTypeDecl, ParsedTypeDeclData,
    ParsedVarDecl, ParsedVariable,
  },
  error::{Error, Result},
  span::Span,
  tokenizer::{Token, TokenKind},
};

#[derive(Debug, Clone)]
pub struct Parser {
  tokens: Vec<Token>,
  pos: usize,
}

impl Parser {
  pub fn new(tokens: Vec<Token>) -> Self {
    Self { tokens, pos: 0 }
  }

  pub fn parse(&mut self) -> Result<ParsedNamespace> {
    trace!("parse: {:?}", self.current());
    let namespace = self.parse_namespace()?;
    Ok(namespace)
  }

  fn parse_namespace(&mut self) -> Result<ParsedNamespace> {
    trace!("parse_namespace: {:?}", self.current());
    let mut namespace = ParsedNamespace::new();

    while self.pos < self.tokens.len() {
      let token = self.tokens.get(self.pos).unwrap();

      match token.kind {
        TokenKind::KwFn => {
          let fun = self.parse_fn()?;
          namespace.functions.push(fun);
        }

        TokenKind::KwPkg => {
          let _ = self.expect(TokenKind::KwPkg)?;
          let name = self.expect(TokenKind::Identifier)?;
          self.expect(TokenKind::OpenBrace)?;
          let mut ns = self.parse_namespace()?;
          ns.name = Some(name.literal.clone());
          self.expect(TokenKind::CloseBrace)?;
          namespace.namespaces.push(ns);
        }

        TokenKind::KwType => {
          let type_decl = self.parse_type_decl()?;
          namespace.type_decls.push(type_decl);
        }

        _ => {
          return Err(Error::new(
            token.span,
            format!("unexpected token `{}`", token.kind),
          ))
        }
      }
    }

    Ok(namespace)
  }

  fn parse_type_decl(&mut self) -> Result<ParsedTypeDecl> {
    trace!("parse_type_decl: {:?}", self.current());
    let _ = self.expect(TokenKind::KwType)?;
    let name_token = self.expect(TokenKind::Identifier)?;
    let name = name_token.literal.clone();
    let name_span = name_token.span;
    let type_params = self.parse_type_params()?;
    let _ = self.expect(TokenKind::Equal)?;
    let data = self.parse_type_decl_data()?;
    self.expect(TokenKind::Semicolon)?;
    Ok(ParsedTypeDecl {
      name,
      name_span,
      type_parameters: type_params,
      data,
    })
  }

  fn parse_type_decl_data(&mut self) -> Result<ParsedTypeDeclData> {
    trace!("parse_type_decl_data: {:?}", self.current());
    match self.current().kind {
      TokenKind::KwInterface => {
        self.expect(TokenKind::KwInterface)?;
        self.expect(TokenKind::OpenBrace)?;
        let mut functions = vec![];
        while self.current().kind != TokenKind::CloseBrace {
          let fun = self.parse_fn()?;
          functions.push(fun);
        }
        self.expect(TokenKind::CloseBrace)?;
        Ok(ParsedTypeDeclData::Interface(functions))
      }

      TokenKind::KwClass => {
        self.expect(TokenKind::KwClass)?;
        let mut implements = vec![];
        let mut fields = vec![];
        let mut methods = vec![];
        if self.current().kind != TokenKind::OpenBrace {
          implements.push(self.parse_type()?);
          while self.current().kind == TokenKind::Ampersand {
            self.expect(TokenKind::Ampersand)?;
            implements.push(self.parse_type()?);
          }
        }
        self.expect(TokenKind::OpenBrace)?;
        while self.current().kind != TokenKind::CloseBrace {
          match self.current().kind {
            TokenKind::KwMut => {
              self.expect(TokenKind::KwMut)?;
              let name = self.expect(TokenKind::Identifier)?;
              let _ = self.expect(TokenKind::Colon)?;
              let r#type = self.parse_type()?;
              let value = if self.current().kind == TokenKind::Equal {
                self.expect(TokenKind::Equal)?;
                Some(self.parse_expression()?)
              } else {
                None
              };
              self.expect(TokenKind::Semicolon)?;
              fields.push((
                ParsedVariable {
                  mutable: true,
                  name: name.literal.clone(),
                  r#type,
                },
                value,
              ));
            }

            TokenKind::KwFn => {
              let fun = self.parse_fn()?;
              methods.push(fun);
            }

            _ => {
              return Err(Error::new(
                self.current().span,
                format!(
                  "expected class field or method but found `{}`",
                  self.current().kind
                ),
              ))
            }
          }
        }
        self.expect(TokenKind::CloseBrace)?;
        Ok(ParsedTypeDeclData::Class {
          implements,
          fields,
          methods,
        })
      }

      _ => {
        return Err(Error::new(
          self.current().span,
          format!(
            "expected type decl data but found `{}`",
            self.current().kind
          ),
        ))
      }
    }
  }

  fn parse_fn(&mut self) -> Result<ParsedFunction> {
    trace!("parse_fn: {:?}", self.current());
    let _ = self.expect(TokenKind::KwFn)?;
    let name_token = self.expect(TokenKind::Identifier)?;
    let name = name_token.literal.clone();
    let name_span = name_token.span;
    let type_params = self.parse_type_params()?;
    let parameters = self.parse_parameters()?;
    let return_type = if self.current().kind == TokenKind::Arrow {
      self.expect(TokenKind::Arrow)?;
      Some(self.parse_type()?)
    } else {
      None
    };
    let body = if self.current().kind == TokenKind::OpenBrace {
      self.parse_block()?
    } else if self.current().kind == TokenKind::Equal {
      self.pos += 1;
      let expr = self.parse_expression()?;
      self.expect(TokenKind::Semicolon)?;
      ParsedBlock {
        stmts: vec![ParsedStatement::Return(expr)],
      }
    } else {
      self.expect(TokenKind::Semicolon)?;
      ParsedBlock { stmts: vec![] }
    };
    Ok(ParsedFunction {
      name,
      name_span,
      type_parameters: type_params,
      parameters,
      return_type,
      body,
    })
  }

  fn parse_parameters(&mut self) -> Result<Vec<ParsedVariable>> {
    trace!("parse_parameters: {:?}", self.current());
    let mut parameters = vec![];
    if self.current().kind == TokenKind::OpenParen {
      self.expect(TokenKind::OpenParen)?;
      while self.current().kind != TokenKind::CloseParen {
        let param = self.parse_variable()?;
        parameters.push(param);
        if self.current().kind == TokenKind::Comma {
          self.expect(TokenKind::Comma)?;
        }
      }
      self.expect(TokenKind::CloseParen)?;
    }
    Ok(parameters)
  }

  fn parse_block(&mut self) -> Result<ParsedBlock> {
    trace!("parse_block: {:?}", self.current());
    self.expect(TokenKind::OpenBrace)?;
    let mut stmts = vec![];
    while self.current().kind != TokenKind::CloseBrace {
      let stmt = self.parse_statement()?;
      stmts.push(stmt);
    }
    self.expect(TokenKind::CloseBrace)?;
    Ok(ParsedBlock { stmts })
  }

  fn parse_variable(&mut self) -> Result<ParsedVariable> {
    trace!("parse_variable: {:?}", self.current());
    let mutable = if self.current().kind == TokenKind::KwMut {
      self.expect(TokenKind::KwMut)?;
      true
    } else {
      false
    };
    if self.current().kind == TokenKind::Identifier && self.current().literal == "self" {
      self.expect(TokenKind::Identifier)?;
      return Ok(ParsedVariable {
        mutable,
        name: "self".to_string(),
        r#type: ParsedType::Name("Self".to_string(), self.current().span),
      });
    }
    let name = self.expect(TokenKind::Identifier)?;
    let _ = self.expect(TokenKind::Colon)?;
    let r#type = self.parse_type()?;
    Ok(ParsedVariable {
      mutable,
      name: name.literal.clone(),
      r#type,
    })
  }

  fn parse_type_params(&mut self) -> Result<Vec<(String, Span)>> {
    trace!("parse_type_params: {:?}", self.current());
    let mut type_params = vec![];
    if self.current().kind == TokenKind::Less {
      self.expect(TokenKind::Less)?;
      while self.current().kind != TokenKind::Greater {
        let name = self.expect(TokenKind::Identifier)?;
        type_params.push((name.literal.clone(), name.span));
        if self.current().kind == TokenKind::Comma {
          self.expect(TokenKind::Comma)?;
        }
      }
      self.expect(TokenKind::Greater)?;
    }
    Ok(type_params)
  }

  fn parse_statement(&mut self) -> Result<ParsedStatement> {
    trace!("parse_statement: {:?}", self.current());
    match self.current().kind {
      TokenKind::KwReturn => {
        self.pos += 1;
        let expr = self.parse_expression()?;
        self.expect(TokenKind::Semicolon)?;
        Ok(ParsedStatement::Return(expr))
      }

      TokenKind::KwLet | TokenKind::KwMut => {
        let mutable = self.current().kind == TokenKind::KwMut;
        self.pos += 1;
        let name_token = self.expect(TokenKind::Identifier)?;
        let name = name_token.literal;
        let span = name_token.span;
        let ty = if self.current().kind == TokenKind::Colon {
          self.pos += 1;
          self.parse_type()?
        } else {
          ParsedType::Empty
        };
        self.expect(TokenKind::Equal)?;
        let value = self.parse_expression()?;
        self.expect(TokenKind::Semicolon)?;
        Ok(ParsedStatement::VarDecl(
          ParsedVarDecl {
            name,
            ty,
            mutable,
            span,
          },
          value,
        ))
      }

      TokenKind::KwIf => {
        self.pos += 1;
        self.expect(TokenKind::OpenParen)?;
        let condition = self.parse_expression()?;
        self.expect(TokenKind::CloseParen)?;
        let then = self.parse_block()?;
        let mut else_ = None;
        if self.current().kind == TokenKind::KwElse {
          self.pos += 1;
          else_ = Some(self.parse_block()?);
        }
        Ok(ParsedStatement::If(condition, then, else_.map(Box::new)))
      }

      TokenKind::KwWhile => {
        self.pos += 1;
        self.expect(TokenKind::OpenParen)?;
        let condition = self.parse_expression()?;
        self.expect(TokenKind::CloseParen)?;
        let body = self.parse_block()?;
        Ok(ParsedStatement::While(condition, body))
      }

      TokenKind::KwBreak => {
        self.pos += 1;
        Ok(ParsedStatement::Break)
      }
      TokenKind::KwContinue => {
        self.pos += 1;
        Ok(ParsedStatement::Continue)
      }

      TokenKind::OpenBrace => {
        let block = self.parse_block()?;
        Ok(ParsedStatement::Block(block))
      }

      _ => {
        let expr = self.parse_expression()?;
        self.expect(TokenKind::Semicolon)?;
        Ok(ParsedStatement::Expression(expr))
      }
    }
  }

  fn parse_expression(&mut self) -> Result<ParsedExpression> {
    trace!("parse_expression: {:?}", self.current());
    let mut expr_stack = vec![];
    let mut last_prec = 1000000;

    let lhs = self.parse_operand()?;
    expr_stack.push(lhs);

    while self.pos < self.tokens.len() {
      if self.current().kind == TokenKind::Semicolon {
        break;
      }

      let op = self.parse_operator();
      if let Err(_) = op {
        break;
      }
      let op = op.unwrap();
      let prec = op.precedence();

      if self.pos >= self.tokens.len() {
        return Err(Error::new(
          self.tokens.get(self.pos - 1).unwrap().span,
          "incomplete math expression".into(),
        ));
      }

      let rhs = self.parse_operand()?;

      while prec <= last_prec && expr_stack.len() > 1 {
        let rhs = expr_stack
          .pop()
          .expect("internal error: expr_stack is empty");
        let op = expr_stack
          .pop()
          .expect("internal error: expr_stack is empty");

        last_prec = op.precedence();
        if last_prec < prec {
          expr_stack.push(op);
          expr_stack.push(rhs);
          break;
        }

        let lhs = expr_stack
          .pop()
          .expect("internal error: expr_stack is empty");

        match op {
          ParsedExpression::Operator(op, _) => {
            let span = Span::new(lhs.span().start, rhs.span().end);
            expr_stack.push(ParsedExpression::BinaryOp(
              Box::new(lhs),
              op,
              Box::new(rhs),
              span,
            ));
          }
          _ => panic!("operator is not an operator"),
        }
      }

      expr_stack.push(op);
      expr_stack.push(rhs);

      last_prec = prec;
    }

    while expr_stack.len() != 1 {
      let rhs = expr_stack
        .pop()
        .expect("internal error: expression stack empty");
      let op = expr_stack
        .pop()
        .expect("internal error: expression stack empty");
      let lhs = expr_stack
        .pop()
        .expect("internal error: expression stack empty");

      match op {
        ParsedExpression::Operator(op, _) => {
          let span = Span::new(lhs.span().start, rhs.span().end);
          expr_stack.push(ParsedExpression::BinaryOp(
            Box::new(lhs),
            op,
            Box::new(rhs),
            span,
          ));
        }
        _ => panic!("internal error: operator is not an operator"),
      }
    }

    let output = expr_stack
      .pop()
      .expect("internal error: expression stack empty");

    Ok(output)
  }

  fn parse_operand(&mut self) -> Result<ParsedExpression> {
    trace!("parse_operand: {:?}", self.current());
    let name = self.current().literal.clone();
    let span = self.current().span.clone();

    let mut expr = match self.current().kind {
      TokenKind::Integer => {
        self.pos += 1;
        ParsedExpression::NumericConstant(
          match name.parse::<i128>() {
            Ok(value) => NumericConstant::I128(value),
            Err(_) => return Err(Error::new(span, "integer is too big".into())),
          },
          span,
        )
      }

      TokenKind::Float => {
        self.pos += 1;
        ParsedExpression::NumericConstant(
          match name.parse::<f64>() {
            Ok(value) => NumericConstant::F64(value),
            Err(_) => return Err(Error::new(span, "float is too big".into())),
          },
          span,
        )
      }

      TokenKind::String => {
        self.pos += 1;
        ParsedExpression::QuotedString(name, span)
      }

      TokenKind::Identifier => {
        if self.pos + 1 < self.tokens.len() {
          match self.tokens.get(self.pos + 1).unwrap().kind {
            TokenKind::OpenParen => {
              let call = self.parse_call()?;
              ParsedExpression::Call(call, span)
            }
            TokenKind::Less => {
              let call = self.parse_call();

              if call.is_err() {
                ParsedExpression::Var(name, span)
              } else {
                ParsedExpression::Call(call.unwrap(), span)
              }
            }
            _ => {
              self.pos += 1;
              ParsedExpression::Var(name, span)
            }
          }
        } else {
          self.pos += 1;
          ParsedExpression::Var(name, span)
        }
      }
      TokenKind::KwTodo => {
        self.pos += 1;
        ParsedExpression::Todo(span)
      }
      TokenKind::KwUnreachable => {
        self.pos += 1;
        ParsedExpression::Unreachable(span)
      }
      _ => {
        return Err(Error::new(
          span,
          format!("expected expression but found {}", self.current().kind),
        ))
      }
    };

    while self.pos < self.tokens.len() {
      match self.current().kind {
        TokenKind::Period => {
          self.expect(TokenKind::Period)?;

          match self.current().kind {
            TokenKind::Identifier => {
              let name = self.current().literal.clone();
              self.expect(TokenKind::Identifier)?;
              if self.pos < self.tokens.len() {
                match self.tokens.get(self.pos).unwrap().kind {
                  TokenKind::OpenParen => {
                    self.pos -= 1;

                    let span = Span {
                      start: expr.span().start,
                      end: self.current().span.end,
                    };

                    let method = self.parse_call()?;
                    expr = ParsedExpression::MethodCall(Box::new(expr), method, span)
                  }
                  TokenKind::Less => {
                    self.pos -= 1;

                    let call = self.parse_call();
                    let span = Span {
                      start: expr.span().start,
                      end: self.current().span.end,
                    };

                    expr = if call.is_err() {
                      ParsedExpression::Var(name, span)
                    } else {
                      ParsedExpression::MethodCall(Box::new(expr), call.unwrap(), span)
                    };
                  }
                  _ => {
                    let span = Span {
                      start: expr.span().start,
                      end: self.tokens.get(self.pos - 1).unwrap().span.end,
                    };

                    expr = ParsedExpression::IndexedStruct(Box::new(expr), name.to_string(), span);
                  }
                }
              } else {
                let span = Span {
                  start: expr.span().start,
                  end: self.current().span.end,
                };

                expr = ParsedExpression::IndexedStruct(Box::new(expr), name.to_string(), span);
              }
            }

            _ => {
              self.pos += 1;
              return Err(Error::new(
                self.current().span,
                "unsupported dot operation".into(),
              ));
            }
          }
        }

        TokenKind::OpenBracket => {
          self.pos += 1;

          if self.pos < self.tokens.len() {
            let idx = self.parse_expression()?;

            let end;
            if self.pos < self.tokens.len() {
              end = self.pos;
              match &self.current().kind {
                TokenKind::CloseBracket => {
                  self.pos += 1;
                }
                _ => {
                  return Err(Error::new(self.current().span, "expected ']'".to_string()));
                }
              }
            } else {
              return Err(Error::new(
                self.tokens.get(self.pos - 1).unwrap().span,
                "expected ']'".to_string(),
              ));
            }

            expr = ParsedExpression::IndexedExpression(
              Box::new(expr),
              Box::new(idx),
              Span {
                start: span.start,
                end,
              },
            );
          }
        }

        _ => break,
      }
    }

    Ok(expr)
  }

  fn parse_operator(&mut self) -> Result<ParsedExpression> {
    trace!("parse_operator: {:?}", self.current());
    let span = self.current().span;

    match self.current().kind {
      TokenKind::Plus => {
        self.pos += 1;
        Ok(ParsedExpression::Operator(BinaryOperator::Add, span))
      }
      TokenKind::Minus => {
        self.pos += 1;
        Ok(ParsedExpression::Operator(BinaryOperator::Subtract, span))
      }
      TokenKind::Asterisk => {
        self.pos += 1;
        Ok(ParsedExpression::Operator(BinaryOperator::Multiply, span))
      }
      TokenKind::Slash => {
        self.pos += 1;
        Ok(ParsedExpression::Operator(BinaryOperator::Divide, span))
      }
      TokenKind::Percent => {
        self.pos += 1;
        Ok(ParsedExpression::Operator(BinaryOperator::Modulo, span))
      }
      TokenKind::Equal => {
        self.pos += 1;
        Ok(ParsedExpression::Operator(BinaryOperator::Modulo, span))
      }
      TokenKind::BangEqual => {
        self.pos += 1;
        Ok(ParsedExpression::Operator(BinaryOperator::NotEquals, span))
      }
      TokenKind::Less => {
        self.pos += 1;
        Ok(ParsedExpression::Operator(BinaryOperator::LessThan, span))
      }
      TokenKind::LessEqual => {
        self.pos += 1;
        Ok(ParsedExpression::Operator(
          BinaryOperator::LessThanEquals,
          span,
        ))
      }
      TokenKind::Greater => {
        self.pos += 1;
        Ok(ParsedExpression::Operator(
          BinaryOperator::GreaterThan,
          span,
        ))
      }
      TokenKind::GreaterEqual => {
        self.pos += 1;
        Ok(ParsedExpression::Operator(
          BinaryOperator::GreaterThanEquals,
          span,
        ))
      }

      TokenKind::PlusEqual => {
        self.pos += 1;
        Ok(ParsedExpression::Operator(BinaryOperator::AddAssign, span))
      }
      TokenKind::MinusEqual => {
        self.pos += 1;
        Ok(ParsedExpression::Operator(
          BinaryOperator::SubtractAssign,
          span,
        ))
      }
      TokenKind::AsteriskEqual => {
        self.pos += 1;
        Ok(ParsedExpression::Operator(
          BinaryOperator::MultiplyAssign,
          span,
        ))
      }
      TokenKind::SlashEqual => {
        self.pos += 1;
        Ok(ParsedExpression::Operator(
          BinaryOperator::DivideAssign,
          span,
        ))
      }
      TokenKind::PercentEqual => {
        self.pos += 1;
        Ok(ParsedExpression::Operator(
          BinaryOperator::ModuloAssign,
          span,
        ))
      }

      _ => {
        return Err(Error::new(
          span,
          format!("illegal operator `{}`", self.current().kind),
        ))
      }
    }
  }

  fn parse_call(&mut self) -> Result<ParsedCall> {
    trace!("parse_call: {:?}", self.current());
    let name_token = self.expect(TokenKind::Identifier)?;
    let type_args = if self.current().kind == TokenKind::Less {
      let mut type_args = vec![];
      self.expect(TokenKind::Less)?;
      while self.pos < self.tokens.len() && self.current().kind != TokenKind::Greater {
        let type_arg = self.parse_type()?;
        type_args.push(type_arg);
        if self.current().kind == TokenKind::Comma {
          self.expect(TokenKind::Comma)?;
        } else {
          break;
        }
      }
      self.expect(TokenKind::Greater)?;
      type_args
    } else {
      vec![]
    };

    let mut args = vec![];
    self.expect(TokenKind::OpenParen)?;
    while self.pos < self.tokens.len() && self.current().kind != TokenKind::CloseParen {
      let argument = self.parse_expression()?;
      args.push(argument);
      if self.current().kind == TokenKind::Comma {
        self.expect(TokenKind::Comma)?;
      } else {
        break;
      }
    }
    self.expect(TokenKind::CloseParen)?;

    Ok(ParsedCall {
      namespace: vec![],
      name: name_token.literal,
      type_args,
      args,
    })
  }

  fn parse_type(&mut self) -> Result<ParsedType> {
    trace!("parse_type: {:?}", self.current());
    let mut type_ = self.parse_type_primary()?;
    while matches!(
      self.current().kind,
      TokenKind::Asterisk | TokenKind::Question
    ) {
      let token = self.expect(self.current().kind.clone())?;
      match token.kind {
        TokenKind::Asterisk => {
          type_ = ParsedType::RawPointer(Box::new(type_), token.span);
        }
        TokenKind::Question => {
          type_ = ParsedType::Optional(Box::new(type_), token.span);
        }
        _ => unreachable!(),
      }
    }
    Ok(type_)
  }

  fn parse_type_primary(&mut self) -> Result<ParsedType> {
    trace!("parse_type_primary: {:?}", self.current());
    let token = self.expect(TokenKind::Identifier)?.clone();
    match token.literal.as_str() {
      "i8" => Ok(ParsedType::Name("i8".to_string(), token.span)),
      "i16" => Ok(ParsedType::Name("i16".to_string(), token.span)),
      "i32" => Ok(ParsedType::Name("i32".to_string(), token.span)),
      "i64" => Ok(ParsedType::Name("i64".to_string(), token.span)),
      "isz" => Ok(ParsedType::Name("isz".to_string(), token.span)),
      "u8" => Ok(ParsedType::Name("u8".to_string(), token.span)),
      "u16" => Ok(ParsedType::Name("u16".to_string(), token.span)),
      "u32" => Ok(ParsedType::Name("u32".to_string(), token.span)),
      "u64" => Ok(ParsedType::Name("u64".to_string(), token.span)),
      "usz" => Ok(ParsedType::Name("usz".to_string(), token.span)),
      "f32" => Ok(ParsedType::Name("f32".to_string(), token.span)),
      "f64" => Ok(ParsedType::Name("f64".to_string(), token.span)),
      "bool" => Ok(ParsedType::Name("bool".to_string(), token.span)),
      "string" => Ok(ParsedType::Name("string".to_string(), token.span)),
      "void" => Ok(ParsedType::Name("void".to_string(), token.span)),
      "any" => Ok(ParsedType::Name("any".to_string(), token.span)),
      "never" => Ok(ParsedType::Name("never".to_string(), token.span)),
      _ => {
        let mut type_args = vec![];
        if self.current().kind == TokenKind::Less {
          self.expect(TokenKind::Less)?;
          while self.current().kind != TokenKind::Greater {
            type_args.push(self.parse_type()?);
            if self.current().kind == TokenKind::Comma {
              self.expect(TokenKind::Comma)?;
            }
          }
          self.expect(TokenKind::Greater)?;
          Ok(ParsedType::GenericType(
            token.literal.clone(),
            type_args,
            token.span,
          ))
        } else {
          Ok(ParsedType::Name(token.literal.clone(), token.span))
        }
      }
    }
  }

  fn expect(&mut self, kind: TokenKind) -> Result<Token> {
    trace!("expect: {:?}, {:?}", self.current(), kind);
    if let Some(current) = self.tokens.get(self.pos) {
      if current.kind == kind {
        self.pos += 1;
        return Ok(current.clone()); // Return a cloned token
      }
      return Err(Error::new(
        current.span,
        format!("expected `{}`, found `{}`", kind, current.kind),
      ));
    }
    Err(Error::new(
      Span::default(),
      "unexpected end of input".to_string(),
    ))
  }

  fn current(&self) -> &Token {
    self.tokens.get(self.pos).unwrap()
  }
}
