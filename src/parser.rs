use log::trace;

use crate::{
  ast::{
    BinaryOperator, DefinitionLinkage, NumericConstant, ParsedBlock, ParsedCall, ParsedConst,
    ParsedExpression, ParsedFunction, ParsedFunctionAttribute, ParsedNamespace, ParsedStatement,
    ParsedType, ParsedTypeArg, ParsedTypeDecl, ParsedTypeDeclData, ParsedVarDecl, UnaryOperator,
  },
  compiler::FileId,
  error::{Error, Result},
  span::Span,
  tokenizer::{Token, TokenKind},
};

#[derive(Debug, Clone)]
pub struct Parser {
  file_id: FileId,
  tokens: Vec<Token>,
  pos: usize,
}

impl Parser {
  pub fn new(file_id: FileId, tokens: Vec<Token>) -> Self {
    Self {
      file_id,
      tokens,
      pos: 0,
    }
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
        TokenKind::KwFn | TokenKind::KwNative => {
          let linkage = if token.kind == TokenKind::KwFn {
            DefinitionLinkage::Internal
          } else {
            self.expect(TokenKind::KwNative)?;
            DefinitionLinkage::External
          };
          let fun = self.parse_fn(linkage)?;
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

        TokenKind::KwConst => {
          self.expect(TokenKind::KwConst)?;
          let name = self.expect(TokenKind::Identifier)?;
          let name_span = name.span;
          self.expect(TokenKind::Colon)?;
          let r#type = self.parse_type()?;
          self.expect(TokenKind::Equal)?;
          let value = self.parse_expression(false)?;
          self.expect(TokenKind::Semicolon)?;
          namespace.constants.push(ParsedConst {
            name: name.literal.clone(),
            name_span,
            linkage: DefinitionLinkage::Internal,
            r#type,
            value,
          });
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
      linkage: DefinitionLinkage::Internal,
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
          let fun = self.parse_fn(DefinitionLinkage::Internal)?;
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
            TokenKind::KwMut | TokenKind::KwLet => {
              let mutable = self.current().kind == TokenKind::KwMut;
              self.pos += 1;
              let name = self.expect(TokenKind::Identifier)?;
              let _ = self.expect(TokenKind::Colon)?;
              let r#type = self.parse_type()?;
              let value = if self.current().kind == TokenKind::Equal {
                self.expect(TokenKind::Equal)?;
                Some(self.parse_expression(false)?)
              } else {
                None
              };
              self.expect(TokenKind::Semicolon)?;
              fields.push((
                ParsedVarDecl {
                  mutable,
                  name: name.literal.clone(),
                  span: name.span,
                  ty: r#type,
                },
                value,
              ));
            }

            TokenKind::KwFn => {
              let fun = self.parse_fn(DefinitionLinkage::Internal)?;
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
        let alias = self.parse_type()?;
        Ok(ParsedTypeDeclData::Alias(alias))
      }
    }
  }

  fn parse_fn(&mut self, linkage: DefinitionLinkage) -> Result<ParsedFunction> {
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
    let attributes = self.parse_function_attributes()?;
    let body = if linkage == DefinitionLinkage::External {
      None
    } else if self.current().kind == TokenKind::OpenBrace {
      Some(self.parse_block()?)
    } else if self.current().kind == TokenKind::Equal {
      let return_token = self.current().clone();
      self.expect(TokenKind::Equal)?;
      let expr = self.parse_expression(false)?;
      let semi = self.expect(TokenKind::Semicolon)?;
      Some(ParsedBlock {
        stmts: vec![ParsedStatement::Return(
          expr,
          return_token.span.join(&semi.span),
        )],
        span: return_token.span.join(&semi.span),
      })
    } else if self.current().kind == TokenKind::Semicolon {
      let span = self.expect(TokenKind::Semicolon)?.span;
      Some(ParsedBlock {
        stmts: vec![],
        span,
      })
    } else {
      None
    };
    Ok(ParsedFunction {
      name,
      name_span,
      linkage,
      attributes,
      type_parameters: type_params,
      parameters,
      return_type,
      body,
    })
  }

  fn parse_function_attributes(&mut self) -> Result<Vec<ParsedFunctionAttribute>> {
    trace!("parse_function_attributes: {:?}", self.current());
    let mut attributes = vec![];
    while self.current().kind == TokenKind::KwCallConv
      || self.current().kind == TokenKind::KwNoReturn
    {
      let attribute = if self.current().kind == TokenKind::KwCallConv {
        self.expect(TokenKind::KwCallConv)?;
        self.expect(TokenKind::OpenParen)?;
        let conv = self.expect(TokenKind::Identifier)?;
        self.expect(TokenKind::CloseParen)?;
        ParsedFunctionAttribute::CallConv(conv.literal)
      } else {
        self.expect(TokenKind::KwNoReturn)?;
        ParsedFunctionAttribute::NoReturn
      };
      attributes.push(attribute);
    }
    Ok(attributes)
  }

  fn parse_parameters(&mut self) -> Result<Vec<ParsedVarDecl>> {
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
    let obrace = self.expect(TokenKind::OpenBrace)?;
    let mut stmts = vec![];
    while self.current().kind != TokenKind::CloseBrace {
      let stmt = self.parse_statement()?;
      stmts.push(stmt);
    }
    let cbrace = self.expect(TokenKind::CloseBrace)?;
    Ok(ParsedBlock {
      stmts,
      span: obrace.span.join(&cbrace.span),
    })
  }

  fn parse_variable(&mut self) -> Result<ParsedVarDecl> {
    trace!("parse_variable: {:?}", self.current());
    let mutable = if self.current().kind == TokenKind::KwMut {
      self.expect(TokenKind::KwMut)?;
      true
    } else {
      false
    };
    if self.current().kind == TokenKind::Identifier && self.current().literal == "self" {
      let id = self.expect(TokenKind::Identifier)?;
      return Ok(ParsedVarDecl {
        mutable,
        name: "self".to_string(),
        ty: ParsedType::Name("Self".to_string(), self.current().span),
        span: id.span,
      });
    }
    let name = self.expect(TokenKind::Identifier)?;
    let _ = self.expect(TokenKind::Colon)?;
    let r#type = self.parse_type()?;
    Ok(ParsedVarDecl {
      mutable,
      name: name.literal.clone(),
      ty: r#type,
      span: name.span,
    })
  }

  fn parse_type_params(&mut self) -> Result<Vec<ParsedTypeArg>> {
    trace!("parse_type_params: {:?}", self.current());
    let mut type_params = vec![];
    if self.current().kind == TokenKind::Less {
      self.expect(TokenKind::Less)?;
      while self.current().kind != TokenKind::Greater {
        let name = self.expect(TokenKind::Identifier)?;
        let constraint = if self.current().kind == TokenKind::Colon {
          self.expect(TokenKind::Colon)?;
          Some(self.parse_type()?)
        } else {
          None
        };
        type_params.push(ParsedTypeArg {
          name: name.literal.clone(),
          constraint,
          span: name.span,
        });
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
        let r#return = self.expect(TokenKind::KwReturn)?;
        let expr = self.parse_expression(false)?;
        let semi = self.expect(TokenKind::Semicolon)?;
        Ok(ParsedStatement::Return(
          expr,
          r#return.span.join(&semi.span),
        ))
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
        let value = self.parse_expression(false)?;
        self.expect(TokenKind::Semicolon)?;
        Ok(ParsedStatement::VarDecl(
          ParsedVarDecl {
            name,
            ty,
            mutable,
            span,
          },
          value,
          span,
        ))
      }

      TokenKind::KwIf => {
        let span = self.expect(TokenKind::KwIf)?.span;
        self.expect(TokenKind::OpenParen)?;
        let condition = self.parse_expression(false)?;
        self.expect(TokenKind::CloseParen)?;
        let then = self.parse_block()?;
        let mut else_ = None;
        if self.current().kind == TokenKind::KwElse {
          self.pos += 1;
          else_ = Some(self.parse_block()?);
        }
        let mut span = span.join(&then.span);
        if let Some(ref else_) = else_ {
          span = span.join(&else_.span);
        }
        Ok(ParsedStatement::If(
          condition,
          then.clone(),
          else_.clone().map(Box::new),
          span,
        ))
      }

      TokenKind::KwWhile => {
        let span = self.expect(TokenKind::KwWhile)?.span;
        let condition = self.parse_expression(false)?;
        let body = self.parse_block()?;
        Ok(ParsedStatement::While(
          condition,
          body.clone(),
          span.join(&body.span),
        ))
      }

      TokenKind::KwLoop => {
        let span = self.expect(TokenKind::KwLoop)?.span;
        let body = self.parse_block()?;
        Ok(ParsedStatement::Loop(body.clone(), span.join(&body.span)))
      }

      TokenKind::KwAsm => {
        let span = self.expect(TokenKind::KwAsm)?.span;
        self.expect(TokenKind::OpenBrace)?;
        let mut asm = vec![];
        while self.current().kind != TokenKind::CloseBrace {
          let line = self.expect(TokenKind::String)?;
          asm.push(line.literal);
        }
        self.expect(TokenKind::CloseBrace)?;
        Ok(ParsedStatement::InlineAsm(asm, span))
      }

      TokenKind::KwBreak => {
        let span = self.expect(TokenKind::KwBreak)?.span;
        self.expect(TokenKind::Semicolon)?;
        Ok(ParsedStatement::Break(span))
      }
      TokenKind::KwContinue => {
        let span = self.expect(TokenKind::KwContinue)?.span;
        self.expect(TokenKind::Semicolon)?;
        Ok(ParsedStatement::Continue(span))
      }

      TokenKind::OpenBrace => {
        let block = self.parse_block()?;
        Ok(ParsedStatement::Block(block))
      }

      _ => {
        let expr = self.parse_expression(true)?;
        self.expect(TokenKind::Semicolon)?;
        Ok(ParsedStatement::Expression(expr))
      }
    }
  }

  fn parse_expression(&mut self, assignable: bool) -> Result<ParsedExpression> {
    trace!("parse_expression: {:?}", self.current());
    let mut expr_stack = vec![];
    let mut last_prec = 1000000;

    let lhs = self.parse_operand()?;
    expr_stack.push(lhs);

    while self.pos < self.tokens.len() {
      if self.current().kind == TokenKind::Semicolon {
        break;
      }

      let op = self.parse_operator(assignable);
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
            let span = Span::new(self.file_id, lhs.span().start, rhs.span().end);
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
          let span = Span::new(self.file_id, lhs.span().start, rhs.span().end);
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
        let value = name.parse::<i128>().unwrap();

        let constant = if value < i8::MAX as i128 {
          if value < 0 {
            NumericConstant::I8(value as i8)
          } else {
            NumericConstant::U8(value as u8)
          }
        } else if value < i16::MAX as i128 {
          if value < 0 {
            NumericConstant::I16(value as i16)
          } else {
            NumericConstant::U16(value as u16)
          }
        } else if value < i32::MAX as i128 {
          if value < 0 {
            NumericConstant::I32(value as i32)
          } else {
            NumericConstant::U32(value as u32)
          }
        } else if value < i64::MAX as i128 {
          if value < 0 {
            NumericConstant::I64(value as i64)
          } else {
            NumericConstant::U64(value as u64)
          }
        } else {
          if value < 0 {
            NumericConstant::I128(value as i128)
          } else {
            NumericConstant::U128(value as u128)
          }
        };

        ParsedExpression::NumericConstant(constant, span)
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

      TokenKind::Char => {
        self.pos += 1;
        ParsedExpression::CharacterLiteral(name.chars().next().unwrap(), span)
      }

      TokenKind::Identifier if name == "c" => {
        self.pos += 1;
        let string = self.expect(TokenKind::String)?;
        ParsedExpression::QuotedCString(string.literal, string.span)
      }

      TokenKind::Identifier if name == "true" => {
        self.pos += 1;
        ParsedExpression::Boolean(true, span)
      }
      TokenKind::Identifier if name == "false" => {
        self.pos += 1;
        ParsedExpression::Boolean(false, span)
      }

      TokenKind::Identifier if name == "nullptr" => {
        self.pos += 1;
        ParsedExpression::Nullptr(span)
      }
      TokenKind::Identifier if name == "null" => {
        self.pos += 1;
        ParsedExpression::Null(span)
      }

      TokenKind::Identifier => {
        if self.pos + 1 < self.tokens.len() {
          match self.tokens.get(self.pos + 1).unwrap().kind {
            TokenKind::OpenParen => {
              let call = self.parse_call()?;
              ParsedExpression::Call(call, span)
            }
            TokenKind::Less => {
              let restore_pos = self.pos;
              let call = self.parse_call();

              if call.is_err() {
                self.pos = restore_pos + 1;
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
        TokenKind::KwAs => {
          self.expect(TokenKind::KwAs)?;
          let type_ = self.parse_type()?;
          let span = Span {
            file_id: self.file_id,
            start: expr.span().start,
            end: self.current().span.end,
          };
          expr = ParsedExpression::UnaryOp(Box::new(expr), UnaryOperator::As(type_), span);
        }

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
                      file_id: self.file_id,
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
                      file_id: self.file_id,
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
                      file_id: self.file_id,
                      start: expr.span().start,
                      end: self.tokens.get(self.pos - 1).unwrap().span.end,
                    };

                    expr = ParsedExpression::IndexedStruct(Box::new(expr), name.to_string(), span);
                  }
                }
              } else {
                let span = Span {
                  file_id: self.file_id,
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
            let idx = self.parse_expression(false)?;

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
                file_id: self.file_id,
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

  fn parse_operator(&mut self, assignable: bool) -> Result<ParsedExpression> {
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
      TokenKind::EqualEqual => {
        self.pos += 1;
        Ok(ParsedExpression::Operator(BinaryOperator::Equals, span))
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

      TokenKind::Equal => {
        if !assignable {
          return Err(Error::new(span, "assignment is not allowed here".into()));
        }

        self.pos += 1;
        Ok(ParsedExpression::Operator(BinaryOperator::Assign, span))
      }
      TokenKind::PlusEqual => {
        if !assignable {
          return Err(Error::new(span, "assignment is not allowed here".into()));
        }

        self.pos += 1;
        Ok(ParsedExpression::Operator(BinaryOperator::AddAssign, span))
      }
      TokenKind::MinusEqual => {
        if !assignable {
          return Err(Error::new(span, "assignment is not allowed here".into()));
        }

        self.pos += 1;
        Ok(ParsedExpression::Operator(
          BinaryOperator::SubtractAssign,
          span,
        ))
      }
      TokenKind::AsteriskEqual => {
        if !assignable {
          return Err(Error::new(span, "assignment is not allowed here".into()));
        }

        self.pos += 1;
        Ok(ParsedExpression::Operator(
          BinaryOperator::MultiplyAssign,
          span,
        ))
      }
      TokenKind::SlashEqual => {
        if !assignable {
          return Err(Error::new(span, "assignment is not allowed here".into()));
        }

        self.pos += 1;
        Ok(ParsedExpression::Operator(
          BinaryOperator::DivideAssign,
          span,
        ))
      }
      TokenKind::PercentEqual => {
        if !assignable {
          return Err(Error::new(span, "assignment is not allowed here".into()));
        }

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
      let argument = self.parse_expression(false)?;
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
      "i128" => Ok(ParsedType::Name("i128".to_string(), token.span)),
      "u8" => Ok(ParsedType::Name("u8".to_string(), token.span)),
      "u16" => Ok(ParsedType::Name("u16".to_string(), token.span)),
      "u32" => Ok(ParsedType::Name("u32".to_string(), token.span)),
      "u64" => Ok(ParsedType::Name("u64".to_string(), token.span)),
      "u128" => Ok(ParsedType::Name("u128".to_string(), token.span)),
      "usz" => Ok(ParsedType::Name("usz".to_string(), token.span)),
      "f32" => Ok(ParsedType::Name("f32".to_string(), token.span)),
      "f64" => Ok(ParsedType::Name("f64".to_string(), token.span)),
      "bool" => Ok(ParsedType::Name("bool".to_string(), token.span)),
      "string" => Ok(ParsedType::Name("string".to_string(), token.span)),
      "c_char" => Ok(ParsedType::Name("c_char".to_string(), token.span)),
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
