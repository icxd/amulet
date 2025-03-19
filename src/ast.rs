#![allow(dead_code)]

use crate::{
  compiler::{
    F32_TYPE_ID, F64_TYPE_ID, I128_TYPE_ID, I16_TYPE_ID, I32_TYPE_ID, I64_TYPE_ID, I8_TYPE_ID,
    ISZ_TYPE_ID, U128_TYPE_ID, U16_TYPE_ID, U32_TYPE_ID, U64_TYPE_ID, U8_TYPE_ID, USZ_TYPE_ID,
  },
  span::Span,
};

#[derive(Debug, Clone)]
pub enum NumericConstant {
  I8(i8),
  I16(i16),
  I32(i32),
  I64(i64),
  I128(i128),
  Isz(isize),
  U8(u8),
  U16(u16),
  U32(u32),
  U64(u64),
  U128(u128),
  Usz(usize),
  F32(f32),
  F64(f64),
}

impl NumericConstant {
  pub(crate) fn type_id(&self) -> usize {
    match self {
      NumericConstant::I8(_) => I8_TYPE_ID,
      NumericConstant::I16(_) => I16_TYPE_ID,
      NumericConstant::I32(_) => I32_TYPE_ID,
      NumericConstant::I64(_) => I64_TYPE_ID,
      NumericConstant::I128(_) => I128_TYPE_ID,
      NumericConstant::Isz(_) => ISZ_TYPE_ID,
      NumericConstant::U8(_) => U8_TYPE_ID,
      NumericConstant::U16(_) => U16_TYPE_ID,
      NumericConstant::U32(_) => U32_TYPE_ID,
      NumericConstant::U64(_) => U64_TYPE_ID,
      NumericConstant::U128(_) => U128_TYPE_ID,
      NumericConstant::Usz(_) => USZ_TYPE_ID,
      NumericConstant::F32(_) => F32_TYPE_ID,
      NumericConstant::F64(_) => F64_TYPE_ID,
    }
  }
}

#[derive(Debug)]
pub struct ParsedNamespace {
  pub(crate) name: Option<String>,
  pub(crate) functions: Vec<ParsedFunction>,
  pub(crate) type_decls: Vec<ParsedTypeDecl>,
  pub(crate) constants: Vec<ParsedConst>,
  pub(crate) namespaces: Vec<ParsedNamespace>,
}

impl ParsedNamespace {
  pub fn new() -> Self {
    Self {
      name: None,
      functions: vec![],
      type_decls: vec![],
      constants: vec![],
      namespaces: vec![],
    }
  }
}

#[derive(Debug)]
pub struct ParsedConst {
  pub(crate) name: String,
  pub(crate) name_span: Span,
  pub(crate) linkage: DefinitionLinkage,
  pub(crate) r#type: ParsedType,
  pub(crate) value: Option<ParsedExpression>,
}

#[derive(Debug, Clone)]
pub struct ParsedTypeArg {
  pub(crate) name: String,
  pub(crate) constraint: Option<ParsedType>,
  pub(crate) span: Span,
}

#[derive(Debug, Clone)]
pub struct ParsedCall {
  pub(crate) namespace: Vec<String>,
  pub(crate) name: String,
  pub(crate) type_args: Vec<ParsedType>,
  pub(crate) args: Vec<ParsedExpression>,
}

#[derive(Debug)]
pub struct ParsedVariable {
  pub(crate) mutable: bool,
  pub(crate) name: String,
  pub(crate) r#type: ParsedType,
}

#[derive(Debug, Clone)]
pub struct ParsedVarDecl {
  pub(crate) name: String,
  pub(crate) ty: ParsedType,
  pub(crate) mutable: bool,
  pub(crate) span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq)]
#[repr(u8)]
pub enum DefinitionLinkage {
  Internal,
  External,
  ImplicitConstructor,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParsedFunctionAttribute {
  CallConv(String),
  NoReturn,
}

#[derive(Debug)]
pub struct ParsedFunction {
  pub(crate) name: String,
  pub(crate) name_span: Span,
  pub(crate) linkage: DefinitionLinkage,
  pub(crate) attributes: Vec<ParsedFunctionAttribute>,
  pub(crate) type_parameters: Vec<ParsedTypeArg>,
  pub(crate) parameters: Vec<ParsedVariable>,
  pub(crate) return_type: Option<ParsedType>,
  pub(crate) body: Option<ParsedBlock>,
}

#[derive(Debug)]
pub struct ParsedTypeDecl {
  pub(crate) name: String,
  pub(crate) name_span: Span,
  pub(crate) linkage: DefinitionLinkage,
  pub(crate) type_parameters: Vec<ParsedTypeArg>,
  pub(crate) data: ParsedTypeDeclData,
}

#[derive(Debug)]
pub enum ParsedTypeDeclData {
  Interface(Vec<ParsedFunction>),
  Class {
    implements: Vec<ParsedType>,
    fields: Vec<(ParsedVarDecl, Option<ParsedExpression>)>,
    methods: Vec<ParsedFunction>,
  },
  Alias(ParsedType),
}

#[derive(Debug, Clone)]
pub struct ParsedBlock {
  pub(crate) stmts: Vec<ParsedStatement>,
  pub(crate) span: Span,
}

#[derive(Debug, Clone)]
pub enum ParsedStatement {
  VarDecl(ParsedVarDecl, ParsedExpression, Span),

  Block(ParsedBlock),
  If(
    ParsedExpression,
    ParsedBlock,
    Option<Box<ParsedBlock>>,
    Span,
  ),
  While(ParsedExpression, ParsedBlock, Span),
  Loop(ParsedBlock, Span),

  // FIXME: This currently doesn't support using variables in the assembly code.
  InlineAsm(Vec<String>, Span),

  Break(Span),
  Continue(Span),
  Return(ParsedExpression, Span),

  Expression(ParsedExpression),
}

#[derive(Debug, Clone)]
pub enum ParsedExpression {
  Null(Span),
  Nullptr(Span),
  Boolean(bool, Span),
  NumericConstant(NumericConstant, Span),
  QuotedCString(String, Span),
  QuotedString(String, Span),
  CharacterLiteral(char, Span),
  Var(String, Span),
  NamespacedVar(Vec<String>, String, Span),
  UnaryOp(Box<ParsedExpression>, UnaryOperator, Span),
  BinaryOp(
    Box<ParsedExpression>,
    BinaryOperator,
    Box<ParsedExpression>,
    Span,
  ),

  IndexedExpression(Box<ParsedExpression>, Box<ParsedExpression>, Span),
  IndexedStruct(Box<ParsedExpression>, String, Span),

  Call(ParsedCall, Span),
  MethodCall(Box<ParsedExpression>, ParsedCall, Span),

  Operator(BinaryOperator, Span),

  Todo(Span),
  Unreachable(Span),

  Garbage(Span),
}

#[derive(Debug, Clone)]
pub enum ParsedType {
  Name(String, Span),
  GenericType(String, Vec<ParsedType>, Span),
  RawPointer(Box<ParsedType>, Span),
  Optional(Box<ParsedType>, Span),
  Empty,
}

#[derive(Debug, Clone)]
pub enum BinaryOperator {
  Add,
  Subtract,
  Multiply,
  Divide,
  Modulo,
  Equals,
  NotEquals,
  LessThan,
  LessThanEquals,
  GreaterThan,
  GreaterThanEquals,

  Assign,
  AddAssign,
  SubtractAssign,
  MultiplyAssign,
  DivideAssign,
  ModuloAssign,
}

#[derive(Debug, Clone)]
pub enum UnaryOperator {
  As(ParsedType),
}

impl ParsedExpression {
  pub fn span(&self) -> Span {
    match self {
      ParsedExpression::Null(span) => *span,
      ParsedExpression::Nullptr(span) => *span,
      ParsedExpression::Boolean(_, span) => *span,
      ParsedExpression::NumericConstant(_, span) => *span,
      ParsedExpression::QuotedCString(_, span) => *span,
      ParsedExpression::QuotedString(_, span) => *span,
      ParsedExpression::CharacterLiteral(_, span) => *span,
      ParsedExpression::Var(_, span) => *span,
      ParsedExpression::NamespacedVar(_, _, span) => *span,
      ParsedExpression::UnaryOp(_, _, span) => *span,
      ParsedExpression::BinaryOp(_, _, _, span) => *span,
      ParsedExpression::IndexedExpression(_, _, span) => *span,
      ParsedExpression::IndexedStruct(_, _, span) => *span,
      ParsedExpression::Call(_, span) => *span,
      ParsedExpression::MethodCall(_, _, span) => *span,
      ParsedExpression::Operator(_, span) => *span,
      ParsedExpression::Todo(span) => *span,
      ParsedExpression::Unreachable(span) => *span,
      ParsedExpression::Garbage(span) => *span,
    }
  }
  pub fn precedence(&self) -> u64 {
    match self {
      ParsedExpression::Operator(BinaryOperator::Multiply, _)
      | ParsedExpression::Operator(BinaryOperator::Modulo, _)
      | ParsedExpression::Operator(BinaryOperator::Divide, _) => 100,
      ParsedExpression::Operator(BinaryOperator::Add, _)
      | ParsedExpression::Operator(BinaryOperator::Subtract, _) => 90,
      // ParsedExpression::Operator(BinaryOperator::BitwiseLeftShift, _)
      // | ParsedExpression::Operator(BinaryOperator::BitwiseRightShift, _)
      // | ParsedExpression::Operator(BinaryOperator::ArithmeticLeftShift, _)
      // | ParsedExpression::Operator(BinaryOperator::ArithmeticRightShift, _) => 85,
      ParsedExpression::Operator(BinaryOperator::LessThan, _)
      | ParsedExpression::Operator(BinaryOperator::LessThanEquals, _)
      | ParsedExpression::Operator(BinaryOperator::GreaterThan, _)
      | ParsedExpression::Operator(BinaryOperator::GreaterThanEquals, _)
      | ParsedExpression::Operator(BinaryOperator::Equals, _)
      | ParsedExpression::Operator(BinaryOperator::NotEquals, _) => 80,
      // ParsedExpression::Operator(BinaryOperator::BitwiseAnd, _) => 73,
      // ParsedExpression::Operator(BinaryOperator::BitwiseXor, _) => 72,
      // ParsedExpression::Operator(BinaryOperator::BitwiseOr, _) => 71,
      // ParsedExpression::Operator(BinaryOperator::LogicalAnd, _) => 70,
      // ParsedExpression::Operator(BinaryOperator::LogicalOr, _)
      // | ParsedExpression::Operator(BinaryOperator::NoneCoalescing, _) => 69,
      ParsedExpression::Operator(BinaryOperator::Assign, _)
      // | ParsedExpression::Operator(BinaryOperator::BitwiseAndAssign, _)
      // | ParsedExpression::Operator(BinaryOperator::BitwiseOrAssign, _)
      // | ParsedExpression::Operator(BinaryOperator::BitwiseXorAssign, _)
      // | ParsedExpression::Operator(BinaryOperator::BitwiseLeftShiftAssign, _)
      // | ParsedExpression::Operator(BinaryOperator::BitwiseRightShiftAssign, _)
      | ParsedExpression::Operator(BinaryOperator::AddAssign, _)
      | ParsedExpression::Operator(BinaryOperator::SubtractAssign, _)
      | ParsedExpression::Operator(BinaryOperator::MultiplyAssign, _)
      | ParsedExpression::Operator(BinaryOperator::ModuloAssign, _)
      | ParsedExpression::Operator(BinaryOperator::DivideAssign, _) => 50,
      _ => 0,
    }
  }
}
