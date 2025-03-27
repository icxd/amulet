#![allow(dead_code)]

use std::ffi::c_char;

use crate::{
  checker::TypeId,
  compiler::{
    BOOL_TYPE_ID, CCHAR_TYPE_ID, F32_TYPE_ID, F64_TYPE_ID, I128_TYPE_ID, I16_TYPE_ID, I32_TYPE_ID,
    I64_TYPE_ID, I8_TYPE_ID, ISZ_TYPE_ID, U128_TYPE_ID, U16_TYPE_ID, U32_TYPE_ID, U64_TYPE_ID,
    U8_TYPE_ID, UNKNOWN_TYPE_ID, USZ_TYPE_ID,
  },
  span::Span,
};

#[derive(Clone, Debug)]
pub enum IntegerConstant {
  Signed(i128),
  Unsigned(u128),
}

pub fn is_integer(type_id: TypeId) -> bool {
  matches!(
    type_id,
    I8_TYPE_ID
      | I16_TYPE_ID
      | I32_TYPE_ID
      | I64_TYPE_ID
      | I128_TYPE_ID
      | ISZ_TYPE_ID
      | U8_TYPE_ID
      | U16_TYPE_ID
      | U32_TYPE_ID
      | U64_TYPE_ID
      | U128_TYPE_ID
      | USZ_TYPE_ID
      | CCHAR_TYPE_ID
  )
}

pub fn is_signed(type_id: TypeId) -> bool {
  match type_id {
    U8_TYPE_ID | U16_TYPE_ID | U32_TYPE_ID | U64_TYPE_ID | U128_TYPE_ID | USZ_TYPE_ID => false,
    _ => true,
  }
}

pub fn flip_signedness(type_id: TypeId) -> Option<TypeId> {
  match type_id {
    I8_TYPE_ID => Some(U8_TYPE_ID),
    I16_TYPE_ID => Some(U16_TYPE_ID),
    I32_TYPE_ID => Some(U32_TYPE_ID),
    I64_TYPE_ID => Some(U64_TYPE_ID),
    I128_TYPE_ID => Some(U128_TYPE_ID),
    ISZ_TYPE_ID => Some(USZ_TYPE_ID),
    U8_TYPE_ID => Some(I8_TYPE_ID),
    U16_TYPE_ID => Some(I16_TYPE_ID),
    U32_TYPE_ID => Some(I32_TYPE_ID),
    U64_TYPE_ID => Some(I64_TYPE_ID),
    U128_TYPE_ID => Some(I128_TYPE_ID),
    USZ_TYPE_ID => Some(ISZ_TYPE_ID),
    _ => None,
  }
}

pub fn get_bits(type_id: TypeId) -> u32 {
  match type_id {
    BOOL_TYPE_ID => 8,
    I8_TYPE_ID => i8::BITS,
    I16_TYPE_ID => i16::BITS,
    I32_TYPE_ID => i32::BITS,
    I64_TYPE_ID => i64::BITS,
    I128_TYPE_ID => i128::BITS,
    ISZ_TYPE_ID => isize::BITS,
    U8_TYPE_ID => u8::BITS,
    U16_TYPE_ID => u16::BITS,
    U32_TYPE_ID => u32::BITS,
    U64_TYPE_ID => u64::BITS,
    U128_TYPE_ID => u128::BITS,
    USZ_TYPE_ID => usize::BITS,
    F32_TYPE_ID => 32,
    F64_TYPE_ID => 64,
    CCHAR_TYPE_ID => c_char::BITS,
    _ => panic!("get_bits not supported for type {}", type_id),
  }
}

pub fn can_fit_integer(type_id: TypeId, value: &IntegerConstant) -> bool {
  match *value {
    IntegerConstant::Signed(value) => match type_id {
      I8_TYPE_ID => value >= i8::MIN as i128 && value <= i8::MAX as i128,
      I16_TYPE_ID => value >= i16::MIN as i128 && value <= i16::MAX as i128,
      I32_TYPE_ID => value >= i32::MIN as i128 && value <= i32::MAX as i128,
      I64_TYPE_ID => value >= i64::MIN as i128 && value <= i64::MAX as i128,
      I128_TYPE_ID => true,
      ISZ_TYPE_ID => true,
      U8_TYPE_ID => value >= 0 && value <= u8::MAX as i128,
      U16_TYPE_ID => value >= 0 && value <= u16::MAX as i128,
      U32_TYPE_ID => value >= 0 && value <= u32::MAX as i128,
      U64_TYPE_ID => value >= 0 && value <= u64::MAX as i128,
      U128_TYPE_ID => value >= 0,
      USZ_TYPE_ID => value >= 0,
      CCHAR_TYPE_ID => value >= 0 && value <= c_char::MAX as i128,
      _ => false,
    },
    IntegerConstant::Unsigned(value) => match type_id {
      I8_TYPE_ID => value <= i8::MAX as u128,
      I16_TYPE_ID => value <= i16::MAX as u128,
      I32_TYPE_ID => value <= i32::MAX as u128,
      I64_TYPE_ID => value <= i64::MAX as u128,
      I128_TYPE_ID => true,
      ISZ_TYPE_ID => true,
      U8_TYPE_ID => value <= u8::MAX as u128,
      U16_TYPE_ID => value <= u16::MAX as u128,
      U32_TYPE_ID => value <= u32::MAX as u128,
      U64_TYPE_ID => value <= u64::MAX as u128,
      U128_TYPE_ID => true,
      USZ_TYPE_ID => true,
      CCHAR_TYPE_ID => value <= c_char::MAX as u128,
      _ => false,
    },
  }
}

impl IntegerConstant {
  pub fn to_usize(&self) -> usize {
    match self {
      IntegerConstant::Signed(value) => *value as usize,
      IntegerConstant::Unsigned(value) => *value as usize,
    }
  }

  pub fn promote(&self, type_id: TypeId) -> (Option<NumericConstant>, TypeId) {
    if !can_fit_integer(type_id, self) {
      return (None, UNKNOWN_TYPE_ID);
    }

    let bits = get_bits(type_id);
    let signed = is_signed(type_id);

    let new_constant = match self {
      IntegerConstant::Signed(value) => match (bits, signed) {
        (8, false) => NumericConstant::U8(*value as u8),
        (16, false) => NumericConstant::U16(*value as u16),
        (32, false) => NumericConstant::U32(*value as u32),
        (64, false) => NumericConstant::U64(*value as u64),
        (128, false) => NumericConstant::U128(*value as u128),
        (8, true) => NumericConstant::I8(*value as i8),
        (16, true) => NumericConstant::I16(*value as i16),
        (32, true) => NumericConstant::I32(*value as i32),
        (64, true) => NumericConstant::I64(*value as i64),
        (128, true) => NumericConstant::I128(*value as i128),
        _ => panic!("Numeric constants can only be 8, 16, 32, 64, or 128 bits long"),
      },
      IntegerConstant::Unsigned(value) => match (bits, signed) {
        (8, false) => NumericConstant::U8(*value as u8),
        (16, false) => NumericConstant::U16(*value as u16),
        (32, false) => NumericConstant::U32(*value as u32),
        (64, false) => NumericConstant::U64(*value as u64),
        (128, false) => NumericConstant::U128(*value as u128),
        (8, true) => NumericConstant::I8(*value as i8),
        (16, true) => NumericConstant::I16(*value as i16),
        (32, true) => NumericConstant::I32(*value as i32),
        (64, true) => NumericConstant::I64(*value as i64),
        (128, true) => NumericConstant::I128(*value as i128),
        _ => panic!("Numeric constants can only be 8, 16, 32, 64, or 128 bits long"),
      },
    };
    (Some(new_constant), type_id)
  }
}

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
  pub fn integer_constant(&self) -> Option<IntegerConstant> {
    match self {
      NumericConstant::I8(value) => Some(IntegerConstant::Signed(*value as i128)),
      NumericConstant::I16(value) => Some(IntegerConstant::Signed(*value as i128)),
      NumericConstant::I32(value) => Some(IntegerConstant::Signed(*value as i128)),
      NumericConstant::I64(value) => Some(IntegerConstant::Signed(*value as i128)),
      NumericConstant::I128(value) => Some(IntegerConstant::Signed(*value as i128)),
      NumericConstant::Isz(value) => Some(IntegerConstant::Signed(*value as i128)),
      NumericConstant::U8(value) => Some(IntegerConstant::Unsigned(*value as u128)),
      NumericConstant::U16(value) => Some(IntegerConstant::Unsigned(*value as u128)),
      NumericConstant::U32(value) => Some(IntegerConstant::Unsigned(*value as u128)),
      NumericConstant::U64(value) => Some(IntegerConstant::Unsigned(*value as u128)),
      NumericConstant::U128(value) => Some(IntegerConstant::Unsigned(*value as u128)),
      NumericConstant::Usz(value) => Some(IntegerConstant::Unsigned(*value as u128)),
      _ => None,
    }
  }

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
  pub(crate) value: ParsedExpression,
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
  CallConv(String, Span),
  NoReturn(Span),
}

#[derive(Debug)]
pub struct ParsedFunction {
  pub(crate) name: String,
  pub(crate) name_span: Span,
  pub(crate) linkage: DefinitionLinkage,
  pub(crate) attributes: Vec<ParsedFunctionAttribute>,
  pub(crate) type_parameters: Vec<ParsedTypeArg>,
  pub(crate) parameters: Vec<ParsedVarDecl>,
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
  Dereference,
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
