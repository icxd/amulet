#![allow(dead_code)]

use std::collections::{HashMap, HashSet};

use crate::{
  ast::{
    inline_asm, BinaryOperator, DefinitionLinkage, IntegerConstant, NumericConstant, ParsedBlock,
    ParsedCall, ParsedEnum, ParsedEnumVariant, ParsedExpression, ParsedFunction,
    ParsedFunctionAttribute, ParsedNamespace, ParsedStatement, ParsedType, ParsedTypeDecl,
    ParsedTypeDeclData, UnaryOperator,
  },
  compiler::{BOOL_TYPE_ID, CCHAR_TYPE_ID, RAWPTR_TYPE_ID, UNKNOWN_TYPE_ID, VOID_TYPE_ID},
  error::Diagnostic,
  span::Span,
};

pub type TypeId = usize;
pub type ScopeId = usize;
pub type FunctionId = usize;
pub type TypeDeclId = usize;
pub type EnumId = usize;

#[derive(Debug)]
pub struct Scope {
  pub(crate) namespace_name: Option<String>,
  pub(crate) vars: Vec<CheckedVarDecl>,
  pub(crate) constants: Vec<CheckedConstant>,
  pub(crate) functions: Vec<(String, FunctionId)>,
  pub(crate) type_decls: Vec<(String, TypeDeclId)>,
  pub(crate) enums: Vec<(String, EnumId)>,
  pub(crate) types: Vec<(String, TypeId)>,
  pub(crate) parent: Option<ScopeId>,
  pub(crate) children: Vec<ScopeId>,
}

impl Scope {
  pub fn new(parent: Option<ScopeId>) -> Self {
    Self {
      namespace_name: None,
      vars: vec![],
      constants: vec![],
      functions: vec![],
      type_decls: vec![],
      enums: vec![],
      types: vec![],
      parent,
      children: vec![],
    }
  }

  pub fn can_access(own: ScopeId, other: ScopeId, project: &Project) -> bool {
    if own == other {
      true
    } else {
      let mut own_scope = &project.scopes[own];
      while let Some(parent) = own_scope.parent {
        if parent == other {
          return true;
        }
        own_scope = &project.scopes[parent];
      }
      false
    }
  }
}

#[derive(Debug, Clone)]
pub struct CheckedNamespace {
  pub(crate) name: Option<String>,
  pub(crate) scope_id: ScopeId,
}

#[derive(Debug, Clone)]
pub struct CheckedVarDecl {
  pub(crate) name: String,
  pub(crate) type_id: TypeId,
  pub(crate) mutable: bool,
  pub(crate) span: Span,
}

#[derive(Debug, Clone)]
pub struct CheckedConstant {
  pub(crate) name: String,
  pub(crate) type_id: TypeId,
  pub(crate) value: CheckedExpression,
  pub(crate) span: Span,
}

#[derive(Debug, Clone)]
pub struct CheckedTypeDecl {
  pub(crate) name: String,
  pub(crate) generic_parameters: Vec<TypeId>,
  pub(crate) kind: CheckedTypeKind,
  pub(crate) linkage: DefinitionLinkage,
  pub(crate) scope_id: ScopeId,
}
impl CheckedTypeDecl {
  fn implements(&self, lhs_struct_id: TypeDeclId) -> bool {
    if let CheckedTypeKind::Class(ref class) = self.kind {
      for interface in &class.implements {
        if *interface == lhs_struct_id {
          return true;
        }
      }
    }
    false
  }
}

#[derive(Debug, Clone)]
pub enum GenericParameter {
  InferenceGuide(TypeId),
  Parameter(TypeId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CallingConvention {
  C,
  FastCall,
  ColdCall,
  Invalid,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CheckedFunctionAttribute {
  CallConv(CallingConvention),
  NoReturn,
}

#[derive(Debug, Clone)]
pub struct CheckedFunction {
  pub(crate) name: String,
  pub(crate) name_span: Span,
  pub(crate) visibility: Visibility,
  pub(crate) return_type_id: TypeId,
  pub(crate) params: Vec<CheckedVarDecl>,
  pub(crate) generic_parameters: Vec<GenericParameter>,
  pub(crate) scope_id: ScopeId,
  pub(crate) block: CheckedBlock,
  pub(crate) linkage: DefinitionLinkage,
  pub(crate) attributes: Vec<CheckedFunctionAttribute>,
}
impl CheckedFunction {
  pub fn is_static(&self) -> bool {
    if let Some(param) = self.params.get(0) {
      return param.name != "self";
    }

    true
  }

  pub fn is_mutating(&self) -> bool {
    if !self.is_static() {
      return self.params.first().unwrap().mutable;
    }

    false
  }
}

#[derive(Debug, Clone)]
pub struct ResolvedNamespace {
  pub(crate) name: String,
  pub(crate) generic_parameters: Option<Vec<TypeId>>,
}

#[derive(Debug, Clone)]
pub struct CheckedCall {
  pub(crate) namespace: Vec<ResolvedNamespace>,
  pub(crate) name: String,
  pub(crate) args: Vec<CheckedExpression>,
  pub(crate) type_args: Vec<TypeId>,
  pub(crate) linkage: DefinitionLinkage,
  pub(crate) type_id: TypeId,
}

#[derive(Debug, Clone)]
pub enum CheckedUnaryOperator {
  As(TypeId),
  Dereference,
  AddressOf,
  Negate,
  Not,
  BitwiseNot,
}

#[derive(Debug, Clone)]
pub enum CheckedExpression {
  Null(Span, TypeId),
  Nullptr(Span, TypeId),
  Boolean(bool, Span),
  NumericConstant(NumericConstant, TypeId, Span),
  QuotedString(String, Span),
  QuotedCString(String, Span),
  CharacterLiteral(char, Span),

  Variable(CheckedVarDecl, Span),
  NamespacedVariable(Vec<CheckedNamespace>, CheckedVarDecl, Span),

  UnaryOp(Box<CheckedExpression>, CheckedUnaryOperator, TypeId, Span),
  BinaryOp(
    Box<CheckedExpression>,
    BinaryOperator,
    Box<CheckedExpression>,
    TypeId,
    Span,
  ),

  Range(Box<CheckedExpression>, Box<CheckedExpression>, TypeId, Span),

  IndexedExpression(Box<CheckedExpression>, Box<CheckedExpression>, TypeId, Span),
  IndexedStruct(Box<CheckedExpression>, String, TypeDeclId, TypeId, Span),
  IndexedEnum(EnumId, String, Vec<CheckedExpression>, TypeId, Span),

  Call(CheckedCall, TypeId, Span),
  MethodCall(Box<CheckedExpression>, CheckedCall, TypeId, Span),

  InlineAsm {
    volatile: bool,
    asm: Vec<String>,
    bindings: Vec<CheckedInlineAsmBinding>,
    clobbers: Vec<CheckedInlineAsmOperand>,
    type_id: TypeId,
    span: Span,
  },
  Switch {
    value: Box<CheckedExpression>,
    cases: Vec<(Box<CheckedExpression>, CheckedBlock)>,
    default: Option<CheckedBlock>,
    type_id: TypeId,
    span: Span,
  },

  Garbage(Span),
}
impl CheckedExpression {
  pub fn type_id(&self, project: &mut Project) -> TypeId {
    match self {
      CheckedExpression::Null(_, type_id) => *type_id,
      CheckedExpression::Nullptr(_, type_id) => *type_id,
      CheckedExpression::Boolean(_, _) => BOOL_TYPE_ID,
      CheckedExpression::NumericConstant(_, type_id, _) => *type_id,
      CheckedExpression::QuotedString(_, _) => {
        let string_type_decl_id = project
          .find_type_decl_in_scope(0, "String")
          .expect("internal error: unable to locate builtin `String` type");
        project.find_or_add_type_id(CheckedType::TypeDecl(string_type_decl_id, Span::default()))
      }
      CheckedExpression::QuotedCString(_, _) => {
        project.find_or_add_type_id(CheckedType::RawPtr(CCHAR_TYPE_ID, Span::default()))
      }
      CheckedExpression::CharacterLiteral(_, _) => CCHAR_TYPE_ID,
      CheckedExpression::Variable(var, _) => var.type_id,
      CheckedExpression::NamespacedVariable(_, var, _) => var.type_id,
      CheckedExpression::UnaryOp(_, _, type_id, _) => *type_id,
      CheckedExpression::BinaryOp(_, _, _, type_id, _) => *type_id,
      CheckedExpression::Range(_, _, type_id, _) => *type_id,
      CheckedExpression::IndexedExpression(_, _, type_id, _) => *type_id,
      CheckedExpression::IndexedStruct(_, _, _, type_id, _) => *type_id,
      CheckedExpression::IndexedEnum(_, _, _, type_id, _) => *type_id,
      CheckedExpression::Call(_, type_id, _) => *type_id,
      CheckedExpression::MethodCall(_, _, type_id, _) => *type_id,
      CheckedExpression::InlineAsm { type_id, .. } => *type_id,
      CheckedExpression::Switch { type_id, .. } => *type_id,
      CheckedExpression::Garbage(_) => UNKNOWN_TYPE_ID,
    }
  }

  fn is_mutable(&self) -> bool {
    match self {
      CheckedExpression::Variable(var, _) => var.mutable,
      CheckedExpression::IndexedExpression(expr, _, _, _) => expr.is_mutable(),
      CheckedExpression::IndexedStruct(expr, _, _, _, _) => expr.is_mutable(),
      CheckedExpression::UnaryOp(expr, _, _, _) => expr.is_mutable(),
      _ => false,
    }
  }

  pub fn to_integer_constant(&self) -> Option<IntegerConstant> {
    match self {
      CheckedExpression::NumericConstant(constant, _, _) => constant.integer_constant(),
      CheckedExpression::UnaryOp(value, CheckedUnaryOperator::As(_), type_id, _) => {
        if !is_integer(*type_id) {
          return None;
        }
        match &**value {
          CheckedExpression::NumericConstant(constant, _, _) => constant.integer_constant(),
          _ => None,
        }
      }
      _ => None,
    }
  }
}

#[derive(Debug, Clone)]
pub enum CheckedInlineAsmOperandAction {
  None,
  In,
  Out(TypeId),
}

#[derive(Debug, Clone)]
pub struct CheckedInlineAsmOperand {
  pub(crate) operand_type: inline_asm::OperandType,
  pub(crate) action: CheckedInlineAsmOperandAction,
  pub(crate) register: String,
}

#[derive(Debug, Clone)]
pub struct CheckedInlineAsmBinding {
  pub(crate) var: CheckedExpression,
  pub(crate) operand: CheckedInlineAsmOperand,
  pub(crate) span: Span,
}

#[derive(Debug, Clone)]
pub enum CheckedStatement {
  VarDecl(CheckedVarDecl, CheckedExpression),

  Block(CheckedBlock),
  If {
    condition: CheckedExpression,
    then_block: CheckedBlock,
    else_block: Option<Box<CheckedStatement>>,
  },
  While {
    condition: CheckedExpression,
    block: CheckedBlock,
  },
  Loop(CheckedBlock),

  Break(Span),
  Continue(Span),
  Return(CheckedExpression),
  Yield(CheckedExpression),

  Expression(CheckedExpression),
}

impl CheckedStatement {
  pub(crate) fn type_id(&self, project: &mut Project) -> TypeId {
    use CheckedStatement::*;
    match self {
      Return(expr) | Yield(expr) => expr.type_id(project),
      _ => UNKNOWN_TYPE_ID,
    }
  }
}

#[derive(Debug, Clone)]
pub struct CheckedBlock {
  pub(crate) stmts: Vec<CheckedStatement>,
  pub(crate) definitely_returns: bool,
}

impl CheckedBlock {
  pub(crate) fn new() -> Self {
    Self {
      stmts: vec![],
      definitely_returns: false,
    }
  }

  pub(crate) fn type_id(&self, project: &mut Project) -> TypeId {
    if self.definitely_returns {
      if let Some(last) = self.stmts.last() {
        last.type_id(project)
      } else {
        VOID_TYPE_ID
      }
    } else {
      VOID_TYPE_ID
    }
  }
}

#[derive(Debug, Clone)]
pub enum CheckedType {
  Builtin(Span),
  TypeVariable(String, Option<TypeId>, Span),
  GenericInstance(TypeDeclId, Vec<TypeId>, Span),
  GenericEnumInstance(EnumId, Vec<TypeId>, Span),
  TypeDecl(TypeDeclId, Span),
  Enum(EnumId, Span),
  RawPtr(TypeId, Span),
  Slice(TypeId, usize, Span),
}

impl PartialEq for CheckedType {
  fn eq(&self, other: &Self) -> bool {
    match (self, other) {
      (CheckedType::Builtin(_), CheckedType::Builtin(_)) => true,
      (CheckedType::TypeVariable(a, _, _), CheckedType::TypeVariable(b, _, _)) => a == b,
      (CheckedType::GenericInstance(a, ap, _), CheckedType::GenericInstance(b, bp, _)) => {
        a == b && ap == bp
      }
      (CheckedType::TypeDecl(a, _), CheckedType::TypeDecl(b, _)) => a == b,
      (CheckedType::GenericEnumInstance(a, ap, _), CheckedType::GenericEnumInstance(b, bp, _)) => {
        a == b && ap == bp
      }
      (CheckedType::Enum(a, _), CheckedType::Enum(b, _)) => a == b,
      (CheckedType::RawPtr(a, _), CheckedType::RawPtr(b, _)) => a == b,
      (CheckedType::Slice(a, a_size, _), CheckedType::Slice(b, b_size, _)) => {
        a == b && a_size == b_size
      }
      _ => false,
    }
  }
}

impl CheckedType {
  fn span(&self) -> Span {
    match self {
      CheckedType::Builtin(span) => *span,
      CheckedType::TypeVariable(_, _, span) => *span,
      CheckedType::GenericInstance(_, _, span) => *span,
      CheckedType::GenericEnumInstance(_, _, span) => *span,
      CheckedType::TypeDecl(_, span) => *span,
      CheckedType::Enum(_, span) => *span,
      CheckedType::RawPtr(_, span) => *span,
      CheckedType::Slice(_, _, span) => *span,
    }
  }
}

#[derive(Debug, Clone)]
pub struct CheckedMethod {
  pub(crate) name: String,
  pub(crate) return_type_id: TypeId,
  pub(crate) params: Vec<CheckedVarDecl>,
  pub(crate) generic_parameters: Vec<GenericParameter>,
  pub(crate) scope_id: ScopeId,
  pub(crate) block: CheckedBlock,
  pub(crate) is_static: bool,
  pub(crate) visibility: Visibility,
}

#[derive(Debug, Clone)]
pub struct CheckedInterface {
  pub(crate) methods: Vec<CheckedFunction>,
}

#[derive(Debug, Clone)]
pub struct CheckedClass {
  pub(crate) implements: Vec<TypeId>,
  pub(crate) fields: Vec<CheckedVarDecl>,
  pub(crate) methods: Vec<FunctionId>,
}

#[derive(Debug, Clone)]
pub enum CheckedTypeKind {
  Interface(CheckedInterface),
  Class(CheckedClass),
  Alias(TypeId),
}

impl CheckedTypeKind {
  pub fn as_class_mut(&mut self) -> Option<&mut CheckedClass> {
    match self {
      CheckedTypeKind::Class(class) => Some(class),
      _ => None,
    }
  }

  fn as_class(&self) -> Option<&CheckedClass> {
    match self {
      CheckedTypeKind::Class(class) => Some(class),
      _ => None,
    }
  }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Visibility {
  Public,
  Private,
  Protected,
}

#[derive(Clone, Debug)]
pub struct CheckedEnum {
  pub name: String,
  pub generic_parameters: Vec<TypeId>,
  pub variants: Vec<CheckedEnumVariant>,
  pub scope_id: ScopeId,
  pub definition_linkage: DefinitionLinkage,
  pub underlying_type_id: Option<TypeId>,
  pub span: Span,
}

#[derive(Clone, Debug)]
pub enum CheckedEnumVariant {
  Untyped(String, Span),
  WithValue(String, CheckedExpression, Span),
  TupleLike(String, Vec<TypeId>, Span),
  StructLike(String, Vec<CheckedVarDecl>, Span),
}

impl CheckedEnumVariant {
  pub fn name(&self) -> String {
    match self {
      CheckedEnumVariant::Untyped(name, _) => name.clone(),
      CheckedEnumVariant::WithValue(name, _, _) => name.clone(),
      CheckedEnumVariant::TupleLike(name, _, _) => name.clone(),
      CheckedEnumVariant::StructLike(name, _, _) => name.clone(),
    }
  }
}

#[derive(Debug)]
pub struct Project {
  pub(crate) scopes: Vec<Scope>,
  pub(crate) constants: Vec<CheckedConstant>,
  pub(crate) type_decls: Vec<CheckedTypeDecl>,
  pub(crate) enums: Vec<CheckedEnum>,
  pub(crate) functions: Vec<CheckedFunction>,
  pub(crate) types: Vec<CheckedType>,
  pub(crate) diagnostics: Vec<Diagnostic>,
  pub(crate) current_function_index: Option<usize>,
}

impl Project {
  pub fn new() -> Self {
    let global_scope = Scope::new(None);
    Self {
      scopes: vec![global_scope],
      constants: vec![],
      type_decls: vec![],
      enums: vec![],
      functions: vec![],
      types: vec![
        CheckedType::Builtin(Span::default()), // Void
        CheckedType::Builtin(Span::default()), // I8
        CheckedType::Builtin(Span::default()), // I16
        CheckedType::Builtin(Span::default()), // I32
        CheckedType::Builtin(Span::default()), // I64
        CheckedType::Builtin(Span::default()), // I128
        CheckedType::Builtin(Span::default()), // ISZ
        CheckedType::Builtin(Span::default()), // U8
        CheckedType::Builtin(Span::default()), // U16
        CheckedType::Builtin(Span::default()), // U32
        CheckedType::Builtin(Span::default()), // U64
        CheckedType::Builtin(Span::default()), // U128
        CheckedType::Builtin(Span::default()), // USZ
        CheckedType::Builtin(Span::default()), // F32
        CheckedType::Builtin(Span::default()), // F64
        CheckedType::Builtin(Span::default()), // Bool
        CheckedType::Builtin(Span::default()), // CChar
        CheckedType::Builtin(Span::default()), // Rawptr
      ],
      diagnostics: vec![],
      current_function_index: None,
    }
  }

  pub fn create_scope(&mut self, parent_id: ScopeId) -> ScopeId {
    self.scopes.push(Scope::new(Some(parent_id)));
    self.scopes.len() - 1
  }

  fn find_or_add_type_id(&mut self, ty: CheckedType) -> TypeId {
    for (idx, t) in self.types.iter().enumerate() {
      if t == &ty {
        return idx;
      }
    }

    self.types.push(ty);
    self.types.len() - 1
  }

  pub fn find_type_in_scope(&self, scope_id: ScopeId, type_name: &str) -> Option<TypeId> {
    let mut scope_id = Some(scope_id);

    while let Some(current_id) = scope_id {
      let scope = &self.scopes[current_id];
      for s in &scope.types {
        if s.0 == type_name {
          return Some(s.1);
        }
      }
      scope_id = scope.parent;
    }

    None
  }

  pub fn add_type_to_scope(
    &mut self,
    scope_id: ScopeId,
    type_name: String,
    type_id: TypeId,
    span: Span,
  ) {
    let scope = &mut self.scopes[scope_id];

    for (existing_type, existing_type_id) in &scope.types {
      if &type_name == existing_type {
        let diagnostic = Diagnostic::error(span, format!("redefinition of type {}", type_name))
          .with_hint(
            self.types[*existing_type_id].span(),
            "previous definition here".into(),
          );
        self.add_diagnostic(diagnostic);
        return;
      }
    }
    scope.types.push((type_name, type_id));
  }

  pub fn find_type_decl_in_scope(&self, scope_id: ScopeId, type_decl: &str) -> Option<TypeDeclId> {
    let mut scope_id = Some(scope_id);

    while let Some(current_id) = scope_id {
      let scope = &self.scopes[current_id];
      for s in &scope.type_decls {
        if s.0 == type_decl {
          return Some(s.1);
        }
      }
      scope_id = scope.parent;
    }

    None
  }

  pub fn add_type_decl_to_scope(
    &mut self,
    scope_id: ScopeId,
    name: String,
    type_decl_id: TypeDeclId,
    span: Span,
  ) {
    let scope = &mut self.scopes[scope_id];

    for (type_decl_name, _) in &scope.type_decls {
      if &name == type_decl_name {
        let diagnostic = Diagnostic::error(
          span,
          format!("redefinition of type declaration {}", type_decl_name),
        );
        self.add_diagnostic(diagnostic);
        return;
      }
    }
    scope.type_decls.push((name, type_decl_id));
  }

  pub fn find_enum_in_scope(&self, scope_id: ScopeId, enum_name: &str) -> Option<EnumId> {
    let mut scope_id = Some(scope_id);

    while let Some(current_id) = scope_id {
      let scope = &self.scopes[current_id];
      for e in &scope.enums {
        if e.0 == enum_name {
          return Some(e.1);
        }
      }
      scope_id = scope.parent;
    }

    None
  }

  pub fn add_enum_to_scope(
    &mut self,
    scope_id: ScopeId,
    name: String,
    enum_id: EnumId,
    span: Span,
  ) {
    let scope = &mut self.scopes[scope_id];
    for (existing_enum, _) in &scope.enums {
      if &name == existing_enum {
        let diagnostic =
          Diagnostic::error(span, format!("redefinition of type declaration {}", name));
        self.add_diagnostic(diagnostic);
        return;
      }
    }

    scope.enums.push((name, enum_id));
  }

  fn find_var_in_scope(&self, scope_id: usize, name: &str) -> Option<CheckedVarDecl> {
    let mut scope_id = Some(scope_id);

    while let Some(current_id) = scope_id {
      let scope = &self.scopes[current_id];
      for var in &scope.vars {
        if var.name == name {
          return Some(var.clone());
        }
      }
      scope_id = scope.parent;
    }

    None
  }

  pub fn add_var_to_scope(&mut self, scope_id: ScopeId, var: CheckedVarDecl, span: Span) {
    let scope = &mut self.scopes[scope_id];

    for existing_var in &scope.vars {
      if &var.name == &existing_var.name {
        self.add_diagnostic(Diagnostic::error(
          span,
          format!("redefinition of variable {}", var.name),
        ));
        return;
      }
    }
    scope.vars.push(var);
  }

  pub fn find_namespace_in_scope(&self, scope_id: ScopeId, name: &str) -> Option<ScopeId> {
    let mut scope_id = Some(scope_id);

    while let Some(current_id) = scope_id {
      let scope = &self.scopes[current_id];
      for child_scope_id in &scope.children {
        let child_scope = &self.scopes[*child_scope_id];
        if let Some(namespace_name) = &child_scope.namespace_name {
          if namespace_name == name {
            return Some(*child_scope_id);
          }
        }
      }
      scope_id = scope.parent;
    }

    None
  }

  pub fn find_function_in_scope(&self, scope_id: ScopeId, name: &str) -> Option<FunctionId> {
    let mut scope_id = Some(scope_id);

    while let Some(current_id) = scope_id {
      let scope = &self.scopes[current_id];
      for s in &scope.functions {
        if s.0 == name {
          return Some(s.1);
        }
      }
      scope_id = scope.parent;
    }

    None
  }

  pub fn add_function_to_scope(
    &mut self,
    scope_id: ScopeId,
    name: String,
    function_id: FunctionId,
    span: Span,
  ) {
    let scope = &mut self.scopes[scope_id];

    for (function_name, _) in &scope.functions {
      if &name == function_name {
        let diagnostic =
          Diagnostic::error(span, format!("redefinition of function {}", function_name));
        self.add_diagnostic(diagnostic);
        return;
      }
    }
    scope.functions.push((name, function_id));
  }

  pub fn add_constant_to_scope(
    &mut self,
    scope_id: ScopeId,
    constant: CheckedConstant,
    span: Span,
  ) {
    let scope = &mut self.scopes[scope_id];

    for existing_constant in &scope.constants {
      if &constant.name == &existing_constant.name {
        let diagnostic =
          Diagnostic::error(span, format!("redefinition of constant {}", constant.name));
        self.add_diagnostic(diagnostic);
        return;
      }
    }
    scope.constants.push(constant);
  }

  pub fn find_constant_in_scope(&self, scope_id: ScopeId, name: &str) -> Option<CheckedConstant> {
    let mut scope_id = Some(scope_id);

    while let Some(current_id) = scope_id {
      let scope = &self.scopes[current_id];
      for constant in &scope.constants {
        if constant.name == name {
          return Some(constant.clone());
        }
      }
      scope_id = scope.parent;
    }

    None
  }

  pub fn typename_for_type_id(&self, type_id: TypeId) -> String {
    match &self.types[type_id] {
      CheckedType::Builtin(_) => match type_id {
        crate::compiler::VOID_TYPE_ID => "void".to_string(),
        crate::compiler::I8_TYPE_ID => "i8".to_string(),
        crate::compiler::I16_TYPE_ID => "i16".to_string(),
        crate::compiler::I32_TYPE_ID => "i32".to_string(),
        crate::compiler::I64_TYPE_ID => "i64".to_string(),
        crate::compiler::I128_TYPE_ID => "i128".to_string(),
        crate::compiler::ISZ_TYPE_ID => "isz".to_string(),
        crate::compiler::U8_TYPE_ID => "u8".to_string(),
        crate::compiler::U16_TYPE_ID => "u16".to_string(),
        crate::compiler::U32_TYPE_ID => "u32".to_string(),
        crate::compiler::U64_TYPE_ID => "u64".to_string(),
        crate::compiler::U128_TYPE_ID => "u128".to_string(),
        crate::compiler::USZ_TYPE_ID => "usz".to_string(),
        crate::compiler::F32_TYPE_ID => "f32".to_string(),
        crate::compiler::F64_TYPE_ID => "f64".to_string(),
        crate::compiler::BOOL_TYPE_ID => "bool".to_string(),
        crate::compiler::CCHAR_TYPE_ID => "c_char".to_string(),
        crate::compiler::RAWPTR_TYPE_ID => "rawptr".to_string(),
        crate::compiler::NEVER_TYPE_ID => "never".to_string(),
        _ => "unknown".to_string(),
      },
      CheckedType::TypeDecl(type_decl_id, _) => {
        format!("{}", self.type_decls[*type_decl_id].name)
      }
      CheckedType::Enum(enum_id, _) => {
        format!("enum {}", self.enums[*enum_id].name)
      }
      CheckedType::GenericInstance(type_decl_id, type_args, _) => {
        let mut output = format!("{}", self.type_decls[*type_decl_id].name);

        output.push('<');
        let mut first = true;
        for arg in type_args {
          if !first {
            output.push_str(", ");
          } else {
            first = false;
          }
          output.push_str(&self.typename_for_type_id(*arg))
        }
        output.push('>');

        output
      }
      CheckedType::GenericEnumInstance(enum_id, type_args, _) => {
        let mut output = format!("enum {}", self.enums[*enum_id].name);

        output.push('<');
        let mut first = true;
        for arg in type_args {
          if !first {
            output.push_str(", ");
          } else {
            first = false;
          }
          output.push_str(&self.typename_for_type_id(*arg))
        }
        output.push('>');

        output
      }
      CheckedType::TypeVariable(name, _, _) => name.clone(),
      CheckedType::RawPtr(type_id, _) => format!("{}*", self.typename_for_type_id(*type_id)),
      CheckedType::Slice(type_id, size, _) => {
        format!("{}[{size}]", self.typename_for_type_id(*type_id))
      }
    }
  }

  pub fn get_function_by_name(&self, name: &str) -> Option<&CheckedFunction> {
    for function in &self.functions {
      if function.name == name {
        return Some(function);
      }
    }

    None
  }

  pub fn add_diagnostic(&mut self, diagnostic: Diagnostic) {
    self.diagnostics.push(diagnostic);
  }

  pub fn is_interface(&self, type_decl_id: TypeDeclId) -> bool {
    if let CheckedTypeKind::Interface(_) = self.type_decls[type_decl_id].kind {
      return true;
    }
    false
  }

  pub fn is_class(&self, type_decl_id: TypeDeclId) -> bool {
    if let CheckedTypeKind::Class(_) = self.type_decls[type_decl_id].kind {
      return true;
    }
    false
  }
}

pub(crate) fn mangle_type(type_id: TypeId, project: &mut Project) -> String {
  let mut mangled_name = "_".to_string();

  match &project.types[type_id] {
    CheckedType::Builtin(_) => match type_id {
      crate::compiler::VOID_TYPE_ID => mangled_name.push_str("void"),
      crate::compiler::I8_TYPE_ID => mangled_name.push_str("i8"),
      crate::compiler::I16_TYPE_ID => mangled_name.push_str("i16"),
      crate::compiler::I32_TYPE_ID => mangled_name.push_str("i32"),
      crate::compiler::I64_TYPE_ID => mangled_name.push_str("i64"),
      crate::compiler::I128_TYPE_ID => mangled_name.push_str("i128"),
      crate::compiler::ISZ_TYPE_ID => mangled_name.push_str("isz"),
      crate::compiler::U8_TYPE_ID => mangled_name.push_str("u8"),
      crate::compiler::U16_TYPE_ID => mangled_name.push_str("u16"),
      crate::compiler::U32_TYPE_ID => mangled_name.push_str("u32"),
      crate::compiler::U64_TYPE_ID => mangled_name.push_str("u64"),
      crate::compiler::U128_TYPE_ID => mangled_name.push_str("u128"),
      crate::compiler::USZ_TYPE_ID => mangled_name.push_str("usz"),
      crate::compiler::F32_TYPE_ID => mangled_name.push_str("f32"),
      crate::compiler::F64_TYPE_ID => mangled_name.push_str("f64"),
      crate::compiler::BOOL_TYPE_ID => mangled_name.push_str("bool"),
      crate::compiler::CCHAR_TYPE_ID => mangled_name.push_str("c_char"),
      crate::compiler::RAWPTR_TYPE_ID => mangled_name.push_str("rawptr"),
      _ => todo!(),
    },
    _ => todo!(),
  }

  mangled_name
}

fn mangle_name(function: &CheckedFunction, project: &mut Project) -> String {
  let mut mangled_name = "_".to_string();

  mangled_name.push_str(&function.name.len().to_string());
  mangled_name.push_str(&function.name.clone());

  for param in function.params.iter() {
    mangled_name.push_str(&mangle_type(param.type_id, project));
  }

  mangled_name
}

pub fn typecheck_namespace(
  parsed_namespace: &ParsedNamespace,
  scope_id: ScopeId,
  project: &mut Project,
) {
  let project_type_decl_len = project.type_decls.len();
  let project_enum_len = project.enums.len();
  let project_function_len = project.functions.len();

  for namespace in parsed_namespace.namespaces.iter() {
    let namespace_scope_id = project.create_scope(scope_id);
    project.scopes[namespace_scope_id].namespace_name = namespace.name.clone();
    project.scopes[scope_id].children.push(namespace_scope_id);
    typecheck_namespace(namespace, namespace_scope_id, project);
  }

  for (type_decl_id, structure) in parsed_namespace.type_decls.iter().enumerate() {
    let type_decl_id = type_decl_id + project_type_decl_len;
    project
      .types
      .push(CheckedType::TypeDecl(type_decl_id, structure.name_span));

    let struct_type_id = project.types.len() - 1;
    project.add_type_to_scope(
      scope_id,
      structure.name.clone(),
      struct_type_id,
      structure.name_span,
    );
  }

  for (enum_id, enum_) in parsed_namespace.enums.iter().enumerate() {
    let enum_id = enum_id + project_enum_len;
    project
      .types
      .push(CheckedType::Enum(enum_id, enum_.name_span));

    let enum_type_id = project.types.len() - 1;
    project.add_type_to_scope(scope_id, enum_.name.clone(), enum_type_id, enum_.name_span)
  }

  for (type_decl_id, type_decl) in parsed_namespace.type_decls.iter().enumerate() {
    let type_decl_id = type_decl_id + project_type_decl_len;

    typecheck_type_decl_predecl(type_decl, type_decl_id, scope_id, project);
  }

  for (enum_id, enum_) in parsed_namespace.enums.iter().enumerate() {
    let enum_id = enum_id + project_enum_len;

    typecheck_enum_predecl(enum_, enum_id, scope_id, project);
  }

  for function in &parsed_namespace.functions {
    typecheck_function_predecl(function, scope_id, project);
  }

  for constant in parsed_namespace.constants.iter() {
    let type_id = typecheck_typename(&constant.r#type, scope_id, project);
    let value = typecheck_expression(&constant.value, scope_id, Some(type_id), project);
    let checked_constant = CheckedConstant {
      name: constant.name.clone(),
      type_id,
      value,
      span: constant.name_span,
    };
    project.constants.push(checked_constant.clone());
    project.add_constant_to_scope(scope_id, checked_constant, constant.name_span);
  }

  for (type_decl_id, type_decl) in parsed_namespace.type_decls.iter().enumerate() {
    typecheck_type_decl(
      type_decl,
      type_decl_id + project_type_decl_len,
      scope_id,
      project,
    );
  }

  for (enum_id, enum_) in parsed_namespace.enums.iter().enumerate() {
    typecheck_enum(
      enum_,
      enum_id + project_enum_len,
      project.find_type_in_scope(scope_id, &enum_.name).unwrap(),
      project.enums[enum_id + project_enum_len].scope_id,
      scope_id,
      project,
    );
  }

  for (i, function) in parsed_namespace.functions.iter().enumerate() {
    project.current_function_index = Some(i + project_function_len);
    typecheck_function(function, scope_id, project);
    project.current_function_index = None;
  }

  let fns = project.functions.clone();
  for i in 0..fns.len() {
    if !fns[i].generic_parameters.is_empty() {
      project.functions.remove(i);
    }
  }
}

fn typecheck_enum_predecl(
  enum_: &ParsedEnum,
  enum_id: EnumId,
  parent_scope_id: ScopeId,
  project: &mut Project,
) {
  let enum_scope_id = project.create_scope(parent_scope_id);
  let mut generic_parameters = Vec::new();

  for type_arg in &enum_.type_parameters {
    project.types.push(CheckedType::TypeVariable(
      type_arg.name.clone(),
      None,
      type_arg.span,
    ));
    let parameter_type_id = project.types.len() - 1;
    project.add_type_to_scope(
      enum_scope_id,
      type_arg.name.clone(),
      parameter_type_id,
      type_arg.span,
    );

    generic_parameters.push(parameter_type_id);
  }

  let type_id = typecheck_typename(&enum_.underlying_type, enum_scope_id, project);
  let underlying_type_id = if type_id == UNKNOWN_TYPE_ID {
    None
  } else {
    Some(type_id)
  };

  project.enums.push(CheckedEnum {
    name: enum_.name.clone(),
    generic_parameters,
    variants: Vec::new(),
    scope_id: enum_scope_id,
    definition_linkage: enum_.linkage,
    underlying_type_id,
    span: enum_.name_span,
  });

  project.add_enum_to_scope(
    parent_scope_id,
    enum_.name.clone(),
    enum_id,
    enum_.name_span,
  )
}

fn typecheck_enum(
  enum_: &ParsedEnum,
  enum_id: EnumId,
  enum_type_id: TypeId,
  enum_scope_id: ScopeId,
  parent_scope_id: ScopeId,
  project: &mut Project,
) {
  let mut variants = vec![];

  let mut next_constant_value: Option<u64> = Some(0);
  let mut seen_names = HashSet::new();

  let cast_to_underlying = |x: ParsedExpression, project: &mut Project| -> CheckedExpression {
    let span = x.span();
    let expression = ParsedExpression::UnaryOp(
      Box::new(x),
      UnaryOperator::As(enum_.underlying_type.clone()),
      span,
    );
    typecheck_expression(&expression, enum_scope_id, None, project)
  };

  let underlying_type_id = project.enums[enum_id].underlying_type_id;

  for variant in &enum_.variants {
    match &variant {
      ParsedEnumVariant::Untyped(name, span) => {
        if seen_names.contains(name) {
          project.add_diagnostic(Diagnostic::error(
            *span,
            format!("enum variant '{}' is defined more than once", name),
          ));
        } else {
          seen_names.insert(name.clone());
          if underlying_type_id.is_some() {
            if next_constant_value.is_none() {
              project.add_diagnostic(Diagnostic::error(
                                *span,
                                "missing enum variant value, the enum underlying type is not numeric, and so all enum variants must have explicit values".to_string(),
                            ));
            } else {
              let checked_expression = cast_to_underlying(
                ParsedExpression::NumericConstant(
                  NumericConstant::U64(next_constant_value.unwrap()),
                  *span,
                ),
                project,
              );

              variants.push(CheckedEnumVariant::WithValue(
                name.clone(),
                checked_expression,
                *span,
              ));
              next_constant_value = Some(next_constant_value.unwrap() + 1);

              project.add_var_to_scope(
                enum_scope_id,
                CheckedVarDecl {
                  name: name.clone(),
                  type_id: enum_type_id,
                  mutable: false,
                  span: *span,
                },
                *span,
              );
            }
          } else {
            variants.push(CheckedEnumVariant::Untyped(name.clone(), *span));
          }

          if project
            .find_function_in_scope(enum_scope_id, name.as_str())
            .is_none()
          {
            let function_scope_id = project.create_scope(parent_scope_id);

            let checked_constructor = CheckedFunction {
              name: name.clone(),
              name_span: *span,
              return_type_id: enum_type_id,
              params: vec![],
              scope_id: function_scope_id,
              visibility: Visibility::Public,
              generic_parameters: enum_
                .type_parameters
                .iter()
                .map(|x| {
                  GenericParameter::InferenceGuide(
                    project.find_type_in_scope(enum_scope_id, &x.name).unwrap(),
                  )
                })
                .collect(),
              block: CheckedBlock::new(),
              linkage: DefinitionLinkage::ImplicitEnumConstructor,
              attributes: vec![],
            };

            project.functions.push(checked_constructor);

            project.add_function_to_scope(
              enum_scope_id,
              name.clone(),
              project.functions.len() - 1,
              *span,
            )
          }
        }
      }
      ParsedEnumVariant::WithValue(name, value, span) => {
        if seen_names.contains(name) {
          project.add_diagnostic(Diagnostic::error(
            *span,
            format!("enum variant '{}' is defined more than once", name),
          ));
        } else {
          seen_names.insert(name.clone());
          let checked_expression = cast_to_underlying((*value).clone(), project);
          match checked_expression.to_integer_constant() {
            Some(constant) => {
              next_constant_value = Some(constant.to_usize() as u64 + 1);
            }
            None => {
              project.add_diagnostic(Diagnostic::error(
                *span,
                format!(
                  "enum variant '{}' in enum '{}' has a non-constant value: {:?}",
                  name, enum_.name, checked_expression
                ),
              ));
            }
          }

          variants.push(CheckedEnumVariant::WithValue(
            name.clone(),
            checked_expression,
            *span,
          ));

          project.add_var_to_scope(
            enum_scope_id,
            CheckedVarDecl {
              name: name.clone(),
              type_id: enum_type_id,
              mutable: false,
              span: *span,
            },
            *span,
          );
        }
      }
      ParsedEnumVariant::TupleLike(name, members, span) => {
        if seen_names.contains(name) {
          project.add_diagnostic(Diagnostic::error(
            *span,
            format!("enum variant '{}' is defined more than once", name),
          ));
        } else {
          seen_names.insert(name.clone());
          let mut checked_members = Vec::new();
          for member in members {
            let decl = typecheck_typename(&member, enum_scope_id, project);
            checked_members.push(decl)
          }

          variants.push(CheckedEnumVariant::TupleLike(
            name.clone(),
            checked_members.clone(),
            *span,
          ));

          if project
            .find_function_in_scope(enum_scope_id, name.as_str())
            .is_none()
          {
            let function_scope_id = project.create_scope(parent_scope_id);

            let checked_constructor = CheckedFunction {
              name: name.clone(),
              name_span: *span,
              return_type_id: enum_type_id,
              params: checked_members
                .iter()
                .enumerate()
                .map(|(i, type_id)| CheckedVarDecl {
                  name: format!("_{i}"),
                  type_id: *type_id,
                  mutable: false,
                  span: Span::default(),
                })
                .collect(),
              visibility: Visibility::Public,
              scope_id: function_scope_id,
              generic_parameters: enum_
                .type_parameters
                .iter()
                .map(|x| {
                  GenericParameter::InferenceGuide(
                    project.find_type_in_scope(enum_scope_id, &x.name).unwrap(),
                  )
                })
                .collect(),
              block: CheckedBlock::new(),
              linkage: DefinitionLinkage::ImplicitEnumConstructor,
              attributes: vec![],
            };

            project.functions.push(checked_constructor);

            project.add_function_to_scope(
              enum_scope_id,
              name.clone(),
              project.functions.len() - 1,
              *span,
            )
          }
        }
      }
      ParsedEnumVariant::StructLike(name, members, span) => {
        if seen_names.contains(name) {
          project.add_diagnostic(Diagnostic::error(
            *span,
            format!("enum variant '{}' is defined more than once", name),
          ));
        } else {
          seen_names.insert(name.clone());
          let mut member_names = HashSet::new();
          let mut checked_members = Vec::new();
          for member in members {
            if member_names.contains(&member.name) {
              project.add_diagnostic(Diagnostic::error(
                *span,
                format!(
                  "enum variant '{}' has a member named '{}' more than once",
                  name, member.name
                ),
              ));
            } else {
              member_names.insert(member.name.clone());
              let decl = typecheck_typename(&member.ty, enum_scope_id, project);
              checked_members.push(CheckedVarDecl {
                name: member.name.clone(),
                type_id: decl,
                mutable: member.mutable,
                span: member.span,
              })
            }
          }

          variants.push(CheckedEnumVariant::StructLike(
            name.clone(),
            checked_members.clone(),
            *span,
          ));

          if project
            .find_function_in_scope(enum_scope_id, name.as_str())
            .is_none()
          {
            let function_scope_id = project.create_scope(parent_scope_id);

            let checked_constructor = CheckedFunction {
              name: name.clone(),
              name_span: *span,
              return_type_id: enum_type_id,
              params: checked_members,
              visibility: Visibility::Public,
              scope_id: function_scope_id,
              generic_parameters: enum_
                .type_parameters
                .iter()
                .map(|x| {
                  GenericParameter::InferenceGuide(
                    project.find_type_in_scope(enum_scope_id, &x.name).unwrap(),
                  )
                })
                .collect(),
              block: CheckedBlock::new(),
              linkage: DefinitionLinkage::ImplicitEnumConstructor,
              attributes: vec![],
            };

            project.functions.push(checked_constructor);

            project.add_function_to_scope(
              enum_scope_id,
              name.clone(),
              project.functions.len() - 1,
              *span,
            )
          }
        }
      }
    }
  }

  project.enums[enum_id].variants = variants;
}

fn typecheck_type_decl_predecl(
  type_decl: &ParsedTypeDecl,
  type_decl_id: TypeDeclId,
  parent_scope_id: ScopeId,
  project: &mut Project,
) {
  let type_decl_type_id =
    project.find_or_add_type_id(CheckedType::TypeDecl(type_decl_id, type_decl.name_span));
  let type_decl_scope_id = project.create_scope(parent_scope_id);

  let mut type_params = vec![];
  for type_param in &type_decl.type_parameters {
    let constraint = if let Some(constraint) = &type_param.constraint {
      Some(typecheck_typename(constraint, type_decl_scope_id, project))
    } else {
      None
    };
    project.types.push(CheckedType::TypeVariable(
      type_param.name.to_string(),
      constraint,
      type_param.span,
    ));
    let param_type_id = project.types.len() - 1;

    type_params.push(param_type_id);

    project.add_type_to_scope(
      type_decl_scope_id,
      type_param.name.to_string(),
      param_type_id,
      type_param.span,
    );
  }

  match &type_decl.data {
    ParsedTypeDeclData::Interface(methods) => {
      let mut checked_methods = vec![];
      for method in methods.iter() {
        let mut type_params = vec![];
        let method_scope_id = project.create_scope(type_decl_scope_id);

        for param in &method.type_parameters {
          let constraint = if let Some(constraint) = &param.constraint {
            Some(typecheck_typename(constraint, type_decl_scope_id, project))
          } else {
            None
          };
          project.types.push(CheckedType::TypeVariable(
            param.name.to_string(),
            constraint,
            param.span,
          ));
          let param_type_id = project.types.len() - 1;

          type_params.push(GenericParameter::Parameter(param_type_id));

          project.add_type_to_scope(
            type_decl_scope_id,
            param.name.to_string(),
            param_type_id,
            param.span,
          );
        }

        let mut checked_function = CheckedFunction {
          name: method.name.clone(),
          name_span: method.name_span,
          visibility: Visibility::Public,
          return_type_id: UNKNOWN_TYPE_ID,
          params: vec![],
          generic_parameters: type_params,
          scope_id: method_scope_id,
          block: CheckedBlock::new(),
          linkage: method.linkage,
          attributes: vec![],
        };

        for param in &method.parameters {
          if param.name.as_str() == "self" {
            let checked_var = CheckedVarDecl {
              name: param.name.clone(),
              type_id: type_decl_type_id,
              mutable: param.mutable,
              span: param.span,
            };
            checked_function.params.push(checked_var);
          } else {
            let type_id = typecheck_typename(&param.ty, method_scope_id, project);
            let checked_var = CheckedVarDecl {
              name: param.name.clone(),
              type_id,
              mutable: param.mutable,
              span: param.span,
            };
            checked_function.params.push(checked_var);
          }
        }

        checked_methods.push(checked_function.clone());

        project.functions.push(checked_function);
        project.add_function_to_scope(
          type_decl_scope_id,
          method.name.clone(),
          project.functions.len() - 1,
          type_decl.name_span,
        );
      }

      project.type_decls.push(CheckedTypeDecl {
        name: type_decl.name.clone(),
        generic_parameters: type_params,
        kind: CheckedTypeKind::Interface(CheckedInterface {
          methods: checked_methods,
        }),
        scope_id: type_decl_scope_id,
        linkage: type_decl.linkage,
      });
    }
    ParsedTypeDeclData::Class {
      implements,
      methods,
      ..
    } => {
      let mut checked_methods = vec![];
      for method in methods.iter() {
        let mut type_params = vec![];
        let method_scope_id = project.create_scope(type_decl_scope_id);

        for param in &method.type_parameters {
          let constraint = if let Some(constraint) = &param.constraint {
            Some(typecheck_typename(constraint, type_decl_scope_id, project))
          } else {
            None
          };
          project.types.push(CheckedType::TypeVariable(
            param.name.to_string(),
            constraint,
            param.span,
          ));
          let param_type_id = project.types.len() - 1;

          type_params.push(GenericParameter::Parameter(param_type_id));

          project.add_type_to_scope(
            type_decl_scope_id,
            param.name.to_string(),
            param_type_id,
            param.span,
          );
        }

        let mut checked_function = CheckedFunction {
          name: method.name.clone(),
          name_span: method.name_span,
          visibility: Visibility::Public,
          return_type_id: UNKNOWN_TYPE_ID,
          params: vec![],
          generic_parameters: type_params,
          scope_id: method_scope_id,
          block: CheckedBlock::new(),
          linkage: method.linkage,
          attributes: vec![],
        };

        for param in &method.parameters {
          if param.name.as_str() == "self" {
            let checked_var = CheckedVarDecl {
              name: param.name.clone(),
              type_id: type_decl_type_id,
              mutable: param.mutable,
              span: param.span,
            };
            checked_function.params.push(checked_var);
          } else {
            let type_id = typecheck_typename(&param.ty, method_scope_id, project);
            let checked_var = CheckedVarDecl {
              name: param.name.clone(),
              type_id,
              mutable: param.mutable,
              span: param.span,
            };
            checked_function.params.push(checked_var);
          }
        }

        project.functions.push(checked_function.clone());
        checked_methods.push(project.functions.len() - 1);
        project.add_function_to_scope(
          type_decl_scope_id,
          method.name.clone(),
          project.functions.len() - 1,
          type_decl.name_span,
        );
      }

      let mut checked_implements = vec![];
      for interface in implements {
        let interface_type_id = typecheck_typename(interface, type_decl_scope_id, project);
        if interface_type_id == UNKNOWN_TYPE_ID {
          continue;
        }

        let interface_type = &project.types[interface_type_id];
        let CheckedType::TypeDecl(interface_id, span) = interface_type else {
          project.add_diagnostic(Diagnostic::error(
            interface_type.span(),
            "unknown type declaration".into(),
          ));
          continue;
        };

        let checked_type_decl = &project.type_decls[*interface_id];
        let CheckedTypeKind::Interface(_) = checked_type_decl.kind else {
          project.add_diagnostic(Diagnostic::error(
            *span,
            format!("unknown interface {}", checked_type_decl.name),
          ));
          continue;
        };

        checked_implements.push(*interface_id);
      }

      project.type_decls.push(CheckedTypeDecl {
        name: type_decl.name.clone(),
        generic_parameters: type_params,
        kind: CheckedTypeKind::Class(CheckedClass {
          implements: checked_implements,
          fields: vec![],
          methods: checked_methods,
        }),
        scope_id: type_decl_scope_id,
        linkage: type_decl.linkage,
      });
    }
    ParsedTypeDeclData::Alias(unchecked_type) => {
      let type_id = typecheck_typename(unchecked_type, type_decl_scope_id, project);
      project.type_decls.push(CheckedTypeDecl {
        name: type_decl.name.clone(),
        generic_parameters: type_params,
        kind: CheckedTypeKind::Alias(type_id),
        scope_id: type_decl_scope_id,
        linkage: type_decl.linkage,
      });
    }
  }

  project.add_type_decl_to_scope(
    parent_scope_id,
    type_decl.name.clone(),
    type_decl_id,
    type_decl.name_span,
  );
}

fn typecheck_type_decl(
  type_decl: &ParsedTypeDecl,
  type_decl_id: TypeDeclId,
  parent_scope_id: ScopeId,
  project: &mut Project,
) {
  let mut checked_fields = vec![];

  let checked_type_decl = &mut project.type_decls[type_decl_id];
  let type_decl_scope_id = checked_type_decl.scope_id;
  let type_decl_type_id =
    project.find_or_add_type_id(CheckedType::TypeDecl(type_decl_id, type_decl.name_span));

  match &type_decl.data {
    ParsedTypeDeclData::Interface(_) => {}
    ParsedTypeDeclData::Class {
      implements, fields, ..
    } => {
      for (unchecked_member, _unchecked_value) in fields {
        let checked_member_type =
          typecheck_typename(&unchecked_member.ty, type_decl_scope_id, project);

        checked_fields.push(CheckedVarDecl {
          name: unchecked_member.name.clone(),
          type_id: checked_member_type,
          mutable: unchecked_member.mutable,
          span: unchecked_member.span,
        });
      }

      if project
        .find_function_in_scope(type_decl_scope_id, &type_decl.name)
        .is_none()
      {
        let constructor_params = checked_fields
          .clone()
          .into_iter()
          .map(|field| CheckedVarDecl {
            name: field.name.clone(),
            type_id: field.type_id,
            mutable: field.mutable,
            span: field.span,
          })
          .collect::<Vec<_>>();

        let generic_parameters = project.type_decls[type_decl_id]
          .clone()
          .generic_parameters
          .iter()
          .map(|x| GenericParameter::Parameter(*x))
          .collect::<Vec<_>>();

        let function_scope_id = project.create_scope(parent_scope_id);
        let checked_constructor = CheckedFunction {
          name: type_decl.name.clone(),
          name_span: type_decl.name_span,
          visibility: Visibility::Public,
          return_type_id: type_decl_type_id,
          params: constructor_params,
          generic_parameters,
          scope_id: function_scope_id,
          block: CheckedBlock::new(),
          linkage: DefinitionLinkage::ImplicitConstructor,
          attributes: vec![],
        };

        project.functions.push(checked_constructor);
        project.add_function_to_scope(
          type_decl_scope_id,
          type_decl.name.clone(),
          project.functions.len() - 1,
          type_decl.name_span,
        );
      }

      let checked_type_decl = &mut project.type_decls[type_decl_id];
      let checked_class = checked_type_decl
        .kind
        .as_class_mut()
        .expect("internal error: got something other than a class while typechecking classes");

      checked_class.fields = checked_fields;
      let checked_method_ids = checked_class.methods.clone();

      let functions = project.functions.clone();
      let mut checked_methods = vec![];
      for method_id in checked_method_ids {
        let method = &functions[method_id];
        checked_methods.push(method);
      }

      for interface in implements {
        let interface_type_id = typecheck_typename(interface, type_decl_scope_id, project);
        if interface_type_id == UNKNOWN_TYPE_ID {
          continue;
        }

        let interface_type = &project.types[interface_type_id];
        let CheckedType::TypeDecl(interface_id, span) = interface_type else {
          project.add_diagnostic(Diagnostic::error(
            interface_type.span(),
            "unknown type declaration".into(),
          ));
          continue;
        };

        let checked_type_decl = &project.type_decls[*interface_id];
        let CheckedTypeKind::Interface(ref interface) = checked_type_decl.kind else {
          project.add_diagnostic(Diagnostic::error(
            *span,
            format!("unknown interface {}", checked_type_decl.name),
          ));
          continue;
        };

        let interface = interface.clone();

        let mut found_methods = vec![];
        for interface_method in interface.methods.iter() {
          let mut method_found = false;
          for class_method in checked_methods.iter() {
            if interface_method.name == class_method.name {
              // Check if the return types match
              if interface_method.return_type_id != class_method.return_type_id {
                project.add_diagnostic(
                  Diagnostic::error(class_method.name_span, format!("mismatched return type"))
                    .with_hint(
                      interface_method.name_span,
                      format!(
                        "expected `{}`, found `{}`",
                        project.typename_for_type_id(interface_method.return_type_id),
                        project.typename_for_type_id(class_method.return_type_id),
                      ),
                    ),
                );
              }

              if interface_method.is_mutating() && !class_method.is_mutating() {
                project.add_diagnostic(
                  Diagnostic::error(
                    class_method.name_span,
                    format!("method {} is not mutating", class_method.name),
                  )
                  .with_hint(
                    interface_method.name_span,
                    format!("expected mutating method"),
                  ),
                );
              }
              if !interface_method.is_mutating() && class_method.is_mutating() {
                project.add_diagnostic(
                  Diagnostic::error(
                    class_method.name_span,
                    format!("method {} is mutating", class_method.name),
                  )
                  .with_hint(
                    interface_method.name_span,
                    format!("expected non-mutating method"),
                  ),
                );
              }

              if interface_method.visibility != class_method.visibility {
                project.add_diagnostic(
                  Diagnostic::error(class_method.name_span, format!("mismatched visibility"))
                    .with_hint(
                      interface_method.name_span,
                      format!(
                        "expected visibility {}",
                        match interface_method.visibility {
                          Visibility::Public => "public",
                          Visibility::Private => "private",
                          Visibility::Protected => "protected",
                        }
                      ),
                    ),
                );
              }

              // Check if the parameter types match
              if interface_method.params.len() != class_method.params.len() {
                project.add_diagnostic(
                  Diagnostic::error(
                    class_method.name_span,
                    "mismatched number of parameters".into(),
                  )
                  .with_hint(
                    interface_method.name_span,
                    format!("expected {} parameters", interface_method.params.len()),
                  ),
                );
              } else {
                for (interface_param, class_param) in interface_method
                  .params
                  .iter()
                  .zip(class_method.params.iter())
                {
                  if interface_param.type_id != class_param.type_id {
                    project.add_diagnostic(
                      Diagnostic::error(
                        class_param.span,
                        format!(
                          "incorrect parameter type for parameter {}",
                          class_method.name,
                        ),
                      )
                      .with_hint(
                        interface_param.span,
                        format!(
                          "expected `{}`, found `{}`",
                          project.typename_for_type_id(interface_param.type_id),
                          project.typename_for_type_id(class_param.type_id),
                        ),
                      ),
                    );
                  }
                }
              }

              method_found = true;
              break;
            }
          }
          if !method_found {
            found_methods.push(interface_method);
          }
        }

        if !found_methods.is_empty() {
          let mut diagnostic = Diagnostic::error(
            type_decl.name_span,
            "class does not implement all methods".into(),
          );
          for method in found_methods {
            diagnostic.with_hint(
              method.name_span,
              format!("missing method `{}`", method.name),
            );
          }
          project.add_diagnostic(diagnostic);
        }
      }

      let ParsedTypeDeclData::Class { methods, .. } = &type_decl.data else {
        panic!("internal error: get something other than a class while typechecking class methods");
      };
      for method in methods {
        typecheck_method(method, project, type_decl_id);
      }
    }
    ParsedTypeDeclData::Alias(_) => {}
  }
}

fn typecheck_function_predecl(
  function: &ParsedFunction,
  parent_scope_id: ScopeId,
  project: &mut Project,
) {
  let scope_id = project.create_scope(parent_scope_id);

  let mut checked_function = CheckedFunction {
    name: function.name.clone(),
    name_span: function.name_span,
    visibility: Visibility::Public,
    return_type_id: UNKNOWN_TYPE_ID,
    params: vec![],
    generic_parameters: vec![],
    scope_id,
    block: CheckedBlock::new(),
    linkage: function.linkage,
    attributes: vec![],
  };

  let checked_function_scope_id = checked_function.scope_id;

  let mut generic_params = vec![];
  for param in &function.type_parameters {
    let constraint = if let Some(constraint) = &param.constraint {
      Some(typecheck_typename(
        constraint,
        checked_function_scope_id,
        project,
      ))
    } else {
      None
    };
    project.types.push(CheckedType::TypeVariable(
      param.name.to_string(),
      constraint,
      param.span,
    ));
    let type_id = project.types.len() - 1;

    generic_params.push(GenericParameter::Parameter(type_id));

    project.add_type_to_scope(
      checked_function.scope_id,
      param.name.to_string(),
      type_id,
      param.span,
    );
  }

  checked_function.generic_parameters = generic_params;

  for param in &function.parameters {
    let param_type_id = typecheck_typename(&param.ty, checked_function_scope_id, project);
    let checked_param = CheckedVarDecl {
      name: param.name.clone(),
      type_id: param_type_id,
      mutable: param.mutable,
      span: param.span,
    };
    checked_function.params.push(checked_param);
  }

  let function_id = project.functions.len();
  project.functions.push(checked_function);

  project.add_function_to_scope(
    parent_scope_id,
    function.name.clone(),
    function_id,
    function.name_span,
  );
}

fn typecheck_function(function: &ParsedFunction, parent_scope_id: ScopeId, project: &mut Project) {
  let function_id = project
    .find_function_in_scope(parent_scope_id, &function.name)
    .expect("internal error: missing previously defined function");

  let checked_function = &mut project.functions[function_id];
  let function_scope_id = checked_function.scope_id;
  let function_linkage = checked_function.linkage;

  let mut param_vars = vec![];
  for param in &checked_function.params {
    param_vars.push(param.clone());
  }

  for var in param_vars.into_iter() {
    project.add_var_to_scope(function_scope_id, var, function.name_span);
  }

  let function_return_type_id = if let Some(return_type) = &function.return_type {
    typecheck_typename(return_type, function_scope_id, project)
  } else {
    VOID_TYPE_ID
  };

  let checked_function = &mut project.functions[function_id].clone();
  checked_function.return_type_id = function_return_type_id;

  let block = if let Some(block) = &function.body {
    typecheck_block(block, function_scope_id, project)
  } else {
    CheckedBlock::new()
  };

  let function_return_type_id = if let Some(return_type) = &function.return_type {
    typecheck_typename(return_type, function_scope_id, project)
  } else {
    VOID_TYPE_ID
  };

  let return_type_id = if function_return_type_id == UNKNOWN_TYPE_ID {
    if let Some(CheckedStatement::Return(ret)) = block.stmts.last() {
      ret.type_id(project)
    } else {
      VOID_TYPE_ID
    }
  } else {
    function_return_type_id
  };

  if function_linkage != DefinitionLinkage::External
    && return_type_id != VOID_TYPE_ID
    && !block.definitely_returns
  {
    project.add_diagnostic(Diagnostic::error(
      function.name_span,
      "control reaches end of non-void function".into(),
    ));
  }

  let mut checked_attributes = vec![];
  for attribute in function.attributes.iter() {
    match attribute {
      ParsedFunctionAttribute::CallConv(call_conv, _) => {
        let cc = match call_conv.to_lowercase().as_str() {
          "c" => CallingConvention::C,
          "fastcall" => CallingConvention::FastCall,
          "coldcall" => CallingConvention::ColdCall,
          _ => CallingConvention::Invalid,
        };
        checked_attributes.push(CheckedFunctionAttribute::CallConv(cc));
      }
      ParsedFunctionAttribute::NoReturn(_) => {
        checked_attributes.push(CheckedFunctionAttribute::NoReturn);
      }
    }
  }

  let checked_function = &mut project.functions[function_id];
  checked_function.block = block;
  checked_function.linkage = function_linkage;
  checked_function.attributes = checked_attributes;
  checked_function.return_type_id = return_type_id;
}

fn typecheck_method(function: &ParsedFunction, project: &mut Project, type_decl_id: TypeDeclId) {
  let type_decl = &mut project.type_decls[type_decl_id];
  let type_decl_scope_id = type_decl.scope_id;
  let type_decl_linkage = type_decl.linkage;

  let method_id = project
    .find_function_in_scope(type_decl_scope_id, &function.name)
    .expect("internal error: literally just pushed a checked function and it ain't here gng");

  let checked_function = &mut project.functions[method_id];
  let function_scope_id = checked_function.scope_id;

  for variable in checked_function.params.clone().into_iter() {
    project.add_var_to_scope(function_scope_id, variable, function.name_span);
  }

  let block = if let Some(block) = &function.body {
    typecheck_block(block, function_scope_id, project)
  } else {
    CheckedBlock::new()
  };

  let function_return_type_id = if let Some(return_type) = &function.return_type {
    typecheck_typename(return_type, function_scope_id, project)
  } else {
    VOID_TYPE_ID
  };

  let return_type_id = if function_return_type_id == UNKNOWN_TYPE_ID {
    if let Some(CheckedStatement::Return(ret)) = block.stmts.last() {
      ret.type_id(project)
    } else {
      VOID_TYPE_ID
    }
  } else {
    function_return_type_id
  };

  if type_decl_linkage != DefinitionLinkage::External
    && return_type_id != VOID_TYPE_ID
    && !block.definitely_returns
  {
    project.add_diagnostic(Diagnostic::error(
      function.name_span,
      "control reaches end of non-void function".into(),
    ));
  }

  let checked_function = &mut project.functions[method_id];
  checked_function.block = block;
  checked_function.return_type_id = return_type_id;
}

fn statement_definitely_returns(stmt: &CheckedStatement) -> bool {
  match stmt {
    CheckedStatement::Return(_) | CheckedStatement::Yield(_) | CheckedStatement::Break(_) => true,
    CheckedStatement::Block(block) => block.definitely_returns,
    CheckedStatement::While { block, .. } => block.definitely_returns,
    CheckedStatement::Loop(block) => block.definitely_returns,
    CheckedStatement::If {
      then_block,
      else_block: Some(else_block),
      ..
    } => then_block.definitely_returns && statement_definitely_returns(else_block),
    CheckedStatement::If {
      then_block,
      else_block: None,
      ..
    } => then_block.definitely_returns,
    _ => false,
  }
}

// fn substitute_typevars_in_type_decl(
//   type_decl_id: TypeDeclId,
//   specializations: &HashMap<TypeId, TypeId>,
//   span: Span,
//   project: &mut Project,
// ) {
//   let type_decl = &mut project.type_decls[type_decl_id];
//   let Some(class) = type_decl.kind.as_class() else {
//     unimplemented!("{:?}", type_decl.kind);
//   };

//   let fields = class.fields.clone();

//   let mut new_fields = vec![];
//   for index in 0..fields.len() {
//     let var = fields[index].clone();
//     let type_id = substitute_typevars_in_type(var.type_id, specializations, project);

//     new_fields.push(CheckedVarDecl {
//       name: var.name.clone(),
//       type_id,
//       mutable: var.mutable,
//       span: var.span,
//     });
//   }

//   // let type_decl_id = project.type_decls.len();
//   project.type_decls.push(CheckedTypeDecl {
//     name: type_decl.name.clone(),
//     generic_parameters: vec![],
//     kind: CheckedTypeKind::Class(CheckedClass {
//       implements: class.implements.clone(),
//       fields: new_fields,
//       methods: class.methods.clone(),
//     }),
//     scope_id: type_decl.scope_id,
//     linkage: type_decl.linkage,
//   });
// }

fn typecheck_block(block: &ParsedBlock, scope_id: ScopeId, project: &mut Project) -> CheckedBlock {
  let mut checked_block = CheckedBlock::new();
  for stmt in &block.stmts {
    let checked_stmt = typecheck_statement(stmt, scope_id, project);
    if statement_definitely_returns(&checked_stmt) {
      checked_block.definitely_returns = true;
    }

    checked_block.stmts.push(checked_stmt);
  }

  checked_block
}

fn typecheck_inline_asm_parameter(
  operand: &inline_asm::Operand,
  scope_id: ScopeId,
  project: &mut Project,
) -> CheckedInlineAsmOperand {
  CheckedInlineAsmOperand {
    operand_type: operand.operand_type.clone(),
    action: match &operand.action {
      inline_asm::OperandAction::None => CheckedInlineAsmOperandAction::None,
      inline_asm::OperandAction::In => CheckedInlineAsmOperandAction::In,
      inline_asm::OperandAction::Out(parsed_type) => {
        let type_id = typecheck_typename(parsed_type, scope_id, project);
        CheckedInlineAsmOperandAction::Out(type_id)
      }
    },
    register: operand.register.clone(),
  }
}

fn typecheck_statement(
  stmt: &ParsedStatement,
  scope_id: ScopeId,
  project: &mut Project,
) -> CheckedStatement {
  match stmt {
    ParsedStatement::VarDecl(var_decl, expr, _span) => {
      let mut checked_type_id = typecheck_typename(&var_decl.ty, scope_id, project);
      let checked_expr = typecheck_expression(expr, scope_id, Some(checked_type_id), project);
      if checked_type_id == UNKNOWN_TYPE_ID {
        checked_type_id = checked_expr.type_id(project);
      }

      let checked_var_decl = CheckedVarDecl {
        name: var_decl.name.clone(),
        type_id: checked_type_id,
        mutable: var_decl.mutable,
        span: var_decl.span,
      };

      project.add_var_to_scope(scope_id, checked_var_decl.clone(), checked_var_decl.span);

      CheckedStatement::VarDecl(checked_var_decl, checked_expr)
    }

    ParsedStatement::Block(block) => {
      let block = typecheck_block(block, scope_id, project);
      CheckedStatement::Block(block)
    }

    ParsedStatement::If(condition, then_block, else_block, span) => {
      let condition = typecheck_expression(condition, scope_id, None, project);
      let type_id = condition.type_id(project);
      if type_id != BOOL_TYPE_ID {
        project.add_diagnostic(Diagnostic::error(
          *span,
          format!(
            "if condition must be a boolean, found `{}`",
            project.typename_for_type_id(type_id)
          ),
        ));
      }

      let then_block = typecheck_block(then_block, scope_id, project);

      let mut checked_else_block = None;
      if let Some(else_block) = else_block {
        checked_else_block = Some(typecheck_statement(else_block, scope_id, project));
      }

      CheckedStatement::If {
        condition,
        then_block,
        else_block: checked_else_block.map(Box::new),
      }
    }

    ParsedStatement::While(condition, block, _span) => {
      let condition = typecheck_expression(condition, scope_id, None, project);

      let block = typecheck_block(block, scope_id, project);

      CheckedStatement::While { condition, block }
    }

    ParsedStatement::Loop(block, _span) => {
      let block = typecheck_block(block, scope_id, project);
      CheckedStatement::Loop(block)
    }

    ParsedStatement::Break(span) => CheckedStatement::Break(*span),

    ParsedStatement::Continue(span) => CheckedStatement::Continue(*span),

    ParsedStatement::Return(expr, _span) => {
      let expr = typecheck_expression(
        expr,
        scope_id,
        project
          .current_function_index
          .map(|i| project.functions[i].return_type_id),
        project,
      );

      CheckedStatement::Return(expr)
    }

    ParsedStatement::Yield(expr, _span) => {
      let expr = typecheck_expression(
        expr,
        scope_id,
        project
          .current_function_index
          .map(|i| project.functions[i].return_type_id),
        project,
      );

      CheckedStatement::Return(expr)
    }

    ParsedStatement::Expression(expr) => {
      let expr = typecheck_expression(expr, scope_id, None, project);

      CheckedStatement::Expression(expr)
    }
  }
}

fn resolve_call<'a>(
  call: &ParsedCall,
  namespaces: &mut [ResolvedNamespace],
  span: &Span,
  scope_id: ScopeId,
  project: &'a mut Project,
) -> Option<CheckedFunction> {
  if let Some(namespace) = call.namespace.first() {
    if let Some(type_decl_id) = project.find_type_decl_in_scope(scope_id, namespace) {
      let type_decl = &project.type_decls[type_decl_id];

      if let Some(type_decl_id) = project.find_type_decl_in_scope(type_decl.scope_id, &call.name) {
        let type_decl = &project.type_decls[type_decl_id];

        if let Some(function_id) = project.find_function_in_scope(type_decl.scope_id, &call.name) {
          return Some(project.functions[function_id].clone());
        }
      } else if let Some(function_id) =
        project.find_function_in_scope(type_decl.scope_id, &call.name)
      {
        return Some(project.functions[function_id].clone());
      }

      if type_decl.generic_parameters.is_empty() {
        namespaces[0].generic_parameters = Some(type_decl.generic_parameters.clone());
      }

      None
    } else if let Some(enum_id) = project.find_enum_in_scope(scope_id, namespace) {
      let enum_ = &project.enums[enum_id];

      if let Some(function_id) = project.find_function_in_scope(enum_.scope_id, &call.name) {
        return Some(project.functions[function_id].clone());
      }

      if !enum_.generic_parameters.is_empty() {
        namespaces[0].generic_parameters = Some(enum_.generic_parameters.clone());
      }

      None
    } else if let Some(scope_id) = project.find_namespace_in_scope(scope_id, namespace) {
      if let Some(type_decl_id) = project.find_type_decl_in_scope(scope_id, &call.name) {
        let type_decl = &project.type_decls[type_decl_id];

        if let Some(function_id) = project.find_function_in_scope(type_decl.scope_id, &call.name) {
          return Some(project.functions[function_id].clone());
        }
      } else if let Some(function_id) = project.find_function_in_scope(scope_id, &call.name) {
        return Some(project.functions[function_id].clone());
      }

      None
    } else {
      project.add_diagnostic(Diagnostic::error(
        *span,
        format!("unknown namespace or type declaration `{}`", namespace),
      ));
      None
    }
  } else {
    if let Some(type_decl_id) = project.find_type_decl_in_scope(scope_id, &call.name) {
      let type_decl = &project.type_decls[type_decl_id];

      if let Some(function_id) = project.find_function_in_scope(type_decl.scope_id, &call.name) {
        return Some(project.functions[function_id].clone());
      }
    } else if let Some(function_id) = project.find_function_in_scope(scope_id, &call.name) {
      return Some(project.functions[function_id].clone());
    }

    project.add_diagnostic(Diagnostic::error(
      *span,
      format!("call to unknown function `{}`", call.name),
    ));
    None
  }
}

fn typecheck_call(
  call: &ParsedCall,
  caller_scope_id: ScopeId,
  span: &Span,
  project: &mut Project,
  this_expr: Option<&CheckedExpression>,
  type_decl_id: Option<TypeDeclId>,
  type_hint: Option<TypeId>,
) -> CheckedCall {
  let mut checked_args = Vec::new();
  let mut return_type_id = UNKNOWN_TYPE_ID;
  let mut linkage = DefinitionLinkage::Internal;
  let mut generic_substitutions = HashMap::new();
  let mut type_args = vec![];
  let mut resolved_namespaces = call
    .namespace
    .iter()
    .map(|n| ResolvedNamespace {
      name: n.clone(),
      generic_parameters: None,
    })
    .collect::<Vec<_>>();

  let callee_scope_id = match type_decl_id {
    Some(type_decl_id) => project.type_decls[type_decl_id].scope_id,
    _ => caller_scope_id,
  };

  let callee = resolve_call(
    call,
    &mut resolved_namespaces,
    span,
    callee_scope_id,
    project,
  );

  if let Some(ref callee) = callee {
    let callee = callee.clone();

    if callee.visibility != Visibility::Public
      && !Scope::can_access(caller_scope_id, callee.scope_id, project)
    {
      project.add_diagnostic(Diagnostic::error(
        *span,
        format!(
          "cannot access function `{}` from scope `{:?}`",
          callee.name, project.scopes[caller_scope_id].namespace_name
        ),
      ));
    }

    return_type_id = callee.return_type_id;
    linkage = callee.linkage;

    for (idx, type_arg) in call.type_args.iter().enumerate() {
      let checked_type = typecheck_typename(type_arg, caller_scope_id, project);

      if callee.generic_parameters.len() <= idx {
        project.add_diagnostic(Diagnostic::error(
          *span,
          "trying to access generic parameter out of bounds".to_string(),
        ));
      }

      let typevar_type_id = match callee.generic_parameters[idx] {
        GenericParameter::Parameter(typevar_type_id) => typevar_type_id,
        GenericParameter::InferenceGuide(typevar_type_id) => typevar_type_id,
      };
      generic_substitutions.insert(typevar_type_id, checked_type);
    }

    {
      if !callee.generic_parameters.is_empty() {
        let mut function = callee.clone();
        function.return_type_id =
          substitute_typevars_in_type(function.return_type_id, &generic_substitutions, project);
        for param in &mut function.params {
          param.type_id =
            substitute_typevars_in_type(param.type_id, &generic_substitutions, project);
        }
        function.generic_parameters = vec![];

        function.name = mangle_name(&function, project);

        substitute_typevars_in_block(&mut function.block, &generic_substitutions, project);

        if project
          .find_function_in_scope(callee_scope_id, &function.name)
          .is_none()
        {
          let function_id = project.functions.len();
          project.functions.push(function.clone());
          project.add_function_to_scope(
            callee_scope_id,
            function.name.clone(),
            function_id,
            function.name_span,
          );
        }

        // dbg!(&function);
      }
    }

    if let Some(this_expr) = this_expr {
      let type_id = this_expr.type_id(project);
      let param_type = &project.types[type_id];

      if let CheckedType::GenericInstance(type_decl_id, args, _span) = param_type {
        let type_decl = &project.type_decls[*type_decl_id];

        let mut idx = 0;
        while idx < type_decl.generic_parameters.len() {
          generic_substitutions.insert(type_decl.generic_parameters[idx], args[idx]);
          idx += 1;
        }
      }

      if callee.is_static() {
        project.add_diagnostic(Diagnostic::error(
          *span,
          "cannot call static method on an instance of a class".into(),
        ));
      }

      if callee.is_mutating() && !this_expr.is_mutable() {
        project.add_diagnostic(Diagnostic::error(
          *span,
          "cannot call a mutating method on an immutable class instance".into(),
        ));
      }
    }

    let arg_offset = if this_expr.is_some() { 1 } else { 0 };

    if callee.params.len() != (call.args.len() + arg_offset) {
      project.add_diagnostic(Diagnostic::error(
        *span,
        format!(
          "expected {} arguments but got {}",
          callee.params.len(),
          call.args.len()
        ),
      ));
    } else {
      let mut idx = 0;
      while idx < call.args.len() {
        let mut checked_arg = typecheck_expression(&call.args[idx], caller_scope_id, None, project);

        let callee = resolve_call(
          call,
          &mut resolved_namespaces,
          span,
          callee_scope_id,
          project,
        )
        .expect("internal error: previously resolved call is now unresolved");

        let lhs_type_id = callee.params[idx + arg_offset].type_id;
        try_promote_constant_expr_to_type(lhs_type_id, &mut checked_arg, span, project);

        let lhs_type_id = callee.params[idx + arg_offset].type_id;
        let rhs_type_id = checked_arg.type_id(project);

        check_types_for_compat(
          lhs_type_id,
          rhs_type_id,
          &mut generic_substitutions,
          call.args[idx].span(),
          project,
        );
        checked_args.push(checked_arg);

        idx += 1;
      }
    }

    if let Some(type_hint_id) = type_hint {
      if type_hint_id != UNKNOWN_TYPE_ID {
        check_types_for_compat(
          type_hint_id,
          return_type_id,
          &mut generic_substitutions,
          *span,
          project,
        );
      }
    }
    return_type_id = substitute_typevars_in_type(return_type_id, &generic_substitutions, project);

    resolved_namespaces = resolved_namespaces
      .iter()
      .map(|n| ResolvedNamespace {
        name: n.name.clone(),
        generic_parameters: n.generic_parameters.as_ref().map(|p| {
          p.iter()
            .map(|type_id| substitute_typevars_in_type(*type_id, &generic_substitutions, project))
            .collect()
        }),
      })
      .collect();

    for generic_typevar in &callee.generic_parameters {
      if let GenericParameter::Parameter(id) = generic_typevar {
        if let Some(substitution) = generic_substitutions.get(id) {
          type_args.push(*substitution)
        } else {
          project.add_diagnostic(Diagnostic::error(
            *span,
            "not all generic parameters have known types".into(),
          ));
        }
      }
    }
  }

  let name = if !type_args.is_empty() {
    let mut mangled_name = format!("_{}{}", call.name.len(), call.name.clone());
    for arg in checked_args.clone() {
      let type_id = arg.type_id(project);
      mangled_name.push_str(&mangle_type(type_id, project));
    }
    mangled_name
  } else {
    call.name.clone()
  };

  CheckedCall {
    namespace: resolved_namespaces,
    name,
    args: checked_args,
    type_args,
    linkage,
    type_id: return_type_id,
  }
}

fn is_integer(type_id: TypeId) -> bool {
  matches!(
    type_id,
    crate::compiler::I8_TYPE_ID
      | crate::compiler::I16_TYPE_ID
      | crate::compiler::I32_TYPE_ID
      | crate::compiler::I64_TYPE_ID
      | crate::compiler::I128_TYPE_ID
      | crate::compiler::ISZ_TYPE_ID
      | crate::compiler::U8_TYPE_ID
      | crate::compiler::U16_TYPE_ID
      | crate::compiler::U32_TYPE_ID
      | crate::compiler::U64_TYPE_ID
      | crate::compiler::U128_TYPE_ID
      | crate::compiler::USZ_TYPE_ID
      | crate::compiler::CCHAR_TYPE_ID
  )
}

fn try_promote_constant_expr_to_type(
  lhs_type_id: TypeId,
  checked_rhs: &mut CheckedExpression,
  span: &Span,
  project: &mut Project,
) {
  if !is_integer(lhs_type_id) {
    return;
  }

  if let Some(rhs_constant) = checked_rhs.to_integer_constant() {
    if let (Some(new_constant), new_type_id) = rhs_constant.promote(lhs_type_id) {
      *checked_rhs = CheckedExpression::NumericConstant(new_constant, new_type_id, *span);
    } else {
      project.add_diagnostic(Diagnostic::error(
        *span,
        "integer promotion failed".to_string(),
      ));
    }
  }
}

fn typecheck_expression(
  expr: &ParsedExpression,
  scope_id: ScopeId,
  type_hint_id: Option<TypeId>,
  project: &mut Project,
) -> CheckedExpression {
  let unify_with_type_hint = |project: &mut Project, type_id: &TypeId| -> TypeId {
    if let Some(type_hint_id) = type_hint_id {
      if type_hint_id == UNKNOWN_TYPE_ID {
        return *type_id;
      }

      let mut inferences = HashMap::new();
      let x = check_types_for_compat(
        type_hint_id,
        *type_id,
        &mut inferences,
        expr.span(),
        project,
      );
      if x.is_none() {
        return *type_id;
      }

      return substitute_typevars_in_type(*type_id, &inferences, project);
    }

    *type_id
  };

  match expr {
    // ParsedExpression::Null(span) => Ok(CheckedExpression::Null(*span, type_id)),
    ParsedExpression::Nullptr(span) => {
      let mut type_id = type_hint_id.unwrap_or(RAWPTR_TYPE_ID);
      if let Some(UNKNOWN_TYPE_ID) = type_hint_id {
        type_id = RAWPTR_TYPE_ID;
      }
      CheckedExpression::Nullptr(*span, type_id)
    }

    ParsedExpression::NumericConstant(constant, span) => {
      // let type_id = unify_with_type_hint(project, &constant.type_id());
      let type_id = constant.type_id();
      CheckedExpression::NumericConstant(constant.clone(), type_id, *span)
    }

    ParsedExpression::Boolean(value, span) => CheckedExpression::Boolean(*value, *span),

    ParsedExpression::QuotedString(s, span) => CheckedExpression::QuotedString(s.clone(), *span),

    ParsedExpression::QuotedCString(s, span) => CheckedExpression::QuotedCString(s.clone(), *span),

    ParsedExpression::CharacterLiteral(c, span) => CheckedExpression::CharacterLiteral(*c, *span),

    ParsedExpression::Var(name, span) => {
      if let Some(var) = project.find_var_in_scope(scope_id, name) {
        let _ = unify_with_type_hint(project, &var.type_id);
        return CheckedExpression::Variable(var, *span);
      } else if let Some(constant) = project.find_constant_in_scope(scope_id, name) {
        let var_decl = CheckedVarDecl {
          name: constant.name.clone(),
          type_id: unify_with_type_hint(project, &constant.type_id),
          mutable: false,
          span: constant.span,
        };
        return CheckedExpression::Variable(var_decl, *span);
      } else {
        project.add_diagnostic(Diagnostic::error(
          *span,
          format!("undeclared variable `{}`", name),
        ));
        return CheckedExpression::Garbage(*span);
      }
    }

    ParsedExpression::Range(lhs, rhs, span) => {
      let mut checked_lhs = typecheck_expression(lhs, scope_id, None, project);
      let lhs_type_id = checked_lhs.type_id(project);
      try_promote_constant_expr_to_type(lhs_type_id, &mut checked_lhs, span, project);

      let mut checked_rhs = typecheck_expression(rhs, scope_id, None, project);
      let rhs_type_id = checked_rhs.type_id(project);
      try_promote_constant_expr_to_type(rhs_type_id, &mut checked_rhs, span, project);

      if lhs_type_id != rhs_type_id {
        project.add_diagnostic(Diagnostic::error(
          *span,
          format!(
            "range bounds must be of the same type, found `{}` and `{}`",
            project.typename_for_type_id(lhs_type_id),
            project.typename_for_type_id(rhs_type_id)
          ),
        ));
      }
      if !is_integer(lhs_type_id) {
        project.add_diagnostic(Diagnostic::error(
          *span,
          format!(
            "range bounds must be integers, found `{}`",
            project.typename_for_type_id(lhs_type_id)
          ),
        ));
      }

      let CheckedExpression::NumericConstant(ref lhs, _, _) = checked_lhs else {
        unreachable!()
      };
      let lhs = lhs.integer_constant().unwrap().to_usize();

      let CheckedExpression::NumericConstant(ref rhs, _, _) = checked_rhs else {
        unreachable!()
      };
      let rhs = rhs.integer_constant().unwrap().to_usize();

      let type_id = project.find_or_add_type_id(CheckedType::Slice(lhs_type_id, rhs - lhs, *span));

      let type_id = unify_with_type_hint(project, &type_id);

      CheckedExpression::Range(Box::new(checked_lhs), Box::new(checked_rhs), type_id, *span)
    }

    ParsedExpression::Call(call, span) => {
      let checked_call = typecheck_call(call, scope_id, span, project, None, None, type_hint_id);

      let type_id = unify_with_type_hint(project, &checked_call.type_id);

      CheckedExpression::Call(checked_call, type_id, *span)
    }

    ParsedExpression::MethodCall(expr, call, span) => {
      if let ParsedExpression::Var(enum_name, _) = *expr.clone() {
        let enum_id = project.find_enum_in_scope(scope_id, &enum_name);
        if let Some(enum_id) = enum_id {
          let enum_ = &project.enums[enum_id];
          for variant in enum_.variants.iter() {
            if variant.name() == call.name {
              let variant_name = variant.clone().name();
              let type_id = project.find_type_in_scope(scope_id, &enum_name);
              if let Some(type_id) = type_id {
                let args = call.args.clone();
                let mut checked_args = vec![];
                for arg in args.iter() {
                  let expr = typecheck_expression(arg, scope_id, None, project);
                  checked_args.push(expr);
                }
                return CheckedExpression::IndexedEnum(
                  enum_id,
                  variant_name,
                  checked_args,
                  type_id,
                  *span,
                );
              }
            }
          }

          project.add_diagnostic(Diagnostic::error(
            *span,
            format!("unknown variant {} in {}", call.name, enum_name),
          ));
          return CheckedExpression::Garbage(*span);
        }
      }

      let checked_expr = typecheck_expression(expr, scope_id, None, project);
      let checked_expr_type_id = checked_expr.type_id(project);

      let checked_expr_type = &project.types[checked_expr_type_id];
      match checked_expr_type {
        CheckedType::TypeDecl(type_decl_id, _) => {
          let type_decl_id = *type_decl_id;
          let checked_call = typecheck_call(
            call,
            scope_id,
            span,
            project,
            Some(&checked_expr),
            Some(type_decl_id),
            type_hint_id,
          );

          let type_id = unify_with_type_hint(project, &checked_call.type_id);
          CheckedExpression::MethodCall(Box::new(checked_expr), checked_call, type_id, *span)
        }
        CheckedType::GenericInstance(struct_id, _, _) => {
          // ignore the inner types for now, but we'll need them in the future
          let struct_id = *struct_id;
          let checked_call = typecheck_call(
            call,
            scope_id,
            span,
            project,
            Some(&checked_expr),
            Some(struct_id),
            type_hint_id,
          );

          let type_id = unify_with_type_hint(project, &checked_call.type_id);

          CheckedExpression::MethodCall(Box::new(checked_expr), checked_call, type_id, *span)
        }
        _ => {
          project.add_diagnostic(Diagnostic::error(
            expr.span(),
            format!(
              "no methods available on type `{}`",
              project.typename_for_type_id(checked_expr_type_id)
            ),
          ));
          CheckedExpression::Garbage(*span)
        }
      }
    }

    ParsedExpression::BinaryOp(lhs, op, rhs, span) => {
      let checked_lhs = typecheck_expression(lhs, scope_id, None, project);
      let mut checked_rhs = typecheck_expression(rhs, scope_id, None, project);

      let mut op = op.clone();

      match op {
        BinaryOperator::AddAssign
        | BinaryOperator::SubtractAssign
        | BinaryOperator::MultiplyAssign
        | BinaryOperator::DivideAssign
        | BinaryOperator::ModuloAssign => {
          let new_rhs = CheckedExpression::BinaryOp(
            Box::new(checked_lhs.clone()),
            match op {
              BinaryOperator::AddAssign => BinaryOperator::Add,
              BinaryOperator::SubtractAssign => BinaryOperator::Subtract,
              BinaryOperator::MultiplyAssign => BinaryOperator::Multiply,
              BinaryOperator::DivideAssign => BinaryOperator::Divide,
              BinaryOperator::ModuloAssign => BinaryOperator::Modulo,
              _ => unreachable!(),
            },
            Box::new(checked_rhs.clone()),
            checked_lhs.type_id(project),
            *span,
          );
          checked_rhs = new_rhs;
          op = BinaryOperator::Assign;
        }

        _ => {}
      };

      try_promote_constant_expr_to_type(
        checked_lhs.type_id(project),
        &mut checked_rhs,
        span,
        project,
      );

      let type_id =
        typecheck_binary_operator(&checked_lhs, op.clone(), &checked_rhs, *span, project);
      let type_id = unify_with_type_hint(project, &type_id);

      CheckedExpression::BinaryOp(
        Box::new(checked_lhs),
        op.clone(),
        Box::new(checked_rhs),
        type_id,
        *span,
      )
    }

    ParsedExpression::UnaryOp(expr, op, span) => {
      let checked_expr = typecheck_expression(expr, scope_id, None, project);

      let checked_op = match op {
        UnaryOperator::As(type_name) => {
          let type_id = typecheck_typename(type_name, scope_id, project);
          CheckedUnaryOperator::As(type_id)
        }
        UnaryOperator::Dereference => CheckedUnaryOperator::Dereference,
        UnaryOperator::AddressOf => CheckedUnaryOperator::AddressOf,
        UnaryOperator::Negate => CheckedUnaryOperator::Negate,
        UnaryOperator::Not => CheckedUnaryOperator::Not,
        UnaryOperator::BitwiseNot => CheckedUnaryOperator::BitwiseNot,
      };

      let checked_expr = typecheck_unary_operation(&checked_expr, checked_op, *span, project);
      checked_expr
    }

    ParsedExpression::IndexedExpression(expr, idx, span) => {
      let checked_expr = typecheck_expression(expr, scope_id, None, project);
      let checked_idx = typecheck_expression(idx, scope_id, None, project);

      let expr_type_id = checked_expr.type_id(project);

      match project.types[expr_type_id] {
        CheckedType::RawPtr(type_id, span) => {
          let expr_type_id = unify_with_type_hint(project, &type_id);
          CheckedExpression::IndexedExpression(
            Box::new(checked_expr),
            Box::new(checked_idx),
            expr_type_id,
            span,
          )
        }

        _ => {
          project.add_diagnostic(Diagnostic::error(
            *span,
            format!(
              "cannot index type {}",
              project.typename_for_type_id(expr_type_id)
            ),
          ));
          CheckedExpression::Garbage(*span)
        }
      }
    }

    ParsedExpression::IndexedStruct(expr, name, span) => {
      if let ParsedExpression::Var(var_name, _) = *expr.clone() {
        if let Some(enum_id) = project.find_enum_in_scope(scope_id, &var_name) {
          let r#enum = &project.enums[enum_id];
          let span = r#enum.span;

          let Some(type_id) = project.find_type_in_scope(scope_id, &r#enum.name) else {
            project.add_diagnostic(Diagnostic::error(span, format!("")));
            return CheckedExpression::Garbage(span);
          };

          for member in &r#enum.variants {
            let (member_name, member_span) = match member {
              CheckedEnumVariant::Untyped(name, span) => (name, span),
              CheckedEnumVariant::WithValue(name, _, span) => (name, span),
              CheckedEnumVariant::TupleLike(name, _, span) => (name, span),
              CheckedEnumVariant::StructLike(name, _, span) => (name, span),
            };
            if member_name == name {
              return CheckedExpression::IndexedEnum(
                enum_id,
                name.to_string(),
                vec![],
                type_id,
                *member_span,
              );
            }
          }

          project.add_diagnostic(Diagnostic::error(
            span,
            format!("unknown member of enum: {}.{}", r#enum.name, name),
          ));
          return CheckedExpression::Garbage(span);
        }
      }

      let checked_expr = typecheck_expression(expr, scope_id, None, project);

      let checked_expr_type_id = checked_expr.type_id(project);
      let checked_expr_type = &project.types[checked_expr_type_id];
      match checked_expr_type {
        CheckedType::GenericInstance(type_decl_id, _, _)
        | CheckedType::TypeDecl(type_decl_id, _) => {
          let type_decl = &project.type_decls[*type_decl_id];
          let CheckedTypeKind::Class(class) = &type_decl.kind else {
            project.add_diagnostic(Diagnostic::error(
              *span,
              format!("cannot index struct {}", type_decl.name),
            ));
            return CheckedExpression::Garbage(*span);
          };

          for member in &class.fields {
            if &member.name == name {
              return CheckedExpression::IndexedStruct(
                Box::new(checked_expr),
                name.to_string(),
                *type_decl_id,
                member.type_id,
                *span,
              );
            }
          }

          project.add_diagnostic(Diagnostic::error(
            *span,
            format!("unknown member of struct: {}.{}", type_decl.name, name),
          ));
        }

        _ => {
          project.add_diagnostic(Diagnostic::error(
            *span,
            "member access of non-struct value".to_string(),
          ));
        }
      }

      CheckedExpression::Garbage(*span)
    }

    ParsedExpression::InlineAsm {
      volatile,
      asm,
      bindings,
      clobbers,
      span,
    } => {
      let mut type_ids = vec![];
      let mut checked_bindings = vec![];
      for binding in bindings {
        let checked_expr = typecheck_expression(&binding.var, scope_id, None, project);
        let operand = typecheck_inline_asm_parameter(&binding.parameter, scope_id, project);

        match operand.action {
          CheckedInlineAsmOperandAction::None => {}
          CheckedInlineAsmOperandAction::In => {}
          CheckedInlineAsmOperandAction::Out(type_id) => {
            if checked_expr.type_id(project) != type_id {
              project.add_diagnostic(Diagnostic::error(
                binding.var.span(),
                "mismatched type for inline asm binding".into(),
              ));
            }

            type_ids.push(type_id);
          }
        }

        checked_bindings.push(CheckedInlineAsmBinding {
          var: checked_expr,
          operand,
          span: binding.var.span(),
        });
      }

      let mut checked_clobbers = vec![];
      for clobber in clobbers {
        let checked_clobber = typecheck_inline_asm_parameter(clobber, scope_id, project);
        checked_clobbers.push(checked_clobber);
      }

      let mut type_id = UNKNOWN_TYPE_ID;
      if type_ids.len() == 1 {
        type_id = type_ids[0];
      } else if type_ids.len() > 1 {
        project.add_diagnostic(Diagnostic::error(
          *span,
          "inline asm has multiple return types".into(),
        ));
      }

      if type_id == UNKNOWN_TYPE_ID {
        type_id = unify_with_type_hint(project, &type_id);
      }

      CheckedExpression::InlineAsm {
        volatile: *volatile,
        asm: asm.clone(),
        bindings: checked_bindings,
        clobbers: checked_clobbers,
        type_id,
        span: *span,
      }
    }

    ParsedExpression::Switch {
      condition,
      cases,
      default,
      span,
    } => {
      let checked_condition = typecheck_expression(&condition, scope_id, None, project);
      let condition_type_id = checked_condition.type_id(project);

      let mut type_id = None;
      let mut checked_cases = vec![];
      for (case, block) in cases.iter() {
        let checked_case = typecheck_expression(&case, scope_id, None, project);
        let case_type_id = checked_case.type_id(project);

        if case_type_id != condition_type_id {
          project.add_diagnostic(Diagnostic::error(
            case.span(),
            format!(
              "type mismatch: expected {}, but got {}",
              project.typename_for_type_id(condition_type_id),
              project.typename_for_type_id(case_type_id),
            ),
          ));
        }

        let checked_block = typecheck_block(block, scope_id, project);
        let block_type_id = checked_block.type_id(project);

        if let Some(type_id) = type_id {
          if check_types_for_compat(block_type_id, type_id, &mut HashMap::new(), *span, project)
            .is_none()
          {
            project.add_diagnostic(Diagnostic::error(
              case.span(),
              format!(
                "type mismatch: expected {}, but got {}",
                project.typename_for_type_id(type_id),
                project.typename_for_type_id(block_type_id),
              ),
            ));
          }
        } else {
          type_id = Some(block_type_id);
        }

        checked_cases.push((Box::new(checked_case), checked_block));
      }

      CheckedExpression::Switch {
        value: Box::new(checked_condition),
        cases: checked_cases,
        default: None,
        type_id: type_id.unwrap_or(UNKNOWN_TYPE_ID),
        span: *span,
      }
    }

    ParsedExpression::Grouped(inner, _) => {
      typecheck_expression(&*inner, scope_id, type_hint_id, project)
    }

    _ => todo!("typecheck_expression: {:?}", expr),
  }
}

fn typecheck_unary_operation(
  expr: &CheckedExpression,
  op: CheckedUnaryOperator,
  span: Span,
  project: &mut Project,
) -> CheckedExpression {
  let types = &project.types.clone();
  let expr_type_id = expr.type_id(project);
  let expr_type = &types[expr_type_id];

  match op {
    CheckedUnaryOperator::As(type_id) => {
      let cast_type = &types[type_id];
      match (expr_type, cast_type) {
        (CheckedType::Builtin(_), CheckedType::Builtin(_))
          if is_integer(expr_type_id) && is_integer(type_id) => {}
        (CheckedType::Builtin(_), CheckedType::RawPtr(_, _))
          if is_integer(expr_type_id) || expr_type_id == RAWPTR_TYPE_ID => {}
        (CheckedType::RawPtr(_, _), CheckedType::Builtin(_))
          if is_integer(type_id) || type_id == RAWPTR_TYPE_ID => {}
        (CheckedType::RawPtr(lhs_inner_id, _), CheckedType::RawPtr(rhs_inner_id, _)) => {
          if lhs_inner_id != rhs_inner_id {
            project.add_diagnostic(Diagnostic::warning(
              span,
              format!(
                "unchecked pointer cast from {} to {}",
                project.typename_for_type_id(expr_type_id),
                project.typename_for_type_id(type_id),
              ),
            ));
          }
        }

        (
          CheckedType::TypeDecl(lhs_type_decl_id, _),
          CheckedType::TypeDecl(rhs_type_decl_id, _),
        ) => {
          if !project.is_class(*lhs_type_decl_id) && !project.is_interface(*rhs_type_decl_id) {
            project.add_diagnostic(Diagnostic::error(
              span,
              format!(
                "cannot cast {} to {}",
                project.typename_for_type_id(expr_type_id),
                project.typename_for_type_id(type_id),
              ),
            ));
          } else {
            let lhs_type_decl = &project.type_decls[*lhs_type_decl_id];
            if !lhs_type_decl.implements(*rhs_type_decl_id) {
              project.add_diagnostic(Diagnostic::error(
                span,
                format!(
                  "cannot cast {} to {}",
                  project.typename_for_type_id(expr_type_id),
                  project.typename_for_type_id(type_id),
                ),
              ));
            }
          }
        }

        (CheckedType::Enum(enum_id, _), _) => {
          let enum_ = &project.enums[*enum_id];
          if let Some(underlying_type_id) = enum_.underlying_type_id {
            if underlying_type_id != type_id {
              project.add_diagnostic(Diagnostic::error(
                span,
                format!(
                  "cannot cast {} to {}",
                  project.typename_for_type_id(expr_type_id),
                  project.typename_for_type_id(type_id),
                ),
              ));
            }
          } else {
            project.add_diagnostic(Diagnostic::error(
              span,
              format!(
                "cannot cast {} to {}",
                project.typename_for_type_id(expr_type_id),
                project.typename_for_type_id(type_id),
              ),
            ));
          }
        }

        _ => {
          project.add_diagnostic(Diagnostic::error(
            span,
            format!(
              "cannot cast {} to {}",
              project.typename_for_type_id(expr_type_id),
              project.typename_for_type_id(type_id),
            ),
          ));
        }
      }

      CheckedExpression::UnaryOp(Box::new(expr.clone()), op, type_id, span)
    }
    CheckedUnaryOperator::Dereference => match expr_type {
      CheckedType::RawPtr(x, _) => CheckedExpression::UnaryOp(Box::new(expr.clone()), op, *x, span),
      _ => {
        project.add_diagnostic(Diagnostic::error(
          span,
          "dereference of a non-pointer value".to_string(),
        ));
        CheckedExpression::Garbage(span)
      }
    },
    CheckedUnaryOperator::AddressOf => {
      let type_id = expr.type_id(project);
      let type_id = project.find_or_add_type_id(CheckedType::RawPtr(type_id, span));
      CheckedExpression::UnaryOp(Box::new(expr.clone()), op, type_id, span)
    }
    CheckedUnaryOperator::Negate => {
      if !is_integer(expr_type_id) {
        project.add_diagnostic(Diagnostic::error(
          span,
          format!(
            "cannot negate type {}",
            project.typename_for_type_id(expr_type_id)
          ),
        ));
      }

      CheckedExpression::UnaryOp(Box::new(expr.clone()), op, expr_type_id, span)
    }
    CheckedUnaryOperator::Not => {
      if expr_type_id != BOOL_TYPE_ID {
        project.add_diagnostic(Diagnostic::error(
          span,
          format!(
            "cannot negate type {}",
            project.typename_for_type_id(expr_type_id)
          ),
        ));
      }

      CheckedExpression::UnaryOp(Box::new(expr.clone()), op, expr_type_id, span)
    }
    CheckedUnaryOperator::BitwiseNot => {
      if !is_integer(expr_type_id) {
        project.add_diagnostic(Diagnostic::error(
          span,
          format!(
            "cannot negate type {}",
            project.typename_for_type_id(expr_type_id)
          ),
        ));
      }

      CheckedExpression::UnaryOp(Box::new(expr.clone()), op, expr_type_id, span)
    }
  }
}

fn typecheck_binary_operator(
  lhs: &CheckedExpression,
  op: BinaryOperator,
  rhs: &CheckedExpression,
  span: Span,
  project: &mut Project,
) -> TypeId {
  let lhs_type_id = lhs.type_id(project);
  let rhs_type_id = rhs.type_id(project);

  let mut type_id = lhs.type_id(project);
  match op {
    BinaryOperator::LessThan
    | BinaryOperator::LessThanEquals
    | BinaryOperator::GreaterThan
    | BinaryOperator::GreaterThanEquals
    | BinaryOperator::Equals
    | BinaryOperator::NotEquals => {
      if lhs_type_id != rhs_type_id {
        project.add_diagnostic(Diagnostic::error(
          span,
          format!("binary operation between incompatible types ({} and {})",
            project.typename_for_type_id(lhs_type_id),
            project.typename_for_type_id(rhs_type_id),
          )
        ));
      }

      type_id = BOOL_TYPE_ID;
    }
    BinaryOperator::LogicalAnd | BinaryOperator::LogicalOr => {
      if lhs_type_id != BOOL_TYPE_ID {
        project.add_diagnostic(Diagnostic::error(
          span,
          "left side of logical binary operation is not a boolean".to_string(),
        ));
      }

      if rhs_type_id != BOOL_TYPE_ID {
        project.add_diagnostic(Diagnostic::error(
          span,
          "right side of logical binary operation is not a boolean".to_string(),
        ));
      }

      type_id = BOOL_TYPE_ID;
    }
    BinaryOperator::Assign
    | BinaryOperator::AddAssign
    | BinaryOperator::SubtractAssign
    | BinaryOperator::MultiplyAssign
    | BinaryOperator::DivideAssign
    | BinaryOperator::ModuloAssign
    /* | BinaryOperator::BitwiseAndAssign
    | BinaryOperator::BitwiseOrAssign
    | BinaryOperator::BitwiseXorAssign
    | BinaryOperator::BitwiseLeftShiftAssign
    | BinaryOperator::BitwiseRightShiftAssign */ => {
      if lhs_type_id != rhs_type_id {
        project.add_diagnostic(
          Diagnostic::error(
            span,
            format!(
              "assignment between incompatible types ({} and {})",
              project.typename_for_type_id(lhs_type_id),
              project.typename_for_type_id(rhs_type_id),
            )
          )
        );
      }

      if !lhs.is_mutable() {
        project.add_diagnostic(Diagnostic::error(
            span,
            "assignment to immutable variable".to_string(),
          ));
      }
    }
    BinaryOperator::Add
    | BinaryOperator::Subtract
    | BinaryOperator::Multiply
    | BinaryOperator::Divide
    | BinaryOperator::Modulo => {
      if lhs_type_id != rhs_type_id {
        project.add_diagnostic(Diagnostic::error(
          span,
          format!("binary operation between incompatible types ({} and {})",
            project.typename_for_type_id(lhs_type_id),
            project.typename_for_type_id(rhs_type_id),
          ),
        ));
      }

      type_id = lhs_type_id;
    }
    BinaryOperator::BitwiseAnd
    | BinaryOperator::BitwiseOr
    | BinaryOperator::BitwiseXor
    | BinaryOperator::BitwiseLeftShift
    | BinaryOperator::BitwiseRightShift => {
      if lhs_type_id != rhs_type_id {
        project.add_diagnostic(Diagnostic::error(
          span,
          format!("binary operation between incompatible types ({} and {})",
            project.typename_for_type_id(lhs_type_id),
            project.typename_for_type_id(rhs_type_id),
          ),
        ));
      }

      type_id = lhs_type_id;
    }
  }

  type_id
}

fn substitute_typevars_in_expression(
  expr: &mut CheckedExpression,
  specializations: &HashMap<TypeId, TypeId>,
  project: &mut Project,
) {
  match expr {
    CheckedExpression::Null(span, _) => todo!(),
    CheckedExpression::Nullptr(span, type_id) => {
      *type_id = substitute_typevars_in_type(*type_id, specializations, project);
    }
    CheckedExpression::Boolean(_, span) => todo!(),
    CheckedExpression::NumericConstant(numeric_constant, _, span) => todo!(),
    CheckedExpression::QuotedString(_, span) => todo!(),
    CheckedExpression::QuotedCString(_, span) => todo!(),
    CheckedExpression::CharacterLiteral(_, span) => todo!(),
    CheckedExpression::Variable(var, span) => {
      var.type_id = substitute_typevars_in_type(var.type_id, specializations, project);
    }
    CheckedExpression::NamespacedVariable(checked_namespaces, checked_var_decl, span) => todo!(),
    CheckedExpression::UnaryOp(expr, op, type_id, _) => {
      substitute_typevars_in_expression(expr, specializations, project);
      *type_id = substitute_typevars_in_type(*type_id, specializations, project);
      match op {
        CheckedUnaryOperator::As(type_id) => {
          *type_id = substitute_typevars_in_type(*type_id, specializations, project);
        }
        _ => {}
      }
    }
    CheckedExpression::BinaryOp(lhs, _, rhs, type_id, _) => {
      substitute_typevars_in_expression(lhs, specializations, project);
      substitute_typevars_in_expression(rhs, specializations, project);
      *type_id = substitute_typevars_in_type(*type_id, specializations, project);
    }
    CheckedExpression::Range(lhs, rhs, type_id, _) => {
      substitute_typevars_in_expression(lhs, specializations, project);
      substitute_typevars_in_expression(rhs, specializations, project);
      *type_id = substitute_typevars_in_type(*type_id, specializations, project);
    }
    CheckedExpression::IndexedExpression(expr, index, type_id, _) => {
      substitute_typevars_in_expression(expr, specializations, project);
      substitute_typevars_in_expression(index, specializations, project);
      *type_id = substitute_typevars_in_type(*type_id, specializations, project);
    }
    CheckedExpression::IndexedStruct(checked_expression, _, _, _, span) => todo!(),
    CheckedExpression::IndexedEnum(_, _, _, _, span) => todo!(),
    CheckedExpression::Call(_, type_id, _) => {
      *type_id = substitute_typevars_in_type(*type_id, specializations, project);
    }
    CheckedExpression::MethodCall(checked_expression, checked_call, _, span) => todo!(),
    CheckedExpression::InlineAsm {
      volatile,
      asm,
      bindings,
      clobbers,
      type_id,
      span,
    } => todo!(),
    CheckedExpression::Switch {
      value,
      cases,
      default,
      type_id,
      span,
    } => todo!(),
    CheckedExpression::Garbage(span) => todo!(),
  }
}

fn substitute_typevars_in_statement(
  statement: &mut CheckedStatement,
  specializations: &HashMap<TypeId, TypeId>,
  project: &mut Project,
) {
  match statement {
    CheckedStatement::VarDecl(var, expr) => {
      var.type_id = substitute_typevars_in_type(var.type_id, specializations, project);
      substitute_typevars_in_expression(expr, specializations, project);
    }
    CheckedStatement::Block(checked_block) => todo!(),
    CheckedStatement::If {
      condition,
      then_block,
      else_block,
    } => todo!(),
    CheckedStatement::While { condition, block } => todo!(),
    CheckedStatement::Loop(checked_block) => todo!(),
    CheckedStatement::Break(span) => todo!(),
    CheckedStatement::Continue(span) => todo!(),
    CheckedStatement::Return(expr) => {
      substitute_typevars_in_expression(expr, specializations, project);
    }
    CheckedStatement::Yield(checked_expression) => todo!(),
    CheckedStatement::Expression(checked_expression) => todo!(),
  }
}

fn substitute_typevars_in_block(
  block: &mut CheckedBlock,
  specializations: &HashMap<TypeId, TypeId>,
  project: &mut Project,
) {
  for statement in &mut block.stmts {
    substitute_typevars_in_statement(statement, specializations, project);
  }
}

fn substitute_typevars_in_type(
  type_id: TypeId,
  specializations: &HashMap<TypeId, TypeId>,
  project: &mut Project,
) -> TypeId {
  let mut result = substitute_typevars_in_type_helper(type_id, specializations, project);

  loop {
    let fixed = substitute_typevars_in_type_helper(type_id, specializations, project);
    if fixed == result {
      break;
    } else {
      result = fixed;
    }
  }

  result
}

fn substitute_typevars_in_type_helper(
  type_id: TypeId,
  specializations: &HashMap<TypeId, TypeId>,
  project: &mut Project,
) -> TypeId {
  let ty = &project.types[type_id];
  match ty {
    CheckedType::TypeVariable(_, _, _) => {
      if let Some(replacement) = specializations.get(&type_id) {
        return *replacement;
      }
    }
    CheckedType::GenericInstance(type_decl_id, args, span) => {
      let span = *span;
      let type_decl_id = *type_decl_id;
      let mut new_args = args.clone();

      for arg in &mut new_args {
        *arg = substitute_typevars_in_type(*arg, specializations, project);
      }

      return project.find_or_add_type_id(CheckedType::GenericInstance(
        type_decl_id,
        new_args,
        span,
      ));
    }
    CheckedType::GenericEnumInstance(enum_id, args, span) => {
      let span = *span;
      let enum_id = *enum_id;
      let mut new_args = args.clone();

      for arg in &mut new_args {
        *arg = substitute_typevars_in_type(*arg, specializations, project);
      }

      return project
        .find_or_add_type_id(CheckedType::GenericEnumInstance(enum_id, new_args, span));
    }
    CheckedType::TypeDecl(type_decl_id, span) => {
      let span = *span;
      let type_decl_id = *type_decl_id;
      let structure = &project.type_decls[type_decl_id];

      if !structure.generic_parameters.is_empty() {
        let mut new_args = structure.generic_parameters.clone();

        for arg in &mut new_args {
          *arg = substitute_typevars_in_type(*arg, specializations, project);
        }

        return project.find_or_add_type_id(CheckedType::GenericInstance(
          type_decl_id,
          new_args,
          span,
        ));
      }
    }

    CheckedType::Enum(enum_id, span) => {
      let span = *span;
      let enum_id = *enum_id;
      let enum_ = &project.enums[enum_id];

      if !enum_.generic_parameters.is_empty() {
        let mut new_args = enum_.generic_parameters.clone();

        for arg in &mut new_args {
          *arg = substitute_typevars_in_type(*arg, specializations, project);
        }

        return project
          .find_or_add_type_id(CheckedType::GenericEnumInstance(enum_id, new_args, span));
      }
    }

    CheckedType::RawPtr(inner_type_id, span) => {
      let span = *span;
      let inner_type_id = substitute_typevars_in_type(*inner_type_id, specializations, project);
      return project.find_or_add_type_id(CheckedType::RawPtr(inner_type_id, span));
    }

    _ => {}
  }

  type_id
}

pub fn check_types_for_compat(
  lhs_type_id: TypeId,
  rhs_type_id: TypeId,
  specializations: &mut HashMap<TypeId, TypeId>,
  span: Span,
  project: &mut Project,
) -> Option<()> {
  let lhs_type = &project.types[lhs_type_id];

  let optional_type_decl_id = project
    .find_type_decl_in_scope(0, "Optional")
    .expect("internal error: unable to locate builtin `Optional` type");

  if let CheckedType::GenericInstance(lhs_type_decl_id, args, _) = lhs_type {
    if *lhs_type_decl_id == optional_type_decl_id
      && args.first().map_or(false, |arg_id| *arg_id == rhs_type_id)
    {
      return Some(());
    }
  }

  match lhs_type {
    CheckedType::TypeVariable(_, _, _) => {
      if let Some(seen_type_id) = specializations.get(&lhs_type_id) {
        if rhs_type_id != *seen_type_id {
          project.add_diagnostic(Diagnostic::error(
            span,
            format!(
              "type mismatch: expected {}, but got {}",
              project.typename_for_type_id(*seen_type_id),
              project.typename_for_type_id(rhs_type_id),
            ),
          ));
          return None;
        }
      } else {
        specializations.insert(lhs_type_id, rhs_type_id);
      }
    }
    CheckedType::GenericInstance(lhs_type_decl_id, lhs_args, _) => {
      let lhs_args = lhs_args.clone();
      let rhs_type = &project.types[rhs_type_id];
      match rhs_type {
        CheckedType::GenericInstance(rhs_type_decl_id, rhs_args, _) => {
          if lhs_type_decl_id == rhs_type_decl_id {
            let rhs_args = rhs_args.clone();

            let lhs_struct = &project.type_decls[*lhs_type_decl_id];
            if rhs_args.len() != lhs_args.len() {
              project.add_diagnostic(Diagnostic::error(
                span,
                format!(
                  "mismatched number of generic parameters for {}",
                  lhs_struct.name
                ),
              ));
              return None;
            }

            let mut idx = 0;

            while idx < lhs_args.len() {
              let lhs_arg_type_id = lhs_args[idx];
              let rhs_arg_type_id = rhs_args[idx];
              check_types_for_compat(
                lhs_arg_type_id,
                rhs_arg_type_id,
                specializations,
                span,
                project,
              )?;
              idx += 1;
            }
          }
        }
        _ => {
          if rhs_type_id != lhs_type_id {
            project.add_diagnostic(Diagnostic::error(
              span,
              format!(
                "type mismatch: expected {}, but got {}",
                project.typename_for_type_id(lhs_type_id),
                project.typename_for_type_id(rhs_type_id),
              ),
            ));
            return None;
          }
        }
      }
    }
    CheckedType::GenericEnumInstance(lhs_enum_id, lhs_args, _) => {
      let lhs_args = lhs_args.clone();
      let rhs_type = &project.types[rhs_type_id];
      match rhs_type {
        CheckedType::GenericEnumInstance(rhs_enum_id, rhs_args, _) => {
          let rhs_args = rhs_args.clone();
          if lhs_enum_id == rhs_enum_id {
            let lhs_enum = &project.enums[*lhs_enum_id];
            if rhs_args.len() != lhs_args.len() {
              project.add_diagnostic(Diagnostic::error(
                span,
                format!(
                  "mismatched number of generic parameters for {}",
                  lhs_enum.name
                ),
              ));
            }

            let mut idx = 0;

            while idx < lhs_args.len() {
              let lhs_arg_type_id = lhs_args[idx];
              let rhs_arg_type_id = rhs_args[idx];

              check_types_for_compat(
                lhs_arg_type_id,
                rhs_arg_type_id,
                specializations,
                span,
                project,
              );
              idx += 1;
            }
          }
        }
        _ => {
          if rhs_type_id != lhs_type_id {
            project.add_diagnostic(Diagnostic::error(
              span,
              format!(
                "type mismatch: expected {}, but got {}",
                project.typename_for_type_id(lhs_type_id),
                project.typename_for_type_id(rhs_type_id),
              ),
            ))
          }
        }
      }
    }
    CheckedType::TypeDecl(lhs_type_decl_id, _) => {
      if rhs_type_id == lhs_type_id {
        return Some(());
      }

      let rhs_type = &project.types[rhs_type_id];
      match rhs_type {
        CheckedType::GenericInstance(rhs_type_decl_id, args, _) => {
          if lhs_type_decl_id == rhs_type_decl_id {
            let args = args.clone();

            let lhs_struct = project.type_decls[*lhs_type_decl_id].clone();
            if args.len() != lhs_struct.generic_parameters.len() {
              project.add_diagnostic(Diagnostic::error(
                span,
                format!(
                  "mismatched number of generic parameters for {}",
                  lhs_struct.name
                ),
              ));
              return None;
            }

            let mut idx = 0;

            let lhs_arg_type_id = lhs_struct.generic_parameters[idx];
            let rhs_arg_type_id = args[idx];

            while idx < args.len() {
              check_types_for_compat(
                lhs_arg_type_id,
                rhs_arg_type_id,
                specializations,
                span,
                project,
              )?;
              idx += 1;
            }
          }
        }

        CheckedType::TypeDecl(rhs_type_decl_id, _) if project.is_interface(*lhs_type_decl_id) => {
          if !project.is_interface(*rhs_type_decl_id) {
            let rhs_struct = &project.type_decls[*rhs_type_decl_id];
            if !rhs_struct.implements(*lhs_type_decl_id) {
              project.add_diagnostic(Diagnostic::error(
                span,
                format!(
                  "type mismatch: expected {}, but got {}",
                  project.typename_for_type_id(lhs_type_id),
                  project.typename_for_type_id(rhs_type_id),
                ),
              ));
              return None;
            }
          }
        }

        _ => {
          if rhs_type_id != lhs_type_id {
            project.add_diagnostic(Diagnostic::error(
              span,
              format!(
                "type mismatch: expected {}, but got {}",
                project.typename_for_type_id(lhs_type_id),
                project.typename_for_type_id(rhs_type_id),
              ),
            ));
            return None;
          }
        }
      }
    }

    CheckedType::Enum(lhs_enum_id, lhs_span) => {
      let lhs_span = *lhs_span;
      if rhs_type_id == lhs_type_id {
        return None;
      }

      let rhs_type = &project.types[rhs_type_id];
      match rhs_type {
        CheckedType::GenericEnumInstance(rhs_enum_id, rhs_args, _) => {
          let rhs_args = rhs_args.clone();
          if lhs_enum_id == rhs_enum_id {
            let lhs_enum = &project.enums[*lhs_enum_id];
            if rhs_args.len() != lhs_enum.generic_parameters.len() {
              project.add_diagnostic(Diagnostic::error(
                lhs_span,
                format!(
                  "mismatched number of generic parameters for {}",
                  lhs_enum.name
                ),
              ));
              return None;
            }

            let lhs_enum_generic_parameters = lhs_enum.generic_parameters.clone();

            let mut idx = 0;

            while idx < rhs_args.len() {
              let lhs_arg_type_id = lhs_enum_generic_parameters[idx];
              let rhs_arg_type_id = rhs_args[idx];

              check_types_for_compat(
                lhs_arg_type_id,
                rhs_arg_type_id,
                specializations,
                span,
                project,
              );
              idx += 1;
            }
          }
        }
        _ => {
          if rhs_type_id != lhs_type_id {
            project.add_diagnostic(Diagnostic::error(
              span,
              format!(
                "type mismatch: expected {}, but got {}",
                project.typename_for_type_id(lhs_type_id),
                project.typename_for_type_id(rhs_type_id),
              ),
            ))
          }
        }
      }
    }

    _ => {
      if rhs_type_id != lhs_type_id {
        project.add_diagnostic(Diagnostic::error(
          span,
          format!(
            "type mismatch: expected {}, but got {}",
            project.typename_for_type_id(lhs_type_id),
            project.typename_for_type_id(rhs_type_id),
          ),
        ));
        return None;
      }
    }
  }

  Some(())
}

pub fn typecheck_typename(
  unchecked_type: &ParsedType,
  scope_id: ScopeId,
  project: &mut Project,
) -> TypeId {
  match unchecked_type {
    ParsedType::Name(name, span) => match name.as_str() {
      "void" => crate::compiler::VOID_TYPE_ID,
      "i8" => crate::compiler::I8_TYPE_ID,
      "i16" => crate::compiler::I16_TYPE_ID,
      "i32" => crate::compiler::I32_TYPE_ID,
      "i64" => crate::compiler::I64_TYPE_ID,
      "i128" => crate::compiler::I128_TYPE_ID,
      "isz" => crate::compiler::ISZ_TYPE_ID,
      "u8" => crate::compiler::U8_TYPE_ID,
      "u16" => crate::compiler::U16_TYPE_ID,
      "u32" => crate::compiler::U32_TYPE_ID,
      "u64" => crate::compiler::U64_TYPE_ID,
      "u128" => crate::compiler::U128_TYPE_ID,
      "usz" => crate::compiler::USZ_TYPE_ID,
      "string" => {
        let string_type_decl_id = project
          .find_type_decl_in_scope(0, "String")
          .expect("internal error: unable to locate builtin `String` type");
        project.find_or_add_type_id(CheckedType::TypeDecl(string_type_decl_id, *span))
      }
      "bool" => crate::compiler::BOOL_TYPE_ID,
      "c_char" => crate::compiler::CCHAR_TYPE_ID,
      "rawptr" => crate::compiler::RAWPTR_TYPE_ID,
      "never" => crate::compiler::NEVER_TYPE_ID,
      x => {
        let type_id = project.find_type_in_scope(scope_id, x);
        match type_id {
          Some(type_id) => type_id,
          None => {
            project.add_diagnostic(Diagnostic::error(*span, "unknown type".to_string()));
            UNKNOWN_TYPE_ID
          }
        }
      }
    },
    ParsedType::GenericType(name, inner_types, span) => {
      let mut checked_inner_types = vec![];

      for inner_type in inner_types {
        let inner_type_id = typecheck_typename(inner_type, scope_id, project);
        checked_inner_types.push(inner_type_id);
      }

      let type_decl_id = project.find_type_decl_in_scope(scope_id, name);

      if let Some(type_decl_id) = type_decl_id {
        project.find_or_add_type_id(CheckedType::GenericInstance(
          type_decl_id,
          checked_inner_types,
          *span,
        ))
      } else {
        project.add_diagnostic(Diagnostic::error(*span, format!("could not find {}", name)));
        UNKNOWN_TYPE_ID
      }
    }
    ParsedType::RawPointer(inner, span) => {
      let inner_type_id = typecheck_typename(inner, scope_id, project);
      let type_id = project.find_or_add_type_id(CheckedType::RawPtr(inner_type_id, *span));
      type_id
    }
    ParsedType::Optional(inner, span) => {
      let inner_type_id = typecheck_typename(inner, scope_id, project);
      let optional_type_decl_id = project
        .find_type_decl_in_scope(0, "Optional")
        .expect("internal error: Optional builtin definition not found");
      let type_id = project.find_or_add_type_id(CheckedType::GenericInstance(
        optional_type_decl_id,
        vec![inner_type_id],
        *span,
      ));

      type_id
    }
    ParsedType::Slice(inner, size, span) => {
      let Some(size) = size else {
        todo!("cannot infer size of slice");
      };

      if !is_integer(size.type_id()) {
        project.add_diagnostic(Diagnostic::error(
          *span,
          "slice size need to be an integer".into(),
        ));
        return UNKNOWN_TYPE_ID;
      }

      let inner_type_id = typecheck_typename(inner, scope_id, project);
      let type_id = project.find_or_add_type_id(CheckedType::Slice(
        inner_type_id,
        size.integer_constant().unwrap().to_usize(),
        *span,
      ));
      type_id
    }
    ParsedType::Empty => UNKNOWN_TYPE_ID,
  }
}
