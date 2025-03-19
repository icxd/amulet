use std::collections::HashMap;

use crate::{
  ast::{
    BinaryOperator, DefinitionLinkage, NumericConstant, ParsedBlock, ParsedCall, ParsedExpression,
    ParsedFunction, ParsedNamespace, ParsedStatement, ParsedType, ParsedTypeDecl,
    ParsedTypeDeclData,
  },
  compiler::{BOOL_TYPE_ID, STRING_TYPE_ID, U8_TYPE_ID, UNKNOWN_TYPE_ID, VOID_TYPE_ID},
  error::{Error, Result},
  span::Span,
};

pub type TypeId = usize;
pub type ScopeId = usize;
pub type FunctionId = usize;
pub type TypeDeclId = usize;

#[derive(Debug)]
pub struct Scope {
  pub(crate) namespace_name: Option<String>,
  pub(crate) vars: Vec<CheckedVariable>,
  pub(crate) functions: Vec<(String, FunctionId)>,
  pub(crate) type_decls: Vec<(String, TypeDeclId)>,
  pub(crate) types: Vec<(String, TypeId)>,
  pub(crate) parent: Option<ScopeId>,
  pub(crate) children: Vec<ScopeId>,
}

impl Scope {
  pub fn new(parent: Option<ScopeId>) -> Self {
    Self {
      namespace_name: None,
      vars: vec![],
      functions: vec![],
      type_decls: vec![],
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
pub struct CheckedVariable {
  pub(crate) name: String,
  pub(crate) type_id: TypeId,
  pub(crate) mutable: bool,
}

#[derive(Debug, Clone)]
pub struct CheckedVarDecl {
  pub(crate) name: String,
  pub(crate) type_id: TypeId,
  pub(crate) mutable: bool,
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

#[derive(Debug, Clone)]
pub enum GenericParameter {
  InferenceGuide(TypeId),
  Parameter(TypeId),
}

#[derive(Debug, Clone)]
pub struct CheckedFunction {
  pub(crate) name: String,
  pub(crate) visibility: Visibility,
  pub(crate) return_type_id: TypeId,
  pub(crate) params: Vec<CheckedVariable>,
  pub(crate) generic_parameters: Vec<GenericParameter>,
  pub(crate) scope_id: ScopeId,
  pub(crate) block: CheckedBlock,
  pub(crate) linkage: DefinitionLinkage,
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
pub enum CheckedExpression {
  Null(Span, TypeId),
  Nullptr(Span, TypeId),
  Boolean(bool, Span),
  NumericConstant(NumericConstant, TypeId, Span),
  QuotedString(String, Span),
  CharacterLiteral(char, Span),

  Variable(CheckedVariable, Span),
  NamespacedVariable(Vec<CheckedNamespace>, CheckedVariable, Span),

  BinaryOp(
    Box<CheckedExpression>,
    BinaryOperator,
    Box<CheckedExpression>,
    TypeId,
    Span,
  ),

  IndexedExpression(Box<CheckedExpression>, Box<CheckedExpression>, TypeId, Span),
  FieldAccess(Box<CheckedExpression>, String, TypeId, Span),

  Call(CheckedCall, TypeId, Span),
  MethodCall(Box<CheckedExpression>, CheckedCall, TypeId, Span),
}
impl CheckedExpression {
  fn type_id(&self) -> TypeId {
    match self {
      CheckedExpression::Null(_, type_id) => *type_id,
      CheckedExpression::Nullptr(_, type_id) => *type_id,
      CheckedExpression::Boolean(_, _) => BOOL_TYPE_ID,
      CheckedExpression::NumericConstant(_, type_id, _) => *type_id,
      CheckedExpression::QuotedString(_, _) => STRING_TYPE_ID,
      CheckedExpression::CharacterLiteral(_, _) => U8_TYPE_ID,
      CheckedExpression::Variable(var, _) => var.type_id,
      CheckedExpression::NamespacedVariable(_, var, _) => var.type_id,
      CheckedExpression::BinaryOp(_, _, right, _, _) => right.type_id(),
      CheckedExpression::IndexedExpression(_, _, type_id, _) => *type_id,
      CheckedExpression::FieldAccess(_, _, type_id, _) => *type_id,
      CheckedExpression::Call(_, type_id, _) => *type_id,
      CheckedExpression::MethodCall(_, _, type_id, _) => *type_id,
    }
  }

  fn is_mutable(&self) -> bool {
    match self {
      CheckedExpression::Variable(var, _) => var.mutable,
      CheckedExpression::IndexedExpression(expr, _, _, _) => expr.is_mutable(),
      CheckedExpression::FieldAccess(expr, _, _, _) => expr.is_mutable(),
      _ => false,
    }
  }
}

#[derive(Debug, Clone)]
pub enum CheckedStatement {
  VarDecl(CheckedVarDecl, CheckedExpression),

  Block(CheckedBlock),
  If {
    condition: CheckedExpression,
    then_block: CheckedBlock,
    else_block: Option<CheckedBlock>,
  },
  While {
    condition: CheckedExpression,
    block: CheckedBlock,
  },

  Break(Span),
  Continue(Span),
  Return(CheckedExpression),

  Expression(CheckedExpression),
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
}

#[derive(Debug, PartialEq)]
pub enum CheckedType {
  Builtin(Span),
  TypeVariable(String, Option<TypeId>, Span),
  GenericInstance(TypeDeclId, Vec<TypeId>, Span),
  TypeDecl(TypeDeclId, Span),
  RawPtr(TypeId, Span),
}
impl CheckedType {
  fn span(&self) -> Span {
    match self {
      CheckedType::Builtin(span) => *span,
      CheckedType::TypeVariable(_, _, span) => *span,
      CheckedType::GenericInstance(_, _, span) => *span,
      CheckedType::TypeDecl(_, span) => *span,
      CheckedType::RawPtr(_, span) => *span,
    }
  }
}

#[derive(Debug, Clone)]
pub struct CheckedMethod {
  pub(crate) name: String,
  pub(crate) return_type_id: TypeId,
  pub(crate) params: Vec<CheckedVariable>,
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
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Visibility {
  Public,
  Private,
  Protected,
}

#[derive(Debug)]
pub struct Project {
  pub(crate) scopes: Vec<Scope>,
  pub(crate) type_decls: Vec<CheckedTypeDecl>,
  pub(crate) functions: Vec<CheckedFunction>,
  pub(crate) types: Vec<CheckedType>,
  pub(crate) current_function_index: Option<usize>,
}

impl Project {
  pub fn new() -> Self {
    let global_scope = Scope::new(None);
    Self {
      scopes: vec![global_scope],
      type_decls: vec![],
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
        CheckedType::Builtin(Span::default()), // String
        CheckedType::Builtin(Span::default()), // Bool
      ],
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
  ) -> Result<()> {
    let scope = &mut self.scopes[scope_id];

    for (existing_type, existing_type_id) in &scope.types {
      if &type_name == existing_type {
        return Err(
          Error::new(span, format!("redefinition of type {}", type_name)).with_hint(
            self.types[*existing_type_id].span(),
            "previous definition here".into(),
          ),
        );
      }
    }
    scope.types.push((type_name, type_id));

    Ok(())
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
  ) -> Result<()> {
    let scope = &mut self.scopes[scope_id];

    for (type_decl_name, _) in &scope.type_decls {
      if &name == type_decl_name {
        return Err(Error::new(
          span,
          format!("redefinition of type declaration {}", type_decl_name),
        ));
      }
    }
    scope.type_decls.push((name, type_decl_id));

    Ok(())
  }

  fn find_var_in_scope(&self, scope_id: usize, name: &str) -> Option<CheckedVariable> {
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

  pub fn add_var_to_scope(
    &mut self,
    scope_id: ScopeId,
    var: CheckedVariable,
    span: Span,
  ) -> Result<()> {
    let scope = &mut self.scopes[scope_id];

    for existing_var in &scope.vars {
      if &var.name == &existing_var.name {
        return Err(Error::new(
          span,
          format!("redefinition of variable {}", var.name),
        ));
      }
    }
    scope.vars.push(var);

    Ok(())
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
  ) -> Result<()> {
    let scope = &mut self.scopes[scope_id];

    for (function_name, _) in &scope.functions {
      if &name == function_name {
        return Err(Error::new(
          span,
          format!("redefinition of function {}", function_name),
        ));
      }
    }
    scope.functions.push((name, function_id));

    Ok(())
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
        crate::compiler::STRING_TYPE_ID => "string".to_string(),
        crate::compiler::BOOL_TYPE_ID => "bool".to_string(),
        _ => "unknown".to_string(),
      },
      CheckedType::TypeDecl(type_decl_id, _) => {
        format!("{}", self.type_decls[*type_decl_id].name)
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
      CheckedType::TypeVariable(name, _, _) => name.clone(),
      CheckedType::RawPtr(type_id, _) => format!("{}*", self.typename_for_type_id(*type_id)),
    }
  }
}

pub fn typecheck_namespace(
  parsed_namespace: &ParsedNamespace,
  scope_id: ScopeId,
  project: &mut Project,
) -> Result<()> {
  let project_type_decl_len = project.type_decls.len();
  let project_function_len = project.functions.len();

  for namespace in parsed_namespace.namespaces.iter() {
    let namespace_scope_id = project.create_scope(scope_id);
    project.scopes[namespace_scope_id].namespace_name = namespace.name.clone();
    project.scopes[scope_id].children.push(namespace_scope_id);
    typecheck_namespace(namespace, namespace_scope_id, project)?;
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
    )?;
  }

  for (type_decl_id, type_decl) in parsed_namespace.type_decls.iter().enumerate() {
    let type_decl_id = type_decl_id + project_type_decl_len;

    typecheck_type_decl_predecl(type_decl, type_decl_id, scope_id, project)?;
  }

  for function in &parsed_namespace.functions {
    typecheck_function_predecl(function, scope_id, project)?;
  }

  for (type_decl_id, type_decl) in parsed_namespace.type_decls.iter().enumerate() {
    typecheck_type_decl(
      type_decl,
      type_decl_id + project_type_decl_len,
      scope_id,
      project,
    )?;
  }

  for (i, function) in parsed_namespace.functions.iter().enumerate() {
    project.current_function_index = Some(i + project_function_len);
    typecheck_function(function, scope_id, project)?;
    project.current_function_index = None;
  }

  Ok(())
}

fn typecheck_type_decl_predecl(
  type_decl: &ParsedTypeDecl,
  type_decl_id: TypeDeclId,
  parent_scope_id: ScopeId,
  project: &mut Project,
) -> Result<()> {
  let type_decl_type_id =
    project.find_or_add_type_id(CheckedType::TypeDecl(type_decl_id, type_decl.name_span));
  let type_decl_scope_id = project.create_scope(parent_scope_id);

  let mut type_params = vec![];
  for type_param in &type_decl.type_parameters {
    let constraint = if let Some(constraint) = &type_param.constraint {
      Some(typecheck_typename(constraint, type_decl_scope_id, project)?)
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
    )?;
  }

  match &type_decl.data {
    ParsedTypeDeclData::Interface(methods) => {
      let mut checked_methods = vec![];
      for method in methods.iter() {
        let mut type_params = vec![];
        let method_scope_id = project.create_scope(type_decl_scope_id);

        for param in &method.type_parameters {
          let constraint = if let Some(constraint) = &param.constraint {
            Some(typecheck_typename(constraint, type_decl_scope_id, project)?)
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
          )?;
        }

        let mut checked_function = CheckedFunction {
          name: method.name.clone(),
          visibility: Visibility::Public,
          return_type_id: UNKNOWN_TYPE_ID,
          params: vec![],
          generic_parameters: type_params,
          scope_id: method_scope_id,
          block: CheckedBlock::new(),
          linkage: method.linkage,
        };

        for param in &method.parameters {
          if param.name.as_str() == "self" {
            let checked_var = CheckedVariable {
              name: param.name.clone(),
              type_id: type_decl_type_id,
              mutable: param.mutable,
            };
            checked_function.params.push(checked_var);
          } else {
            let type_id = typecheck_typename(&param.r#type, method_scope_id, project)?;
            let checked_var = CheckedVariable {
              name: param.name.clone(),
              type_id,
              mutable: param.mutable,
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
        )?;
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
    ParsedTypeDeclData::Class { methods, .. } => {
      for method in methods.iter() {
        let mut type_params = vec![];
        let method_scope_id = project.create_scope(type_decl_scope_id);

        for param in &method.type_parameters {
          let constraint = if let Some(constraint) = &param.constraint {
            Some(typecheck_typename(constraint, type_decl_scope_id, project)?)
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
          )?;
        }

        let mut checked_function = CheckedFunction {
          name: method.name.clone(),
          visibility: Visibility::Public,
          return_type_id: UNKNOWN_TYPE_ID,
          params: vec![],
          generic_parameters: type_params,
          scope_id: method_scope_id,
          block: CheckedBlock::new(),
          linkage: method.linkage,
        };

        for param in &method.parameters {
          if param.name.as_str() == "self" {
            let checked_var = CheckedVariable {
              name: param.name.clone(),
              type_id: type_decl_type_id,
              mutable: param.mutable,
            };
            checked_function.params.push(checked_var);
          } else {
            let type_id = typecheck_typename(&param.r#type, method_scope_id, project)?;
            let checked_var = CheckedVariable {
              name: param.name.clone(),
              type_id,
              mutable: param.mutable,
            };
            checked_function.params.push(checked_var);
          }
        }

        project.functions.push(checked_function);
        project.add_function_to_scope(
          type_decl_scope_id,
          method.name.clone(),
          project.functions.len() - 1,
          type_decl.name_span,
        )?;
      }

      project.type_decls.push(CheckedTypeDecl {
        name: type_decl.name.clone(),
        generic_parameters: type_params,
        kind: CheckedTypeKind::Class(CheckedClass {
          implements: vec![],
          fields: vec![],
        }),
        scope_id: type_decl_scope_id,
        linkage: type_decl.linkage,
      });
    }
    ParsedTypeDeclData::Alias(unchecked_type) => {
      let type_id = typecheck_typename(unchecked_type, type_decl_scope_id, project)?;
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
  )?;

  Ok(())
}

fn typecheck_type_decl(
  type_decl: &ParsedTypeDecl,
  type_decl_id: TypeDeclId,
  parent_scope_id: ScopeId,
  project: &mut Project,
) -> Result<()> {
  let mut checked_fields = vec![];

  let checked_type_decl = &mut project.type_decls[type_decl_id];
  let type_decl_scope_id = checked_type_decl.scope_id;
  let type_decl_type_id =
    project.find_or_add_type_id(CheckedType::TypeDecl(type_decl_id, type_decl.name_span));

  match &type_decl.data {
    ParsedTypeDeclData::Interface(_) => {}
    ParsedTypeDeclData::Class { fields, .. } => {
      for (unchecked_member, unchecked_value) in fields {
        let checked_member_type =
          typecheck_typename(&unchecked_member.ty, type_decl_scope_id, project)?;

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
          .map(|field| CheckedVariable {
            name: field.name.clone(),
            type_id: field.type_id,
            mutable: field.mutable,
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
          visibility: Visibility::Public,
          return_type_id: type_decl_type_id,
          params: constructor_params,
          generic_parameters,
          scope_id: function_scope_id,
          block: CheckedBlock::new(),
          linkage: DefinitionLinkage::ImplicitConstructor,
        };

        project.functions.push(checked_constructor);
        project.add_function_to_scope(
          type_decl_scope_id,
          type_decl.name.clone(),
          project.functions.len() - 1,
          type_decl.name_span,
        )?;
      }

      let checked_type_decl = &mut project.type_decls[type_decl_id];
      checked_type_decl
        .kind
        .as_class_mut()
        .expect("internal error: got something other than a class while typechecking classes (???)")
        .fields = checked_fields;

      let ParsedTypeDeclData::Class { methods, .. } = &type_decl.data else {
        panic!("internal error: get something other than a class while typechecking class methods");
      };
      for method in methods {
        typecheck_method(method, project, type_decl_id)?;
      }
    }
    ParsedTypeDeclData::Alias(_) => {}
  }

  Ok(())
}

fn typecheck_function_predecl(
  function: &ParsedFunction,
  parent_scope_id: ScopeId,
  project: &mut Project,
) -> Result<()> {
  let scope_id = project.create_scope(parent_scope_id);

  let mut checked_function = CheckedFunction {
    name: function.name.clone(),
    visibility: Visibility::Public,
    return_type_id: UNKNOWN_TYPE_ID,
    params: vec![],
    generic_parameters: vec![],
    scope_id,
    block: CheckedBlock::new(),
    linkage: function.linkage,
  };

  let checked_function_scope_id = checked_function.scope_id;

  let mut generic_params = vec![];
  for param in &function.type_parameters {
    let constraint = if let Some(constraint) = &param.constraint {
      Some(typecheck_typename(
        constraint,
        checked_function_scope_id,
        project,
      )?)
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
    )?;
  }

  checked_function.generic_parameters = generic_params;

  for param in &function.parameters {
    let param_type_id = typecheck_typename(&param.r#type, checked_function_scope_id, project)?;
    let checked_param = CheckedVariable {
      name: param.name.clone(),
      type_id: param_type_id,
      mutable: param.mutable,
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
  )?;

  Ok(())
}

fn typecheck_function(
  function: &ParsedFunction,
  parent_scope_id: ScopeId,
  project: &mut Project,
) -> Result<()> {
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
    project.add_var_to_scope(function_scope_id, var, function.name_span)?;
  }

  let function_return_type_id = if let Some(return_type) = &function.return_type {
    typecheck_typename(return_type, function_scope_id, project)?
  } else {
    VOID_TYPE_ID
  };

  let checked_function = &mut project.functions[function_id];
  checked_function.return_type_id = function_return_type_id;

  let block = if let Some(block) = &function.body {
    typecheck_block(block, function_scope_id, project)?
  } else {
    CheckedBlock::new()
  };

  let function_return_type_id = if let Some(return_type) = &function.return_type {
    typecheck_typename(return_type, function_scope_id, project)?
  } else {
    VOID_TYPE_ID
  };

  let return_type_id = if function_return_type_id == UNKNOWN_TYPE_ID {
    if let Some(CheckedStatement::Return(ret)) = block.stmts.last() {
      ret.type_id()
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
    return Err(Error::new(
      function.name_span,
      "control reaches end of non-void function".into(),
    ));
  }

  let checked_function = &mut project.functions[function_id];
  checked_function.block = block;
  checked_function.linkage = function_linkage;

  Ok(())
}

fn typecheck_method(
  function: &ParsedFunction,
  project: &mut Project,
  type_decl_id: TypeDeclId,
) -> Result<()> {
  let type_decl = &mut project.type_decls[type_decl_id];
  let type_decl_scope_id = type_decl.scope_id;
  let type_decl_linkage = type_decl.linkage;

  let method_id = project
    .find_function_in_scope(type_decl_scope_id, &function.name)
    .expect("internal error: literally just pushed a checked function and it ain't here gng");

  let checked_function = &mut project.functions[method_id];
  let function_scope_id = checked_function.scope_id;

  for variable in checked_function.params.clone().into_iter() {
    project.add_var_to_scope(function_scope_id, variable, function.name_span)?;
  }

  let block = if let Some(block) = &function.body {
    typecheck_block(block, function_scope_id, project)?
  } else {
    CheckedBlock::new()
  };

  let function_return_type_id = if let Some(return_type) = &function.return_type {
    typecheck_typename(return_type, function_scope_id, project)?
  } else {
    VOID_TYPE_ID
  };

  let return_type_id = if function_return_type_id == UNKNOWN_TYPE_ID {
    if let Some(CheckedStatement::Return(ret)) = block.stmts.last() {
      ret.type_id()
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
    return Err(Error::new(
      function.name_span,
      "control reaches end of non-void function".into(),
    ));
  }

  let checked_function = &mut project.functions[method_id];

  checked_function.block = block;
  checked_function.return_type_id = return_type_id;

  Ok(())
}

fn statement_definitely_returns(stmt: &CheckedStatement) -> bool {
  match stmt {
    CheckedStatement::Return(_) => true,
    CheckedStatement::Block(block) => block.definitely_returns,
    CheckedStatement::While { block, .. } => block.definitely_returns,
    CheckedStatement::If {
      then_block,
      else_block: Some(else_block),
      ..
    } => then_block.definitely_returns && else_block.definitely_returns,
    _ => false,
  }
}

fn typecheck_block(
  block: &ParsedBlock,
  scope_id: ScopeId,
  project: &mut Project,
) -> Result<CheckedBlock> {
  let mut checked_block = CheckedBlock::new();
  for stmt in &block.stmts {
    let checked_stmt = typecheck_statement(stmt, scope_id, project)?;
    if statement_definitely_returns(&checked_stmt) {
      checked_block.definitely_returns = true;
    }

    checked_block.stmts.push(checked_stmt);
  }

  Ok(checked_block)
}

fn typecheck_statement(
  stmt: &ParsedStatement,
  scope_id: ScopeId,
  project: &mut Project,
) -> Result<CheckedStatement> {
  match stmt {
    ParsedStatement::VarDecl(var_decl, expr, span) => {
      let mut checked_type_id = typecheck_typename(&var_decl.ty, scope_id, project)?;
      let checked_expr = typecheck_expression(expr, scope_id, Some(checked_type_id), project)?;
      if checked_type_id == UNKNOWN_TYPE_ID && checked_expr.type_id() != UNKNOWN_TYPE_ID {
        checked_type_id = checked_expr.type_id();
      }

      let checked_var_decl = CheckedVarDecl {
        name: var_decl.name.clone(),
        type_id: checked_type_id,
        mutable: var_decl.mutable,
        span: var_decl.span,
      };

      project.add_var_to_scope(
        scope_id,
        CheckedVariable {
          name: checked_var_decl.name.clone(),
          type_id: checked_var_decl.type_id,
          mutable: checked_var_decl.mutable,
        },
        checked_var_decl.span,
      )?;

      Ok(CheckedStatement::VarDecl(checked_var_decl, checked_expr))
    }

    ParsedStatement::Block(block) => {
      let mut block = typecheck_block(block, scope_id, project)?;
      Ok(CheckedStatement::Block(block))
    }

    ParsedStatement::If(condition, then_block, else_block, span) => {
      let condition = typecheck_expression(condition, scope_id, None, project)?;

      let mut then_block = typecheck_block(then_block, scope_id, project)?;

      let mut checked_else_block = None;
      if let Some(else_block) = else_block {
        let mut checked_else_block = Some(typecheck_block(else_block, scope_id, project)?);
      }

      Ok(CheckedStatement::If {
        condition,
        then_block,
        else_block: checked_else_block,
      })
    }

    ParsedStatement::While(condition, block, span) => {
      let condition = typecheck_expression(condition, scope_id, None, project)?;

      let mut block = typecheck_block(block, scope_id, project)?;

      Ok(CheckedStatement::While { condition, block })
    }

    ParsedStatement::Loop(block, _span) => {
      let block = typecheck_block(block, scope_id, project)?;
      Ok(CheckedStatement::Block(block))
    }

    ParsedStatement::InlineAsm(asm, span) => todo!("inline asm"),

    ParsedStatement::Break(span) => Ok(CheckedStatement::Break(*span)),

    ParsedStatement::Continue(span) => Ok(CheckedStatement::Continue(*span)),

    ParsedStatement::Return(expr, span) => {
      let expr = typecheck_expression(
        expr,
        scope_id,
        project
          .current_function_index
          .map(|i| project.functions[i].return_type_id),
        project,
      )?;

      Ok(CheckedStatement::Return(expr))
    }

    ParsedStatement::Expression(expr) => {
      let expr = typecheck_expression(expr, scope_id, None, project)?;

      Ok(CheckedStatement::Expression(expr))
    }
  }
}

fn resolve_call<'a>(
  call: &ParsedCall,
  namespaces: &mut [ResolvedNamespace],
  span: &Span,
  scope_id: ScopeId,
  project: &'a Project,
) -> Result<Option<&'a CheckedFunction>> {
  let mut callee = None;

  if let Some(namespace) = call.namespace.first() {
    if let Some(type_decl_id) = project.find_type_decl_in_scope(scope_id, namespace) {
      let type_decl = &project.type_decls[type_decl_id];

      if let Some(type_decl_id) = project.find_type_decl_in_scope(type_decl.scope_id, &call.name) {
        let type_decl = &project.type_decls[type_decl_id];

        if let Some(function_id) = project.find_function_in_scope(type_decl.scope_id, &call.name) {
          callee = Some(&project.functions[function_id]);
        }
      } else if let Some(function_id) =
        project.find_function_in_scope(type_decl.scope_id, &call.name)
      {
        callee = Some(&project.functions[function_id]);
      }

      if type_decl.generic_parameters.is_empty() {
        namespaces[0].generic_parameters = Some(type_decl.generic_parameters.clone());
      }

      Ok(callee)
    } else if let Some(scope_id) = project.find_namespace_in_scope(scope_id, namespace) {
      if let Some(type_decl_id) = project.find_type_decl_in_scope(scope_id, &call.name) {
        let type_decl = &project.type_decls[type_decl_id];

        if let Some(function_id) = project.find_function_in_scope(type_decl.scope_id, &call.name) {
          callee = Some(&project.functions[function_id]);
        }
      } else if let Some(function_id) = project.find_function_in_scope(scope_id, &call.name) {
        callee = Some(&project.functions[function_id]);
      }

      Ok(callee)
    } else {
      Err(Error::new(
        *span,
        format!("unknown namespace or type declaration `{}`", namespace),
      ))
    }
  } else {
    if let Some(type_decl_id) = project.find_type_decl_in_scope(scope_id, &call.name) {
      let type_decl = &project.type_decls[type_decl_id];

      if let Some(function_id) = project.find_function_in_scope(type_decl.scope_id, &call.name) {
        callee = Some(&project.functions[function_id]);
      }
    } else if let Some(function_id) = project.find_function_in_scope(scope_id, &call.name) {
      callee = Some(&project.functions[function_id]);
    }

    if callee.is_none() {
      return Err(Error::new(
        *span,
        format!("call to unknown function `{}`", call.name),
      ));
    }

    Ok(callee)
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
) -> Result<CheckedCall> {
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
  )?;

  if let Some(callee) = callee {
    let callee = callee.clone();

    if callee.visibility != Visibility::Public
      && !Scope::can_access(caller_scope_id, callee.scope_id, project)
    {
      return Err(Error::new(
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
      let checked_type = typecheck_typename(type_arg, caller_scope_id, project)?;

      if callee.generic_parameters.len() <= idx {
        return Err(Error::new(
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

    if let Some(this_expr) = this_expr {
      let type_id = this_expr.type_id();
      let param_type = &project.types[type_id];

      if let CheckedType::GenericInstance(type_decl_id, args, span) = param_type {
        let type_decl = &project.type_decls[*type_decl_id];

        let mut idx = 0;
        while idx < type_decl.generic_parameters.len() {
          generic_substitutions.insert(type_decl.generic_parameters[idx], args[idx]);
          idx += 1;
        }
      }

      if callee.is_static() {
        return Err(Error::new(
          *span,
          "cannot call static method on an instance of a class".into(),
        ));
      }

      if callee.is_mutating() && !this_expr.is_mutable() {
        return Err(Error::new(
          *span,
          "cannot call a mutating method on an immutable class instance".into(),
        ));
      }
    }

    let arg_offset = if this_expr.is_some() { 1 } else { 0 };

    if callee.params.len() != (call.args.len() + arg_offset) {
      return Err(Error::new(
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
        let checked_arg = typecheck_expression(&call.args[idx], caller_scope_id, None, project)?;

        let callee = resolve_call(
          call,
          &mut resolved_namespaces,
          span,
          callee_scope_id,
          project,
        )?
        .expect("internal error: previously resolved call is now unresolved");

        let lhs_type_id = callee.params[idx + arg_offset].type_id;
        let rhs_type_id = checked_arg.type_id();

        check_types_for_compat(
          lhs_type_id,
          rhs_type_id,
          &mut generic_substitutions,
          call.args[idx].span(),
          project,
        )?;
        checked_args.push(checked_arg);

        idx += 1;
      }
    }

    if let Some(type_hint_id) = type_hint {
      if type_hint_id != UNKNOWN_TYPE_ID {
        check_types_for_compat(
          return_type_id,
          type_hint_id,
          &mut generic_substitutions,
          *span,
          project,
        )?;
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
          return Err(Error::new(
            *span,
            "not all generic parameters have known types".into(),
          ));
        }
      }
    }
  }

  Ok(CheckedCall {
    namespace: resolved_namespaces,
    name: call.name.clone(),
    args: checked_args,
    type_args,
    linkage,
    type_id: return_type_id,
  })
}

fn typecheck_expression(
  expr: &ParsedExpression,
  scope_id: ScopeId,
  type_hint_id: Option<TypeId>,
  project: &mut Project,
) -> Result<CheckedExpression> {
  let unify_with_type_hint = |project: &mut Project, type_id: &TypeId| -> Result<TypeId> {
    if let Some(type_hint_id) = type_hint_id {
      if type_hint_id == UNKNOWN_TYPE_ID {
        return Ok(*type_id);
      }

      let mut inferences = HashMap::new();
      check_types_for_compat(
        type_hint_id,
        *type_id,
        &mut inferences,
        expr.span(),
        project,
      )?;

      return Ok(substitute_typevars_in_type(*type_id, &inferences, project));
    }

    Ok(*type_id)
  };

  match expr {
    ParsedExpression::NumericConstant(constant, span) => {
      let type_id = if type_hint_id.is_some()
        && constant.type_id() != type_hint_id.unwrap_or(UNKNOWN_TYPE_ID)
      {
        type_hint_id.unwrap()
      } else {
        unify_with_type_hint(project, &constant.type_id())?
      };
      Ok(CheckedExpression::NumericConstant(
        constant.clone(),
        type_id,
        *span,
      ))
    }

    ParsedExpression::Var(name, span) => {
      if let Some(var) = project.find_var_in_scope(scope_id, name) {
        let _ = unify_with_type_hint(project, &var.type_id)?;
        return Ok(CheckedExpression::Variable(var, *span));
      } else {
        return Err(Error::new(*span, format!("undeclared variable `{}`", name)));
      }
    }

    ParsedExpression::Call(call, span) => {
      let checked_call = typecheck_call(call, scope_id, span, project, None, None, type_hint_id)?;

      let type_id = unify_with_type_hint(project, &checked_call.type_id)?;

      Ok(CheckedExpression::Call(checked_call, type_id, *span))
    }

    ParsedExpression::MethodCall(expr, call, span) => {
      let checked_expr = typecheck_expression(expr, scope_id, None, project)?;

      let checked_expr_type = &project.types[checked_expr.type_id()];
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
          )?;

          let type_id = unify_with_type_hint(project, &checked_call.type_id)?;
          Ok(CheckedExpression::MethodCall(
            Box::new(checked_expr),
            checked_call,
            type_id,
            *span,
          ))
        }

        _ => Err(Error::new(
          expr.span(),
          format!(
            "no methods available on type `{}`",
            project.typename_for_type_id(checked_expr.type_id())
          ),
        )),
      }
    }

    ParsedExpression::BinaryOp(lhs, op, rhs, span) => {
      let checked_lhs = typecheck_expression(lhs, scope_id, None, project)?;
      let checked_rhs = typecheck_expression(rhs, scope_id, None, project)?;

      let type_id =
        typecheck_binary_operator(&checked_lhs, op.clone(), &checked_rhs, *span, project)?;
      let type_id = unify_with_type_hint(project, &type_id)?;

      Ok(CheckedExpression::BinaryOp(
        Box::new(checked_lhs),
        op.clone(),
        Box::new(checked_rhs),
        type_id,
        *span,
      ))
    }

    ParsedExpression::IndexedExpression(expr, idx, span) => {
      let checked_expr = typecheck_expression(expr, scope_id, None, project)?;
      let checked_idx = typecheck_expression(idx, scope_id, None, project)?;

      let expr_type_id = checked_expr.type_id();

      match project.types[expr_type_id] {
        CheckedType::RawPtr(type_id, span) => {
          let expr_type_id = unify_with_type_hint(project, &type_id)?;
          Ok(CheckedExpression::IndexedExpression(
            Box::new(checked_expr),
            Box::new(checked_idx),
            expr_type_id,
            span,
          ))
        }

        _ => {
          return Err(Error::new(
            *span,
            format!(
              "cannot index type {}",
              project.typename_for_type_id(expr_type_id)
            ),
          ));
        }
      }
    }

    _ => todo!("typecheck_expression: {:?}", expr),
  }
}

fn typecheck_binary_operator(
  lhs: &CheckedExpression,
  op: BinaryOperator,
  rhs: &CheckedExpression,
  span: Span,
  project: &mut Project,
) -> Result<TypeId> {
  let lhs_type_id = lhs.type_id();
  let rhs_type_id = rhs.type_id();

  let mut type_id = lhs.type_id();
  match op {
    BinaryOperator::LessThan
    | BinaryOperator::LessThanEquals
    | BinaryOperator::GreaterThan
    | BinaryOperator::GreaterThanEquals
    | BinaryOperator::Equals
    | BinaryOperator::NotEquals => {
      if lhs_type_id != rhs_type_id {
        return Err(Error::new(
          span,
          format!("binary operation between incompatible types ({} and {})", 
            project.typename_for_type_id(lhs_type_id),
            project.typename_for_type_id(rhs_type_id),
          )
        ));
      }

      type_id = BOOL_TYPE_ID;
    }
    // BinaryOperator::LogicalAnd | BinaryOperator::LogicalOr => {
    //   if lhs_type_id != BOOL_TYPE_ID {
    //     return Err(Error::new(
    //       span,
    //       "left side of logical binary operation is not a boolean".to_string(),
    //     ));
    //   }

    //   if rhs_type_id != BOOL_TYPE_ID {
    //     return Err(Error::new(
    //       "right side of logical binary operation is not a boolean".to_string(),
    //       span,
    //     ));
    //   }

    //   type_id = BOOL_TYPE_ID;
    // }
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
        return Err(
          Error::new(
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
        return Err(Error::new(
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
        return Err(Error::new(
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

  Ok(type_id)
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
) -> Result<()> {
  let lhs_type = &project.types[lhs_type_id];

  let optional_type_decl_id = project
    .find_type_decl_in_scope(0, "Optional")
    .expect("internal error: unable to locate builtin `Optional` type");

  if let CheckedType::GenericInstance(lhs_type_decl_id, args, _) = lhs_type {
    if *lhs_type_decl_id == optional_type_decl_id
      && args.first().map_or(false, |arg_id| *arg_id == rhs_type_id)
    {
      return Ok(());
    }
  }

  match lhs_type {
    CheckedType::TypeVariable(_, _, _) => {
      if let Some(seen_type_id) = specializations.get(&lhs_type_id) {
        if rhs_type_id != *seen_type_id {
          return Err(Error::new(
            span,
            format!(
              "type mismatch: expected {}, but got {}",
              project.typename_for_type_id(*seen_type_id),
              project.typename_for_type_id(rhs_type_id),
            ),
          ));
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
            // Same struct, perhaps this is an instantiation of it

            let lhs_struct = &project.type_decls[*lhs_type_decl_id];
            if rhs_args.len() != lhs_args.len() {
              return Err(Error::new(
                span,
                format!(
                  "mismatched number of generic parameters for {}",
                  lhs_struct.name
                ),
              ));
            }

            let mut idx = 0;

            while idx < lhs_args.len() {
              let lhs_arg_type_id = lhs_args[idx];
              let rhs_arg_type_id = rhs_args[idx];

              if let Err(err) = check_types_for_compat(
                lhs_arg_type_id,
                rhs_arg_type_id,
                specializations,
                span,
                project,
              ) {
                return Err(err);
              }
              idx += 1;
            }
          }
        }
        _ => {
          if rhs_type_id != lhs_type_id {
            // They're the same type, might be okay to just leave now
            return Err(Error::new(
              span,
              format!(
                "type mismatch: expected {}, but got {}",
                project.typename_for_type_id(lhs_type_id),
                project.typename_for_type_id(rhs_type_id),
              ),
            ));
          }
        }
      }
    }
    CheckedType::TypeDecl(lhs_type_decl_id, _) => {
      if rhs_type_id == lhs_type_id {
        // They're the same type, might be okay to just leave now
        return Ok(());
      }

      let rhs_type = &project.types[rhs_type_id];
      match rhs_type {
        CheckedType::GenericInstance(rhs_type_decl_id, args, _) => {
          if lhs_type_decl_id == rhs_type_decl_id {
            let args = args.clone();
            // Same struct, perhaps this is an instantiation of it

            let lhs_struct = &project.type_decls[*lhs_type_decl_id];
            if args.len() != lhs_struct.generic_parameters.len() {
              return Err(Error::new(
                span,
                format!(
                  "mismatched number of generic parameters for {}",
                  lhs_struct.name
                ),
              ));
            }

            let mut idx = 0;

            let lhs_arg_type_id = lhs_struct.generic_parameters[idx];
            let rhs_arg_type_id = args[idx];

            while idx < args.len() {
              if let Err(err) = check_types_for_compat(
                lhs_arg_type_id,
                rhs_arg_type_id,
                specializations,
                span,
                project,
              ) {
                return Err(err);
              }
              idx += 1;
            }
          }
        }
        _ => {
          if rhs_type_id != lhs_type_id {
            // They're the same type, might be okay to just leave now
            return Err(Error::new(
              span,
              format!(
                "type mismatch: expected {}, but got {}",
                project.typename_for_type_id(lhs_type_id),
                project.typename_for_type_id(rhs_type_id),
              ),
            ));
          }
        }
      }
    }
    _ => {
      if rhs_type_id != lhs_type_id {
        return Err(Error::new(
          span,
          format!(
            "type mismatch: expected {}, but got {}",
            project.typename_for_type_id(lhs_type_id),
            project.typename_for_type_id(rhs_type_id),
          ),
        ));
      }
    }
  }

  Ok(())
}

pub fn typecheck_typename(
  unchecked_type: &ParsedType,
  scope_id: ScopeId,
  project: &mut Project,
) -> Result<TypeId> {
  match unchecked_type {
    ParsedType::Name(name, span) => match name.as_str() {
      "void" => Ok(crate::compiler::VOID_TYPE_ID),
      "i8" => Ok(crate::compiler::I8_TYPE_ID),
      "i16" => Ok(crate::compiler::I16_TYPE_ID),
      "i32" => Ok(crate::compiler::I32_TYPE_ID),
      "i64" => Ok(crate::compiler::I64_TYPE_ID),
      "isz" => Ok(crate::compiler::ISZ_TYPE_ID),
      "u8" => Ok(crate::compiler::U8_TYPE_ID),
      "u16" => Ok(crate::compiler::U16_TYPE_ID),
      "u32" => Ok(crate::compiler::U32_TYPE_ID),
      "u64" => Ok(crate::compiler::U64_TYPE_ID),
      "usz" => Ok(crate::compiler::USZ_TYPE_ID),
      "string" => Ok(crate::compiler::STRING_TYPE_ID),
      "bool" => Ok(crate::compiler::BOOL_TYPE_ID),
      x => {
        let type_id = project.find_type_in_scope(scope_id, x);
        match type_id {
          Some(type_id) => Ok(type_id),
          None => Err(Error::new(*span, "unknown type".to_string())),
        }
      }
    },
    ParsedType::GenericType(name, inner_types, span) => {
      let mut checked_inner_types = vec![];

      for inner_type in inner_types {
        let inner_type_id = typecheck_typename(inner_type, scope_id, project)?;
        checked_inner_types.push(inner_type_id);
      }

      let type_decl_id = project.find_type_decl_in_scope(scope_id, name);

      if let Some(type_decl_id) = type_decl_id {
        Ok(project.find_or_add_type_id(CheckedType::GenericInstance(
          type_decl_id,
          checked_inner_types,
          *span,
        )))
      } else {
        Err(Error::new(*span, format!("could not find {}", name)))
      }
    }
    ParsedType::RawPointer(inner, span) => {
      let inner_type_id = typecheck_typename(inner, scope_id, project)?;
      let type_id = project.find_or_add_type_id(CheckedType::RawPtr(inner_type_id, *span));
      Ok(type_id)
    }
    ParsedType::Optional(inner, span) => {
      let inner_type_id = typecheck_typename(inner, scope_id, project)?;
      let optional_type_decl_id = project
        .find_type_decl_in_scope(0, "Optional")
        .expect("internal error: Optional builtin definition not found");
      let type_id = project.find_or_add_type_id(CheckedType::GenericInstance(
        optional_type_decl_id,
        vec![inner_type_id],
        *span,
      ));

      Ok(type_id)
    }
    ParsedType::Empty => Ok(UNKNOWN_TYPE_ID),
  }
}
