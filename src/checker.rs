pub type TypeId = usize;
pub type ScopeId = usize;
pub type FunctionId = usize;

#[derive(Debug)]
pub struct Scope {
  pub namespace_name: Option<String>,
  pub vars: Vec<CheckedVariable>,
  pub functions: Vec<(String, FunctionId)>,
  pub types: Vec<(String, TypeId)>,
  pub parent: Option<ScopeId>,
  pub children: Vec<ScopeId>,
}

#[derive(Debug)]
pub struct CheckedNamespace {
  pub(crate) name: Option<String>,
  pub(crate) scope_id: ScopeId,
}

#[derive(Debug)]
pub struct CheckedVariable {
  pub(crate) name: String,
  pub(crate) type_id: TypeId,
  pub(crate) mutable: bool,
}
