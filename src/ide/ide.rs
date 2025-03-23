use super::symbol::{Symbol, SymbolKind};
use crate::{
  checker::{Project, Scope},
  span::Span,
};

pub fn get_symbols(project: &Project) -> Vec<Symbol> {
  let mut symbols = vec![];

  for function in &project.functions {
    symbols.push(Symbol {
      name: function.name.clone(),
      detail: None,
      kind: SymbolKind::Function,
      range: function.name_span,
      selection_range: function.name_span,
      children: vec![],
    });
  }

  symbols
}

pub fn get_symbol_at_position(project: &Project, position: usize) -> Option<Symbol> {
  find_symbol_in_project(project, position)
}

fn find_symbol_in_project(project: &Project, position: usize) -> Option<Symbol> {
  for scope in &project.scopes {
    if let Some(symbol) = find_symbol_in_scope(scope, position) {
      return Some(symbol);
    }
  }

  None
}

fn find_symbol_in_scope(scope: &Scope, position: usize) -> Option<Symbol> {
  for var in &scope.vars {
    if var.span.contains(position) {
      return Some(Symbol {
        name: var.name.clone(),
        detail: None,
        kind: SymbolKind::Variable,
        range: var.span,
        selection_range: var.span,
        children: vec![],
      });
    }
  }

  None
}

pub fn get_symbol_signature(project: &mut Project, symbol: &Symbol) -> String {
  let mut signature = String::new();

  if let Some(function) = project.get_function_by_name(&symbol.name) {
    let mut param_types = vec![];
    for param in &function.params {
      param_types.push(project.typename_for_type_id(param.type_id));
    }
    let return_type = project.typename_for_type_id(function.return_type_id);
    signature.push_str(&format!(
      "fn {}({}) -> {};",
      symbol.name,
      param_types.join(", "),
      return_type
    ));
  }

  for scope in &project.scopes {
    for variable in &scope.vars {
      if variable.name == symbol.name {
        signature.push_str(&format!(
          "{} {}: {};",
          if variable.mutable { "mut" } else { "let" },
          variable.name,
          project.typename_for_type_id(variable.type_id)
        ));
      }
    }
  }

  signature
}
