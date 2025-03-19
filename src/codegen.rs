use crate::{
  ast::DefinitionLinkage,
  checker::{CheckedType, CheckedTypeDecl, CheckedTypeKind, Project, Scope, TypeId},
  compiler,
};

pub fn codegen(project: &Project, scope: &Scope) -> String {
  let mut output = String::new();

  output.push_str("#include \"runtime/lib.h\"\n");
  output.push_str(&codegen_namespace(project, scope));

  output
}

fn codegen_namespace(project: &Project, scope: &Scope) -> String {
  let mut output = String::new();

  for child_scope_id in &scope.children {
    let child_scope = &project.scopes[*child_scope_id];
    if let Some(name) = &child_scope.namespace_name {
      output.push_str(&format!("namespace {name} {{\n"));
      output.push_str(&codegen_namespace(project, child_scope));
      output.push_str("}\n");
    }
  }

  for (_, type_decl_id) in &scope.type_decls {
    let type_decl = &project.type_decls[*type_decl_id];
    let type_decl_output = codegen_type_decl_predecl(type_decl, project);

    if !type_decl_output.is_empty() {
      output.push_str(&type_decl_output);
      output.push('\n');
    }
  }

  output
}

fn codegen_type_decl_predecl(type_decl: &CheckedTypeDecl, project: &Project) -> String {
  if !matches!(type_decl.kind, CheckedTypeKind::Class(_)) {
    String::new()
  } else if type_decl.linkage == DefinitionLinkage::External {
    String::new()
  } else {
    let mut output = String::new();

    if !type_decl.generic_parameters.is_empty() {
      output.push_str("template <");

      for (idx, param) in type_decl.generic_parameters.iter().enumerate() {
        let checked_type = &project.types[*param];
        if let CheckedType::TypeVariable(name, constraint, span) = checked_type {
          if let Some(constraint) = constraint {
            output.push_str(&codegen_type(*constraint, project));
            output.push(' ');
          } else {
            output.push_str("typename ");
          }

          output.push_str(&name);
        }

        if idx + 1 < type_decl.generic_parameters.len() {
          output.push_str(", ");
        }
      }
      output.push_str(">\n");
    }

    output.push_str(match type_decl.kind {
      CheckedTypeKind::Class(_) => "class",
      CheckedTypeKind::Interface(_) | CheckedTypeKind::Alias(_) => unreachable!(),
    });
    output.push_str(&format!(" {};", type_decl.name.clone()));

    output
  }
}

fn codegen_type(type_id: TypeId, project: &Project) -> String {
  let mut output = String::new();
  let ty = &project.types[type_id];

  match ty {
    CheckedType::Builtin(_) => match type_id {
      compiler::VOID_TYPE_ID => "void".to_string(),
      compiler::I8_TYPE_ID => "i8".to_string(),
      compiler::I16_TYPE_ID => "i16".to_string(),
      compiler::I32_TYPE_ID => "i32".to_string(),
      compiler::I64_TYPE_ID => "i64".to_string(),
      compiler::I128_TYPE_ID => "i128".to_string(),
      compiler::ISZ_TYPE_ID => "isz".to_string(),
      compiler::U8_TYPE_ID => "u8".to_string(),
      compiler::U16_TYPE_ID => "u16".to_string(),
      compiler::U32_TYPE_ID => "u32".to_string(),
      compiler::U64_TYPE_ID => "u64".to_string(),
      compiler::U128_TYPE_ID => "u128".to_string(),
      compiler::USZ_TYPE_ID => "usz".to_string(),
      compiler::F32_TYPE_ID => "f32".to_string(),
      compiler::F64_TYPE_ID => "f64".to_string(),
      compiler::STRING_TYPE_ID => "string".to_string(),
      compiler::BOOL_TYPE_ID => "bool".to_string(),
      _ => "auto".to_string(),
    },
    CheckedType::TypeVariable(name, _, _) => name.clone(),
    CheckedType::GenericInstance(_, items, span) => todo!(),
    CheckedType::TypeDecl(type_decl_id, span) => {
      let type_decl = &project.type_decls[*type_decl_id];
      type_decl.name.clone()
    }
    CheckedType::RawPtr(_, span) => todo!(),
  }
}
