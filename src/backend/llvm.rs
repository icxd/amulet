use std::{cell::RefCell, collections::HashMap, path::PathBuf};

use inkwell::{
  attributes::AttributeLoc,
  builder::Builder,
  context::{AsContextRef, Context},
  debug_info::{DICompileUnit, DWARFEmissionKind, DWARFSourceLanguage, DebugInfoBuilder},
  llvm_sys::core::{
    LLVMArrayType2, LLVMFunctionType, LLVMPointerType, LLVMStructCreateNamed, LLVMStructSetBody,
  },
  module::{Linkage, Module},
  targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple},
  types::{
    ArrayType, AsTypeRef, BasicMetadataTypeEnum, BasicTypeEnum, FunctionType, PointerType,
    StructType,
  },
  values::{
    BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, GlobalValue, PointerValue,
  },
  AddressSpace, FloatPredicate, IntPredicate,
};

use crate::{
  ast::{inline_asm::OperandType, BinaryOperator, DefinitionLinkage, NumericConstant},
  checker::{
    CheckedBlock, CheckedEnum, CheckedEnumVariant, CheckedExpression, CheckedFunction,
    CheckedFunctionAttribute, CheckedInlineAsmOperandAction, CheckedStatement, CheckedType,
    CheckedTypeDecl, CheckedTypeKind, CheckedUnaryOperator, Project, TypeId,
  },
  compiler::VOID_TYPE_ID,
  Opts,
};

fn size_of(project: &mut Project, type_id: TypeId) -> usize {
  let ty = project.types[type_id].clone();
  match ty {
    CheckedType::Builtin(_) => match type_id {
      crate::compiler::UNKNOWN_TYPE_ID => unreachable!("size_of({type_id}) UNKNOWN_TYPE_ID"),
      crate::compiler::VOID_TYPE_ID => 0,
      crate::compiler::I8_TYPE_ID => std::mem::size_of::<i8>(),
      crate::compiler::I16_TYPE_ID => std::mem::size_of::<i16>(),
      crate::compiler::I32_TYPE_ID => std::mem::size_of::<i32>(),
      crate::compiler::I64_TYPE_ID => std::mem::size_of::<i64>(),
      crate::compiler::I128_TYPE_ID => std::mem::size_of::<i128>(),
      crate::compiler::ISZ_TYPE_ID => std::mem::size_of::<isize>(),
      crate::compiler::U8_TYPE_ID => std::mem::size_of::<u8>(),
      crate::compiler::U16_TYPE_ID => std::mem::size_of::<u16>(),
      crate::compiler::U32_TYPE_ID => std::mem::size_of::<u32>(),
      crate::compiler::U64_TYPE_ID => std::mem::size_of::<u64>(),
      crate::compiler::U128_TYPE_ID => std::mem::size_of::<u128>(),
      crate::compiler::USZ_TYPE_ID => std::mem::size_of::<usize>(),
      crate::compiler::F32_TYPE_ID => std::mem::size_of::<f32>(),
      crate::compiler::F64_TYPE_ID => std::mem::size_of::<f64>(),
      crate::compiler::BOOL_TYPE_ID => std::mem::size_of::<bool>(),
      crate::compiler::CCHAR_TYPE_ID => std::mem::size_of::<std::ffi::c_char>(),
      crate::compiler::RAWPTR_TYPE_ID => std::mem::size_of::<*const ()>(),
      _ => unreachable!("size_of({type_id}) unknown builtin"),
    },
    CheckedType::TypeVariable(_, _, _) => unreachable!("size_of({type_id}) TypeVariable"),
    CheckedType::GenericInstance(_, _, _) => todo!(),
    CheckedType::GenericEnumInstance(_, _, _) => todo!(),
    CheckedType::TypeDecl(type_decl_id, _) => {
      let mut type_decl = project.type_decls[type_decl_id].clone();
      let class = type_decl.kind.as_class_mut().unwrap();
      class
        .fields
        .iter()
        .map(|field| size_of(project, field.type_id))
        .sum()
    }
    CheckedType::Enum(enum_id, _) => {
      let enum_ = project.enums[enum_id].clone();
      let mut size = 0;
      for variant in enum_.variants.clone() {
        size = size.max(size_of_variant(project, &variant));
      }
      size
    }
    CheckedType::RawPtr(_, _) => std::mem::size_of::<*const ()>(),
    CheckedType::Slice(type_id, size, _) => {
      let inner_type_size = size_of(project, type_id);
      inner_type_size * size
    }
  }
}

#[derive(Debug)]
pub struct LLVMBackend<'ctx> {
  pub path: &'ctx PathBuf,
  pub context: &'ctx Context,
  pub module: Module<'ctx>,
  pub builder: Builder<'ctx>,
  pub machine: TargetMachine,
  pub debug_info_builder: DebugInfoBuilder<'ctx>,
  pub compile_unit: DICompileUnit<'ctx>,

  pub current_function: Option<FunctionValue<'ctx>>,
  pub constants: RefCell<HashMap<String, BasicTypeEnum<'ctx>>>,
  pub variables: RefCell<HashMap<String, BasicTypeEnum<'ctx>>>,
  pub variable_ptrs: RefCell<HashMap<String, PointerValue<'ctx>>>,
  pub functions: RefCell<HashMap<String, FunctionValue<'ctx>>>,
  pub struct_types: RefCell<HashMap<String, StructType<'ctx>>>,
  pub struct_fields: RefCell<HashMap<String, Vec<String>>>,
  pub enum_types: RefCell<HashMap<String, StructType<'ctx>>>,
  pub enum_members: RefCell<HashMap<String, Vec<String>>>,
}

impl<'ctx> LLVMBackend<'ctx> {
  pub fn new(opts: Opts, path: &'ctx PathBuf, context: &'ctx Context) -> Self {
    let target_config = InitializationConfig::default();
    Target::initialize_native(&target_config).expect("Failed to initialize native machine target!");
    Target::initialize_all(&target_config);

    let module = context.create_module(&path.display().to_string());
    let builder = context.create_builder();

    let triple = match opts.target.as_ref() {
      Some(target) => TargetTriple::create(target.as_str()),
      None => TargetMachine::get_default_triple(),
    };

    let target = Target::from_triple(&triple).expect("invalid target triple");
    let machine = target
      .create_target_machine(
        &triple,
        "generic",
        "",
        opts.opt_level.into(),
        RelocMode::Default,
        CodeModel::Default,
      )
      .unwrap();

    let (debug_info_builder, compile_unit) = module.create_debug_info_builder(
      false,
      DWARFSourceLanguage::C,
      &path.clone().display().to_string(),
      &path.clone().parent().unwrap().display().to_string(),
      "amulet",
      false,
      "",
      1,
      "",
      DWARFEmissionKind::Full,
      0,
      false,
      true,
      "/",
      "",
    );

    Self {
      path,
      context,
      module,
      builder,
      machine,
      debug_info_builder,
      compile_unit,
      current_function: None,
      constants: RefCell::new(HashMap::new()),
      variables: RefCell::new(HashMap::new()),
      variable_ptrs: RefCell::new(HashMap::new()),
      functions: RefCell::new(HashMap::new()),
      struct_types: RefCell::new(HashMap::new()),
      struct_fields: RefCell::new(HashMap::new()),
      enum_types: RefCell::new(HashMap::new()),
      enum_members: RefCell::new(HashMap::new()),
    }
  }
}

pub fn compile_namespace<'ctx>(backend: &mut LLVMBackend<'ctx>, project: &mut Project) {
  for constant in &project.constants.clone() {
    let compiled_type = compile_type(backend, project, constant.type_id);
    let value = compile_expression(backend, project, &constant.value);
    let global_value = backend.module.add_global(
      compiled_type,
      Some(AddressSpace::from(4u16)),
      &constant.name,
    );
    global_value.set_initializer(&value.unwrap());
    global_value.set_linkage(Linkage::External);
    global_value.set_constant(true);

    backend
      .constants
      .borrow_mut()
      .insert(constant.name.clone(), compiled_type);
  }

  for type_decl in &project.type_decls.clone() {
    compile_type_decl_predecl(backend, project, type_decl);
  }

  for enum_ in &project.enums.clone() {
    compile_enum_predecl(backend, project, enum_);
  }

  for function in &project.functions.clone() {
    compile_function_predecl(backend, project, function);
  }

  for type_decl in &project.type_decls.clone() {
    compile_type_decl(backend, project, type_decl);
  }

  for enum_ in &project.enums.clone() {
    compile_enum(backend, project, enum_);
  }

  for function in &project.functions.clone() {
    compile_function(backend, project, function);
  }
}

fn compile_type_decl_predecl<'ctx>(
  backend: &mut LLVMBackend<'ctx>,
  _project: &mut Project,
  type_decl: &CheckedTypeDecl,
) {
  let CheckedTypeKind::Class(_class) = &type_decl.kind else {
    return;
  };

  // Insert opaque type before compiling the struct
  let opaque_type = unsafe {
    let struct_type = LLVMStructCreateNamed(
      backend.context.as_ctx_ref(),
      type_decl.name.clone().as_mut_ptr() as *mut _,
    );
    StructType::new(struct_type)
  };
  backend
    .struct_types
    .borrow_mut()
    .insert(type_decl.name.clone(), opaque_type);
}

fn compile_enum_predecl<'ctx>(
  backend: &mut LLVMBackend<'ctx>,
  _project: &mut Project,
  enum_: &CheckedEnum,
) {
  let opaque_type = unsafe {
    let struct_type = LLVMStructCreateNamed(
      backend.context.as_ctx_ref(),
      enum_.name.clone().as_mut_ptr() as *mut _,
    );
    StructType::new(struct_type)
  };
  backend
    .enum_types
    .borrow_mut()
    .insert(enum_.name.clone(), opaque_type);
}

fn compile_type_decl<'ctx>(
  backend: &mut LLVMBackend<'ctx>,
  project: &mut Project,
  type_decl: &CheckedTypeDecl,
) {
  let CheckedTypeKind::Class(class) = &type_decl.kind else {
    return;
  };

  let field_type_ids = class
    .fields
    .iter()
    .map(|field| field.type_id)
    .collect::<Vec<_>>();

  let mut field_types = vec![];
  for type_id in &field_type_ids {
    let field_type = compile_type(backend, project, *type_id);
    field_types.push(field_type);
  }
  let struct_type = unsafe {
    let struct_type = backend
      .struct_types
      .borrow_mut()
      .get(type_decl.name.as_str())
      .expect("internal error: failed to get struct type")
      .as_type_ref();
    LLVMStructSetBody(
      struct_type,
      field_types
        .iter()
        .map(|ty| ty.as_type_ref())
        .collect::<Vec<_>>()
        .as_mut_ptr(),
      field_types.len() as u32,
      false as i32,
    );
    StructType::new(struct_type)
  };

  let field_names = class
    .fields
    .iter()
    .map(|field| field.name.clone())
    .collect::<Vec<_>>();

  backend
    .struct_types
    .borrow_mut()
    .insert(type_decl.name.clone(), struct_type);
  backend
    .struct_fields
    .borrow_mut()
    .insert(type_decl.name.clone(), field_names);
}

fn size_of_variant(project: &mut Project, variant: &CheckedEnumVariant) -> usize {
  match variant {
    CheckedEnumVariant::Untyped(_, _) => 0,
    CheckedEnumVariant::WithValue(_, expr, _) => {
      let type_id = expr.type_id(project);
      size_of(project, type_id)
    }
    CheckedEnumVariant::TupleLike(_, type_ids, _) => {
      type_ids.iter().map(|id| size_of(project, *id)).sum()
    }
    CheckedEnumVariant::StructLike(_, variables, _) => variables
      .iter()
      .map(|var| size_of(project, var.type_id))
      .sum(),
  }
}

fn compile_enum<'ctx>(backend: &mut LLVMBackend<'ctx>, project: &mut Project, enum_: &CheckedEnum) {
  let binding = backend.enum_types.borrow_mut().clone();
  let enum_type = binding
    .get(enum_.name.as_str())
    .expect("internal error: failed to get enum type");

  let mut members = vec![];
  for variant in enum_.variants.clone() {
    members.push(match variant {
      CheckedEnumVariant::Untyped(name, _)
      | CheckedEnumVariant::WithValue(name, _, _)
      | CheckedEnumVariant::TupleLike(name, _, _)
      | CheckedEnumVariant::StructLike(name, _, _) => name,
    });
  }

  // Get the size of all the variants
  let mut member_sizes = vec![];
  for variant in enum_.variants.clone() {
    let size = size_of_variant(project, &variant);
    member_sizes.push(size);
  }

  // Get the biggest size
  let max_size = member_sizes
    .iter()
    .max()
    .expect("internal error: failed to get max size");

  unsafe {
    LLVMStructSetBody(
      enum_type.as_type_ref(),
      [
        backend.context.i8_type().as_type_ref(),
        backend
          .context
          .i8_type()
          .array_type(*max_size as u32)
          .as_type_ref(),
      ]
      .as_mut_ptr(),
      2 as u32,
      false as i32,
    );
  }

  for (variant, name) in enum_.variants.iter().zip(members.iter()) {
    if let CheckedEnumVariant::Untyped(_, _) | CheckedEnumVariant::WithValue(_, _, _) = variant {
      continue;
    }

    let variant_type = unsafe {
      StructType::new(LLVMStructCreateNamed(
        backend.context.as_ctx_ref(),
        format!("{}_{}", enum_.name, name).as_mut_ptr() as *mut _,
      ))
    };
    match variant {
      CheckedEnumVariant::Untyped(_, _) | CheckedEnumVariant::WithValue(_, _, _) => unreachable!(),
      CheckedEnumVariant::TupleLike(_, type_ids, _) => {
        let mut variant_types = vec![backend.context.i8_type().as_type_ref()];
        for type_id in type_ids {
          let variant_type = compile_type(backend, project, *type_id);
          variant_types.push(variant_type.as_type_ref());
        }

        unsafe {
          LLVMStructSetBody(
            variant_type.as_type_ref(),
            variant_types.as_mut_ptr() as *mut _,
            variant_types.len() as u32,
            false as i32,
          );
        }
      }
      CheckedEnumVariant::StructLike(_, fields, _) => continue,
    }
  }

  backend
    .enum_members
    .borrow_mut()
    .insert(enum_.name.clone(), members);
}

fn compile_function_predecl<'ctx>(
  backend: &mut LLVMBackend<'ctx>,
  project: &mut Project,
  function: &CheckedFunction,
) {
  let return_type = if function.return_type_id == VOID_TYPE_ID {
    backend.context.void_type().as_type_ref()
  } else {
    compile_type(backend, project, function.return_type_id).as_type_ref()
  };

  let param_type_ids = function
    .params
    .iter()
    .map(|param| param.type_id)
    .collect::<Vec<_>>();
  let param_names = function
    .params
    .iter()
    .map(|param| param.name.as_str())
    .collect::<Vec<_>>();

  let mut param_types = vec![];
  for (name, type_id) in param_names.iter().zip(param_type_ids.iter()) {
    let param_type = compile_type(backend, project, *type_id);
    param_types.push(param_type.as_type_ref());
    backend
      .variables
      .borrow_mut()
      .insert(name.to_string(), param_type);
  }

  let fn_type = unsafe {
    FunctionType::new(LLVMFunctionType(
      return_type,
      param_types.as_mut_ptr(),
      param_types.len() as u32,
      false as i32,
    ))
  };

  let fn_value = backend.module.add_function(
    &function.name,
    fn_type,
    if function.linkage == DefinitionLinkage::External {
      Some(Linkage::External)
    } else {
      None
    },
  );
  backend
    .functions
    .borrow_mut()
    .insert(function.name.clone(), fn_value);
}

fn compile_function<'ctx>(
  backend: &mut LLVMBackend<'ctx>,
  project: &mut Project,
  function: &CheckedFunction,
) {
  let param_type_ids = function
    .params
    .iter()
    .map(|param| param.type_id)
    .collect::<Vec<_>>();
  let param_names = function
    .params
    .iter()
    .map(|param| param.name.as_str())
    .collect::<Vec<_>>();

  let mut param_types = vec![];
  for (name, type_id) in param_names.iter().zip(param_type_ids.iter()) {
    let param_type = compile_type(backend, project, *type_id);
    param_types.push(param_type.as_type_ref());
    backend
      .variables
      .borrow_mut()
      .insert(name.to_string(), param_type);
  }

  match &function.linkage {
    DefinitionLinkage::Internal => {
      let fn_value = *backend
        .functions
        .borrow_mut()
        .get(function.name.as_str())
        .unwrap();
      backend.current_function = Some(fn_value);

      let entry_basic_block = backend.context.append_basic_block(fn_value, "entry");
      backend.builder.position_at_end(entry_basic_block);

      for (idx, param) in function.params.iter().enumerate() {
        let param_type = compile_type(backend, project, param_type_ids[idx]);
        let ptr = backend
          .builder
          .build_alloca(param_type, param.name.as_str())
          .expect("internal error: failed to allocate param");
        backend
          .builder
          .build_store(ptr, fn_value.get_nth_param(idx as u32).unwrap())
          .expect("internal error: failed to store param");
        backend
          .variable_ptrs
          .borrow_mut()
          .insert(param.name.to_string(), ptr);
      }

      compile_block(backend, project, function.block.clone());

      if function
        .attributes
        .contains(&CheckedFunctionAttribute::NoReturn)
      {
        let attribute = backend.context.create_string_attribute("noreturn", "");
        fn_value.add_attribute(AttributeLoc::Return, attribute);
        backend.builder.build_unreachable().unwrap();
      } else if function.return_type_id == VOID_TYPE_ID {
        backend.builder.build_return(None).unwrap();
      }

      backend.current_function = None;
    }
    DefinitionLinkage::External => {}
    DefinitionLinkage::ImplicitConstructor => {
      let fn_value = *backend
        .functions
        .borrow_mut()
        .get(function.name.as_str())
        .unwrap();

      let entry_basic_block = backend.context.append_basic_block(fn_value, "entry");
      backend.builder.position_at_end(entry_basic_block);

      let struct_type = backend
        .struct_types
        .borrow()
        .get(function.name.as_str())
        .unwrap()
        .clone();

      let mut previous_struct = None;
      let heap_struct = backend
        .builder
        .build_alloca(struct_type, "structure.addr")
        .expect("internal error: failed to build alloca");
      let structure = backend
        .builder
        .build_load(struct_type, heap_struct, "structure")
        .expect("internal error: failed to build load");
      for (idx, param) in function.params.iter().enumerate() {
        let param_type = compile_type(backend, project, param.type_id);
        let ptr = backend
          .builder
          .build_alloca(param_type, &format!("{}.addr", param.name))
          .expect("internal error: failed to build alloca");
        backend
          .builder
          .build_store(
            ptr,
            fn_value
              .get_nth_param(idx as u32)
              .expect("internal error: failed to get nth param"),
          )
          .expect("internal error: failed to build store");
        let value = backend
          .builder
          .build_load(param_type, ptr, param.name.as_str())
          .expect("internal error: failed to build load");
        let structure = backend
          .builder
          .build_insert_value(
            previous_struct.unwrap_or(structure).into_struct_value(),
            value,
            idx as u32,
            param.name.as_str(),
          )
          .expect("internal error: failed to build insert value");
        previous_struct = Some(structure.as_basic_value_enum());
      }

      backend
        .builder
        .build_return(Some(&previous_struct.unwrap()))
        .expect("internal error: failed to build return");
    }
    DefinitionLinkage::ImplicitEnumConstructor => {}
  }
}

fn compile_block<'ctx>(
  backend: &mut LLVMBackend<'ctx>,
  project: &mut Project,
  block: CheckedBlock,
) -> () {
  for stmt in &block.stmts {
    compile_statement(backend, project, stmt);
  }
}

fn compile_statement<'ctx>(
  backend: &mut LLVMBackend<'ctx>,
  project: &mut Project,
  stmt: &CheckedStatement,
) {
  match stmt {
    CheckedStatement::VarDecl(var_decl, expr) => {
      let compiled_type = compile_type(backend, project, var_decl.type_id);
      let value = compile_expression(backend, project, expr);

      assert!(value.is_some());
      let value = value.unwrap();

      let alloca = backend
        .builder
        .build_alloca(compiled_type, &format!("{}.addr", var_decl.name))
        .expect("internal error: failed to create alloca");
      backend
        .builder
        .build_store(alloca, value)
        .expect("internal error: failed to store value");
      backend
        .variables
        .borrow_mut()
        .insert(var_decl.name.clone(), compiled_type);
      backend
        .variable_ptrs
        .borrow_mut()
        .insert(var_decl.name.clone(), alloca);
    }

    CheckedStatement::While { condition, block } => {
      let fn_value = backend
        .current_function
        .expect("internal error: no current function");

      let entry_basic_block = backend.context.append_basic_block(fn_value, "while.cond");
      let loop_basic_block = backend.context.append_basic_block(fn_value, "while.body");
      let after_basic_block = backend.context.append_basic_block(fn_value, "while.end");

      backend
        .builder
        .build_unconditional_branch(entry_basic_block)
        .expect("internal error: failed to build branch");

      backend.builder.position_at_end(entry_basic_block);
      let condition = compile_expression(backend, project, condition);

      assert!(condition.is_some());
      let condition = condition.unwrap();

      backend
        .builder
        .build_conditional_branch(
          condition.into_int_value(),
          loop_basic_block,
          after_basic_block,
        )
        .expect("internal error: failed to build branch");

      backend.builder.position_at_end(loop_basic_block);

      compile_block(backend, project, block.clone());

      backend
        .builder
        .build_unconditional_branch(entry_basic_block)
        .expect("internal error: failed to build branch");

      backend.builder.position_at_end(after_basic_block);
    }

    CheckedStatement::Loop(block) => {
      let fn_value = backend
        .current_function
        .expect("internal error: no current function");

      let entry_basic_block = backend.context.append_basic_block(fn_value, "loop.body");
      let after_basic_block = backend.context.append_basic_block(fn_value, "loop.end");

      backend
        .builder
        .build_unconditional_branch(entry_basic_block)
        .expect("internal error: failed to build branch");

      backend.builder.position_at_end(entry_basic_block);
      compile_block(backend, project, block.clone());

      backend
        .builder
        .build_unconditional_branch(entry_basic_block)
        .expect("internal error: failed to build branch");

      backend.builder.position_at_end(after_basic_block);
    }

    CheckedStatement::If {
      condition,
      then_block,
      else_block,
    } => {
      let fn_value = backend
        .current_function
        .expect("internal error: no current function");

      let then_basic_block = backend.context.append_basic_block(fn_value, "if.then");
      let else_basic_block = if else_block.is_some() {
        Some(backend.context.append_basic_block(fn_value, "if.else"))
      } else {
        None
      };
      let end_basic_block = backend.context.append_basic_block(fn_value, "if.end");

      let condition = compile_expression(backend, project, condition);
      assert!(condition.is_some());
      let condition = condition.unwrap();
      backend
        .builder
        .build_conditional_branch(
          condition.into_int_value(),
          then_basic_block,
          else_basic_block.unwrap_or(end_basic_block),
        )
        .expect("internal error: failed to build branch");
      backend.builder.position_at_end(then_basic_block);
      compile_block(backend, project, then_block.clone());
      backend
        .builder
        .build_unconditional_branch(end_basic_block)
        .expect("internal error: failed to build branch");
      if let Some(else_basic_block) = else_basic_block {
        backend.builder.position_at_end(else_basic_block);

        compile_statement(backend, project, else_block.as_ref().unwrap());

        backend
          .builder
          .build_unconditional_branch(end_basic_block)
          .expect("internal error: failed to build branch");
      }
      backend.builder.position_at_end(end_basic_block);
    }

    CheckedStatement::Return(expr) => {
      let value = compile_expression(backend, project, expr);
      backend
        .builder
        .build_return(Some(&value.unwrap()))
        .expect("internal error: failed to build return");
    }

    CheckedStatement::Block(block) => {
      compile_block(backend, project, block.clone());
    }

    CheckedStatement::Expression(expr) => {
      let _ = compile_expression(backend, project, expr);
    }

    _ => todo!("compile_statement not implemented for {:?}", stmt),
  }
}

fn compile_expression<'ctx>(
  backend: &mut LLVMBackend<'ctx>,
  project: &mut Project,
  expr: &CheckedExpression,
) -> Option<BasicValueEnum<'ctx>> {
  match expr {
    CheckedExpression::Null(span, _) => todo!("{:?}", span),

    CheckedExpression::Nullptr(_, type_id) => {
      let compiled_type = compile_type(backend, project, *type_id);
      let value = compiled_type.into_pointer_type().const_null();
      Some(BasicValueEnum::PointerValue(value))
    }

    CheckedExpression::NumericConstant(constant, type_id, _) => {
      let compiled_type = compile_type(backend, project, *type_id);
      match constant {
        NumericConstant::I8(value) => {
          let value = i8::try_from(*value).unwrap();
          Some(BasicValueEnum::IntValue(
            compiled_type.into_int_type().const_int(value as u64, false),
          ))
        }

        NumericConstant::I16(value) => {
          let value = i16::try_from(*value).unwrap();
          Some(BasicValueEnum::IntValue(
            compiled_type.into_int_type().const_int(value as u64, false),
          ))
        }

        NumericConstant::I32(value) => {
          let value = i32::try_from(*value).unwrap();
          Some(BasicValueEnum::IntValue(
            compiled_type.into_int_type().const_int(value as u64, false),
          ))
        }

        NumericConstant::I64(value) => {
          let value = i64::try_from(*value).unwrap();
          Some(BasicValueEnum::IntValue(
            compiled_type.into_int_type().const_int(value as u64, false),
          ))
        }

        NumericConstant::I128(value) => {
          let value = i128::try_from(*value).unwrap();
          Some(BasicValueEnum::IntValue(
            compiled_type.into_int_type().const_int(value as u64, false),
          ))
        }

        NumericConstant::Isz(value) => {
          let value = isize::try_from(*value).unwrap();
          Some(BasicValueEnum::IntValue(
            compiled_type.into_int_type().const_int(value as u64, false),
          ))
        }

        NumericConstant::U8(value) => {
          let value = u8::try_from(*value).unwrap();
          Some(BasicValueEnum::IntValue(
            compiled_type.into_int_type().const_int(value as u64, false),
          ))
        }

        NumericConstant::U16(value) => {
          let value = u16::try_from(*value).unwrap();
          Some(BasicValueEnum::IntValue(
            compiled_type.into_int_type().const_int(value as u64, false),
          ))
        }

        NumericConstant::U32(value) => {
          let value = u32::try_from(*value).unwrap();
          Some(BasicValueEnum::IntValue(
            compiled_type.into_int_type().const_int(value as u64, false),
          ))
        }

        NumericConstant::U64(value) => {
          let value = u64::try_from(*value).unwrap();
          Some(BasicValueEnum::IntValue(
            compiled_type.into_int_type().const_int(value as u64, false),
          ))
        }

        NumericConstant::U128(value) => {
          let value = u128::try_from(*value).unwrap();
          Some(BasicValueEnum::IntValue(
            compiled_type.into_int_type().const_int(value as u64, false),
          ))
        }

        NumericConstant::Usz(value) => {
          let value = usize::try_from(*value).unwrap();
          Some(BasicValueEnum::IntValue(
            compiled_type.into_int_type().const_int(value as u64, false),
          ))
        }

        NumericConstant::F32(value) => {
          let value = f32::try_from(*value).unwrap();
          Some(BasicValueEnum::FloatValue(
            compiled_type.into_float_type().const_float(value as f64),
          ))
        }

        NumericConstant::F64(value) => {
          let value = f64::try_from(*value).unwrap();
          Some(BasicValueEnum::FloatValue(
            compiled_type.into_float_type().const_float(value),
          ))
        }
      }
    }

    CheckedExpression::Boolean(value, _) => Some(BasicValueEnum::IntValue(
      backend.context.bool_type().const_int(*value as u64, false),
    )),

    CheckedExpression::CharacterLiteral(c, _) => {
      let value = i8::try_from(*c as i8).unwrap();
      Some(BasicValueEnum::IntValue(
        backend.context.i8_type().const_int(value as u64, false),
      ))
    }

    CheckedExpression::QuotedString(s, _) => {
      let functions = backend.functions.borrow();
      let string_constructor = functions
        .get("String")
        .expect("internal error: failed to get string constructor");

      let string_const = backend
        .builder
        .build_call(
          string_constructor.clone(),
          &[
            backend
              .builder
              .build_global_string_ptr(&s, "str")
              .expect("internal error: failed to build global string")
              .as_basic_value_enum()
              .into(),
            backend
              .context
              .i64_type()
              .const_int(s.len() as u64, false)
              .as_basic_value_enum()
              .into(),
          ],
          "string",
        )
        .expect("internal error: failed to build call to string constructor");

      Some(string_const.try_as_basic_value().unwrap_left())
    }

    CheckedExpression::QuotedCString(s, _) => {
      let string_value: GlobalValue<'ctx> = unsafe {
        backend
          .builder
          .build_global_string(s.as_str(), "s")
          .expect("internal error: failed to build global string")
      };

      let ptr = backend
        .builder
        .build_bit_cast(
          string_value,
          backend.context.ptr_type(AddressSpace::default()),
          "ptr",
        )
        .expect("internal error: failed to build bitcast");

      Some(BasicValueEnum::PointerValue(ptr.into_pointer_value()))
    }

    CheckedExpression::Variable(var, _) => {
      if let Some(_) = backend.constants.borrow().get(var.name.as_str()) {
        let value = backend
          .module
          .get_global(var.name.as_str())
          .expect("internal error: failed to get global");
        return Some(value.as_basic_value_enum());
      }
      let ty = backend.variables.borrow()[var.name.as_str()].clone();
      let ptr = backend.variable_ptrs.borrow()[var.name.as_str()].clone();
      let value = backend
        .builder
        .build_load(ty, ptr, "")
        .expect("internal error: failed to build load");
      Some(value)
    }

    CheckedExpression::BinaryOp(lhs, op, rhs, _, _) => {
      if matches!(op, BinaryOperator::Assign) {
        match *lhs.clone() {
          CheckedExpression::Variable(var, _) => {
            let type_id = lhs.type_id(project);
            let ty = compile_type(backend, project, type_id);
            let rhs = compile_expression(backend, project, rhs)?;
            let var_ptr = backend.variable_ptrs.borrow()[var.name.as_str()].clone();
            backend
              .builder
              .build_store(var_ptr, rhs)
              .expect("internal error: failed to build store");
            backend.variables.borrow_mut().insert(var.name.clone(), ty);
            backend
              .variable_ptrs
              .borrow_mut()
              .insert(var.name.clone(), var_ptr);
            return None;
          }

          CheckedExpression::UnaryOp(expr, CheckedUnaryOperator::Dereference, _, _) => {
            let lhs = compile_expression(backend, project, &*expr)?;
            let rhs = compile_expression(backend, project, rhs)?;
            backend
              .builder
              .build_store(lhs.into_pointer_value(), rhs)
              .unwrap();
            return None;
          }

          CheckedExpression::IndexedExpression(expr, idx, type_id, _) => {
            let compiled_expr = compile_expression(backend, project, &*expr)?;
            let compiled_idx = compile_expression(backend, project, &*idx)?;
            let compiled_rhs = compile_expression(backend, project, rhs)?;
            let pointee_ty = compile_type(backend, project, type_id);

            let value = unsafe {
              backend
                .builder
                .build_in_bounds_gep(
                  pointee_ty,
                  compiled_expr.into_pointer_value(),
                  &[compiled_idx.into_int_value()],
                  "arrayidx",
                )
                .expect("internal error: failed to build inbounds gep")
            };
            backend
              .builder
              .build_store(value, compiled_rhs)
              .expect("internal error: failed to build store");
            return None;
          }

          _ => unreachable!(),
        }
      }

      let compiled_lhs = compile_expression(backend, project, lhs)?;
      let compiled_rhs = compile_expression(backend, project, rhs)?;

      let lhs_type_id = lhs.type_id(project);
      let rhs_type_id = rhs.type_id(project);
      if lhs_type_id != rhs_type_id {
        panic!("internal error: mismatched types in binary operation in codegen");
      }

      let ty = compile_type(backend, project, lhs_type_id);

      match op {
        BinaryOperator::Add => match ty {
          BasicTypeEnum::IntType(_) => {
            let value = backend
              .builder
              .build_int_add(
                compiled_lhs.into_int_value(),
                compiled_rhs.into_int_value(),
                "",
              )
              .expect("internal error: failed to build int add");
            Some(BasicValueEnum::IntValue(value))
          }
          BasicTypeEnum::FloatType(_) => {
            let value = backend
              .builder
              .build_float_add(
                compiled_lhs.into_float_value(),
                compiled_rhs.into_float_value(),
                "",
              )
              .expect("internal error: failed to build float add");
            Some(BasicValueEnum::FloatValue(value))
          }
          _ => panic!("internal error: invalid type in binary operation in codegen"),
        },
        BinaryOperator::Subtract => match ty {
          BasicTypeEnum::IntType(_) => {
            let value = backend
              .builder
              .build_int_sub(
                compiled_lhs.into_int_value(),
                compiled_rhs.into_int_value(),
                "",
              )
              .expect("internal error: failed to build int sub");
            Some(BasicValueEnum::IntValue(value))
          }
          BasicTypeEnum::FloatType(_) => {
            let value = backend
              .builder
              .build_float_sub(
                compiled_lhs.into_float_value(),
                compiled_rhs.into_float_value(),
                "",
              )
              .expect("internal error: failed to build float sub");
            Some(BasicValueEnum::FloatValue(value))
          }
          _ => panic!("internal error: invalid type in binary operation in codegen"),
        },
        BinaryOperator::Multiply => match ty {
          BasicTypeEnum::IntType(_) => {
            let value = backend
              .builder
              .build_int_mul(
                compiled_lhs.into_int_value(),
                compiled_rhs.into_int_value(),
                "",
              )
              .expect("internal error: failed to build int mul");
            Some(BasicValueEnum::IntValue(value))
          }
          BasicTypeEnum::FloatType(_) => {
            let value = backend
              .builder
              .build_float_mul(
                compiled_lhs.into_float_value(),
                compiled_rhs.into_float_value(),
                "",
              )
              .expect("internal error: failed to build float mul");
            Some(BasicValueEnum::FloatValue(value))
          }
          _ => panic!("internal error: invalid type in binary operation in codegen"),
        },
        BinaryOperator::Divide => match ty {
          BasicTypeEnum::IntType(_) => {
            let value = backend
              .builder
              .build_int_signed_div(
                compiled_lhs.into_int_value(),
                compiled_rhs.into_int_value(),
                "",
              )
              .expect("internal error: failed to build int div");
            Some(BasicValueEnum::IntValue(value))
          }
          BasicTypeEnum::FloatType(_) => {
            let value = backend
              .builder
              .build_float_div(
                compiled_lhs.into_float_value(),
                compiled_rhs.into_float_value(),
                "",
              )
              .expect("internal error: failed to build float div");
            Some(BasicValueEnum::FloatValue(value))
          }
          _ => panic!("internal error: invalid type in binary operation in codegen"),
        },
        BinaryOperator::Modulo => match ty {
          BasicTypeEnum::IntType(_) => {
            let value = backend
              .builder
              .build_int_signed_rem(
                compiled_lhs.into_int_value(),
                compiled_rhs.into_int_value(),
                "",
              )
              .expect("internal error: failed to build int rem");
            Some(BasicValueEnum::IntValue(value))
          }
          _ => panic!("internal error: invalid type in binary operation in codegen"),
        },
        BinaryOperator::Equals => match ty {
          BasicTypeEnum::IntType(_) => {
            let value = backend
              .builder
              .build_int_compare(
                IntPredicate::EQ,
                compiled_lhs.into_int_value(),
                compiled_rhs.into_int_value(),
                "",
              )
              .expect("internal error: failed to build int eq");
            Some(BasicValueEnum::IntValue(value))
          }
          BasicTypeEnum::FloatType(_) => {
            let value = backend
              .builder
              .build_float_compare(
                FloatPredicate::OEQ,
                compiled_lhs.into_float_value(),
                compiled_rhs.into_float_value(),
                "",
              )
              .expect("internal error: failed to build float eq");
            Some(BasicValueEnum::IntValue(value))
          }
          BasicTypeEnum::PointerType(_) => {
            let value = backend
              .builder
              .build_int_compare(
                IntPredicate::EQ,
                compiled_lhs.into_pointer_value(),
                compiled_rhs.into_pointer_value(),
                "",
              )
              .expect("internal error: failed to build ptr eq");
            Some(BasicValueEnum::IntValue(value))
          }
          _ => panic!("internal error: invalid type in binary operation in codegen"),
        },
        BinaryOperator::NotEquals => match ty {
          BasicTypeEnum::IntType(_) => {
            let value = backend
              .builder
              .build_int_compare(
                IntPredicate::NE,
                compiled_lhs.into_int_value(),
                compiled_rhs.into_int_value(),
                "",
              )
              .expect("internal error: failed to build int ne");
            Some(BasicValueEnum::IntValue(value))
          }
          BasicTypeEnum::FloatType(_) => {
            let value = backend
              .builder
              .build_float_compare(
                FloatPredicate::ONE,
                compiled_lhs.into_float_value(),
                compiled_rhs.into_float_value(),
                "",
              )
              .expect("internal error: failed to build float ne");
            Some(BasicValueEnum::IntValue(value))
          }
          BasicTypeEnum::PointerType(_) => {
            let value = backend
              .builder
              .build_int_compare(
                IntPredicate::NE,
                compiled_lhs.into_pointer_value(),
                compiled_rhs.into_pointer_value(),
                "",
              )
              .expect("internal error: failed to build ptr ne");
            Some(BasicValueEnum::IntValue(value))
          }
          _ => panic!("internal error: invalid type in binary operation in codegen"),
        },
        BinaryOperator::LessThan => match ty {
          BasicTypeEnum::IntType(_) => {
            let value = backend
              .builder
              .build_int_compare(
                IntPredicate::SLT,
                compiled_lhs.into_int_value(),
                compiled_rhs.into_int_value(),
                "",
              )
              .expect("internal error: failed to build int lt");
            Some(BasicValueEnum::IntValue(value))
          }
          BasicTypeEnum::FloatType(_) => {
            let value = backend
              .builder
              .build_float_compare(
                FloatPredicate::OLT,
                compiled_lhs.into_float_value(),
                compiled_rhs.into_float_value(),
                "",
              )
              .expect("internal error: failed to build float lt");
            Some(BasicValueEnum::IntValue(value))
          }
          _ => panic!("internal error: invalid type in binary operation in codegen"),
        },
        BinaryOperator::LessThanEquals => match ty {
          BasicTypeEnum::IntType(_) => {
            let value = backend
              .builder
              .build_int_compare(
                IntPredicate::SLE,
                compiled_lhs.into_int_value(),
                compiled_rhs.into_int_value(),
                "",
              )
              .expect("internal error: failed to build int le");
            Some(BasicValueEnum::IntValue(value))
          }
          BasicTypeEnum::FloatType(_) => {
            let value = backend
              .builder
              .build_float_compare(
                FloatPredicate::OLE,
                compiled_lhs.into_float_value(),
                compiled_rhs.into_float_value(),
                "",
              )
              .expect("internal error: failed to build float le");
            Some(BasicValueEnum::IntValue(value))
          }
          _ => panic!("internal error: invalid type in binary operation in codegen"),
        },
        BinaryOperator::GreaterThan => match ty {
          BasicTypeEnum::IntType(_) => {
            let value = backend
              .builder
              .build_int_compare(
                IntPredicate::SGT,
                compiled_lhs.into_int_value(),
                compiled_rhs.into_int_value(),
                "",
              )
              .expect("internal error: failed to build int gt");
            Some(BasicValueEnum::IntValue(value))
          }
          BasicTypeEnum::FloatType(_) => {
            let value = backend
              .builder
              .build_float_compare(
                FloatPredicate::OGT,
                compiled_lhs.into_float_value(),
                compiled_rhs.into_float_value(),
                "",
              )
              .expect("internal error: failed to build float gt");
            Some(BasicValueEnum::IntValue(value))
          }
          _ => panic!("internal error: invalid type in binary operation in codegen"),
        },
        BinaryOperator::GreaterThanEquals => match ty {
          BasicTypeEnum::IntType(_) => {
            let value = backend
              .builder
              .build_int_compare(
                IntPredicate::SGE,
                compiled_lhs.into_int_value(),
                compiled_rhs.into_int_value(),
                "",
              )
              .expect("internal error: failed to build int ge");
            Some(BasicValueEnum::IntValue(value))
          }
          BasicTypeEnum::FloatType(_) => {
            let value = backend
              .builder
              .build_float_compare(
                FloatPredicate::OGE,
                compiled_lhs.into_float_value(),
                compiled_rhs.into_float_value(),
                "",
              )
              .expect("internal error: failed to build float ge");
            Some(BasicValueEnum::IntValue(value))
          }
          _ => panic!("internal error: invalid type in binary operation in codegen"),
        },
        BinaryOperator::Assign => unreachable!(),
        BinaryOperator::AddAssign
        | BinaryOperator::SubtractAssign
        | BinaryOperator::MultiplyAssign
        | BinaryOperator::DivideAssign
        | BinaryOperator::ModuloAssign => unreachable!(),
        BinaryOperator::BitwiseAnd => match ty {
          BasicTypeEnum::IntType(_) => {
            let value = backend
              .builder
              .build_and(
                compiled_lhs.into_int_value(),
                compiled_rhs.into_int_value(),
                "",
              )
              .expect("internal error: failed to build and");
            Some(BasicValueEnum::IntValue(value))
          }
          _ => panic!("internal error: invalid type in binary operation in codegen"),
        },
        BinaryOperator::BitwiseOr => match ty {
          BasicTypeEnum::IntType(_) => {
            let value = backend
              .builder
              .build_or(
                compiled_lhs.into_int_value(),
                compiled_rhs.into_int_value(),
                "",
              )
              .expect("internal error: failed to build or");
            Some(BasicValueEnum::IntValue(value))
          }
          _ => panic!("internal error: invalid type in binary operation in codegen"),
        },
        BinaryOperator::BitwiseXor => match ty {
          BasicTypeEnum::IntType(_) => {
            let value = backend
              .builder
              .build_xor(
                compiled_lhs.into_int_value(),
                compiled_rhs.into_int_value(),
                "",
              )
              .expect("internal error: failed to build xor");
            Some(BasicValueEnum::IntValue(value))
          }
          _ => panic!("internal error: invalid type in binary operation in codegen"),
        },
        BinaryOperator::BitwiseLeftShift => match ty {
          BasicTypeEnum::IntType(_) => {
            let value = backend
              .builder
              .build_left_shift(
                compiled_lhs.into_int_value(),
                compiled_rhs.into_int_value(),
                "",
              )
              .expect("internal error: failed to build left shift");
            Some(BasicValueEnum::IntValue(value))
          }
          _ => panic!("internal error: invalid type in binary operation in codegen"),
        },
        BinaryOperator::BitwiseRightShift => match ty {
          BasicTypeEnum::IntType(_) => {
            let value = backend
              .builder
              .build_right_shift(
                compiled_lhs.into_int_value(),
                compiled_rhs.into_int_value(),
                true,
                "",
              )
              .expect("internal error: failed to build right shift");
            Some(BasicValueEnum::IntValue(value))
          }
          _ => panic!("internal error: invalid type in binary operation in codegen"),
        },
        BinaryOperator::LogicalAnd => {
          let value = backend
            .builder
            .build_and(
              compiled_lhs.into_int_value(),
              compiled_rhs.into_int_value(),
              "",
            )
            .expect("internal error: failed to build and");
          Some(BasicValueEnum::IntValue(value))
        }
        BinaryOperator::LogicalOr => {
          let value = backend
            .builder
            .build_or(
              compiled_lhs.into_int_value(),
              compiled_rhs.into_int_value(),
              "",
            )
            .expect("internal error: failed to build or");
          Some(BasicValueEnum::IntValue(value))
        }
      }
    }

    CheckedExpression::UnaryOp(expr, op, _, _) => {
      let compiled_expr = compile_expression(backend, project, expr)?;

      let type_id = expr.type_id(project);
      let compiled_type = compile_type(backend, project, type_id);

      match op {
        CheckedUnaryOperator::As(type_id) => {
          let ty = compile_type(backend, project, *type_id);

          match (compiled_type, ty) {
            (BasicTypeEnum::IntType(_), BasicTypeEnum::IntType(_)) => {
              let value = backend
                .builder
                .build_int_cast(compiled_expr.into_int_value(), ty.into_int_type(), "")
                .expect("internal error: failed to build int cast");
              Some(BasicValueEnum::IntValue(value))
            }

            (BasicTypeEnum::FloatType(_), BasicTypeEnum::FloatType(_)) => {
              Some(BasicValueEnum::FloatValue(compiled_expr.into_float_value()))
            }

            (BasicTypeEnum::PointerType(_), BasicTypeEnum::IntType(_)) => {
              let value = backend
                .builder
                .build_ptr_to_int(compiled_expr.into_pointer_value(), ty.into_int_type(), "")
                .expect("internal error: failed to build ptr to int");
              Some(BasicValueEnum::IntValue(value))
            }

            (BasicTypeEnum::IntType(_), BasicTypeEnum::PointerType(_)) => {
              let value = backend
                .builder
                .build_int_to_ptr(compiled_expr.into_int_value(), ty.into_pointer_type(), "")
                .expect("internal error: failed to build int to ptr");
              Some(BasicValueEnum::PointerValue(value))
            }

            (BasicTypeEnum::PointerType(_), BasicTypeEnum::PointerType(_)) => Some(
              BasicValueEnum::PointerValue(compiled_expr.into_pointer_value()),
            ),

            _ => panic!("internal error: invalid type in unary operation in codegen"),
          }
        }

        CheckedUnaryOperator::Dereference => {
          let expr_type = &project.types[type_id];

          let CheckedType::RawPtr(inner_type_id, _) = expr_type else {
            panic!("internal error: expected raw pointer type");
          };

          let inner_type = compile_type(backend, project, *inner_type_id);
          let value = backend
            .builder
            .build_load(inner_type, compiled_expr.into_pointer_value(), "")
            .expect("internal error: failed to build load");
          Some(value)
        }
        CheckedUnaryOperator::AddressOf => {
          let value = backend
            .builder
            .build_alloca(compiled_type, "")
            .expect("internal error: failed to build alloca");
          backend
            .builder
            .build_store(value, compiled_expr)
            .expect("internal error: failed to build store");
          Some(BasicValueEnum::PointerValue(value))
        }
        CheckedUnaryOperator::Negate => match compiled_type {
          BasicTypeEnum::IntType(_) => {
            let value = backend
              .builder
              .build_int_neg(compiled_expr.into_int_value(), "")
              .expect("internal error: failed to build int neg");
            Some(BasicValueEnum::IntValue(value))
          }
          BasicTypeEnum::FloatType(_) => {
            let value = backend
              .builder
              .build_float_neg(compiled_expr.into_float_value(), "")
              .expect("internal error: failed to build float neg");
            Some(BasicValueEnum::FloatValue(value))
          }
          _ => panic!("internal error: invalid type in unary operation in codegen"),
        },
        CheckedUnaryOperator::Not => match compiled_type {
          BasicTypeEnum::IntType(_) => {
            let value = backend
              .builder
              .build_not(compiled_expr.into_int_value(), "")
              .expect("internal error: failed to build not");
            Some(BasicValueEnum::IntValue(value))
          }
          _ => panic!("internal error: invalid type in unary operation in codegen"),
        },
        CheckedUnaryOperator::BitwiseNot => match compiled_type {
          BasicTypeEnum::IntType(_) => {
            let value = backend
              .builder
              .build_not(compiled_expr.into_int_value(), "")
              .expect("internal error: failed to build not");
            Some(BasicValueEnum::IntValue(value))
          }
          _ => panic!("internal error: invalid type in unary operation in codegen"),
        },
      }
    }

    CheckedExpression::IndexedExpression(expr, idx, type_id, _) => {
      let compiled_expr = compile_expression(backend, project, &*expr)?;
      let compiled_idx = compile_expression(backend, project, &*idx)?;
      let pointee_ty = compile_type(backend, project, *type_id);

      let value = unsafe {
        backend
          .builder
          .build_in_bounds_gep(
            pointee_ty,
            compiled_expr.into_pointer_value(),
            &[compiled_idx.into_int_value()],
            "arrayidx",
          )
          .expect("internal error: failed to build inbounds gep")
      };
      let value = backend
        .builder
        .build_load(pointee_ty, value, "")
        .expect("internal error: failed to build load");

      Some(value)
    }

    CheckedExpression::Call(call, _, _) => {
      let fns = backend.functions.clone();
      let fns = fns.borrow();
      let callee = fns.get(&call.name).unwrap();

      let mut args: Vec<BasicMetadataValueEnum<'ctx>> = vec![];
      for arg in call.args.iter() {
        let arg = compile_expression(backend, project, arg)?;
        args.push(arg.into());
      }

      let value = backend
        .builder
        .build_call(callee.clone(), args.as_slice(), "call")
        .expect("internal error: failed to build call");

      let value = value.try_as_basic_value();
      if value.is_right() {
        return None;
      }
      Some(value.unwrap_left())
    }

    CheckedExpression::MethodCall(expr, call, _, _) => {
      let fns = backend.functions.clone();
      let fns = fns.borrow();
      let callee = fns.get(&call.name).unwrap();

      let mut args: Vec<BasicMetadataValueEnum<'ctx>> =
        vec![compile_expression(backend, project, expr)?.into()];
      for arg in call.args.iter() {
        let arg = compile_expression(backend, project, arg)?;
        args.push(arg.into());
      }

      let value = backend
        .builder
        .build_call(callee.clone(), args.as_slice(), "call")
        .expect("internal error: failed to build call");

      let value = value.try_as_basic_value();
      if value.is_right() {
        return None;
      }
      Some(value.unwrap_left())
    }

    CheckedExpression::IndexedStruct(expr, name, type_decl_id, _, _) => {
      let compiled_expr = compile_expression(backend, project, expr)?;

      let type_decl = &project.type_decls[*type_decl_id];
      let field_names = backend
        .struct_fields
        .borrow()
        .get(type_decl.name.as_str())
        .unwrap()
        .clone();

      let mut idx = 0;
      while idx < field_names.len() {
        if &field_names[idx] == name {
          break;
        }
        idx += 1;
      }

      if idx >= field_names.len() {
        panic!("internal error: field not found in struct");
      }

      let value = backend
        .builder
        .build_extract_value(compiled_expr.into_struct_value(), idx as u32, "")
        .unwrap();
      Some(value)
    }

    CheckedExpression::IndexedEnum(enum_id, name, _, _, _) => {
      let enum_ = &project.enums[*enum_id];
      let members = backend
        .enum_members
        .borrow()
        .get(enum_.name.as_str())
        .unwrap()
        .clone();

      let mut idx = 0;
      while idx < members.len() {
        if &members[idx] == name {
          break;
        }
        idx += 1;
      }

      if idx >= members.len() {
        panic!("internal error: member not found in enum");
      }

      let variant = enum_.variants[idx].clone();
      match variant {
        CheckedEnumVariant::Untyped(_, _) => unreachable!("untyped enum variant"),
        CheckedEnumVariant::WithValue(_, ref value, _) => {
          compile_expression(backend, project, value)
        }
        CheckedEnumVariant::TupleLike(_, _, _) => todo!(),
        CheckedEnumVariant::StructLike(_, _, _) => todo!(),
      }
    }

    CheckedExpression::InlineAsm {
      volatile,
      asm,
      bindings,
      clobbers,
      ..
    } => {
      let mut param_types: Vec<BasicMetadataTypeEnum<'_>> = vec![];
      let mut args: Vec<BasicMetadataValueEnum<'_>> = vec![];
      let mut return_types = vec![];
      let mut constraints: String = "".into();

      for binding in bindings {
        let prefix = match binding.operand.operand_type {
          OperandType::Register => "r",
          OperandType::Memory => "",
          OperandType::Immediate => "N",
        };
        use CheckedInlineAsmOperandAction::*;
        match binding.operand.action {
          None => unreachable!(),
          In => {
            let var = compile_expression(backend, project, &binding.var.clone()).unwrap();
            let type_id = binding.var.type_id(project);
            let compiled_type = compile_type(backend, project, type_id);

            param_types.push(compiled_type.into());
            args.push(var.into());
            constraints.push_str(&format!("{}{{{}}},", prefix, binding.operand.register));
          }
          Out(type_id) => {
            if let CheckedExpression::Variable(_var, _) = &binding.var {
              let compiled_type = compile_type(backend, project, type_id);

              // let ptr_type = backend.context.ptr_type(AddressSpace::default());
              // let var = var.name.clone();
              // let ptr = backend.variable_ptrs.borrow()[&var].clone();
              // param_types.push(ptr_type.into());
              // args.push(ptr.into());

              return_types.push(compiled_type);
              constraints.push_str(&format!("={}{{{}}},", prefix, binding.operand.register));
            } else {
              panic!();
            }
          }
        }
      }

      for clobber in clobbers {
        use CheckedInlineAsmOperandAction::*;
        match clobber.action {
          None => constraints.push_str(&format!("~{{{}}},", clobber.register)),
          In => unreachable!(),
          Out(_) => unreachable!(),
        }
      }

      let return_type = if return_types.is_empty() {
        backend.context.void_type().as_type_ref()
      } else if return_types.len() == 1 {
        return_types[0].as_type_ref()
      } else {
        backend
          .context
          .struct_type(&return_types, false)
          .as_type_ref()
      };

      let asm_fn = unsafe {
        LLVMFunctionType(
          return_type,
          param_types
            .iter()
            .map(|p| p.as_type_ref())
            .collect::<Vec<_>>()
            .as_mut_ptr(),
          param_types.len() as u32,
          false as i32,
        )
      };

      let constraints = constraints.trim_end_matches(',').to_string();

      let asm_fn = unsafe { FunctionType::new(asm_fn) };
      let asm = backend.context.create_inline_asm(
        asm_fn,
        asm.join("\n"),
        constraints,
        *volatile,
        false,
        None,
        false,
      );
      let value = backend
        .builder
        .build_indirect_call(asm_fn, asm, args.as_slice(), "")
        .unwrap();

      if return_types.is_empty() {
        None
      } else if return_types.len() == 1 {
        let value = value.try_as_basic_value().left().unwrap();
        Some(value)
      } else {
        let value = value
          .try_as_basic_value()
          .left()
          .unwrap()
          .into_struct_value();
        let value = backend.builder.build_extract_value(value, 0, "").unwrap();
        Some(value)
      }
    }

    CheckedExpression::Range(start, end, type_id, span) => {
      let start = compile_expression(backend, project, start)?;
      let end = compile_expression(backend, project, end)?;
      let ty = compile_type(backend, project, *type_id);

      let size = end.into_int_value().const_sub(start.into_int_value());

      let ptr_value = backend
        .builder
        .build_array_alloca(ty, size, "array_ptr")
        .expect("internal error: failed to build array alloca");

      Some(BasicValueEnum::PointerValue(ptr_value))
    }

    _ => todo!("compile_expression not implemented for {:?}", expr),
  }
}

fn compile_type<'ctx>(
  backend: &mut LLVMBackend<'ctx>,
  project: &mut Project,
  type_id: TypeId,
) -> BasicTypeEnum<'ctx> {
  let ty = &project.types[type_id];
  match ty {
    CheckedType::Builtin(_) => match type_id {
      crate::compiler::UNKNOWN_TYPE_ID => {
        panic!("internal error: encountered UNKNOWN_TYPE_ID in codegen")
      }
      crate::compiler::VOID_TYPE_ID => panic!("cannot compile void type"),
      crate::compiler::I8_TYPE_ID => BasicTypeEnum::IntType(backend.context.i8_type()),
      crate::compiler::I16_TYPE_ID => BasicTypeEnum::IntType(backend.context.i16_type()),
      crate::compiler::I32_TYPE_ID => BasicTypeEnum::IntType(backend.context.i32_type()),
      crate::compiler::I64_TYPE_ID => BasicTypeEnum::IntType(backend.context.i64_type()),
      crate::compiler::I128_TYPE_ID => BasicTypeEnum::IntType(backend.context.i128_type()),
      crate::compiler::ISZ_TYPE_ID => BasicTypeEnum::IntType(backend.context.i64_type()),
      crate::compiler::U8_TYPE_ID => BasicTypeEnum::IntType(backend.context.i8_type()),
      crate::compiler::U16_TYPE_ID => BasicTypeEnum::IntType(backend.context.i16_type()),
      crate::compiler::U32_TYPE_ID => BasicTypeEnum::IntType(backend.context.i32_type()),
      crate::compiler::U64_TYPE_ID => BasicTypeEnum::IntType(backend.context.i64_type()),
      crate::compiler::U128_TYPE_ID => BasicTypeEnum::IntType(backend.context.i128_type()),
      crate::compiler::USZ_TYPE_ID => BasicTypeEnum::IntType(backend.context.i64_type()),
      crate::compiler::F32_TYPE_ID => BasicTypeEnum::FloatType(backend.context.f32_type()),
      crate::compiler::F64_TYPE_ID => BasicTypeEnum::FloatType(backend.context.f64_type()),
      crate::compiler::BOOL_TYPE_ID => BasicTypeEnum::IntType(backend.context.bool_type()),
      crate::compiler::CCHAR_TYPE_ID => BasicTypeEnum::IntType(backend.context.i8_type()),
      crate::compiler::RAWPTR_TYPE_ID => {
        BasicTypeEnum::PointerType(backend.context.ptr_type(AddressSpace::default()))
      }
      _ => unreachable!(),
    },

    CheckedType::TypeDecl(type_decl_id, _) => BasicTypeEnum::StructType(
      backend
        .struct_types
        .borrow()
        .get(project.type_decls[*type_decl_id].name.as_str())
        .unwrap()
        .clone(),
    ),

    CheckedType::Enum(enum_id, _) => {
      let enum_ = project.enums[*enum_id].clone();
      BasicTypeEnum::StructType(
        backend
          .enum_types
          .borrow()
          .get(&enum_.name)
          .unwrap()
          .clone(),
      )
    }

    CheckedType::RawPtr(type_id, _) => BasicTypeEnum::PointerType(unsafe {
      PointerType::new(LLVMPointerType(
        compile_type(backend, project, *type_id).as_type_ref(),
        0,
      ))
    }),

    CheckedType::Slice(type_id, size, _) => {
      let size = *size;
      let compiled_type = compile_type(backend, project, *type_id);
      BasicTypeEnum::ArrayType(unsafe {
        ArrayType::new(LLVMArrayType2(compiled_type.as_type_ref(), size as u64))
      })
    }

    _ => todo!(
      "compile_type not implemented for type {:?}",
      project.typename_for_type_id(type_id)
    ),
  }
}
