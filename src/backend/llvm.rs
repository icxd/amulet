use std::{cell::RefCell, collections::HashMap};

use inkwell::{
  builder::Builder,
  context::{AsContextRef, Context},
  llvm_sys::core::{
    LLVMConstNamedStruct, LLVMFunctionType, LLVMPointerType, LLVMStructCreateNamed,
    LLVMStructSetBody,
  },
  module::{Linkage, Module},
  targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple},
  types::{AsTypeRef, BasicTypeEnum, FunctionType, PointerType, StructType},
  values::{
    AsValueRef, BasicMetadataValueEnum, BasicValueEnum, FunctionValue, GlobalValue, IntValue,
    PointerValue, StructValue,
  },
  AddressSpace, FloatPredicate, IntPredicate,
};

use crate::{
  ast::{BinaryOperator, DefinitionLinkage, NumericConstant},
  checker::{
    CheckedBlock, CheckedExpression, CheckedFunction, CheckedStatement, CheckedType,
    CheckedTypeDecl, CheckedTypeKind, CheckedUnaryOperator, Project, TypeId,
  },
  compiler::VOID_TYPE_ID,
  error::Result,
  Opts,
};

#[derive(Debug)]
pub struct LLVMBackend<'ctx> {
  pub context: &'ctx Context,
  pub module: Module<'ctx>,
  pub builder: Builder<'ctx>,
  pub machine: TargetMachine,

  pub current_function: Option<FunctionValue<'ctx>>,
  pub variables: RefCell<HashMap<String, BasicTypeEnum<'ctx>>>,
  pub variable_ptrs: RefCell<HashMap<String, PointerValue<'ctx>>>,
  pub functions: RefCell<HashMap<String, FunctionValue<'ctx>>>,
  pub struct_types: RefCell<HashMap<String, StructType<'ctx>>>,
}

impl<'ctx> LLVMBackend<'ctx> {
  pub fn new(opts: Opts, path: &String, context: &'ctx Context) -> Self {
    let target_config = InitializationConfig::default();
    Target::initialize_native(&target_config).expect("Failed to initialize native machine target!");
    Target::initialize_all(&target_config);

    let module = context.create_module(path.as_str());
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

    Self {
      context,
      module,
      builder,
      machine,
      current_function: None,
      variables: RefCell::new(HashMap::new()),
      variable_ptrs: RefCell::new(HashMap::new()),
      functions: RefCell::new(HashMap::new()),
      struct_types: RefCell::new(HashMap::new()),
    }
  }
}

pub fn compile_namespace<'ctx>(
  backend: &mut LLVMBackend<'ctx>,
  project: &mut Project,
) -> Result<()> {
  for type_decl in &project.type_decls.clone() {
    compile_type_decl(backend, project, type_decl)?;
  }

  for function in &project.functions.clone() {
    compile_function(backend, project, function)?;
  }

  Ok(())
}

fn compile_type_decl<'ctx>(
  backend: &mut LLVMBackend<'ctx>,
  project: &mut Project,
  type_decl: &CheckedTypeDecl,
) -> Result<()> {
  let CheckedTypeKind::Class(class) = &type_decl.kind else {
    return Ok(());
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
    let struct_type = LLVMStructCreateNamed(
      backend.context.as_ctx_ref(),
      type_decl.name.clone().as_mut_ptr() as *mut _,
    );
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

  backend
    .struct_types
    .borrow_mut()
    .insert(type_decl.name.clone(), struct_type);

  Ok(())
}

fn compile_function<'ctx>(
  backend: &mut LLVMBackend<'ctx>,
  project: &mut Project,
  function: &CheckedFunction,
) -> Result<()> {
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

  match &function.linkage {
    DefinitionLinkage::Internal => {
      let fn_value = backend.module.add_function(&function.name, fn_type, None);
      backend.current_function = Some(fn_value);
      backend
        .functions
        .borrow_mut()
        .insert(function.name.clone(), fn_value);

      let entry_basic_block = backend.context.append_basic_block(fn_value, "entry");
      backend.builder.position_at_end(entry_basic_block);

      compile_block(backend, project, function.block.clone())?;

      if function.return_type_id == VOID_TYPE_ID {
        backend.builder.build_return(None).unwrap();
      }

      backend.current_function = None;
    }
    DefinitionLinkage::External => {
      let fn_value = backend
        .module
        .add_function(&function.name, fn_type, Some(Linkage::External));
      backend
        .functions
        .borrow_mut()
        .insert(function.name.clone(), fn_value);
    }
    DefinitionLinkage::ImplicitConstructor => {
      let fn_value = backend.module.add_function(&function.name, fn_type, None);
      backend
        .functions
        .borrow_mut()
        .insert(function.name.clone(), fn_value);

      let entry_basic_block = backend.context.append_basic_block(fn_value, "entry");
      backend.builder.position_at_end(entry_basic_block);

      let struct_type = backend
        .struct_types
        .borrow()
        .get(function.name.as_str())
        .unwrap()
        .clone();

      // let heap_struct = backend
      //   .builder
      //   .build_alloca(struct_type, "structure")
      //   .expect("internal error: failed to build alloca");

      let mut fields = vec![];
      for (idx, _) in function.params.iter().enumerate() {
        fields.push(
          fn_value
            .get_nth_param(idx as u32)
            .expect("internal error: failed to get param")
            .as_value_ref(),
        );
      }

      let value = BasicValueEnum::StructValue(unsafe {
        StructValue::new(LLVMConstNamedStruct(
          struct_type.as_type_ref(),
          fields.as_mut_ptr(),
          fields.len() as u32,
        ))
      });

      backend
        .builder
        .build_return(Some(&value))
        .expect("internal error: failed to build return");
    }
  }

  Ok(())
}

fn compile_block<'ctx>(
  backend: &mut LLVMBackend<'ctx>,
  project: &mut Project,
  block: CheckedBlock,
) -> Result<()> {
  for stmt in &block.stmts {
    compile_statement(backend, project, stmt)?;
  }

  Ok(())
}

fn compile_statement<'ctx>(
  backend: &mut LLVMBackend<'ctx>,
  project: &mut Project,
  stmt: &CheckedStatement,
) -> Result<()> {
  match stmt {
    CheckedStatement::VarDecl(var_decl, expr) => {
      let compiled_type = compile_type(backend, project, var_decl.type_id);
      let value = compile_expression(backend, project, expr)?;
      let alloca = backend
        .builder
        .build_alloca(compiled_type, var_decl.name.as_str())
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

      let entry_basic_block = backend.context.append_basic_block(fn_value, "entry");
      let loop_basic_block = backend.context.append_basic_block(fn_value, "loop");
      let after_basic_block = backend.context.append_basic_block(fn_value, "after");

      backend
        .builder
        .build_unconditional_branch(entry_basic_block)
        .expect("internal error: failed to build branch");

      backend.builder.position_at_end(entry_basic_block);
      let condition = compile_expression(backend, project, condition)?;

      backend
        .builder
        .build_conditional_branch(
          condition.into_int_value(),
          loop_basic_block,
          after_basic_block,
        )
        .expect("internal error: failed to build branch");

      backend.builder.position_at_end(loop_basic_block);

      compile_block(backend, project, block.clone())?;

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

      let entry_basic_block = backend.context.append_basic_block(fn_value, "loop");
      let after_basic_block = backend.context.append_basic_block(fn_value, "after_loop");

      backend
        .builder
        .build_unconditional_branch(entry_basic_block)
        .expect("internal error: failed to build branch");

      backend.builder.position_at_end(entry_basic_block);
      compile_block(backend, project, block.clone())?;

      backend
        .builder
        .build_unconditional_branch(entry_basic_block)
        .expect("internal error: failed to build branch");

      backend.builder.position_at_end(after_basic_block);
    }

    CheckedStatement::Return(expr) => {
      let value = compile_expression(backend, project, expr)?;
      backend
        .builder
        .build_return(Some(&value))
        .expect("internal error: failed to build return");
    }

    CheckedStatement::Expression(expr) => {
      let _ = compile_expression(backend, project, expr)?;
    }

    _ => todo!("compile_statement not implemented for {:?}", stmt),
  }

  Ok(())
}

fn compile_expression<'ctx>(
  backend: &mut LLVMBackend<'ctx>,
  project: &mut Project,
  expr: &CheckedExpression,
) -> Result<BasicValueEnum<'ctx>> {
  match expr {
    CheckedExpression::Null(span, _) => todo!("{:?}", span),

    CheckedExpression::Nullptr(_, type_id) => {
      let compiled_type = compile_type(backend, project, *type_id);
      let value = compiled_type.into_pointer_type().const_null();
      Ok(BasicValueEnum::PointerValue(value))
    }

    CheckedExpression::NumericConstant(constant, type_id, _) => {
      let compiled_type = compile_type(backend, project, *type_id);
      match constant {
        NumericConstant::I8(value) => {
          let value = i8::try_from(*value).unwrap();
          Ok(BasicValueEnum::IntValue(
            compiled_type.into_int_type().const_int(value as u64, false),
          ))
        }

        NumericConstant::I16(value) => {
          let value = i16::try_from(*value).unwrap();
          Ok(BasicValueEnum::IntValue(
            compiled_type.into_int_type().const_int(value as u64, false),
          ))
        }

        NumericConstant::I32(value) => {
          let value = i32::try_from(*value).unwrap();
          Ok(BasicValueEnum::IntValue(
            compiled_type.into_int_type().const_int(value as u64, false),
          ))
        }

        NumericConstant::I64(value) => {
          let value = i64::try_from(*value).unwrap();
          Ok(BasicValueEnum::IntValue(
            compiled_type.into_int_type().const_int(value as u64, false),
          ))
        }

        NumericConstant::I128(value) => {
          let value = i128::try_from(*value).unwrap();
          Ok(BasicValueEnum::IntValue(
            compiled_type.into_int_type().const_int(value as u64, false),
          ))
        }

        NumericConstant::Isz(value) => {
          let value = isize::try_from(*value).unwrap();
          Ok(BasicValueEnum::IntValue(
            compiled_type.into_int_type().const_int(value as u64, false),
          ))
        }

        NumericConstant::U8(value) => {
          let value = u8::try_from(*value).unwrap();
          Ok(BasicValueEnum::IntValue(
            compiled_type.into_int_type().const_int(value as u64, false),
          ))
        }

        NumericConstant::U16(value) => {
          let value = u16::try_from(*value).unwrap();
          Ok(BasicValueEnum::IntValue(
            compiled_type.into_int_type().const_int(value as u64, false),
          ))
        }

        NumericConstant::U32(value) => {
          let value = u32::try_from(*value).unwrap();
          Ok(BasicValueEnum::IntValue(
            compiled_type.into_int_type().const_int(value as u64, false),
          ))
        }

        NumericConstant::U64(value) => {
          let value = u64::try_from(*value).unwrap();
          Ok(BasicValueEnum::IntValue(
            compiled_type.into_int_type().const_int(value as u64, false),
          ))
        }

        NumericConstant::U128(value) => {
          let value = u128::try_from(*value).unwrap();
          Ok(BasicValueEnum::IntValue(
            compiled_type.into_int_type().const_int(value as u64, false),
          ))
        }

        NumericConstant::Usz(value) => {
          let value = usize::try_from(*value).unwrap();
          Ok(BasicValueEnum::IntValue(
            compiled_type.into_int_type().const_int(value as u64, false),
          ))
        }

        NumericConstant::F32(value) => {
          let value = f32::try_from(*value).unwrap();
          Ok(BasicValueEnum::FloatValue(
            compiled_type.into_float_type().const_float(value as f64),
          ))
        }

        NumericConstant::F64(value) => {
          let value = f64::try_from(*value).unwrap();
          Ok(BasicValueEnum::FloatValue(
            compiled_type.into_float_type().const_float(value),
          ))
        }
      }
    }

    CheckedExpression::CharacterLiteral(c, _) => {
      // TODO: this is probably wrong
      let value = i8::try_from(*c as i8).unwrap();
      Ok(BasicValueEnum::IntValue(
        backend.context.i8_type().const_int(value as u64, false),
      ))
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

      Ok(BasicValueEnum::PointerValue(ptr.into_pointer_value()))
    }

    CheckedExpression::Variable(var, _) => {
      let ty = backend.variables.borrow()[var.name.as_str()].clone();
      let ptr = backend.variable_ptrs.borrow()[var.name.as_str()].clone();
      let value = backend
        .builder
        .build_load(ty, ptr, "tmp")
        .expect("internal error: failed to build load");
      Ok(value)
    }

    CheckedExpression::BinaryOp(lhs, op, rhs, _, _) => {
      if matches!(op, BinaryOperator::Assign) {
        if let CheckedExpression::Variable(var, _) = *lhs.clone() {
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
          return Ok(rhs);
        } else {
          let lhs_type_id = lhs.type_id(project);
          let lhs_type = compile_type(backend, project, lhs_type_id);

          let lhs = compile_expression(backend, project, lhs)?;

          let lhs_ptr = backend
            .builder
            .build_alloca(lhs_type, "tmp")
            .expect("internal error: failed to build alloca");
          backend
            .builder
            .build_store(lhs_ptr, lhs)
            .expect("internal error: failed to build store");

          let rhs = compile_expression(backend, project, rhs)?;

          let ty = compile_type(backend, project, lhs_type_id);
          let value = unsafe {
            backend
              .builder
              .build_gep(ty, lhs_ptr, &[rhs.into_int_value()], "tmp")
              .expect("internal error: failed to build gep")
          };

          backend.builder.build_store(lhs_ptr, value).unwrap();

          return Ok(BasicValueEnum::PointerValue(value));
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
                "tmp",
              )
              .expect("internal error: failed to build int add");
            Ok(BasicValueEnum::IntValue(value))
          }
          BasicTypeEnum::FloatType(_) => {
            let value = backend
              .builder
              .build_float_add(
                compiled_lhs.into_float_value(),
                compiled_rhs.into_float_value(),
                "tmp",
              )
              .expect("internal error: failed to build float add");
            Ok(BasicValueEnum::FloatValue(value))
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
                "tmp",
              )
              .expect("internal error: failed to build int sub");
            Ok(BasicValueEnum::IntValue(value))
          }
          BasicTypeEnum::FloatType(_) => {
            let value = backend
              .builder
              .build_float_sub(
                compiled_lhs.into_float_value(),
                compiled_rhs.into_float_value(),
                "tmp",
              )
              .expect("internal error: failed to build float sub");
            Ok(BasicValueEnum::FloatValue(value))
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
                "tmp",
              )
              .expect("internal error: failed to build int mul");
            Ok(BasicValueEnum::IntValue(value))
          }
          BasicTypeEnum::FloatType(_) => {
            let value = backend
              .builder
              .build_float_mul(
                compiled_lhs.into_float_value(),
                compiled_rhs.into_float_value(),
                "tmp",
              )
              .expect("internal error: failed to build float mul");
            Ok(BasicValueEnum::FloatValue(value))
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
                "tmp",
              )
              .expect("internal error: failed to build int div");
            Ok(BasicValueEnum::IntValue(value))
          }
          BasicTypeEnum::FloatType(_) => {
            let value = backend
              .builder
              .build_float_div(
                compiled_lhs.into_float_value(),
                compiled_rhs.into_float_value(),
                "tmp",
              )
              .expect("internal error: failed to build float div");
            Ok(BasicValueEnum::FloatValue(value))
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
                "tmp",
              )
              .expect("internal error: failed to build int rem");
            Ok(BasicValueEnum::IntValue(value))
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
                "tmp",
              )
              .expect("internal error: failed to build int eq");
            Ok(BasicValueEnum::IntValue(value))
          }
          BasicTypeEnum::FloatType(_) => {
            let value = backend
              .builder
              .build_float_compare(
                FloatPredicate::OEQ,
                compiled_lhs.into_float_value(),
                compiled_rhs.into_float_value(),
                "tmp",
              )
              .expect("internal error: failed to build float eq");
            Ok(BasicValueEnum::IntValue(value))
          }
          BasicTypeEnum::PointerType(_) => {
            let value = backend
              .builder
              .build_int_compare(
                IntPredicate::EQ,
                compiled_lhs.into_pointer_value(),
                compiled_rhs.into_pointer_value(),
                "tmp",
              )
              .expect("internal error: failed to build ptr eq");
            Ok(BasicValueEnum::IntValue(value))
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
                "tmp",
              )
              .expect("internal error: failed to build int ne");
            Ok(BasicValueEnum::IntValue(value))
          }
          BasicTypeEnum::FloatType(_) => {
            let value = backend
              .builder
              .build_float_compare(
                FloatPredicate::ONE,
                compiled_lhs.into_float_value(),
                compiled_rhs.into_float_value(),
                "tmp",
              )
              .expect("internal error: failed to build float ne");
            Ok(BasicValueEnum::IntValue(value))
          }
          BasicTypeEnum::PointerType(_) => {
            let value = backend
              .builder
              .build_int_compare(
                IntPredicate::NE,
                compiled_lhs.into_pointer_value(),
                compiled_rhs.into_pointer_value(),
                "tmp",
              )
              .expect("internal error: failed to build ptr ne");
            Ok(BasicValueEnum::IntValue(value))
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
                "tmp",
              )
              .expect("internal error: failed to build int lt");
            Ok(BasicValueEnum::IntValue(value))
          }
          BasicTypeEnum::FloatType(_) => {
            let value = backend
              .builder
              .build_float_compare(
                FloatPredicate::OLT,
                compiled_lhs.into_float_value(),
                compiled_rhs.into_float_value(),
                "tmp",
              )
              .expect("internal error: failed to build float lt");
            Ok(BasicValueEnum::IntValue(value))
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
                "tmp",
              )
              .expect("internal error: failed to build int le");
            Ok(BasicValueEnum::IntValue(value))
          }
          BasicTypeEnum::FloatType(_) => {
            let value = backend
              .builder
              .build_float_compare(
                FloatPredicate::OLE,
                compiled_lhs.into_float_value(),
                compiled_rhs.into_float_value(),
                "tmp",
              )
              .expect("internal error: failed to build float le");
            Ok(BasicValueEnum::IntValue(value))
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
                "tmp",
              )
              .expect("internal error: failed to build int gt");
            Ok(BasicValueEnum::IntValue(value))
          }
          BasicTypeEnum::FloatType(_) => {
            let value = backend
              .builder
              .build_float_compare(
                FloatPredicate::OGT,
                compiled_lhs.into_float_value(),
                compiled_rhs.into_float_value(),
                "tmp",
              )
              .expect("internal error: failed to build float gt");
            Ok(BasicValueEnum::IntValue(value))
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
                "tmp",
              )
              .expect("internal error: failed to build int ge");
            Ok(BasicValueEnum::IntValue(value))
          }
          BasicTypeEnum::FloatType(_) => {
            let value = backend
              .builder
              .build_float_compare(
                FloatPredicate::OGE,
                compiled_lhs.into_float_value(),
                compiled_rhs.into_float_value(),
                "tmp",
              )
              .expect("internal error: failed to build float ge");
            Ok(BasicValueEnum::IntValue(value))
          }
          _ => panic!("internal error: invalid type in binary operation in codegen"),
        },

        BinaryOperator::Assign => unreachable!(),

        BinaryOperator::AddAssign
        | BinaryOperator::SubtractAssign
        | BinaryOperator::MultiplyAssign
        | BinaryOperator::DivideAssign
        | BinaryOperator::ModuloAssign => unreachable!(),
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
              Ok(BasicValueEnum::IntValue(compiled_expr.into_int_value()))
            }

            (BasicTypeEnum::FloatType(_), BasicTypeEnum::FloatType(_)) => {
              Ok(BasicValueEnum::FloatValue(compiled_expr.into_float_value()))
            }

            (BasicTypeEnum::PointerType(_), BasicTypeEnum::IntType(_)) => {
              let value = backend
                .builder
                .build_ptr_to_int(
                  compiled_expr.into_pointer_value(),
                  ty.into_int_type(),
                  "tmp",
                )
                .expect("internal error: failed to build ptr to int");
              Ok(BasicValueEnum::IntValue(value))
            }

            (BasicTypeEnum::IntType(_), BasicTypeEnum::PointerType(_)) => {
              let value = backend
                .builder
                .build_int_to_ptr(
                  compiled_expr.into_int_value(),
                  ty.into_pointer_type(),
                  "tmp",
                )
                .expect("internal error: failed to build int to ptr");
              Ok(BasicValueEnum::PointerValue(value))
            }

            (BasicTypeEnum::PointerType(_), BasicTypeEnum::PointerType(_)) => Ok(
              BasicValueEnum::PointerValue(compiled_expr.into_pointer_value()),
            ),

            _ => panic!("internal error: invalid type in unary operation in codegen"),
          }
        }

        CheckedUnaryOperator::Dereference => todo!("{:?}", op),
      }
    }

    CheckedExpression::IndexedExpression(expr, idx, type_id, _) => {
      let compiled_expr = compile_expression(backend, project, expr)?;
      let compiled_idx = compile_expression(backend, project, idx)?;

      let compiled_type = compile_type(backend, project, *type_id);

      let expr_type_id = expr.type_id(project);
      let compiled_expr_type = compile_type(backend, project, expr_type_id);
      match compiled_expr_type {
        BasicTypeEnum::PointerType(_) => {
          let value = unsafe {
            backend
              .builder
              .build_gep(
                compiled_expr_type,
                compiled_expr.into_pointer_value(),
                &[compiled_idx.into_int_value()],
                "tmp",
              )
              .expect("internal error: failed to build gep")
          };
          let value = backend
            .builder
            .build_load(compiled_type, value, "tmp")
            .expect("internal error: failed to build load");
          Ok(value)
        }

        _ => panic!("internal error: invalid type in indexed expression in codegen"),
      }
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
        .build_call(callee.clone(), args.as_slice(), "tmp")
        .expect("internal error: failed to build call");

      let value = value.try_as_basic_value();
      Ok(value.unwrap_left())
    }

    _ => todo!("compile_expression not implemented for {:?}", expr),
  }
}

fn compile_type<'ctx>(
  backend: &mut LLVMBackend<'ctx>,
  project: &mut Project,
  type_id: TypeId,
) -> BasicTypeEnum<'ctx> {
  match &project.types[type_id] {
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
      crate::compiler::STRING_TYPE_ID => BasicTypeEnum::StructType(backend.context.struct_type(
        &[
          BasicTypeEnum::PointerType(unsafe {
            PointerType::new(LLVMPointerType(backend.context.i8_type().as_type_ref(), 0))
          }),
          BasicTypeEnum::IntType(backend.context.i64_type()),
        ],
        false,
      )),
      crate::compiler::BOOL_TYPE_ID => BasicTypeEnum::IntType(backend.context.bool_type()),
      crate::compiler::CCHAR_TYPE_ID => BasicTypeEnum::IntType(backend.context.i8_type()),
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

    CheckedType::RawPtr(type_id, _) => BasicTypeEnum::PointerType(unsafe {
      PointerType::new(LLVMPointerType(
        compile_type(backend, project, *type_id).as_type_ref(),
        0,
      ))
    }),

    _ => todo!(
      "compile_type not implemented for type {:?}",
      project.typename_for_type_id(type_id)
    ),
  }
}
