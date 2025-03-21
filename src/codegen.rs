use std::collections::HashMap;

use clap::builder;
use inkwell::{
  basic_block::BasicBlock,
  builder::Builder,
  context::Context,
  execution_engine::ExecutionEngine,
  llvm_sys::{core::LLVMFunctionType, prelude::LLVMTypeRef},
  module::Module,
  targets::{CodeModel, RelocMode, Target, TargetMachine, TargetTriple},
  types::{AnyTypeEnum, AsTypeRef, BasicMetadataTypeEnum, BasicTypeEnum, FunctionType},
  values::{AnyValue, AnyValueEnum, AsValueRef, BasicValueEnum, FunctionValue, PointerValue},
  AddressSpace, OptimizationLevel,
};

use crate::{
  ast::{is_integer, BinaryOperator, NumericConstant},
  checker::{
    CheckedBlock, CheckedExpression, CheckedFunction, CheckedStatement, CheckedType,
    CheckedTypeDecl, CheckedTypeKind, CheckedUnaryOperator, Project, TypeId,
  },
  compiler::{I8_TYPE_ID, USZ_TYPE_ID},
  error::Result,
  Opts,
};

#[derive(Debug)]
pub struct CodeGen<'ctx> {
  pub(crate) context: &'ctx Context,
  pub(crate) module: Module<'ctx>,
  pub(crate) builder: Builder<'ctx>,
  pub(crate) machine: TargetMachine,

  variables: HashMap<String, PointerValue<'ctx>>,
}

impl<'ctx> CodeGen<'ctx> {
  pub fn new(opts: Opts, path: &String, context: &'ctx Context) -> Self {
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
      variables: HashMap::new(),
    }
  }
}
