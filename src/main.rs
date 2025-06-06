use std::{path::PathBuf, process::ExitCode};

use clap::{
  builder::{OsStr, PossibleValue},
  Parser, ValueEnum,
};
use colored::Colorize;
use compiler::Compiler;
use lyneate::Report;

pub mod ast;
pub mod backend;
pub mod checker;
pub mod compiler;
pub mod error;
pub mod ide;
pub mod ir;
pub mod parser;
pub mod span;
pub mod tokenizer;

#[derive(Parser, Debug, Clone)]
#[command(version, about, long_about = None)]
pub struct Opts {
  files: Vec<PathBuf>,

  #[arg(long, default_value_t = false)]
  nostdlib: bool,

  #[arg(short = 'O', long, value_enum, default_value = OptLevel::O2)]
  opt_level: OptLevel,

  #[arg(short = 'I', long)]
  include_paths: Vec<PathBuf>,

  #[arg(long)]
  target: Option<String>,

  #[arg(long, default_value_t = false)]
  print_symbols: bool,

  #[arg(long)]
  hover_position: Option<usize>,

  #[arg(long, default_value_t = false)]
  no_codegen: bool,

  #[arg(long, default_value_t = false)]
  json_errors: bool,

  #[arg(short = 'E', long, default_value_t = false)]
  emit_llvm_ir: bool,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum OptLevel {
  O0,
  O1,
  O2,
  O3,
}

impl ValueEnum for OptLevel {
  fn value_variants<'a>() -> &'a [Self] {
    &[OptLevel::O0, OptLevel::O1, OptLevel::O2, OptLevel::O3]
  }

  fn to_possible_value(&self) -> Option<clap::builder::PossibleValue> {
    Some(match self {
      OptLevel::O0 => PossibleValue::new("0").help("No optimization"),
      OptLevel::O1 => PossibleValue::new("1").help("Less optimization"),
      OptLevel::O2 => PossibleValue::new("2").help("Default optimization"),
      OptLevel::O3 => PossibleValue::new("3").help("Aggressive optimization"),
    })
  }
}

impl Into<OsStr> for OptLevel {
  fn into(self) -> OsStr {
    match self {
      OptLevel::O0 => "0".into(),
      OptLevel::O1 => "1".into(),
      OptLevel::O2 => "2".into(),
      OptLevel::O3 => "3".into(),
    }
  }
}

impl Into<inkwell::OptimizationLevel> for OptLevel {
  fn into(self) -> inkwell::OptimizationLevel {
    match self {
      OptLevel::O0 => inkwell::OptimizationLevel::None,
      OptLevel::O1 => inkwell::OptimizationLevel::Less,
      OptLevel::O2 => inkwell::OptimizationLevel::Default,
      OptLevel::O3 => inkwell::OptimizationLevel::Aggressive,
    }
  }
}

fn main() -> ExitCode {
  env_logger::init();
  let opts = Opts::parse();

  let mut compiler = Compiler::new(&opts);
  for file in &opts.files {
    let path = file.to_str().unwrap().to_string();
    if let Err(errors) = compiler.compile_file(path.clone()) {
      if opts.json_errors {
        for error in errors {
          println!("{}", serde_json::to_string(&error).unwrap());
        }
        return ExitCode::FAILURE;
      }
      if opts.print_symbols {
        println!("[]");
        continue;
      }
      let source = &compiler.files[compiler.files.len() - 1].1;
      for error in errors {
        error.display(&source);
      }

      return ExitCode::FAILURE;
    };
  }

  ExitCode::SUCCESS
}
