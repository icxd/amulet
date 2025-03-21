use std::path::PathBuf;

use clap::{
  builder::{OsStr, PossibleValue},
  Parser, ValueEnum,
};
use compiler::Compiler;
use error::display_error;

pub mod ast;
pub mod checker;
pub mod codegen;
pub mod compiler;
pub mod error;
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

  #[arg(long)]
  target: Option<String>,
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

fn main() {
  env_logger::init();
  let opts = Opts::parse();

  let mut compiler = Compiler::new(&opts);
  for file in &opts.files {
    let path = file.to_str().unwrap().to_string();
    if let Err(error) = compiler.compile_file(path.clone()) {
      let source = &compiler.files[compiler.files.len() - 1].1;
      display_error(&error, source.as_str(), path.as_str());
    };
  }
}
