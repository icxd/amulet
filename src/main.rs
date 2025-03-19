use std::path::PathBuf;

use clap::Parser;
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
}

fn main() {
  let opts = Opts::parse();
  env_logger::init();

  let mut compiler = Compiler::new(&opts);
  for file in &opts.files {
    let path = file.to_str().unwrap().to_string();
    if let Err(error) = compiler.compile_file(path.clone()) {
      let source = &compiler.files[compiler.files.len() - 1].1;
      display_error(&error, source.as_str(), path.as_str());
    };
  }
}
