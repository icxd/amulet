use std::path::PathBuf;

use inkwell::context::Context;

use crate::{
  checker::{typecheck_namespace, CheckedType, Project, Scope},
  codegen::{self, CodeGen},
  error::Result,
  parser::Parser,
  span::Span,
  tokenizer::Tokenizer,
  Opts,
};

pub(crate) const UNKNOWN_TYPE_ID: usize = 0;
pub(crate) const VOID_TYPE_ID: usize = 1;
pub(crate) const I8_TYPE_ID: usize = 2;
pub(crate) const I16_TYPE_ID: usize = 3;
pub(crate) const I32_TYPE_ID: usize = 4;
pub(crate) const I64_TYPE_ID: usize = 5;
pub(crate) const I128_TYPE_ID: usize = 6;
pub(crate) const ISZ_TYPE_ID: usize = 7;
pub(crate) const U8_TYPE_ID: usize = 8;
pub(crate) const U16_TYPE_ID: usize = 9;
pub(crate) const U32_TYPE_ID: usize = 10;
pub(crate) const U64_TYPE_ID: usize = 11;
pub(crate) const U128_TYPE_ID: usize = 12;
pub(crate) const USZ_TYPE_ID: usize = 13;
pub(crate) const F32_TYPE_ID: usize = 14;
pub(crate) const F64_TYPE_ID: usize = 15;
pub(crate) const STRING_TYPE_ID: usize = 16;
pub(crate) const BOOL_TYPE_ID: usize = 17;
pub(crate) const CCHAR_TYPE_ID: usize = 18;
pub(crate) const COUNT_TYPE_IDS: usize = 19;

pub type FileId = usize;

// Everything in the core library is GARANTEED to be usable in a
// freestanding environment, meaning nothing in the core library
// does any sort of heap allocation. Hence it will still be
// included if --nostdlib is passed, unlike standard library.
pub const CORE_LIB_PATH: &str = "runtime/core/";
pub const STD_LIB_PATH: &str = "runtime/std/";

#[derive(Debug)]
pub struct Compiler {
  pub(crate) opts: Opts,
  pub(crate) files: Vec<(String, String)>,
}

impl Compiler {
  pub fn new(opts: &Opts) -> Self {
    Self {
      opts: opts.clone(),
      files: vec![],
    }
  }

  pub fn include_runtime(&mut self, project: &mut Project) -> Result<()> {
    for _ in 0..COUNT_TYPE_IDS {
      project.types.push(CheckedType::Builtin(Span::default()));
    }

    let runtime = if self.opts.nostdlib {
      self.get_core_lib()
    } else {
      self
        .get_core_lib()
        .into_iter()
        .chain(self.get_std_lib())
        .collect()
    };
    for (name, contents) in runtime {
      self.files.push((name.clone(), contents.clone()));

      let mut tokenizer = Tokenizer::new(self.files.len() - 1, contents);
      let tokens = tokenizer.tokenize()?;

      let mut parser = Parser::new(self.files.len() - 1, tokens);
      let parsed_namespace = parser.parse()?;

      typecheck_namespace(&parsed_namespace, 0, project)?;

      let path = PathBuf::from(name.clone()).with_extension("ll");
      let context = Context::create();
      let mut gen = CodeGen::new(self.opts.clone(), &path.display().to_string(), &context);
      // codegen::compile(&mut gen, project)?;
      // gen.module.print_to_file(path).unwrap();
    }

    Ok(())
  }

  pub fn compile_file(&mut self, path: String) -> Result<()> {
    let project = &mut Project::new();

    self.include_runtime(project)?;

    let contents = std::fs::read_to_string(path.clone()).expect("failed to read file.");
    self.files.push((path.clone(), contents.clone()));

    let mut tokenizer = Tokenizer::new(self.files.len() - 1, contents);
    let tokens = tokenizer.tokenize()?;

    let mut parser = Parser::new(self.files.len() - 1, tokens);
    let parsed_namespace = parser.parse()?;

    let scope = Scope::new(Some(0));
    project.scopes.push(scope);

    let file_scope_id = project.scopes.len() - 1;

    typecheck_namespace(&parsed_namespace, file_scope_id, project)?;

    let path = PathBuf::from(path.clone()).with_extension("ll");
    let context = Context::create();
    let mut gen = CodeGen::new(self.opts.clone(), &path.display().to_string(), &context);
    // codegen::compile(&mut gen, project)?;
    // gen.module.print_to_file(path).unwrap();

    Ok(())
  }

  pub fn get_core_lib(&mut self) -> Vec<(String, String)> {
    let mut files = vec![];
    for entry in std::fs::read_dir(CORE_LIB_PATH).unwrap() {
      let path = entry.unwrap().path();
      if path.extension().is_some() && path.extension().unwrap() != "am" {
        continue;
      }
      let contents = std::fs::read_to_string(path.clone()).unwrap();
      files.push((path.display().to_string(), contents));
    }
    files
  }

  pub fn get_std_lib(&mut self) -> Vec<(String, String)> {
    let mut files = vec![];
    for entry in std::fs::read_dir(STD_LIB_PATH).unwrap() {
      let path = entry.unwrap().path();
      if path.extension().is_some() && path.extension().unwrap() != "am" {
        continue;
      }
      let contents = std::fs::read_to_string(path.clone()).unwrap();
      files.push((path.display().to_string(), contents));
    }
    files
  }
}
