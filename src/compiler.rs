use crate::{
  checker::{typecheck_namespace, CheckedType, Project, Scope},
  codegen::codegen,
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
pub(crate) const COUNT_TYPE_IDS: usize = 18;

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

  pub fn include_prelude(&mut self, project: &mut Project) -> Result<()> {
    for _ in 0..COUNT_TYPE_IDS {
      project.types.push(CheckedType::Builtin(Span::default()));
    }

    let prelude_contents = Compiler::get_prelude();
    self
      .files
      .push(("<prelude>".into(), prelude_contents.clone()));

    let mut tokenizer = Tokenizer::new(self.files.len() - 1, prelude_contents);
    let tokens = tokenizer.tokenize()?;

    let mut parser = Parser::new(self.files.len() - 1, tokens);
    let parsed_namespace = parser.parse()?;

    typecheck_namespace(&parsed_namespace, 0, project)?;

    Ok(())
  }

  pub fn include_runtime(&mut self, project: &mut Project) -> Result<()> {
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

    let output = codegen(project, &project.scopes[file_scope_id]);
    std::fs::write(format!("{path}.cpp"), output).expect("guh");

    Ok(())
  }

  pub fn get_prelude() -> String {
    include_str!("../runtime/prelude.am").to_string()
  }

  pub fn get_core_lib(&mut self) -> Vec<(String, String)> {
    std::fs::read_dir(CORE_LIB_PATH)
      .unwrap()
      .map(|entry| {
        let path = entry.unwrap().path();
        let name = path.file_name().unwrap().to_str().unwrap().to_string();
        let contents = std::fs::read_to_string(path).unwrap();
        (name, contents)
      })
      .collect()
  }

  pub fn get_std_lib(&mut self) -> Vec<(String, String)> {
    std::fs::read_dir(STD_LIB_PATH)
      .unwrap()
      .map(|entry| {
        let path = entry.unwrap().path();
        let name = path.file_name().unwrap().to_str().unwrap().to_string();
        let contents = std::fs::read_to_string(path).unwrap();
        (name, contents)
      })
      .collect()
  }
}
