pub mod ast;
pub mod error;
pub mod parser;
pub mod span;
pub mod tokenizer;

fn main() {
  env_logger::init();

  let mut args = std::env::args();
  args.next();
  let path = args.next().unwrap();
  let source = std::fs::read_to_string(path.clone()).unwrap();
  let mut tokenizer = tokenizer::Tokenizer::new(source.clone());
  match tokenizer.tokenize() {
    Ok(tokens) => {
      let mut parser = parser::Parser::new(tokens);
      let namespace = parser.parse();
      if let Ok(namespace) = namespace {
        println!("{:#?}", namespace);
      } else {
        let err = namespace.unwrap_err();
        error::display_error(&err, &source, &path);
        std::process::exit(1);
      }
    }
    Err(err) => {
      error::display_error(&err, &source, &path);
    }
  }
}
