#[macro_use]
mod nodes;
mod abbreviations;
mod lexer;
mod vars;
use super::interpreter;

pub(crate) fn compile_sharkfin<S: AsRef<str>>(source: S) -> interpreter::FinProgram {
	use nodes::ASTNode;
	let tokens = lexer::Lexer::new(source.as_ref())
		.filter(|t| t.0 != lexer::TokenKind::Whitespace)
		.collect::<Vec<_>>();

	let mut scope = Default::default();
	let mut code = vec![];
	let ast = nodes::ProgramRoot::construct(&tokens, &mut scope)
		.expect("Unable to compile code!")
		.1;
	ast.generate_source(&mut scope, &mut code);

	let program = interpreter::FinProgram {
		program_data: vec![],
		code,
	};
	println!("\n{}", program.dissassemble());
	program.execute();
	program
}
