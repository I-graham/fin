#[macro_use]
mod nodes;
mod abbreviations;
mod error;
mod lexer;
mod vars;
use super::interpreter;

pub(crate) fn compile_sharkfin<S: AsRef<str>>(source: S) -> interpreter::FinProgram {
	use nodes::ASTNode;

	let src_str = source.as_ref();
	let mut context = nodes::CompileContext::new(src_str);
	match nodes::ProgramRoot::construct(&mut context, 0) {
		Ok((_, ast)) => {
			ast.generate_source(&mut context);

			let program = context.output;

			println!("\n{}", program.dissassemble());
			program.execute();
			program
		},
		Err((token, suggestions)) => {
			context.error.suggest_at_token(token, suggestions);
		},
	}
}
