mod lexer;
#[macro_use]
mod nodes;
use super::interpreter;
use super::bytecode;

pub(crate) fn compile_sharkfin<S: AsRef<str>>(source: S) -> interpreter::FinProgram {
	use nodes::ASTNode;
	let tokens = lexer::Lexer::new(source.as_ref())
		.filter(|t| t.0 != lexer::TokenKind::Whitespace)
		.collect::<Vec<_>>();
	let ast = nodes::ProgramRoot::construct(&tokens)
		.expect("Unable to compile code!")
		.1;
	let mut code = vec![];
	nodes::compile(&ast, &(0..16), &mut code);
	code.push(bytecode::Instruction {
		condition: bytecode::Condition::Al,
		mnemonic: bytecode::Mnemonic::Dbg,
		args: [0; 6]
	});

	let program = interpreter::FinProgram {
		program_data: vec![],
		code,
	};
	println!("{}", program.dissassemble());
	program.execute();
	program
}
