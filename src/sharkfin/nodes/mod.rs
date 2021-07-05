mod node_impls;
mod node_structs;
pub use node_structs::*;

use super::error::Error;
use super::lexer::*;
use super::vars::*;
use crate::bytecode::*;
use crate::interpreter::*;
use std::fmt::Debug;

type RootNode<'a> = CodeBlock<'a>;

pub(super) struct CompileContext<'a> {
	pub tokens: Vec<Token<'a>>,
	pub scope: FunctionScope<'a>,
	pub output: FinProgram,
	pub error: Error<'a>,
}

impl<'a> CompileContext<'a> {
	pub(super) fn new(source: &'a str) -> Self {
		let mut tokens = vec![];
		let mut lexer = Lexer::new(source);
		let error = Error::new(source);
		loop {
			use TokenKind::*;
			match lexer.advance_token() {
				Ok(token) if token.kind != Whitespace => {
					tokens.push(token);
					if token.kind == TokenKind::Eof {
						break;
					}
				}
				Err(msg) => error.err_at_char(&msg, lexer.cursor, lexer.line),
				_ => (),
			}
		}

		Self {
			tokens,
			error,
			scope: Default::default(),
			output: Default::default(),
		}
	}
}

pub(super) trait ASTNode<'a>: Debug + Sized {
	fn output_var(&self) -> Option<VariableID<'a>> {
		None
	}
	fn construct(
		context: &mut CompileContext<'a>,
		start: usize,
	) -> Result<(usize, Self), (Token<'a>, &'static [TokenKind])>;
	fn generate_source(&self, context: &mut CompileContext<'a>);
}
