#[macro_use]
mod nodes;
mod abbreviations;
mod compile_error;
mod lexer;
mod vars;

use super::bytecode::*;
use super::interpreter::*;
use crate::runtime_error::*;
use compile_error::CompileError;
use lexer::*;
use vars::FunctionScope;

struct CompileContext<'a> {
	tokens: Vec<Token<'a>>,
	scope: FunctionScope<'a>,
	output: FinProgram<'a>,
	error: CompileError<'a>,
	debug_info: DebugInfo<'a>,
}

impl<'a> CompileContext<'a> {
	pub(super) fn new(source: &'a str) -> Self {
		let mut tokens = vec![];
		let mut lexer = Lexer::new(source);
		let error = CompileError::new(source);
		loop {
			use TokenKind::*;
			match lexer.advance_token() {
				Ok(token) if token.kind != Whitespace => {
					tokens.push(token);
					if token.kind == TokenKind::Eof {
						break;
					}
				}
				Err(msg) => error.err_at_char(&msg, "", lexer.cursor, lexer.line),
				_ => (),
			}
		}

		Self {
			tokens,
			error,
			scope: Default::default(),
			output: Default::default(),
			debug_info: DebugInfo {
				source,
				line_ranges: Default::default(),
			},
		}
	}

	//token most nearly associated with code emitted, used for creation of debug info
	pub(super) fn emit(&mut self, code: &[Instruction], associated_token: Option<Token<'a>>) {
		self.output.code.extend(code);
		self.update_debug_info(associated_token);
	}

	pub(super) fn update_debug_info(&mut self, associated_token: Option<Token<'a>>) {
		let line_no = associated_token.map(|token| token.line).unwrap_or(0) as u64;
		let code_len = self.output.code.len() as u64;
		self.debug_info.emitted_code(code_len, line_no);
	}
}

pub(crate) fn compile_sharkfin(source: &str) -> FinProgram {
	use nodes::ASTNode;

	let mut context = CompileContext::new(source);
	match nodes::ProgramRoot::construct(&mut context, 0) {
		Ok((_, ast)) => {
			ast.record_var_usage(&mut context);
			ast.generate_source(&mut context);

			let CompileContext {
				output: mut program,
				debug_info,
				..
			} = context;

			program.debug_info = Some(debug_info);
			println!("\n{}", program.dissassemble());
			program.execute();
			program
		}
		Err((token, suggestions)) => {
			context.error.suggest_at_token(token, suggestions);
		}
	}
}
