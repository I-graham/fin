#[macro_use]
mod nodes;
mod abbreviations;
mod compile_error;
mod lexer;
mod types;
mod vars;

use super::bytecode::*;
use super::interpreter::*;
use crate::runtime_error::*;
use compile_error::CompileError;
use fnv::FnvHashMap;
use lexer::*;
use nodes::node_structs::FunctionHeader;
use types::*;
use vars::FunctionScope;

struct CompileContext<'a> {
	tokens: Vec<Token<'a>>,
	scope: FunctionScope<'a>,
	types: TypeReader,
	program: FinProgram<'a>,
	error: CompileError<'a>,
	//Map function name to index of starting position in program_data
	//Ex. if "main" maps to 0, then program.program_data[0] == first
	//instruction of main function.
	function_map: FnvHashMap<&'a str, (usize, FunctionHeader<'a>)>,
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
			types: Default::default(),
			function_map: Default::default(),
			program: FinProgram {
				debug_info: Some(DebugInfo {
					source,
					line_ranges: Default::default(),
				}),
				..Default::default()
			},
		}
	}

	fn code(&mut self) -> &mut Vec<Instruction> {
		&mut self.program.code
	}

	//token most nearly associated with code emitted, used for creation of debug info
	fn emit(&mut self, code: &[Instruction], associated_token: Option<Token<'a>>) {
		self.program.code.extend(code);
		self.update_debug_info(associated_token);
	}

	fn declare_function(&mut self, header: FunctionHeader<'a>) {
		let name = header.func_name.text;
		if !self.function_map.contains_key(name) {
			let len = self.program.data.len();
			self.function_map.insert(name, (len, header));
			self.program.data.push(0);
		}
	}

	fn update_debug_info(&mut self, associated_token: Option<Token<'a>>) {
		if let Some(token) = associated_token {
			let line_no = token.line as Word;
			let code_len = self.program.code.len() as Word;
			self.program
				.debug_info
				.as_mut()
				.unwrap()
				.emitted_code(code_len, line_no);
		}
	}

	pub(crate) fn post_process(&mut self) {
		let get_jmp_dest = |inst: Instruction, ip: usize| {
			if inst.args[0] != 0 {
				inst.args_as_const().wrapping_neg()
			} else {
				inst.args_as_const()
			}
			.wrapping_add(1)
			.wrapping_add(ip as Word)
		};

		let elim_inst = |ip: usize, code: &mut Vec<Instruction>| {
			use Condition::*;
			use Mnemonic::*;
			let inst = code[ip];
			if inst.mnemonic == RelJmp {
				inst.args == [0; 6]
			} else {
				inst.condition == Nev || inst.mnemonic == Nop
			}
		};

		let mut modified_code = true;
		while modified_code {
			modified_code = false;
			let mut ip = 0;
			while ip < self.program.code.len() {
				if elim_inst(ip, self.code()) {
					modified_code = true;
					self.program.code.remove(ip);
					for early in 0..self.program.code.len() {
						let early_inst = self.program.code[early];
						let inst_moved = early >= ip;
						let old_early = early + inst_moved as usize;
						let old_dest = get_jmp_dest(early_inst, old_early);
						let dest_moved = old_dest > ip as Word;
						if early_inst.mnemonic == Mnemonic::RelJmp && inst_moved != dest_moved {
							let new_dest = if dest_moved { old_dest - 1 } else { old_dest };
							abbreviations::branch(early as Word, new_dest, self.code());
						}
					}

					for (func_entry_id, _) in self.function_map.values() {
						let data = self.program.data.get_mut(*func_entry_id).unwrap();
						if *data > ip as Word {
							*data -= 1;
						}
					}
				} else {
					ip += 1;
				}
			}
		}

		for ip in 0..self.program.code.len() {
			let inst = self.program.code[ip];
			if inst.mnemonic == Mnemonic::RelJmp {
				let mut jmp_dest = get_jmp_dest(inst, ip);
				let mut jmp_dest_inst = self.program.code[jmp_dest as usize];
				while jmp_dest_inst.mnemonic == Mnemonic::RelJmp
					&& jmp_dest != ip as Word
					&& inst.condition.implies(jmp_dest_inst.condition)
				{
					jmp_dest = get_jmp_dest(jmp_dest_inst, jmp_dest as usize);
					abbreviations::branch(ip as Word, jmp_dest, self.code());
					jmp_dest_inst = self.program.code[jmp_dest as usize];
				}
			}
		}
	}
}

pub(crate) fn compile_sharkfin(source: &str) -> FinProgram {
	use nodes::{ASTNode, ProgramRoot};

	let mut context = CompileContext::new(source);
	if context.tokens.is_empty() {
		context.error.err_at_substr(
			"Source file empty!",
			"Ensure that the correct input was given.",
			source,
			0,
		);
	}

	match ProgramRoot::construct(&mut context, 0) {
		Ok((_, mut ast)) => {
			ast.record_var_usage(&mut context);
			ast.generate_source(&mut context);
			context.post_process();
			context.program
		}
		Err((token, suggestions)) => {
			context.error.suggest_at_token(token, suggestions);
		}
	}
}
