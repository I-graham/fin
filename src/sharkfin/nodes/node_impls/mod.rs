mod int_exprs;
mod log_exprs;

use super::node_structs::*;
use super::*;
use crate::abbreviations;
use crate::interpreter::REGISTERS;

fn tokens_match_kinds<'a>(
	inputs: &[Token<'a>],
	kinds: &'static [TokenKind],
) -> Result<(), (Token<'a>, &'static [TokenKind])> {
	if inputs.len() >= kinds.len() {
		for (i, (kind, token)) in kinds.iter().zip(inputs.iter()).enumerate() {
			if *kind != token.kind {
				return Err((*token, &kinds[i..=i]));
			}
		}
		Ok(())
	} else {
		Err((*inputs.last().unwrap(), kinds))
	}
}

impl<'a> ASTNode<'a> for ProgramRoot<'a> {
	fn construct(
		context: &mut CompileContext<'a>,
		start: usize,
	) -> Result<(usize, Self), (Token<'a>, &'static [TokenKind])> {
		let (prog_end, program) = RootNode::construct(context, start)?;
		let _ = tokens_match_kinds(&context.tokens[prog_end..], &[TokenKind::Eof])?;
		Ok((prog_end, Self(program)))
	}

	fn record_var_usage(&mut self, context: &mut CompileContext<'a>) -> Option<VariableID<'a>> {
		self.0.record_var_usage(context)
	}

	fn generate_source(&self, context: &mut CompileContext<'a>) {
		//pad value so programs which loop to the first instruction do not overflow
		//instruction pointer
		context.emit(&[Default::default()], None);
		context.update_debug_info(context.tokens.first().copied());
		self.0.generate_source(context);
		abbreviations::load_const(0, 0, None, context.code());
		context.emit(
			&[Instruction {
				mnemonic: Mnemonic::Hlt,
				..Default::default()
			}],
			None,
		);
		context.update_debug_info(context.tokens.last().copied());
	}
}

impl<'a> ASTNode<'a> for CodeBlock<'a> {
	fn construct(
		context: &mut CompileContext<'a>,
		start: usize,
	) -> Result<(usize, Self), (Token<'a>, &'static [TokenKind])> {
		use TokenKind::*;
		let _ = tokens_match_kinds(&context.tokens[start..], &[LCurly])?;
		let mut ret = Self {
			code: vec![],
			recorder: None,
		};
		let mut end = start + 1;
		while let Ok((node_end, next)) = Statement::construct(context, end) {
			end = node_end;
			ret.code.push(next);
		}
		let _ = tokens_match_kinds(&context.tokens[end..], &[RCurly])?;
		Ok((end + 1, ret))
	}
	fn record_var_usage(&mut self, context: &mut CompileContext<'a>) -> Option<VariableID<'a>> {
		context.scope.open_scope();
		self.code.iter_mut().for_each(|statement| {
			statement.record_var_usage(context);
		});
		self.recorder = Some(context.scope.close_scope());
		None
	}

	fn generate_source(&self, context: &mut CompileContext<'a>) {
		for statement in &self.code {
			statement.generate_source(context);
		}
	}
}

impl<'a> ASTNode<'a> for Statement<'a> {
	fn construct(
		context: &mut CompileContext<'a>,
		start: usize,
	) -> Result<(usize, Self), (Token<'a>, &'static [TokenKind])> {
		let is_later_string = |a: Token<'a>, b: Token<'a>| a.text.as_ptr() > b.text.as_ptr();
		let mut err: (Token<'a>, &'static [TokenKind]);
		match tokens_match_kinds(&context.tokens[start..], &[TokenKind::Semicolon]) {
			Ok(()) => return Ok((start + 1, Self::Empty)),
			Err(end) => err = end,
		};
		match LetVar::construct(context, start) {
			Ok((node_end, let_var)) => return Ok((node_end, Self::LetVar(let_var))),
			Err(end) => {
				if is_later_string(end.0, err.0) {
					err = end
				}
			}
		};

		match MutateVar::construct(context, start) {
			Ok((node_end, mut_var)) => return Ok((node_end, Self::MutateVar(mut_var))),
			Err(end) => {
				if is_later_string(end.0, err.0) {
					err = end
				}
			}
		};

		match IfStatement::construct(context, start) {
			Ok((node_end, if_statement)) => return Ok((node_end, Self::If(if_statement))),
			Err(end) => {
				if is_later_string(end.0, err.0) {
					err = end
				}
			}
		};

		match WhileLoop::construct(context, start) {
			Ok((node_end, while_loop)) => return Ok((node_end, Self::While(while_loop))),
			Err(end) => {
				if is_later_string(end.0, err.0) {
					err = end
				}
			}
		};

		match CodeBlock::construct(context, start) {
			Ok((node_end, code_block)) => return Ok((node_end, Self::CodeBlock(code_block))),
			Err(end) => {
				if is_later_string(end.0, err.0) {
					err = end
				}
			}
		};

		Err(err)
	}

	fn record_var_usage(&mut self, context: &mut CompileContext<'a>) -> Option<VariableID<'a>> {
		match self {
			Self::LetVar(let_var) => let_var.record_var_usage(context),
			Self::MutateVar(mut_var) => mut_var.record_var_usage(context),
			Self::CodeBlock(code_block) => code_block.record_var_usage(context),
			Self::If(if_statement) => if_statement.record_var_usage(context),
			Self::While(while_loop) => while_loop.record_var_usage(context),
			Self::Empty => None,
		}
	}

	fn generate_source(&self, context: &mut CompileContext<'a>) {
		match self {
			Self::LetVar(letvar) => letvar.generate_source(context),
			Self::MutateVar(mut_var) => mut_var.generate_source(context),
			Self::CodeBlock(code_block) => code_block.generate_source(context),
			Self::If(if_statement) => if_statement.generate_source(context),
			Self::While(while_loop) => while_loop.generate_source(context),
			Self::Empty => (),
		}
	}
}

impl<'a> ASTNode<'a> for WhileLoop<'a> {
	fn construct(
		context: &mut CompileContext<'a>,
		start: usize,
	) -> Result<(usize, Self), (Token<'a>, &'static [TokenKind])> {
		use TokenKind::*;
		static START_TOKENS: [TokenKind; 1] = [While];
		let _ = tokens_match_kinds(&context.tokens[start..], &START_TOKENS)?;
		let (end, cond) = LogExpr::construct(context, start + START_TOKENS.len())?;
		let (end, code) = CodeBlock::construct(context, end)?;
		Ok((
			end,
			Self {
				cond,
				code,
				vars: None,
			},
		))
	}
	fn record_var_usage(&mut self, context: &mut CompileContext<'a>) -> Option<VariableID<'a>> {
		context.scope.open_scope();
		self.cond.record_var_usage(context);
		self.code.record_var_usage(context);
		let vars = context.scope.close_scope();
		context.scope.record_usage(vars.keys());
		self.vars = Some(vars);
		None
	}

	fn generate_source(&self, context: &mut CompileContext<'a>) {
		match self.cond.precompute() {
			Some(0) => (),
			pre @ None | pre @ Some(1) => {
				let vars = self.vars.as_ref().unwrap();
				if vars.len() > REGISTERS as usize {
					let mut spill_count = vars.len() - REGISTERS as usize;
					context.scope.spill_all(
						|var| {
							let ret = spill_count > 0 && !vars.contains_key(&var);
							spill_count = spill_count.saturating_sub(1);
							ret
						},
						&mut context.program.code,
					);
				} else {
					context
						.scope
						.place_from_iter(vars.keys(), &mut context.program.code);
				}
				context.scope.save_state();
				let enter_inst = if pre.is_none() {
					let len = context.code().len() as Word;
					context.emit(
						&[Instruction {
							mnemonic: Mnemonic::RelJmp,
							..Default::default()
						}],
						None,
					);
					Some(len)
				} else {
					None
				};
				let start_inst = context.code().len() as Word;
				self.code.generate_source(context);
				context.scope.restore_state(&mut context.program.code);

				if let Some(inst_id) = enter_inst {
					let cmp_inst = context.code().len() as Word;
					abbreviations::branch(inst_id, cmp_inst, context.code());
					self.cond.generate_source(context);
				} else {
					context.emit(
						&[Instruction {
							mnemonic: Mnemonic::RelJmp,
							..Default::default()
						}],
						None,
					);
				}
				abbreviations::branch(
					context.code().len() as Word - pre.is_none() as Word - 1,
					start_inst,
					&mut context.program.code,
				);
			}
			_ => unreachable!(),
		};
	}
}

impl<'a> ASTNode<'a> for IfStatement<'a> {
	fn construct(
		context: &mut CompileContext<'a>,
		start: usize,
	) -> Result<(usize, Self), (Token<'a>, &'static [TokenKind])> {
		use TokenKind::*;
		static START_TOKENS: [TokenKind; 1] = [If];
		let _ = tokens_match_kinds(&context.tokens[start..], &START_TOKENS)?;
		let (end, cond) = LogExpr::construct(context, start + START_TOKENS.len())?;
		let (end, code) = CodeBlock::construct(context, end + 1)?;
		Ok((end, Self { cond, code }))
	}

	fn record_var_usage(&mut self, context: &mut CompileContext<'a>) -> Option<VariableID<'a>> {
		self.cond.record_var_usage(context);
		self.code.record_var_usage(context);
		None
	}

	fn generate_source(&self, context: &mut CompileContext<'a>) {
		match self.cond.precompute() {
			None => {
				self.cond.generate_source(context);
				context.scope.save_state();
				let jmp_inst_id = context.code().len() as Word - 1;
				self.code.generate_source(context);
				let out = &mut context.program.code;
				context.scope.restore_state(out);

				abbreviations::branch(jmp_inst_id, out.len() as Word, out);
			}
			Some(0) => (),
			Some(1) => self.code.generate_source(context),
			_ => unreachable!(),
		}
	}
}

impl<'a> ASTNode<'a> for LetVar<'a> {
	fn construct(
		context: &mut CompileContext<'a>,
		start: usize,
	) -> Result<(usize, Self), (Token<'a>, &'static [TokenKind])> {
		use TokenKind::*;
		static FORMAT: [TokenKind; 4] = [Let, Type, Ident, Assign];
		let _ = tokens_match_kinds(&context.tokens[start..], &FORMAT)?;
		let (end, mut_var) = MutateVar::construct(context, start + 2)?;
		Ok((end, Self { mut_var }))
	}

	fn record_var_usage(&mut self, context: &mut CompileContext<'a>) -> Option<VariableID<'a>> {
		let sum_var = self.mut_var.val.record_var_usage(context).unwrap();
		let access = &self.mut_var.access;
		let var_name = access.var.text;
		if context.scope.get_var(var_name).is_some() {
			context.error.err_at_token(
				"Variable name already in use!",
				"Use a different variable name.",
				access.var,
			);
		}
		let opvar = match sum_var {
			VariableID::Named(_) => {
				let [var] = context
					.scope
					.allocate_var(&[(Some(var_name), VarType::Int)]);
				var
			}
			VariableID::Unnamed(_) => context.scope.create_synonym(Some(var_name), sum_var),
		};
		self.mut_var.access.record_var_usage(context);
		context.scope.record_usage(&[opvar, sum_var]);
		None
	}

	fn generate_source(&self, context: &mut CompileContext<'a>) {
		self.mut_var.generate_source(context);
		let access = &self.mut_var.access;
		let var_id = VariableID::Named(access.var.text);
		println!(
			"Var `{}` stored in reg #{:?} at inst #{}.",
			access.var.text,
			&context
				.scope
				.place_vars(&[var_id], &mut context.program.code),
			context.program.code.len() - 1
		);
		context.update_debug_info(Some(access.var));
	}
}

impl<'a> ASTNode<'a> for MutateVar<'a> {
	fn construct(
		context: &mut CompileContext<'a>,
		start: usize,
	) -> Result<(usize, Self), (Token<'a>, &'static [TokenKind])> {
		use TokenKind::*;
		static FORMAT: [TokenKind; 2] = [Ident, Assign];
		let _ = tokens_match_kinds(&context.tokens[start..], &FORMAT)?;
		let (end, access) = AccessVar::construct(context, start)?;
		let (node_end, sum) = Sum::construct(context, end + 1)?;

		let _ = tokens_match_kinds(&context.tokens[node_end..], &[Semicolon])?;
		Ok((node_end + 1, Self { access, val: sum }))
	}

	fn record_var_usage(&mut self, context: &mut CompileContext<'a>) -> Option<VariableID<'a>> {
		let value = self.val.record_var_usage(context).unwrap();
		let access = self.access.record_var_usage(context).unwrap();
		context.scope.record_usage(&[value, access]);
		None
	}

	fn generate_source(&self, context: &mut CompileContext<'a>) {
		self.val.generate_source(context);
		let val_op = self.val.output_var().unwrap();
		self.access.generate_source(context);
		let var_op = self.access.output_var().unwrap();
		let [var, val] = context
			.scope
			.place_vars(&[var_op, val_op], &mut context.program.code);
		if val != var {
			context.emit(
				&[Instruction {
					condition: Condition::Al,
					mnemonic: Mnemonic::Mov,
					args: [val, var, 0, 0, 0, 0],
				}],
				Some(self.access.var),
			)
		}
	}
}

impl<'a> ASTNode<'a> for AccessVar<'a> {
	fn construct(
		context: &mut CompileContext<'a>,
		start: usize,
	) -> Result<(usize, Self), (Token<'a>, &'static [TokenKind])> {
		let _ = tokens_match_kinds(&context.tokens[start..], &[TokenKind::Ident])?;
		Ok((
			start + 1,
			Self {
				var: context.tokens[start],
			},
		))
	}

	fn record_var_usage(&mut self, context: &mut CompileContext<'a>) -> Option<VariableID<'a>> {
		if let Some(var) = context.scope.get_var(self.var.text) {
			context.scope.record_usage(&[var]);
			Some(var)
		} else {
			context.error.err_at_token(
				"Variable undefined or out of scope.",
				"Define this variable before use.",
				self.var,
			);
		}
	}

	fn output_var(&self) -> Option<VariableID<'a>> {
		Some(VariableID::Named(self.var.text))
	}

	fn generate_source(&self, _context: &mut CompileContext<'a>) {}
}

impl<'a> ASTNode<'a> for LiteralInt<'a> {
	fn construct(
		context: &mut CompileContext<'a>,
		start: usize,
	) -> Result<(usize, Self), (Token<'a>, &'static [TokenKind])> {
		use TokenKind::*;
		if context.tokens.len() <= start {
			return Err((*context.tokens.last().unwrap(), &[Integer]));
		}
		let minus_count = context.tokens[start..]
			.iter()
			.take_while(|token| token.kind == Minus)
			.count();
		let negate = minus_count % 2 == 1;
		let rest = &context.tokens[start + minus_count..];
		let _ = tokens_match_kinds(rest, &[Integer])?;
		let int_token = rest[0];
		let literal: Word = int_token.text.parse().unwrap();
		let value = if negate {
			literal.wrapping_neg()
		} else {
			literal
		};
		Ok((start + minus_count + 1, Self::from_const(value, context)))
	}

	fn precompute(&self) -> Option<Word> {
		Some(self.value)
	}

	fn record_var_usage(&mut self, context: &mut CompileContext<'a>) -> Option<VariableID<'a>> {
		let op = self.output.0;
		match self.output.1 {
			Some(op2) => context.scope.record_usage(&[op, op2]),
			None => context.scope.record_usage(&[op]),
		}
		Some(op)
	}

	fn output_var(&self) -> Option<VariableID<'a>> {
		Some(self.output.0)
	}

	fn generate_source(&self, context: &mut CompileContext<'a>) {
		if abbreviations::fits_in_const_inst(self.value) {
			let [start] = context
				.scope
				.place_vars(&[self.output.0], &mut context.program.code);
			abbreviations::load_const(self.value, start, None, &mut context.program.code);
		} else {
			let scratch_var = self.output.1.unwrap();
			let [start, scratch] = context
				.scope
				.place_vars(&[self.output.0, scratch_var], &mut context.program.code);
			abbreviations::load_const(self.value, start, Some(scratch), &mut context.program.code);
		}
		context.update_debug_info(None);
	}
}
