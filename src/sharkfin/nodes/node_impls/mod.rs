mod log_exprs;
mod math_exprs;

use super::node_structs::*;
use super::*;
use crate::interpreter::REGISTERS;
use crate::sharkfin::abbreviations;

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

fn is_later_token<'a>(a: Token<'a>, b: Token<'a>) -> bool {
	a.text.as_ptr() > b.text.as_ptr()
}

impl<'a> ASTNode<'a> for ProgramRoot<'a> {
	fn construct(
		context: &mut CompileContext<'a>,
		start: usize,
	) -> Result<(usize, Self), (Token<'a>, &'static [TokenKind])> {
		let (prog_end, program) = RootNode::construct(context, start)?;
		let _ = tokens_match_kinds(&context.tokens[prog_end..], &[TokenKind::Eof])?;
		Ok((prog_end + 1, Self(program)))
	}

	fn record_var_usage(&mut self, context: &mut CompileContext<'a>) -> Option<VariableID<'a>> {
		self.0.record_var_usage(context)
	}

	fn generate_source(&mut self, context: &mut CompileContext<'a>) {
		context.emit(
			&[Instruction {
				mnemonic: Mnemonic::Call,
				..Default::default()
			}],
			None,
		);
		abbreviations::load_const(0, 0, None, context.code());
		context.emit(
			&[Instruction {
				mnemonic: Mnemonic::Hlt,
				..Default::default()
			}],
			None,
		);

		let (main_id, _) = context.function_map["main"];
		let [shift, a, b, c, d] = abbreviations::num_as_const_bytes(main_id as Word);
		context.code()[0].args = [0, shift, a, b, c, d];

		self.0.generate_source(context);
		context.update_debug_info(context.tokens.last().copied());
	}
}

impl<'a> ASTNode<'a> for GlobalScope<'a> {
	fn construct(
		context: &mut CompileContext<'a>,
		start: usize,
	) -> Result<(usize, Self), (Token<'a>, &'static [TokenKind])> {
		let (mut end, statement) = GlobalStatement::construct(context, start)?;
		let mut ret = Self {
			functions: vec![statement],
		};
		loop {
			if end < context.tokens.len() - 1 {
				let (new_end, statement) = GlobalStatement::construct(context, end)?;
				ret.functions.push(statement);
				end = new_end;
			} else {
				break Ok((end, ret));
			}
		}
	}

	fn record_var_usage(&mut self, context: &mut CompileContext<'a>) -> Option<VariableID<'a>> {
		for func in &mut self.functions {
			func.record_var_usage(context);
		}
		None
	}

	fn generate_source(&mut self, context: &mut CompileContext<'a>) {
		for func in &mut self.functions {
			func.generate_source(context);
		}
	}
}

impl<'a> ASTNode<'a> for GlobalStatement<'a> {
	fn construct(
		context: &mut CompileContext<'a>,
		start: usize,
	) -> Result<(usize, Self), (Token<'a>, &'static [TokenKind])> {
		let err: (Token<'a>, &'static [TokenKind]);

		match FunctionDefinition::construct(context, start) {
			Ok((node_end, definition)) => {
				return Ok((node_end, Self::Definition(Box::new(definition))))
			}
			Err(end) => err = end,
		};

		use TokenKind::*;
		match FunctionHeader::construct(context, start) {
			Ok((node_end, declaration)) if context.tokens[node_end].kind == Semicolon => {
				Ok((node_end + 1, Self::Declaration(declaration)))
			}

			Ok((node_end, _)) => {
				let next_token = context.tokens[node_end];
				if is_later_token(next_token, err.0) {
					Err((next_token, &[Semicolon]))
				} else {
					Err(err)
				}
			}

			Err(end) => {
				if is_later_token(end.0, err.0) {
					Err(end)
				} else {
					Err(err)
				}
			}
		}
	}

	fn record_var_usage(&mut self, context: &mut CompileContext<'a>) -> Option<VariableID<'a>> {
		match self {
			Self::Declaration(declaration) => declaration.record_var_usage(context),
			Self::Definition(definition) => definition.record_var_usage(context),
		}
	}

	fn generate_source(&mut self, context: &mut CompileContext<'a>) {
		match self {
			Self::Declaration(declaration) => declaration.generate_source(context),
			Self::Definition(definition) => definition.generate_source(context),
		}
	}
}

impl<'a> ASTNode<'a> for FunctionDefinition<'a> {
	fn construct(
		context: &mut CompileContext<'a>,
		start: usize,
	) -> Result<(usize, Self), (Token<'a>, &'static [TokenKind])> {
		let (header_end, header) = FunctionHeader::construct(context, start)?;

		let mut scope: FunctionScope<'a> = Default::default();
		scope.return_type = header.return_type;
		std::mem::swap(&mut context.scope, &mut scope);
		for &(ty, name) in &header.args {
			context.scope.allocate_vars(&[(Some(name.text), ty)]);
		}
		let body = CodeBlock::construct(context, header_end);
		std::mem::swap(&mut context.scope, &mut scope);
		let (body_end, body) = body?;

		Ok((
			body_end,
			Self {
				header,
				body,
				scope,
			},
		))
	}

	fn record_var_usage(&mut self, context: &mut CompileContext<'a>) -> Option<VariableID<'a>> {
		std::mem::swap(&mut context.scope, &mut self.scope);
		//chain args to self to ensure all lifetimes overlap.
		//this ensures correct variable loading.
		for &(_, var_name) in self.header.args.iter().chain(self.header.args.iter()) {
			let var_id = VariableID::Named(var_name.text);
			context.scope.record_usage(&[var_id]);
		}

		self.body.record_var_usage(context);
		std::mem::swap(&mut context.scope, &mut self.scope);
		None
	}

	fn generate_source(&mut self, context: &mut CompileContext<'a>) {
		std::mem::swap(&mut context.scope, &mut self.scope);
		let name = self.header.func_name.text;
		let (ptr_index, _) = context.function_map[name];
		let ptr = (context.code().len() as Word).wrapping_sub(1);
		context.program.data[ptr_index] = ptr;

		for (_ty, var_name) in self.header.args.iter() {
			let var_id = VariableID::Named(var_name.text);
			context
				.scope
				.place_vars(&[(var_id, false)], &mut context.program.code);
		}

		self.body.generate_source(context);
		context.emit(
			&[Instruction {
				mnemonic: Mnemonic::Ret,
				args: [self.header.args.len() as u8, 0, 0, 0, 0, 0],
				..Default::default()
			}],
			None,
		);
		std::mem::swap(&mut context.scope, &mut self.scope);
	}
}

impl<'a> ASTNode<'a> for FunctionHeader<'a> {
	fn construct(
		context: &mut CompileContext<'a>,
		start: usize,
	) -> Result<(usize, Self), (Token<'a>, &'static [TokenKind])> {
		use TokenKind::*;
		static START_FORMAT: &[TokenKind] = &[Func, Ident, LParen];
		let _ = tokens_match_kinds(&context.tokens[start..], START_FORMAT)?;
		let name = context.tokens[start + 1];
		let mut ret = Self {
			func_name: name,
			args: vec![],
			return_type: None,
		};

		static ARG_FORMAT: &[TokenKind] = &[Type, Ident];
		let mut end = start + START_FORMAT.len();
		while let Ok(()) = tokens_match_kinds(&context.tokens[end..], ARG_FORMAT) {
			let (new_end, ty) = TypeNode::construct(context, end)?;
			let arg = context.tokens[new_end];

			if ret
				.args
				.iter()
				.any(|(_, argument)| argument.text == arg.text)
			{
				context.error.err_at_token(
					"Parameter name used more than once.",
					"Use a different variable name.",
					arg,
				);
			}

			//TODO
			//TEMPORARY FIX
			if ret.args.len() > REGISTERS as usize {
				context.error.err_at_token(
					"Cannot use more arguments than there are registers",
					"Welp... Nothing you can do yet.",
					arg,
				);
			}

			ret.args.push((ty.ty, arg));
			end += ARG_FORMAT.len();
			if tokens_match_kinds(&context.tokens[end..], &[Comma]).is_err() {
				break;
			}
			end += 1;
		}

		let _ = tokens_match_kinds(&context.tokens[end..], &[RParen])?;

		end += 1;

		if context.tokens[end].kind == ReturnArrow {
			let (new_end, ret_ty) = TypeNode::construct(context, end + 1)?;
			ret.return_type = Some(ret_ty.ty);
			end = new_end;
		}

		context.declare_function(ret.clone());
		Ok((end, ret))
	}

	fn record_var_usage(&mut self, _context: &mut CompileContext<'a>) -> Option<VariableID<'a>> {
		None
	}

	fn generate_source(&mut self, _context: &mut CompileContext<'a>) {}
}

impl<'a> ASTNode<'a> for TypeNode {
	fn construct(
		context: &mut CompileContext<'a>,
		start: usize,
	) -> Result<(usize, Self), (Token<'a>, &'static [TokenKind])> {
		let type_token = context.tokens[start];

		use TokenKind::Type;
		if type_token.kind == Type {
			let ty = match context.types.named_type(type_token) {
				Some(ty) => ty,
				None => context.error.err_at_token(
					"Unknown type!",
					"Check that this type is spelled correctly.",
					type_token,
				),
			};
			Ok((start + 1, TypeNode { ty }))
		} else {
			Err((type_token, &[Type]))
		}
	}

	fn record_var_usage(&mut self, _: &mut CompileContext<'a>) -> Option<VariableID<'a>> {
		None
	}

	fn generate_source(&mut self, _: &mut CompileContext<'a>) {}
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
		loop {
			match Statement::construct(context, end) {
				Ok((node_end, next)) => {
					end = node_end;
					ret.code.push(next);
				}
				Err(_) if tokens_match_kinds(&context.tokens[end..], &[RCurly]).is_ok() => {
					break Ok((end + 1, ret));
				}
				Err(err) => break Err(err),
			}
		}
	}

	fn record_var_usage(&mut self, context: &mut CompileContext<'a>) -> Option<VariableID<'a>> {
		context.scope.open_scope();
		self.code.iter_mut().for_each(|statement| {
			statement.record_var_usage(context);
		});
		self.recorder = Some(context.scope.close_scope());
		None
	}

	fn generate_source(&mut self, context: &mut CompileContext<'a>) {
		for statement in &mut self.code {
			statement.generate_source(context);
		}
		let vars = self.recorder.as_ref().unwrap();
		context
			.scope
			.kill_vars(vars.iter().filter_map(|(v, b)| Some(v).filter(|_| *b)));
	}
}

impl<'a> ASTNode<'a> for Statement<'a> {
	fn construct(
		context: &mut CompileContext<'a>,
		start: usize,
	) -> Result<(usize, Self), (Token<'a>, &'static [TokenKind])> {
		use TokenKind::*;

		let mut err: (Token<'a>, &'static [TokenKind]);

		match tokens_match_kinds(&context.tokens[start..], &[Semicolon]) {
			Ok(()) => return Ok((start + 1, Self::Empty)),
			Err(end) => err = end,
		};

		let mut select_err = |end: (Token<'a>, &'static [TokenKind])| {
			if is_later_token(end.0, err.0) {
				err = end;
			}
		};

		match LetVar::construct(context, start) {
			Ok((node_end, let_var)) => return Ok((node_end, Self::LetVar(let_var))),
			Err(end) => select_err(end),
		};

		match MutateVar::construct(context, start) {
			Ok((node_end, mut_var)) => return Ok((node_end, Self::MutateVar(mut_var))),
			Err(end) => select_err(end),
		};

		match IfStatement::construct(context, start) {
			Ok((node_end, if_statement)) => return Ok((node_end, Self::If(if_statement))),
			Err(end) => select_err(end),
		};

		match WhileLoop::construct(context, start) {
			Ok((node_end, while_loop)) => return Ok((node_end, Self::While(while_loop))),
			Err(end) => select_err(end),
		};

		match CodeBlock::construct(context, start) {
			Ok((node_end, code_block)) => return Ok((node_end, Self::CodeBlock(code_block))),
			Err(end) => select_err(end),
		};

		match FunctionCall::construct(context, start) {
			Ok((node_end, code_block)) => return Ok((node_end, Self::FunctionCall(code_block))),
			Err(end) => select_err(end),
		};

		if context.tokens[start].kind == Return {
			match MathExpr::construct(context, start + 1) {
				Ok((end, ret_val)) => {
					let end_token = context.tokens[end];
					if end_token.kind == Semicolon {
						if ret_val.output_type(context) != context.scope.return_type {
							context.error.err_at_token(
								"Type mismatch.",
								"Expression type does not match return type of function.",
								context.tokens[start],
							);
						}
						return Ok((end + 1, Self::Return(ret_val)));
					} else {
						select_err((end_token, &[Semicolon]))
					}
				}
				Err(end) => select_err(end),
			};
		}

		Err(err)
	}

	fn record_var_usage(&mut self, context: &mut CompileContext<'a>) -> Option<VariableID<'a>> {
		match self {
			Self::Empty => None,
			Self::LetVar(let_var) => let_var.record_var_usage(context),
			Self::MutateVar(mut_var) => mut_var.record_var_usage(context),
			Self::CodeBlock(code_block) => code_block.record_var_usage(context),
			Self::If(if_statement) => if_statement.record_var_usage(context),
			Self::While(while_loop) => while_loop.record_var_usage(context),
			Self::FunctionCall(function_call) => function_call.record_var_usage(context),
			Self::Return(ret_val) => ret_val.record_var_usage(context),
		}
	}

	fn generate_source(&mut self, context: &mut CompileContext<'a>) {
		match self {
			Self::Empty => (),
			Self::LetVar(letvar) => letvar.generate_source(context),
			Self::MutateVar(mut_var) => mut_var.generate_source(context),
			Self::CodeBlock(code_block) => code_block.generate_source(context),
			Self::If(if_statement) => if_statement.generate_source(context),
			Self::While(while_loop) => while_loop.generate_source(context),
			Self::FunctionCall(function_call) => function_call.generate_source(context),
			Self::Return(ret_val) => {
				ret_val.generate_source(context);
				let opvar = ret_val.output_var().unwrap();
				context
					.scope
					.place_args(&[(opvar, false)], &mut context.program.code);
			}
		}
	}
}

impl<'a> ASTNode<'a> for FunctionCall<'a> {
	fn construct(
		context: &mut CompileContext<'a>,
		start: usize,
	) -> Result<(usize, Self), (Token<'a>, &'static [TokenKind])> {
		use TokenKind::*;
		let () = tokens_match_kinds(&context.tokens[start..], &[Ident, LParen])?;
		let func_name = context.tokens[start];
		let return_ty = context.function_map[func_name.text].1.return_type;
		let opvar = return_ty.map(|ty| context.scope.allocate_vars(&[(None, ty)])[0]);
		let mut ret = Self {
			args: vec![],
			func_name,
			opvar,
		};

		let mut end = start + 2;
		loop {
			let arg_result = MathExpr::construct(context, end);
			match arg_result {
				Ok((new_end, arg)) => {
					end = new_end;
					ret.args.push(arg);
				}
				Err(err) if is_later_token(err.0, context.tokens[end]) => break Err(err),
				Err((next, _)) => match next.kind {
					RParen => {
						let expected = &context
							.function_map
							.get(ret.func_name.text)
							.unwrap_or_else(|| {
								context.error.err_at_token(
									"Use of undeclared function!",
									"Make sure this function is declared before use.",
									ret.func_name,
								)
							})
							.1
							.args;

						let arg_num = ret.args.len();
						if arg_num != expected.len() {
							context.error.err_at_token(
								"Incorrect number of arguments!",
								format!(
									"Found {} arguments, expected {}.",
									arg_num,
									expected.len()
								)
								.as_str(),
								ret.func_name,
							)
						}

						if !ret
							.args
							.iter()
							.map(|expr| expr.output_type(context).unwrap())
							.zip(expected.iter().copied())
							.all(|(arg_ty, expected_ty)| arg_ty == expected_ty.0)
						{
							context.error.err_at_token(
								"Incorrect argument types!",
								"Convert arguments to the expected types.",
								ret.func_name,
							)
						}

						break Ok((end + 1, ret));
					}
					Comma => end += 1,
					_ => break Err((next, &[RParen, Comma])),
				},
			}
		}
	}

	fn output_var(&self) -> Option<VariableID<'a>> {
		self.opvar
	}

	fn output_type(&self, context: &CompileContext<'a>) -> Option<VarType> {
		self.opvar.and_then(|var| context.scope.var_type(var))
	}

	fn record_var_usage(&mut self, context: &mut CompileContext<'a>) -> Option<VariableID<'a>> {
		for arg in &mut self.args {
			arg.record_var_usage(context);
		}

		for arg in &mut self.args {
			let opvar = arg.output_var().unwrap();
			context.scope.record_usage(&[opvar]);
		}

		self.opvar
	}

	fn generate_source(&mut self, context: &mut CompileContext<'a>) {
		let call_id = context.function_map.get(self.func_name.text).unwrap().0;

		for arg in &mut self.args {
			arg.generate_source(context);
		}

		let ids: Vec<_> = self
			.args
			.iter()
			.map(|expr| (expr.output_var().unwrap(), true))
			.collect();

		context.scope.place_args(&ids, &mut context.program.code);

		let [shift, a, b, c, d] = abbreviations::num_as_const_bytes(call_id as Word);
		context.emit(
			&[Instruction {
				mnemonic: Mnemonic::Call,
				args: [self.args.len() as u8, shift, a, b, c, d],
				..Default::default()
			}],
			Some(self.func_name),
		);

		if let Some(opvar) = self.opvar {
			context
				.scope
				.place_args(&[(opvar, false)], &mut context.program.code);
		}
	}
}

impl<'a> ASTNode<'a> for WhileLoop<'a> {
	fn construct(
		context: &mut CompileContext<'a>,
		start: usize,
	) -> Result<(usize, Self), (Token<'a>, &'static [TokenKind])> {
		use TokenKind::*;
		static START_TOKENS: &[TokenKind] = &[While];
		let _ = tokens_match_kinds(&context.tokens[start..], START_TOKENS)?;
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

	fn generate_source(&mut self, context: &mut CompileContext<'a>) {
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
				context.scope.kill_vars(
					vars.iter()
						.filter_map(|(v, b)| if *b { Some(v) } else { None }),
				);
				self.code.generate_source(context);
				context.scope.kill_vars(
					vars.iter()
						.filter_map(|(v, b)| if *b { Some(v) } else { None }),
				);
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
		static START_TOKEN: &[TokenKind] = &[If];
		let _ = tokens_match_kinds(&context.tokens[start..], START_TOKEN)?;
		let (cond_end, cond) = LogExpr::construct(context, start + START_TOKEN.len())?;
		let (mut end, code) = CodeBlock::construct(context, cond_end)?;
		let mut ret = Self {
			cond_code: vec![(cond, code)],
			else_code: None,
		};
		static ELSEIF_TOKENS: &[TokenKind] = &[Else, If];
		while let Ok(()) = tokens_match_kinds(&context.tokens[end..], ELSEIF_TOKENS) {
			let (cond_end, cond) = LogExpr::construct(context, end + ELSEIF_TOKENS.len())?;
			let (code_end, code) = CodeBlock::construct(context, cond_end)?;
			end = code_end;
			ret.cond_code.push((cond, code));
		}
		static ELSE_TOKEN: &[TokenKind] = &[Else];
		if let Ok(()) = tokens_match_kinds(&context.tokens[end..], ELSE_TOKEN) {
			let (code_end, code) = CodeBlock::construct(context, end + ELSE_TOKEN.len())?;
			end = code_end;
			ret.else_code = Some(code);
		}

		Ok((end, ret))
	}

	fn record_var_usage(&mut self, context: &mut CompileContext<'a>) -> Option<VariableID<'a>> {
		for (cond, code) in &mut self.cond_code {
			cond.record_var_usage(context);
			code.record_var_usage(context);
		}
		if let Some(else_statement) = self.else_code.as_mut() {
			else_statement.record_var_usage(context);
		}
		None
	}

	fn generate_source(&mut self, context: &mut CompileContext<'a>) {
		let create_exit = |context: &mut CompileContext, exits: &mut Vec<Word>| {
			exits.push(context.code().len() as Word);
			context.emit(
				&[Instruction {
					mnemonic: Mnemonic::RelJmp,
					..Default::default()
				}],
				None,
			);
		};
		let mut exits = vec![];
		let mut const_cond_found = false;
		for (cond, code) in &mut self.cond_code {
			if !const_cond_found {
				match cond.precompute() {
					None => {
						cond.generate_source(context);
						let jmp_to_next_inst = context.code().len() as Word - 1;
						context.scope.save_state();
						code.generate_source(context);
						create_exit(context, &mut exits);
						context.scope.restore_state(&mut context.program.code);
						abbreviations::branch(
							jmp_to_next_inst,
							context.code().len() as Word,
							context.code(),
						)
					}
					Some(0) => (),
					Some(1) => {
						code.generate_source(context);
						create_exit(context, &mut exits);
						const_cond_found = true;
						break;
					}
					_ => unreachable!(),
				}
			}
		}
		if let Some(else_statement) = &mut self.else_code {
			if !const_cond_found {
				else_statement.generate_source(context);
			}
		}
		for exit in exits {
			abbreviations::branch(exit, context.code().len() as Word, context.code());
		}
	}
}

impl<'a> ASTNode<'a> for LetVar<'a> {
	fn construct(
		context: &mut CompileContext<'a>,
		start: usize,
	) -> Result<(usize, Self), (Token<'a>, &'static [TokenKind])> {
		use TokenKind::*;
		static FORMAT: &[TokenKind] = &[Let, Type, Ident, Assign];
		let _ = tokens_match_kinds(&context.tokens[start..], FORMAT)?;
		let (end, mut_var) = MutateVar::construct(context, start + 2)?;

		let sum_var = mut_var.val.output_var().unwrap();
		let access = &mut_var.access;
		let var_name = access.var.text;

		if context.scope.var_in_scope(VariableID::Named(var_name)) {
			context.error.err_at_token(
				"Variable name already in use!",
				"Use a different variable name.",
				access.var,
			);
		}

		let (_, ty) = TypeNode::construct(context, start + 1)?;

		let var = context.scope.create_synonym(Some(var_name), sum_var);

		if context.scope.var_type(var).unwrap() != ty.ty {
			context.error.err_at_token(
				"Incorrect type!",
				"Explicit type is not equal to the type of the assigned expression.",
				context.tokens[start + 1],
			);
		}

		Ok((end, Self { mut_var }))
	}

	fn record_var_usage(&mut self, context: &mut CompileContext<'a>) -> Option<VariableID<'a>> {
		let sum_var = self.mut_var.val.record_var_usage(context).unwrap();
		let opvar = self.mut_var.access.record_var_usage(context).unwrap();
		context.scope.record_usage(&[opvar, sum_var]);
		None
	}

	fn generate_source(&mut self, context: &mut CompileContext<'a>) {
		self.mut_var.generate_source(context);
		let access = &self.mut_var.access;
		let text = access.var.text;
		let var_id = VariableID::Named(text);
		let [reg] = context
			.scope
			.place_vars(&[(var_id, false)], &mut context.program.code);
		let pos = context.code().len() - 1;
		println!("Var `{}` stored in reg #{:?} at inst #{}.", text, reg, pos);
		context.update_debug_info(Some(access.var));
	}
}

impl<'a> ASTNode<'a> for MutateVar<'a> {
	fn construct(
		context: &mut CompileContext<'a>,
		start: usize,
	) -> Result<(usize, Self), (Token<'a>, &'static [TokenKind])> {
		use TokenKind::*;
		static FORMAT: &[TokenKind] = &[Ident, Assign];
		let _ = tokens_match_kinds(&context.tokens[start..], FORMAT)?;
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

	fn generate_source(&mut self, context: &mut CompileContext<'a>) {
		self.val.generate_source(context);
		let val_op = self.val.output_var().unwrap();
		self.access.generate_source(context);
		let var_op = self.access.output_var().unwrap();
		let [val, var] = context.scope.place_vars(
			&[(val_op, false), (var_op, false)],
			&mut context.program.code,
		);
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
		let id = VariableID::Named(self.var.text);
		if context.scope.var_in_scope(id) {
			context.scope.record_usage(&[id]);
			Some(id)
		} else {
			context.error.err_at_token(
				"Variable undefined or out of scope.",
				"Define this variable before use.",
				self.var,
			)
		}
	}

	fn output_var(&self) -> Option<VariableID<'a>> {
		Some(VariableID::Named(self.var.text))
	}

	fn output_type(&self, context: &CompileContext) -> Option<VarType> {
		let id = VariableID::Named(self.var.text);
		if context.scope.var_in_scope(id) {
			self.output_var()
				.map(|id| context.scope.var_type(id).unwrap())
		} else {
			context.error.err_at_token(
				"Variable undefined or out of scope.",
				"Define this variable before use.",
				self.var,
			)
		}
	}

	fn generate_source(&mut self, _context: &mut CompileContext<'a>) {}
}
