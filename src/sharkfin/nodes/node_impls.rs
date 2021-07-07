use super::node_structs::*;
use super::*;
use crate::sharkfin::abbreviations;

fn tokens_match_kinds<'l, I: IntoIterator<Item = &'l TokenKind>>(
	inputs: &[Token],
	kinds: I,
) -> Result<(), usize> {
	for (i, kind) in kinds.into_iter().enumerate() {
		if i >= inputs.len() || inputs[i].kind != *kind {
			return Err(i);
		}
	}
	Ok(())
}

impl<'a> ASTNode<'a> for ProgramRoot<'a> {
	fn construct(
		context: &mut CompileContext<'a>,
		start: usize,
	) -> Result<(usize, Self), (Token<'a>, &'static [TokenKind])> {
		match RootNode::construct(context, start) {
			Ok((prog_end, program)) => {
				if context.tokens[prog_end].kind == TokenKind::Eof {
					Ok((prog_end, Self(program)))
				} else {
					Err((context.tokens[prog_end], &[TokenKind::Eof]))
				}
			}
			Err(err) => Err(err),
		}
	}

	fn record_var_usage(&self, context: &mut CompileContext<'a>) -> Option<VariableID<'a>> {
		self.0.record_var_usage(context)
	}

	fn generate_source(&self, context: &mut CompileContext<'a>) {
		self.0.generate_source(context);
		abbreviations::load_const(0, 0, None, &mut context.output.code);
		context.output.code.push(Instruction {
			mnemonic: Mnemonic::Hlt,
			..Default::default()
		});
		context.update_debug_info(context.tokens.last().copied());
	}
}

impl<'a> ASTNode<'a> for CodeBlock<'a> {
	fn construct(
		context: &mut CompileContext<'a>,
		start: usize,
	) -> Result<(usize, Self), (Token<'a>, &'static [TokenKind])> {
		use TokenKind::*;
		if context.tokens[start].kind == LCurly {
			match Statement::construct(context, start + 1) {
				Ok((mut statement_end, first)) => {
					let mut ret = Self { code: vec![first] };
					while let Ok((node_end, next)) = Statement::construct(context, statement_end) {
						statement_end = node_end;
						ret.code.push(next);
					}
					if context.tokens[statement_end].kind == RCurly {
						Ok((statement_end + 1, ret))
					} else {
						Err((context.tokens[statement_end], &[RCurly, Let]))
					}
				}
				Err(err) => Err(err),
			}
		} else {
			Err((context.tokens[start], &[LCurly]))
		}
	}

	fn record_var_usage(&self, context: &mut CompileContext<'a>) -> Option<VariableID<'a>> {
		self.code.iter().for_each(|statement| {
			statement.record_var_usage(context);
		});
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
		if context.tokens[start].kind == TokenKind::Semicolon {
			Ok((start + 1, Self::Empty))
		} else if let Ok((node_end, let_var)) = LetVar::construct(context, start) {
			Ok((node_end, Self::LetVar(let_var)))
		} else {
			match MutateVar::construct(context, start) {
				Ok((node_end, mut_var)) => Ok((node_end, Self::MutateVar(mut_var))),
				Err(err) => Err(err),
			}
		}
	}

	fn record_var_usage(&self, context: &mut CompileContext<'a>) -> Option<VariableID<'a>> {
		match self {
			Self::LetVar(let_var) => let_var.record_var_usage(context),
			Self::MutateVar(mut_var) => mut_var.record_var_usage(context),
			Self::Empty => None,
		}
	}

	fn generate_source(&self, context: &mut CompileContext<'a>) {
		match self {
			Self::LetVar(letvar) => letvar.generate_source(context),
			Self::MutateVar(mut_var) => mut_var.generate_source(context),
			Self::Empty => (),
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
		match tokens_match_kinds(&context.tokens[start..], &FORMAT) {
			Ok(()) => match MutateVar::construct(context, start + 2) {
				Ok((end, mut_var)) => Ok((end, Self { mut_var })),
				Err(err) => Err(err),
			},
			Err(i) => Err((context.tokens[i + start], &FORMAT[i..=i])),
		}
	}

	fn record_var_usage(&self, context: &mut CompileContext<'a>) -> Option<VariableID<'a>> {
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
		context.scope.create_synonym(Some(var_name), sum_var);
		self.mut_var.access.record_var_usage(context);
		None
	}

	fn generate_source(&self, context: &mut CompileContext<'a>) {
		self.mut_var.generate_source(context);
		let access = &self.mut_var.access;
		let var_id = VariableID::Named(access.var.text);
		println!(
			"Var `{}` stored in reg #{:?} at inst #{}.",
			access.var.text,
			&context.scope.register(&[var_id], &mut context.output.code),
			context.output.code.len()
		);
		context.update_debug_info(Some(access.var));

		context.output.code.push(Instruction {
			mnemonic: Mnemonic::Dbg,
			..Default::default()
		});
	}
}

impl<'a> ASTNode<'a> for Sum<'a> {
	fn construct(
		context: &mut CompileContext<'a>,
		start: usize,
	) -> Result<(usize, Self), (Token<'a>, &'static [TokenKind])> {
		match Multiplication::construct(context, start) {
			Ok((mut node_end, term)) => {
				let mut ret = Self {
					addends: vec![(term, None)],
				};
				while context.tokens.len() > node_end {
					let token = context.tokens[node_end];
					match token.kind {
						TokenKind::Plus | TokenKind::Minus => {
							match Multiplication::construct(context, node_end + 1) {
								Ok((node2_end, term2)) => {
									ret.addends.push((term2, Some(token)));
									node_end = node2_end;
								}
								Err(err) => return Err(err),
							}
						}
						_ => break,
					}
				}

				let mut pre: Word = 0;
				let mut i = 0;
				while i < ret.addends.len() {
					let addend = &ret.addends[i];
					if let Some(val) = addend.0.precompute() {
						let token = addend.1;
						ret.addends.remove(i);
						if token.is_none() || token.unwrap().kind == TokenKind::Plus {
							pre = pre.wrapping_add(val);
						} else {
							pre = pre.wrapping_sub(val);
						}
					} else {
						i += 1;
					}
				}
				if pre != 0 || ret.addends.is_empty() {
					let factors =
						vec![(Factor::Literal(LiteralInt::from_const(pre, context)), None)];
					ret.addends.insert(0, (Multiplication { factors }, None));
				}
				Ok((node_end, ret))
			}
			Err(err) => Err(err),
		}
	}

	fn precompute(&self) -> Option<Word> {
		if self.addends.len() == 1 {
			self.addends[0].0.precompute()
		} else {
			None
		}
	}

	fn record_var_usage(&self, context: &mut CompileContext<'a>) -> Option<VariableID<'a>> {
		let acc = self.addends[0].0.record_var_usage(context).unwrap();
		context.scope.record_usage(&[acc]);
		for (addend, _) in &self.addends[1..] {
			let data = addend.record_var_usage(context).unwrap();
			context.scope.record_usage(&[acc, data]);
		}
		Some(acc)
	}

	fn output_var(&self) -> Option<VariableID<'a>> {
		self.addends[0].0.output_var()
	}

	fn generate_source(&self, context: &mut CompileContext<'a>) {
		self.addends[0].0.generate_source(context);
		let opvar = self.output_var().unwrap();
		for (addend, token) in &self.addends[1..] {
			addend.generate_source(context);
			let next = addend.output_var().unwrap();
			let [acc, data] = context
				.scope
				.register(&[opvar, next], &mut context.output.code);
			context.update_debug_info(*token);
			context.emit(
				&[Instruction {
					condition: Condition::Al,
					mnemonic: if token.is_none() || token.unwrap().kind == TokenKind::Plus {
						Mnemonic::Add
					} else {
						Mnemonic::Sub
					},
					args: [acc, acc, data, 0, 0, 0],
				}],
				*token,
			);
		}
	}
}

impl<'a> ASTNode<'a> for Multiplication<'a> {
	fn construct(
		context: &mut CompileContext<'a>,
		start: usize,
	) -> Result<(usize, Self), (Token<'a>, &'static [TokenKind])> {
		match Factor::construct(context, start) {
			Ok((mut node_end, term)) => {
				let mut ret = Self {
					factors: vec![(term, None)],
				};
				while context.tokens.len() > node_end + 1 {
					let token = context.tokens[node_end];
					match token.kind {
						TokenKind::Mul | TokenKind::Div => {
							match Factor::construct(context, node_end + 1) {
								Ok((node2_end, term2)) => {
									ret.factors.push((term2, Some(token)));
									node_end = node2_end;
								}
								Err(err) => return Err(err),
							}
						}
						_ => break,
					}
				}

				let mut pre: Word = 1;
				let mut i = 0;
				while i < ret.factors.len() {
					let factor = &ret.factors[i];
					if let Some(val) = factor.0.precompute() {
						let token = factor.1;
						ret.factors.remove(i);
						match token {
							None
							| Some(Token {
								kind: TokenKind::Mul,
								..
							}) => pre = pre.wrapping_mul(val),
							Some(_) => {
								if val != 0 {
									pre = (pre as i64 / val as i64) as Word;
								} else {
									context.error.err_at_token(
										"Unconditional division by zero.",
										"",
										token.unwrap(),
									);
								}
							}
						}
					} else {
						i += 1;
					}
				}

				if pre != 1 {
					let lit = LiteralInt::from_const(pre, context);
					ret.factors.push((Factor::Literal(lit), None));
				}
				if ret.factors.is_empty()
					|| matches!(
						ret.factors[0].1,
						Some(Token {
							kind: TokenKind::Div,
							..
						})
					) {
					let lit = LiteralInt::from_const(1, context);
					ret.factors.insert(0, (Factor::Literal(lit), None));
				}
				Ok((node_end, ret))
			}
			Err(err) => Err(err),
		}
	}

	fn precompute(&self) -> Option<Word> {
		if self.factors.len() == 1 {
			self.factors[0].0.precompute()
		} else {
			None
		}
	}

	fn record_var_usage(&self, context: &mut CompileContext<'a>) -> Option<VariableID<'a>> {
		self.factors.iter().for_each(|(factor, _)| {
			factor.record_var_usage(context);
		});
		self.output_var()
	}

	fn output_var(&self) -> Option<VariableID<'a>> {
		self.factors[0].0.output_var()
	}

	fn generate_source(&self, context: &mut CompileContext<'a>) {
		self.factors[0].0.generate_source(context);
		let opvar = self.output_var().unwrap();
		for (factor, token) in &self.factors[1..] {
			factor.generate_source(context);
			let next = factor.output_var().unwrap();
			let [acc, data] = context
				.scope
				.register(&[opvar, next], &mut context.output.code);
			context.update_debug_info(*token);
			context.emit(
				&[Instruction {
					condition: Condition::Al,
					mnemonic: if token.is_none() || token.unwrap().kind == TokenKind::Mul {
						Mnemonic::Mul
					} else {
						Mnemonic::Div
					},
					args: [acc, acc, data, 0, 0, 0],
				}],
				*token,
			);
		}
	}
}

impl<'a> ASTNode<'a> for Factor<'a> {
	fn construct(
		context: &mut CompileContext<'a>,
		start: usize,
	) -> Result<(usize, Self), (Token<'a>, &'static [TokenKind])> {
		use TokenKind::*;
		if let Ok((node_end, lit)) = LiteralInt::construct(context, start) {
			Ok((node_end, Self::Literal(lit)))
		} else if let Ok((node_end, var)) = AccessVar::construct(context, start) {
			Ok((node_end, Self::Var(var)))
		} else if context.tokens.len() > 2 && context.tokens[start].kind == LParen {
			match Sum::construct(context, start + 1) {
				Ok((node_end, sum)) => {
					if context.tokens[node_end].kind == RParen {
						Ok((node_end + 1, Self::Parenthesized(sum)))
					} else {
						Err((context.tokens[node_end], &[RParen]))
					}
				}
				Err(err) => Err(err),
			}
		} else {
			Err((context.tokens[start], &[Integer, Ident, LParen]))
		}
	}

	fn precompute(&self) -> Option<Word> {
		match self {
			Self::Literal(lit) => lit.precompute(),
			Self::Parenthesized(sum) => sum.precompute(),
			Self::Var(_var) => None,
		}
	}

	fn record_var_usage(&self, context: &mut CompileContext<'a>) -> Option<VariableID<'a>> {
		match self {
			Self::Var(var) => var.record_var_usage(context),
			Self::Literal(lit) => lit.record_var_usage(context),
			Self::Parenthesized(sum) => sum.record_var_usage(context),
		}
	}

	fn output_var(&self) -> Option<VariableID<'a>> {
		match self {
			Self::Var(var) => var.output_var(),
			Self::Literal(lit) => lit.output_var(),
			Self::Parenthesized(sum) => sum.output_var(),
		}
	}

	fn generate_source(&self, context: &mut CompileContext<'a>) {
		match self {
			Self::Var(var) => var.generate_source(context),
			Self::Literal(lit) => lit.generate_source(context),
			Self::Parenthesized(sum) => sum.generate_source(context),
		}
	}
}

impl<'a> ASTNode<'a> for AccessVar<'a> {
	fn construct(
		context: &mut CompileContext<'a>,
		start: usize,
	) -> Result<(usize, Self), (Token<'a>, &'static [TokenKind])> {
		if context.tokens.len() > start && context.tokens[start].kind == TokenKind::Ident {
			Ok((
				start + 1,
				Self {
					var: context.tokens[start],
				},
			))
		} else {
			Err((context.tokens[start], &[TokenKind::Ident]))
		}
	}

	fn record_var_usage(&self, context: &mut CompileContext<'a>) -> Option<VariableID<'a>> {
		if let Some(var) = context.scope.get_var(self.var.text) {
			context.scope.record_usage(&[var]);
			Some(var)
		} else {
			context.error.err_at_token(
				"Use of undefined variable",
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

impl<'a> ASTNode<'a> for MutateVar<'a> {
	fn construct(
		context: &mut CompileContext<'a>,
		start: usize,
	) -> Result<(usize, Self), (Token<'a>, &'static [TokenKind])> {
		use TokenKind::*;
		static FORMAT: [TokenKind; 2] = [Ident, Assign];
		match tokens_match_kinds(&context.tokens[start..], &FORMAT) {
			Ok(()) => match AccessVar::construct(context, start) {
				Ok((end, access)) => match Sum::construct(context, end + 1) {
					Ok((node_end, sum)) => {
						if context.tokens.len() > node_end
							&& context.tokens[node_end].kind == Semicolon
						{
							Ok((node_end + 1, Self { access, val: sum }))
						} else {
							Err((context.tokens[node_end], &[Semicolon]))
						}
					}
					Err(err) => Err(err),
				},
				Err(err) => Err(err),
			},
			Err(i) => Err((context.tokens[i + start], &FORMAT[i..=i])),
		}
	}

	fn record_var_usage(&self, context: &mut CompileContext<'a>) -> Option<VariableID<'a>> {
		let value = self.val.record_var_usage(context).unwrap();
		let access = self.access.record_var_usage(context).unwrap();
		context.scope.record_usage(&[value, access]);
		None
	}

	fn generate_source(&self, context: &mut CompileContext<'a>) {
		self.access.generate_source(context);
		self.val.generate_source(context);
		let var_op = self.access.output_var().unwrap();
		let val_op = self.val.output_var().unwrap();
		let out = &mut context.output.code;
		let [val, var] = context.scope.register(&[val_op, var_op], out);
		context.emit(
			&[
				Instruction {
					condition: Condition::Al,
					mnemonic: Mnemonic::Mov,
					args: [val, var, 0, 0, 0, 0],
				},
				Instruction {
					condition: Condition::Al,
					mnemonic: Mnemonic::Dbg,
					args: [val, var, 0, 0, 0, 0],
				},
			],
			Some(self.access.var),
		)
	}
}

impl<'a> ASTNode<'a> for LiteralInt<'a> {
	fn construct(
		context: &mut CompileContext<'a>,
		start: usize,
	) -> Result<(usize, Self), (Token<'a>, &'static [TokenKind])> {
		use TokenKind::*;
		if context.tokens.len() > start {
			let op = context.tokens[start].kind;
			if op == Integer {
				let value = context.tokens[start].text.parse().unwrap();
				return Ok((start + 1, Self::from_const(value, context)));
			} else if op == Minus {
				let mut negate = true;
				for (i, token) in context.tokens[start + 1..].iter().enumerate() {
					if token.kind == Minus {
						negate = !negate;
					} else if token.kind == Integer {
						let literal: Word = token.text.parse().unwrap();
						let value = if negate {
							literal.wrapping_neg()
						} else {
							literal
						};
						return Ok((start + i + 2, Self::from_const(value, context)));
					} else {
						return Err((context.tokens[start + i + 1], &[Integer]));
					}
				}
			}
		}
		Err((context.tokens[start], &[Integer]))
	}

	fn precompute(&self) -> Option<Word> {
		Some(self.value)
	}

	fn record_var_usage(&self, context: &mut CompileContext<'a>) -> Option<VariableID<'a>> {
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
		if self.value.leading_zeros() + self.value.trailing_zeros() >= Word::BITS / 2 {
			let [start] = context
				.scope
				.register(&[self.output.0], &mut context.output.code);
			abbreviations::load_const(self.value, start, None, &mut context.output.code);
		} else {
			let scratch_var = self.output.1.unwrap();
			let [start, scratch] = context
				.scope
				.register(&[self.output.0, scratch_var], &mut context.output.code);
			abbreviations::load_const(self.value, start, Some(scratch), &mut context.output.code);
		}
		context.update_debug_info(None);
	}
}
