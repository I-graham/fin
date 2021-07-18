use super::*;

impl<'a> ASTNode<'a> for Sum<'a> {
	fn construct(
		context: &mut CompileContext<'a>,
		start: usize,
	) -> Result<(usize, Self), (Token<'a>, &'static [TokenKind])> {
		let (mut node_end, term) = Multiplication::construct(context, start)?;
		let mut ret = Self {
			addends: vec![(term, None)],
		};
		while context.tokens.len() > node_end {
			let token = context.tokens[node_end];
			match token.kind {
				TokenKind::Plus | TokenKind::Minus => {
					let (node2_end, term2) = Multiplication::construct(context, node_end + 1)?;
					ret.addends.push((term2, Some(token)));
					node_end = node2_end;
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
			let factors = vec![(Factor::Literal(LiteralInt::from_const(pre, context)), None)];
			ret.addends.insert(0, (Multiplication { factors }, None));
		}
		Ok((node_end, ret))
	}

	fn precompute(&self) -> Option<Word> {
		if self.addends.len() == 1 {
			self.addends[0].0.precompute()
		} else {
			None
		}
	}

	fn record_var_usage(&mut self, context: &mut CompileContext<'a>) -> Option<VariableID<'a>> {
		let acc = self.addends[0].0.record_var_usage(context).unwrap();
		context.scope.record_usage(&[acc]);
		for (addend, _) in &mut self.addends[1..] {
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
				.place_vars(&[opvar, next], &mut context.program.code);
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
		use TokenKind::*;
		let (mut node_end, term) = Factor::construct(context, start)?;
		let mut ret = Self {
			factors: vec![(term, None)],
		};
		while context.tokens.len() > node_end + 1 {
			let token = context.tokens[node_end];
			match token.kind {
				Mul | Div => {
					let (node2_end, term2) = Factor::construct(context, node_end + 1)?;
					ret.factors.push((term2, Some(token)));
					node_end = node2_end;
				}
				_ => break,
			}
		}

		let mut pre: Word = 1;
		let mut i = 0;
		while i < ret.factors.len() {
			let factor = &ret.factors[i];
			let token = factor.1;
			match factor.0.precompute() {
				Some(val) => {
					ret.factors.remove(i);
					match token {
						Some(Token { kind: Div, .. }) => {
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
						_ => pre = pre.wrapping_mul(val),
					}
				}
				None => i += 1,
			}
		}

		if pre != 1
			|| ret.factors.is_empty()
			|| ret.factors[0].1.filter(|token| token.kind == Div).is_some()
		{
			let lit = LiteralInt::from_const(pre, context);
			ret.factors.insert(0, (Factor::Literal(lit), None));
		}
		Ok((node_end, ret))
	}

	fn precompute(&self) -> Option<Word> {
		if self.factors.len() == 1 {
			self.factors[0].0.precompute()
		} else {
			None
		}
	}

	fn record_var_usage(&mut self, context: &mut CompileContext<'a>) -> Option<VariableID<'a>> {
		for (factor, _) in &mut self.factors {
			factor.record_var_usage(context);
		};
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
				.place_vars(&[opvar, next], &mut context.program.code);
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
			let (node_end, sum) = Expr::construct(context, start + 1)?;
			let _ = tokens_match_kinds(&context.tokens[node_end..], &[RParen])?;
			Ok((node_end + 1, Self::Parenthesized(sum)))
		} else {
			Err((context.tokens[start], &[Integer, Ident, LParen]))
		}
	}

	fn precompute(&self) -> Option<Word> {
		match self {
			Self::Literal(lit) => lit.precompute(),
			Self::Parenthesized(sum) => sum.precompute(),
			Self::Var(_) => None,
		}
	}

	fn record_var_usage(&mut self, context: &mut CompileContext<'a>) -> Option<VariableID<'a>> {
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
