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

		let is_float_expr = ret
			.addends
			.iter()
			.any(|(addend, _)| addend.output_type(context).unwrap() == VarType::Float);
		let mut pre: Word = 0;
		let mut i = 0;
		while i < ret.addends.len() {
			let addend = &ret.addends[i];
			let aty = addend.0.output_type(context).unwrap();
			if let Some(val) = addend.0.precompute() {
				let token = addend.1;
				ret.addends.remove(i);
				let to_fw = FWord::from_bits;
				if token.is_none() || token.unwrap().kind == TokenKind::Plus {
					if is_float_expr {
						let arg = if aty == VarType::Float {
							FWord::from_bits(val)
						} else {
							val as FWord
						};
						pre = (to_fw(pre) + arg).to_bits();
					} else {
						pre = pre.wrapping_add(val);
					}
				} else if is_float_expr {
					let arg = if aty == VarType::Float {
						FWord::from_bits(val)
					} else {
						val as FWord
					};
					pre = (to_fw(pre) - arg).to_bits();
				} else {
					pre = pre.wrapping_sub(val);
				}
			} else {
				i += 1;
			}
		}

		let is_nop = if is_float_expr {
			use std::cmp::Ordering;
			FWord::from_bits(pre).partial_cmp(&0.0) == Some(Ordering::Equal)
		} else {
			pre == 0
		};

		if !is_nop || ret.addends.is_empty() {
			let addend = if is_float_expr {
				let lit = LiteralFloat::from_const(FWord::from_bits(pre), context);
				Factor::LiteralFloat(lit)
			} else {
				let lit = LiteralInt::from_const(pre, context);
				Factor::LiteralInt(lit)
			};
			let ty = addend.output_type(context).unwrap();
			let [acc_var] = context.scope.allocate_vars(&[(None, ty)]);
			ret.addends.insert(
				0,
				(
					Multiplication {
						factors: vec![(addend, None)],
						accumulator: Some(acc_var),
					},
					None,
				),
			);
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

	fn output_var(&self) -> Option<VariableID<'a>> {
		self.addends[0].0.output_var()
	}

	fn output_type(&self, context: &CompileContext) -> Option<VarType> {
		let is_float_expr = self
			.addends
			.iter()
			.any(|(addend, _)| addend.output_type(context).unwrap() == VarType::Float);
		Some(if is_float_expr {
			VarType::Float
		} else {
			VarType::Int
		})
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

	fn generate_source(&self, context: &mut CompileContext<'a>) {
		let is_float_expr = self.output_type(context).unwrap() == VarType::Float;

		let convert = |var, context: &mut CompileContext<'a>| {
			if is_float_expr && context.scope.var_type(var).unwrap() != VarType::Float {
				let [data] = context
					.scope
					.place_vars(&[(var, false)], &mut context.program.code);
				context.emit(
					&[Instruction {
						mnemonic: Mnemonic::IToF,
						args: [data, data, 0, 0, 0, 0],
						..Default::default()
					}],
					None,
				);
			}
		};

		self.addends[0].0.generate_source(context);
		let opvar = self.output_var().unwrap();
		convert(opvar, context);
		for (addend, token) in &self.addends[1..] {
			addend.generate_source(context);
			let next = addend.output_var().unwrap();
			let [acc, data] = context
				.scope
				.place_vars(&[(opvar, false), (next, false)], &mut context.program.code);
			convert(next, context);
			context.emit(
				&[Instruction {
					condition: Condition::Al,
					mnemonic: if token.is_none() || token.unwrap().kind == TokenKind::Plus {
						if is_float_expr {
							Mnemonic::FAdd
						} else {
							Mnemonic::Add
						}
					} else if is_float_expr {
						Mnemonic::FSub
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
			accumulator: None,
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

		let is_float_expr = ret
			.factors
			.iter()
			.any(|(factor, _)| factor.output_type(context).unwrap() == VarType::Float);
		let mut pre: Word = if is_float_expr { 1f64.to_bits() } else { 1u64 };
		let mut i = 0;
		while i < ret.factors.len() {
			let factor = &ret.factors[i];
			let token = factor.1;
			let fty = factor.0.output_type(&context).unwrap();
			match factor.0.precompute() {
				Some(val) => {
					let removed = ret.factors.remove(i);
					let to_fw = FWord::from_bits;

					match token {
						Some(Token { kind: Div, .. }) => {
							if val == 0 {
								context.error.err_at_token(
									"Unconditional division by zero.",
									"",
									token.unwrap(),
								);
							}
							if is_float_expr {
								let arg = if fty != VarType::Float {
									val as FWord
								} else {
									to_fw(val)
								};
								pre = (to_fw(pre) / arg).to_bits();
							} else if pre as i64 % val as i64 == 0 {
								pre = (pre as i64 / val as i64) as Word;
							} else {
								ret.factors.push(removed);
								i += 1;
							}
						}
						_ => {
							if is_float_expr {
								let arg = if fty != VarType::Float {
									val as FWord
								} else {
									to_fw(val)
								};
								pre = (to_fw(pre) * arg).to_bits();
							} else {
								pre = pre.wrapping_mul(val)
							}
						}
					}
				}
				None => i += 1,
			}
		}

		let is_nop = if is_float_expr {
			use std::cmp::Ordering;
			FWord::from_bits(pre).partial_cmp(&1.0) == Some(Ordering::Equal)
		} else {
			pre == 1
		};

		if !is_nop
			|| ret.factors.is_empty()
			|| ret.factors[0].1.filter(|token| token.kind == Div).is_some()
		{
			let factor = if is_float_expr {
				let lit = LiteralFloat::from_const(FWord::from_bits(pre), context);
				Factor::LiteralFloat(lit)
			} else {
				let lit = LiteralInt::from_const(pre, context);
				Factor::LiteralInt(lit)
			};
			ret.factors.insert(0, (factor, None));
		}

		let ty = if is_float_expr {
			VarType::Float
		} else {
			VarType::Int
		};

		let [acc] = context.scope.allocate_vars(&[(None, ty)]);
		ret.accumulator = Some(acc);

		Ok((node_end, ret))
	}

	fn precompute(&self) -> Option<Word> {
		if self.factors.len() == 1 {
			self.factors[0].0.precompute()
		} else {
			None
		}
	}

	fn output_var(&self) -> Option<VariableID<'a>> {
		let factor = &self.factors[0].0;
		match factor {
			Factor::Var(_) => self.accumulator,
			_ => factor.output_var(),
		}
	}

	fn output_type(&self, context: &CompileContext) -> Option<VarType> {
		Some(context.scope.var_type(self.output_var().unwrap()).unwrap())
	}

	fn record_var_usage(&mut self, context: &mut CompileContext<'a>) -> Option<VariableID<'a>> {
		let first = &self.factors[0].0;
		let opvar = if matches!(first, Factor::Var(_)) {
			self.accumulator.unwrap()
		} else {
			first.output_var().unwrap()
		};
		for (factor, _) in &mut self.factors {
			let data = factor.record_var_usage(context).unwrap();
			context.scope.record_usage(&[opvar, data]);
		}
		self.output_var()
	}

	fn generate_source(&self, context: &mut CompileContext<'a>) {
		let is_float_expr = self.output_type(context).unwrap() == VarType::Float;

		let first = &self.factors[0].0;
		first.generate_source(context);
		let use_acc = matches!(first, Factor::Var(_));
		let opvar = if use_acc {
			let accumulator = self.accumulator.unwrap();
			let data_var = first.output_var().unwrap();
			let ty = context.scope.var_type(data_var).unwrap();
			let [acc, data] = context.scope.place_vars(
				&[(accumulator, false), (data_var, false)],
				&mut context.program.code,
			);
			if is_float_expr && ty != VarType::Float {
				context.emit(
					&[Instruction {
						mnemonic: Mnemonic::IToF,
						args: [acc, data, 0, 0, 0, 0],
						..Default::default()
					}],
					None,
				);
			} else {
				context.emit(
					&[Instruction {
						mnemonic: Mnemonic::Mov,
						args: [data, acc, 0, 0, 0, 0],
						..Default::default()
					}],
					None,
				);
			}
			accumulator
		} else {
			first.output_var().unwrap()
		};

		for (factor, token) in &self.factors[1..] {
			factor.generate_source(context);
			let is_var = matches!(factor, Factor::Var(_));
			let next = factor.output_var().unwrap();
			let ty = context.scope.var_type(next).unwrap();
			let matches_ty = if is_float_expr {
				ty == VarType::Float
			} else {
				ty == VarType::Int
			};
			let [acc, data] = context.scope.place_vars(
				&[(opvar, false), (next, is_var && !matches_ty)],
				&mut context.program.code,
			);
			if is_float_expr && ty != VarType::Float {
				context.emit(
					&[Instruction {
						mnemonic: Mnemonic::IToF,
						args: [data, data, 0, 0, 0, 0],
						..Default::default()
					}],
					None,
				);
			}
			context.emit(
				&[Instruction {
					condition: Condition::Al,
					mnemonic: if token.is_none() || token.unwrap().kind == TokenKind::Mul {
						if is_float_expr {
							Mnemonic::FMul
						} else {
							Mnemonic::Mul
						}
					} else if is_float_expr {
						Mnemonic::FDiv
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
		if let Ok((node_end, lit)) = LiteralFloat::construct(context, start) {
			Ok((node_end, Self::LiteralFloat(lit)))
		} else if let Ok((node_end, lit)) = LiteralInt::construct(context, start) {
			Ok((node_end, Self::LiteralInt(lit)))
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
			Self::LiteralInt(lit) => lit.precompute(),
			Self::LiteralFloat(float) => float.precompute(),
			Self::Parenthesized(sum) => sum.precompute(),
			Self::Var(_) => None,
		}
	}

	fn record_var_usage(&mut self, context: &mut CompileContext<'a>) -> Option<VariableID<'a>> {
		match self {
			Self::Var(var) => var.record_var_usage(context),
			Self::LiteralInt(lit) => lit.record_var_usage(context),
			Self::LiteralFloat(float) => float.record_var_usage(context),
			Self::Parenthesized(sum) => sum.record_var_usage(context),
		}
	}

	fn output_var(&self) -> Option<VariableID<'a>> {
		match self {
			Self::Var(var) => var.output_var(),
			Self::LiteralInt(lit) => lit.output_var(),
			Self::LiteralFloat(float) => float.output_var(),
			Self::Parenthesized(sum) => sum.output_var(),
		}
	}

	fn output_type(&self, context: &CompileContext) -> Option<VarType> {
		match self {
			Self::Var(var) => var.output_type(context),
			Self::LiteralInt(lit) => lit.output_type(context),
			Self::LiteralFloat(float) => float.output_type(context),
			Self::Parenthesized(sum) => sum.output_type(context),
		}
	}

	fn generate_source(&self, context: &mut CompileContext<'a>) {
		match self {
			Self::Var(var) => var.generate_source(context),
			Self::LiteralInt(lit) => lit.generate_source(context),
			Self::LiteralFloat(float) => float.generate_source(context),
			Self::Parenthesized(sum) => sum.generate_source(context),
		}
	}
}

impl<'a> ASTNode<'a> for LiteralFloat<'a> {
	fn construct(
		context: &mut CompileContext<'a>,
		start: usize,
	) -> Result<(usize, Self), (Token<'a>, &'static [TokenKind])> {
		use TokenKind::*;
		if context.tokens.len() <= start {
			return Err((*context.tokens.last().unwrap(), &[Float]));
		}
		let minus_count = context.tokens[start..]
			.iter()
			.take_while(|token| token.kind == Minus)
			.count();
		let rest = &context.tokens[start + minus_count..];
		let _ = tokens_match_kinds(rest, &[Float])?;
		let int_token = rest[0];
		let literal: FWord = int_token.text.parse().unwrap();
		let value = if minus_count % 2 == 1 {
			-literal
		} else {
			literal
		};
		Ok((start + minus_count + 1, Self::from_const(value, context)))
	}

	fn precompute(&self) -> Option<Word> {
		Some(self.value.to_bits())
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

	fn output_type(&self, _context: &CompileContext) -> Option<VarType> {
		Some(VarType::Float)
	}

	fn generate_source(&self, context: &mut CompileContext<'a>) {
		let bits = self.value.to_bits();
		if abbreviations::fits_in_const_inst(bits) {
			let [start] = context
				.scope
				.place_vars(&[(self.output.0, false)], &mut context.program.code);
			abbreviations::load_const(bits, start, None, &mut context.program.code);
		} else {
			let scratch_var = self.output.1.unwrap();
			let [start, scratch] = context.scope.place_vars(
				&[(self.output.0, false), (scratch_var, false)],
				&mut context.program.code,
			);
			abbreviations::load_const(bits, start, Some(scratch), &mut context.program.code);
		}
		context.update_debug_info(None);
	}
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

	fn output_type(&self, _context: &CompileContext) -> Option<VarType> {
		Some(VarType::Int)
	}

	fn generate_source(&self, context: &mut CompileContext<'a>) {
		if abbreviations::fits_in_const_inst(self.value) {
			let [start] = context
				.scope
				.place_vars(&[(self.output.0, false)], &mut context.program.code);
			abbreviations::load_const(self.value, start, None, &mut context.program.code);
		} else {
			let scratch_var = self.output.1.unwrap();
			let [start, scratch] = context.scope.place_vars(
				&[(self.output.0, false), (scratch_var, false)],
				&mut context.program.code,
			);
			abbreviations::load_const(self.value, start, Some(scratch), &mut context.program.code);
		}
		context.update_debug_info(None);
	}
}
