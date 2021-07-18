use super::*;

//cond is emitted after comparison, negation is emitted after cond

impl<'a> ASTNode<'a> for OrExpr<'a> {
	fn construct(
		context: &mut CompileContext<'a>,
		start: usize,
	) -> Result<(usize, Self), (Token<'a>, &'static [TokenKind])> {
		let (mut node_end, term) = AndExpr::construct(context, start)?;
		let mut ret = Self { ands: vec![term] };
		while tokens_match_kinds(&context.tokens[node_end..], &[TokenKind::Or]).is_ok() {
			let (node2_end, term2) = AndExpr::construct(context, node_end + 1)?;
			ret.ands.push(term2);
			node_end = node2_end;
		}
		Ok((node_end, ret))
	}
	fn record_var_usage(&mut self, context: &mut CompileContext<'a>) -> Option<VariableID<'a>> {
		for and in &mut self.ands {
			and.record_var_usage(context);
		}
		None
	}
	fn generate_source(&self, context: &mut CompileContext<'a>) {
		let mut early_exits = vec![];
		for and in &self.ands {
			and.generate_source(context);
			early_exits.push(context.code().len() - 1);
		}
		let true_jmp = context.code().len() as Word;
		let false_jmp = true_jmp + 1;
		context.emit(
			&[
				Instruction {
					mnemonic: Mnemonic::RelJmp,
					..Default::default()
				},
				Instruction {
					mnemonic: Mnemonic::RelJmp,
					..Default::default()
				},
			],
			None,
		);
		for early_exit in early_exits.iter().map(|&u| u as Word) {
			abbreviations::branch(early_exit - 1, true_jmp, context.code());
		}
		abbreviations::branch(
			*early_exits.last().unwrap() as Word,
			false_jmp,
			context.code(),
		);
	}
}

impl<'a> ASTNode<'a> for AndExpr<'a> {
	fn construct(
		context: &mut CompileContext<'a>,
		start: usize,
	) -> Result<(usize, Self), (Token<'a>, &'static [TokenKind])> {
		let (mut node_end, term) = Boolean::construct(context, start)?;
		let mut ret = Self { bools: vec![term] };
		while tokens_match_kinds(&context.tokens[node_end..], &[TokenKind::And]).is_ok() {
			let (node2_end, term2) = Boolean::construct(context, node_end + 1)?;
			ret.bools.push(term2);
			node_end = node2_end;
		}
		Ok((node_end, ret))
	}
	fn record_var_usage(&mut self, context: &mut CompileContext<'a>) -> Option<VariableID<'a>> {
		for boolean in &mut self.bools {
			boolean.record_var_usage(context);
		}
		None
	}
	fn generate_source(&self, context: &mut CompileContext<'a>) {
		let mut early_exits = vec![];
		for boolean in &self.bools {
			boolean.generate_source(context);
			early_exits.push(context.code().len() - 1);
		}
		for early_exit in early_exits.into_iter().map(|u| u as Word) {
			abbreviations::branch(early_exit - 1, early_exit + 1, context.code());
			abbreviations::branch(early_exit, context.code().len() as Word, context.code());
		}
	}
}

impl<'a> ASTNode<'a> for Boolean<'a> {
	fn construct(
		context: &mut CompileContext<'a>,
		start: usize,
	) -> Result<(usize, Self), (Token<'a>, &'static [TokenKind])> {
		let (end, comparison) = Comparison::construct(context, start)?;
		Ok((end, Self::Comparison(comparison)))
	}
	fn record_var_usage(&mut self, context: &mut CompileContext<'a>) -> Option<VariableID<'a>> {
		match self {
			Self::Comparison(comp) => comp.record_var_usage(context),
		}
	}
	fn generate_source(&self, context: &mut CompileContext<'a>) {
		match self {
			Self::Comparison(comp) => {
				comp.generate_source(context);
				context.emit(
					&[
						Instruction {
							mnemonic: Mnemonic::RelJmp,
							condition: comp.cond(),
							..Default::default()
						},
						Instruction {
							mnemonic: Mnemonic::RelJmp,
							condition: comp.cond().negate(),
							..Default::default()
						},
					],
					Some(comp.op),
				);
			}
		}
	}
}

impl<'a> ASTNode<'a> for Comparison<'a> {
	fn construct(
		context: &mut CompileContext<'a>,
		start: usize,
	) -> Result<(usize, Self), (Token<'a>, &'static [TokenKind])> {
		use TokenKind::*;
		static COMP_OPS: [TokenKind; 5] = [Less, Greater, LessEq, GreaterEq, Eq];
		let err_ret = Err((*context.tokens.last().unwrap(), &COMP_OPS[..]));

		let (arg_a_end, arg_a) = Expr::construct(context, start)?;
		if context.tokens.len() <= arg_a_end {
			return err_ret;
		}
		let op = context.tokens[arg_a_end];
		if !COMP_OPS.contains(&op.kind) {
			return err_ret;
		}
		let (arg_b_end, arg_b) = Expr::construct(context, arg_a_end + 1)?;
		Ok((
			arg_b_end,
			Self {
				args: (arg_a, arg_b),
				op,
			},
		))
	}

	fn record_var_usage(&mut self, context: &mut CompileContext<'a>) -> Option<VariableID<'a>> {
		let a = self.args.0.record_var_usage(context).unwrap();
		let b = self.args.1.record_var_usage(context).unwrap();
		let [opvar] = context.scope.allocate_var(&[(None, VarType::Int)]);
		context.scope.record_usage(&[a, b, opvar]);
		Some(opvar)
	}

	fn precompute(&self) -> Option<Word> {
		use TokenKind::*;
		let a_arg = self.args.0.precompute();
		let b_arg = self.args.1.precompute();

		a_arg.zip(b_arg).map(|(a, b)| match self.op.kind {
			Less => a < b,
			Greater => a > b,
			LessEq => a <= b,
			GreaterEq => a >= b,
			Eq => a == b,
			_ => unreachable!(),
		} as Word)
	}

	fn generate_source(&self, context: &mut CompileContext<'a>) {
		self.args.0.generate_source(context);
		self.args.1.generate_source(context);
		let a = self.args.0.output_var().unwrap();
		let b = self.args.1.output_var().unwrap();
		let [a_opreg, b_opreg] = context.scope.place_vars(&[a, b], &mut context.program.code);
		context.emit(
			&[Instruction {
				mnemonic: Mnemonic::Cmp,
				condition: Condition::Al,
				args: [a_opreg, b_opreg, 0, 0, 0, 0],
			}],
			Some(self.op),
		);
	}
}