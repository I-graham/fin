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
				assert_eq!(context.tokens[prog_end].kind, TokenKind::Eof);
				Ok((prog_end, Self(program)))
			}
			Err(err) => Err(err),
		}
	}

	fn generate_source(&self, context: &mut CompileContext<'a>) {
		self.0.generate_source(context);
		context.output.code.push(Instruction {
			mnemonic: Mnemonic::Dbg,
			..Default::default()
		});
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
		match LetVar::construct(context, start) {
			Ok((node_end, letvar)) => Ok((node_end, Self::LetVar(letvar))),
			Err(err) => Err(err),
		}
	}

	fn generate_source(&self, context: &mut CompileContext<'a>) {
		match self {
			Self::LetVar(letvar) => letvar.generate_source(context),
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
			Ok(()) => {
				let new_start = start + FORMAT.len();
				let ident = context.tokens[start + 2];
				if context.scope.get_var(ident.text).is_some() {
					context
						.error
						.err_at_token("Variable name already in use!", ident);
				}
				match Sum::construct(context, new_start) {
					Ok((node_end, sum)) => {
						if context.tokens.len() > node_end
							&& context.tokens[node_end].kind == Semicolon
						{
							let variable = context
								.scope
								.create_synonym(Some(ident.text), sum.output_var().unwrap());
							Ok((
								node_end + 1,
								Self {
									init: sum,
									variable,
								},
							))
						} else {
							Err((context.tokens[node_end], &[Semicolon]))
						}
					}
					Err(err) => Err(err),
				}
			}
			Err(i) => Err((context.tokens[i + start], &FORMAT[i..=i])),
		}
	}

	fn generate_source(&self, context: &mut CompileContext<'a>) {
		self.init.generate_source(context);
		if let VariableID::Named(name) = self.variable {
			println!(
				"Var `{}` stored in reg #{:?} at inst #{}.",
				name,
				&context
					.scope
					.register(&[self.variable], &mut context.output.code),
				context.output.code.len()
			);
		} else {
			unreachable!();
		}

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
				let acc = term.output_var().unwrap();
				context.scope.record_usage(&[acc]);
				let mut ret = Self {
					addends: vec![(term, true)],
				};
				while context.tokens.len() > node_end + 1 {
					match context.tokens[node_end].kind {
						op @ TokenKind::Plus | op @ TokenKind::Minus => {
							match Multiplication::construct(context, node_end + 1) {
								Ok((node2_end, term2)) => {
									context.scope.record_usage(&[acc]);
									context.scope.record_usage(&[term2.output_var().unwrap()]);
									ret.addends.push((term2, op == TokenKind::Plus));
									node_end = node2_end;
								}
								Err(err) => return Err(err),
							}
						}
						_ => break,
					}
				}
				Ok((node_end, ret))
			}
			Err(err) => Err(err),
		}
	}

	fn output_var(&self) -> Option<VariableID<'a>> {
		self.addends[0].0.output_var()
	}

	fn generate_source(&self, context: &mut CompileContext<'a>) {
		self.addends[0].0.generate_source(context);
		let opvar = self.output_var().unwrap();
		for addend in &self.addends[1..] {
			addend.0.generate_source(context);
			let next_opvar = addend.0.output_var().unwrap();
			let [acc, data] = context
				.scope
				.register(&[opvar, next_opvar], &mut context.output.code);
			let inst = Instruction {
				condition: Condition::Al,
				mnemonic: if addend.1 {
					Mnemonic::Add
				} else {
					Mnemonic::Sub
				},
				args: [acc, acc, data, 0, 0, 0],
			};
			context.output.code.push(inst);
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
				let acc = term.output_var().unwrap();
				context.scope.record_usage(&[acc]);
				let mut ret = Self {
					factors: vec![(term, true)],
				};
				while context.tokens.len() > node_end + 1 {
					match context.tokens[node_end].kind {
						op @ TokenKind::Mul | op @ TokenKind::Div => {
							match Factor::construct(context, node_end + 1) {
								Ok((node2_end, term2)) => {
									context.scope.record_usage(&[acc]);
									context.scope.record_usage(&[term2.output_var().unwrap()]);
									ret.factors.push((term2, op == TokenKind::Mul));
									node_end = node2_end;
								}
								Err(err) => return Err(err),
							}
						}
						_ => break,
					}
				}
				Ok((node_end, ret))
			}
			Err(err) => Err(err),
		}
	}

	fn output_var(&self) -> Option<VariableID<'a>> {
		self.factors[0].0.output_var()
	}

	fn generate_source(&self, context: &mut CompileContext<'a>) {
		self.factors[0].0.generate_source(context);
		let opvar = self.output_var().unwrap();
		for factor in &self.factors[1..] {
			factor.0.generate_source(context);
			let next_opvar = factor.0.output_var().unwrap();
			let [acc, data] = context
				.scope
				.register(&[opvar, next_opvar], &mut context.output.code);
			let inst = Instruction {
				condition: Condition::Al,
				mnemonic: if factor.1 {
					Mnemonic::Mul
				} else {
					Mnemonic::Div
				},
				args: [acc, acc, data, 0, 0, 0],
			};
			context.output.code.push(inst);
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
			context.scope.record_usage(&[lit.output_var().unwrap()]);
			Ok((node_end, Self::Literal(lit)))
		} else if let Ok((node_end, var)) = AccessVar::construct(context, start) {
			context.scope.record_usage(&[var.output_var().unwrap()]);
			Ok((node_end, Self::Var(var)))
		} else if context.tokens.len() > 2 && context.tokens[start].kind == LParen {
			match Sum::construct(context, start + 1) {
				Ok((node_end, sum)) => {
					if context.tokens[node_end].kind == RParen {
						context.scope.record_usage(&[sum.output_var().unwrap()]);
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
			let ident = context.tokens[start];
			if let Some(variable) = context.scope.get_var(ident.text) {
				Ok((start + 1, Self { variable }))
			} else {
				context
					.error
					.err_at_token("Use of undeclared variable", ident)
			}
		} else {
			Err((context.tokens[start], &[TokenKind::Ident]))
		}
	}

	fn output_var(&self) -> Option<VariableID<'a>> {
		Some(self.variable)
	}

	fn generate_source(&self, _context: &mut CompileContext<'a>) {}
}

impl<'a> ASTNode<'a> for LiteralInt<'a> {
	fn construct(
		context: &mut CompileContext<'a>,
		start: usize,
	) -> Result<(usize, Self), (Token<'a>, &'static [TokenKind])> {
		let allocate_vars = |value: Word, scope: &mut FunctionScope<'a>| {
			if value.leading_zeros() + value.trailing_zeros() < Word::BITS / 2 {
				let [f, s] = scope.alloc_and_use(&[(None, VarType::Int); 2]);
				(f, Some(s))
			} else {
				let [v] = scope.alloc_and_use(&[(None, VarType::Int)]);
				(v, None)
			}
		};

		use TokenKind::*;
		if context.tokens.len() > start {
			let op = context.tokens[start].kind;
			if op == Integer {
				let value = context.tokens[start].text.parse().unwrap();
				return Ok((
					start + 1,
					Self {
						value,
						output: allocate_vars(value, &mut context.scope),
					},
				));
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
						return Ok((
							start + i + 2,
							Self {
								value,
								output: allocate_vars(value, &mut context.scope),
							},
						));
					} else {
						return Err((context.tokens[start + i + 1], &[Integer]));
					}
				}
			}
		}
		Err((context.tokens[start], &[Integer]))
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
	}
}
