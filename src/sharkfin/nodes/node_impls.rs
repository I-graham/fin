use super::node_structs::*;
use super::*;
use crate::sharkfin::abbreviations;

fn tokens_match_kinds<I: IntoIterator<Item = TokenKind>>(inputs: &[Token], kinds: I) -> bool {
	for (i, kind) in kinds.into_iter().enumerate() {
		if i >= inputs.len() || inputs[i].0 != kind {
			return false;
		}
	}
	true
}

impl<'a> ASTNode<'a> for ProgramRoot<'a> {
	fn construct(input: &[Token<'a>], scope: &mut FunctionScope<'a>) -> Option<(usize, Self)> {
		match RootNode::construct(input, scope) {
			Some((size, program)) => {
				assert_eq!(size, input.len());
				Some((size, Self(program)))
			}
			None => None,
		}
	}

	fn generate_source(&self, scope: &mut FunctionScope<'a>, out: &mut Vec<Instruction>) {
		self.0.generate_source(scope, out);
		out.push(Instruction {
			condition: Condition::Al,
			mnemonic: Mnemonic::Dbg,
			args: [0; 6],
		});
	}
}

impl<'a> ASTNode<'a> for CodeBlock<'a> {
	fn construct(input: &[Token<'a>], scope: &mut FunctionScope<'a>) -> Option<(usize, Self)> {
		if let Some((mut size, first)) = Statement::construct(input, scope) {
			let mut ret = Self { code: vec![first] };
			while let Some((len, next)) = Statement::construct(&input[size..], scope) {
				size += len;
				ret.code.push(next);
			}
			Some((size, ret))
		} else {
			None
		}
	}

	fn generate_source(&self, scope: &mut FunctionScope<'a>, out: &mut Vec<Instruction>) {
		for statement in &self.code {
			statement.generate_source(scope, out);
		}
	}
}

impl<'a> ASTNode<'a> for Statement<'a> {
	fn construct(input: &[Token<'a>], scope: &mut FunctionScope<'a>) -> Option<(usize, Self)> {
		if let Some((size, letvar)) = LetVar::construct(input, scope) {
			Some((size, Self::LetVar(letvar)))
		} else {
			None
		}
	}

	fn generate_source(&self, scope: &mut FunctionScope<'a>, out: &mut Vec<Instruction>) {
		match self {
			Self::LetVar(letvar) => letvar.generate_source(scope, out),
		}
	}
}

impl<'a> ASTNode<'a> for LetVar<'a> {
	fn construct(input: &[Token<'a>], scope: &mut FunctionScope<'a>) -> Option<(usize, Self)> {
		use TokenKind::*;
		if tokens_match_kinds(input, [Let, Type, Ident, Assign]) {
			if let Some((size, sum)) = Sum::construct(&input[4..], scope) {
				if input.len() > 4 + size && input[4 + size].0 == Semicolon {
					let variable =
						scope.create_synonym(Some(input[2].1), sum.output_var().unwrap());
					return Some((
						5 + size,
						Self {
							init: sum,
							variable,
						},
					));
				}
			}
		}
		None
	}

	fn generate_source(&self, scope: &mut FunctionScope<'a>, out: &mut Vec<Instruction>) {
		self.init.generate_source(scope, out);
		if let VariableID::Named(name) = self.variable {
			println!(
				"Var `{}` stored in reg #{:?} at inst #{}.",
				name,
				&scope.register(&[self.variable], out),
				out.len()
			);
		} else {
			unreachable!();
		}

		out.push(Instruction {
			condition: Condition::Al,
			mnemonic: Mnemonic::Dbg,
			args: [0; 6],
		});
	}
}

impl<'a> ASTNode<'a> for Sum<'a> {
	fn construct(input: &[Token<'a>], scope: &mut FunctionScope<'a>) -> Option<(usize, Self)> {
		match Multiplication::construct(input, scope) {
			Some((mut size, term)) => {
				let acc = term.output_var().unwrap();
				scope.record_usage(&[acc]);
				let mut ret = Self {
					addends: vec![(term, true)],
				};
				let term = loop {
					if input.len() <= size + 1 {
						break ret;
					} else {
						match input[size].0 {
							TokenKind::Plus | TokenKind::Minus => {
								if let Some((term2size, term2)) =
									Multiplication::construct(&input[size + 1..], scope)
								{
									scope.record_usage(&[acc]);
									scope.record_usage(&[term2.output_var().unwrap()]);
									ret.addends.push((term2, input[size].0 == TokenKind::Plus));
									size += term2size + 1;
								} else {
									break ret;
								}
							}
							_ => break ret,
						}
					}
				};
				Some((size, term))
			}
			None => None,
		}
	}

	fn output_var(&self) -> Option<VariableID<'a>> {
		self.addends[0].0.output_var()
	}

	fn generate_source(&self, scope: &mut FunctionScope<'a>, out: &mut Vec<Instruction>) {
		self.addends[0].0.generate_source(scope, out);
		let opvar = self.output_var().unwrap();
		for addend in &self.addends[1..] {
			addend.0.generate_source(scope, out);
			let next_opvar = addend.0.output_var().unwrap();
			let [acc, data] = scope.register(&[opvar, next_opvar], out);
			let inst = Instruction {
				condition: Condition::Al,
				mnemonic: if addend.1 {
					Mnemonic::Add
				} else {
					Mnemonic::Sub
				},
				args: [acc, acc, data, 0, 0, 0],
			};
			out.push(inst);
		}
	}
}

impl<'a> ASTNode<'a> for Multiplication<'a> {
	fn construct(input: &[Token<'a>], scope: &mut FunctionScope<'a>) -> Option<(usize, Self)> {
		match Factor::construct(input, scope) {
			Some((mut size, factor)) => {
				let acc = factor.output_var().unwrap();
				scope.record_usage(&[acc]);
				let mut ret = Multiplication {
					factors: vec![(factor, true)],
				};
				let term = loop {
					if input.len() <= size + 1 {
						break ret;
					} else {
						match input[size].0 {
							TokenKind::Mul | TokenKind::Div => {
								if let Some((factor2size, factor2)) =
									Factor::construct(&input[size + 1..], scope)
								{
									scope.record_usage(&[acc]);
									scope.record_usage(&[factor2.output_var().unwrap()]);
									ret.factors.push((factor2, input[size].0 == TokenKind::Mul));
									size += factor2size + 1;
								} else {
									break ret;
								}
							}
							_ => break ret,
						}
					}
				};
				Some((size, term))
			}
			None => None,
		}
	}

	fn output_var(&self) -> Option<VariableID<'a>> {
		self.factors[0].0.output_var()
	}

	fn generate_source(&self, scope: &mut FunctionScope<'a>, out: &mut Vec<Instruction>) {
		self.factors[0].0.generate_source(scope, out);
		let opvar = self.output_var().unwrap();
		for factor in &self.factors[1..] {
			factor.0.generate_source(scope, out);
			let next_opvar = factor.0.output_var().unwrap();
			let [acc, data] = scope.register(&[opvar, next_opvar], out);
			let inst = Instruction {
				condition: Condition::Al,
				mnemonic: if factor.1 {
					Mnemonic::Mul
				} else {
					Mnemonic::Div
				},
				args: [acc, acc, data, 0, 0, 0],
			};
			out.push(inst);
		}
	}
}

impl<'a> ASTNode<'a> for Factor<'a> {
	fn construct(input: &[Token<'a>], scope: &mut FunctionScope<'a>) -> Option<(usize, Self)> {
		if let Some((size, lit)) = LiteralInt::construct(input, scope) {
			scope.record_usage(&[lit.output_var().unwrap()]);
			return Some((size, Self::Literal(lit)));
		} else if let Some((size, var)) = AccessVar::construct(input, scope) {
			scope.record_usage(&[var.output_var().unwrap()]);
			return Some((size, Self::Var(var)));
		} else if input.len() > 2 && input[0].0 == TokenKind::LParen {
			if let Some((size, sum)) = Sum::construct(&input[1..], scope) {
				if input[1 + size].0 == TokenKind::RParen {
					scope.record_usage(&[sum.output_var().unwrap()]);
					return Some((2 + size, Self::Parenthesized(sum)));
				}
			}
		}
		None
	}

	fn output_var(&self) -> Option<VariableID<'a>> {
		match self {
			Self::Var(var) => var.output_var(),
			Self::Literal(lit) => lit.output_var(),
			Self::Parenthesized(sum) => sum.output_var(),
		}
	}

	fn generate_source(&self, scope: &mut FunctionScope<'a>, out: &mut Vec<Instruction>) {
		match self {
			Self::Var(var) => var.generate_source(scope, out),
			Self::Literal(lit) => lit.generate_source(scope, out),
			Self::Parenthesized(sum) => sum.generate_source(scope, out),
		}
	}
}

impl<'a> ASTNode<'a> for AccessVar<'a> {
	fn construct(input: &[Token<'a>], scope: &mut FunctionScope<'a>) -> Option<(usize, Self)> {
		if !input.is_empty() && input[0].0 == TokenKind::Ident {
			Some((
				1,
				Self {
					variable: scope.get_var(input[0].1).unwrap(),
				},
			))
		} else {
			None
		}
	}

	fn output_var(&self) -> Option<VariableID<'a>> {
		Some(self.variable)
	}

	fn generate_source(&self, _scope: &mut FunctionScope<'a>, _out: &mut Vec<Instruction>) {}
}

impl<'a> ASTNode<'a> for LiteralInt<'a> {
	fn construct(input: &[Token<'a>], scope: &mut FunctionScope<'a>) -> Option<(usize, Self)> {
		let mut allocate_vars = |value: Word| {
			if value.leading_zeros() + value.trailing_zeros() < Word::BITS / 2 {
				let [f, s] = scope.alloc_and_use(&[(None, VarType::Int); 2]);
				(f, Some(s))
			} else {
				let [v] = scope.alloc_and_use(&[(None, VarType::Int)]);
				(v, None)
			}
		};

		if !input.is_empty() && input[0].0 == TokenKind::Integer {
			let value = input[0].1.parse().unwrap();
			return Some((
				1,
				Self {
					value,
					output: allocate_vars(value),
				},
			));
		} else if input.len() > 1 && input[0].0 == TokenKind::Minus {
			let mut negate = false;
			for (i, token) in input.iter().enumerate() {
				if token.0 == TokenKind::Minus {
					negate = !negate;
				} else if token.0 == TokenKind::Integer {
					let literal: Word = token.1.parse().unwrap();
					let value = if negate {
						literal.wrapping_neg()
					} else {
						literal
					};
					return Some((
						i + 1,
						Self {
							value,
							output: allocate_vars(value),
						},
					));
				}
			}
		}
		None
	}

	fn output_var(&self) -> Option<VariableID<'a>> {
		Some(self.output.0)
	}

	fn generate_source(&self, scope: &mut FunctionScope<'a>, out: &mut Vec<Instruction>) {
		if self.value.leading_zeros() + self.value.trailing_zeros() >= Word::BITS / 2 {
			let [start] = scope.register(&[self.output.0], out);
			abbreviations::load_const(self.value, start, None, out);
		} else {
			let scratch_var = self.output.1.unwrap();
			let [start, scratch] = scope.register(&[self.output.0, scratch_var], out);
			abbreviations::load_const(self.value, start, Some(scratch), out);
		}
	}
}
