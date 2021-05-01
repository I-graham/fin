use super::lexer::*;
use crate::bytecode::*;
use std::any::Any;
use std::fmt::Debug;
use std::ops::Range;

pub(super) fn compile<T: ASTNode + ?Sized>(node: &T, free: &Range<u8>, out: &mut Vec<Instruction>) {
	debug_assert!(free.start < free.end);
	let clear = node.register_clears_needed(&(0..16));
	let noclear = node.register_clears_needed(free);
	if noclear.1 || clear.0 + 1 < noclear.0 {
		out.push(Instruction {
			condition: Condition::Al,
			mnemonic: Mnemonic::PushAll,
			args: [0, 15, 0, 0, 0, 0],
		});
		node.generate_source(&(0..16), out);
		out.push(Instruction {
			condition: Condition::Al,
			mnemonic: Mnemonic::PushAll,
			args: [0, 15, 0, 0, 0, 0],
		});
		out.push(Instruction {
			condition: Condition::Al,
			mnemonic: Mnemonic::Mov,
			args: [0, free.start, 0, 0, 0, 0],
		});
	} else {
		node.generate_source(free, out);
	}
}

pub(crate) trait ASTNode: Any + Debug {
	fn construct<'l>(input: &[Token<'l>]) -> Option<(usize, Self)>
	where
		Self: Sized;

	fn register_clears_needed(&self, free: &Range<u8>) -> (usize, bool);
	fn generate_source(&self, free: &Range<u8>, out: &mut Vec<Instruction>);
}

type RootNode = Sum;

#[derive(Debug)]
pub(crate) struct ProgramRoot(RootNode);

#[derive(Debug)]
struct LiteralInt {
	value: Word,
}

#[derive(Debug)]
struct Multiplication {
	factors: Vec<(Factor, bool)>,
}

#[derive(Debug)]
struct Sum {
	addends: Vec<(Multiplication, bool)>,
}

#[derive(Debug)]
enum Factor {
	Literal(LiteralInt),
	Parenthesized(Sum),
}

impl ASTNode for ProgramRoot {
	fn register_clears_needed(&self, free: &Range<u8>) -> (usize, bool) {
		self.0.register_clears_needed(free)
	}

	fn construct<'l>(input: &[Token<'l>]) -> Option<(usize, Self)> {
		match RootNode::construct(input) {
			Some((size, program)) => {
				//assert_eq!(size, input.len());
				Some((size, Self(program)))
			}
			None => None,
		}
	}

	fn generate_source(&self, free: &Range<u8>, out: &mut Vec<Instruction>) {
		compile(&self.0, free, out);
	}
}

impl ASTNode for LiteralInt {
	fn construct<'l>(input: &[Token<'l>]) -> Option<(usize, Self)> {
		if input.len() > 0 && input[0].0 == TokenKind::Integer {
			Some((
				1,
				Self {
					value: input[0].1.parse().unwrap(),
				},
			))
		} else if input.len() > 1 && input[0].0 == TokenKind::Minus {
			if let Some((size, literal)) = Self::construct(&input[1..]) {
				Some((
					size + 1,
					Self {
						value: literal.value.wrapping_neg(),
					},
				))
			} else {
				None
			}
		} else {
			None
		}
	}

	fn register_clears_needed(&self, free: &Range<u8>) -> (usize, bool) {
		let size = free.end - free.start;
		let regs_needed = if self.value.leading_zeros() + self.value.trailing_zeros() > 32 {
			2
		} else {
			1
		};
		((size < regs_needed) as usize, size < regs_needed)
	}

	fn generate_source(&self, free: &Range<u8>, out: &mut Vec<Instruction>) {
		if self.value.leading_zeros() + self.value.trailing_zeros() >= Word::MAX.count_ones() / 2 {
			let shift = self.value.trailing_zeros() as u8;
			let val = (self.value >> shift) as u32;
			let [a, b, c, d] = val.to_le_bytes();
			out.push(Instruction {
				condition: Condition::Al,
				mnemonic: Mnemonic::Const,
				args: [free.start, shift, a, b, c, d],
			});
		} else {
			let val = self.value as u32;
			let [a, b, c, d] = val.to_le_bytes();
			out.push(Instruction {
				condition: Condition::Al,
				mnemonic: Mnemonic::Const,
				args: [free.start, 0, a, b, c, d],
			});
			let shift = 32;
			let val = (self.value >> shift) as u32;
			let [a, b, c, d] = val.to_le_bytes();
			out.push(Instruction {
				condition: Condition::Al,
				mnemonic: Mnemonic::Const,
				args: [free.start + 1, shift, a, b, c, d],
			});
			out.push(Instruction {
				condition: Condition::Al,
				mnemonic: Mnemonic::Or,
				args: [free.start, free.start, free.start + 1, 0, 0, 0],
			});
		}
	}
}

impl ASTNode for Factor {
	fn construct<'l>(input: &[Token<'l>]) -> Option<(usize, Self)> {
		if let Some((size, lit)) = LiteralInt::construct(input) {
			Some((size, Self::Literal(lit)))
		} else if input.len() > 2 && input[0].0 == TokenKind::LParen {
			if let Some((size, sum)) = Sum::construct(&input[1..]) {
				if input[1+size].0 == TokenKind::RParen {
					Some((2+size, Self::Parenthesized(sum)))
				} else {
					None
				}
			} else {
				None
			}
		} else {
			None
		}
	}

	fn register_clears_needed(&self, free: &Range<u8>) -> (usize, bool) {
		match self {
			Self::Literal(lit) => lit.register_clears_needed(free),
			Self::Parenthesized(sum) => sum.register_clears_needed(free),
		}
	}

	fn generate_source(&self, free: &Range<u8>, out: &mut Vec<Instruction>) {
		match self {
			Self::Literal(lit) => lit.generate_source(free, out),
			Self::Parenthesized(sum) => sum.generate_source(free, out),
		}
	}
}

impl ASTNode for Multiplication {
	fn construct<'l>(input: &[Token<'l>]) -> Option<(usize, Self)> {
		match Factor::construct(input) {
			Some((mut size, factor)) => {
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
									Factor::construct(&input[size + 1..])
								{
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

	fn register_clears_needed(&self, free: &Range<u8>) -> (usize, bool) {
		let size = free.end - free.start;
		if size > 1 {
			(
				self.factors
					.iter()
					.enumerate()
					.map(|(id, addend)| {
						addend
							.0
							.register_clears_needed(&((id > 0) as u8 + free.start..free.end))
							.0
					})
					.sum::<usize>(),
				false,
			)
		} else {
			(
				self.factors
					.iter()
					.enumerate()
					.map(|(id, addend)| {
						addend.0.register_clears_needed(&((id > 0) as u8 + 0..16)).0
					})
					.sum::<usize>() + 1,
				true,
			)
		}
	}

	fn generate_source(&self, free: &Range<u8>, out: &mut Vec<Instruction>) {
		compile(&self.factors[0].0, free, out);
		if self.factors.len() > 1 {
			for addend in &self.factors[1..] {
				compile(&addend.0, &(1 + free.start..free.end), out);
				out.push(Instruction {
					condition: Condition::Al,
					mnemonic: if addend.1 {
						Mnemonic::Mul
					} else {
						Mnemonic::Div
					},
					args: [free.start, free.start, free.start + 1, 0, 0, 0],
				});
			}
		}
	}
}

impl ASTNode for Sum {
	fn construct<'l>(input: &[Token<'l>]) -> Option<(usize, Self)> {
		match Multiplication::construct(input) {
			Some((mut size, term)) => {
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
									Multiplication::construct(&input[size + 1..])
								{
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

	fn register_clears_needed(&self, free: &Range<u8>) -> (usize, bool) {
		let size = free.end - free.start;
		if size > 1 {
			(
				self.addends
					.iter()
					.enumerate()
					.map(|(id, addend)| {
						addend
							.0
							.register_clears_needed(&((id > 0) as u8 + free.start..free.end))
							.0
					})
					.sum::<usize>(),
				false,
			)
		} else {
			(
				self.addends
					.iter()
					.enumerate()
					.map(|(id, addend)| {
						addend.0.register_clears_needed(&((id > 0) as u8 + 0..16)).0
					})
					.sum::<usize>() + 1,
				true,
			)
		}
	}

	fn generate_source(&self, free: &Range<u8>, out: &mut Vec<Instruction>) {
		compile(&self.addends[0].0, free, out);
		if self.addends.len() > 1 {
			for addend in &self.addends[1..] {
				compile(&addend.0, &(1 + free.start..free.end), out);
				out.push(Instruction {
					condition: Condition::Al,
					mnemonic: if addend.1 {
						Mnemonic::Add
					} else {
						Mnemonic::Sub
					},
					args: [free.start, free.start, free.start + 1, 0, 0, 0],
				});
			}
		}
	}
}
