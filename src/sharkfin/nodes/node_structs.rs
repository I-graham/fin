use super::{CompileContext, RootNode};
use crate::bytecode::*;
use crate::sharkfin::lexer::*;
use crate::sharkfin::vars::*;

#[derive(Debug)]
pub struct ProgramRoot<'a>(pub(super) RootNode<'a>);

#[derive(Debug)]
pub(super) struct StatementList<'a> {
	pub statements: Vec<Statement<'a>>,
}

#[derive(Debug)]
pub(super) struct CodeBlock<'a> {
	pub code: Vec<Statement<'a>>,
}

#[derive(Debug)]
pub(super) enum Statement<'a> {
	LetVar(LetVar<'a>),
	MutateVar(MutateVar<'a>),
	Empty,
}

#[derive(Debug)]
pub(super) struct LetVar<'a> {
	pub mut_var : MutateVar<'a>,
}

#[derive(Debug)]
pub(super) struct MutateVar<'a> {
	pub access: AccessVar<'a>,
	pub val: Sum<'a>,
}

#[derive(Debug)]
pub(super) struct Sum<'a> {
	pub addends: Vec<(Multiplication<'a>, Option<Token<'a>>)>,
}

#[derive(Debug)]
pub(super) struct Multiplication<'a> {
	pub factors: Vec<(Factor<'a>, Option<Token<'a>>)>,
}

#[derive(Debug)]
pub(super) enum Factor<'a> {
	Var(AccessVar<'a>),
	Literal(LiteralInt<'a>),
	Parenthesized(Sum<'a>),
}

#[derive(Debug)]
pub(super) struct AccessVar<'a> {
	pub var: Token<'a>,
}

#[derive(Debug)]
pub(super) struct LiteralInt<'a> {
	pub value: Word,
	pub output: (VariableID<'a>, Option<VariableID<'a>>),
}

impl<'a> LiteralInt<'a> {
	pub(super) fn from_const(value: Word, context: &mut CompileContext<'a>) -> Self {
		Self {
			value,
			output: if value.leading_zeros() + value.trailing_zeros() >= Word::BITS / 2 {
				let [v] = context.scope.allocate_var(&[(None, VarType::Int)]);
				(v, None)
			} else {
				let [f, s] = context.scope.allocate_var(&[(None, VarType::Int); 2]);
				(f, Some(s))
			},
		}
	}
}
