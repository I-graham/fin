use crate::bytecode::*;
use crate::sharkfin::vars::*;

pub(super) type RootNode<'a> = CodeBlock<'a>;

#[derive(Debug)]
pub struct ProgramRoot<'a>(pub(super) RootNode<'a>);

#[derive(Debug)]
pub(super) struct StatementList<'a> {
	pub(super) statements: Vec<Statement<'a>>,
}

#[derive(Debug)]
pub(super) struct CodeBlock<'a> {
	pub(super) code: Vec<Statement<'a>>,
}

#[derive(Debug)]
pub(super) enum Statement<'a> {
	LetVar(LetVar<'a>),
}

#[derive(Debug)]
pub(super) struct LetVar<'a> {
	pub(super) init: Sum<'a>,
	pub(super) variable: VariableID<'a>,
}

#[derive(Debug)]
pub(super) struct Sum<'a> {
	pub(super) addends: Vec<(Multiplication<'a>, bool)>,
}

#[derive(Debug)]
pub(super) struct Multiplication<'a> {
	pub(super) factors: Vec<(Factor<'a>, bool)>,
}

#[derive(Debug)]
pub(super) enum Factor<'a> {
	Var(AccessVar<'a>),
	Literal(LiteralInt<'a>),
	Parenthesized(Sum<'a>),
}

#[derive(Debug)]
pub(super) struct AccessVar<'a> {
	pub(super) variable: VariableID<'a>,
}

#[derive(Debug)]
pub(super) struct LiteralInt<'a> {
	pub(super) value: Word,
	pub(super) output: (VariableID<'a>, Option<VariableID<'a>>),
}
