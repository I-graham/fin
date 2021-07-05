use crate::bytecode::*;
use crate::sharkfin::vars::*;
use super::RootNode;

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
}

#[derive(Debug)]
pub(super) struct LetVar<'a> {
	pub init: Sum<'a>,
	pub variable: VariableID<'a>,
}

#[derive(Debug)]
pub(super) struct Sum<'a> {
	pub addends: Vec<(Multiplication<'a>, bool)>,
}

#[derive(Debug)]
pub(super) struct Multiplication<'a> {
	pub factors: Vec<(Factor<'a>, bool)>,
}

#[derive(Debug)]
pub(super) enum Factor<'a> {
	Var(AccessVar<'a>),
	Literal(LiteralInt<'a>),
	Parenthesized(Sum<'a>),
}

#[derive(Debug)]
pub(super) struct AccessVar<'a> {
	pub variable: VariableID<'a>,
}

#[derive(Debug)]
pub(super) struct LiteralInt<'a> {
	pub value: Word,
	pub output: (VariableID<'a>, Option<VariableID<'a>>),
}
