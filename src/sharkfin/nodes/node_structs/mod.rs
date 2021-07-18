mod int_exprs;
mod log_exprs;
pub(super) use int_exprs::*;
pub(super) use log_exprs::*;

use super::CompileContext;
use crate::bytecode::*;
use crate::sharkfin::lexer::*;
use crate::sharkfin::vars::*;
use fnv::FnvHashMap;

pub(crate) type RootNode<'a> = CodeBlock<'a>;

#[derive(Debug)]
pub(crate) struct ProgramRoot<'a>(pub(super) RootNode<'a>);

#[derive(Debug)]
pub(crate) struct StatementList<'a> {
	pub statements: Vec<Statement<'a>>,
}

#[derive(Debug)]
pub(crate) struct CodeBlock<'a> {
	pub code: Vec<Statement<'a>>,
	pub recorder: Option<FnvHashMap<VariableID<'a>, bool>>,
}

#[derive(Debug)]
pub(crate) enum Statement<'a> {
	LetVar(LetVar<'a>),
	MutateVar(MutateVar<'a>),
	CodeBlock(CodeBlock<'a>),
	If(IfStatement<'a>),
	While(WhileLoop<'a>),
	Empty,
}

#[derive(Debug)]
pub(crate) struct WhileLoop<'a> {
	pub cond: LogExpr<'a>,
	pub code: CodeBlock<'a>,
	pub vars: Option<FnvHashMap<VariableID<'a>, bool>>,
}

#[derive(Debug)]
pub(crate) struct IfStatement<'a> {
	pub cond: LogExpr<'a>,
	pub code: CodeBlock<'a>,
}

#[derive(Debug)]
pub(crate) struct LetVar<'a> {
	pub mut_var: MutateVar<'a>,
}

#[derive(Debug)]
pub(crate) struct MutateVar<'a> {
	pub access: AccessVar<'a>,
	pub val: Sum<'a>,
}

#[derive(Debug)]
pub(crate) struct AccessVar<'a> {
	pub var: Token<'a>,
}
