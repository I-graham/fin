mod log_exprs;
mod math_exprs;
pub(super) use log_exprs::*;
pub(super) use math_exprs::*;

use super::CompileContext;
use crate::bytecode::*;
use crate::sharkfin::lexer::*;
use crate::sharkfin::types::*;
use crate::sharkfin::vars::*;

pub(crate) type RootNode<'a> = GlobalScope<'a>;

#[derive(Debug)]
pub(crate) struct ProgramRoot<'a>(pub(super) RootNode<'a>);

#[derive(Debug)]
pub(crate) struct GlobalScope<'a> {
	pub functions: Vec<GlobalStatement<'a>>,
}

#[derive(Debug)]
pub(crate) enum GlobalStatement<'a> {
	Declaration(FunctionHeader<'a>),
	Definition(Box<FunctionDefinition<'a>>),
}

#[derive(Debug)]
pub(crate) struct FunctionDefinition<'a> {
	pub header: FunctionHeader<'a>,
	pub body: CodeBlock<'a>,
	pub scope: FunctionScope<'a>,
}

#[derive(Clone, Debug)]
pub(crate) struct FunctionHeader<'a> {
	pub func_name: Token<'a>,
	//(Type, Variable name)
	pub args: Vec<(VarType, Token<'a>)>,
	pub return_type: Option<VarType>,
}

#[derive(Debug)]
pub(crate) struct TypeNode {
	pub ty: VarType,
}

#[derive(Debug)]
pub(crate) struct CodeBlock<'a> {
	pub code: Vec<Statement<'a>>,
	pub recorder: Option<Recorder<'a>>,
}

#[derive(Debug)]
pub(crate) enum Statement<'a> {
	Empty,
	LetVar(LetVar<'a>),
	MutateVar(MutateVar<'a>),
	CodeBlock(CodeBlock<'a>),
	If(IfStatement<'a>),
	While(WhileLoop<'a>),
	FunctionCall(FunctionCall<'a>),
	Return(MathExpr<'a>),
}

#[derive(Debug)]
pub(crate) struct FunctionCall<'a> {
	pub func_name: Token<'a>,
	pub args: Vec<MathExpr<'a>>,
	pub opvar: Option<VariableID<'a>>,
}

#[derive(Debug)]
pub(crate) struct WhileLoop<'a> {
	pub cond: LogExpr<'a>,
	pub code: CodeBlock<'a>,
	pub vars: Option<Recorder<'a>>,
}

#[derive(Debug)]
pub(crate) struct IfStatement<'a> {
	pub cond_code: Vec<(LogExpr<'a>, CodeBlock<'a>)>,
	pub else_code: Option<CodeBlock<'a>>,
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
