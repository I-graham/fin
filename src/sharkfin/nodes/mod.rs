mod node_impls;
mod node_structs;
pub use node_structs::*;

use super::lexer::*;
use super::vars::*;
use super::CompileContext;
use crate::bytecode::*;
use std::fmt::Debug;

type RootNode<'a> = CodeBlock<'a>;

pub(super) trait ASTNode<'a>: Debug + Sized {
	fn precompute(&self) -> Option<Word> {
		None
	}
	fn output_var(&self) -> Option<VariableID<'a>> {
		None
	}
	fn construct(
		context: &mut CompileContext<'a>,
		start: usize,
	) -> Result<(usize, Self), (Token<'a>, &'static [TokenKind])>;
	fn record_var_usage(&self, context: &mut CompileContext<'a>) -> Option<VariableID<'a>>;
	fn generate_source(&self, context: &mut CompileContext<'a>);
}
