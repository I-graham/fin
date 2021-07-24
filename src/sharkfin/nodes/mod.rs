mod node_impls;
mod node_structs;
pub(crate) use node_structs::ProgramRoot;

use super::lexer::*;
use super::vars::*;
use super::CompileContext;
use crate::bytecode::*;
use std::fmt::Debug;

pub(super) trait ASTNode<'a>: Debug + Sized {
	fn precompute(&self) -> Option<Word> {
		None
	}

	fn output_var(&self) -> Option<VariableID<'a>> {
		None
	}

	fn output_type(&self, context: &CompileContext) -> Option<VarType> {
		self.output_var()
			.map(|id| context.scope.var_type(id).unwrap())
	}

	fn construct(
		context: &mut CompileContext<'a>,
		start: usize,
	) -> Result<(usize, Self), (Token<'a>, &'static [TokenKind])>;

	fn record_var_usage(&mut self, context: &mut CompileContext<'a>) -> Option<VariableID<'a>>;

	fn generate_source(&self, context: &mut CompileContext<'a>);
}
