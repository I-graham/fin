mod node_impls;
mod node_structs;
pub use node_structs::ProgramRoot;

use super::lexer::*;
use super::vars::*;
use crate::bytecode::*;
use std::fmt::Debug;

pub(super) trait ASTNode<'a>: Debug + Sized {
	fn output_var(&self) -> Option<VariableID<'a>> {
		None
	}
	fn construct(input: &[Token<'a>], scope: &mut FunctionScope<'a>) -> Option<(usize, Self)>;
	fn generate_source(&self, scope: &mut FunctionScope<'a>, out: &mut Vec<Instruction>);
}
