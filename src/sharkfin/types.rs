use super::*;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum VarType {
	Int,
	Float,
}

#[derive(Default)]
pub struct TypeReader;

impl TypeReader {
	pub(crate) fn named_type(&self, type_token: Token<'_>) -> Option<VarType> {
		debug_assert!(type_token.kind == TokenKind::Type);
		use VarType::*;
		let type_name = type_token.text;
		const BUILTINS: &[(&str, VarType)] = &[("Int", Int), ("Float", Float)];
		BUILTINS
			.iter()
			.find_map(|(name, ty)| Some(*ty).filter(|_| type_name == *name))
	}
}
