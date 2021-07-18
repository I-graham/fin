use super::*;

pub(crate) type LogExpr<'a> = OrExpr<'a>;

#[derive(Debug)]
pub(crate) struct OrExpr<'a> {
	pub ands: Vec<AndExpr<'a>>,
}

#[derive(Debug)]
pub(crate) struct AndExpr<'a> {
	pub bools: Vec<Boolean<'a>>,
}

#[derive(Debug)]
pub(crate) enum Boolean<'a> {
	Comparison(Comparison<'a>),
}

#[derive(Debug)]
pub(crate) struct Comparison<'a> {
	pub args: (Expr<'a>, Expr<'a>),
	pub op: Token<'a>,
}

impl<'a> Comparison<'a> {
	pub(crate) fn cond(&self) -> Condition {
		use Condition::*;
		use TokenKind::*;
		match self.op.kind {
			Less => Ls,
			Greater => Gr,
			LessEq => LsEq,
			GreaterEq => GrEq,
			TokenKind::Eq => Condition::Eq,
			_ => unreachable!(),
		}
	}
}
