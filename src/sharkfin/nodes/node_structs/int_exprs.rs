use super::*;
use crate::abbreviations;

pub(crate) type Expr<'a> = Sum<'a>;

#[derive(Debug)]
pub(crate) struct Sum<'a> {
    pub addends: Vec<(Multiplication<'a>, Option<Token<'a>>)>,
}

#[derive(Debug)]
pub(crate) struct Multiplication<'a> {
    pub factors: Vec<(Factor<'a>, Option<Token<'a>>)>,
}

#[derive(Debug)]
pub(crate) enum Factor<'a> {
    Var(AccessVar<'a>),
    Literal(LiteralInt<'a>),
    Parenthesized(Expr<'a>),
}

#[derive(Debug)]
pub(crate) struct LiteralInt<'a> {
    pub value: Word,
    pub output: (VariableID<'a>, Option<VariableID<'a>>),
}

impl<'a> LiteralInt<'a> {
    pub(in super::super) fn from_const(value: Word, context: &mut CompileContext<'a>) -> Self {
        Self {
            value,
            output: if abbreviations::fits_in_const_inst(value) {
                let [v] = context.scope.allocate_var(&[(None, VarType::Int)]);
                (v, None)
            } else {
                let [f, s] = context.scope.allocate_var(&[(None, VarType::Int); 2]);
                (f, Some(s))
            },
        }
    }
}
