use crate::bytecode::{FWord, Word};

pub(crate) struct Lexer<'a> {
	pub(super) source: &'a str,
	pub(super) cursor: usize,
	pub(super) line: usize,
}

impl<'a> Lexer<'a> {
	pub(crate) fn new(source: &'a str) -> Self {
		Self {
			source,
			cursor: 0,
			line: 1,
		}
	}

	pub(crate) fn advance_token(&mut self) -> Result<Token<'a>, String> {
		use TokenKind::*;
		//Tokens with an exact one-to-one correspondance with strings,
		//Ex. Semicolon <=> ";", Div <=> "/", etc...
		const EXACT_TOKENS: &[TokenKind] = &[
			Semicolon,
			Comma,
			Return,
			ReturnArrow,
			Plus,
			Minus,
			Mod,
			Mul,
			Div,
			LParen,
			RParen,
			LCurly,
			RCurly,
			NEq,
			Eq,
			Or,
			And,
			Not,
			GreaterEq,
			Greater,
			LessEq,
			Less,
			Assign,
			Let,
			Func,
			Else,
			If,
			While,
		];

		for &kind in EXACT_TOKENS {
			let text = kind.as_str();
			if self.remaining().starts_with(text) {
				let text = self.advance_source(text);
				return Ok(self.create_token(kind, text));
			}
		}

		if let Some(next) = self.remaining().chars().next() {
			match next {
				c if !c.is_ascii() => Err("Non-ASCII character encountered!".into()),
				d if d.is_digit(10) => {
					let mut decimal_encountered = false;
					let text = self.advance_until_char(|c| {
						if !decimal_encountered && c == '.' {
							decimal_encountered = true;
							false
						} else {
							!c.is_digit(10)
						}
					});
					if decimal_encountered {
						use std::str::FromStr;
						match FWord::from_str(text) {
							Ok(_) => Ok(self.create_token(TokenKind::Float, text)),
							Err(err) => Err(format!("{:?}", err)),
						}
					} else {
						match Word::from_str_radix(text, 10) {
							Ok(_) => Ok(self.create_token(TokenKind::Integer, text)),
							Err(err) => Err(format!("{:?}", err)),
						}
					}
				}
				c if c.is_alphabetic() && c.is_lowercase() => {
					let text = self.advance_until_char(|c| !c.is_alphanumeric());
					Ok(self.create_token(TokenKind::Ident, text))
				}
				c if c.is_alphabetic() && c.is_uppercase() => {
					let text = self.advance_until_char(|c| !c.is_alphanumeric());
					Ok(self.create_token(TokenKind::Type, text))
				}
				c if c.is_whitespace() => {
					let text = self.advance_until_char(|c| !c.is_whitespace());
					Ok(self.create_token(TokenKind::Whitespace, text))
				}
				_ => Err("Unknown character".into()),
			}
		} else {
			Ok(self.create_token(TokenKind::Eof, &self.source[0..0]))
		}
	}

	fn advance_source(&mut self, start: &str) -> &'a str {
		debug_assert!(self.remaining().starts_with(start));
		self.line += start.split('\n').count() - 1;
		let ret = &self.remaining()[..start.len()];
		self.cursor += start.len();
		ret
	}

	fn advance_until_char<F: FnMut(char) -> bool>(&mut self, f: F) -> &'a str {
		let index = self
			.remaining()
			.find(f)
			.unwrap_or_else(|| self.remaining().len());
		let ret = &self.source[self.cursor..(index + self.cursor)];
		self.cursor += index;
		self.line += ret.split('\n').count() - 1;
		ret
	}

	fn remaining(&self) -> &'a str {
		&self.source[self.cursor..]
	}

	fn create_token(&self, kind: TokenKind, text: &'a str) -> Token<'a> {
		Token {
			kind,
			text,
			line: self.line,
		}
	}
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct Token<'a> {
	pub(crate) kind: TokenKind,
	pub(crate) text: &'a str,
	pub(crate) line: usize,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum TokenKind {
	Ident,
	Type,
	Integer,
	Float,
	Whitespace,
	Eof,
	ReturnArrow,
	Assign,
	Plus,
	Minus,
	Mod,
	Mul,
	Div,
	Less,
	Greater,
	LessEq,
	GreaterEq,
	NEq,
	Eq,
	Or,
	And,
	Not,
	Comma,
	LParen,
	RParen,
	LCurly,
	RCurly,
	Else,
	If,
	While,
	Func,
	Let,
	Return,
	Semicolon,
}

impl TokenKind {
	pub(crate) fn as_str(&self) -> &'static str {
		use TokenKind::*;
		match self {
			Ident => "{identifier}",
			Type => "{Type}",
			Integer => "{integer}",
			Float => "{float}",
			Whitespace => "{whitespace}",
			Eof => "{end of file}",
			ReturnArrow => "->",
			Assign => "=",
			Plus => "+",
			Minus => "-",
			Mod => "%",
			Mul => "*",
			Div => "/",
			Less => "<",
			Greater => ">",
			LessEq => "<=",
			GreaterEq => ">=",
			NEq => "!=",
			Eq => "==",
			Or => "||",
			And => "&&",
			Not => "!",
			Comma => ",",
			LParen => "(",
			RParen => ")",
			LCurly => "{",
			RCurly => "}",
			Else => "else",
			If => "if",
			While => "while",
			Func => "func",
			Let => "let",
			Return => "return",
			Semicolon => ";",
		}
	}
}
