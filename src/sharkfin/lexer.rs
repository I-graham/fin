pub(crate) struct Lexer<'a> {
	source: &'a str,
	cursor: usize,
}

impl<'a> Lexer<'a> {
	pub(crate) fn new(source: &'a str) -> Self {
		Self { source, cursor: 0 }
	}

	pub(crate) fn advance_token(&mut self) -> Option<Token<'a>> {
		if let Some(next_char) = self.remaining().chars().next() {
			Some(match next_char {
				'+' => Token(TokenKind::Plus, self.advance_source("+")),
				'-' => Token(TokenKind::Minus, self.advance_source("-")),
				'*' => Token(TokenKind::Mul, self.advance_source("*")),
				'/' => Token(TokenKind::Div, self.advance_source("/")),
				'(' => Token(TokenKind::LParen, self.advance_source("(")),
				')' => Token(TokenKind::RParen, self.advance_source(")")),
				'p' if self.source.starts_with("proc") => {
					Token(TokenKind::Proc, self.advance_source("proc"))
				}
				d if d.is_digit(10) => {
					let num_str = self.advance_until_char(|d| !d.is_digit(10));
					match u64::from_str_radix(num_str, 10) {
						Ok(_) => Token(TokenKind::Integer, num_str),
						Err(err) => self.syntax_error(err),
					}
				}
				c if c.is_whitespace() => Token(
					TokenKind::Whitespace,
					self.advance_until_char(|c| !c.is_whitespace()),
				),
				_ => self.syntax_error(format!("Unexpected character '{}' encountered", next_char)),
			})
		} else {
			None
		}
	}

	fn advance_source(&mut self, start: &str) -> &'a str {
		debug_assert!(self.remaining().starts_with(start));
		let ret = &self.source[self.cursor..(start.len() + self.cursor)];
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
		ret
	}

	fn remaining(&self) -> &'a str {
		&self.source[self.cursor..]
	}

	fn syntax_error<T: ToString>(&self, message: T) -> ! {
		use std::panic;

		let lines = self.source.lines().count();
		let line_start = self.source[..self.cursor]
			.rfind('\n')
			.map(|p| p + 1)
			.unwrap_or(0);
		let line_end = self
			.remaining()
			.find('\n')
			.unwrap_or_else(|| self.remaining().len())
			+ self.cursor;

		println!(
			"Error at {line_num}:{col}!\n\t{err}\n\t{line_text}\n\t{pointer:>col$}",
			err = message.to_string(),
			line_num = lines,
			col = line_end - line_start - 1,
			line_text = &self.source[line_start..line_end],
			pointer = '^',
		);

		panic::set_hook(Box::new(
			|_info| { /* Remove Rust's custom error message. */ },
		));
		panic!()
	}
}

#[derive(Debug)]
pub(crate) struct Token<'l>(pub(crate) TokenKind, pub(crate) &'l str);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum TokenKind {
	Integer,
	Plus,
	Minus,
	Mul,
	Div,
	LParen,
	RParen,
	Whitespace,
	Proc,
}

impl<'a> Iterator for Lexer<'a> {
	type Item = Token<'a>;
	fn next(&mut self) -> Option<Self::Item> {
		self.advance_token()
	}
}
