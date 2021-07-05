use super::lexer::{Token, TokenKind};
use std::ops::Range;
use std::process::exit;

fn get_pos_in(full: &str, substr: &str) -> usize {
	let src_ptr = full.as_ptr() as usize;
	let sub_ptr = substr.as_ptr() as usize;
	debug_assert!(src_ptr <= sub_ptr);
	let pos = sub_ptr - src_ptr;
	debug_assert!(pos < full.len());
	pos
}

pub(super) struct Error<'a> {
	source: &'a str,
}

impl<'a> Error<'a> {
	pub(super) fn new(source: &'a str) -> Self {
		Self { source }
	}

	pub(super) fn err_at_char(&self, msg: &str, cursor_pos: usize, line_num: usize) -> ! {
		self.display_err(msg, "", cursor_pos..cursor_pos + 1, line_num)
	}

	pub(super) fn err_at_token(&self, msg: &str, token: Token) -> ! {
		self.err_at_substr(msg, token.text, token.line);
	}

	pub(super) fn suggest_at_token(&self, token: Token, suggestions: &[TokenKind]) -> ! {
		debug_assert!(!suggestions.is_empty());
		let mut help = "Perhaps you meant to use one of the following: ".to_string();
		for string in suggestions
			.iter()
			.take(suggestions.len() - 1)
			.map(|kind| kind.as_str())
		{
			help += &format!("`{}`, ", string);
		}
		if suggestions.len() > 1 {
			help += "or ";
		}
		help += &format!("`{}`.", suggestions.last().unwrap().as_str());

		let pos = get_pos_in(self.source, token.text);
		self.display_err(
			&format!("Encountered unexpected `{}`", token.kind.as_str()),
			&help,
			pos..(pos + token.text.len()),
			token.line,
		)
	}

	pub(super) fn err_at_substr(&self, msg: &str, substr: &str, line_num: usize) -> ! {
		let pos = get_pos_in(self.source, substr);
		self.display_err(msg, "", pos..(pos + substr.len()), line_num)
	}

	fn display_err(
		&self,
		error_message: &str,
		help_msg: &str,
		range: Range<usize>,
		line_num: usize,
	) -> ! {
		debug_assert!(range.start < self.source.len());
		let full_line = self.get_full_line(range.start);
		let col = range.start - get_pos_in(self.source, full_line);
		let line_num_width = (line_num as f64).log10().round() as usize + 1;
		println!(
			"\nerror (line {1}): {err_msg}\n\
			\t{0:<width$}| \n\
			\t{1:<width$}| {2}\n\
			\t{0:<width$}| {0:>col$}{0:^>arrowlen$} {err_msg}\
			\nhelp: {help_msg}",
			"",
			line_num,
			full_line.replace('\t', " "),
			width = line_num_width,
			col = col,
			arrowlen = range.end - range.start,
			err_msg = error_message,
			help_msg = help_msg,
		);
		exit(0)
	}

	fn get_full_line(&self, pos: usize) -> &str {
		debug_assert!(pos < self.source.len());
		debug_assert!(self.source.is_char_boundary(pos));
		let start = self.source[..pos].rfind('\n').map(|l| l + 1).unwrap_or(0);
		let end = self.source[pos..]
			.find('\n')
			.map(|l| l + pos)
			.unwrap_or(self.source.len());
		&self.source[start..end]
	}
}
