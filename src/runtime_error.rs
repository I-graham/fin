use super::bytecode::*;
use std::ops::Range;
use std::process::exit;

pub(super) fn get_pos_in(full: &str, substr: &str) -> usize {
	let src_ptr = full.as_ptr() as usize;
	let sub_ptr = substr.as_ptr() as usize;
	debug_assert!(src_ptr <= sub_ptr);
	let pos = sub_ptr - src_ptr;
	debug_assert!(pos < full.len());
	pos
}

pub(super) fn get_full_line(source: &str, pos: usize) -> &str {
	debug_assert!(pos < source.len());
	debug_assert!(source.is_char_boundary(pos));
	let start = source[..pos].rfind('\n').map(|l| l + 1).unwrap_or(0);
	let end = source[pos..]
		.find('\n')
		.map(|l| l + pos)
		.unwrap_or(source.len());
	&source[start..end]
}

pub(super) fn throw_error(msg: &str, ip: usize, inst: &Instruction) -> ! {
	let width = (ip as f64).log10().round() as usize + 1;
	println!(
		"\nerror (line {0}): {1}\n\
		\t{0:<width$}| {2}\n\
		",
		ip,
		msg,
		inst.as_string(),
		width = width,
	);
	exit(-1);
}

pub(super) struct DebugInfo<'a> {
	pub source: &'a str,
	pub line_ranges: Vec<(Range<Word>, Word)>,
}

impl<'a> DebugInfo<'a> {
	pub(super) fn emitted_code(&mut self, curr_code_len: Word, line_no: Word) {
		match self.line_ranges.last_mut() {
			Some((range, line)) => {
				if *line == line_no {
					range.end = curr_code_len;
				} else {
					let end = range.end;
					self.line_ranges.push((end..curr_code_len, line_no));
				}
			}
			None => self.line_ranges.push((0..curr_code_len, line_no)),
		}
	}

	pub(super) fn throw_with_debug_info(&self, err_msg: &str, ip: Word) -> ! {
		let width = (ip as f64).log10().round() as usize + 1;
		let line_no = self
			.line_ranges
			.iter()
			.find_map(|(range, line)| {
				if range.contains(&(ip as u64)) {
					Some(*line)
				} else {
					None
				}
			})
			.expect("Invalid debug info!");
		println!(
			"\nerror (line {0}): {1}\n\
				\t{0:<width$}| {2}\n\
				",
			line_no,
			err_msg,
			self.source
				.lines()
				.nth((line_no-1) as usize)
				.expect("Invalid debug info!")
				.replace(char::is_whitespace, " "),
			width = width,
		);

		exit(-1);
	}
}
