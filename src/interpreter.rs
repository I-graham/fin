use super::bytecode::*;
use super::runtime_error::*;

pub const REGISTERS: u8 = 16;

#[derive(Default)]
pub(crate) struct FinProgram<'a> {
	pub(crate) program_data: Vec<Word>,
	pub(crate) code: Vec<Instruction>,
	pub(crate) debug_info: Option<DebugInfo<'a>>,
}

impl<'a> FinProgram<'a> {
	pub(crate) fn new(bytecode: &'a [u8]) -> Self {
		let mut counter = 0;
		let mut read_n_bytes = |n: usize| {
			counter += n;
			&bytecode[(counter - n)..counter]
		};

		let slice_to_int = |bytes: &[u8]| Word::from_le_bytes(bytes[..].try_into().unwrap());

		use std::convert::TryInto;
		let src_size = Word::from_le_bytes(read_n_bytes(8).try_into().unwrap()) as usize;
		let source = std::str::from_utf8(&read_n_bytes(src_size)).expect("Malformed bytecode!");
		let debug_size = Word::from_le_bytes(read_n_bytes(8).try_into().unwrap()) as usize;
		let debug = read_n_bytes(debug_size)
			.chunks_exact(24)
			.map(|bytes| {
				(
					slice_to_int(&bytes[0..8])..slice_to_int(&bytes[8..16]),
					slice_to_int(&bytes[16..24]),
				)
			})
			.collect::<Vec<_>>();
		let data_size = Word::from_le_bytes(read_n_bytes(8).try_into().unwrap()) as usize;
		let program_data = read_n_bytes(data_size)
			.chunks_exact(8)
			.map(slice_to_int)
			.collect::<Vec<_>>();
		let code_size = Word::from_le_bytes(read_n_bytes(8).try_into().unwrap()) as usize;
		let code = read_n_bytes(code_size)
			.chunks_exact(8)
			.map(|bytes| Instruction::from_raw(bytes[..].try_into().unwrap()))
			.collect::<Vec<_>>();
		Self {
			program_data,
			code,
			debug_info: Some(DebugInfo {
				source,
				line_ranges: debug,
			}),
		}
	}

	pub(crate) fn execute(&self) {
		let mut data = Data {
			program_data: self.program_data.clone(),
			stack: vec![],
		};
		let mut reg = RegisterData::default();

		while reg.ip < self.code.len() as Word {
			let instruction = self.code[reg.ip as usize];
			if reg.cond_matches(instruction.condition) {
				use Mnemonic::*;
				match instruction.mnemonic {
					Nop => (),
					Dbg => println!("{:?}\n{:?}", &reg, &data),
					Inc => *reg.write_gp(instruction.args[0]) += 1,
					Load => {
						*reg.write_gp(instruction.args[1]) =
							data.read_addr(reg.read_gp(instruction.args[0]))
					}
					StkLd => {
						*reg.write_gp(instruction.args[1]) =
							data.read_addr(reg.bp + reg.read_gp(instruction.args[0]))
					}
					Const => {
						use std::convert::TryInto;
						*reg.write_gp(instruction.args[0]) =
							(u32::from_le_bytes(instruction.args[2..6].try_into().unwrap())
								as Word) << (instruction.args[1] as Word);
					}
					Store => {
						*data.write_addr(reg.read_gp(instruction.args[1])) =
							reg.read_gp(instruction.args[0])
					}
					StkStr => {
						*data.write_addr(reg.bp + reg.read_gp(instruction.args[1])) =
							reg.read_gp(instruction.args[0])
					}
					Push => data.stack.push(reg.read_gp(instruction.args[0])),
					PushAll => {
						for i in instruction.args[0]..instruction.args[1] {
							data.stack.push(reg.read_gp(i));
						}
					}
					Pop => {
						*reg.write_gp(instruction.args[0]) = data.stack.pop().expect("Empty stack");
					}
					PopAll => {
						for i in instruction.args[0]..instruction.args[1] {
							*reg.write_gp(i) = data.stack.pop().expect("Empty stack");
						}
					}
					Puts => {
						unimplemented!()
					}
					Or => {
						*reg.write_gp(instruction.args[0]) =
							reg.read_gp(instruction.args[1]) | reg.read_gp(instruction.args[2]);
					}
					Xor => {
						*reg.write_gp(instruction.args[0]) =
							reg.read_gp(instruction.args[1]) ^ reg.read_gp(instruction.args[2]);
					}
					Add => {
						*reg.write_gp(instruction.args[0]) = reg
							.read_gp(instruction.args[1])
							.wrapping_add(reg.read_gp(instruction.args[2]));
					}
					Sub => {
						*reg.write_gp(instruction.args[0]) = reg
							.read_gp(instruction.args[1])
							.wrapping_sub(reg.read_gp(instruction.args[2]));
					}
					Mul => {
						*reg.write_gp(instruction.args[0]) = reg
							.read_gp(instruction.args[1])
							.wrapping_mul(reg.read_gp(instruction.args[2]));
					}
					Div => {
						let divisor = reg.read_gp(instruction.args[2]) as i64;
						if divisor != 0 {
							*reg.write_gp(instruction.args[0]) =
								(reg.read_gp(instruction.args[1]) as i64 / divisor) as Word;
						} else {
							self.throw_error("Division by zero!", reg.ip);
						}
					}
					Mov => {
						*reg.write_gp(instruction.args[1]) = reg.read_gp(instruction.args[0]);
					}
					Hlt => {
						std::process::exit(reg.read_gp(instruction.args[0]) as i32);
					}
				}
			}
			reg.ip += 1;
		}
	}

	pub(crate) fn assemble(input: &str) -> Self {
		use std::str::FromStr;
		let (data, code) = input.split_at(input.find(']').expect("Invalid program data format"));
		Self {
			program_data: data
				.split(',')
				.filter_map(|num| {
					if !num.is_empty() {
						Some(
							Word::from_str(num.replace(char::is_whitespace, "").as_str())
								.expect("Invalid Number"),
						)
					} else {
						None
					}
				})
				.collect(),
			code: code
				.split(';')
				.filter_map(|inst| {
					if !inst.is_empty() {
						Some(Instruction::from_string(
							inst.replace(char::is_whitespace, ""),
						))
					} else {
						None
					}
				})
				.collect(),
			debug_info: None,
		}
	}

	pub(crate) fn dissassemble(&self) -> String {
		let mut output = String::from("[\n");
		for word in &self.program_data {
			output += &word.to_string();
			output += ",\n";
		}
		output += "]\n\n";

		let width = (self.code.len() as f64).log10().round() as usize + 1;
		for (i, instruction) in self.code.iter().enumerate() {
			output += &format!(
				"{:>width$}| {};\n",
				i,
				&instruction.as_string(),
				width = width
			);
		}

		output
	}

	pub(crate) fn to_raw(&self) -> Vec<u8> {
		let mut output = vec![];
		let push_word = |output: &mut Vec<_>, val: Word| {
			output.extend(&val.to_le_bytes());
		};
		if let Some(debug) = &self.debug_info {
			push_word(&mut output, debug.source.len() as Word);
			output.extend(debug.source.as_bytes());
			push_word(&mut output, debug.line_ranges.len() as Word);
			for (range, line) in &debug.line_ranges {
				push_word(&mut output, range.start);
				push_word(&mut output, range.end);
				push_word(&mut output, *line);
			}
		} else {
			push_word(&mut output, 0);
			push_word(&mut output, 0);
		}
		push_word(&mut output, self.program_data.len() as Word);
		for word in &self.program_data {
			push_word(&mut output, *word);
		}
		push_word(&mut output, self.code.len() as Word);
		for inst in &self.code {
			push_word(&mut output, inst.as_raw());
		}
		output
	}

	fn throw_error(&self, msg: &str, ip: Word) -> ! {
		if let Some(debug) = &self.debug_info {
			debug.throw_with_debug_info(msg, ip);
		} else {
			throw_error(msg, ip as usize, &self.code[ip as usize]);
		};
	}
}

#[derive(Debug, Default)]
struct RegisterData {
	ip: Word,
	bp: Word,
	cmp: u8,
	gp: [Word; REGISTERS as usize],
}

impl RegisterData {
	const GREATER_MASK: u8 = 1 << 0;
	const LESS_MASK: u8 = 1 << 1;
	const EQ_MASK: u8 = 1 << 2;
	const UNSIGNED_GREATER_MASK: u8 = 1 << 3;
	const UNSIGNED_LESS_MASK: u8 = 1 << 4;

	fn cond_matches(&self, cond: Condition) -> bool {
		let greater = self.cmp & Self::GREATER_MASK;
		let less = self.cmp & Self::LESS_MASK;
		let equal = self.cmp & Self::EQ_MASK;
		let unsigned_greater = self.cmp & Self::UNSIGNED_GREATER_MASK;
		let unsigned_less = self.cmp & Self::UNSIGNED_LESS_MASK;

		use Condition::*;
		match cond {
			Al => true,
			Eq => equal != 0,
			NEq => equal == 0,
			Gr => greater != 0,
			Ls => less != 0,
			GrEq => less == 0,
			LsEq => greater == 0,
			UGr => unsigned_greater != 0,
			ULs => unsigned_less != 0,
			UGrEq => unsigned_less == 0,
			ULsEq => unsigned_greater == 0,
		}
	}

	fn read_gp(&self, register: u8) -> Word {
		assert!(register < REGISTERS);
		self.gp[register as usize]
	}

	fn write_gp(&mut self, register: u8) -> &mut Word {
		assert!(register < REGISTERS);
		&mut self.gp[register as usize]
	}
}

#[derive(Debug)]
struct Data {
	pub(crate) program_data: Vec<Word>,
	pub(crate) stack: Vec<Word>,
}

impl Data {
	fn read_addr(&self, address: Word) -> Word {
		//If the first bit is a 1, then the value is fetched from program_data
		let program_data_bit = address & (1 << (Word::BITS - 1));

		if program_data_bit != 0 {
			assert!(address < self.program_data.len() as Word);
			self.program_data[(address & !program_data_bit) as usize]
		} else {
			assert!(address < self.stack.len() as Word);
			self.stack[address as usize]
		}
	}

	fn write_addr(&mut self, address: Word) -> &mut Word {
		//If the first bit is a 1, then the value written to is in program_data
		let program_data_bit = address & (1 << (Word::BITS - 1));

		if program_data_bit != 0 {
			assert!(address < self.program_data.len() as Word);
			&mut self.program_data[(address & !program_data_bit) as usize]
		} else {
			assert!(address < self.stack.len() as Word);
			&mut self.stack[address as usize]
		}
	}
}
