use super::abbreviations;
use super::bytecode::*;
use super::runtime_error::*;
use std::io;

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
		let src_size = slice_to_int(read_n_bytes(8)) as usize;
		let source = std::str::from_utf8(&read_n_bytes(src_size)).expect("Malformed bytecode!");
		let debug_size = slice_to_int(read_n_bytes(8)) as usize;
		let debug = read_n_bytes(debug_size * 8 * 3)
			.chunks_exact(8 * 3)
			.map(|bytes| {
				(
					slice_to_int(&bytes[0..8])..slice_to_int(&bytes[8..16]),
					slice_to_int(&bytes[16..24]),
				)
			})
			.collect::<Vec<_>>();
		let data_size = slice_to_int(read_n_bytes(8)) as usize;
		let program_data = read_n_bytes(data_size * 8)
			.chunks_exact(8)
			.map(slice_to_int)
			.collect::<Vec<_>>();
		let code_size = slice_to_int(read_n_bytes(8)) as usize;
		let code = read_n_bytes(code_size * 8)
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

	pub(crate) fn execute(&self, debug: bool) {
		let mut data = Data {
			program_data: self.program_data.clone(),
			..Default::default()
		};

		while data.regs.ip < self.code.len() as Word {
			let ip = data.regs.ip;
			let instruction = self.code[ip as usize];
			if debug {
				print!(
					"cond: {}\ninstruction: \n{}\ndata: \n{:?}\n",
					if data.regs.cond_matches(instruction.condition) {
						"matches"
					} else {
						"does not match"
					},
					instruction.as_string(),
					&data
				);
				if ip < self.code.len() as Word - 1 {
					loop {
						use std::io::Write;
						print!(">");
						io::stdout().flush().unwrap();
						let mut cmd = Default::default();
						io::stdin().read_line(&mut cmd).unwrap();
						if cmd.starts_with("exit") {
							throw_error("Early exit", ip as usize, instruction);
						} else if let Some(num) = cmd.strip_prefix("skip:") {
							match num.parse::<Word>() {
								Ok(dist) => data.regs.ip += dist - 1,
								Err(err) => println!("Unable to parse command: `{:?}`", err),
							}
						} else if cmd.trim() == "" {
							break;
						} else {
							println!("Unknown Command `{}`", cmd);
						}
					}
				}
			}
			if let Err(msg) = data.exec_instruction(instruction) {
				self.throw_error(msg, ip);
			}
			data.regs.ip += 1;
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

	pub(crate) fn post_process(&mut self) {
		let get_jmp_dest = |inst: Instruction, ip: usize| {
			if inst.args[0] != 0 {
				inst.args_as_const().wrapping_neg()
			} else {
				inst.args_as_const()
			}
			.wrapping_add(1)
			.wrapping_add(ip as Word)
		};
		for ip in 0..self.code.len() {
			let inst = self.code[ip];
			if inst.mnemonic == Mnemonic::RelJmp {
				let mut jmp_dest = get_jmp_dest(inst, ip);
				let mut jmp_dest_inst = self.code[jmp_dest as usize];
				while jmp_dest_inst.mnemonic == Mnemonic::RelJmp
					&& jmp_dest != ip as Word
					&& inst.condition.implies(jmp_dest_inst.condition)
				{
					jmp_dest = get_jmp_dest(jmp_dest_inst, jmp_dest as usize);
					abbreviations::branch(ip as Word, jmp_dest, &mut self.code);
					jmp_dest_inst = self.code[jmp_dest as usize];
				}
			}
		}
		let mut ip = 0;
		while ip < self.code.len() {
			let inst = self.code[ip];
			if inst.mnemonic == Mnemonic::RelJmp && inst.args == [0; 6] {
				self.code.remove(ip);
				for early in 0..self.code.len() {
					let early_inst = self.code[early];
					let inst_moved = early >= ip;
					let old_early = early + inst_moved as usize;
					let old_dest = get_jmp_dest(early_inst, old_early);
					if early_inst.mnemonic == Mnemonic::RelJmp
						&& (old_early >= ip) != (old_dest >= ip as Word)
					{
						let new_dest = if inst_moved { old_dest } else { old_dest - 1 };
						abbreviations::branch(early as Word, new_dest, &mut self.code);
					}
				}
			} else {
				ip += 1;
			}
		}
	}

	fn throw_error(&self, msg: &str, ip: Word) -> ! {
		if let Some(debug) = &self.debug_info {
			debug.throw_with_debug_info(msg, ip);
		} else {
			throw_error(msg, ip as usize, self.code[ip as usize]);
		};
	}
}

#[derive(Debug, Default)]
struct Data {
	pub(crate) program_data: Vec<Word>,
	pub(crate) stack: Vec<Word>,
	pub(crate) regs: RegisterData,
}

impl Data {
	pub fn exec_instruction(&mut self, inst: Instruction) -> Result<(), &str> {
		let args = &inst.args;
		if self.regs.cond_matches(inst.condition) {
			use Mnemonic::*;
			match inst.mnemonic {
				Nop => (),
				Dbg => println!("{:?}\n{:?}", &self.regs, &self),
				Inc => *self.regs.write_gp(args[0]) += 1,
				Load => *self.regs.write_gp(args[1]) = self.read_addr(self.regs.read_gp(args[0])),
				StkLd => {
					let addr = self.regs.bp + inst.args_as_const() as Word;
					*self.regs.write_gp(args[0]) = self.read_addr(addr);
				}
				Const => {
					*self.regs.write_gp(args[0]) = inst.args_as_const() as Word;
				}
				Store => *self.write_addr(self.regs.read_gp(args[1])) = self.regs.read_gp(args[0]),
				StkStr => {
					let addr = self.regs.bp + inst.args_as_const() as Word;
					*self.write_addr(addr) = self.regs.read_gp(args[0]);
				}
				Push => self.stack.push(self.regs.read_gp(args[0])),
				Pop => {
					*self.regs.write_gp(args[0]) = self.stack.pop().expect("Empty stack");
				}
				Puts => {
					unimplemented!()
				}
				Or => {
					let [a, b] = self.regs.map_to_gps(&[args[1], args[2]]);
					*self.regs.write_gp(args[0]) = a | b;
				}
				Xor => {
					let [a, b] = self.regs.map_to_gps(&[args[1], args[2]]);
					*self.regs.write_gp(args[0]) = a ^ b;
				}
				Add => {
					let [a, b] = self.regs.map_to_gps(&[args[1], args[2]]);
					*self.regs.write_gp(args[0]) = a.wrapping_add(b);
				}
				Sub => {
					let [a, b] = self.regs.map_to_gps(&[args[1], args[2]]);
					*self.regs.write_gp(args[0]) = a.wrapping_sub(b);
				}
				Mul => {
					let [a, b] = self.regs.map_to_gps(&[args[1], args[2]]);
					*self.regs.write_gp(args[0]) = a.wrapping_mul(b);
				}
				Div => {
					let [a, b] = self.regs.map_to_gps(&[args[1], args[2]]);
					if b != 0 {
						*self.regs.write_gp(args[0]) = (a / b) as Word;
					} else {
						return Err("Division by zero!");
					}
				}
				Mov => {
					*self.regs.write_gp(args[1]) = self.regs.read_gp(args[0]);
				}
				Cmp => {
					let [a, b] = self.regs.map_to_gps(&[args[0], args[1]]);
					self.regs.set_cond(a, b);
				}
				RelJmp => {
					let const_val = inst.args_as_const() as u64;
					self.regs.ip = self.regs.ip.wrapping_add(if args[0] != 0 {
						const_val.wrapping_neg()
					} else {
						const_val
					});
				}
				Hlt => {
					std::process::exit(self.regs.read_gp(args[0]) as i32);
				}
			}
		}
		Ok(())
	}

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

	fn map_to_gps<const N: usize>(&self, regs: &[u8; N]) -> [Word; N] {
		let mut out = [0; N];
		for (i, reg) in regs.iter().enumerate() {
			out[i] = self.read_gp(*reg);
		}
		out
	}

	fn set_cond(&mut self, a: Word, b: Word) {
		self.cmp = 0;
		let mut set_bit = |bit| self.cmp |= bit;
		use std::cmp::Ordering::*;
		if a == b {
			set_bit(Self::EQ_MASK);
		} else {
			match a.cmp(&b) {
				Less => set_bit(Self::UNSIGNED_LESS_MASK),
				Greater => set_bit(Self::UNSIGNED_GREATER_MASK),
				_ => (),
			}
			match (a as i64).cmp(&(b as i64)) {
				Less => set_bit(Self::LESS_MASK),
				Greater => set_bit(Self::GREATER_MASK),
				_ => (),
			}
		}
	}

	fn cond_matches(&self, cond: Condition) -> bool {
		let greater = self.cmp & Self::GREATER_MASK;
		let less = self.cmp & Self::LESS_MASK;
		let equal = self.cmp & Self::EQ_MASK;
		let unsigned_greater = self.cmp & Self::UNSIGNED_GREATER_MASK;
		let unsigned_less = self.cmp & Self::UNSIGNED_LESS_MASK;

		use Condition::*;
		match cond {
			Al => true,
			Nev => false,
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
