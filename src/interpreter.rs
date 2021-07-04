use super::bytecode::*;

pub const REGISTERS: u8 = 16;

pub(crate) struct FinProgram {
	pub(crate) program_data: Vec<Word>,
	pub(crate) code: Vec<Instruction>,
}

impl FinProgram {
	pub(crate) fn new(bytecode: &[u8]) -> Self {
		use std::convert::TryInto;
		let data_size = Word::from_le_bytes(bytecode[0..8].try_into().unwrap());
		Self {
			program_data: bytecode[8..(8 + data_size as usize)]
				.rchunks_exact(8)
				.map(|bytes| Word::from_le_bytes(bytes[..].try_into().unwrap()))
				.collect::<Vec<_>>(),
			code: bytecode[(8 + data_size as usize)..]
				.chunks_exact(8)
				.map(|bytes| Instruction::from_raw(bytes[..].try_into().unwrap()))
				.collect::<Vec<_>>(),
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
						*reg.write_gp(instruction.args[0]) =
							(reg.read_gp(instruction.args[1]) as i64
								/ reg.read_gp(instruction.args[2]) as i64) as Word;
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

	pub(crate) fn assemble(mut input: String) -> Self {
		use std::str::FromStr;
		let code = input.split_off(input.find(']').expect("Invalid program data format"));
		input.retain(|c| !c.is_whitespace());
		Self {
			program_data: input[1..]
				.split(',')
				.filter_map(|num| {
					if !num.is_empty() {
						Some(Word::from_str(num).expect("Invalid Number"))
					} else {
						None
					}
				})
				.collect(),
			code: code[2..]
				.split(';')
				.filter_map(|inst| {
					if !inst.is_empty() {
						Some(Instruction::from_string(inst.into()))
					} else {
						None
					}
				})
				.collect(),
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
		output.extend_from_slice(&(self.program_data.len() as Word).to_le_bytes());
		for word in &self.program_data {
			output.extend(&word.to_le_bytes());
		}
		for inst in &self.code {
			output.extend(&inst.as_raw().to_le_bytes());
		}
		output
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
