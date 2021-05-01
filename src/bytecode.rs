use strum::EnumCount;
use strum_macros::{EnumCount, EnumString, ToString};

pub(crate) type Word = u64;

#[derive(Debug, Clone, Copy)]
pub(crate) struct Instruction {
	pub(crate) condition: Condition,
	pub(crate) mnemonic: Mnemonic,
	pub(crate) args: [u8; 6],
}

impl Instruction {
	pub(crate) fn from_raw(instruction: &[u8; 8]) -> Self {
		use std::convert::TryInto;
		use std::mem::transmute;
		assert!((instruction[0] as usize) < Condition::COUNT);
		assert!((instruction[1] as usize) < Mnemonic::COUNT);
		Self {
			condition: unsafe { transmute::<u8, Condition>(instruction[0]) },
			mnemonic: unsafe { transmute::<u8, Mnemonic>(instruction[1]) },
			args: instruction[2..].try_into().unwrap(),
		}
	}

	pub(crate) fn to_raw(&self) -> Word {
		let mut bytes = [0u8; 8];
		bytes[0] = self.condition as u8;
		bytes[1] = self.mnemonic as u8;
		bytes[2..].copy_from_slice(&self.args);
		Word::from_le_bytes(bytes)
	}

	pub(crate) fn as_string(&self) -> String {
		format!(
			"{:8} {:8} [{}, {}, {}, {}, {}, {}]",
			self.condition.to_string(),
			self.mnemonic.to_string(),
			self.args[0],
			self.args[1],
			self.args[2],
			self.args[3],
			self.args[4],
			self.args[5],
		)
	}

	pub(crate) fn from_string(mut input: String) -> Self {
		use std::convert::TryInto;
		use std::iter::repeat;
		use std::str::FromStr;

		let mut instruction = input.split_off(input.find('\t').unwrap() + 1);
		let args: [u8; 6] = instruction
			.split_off(instruction.find('\t').unwrap())
			.trim_matches(|c: char| c.is_whitespace() || "[]".contains(c))
			.split(',')
			.filter_map(|arg| {
				if !arg.is_empty() {
					Some(u8::from_str(arg).expect("Invalid argument"))
				} else {
					None
				}
			})
			.chain(repeat(0))
			.take(6)
			.collect::<Vec<u8>>()
			.try_into()
			.unwrap();

		input.retain(|c| !c.is_whitespace());
		instruction.retain(|c| !c.is_whitespace());

		Self {
			condition: Condition::from_str(input.trim()).unwrap(),
			mnemonic: Mnemonic::from_str(&instruction.trim()).unwrap(),
			args,
		}
	}
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy, EnumCount, EnumString, ToString)]
#[repr(u8)]
pub(crate) enum Condition {
	Al = 0,
	Eq,
	NEq,
	Gr,
	Ls,
	GrEq,
	LsEq,
	UGr,
	ULs,
	UGrEq,
	ULsEq,
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy, EnumCount, EnumString, ToString)]
#[repr(u8)]
pub(crate) enum Mnemonic {
	//Print debug info
	Dbg = 0,
	//Increment value in registers[args[0]]
	Inc,
	//Load args[2..6] as little endian u32, left shifted by args[1] into registers[args[0]]
	Const,
	//registers[args[0]] = registers[data[args[0]]]
	Load,
	//data[registers[args[0]]] = registers[args[0]]
	Store,
	//Pop off stack into registers[args[0]]
	Pop,
	//Restore all registers in range args[0]..args[1] from stack
	PopAll,
	//Push registers[args[0]] onto stack
	Push,
	//Push all registers in range args[0]..args[1] from stack
	PushAll,
	//Print string stored at data[register[args[0]]]
	Puts,
	//Or registers[args[1]] and registers[args[2]] into registers[args[0]]
	Or,
	//Xor registers[args[1]] and registers[args[2]] into registers[args[0]]
	Xor,
	//Add registers[args[1]] and registers[args[2]] into registers[args[0]]
	Add,
	//Subtract registers[args[2]] from registers[args[1]] into registers[args[0]]
	Sub,
	//Integer multiply registers[args[1]] by registers[args[2]] into registers[args[0]]
	Mul,
	//Integer divide registers[args[2]] by registers[args[1]] into registers[args[0]]
	Div,
	//Move registers[args[0]] into registers[args[1]]
	Mov,
}
