use strum::EnumCount;
use strum_macros::{EnumCount, EnumString, ToString};

pub type Word = u64;

#[derive(Debug, Clone, Copy)]
pub struct Instruction {
	pub condition: Condition,
	pub mnemonic: Mnemonic,
	pub args: [u8; 6],
}

impl Instruction {
	pub fn from_raw(instruction: &[u8; 8]) -> Self {
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

	pub fn to_raw(&self) -> Word {
		let mut bytes = [0u8; 8];
		bytes[0] = self.condition as u8;
		bytes[1] = self.mnemonic as u8;
		bytes[2..].copy_from_slice(&self.args);
		Word::from_le_bytes(bytes)
	}

	pub fn as_string(&self) -> String {
		format!(
			"{:5}\t{:5}\t[{}, {}, {}, {}, {}, {}]",
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

	pub fn from_string(mut input: String) -> Self {
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
pub enum Condition {
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
pub enum Mnemonic {
	Nop = 0,
	Dbg,
	Inc,
	Pop,
	Push,
	Puts,
}
