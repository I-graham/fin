use strum::EnumCount;
use strum_macros::{EnumCount, EnumString, ToString};

pub type Word = u64;
pub type FWord = f64;

#[derive(Debug, Clone, Copy)]
pub(crate) struct Instruction {
	pub(crate) condition: Condition,
	pub(crate) mnemonic: Mnemonic,
	pub(crate) args: [u8; 6],
}

impl Instruction {
	pub(crate) fn args_as_const(&self) -> Word {
		const NEG_MASK: u8 = 1 << 7;
		let neg = self.args[1] & NEG_MASK;
		let shift = (self.args[1] & !NEG_MASK) as Word;
		use std::convert::TryInto;
		let unsigned = (u32::from_le_bytes(self.args[2..6].try_into().unwrap()) as Word) << shift;
		if neg != 0 {
			!unsigned
		} else {
			unsigned
		}
	}

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

	pub(crate) fn as_raw(&self) -> Word {
		let mut bytes = [0u8; 8];
		bytes[0] = self.condition as u8;
		bytes[1] = self.mnemonic as u8;
		bytes[2..].copy_from_slice(&self.args);
		Word::from_le_bytes(bytes)
	}

	pub(crate) fn as_string(&self) -> String {
		format!(
			"{:6} {:8} [{:3}, {:3}, {:3}, {:3}, {:3}, {:3}]",
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

impl Default for Instruction {
	fn default() -> Self {
		Self {
			condition: Condition::Al,
			mnemonic: Mnemonic::Nop,
			args: [0; 6],
		}
	}
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, EnumCount, EnumString, ToString)]
#[repr(u8)]
pub(crate) enum Condition {
	Al = 0,
	Nev,
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
	FEq,
	FNEq,
	FGr,
	FLs,
	FGrEq,
	FLsEq,
}

impl Condition {
	pub fn implies(self, other: Condition) -> bool {
		use Condition::*;
		let possibilities: &[Condition] = match self {
			Eq => &[UGrEq, ULsEq, GrEq, LsEq, FEq, FGrEq, FLsEq],
			Gr => &[GrEq],
			Ls => &[LsEq],
			UGr => &[UGrEq],
			ULs => &[ULsEq],
			FGr => &[FGrEq],
			FLs => &[FLsEq],
			_ => &[],
		};
		self == other || other == Al || possibilities.contains(&other)
	}

	pub fn negate(self) -> Self {
		use Condition::*;
		//Pairs of conditions & their negations. Even indices are followed by their negations,
		//odd indices are preceded by their negations.
		const PAIRS: &[Condition; Condition::COUNT] = &[
			Al, Nev, Eq, NEq, Gr, LsEq, Ls, GrEq, UGr, ULsEq, ULs, UGrEq, FEq, FNEq, FGrEq, FLsEq,
			FGr, FLs,
		];

		let index = PAIRS
			.iter()
			.enumerate()
			.find_map(|(i, &cond)| Some(i).filter(|_| cond == self))
			.unwrap();
		if index % 2 == 0 {
			PAIRS[index + 1]
		} else {
			PAIRS[index - 1]
		}
	}
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, EnumCount, EnumString, ToString)]
#[repr(u8)]
pub(crate) enum Mnemonic {
	Nop = 0,
	//Does nothing: no-op
	Dbg,
	//Print debug info
	Call,
	//Call function
	//procedure:
	//  store registers[args[0]..] on stack
	//	push ip onto stack
	//	push base pointer onto stack
	//	goto data[args[2..6] as const]
	Ret,
	//Call function
	//procedure:
	//	pop base pointer from stack
	//	pop ip from stack
	//	read registers[args[0]..] from stack
	//  deallocate memory

	Inc,
	//Increment value in registers[args[0]]
	Const,
	//Load args[2..6] as little endian u32, left shifted by args[1] into registers[args[0]]
	Load,
	//registers[args[1]] = stack[registers[args[0]]]
	Store,
	//stack[registers[args[1]]] = registers[args[0]]
	StkLd,
	//registers[args[0]] = stack[args[2..6] as const + base pointer]
	StkStr,
	//stack[args[2..6] as const + base pointer] = registers[args[0]]
	Puts,
	//Print string stored at stack[register[args[0]]]
	Or,
	//Or registers[args[1]] and registers[args[2]] into registers[args[0]]
	Xor,
	//Xor registers[args[1]] and registers[args[2]] into registers[args[0]]
	Add,
	//Add registers[args[1]] and registers[args[2]] into registers[args[0]]
	Sub,
	//Subtract registers[args[2]] from registers[args[1]] into registers[args[0]]
	Mod,
	//Integer modulo registers[args[1]] by registers[args[2]] into registers[args[0]]
	Mul,
	//Integer multiply registers[args[1]] by registers[args[2]] into registers[args[0]]
	Div,
	//Integer divide registers[args[1]] by registers[args[2]] into registers[args[0]]
	FAdd,
	//Float add registers[args[1]] and registers[args[2]] into registers[args[0]]
	FSub,
	//Float subtract registers[args[2]] from registers[args[1]] into registers[args[0]]
	FMod,
	//modulo registers[args[1]] by registers[args[2]] into registers[args[0]]
	FMul,
	//multiply registers[args[1]] by registers[args[2]] into registers[args[0]]
	FDiv,
	//divide registers[args[1]] by registers[args[2]] into registers[args[0]]
	Mov,
	//Move registers[args[0]] into registers[args[1]]
	Cmp,
	//Update condition register
	RelJmp,
	//wrapping adds const args[2..6] to instruction pointer.
	FToI,
	//Convert float value in args[1] to int, stores output in args[0],
	IToF,
	//Convert int value in args[1] to float, stores output in args[0],
	Hlt,
	//Exit program with return value registers[args[0]] as i32
}
