use crate::bytecode::*;
//collection of bytecode snippets

pub(super) fn load_const(value: Word, reg1: u8, reg2: Option<u8>, out: &mut Vec<Instruction>) {
	if value.leading_zeros() + value.trailing_zeros() >= Word::BITS / 2 {
		let shift = if value != 0 {
			value.trailing_zeros() as u8
		} else {
			0
		};
		let val = (value >> shift) as u32;
		let [a, b, c, d] = val.to_le_bytes();
		out.push(Instruction {
			condition: Condition::Al,
			mnemonic: Mnemonic::Const,
			args: [reg1, shift, a, b, c, d],
		});
	} else if let Some(reg2) = reg2 {
		let shift = (Word::BITS / 2) as u8;
		let val = value as u32;
		let [a, b, c, d] = val.to_le_bytes();
		let val2 = (value >> shift) as u32;
		let [a2, b2, c2, d2] = val2.to_le_bytes();
		out.extend(&[
			Instruction {
				condition: Condition::Al,
				mnemonic: Mnemonic::Const,
				args: [reg1, 0, a, b, c, d],
			},
			Instruction {
				condition: Condition::Al,
				mnemonic: Mnemonic::Const,
				args: [reg2, shift, a2, b2, c2, d2],
			},
			Instruction {
				condition: Condition::Al,
				mnemonic: Mnemonic::Or,
				args: [reg1, reg1, reg2, 0, 0, 0],
			},
		]);
	} else {
		unimplemented!("Invalid usage of abbreviation: sufficient registers must be guaranteed.");
	}
}
