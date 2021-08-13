//collection of bytecode snippets

use crate::bytecode::*;

pub(super) fn num_as_const_bytes(value: Word) -> [u8; 5] {
	debug_assert!(fits_in_const_inst(value));
	let (unsigned, negated) = if value.leading_zeros() + value.trailing_zeros() >= Word::BITS / 2 {
		(value, false)
	} else {
		(!value, true)
	};
	let shift = if unsigned != 0 {
		unsigned.trailing_zeros() as u8
	} else {
		0u8
	};
	let val = (unsigned >> shift) as u32;
	let [a, b, c, d] = val.to_le_bytes();
	[shift | if negated { 1u8 << 7 } else { 0u8 }, a, b, c, d]
}

pub(super) fn fits_in_const_inst(value: Word) -> bool {
	let negated = !value;
	value.leading_zeros() + value.trailing_zeros() >= Word::BITS / 2
		|| negated.leading_zeros() + negated.trailing_zeros() >= Word::BITS / 2
}

pub(super) fn load_const(value: Word, reg1: u8, reg2: Option<u8>, out: &mut Vec<Instruction>) {
	if fits_in_const_inst(value) {
		let [shift, a, b, c, d] = num_as_const_bytes(value);
		out.extend(&[Instruction {
			mnemonic: Mnemonic::Const,
			args: [reg1, shift, a, b, c, d],
			..Default::default()
		}]);
	} else if let Some(reg2) = reg2 {
		let [shift1, a1, b1, c1, d1] = num_as_const_bytes(value & u32::MAX as Word);
		let [shift2, a2, b2, c2, d2] = num_as_const_bytes(value & !(u32::MAX as Word));
		out.extend(&[
			Instruction {
				condition: Condition::Al,
				mnemonic: Mnemonic::Const,
				args: [reg1, shift1, a1, b1, c1, d1],
			},
			Instruction {
				condition: Condition::Al,
				mnemonic: Mnemonic::Const,
				args: [reg2, shift2, a2, b2, c2, d2],
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

pub(super) fn branch(instruction_index: Word, jmp_dest: Word, code: &mut Vec<Instruction>) {
	let inst = &mut code[instruction_index as usize];
	debug_assert!(inst.mnemonic == Mnemonic::RelJmp);
	let dist = jmp_dest.wrapping_sub(instruction_index + 1);
	let [shift, a, b, c, d] = num_as_const_bytes(dist);
	inst.args = [0, shift, a, b, c, d];
}
