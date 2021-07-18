//collection of bytecode snippets

use crate::bytecode::*;

pub(super) const fn num_as_const_bytes(value: Word) -> [u8; 5] {
	let shift = if value != 0 {
		value.trailing_zeros() as u8
	} else {
		0
	};
	let val = (value >> shift) as u32;
	let [a, b, c, d] = val.to_le_bytes();
	[shift, a, b, c, d]
}

pub(super) fn fits_in_const_inst(value: Word) -> bool {
	value.leading_zeros() + value.trailing_zeros() >= Word::BITS / 2
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
		let [_, a, b, c, d] = num_as_const_bytes(value & u32::MAX as Word);
		let [_, a2, b2, c2, d2] = num_as_const_bytes(value);
		out.extend(&[
			Instruction {
				condition: Condition::Al,
				mnemonic: Mnemonic::Const,
				args: [reg1, 0, a, b, c, d],
			},
			Instruction {
				condition: Condition::Al,
				mnemonic: Mnemonic::Const,
				args: [reg2, 32, a2, b2, c2, d2],
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
	let raw_dist = jmp_dest.wrapping_sub(instruction_index + 1);
	let negated = !fits_in_const_inst(raw_dist);
	let jmp_dist = if negated {
		raw_dist.wrapping_neg()
	} else {
		raw_dist
	};
	debug_assert!(fits_in_const_inst(jmp_dist));

	let [shift, a, b, c, d] = num_as_const_bytes(jmp_dist);
	let inst = &mut code[instruction_index as usize];
	debug_assert!(inst.mnemonic == Mnemonic::RelJmp);
	inst.args = [negated as u8, shift, a, b, c, d];
}
