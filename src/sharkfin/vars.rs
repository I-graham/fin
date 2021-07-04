use super::abbreviations;
use crate::bytecode::*;
use crate::interpreter::*;
use fnv::FnvHashMap;
use std::ops::Range;

#[derive(Debug, Default)]
pub(super) struct FunctionScope<'a> {
	framepointer: Word,
	id_counter: usize,
	stack_size: Word,
	//bool is true at last usage / if variable has been used before (false on creation, true elsewhere)
	intervals: (usize, FnvHashMap<VariableID<'a>, Range<usize>>),
	synonyms: FnvHashMap<VariableID<'a>, VariableID<'a>>,
	variables: FnvHashMap<VariableID<'a>, Variable>,
	registers: [Option<VariableID<'a>>; REGISTERS as usize],
}

impl<'a> FunctionScope<'a> {
	pub fn create_synonym(
		&mut self,
		name: Option<&'a str>,
		old_var: VariableID<'a>,
	) -> VariableID<'a> {
		let syn = self.create_id(name);
		let var = *self.synonyms.get(&old_var).unwrap_or(&old_var);
		self.synonyms.insert(syn, var);
		syn
	}

	pub fn alloc_and_use<const N: usize>(
		&mut self,
		info: &[(Option<&'a str>, VarType); N],
	) -> [VariableID<'a>; N] {
		let allocated = self.allocate_var(info);
		self.record_usage(&allocated);
		allocated
	}

	pub fn allocate_var<const N: usize>(
		&mut self,
		info: &[(Option<&'a str>, VarType); N],
	) -> [VariableID<'a>; N] {
		let mut out = [VariableID::Unnamed(0); N];
		for (i, &(name, ty)) in info.iter().enumerate() {
			let id = self.create_id(name);
			self.intervals.1.insert(id, 0..0);
			self.variables.insert(
				id,
				Variable {
					location: Location::Nowhere,
					ty,
				},
			);
			out[i] = id;
		}
		out
	}

	pub fn record_usage<const N: usize>(&mut self, ids: &[VariableID<'a>; N]) {
		self.intervals.0 += 1;
		let usage_num = self.intervals.0;
		for id in ids {
			let base_id = self.get_base_id(*id);
			let mut_var = self.intervals.1.get_mut(&base_id).unwrap();
			if mut_var.start == 0 {
				mut_var.start = usage_num;
			}
			mut_var.end = usage_num;
		}
	}

	pub fn register<const N: usize>(
		&mut self,
		ids: &[VariableID<'a>; N],
		out: &mut Vec<Instruction>,
	) -> [u8; N] {
		let move_var_from_stack = |var: &mut Variable, reg_id, out: &mut Vec<Instruction>| {
			if let Location::Stack(addr) = var.location {
				if addr.leading_zeros() + addr.trailing_zeros() < Word::BITS / 2 {
					unimplemented!(
						"Number of variables in single function must fit within 64 bits! ()"
					);
				}
				abbreviations::load_const(addr, reg_id, None, out);
				out.push(Instruction {
					condition: Condition::Al,
					mnemonic: Mnemonic::StkLd,
					args: [reg_id, reg_id, 0, 0, 0, 0],
				});
			}
			var.location = Location::Register(reg_id);
		};

		debug_assert!(N < REGISTERS.into());

		for id in ids.iter() {
			let base_id = self.get_base_id(*id);
			//erase vars which died before this one was born, AKA vars which will never be used
			for (var, interval) in self.intervals.1.iter() {
				if interval.end < self.intervals.1[&base_id].start {
					if let Some(dead_var) = self.variables.remove(&var) {
						self.synonyms.retain(|_, v| v != var);
						if let Location::Register(reg) = dead_var.location {
							self.registers[reg as usize] = None;
						}
					}
				}
			}
		}

		let variables = &mut self.variables;
		self.intervals
			.1
			.retain(|vid, _lifetime| variables.contains_key(vid));

		let mut opregs = [0; N];

		'place_reg: for (i, id) in ids.iter().enumerate() {
			let base_id = self.get_base_id(*id);

			if let Location::Register(reg) = self.variables[&base_id].location {
				opregs[i] = reg;
				continue 'place_reg;
			}

			for (reg_id, reg) in self.registers.iter_mut().enumerate() {
				if reg.is_none() {
					let var = self.variables.get_mut(&base_id).unwrap();
					*reg = Some(base_id);
					move_var_from_stack(var, reg_id as u8, out);
					opregs[i] = reg_id as u8;
					continue 'place_reg;
				}
			}

			let registers = &self.registers;

			let replaced_id = self
				.intervals
				.1
				.iter()
				.filter_map(|(&var, lifetime)| {
					if !ids.contains(&var) && registers.contains(&Some(var)) {
						Some((var, lifetime.end))
					} else {
						None
					}
				})
				.max_by_key(|&(_var, end)| end)
				.map(|(var, _end)| var)
				.unwrap();

			let replaced = self.variables.get_mut(&replaced_id).unwrap();

			if let Location::Register(reg) = replaced.location {
				self.registers[reg as usize] = Some(base_id);
				replaced.location = Location::Stack(self.stack_size);
				self.stack_size += 1;

				out.push(Instruction {
					condition: Condition::Al,
					mnemonic: Mnemonic::Push,
					args: [reg, 0, 0, 0, 0, 0],
				});

				let var = self.variables.get_mut(&base_id).unwrap();
				move_var_from_stack(var, reg, out);
				opregs[i] = reg;
				continue 'place_reg;
			} else {
				//No slot found
				unreachable!();
			}
		}

		opregs
	}

	pub fn get_var(&self, name: &'a str) -> Option<VariableID<'a>> {
		let id = VariableID::Named(name);
		if self.synonyms.contains_key(&id) || self.variables.contains_key(&id) {
			Some(self.get_base_id(id))
		} else {
			None
		}
	}

	fn create_id(&mut self, name: Option<&'a str>) -> VariableID<'a> {
		if let Some(varname) = name {
			VariableID::Named(varname)
		} else {
			self.id_counter += 1;
			VariableID::Unnamed(self.id_counter)
		}
	}

	fn get_base_id(&self, id: VariableID<'a>) -> VariableID<'a> {
		let out = *self.synonyms.get(&id).unwrap_or(&id);
		debug_assert!(self.variables.contains_key(&out));
		out
	}
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum VariableID<'a> {
	Unnamed(usize),
	Named(&'a str),
}

#[derive(Clone, Copy, Debug)]
pub struct Variable {
	pub location: Location,
	pub ty: VarType,
}

#[derive(Clone, Copy, Debug)]
pub enum Location {
	Register(u8),
	Stack(Word),
	//No location information is needed/available
	Nowhere,
}

#[derive(Clone, Copy, Debug)]
pub enum VarType {
	Int,
}
