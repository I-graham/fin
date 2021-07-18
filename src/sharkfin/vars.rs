use crate::abbreviations;
use crate::bytecode::*;
use crate::interpreter::*;
use fnv::FnvHashMap;
use std::ops::Range;

type RegisterMap<'a> = [Option<VariableID<'a>>; REGISTERS as usize];
type StackMap<'a> = Vec<Option<VariableID<'a>>>;

#[derive(Default)]
pub(super) struct FunctionScope<'a> {
	id_counter: usize,
	//usize used to order intervals
	//bool is true at last usage / if variable has been used before (false on creation, true elsewhere)
	intervals: (usize, FnvHashMap<VariableID<'a>, Range<usize>>),
	synonyms: FnvHashMap<VariableID<'a>, VariableID<'a>>,
	variables: FnvHashMap<VariableID<'a>, Variable>,
	registers: RegisterMap<'a>,
	stack: StackMap<'a>,
	saved_reg_states: Vec<(RegisterMap<'a>, StackMap<'a>)>,
	//bool is true if variable is new
	recorders: Vec<FnvHashMap<VariableID<'a>, bool>>,
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

	pub fn allocate_var<const N: usize>(
		&mut self,
		info: &[(Option<&'a str>, VarType); N],
	) -> [VariableID<'a>; N] {
		let mut out = [VariableID::Unnamed(0); N];
		for (i, &(name, ty)) in info.iter().enumerate() {
			let id = self.create_id(name);
			for set in &mut self.recorders {
				set.insert(id, true);
			}

			match self.variables.get_mut(&id) {
				Some(var) => {
					var.in_scope = true;
					self.record_usage(&[id]);
				}
				None => {
					self.intervals.1.insert(id, 0..0);
					self.variables.insert(
						id,
						Variable {
							location: Location::Nowhere,
							stack_loc: None,
							ty,
							in_scope: true,
						},
					);
				}
			}
			out[i] = id;
		}
		out
	}

	pub fn record_usage<'l, I: IntoIterator<Item = &'l VariableID<'a>>>(&mut self, ids: I)
	where
		'a: 'l,
	{
		self.intervals.0 += 1;
		let usage_num = self.intervals.0;
		for id in ids {
			let base_id = self.get_base_id(*id);
			let var = self.intervals.1.get_mut(&base_id).unwrap();
			if var.start == 0 {
				var.start = usage_num;
			}
			var.end = usage_num;
			for recorder in &mut self.recorders {
				recorder.entry(base_id).or_insert(false);
			}
		}
	}

	//Used to place variables from an iterator into registers exclusively for performance reasons.
	//Does not return locations of variables.
	pub fn place_from_iter<'l, I: IntoIterator<Item = &'l VariableID<'a>>>(
		&mut self,
		ids: I,
		out: &mut Vec<Instruction>,
	) where
		'a: 'l,
	{
		for (i, var) in ids.into_iter().enumerate() {
			debug_assert!(i < REGISTERS.into());
			self.place_vars(&[*var], out);
		}
	}

	pub fn place_vars<const N: usize>(
		&mut self,
		ids: &[VariableID<'a>; N],
		out: &mut Vec<Instruction>,
	) -> [u8; N] {
		debug_assert!(N < REGISTERS.into());

		for id in ids.iter() {
			let base_id = self.get_base_id(*id);
			//erase vars which died before this one was born, AKA vars which will never be used again
			for (var, interval) in self.intervals.1.iter() {
				if interval.end <= self.intervals.1[&base_id].start && !ids.contains(&var) {
					if let Some(dead_var) = self.variables.remove(&var) {
						self.synonyms.retain(|_, v| v != var);
						if let Location::Register(reg) = dead_var.location {
							self.registers[reg as usize] = None;
						}
						if let Some(addr) = dead_var.stack_loc {
							self.stack[addr] = None;
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
					*reg = Some(base_id);
					self.load_var(base_id, reg_id as u8, out);
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
				self.spill_var(replaced_id, out);
				self.load_var(base_id, reg, out);

				opregs[i] = reg;
				continue 'place_reg;
			} else {
				//No slot found
				unreachable!();
			}
		}

		opregs
	}

	pub fn cleanup(&mut self) {
		let intervals = &mut self.intervals.1;
		self.variables.retain(|id, _| intervals[id].end > 0);
		let variables = &mut self.variables;
		self.synonyms.retain(|_, v| variables.contains_key(&v));
	}

	pub fn get_var(&self, name: &'a str) -> Option<VariableID<'a>> {
		let id = VariableID::Named(name);
		if self.synonyms.contains_key(&id) || self.variables.contains_key(&id) {
			let base = self.get_base_id(id);
			if self.variables[&base].in_scope {
				return Some(base);
			}
		}
		None
	}

	pub fn spill_all<F: FnMut(VariableID) -> bool>(
		&mut self,
		mut predicate: F,
		out: &mut Vec<Instruction>,
	) {
		for _ in 0..self.registers.len() {
			if let Some(id) = self
				.registers
				.iter()
				.filter_map(|&v| v)
				.filter(|&id| predicate(id))
				.max_by_key(|id| self.intervals.1.get(id).map(|range| range.end).unwrap_or(0))
			{
				self.spill_var(id, out);
			}
		}
	}

	pub fn open_scope(&mut self) {
		self.recorders.push(Default::default());
	}

	pub fn close_scope(&mut self) -> FnvHashMap<VariableID<'a>, bool> {
		let scope = self.recorders.pop().expect("Popped empty stack!");
		for var_id in scope
			.iter()
			.filter_map(|(id, new)| if *new { Some(*id) } else { None })
		{
			let base = self.get_base_id(var_id);
			self.variables.get_mut(&base).unwrap().in_scope = false;
		}
		scope
	}

	pub fn save_state(&mut self) {
		for i in 0..REGISTERS as usize {
			if self.registers[i].filter(|id| self.exists(*id)).is_none() {
				self.registers[i] = None;
			}
		}
		self.saved_reg_states
			.push((self.registers, self.stack.clone()));
	}

	pub fn restore_state(&mut self, out: &mut Vec<Instruction>) {
		let (regs, stack) = self.saved_reg_states.pop().expect("Popped unsaved state!");
		for (stack_var, curr_var) in stack.iter().zip(self.stack.iter()) {
			debug_assert_eq!(stack_var, curr_var);
		}

		for (i, goal) in regs.iter().enumerate() {
			match self.registers[i].filter(|id| self.exists(*id)) {
				Some(old_var) => match goal.filter(|&id| self.exists(id)) {
					Some(new_var) if new_var != old_var => {
						self.spill_var(old_var, out);
						self.load_var(new_var, i as u8, out);
					}
					None => self.spill_var(old_var, out),
					_ => (),
				},
				None => {
					if let Some(new_var) = goal.filter(|id| self.exists(*id)) {
						self.load_var(new_var, i as u8, out);
					}
				}
			}
		}
	}

	fn load_var(&mut self, var_id: VariableID<'a>, register: u8, out: &mut Vec<Instruction>) {
		let base_id = self.get_base_id(var_id);
		self.registers[register as usize] = Some(base_id);
		let var = self.variables.get_mut(&base_id).unwrap();
		match var.location {
			Location::Stack(addr) => {
				self.stack[addr as usize] = None;
				if addr as usize == self.stack.len() - 1 {
					out.extend(&[Instruction {
						condition: Condition::Al,
						mnemonic: Mnemonic::Pop,
						args: [register, 0, 0, 0, 0, 0],
					}]);
				} else if abbreviations::fits_in_const_inst(addr) {
					debug_assert!(abbreviations::fits_in_const_inst(addr as Word));
					let [shift, a, b, c, d] = abbreviations::num_as_const_bytes(addr as Word);
					out.extend(&[Instruction {
						condition: Condition::Al,
						mnemonic: Mnemonic::StkLd,
						args: [register, shift, a, b, c, d],
					}]);
				} else {
					unimplemented!(
						"Number of variables in single function must fit within 64 bits!"
					);
				}
			}
			Location::Register(reg) if reg != register => {
				self.registers[reg as usize] = None;
				out.extend(&[Instruction {
					condition: Condition::Al,
					mnemonic: Mnemonic::Mov,
					args: [reg, register, 0, 0, 0, 0],
				}]);
			}
			_ => (),
		}
		var.location = Location::Register(register);
	}

	fn spill_var(&mut self, var_id: VariableID<'a>, out: &mut Vec<Instruction>) {
		let base_id = self.get_base_id(var_id);
		let var = self.variables.get_mut(&base_id).unwrap();
		if let Location::Register(reg) = var.location {
			self.registers[reg as usize] = None;
			let addr = if let Some(addr) = var.stack_loc {
				addr
			} else {
				self.stack
					.iter()
					.enumerate()
					.find_map(|(i, id)| if id.is_none() { Some(i) } else { None })
					.unwrap_or(self.stack.len())
			};
			var.location = Location::Stack(addr as Word);

			if addr == self.stack.len() {
				self.stack.push(Some(base_id));
				out.extend(&[Instruction {
					condition: Condition::Al,
					mnemonic: Mnemonic::Push,
					args: [reg, 0, 0, 0, 0, 0],
				}]);
			} else {
				debug_assert!(abbreviations::fits_in_const_inst(addr as Word));
				self.stack[addr] = Some(base_id);
				let [shift, a, b, c, d] = abbreviations::num_as_const_bytes(addr as Word);
				out.extend(&[Instruction {
					condition: Condition::Al,
					mnemonic: Mnemonic::StkStr,
					args: [reg, shift, a, b, c, d],
				}]);
			}
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

	fn exists(&self, var_id: VariableID<'a>) -> bool {
		let base = *self.synonyms.get(&var_id).unwrap_or(&var_id);
		self.variables.contains_key(&base)
	}

	fn get_base_id(&self, id: VariableID<'a>) -> VariableID<'a> {
		let base = *self.synonyms.get(&id).unwrap_or(&id);
		debug_assert!(self.variables.contains_key(&base));
		base
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
	pub stack_loc: Option<usize>,
	pub ty: VarType,
	pub in_scope: bool,
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
