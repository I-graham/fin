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
	free_stack_locs: Vec<usize>,
	saved_states: Vec<(RegisterMap<'a>, StackMap<'a>)>,
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
		debug_assert!(self.variables.contains_key(&var));
		self.synonyms.insert(syn, var);
		syn
	}

	pub fn allocate_vars<const N: usize>(
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
					var.ty = ty;
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
			let base_id = self.get_base(*id).expect("Nonexistent ID");
			let var = self.intervals.1.get_mut(&base_id).unwrap();
			if var.start == 0 {
				var.start = usage_num;
				for recorder in &mut self.recorders {
					recorder.entry(base_id).or_insert(true);
				}
			} else {
				for recorder in &mut self.recorders {
					recorder.entry(base_id).or_insert(false);
				}
			}
			var.end = usage_num;
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
			self.place_vars(&[(*var, false)], out);
		}
	}

	pub fn place_vars<const N: usize>(
		&mut self,
		ids: &[(VariableID<'a>, bool); N],
		out: &mut Vec<Instruction>,
	) -> [u8; N] {
		debug_assert!(N < REGISTERS.into());

		for (id, _) in ids.iter() {
			let base_id = self.get_base(*id).unwrap();
			//erase vars which died before this one was born, AKA vars which will never be used again
			for (&var, interval) in self.intervals.1.iter() {
				if interval.end <= self.intervals.1[&base_id].start
					&& !ids.iter().any(|&(v, _)| self.get_base(v).unwrap() == var)
				{
					if let Some(dead_var) = self.variables.remove(&var) {
						self.synonyms.retain(|_, v| *v != var);
						if let Location::Register(reg) = dead_var.location {
							self.registers[reg as usize] = None;
						}
						if let Some(addr) = dead_var.stack_loc {
							self.stack[addr] = None;
							self.free_stack_locs.push(addr);
						}
					}
				}
			}
		}

		let variables = &mut self.variables;
		self.intervals
			.1
			.retain(|vid, _lifetime| variables.contains_key(vid));

		let mut opregs = [REGISTERS + 1; N];

		'place_reg: for (i, &(id, copy)) in ids.iter().enumerate() {
			let base_id = self.get_base(id).unwrap();
			let location = self.variables[&base_id].location;

			let place_copy_in_reg = |to: u8, opregs: &mut [u8; N], out: &mut Vec<Instruction>| {
				opregs[i] = to;
				match location {
					Location::Register(var_reg) if var_reg != to => out.extend(&[Instruction {
						mnemonic: Mnemonic::Mov,
						args: [var_reg, to, 0, 0, 0, 0],
						..Default::default()
					}]),
					Location::Stack(addr) => {
						let [shift, a, b, c, d] = abbreviations::num_as_const_bytes(addr);
						out.extend(&[Instruction {
							mnemonic: Mnemonic::StkLd,
							args: [to, shift, a, b, c, d],
							..Default::default()
						}])
					}
					_ => (),
				}
			};

			if let Location::Register(reg) = location {
				if !copy {
					opregs[i] = reg;
					continue 'place_reg;
				}
			}

			for (reg_id, reg) in self.registers.iter_mut().enumerate() {
				if reg.is_none() && !opregs.contains(&(reg_id as u8)) {
					if copy {
						place_copy_in_reg(reg_id as u8, &mut opregs, out);
					} else {
						opregs[i] = reg_id as u8;
						*reg = Some(base_id);
						self.load_var(base_id, reg_id as u8, out);
					}
					continue 'place_reg;
				}
			}

			let registers = &self.registers;

			let replaced_id = self
				.intervals
				.1
				.iter()
				.filter_map(|(&var, lifetime)| {
					if !ids.iter().any(|&(v, _)| self.get_base(v).unwrap() == var)
						&& registers.contains(&Some(var))
					{
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
				if copy {
					place_copy_in_reg(reg, &mut opregs, out);
				} else {
					self.load_var(base_id, reg, out);
					opregs[i] = reg;
				}
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

	pub fn var_in_scope(&self, var_id: VariableID<'a>) -> bool {
		self.get_base(var_id)
			.map(|v| self.variables[&v].in_scope)
			.unwrap_or(false)
	}

	pub fn var_type(&self, var_id: VariableID<'a>) -> Option<VarType> {
		self.get_base(var_id).map(|v| self.variables[&v].ty)
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
			let base = self.get_base(var_id).unwrap();
			self.variables.get_mut(&base).unwrap().in_scope = false;
		}
		scope
	}

	pub fn save_state(&mut self) {
		for i in 0..REGISTERS as usize {
			if self.registers[i]
				.filter(|id| self.get_base(*id).is_some())
				.is_none()
			{
				self.registers[i] = None;
			}
		}
		self.saved_states.push((self.registers, self.stack.clone()));
	}

	pub fn restore_state(&mut self, out: &mut Vec<Instruction>) {
		let (regs, stack) = self.saved_states.pop().expect("Popped unsaved state!");
		for (i, stack_var) in stack.iter().enumerate() {
			let exists = |&id: &VariableID<'a>| self.var_in_scope(id);
			let curr_var = self.stack[i].filter(exists);
			match (stack_var.filter(exists), curr_var) {
				(Some(v), None) => {
					self.spill_var(v, out);
					debug_assert!({
						let base = self.get_base(v).unwrap();
						let from_loc = self.variables[&base].location;
						from_loc == Location::Stack(i as Word)
					});
				}
				(None, Some(v)) => {
					let base = self.get_base(v).unwrap();
					if let Some(reg) = regs.iter().enumerate().find_map(|(j, &var)| {
						if var == Some(base) {
							Some(j as u8)
						} else {
							None
						}
					}) {
						self.load_var(v, reg, out);
						debug_assert!({
							let from_loc = self.variables[&base].location;
							from_loc == Location::Register(reg)
						});
					} else {
						self.stack[i] = None;
					}
				}
				(Some(a), Some(b)) if a == b => (),
				(None, None) => (),
				impossible => unreachable!("{:?}", impossible),
			};
		}

		for (i, goal) in regs.iter().enumerate() {
			let exists = |&id: &VariableID<'a>| self.var_in_scope(id);
			let filtered_goal = goal.filter(exists);
			match self.registers[i].filter(exists) {
				Some(old_var) => match filtered_goal {
					Some(new_var) if new_var != old_var => {
						self.spill_var(old_var, out);
						self.load_var(new_var, i as u8, out);
					}
					None => self.spill_var(old_var, out),
					_ => (),
				},
				None => {
					if let Some(new_var) = filtered_goal {
						self.load_var(new_var, i as u8, out);
					}
				}
			}
		}
	}

	pub fn kill_vars<'l, I: IntoIterator<Item = &'l VariableID<'a>>>(&mut self, ids: I)
	where
		'a: 'l,
	{
		for &id in ids {
			if let Some(base) = self.get_base(id) {
				let dead_var = self.variables.get_mut(&base).unwrap();
				if let Location::Register(reg) = dead_var.location {
					self.registers[reg as usize] = None;
				}
				if let Some(addr) = dead_var.stack_loc {
					self.stack[addr] = None;
					self.free_stack_locs.push(addr);
				}
				dead_var.in_scope = false;
				dead_var.stack_loc = None;
				dead_var.location = Location::Nowhere;
			}
		}
	}

	fn load_var(&mut self, var_id: VariableID<'a>, register: u8, out: &mut Vec<Instruction>) {
		let base_id = self.get_base(var_id).unwrap();
		self.registers[register as usize] = Some(base_id);
		let var = self.variables.get_mut(&base_id).unwrap();
		match var.location {
			Location::Stack(addr) => {
				self.stack[addr as usize] = None;
				debug_assert!(abbreviations::fits_in_const_inst(addr as Word));
				let [shift, a, b, c, d] = abbreviations::num_as_const_bytes(addr as Word);
				out.extend(&[Instruction {
					condition: Condition::Al,
					mnemonic: Mnemonic::StkLd,
					args: [register, shift, a, b, c, d],
				}]);
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
		let base_id = self.get_base(var_id).unwrap();
		let var = self.variables.get_mut(&base_id).unwrap();
		if let Location::Register(reg) = var.location {
			self.registers[reg as usize] = None;
			let addr = if let Some(addr) = var.stack_loc {
				addr
			} else if let Some(addr) = self.free_stack_locs.pop() {
				addr
			} else {
				self.stack.len()
			};
			var.location = Location::Stack(addr as Word);
			var.stack_loc = Some(addr);

			if addr == self.stack.len() {
				self.stack.push(Some(base_id));
			}

			debug_assert!(abbreviations::fits_in_const_inst(addr as Word));
			self.stack[addr] = Some(base_id);
			let [shift, a, b, c, d] = abbreviations::num_as_const_bytes(addr as Word);
			out.extend(&[Instruction {
				condition: Condition::Al,
				mnemonic: Mnemonic::StkStr,
				args: [reg, shift, a, b, c, d],
			}]);
		} else {
			unreachable!();
		}
	}

	fn get_base(&self, var_id: VariableID<'a>) -> Option<VariableID<'a>> {
		let base = *self.synonyms.get(&var_id).unwrap_or(&var_id);
		Some(base).filter(|id| self.variables.contains_key(id))
	}

	fn create_id(&mut self, name: Option<&'a str>) -> VariableID<'a> {
		if let Some(varname) = name {
			VariableID::Named(varname)
		} else {
			self.id_counter += 1;
			VariableID::Unnamed(self.id_counter)
		}
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Location {
	Register(u8),
	Stack(Word),
	//No location information is needed/available
	Nowhere,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum VarType {
	Int,
	Float,
}
