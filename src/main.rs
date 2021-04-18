mod interpreter;
mod bytecode;

use clap::{App, Arg, SubCommand};
use interpreter::*;

fn main() {
	let matches = App::new("fin")
		.version("1.0")
		.author("Ian Graham Martinez <ian.graham28@gmail.com>")
		.about("Interpreter for fin bytecode to which sharkfin compiles.")
		.subcommands(vec![
			SubCommand::with_name("run")
				.about("Runs assembled fin bytecode file.")
				.arg(
					Arg::with_name("INPUT")
						.help("Input file")
						.required(true)
						.index(1),
				),
			SubCommand::with_name("assemble")
				.about("Assembles and save bytecode file to given output destination.")
				.arg(
					Arg::with_name("INPUT")
						.help("Input file")
						.required(true)
						.index(1),
				)
				.arg(
					Arg::with_name("output")
						.short("o")
						.long("output")
						.takes_value(true),
				),
			SubCommand::with_name("disassemble")
				.about("Dissassembles given file and save output to given destination.")
				.arg(
					Arg::with_name("INPUT")
						.help("Input file")
						.required(true)
						.index(1),
				)
				.arg(
					Arg::with_name("output")
						.short("o")
						.long("output")
						.takes_value(true),
				),
		]).get_matches();

	use std::fs::{read_to_string, read, write};
	if let Some(matches) = matches.subcommand_matches("assemble") {
		let input_file_name = matches.value_of("INPUT").unwrap();
		let input_contents = read_to_string(input_file_name).expect("Unable to read file");
		write(
			matches.value_of("output").unwrap_or("a.out"),
			FinProgram::assemble(input_contents).to_raw(),
		)
		.expect("Unable to output file");
	} else if let Some(matches) = matches.subcommand_matches("dissassemble") {
		let input_file_name = matches.value_of("INPUT").unwrap();
		let input_contents = read(input_file_name).expect("Unable to read file");
		write(
			matches.value_of("output").unwrap_or("a.out"),
			FinProgram::new(&input_contents).dissassemble(),
		)
		.expect("Unable to output file");
	} else if let Some(matches) = matches.subcommand_matches("run") {
		let input_file_name = matches.value_of("INPUT").unwrap();
		let input_contents = read(input_file_name).expect("Unable to read file");
		FinProgram::new(&input_contents).execute();
	} else {
		unreachable!();
	}
}
