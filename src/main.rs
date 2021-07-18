mod abbreviations;
mod bytecode;
mod interpreter;
mod runtime_error;
mod sharkfin;

use clap::{App, Arg, SubCommand};
use interpreter::*;

fn main() {
	let app = App::new("fin")
		.version("1.0")
		.author("Ian Graham Martinez <ian.graham28@gmail.com>")
		.about("Interpreter for Fin bytecode to which SharkFin compiles.")
		.subcommands(vec![
			SubCommand::with_name("run")
				.about("Runs assembled Fin bytecode file.")
				.arg(
					Arg::with_name("INPUT")
						.help("Input file")
						.required(true)
						.index(1),
				),
			SubCommand::with_name("dbg")
				.about("Debug assembled Fin bytecode file.")
				.arg(
					Arg::with_name("INPUT")
						.help("Input file")
						.required(true)
						.index(1),
				),
			SubCommand::with_name("build")
				.about("Compiles SharkFin code into Fin bytecode.")
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
			SubCommand::with_name("asm")
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
			SubCommand::with_name("disas")
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
		]);

	let matches = app.clone().get_matches();

	use std::fs::{read, read_to_string, write};
	if let Some(matches) = matches.subcommand_matches("run") {
		let input_file_name = matches.value_of("INPUT").unwrap();
		let input_contents = read(input_file_name).expect("Unable to read file");
		let program = FinProgram::new(&input_contents);
		program.execute(false);
	} else if let Some(matches) = matches.subcommand_matches("dbg") {
		let input_file_name = matches.value_of("INPUT").unwrap();
		let input_contents = read(input_file_name).expect("Unable to read file");
		let program = FinProgram::new(&input_contents);
		program.execute(true);
	} else if let Some(matches) = matches.subcommand_matches("asm") {
		let input_file_name = matches.value_of("INPUT").unwrap();
		let input_contents = read_to_string(input_file_name).expect("Unable to read file");
		write(
			matches.value_of("output").unwrap_or("out.fin"),
			FinProgram::assemble(&input_contents).to_raw(),
		)
		.expect("Unable to output file");
	} else if let Some(matches) = matches.subcommand_matches("disas") {
		let input_file_name = matches.value_of("INPUT").unwrap();
		let input_contents = read(input_file_name).expect("Unable to read file");
		write(
			matches.value_of("output").unwrap_or("out.fasm"),
			FinProgram::new(&input_contents).dissassemble(),
		)
		.expect("Unable to output file");
	} else if let Some(matches) = matches.subcommand_matches("build") {
		let input_file_name = matches.value_of("INPUT").unwrap();
		let input_contents = read_to_string(input_file_name).expect("Unable to read file");
		let program = sharkfin::compile_sharkfin(&input_contents);
		write(
			matches.value_of("output").unwrap_or("out.fin"),
			program.to_raw(),
		)
		.expect("Unable to output file");
		println!("\n{}", program.dissassemble());
		let s = std::time::Instant::now();
		program.execute(false);
		dbg!(std::time::Instant::now() - s);
	} else {
		app.clone().print_help().expect("");
	}
}
