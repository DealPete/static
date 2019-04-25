pub mod graph;
pub mod defs;
pub mod chip8;
mod exhaust;

use defs::main::Architecture;
use chip8::arch::Chip8;

fn main() {
    use std::env;
    use std::io::Read;
    use std::fs::File;

    let mut args = env::args();
    args.next();

	if let Some(file_arg) = args.next() {
		let mut file = File::open(file_arg).expect(
			"Failed to open file.");
		let mut buffer = Vec::new();
		file.read_to_end(&mut buffer).expect(
			"Failed to read into buffer.");

        let simulator = chip8::sim::Interpreter {};
        let initial_state = chip8::state::State::new(&buffer, 0);
        
        let mut log_type = None;

        while let Some(arg) = args.next() {
            if arg == "-v" {
                log_type = Some(exhaust::LogType::Verbose);
            }

            if arg == "-c" {
                log_type = Some(exhaust::LogType::StateCount);
            }
        }

        let result = exhaust::simulate_exhaustively(
            &buffer,
            simulator,
            initial_state,
            Chip8 {},
            log_type,
        );

        match result {
            Ok(state_graph) => {
                Chip8::print_listing(state_graph.listing());
            },
            Err(error) => println!("{}", error)
        }
    } else {
		println!("usage: dis <file-to-disassemble> (<options>)");
    }
}
