pub mod graph;
pub mod defs;
mod exhaust;
mod chip8;

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
            chip8::arch::Chip8 {},
            log_type,
        );

        match result {
            Ok((_state_graph, listing)) => {
                listing.print_instructions()
            },
            Err(error) => println!("{}", error)
        }
    } else {
		println!("usage: dis <file-to-disassemble> (<options>)");
    }
}
