pub mod analyse;
pub mod defs;
pub mod graph;
mod state_flow_graph;
mod chip8;

fn main() {
    use std::env;
    use std::io::Read;
    use std::fs::File;

	if let Some(arg) = env::args().nth(1) {
		let mut file = File::open(arg).expect(
			"Failed to open file.");
		let mut buffer = Vec::new();
		file.read_to_end(&mut buffer).expect(
			"Failed to read into buffer.");

        let simulator = chip8::sim::Interpreter {};
        let initial_state = chip8::state::State::new(&buffer, 0);

        let result = analyse::simulate_exhaustively(
            &buffer,
            simulator,
            initial_state,
            chip8::arch::Chip8 {}
        );

        match result {
            Ok((state_graph, listing)) => {
                println!("{:?}", state_graph);
                listing.print_instructions()
            },
            Err(error) => println!("{}", error)
        }
    } else {
		println!("usage: dis <file-to-disassemble>");
    }
}
