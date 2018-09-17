mod analyse;
mod defs;
mod graph;
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

        let context = chip8::arch::Interpreter {};
        let initial_state = chip8::state::State::new(&buffer);

        let analysis = analyse::recursive_descent(
            &buffer,
            initial_state,
            chip8::arch::Chip8 {},
            &context
        );

        analysis.print_instructions();
    } else {
		println!("usage: dis <file-to-disassemble>");
    }
}