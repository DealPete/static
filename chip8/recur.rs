mod recur;
pub mod defs;
pub mod chip8;

use defs::main::*;

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

        let start_offset = match env::args().nth(2) {
            None => 0,
            Some(offset) => usize::from_str_radix(&offset, 16).unwrap_or(0)
        };

        let listing = recur::recursive_descent(
            &buffer,
            chip8::arch::Chip8 {},
            start_offset
        );

        chip8::arch::Chip8::print_listing(&listing);
    } else {
		println!("usage: dis <file-to-disassemble> [<start-offset>]");
    }
}
