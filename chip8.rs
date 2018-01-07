mod defs;
mod chip8 {
    pub mod arch;
    pub mod dis;
}

use defs::*;
use chip8::arch::*;

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

    let mut index = 0;
    while index < buffer.len() {
        match decode_instruction(&buffer, index) {
            Ok(instruction) => println!("{}", instruction),
            Err(_) => println!("????")
        }
        index += 2;
    }
}
