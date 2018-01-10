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

        let mut index = 0;
        while index < buffer.len() {
            match chip8::dis::decode_instruction(&buffer, index) {
                Ok(instruction) => println!("{}", instruction),
                Err(_) => println!("????")
            }
            index += 2;
        }
    } else {
		println!("usage: dis <file-to-disassemble>");
    }
}
