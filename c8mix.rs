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

        let result = analyse::analyse(
            &buffer, chip8::arch::Chip8 {}, 0);

        match result {
            Ok(graph) => println!("{}", graph),
            Err(error) => println!("{}", error)
        }
    } else {
		println!("usage: dis <file-to-disassemble>");
    }
}
