pub mod analyse;
pub mod defs;
pub mod graph;
pub mod chip8;
mod slicer;

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
            &buffer, chip8::arch::Chip8 {},
            slicer::Chip8Slicer {}, 0);

        match result {
            Ok(graph) => {
                let slicer = slicer::Chip8Slicer {};
                let slice = slicer.from_operand(&graph, 0x12, chip8::arch::Operand::V(0));
                println!("{:?}", slice);
            },
            Err(error) => println!("{}", error)
        }
    } else {
		println!("usage: dis <file-to-disassemble>");
    }
}
