mod analyse;
mod defs;
mod graph;
mod x86;

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

        let context = x86::dos::DOS::new(&buffer);

        let listing = analyse::recursive_descent(
            &buffer,
            x86::arch::X86 {},
            context.load_module.file_offset
        );

        listing.print_instructions();
    } else {
		println!("usage: dis <file-to-disassemble>");
    }
}
