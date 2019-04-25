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
        let initial_state = context.initial_state(&buffer);

        let (analysis, error) = analyse::simulate(
            &buffer,
            initial_state,
            x86::arch::X86 {},
            &context
        );

        analysis.print_instructions();
        if let Some(message) = error {
            println!("{}", message);
        }
    } else {
		println!("usage: dis <file-to-disassemble>");
    }
}
