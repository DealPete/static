pub mod defs;
mod lex;
mod parse;

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

        let lexical_program = lex::lex(&buffer);
        let parsed_program = parse::parse(lexical_program);

        println!("{:?}", parsed_program);
    } else {
		println!("usage: c64list <file-to-analyse>");
    }
}
