pub mod defs;
mod list;

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

        let listing = list::list(&buffer);

        println!("{}", listing);
    } else {
		println!("usage: c64list <file-to-list>");
    }
}
