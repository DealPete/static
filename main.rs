mod dis;
mod dos;
mod defs;
mod emu;

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

        let entry_point = emu::find_entry(&buffer);
        println!("{}", dis::decode_file(buffer, entry_point));
    } else {
		println!("usage: dis <file-to-disassemble>");
    }
}
