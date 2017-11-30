mod defs;
mod dis;
mod dos;
mod emu;
mod graph;

use defs::*;

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

        let entry_point = find_entry(&buffer);
        println!("{}", dis::decode_file(buffer, entry_point));
    } else {
		println!("usage: dis <file-to-disassemble>");
    }
}

// We add 0x10 (load_module_segment_offset), then subtract it in
// the final calculation, to avoid dealing with
// a negative code_segment, since some applications (e.g. DEBUG.EXE)
// have an initial code_segment of 0xFFFE, which pushes CS back
// 0x100 bytes to the beginning of the PSP.
pub fn find_entry(buffer: &Vec<u8>) -> usize {
    if &buffer[0..2] == [0x4d, 0x5a] {
        let load_module_segment_offset = 0x10;
        let header_size = 16*get_word(buffer, 0x08) as usize;
        let instruction_pointer = get_word(buffer, 0x14) as usize;
        let code_segment = get_word(buffer, 0x16).wrapping_add(load_module_segment_offset);
        return header_size + 16*(code_segment as usize) +
            instruction_pointer - 16*(load_module_segment_offset as usize);
    } else {
        return 0;
    }
}
