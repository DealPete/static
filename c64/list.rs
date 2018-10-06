use defs::*;

#[derive(PartialEq)]
pub enum Mode {
    Normal,
    Remark,
    Quote
}

pub fn list(memory_buffer : &[u8]) -> String {
    let mut listing = String::new();
    let load_offset = get_word_le(memory_buffer, 0x42) as usize;
    let code_offset = get_word_le(memory_buffer, 0x48) as usize;
    let mut next_line = load_offset;

    loop {
        let mut mode = Mode::Normal;
        let mut offset = next_line - load_offset + code_offset;
        next_line = get_word_le(memory_buffer, offset) as usize;
        offset += 2;

        if next_line == 0 {
            break;
        }

        let line_number = get_word_le(memory_buffer, offset);
        listing.push_str(format!("{} ", line_number).as_str());
        offset += 2;

        loop {
            let next_byte = memory_buffer[offset];
            if next_byte == 0 {
                break;
            }

            if next_byte >= 0x80  && mode == Mode::Normal {
                match keyword(next_byte) {
                    Some(string) => {
                        listing.push_str(string.as_str());
                    },
                    None => {
                        println!("{}", listing);
                        panic!(
    "undefined expansion code {:x} in BASIC listing at 0x{:x}.", next_byte, offset);
                    }
                }
                if next_byte == 0x8f {
                    mode = Mode::Remark;
                }
            } else {
                listing.push(memory_buffer[offset] as char);
            }

            if next_byte == 34 {
                if mode == Mode::Normal {
                    mode = Mode::Quote;
                } else {
                    mode = Mode::Normal;
                }
            }
            offset += 1;
        }

        listing.push('\n');
    }

    return listing;
}
