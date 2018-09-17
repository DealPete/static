use defs::get_word_le;

#[derive(PartialEq)]
enum modes {
    MODE_NORMAL,
    MODE_REM,
    MODE_QUOTE
}

pub fn list(memory_buffer : &[u8]) -> String {
    let mut listing = String::new();
    let load_offset = get_word_le(memory_buffer, 0x42) as usize;
    let code_offset = get_word_le(memory_buffer, 0x48) as usize;
    let mut next_line = load_offset;
    let mut offset = code_offset;

    while true {
        let mut mode = modes::MODE_NORMAL;
        let mut offset = next_line - load_offset + code_offset;
        next_line = get_word_le(memory_buffer, offset) as usize;
        offset += 2;

        if next_line == 0 {
            break;
        }

        let line_number = get_word_le(memory_buffer, offset);
        listing.push_str(format!("{} ", line_number).as_str());
        offset += 2;

        while true {
            let next_byte = memory_buffer[offset];
            if next_byte == 0 {
                break;
            }

            if next_byte >= 0x80  && mode == modes::MODE_NORMAL {
                match expand(next_byte) {
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
                    mode = modes::MODE_REM;
                }
            } else {
                listing.push(memory_buffer[offset] as char);
            }

            if next_byte == 34 {
                if mode == modes::MODE_NORMAL {
                    mode = modes::MODE_QUOTE;
                } else {
                    mode = modes::MODE_NORMAL;
                }
            }
            offset += 1;
        }

        offset += 1;
        listing.push('\n');
    }

    return listing;
}

fn expand(next_byte: u8) -> Option<String> {
    if next_byte > 0xcb {
        return None
    };
    Some(String::from(
        match next_byte {
            0x80 => "END",
            0x81 => "FOR",
            0x82 => "NEXT",
            0x83 => "DATA",
            0x84 => "INPUT#",
            0x85 => "INPUT",
            0x86 => "DIM",
            0x87 => "READ",
            0x88 => "LET",
            0x89 => "GOTO",
            0x8A => "RUN",
            0x8B => "IF",
            0x8C => "RESTORE",
            0x8D => "GOSUB",
            0x8E => "RETURN",
            0x8F => "REM",
            0x90 => "STOP",
            0x91 => "ON",
            0x92 => "WAIT",
            0x93 => "LOAD",
            0x94 => "SAVE",
            0x95 => "VERIFY",
            0x96 => "DEF",
            0x97 => "POKE",
            0x98 => "PRINT#",
            0x99 => "PRINT",
            0x9A => "CONT",
            0x9B => "LIST",
            0x9C => "CLR",
            0x9D => "CMD",
            0x9E => "SYS",
            0x9F => "OPEN",
            0xA0 => "CLOSE",
            0xA1 => "GET",
            0xA2 => "NEW",
            0xA3 => "TAB(",
            0xA4 => "TO",
            0xA5 => "FN",
            0xA6 => "SPC(",
            0xA7 => "THEN",
            0xA8 => "NOT",
            0xA9 => "STEP",
            0xAA => "+",
            0xAB => "−",
            0xAC => "*",
            0xAD => "/",
            0xAE => "↑",
            0xAF => "AND",
            0xB0 => "OR",
            0xB1 => ">",
            0xB2 => "=",
            0xB3 => "<",
            0xB4 => "SGN",
            0xB5 => "INT",
            0xB6 => "ABS",
            0xB7 => "USR",
            0xB8 => "FRE",
            0xB9 => "POS",
            0xBA => "SQR",
            0xBB => "RND",
            0xBC => "LOG",
            0xBD => "EXP",
            0xBE => "COS",
            0xBF => "SIN",
            0xC0 => "TAN",
            0xC1 => "ATN",
            0xC2 => "PEEK",
            0xC3 => "LEN",
            0xC4 => "STR$",
            0xC5 => "VAL",
            0xC6 => "ASC",
            0xC7 => "CHR$",
            0xC8 => "LEFT$",
            0xC9 => "RIGHT$",
            0xCA => "MID$",
            0xCB => "GO",
            _ => panic!("undefined expansion code in BASIC listing.")
        }
    ))
}
