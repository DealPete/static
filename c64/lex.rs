use defs::*;

pub fn lex(memory_buffer : &[u8]) -> LexProg {
    let load_offset = get_word_le(memory_buffer, 0x42) as usize;
    let code_offset = get_word_le(memory_buffer, 0x48) as usize;
    let mut next_line = load_offset;

    let mut raw_lines = Vec::new();

    loop {
        let mut offset = next_line - load_offset + code_offset;
        next_line = get_word_le(memory_buffer, offset) as usize;
        offset += 2;

        if next_line == 0 {
            break;
        }

        let line_number = get_word_le(memory_buffer, offset);
        let mut line_bytes = Vec::new();

        offset += 2;

        loop {
            let next_byte = memory_buffer[offset];
            if next_byte == 0 {
                break;
            }

            line_bytes.push(next_byte);
            offset += 1;
        }

        raw_lines.push( RawLine {
            number: line_number,
            bytes: line_bytes
        });
    }

    let mut program = LexProg::new();

    for raw_line in raw_lines {
        program.push( LexLine {
            number: raw_line.number,
            lexemes: lex_line( raw_line.bytes )
        });
    }

    program
}            

fn lex_line( bytes: Vec<u8> ) -> Vec<Lexeme> {
    let mut offset = 0;
    let mut lexemes = Vec::new();

    while offset < bytes.len() {
        match bytes[offset] {
            0x00 => panic!("byte 0 shouldn't appear in a raw line."),
            0x20 => offset += 1,
            0x22 => {
                offset += 1;
                let mut string = Vec::new();
                while offset < bytes.len() && bytes[offset] != 0x22 {
                    string.push(bytes[offset]);
                    offset += 1;
                }
                offset += 1;
                lexemes.push(Lexeme::Literal(Literal::String(string)));
            },
            0x28 => {
                lexemes.push(Lexeme::OpenParen);
                offset += 1;
            },
            0x29 => {
                lexemes.push(Lexeme::CloseParen);
                offset += 1;
            },
            0x2c => {
                lexemes.push(Lexeme::Comma);
                offset += 1;
            },
            0x2e | 0x30...0x39 => {
                let mut byte = bytes[offset];
                let mut characteristic: isize = 0;
                let mut mantissa: f32 = 0.0;

                while is_numeral(byte) {
                    characteristic *= 10;
                    characteristic += (bytes[offset] - 0x30) as isize;
                    offset += 1;

                    if offset >= bytes.len() {
                        break;
                    }

                    byte = bytes[offset];
                }

                if byte == 0x2e {
                    offset += 1;
                    let mut magnitude = 0.1;
                    while is_numeral(byte) {
                        mantissa += (bytes[offset] - 0x30) as f32 * magnitude;
                        magnitude *= 0.1;
                        offset += 1;

                        if offset >= bytes.len() {
                            break;
                        }

                        byte = bytes[offset];
                    }
                    lexemes
                        .push(Lexeme::Literal(Literal::
                            Float(characteristic as f32 + mantissa)));
                } else {
                    lexemes
                        .push(Lexeme::Literal(Literal::Int(characteristic)));
                }
            },
            0x3a => {
                lexemes.push(Lexeme::Colon);
                offset += 1;
            },
            0x3b => {
                lexemes.push(Lexeme::SemiColon);
                offset += 1;
            },
            0x41...0x5a => {
                let mut seen_numeral = false;
                let mut byte = bytes[offset];
                let mut name = Vec::new();
                while seen_numeral == false && is_numeral(byte) || is_letter(byte) {
                    if is_numeral(byte) {
                        seen_numeral = true;
                    }

                    name.push(byte);

                    offset += 1;
                    if offset >= bytes.len() {
                        break;
                    }

                    byte = bytes[offset];
                }

                if offset < bytes.len() && byte == 0x24 {
                    lexemes.push(Lexeme::Var(Var::String(String::from_utf8(name)
                        .expect("variable name not utf8"))));
                    offset += 1;
                } else if offset < bytes.len() && byte == 0x25 {
                    lexemes.push(Lexeme::Var(Var::Int(String::from_utf8(name)
                        .expect("variable name not utf8"))));
                    offset += 1;
                } else {
                    lexemes.push(Lexeme::Var(Var::Num(String::from_utf8(name)
                        .expect("variable name not utf8"))));
                }
            },
            0x80...0x8e | 0x90...0xa9 | 0xb5...0xcb => {
                lexemes.push(Lexeme::Keyword(bytes[offset]));
                offset += 1;
            },
            0x8f => {
                lexemes.push(Lexeme::Remark(bytes[offset..].to_vec()));
                break;
            },
            0xaa => {
                lexemes.push(Lexeme::Operator(Op::Add));
                offset += 1;
            },
            0xab => {
                lexemes.push(Lexeme::Operator(Op::Sub));
                offset += 1;
            },
            0xac => {
                lexemes.push(Lexeme::Operator(Op::Mult));
                offset += 1;
            },
            0xad => {
                lexemes.push(Lexeme::Operator(Op::Div));
                offset += 1;
            },
            0xae => {
                lexemes.push(Lexeme::Operator(Op::Exp));
                offset += 1;
            },
            0xaf => {
                lexemes.push(Lexeme::Operator(Op::And));
                offset += 1;
            },
            0xb0 => {
                lexemes.push(Lexeme::Operator(Op::Or));
                offset += 1;
            },
            0xb1 => {
                lexemes.push(Lexeme::Operator(Op::GreaterThan));
                offset += 1;
            },
            0xb2 => {
                lexemes.push(Lexeme::Operator(Op::Equals));
                offset += 1;
            },
            0xb3 => {
                lexemes.push(Lexeme::Operator(Op::LessThan));
                offset += 1;
            },
            _ => {
                lexemes.push(Lexeme::Junk(bytes[offset..].to_vec()));
                break;
            }
        }
    }

    lexemes
}

fn is_numeral(byte: u8) -> bool {
    byte >= 0x30 && byte <= 0x39
}

fn is_letter(byte: u8) -> bool {
    byte >= 0x41 && byte <= 0x5a
}
