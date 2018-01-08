use defs::*;
use chip8::arch::*;

pub fn decode_instruction(buffer: &[u8], offset: usize) -> Result<Instruction, String> {
    let code = get_word_be(buffer, offset);
    let x = ((code & 0x0f00) >> 8) as u8;
    let y = ((code & 0x00f0) >> 4) as u8;
    let kk = (code & 0x00ff) as u8;
    let n = (code & 0x000f) as u8;
    match code {
        0x0000...0x0fff => decode_system_call(kk),
        0x1000...0x1fff => Ok(Instruction {
            op1: Some(Operand::Address(code & 0x0fff)),
            .. Instruction::new(Mnemonic::JP)
        }),
        0x2000...0x2fff => Ok(Instruction {
            op1: Some(Operand::Address(code & 0x0fff)),
            .. Instruction::new(Mnemonic::CALL)
        }),
        0x3000...0x3fff => Ok(Instruction {
            op1: Some(Operand::V(x)),
            op2: Some(Operand::Byte(kk)),
            .. Instruction::new(Mnemonic::SE)
        }),
        0x4000...0x4fff => Ok(Instruction {
            op1: Some(Operand::V(x)),
            op2: Some(Operand::Byte(kk)),
            .. Instruction::new(Mnemonic::SNE)
        }),
        0x5000...0x5fff => Ok(Instruction {
            op1: Some(Operand::V(x)),
            op2: Some(Operand::V(y)),
            .. Instruction::new(Mnemonic::SE)
        }),
        0x6000...0x6fff => Ok(Instruction {
            op1: Some(Operand::V(x)),
            op2: Some(Operand::Byte(kk)),
            .. Instruction::new(Mnemonic::LD)
        }),
        0x7000...0x7fff => Ok(Instruction {
            op1: Some(Operand::V(x)),
            op2: Some(Operand::Byte(kk)),
            .. Instruction::new(Mnemonic::ADD)
        }),
        0x8000...0x8fff => decode_8xyn(x, y, n),
        0x9000...0x9fff => Ok(Instruction {
            op1: Some(Operand::V(x)),
            op2: Some(Operand::V(y)),
            .. Instruction::new(Mnemonic::SNE)
        }),
        0xA000...0xAfff => Ok(Instruction {
            op1: Some(Operand::I),
            op2: Some(Operand::Address(code & 0x0fff)),
            .. Instruction::new(Mnemonic::LD)
        }),
        0xB000...0xBfff => Ok(Instruction {
            op1: Some(Operand::V(0)),
            op2: Some(Operand::Address(code & 0x0fff)),
            .. Instruction::new(Mnemonic::JP)
        }),
        0xC000...0xCfff => Ok(Instruction {
            op1: Some(Operand::V(x)),
            op2: Some(Operand::Byte(kk)),
            .. Instruction::new(Mnemonic::RND)
        }),
        0xD000...0xDfff => Ok(Instruction {
            op1: Some(Operand::V(x)),
            op2: Some(Operand::V(y)),
            op3: Some(Operand::Byte(n)),
            .. Instruction::new(Mnemonic::DRW)
        }),
        0xE000...0xEfff => decode_read_keypress(x, kk),
        0xF000...0xFfff => decode_fxkk(x, kk), 
        _ => panic!(format!("Instruction code not covered by match: {:x}", code))
    }
}

fn decode_system_call(kk: u8) -> Result<Instruction, String> {
    match kk {
        0xe0 => Ok(Instruction::new(Mnemonic::CLS)),
        0xee => Ok(Instruction::new(Mnemonic::RET)),
        _ => Err(format!("Unfamiliar system call {:x}", kk))
    }
}

fn decode_8xyn(x: u8, y: u8, n: u8) -> Result<Instruction, String> {
    match n {
        0x0 => Ok(Instruction {
            op1: Some(Operand::V(x)),
            op2: Some(Operand::V(y)),
            .. Instruction::new(Mnemonic::LD)
        }),
        0x1 => Ok(Instruction {
            op1: Some(Operand::V(x)),
            op2: Some(Operand::V(y)),
            .. Instruction::new(Mnemonic::OR)
        }),
        0x2 => Ok(Instruction {
            op1: Some(Operand::V(x)),
            op2: Some(Operand::V(y)),
            .. Instruction::new(Mnemonic::AND)
        }),
        0x3 => Ok(Instruction {
            op1: Some(Operand::V(x)),
            op2: Some(Operand::V(y)),
            .. Instruction::new(Mnemonic::XOR)
        }),
        0x4 => Ok(Instruction {
            op1: Some(Operand::V(x)),
            op2: Some(Operand::V(y)),
            .. Instruction::new(Mnemonic::ADD)
        }),
        0x5 => Ok(Instruction {
            op1: Some(Operand::V(x)),
            op2: Some(Operand::V(y)),
            .. Instruction::new(Mnemonic::SUB)
        }),
        0x6 => Ok(Instruction {
            op1: Some(Operand::V(x)),
            .. Instruction::new(Mnemonic::SHR)
        }),
        0x7 => Ok(Instruction {
            op1: Some(Operand::V(x)),
            op2: Some(Operand::V(y)),
            .. Instruction::new(Mnemonic::SUBN)
        }),
        0xE => Ok(Instruction {
            op1: Some(Operand::V(x)),
            .. Instruction::new(Mnemonic::SHL)
        }),
        _ => Err(String::from("Unknown code"))
    }
}

fn decode_read_keypress(x: u8, kk: u8) -> Result<Instruction, String> {
    match kk {
        0x9e => Ok(Instruction {
            op1: Some(Operand::V(x)),
            .. Instruction::new(Mnemonic::SKP)
        }),
        0xa1 => Ok(Instruction {
            op1: Some(Operand::V(x)),
            .. Instruction::new(Mnemonic::SKNP)
        }),
        _ => Err(format!("Unknown key press function Dx{:x}", kk))
    }
}

fn decode_fxkk(x: u8, kk: u8) -> Result<Instruction, String> {
    match kk {
        0x07 => Ok(Instruction {
            op1: Some(Operand::V(x)),
            op2: Some(Operand::DelayTimer),
            .. Instruction::new(Mnemonic::LD)
        }),
        0x0a => Ok(Instruction {
            op1: Some(Operand::V(x)),
            op2: Some(Operand::KeyPress),
            .. Instruction::new(Mnemonic::LD)
        }),
        0x15 => Ok(Instruction {
            op1: Some(Operand::DelayTimer),
            op2: Some(Operand::V(x)),
            .. Instruction::new(Mnemonic::LD)
        }),
        0x18 => Ok(Instruction {
            op1: Some(Operand::SoundTimer),
            op2: Some(Operand::V(x)),
            .. Instruction::new(Mnemonic::LD)
        }),
        0x1e => Ok(Instruction {
            op1: Some(Operand::I),
            op2: Some(Operand::V(x)),
            .. Instruction::new(Mnemonic::ADD)
        }),
        0x29 => Ok(Instruction {
            op1: Some(Operand::I),
            op2: Some(Operand::Numeral(x)),
            .. Instruction::new(Mnemonic::LD)
        }),
        0x33 => Ok(Instruction {
            op1: Some(Operand::V(x)),
            .. Instruction::new(Mnemonic::LDBCD)
        }),
        0x55 => Ok(Instruction {
            op1: Some(Operand::Pointer),
            op2: Some(Operand::V(x)),
            .. Instruction::new(Mnemonic::LDPTR)
        }),
        0x65 => Ok(Instruction {
            op1: Some(Operand::V(x)),
            op2: Some(Operand::Pointer),
            .. Instruction::new(Mnemonic::LDPTR)
        }),
        _ => Err(String::from("Unknown code"))
    }
}
