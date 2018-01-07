use defs::*;
use chip8::arch::*;

pub fn decode_instruction(buffer: &[u8], offset: usize) -> Result<Instruction, String> {
    let code = get_word_be(buffer, offset);
    let x = (code & 0x0f00 >> 8) as u8;
    let y = (code & 0x00f0 >> 4) as u8;
    let kk = (code & 0x00ff) as u8;
    match code {
        0x0000...0x0fff => decode_system_call(code),
        0x1000...0x1fff => Ok(Instruction {
            op1: Some(Operand::Address(code & 0x0fff),
            .. Instruction::new(Mnemonic::JP)
        }),
        0x2000...0x2fff => Ok(Instruction {
            op1: Some(Operand::Address(code & 0x0fff),
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
        0x8000...0x8fff => decode_8xyn(code),
        0x9000...0x9fff => Ok(Instruction {
            op1: Some(Operand::V(x)),
            op2: Some(Operand::V(y)),
            .. Instruction::new(Mnemonic::SNE)
        }),
        0xA000...0xAfff => Ok(Instruction {
            op1: Some(Operand::I),
            op2: Some(Operand::Address(code & 0x0fff),
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
            op3: Some(Operand::Byte((code & 0x000f) as u8)),
            .. Instruction::new(Mnemonic::DRW)
        }),
        0xE000...0xEfff => decode_read_keypress(code),
        0xF000...0xFfff => decode_fxkk(code) 
    }
}

fn decode_8xyn(code: u16) -> Result<Instruction, String> {
    let x = (code & 0x0f00 >> 8) as u8;
    let y = (code & 0x00f0 >> 4) as u8;
    match code & 0x000f {
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
        _ => Err(format!("Unknown code {:x}", code))
    }
}

fn decode_read_keypress(code: u16) -> Result<Instruction, String> {
    let x = (code & 0x0f00 >> 8) as u8;
    match code & 0x00ff {
        0x009e => Ok(Instruction {
            op1: Some(Operand::V(x)),
            .. Instruction::new(Mnemonic::SKP)
        }),
        0x00a1 => Ok(Instruction {
            op1: Some(Operand::V(x)),
            .. Instruction::new(Mnemonic::SKNP)
        }),
        _ => Err(format!("Unknown code {:x}", code))
    }
}

fn decode_fxkk(code: u16) -> Result<Instruction, String> {
    let x = (code & 0x0f00 >> 8) as u8;
    match code & 0x00ff {
        0x0007 => Ok(Instruction {
            op1: Some(Operand::V(x)),
            op2: Some(Operand::DelayTimer),
            .. Instruction::new(Mnemonic::LD)
        }),
        0x000a => Ok(Instruction {
            op1: Some(Operand::V(x)),
            op2: Some(Operand::KeyPress),
            .. Instruction::new(Mnemonic::LD)
        }),
        0x0015 => Ok(Instruction {
            op1: Some(Operand::DelayTimer),
            op2: Some(Operand::V(x)),
            .. Instruction::new(Mnemonic::LD)
        }),
        0x0018 => Ok(Instruction {
            op1: Some(Operand::SoundTimer),
            op2: Some(Operand::V(x)),
            .. Instruction::new(Mnemonic::LD)
        }),
        0x001e => Ok(Instruction {
            op1: Some(Operand::I),
            op2: Some(Operand::V(x)),
            .. Instruction::new(Mnemonic::ADD)
        }),
        0x0029 => Ok(Instruction {
            op1: Some(Operand::I),
            op2: Some(Operand::Numeral(x)),
            .. Instruction::new(Mnemonic::LD)
        }),
        0x0033 => Ok(Instruction {
            op1: Some(Operand::V(x)),
            .. Instruction::new(Mnemonic::LDBCD)
        }),
        0x0055 => Ok(Instruction {
            op1: Some(Operand::Pointer),
            op2: Some(Operand::V(x)),
            .. Instruction::new(Mnemonic::LDPTR)
        }),
        0x0065 => Ok(Instruction {
            op1: Some(Operand::V(x)),
            op2: Some(Operand::Pointer),
            .. Instruction::new(Mnemonic::LDPTR)
        }),
        _ => Err(format!("Unknown code {:x}", code))
    }
}
