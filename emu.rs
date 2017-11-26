use defs::*;
use std::collections::HashMap;

// We assume the load module is at 0x76a - this is what DOSBOX uses as a default.
pub fn find_entry(buffer: &Vec<u8>) -> usize {
    if &buffer[0..2] == [0x4d, 0x5a] {
        let header_size = 16*get_word(buffer, 0x08) as usize;
        let instruction_pointer = get_word(buffer, 0x14) as usize;
        let default_code_segment = 0x75a;
        let code_segment = get_word(buffer, 0x16).wrapping_add(default_code_segment);
        return header_size + 16*(code_segment as usize) + instruction_pointer - 16*(default_code_segment as usize);
    } else {
        return 0;
    }
}

pub fn find_ah(instruction: &Instruction, instructions: &HashMap<usize, Instruction>) -> Option<u8> {
    let mut cur_inst : &Instruction = instruction;
    while let Some(offset) = cur_inst.previous {
        if let Some(inst) = instructions.get(&offset) {
            cur_inst = inst;
            if let Mnemonic::MOV = cur_inst.mnemonic {
                match cur_inst.op1 {
                    Operand::Register(Register::AX) =>
                        if let Operand::Imm16(imm) = cur_inst.op2 {
                            return Some((imm >> 8) as u8);
                        },
                    Operand::Register(Register::AH) =>
                        if let Operand::Imm8(imm) = cur_inst.op2 {
                            return Some(imm as u8);
                        },
                    _ => ()
                }
            }
        }
    }
    return None;
}
