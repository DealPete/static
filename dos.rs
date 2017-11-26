use defs::*;
use emu;
use std::collections::HashMap;

pub fn should_continue(instruction: &Instruction, instructions: &HashMap<usize, Instruction>) -> Option<bool> {
    if let Operand::Imm8(func) = instruction.op1 {
        if func == 0x20 {
            return Some(false);
        } else if func != 0x21 {
            return Some(true);
        }
        return match emu::find_ah(&instruction, &instructions) {
            Some(reg_ah) => Some(reg_ah != 0 && reg_ah != 0x4c),
            None => None
        };
    }
    panic!("Expected first operand to be imm8");
}
