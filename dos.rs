use defs::*;
use emu;
use std::collections::HashMap;

pub fn should_continue(instruction: &Instruction, program: &Program) -> bool {
    if let Some(Operand::Imm8(func)) = instruction.op1 {
        if func == 0x20 {
            return false;
        } else if func != 0x21 {
            return true;
        }
        return match emu::find_ah(instruction, program) {
            Some(reg_ah) => reg_ah != 0 && reg_ah != 0x4c,
            None =>
                panic!("Couldn't determine if INT at 0x{:x} ends program.", instruction.position)
        };
    }
    panic!("Expected first operand to be imm8");
}
