use defs::*;
use analyse;

pub fn does_int_end_program(instruction: &Instruction) -> bool {
    if let Some(Operand::Imm8(func)) = instruction.op1 {
        return func == 0x20;
    }
    panic!("Expected first operand of INT to be imm8");
}

pub fn is_int_loose_end(instruction: &Instruction) -> bool {
    if let Some(Operand::Imm8(func)) = instruction.op1 {
        return func == 0x21;
    }
    panic!("Expected first operand of INT to be imm8");
}

pub fn does_int21_always_end_program(inst_index: usize, program: &Program) -> bool {
    let values = vec!(0, 0x4c).into_iter().collect();
    return analyse::reg8_at_is_always_in(Register::AH, inst_index, values, program);
}
