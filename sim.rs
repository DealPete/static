use std::collections::HashMap;
use std::collections::HashSet;
use state::*;
use defs::*;

pub fn simulate_instruction<C: Context>(state: State, context: &C, inst: &Instruction) -> State {
    println!("\nInstruction: {}", inst);
    let mut new_state = match inst.mnemonic {
        Mnemonic::INT => context.simulate_int(state, inst),
        Mnemonic::MOV => simulate_mov(state, inst),
        Mnemonic::POP => simulate_pop(state, inst),
        Mnemonic::PUSH => simulate_push(state, inst),
        _ => panic!("Instruction:\n{}\nunimplemented in simulator.", inst)
    };
    new_state.ip = new_state.ip.wrapping_add(inst.length as u16);
    println!("{}", new_state);

    new_state
}

fn simulate_mov(state: State, inst: &Instruction) -> State {
    let (op1, op2) = (inst.unpack_op1(), inst.unpack_op2());
    match op1 {
        Operand::Register8(_) | Operand::Imm8(_) => {
            let byte = get_byte_op(&state, op2);
            set_byte_op(state, op1, byte)
        },
        _ => {
            let word = get_word_op(&state, op2);
            set_word_op(state, op1, word)
        }
    }
}

fn simulate_pop(state: State, inst: &Instruction) -> State {
    match inst.op1 {
        Some(Operand::Register16(target_reg)) => match state.get_reg16(Register::SP) {
            Word::Undefined => state.set_reg16(target_reg, Word::Undefined),
            _ => panic!("stack not yet implemented")
        },
        _ => panic!("Incorrect operand for PUSH.")
    }
}

fn simulate_push(state: State, inst: &Instruction) -> State {
    if state.get_reg16(Register::SP) == Word::Undefined {
        return state;
    }
    panic!("stack not yet implemented.")
    /*match inst.op1 {
        Some(Operand::Register8(source_reg) => {
        }*/
}
