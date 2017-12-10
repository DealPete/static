use defs::*;
use state::*;
use sim;
use graph;
use std::collections::HashMap;
use std::collections::HashSet;

pub enum SimResult {
    State(State),
    Branch(Vec<State>)
}

fn simulate_program_up_to<C: Context>(load_module_offset: usize, program: &Program, context: &C) -> State {
    let mut state: State;
    let mut states: Vec<State> = Vec::new();
    let mut final_states = Vec::new();
    states.push(program.initial_state.clone());

    while let Some(state) = states.pop() {
        if state.next_inst_address() == program.get_memory_address(load_module_offset) {
            final_states.push(state);
            continue;
        }
        match simulate_next(state, context, &program.instructions) {
            SimResult::State(next_state) => states.push(next_state),
            SimResult::Branch(ref mut new_states) => states.append(new_states)
        }
    }

    let mut final_state = State::new();
    while let Some(state) = final_states.pop() {
        final_state = final_state.union(state);
    }

    final_state
}

fn branch(mut state: State, instruction: &Instruction) -> Vec<State> {
    let mut new_states = Vec::new();

    let offsets = match instruction.op1 {
        Some(Operand::Imm8(rel)) => vec!(rel as i16),
        Some(Operand::Imm16(rel)) => vec!(rel),
        _ => panic!("unimplemented operand for jump!")
    };

    for offset in offsets {
        let mut new_state = state.clone();
        new_state.ip = new_state.ip.wrapping_add(offset as u16);
        new_states.push(new_state);
    }

    match instruction.mnemonic {
        Mnemonic::CALL | Mnemonic::JMP => new_states,
        _ => {
            state.ip = state.ip.wrapping_add(instruction.length as u16);
            new_states.push(state);
            new_states
        }
    }
}

fn simulate_next<C: Context>(state: State, context: &C, instructions: &HashMap<usize, Instruction>) -> SimResult {
    let address = state.next_inst_address();

    match instructions.get(&address) {
        Some(inst) => {
            if inst.mnemonic.is_branch() {
                SimResult::Branch(branch(state, inst))
            } else if inst.mnemonic == Mnemonic::INT {
                SimResult::State(context.simulate_int(state, inst))
            } else {
                SimResult::State(sim::simulate_instruction(state, context, inst))
            }
        },
        None => panic!("no instruction at memory address 0x{:x}", address)
    }
}

pub fn find_indirect_branch_dests<C: Context>(offset: usize, program: &Program, context: &C) -> Vec<usize> {
    match program.instructions.get(&offset) {
        None => panic!("asked to simulate program from undiscovered instruction."),
        Some(instruction) => {
            let state = simulate_program_up_to(offset, program, context);
            match get_combined_word_op(&state, instruction.unpack_op1()) {
                Word::Undefined => panic!("branching to undefined instruction!"),
                Word::AnyValue => panic!("can't branch to any value"),
                Word::Int(set) => {
                    let mut dests = Vec::new();
                    for dest in set {
                        dests.push(program.get_inst_offset(
                            16*(state.cs as usize) + dest as usize));
                    }
                    dests
                },
                _ => panic!("shouldn't be here")
            }
        }
    }
}

fn simulate_node_up_to<C: Context>(node: &graph::Node, offset: usize, program: &Program, context: &C) -> State {
    let mut next_offset = node.insts[0];
    let mut state = State::new();
    state.cs = 0;
    state.ip = next_offset as u16;

    while next_offset != offset {
        let inst = &program.instructions.get(&next_offset)
            .expect(format!("No instruction at 0x{:x}.", next_offset).as_str());
        state = sim::simulate_instruction(state, context, inst);
        next_offset += inst.length as usize;
    }

    state
}

pub fn reg8_at_is_always_in<C: Context>(reg: Register, inst_index: usize, values: HashSet<u8>, program: &Program, context: &C) -> bool {
    match program.flow_graph.get_node_at(inst_index) {
        None => panic!("Asked to search from instruction not in graph!"),
        Some(node) => {
            let state = simulate_node_up_to(node, inst_index, program, context);
            match state.get_reg8(reg) {
                Byte::Undefined => panic!("Couldn't determine value of {:?} at 0x{:x}.", reg, inst_index),
                Byte::AnyValue => false,
                Byte::Int(ref reg_values) => reg_values.is_subset(&values)
            }
        }
    }
}
