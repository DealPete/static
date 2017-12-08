use defs::*;
use emu::*;
use graph;
use std::collections::HashSet;

fn emulate_program_up_to(offset: usize, program: &Program) -> State {
    return State::new();
}

pub fn find_indirect_branch_dests(offset: usize, program: &Program) -> Vec<usize> {
    match program.instructions.get(&offset) {
        None => panic!("asked to emulate program from undiscovered instruction."),
        Some(instruction) => {
            let state = emulate_program_up_to(offset, program);
            match read_word_op(&state, instruction.unpack_op1()) {
                Word::Undefined => panic!("branching to undefined instruction!"),
                Word::AnyValue => panic!("can't branch to any value"),
                Word::Int(set) => {
                    let mut dests = Vec::new();
                    for dest in set {
                        dests.push(16*(state.cs as usize) + state.ip.wrapping_add(dest) as usize);
                    }
                    dests
                },
                _ => panic!("shouldn't be here")
            }
        }
    }
}

fn emulate_node_up_to(node: &graph::Node, inst_index: usize, program: &Program) -> State {
    let mut state = State::new();
    state.cs = 0;
    state.ip = node.insts[0] as u16;

    while 16*state.cs + state.ip != inst_index as u16 {
        state = emulate_next(state, &program.instructions);
    }

    state
}

pub fn reg8_at_is_always_in(reg: Register, inst_index: usize, values: HashSet<u8>, program: &Program) -> bool {
    match program.flow_graph.get_node_at(inst_index) {
        None => panic!("Asked to search from instruction not in graph!"),
        Some(node) => {
            let state = emulate_node_up_to(node, inst_index, program);
            match state.get_reg8(reg) {
                Byte::Undefined => panic!("Couldn't determine value of {:?} at 0x{:x}.", reg, inst_index),
                Byte::AnyValue => false,
                Byte::Int(ref reg_values) => reg_values.is_subset(&values)
            }
        }
    }
}
