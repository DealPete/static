use defs::*;
use state::*;
use sim;
use graph;
use std::collections::HashSet;

pub enum SimResult<'program> {
    State(State<'program>),
    Branch(Vec<State<'program>>)
}

fn simulate_program_up_to<'a, 'program, C: Context<'program>>(target_offset: usize, program: &'a Program<'program>, context: &C) -> State<'program> {
    let mut states: Vec<State> = Vec::new();
    let mut final_states = Vec::new();
    states.push(program.initial_state.clone());

    while let Some(state) = states.pop() {
//        println!("{}", state);
        let instruction_offset = program.get_inst_offset(state.next_inst_address());
        if instruction_offset == target_offset {
            final_states.push(state);
            continue;
        }

        let instruction = program.instructions.get(&instruction_offset)
            .expect(format!("no instruction at 0x{:x}", instruction_offset).as_str());

//        println!("\nInstruction: {}", instruction);
        match simulate_instruction(state, context, instruction) {
            SimResult::State(next_state) => states.push(next_state),
            SimResult::Branch(ref mut new_states) => states.append(new_states)
        }
    }

    match final_states.pop() {
        None => panic!("no states reached target instruction"),
        Some(mut final_state) => {
            while let Some(state) = final_states.pop() {
                final_state = final_state.union(state);
            }

            println!("{}", final_state);
            final_state
        }
    }
}

fn branch<'program>(mut state: State<'program>, instruction: &Instruction) -> Vec<State<'program>> {
    let mut new_states = Vec::new();

    let offsets = match instruction.op1 {
        Some(Operand::Imm8(rel)) => vec!(rel as i16),
        Some(Operand::Imm16(rel)) => vec!(rel),
        _ => panic!("unimplemented operand for jump!")
    };

    for offset in offsets {
        let mut new_state = state.clone();
        new_state.ip = new_state.ip.wrapping_add(instruction.length as u16);
        if instruction.mnemonic == Mnemonic::CALL {
            let return_address = Word::new(new_state.ip);
            new_state = sim::push_word(new_state, return_address);
        }
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

fn simulate_instruction<'program, C: Context<'program>>(mut state: State<'program>, context: &C, instruction: &Instruction) -> SimResult<'program> {
    if instruction.mnemonic.is_branch() {
        SimResult::Branch(branch(state, instruction))
    } else if instruction.mnemonic == Mnemonic::INT {
        state.ip = state.ip.wrapping_add(instruction.length as u16);
        SimResult::State(context.simulate_int(state, instruction))
    } else {
        SimResult::State(sim::simulate_instruction(state, context, instruction))
    }
}

pub fn find_indirect_branch_dests<'a, 'program, C: Context<'program>>(offset: usize, program: &'a Program<'program>, context: &C) -> Vec<usize> {
    match program.instructions.get(&offset) {
        None => panic!("asked to simulate program from undiscovered instruction."),
        Some(instruction) => {
            let state = simulate_program_up_to(offset, program, context);
            println!("{}", state.get_combined_word(instruction.unpack_op1()));
            match state.get_combined_word(instruction.unpack_op1()) {
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

fn simulate_node_up_to<'program, C: Context<'program>>(node: &graph::Node, offset: usize, program: &'program Program, context: &C) -> State<'program> {
    let mut next_offset = node.insts[0];
    let mut state = State::new(&program.initial_state.load_module);
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

pub fn reg8_at_is_always_in<'program, C: Context<'program>>(reg: Register, inst_index: usize, values: HashSet<u8>, program: &'program Program<'program>, context: &C) -> bool {
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
