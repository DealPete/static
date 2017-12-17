use defs::*;
use state::*;
use sim;
use std::collections::HashMap;

pub struct Analyser<'a> {
    node_state: HashMap<usize, State<'a>>
}

enum SimResult<'a> {
    State(State<'a>),
    Branch(Vec<State<'a>>)
}

impl<'a> Analyser<'a> {
    pub fn new() -> Analyser<'a> {
        Analyser {
            node_state: HashMap::new()
        }
    }

    pub fn find_undiscovered_code<C: Context<'a>>(&mut self, program: &Program<'a, C>) -> Vec<(State<'a>, usize, usize)> {
        let mut results: Vec<(State<'a>, usize, usize)> = Vec::new();
        let mut states: Vec<State<'a>> = Vec::new();
        states.push(program.initial_state.clone());
        let entry_node_index = program.flow_graph.get_node_index_at(program.entry_point())
            .expect("program entry point not in node graph!");
        self.node_state.insert(entry_node_index, program.initial_state.clone());

        while let Some(state) = states.pop() {
            let inst_index = program.next_inst_offset(&state);
            match self.simulate_next_instruction(state, program) {
                SimResult::State(next_state) => states.push(next_state),
                SimResult::Branch(new_states) => {
                    for new_state in new_states {
                        let new_inst_index = program.next_inst_offset(&new_state);
                        match program.flow_graph.get_node_index_at(new_inst_index) {
                            None => results.push((new_state.clone(), inst_index, new_inst_index)),
                            Some(node_index) => {
                                let mut next_state = new_state.clone();
                                if let Some(node_state) = self.node_state.get(&node_index) {
                                    if next_state.is_subset(node_state) {
                                        continue;
                                    } else {
                                        next_state = new_state.clone().union(node_state.clone());
                                    }
                                }
                                self.node_state.insert(node_index, next_state.clone());
                                states.push(next_state);
                            }
                        }
                    }
                }
            }
        }

        return results;
    }

    fn simulate_next_instruction<C: Context<'a>>(&self, mut state: State<'a>, program: &Program<'a, C>) -> SimResult<'a> {

        let instruction =
            program.instructions.get(&program.next_inst_offset(&state)) 
            .expect("tried to simulate instruction not found in program.");

            println!("{}\n", state);
            println!("{}", instruction);
        if instruction.mnemonic.is_branch() {
            SimResult::Branch(self.branch(state, instruction))
        } else if instruction.mnemonic == Mnemonic::INT {
            state.ip = state.ip.wrapping_add(instruction.length as u16);
            SimResult::State(program.context.simulate_int(state, instruction))
        } else {
            SimResult::State(sim::simulate_instruction(state, &program.context, instruction))
        }
    }

    fn branch(&self, mut state: State<'a>, instruction: &Instruction) -> Vec<State<'a>> {
        let mut new_states = Vec::new();

        if instruction.mnemonic == Mnemonic::RET {
            let (state, word) = sim::pop_word(state);
            match word {
                Word::Undefined => panic!("can't jump to undefined location."),
                Word::AnyValue => panic!("can't jump to unlimited location."),
                Word::Int(ref set) => {
                    for offset in set {
                        let mut new_state = state.clone();
                        new_state.ip = *offset;
                        new_states.push(new_state);
                    }
                },
                _ => panic!("Unsupported return type.")
            };
            return new_states;
        }

        let offsets: Vec<i16> = if instruction.op1 == None {
            Vec::new()
        } else {
            match state.get_value(instruction.unpack_op1()) {
                Value::Word(Word::Undefined) | Value::Byte(Byte::Undefined) =>
                    panic!("can't jump to undefined location."),
                Value::Word(Word::AnyValue) | Value::Byte(Byte::AnyValue) =>
                    panic!("can't jump to unlimited location."),
                Value::Word(Word::Int(set)) => {
                    let mut vec = Vec::new();
                    for word in set {
                        vec.push(word as i16);
                    };
                    vec
                },
                Value::Byte(Byte::Int(set)) => {
                    let mut vec = Vec::new();
                    for byte in set {
                        vec.push(byte as i16);
                    };
                    vec
                },
                _ => panic!("Unsupport jump offset type.")
            }
        };

        let mut cont = false;
        let mut jump = false;

        if instruction.mnemonic == Mnemonic::CALL
            || instruction.mnemonic == Mnemonic::JMP {
                jump = true;
        } else {
            let (flag_to_check, truth_value) = match instruction.mnemonic {
                Mnemonic::JZ => (Flag::Zero, true),
                _ => panic!("jump opeand not yet implemented.")
            };

            match state.get_flag(flag_to_check) {
                Bit::Undefined => panic!("conditional jump for undefined flag."),
                Bit::True => jump = true,
                Bit::False => cont = true,
                Bit::TrueAndFalse => { jump = true; cont = true }
            }
        }

        if jump {
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
        }

        if cont {
            state.ip = state.ip.wrapping_add(instruction.length as u16);
            new_states.push(state);
        }

        new_states
    }
}
