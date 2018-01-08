use defs::*;
use x86::state::*;
use x86;

pub fn analyse<A: Architecture, C: Context, S: StateTrait, I: InstructionTrait>(mut program: Program<S, I>, architecture: A, context: &C) -> Program<S, I> {
    let entry = program.entry_point();

    program.flow_graph.add_node_at(entry, true);

    let mut live_states = Vec::new();
    live_states.push(program.initial_state.clone());

    while let Some(state) = live_states.pop() {
        let inst_offset = program.next_inst_offset(&state);
        let inst: Instruction = match architecture.decode_instruction(&state.load_module.buffer, inst_offset) {
            Ok(instruction) => instruction,
            Err(err) => {
               println!("{}", program);
               panic!(err);
            }
        };
    
        match architecture.simulate_next_instruction(state, context, inst) {
            Result::End => (),
            Result::State(next_state) => {
                let node_index = program.flow_graph.get_node_index_at(inst_offset)
                    .expect("No node at instruction offset");
                let next_inst_offset = program.next_inst_offset(&next_state);
                program.flow_graph.insert_inst(node_index, next_inst_offset);
                live_states.push(next_state);
            },
            Result::Branch(new_states) => {
                live_states.append(&mut add_new_states(inst_offset, &mut program, new_states, inst.is_return()));
                println!("{}", program.flow_graph);
                println!("{}", program);
            }
        }

        program.instructions.insert(inst_offset, inst);
    }

    program
}

fn add_new_states<S: StateTrait, I: InstructionTrait>(inst_offset: usize, program: &mut Program<S, I>, new_states: Vec<S>, needs_label: bool) -> Vec<S> {
    let mut live_states = Vec::new();

    for new_state in new_states {
        let node_index = program.flow_graph.get_node_index_at(inst_offset)
            .expect("No node at instruction offset");
        let new_inst_offset = program.next_inst_offset(&new_state);
        match program.flow_graph.get_node_index_at(new_inst_offset) {
            None => {
                let new_node_index = program.flow_graph.add_node_at(new_inst_offset, needs_label);
                program.flow_graph.add_edge(node_index, new_node_index, Some(new_state.clone()));
                live_states.push(new_state.clone());
            },
            Some(new_node_index) => {
                match program.flow_graph.split_node_at(new_node_index, new_inst_offset, needs_label) {
                    None => {
                        match program.flow_graph.has_edge(node_index, new_node_index) {
                            false => {
                                program.flow_graph.add_edge(node_index, new_node_index, Some(new_state.clone()));
                                live_states.push(new_state)
                            },
                            true => {
                                let mut new_edge_state = None;
                                let mut edge = program.flow_graph.get_edge_mut(node_index, new_node_index)
                                    .expect("Shouldn't be here");
                                match edge.state {
                                    None => {
                                        new_edge_state = Some(new_state.clone());
                                        live_states.push(new_state);
                                    },
                                    Some(ref edge_state) => {
                                        if !new_state.is_subset(&edge_state) {
                                            new_edge_state = Some(new_state.clone().union(edge_state.clone()));
                                            live_states.push(new_state.clone());
                                        }
                                    }
                                }
                                edge.state = new_edge_state;
                            }
                        }
                    },
                    Some(split_node_index) => {
                        program.flow_graph.add_edge(node_index, split_node_index, Some(new_state.clone()));
                        live_states.push(new_state);
                    }
                }
            }
        }
    };

    live_states
}
