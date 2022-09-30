use crate::defs::main::*;
use crate::graph::state::StateFlowGraph;

pub enum LogType {
    StateCount,
    Verbose
}

pub fn simulate_exhaustively<S, I, A, Z>(file_buffer: &Vec<u8>, simulator: Z, initial_state: S, architecture: A, log_type: Option<LogType>) -> Result<StateFlowGraph<I, S>, String>
    where S: StateTrait<S>,
          I: InstructionTrait,
          A: Architecture<I>,
          Z: SimulatorTrait<S, I>
{
    let entry_offset = Z::next_inst_offset(&initial_state);
    let mut graph = StateFlowGraph::new();
    let node = graph.add_node_at(entry_offset);
    graph.add_state(initial_state, node);

    while let Some(mut state) = graph.next_live_state() {
        match log_type {
            Some(LogType::StateCount) =>
                graph.log_state_count(),
            Some(LogType::Verbose) => {
                println!("{:?}", graph);
                println!("Using state:\n{}", state.debug_string());
            },
            None => ()
        }

        loop {
            let inst_offset = Z::next_inst_offset(&state);
            let inst = match graph.get_inst(inst_offset) {
                None => architecture.decode_instruction(file_buffer, inst_offset)?,
                Some(instruction) => instruction.unwrap()
            };
            
            graph.add_inst_to_listing(inst_offset, inst);
        
            match simulator.simulate_next_instruction(state, inst) {
                SimResult::Error(state, error) => {
                    println!("{}", state.debug_string());
                    return Err(error);
                },
                SimResult::End => break,
                SimResult::State(next_state) => {
                    let next_inst_offset = Z::next_inst_offset(&next_state);
                    let node_index = graph.get_node_at(inst_offset)
                        .expect(format!("No node at instruction offset {}", inst_offset).as_str());
                    match graph.get_node_at(next_inst_offset) {
                        None => {
                            graph.insert_offset_at_node_index(next_inst_offset, node_index);
                            state = next_state;
                        },
                        Some(next_inst_node_index) => {
                            if next_inst_node_index != node_index {
                                graph.extend_with_state(inst_offset, Z::next_inst_offset(&next_state), next_state);
                                break;
                            } else {
                                state = next_state;
                            }
                        }
                    }
                },
                SimResult::Branch(new_states, new_labels) => {
                    for new_state in new_states {
                        graph.extend_with_state(inst_offset, Z::next_inst_offset(&new_state), new_state);
                    }

                    for label in new_labels {
                        graph.add_label(label);
                    }

                    break;
                }
            }
        }
    }

    Ok(graph)
}
