use defs::*;
use graph;
use std::collections::HashMap;

pub fn recursive_descent<S: StateTrait<S>, I: InstructionTrait, A: Architecture<S, I>, C: Context<S, I>>(file_buffer: &Vec<u8>, initial_state: S, architecture: A, context: &C) -> Analysis<S, I> {
    let entry_offset = context.entry_offset(&initial_state);

    let mut analysis = Analysis {
        entry_offset: entry_offset,
        highest_offset: entry_offset,
        flow_graph: graph::FlowGraph::new(),
        instructions: HashMap::<usize, I>::new(),
    };

    analysis.flow_graph.add_node_at(entry_offset, initial_state.clone());
    analysis.flow_graph.add_label(entry_offset);

    let mut unexplored = Vec::new();
    unexplored.push(entry_offset);

    while let Some(offset) = unexplored.pop() {
        if let None = analysis.instructions.get(&offset) {
            let inst = match architecture.decode_instruction(file_buffer, offset) {
                Ok(instruction) => instruction,
                Err(err) => panic!(err)
            };

            let (addresses, labels, incomplete) = architecture.naive_successors(inst, offset);

            for address in addresses {
                unexplored.push(address);
            }

            for label in labels {
                analysis.flow_graph.add_label(label);
            }

            if incomplete {
            }

            if offset > analysis.highest_offset {
                analysis.highest_offset = offset
            }

            analysis.instructions.insert(offset, inst);
        }
    }

    analysis
}

pub fn analyse<S: StateTrait<S>, I: InstructionTrait, A: Architecture<S, I>, C: Context<S, I>>(file_buffer: &Vec<u8>, initial_state: S, architecture: A, context: &C) -> (Analysis<S, I>, Option<String>) {
    let entry_offset = context.entry_offset(&initial_state);

    let mut analysis = Analysis {
        entry_offset: entry_offset,
        highest_offset: entry_offset,
        flow_graph: graph::FlowGraph::new(),
        instructions: HashMap::<usize, I>::new(),
    };

    analysis.flow_graph.add_node_at(entry_offset, initial_state.clone());
    analysis.flow_graph.add_label(entry_offset);

    let mut unexplored = Vec::new();
    //let mut unfollowed_branches = Vec::new();
    unexplored.push(entry_offset);

    while let Some(offset) = unexplored.pop() {
        if let None = analysis.instructions.get(&offset) {
            let inst = match architecture.decode_instruction(file_buffer, offset) {
                Ok(instruction) => instruction,
                Err(err) => panic!(err)
            };

            analysis.instructions.insert(offset, inst);
            let (addresses, labels, indeterminate) = architecture.true_successors(&analysis, offset);

            for address in addresses {
                unexplored.push(address);
            }

            for label in labels {
                analysis.flow_graph.add_label(label);
            }

            if indeterminate {
                analysis.flow_graph.add_indeterminate(offset);
            }

            if offset > analysis.highest_offset {
                analysis.highest_offset = offset
            }

            analysis.instructions.insert(offset, inst);
        }
    }

    (analysis, None)
}

pub fn simulate<S: StateTrait<S>, I: InstructionTrait, A: Architecture<S, I>, C: Context<S, I>>(file_buffer: &Vec<u8>, initial_state: S, architecture: A, context: &C) -> (Analysis<S, I>, Option<String>) {
    let entry_offset = context.entry_offset(&initial_state);

    let mut analysis = Analysis {
        entry_offset: entry_offset,
        highest_offset: entry_offset,
        flow_graph: graph::FlowGraph::new(),
        instructions: HashMap::<usize, I>::new(),
    };

    analysis.flow_graph.add_node_at(entry_offset, initial_state.clone());
    analysis.flow_graph.add_label(entry_offset);

    let mut live_states = Vec::new();
    live_states.push(initial_state.clone());

    while let Some(state) = live_states.pop() {
        let inst_offset = context.next_inst_offset(&state);
        let inst = match analysis.instructions.get(&inst_offset) {
            None => match architecture.decode_instruction(file_buffer, inst_offset) {
                Ok(instruction) => instruction,
                Err(err) => panic!(err)
            },
            Some(instruction) => *instruction
        };
    
        match architecture.simulate_next_instruction(state, context, inst) {
            SimResult::End => (),
            SimResult::State(next_state) => {
                let next_inst_offset = context.next_inst_offset(&next_state);
                let node_index = analysis.flow_graph.get_node_index_at(inst_offset)
                    .expect("No node at instruction offset");
                match analysis.flow_graph.get_node_index_at(next_inst_offset) {
                    None => {
                        analysis.flow_graph.insert_inst(node_index, next_inst_offset);
                        live_states.push(next_state);
                    },
                    Some(next_inst_node_index) => {
                        if node_index == next_inst_node_index {
                            live_states.push(next_state);
                        } else {
                            if let Some(next_state) = analysis.flow_graph.extend_with_state(inst_offset, context.next_inst_offset(&next_state), next_state.clone()) {
                                live_states.push(next_state);
                            }
                        }
                    }
                }
            },
            SimResult::Branch((new_states, new_labels)) => {
                for new_label in new_labels {
                    analysis.flow_graph.add_label(new_label);
                }
                for new_state in new_states {
                    if let Some(new_state) = analysis.flow_graph.extend_with_state(inst_offset, context.next_inst_offset(&new_state), new_state.clone()) {
                        live_states.push(new_state);
                    }
                }
            }
        }

        analysis.instructions.insert(inst_offset, inst);
        if inst_offset > analysis.highest_offset {
            analysis.highest_offset = inst_offset;
        }
    }

    (analysis, None)
}

