use defs::*;
use graph;
use std::collections::HashMap;

pub fn analyse<S: StateTrait<S>, I: InstructionTrait, A: Architecture<S, I>, C: Context<S, I>>(file_buffer: &Vec<u8>, initial_state: S, architecture: A, context: &C) -> (Analysis<S, I>, Option<String>) {
    let entry_offset = context.entry_offset(&initial_state);

    let mut analysis = Analysis {
        entry_offset: entry_offset,
        highest_offset: entry_offset,
        flow_graph: graph::FlowGraph::new(),
        instructions: HashMap::<usize, I>::new(),
    };

    analysis.flow_graph.add_node_at(entry_offset);
    analysis.flow_graph.add_label(entry_offset);

    let mut live_states = Vec::new();
    live_states.push(initial_state.clone());

    while let Some(state) = live_states.pop() {
        let inst_offset = context.next_inst_offset(&state);
        let inst = match analysis.instructions.get(&inst_offset) {
            None => match architecture.decode_instruction(file_buffer, inst_offset) {
                Ok(instruction) => instruction,
                Err(err) => {
                   panic!(err);
                }
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
                            if analysis.flow_graph.extend_with_state(inst_offset, context.next_inst_offset(&next_state), next_state.clone()) {
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
                    if analysis.flow_graph.extend_with_state(inst_offset, context.next_inst_offset(&new_state), new_state.clone()) {
                        //println!("inst_offset = {}, next_inst_offset = {}, new state:\n{}", inst_offset, context.next_inst_offset(&new_state), new_state);
                        live_states.push(new_state);
                    }
                }
                println!("live states len: {}", live_states.len());
            }
        }

        analysis.instructions.insert(inst_offset, inst);
        if inst_offset > analysis.highest_offset {
            analysis.highest_offset = inst_offset;
        }
    }

    (analysis, None)
}

