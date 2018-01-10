use defs::*;
use graph;
use std::collections::HashMap;

pub fn analyse<S: StateTrait<S>, I: InstructionTrait, A: Architecture<S, I>, C: Context<S, I>>(file_buffer: Vec<u8>, initial_state: S, architecture: A, context: &C) -> (Analysis<S, I>, Option<String>) {
    let entry_offset = context.entry_offset(&initial_state);

    let mut analysis = Analysis {
        entry_offset: entry_offset,
        highest_offset: entry_offset,
        flow_graph: graph::FlowGraph::new(),
        instructions: HashMap::<usize, I>::new(),
    };

    analysis.flow_graph.add_node_at(entry_offset, true);

    let mut live_states = Vec::new();
    live_states.push(initial_state.clone());

    while let Some(state) = live_states.pop() {
        let inst_offset = context.next_inst_offset(&state);
        let inst = match analysis.instructions.get(&inst_offset) {
            None => match architecture.decode_instruction(&file_buffer, inst_offset) {
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
                let node_index = analysis.flow_graph.get_node_index_at(inst_offset)
                    .expect("No node at instruction offset");
                let next_inst_offset = context.next_inst_offset(&next_state);
                analysis.flow_graph.insert_inst(node_index, next_inst_offset);
                live_states.push(next_state);
            },
            SimResult::Branch(new_states) => {
                for new_state in new_states {
                    if add_new_state(inst_offset, context.next_inst_offset(&new_state), &mut analysis.flow_graph, new_state.clone(), inst.is_return()) {
                        live_states.push(new_state)
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

fn add_new_state<S: StateTrait<S>>(inst_offset: usize, new_inst_offset: usize, flow_graph: &mut graph::FlowGraph<S>, new_state: S, needs_label: bool) -> bool {
    let mut state_live = true;

    let node_index = flow_graph.get_node_index_at(inst_offset)
        .expect("No node at instruction offset");
    match flow_graph.get_node_index_at(new_inst_offset) {
        None => {
            let new_node_index = flow_graph.add_node_at(new_inst_offset, needs_label);
            flow_graph.add_edge(node_index, new_node_index, Some(new_state));
        },
        Some(new_node_index) => {
            match flow_graph.split_node_at(new_node_index, new_inst_offset, needs_label) {
                None => {
                    match flow_graph.has_edge(node_index, new_node_index) {
                        false => {
                            flow_graph.add_edge(node_index, new_node_index, Some(new_state));
                        },
                        true => {
                            let mut new_edge_state = None;
                            let mut edge = flow_graph.get_edge_mut(node_index, new_node_index)
                                .expect("Shouldn't be here");
                            match edge.state {
                                None => {
                                    new_edge_state = Some(new_state);
                                },
                                Some(ref edge_state) => {
                                    if !new_state.is_subset(&edge_state) {
                                        new_edge_state = Some(new_state.union(edge_state.clone()));
                                    } else {
                                        state_live = false;
                                    }
                                }
                            }
                            edge.state = new_edge_state;
                        }
                    }
                },
                Some(split_node_index) => {
                    flow_graph.add_edge(node_index, split_node_index, Some(new_state));
                }
            }
        }
    };

    state_live
}
