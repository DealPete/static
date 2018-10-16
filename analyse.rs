use defs::*;
use graph::{FlowGraph};
use std::collections::HashMap;

pub fn recursive_descent<I: InstructionTrait, A: Architecture<I>>(file_buffer: &Vec<u8>, architecture: A, entry_offset: usize) -> Listing<I> {
    let mut listing = Listing::new(entry_offset);

    let mut unexplored = Vec::new();
    unexplored.push(entry_offset);

    while let Some(offset) = unexplored.pop() {
        if let None = listing.instructions.get(&offset) {
            let inst = match architecture.decode_instruction(file_buffer, offset) {
                Ok(instruction) => instruction,
                Err(err) => panic!(err)
            };

            let (addresses, labels, indeterminate) = architecture.successors(inst, offset);

            for address in addresses {
                unexplored.push(address);
            }

            for label in labels {
                listing.add_label(label);
            }

            if indeterminate {
                listing.add_indeterminate(offset);
            }

            if offset > listing.highest_offset {
                listing.highest_offset = offset
            }

            listing.instructions.insert(offset, inst);
        }
    }

    listing
}

pub fn analyse<I: InstructionTrait, A: Architecture<I>>(file_buffer: &Vec<u8>, architecture: A, entry_offset: usize) -> Result<FlowGraph, String> {
    let mut flow_graph = FlowGraph::new();
    let mut instructions = HashMap::<usize, I>::new();

    flow_graph.add_node_at(entry_offset);

    let mut unexplored = Vec::new();
    unexplored.push(entry_offset);

    while let Some(offset) = unexplored.pop() {
        if let None = instructions.get(&offset) {
            let inst = match architecture.decode_instruction(file_buffer, offset) {
                Ok(instruction) => instruction,
                Err(err) => return Err(err)
            };

            instructions.insert(offset, inst);
            let (addresses, _, _) = architecture.successors(inst, offset);
            let node_index = flow_graph.get_node_index_at(offset)
                .expect(format!("No node at instruction offset {}", offset).as_str());

            if addresses.len() == 1 {
                let successor = addresses[0];
                match flow_graph.get_node_index_at(successor) {
                    None => {
                        flow_graph.insert_inst(node_index, successor);
                        unexplored.push(successor);
                    },
                    Some(successor_node_index) => {
                        flow_graph.add_edge(node_index, successor_node_index);
                    }
                }
            } else if addresses.len() > 1 {
                for successor in addresses {
                    match flow_graph.get_node_index_at(successor) {
                        None => {
                            let new_node_index = flow_graph.add_node_at(successor);
                            flow_graph.add_edge(node_index, new_node_index);
                            flow_graph.insert_inst(new_node_index, successor);
                            unexplored.push(successor);
                        },
                        Some(successor_node_index) =>
                            flow_graph.add_edge(node_index, successor_node_index)
                    }
                }
            }
        }
    }

    Ok(flow_graph)
}
