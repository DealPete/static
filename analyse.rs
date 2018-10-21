use defs::*;
use graph::flow_graph::FlowGraph;
use std::collections::HashMap;
use std::collections::HashSet;

pub fn analyse<I: InstructionTrait, A: Architecture<I>>(file_buffer: &Vec<u8>, architecture: A, entry_offset: usize) -> Result<FlowGraph<I>, String> {
    let mut graph = FlowGraph::new(entry_offset);

    let mut unexplored = Vec::new();
    let mut indeterminates = Vec::new();
    unexplored.push(entry_offset);

    loop {
        while let Some(offset) = unexplored.pop() {
            let inst = match architecture.decode_instruction(file_buffer, offset) {
                Ok(instruction) => instruction,
                Err(err) => return Err(err)
            };

            graph.insert_inst(offset, inst);

            let (addresses, labels, indeterminate) =
                architecture.successors(inst, offset);

            for label in labels {
                graph.add_label(label);
            }

            if indeterminate {
                indeterminates.push(offset);
            }
            
            unexplored.append(&mut graph.insert_addresses(offset, addresses));
        }

        update_return_statement_targets(&mut graph);

        let mut new_code = false;

        for offset in indeterminates {
        }

        return Ok(graph);
    }
}

fn update_return_statement_targets<I: InstructionTrait>(graph: &mut FlowGraph<I>) {
    let mut reaching_sets = HashMap::new();
    reaching_sets.insert(0, HashSet::new());
    let mut new_edges = Vec::new();
    let mut live_nodes = vec!(0);

    while let Some(node) = live_nodes.pop() {
        let final_instruction =
            graph.get_inst(graph.final_instruction(node).unwrap()).unwrap();

        if final_instruction.is_return() {
            for source in reaching_sets.get(&node).unwrap() {
                let source_final_inst =
                    graph.final_instruction(*source).unwrap();
                let target_address = source_final_inst +
                    graph.get_inst(source_final_inst).unwrap().length();
                let target_node = graph.get_node_at(target_address).unwrap();
                new_edges.push((node, target_node));
            }
        }

        for adjacent_node in graph.get_adjacent_nodes(node) {
            let node_calls = if final_instruction.is_call() {
                let mut set = HashSet::new();
                set.insert(node);
                set
            } else {
                reaching_sets.get(&node).unwrap().clone()
            };

            if reaching_sets.contains_key(&adjacent_node) {
                let adjacent_node_calls = reaching_sets.get(&adjacent_node)
                    .unwrap().clone();
                if !node_calls.is_subset(&adjacent_node_calls) {
                    reaching_sets.insert(adjacent_node, node_calls.
                        union(&adjacent_node_calls).cloned().collect());
                    live_nodes.push(adjacent_node);
                }
            } else {
                reaching_sets.insert(adjacent_node, node_calls);
                live_nodes.push(adjacent_node);
            }
        }
    }

    for (from, to) in new_edges {
        graph.add_edge(from, to);
    }
}
