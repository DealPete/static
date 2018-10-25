use defs::*;
use graph::flow_graph::*;
use std::collections::HashMap;
use std::collections::HashSet;

pub fn analyse<I, A, S>(file_buffer: &Vec<u8>, architecture: A, slicer: S, entry_offset: usize) -> Result<FlowGraph<I>, String>
    where I: InstructionTrait,
          A: Architecture<I>,
          S: SlicerTrait<I>
{
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

            graph.add_inst_to_listing(offset, inst);

            let (addresses, labels, indeterminate) = inst.successors(offset);

            for label in labels {
                graph.add_label(label);
            }

            if indeterminate {
                indeterminates.push(offset);
            }
            
            let mut new_addresses = graph.insert_addresses(offset, addresses);
            unexplored.append(&mut new_addresses);
        }

        update_return_statement_targets(&mut graph);
        println!("{}", graph);

        let mut new_code = false;

        let static_slice = slicer.create_slice(&graph, 0x12)?;
        //println!("{:?}", static_slice);

        /*for offset in indeterminates {
        }*/

        return Ok(graph);
    }
}

fn update_return_statement_targets<I: InstructionTrait>(graph: &mut FlowGraph<I>) {
    let mut reaching_sets = HashMap::new();
    let entry_node = graph.get_entry_node();
    reaching_sets.insert(entry_node, HashSet::new());
    let mut new_edges = Vec::new();
    let mut live_nodes = vec!(entry_node);

    while let Some(node) = live_nodes.pop() {
       println!("trying node {}", node);
        println!("with reaching set: {:?}", reaching_sets.get(&node).unwrap());
        let final_instruction_address = graph.final_instruction(node).unwrap();
        let final_instruction =
            graph.get_inst(final_instruction_address).unwrap();

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

        for target in graph.get_next_nodes(node) {
            let calls = if final_instruction.is_call() &&
                (target != final_instruction_address
                + final_instruction.length()) {
                [node].iter().cloned().collect()
            } else {
                reaching_sets.get(&node).unwrap().clone()
            };

            if reaching_sets.contains_key(&target) {
                let target_calls = reaching_sets.get(&target)
                    .unwrap().clone();
                if !calls.is_subset(&target_calls) {
                    reaching_sets.insert(target, calls.
                        union(&target_calls).cloned().collect());
                    live_nodes.push(target);
                }
            } else {
                reaching_sets.insert(target, calls.clone());
                live_nodes.push(target);
            }
        }
    }

    for (from, to) in new_edges {
        graph.add_edge(from, to);
    }
}
