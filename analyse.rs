use defs::*;
use graph::flow_graph::*;
use std::collections::HashMap;
use std::collections::HashSet;

pub fn analyse<I, A, Z>(file_buffer: &Vec<u8>, architecture: A, analyzer: Z, entry_offset: usize) -> Result<FlowGraph<I>, String>
    where I: InstructionTrait,
          A: Architecture<I>,
          Z: AnalyzerTrait<I>
{
    let mut graph = FlowGraph::with_entry(entry_offset);

    let mut new_code = true;
    let mut unexplored = Vec::new();
    let mut indeterminates = Vec::new();
    unexplored.push(entry_offset);

    while new_code {
        while let Some(offset) = unexplored.pop() {
            let inst = match architecture.decode_instruction(file_buffer, offset) {
                Ok(instruction) => instruction,
                Err(err) => return Err(format!("Error decoding offset {:x}:\n{}",
                    offset, err))
            };

            graph.add_inst_to_listing(offset, inst);

            let (offsets, labels, indeterminate) = inst.successors(offset);

            let branching = offsets.len() > 1
                || labels.len() > 0;

            for label in labels {
                graph.add_label(label);
            }

            if indeterminate {
                indeterminates.push(offset);
            }
            
            let mut new_offsets = graph.
                insert_offsets(offset, offsets, branching);
            unexplored.append(&mut new_offsets);
        }

        update_return_statement_targets(&mut graph);

        new_code = false;

        for offset in indeterminates.iter() {
            println!("offset {} is indeterminate", offset);
            let offsets = analyzer.determine_successors(file_buffer, &graph, *offset)?;

            print!("Offsets for {}:", offset);
            for target in offsets.iter() {
                match graph.get_node_at(*target) {
                    None => new_code = true,
                    Some(node) => if !graph.has_edge(
                        graph.get_node_at(*offset).unwrap(), node) {
                        new_code = true
                    }
                }
                print!("{:x}\t", target);
            }

            unexplored.append(&mut graph.insert_offsets(*offset, offsets.iter().cloned().collect(), true));
        }
    }
    
    return Ok(graph);
}

fn update_return_statement_targets<I: InstructionTrait>(graph: &mut FlowGraph<I>) {
    let mut reaching_sets = HashMap::new();
    let mut live_nodes = graph.get_entry_nodes();
    for node in live_nodes.clone() {
        reaching_sets.insert(node, HashSet::new());
    }
    let mut new_edges = Vec::new();

    while let Some(node) = live_nodes.pop() {
        let final_instruction_offset = graph.final_instruction(node).unwrap();
        let final_instruction =
            graph.get_inst(final_instruction_offset).unwrap();

        if final_instruction.is_return() {
            for source in reaching_sets.get(&node).unwrap() {
                let source_final_inst =
                    graph.final_instruction(*source).unwrap();
                let target_offset = source_final_inst +
                    graph.get_inst(source_final_inst).unwrap().length();
                let target_node = graph.get_node_at(target_offset).unwrap();
                new_edges.push((node, target_node));
            }
        }

        for target in graph.get_next_nodes(node) {
            let calls = if final_instruction.is_call() &&
                (graph.initial_instruction(target).unwrap()
                != final_instruction_offset
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
