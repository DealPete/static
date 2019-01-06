use defs::main::*;
use graph::flow::*;

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

            let (targets, calls, branching, indeterminate) = inst.successors(offset);

            if indeterminate {
                indeterminates.push(offset);
            }
            
            let valid_successors = targets.into_iter()
                .filter(|&target| target < file_buffer.len()).collect();

            let mut new_offsets = graph.
                insert_offsets(offset, valid_successors, branching, false);
            unexplored.append(&mut new_offsets);

            let mut new_call_offsets = graph.
                insert_offsets(offset, calls, true, true);
            unexplored.append(&mut new_call_offsets);
        }

        let call_graph = graph.construct_call_graph()?;

        new_code = false;

        for offset in indeterminates.iter() {
            println!("offset {} is indeterminate", offset);
            let offsets = analyzer.determine_successors(file_buffer, &graph, &call_graph, *offset)?;

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

            unexplored.append(&mut graph.insert_offsets(*offset, offsets.iter().cloned().collect(), true, false));
        }
    }

    return Ok(graph);
}
