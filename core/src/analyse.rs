use crate::defs::main::*;
use crate::defs::range::*;
use crate::graph::flow::*;

pub fn analyse<I, A, Z>(file_buffer: &Vec<u8>, architecture: A, analyzer: &Z, entry_offset: usize) -> Result<FlowGraph<I>, String>
    where I: InstructionTrait,
          A: Architecture<I>,
          Z: AnalyzerTrait<I>
{
    let mut graph = FlowGraph::with_entry(entry_offset);
    let mut code_offsets = USize::from_value(entry_offset);
    let mut written_offsets = USize::new();

    let mut new_code = true;
    let mut unexplored = Vec::new();
    let mut indeterminates = Vec::new();
    unexplored.push(entry_offset);

    while new_code {
        while let Some(offset) = unexplored.pop() {
            // change this for non-chip8
            if written_offsets.contains(offset) || written_offsets.contains(offset + 1) {
                return Err(format!("Code is polymorphic at offset {}", offset));
            }

            let inst = match architecture.decode_instruction(file_buffer, offset) {
                Ok(instruction) => instruction,
                Err(err) => return Err(format!("Error decoding offset {:x}:\n{}",
                    offset, err))
            };

            graph.add_inst_to_listing(offset, inst);
            code_offsets = code_offsets.insert_range(offset, offset + inst.length() - 1);

            let (targets, calls, branching, indeterminate) = inst.successors(offset);

            let add_to_indeterminates = indeterminate || inst.writes_memory();

            if add_to_indeterminates {
                indeterminates.push(offset);
            } else {
                let valid_successors = targets.into_iter()
                    .filter(|&target| target < file_buffer.len()).collect();

                let edge_value = if inst.is_call() {
                    EdgeValue::CallSuccessor
                } else {
                    EdgeValue::Regular
                };

                let mut new_offsets = graph.
                    insert_offsets(offset, valid_successors, branching, edge_value);
                unexplored.append(&mut new_offsets);

                let mut new_call_offsets = graph.
                    insert_offsets(offset, calls, true, EdgeValue::Call);
                unexplored.append(&mut new_call_offsets);
            }
        }

        new_code = false;
        let mut index = 0;

        while !new_code && index < indeterminates.len() {
            let offset = indeterminates[index];
            let mut unexplored_offsets;
            let inst = graph.get_inst(offset).unwrap().unwrap();

            if inst.writes_memory() {
                for written_offset in analyzer.written_offsets(file_buffer, &graph, offset)? {
                    if code_offsets.contains(written_offset) {
                        return Err(format!("Instruction at offset 0x{:x} writes to code offset 0x{:x}",
                            offset, written_offset));
                    }

                    print!("writing 0x{:x} to get:", written_offset);
                    written_offsets = written_offsets.insert(written_offset);
                    println!(" {}", written_offsets.display());
                }

                let (successors, _, branching, _) = inst.successors(offset);

                for target in successors.iter() {
                    match graph.get_node_at(*target) {
                        None => new_code = true,
                        Some(target_node) => {
                            let node = graph.get_node_at(offset).unwrap();
                            if !(node == target_node || graph.has_edge(node, target_node)) {
                                new_code = true;
                            }
                        }
                    }
                }

                unexplored_offsets = graph.
                    insert_offsets(offset, successors, branching, EdgeValue::Regular);
            } else {
                let successors = analyzer.determine_successors(file_buffer, &graph, offset)?;

                for target in successors.iter() {
                    match graph.get_node_at(*target) {
                        None => new_code = true,
                        Some(node) => if !graph.has_edge(
                            graph.get_node_at(offset).unwrap(), node) {
                            new_code = true
                        }
                    }
                }

                unexplored_offsets = graph.
                    insert_offsets(offset, successors.iter().cloned().collect(), true, EdgeValue::Regular);
            }

            unexplored.append(&mut unexplored_offsets);
            index += 1;
        }
    }

    println!("code offsets: {}", code_offsets.display());
    println!("written offsets: {}", written_offsets.display());
    return Ok(graph);
}
