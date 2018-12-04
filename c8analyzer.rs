use defs::main::*;
use defs::set::*;
use defs::ir;
use graph::state_flow_graph::StateFlowGraph;
use chip8::sim::Interpreter;
use chip8::state::State;
use chip8::arch::*;
use graph::flow_graph::*;
use std::collections::BTreeSet;
use std::collections::HashSet;
use std::collections::HashMap;

pub struct Searcher {
    call_stack: Vec<usize>,
    current_node: usize,
    origin_node: usize
}

impl Searcher {
    fn new(current_node: usize, origin_node) -> Searcher {
        Searcher {
            call_stack: Vec::new(),
            current_node: current_node,
            origin_node: origin_node
        }
    }
}

pub struct Chip8Analyzer {
}

impl Chip8Analyzer {
    pub fn to_slice(&self, graph: &FlowGraph<Instruction>, call_graph: &CallGraph, offset: usize, operand: Operand) -> Result<FlowGraph<ir::Instruction>, String> {
        match operand {
            Operand::I | Operand::V(_) => (),
            _ => return Err(String::from("Invalid operand to construct slice from."))
        }
            
        let mut current_node = match graph.get_node_at(offset) {
            None => return Err(format!("no node found at offset 0x{:x}", offset)),
            Some(node) => node
        };

        let mut inst_slice = BTreeSet::new();
        let mut slice_graph = FlowStateGraph::with_entry();
        slice_graph.add_node();

        let mut current_origin = current_node;
        let mut node_map = [(current_node, 1)].iter().cloned().collect();
        let mut coverage_sets: HashMap<usize, HashSet<Operand>> = HashMap::new();
        let mut searchers = Vec::new();
        let mut coverage: HashSet<Operand> = [operand].iter().cloned().collect();
        let mut insts = graph.get_instructions_at(current_node);
        let mut index = insts.iter().position(|&inst| inst == offset).unwrap();

        loop {
            if index == 0 {
                let previous_nodes = graph.get_previous_nodes(current_node);

                for previous_node in ... {
                    if previous_node > 0 {
                        if !coverage_sets.contains_key(&previous_node) {
                            coverage_sets.insert(previous_node, coverage.clone());
                            searchers.push(Searcher::new(previous_node, current_origin));
                        } else {
                            let node_coverage = coverage_sets.get_mut(&previous_node)
                                .unwrap();
                            if !coverage.is_subset(&node_coverage) {
                                *node_coverage = coverage.union(node_coverage).cloned().collect();
                            }
                        }
                    }
                }
            }

            if index == 0 || coverage.len() == 0 {
                match searchers.pop() {
                    None => break,
                    Some(searcher) => {
                        coverage = coverage_sets.get(&searcher.current_node)
                            .expect("expected coverage set").clone();
                        current_node = searcher.current_node;
                        current_origin = searcher.origin_node;
                        insts = graph.get_instructions_at(current_node);
                        index = insts.len() - 1;
                    }
                }
            } else {
                index -= 1;
            }

            let inst_offset = insts[index];
            let inst = graph.get_inst(inst_offset).expect(format!(
                    "graph doesn't contain index {}", inst_offset).as_str()).unwrap();
            match inst.mnemonic {
                Mnemonic::CLS | Mnemonic::LDBCD | Mnemonic::SCD | Mnemonic::JP
                | Mnemonic::SCR | Mnemonic::SCL | Mnemonic::LOW | Mnemonic::HIGH
                | Mnemonic::RET | Mnemonic::CALL | Mnemonic::DRW | Mnemonic::SKP
                | Mnemonic::SKNP => (),
                Mnemonic::EXIT =>
                    panic!("slicer shouldn't encounter EXIT instruction."),
                Mnemonic::OR | Mnemonic::AND | Mnemonic::XOR | Mnemonic::ADD
                | Mnemonic::SUB | Mnemonic::SUBN | Mnemonic::SHL | Mnemonic::SHR
                | Mnemonic::LD | Mnemonic::LDPTR | Mnemonic::RND | Mnemonic::SE
                | Mnemonic::SNE => {
                    let operand = inst.op1.expect("expected operand 1");

                    if coverage.contains(&operand) {
                        let slice_graph_current_origin = node_map.get(&current_origin)
                            .expect("exprected current origin to be in node map");
                        match node_map.get(&current_node) {
                            Some(node) => {
                                slice_graph.add_edge(node, slice_graph_current_origin);
                            }
                            None => {
                                let new_node = slice_graph.add_node();
                                node_map.insert(current_node, new_node);
                                slice_graph.add_edge(new_node, slice_graph_current_origin);
                            }
                        }

                        inst_slice.insert(inst_offset);
                        current_origin = current_node;
                        if inst.mnemonic == Mnemonic::LD
                            || inst.mnemonic == Mnemonic::RND
                            || inst.mnemonic == Mnemonic::LDPTR
                            || inst.mnemonic == Mnemonic::SHL
                            || inst.mnemonic == Mnemonic::SHR {
                            coverage.remove(&operand);
                        }
                        match inst.op2 {
                            None => panic!("inst should have op2"),
                            Some(Operand::Pointer) | Some(Operand::I) =>
                                { coverage.insert(Operand::I); },
                            Some(Operand::Address(addr)) => {
                                coverage.insert(Operand::Address(addr));
                            },
                            Some(Operand::V(reg)) => {
                                coverage.insert(Operand::V(reg));
                            },
                            _ => ()
                        }
                    }
                }
            }
        }

        for offset in inst_slice {
            let graph_node = graph.get_node_at(offset)
                .expect("inst should exist at node.");
            let graph_slice_node = node_map.get(&graph_node)
                .expect("node_map should contain graph_node.");
            let instruction = graph.get_inst(&graph_node)
                .expect("inst should exist in listing.");
            graph_slice.add_inst_to_listing(graph_slice_node, *instruction);
            graph_slice.insert_offset_at_node_index(
                offset, graph_slice_node);
        }

        Ok(slice_graph)
    }
}

impl AnalyzerTrait<Instruction> for Chip8Analyzer {
    fn determine_successors(&self, file_buffer: &[u8], graph: &FlowGraph<Instruction>, call_graph: &CallGraph, offset: usize) -> Result<HashSet<usize>, String> {
        let instruction = match graph.get_inst(offset) {
            None => return Err(format!("no instruction found at offset 0x{:x}", offset)),
            Some(instruction) => instruction
        };

        if let Mnemonic::JP = instruction.mnemonic {
            if let Operand::V(reg) = instruction.unpack_op1() {
                let slice = self.to_slice(graph, call_graph, offset, Operand::V(reg))?;
                let states = simulate_slice(file_buffer, graph, slice, offset)?;
                let mut offsets = HashSet::new();
                for state in states {
                    match state.get_value(Operand::V(reg)) {
                        Value::Word(_) => return Err("expected byte".into()),
                        Value::Byte(byte) => match byte {
                            Byte::Undefined => return Err("undefined byte".into()),
                            Byte::AnyValue => return Err("byte can have any value".into()),
                            Byte::Int(set) => {
                                match instruction.op2 {
                                    Some(Operand::Address(address)) =>
                                        for byte in set {
                                            offsets.insert(address as usize
                                                + byte as usize - 0x200);
                                        },
                                    _ => return Err("op2 should be byte".into())
                                }
                            }
                        }
                    }
                }

                return Ok(offsets);
            }
        }

        Err(format!("not sure how to determine successors for instruction {}", instruction))
    }
}

fn simulate_slice<'a>(file_buffer: &'a [u8], flow_graph: &FlowGraph<Instruction>, slice: HashSet<usize>, target_offset: usize) -> Result<Vec<State<'a>>, String> {
    let interpreter = Interpreter {};
    let initial_state = State::new(file_buffer, 0x0);
    let mut graph = StateFlowGraph::from_flow_graph(flow_graph, initial_state);
//    graph.show_slice(&slice);
    graph.reduce_to_slice(slice.clone());
    graph.show_slice(&slice);
    let mut final_states = Vec::new();

    while let Some(mut state) = graph.next_live_state() {
        graph.log_state_count();
/*        match log_type {
            Some(LogType::StateCount) =>
                graph.log_state_count(),
            Some(LogType::Verbose) => {
                println!("{:?}", graph);
                println!("Using state:\n{}", state.debug_string());
            },
            None => ()
        }*/
        let node = graph.get_node_at(Interpreter::next_inst_offset(&state))
            .expect("Couldn't find node");
        let insts: Vec<usize> = graph.get_instructions_at(node).iter().cloned().collect();
        let mut index = 0;

        loop {
            let inst_offset = Interpreter::next_inst_offset(&state);

            if slice.contains(&inst_offset) {
                let inst = match graph.get_inst(inst_offset) {
                    None => panic!("no instruction at offset {}", inst_offset),
                    Some(inst) => inst.clone()
                };
                
                match interpreter.simulate_next_instruction(state, inst) {
                    SimResult::Error(state, error) => {
                        println!("{}", state.debug_string());
                        return Err(error)
                    },
                    SimResult::End => break,
                    SimResult::State(next_state) => state = next_state,
                    SimResult::Branch(new_states, _) => {
                        for new_state in new_states {
                            if let Some(node) = graph.get_node_at(
                                Interpreter::next_inst_offset(&new_state)) {
                                graph.add_state(new_state, node);
                            }
                        }
                        break;
                    }
                }
            } else {
                if index == insts.len() - 1 {
                    for adjacent_node in graph.get_next_nodes(node) {
                        let mut new_state = state.clone();
                        let new_offset = graph.initial_instruction(adjacent_node)?;
                        new_state.pc = new_offset as u16 + 0x200;
                        graph.add_state(new_state, adjacent_node);
                    }
                    break;
                }

                state.pc = insts[index+1] as u16 + 0x200;
            }

            index += 1;

            if index >= insts.len() {
                let node = graph.get_node_at(
                    Interpreter::next_inst_offset(&state))
                    .expect("No node at instruction offset");
                graph.add_state(state, node);
                break;
            }
                
            if insts[index] == target_offset {
                final_states.push(state.clone());
            }
        }
    }

    Ok(final_states)
}
