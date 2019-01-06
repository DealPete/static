use defs::main::*;
use defs::set::*;
use graph::state::StateFlowGraph;
use chip8::sim::Interpreter;
use chip8::state::State;
use chip8::arch::*;
use graph::flow::*;
use std::collections::BTreeSet;
use std::collections::HashSet;
use std::collections::HashMap;

type Slice<'a> = StateFlowGraph<Instruction, State<'a>>;

pub struct Chip8Analyzer {
}

impl Chip8Analyzer {
    fn from_operand(&self, graph: &FlowGraph<Instruction>, offset: usize, operand: Operand) -> Result<Slice, String> {
        match operand {
            Operand::I | Operand::V(_) => (),
            _ => return Err(String::from("Invalid operand to construct slice from."))
        }
            
        let mut current_node = match graph.get_node_at(offset) {
            None => return Err(format!("no node found at offset 0x{:x}", offset)),
            Some(node) => node
        };

        let mut coverage_sets: HashMap<usize, HashSet<Operand>> = HashMap::new();
        let mut coverage: HashSet<Operand> = [operand].iter().cloned().collect();
        let mut insts = graph.get_instructions_at(current_node);
        let mut index = insts.iter().position(|&inst| inst == offset).unwrap();
        let mut live_nodes = Vec::new();

        let mut offsets: BTreeSet<usize> = [offset].iter().cloned().collect();
        let mut entry_nodes = HashSet::new();
        let mut slice_nodes = HashSet::new();
        let mut explored_nodes: HashSet<usize> = [current_node].iter().cloned().collect();

        loop {
            if index == 0 {
                for previous_node in graph.get_previous_nodes(current_node) {
                    if previous_node > 0 {
                        if !coverage_sets.contains_key(&previous_node) {
                            coverage_sets.insert(previous_node, coverage.clone());
                            live_nodes.push(previous_node);
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

            if coverage.len() == 0 {
                entry_nodes.insert(current_node);
            }

            if index == 0 || coverage.len() == 0 {
                match live_nodes.pop() {
                    None => break,
                    Some(live_node) => {
                        coverage = coverage_sets.get(&live_node)
                            .expect("expected coverage set").clone();
                        current_node = live_node;
                        insts = graph.get_instructions_at(current_node);
                        explored_nodes.insert(current_node);
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
                        offsets.insert(inst_offset);
                        slice_nodes.insert(current_node);
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
                            Some(Operand::Numeral(reg)) => {
                                coverage.insert(Operand::V(reg));
                            },
                            Some(Operand::LargeNumeral(reg)) => {
                                coverage.insert(Operand::V(reg));
                            },
                            _ => ()
                        }
                    }
                }
            }
        }

        let mut slice = Slice::new();
        let mut node_map: HashMap<usize, usize> = HashMap::new();

        for node in explored_nodes.iter() {
            node_map.insert(*node, slice.add_empty_node());
        }

        for offset in offsets {
            let node = graph.get_node_at(offset).unwrap();
            slice.insert_offset_at_node_index(offset, node_map[&node]);

            let instruction = graph.get_inst(offset).unwrap();
            slice.add_inst_to_listing(offset, instruction.unwrap());
        }

        for node in explored_nodes.iter() {
            let nodes = graph.get_previous_nodes(*node);

            for previous_node in nodes {
                if explored_nodes.contains(&previous_node) { 
                    slice.add_edge(previous_node, *node);
                }
            }
        }

        for node in explored_nodes {
            if !slice_nodes.contains(&node) {
                slice.remove_node(node_map[&node]);
            }
        }

        for node in entry_nodes {
            slice.add_edge(0, node_map[&node]);
        }

        Ok(slice)
    }

    pub fn possible_I_values(&self, file_buffer: &[u8], graph: &FlowGraph<Instruction>, call_graph: &CallGraph, offset: usize) -> Result<HashSet<u16>, String> {
        let slice = self.from_operand(graph, offset, Operand::I)?;
        let states = simulate_slice(file_buffer, slice, offset)?;

        let value = states.iter().fold(
            Value::Word(Word::from_vec(Vec::new())),
            |acc, state| acc.union(state.get_value(Operand::I))
        );

        match value {
            Value::Byte(_) => Err("expected word".into()),
            Value::Word(word) => match word {
                Word::Undefined => Err("undefined word".into()),
                Word::AnyValue => Err("word can have any value".into()),
                Word::Bytes(_, _) => Err("word can be two bytes".into()),
                Word::Int(set) => Ok(set)
            }
        }
    }
}

impl AnalyzerTrait<Instruction> for Chip8Analyzer {
    fn determine_successors(&self, file_buffer: &[u8], graph: &FlowGraph<Instruction>, call_graph: &CallGraph, offset: usize) -> Result<HashSet<usize>, String> {
        let instruction = match graph.get_inst(offset) {
            None => return Err(format!("no instruction found at offset 0x{:x}", offset)),
            Some(instruction) => instruction.unwrap()
        };

        if let Mnemonic::JP = instruction.mnemonic {
            if let Operand::V(reg) = instruction.unpack_op1() {
                let slice = self.from_operand(graph, offset, Operand::V(reg))?;
                let states = simulate_slice(file_buffer, slice, offset)?;
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

fn simulate_slice<'a>(file_buffer: &'a [u8], mut slice: Slice<'a>, target_offset: usize) -> Result<Vec<State<'a>>, String> {
    let interpreter = Interpreter {};

    let mut final_states = Vec::new();
    let entry_nodes = slice.get_entry_nodes();

    for node in entry_nodes {
        let offset = slice.initial_instruction(node)?;
        let state = State::new(file_buffer, offset);
        slice.add_state(state, node);
    }

    while let Some(mut state) = slice.next_live_state() {
        slice.log_state_count();
/*        match log_type {
            Some(LogType::StateCount) =>
                graph.log_state_count(),
            Some(LogType::Verbose) => {
                println!("{:?}", graph);
                println!("Using state:\n{}", state.debug_string());
            },
            None => ()
        }*/
        let offset = Interpreter::next_inst_offset(&state);
        let node = slice.get_node_at(offset)
            .expect(format!("Expected instruction at offset {}", offset).as_str());
        let insts: Vec<usize> = slice.get_instructions_at(node).iter().cloned().collect();

        for index in 0..insts.len() {
            if insts[index] == target_offset {
                final_states.push(state.clone());
            }

            state.pc = insts[index] as u16 + 0x200;
            
            let inst = match slice.get_inst(insts[index]) {
                None => panic!("no instruction at offset {}", insts[index]),
                Some(inst) => inst.unwrap()
            };
                
            match interpreter.simulate_next_instruction(state, inst) {
                SimResult::Error(state, error) => {
                    return Err(error)
                },
                SimResult::End => break,
                SimResult::State(next_state) => {
                    if index == insts.len() - 1 {
                        for adjacent_node in slice.get_next_nodes(node) {
                            let mut new_state = next_state.clone();
                            let new_offset = slice.initial_instruction(adjacent_node)?;
                            new_state.pc = new_offset as u16 + 0x200;
                            slice.add_state(new_state, adjacent_node);
                        }
                        
                        break;
                    }

                    state = next_state;
                },
                SimResult::Branch(new_states, _) => {
                    for new_state in new_states {
                        if let Some(node) = slice.get_node_at(
                            Interpreter::next_inst_offset(&new_state)) {
                            slice.add_state(new_state, node);
                        }
                    }
                    break;
                }
            }
        }
    }

    Ok(final_states)
}
