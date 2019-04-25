use defs::main::*;
use defs::set::*;
use graph::state::*;
use chip8::sim::Interpreter;
use chip8::state::State;
use chip8::arch::*;
use graph::flow::*;
use std::collections::HashSet;
use std::collections::HashMap;

pub struct Analyzer {
    pub old_shift_behavior: bool
}

impl<'a> Analyzer {
    fn from_operand(&self, graph: &FlowGraph<Instruction>, offset: usize, operand: Operand) -> Result<Slice<Instruction, State<'a>>, String> {
        println!("{}", graph);

        match operand {
            Operand::I | Operand::V(_) => (),
            _ => return Err(String::from("Invalid operand to construct slice from."))
        }
            
        let mut current_node = match graph.get_node_at(offset) {
            None => return Err(format!("no node found at offset 0x{:x}", offset)),
            Some(node) => node
        };

        let call_graph = graph.call_graph()?;
        let mut coverage_sets: HashMap<usize, HashSet<Operand>> = HashMap::new();
        let mut coverage: HashSet<Operand> = [operand].iter().cloned().collect();
        let mut insts = graph.get_instructions_at(current_node);
        let mut index = insts.iter().position(|&inst| inst == offset).unwrap();
        let mut live_nodes = Vec::new();

        let mut offsets: HashSet<usize> = [offset].iter().cloned().collect();
        let mut entry_nodes = HashSet::new();
        let mut explored_nodes: HashSet<usize> = [current_node].iter().cloned().collect();

        loop {
            if coverage.len() == 0 {
                entry_nodes.insert(current_node);
            } else if index == 0 {
                let mut inbounds = Vec::new();

                for edge in graph.get_inbound_edges(current_node) {
                    match edge.value() {
                        EdgeValue::CallSuccessor => { 
                            let function = call_graph.get_entry(edge.get_from())
                                .expect("Node should have call graph entry");
                            
                            for exit in call_graph.get_exits(*function) {
                                inbounds.push(*exit);
                            }
                        },
                        _ => inbounds.push(edge.get_from())
                    }
                }

                for previous_node in inbounds {
                    if previous_node == 0 {
                        entry_nodes.insert(current_node);
                    } else {
                        if !coverage_sets.contains_key(&previous_node) {
                            coverage_sets.insert(previous_node, coverage.clone());
                            live_nodes.push(previous_node);
                        } else {
                            let node_coverage = coverage_sets.get_mut(&previous_node)
                                .unwrap();
                            if !coverage.is_subset(&node_coverage) {
                                *node_coverage = coverage.union(node_coverage).cloned().collect();
                                live_nodes.push(previous_node);
                            }
                        }
                    }
                }
            }

            println!("Lives nodes: {:?} ({})", live_nodes, current_node);
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

            println!("coverage set: {:?}", coverage);
            println!("considering offset 0x{:x}: instruction {}", inst_offset, inst);

            if coverage.contains(&Operand::V(0xf)) {
                match inst.mnemonic {
                    Mnemonic::DRW => {
                        offsets.insert(inst_offset);
                        coverage.remove(&Operand::V(0xf));
                    },
                    Mnemonic::SUB | Mnemonic::SUBN => {
                        offsets.insert(inst_offset);
                        coverage.remove(&Operand::V(0xf));
                        coverage.insert(inst.op1.expect("expected op1"));
                        coverage.insert(inst.op2.expect("expected op2"));
                    },
                    Mnemonic::ADD => {
                        offsets.insert(inst_offset);
                        coverage.remove(&Operand::V(0xf));
                        coverage.insert(inst.op1.expect("expected op1"));
                        match inst.op2 {
                            None => {
                                return Err(String::from("expected op2 for ADD instruction"));
                            },
                            Some(Operand::V(x)) => { coverage.insert(Operand::V(x)); }
                            _ => ()
                        }
                    },
                    Mnemonic::SHL | Mnemonic::SHR => {
                        offsets.insert(inst_offset);
                        coverage.remove(&Operand::V(0xf));
                        coverage.insert(inst.op1.expect("expected op1"));
                    },
                    _ => ()
                }
            }

            match inst.mnemonic {
                Mnemonic::CLS | Mnemonic::LDBCD | Mnemonic::SCD | Mnemonic::JP
                | Mnemonic::SCR | Mnemonic::SCL | Mnemonic::LOW | Mnemonic::HIGH
                | Mnemonic::SKP | Mnemonic::SKNP | Mnemonic::DRW => (),
                Mnemonic::CALL | Mnemonic::RET => { offsets.insert(inst_offset); },
                Mnemonic::EXIT =>
                    panic!("slicer shouldn't encounter EXIT instruction."),
                Mnemonic::LDPTR => {
                    match inst.op1 {
                        Some(Operand::V(reg)) => {
                            for index in 0..=reg {
                                if coverage.contains(&Operand::V(index)) {
                                    coverage.insert(Operand::I);
                                    coverage.insert(Operand::Pointer);
                                    offsets.insert(inst_offset);
                                }

                                coverage.remove(&Operand::V(index));
                            }

                        },
                        Some(Operand::Pointer) => {
                            if coverage.contains(&Operand::Pointer) {
                                coverage.insert(Operand::I);
                                offsets.insert(inst_offset);

                                match inst.op2 {
                                    Some(Operand::V(reg)) => {
                                        for index in 0..=reg {
                                            coverage.insert(Operand::V(index));
                                        }
                                    },
                                    _ => return Err(String::from("expected operand 2 to be register"))
                                }
                            }
                        },
                        _ => panic!("improper operand for LDPTR.")
                    }
                },
                Mnemonic::SE | Mnemonic::SNE => {
                    offsets.insert(inst_offset);
                    coverage.insert(inst.op1.expect("expected op1"));
                    match inst.op2 {
                        None => {
                            return Err(String::from("exported op2"));
                        },
                        Some(Operand::V(x)) => { coverage.insert(Operand::V(x)); },
                        _ => ()
                    }
                    let op1 = inst.op1.expect("expected op1");
                    if coverage.contains(&op1) {
                        offsets.insert(inst_offset);
                        match inst.op2 {
                            None => {
                                return Err(String::from("exported op2"));
                            },
                            Some(Operand::V(x)) => { coverage.insert(Operand::V(x)); },
                            _ => ()
                        }
                    }
                },
                Mnemonic::OR | Mnemonic::AND | Mnemonic::XOR | Mnemonic::ADD
                | Mnemonic::SUB | Mnemonic::SUBN | Mnemonic::SHL | Mnemonic::SHR
                | Mnemonic::LD | Mnemonic::RND => {
                    let operand = inst.op1.expect("expected operand 1");

                    if coverage.contains(&operand) {
                        offsets.insert(inst_offset);
                        if inst.mnemonic == Mnemonic::LD
                            || inst.mnemonic == Mnemonic::RND
                            || inst.mnemonic == Mnemonic::SHL
                            || inst.mnemonic == Mnemonic::SHR {
                            coverage.remove(&operand);
                        }

                        match inst.op2 {
                            None => panic!("inst should have op2"),
                            Some(Operand::Pointer) | Some(Operand::I) =>
                                { coverage.insert(Operand::I); },
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

        let mut slice = Slice::with_offsets(offsets.clone());
        let mut empty_nodes = vec!(false);

        let mut node_map: HashMap<usize, usize> = HashMap::new();
        
        for node in explored_nodes.iter() {
            let mut empty = true;
            let insts = graph.get_instructions_at(*node);
            let index = slice.add_node_with_insts(insts);

            node_map.insert(*node, index);

            for offset in insts.iter() {
                if offsets.contains(offset) {
                    empty = false;
                }

                let inst = graph.get_inst(*offset).unwrap().unwrap();
                slice.add_inst_to_listing(*offset, inst);
            }

            empty_nodes.push(empty);
        }

        for node in explored_nodes.iter() {
            let nodes = graph.get_inbound_edges(*node);

            for edge in nodes {
                if explored_nodes.contains(&edge.get_from()) { 
                    let from = node_map[&edge.get_from()];
                    let to = node_map[node];

                    slice.add_edge(from, to)?;

                    match edge.value() {
                        EdgeValue::Regular => (),
                        _ => {
                            empty_nodes[from] = false;
                            empty_nodes[to] = false;
                        }
                    }
                }
            }
        }

        for node in entry_nodes.iter() {
            slice.add_edge(0, node_map[&node])?;
        }

        for index in 0..empty_nodes.len() {
            if empty_nodes[index] {
                slice.remove_node(index)?
            }
        }

        Ok(slice)
    }

    fn simulate_slice(&self, file_buffer: &'a [u8], mut slice: Slice<Instruction, State<'a>>, target_offset: usize) -> Result<Vec<State<'a>>, String> {
        println!("{}", slice);
        let interpreter = Interpreter {
            old_shift_behavior: self.old_shift_behavior
        };

        let mut final_states = Vec::new();
        let entry_nodes = slice.get_entry_nodes();

        for node in entry_nodes {
            let offset = match slice.initial_instruction(node) {
                Err(err) => {
                    println!("Error with Slice:\n\n{}", slice);
                    return Err(err);
                },
                Ok(offset) => offset
            };

            let state = State::new(file_buffer, offset);
            slice.add_state(state, node);
        }

        while let Some(mut state) = slice.next_live_state() {
            slice.log_state_count();
            let offset = Interpreter::next_inst_offset(&state);
            let node = slice.get_node_at(offset)
                .expect(format!("Expected instruction at offset {}", offset).as_str());

            let insts: Vec<usize> = slice.get_instructions_at(node)
                .iter()
                .cloned()
                .filter(|offset| slice.contains(offset)) 
                .collect();

            let mut index = 0;

            if insts.len() == 0 {
                for adjacent_node in slice.get_next_nodes(node) {
                    let mut new_state = state.clone();
                    let offset = slice.initial_instruction(adjacent_node)?;
                    new_state.pc = offset as u16 + 0x200;
                    slice.add_state(new_state, adjacent_node);
                }
            }

            while index < insts.len() {
                let offset = insts[index];
                state.pc = offset as u16 + 0x200;
                
                if offset == target_offset {
                    println!("{}", state);
                    final_states.push(state.clone());
                }

                let inst = match slice.get_inst(offset) {
                    None => panic!("no instruction at offset {}", offset),
                    Some(inst) => inst.unwrap()
                };
                    
                match interpreter.simulate_next_instruction(state, inst) {
                    SimResult::State(next_state) => {
                        state = next_state;
                    },
                    SimResult::Error(_, error) => {
                        return Err(error)
                    },
                    SimResult::End => break,
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

                index += 1;

                if index == insts.len() {
                    for adjacent_node in slice.get_next_nodes(node) {
                        let mut new_state = state.clone();
                        let offset = slice.initial_instruction(adjacent_node)?;
                        new_state.pc = offset as u16 + 0x200;
                        slice.add_state(new_state, adjacent_node);
                    }
                }
            }
        }

        Ok(final_states)
    }
}

impl AnalyzerTrait<Instruction> for Analyzer {
    fn written_offsets(&self, file_buffer: &[u8], graph: &FlowGraph<Instruction>, offset: usize) -> Result<HashSet<usize>, String> {
        let slice = match self.from_operand(graph, offset, Operand::I) {
            Err(err) => {
                println!("Error generating slice:");
                return Err(err);
            },
            Ok(slice) => slice
        };

        let states = match self.simulate_slice(file_buffer, slice, offset) {
            Err(err) => {
                println!("Error simulating slice:");
                return Err(err);
            },
            Ok(states) => states
        };

        let value = states.iter().fold(
            Value::Word(Word::from_vec(Vec::new())),
            |acc, state| {
                let instruction = graph
                    .get_inst(state.pc as usize - 0x200)
                    .unwrap()
                    .unwrap();

                let base = state.get_value(Operand::I);

                let range = match instruction.op2 {
                    Some(Operand::V(reg)) => reg,
                    _ => panic!("expected op2")
                };

                (0..=range).fold(acc, |acc, addend|
                    acc.union(base.clone().plus(addend)))
            }
        );

        match value {
            Value::Byte(_) => Err("expected word".into()),
            Value::Word(word) => match word {
                Word::Undefined => Err("undefined word".into()),
                Word::AnyValue => Err("word can have any value".into()),
                Word::Bytes(_, _) => Err("word can be two bytes".into()),
                Word::Int(set) => Ok(set.iter().map(|&value| (value - 0x200) as usize).collect())
            }
        }
    }

    fn determine_successors(&self, file_buffer: &[u8], graph: &FlowGraph<Instruction>, offset: usize) -> Result<HashSet<usize>, String> {
        let instruction = match graph.get_inst(offset) {
            None => return Err(format!("no instruction found at offset 0x{:x}", offset)),
            Some(instruction) => instruction.unwrap()
        };

        if let Mnemonic::JP = instruction.mnemonic {
            if let Operand::V(reg) = instruction.unpack_op1() {
                let slice = self.from_operand(graph, offset, Operand::V(reg))?;
                let states = self.simulate_slice(file_buffer, slice, offset)?;
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
