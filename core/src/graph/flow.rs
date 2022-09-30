use crate::defs::main::*;
use std::fmt;
use std::collections::HashMap;
use std::collections::HashSet;

pub struct FlowGraph<I: InstructionTrait> {
    listing: Listing<I>,
    nodes: Vec<Node>,
    edges: Vec<Edge>,
    inst_map: HashMap<usize, usize>,
}

impl<I: InstructionTrait> FlowGraph<I> {
    pub fn new() -> FlowGraph<I> {
        FlowGraph {
            listing: Listing::new(),
            nodes: vec!(Node::new()),
            edges: Vec::new(),
            inst_map: HashMap::new()
        }
    }

    pub fn with_entry(entry_offset: usize) -> FlowGraph<I> {
        FlowGraph {
            listing: Listing::with_entry(entry_offset),
            nodes: vec!(Node {
                outbound_edges: [0].iter().cloned().collect(),
                .. Node::new()
            }, Node {
                insts: vec!(entry_offset),
                inbound_edges: [0].iter().cloned().collect(),
                .. Node::new()
            }),
            edges: vec!(Edge::new(0, 1, EdgeValue::Regular)),
            inst_map: [(entry_offset, 1)].iter().cloned().collect()
        }
    }

    pub fn add_node_at(&mut self, offset: usize) -> usize {
        self.nodes.push(Node::new());
        let node_index = self.nodes.len() - 1;
        self.insert_offset_at_node_index(offset, node_index);

        node_index
    }

    pub fn get_node_at(&self, offset: usize) -> Option<usize> {
        match self.inst_map.get(&offset) {
            None => None,
            Some(index) => Some(*index)
        }
    }

    pub fn get_entry_node(&self) -> usize {
        let entry_offset = self.listing.entry_offset;
        self.get_node_at(entry_offset).expect("Graph has no entry node!")
    }

    pub fn get_instructions_at(&self, node_index: usize) -> &[usize] {
        &self.nodes[node_index].insts
    }

    pub fn get_inbound_edges(&self, node_index: usize) -> Vec<Edge> {
        self.nodes[node_index].inbound_edges.iter().map(
            |&edge_index| self.edges[edge_index]
        ).collect()
    }

    pub fn get_next_nodes(&self, node_index: usize) -> (Vec<usize>, Vec<usize>) {
        let mut next_nodes = Vec::new();
        let mut call_nodes = Vec::new();

        for edge_index in (&self.nodes[node_index]).outbound_edges.iter() {
            let edge = &self.edges[*edge_index];
            match edge.value {
                EdgeValue::Call => call_nodes.push(edge.get_to()),
                _ => next_nodes.push(edge.get_to())
            }
        }

        (next_nodes, call_nodes)
    }

    pub fn remove_node(&mut self, node_index: usize) {
        let node = self.nodes[node_index].clone();
        for in_edge in node.inbound_edges.iter() {
            for out_edge in node.outbound_edges.iter() {
                self.add_edge(*in_edge, *out_edge, EdgeValue::Regular);
            }
        }
    }

    pub fn node_count(&self) -> usize {
        self.nodes.len()
    }

    pub fn has_edge(&self, source: usize, target: usize) -> bool {
        for out in self.nodes[source].outbound_edges.iter() {
            if self.edges[*out].to == target {
                return true;
            }
        }
        
        false
    }

    pub fn add_edge(&mut self, source: usize, target: usize, value: EdgeValue) {
        if !self.has_edge(source, target) {
            self.nodes[source].outbound_edges.insert(self.edges.len());
            self.nodes[target].inbound_edges.insert(self.edges.len());
            self.edges.push(Edge::new(source, target, value));
        }
    }
    
    pub fn get_inst(&self, offset: usize) -> Option<&Meta<I>> {
        self.listing.instructions.get(&offset)
    }

    pub fn add_label(&mut self, offset: usize) {
        self.listing.add_label(offset);
    }

    pub fn initial_instruction(&self, node_index: usize) -> Result<Option<usize>, String> {
        if node_index >= self.nodes.len() {
            Err("Node doesn't exist.".into())
        } else {
            Ok(self.nodes[node_index].insts.first().cloned())
        }
    }

    pub fn final_instruction(&self, node_index: usize) -> Result<Option<usize>, String> {
        if node_index >= self.nodes.len() {
            Err("Node doesn't exist.".into())
        } else {
            Ok(self.nodes[node_index].insts.last().cloned())
        }
    }
        
    pub fn add_inst_to_listing(&mut self, offset: usize, instruction: I) {
        self.listing.add(offset, instruction);
    }

    pub fn listing(&self) -> &Listing<I> {
        &self.listing
    }

    pub fn insert_offset_at_node_index(&mut self, offset: usize, node_index: usize) {
        self.inst_map.insert(offset, node_index);
        self.nodes[node_index].insts.push(offset);
    }

    // This function splits the node with index "node_index" into two nodes;
    // The first node contains all the instructions coming before "inst_offset".
    // The second node contains the instruction at "inst_offset", and all
    // instructions coming after. It also adds an edge between the end of the
    // first node and the beginning of the second.
    //
    // If "inst_offset" points to the first instruction in the node,
    // then there is no need to split the node - we just return "None".
    // Otherwise we return the index of the second node.
    pub fn split_node_at(&mut self, node_index: usize, inst_offset: usize) -> Option<usize> {
        match self.nodes[node_index].insts.iter().position(|&r| r == inst_offset) {
            None => panic!("can't find offset to split at in node {}, instruction 0x{:x}.", node_index, inst_offset),
            Some(index) => {
                if index == 0 {
                    return None;
                }

                let mut new_node = Node::new();
                let new_node_index = self.nodes.len();

                new_node.insts = self.nodes[node_index].insts.split_off(index);
                self.nodes.push(new_node);
                self.nodes.swap(node_index, new_node_index);

                for edge in self.nodes[new_node_index].inbound_edges.iter() {
                    self.edges[*edge].set_to(new_node_index);
                }

                self.nodes[node_index].outbound_edges = self.nodes[new_node_index].outbound_edges.clone();
                self.nodes[new_node_index].outbound_edges.clear();
                self.add_edge(new_node_index, node_index, EdgeValue::Regular);

                for inst in self.nodes[new_node_index].insts.iter() {
                    self.inst_map.insert(*inst, new_node_index);
                }

                Some(node_index)
            }
        }
    }

    pub fn insert_offsets(&mut self, source: usize, targets: Vec<usize>, branching: bool, value: EdgeValue) -> Vec<usize> {
        let node_index = self.get_node_at(source)
            .expect(format!("No node at instruction offset {:x}", source).as_str());

        let mut unexplored = Vec::new();

        for successor in targets {
            match self.get_node_at(successor) {
                None => {
                    if branching {
                        let new_node_index = self.add_node_at(successor);
                        self.add_edge(node_index, new_node_index, value);
                        self.add_label(successor);
                    } else {
                        self.insert_offset_at_node_index(successor, node_index);
                    }
                    unexplored.push(successor);
                },
                Some(successor_node_index) =>
                    if successor_node_index != node_index {
                        match self.split_node_at(successor_node_index, successor) {
                            None => self.add_edge(node_index, successor_node_index, value),
                            Some(new_node_index) =>
                                self.add_edge(node_index, new_node_index, value)
                        }
                    }
            }
        }

        unexplored
    }

    pub fn call_graph(&self) -> Result<CallGraph, String> {
        let mut call_graph = CallGraph::new();
        let mut functions = vec!(0);
        let mut entries = vec!(0);

        while let Some(node) = entries.pop() {
            let mut exits = Vec::new();
            let mut current_function_nodes = vec!(node);
            let mut live_nodes = vec!(node);

            while let Some(live_node) = live_nodes.pop() {
                if let Some(final_offset) = self.final_instruction(live_node)? {
                    let final_instruction = match self.get_inst(final_offset) {
                        None => {
                            println!("{}", self);
                            return Err(format!(
"Node {} lists final instruction {:x}, but this instruction could not be found in the graph"
                                , live_node, final_offset));
                        },
                        Some(inst) => inst.unwrap(),
                    };

                    if final_instruction.is_return() {
                        exits.push(live_node);
                        continue
                    }
                }
                    
                let (targets, calls) = self.get_next_nodes(live_node);

                for target in targets {
                    if !current_function_nodes.contains(&target) {
                        live_nodes.push(target);
                        current_function_nodes.push(target);
                    }
                }

                for call in calls {
                    match functions.iter().position(|&func| func == call) {
                        None => {
                            entries.push(call);
                            functions.push(call);
                            call_graph.add_entry(live_node, functions.len() - 1);
                        },
                        Some(index) => call_graph.add_entry(live_node, index)
                    }
                }
            }

            call_graph.add_function(current_function_nodes, exits);
        }

        Ok(call_graph)
    }
}

#[derive(Clone)]
struct Node {
    pub insts: Vec<usize>,
    pub inbound_edges: HashSet<usize>,
    pub outbound_edges: HashSet<usize>
}

impl Node {
    fn new() -> Node {
        Node {
            insts: Vec::new(),
            inbound_edges: HashSet::new(),
            outbound_edges: HashSet::new()
        }
    }
}

#[derive(Clone, Copy)]
pub enum EdgeValue {
    Regular,
    Call,
    CallSuccessor,
    Return
}

#[derive(Clone, Copy)]
pub struct Edge {
    from: usize,
    to: usize,
    value: EdgeValue
}

impl Edge {
    fn new(from_node_index: usize, to_node_index: usize, value: EdgeValue) -> Edge {
        Edge {
            from: from_node_index,
            to: to_node_index,
            value: value
        }
    }

    pub fn get_from(&self) -> usize {
        self.from
    }

    pub fn get_to(&self) -> usize {
        self.to
    }

    pub fn set_to(&mut self, to: usize) {
        self.to = to;
    }

    pub fn value(&self) -> EdgeValue {
        self.value
    }
}

impl<I: InstructionTrait> fmt::Display for FlowGraph<I> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut output = String::from("=== Flow Graph ===\n\n");
        for i in 1..self.nodes.len() {
            match self.initial_instruction(i).unwrap() {
                None => {
                    output.push_str(format!("=== Node {} [empty] ===\n", i).as_str())
                },
                Some(instruction) => {
                    output.push_str(format!("=== Node {} [{:x}] ===\n", i,
                        instruction).as_str());
                }
            }

            let ref node = self.nodes[i];
            for inst in node.insts.iter() {
                match self.listing.get(*inst) {
                    None => {
                        output.push_str(format!(
                            "error: no instruction at 0x{:x}\n", inst).as_str());
                    },
                    Some(instruction) => {
                        output.push_str(
                            format!("{}\n", instruction).as_str());
                    }
                }
            }

            if !node.inbound_edges.is_empty() {
                let mut inbound = String::new();
                for edge in node.inbound_edges.iter() {
                    inbound.push_str(format!("{} ", self.edges[*edge].get_from()).as_str());
                }
                output.push_str(format!("Inbound nodes: {}\n", inbound).as_str());
            }

            if !node.outbound_edges.is_empty() {
                let mut outbound = String::new();
                for edge in node.outbound_edges.iter() {
                    outbound.push_str(format!("{} ", self.edges[*edge].get_to()).as_str());
                }
                output.push_str(format!("Outbound nodes: {}\n", outbound).as_str());
            }
            output.push_str("\n"); 
        }

        write!(f, "{}", output)
    }
}

pub trait AnalyzerTrait<I: InstructionTrait> {
    fn determine_successors(&self, file_buffer: &[u8], graph: &FlowGraph<I>, offset: usize) -> Result<HashSet<usize>, String>;

    fn written_offsets(&self, file_buffer: &[u8], graph: &FlowGraph<I>, offset: usize) -> Result<HashSet<usize>, String>;
}

#[derive(Debug)]
pub struct Function {
    nodes: Vec<usize>,
    exits: Vec<usize>
}

impl Function {
    pub fn nodes(&self) -> &Vec<usize> {
        &self.nodes
    }
}

#[derive(Debug)]
pub struct CallGraph {
    functions: Vec<Function>,
    entries: HashMap<usize, usize>
}

impl CallGraph {
    fn new() -> CallGraph {
        CallGraph {
            functions: Vec::new(),
            entries: HashMap::new(),
        }
    }

    pub fn pop(&mut self) -> Option<Function> {
        self.functions.pop()
    }

    pub fn functions(&self) -> &Vec<Function> {
        &self.functions
    }

    fn add_function(&mut self, nodes: Vec<usize>, exits: Vec<usize>) -> usize {
        self.functions.push(Function {
            nodes: nodes,
            exits: exits,
        });

        self.functions.len() - 1
    }

    pub fn get_exits(&self, function_index: usize) -> &[usize] {
        &self.functions[function_index].exits
    }

    fn add_entry(&mut self, node_index: usize, target_function: usize) {
        self.entries.insert(node_index, target_function);
    }

    pub fn get_entry(&self, node_index: usize) -> Option<&usize> {
        self.entries.get(&node_index)
    }
}
