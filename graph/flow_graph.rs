use graph::state_flow_graph::StateFlowGraph;
use defs::*;
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
            edges: vec!(Edge::new(0, 1)),
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

    pub fn get_entry_nodes(&self) -> Vec<usize> {
        self.listing.entry_offsets.iter().map(|offset|
            self.get_node_at(*offset).expect("Graph has no entry node!"))
            .collect()
    }

    pub fn get_instructions_at_node(&self, node: usize) -> &[usize] {
        &self.nodes[node].insts
    }

    pub fn get_previous_nodes(&self, node_index: usize) -> Vec<usize> {
        let mut nodes = Vec::new();
        for edge_index in (&self.nodes[node_index]).inbound_edges.iter() {
            nodes.push((&self.edges[*edge_index]).get_from());
        }

        nodes
    }

    pub fn get_next_nodes(&self, node_index: usize) -> Vec<usize> {
        let mut nodes = Vec::new();
        for edge_index in (&self.nodes[node_index]).outbound_edges.iter() {
            nodes.push((&self.edges[*edge_index]).get_to());
        }

        nodes
    }

    pub fn remove_node(&mut self, node_index: usize) {
        let node = self.nodes[node_index].clone();
        for in_edge in node.inbound_edges.iter() {
            for out_edge in node.outbound_edges.iter() {
                self.add_edge(*in_edge, *out_edge);
            }
        }
    }

    pub fn has_edge(&self, source: usize, target: usize) -> bool {
        for out in self.nodes[source].outbound_edges.iter() {
            if self.edges[*out].to == target {
                return true;
            }
        }
        
        false
    }

    pub fn add_edge(&mut self, source: usize, target: usize) {
        if !self.has_edge(source, target) {
            self.nodes[source].outbound_edges.insert(self.edges.len());
            self.nodes[target].inbound_edges.insert(self.edges.len());
            self.edges.push(Edge::new(source, target));
        }
    }
    
    pub fn get_inst(&self, offset: usize) -> Option<&I> {
        self.listing.instructions.get(&offset)
    }

    pub fn add_label(&mut self, offset: usize) {
        self.listing.add_label(offset);
    }

    pub fn initial_instruction(&self, node_index: usize) -> Result<usize, String> {
        if node_index >= self.nodes.len() {
            Err(String::from("Node doesn't exist."))
        } else {
            Ok(*self.nodes[node_index].insts.first().unwrap())
        }
    }

    pub fn final_instruction(&self, node_index: usize) -> Result<usize, String> {
        if node_index >= self.nodes.len() {
            Err(String::from("Node doesn't exist."))
        } else {
            Ok(*self.nodes[node_index].insts.last().unwrap())
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
                self.add_edge(new_node_index, node_index);

                for inst in self.nodes[new_node_index].insts.iter() {
                    self.inst_map.insert(*inst, new_node_index);
                }

                Some(node_index)
            }
        }
    }

    pub fn insert_offsets(&mut self, source: usize, targets: Vec<usize>, branching: bool) -> Vec<usize> {
        let node_index = self.get_node_at(source)
            .expect(format!("No node at instruction offset {:x}", source).as_str());

        let mut unexplored = Vec::new();

        for successor in targets {
            match self.get_node_at(successor) {
                None => {
                    if branching {
                        let new_node_index = self.add_node_at(successor);
                        self.add_edge(node_index, new_node_index);
                        self.add_label(successor);
                    } else {
                        self.insert_offset_at_node_index(successor, node_index);
                    }
                    unexplored.push(successor);
                },
                Some(successor_node_index) =>
                    match self.split_node_at(successor_node_index, successor) {
                        None => self.add_edge(node_index, successor_node_index),
                        Some(new_node_index) =>
                            self.add_edge(node_index, new_node_index)
                    }
            }
        }

        unexplored
    }

    pub fn show_slice(&self, slice: &HashSet<usize>) {
        let mut output = String::from("=== Flow Graph ===\n\n");
        for i in 1..self.nodes.len() {
            output.push_str(format!("=== Node {} [{:x}] ===", i,
                self.initial_instruction(i).unwrap()).as_str());
            let ref node = self.nodes[i];
            let mut has_instructions = false;
            for inst in node.insts.iter() {
                if slice.contains(&inst) {
                    has_instructions = true;
                    output.push_str(
                        format!("\n{}", self.listing.get(*inst).unwrap()).as_str());
                }
            }
            if has_instructions {
                if !node.inbound_edges.is_empty() {
                    let mut inbound = String::new();
                    for edge in node.inbound_edges.iter() {
                        inbound.push_str(format!("{} ", self.edges[*edge].get_from()).as_str());
                    }
                    output.push_str(format!("\nInbound nodes: {}", inbound).as_str());
                }
                if !node.outbound_edges.is_empty() {
                    let mut outbound = String::new();
                    for edge in node.outbound_edges.iter() {
                        outbound.push_str(format!("{} ", self.edges[*edge].get_to()).as_str());
                    }
                    output.push_str(format!("\nOutbound nodes: {}", outbound).as_str());
                }
            }
            output.push_str("\n"); 
        }

        println!("{}", output)
    }
}

#[derive(Clone)]
pub struct Node {
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

struct Edge {
    from: usize,
    to: usize
}

impl Edge {
    fn new(from_node_index: usize, to_node_index: usize) -> Edge {
        Edge {
            from: from_node_index,
            to: to_node_index
        }
    }

    fn get_from(&self) -> usize {
        self.from
    }

    fn get_to(&self) -> usize {
        self.to
    }

    fn set_to(&mut self, to: usize) {
        self.to = to;
    }
}

impl<I: InstructionTrait> fmt::Display for FlowGraph<I> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut output = String::from("=== Flow Graph ===\n\n");
        for i in 1..self.nodes.len() {
            output.push_str(format!("=== Node {} [{:x}] ===\n", i,
                self.initial_instruction(i).unwrap()).as_str());
            let ref node = self.nodes[i];
            for inst in node.insts.iter() {
                output.push_str(
                    format!("{}\n", self.listing.get(*inst).unwrap()).as_str());
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
}

impl<I: InstructionTrait, S: StateTrait<S>> StateFlowGraph<I, S> {
    pub fn from_flow_graph(graph: &FlowGraph<I>, state: S) -> StateFlowGraph<I, S> {
        let mut new_graph = StateFlowGraph::from_listing(graph.listing());
        for index in 0..graph.nodes.len() {
            new_graph.add_node_with_insts(graph.get_instructions_at_node(index));
        }
        for edge in graph.edges.iter() {
            new_graph.add_edge(edge.get_from(), edge.get_to());
        }
        for node in graph.get_entry_nodes() {
            new_graph.add_state(state.clone(), node);
        }

        new_graph
    }
}
