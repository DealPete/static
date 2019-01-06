//use std::time::{SystemTime, UNIX_EPOCH};

use defs::main::*;
use std::fmt;
use std::collections::HashMap;
use std::collections::HashSet;

pub struct StateFlowGraph<I: InstructionTrait, S: StateTrait<S>> {
    listing: Listing<I>,
    nodes: Vec<Node>,
    edges: Vec<Edge>,
    states: Vec<S>,
    inst_map: HashMap<usize, usize>,
    state_map: HashMap<usize, usize>,
    live_states: Vec<usize>
}

impl<I: InstructionTrait, S: StateTrait<S>> StateFlowGraph<I, S> {
    pub fn new() -> StateFlowGraph<I, S> {
        StateFlowGraph {
            listing: Listing::new(),
            nodes: vec!(Node::new()),
            edges: Vec::new(),
            states: Vec::new(),
            inst_map: HashMap::new(),
            state_map: HashMap::new(),
            live_states: Vec::new()
        }
    }

    pub fn from_listing(listing: Listing<I>) -> StateFlowGraph<I, S> {
        StateFlowGraph {
            listing: listing,
            nodes: Vec::new(),
            edges: Vec::new(),
            states: Vec::new(),
            inst_map: HashMap::new(),
            state_map: HashMap::new(),
            live_states: Vec::new()
        }
    }

    pub fn add_empty_node(&mut self) -> usize {
        self.nodes.push(Node::new());
        self.nodes.len() - 1
    }

    pub fn add_node_at(&mut self, offset: usize) -> usize {
        self.nodes.push(Node::new());
        let node_index = self.nodes.len() - 1;
        self.insert_offset_at_node_index(offset, node_index);

        node_index
    }

    pub fn add_node_with_insts(&mut self, offsets: &[usize]) -> usize {
        self.nodes.push(Node::new());
        let node_index = self.nodes.len() - 1;
        for offset in offsets {
            self.insert_offset_at_node_index(*offset, node_index);
        }

        node_index
    }

    pub fn add_node_with_state_at(&mut self, state: S, offset: usize) -> usize {
        let state_index = self.states.len();
        let node_index = self.nodes.len();

        let mut node = Node::from_state(offset, state_index);
        node.insts.push(offset);
        self.nodes.push(node);
        self.inst_map.insert(offset, node_index);

        self.states.push(state);
        self.state_map.insert(state_index, node_index);
        self.live_states.push(state_index);
        
        node_index
    }

    pub fn get_node_at(&self, offset: usize) -> Option<usize> {
        match self.inst_map.get(&offset) {
            None => None,
            Some(index) => Some(*index)
        }
    }

    pub fn get_entry_nodes(&self) -> Vec<usize> {
        self.get_next_nodes(0)
    }

    pub fn get_instructions_at(&self, node: usize) -> &[usize] {
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

    pub fn get_states_at_node(&self, node_index: usize) -> Vec<usize> {
        self.nodes[node_index].states.iter().cloned().collect()
    }

    pub fn remove_node(&mut self, node_index: usize) {
        let node = self.nodes[node_index].clone();
        for in_edge in node.inbound_edges.iter() {
            for out_edge in node.outbound_edges.iter() {
                let from = self.edges[*in_edge].get_from();
                let to = self.edges[*out_edge].get_to();
                println!("adding edge from {} to {}", from, to);
                self.add_edge(from, to);
            }
        }

        for in_edge in node.inbound_edges.iter() {
            let from = self.edges[*in_edge].get_from();
            println!("removing edge from {} to {}", from, node_index);
            self.nodes[from].remove_edge(*in_edge);
        }

        for out_edge in node.outbound_edges.iter() {
            let to = self.edges[*out_edge].get_to();
            println!("removing edge from {} to {}", node_index, to);
            self.nodes[to].remove_edge(*out_edge);
        }

        self.nodes[node_index].deleted = true;
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

    pub fn get_inst(&self, address: usize) -> Option<&Meta<I>> {
        self.listing.instructions.get(&address)
    }

    pub fn add_label(&mut self, address: usize) {
        self.listing.add_label(address);
    }

    pub fn initial_instruction(&self, node_index: usize) -> Result<usize, String> {
        if node_index >= self.nodes.len() {
            Err(String::from("Node doesn't exist."))
        } else {
            Ok(*self.nodes[node_index].insts.first().unwrap())
        }
    }

    pub fn final_instruction(&self, node_index: usize) -> Result<Option<&usize>, String> {
        if node_index >= self.nodes.len() {
            Err(String::from("Node doesn't exist."))
        } else {
            Ok(self.nodes[node_index].insts.last())
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
    // Return value is the index of the second node.
    pub fn split_node_at(&mut self, node_index: usize, inst_offset: usize) -> usize {
        match self.nodes[node_index].insts.iter().position(|&r| r == inst_offset) {
            None => panic!("can't find offset to split at in node {}, instruction 0x{:x}.", node_index, inst_offset),
            Some(index) => {
                if index == 0 {
                    return node_index;
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

                for state in self.nodes[new_node_index].states.iter() {
                    self.state_map.insert(*state, new_node_index);
                }

                node_index
            }
        }
    }

    pub fn next_live_state(&mut self) -> Option<S> {
        /*let length = self.live_states.len();
        if length == 0 {
            None
        } else {
            match SystemTime::now().duration_since(UNIX_EPOCH) {
                Err(_) => panic!("SystemTime before UNIX EPOCH!"),
                Ok(n) => {
                    let live_states_index = n.as_secs() as usize % length;
                    let state_index = self.live_states.remove(live_states_index);
                    Some(self.states[state_index].clone())
                }
            }
        }*/
        match self.live_states.pop() {
            Some(index) => Some(self.states[index].clone()),
            None => None
        }
    }

    pub fn extend_with_state(&mut self, inst_offset: usize, new_inst_offset: usize, state: S) {
        let node_index = self.get_node_at(inst_offset)
            .expect("No node at instruction offset");
        match self.get_node_at(new_inst_offset) {
            None => {
                let new_node_index = self.add_node_with_state_at(state, new_inst_offset);
                self.add_edge(node_index, new_node_index);
            },
            Some(target_node_index) => {
               let new_node_index = self.split_node_at(target_node_index, new_inst_offset);
                self.add_edge(node_index, new_node_index);
                self.add_state(state, new_node_index);
            }
        };
    }

    pub fn add_state(&mut self, mut state: S, node_index: usize) {
        let mut state_indices = self.get_states_at_node(node_index);

        let mut index = 0;

        while index < state_indices.len() {
            let state_index = state_indices[index];
            match state.combine(&self.states[state_index]) {
                CombineResult::Subset => return,
                CombineResult::Superset => {
                    self.remove_state(state_index);
                    state_indices = self.get_states_at_node(node_index);
                    index = 0;
                },
                CombineResult::Uncombinable => index += 1,
                CombineResult::ExtendSelf(extended_state) => {
                    state = extended_state;
                    index = 0;
                },
                CombineResult::Combination(combined_state) => {
                    state = combined_state;
                    self.remove_state(state_index);

                    state_indices = self.get_states_at_node(node_index);
                    index = 0;
                }
            }
        }

        let state_index = self.states.len();
        self.state_map.insert(state_index, node_index);
        self.nodes[node_index].add_state(state_index);
        self.live_states.push(state_index);
        self.states.push(state);
    }

    fn remove_state(&mut self, state_index: usize) {
        let final_index = self.states.len() - 1;
        let node_index = self.state_map[&state_index];

        if !self.nodes[node_index].remove_state(state_index) {
            panic!("Node {} does not contain state_index {}",
                node_index, state_index);
        }
        
        if state_index == final_index {
            self.state_map.remove(&final_index);
            self.states.remove(final_index);
            self.live_states.retain(|&x| x != final_index);
            return;
        }

        let final_index_node_index = self.state_map[&final_index];
        let node = self.nodes.get_mut(final_index_node_index)
            .expect("no node at final index!");
        if !node.remove_state(final_index) {
            panic!("Node {} does not contain state_index {}",
                final_index_node_index, final_index);
        }
        node.add_state(state_index);
        self.state_map.insert(state_index, final_index_node_index);

        self.state_map.remove(&final_index);
        self.states.swap_remove(state_index);

        if self.live_states.contains(&final_index) {
            self.live_states.retain(|&x| x != final_index);
            if !self.live_states.contains(&state_index) {
                self.live_states.push(state_index);
            }
        } else {
            self.live_states.retain(|&x| x != state_index);
        }
    }

    pub fn log_state_count(&self) {
        println!("Total states: {}\t Total live states: {}\t ",
            self.states.len(), self.live_states.len() + 1);
    }
}

#[derive(Clone)]
struct Node {
    pub deleted: bool,
    pub insts: Vec<usize>,
    pub states: HashSet<usize>,
    pub inbound_edges: HashSet<usize>,
    pub outbound_edges: HashSet<usize>
}

impl Node {
    fn new() -> Node {
        Node {
            deleted: false,
            states: HashSet::new(),
            insts: Vec::new(),
            inbound_edges: HashSet::new(),
            outbound_edges: HashSet::new()
        }
    }

    fn from_state(offset: usize, state_index: usize) -> Node {
        Node {
            deleted: false,
            states: [state_index].iter().cloned().collect(),
            insts: vec!(offset),
            inbound_edges: HashSet::new(),
            outbound_edges: HashSet::new()
        }
    }

    fn remove_edge(&mut self, edge: usize) {
        self.inbound_edges.remove(&edge);
        self.outbound_edges.remove(&edge);
    }

    pub fn add_state(&mut self, state_index: usize) {
        self.states.insert(state_index);
    }

    pub fn remove_state(&mut self, state_index: usize) -> bool {
        match self.states.contains(&state_index) {
            false => false,
            true => {
                self.states.remove(&state_index);
                true
            }
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

impl<I: InstructionTrait, S: StateTrait<S>> fmt::Display for StateFlowGraph<I, S> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut output = String::new();
        for i in 0..self.nodes.len() {
            let ref node = self.nodes[i];
            if !node.deleted {
                output.push_str(format!("Node {}\n========\n", i).as_str());
                match node.insts.len() {
                    0 => (),//panic!("There shouldn't be a node with 0 instructions."),
                    1 => output.push_str(format!("Instruction 0x{:x}\n", node.insts[0]).as_str()),
                    _ => output.push_str(format!("Instructions 0x{:x} through 0x{:x}\n",
                        node.insts[0], node.insts[node.insts.len()-1]).as_str())
                }
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
            }
        }

        output.push_str(format!("Total States: {}\n", self.states.len()).as_str());
        write!(f, "{}", output)
    }
}

impl<I: InstructionTrait, S: StateTrait<S>> fmt::Debug for StateFlowGraph<I, S> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut output = String::new();
        output.push_str("\n========= Flow Graph =========\n\n");
        for i in 0..self.nodes.len() {
            output.push_str("========\n");
            output.push_str(format!("Node {}\n========\n", i).as_str());
            let ref node = self.nodes[i];
            match node.insts.len() {
                0 => (),//panic!("There shouldn't be a node with 0 instructions."),
                1 => output.push_str(format!("Instruction 0x{:x}\n", node.insts[0]).as_str()),
                _ => output.push_str(format!("Instructions 0x{:x} through 0x{:x}\n",
                    node.insts[0], node.insts[node.insts.len()-1]).as_str())
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
            for index in node.states.iter() {
                output.push_str(format!("\n==== STATE {} ====\n", index).as_str());
                output.push_str(self.states[*index].debug_string().as_str());
            }
        }
        output.push_str(format!("Total States: {}\n", self.states.len()).as_str());
        output.push_str(format!("Live States: {:?}\n", self.live_states).as_str()); 

        write!(f, "{}", output)
    }
}

