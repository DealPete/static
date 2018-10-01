use defs::*;
use std::fmt;
use std::collections::HashMap;
use std::collections::HashSet;

pub struct StateFlowGraph<S: StateTrait<S>> {
    nodes: Vec<Node>,
    edges: Vec<Edge>,
    states: Vec<S>,
    inst_map: HashMap<usize, usize>,
    pub state_map: HashMap<usize, usize>,
    live_states: Vec<usize>
}

impl<S: StateTrait<S>> StateFlowGraph<S> {
    pub fn new(entry_offset: usize, state: S) -> StateFlowGraph<S> {
        StateFlowGraph {
            nodes: vec!(Node::from(entry_offset, 0)),
            edges: Vec::new(),
            states: vec!(state),
            inst_map: [(entry_offset, 0)].iter().cloned().collect(),
            state_map: [(0, 0)].iter().cloned().collect(),
            live_states: vec!(0)
        }
    }

    pub fn add_node_with_state_at(&mut self, state: S, offset: usize) -> usize {
        let state_index = self.states.len();
        let node_index = self.nodes.len();

        let mut node = Node::from(offset, state_index);
        node.insts.push(offset);
        self.nodes.push(node);
        self.inst_map.insert(offset, node_index);

        self.states.push(state);
        self.state_map.insert(state_index, node_index);
        self.live_states.push(state_index);
        
        node_index
    }

    pub fn get_node_index_at(&self, offset: usize) -> Option<usize> {
        match self.inst_map.get(&offset) {
            None => None,
            Some(index) => Some(*index)
        }
    }

    pub fn get_states_at_node_index(&self, node_index: usize) -> Vec<usize> {
        self.nodes[node_index].states.iter().cloned().collect()
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
    
    pub fn insert_inst(&mut self, node_index: usize, inst_offset: usize) {
        self.inst_map.insert(inst_offset, node_index);
        self.nodes[node_index].insts.push(inst_offset);
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
        println!("Total States: {}\nLive States: {:?}\n", self.states.len(), self.live_states); 
        match self.live_states.pop() {
            Some(index) => Some(self.states[index].clone()),
            None => None
        }
    }

    pub fn extend_with_state(&mut self, inst_offset: usize, new_inst_offset: usize, state: S) {
        let node_index = self.get_node_index_at(inst_offset)
            .expect("No node at instruction offset");
        match self.get_node_index_at(new_inst_offset) {
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

    fn add_state(&mut self, mut state: S, node_index: usize) {
        println!("adding state to node {}.", node_index);
        let mut state_indices = self.get_states_at_node_index(node_index);

        let mut index = 0;

        while index < state_indices.len() {
            let state_index = state_indices[index];
            match state.combine(&self.states[state_index]) {
                CombineResult::Subset => return,
                CombineResult::Uncombinable => index += 1,
                CombineResult::Combination(combined_state) => {
                    state = combined_state;
                    self.remove_state(state_index);

                    state_indices = self.get_states_at_node_index(node_index);
                    index = 0;
                }
            }
        }

        let state_index = self.states.len();
        println!("state_index: {}", state_index);
        self.state_map.insert(state_index, node_index);
        self.nodes[node_index].add_state(state_index);
        self.live_states.push(state_index);
        self.states.push(state);
    }

    fn remove_state(&mut self, state_index: usize) {
        let final_index = self.states.len() - 1;
        let node_index = self.state_map[&state_index];

        println!("There are {} states.", final_index + 1);
        println!("state_index = {}.", state_index);
        println!("There are {} nodes.", self.nodes.len() - 1);
        println!("state_index is in node {}", node_index);

        println!("\nNode {} has states {:?}", node_index, self.nodes[node_index].states);

        if !self.nodes[node_index].remove_state(state_index) {
            panic!(format!("Node {} does not contain state_index {}",
                node_index, state_index));
        }
        
        if state_index != final_index {
            let final_index_node_index = self.state_map[&final_index];
            let node = self.nodes.get_mut(final_index_node_index)
                .expect("no node at final index!");
            if !node.remove_state(final_index) {
                panic!(format!("Node {} does not contain state_index {}",
                    final_index_node_index, final_index));
            }
            node.add_state(state_index);
            self.state_map.insert(state_index, final_index_node_index);
        }

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
}

struct Node {
    pub insts: Vec<usize>,
    pub states: HashSet<usize>,
    pub inbound_edges: HashSet<usize>,
    pub outbound_edges: HashSet<usize>
}

impl Node {
    fn new() -> Node {
        Node {
            states: HashSet::new(),
            insts: Vec::new(),
            inbound_edges: HashSet::new(),
            outbound_edges: HashSet::new()
        }
    }

    fn from(offset: usize, state_index: usize) -> Node {
        println!("adding state {} to this node", state_index);
        Node {
            states: [state_index].iter().cloned().collect(),
            insts: vec!(offset),
            inbound_edges: HashSet::new(),
            outbound_edges: HashSet::new()
        }
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

impl<S: StateTrait<S>> fmt::Display for StateFlowGraph<S> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut output = String::new();
        for i in 0..self.nodes.len() {
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
        }

        write!(f, "{}", output)
    }
}

impl<S: StateTrait<S>> fmt::Debug for StateFlowGraph<S> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut output = String::new();
        output.push_str("========= Flow Graph =========\n\n");
        for i in 0..self.nodes.len() {
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
                output.push_str(format!("{:?}", self.states[*index].debug_string()).as_str());
            }
        }

        write!(f, "{}", output)
    }
}


