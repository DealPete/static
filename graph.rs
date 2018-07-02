use defs::*;
use std::fmt;
use std::collections::HashMap;
use std::collections::HashSet;

pub struct FlowGraph<S: StateTrait<S>> {
    indeterminates: HashSet<usize>,
    labels: HashSet<usize>,
    nodes: Vec<Node<S>>,
    edges: Vec<Edge>,
    inst_map: HashMap<usize, usize>
}

impl<S: StateTrait<S>> fmt::Display for FlowGraph<S> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut output = String::new();
        for i in 0..self.nodes.len() {
            let ref node = self.nodes[i];
            output.push_str(format!("Node {}\n========\n", i).as_str());
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
                if let Some(ref states) = node.states {
                    for state in states {
                        output.push_str(format!("state:\n=================\n{}\n ", state).as_str());
                    }
                }
            }
            output.push_str("\n"); 
        }
        write!(f, "{}", output)
    }
}

impl<S: StateTrait<S>> FlowGraph<S> {
    pub fn new() -> FlowGraph<S> {
        FlowGraph {
            indeterminates: HashSet::new(),
            labels: HashSet::new(),
            nodes: Vec::new(),
            edges: Vec::new(),
            inst_map: HashMap::new()
        }
    }

    pub fn add_node_at(&mut self, offset: usize, state: S) -> usize {
        let mut node = Node::new(Some(vec!(state)));
        node.insts.push(offset);
        self.nodes.push(node);
        self.inst_map.insert(offset, self.nodes.len() - 1);
        return self.nodes.len() - 1;
    }

    pub fn get_node_index_at(&self, offset: usize) -> Option<usize> {
        match self.inst_map.get(&offset) {
            None => None,
            Some(index) => Some(*index)
        }
    }

    pub fn get_node_at(&self, offset: usize) -> Option<&Node<S>> {
        match self.inst_map.get(&offset) {
            None => None,
            Some(index) => Some(&self.nodes[*index])
        }
    }

    pub fn get_states_at(&self, index: usize) -> Option<Vec<S>> {
        self.nodes[index].states.clone()
    }
    
    pub fn set_states_at(&mut self, index: usize, states: Vec<S>) {
        self.nodes[index].states = Some(states);
    }

    pub fn add_state_at(&mut self, index: usize, state: S) {
        match self.nodes[index].states {
            Some(ref mut states) => {
                states.push(state);
                return
            },
            None => ()
        }

        self.nodes[index].states = Some(vec!(state));
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

    pub fn add_label(&mut self, offset: usize) {
        self.labels.insert(offset);
    }

    pub fn add_indeterminate(&mut self, offset: usize) {
        self.indeterminates.insert(offset);
    }

    pub fn is_labelled(&self, offset: usize) -> bool {
        self.labels.contains(&offset)
    }
    
    pub fn is_indeterminate(&self, offset: usize) -> bool {
        self.indeterminates.contains(&offset)
    }

    // This function splits the node with index "node_index" into two nodes;
    // The first node contains all the instructions coming before "inst_offset".
    // THe second node contains the instruction at "inst_offset", and all
    // instructions coming after. It also adds an edge between the end of the
    // first node and the beginning of the second.
    //
    // If "inst_offset" points to the first instruction in the node,
    // then there is no need to split the node - we just return "None".
    // Otherwise we return the index of the first node.
    pub fn split_node_at(&mut self, node_index: usize, inst_offset: usize) -> Option<usize> {
        match self.nodes[node_index].insts.iter().position(|&r| r == inst_offset) {
            None => panic!("can't find offset to split at in node {}, instruction 0x{:x}.", node_index, inst_offset),
            Some(index) => {
                if index == 0 {
                    return None;
                }

                let mut new_node = Node::new(None);
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

                Some(new_node_index)
            }
        }
    }

    pub fn extend_with_state(&mut self, inst_offset: usize, new_inst_offset: usize, state: S) -> Option<S> {
        let node_index = self.get_node_index_at(inst_offset)
            .expect("No node at instruction offset");
        match self.get_node_index_at(new_inst_offset) {
            None => {
                let new_node_index = self.add_node_at(new_inst_offset, state.clone());
                self.add_edge(node_index, new_node_index);
            },
            Some(new_node_index) => {
                match self.split_node_at(new_node_index, new_inst_offset) {
                    None => {
                        self.add_edge(node_index, new_node_index);

                        match self.get_states_at(new_node_index) {
                            None => (),
                            Some(node_states) => {
                                println!("Node has {} states:", node_states.len());
                                /*if node_states.len() == 20 {
                                    for state in node_states {
                                        println!("{}", state);
                                    }
                                    panic!("Above are the states for a node.");
                                }*/
                                
                                match self.combine_states(&state, &node_states) {
                                    None => return None,
                                    Some((combined_state, new_states)) => {
                                        self.set_states_at(new_node_index, new_states);
                                        return Some(combined_state);
                                    }
                                }
                            }
                        }
                    },
                    Some(split_node_index) => {
                        self.add_edge(node_index, split_node_index);
                        self.add_state_at(new_node_index, state.clone());
                    }
                }
            }
        };

        Some(state.clone())
    }

    fn combine_states(&self, state: &S, states: &Vec<S>) -> Option<(S, Vec<S>)> {
        let mut new_state = state.clone();
        let mut new_states = states.clone();
        let mut working_states = Vec::new();
        let mut index = 0;

        while index < new_states.len() {
            match new_state.combine(&new_states[index]) {
                CombineResult::Subset => return None,
                CombineResult::Uncombinable => {
                    working_states.push(new_states[index].clone());
                    index += 1;
                },
                CombineResult::Combination(combined_state) => {
                    new_state = combined_state;
                    for jndex in (index+1)..new_states.len() {
                        working_states.push(new_states[jndex].clone());
                    }
                    new_states = working_states;
                    working_states = Vec::new();
                    index = 0;
                }
            }
        }

        new_states.push(new_state.clone());
        return Some((new_state, new_states));
    }

}

pub struct Node<S: StateTrait<S>> {
    pub states: Option<Vec<S>>,
    pub insts: Vec<usize>,
    pub inbound_edges: HashSet<usize>,
    pub outbound_edges: HashSet<usize>
}

impl<S: StateTrait<S>> Node<S> {
    fn new(states: Option<Vec<S>>) -> Node<S> {
        Node {
            states: states,
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
