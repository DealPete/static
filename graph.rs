use defs::*;
use std::fmt;
use std::collections::HashMap;
use std::collections::HashSet;

pub struct FlowGraph<S: StateTrait<S>> {
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
                if let Some(ref state) = node.state {
                    output.push_str(format!("state:\n=================\n{}\n ", state).as_str());
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
            labels: HashSet::new(),
            nodes: Vec::new(),
            edges: Vec::new(),
            inst_map: HashMap::new()
        }
    }

    pub fn add_node_at(&mut self, offset: usize, state: S) -> usize {
        let mut node = Node::new(Some(state));
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

    pub fn get_state_at(&self, index: usize) -> Option<S> {
        self.nodes[index].state.clone()
    }
    
    pub fn set_state_at(&mut self, index: usize, state: S) {
        self.nodes[index].state = Some(state);
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

    pub fn add_label(&mut self, label_offset: usize) {
        self.labels.insert(label_offset);
    }

    pub fn is_labelled(&self, label_offset: usize) -> bool {
        self.labels.contains(&label_offset)
    }

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
        let mut new_state = Some(state.clone());

        let node_index = self.get_node_index_at(inst_offset)
            .expect("No node at instruction offset");
        match self.get_node_index_at(new_inst_offset) {
            None => {
                let new_node_index = self.add_node_at(new_inst_offset, state);
                self.add_edge(node_index, new_node_index);
            },
            Some(new_node_index) => {
                match self.split_node_at(new_node_index, new_inst_offset) {
                    None => {
                        self.add_edge(node_index, new_node_index);
                        match self.get_state_at(new_node_index) {
                            None => self.set_state_at(new_node_index, state),
                            Some(node_state) => {
                                if !state.is_subset(&node_state) {
                                    let combined_state = state.union(node_state);
                                    self.set_state_at(new_node_index, combined_state.clone());
                                    new_state = Some(combined_state);
                                } else {
                                    new_state = None;
                                }
                            }
                        }
                    },
                    Some(split_node_index) => {
                        self.add_edge(node_index, split_node_index);
                        self.set_state_at(new_node_index, state);
                    }
                }
            }
        };

        new_state
    }
}

pub struct Node<S: StateTrait<S>> {
    pub state: Option<S>,
    pub insts: Vec<usize>,
    pub inbound_edges: HashSet<usize>,
    pub outbound_edges: HashSet<usize>
}

impl<S: StateTrait<S>> Node<S> {
    fn new(state: Option<S>) -> Node<S> {
        Node {
            state: state,
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
