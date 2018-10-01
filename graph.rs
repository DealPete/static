use std::fmt;
use std::collections::HashMap;
use std::collections::HashSet;

pub struct FlowGraph {
    nodes: Vec<Node>,
    edges: Vec<Edge>,
    inst_map: HashMap<usize, usize>,
    data_map: HashMap<usize, usize>
}

impl FlowGraph {
    pub fn new() -> FlowGraph {
        FlowGraph {
            nodes: Vec::new(),
            edges: Vec::new(),
            inst_map: HashMap::new(),
            data_map: HashMap::new()
        }
    }

    pub fn add_node_at(&mut self, offset: usize) -> usize {
        let mut node = Node::new();
        node.insts.push(offset);
        self.nodes.push(node);
        self.inst_map.insert(offset, self.nodes.len() - 1);
        return self.nodes.len() - 1;
    }

    pub fn add_node_with_data_index_at(&mut self, datum: usize, offset: usize)
        -> usize {
        self.data_map.insert(datum, self.nodes.len());

        let mut node = Node::from_datum(datum);
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

    pub fn get_node_index_for_data_index(&self, datum: usize) -> Option<usize> {
        match self.data_map.get(&datum) {
            None => None,
            Some(index) => Some(*index)
        }
    }

    pub fn get_data_at_node_index(&self, node_index: usize) -> Vec<usize> {
        self.nodes[node_index].data.clone()
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

    pub fn add_datum(&mut self, data_index: usize, node_index: usize) {
        self.data_map.insert(data_index, node_index);
        self.nodes[node_index].add_datum(data_index);
    }

    pub fn swap_remove_datum(&mut self, data_index: usize, swap_index: usize) {
        let node_index = self.data_map[&data_index];
        if !self.nodes[node_index].remove_datum(data_index) {
            panic!(format!("Node {} does not contain data_index {}",
                node_index, data_index));
        }
        
        if data_index != swap_index {
            let swap_datum_node_index = self.data_map[&swap_index];
            let node = self.nodes.get_mut(swap_datum_node_index)
                .expect("no node at swap index!");
            if !node.remove_datum(swap_index) {
                panic!(format!("Node {} does not contain data_index {}",
                    swap_datum_node_index, swap_index));
            }
            node.add_datum(data_index);
            self.data_map.insert(data_index, swap_datum_node_index);
        }

        self.data_map.remove(&swap_index);
    }

    // This function splits the node with index "node_index" into two nodes;
    // The first node contains all the instructions coming before "inst_offset".
    // The second node contains the instruction at "inst_offset", and all
    // instructions coming after. It also adds an edge between the end of the
    // first node and the beginning of the second.
    //
    // If "inst_offset" points to the first instruction in the node,
    // then there is no need to split the node - we just return "None".
    // Otherwise we return the indices of the nodes.
    pub fn split_node_at(&mut self, node_index: usize, inst_offset: usize) -> Option<(usize, usize)> {
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

                for datum in self.nodes[new_node_index].data.iter() {
                    self.data_map.insert(*datum, new_node_index);
                }

                Some((new_node_index, node_index))
            }
        }
    }
}

pub struct Node {
    pub data: Vec<usize>,
    pub insts: Vec<usize>,
    pub inbound_edges: HashSet<usize>,
    pub outbound_edges: HashSet<usize>
}

impl Node {
    fn new() -> Node {
        Node {
            data: Vec::new(),
            insts: Vec::new(),
            inbound_edges: HashSet::new(),
            outbound_edges: HashSet::new()
        }
    }

    fn from_datum(datum: usize) -> Node {
        Node {
            data: vec!(datum),
            insts: Vec::new(),
            inbound_edges: HashSet::new(),
            outbound_edges: HashSet::new()
        }
    }

    pub fn add_datum(&mut self, data_index: usize) {
        self.data.push(data_index);
    }

    pub fn remove_datum(&mut self, data_index: usize) -> bool {
        match self.data.iter().position(|&r| r == data_index) {
            None => false,
            Some(index) => {
                self.data.swap_remove(index);
                true
            }
        }
    }

    pub fn get_data(&self) -> Vec<usize> {
        self.data.clone()
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

impl fmt::Display for FlowGraph {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut output = String::new();
        for i in 0..self.nodes.len() {
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
            output.push_str("\n"); 
        }

        write!(f, "{}", output)
    }
}
