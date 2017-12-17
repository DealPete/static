use std::fmt;
use std::collections::HashMap;

pub struct FlowGraph {
    nodes: Vec<Node>,
    edges: Vec<Edge>,
    inst_map: HashMap<usize, usize>
}

impl fmt::Display for FlowGraph {
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
            if node.inbound_edges.len() > 0 {
                let mut inbound = String::new();
                for edge in node.inbound_edges.iter() {
                    inbound.push_str(format!("{} ", self.edges[*edge].get_from()).as_str());
                }
                output.push_str(format!("Inbound nodes: {}\n", inbound).as_str());
            }
            if node.outbound_edges.len() > 0 {
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

impl FlowGraph {
    pub fn new() -> FlowGraph {
        FlowGraph {
            nodes: Vec::new(),
            edges: Vec::new(),
            inst_map: HashMap::new()
        }
    }

    pub fn add_node_at(&mut self, offset: usize, label: bool) -> usize {
        let mut node = Node::new(label);
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

    pub fn get_node_at(&self, offset: usize) -> Option<&Node> {
        match self.inst_map.get(&offset) {
            None => None,
            Some(index) => Some(&self.nodes[*index])
        }
    }

    pub fn add_edge(&mut self, source: usize, target: usize) {
        self.nodes[source].outbound_edges.push(self.edges.len());
        self.nodes[target].inbound_edges.push(self.edges.len());
        self.edges.push(Edge::new(source, target));
    }
    
    pub fn insert_inst(&mut self, node_index: usize, inst_offset: usize) {
        self.inst_map.insert(inst_offset, node_index);
        self.nodes[node_index].insts.push(inst_offset);
    }

    pub fn split_node_at(&mut self, node_index: usize, inst_offset: usize) {
        let mut new_node = Node::new(true);
        let new_node_index = self.nodes.len();
        match self.nodes[node_index].insts.iter().position(|&r| r == inst_offset) {
            None => panic!("can't find offset to split at in node {}, instruction 0x{:x}.", node_index, inst_offset),
            Some(index) => {
                if index == 0 {
                    return
                }
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
            }
        }
    }

    pub fn extend_node(&mut self, from_index: usize, to_index: usize) -> bool {
        let from_node_index = self.get_node_index_at(from_index)
            .expect("trying to extend non-existant node!");

        match self.get_node_index_at(to_index) {
            None => {
                self.insert_inst(from_node_index, to_index);
                return true;
            },
            Some(to_node_index) => {
                self.add_edge(from_node_index, to_node_index);
                return false;
            }
        }
    }
}

pub struct Node {
    pub label: bool,
    pub insts: Vec<usize>,
    pub inbound_edges: Vec<usize>,
    pub outbound_edges: Vec<usize>
}

impl Node {
    fn new(label: bool) -> Node {
        Node {
            label: label,
            insts: Vec::new(),
            inbound_edges: Vec::new(),
            outbound_edges: Vec::new()
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
