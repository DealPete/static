use state::*;
use std::fmt;
use std::collections::HashMap;
use std::collections::HashSet;

pub struct FlowGraph<'a> {
    nodes: Vec<Node>,
    edges: Vec<Edge<'a>>,
    inst_map: HashMap<usize, usize>
}

impl<'a> fmt::Display for FlowGraph<'a> {
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
                for edge in node.outbound_edges.iter() {
                    if let Some(ref state) = self.edges[*edge].state {
                        output.push_str(
                            format!("Edge {}--{} state:\n=================\n{}\n ",
                            self.edges[*edge].from, self.edges[*edge].to, state).as_str()
                        );
                    }
                }
            }
            output.push_str("\n"); 
        }
        write!(f, "{}", output)
    }
}

impl<'a> FlowGraph<'a> {
    pub fn new() -> FlowGraph<'a> {
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

    pub fn get_edge_mut(&mut self, source: usize, target: usize) -> Option<&mut Edge<'a>> {
        for out in self.nodes[source].outbound_edges.iter() {
            if self.edges[*out].to == target {
                return Some(&mut self.edges[*out])
            }
        }

        None
    }

    pub fn has_edge(&self, source: usize, target: usize) -> bool {
        for out in self.nodes[source].outbound_edges.iter() {
            if self.edges[*out].to == target {
                return true;
            }
        }
        
        false
    }

    pub fn add_edge(&mut self, source: usize, target: usize, state: Option<State<'a>>) {
        match self.has_edge(source, target) {
            true => panic!("trying to add duplicate edge."),
            false => {
                self.nodes[source].outbound_edges.insert(self.edges.len());
                self.nodes[target].inbound_edges.insert(self.edges.len());
                self.edges.push(Edge::new(source, target, state));
            }
        }
    }
    
    pub fn insert_inst(&mut self, node_index: usize, inst_offset: usize) {
        self.inst_map.insert(inst_offset, node_index);
        self.nodes[node_index].insts.push(inst_offset);
    }

    pub fn split_node_at(&mut self, node_index: usize, inst_offset: usize, needs_label: bool) -> Option<usize> {
        match self.nodes[node_index].insts.iter().position(|&r| r == inst_offset) {
            None => panic!("can't find offset to split at in node {}, instruction 0x{:x}.", node_index, inst_offset),
            Some(index) => {
                if index == 0 {
                    return None;
                }

                let mut new_node = Node::new(needs_label);
                let new_node_index = self.nodes.len();

                new_node.insts = self.nodes[node_index].insts.split_off(index);
                self.nodes.push(new_node);
                self.nodes.swap(node_index, new_node_index);

                for edge in self.nodes[new_node_index].inbound_edges.iter() {
                    self.edges[*edge].set_to(new_node_index);
                }

                self.nodes[node_index].outbound_edges = self.nodes[new_node_index].outbound_edges.clone();
                self.nodes[new_node_index].outbound_edges.clear();
                self.add_edge(new_node_index, node_index, None);

                for inst in self.nodes[new_node_index].insts.iter() {
                    self.inst_map.insert(*inst, new_node_index);
                }

                Some(new_node_index)
            }
        }
    }
}

pub struct Node {
    pub label: bool,
    pub insts: Vec<usize>,
    pub inbound_edges: HashSet<usize>,
    pub outbound_edges: HashSet<usize>,
}

impl Node {
    fn new(label: bool) -> Node {
        Node {
            label: label,
            insts: Vec::new(),
            inbound_edges: HashSet::new(),
            outbound_edges: HashSet::new()
        }
    }
}

struct Edge<'a> {
    from: usize,
    to: usize,
    pub state: Option<State<'a>>
}

impl<'a> Edge<'a> {
    fn new(from_node_index: usize, to_node_index: usize, state: Option<State<'a>>) -> Edge {
        Edge {
            from: from_node_index,
            to: to_node_index,
            state: state
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
