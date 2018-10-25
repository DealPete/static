use std::fmt;
use std::collections::HashSet;

pub struct Graph<T> {
    nodes: Vec<Node<T>>,
    edges: Vec<Edge>
}

impl<T> Graph<T> {
    pub fn new() -> Graph<T> {
        Graph {
            nodes: Vec::new(),
            edges: Vec::new()
        }
    }

    pub fn add_node(&mut self, value: T) -> usize {
        self.nodes.push(Node::new(value));
        return self.nodes.len() - 1;
    }

    pub fn get_value_at_node(&self, node_index: usize) -> &T {
        &self.nodes[node_index].value
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
    
    pub fn update_value(&mut self, value: T, node_index: usize) {
        self.nodes[node_index].update_value(value);
    }
}

pub struct Node<T> {
    pub value: T,
    pub inbound_edges: HashSet<usize>,
    pub outbound_edges: HashSet<usize>
}

impl<T> Node<T> {
    fn new(value: T) -> Node<T> {
        Node {
            value: value,
            inbound_edges: HashSet::new(),
            outbound_edges: HashSet::new()
        }
    }

    pub fn get_value(&self) -> &T {
        &self.value
    }

    pub fn update_value(&mut self, value: T) {
        self.value = value;
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
}

impl<T> fmt::Display for Graph<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut output = String::new();
        for i in 0..self.nodes.len() {
            output.push_str(format!("Node {}\n========\n", i).as_str());
            let ref node = self.nodes[i];
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
