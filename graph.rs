use std::fmt;
use std::collections::HashMap;
use defs::*;

pub struct FlowGraph {
    nodes: Vec<Node>,
    inst_map: HashMap<usize, usize>
}

impl fmt::Display for FlowGraph {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut output = String::new();
        for i in 0..self.nodes.len() {
            let ref node = self.nodes[i];
            output.push_str(format!("Node {}\n=======\n", i).as_str());
            match node.insts.len() {
                0 => panic!("There shouldn't be a node with 0 instructions."),
                1 => output.push_str(format!("Instruction 0x{:x}\n", node.insts[0]).as_str()),
                _ => output.push_str(format!("Instructions 0x{:x} through 0x{:x}\n",
                    node.insts[0], node.insts[node.insts.len()-1]).as_str())
            }
            if node.inbound_nodes.len() > 0 {
                let mut inbound = String::new();
                for edge in node.inbound_nodes.iter() {
                    inbound.push_str(format!("{} ", edge).as_str());
                }
                output.push_str(format!("Inbound nodes: {}\n", inbound).as_str());
            }
            if node.outbound_nodes.len() > 0 {
                let mut outbound = String::new();
                for edge in node.outbound_nodes.iter() {
                    outbound.push_str(format!("{} ", edge).as_str());
                }
                output.push_str(format!("Outbound nodes: {}\n", outbound).as_str());
            }
            output.push_str("\n"); 
        }
        write!(f, "{}", output)
    }
}

impl FlowGraph {
    fn new() -> FlowGraph {
        FlowGraph {
            nodes: Vec::new(),
            inst_map: HashMap::new()
        }
    }

    pub fn add_node(&mut self, insts: Vec<usize>) {
        for inst in insts.iter() {
            self.inst_map.insert(*inst, self.nodes.len());
        }
        self.nodes.push(Node {
            insts: insts,
            inbound_nodes: Vec::new(),
            outbound_nodes: Vec::new()
        });
    }

    pub fn get_node(&self, i: usize) -> &Node {
        &self.nodes[i]
    }

    fn get_node_index_at(&self, offset: usize) -> Option<usize> {
        match self.inst_map.get(&offset) {
            None => None,
            Some(node) => Some(*node)
        }
    }

    pub fn get_node_at(&self, offset: usize) -> Option<&Node> {
        match self.inst_map.get(&offset) {
            None => None,
            Some(node_index) => Some(&self.nodes[*node_index])
        }
    }

    pub fn add_edge(&mut self, source: usize, target: usize) {
        self.nodes[source].outbound_nodes.push(target);
        self.nodes[target].inbound_nodes.push(source);
    }
}

pub struct Node {
    pub insts: Vec<usize>,
    pub inbound_nodes: Vec<usize>,
    pub outbound_nodes: Vec<usize>
}

impl Node {
    fn new() -> Node {
        Node {
            insts: Vec::new(),
            inbound_nodes: Vec::new(),
            outbound_nodes: Vec::new()
        }
    }
}

pub fn generate_flow_graph(program: &Program) -> FlowGraph {
    let mut graph = FlowGraph::new();
    
    for i in 0..program.length {
        if let Some(ref inst) = program.instructions.get(&i) {
            if graph.get_node_index_at(i) == None {
                let mut insts: Vec<usize> = Vec::new();
                let mut cur_index = inst.position;
                loop {
                    let cur_inst = program.instructions.get(&cur_index);
                    match cur_inst {
                        None => break,
                        Some(ref inst) => {
                            insts.push(cur_index);
                            if inst.mnemonic.is_jump() {
                                break;
                            }
                            match inst.next {
                                None => break,
                                Some(index) => cur_index = index
                            }
                        }
                    }
                }
                
                graph.add_node(insts);
            }
        }
    }

    let mut edges = Vec::new();

    println!("{}", program);
    for i in 0..graph.nodes.len() {
        let node = graph.get_node(i);
        let last_inst = node.insts[node.insts.len()-1];
        if let Some(ref inst) = program.instructions.get(&last_inst) {
            if inst.mnemonic.is_jump() {
                let target = match inst.op1 {
                    Some(Operand::Imm8(rel))
                        => add_rel8(inst.position + inst.length as usize, rel),
                    Some(Operand::Imm16(rel))
                        => add_rel16(inst.position + inst.length as usize, rel),
                    Some(Operand::PtrReg(reg))
                        => break,
                    _ => panic!("unimplemented operand for jump!")
                };

                if let Some(target_node) = graph.get_node_index_at(target) {
                    edges.push((i, target_node));
                }
            }
            if let Some(index) = inst.next {
                if let Some(target_node) = graph.get_node_index_at(index) {
                    edges.push((i, target_node));
                }
            }
        } else {
            panic!("Instruct in node not found in program!");
        }
    }
    
    for (source, target) in edges {
        graph.add_edge(source, target);
    }

    return graph;
}
