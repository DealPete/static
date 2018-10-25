use chip8::arch::*;
use graph::flow_graph::*;

pub struct Chip8Slicer {
}

impl Chip8Slicer {
    pub fn from_operand(&self, graph: &FlowGraph<Instruction>, offset: usize, operand: Operand) -> Result<FlowGraphSlice, String> {
        match operand {
            Operand::I | Operand::V(_) => (),
            _ => return Err(String::from("Invalid operand to construct slice from."))
        }
            
        let mut node = match graph.get_node_at(offset) {
            None => return Err(format!("no node found at offset 0x{:x}", offset)),
            Some(node) => node
        };
        let mut live_nodes = vec!(node);
        let mut insts = graph.get_instructions_at_node(node);
        let mut index = insts.iter().position(|&inst| inst == offset).unwrap();

        /*loop {
            if index == 0 {
                match 
        }*/
        Ok(FlowGraphSlice::new())
    }
}

impl SlicerTrait<Instruction> for Chip8Slicer {
    fn create_slice(graph: &FlowGraph<Instruction>, offset: usize) -> Result<FlowGraphSlice, String> {
        let mut slice = FlowGraphSlice::new();

        let instruction = match graph.get_inst(offset) {
            None => return Err(format!("no instruction found at offset 0x{:x}", offset)),
            Some(instruction) => instruction
        };

        slice.insert(offset);
        Ok(slice)
    }
}

/*
pub fn target(&self) -> Option<Operand> {
    match self.mnemonic {
        Mnemonic::CLS | Mnemonic::RET | Mnemonic::JP => None
        Mnemonic::SE | Mnemonic::SNE => Some(instruction.op1),
        Mnemonic::ADD | Mnemonic::SUB => Some(instruction.op2)
    }
}
*/
