mod analyse;
mod defs;
mod x86 {
    pub mod arch;
    pub mod dis;
    pub mod dos;
    pub mod sim;
}
mod graph;
mod state;

use defs::*;
use x86::arch::*;
use std::collections::HashMap;

fn main() {
	use std::env;
	use std::io::Read;
	use std::fs::File;

	if let Some(arg) = env::args().nth(1) {
		let mut file = File::open(arg).expect(
			"Failed to open file.");
		let mut buffer = Vec::new();
		file.read_to_end(&mut buffer).expect(
			"Failed to read into buffer.");

        let load_module = x86::dos::load_module(&buffer);
        let initial_state = x86::dos::initial_state(buffer, &load_module); 

        let program = Program {
            initial_state: initial_state,
            instructions: HashMap::<usize, Instruction>::new(),
            flow_graph: graph::FlowGraph::new(),
            context: x86::dos::DOS {}
        };

        let final_program = analyse::disassemble_load_module(program);
        println!("{}", final_program);
    } else {
		println!("usage: dis <file-to-disassemble>");
    }
}
