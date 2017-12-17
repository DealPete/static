mod analyse;
mod defs;
mod dis;
mod dos;
mod sim;
mod graph;
mod state;

use defs::*;
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

        let load_module = dos::load_module(&buffer);
        let initial_state = dos::initial_state(buffer, &load_module); 

        let program = Program {
            initial_state: initial_state,
            flow_graph: graph::FlowGraph::new(),
            instructions: HashMap::new(),
            context: dos::DOS {}
        };

        let final_program = dis::disassemble_load_module(program);
        println!("{}", final_program);
    } else {
		println!("usage: dis <file-to-disassemble>");
    }
}
