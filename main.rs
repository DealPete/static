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

        let context = dos::DOS { };

        let program = Program {
            initial_state: dos::initial_state(&buffer),
            load_module: dos::load_module(buffer),
            flow_graph: graph::FlowGraph::new(),
            instructions: HashMap::new()
        };

        let program = dis::disassemble_load_module(program, context);
        println!("{}", program);
    } else {
		println!("usage: dis <file-to-disassemble>");
    }
}
