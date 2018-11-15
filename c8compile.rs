pub mod analyse;
pub mod defs;
pub mod graph;
pub mod chip8;
mod c8analyzer;

use chip8::arch::*;
use graph::flow_graph::{FlowGraph, CallGraph};

static PROGRAM: &str =
"#include \"api.h\"
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

unsigned char memory[4096];
uint8_t V[16];
uint16_t I;

unsigned char initial_memory[4096] = {
    // numerals
	0xf0, 0x90, 0x90, 0x90, 0xf0,	// 0
	0x20, 0x60, 0x20, 0x20, 0x70,	// 1
	0xf0, 0x10, 0xf0, 0x80, 0xf0,	// 2
	0xf0, 0x10, 0xf0, 0x10, 0xf0,	// 3
	0x90, 0x90, 0xf0, 0x10, 0x10,	// 4
	0xf0, 0x80, 0xf0, 0x10, 0xf0,	// 5
	0xf0, 0x80, 0xf0, 0x90, 0xf0,	// 6
	0xf0, 0x10, 0x20, 0x40, 0x40,	// 7
	0xf0, 0x90, 0xf0, 0x90, 0xf0,	// 8
	0xf0, 0x90, 0xf0, 0x10, 0xf0,	// 9
	0xf0, 0x90, 0xf0, 0x90, 0x90,	// A
	0xe0, 0x90, 0xe0, 0x90, 0xe0,	// B
	0xf0, 0x80, 0x80, 0x80, 0xf0,	// C
	0xe0, 0x90, 0x90, 0x90, 0xe0,	// D
	0xf0, 0x80, 0xf0, 0x80, 0xf0,	// E
	0xf0, 0x80, 0xf0, 0x80, 0x80,	// F

    // big numerals (for SuperChip8)
    0xff, 0xff, 0xc3, 0xc3, 0xc3,
    0xc3, 0xc3, 0xc3, 0xff, 0xff,   // 0
    0x18, 0x78, 0x78, 0x18, 0x18,
    0x18, 0x18, 0x18, 0xff, 0xff,   // 1
    0xff, 0xff, 0x03, 0x03, 0xff,
    0xff, 0xc0, 0xc0, 0xff, 0xff,   // 2
    0xff, 0xff, 0x03, 0x03, 0xff,
    0xff, 0x03, 0x03, 0xff, 0xff,   // 3
    0xc3, 0xc3, 0xc3, 0xc3, 0xff,
    0xff, 0x03, 0x03, 0x03, 0x03,   // 4
    0xff, 0xff, 0xc0, 0xc0, 0xff,
    0xff, 0x03, 0x03, 0xff, 0xff,   // 5
    0xff, 0xff, 0xc0, 0xc0, 0xff,
    0xff, 0xc3, 0xc3, 0xff, 0xff,   // 6 
    0xff, 0xff, 0x03, 0x03, 0x06,
    0x0c, 0x18, 0x18, 0x18, 0x18,   // 7
    0xff, 0xff, 0xc3, 0xc3, 0xff,
    0xff, 0xc3, 0xc3, 0xff, 0xff,   // 8
    0xff, 0xff, 0xc3, 0xc3, 0xff,
    0xff, 0x03, 0x03, 0xff, 0xff,   // 9
    0x7e, 0xff, 0xc3, 0xc3, 0xc3,
    0xff, 0xff, 0xc3, 0xc3, 0xc3,   // a
    0xfc, 0xfc, 0xc3, 0xc3, 0xfc,
    0xfc, 0xc3, 0xc3, 0xfc, 0xfc,   // b
    0x3c, 0xff, 0xc3, 0xc0, 0xc0,
    0xc0, 0xc0, 0xc3, 0xff, 0x3c,   // c
    0xfc, 0xfe, 0xc3, 0xc3, 0xc3,
    0xc3, 0xc3, 0xc3, 0xfe, 0xfc,   // d
    0xff, 0xff, 0xc0, 0xc0, 0xff,
    0xff, 0xc0, 0xc0, 0xff, 0xff,   // e
    0xff, 0xff, 0xc0, 0xc0, 0xff,
    0xff, 0xc0, 0xc0, 0xc0, 0xc0,   // f

    // unused 272 bytes
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,

    // program
    {program}
};

void init_data() {
    memcpy(memory, initial_memory, 4096);

    for (int i = 0; i < 16; i++) {
        V[i] = 0;
    }

    I = 0;
}

char* get_filename() {
    return \"{filename}\";
}

{headers}
{functions}int run_game(void* data) {
\tinit_data();
{main}
\treturn 0;
}
";

static MAKEFILE: &str =
"TARGETS = code.c chip8/src/*.c
INCLUDES = `sdl2-config --cflags` -Ichip8/src
LIBS = `sdl2-config --libs` -lsodium

all: $(TARGETS)
\tcc -o game $(INCLUDES) $(LIBS) $(TARGETS)

clean:
\t$(RM) game

remove:
\t$(RM) game makefile code.c
";

fn main() {
    use std::env;
    use std::io::Read;
    use std::io::Write;
    use std::fs::File;
    use std::path::Path;

	if let Some(arg) = env::args().nth(1) {
        let input_file = arg.clone();
        let path = Path::new(&input_file);
        let stem = path.file_stem()
            .expect("argument has no stem").to_str().unwrap();

		let mut file = File::open(arg).expect(
			"Failed to open file.");
		let mut buffer = Vec::new();
		file.read_to_end(&mut buffer).expect(
			"Failed to read into buffer.");

        let result = analyse::analyse(
            &buffer, chip8::arch::Chip8 {},
            c8analyzer::Chip8Analyzer {}, 0);

        match result {
            Ok(graph) => {
                let mut file = File::create("code.c")
                    .expect("Couldn't create output file.");

                let call_graph = match graph.construct_call_graph() {
                    Err(err) => panic!(err),
                    Ok(call_graph) => call_graph
                };

                let source = match source_string(graph, call_graph, stem, buffer) {
                    Err(err) => panic!(err),
                    Ok(source) => source
                };
                
                file.write_all(source.as_bytes())
                    .expect("Couldn't write output file.");

                let mut makefile = File::create("makefile")
                    .expect("Couldn't create makefile.");

                makefile.write_all(MAKEFILE.as_bytes())
                    .expect("Couldn't write makefile.");
            },
            Err(error) => println!("{}", error)
        }
    } else {
		println!("usage: recompile <file-to-recompile>");
    }
}

fn source_string(graph: FlowGraph<Instruction>, mut call_graph: CallGraph, file_stem: &str, data: Vec<u8>) -> Result<String, String> {
    let mut data_string = String::new();

    for byte in data {
        data_string.push_str(format!("0x{:x}, ", byte).as_str());
    }

    let mut headers = String::new();

    for function in call_graph.iter().skip(1) {
        let first_offset = graph.initial_instruction(function[0])?.unwrap();
        headers.push_str(format!(
            "void f{:x}();\n", first_offset + 0x200).as_str());
    }

    let mut functions = String::new();
    let mut main = String::new();

    while let Some(function) = call_graph.pop() {
        if call_graph.len() > 0 {
            let address = graph.initial_instruction(function[0])?.unwrap() + 0x200;
            functions.push_str(format!("void f{:x}() {{\n", address).as_str());
            functions.push_str(compile_function(&graph, function, false)?.as_str());
            functions.push_str("}\n\n");
        } else {
            main = compile_function(&graph, function, true)?;
        }
    }

    Ok(PROGRAM.replace("{program}", &data_string)
              .replace("{filename}", file_stem)
              .replace("{headers}", &headers)
              .replace("{functions}", &functions)
              .replace("{main}", &main))
}

fn compile_function(graph: &FlowGraph<Instruction>, function: Vec<usize>, main: bool) -> Result<String, String> {
    let mut output = String::new();
    let mut node_outputs = Vec::new();

    for node in function {
        if let Some(offset) = graph.initial_instruction(node)? {
            node_outputs.push((offset, compile_node(&graph, node, main)));
        }
    }

    node_outputs.sort_by_key(|&(key, _)| key);

    for (_, node_output) in node_outputs {
        output.push_str(node_output.as_str());
    }

    Ok(output)
}

fn compile_node(graph: &FlowGraph<Instruction>, node: usize, main: bool) -> String {
    let node_address = graph.initial_instruction(node).unwrap()
        .expect(format!("no instruction at node {}", node).as_str()) + 0x200;
let mut output = format!("\nl{:x}:\n", node_address);
    
    for offset in graph.get_instructions_at(node) {
        let inst = graph.get_inst(*offset)
            .expect(format!("no instruction at offset {:x}", offset).as_str());

        output.push_str("\t");
        output.push_str( match inst.mnemonic {
            Mnemonic::LOW => "lores();\n".into(),
            Mnemonic::HIGH => "hires();\n".into(),
            Mnemonic::CLS => "clear_screen();\n".into(),
            Mnemonic::AND => and(inst.unpack_op1(), inst.unpack_op2()),
            Mnemonic::OR => or(inst.unpack_op1(), inst.unpack_op2()),
            Mnemonic::XOR => xor(inst.unpack_op1(), inst.unpack_op2()),
            Mnemonic::SHL => shl(inst.unpack_op1(), inst.unpack_op2()),
            Mnemonic::SHR => shr(inst.unpack_op1(), inst.unpack_op2()),
            Mnemonic::LD => load(inst.unpack_op1(), inst.unpack_op2()),
            Mnemonic::LDPTR => load_ptr(inst.unpack_op1(), inst.unpack_op2()),
            Mnemonic::LDBCD => format!("load_bcd(memory + I, {});\n",
                encode_op(inst.unpack_op1())),
            Mnemonic::ADD => add(inst.unpack_op1(), inst.unpack_op2()),
            Mnemonic::SUB => sub(inst.unpack_op1(), inst.unpack_op2()),
            Mnemonic::SUBN => subn(inst.unpack_op1(), inst.unpack_op2()),
            Mnemonic::SNE => skip(false, *offset, inst.unpack_op1(), inst.unpack_op2()),
            Mnemonic::SE => skip(true, *offset, inst.unpack_op1(), inst.unpack_op2()),
            Mnemonic::JP => jump(graph, *offset, inst.unpack_op1(), inst.op2),
            Mnemonic::SKP => skip_key(true, *offset, inst.unpack_op1()),
            Mnemonic::SKNP => skip_key(false, *offset, inst.unpack_op1()),
            Mnemonic::DRW => draw(inst.unpack_op1(), inst.unpack_op2(), inst.unpack_op3()),
            Mnemonic::RND => random(inst.unpack_op1(), inst.unpack_op2()),
            Mnemonic::CALL => call(inst.unpack_op1()),
            Mnemonic::RET => if main { "exit(0);\n".into() } else { "return;\n".into() },
            Mnemonic::EXIT => "exit(0);\n".into(),
            Mnemonic::SCL => "scroll_left();\n".into(),
            Mnemonic::SCR => "scroll_right();\n".into(),
            Mnemonic::SCD => format!("scroll_down({});\n", encode_op(inst.unpack_op1())),
        }.as_str());
    }

    output
}

fn add(op1: Operand, op2: Operand) -> String {
    if let Operand::V(_) = op1 {
        if let Operand::V(_) = op2 {
            let vx = encode_op(op1);
            let vy = encode_op(op2);
            return format!(
                "V[0xf] = {} + {} < {} ? 0 : 1;\n\t {} += {};\n",
                vx, vy, vx, vx, vy);
        }
    }

    format!("{} += {};\n", encode_op(op1), encode_op(op2))
}

fn sub(op1: Operand, op2: Operand) -> String {
    let vx = encode_op(op1);
    let vy = encode_op(op2);
    format!("V[0xf] = {} < {} ? 0 : 1;\n\t{} -= {};\n",
        vx, vy, vx, vy)
}

fn subn(op1: Operand, op2: Operand) -> String {
    let vx = encode_op(op1);
    let vy = encode_op(op2);
    format!("V[0xf] = {} < {} ? 0 : 1;\n\t{} = {} - {};\n",
        vy, vx, vx, vy, vx)
}

fn shl(op1: Operand, op2: Operand) -> String {
    let vx = encode_op(op1);
    let vy = encode_op(op2);
    format!("V[0xf] = {} & 0x80 ? 1 : 0;\n\t{} = {} << 1;\n",
        vx, vx, vy)
}

fn shr(op1: Operand, op2: Operand) -> String {
    let vx = encode_op(op1);
    let vy = encode_op(op2);
    format!("V[0xf] = {} & 0x1 ? 1 : 0;\n\t{} = {} >> 1;\n",
        vx, vx, vy)
}

fn and(op1: Operand, op2: Operand) -> String {
    let lhs = encode_op(op1);
    format!("{} = {} & {};\n", lhs, lhs, encode_op(op2))
}

fn or(op1: Operand, op2: Operand) -> String {
    let lhs = encode_op(op1);
    format!("{} = {} | {};\n", lhs, lhs, encode_op(op2))
}

fn xor(op1: Operand, op2: Operand) -> String {
    let lhs = encode_op(op1);
    format!("{} = {} ^ {};\n", lhs, lhs, encode_op(op2))
}

fn jump(graph: &FlowGraph<Instruction>, offset: usize, op1: Operand, op2: Option<Operand>) -> String {
    match op1 {
        Operand::Address(address) => {
            format!("goto l{:x};\n", address)
        },
        Operand::V(0) => match op2 {
            Some(Operand::Address(address)) => {
                let mut output = String::from("switch (V[0]) {\n");
                let node = graph.get_node_at(offset).unwrap();

                for target in graph.get_next_nodes(node) {
                    let target_offset = graph.initial_instruction(target)
                        .unwrap().unwrap();
                    let v_0 = target_offset + 0x200 - address as usize;
                    output.push_str(format!("\t\tcase {}:\n\t\tgoto l{:x};\n\n",
                        v_0, target_offset + 0x200).as_str());
                }

                output.push_str(format!(
"\t\tdefault:
\t\tprintf(\"Error at 0x{:x}. Unexpected value %d for V0.\", V[0]); 
\t\texit(0);
\t}}
",
                    offset + 0x200).as_str());

                output
            },
            _ => panic!("Invalid operand for JMP V0, ?.")
        },
        _ => panic!("Invalid operand for JMP.")
    }
}
 
fn load(op1: Operand, op2: Operand) -> String {
    match op1 {
        Operand::DelayTimer => format!("set_delay_timer({});\n", encode_op(op2)),
        Operand::SoundTimer => format!("set_sound_timer({});\n", encode_op(op2)),
        _ => format!("{} = {};\n", encode_op(op1), encode_op(op2))
    }
}

fn load_ptr(op1: Operand, op2: Operand) -> String {
    if let Operand::Pointer = op1 {
        match op2 {
            Operand::V(x) => format!("memcpy(memory + I, V, {});\n\tI += {};\n",
                x+1, x+1),
            _ => panic!("Invalid operand for LDPTR [I], ?")
        }
    } else {
        match op1 {
            Operand::V(x) => format!("memcpy(V, memory + I, {});\n\tI += {};\n",
                x+1, x+1),
            _ => panic!("Invalid operand for LDPTR ?, [I]")
        }
    }
}

fn skip(equal: bool, offset: usize, op1: Operand, op2: Operand) -> String {
    let address = offset + 0x200;
    let comparison = if equal { "==" } else { "!=" };

    match op1 {
        Operand::V(x) => match op2 {
            Operand::Byte(byte) => format!(
                "if (V[{}] {} {}) goto l{:x}; else goto l{:x};\n",
                x, comparison, byte, address + 4, address + 2),
            Operand::V(y) => format!(
                "if (V[{}] {} V[{}]) goto l{:x}; else goto l{:x};\n",
                x, comparison, y, address + 4, address + 2),
            _ => panic!("invalid operand for SE")
        },
        _ => panic!("invalid operand for S(N)E")
    }
}

fn skip_key(equal: bool, offset: usize, op1: Operand) -> String {
    let address = offset + 0x200;
    let comparison = if equal { "" } else { "!" };

    match op1 {
        Operand::V(x) => format!(
            "if ({}key_pressed(V[{}])) goto l{:x}; else goto l{:x};\n",
            comparison, x, address + 4, address + 2),
        _ => panic!("invalid operand for SK(N)P")
    }
}

fn draw(op1: Operand, op2: Operand, op3: Operand) -> String {
    let xpos = match op1 {
        Operand::V(x) => x,
        _ => panic!("invalid operand for DRW ?.")
    };

    let ypos = match op2 {
        Operand::V(y) => y,
        _ => panic!("invalid operand for DRW Vx, ?.")
    };

    let lines = match op3 {
        Operand::Byte(byte) => byte,
        _ => panic!("invalid operand for DRW Vx, Vy, ?.")
    };

    format!("V[0xf] = draw_sprite(memory + I, V[{}], V[{}], {});\n",
        xpos, ypos, lines)
}

fn random(op1: Operand, op2: Operand) -> String {
    let target = match op1 {
        Operand::V(x) => x,
        _ => panic!("invalid operand for RND ?.")
    };

    let mask = match op2 {
        Operand::Byte(byte) => byte,
        _ => panic!("invalid operand for RND Vx, ?.")
    };

    format!("V[{}] = random_byte() & {:#b};\n", target, mask)
}

fn call(op: Operand) -> String {
    let target = match op {
        Operand::Address(address) => address,
        _ => panic!("Invalid operand for CALL ?.")
    };

    format!("f{:x}();\n", target)
}

fn encode_op(op: Operand) -> String {
    match op {
        Operand::Byte(byte) => format!("0x{:x}", byte),
        Operand::V(x) => format!("V[{}]", x),
        Operand::I => "I".into(),
        Operand::Address(address) => format!("0x{:x}", address),
        Operand::Numeral(n) => format!("5 * V[{}]", n),
        Operand::LargeNumeral(n) => format!("10 * V[{}] + 80", n),
        Operand::KeyPress => "wait_for_keypress()".into(),
        Operand::DelayTimer => "get_delay_timer()".into(),
        Operand::SoundTimer => "get_sound_timer()".into(),
        _ => panic!("Invalid operand for op encoding.")
    }
}
