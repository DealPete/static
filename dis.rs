struct Instruction {
	previous: Option<usize>,
	label: bool,
	mnemonic: Mnemonic,
	op1: Operand,
	op2: Operand,
	op3: Operand
}

impl Instruction {
	pub fn new(mnemonic: Mnemonic) -> Instruction {
		Instruction {
			previous: None,
			mnemonic: mnemonic,
			label: false,
			op1: Operand::None,
			op2: Operand::None,
			op3: Operand::None
		}
	}
}

struct Regs {
	ip : usize,
	cs : u16
}

impl Regs {
	pub fn new() -> Regs {
		Regs {
			ip : 0,
			cs : 0
		}
	}
}

enum Mnemonic {
	ADC, ADD, AND, CMP, DEC, INC, MOV, OR, POP, PUSH, SBB, SUB, XOR
}

enum Operand {
	None,
	Register(Register),
	Imm8(i8),
	Imm16(i16),
	Memory(u16, u16)
}

enum Register {
	AX, AL, AH, BX, BL, BH, CX, CL, CH, DX, DL, DH,
	BP, SP, CS, DS, ES, DI, SI
}

fn main() {
	use std::io::Read;
	use std::fs::File;

	let mut file = File::open("test.bin").expect(
		"Failed to open file.");
	let mut buffer = Vec::new();
	file.read_to_end(&mut buffer).expect(
		"Failed to read into buffer.");
	
	let program = decode(buffer);
}

fn decode(buffer: Vec<u8>) {
	use std::collections::HashMap;

	let mut instructions : HashMap<usize, Instruction> = HashMap::new();
	let mut regs = Regs::new();
	let mut previous : Option<usize> = None;

	while regs.ip < buffer.len() {
		let code = buffer[regs.ip];
		if code < 0x40 && code % 8 < 6 {
			previous = Some(regs.ip);
			let mut inst : Instruction = decode_regular_inst(&buffer, &mut regs);
			inst.previous = previous;
			instructions.insert(regs.ip, inst);
		}
		regs.ip += 1;
	}
}

fn decode_regular_inst(buffer: &Vec<u8>, regs: &mut Regs) -> Instruction {
	let code = buffer[regs.ip];
	let code_type = code % 8;

	let mut inst = Instruction::new(
		match code / 8 {
			0 => Mnemonic::ADD,
			1 => Mnemonic::OR,
			2 => Mnemonic::ADC,
			3 => Mnemonic::SBB,
			4 => Mnemonic::AND,
			5 => Mnemonic::SUB,
			6 => Mnemonic::XOR,
			7 => Mnemonic::CMP,
			11 => Mnemonic::MOV,
			_ => panic!("instruction is not regular")
		}
	);
			
	regs.ip += 1;
	match code_type {
		0...3 =>
			{
			let (op1, op2) = decode_mod_rm(buffer, regs, code_type % 2);
			inst.op1 = op1;
			inst.op2 = op2;
			},
		4 =>
			{
			inst.op1 = Operand::Register(Register::AL);
			inst.op2 = Operand::Imm8(buffer[regs.ip] as i8)
			},
		5 => 
			{
			let mut imm = buffer[regs.ip + 1] as u16;
			regs.ip <<= 8;
			imm += buffer[regs.ip] as u16;
			inst.op1 = Operand::Register(Register::AX);
			inst.op2 = Operand::Imm16(imm as i16);
			},
		_ => panic!("Improper code type")
	}

	return inst;
}

fn decode_mod_rm(buffer: &Vec<u8>, regs: &Regs, size: u8) -> (Operand, Operand) {
	let byte = buffer[regs.ip];
	let mode = byte >> 6;
	let op1 = byte & 0x38 >> 3;
	let op2 = byte & 0xf8;
	match size {
		0 => return (Operand::Register(reg8(op1)), Operand::Register(reg8(op2))),
		1 => return (Operand::Register(reg16(op1)), Operand::Register(reg16(op2))),
		_ => panic!("Improper size")
	}
}

fn reg8(reg: u8) -> Register {
	match reg {
		0 => Register::AL,
		1 => Register::CL,
		2 => Register::DL,
		3 => Register::BL,
		4 => Register::AH,
		5 => Register::CH,
		6 => Register::DH,
		7 => Register::BH,
		_ => panic!("Incorrect register")
	}
}

fn reg16(reg: u8) -> Register {
	match reg {
		0 => Register::AX,
		1 => Register::CX,
		2 => Register::DX,
		3 => Register::BX,
		4 => Register::SP,
		5 => Register::BP,
		6 => Register::SI,
		7 => Register::DI,
		_ => panic!("Incorrect register")
	}
}
