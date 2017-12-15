use state::*;
use graph;
use std::fmt;
use std::collections::HashMap;

pub struct Program<'program> {
    pub initial_state: State<'program>,
    pub flow_graph: graph::FlowGraph,
    pub instructions: HashMap<usize, Instruction>
}

impl<'program> Program<'program> {
    pub fn entry_point(&self) -> usize {
        16 * self.initial_state.cs as usize
            + self.initial_state.ip as usize
            - 16 * self.initial_state.load_module.memory_segment as usize
    }

    pub fn get_memory_address(&self, offset: usize) -> usize {
        16 * self.initial_state.load_module.memory_segment as usize + offset
    }

    pub fn get_inst_offset(&self, address: usize) -> usize {
        address - 16 * self.initial_state.load_module.memory_segment as usize
    }
}

impl<'program> fmt::Display for Program<'program> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut output = String::new();
        for i in 0..self.initial_state.load_module.buffer.len() {
            if let Some(instruction) = self.instructions.get(&i) {
                let prefix = {
                    let node = self.flow_graph.get_node_at(i).expect(
                        format!("instruction 0x{:x} has no node!", i).as_str());
                    if node.insts[0] == i && node.label {
                        format!("{:4x}:   ", i)
                    } else {
                        format!("        ")
                    }
                };
                let inst_output = if instruction.is_rel_branch() {
                    let mut target = i + instruction.length as usize;
                    match instruction.op1 {
                        Some(Operand::Imm8(rel)) => target = add_rel8(target, rel),
                        Some(Operand::Imm16(rel)) => target = add_rel16(target, rel),
                        _ => panic!("Wrong operand for relative jump.")
                    }
                    format!("{:?} <{:x}>", instruction.mnemonic, target)
                } else {
                    format!("{}", instruction)
                };
                output.push_str(format!("{}{}{}\n", prefix.as_str(), inst_output,
                    if i == self.entry_point() {
                        "\t; program entry point"
                    } else {
                        ""
                    }).as_str());
            }
        }
        write!(f, "{}", output)
    }
}

pub trait Context<'state> {
    fn simulate_int(&self, state: State<'state>, inst:&Instruction) -> State<'state>;
    fn does_int_end_program(&self, instruction: &Instruction) -> bool;
    fn does_int_always_end_program(&self, inst_index: usize, program: &Program) -> bool;
    fn is_int_loose_end(&self, instruction: &Instruction) -> bool;
}

pub struct LoadModule {
    pub file_offset: usize,
    pub memory_segment: u16,
    pub buffer: Vec<u8>
}

pub struct Instruction {
    pub rep_prefix: Option<Mnemonic>,
    pub mnemonic: Mnemonic,
    pub length: u8,
    pub op1: Option<Operand>,
    pub op2: Option<Operand>
}

impl Instruction {
    pub fn new(mnemonic: Mnemonic) -> Instruction {
        Instruction {
            rep_prefix: None,
            mnemonic: mnemonic,
            length: 0,
            op1: None,
            op2: None
        }
    }

    pub fn is_rel_branch(&self) -> bool {
        self.mnemonic.is_branch() && match self.op1 {
            Some(Operand::Imm8(_)) | Some(Operand::Imm16(_)) => true,
            _ => false
        }
    }
    
    pub fn unpack_op1(&self) -> Operand {
        self.op1.expect(
            format!("Instruction doesn't have a first operand: {}", self).as_str())
    }

    pub fn unpack_op2(&self) -> Operand {
        self.op2.expect(
            format!("Instruction doesn't have a second operand: {}", self).as_str())
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut prefix = String::new();
        if let Some(ref rep_prefix) = self.rep_prefix {
            prefix.push_str(format!("{:?} ", rep_prefix).as_str());
        }
        match self.op1 {
            None => write!(f, "{}{:?}", prefix, self.mnemonic),
            Some(op1) => match self.op2 {
                None => write!(f, "{}{:?} {}", prefix, self.mnemonic, op1),
                Some(op2) =>
                    write!(f, "{}{:?} {}, {}", prefix, self.mnemonic, op1, op2)
            }
        }
    }
}

#[derive(Copy, Clone, PartialEq)]
pub enum Operand {
    Register8(Register),
    Register16(Register),
    Imm8(i8),
    Imm16(i16),
    SegPtr(Register, Pointer),
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Operand::Register8(ref reg) |
                &Operand::Register16(ref reg) => write!(f, "{:?}", reg),
            &Operand::Imm8(val) => write!(f, "{:02x}", val),
            &Operand::Imm16(val) => write!(f, "{:04x}", val),
            &Operand::SegPtr(segment, pointer) => {
                let pfx = match segment {
                    Register::DS => String::from(""),
                    _ => format!("{:?}:", segment)
                };
                match pointer {
                    Pointer::Disp16(val) => write!(f, "[{}{:04x}]", pfx, val),
                    Pointer::Reg(ref reg) => write!(f, "[{}{:?}]", pfx, reg),
                    Pointer::RegReg(ref reg1, ref reg2) =>
                        write!(f, "[{}{:?}+{:?}]", pfx, reg1, reg2),
                    Pointer::RegDisp8(ref reg, val) =>
                        write!(f, "[{}{:?}+{:02x}]", pfx, reg, val),
                    Pointer::RegRegDisp8(ref reg1, ref reg2, val) =>
                        write!(f, "[{}{:?}+{:?}+{:02x}]", pfx, reg1, reg2, val),
                    Pointer::RegDisp16(ref reg, val) =>
                        write!(f, "[{}{:?}+{:04x}]", pfx, reg, val),
                    Pointer::RegRegDisp16(ref reg1, ref reg2, val) =>
                        write!(f, "[{}{:?}+{:?}+{:04x}]", pfx, reg1, reg2, val)
                }
            }
        }
    }
}

#[derive(Copy, Clone, PartialEq)]
pub enum Pointer {
    Disp16(u16),
    Reg(Register),
    RegReg(Register, Register),
    RegDisp8(Register, u8),
    RegRegDisp8(Register, Register, u8),
    RegDisp16(Register, u16),
    RegRegDisp16(Register, Register, u16)
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Mnemonic {
    ADC, ADD, AND, CALL, CMP, DEC, INC, INT, JMP, LEA, MOV, NEG, NOP, NOT,
    OR, POP, PUSH, RET, SBB, SUB, TEST, XCHG, XOR,
    CLC, STC, CLI, STI, CLD, STD,
    DAA, DAS, AAA, AAS,
    IN, OUT,
    JO, JNO, JB, JNB, JZ, JNZ, JBE, JNBE, JS, JNS, JP, JNP, JL, JNL, JLE, JNLE,
    LOOPNZ, LOOPZ, LOOP, JCXZ,
    MUL, IMUL, DIV, IDIV,
    MOVSB, MOVSW, CMPSB, CMPSW, STOSB, STOSW, LODSB, LODSW, SCASB, SCASW,
    REPZ, REPNZ,
    ROL, ROR, RCL, RCR, SHL, SHR, SAL, SAR
}

impl Mnemonic {
    pub fn is_branch(&self) -> bool {
        match *self {
            Mnemonic::JMP | Mnemonic::JO | Mnemonic::JB | Mnemonic::JNB |
            Mnemonic::JZ | Mnemonic::JNZ | Mnemonic::JBE | Mnemonic::JNBE |
            Mnemonic::JS | Mnemonic::JNS | Mnemonic::JP | Mnemonic::JNP |
            Mnemonic::JL | Mnemonic::JNL | Mnemonic::JNLE | Mnemonic::JCXZ |
            Mnemonic::LOOP | Mnemonic::LOOPNZ | Mnemonic::LOOPZ |
            Mnemonic::CALL => true,
            _ => false
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Register {
    AX, AL, AH, BX, BL, BH, CX, CL, CH, DX, DL, DH,
    BP, SP, SS, CS, DS, ES, DI, SI, FS, GS
}

pub fn get_word(buffer : &[u8], offset: usize) -> u16 {
	let mut word = buffer[offset + 1] as u16;
	word <<= 8;
	word += buffer[offset] as u16;
	return word;
}

pub fn add_rel8(address: usize, rel: i8) -> usize {
    if rel < 0 {
        address - (-(rel as isize) as usize)
    } else {
        address + (rel as usize)
    }
}

pub fn add_rel16(address: usize, rel: i16) -> usize {
    if rel < 0 {
        address - ((-rel) as usize)
    } else {
        address + (rel as usize)
    }
}
