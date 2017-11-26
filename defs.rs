use std::fmt;
use std::collections::HashMap;

pub struct Program {
    pub length: usize,
    pub instructions: HashMap<usize, Instruction>
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut output = String::new();
        for i in 0..self.length {
            if let Some(instruction) = self.instructions.get(&i) {
                output.push_str(format!("{}\n", instruction).as_str());
            }
        }
        return write!(f, "{}", output);
    }
}

pub struct Instruction {
    pub position: usize,
    pub previous: Option<usize>,
    pub label: bool,
    pub seg_prefix: Option<Register>,
    pub rep_prefix: Option<Mnemonic>,
    pub mnemonic: Mnemonic,
    pub size: u8,
    pub op1: Operand,
    pub op2: Operand
}

impl Instruction {
    pub fn new(mnemonic: Mnemonic) -> Instruction {
        Instruction {
            position: 0,
            previous: None,
            seg_prefix: None,
            rep_prefix: None,
            mnemonic: mnemonic,
            size: 0,
            label: false,
            op1: Operand::None,
            op2: Operand::None
        }
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut prefix = String::new();
        if self.label {
            prefix.push_str(format!("{:4x}:   ", self.position).as_str())
        } else {
            prefix.push_str("        ")
        }
        if let Some(ref rep_prefix) = self.rep_prefix {
            prefix.push_str(format!("{:?} ", rep_prefix).as_str());
        }
        if self.mnemonic.is_jump() {
            let mut target = self.position + self.size as usize;
            if let Operand::Imm8(rel) = self.op1 {
                target = add_rel8(target, rel);
            } else if let Operand::Imm16(rel) = self.op1 {
                target = add_rel16(target, rel);
            } else {
                panic!("Wrong operand for jump instruction.");
            }
            write!(f, "{}{:?} <{:x}>", prefix, self.mnemonic, target)
        } else if let Operand::None = self.op1 {
            write!(f, "{}{:?}", prefix, self.mnemonic)
        } else if let Operand::None = self.op2 {
            write!(f, "{}{:?} {}", prefix, self.mnemonic,
                self.op1.format(&self.seg_prefix))
        } else {
            write!(f, "{}{:?} {}, {}", prefix, self.mnemonic,
                self.op1.format(&self.seg_prefix),
                self.op2.format(&self.seg_prefix))
        }
    }
}

pub enum Operand {
    None,
    Register(Register),
    Imm8(i8),
    Imm16(i16),
    Ptr16(u16),
    PtrReg(Register),
    PtrRegReg(Register, Register),
    PtrRegDisp8(Register, u8),
    PtrRegRegDisp8(Register, Register, u8),
    PtrRegDisp16(Register, u16),
    PtrRegRegDisp16(Register, Register, u16)
}

impl Operand {
    pub fn format(&self, prefix: &Option<Register>) -> String {
        let pfx = match prefix {
            &Some(ref reg) => format!("{:?}:", reg),
            &None => String::from("")
        };
        match self {
            &Operand::None => format!(""),
            &Operand::Register(ref reg) => format!("{:?}", reg),
            &Operand::Imm8(val) => format!("{:02x}", val),
            &Operand::Imm16(val) => format!("{:04x}", val),
            &Operand::Ptr16(val) => format!("[{}{:04x}]", pfx, val),
            &Operand::PtrReg(ref reg) => format!("[{}{:?}]", pfx, reg),
            &Operand::PtrRegReg(ref reg1, ref reg2) =>
                format!("[{}{:?}+{:?}]", pfx, reg1, reg2),
            &Operand::PtrRegDisp8(ref reg, val) =>
                format!("[{}{:?}+{:02x}]", pfx, reg, val),
            &Operand::PtrRegRegDisp8(ref reg1, ref reg2, val) =>
                format!("[{}{:?}+{:?}+{:02x}]", pfx, reg1, reg2, val),
            &Operand::PtrRegDisp16(ref reg, val) =>
                format!("[{}{:?}+{:04x}]", pfx, reg, val),
            &Operand::PtrRegRegDisp16(ref reg1, ref reg2, val) =>
                format!("[{}{:?}+{:?}+{:04x}]", pfx, reg1, reg2, val)
        }
    }
}

#[derive(Debug)]
pub enum Mnemonic {
    ADC, ADD, AND, CALL, CMP, DEC, INC, INT, JMP, LEA, MOV, NEG, NOP, NOT,
    OR, POP, PUSH, RET, SBB, SUB, TEST, XCHG, XOR,
    CLC, STC, CLI, STI, CLD, STD,
    IN, OUT,
    JO, JNO, JB, JNB, JZ, JNZ, JBE, JNBE, JS, JNS, JP, JNP, JL, JNL, JLE, JNLE,
    LOOPNZ, LOOPZ, LOOP, JCXZ,
    MUL, IMUL, DIV, IDIV,
    MOVSB, MOVSW, CMPSB, CMPSW, STOSB, STOSW, LODSB, LODSW, SCASB, SCASW,
    REPZ, REPNZ,
    ROL, ROR, RCL, RCR, SHL, SHR, SAL, SAR
}

impl Mnemonic {
    pub fn is_jump(&self) -> bool {
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

#[derive(Debug)]
pub enum Register {
    AX, AL, AH, BX, BL, BH, CX, CL, CH, DX, DL, DH,
    BP, SP, SS, CS, DS, ES, DI, SI, FS, GS
}

pub fn get_word(buffer : &Vec<u8>, offset: usize) -> u16 {
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
