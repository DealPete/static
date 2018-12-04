use defs::main::*;
use x86::dis;
use std::fmt;

#[derive(Copy, Clone)]
pub struct X86 {
}

impl<'a> Architecture<Instruction> for X86 {
    fn decode_instruction(&self, buffer: &[u8], offset: usize) -> Result<Instruction, String> {
        dis::decode_instruction(buffer, offset)
    }

    fn successors(&self, instruction: Instruction, offset: usize) -> (Vec<usize>, Vec<usize>, bool) {
        println!("{}", instruction);
        match instruction.mnemonic {
            Mnemonic::JB | Mnemonic::JO | Mnemonic::JNO
            | Mnemonic::JB | Mnemonic::JNB | Mnemonic::JZ
            | Mnemonic::JNZ | Mnemonic::JBE | Mnemonic::JNBE
            | Mnemonic::JS | Mnemonic::JNS | Mnemonic::JP
            | Mnemonic::JNP | Mnemonic::JL | Mnemonic::JNL
            | Mnemonic::JLE | Mnemonic::JNLE | Mnemonic::CALL =>
                match instruction.unpack_op1() {
                    Operand::Imm8(byte) => {
                        let base = offset + instruction.length;
                        let target = add_rel8(base, byte);
                        (vec!(base, target), vec!(target), false)
                    },
                    Operand::Imm16(word) => {
                        let base = offset + instruction.length;
                        let target = add_rel16(base, word);
                        (vec!(base, target), vec!(target), false)
                    },
                    _ => panic!("expected byte or word operand for branch")
                },
            Mnemonic::JMP =>
                match instruction.unpack_op1() {
                    Operand::Imm8(byte) => {
                        let base = offset + instruction.length;
                        let target = add_rel8(base, byte);
                        (vec!(target), vec!(target), false)
                    },
                    Operand::Imm16(word) => {
                        let base = offset + instruction.length;
                        let target = add_rel16(base, word);
                        (vec!(target), vec!(target), false)
                    },
                    Operand::Pointer(_) => (Vec::new(), Vec::new(), true),
                    _ => panic!("unexpected JMP operand")
                },
            Mnemonic::RET => (Vec::new(), Vec::new(), false),
            Mnemonic::INT => match instruction.unpack_op1() {
                Operand::Imm8(0x20) =>
                    (Vec::new(), Vec::new(), false),
                Operand::Imm8(0x21) =>
                    (Vec::new(), Vec::new(), true),
                Operand::Imm8(0x27) =>
                    (Vec::new(), Vec::new(), false),
                _ => (vec!(offset + instruction.length), Vec::new(), false)
            },
            _ => (vec!(offset + instruction.length), Vec::new(), false)
        }
    }
}
/*
    fn simulate_next_instruction<C: Context<State<'a>, Instruction>>(&self, state: State<'a>, context: &C, instruction: Instruction) -> SimResult<State<'a>> {
        sim::simulate_next_instruction(state, context, instruction)
    }
*/
impl Listing<Instruction> {
    pub fn print_instructions(&self) {
        let mut output = String::new();
        for i in 0..self.highest_offset+1 {
            if let Some(instruction) = self.instructions.get(&i) {
                let prefix =
                    if self.is_labelled(i) {
                        format!("{:4x}:   ", i)
                    } else {
                        format!("        ")
                    };
                let indeterminate = if self.is_indeterminate(i) {
                    "* "
                } else {
                    ""
                };
                let inst_output = if instruction.is_rel_branch() {
                    let mut target = i + instruction.length;
                    match instruction.op1 {
                        Some(Operand::Imm8(rel)) => target = add_rel8(target, rel),
                        Some(Operand::Imm16(rel)) => target = add_rel16(target, rel),
                        _ => panic!("Wrong operand for relative jump.")
                    }
                    format!("{:?} <{:x}>", instruction.mnemonic, target)
                } else {
                    format!("{}", instruction)
                };
                output.push_str(format!("{}{}{}{}\n", prefix.as_str(), indeterminate, inst_output,
                    if i == self.entry_offset {
                        "\t; program entry point"
                    } else {
                        ""
                    }).as_str());
            }
        }
        println!("{}", output);
    }
}

#[derive(Copy, Clone)]
pub struct Instruction {
    pub rep_prefix: Option<Mnemonic>,
    pub mnemonic: Mnemonic,
    pub length: usize,
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

    pub fn unpack_op1(&self) -> Operand {
        self.op1.expect(
            format!("Instruction doesn't have a first operand: {}", self).as_str())
    }

    pub fn unpack_op2(&self) -> Operand {
        self.op2.expect(
            format!("Instruction doesn't have a second operand: {}", self).as_str())
    }
}

impl InstructionTrait for Instruction {
    fn length(&self) -> usize {
        self.length
    }

    fn is_call(&self) -> bool {
        self.mnemonic == Mnemonic::CALL
    }

    fn is_return(&self) -> bool {
        self.mnemonic == Mnemonic::RET
    }

    fn is_rel_branch(&self) -> bool {
        self.mnemonic.is_branch() && match self.op1 {
            Some(Operand::Imm8(_)) | Some(Operand::Imm16(_)) => true,
            _ => false
        }
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut prefix = String::new();
        if let Some(ref rep_prefix) = self.rep_prefix {
            prefix.push_str(format!("{:?} ", rep_prefix).as_str());
        }
        match &self.op1 {
            &None => write!(f, "{}{:?}", prefix, self.mnemonic),
            &Some(ref op1) => match self.op2 {
                None => write!(f, "{}{:?} {}", prefix, self.mnemonic, op1),
                Some(ref op2) =>
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
    Pointer(Pointer),
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Operand::Register8(ref reg) |
                &Operand::Register16(ref reg) => write!(f, "{:?}", reg),
            &Operand::Imm8(val) => write!(f, "{:02x}", val),
            &Operand::Imm16(val) => write!(f, "{:04x}", val),
            &Operand::Pointer(pointer) => {
                let pfx = match pointer.segment {
                    Register::DS => String::from(""),
                    segment => format!("{:?}:", segment)
                };
                match pointer.value {
                    PtrType::Disp16(val) => write!(f, "[{}{:04x}]", pfx, val),
                    PtrType::Reg(ref reg) => write!(f, "[{}{:?}]", pfx, reg),
                    PtrType::RegReg(ref reg1, ref reg2) =>
                        write!(f, "[{}{:?}+{:?}]", pfx, reg1, reg2),
                    PtrType::RegDisp8(ref reg, val) =>
                        write!(f, "[{}{:?}+{:02x}]", pfx, reg, val),
                    PtrType::RegRegDisp8(ref reg1, ref reg2, val) =>
                        write!(f, "[{}{:?}+{:?}+{:02x}]", pfx, reg1, reg2, val),
                    PtrType::RegDisp16(ref reg, val) =>
                        write!(f, "[{}{:?}+{:04x}]", pfx, reg, val),
                    PtrType::RegRegDisp16(ref reg1, ref reg2, val) =>
                        write!(f, "[{}{:?}+{:?}+{:04x}]", pfx, reg1, reg2, val)
                }
            }
        }
    }
}

#[derive(Copy, Clone, PartialEq)]
pub struct Pointer {
    pub segment: Register,
    pub size: u8,
    pub value: PtrType
}

impl Pointer {
    pub fn new(size: u8, ptr_type: PtrType) -> Pointer {
        Pointer {
            segment: Register::DS,
            size: size,
            value: ptr_type
        }
    }

    pub fn set_segment(self, reg: Register) -> Pointer {
        Pointer {
            segment: reg,
            .. self
        }
    }
}

#[derive(Copy, Clone, PartialEq)]
pub enum PtrType {
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
