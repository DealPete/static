use defs::*;
use std::fmt;

#[derive(Copy, Clone)]
pub struct Instruction {
    pub mnemonic: Mnemonic,
    pub op1: Option<Operand>,
    pub op2: Option<Operand>,
    pub op3: Option<Operand>
}

impl Instruction {
    pub fn new(mnemonic: Mnemonic) -> Instruction {
        Instruction {
            mnemonic: mnemonic,
            op1: None,
            op2: None,
            op3: None
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
    fn is_rel_branch(&self) -> bool {
        false
    }

    fn is_return(&self) -> bool {
        self.mnemonic == Mnemonic::RET
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.op1 {
            &None => write!(f, "{:?}", self.mnemonic),
            &Some(ref op1) => match self.op2 {
                None => write!(f, "{:?} {}", self.mnemonic, op1),
                Some(ref op2) => match self.op3 {
                    None => write!(f, "{:?} {}, {}", self.mnemonic, op1, op2),
                    Some(ref op3) =>
                        write!(f, "{:?} {}, {}, {}", self.mnemonic, op1, op2, op3)
                }
            }
        }
    }
}

#[derive(Copy, Clone, PartialEq)]
pub enum Operand {
    I,
    V(u8),
    Address(u16),
    Byte(u8),
    KeyPress,
    DelayTimer,
    SoundTimer,
    Numeral(u8),
    Pointer
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Operand::I => write!(f, "I"),
            &Operand::V(x) => write!(f, "V{:X}", x),
            &Operand::Address(address) => write!(f, "{:x}", address),
            &Operand::Byte(byte) => write!(f, "{:x}", byte),
            &Operand::KeyPress => write!(f, "Key-Press"),
            &Operand::DelayTimer => write!(f, "Delay-timer"),
            &Operand::SoundTimer => write!(f, "Sound-Timer"),
            &Operand::Numeral(x) => write!(f, "Numeral-V{:X}", x),
            &Operand::Pointer => write!(f, "[I]")
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Mnemonic {
    CLS, RET, JP, CALL, SE, SNE, LD, ADD, OR, AND, XOR, SUB, SHR, SUBN,
    SHL, RND, DRW, SKP, SKNP, LDBCD, LDPTR
}
