use defs::main::*;
use chip8::dis;
use std::fmt;

#[derive(Copy, Clone)]
pub struct Chip8 {
}

impl<'a> Architecture<Instruction> for Chip8 {
    fn decode_instruction(&self, buffer: &[u8], offset: usize) -> Result<Instruction, String> {
        dis::decode_instruction(buffer, offset)
    }

    fn print_listing(listing: &Listing<Instruction>) {
        let mut output = String::new();
        let mut last_inst_was_skip = false;
        for i in 0..0x1000 {
            if let Some(instruction) = listing.instructions.get(&i) {
                if listing.is_labelled(i) {
                    output.push_str(format!("{:4x}:   ", i + 0x200).as_str());
                } else {
                    output.push_str("        ");
                }

                if last_inst_was_skip {
                    output.push_str("    ");
                }
                last_inst_was_skip = false;
                    
                output.push_str(format!("{}{}\n",
                    if listing.is_indeterminate(i) {
                        "* "
                    } else {
                        ""
                    }, instruction).as_str());
                if instruction.mnemonic == Mnemonic::SE
                    || instruction.mnemonic == Mnemonic::SNE
                    || instruction.mnemonic == Mnemonic::SKP
                    || instruction.mnemonic == Mnemonic::SKNP {
                    if let None = listing.instructions.get(&(i+2)) {
                        output.push_str("            ????\n");
                    } else {
                        last_inst_was_skip = true;
                    }
                }
            }
        }
        println!("{}", output);
    }
}
                
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

    pub fn unpack_op3(&self) -> Operand {
        self.op3.expect(
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

    fn is_call(&self) -> bool {
        self.mnemonic == Mnemonic::CALL
    }

    fn length(&self) -> usize {
        2
    }

    fn successors(&self, offset: usize) -> (Vec<usize>, Vec<usize>, bool) {
        match self.mnemonic {
            Mnemonic::CALL => match self.unpack_op1() {
                Operand::Address(address) =>
                    (vec!(offset + 2, address as usize - 0x200),
                        vec!(address as usize - 0x200), false),
                _ => panic!("CALL instruction should have address as operand.")
            },
            Mnemonic::JP => match self.unpack_op1() {
                Operand::Address(address) =>
                    (vec!(address as usize - 0x200), vec!(address as usize - 0x200),
                    false),
                _ => (Vec::new(), Vec::new(), true)
            },
            Mnemonic::RET => (Vec::new(), Vec::new(), false),
            Mnemonic::SE | Mnemonic::SNE | Mnemonic::SKP | Mnemonic::SKNP =>
                (vec!(offset + 2, offset + 4), Vec::new(), false),
            Mnemonic::EXIT => (Vec::new(), Vec::new(), false),
            _ => (vec!(offset + 2), Vec::new(), false)
        }
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

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum Operand {
    I,
    V(usize),
    Address(u16),
    Byte(u8),
    KeyPress,
    DelayTimer,
    SoundTimer,
    Numeral(usize),
    LargeNumeral(usize),
    UserFlags,
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
            &Operand::DelayTimer => write!(f, "Delay-Timer"),
            &Operand::SoundTimer => write!(f, "Sound-Timer"),
            &Operand::Numeral(x) => write!(f, "Numeral-V{:X}", x),
            &Operand::LargeNumeral(x) => write!(f, "Large-Numeral-V{:X}", x),
            &Operand::UserFlags => write!(f, "User-Flags"),
            &Operand::Pointer => write!(f, "[I]")
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Mnemonic {
    CLS, RET, JP, CALL, SE, SNE, LD, ADD, OR, AND, XOR, SUB, SHR, SUBN,
    SHL, RND, DRW, SKP, SKNP, LDBCD, LDPTR,
    SCD, SCR, SCL, EXIT, LOW, HIGH
}
