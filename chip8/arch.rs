use defs::*;
use chip8::dis;
use std::fmt;

#[derive(Copy, Clone)]
pub struct Chip8 {
}

impl<'a> Architecture<Instruction> for Chip8 {
    fn decode_instruction(&self, buffer: &[u8], offset: usize) -> Result<Instruction, String> {
        dis::decode_instruction(buffer, offset)
    }

    fn successors(&self, instruction: Instruction, offset: usize) -> (Vec<usize>, Vec<usize>, bool) {
        match instruction.mnemonic {
            Mnemonic::CALL => match instruction.unpack_op1() {
                Operand::Address(address) =>
                    (vec!(address as usize - 0x200, offset + 2), vec!(address as usize - 0x200), false),
                _ => panic!("CALL instruction should have address as operand.")
            },
            Mnemonic::JP => match instruction.unpack_op1() {
                Operand::Address(address) =>
                    (vec!(address as usize - 0x200), vec!(address as usize - 0x200),
                    false),
                _ => (Vec::new(), Vec::new(), true)
            },
            Mnemonic::RET => (Vec::new(), Vec::new(), false),
            Mnemonic::SE | Mnemonic::SNE | Mnemonic::SKP | Mnemonic::SKNP =>
                (vec!(offset + 2, offset + 4), Vec::new(), false),
            _ => (vec!(offset + 2), Vec::new(), false)
        }
    }

/*
    fn true_successors(&self, analysis: &Analysis<State<'a>, Instruction>, offset: usize) -> (Vec<usize>, Vec<usize>, bool) {
        let instruction = analysis.instructions.get(&offset).expect("No instruction at offset!");
        let (mut addresses, mut labels, incomplete) = self.naive_successors(instruction.clone(), offset);
        
        if incomplete {
            match search::find_value(Operand::V(0), analysis, offset) {
                Value::Byte(byte) => {
                    match byte {
                        Byte::Undefined => panic!("Can't jump to undefined address!"),
                        Byte::AnyValue => panic!("Can't jump to every address!"),
                        Byte::Int(set) => {
                            let operand = instruction.unpack_op2();
                            if let Operand::Address(jump_offset) = operand {
                                for value in set {
                                    let destination = (value as u16 + jump_offset - 0x200) as usize;
                                    addresses.push(destination);
                                    labels.push(destination);
                                }
                            } else {
                                panic!("indirect jump should have address as second operand");
                            }
                        }
                    }
                },
                _ => panic!("V0 should have byte value")
            }
        }

        (addresses, labels, incomplete)
    }
*/
}

impl Listing<Instruction> {
    pub fn print_instructions(&self) {
        let mut output = String::new();
        let mut last_inst_was_skip = false;
        for i in 0..0x1000 {
            if let Some(instruction) = self.instructions.get(&i) {
                if self.is_labelled(i) {
                    output.push_str(format!("{:4x}:   ", i + 0x200).as_str());
                } else {
                    output.push_str("        ");
                }

                if last_inst_was_skip {
                    output.push_str("    ");
                }
                last_inst_was_skip = false;
                    
                output.push_str(format!("{}{}\n",
                    if self.is_indeterminate(i) {
                        "* "
                    } else {
                        ""
                    }, instruction).as_str());
                if instruction.mnemonic == Mnemonic::SE
                    || instruction.mnemonic == Mnemonic::SNE
                    || instruction.mnemonic == Mnemonic::SKP
                    || instruction.mnemonic == Mnemonic::SKNP {
                    if let None = self.instructions.get(&(i+2)) {
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

#[derive(Copy, Clone, PartialEq, Debug)]
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
