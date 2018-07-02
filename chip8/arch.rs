use defs::*;
use chip8::dis;
use chip8::sim;
use chip8::state::{State};
use std::fmt;

#[derive(Copy, Clone)]
pub struct Chip8 {
}

impl<'a> Architecture<State<'a>, Instruction> for Chip8 {
    fn decode_instruction(&self, buffer: &[u8], offset: usize) -> Result<Instruction, String> {
        dis::decode_instruction(buffer, offset)
    }

    fn simulate_next_instruction<C: Context<State<'a>, Instruction>>(&self, state: State<'a>,
        context: &C, instruction: Instruction) -> SimResult<State<'a>> {
        sim::simulate_next_instruction(state, context, instruction)
    }

    fn naive_successors(&self, instruction: Instruction, offset: usize) -> (Vec<usize>, Vec<usize>, bool) {
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

    fn true_successors(&self, analysis: &Analysis<State<'a>, Instruction>, offset: usize) -> (Vec<usize>, Vec<usize>, bool) {
        let instruction = analysis.instructions.get(&offset).expect("No instruction at offset!");
        println!("INSTRUCTION {}", instruction);
        self.naive_successors(*analysis.instructions.get(&offset).unwrap(), offset)
    }
}

impl<'a> Analysis<State<'a>, Instruction> {
    pub fn print_instructions(&self) {
        let mut output = String::new();
        let mut last_inst_was_skip = false;
        for i in 0..0x1000 {
            if let Some(instruction) = self.instructions.get(&i) {
                if self.flow_graph.is_labelled(i) {
                    output.push_str(format!("{:4x}:   ", i + 0x200).as_str());
                } else {
                    output.push_str("        ");
                }

                if last_inst_was_skip {
                    output.push_str("    ");
                }
                last_inst_was_skip = false;
                    
                output.push_str(format!("{}{}\n",
                    if self.flow_graph.is_indeterminate(i) {
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
                
pub struct Interpreter {
}

impl<'a> Context<State<'a>, Instruction> for Interpreter {
    fn entry_offset(&self, state: &State<'a>) -> usize {
        0
    }

    fn next_inst_offset(&self, state: &State<'a>) -> usize {
        (state.pc - 0x200) as usize
    }

    fn simulate_system_call(&self, state: State<'a>, inst: Instruction) -> Option<State<'a>> {
        panic!("Can't simluate system calls in interpreter");
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

#[derive(Copy, Clone, PartialEq)]
pub enum Operand {
    I,
    V(usize),
    Address(u16),
    Byte(u8),
    KeyPress,
    DelayTimer,
    SoundTimer,
    Numeral(usize),
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
