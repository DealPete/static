use std::fmt;
use std::collections::HashMap;
use std::collections::HashSet;
use defs::*;
use graph;

pub struct State {
    regs : Registers,
    flags : Flags,
    memory : HashMap<usize, u8>
}

impl State {
    fn new() -> State {
        State {
            regs : Registers::new(),
            flags : Flags::new(),
            memory : HashMap::new()
        }
    }
    
    pub fn get_reg8(&self, reg: Register) -> Byte {
        let split_low = |reg: &Word| {
            match *reg {
                Word::Undefined => Byte::Undefined,
                Word::AnyValue => Byte::AnyValue,
                Word::Int(ref words) =>
                    Byte::Int({
                        let mut set = HashSet::new();
                        for word in words {
                            set.insert(*word as u8);
                        }
                        set
                    }),
                Word::Bytes(ref byte_high, ref byte_low) => byte_low.clone()
            }
        };
        let split_high = |reg: &Word| {
            match *reg {
                Word::Undefined => Byte::Undefined,
                Word::AnyValue => Byte::AnyValue,
                Word::Int(ref words) =>
                    Byte::Int({
                        let mut set = HashSet::new();
                        for word in words {
                            set.insert((*word >> 8) as u8);
                        }
                        set
                    }),
                Word::Bytes(ref byte_high, ref byte_low) => byte_high.clone()
            }
        };
        match reg {
            Register::AL => split_low(&self.regs.ax),
            Register::AH => split_high(&self.regs.ax),
            Register::BL => split_low(&self.regs.bx),
            Register::BH => split_high(&self.regs.bx),
            Register::CL => split_low(&self.regs.cx),
            Register::CH => split_high(&self.regs.cx),
            Register::DL => split_low(&self.regs.dx),
            Register::DH => split_high(&self.regs.dx),
            _ => panic!("register {:?} is not a byte register.", reg)
        }
    }

    pub fn get_reg16(&self, reg: Register) -> Word {
        match reg {
            Register::AX => self.regs.ax.clone(),
            Register::BX => self.regs.bx.clone(),
            Register::CX => self.regs.cx.clone(),
            Register::DX => self.regs.dx.clone(),
            Register::CS => self.regs.cs.clone(),
            _ => panic!("register {:?} not implemented yet in emulator.", reg)
        }
    }

    pub fn init_byte(&mut self, reg: Register, int: u8) {
        let mut byte_set = HashSet::new();
        byte_set.insert(int);
        self.set_byte(reg, Byte::Int(byte_set));
    }

    pub fn set_byte(&mut self, reg: Register, value: Byte) {
        match reg {
            Register::AL => self.regs.ax = Word::Bytes(self.get_reg8(Register::AH), value),
            Register::AH => self.regs.ax = Word::Bytes(value, self.get_reg8(Register::AL)),
            Register::BL => self.regs.bx = Word::Bytes(self.get_reg8(Register::BH), value),
            Register::BH => self.regs.bx = Word::Bytes(value, self.get_reg8(Register::BL)),
            Register::CL => self.regs.cx = Word::Bytes(self.get_reg8(Register::CH), value),
            Register::CH => self.regs.cx = Word::Bytes(value, self.get_reg8(Register::CL)),
            Register::DL => self.regs.dx = Word::Bytes(self.get_reg8(Register::DH), value),
            Register::DH => self.regs.dx = Word::Bytes(value, self.get_reg8(Register::DL)),
            _ => panic!("can't set byte in register {:?}.", reg)
        }
    }

    pub fn init_word(&mut self, reg: Register, int: u16) {
        let mut word_set = HashSet::new();
        word_set.insert(int);
        self.set_word(reg, Word::Int(word_set));
    }

    pub fn set_word(&mut self, reg: Register, value: Word) {
        match reg {
            Register::AX => self.regs.ax = value,
            Register::BX => self.regs.bx = value,
            Register::CX => self.regs.cx = value,
            Register::DX => self.regs.dx = value,
            Register::CS => self.regs.cs = value,
            Register::SP => self.regs.sp = value,
            _ => panic!("can't set word in register {:?}.", reg)
        }
    }
}

impl fmt::Display for State {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let regs = &self.regs;
        let line1 = format!("AX={}  BX={}  CX={}  DX={}  SP={}  BP={}  SI={}  DI={}",
        regs.ax, regs.bx, regs.cx, regs.dx, regs.sp, regs.bp, regs.si, regs.di);
        let line2 = format!("DS={}  ES={}  SS={}  CS={}  IP={}",
            regs.ds, regs.es, regs.ss, regs.cs, regs.ip);
        return write!(f, "{}\n{}", line1, line2);
    }
}

struct Registers {
    ax : Word,
    bx : Word,
    cx : Word,
    dx : Word,
    sp : Word,
    bp : Word,
    si : Word,
    di : Word,
    cs : Word,
    ds : Word,
    es : Word,
    ss : Word,
    ip : Word
}

impl Registers {
    fn new() -> Registers {
        return Registers {
            ax : Word::Undefined,
            bx : Word::Undefined,
            cx : Word::Undefined,
            dx : Word::Undefined,
            sp : Word::Undefined,
            bp : Word::Undefined,
            si : Word::Undefined,
            di : Word::Undefined,
            cs : Word::Undefined,
            ds : Word::Undefined,
            es : Word::Undefined,
            ss : Word::Undefined,
            ip : Word::Undefined
        }
    }
}

#[derive(Clone)]
enum Word {
    Undefined,
    AnyValue,
    Int(HashSet<u16>),
    Bytes(Byte, Byte),
}

impl fmt::Display for Word {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match *self {
            Word::Undefined => String::from("????"),
            Word::AnyValue => String::from("****"),
            Word::Int(ref word) => String::from("<-->"),
            Word::Bytes(ref reg1, ref reg2) => format!("{}{}", reg1, reg2)
        })
    }
}

#[derive(Clone)]
enum Byte {
    Undefined,
    AnyValue,
    Int(HashSet<u8>),
}

impl fmt::Display for Byte {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match *self {
            Byte::Undefined => String::from("??"),
            Byte::AnyValue => String::from("**"),
            Byte::Int(ref byte) => String::from("<>")
        })
    }
}

struct Flags {
    carry_flag : Flag,
    parity_flag : Flag,
    adjust_flag : Flag,
    zero_flag : Flag,
    sign_flag : Flag,
    int_flag : Flag,
    dir_flag : Flag,
    overflow_flag : Flag
}

impl Flags {
    fn new() -> Flags {
        Flags {
            carry_flag: Flag::Undefined,
            parity_flag: Flag::Undefined,
            adjust_flag: Flag::Undefined,
            zero_flag: Flag::Undefined,
            sign_flag: Flag::Undefined,
            int_flag: Flag::Undefined,
            dir_flag: Flag::Undefined,
            overflow_flag: Flag::Undefined,
        }
    }
}

#[derive(Copy, Clone)]
enum Flag {
    Undefined,
    True,
    False,
    TrueAndFalse
}

// We assume the PSP is loaded at 0x75a and the program at 0x76a.
// This is what DOSBOX uses as a default.
pub fn get_initial_state(buffer: &Vec<u8>) -> State {
    let mut state = State::new();
    state.init_word(Register::DS, 0x75a);
    state.init_word(Register::ES, 0x75a);
    if &buffer[0..2] == [0x4d, 0x5a] {
        state.init_word(Register::SS, get_word(buffer, 0xe).wrapping_add(0x76a));
        state.init_word(Register::SP, get_word(buffer, 0x10));
        state.init_word(Register::IP, get_word(buffer, 0x14));
        state.init_word(Register::CS, get_word(buffer, 0x16).wrapping_add(0x76a));
    } else {
        state.init_word(Register::SS, 0x75a);
        state.init_word(Register::SP, 0xfffe);
        state.init_word(Register::IP, 0x100);
        state.init_word(Register::CS, 0x75a);
    }

    return state;
}

fn emulate(state: &mut State, inst: &Instruction) {
    match inst.mnemonic {
        Mnemonic::MOV => emulate_mov(state, inst),
        Mnemonic::INT | Mnemonic::PUSH => (),
        _ => panic!("unimplemented instruction at {:x}", inst.position)
    }
}

fn emulate_mov(state: &mut State, inst: &Instruction) {
    match inst.op1 {
        Some(Operand::Register8(target_reg)) => {
            match inst.op2 {
                Some(Operand::Register8(source_reg)) => {
                    let reg = state.get_reg8(source_reg);
                    state.set_byte(target_reg, reg);
                },
                Some(Operand::Imm8(imm)) =>
                    state.init_byte(target_reg, imm as u8),
                _ => panic!("Invalid operand!")
            }
        },
        Some(Operand::Register16(target_reg)) => {
            match inst.op2 {
                Some(Operand::Register16(source_reg)) => {
                    let reg = state.get_reg16(source_reg);
                    state.set_word(target_reg, reg);
                },
                Some(Operand::Imm16(imm)) =>
                    state.init_word(target_reg, imm as u16),
                _ => panic!("Invalid operand!")
            }
        },
        _ => panic!("Invalid operand!")
    }
}

fn find_reg_at(reg: Register, inst_index: usize, program: &Program) -> Byte {
    let graph = graph::generate_flow_graph(program);
    let node = match graph.get_node_at(inst_index) {
        None => panic!("Asked to search from instruction not in graph!"),
        Some(node) => node
    };
    let mut state = State::new();
    let mut offset = node.insts[0];

    let mut i = 0;
    while offset != inst_index {
        match program.instructions.get(&offset) {
            None => panic!("There should be an instruction at {:x}!", offset),
            Some(inst) => emulate(&mut state, &inst)
        }
        i += 1;
        offset = node.insts[i];
        println!("{}", state);
    }

    return state.get_reg8(Register::AH);
}

pub fn reg_at_is_always_in(reg: Register, inst_index: usize, values: HashSet<u8>, program: &Program) -> bool {
    match find_reg_at(reg, inst_index, program) {
        Byte::Undefined => panic!("Couldn't determine value of {:?} at 0x{:x}.", reg, inst_index),
        Byte::AnyValue => false,
        Byte::Int(ref reg_values) => reg_values.is_subset(&values)
    }
}
