use std::fmt;
use std::collections::HashMap;
use defs::*;
use graph;

pub struct State {
    ax : Reg16,
    bx : Reg16,
    cx : Reg16,
    dx : Reg16,
    sp : Reg16,
    bp : Reg16,
    si : Reg16,
    di : Reg16,
    cs : Reg16,
    ds : Reg16,
    es : Reg16,
    ss : Reg16,
    ip : Reg16,
    memory : HashMap<usize, u8>
}

impl State {
    fn new() -> State {
        State {
            ax : Reg16::Undefined,
            bx : Reg16::Undefined,
            cx : Reg16::Undefined,
            dx : Reg16::Undefined,
            sp : Reg16::Undefined,
            bp : Reg16::Undefined,
            si : Reg16::Undefined,
            di : Reg16::Undefined,
            cs : Reg16::Undefined,
            ds : Reg16::Undefined,
            es : Reg16::Undefined,
            ss : Reg16::Undefined,
            ip : Reg16::Undefined,
            memory : HashMap::new()
        }
    }

    
    pub fn get_reg8(&self, reg: Register) -> Reg8 {
        struct Split {
            low: Reg8,
            high: Reg8
        };
        let split = |reg: Reg16| {
            match reg {
                Reg16::Undefined => Split { low: Reg8::Undefined, high: Reg8::Undefined },
                Reg16::AnyValue => Split { low: Reg8::AnyValue, high: Reg8::AnyValue },
                Reg16::Word(word) => Split {
                    low: Reg8::Byte(word as u8),
                    high: Reg8::Byte((word >> 8) as u8)
                },
                Reg16::Regs(reg_high, reg_low) => Split { low: reg_low, high: reg_high }
            }
        };
        match reg {
            Register::AL => split(self.ax).low,
            Register::AH => split(self.ax).high,
            Register::BL => split(self.bx).low,
            Register::BH => split(self.bx).high,
            Register::CL => split(self.cx).low,
            Register::CH => split(self.cx).high,
            Register::DL => split(self.dx).low,
            Register::DH => split(self.dx).high,
            _ => panic!("register {:?} is not a byte register.", reg)
        }
    }

    pub fn get_reg16(&self, reg: Register) -> Reg16 {
        match reg {
            Register::AX => self.ax,
            Register::BX => self.bx,
            Register::CX => self.cx,
            Register::DX => self.dx,
            Register::CS => self.cs,
            _ => panic!("register {:?} not implemented yet in emulator.", reg)
        }
    }

    pub fn set_byte(&mut self, reg: Register, value: Reg8) {
        match reg {
            Register::AL => self.ax = Reg16::Regs(self.get_reg8(Register::AH), value),
            Register::AH => self.ax = Reg16::Regs(value, self.get_reg8(Register::AL)),
            Register::BL => self.ax = Reg16::Regs(self.get_reg8(Register::BH), value),
            Register::BH => self.ax = Reg16::Regs(value, self.get_reg8(Register::BL)),
            Register::CL => self.ax = Reg16::Regs(self.get_reg8(Register::CH), value),
            Register::CH => self.ax = Reg16::Regs(value, self.get_reg8(Register::CL)),
            Register::DL => self.ax = Reg16::Regs(self.get_reg8(Register::DH), value),
            Register::DH => self.ax = Reg16::Regs(value, self.get_reg8(Register::DL)),
            _ => panic!("can't set byte in register {:?}.", reg)
        }
    }

    pub fn set_word(&mut self, reg: Register, value: Reg16) {
        match reg {
            Register::AX => self.ax = value,
            Register::BX => self.bx = value,
            Register::CX => self.cx = value,
            Register::DX => self.dx = value,
            Register::CS => self.cs = value,
            _ => panic!("can't set word in register {:?}.", reg)
        }
    }
}

impl fmt::Display for State {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let line1 = format!("AX={}  BX={}  CX={}  DX={}  SP={}  BP={}  SI={}  DI={}",
        self.ax, self.bx, self.cx, self.dx, self.sp, self.bp, self.si, self.di);
        let line2 = format!("DS={}  ES={}  SS={}  CS={}  IP={}",
            self.ds, self.es, self.ss, self.cs, self.ip);
        return write!(f, "{}\n{}", line1, line2);
    }
}

#[derive(Copy, Clone)]
enum Reg16 {
    Undefined,
    AnyValue,
    Word(u16),
    Regs(Reg8, Reg8)
}

impl fmt::Display for Reg16 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match *self {
            Reg16::Undefined => String::from("????"),
            Reg16::AnyValue => String::from("****"),
            Reg16::Word(word) => format!("{:04x}", word),
            Reg16::Regs(reg1, reg2) => format!("{}{}", reg1, reg2)
        })
    }
}

#[derive(Copy, Clone)]
enum Reg8 {
    Undefined,
    AnyValue,
    Byte(u8)
}

impl fmt::Display for Reg8 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            &Reg8::Undefined => String::from("??"),
            &Reg8::AnyValue => String::from("**"),
            &Reg8::Byte(byte) => format!("{:02x}", byte)
        })
    }
}

// We assume the PSP is loaded at 0x75a and the program at 0x76a.
// This is what DOSBOX uses as a default.
pub fn get_initial_state(buffer: &Vec<u8>) -> State {
    let mut state = State::new();
    state.set_word(Register::DS, Reg16::Word(0x75a));
    state.set_word(Register::ES, Reg16::Word(0x75a));
    if &buffer[0..2] == [0x4d, 0x5a] {
        state.set_word(Register::SS, Reg16::Word(get_word(buffer, 0xe).wrapping_add(0x76a)));
        state.set_word(Register::SP, Reg16::Word(get_word(buffer, 0x10)));
        state.set_word(Register::IP, Reg16::Word(get_word(buffer, 0x14)));
        state.set_word(Register::CS, Reg16::Word(get_word(buffer, 0x16).wrapping_add(0x76a)));
    } else {
        state.set_word(Register::SS, Reg16::Word(0x75a));
        state.set_word(Register::SP, Reg16::Word(0xfffe));
        state.set_word(Register::IP, Reg16::Word(0x100));
        state.set_word(Register::CS, Reg16::Word(0x75a));
    }

    return state;
}

fn emulate(state: &mut State, inst: &Instruction) {
    match inst.mnemonic {
        Mnemonic::MOV => emulate_mov(state, inst),
        _ => panic!("unimplemented instruction")
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
                    state.set_byte(target_reg, Reg8::Byte(imm as u8)),
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
                    state.set_word(target_reg, Reg16::Word(imm as u16)),
                _ => panic!("Invalid operand!")
            }
        },
        _ => panic!("Invalid operand!")
    }
}

pub fn find_ah(instruction: &Instruction, program: &Program) -> Option<u8> {
    let graph = graph::generate_flow_graph(program);
    println!("{}", graph);
    println!("{}", instruction.position);
    let node = match graph.get_node_at(instruction.position) {
        None => panic!("Asked to search from instruction not in graph!"),
        Some(node) => node
    };
    let mut state = State::new();
    let mut offset = node.insts[0];

    let mut i = 0;
    while node.insts[i] != offset {
        match program.instructions.get(&offset) {
            None => panic!("There should be an instruction at {:x}!", offset),
            Some(inst) => emulate(&mut state, &inst)
        }
        i += 1;
    }

    return match state.get_reg8(Register::AH) {
        Reg8::Undefined => None,
        Reg8::AnyValue => None,
        Reg8::Byte(byte) => Some(byte)
    };
}
