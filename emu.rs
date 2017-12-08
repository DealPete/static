use std::fmt;
use std::collections::HashMap;
use std::collections::HashSet;
use defs::*;

pub struct State {
    pub cs : u16,
    pub ip : u16,
    regs : Registers,
    flags : Flags,
    memory : HashMap<usize, Byte>
}

impl State {
    pub fn new() -> State {
        State {
            cs : 0,
            ip : 0,
            regs : Registers::new(),
            flags : Flags::new(),
            memory : HashMap::new()
        }
    }
    
    pub fn union(self, state: State) -> State {
        if self.cs != state.cs || self.ip != state.ip {
            panic!("Unifying states with different cs/ip unimplemented");
        };
        State {
            cs: self.cs,
            ip: self.ip,
            regs: self.regs.union(state.regs),
            flags: self.flags.union(state.flags),
            memory: {
                let mut new_memory = HashMap::new();
                for (offset, byte) in self.memory {
                    new_memory.insert(offset, byte);
                }
                for (offset, byte) in state.memory {
                    new_memory.insert(offset, byte);
                }
                new_memory
            }
        }
    }

    pub fn is_subset(&self, state: &State) -> bool {
        for (offset, byte1) in self.memory.iter() {
            match state.memory.get(&offset) {
                None => return false,
                Some(ref byte2) => if !byte1.is_subset(byte2) {
                    return false;
                }
            }
        };
        self.cs == state.cs &&
        self.ip == state.ip &&
        self.regs.is_subset(&state.regs) &&
        self.flags.is_subset(state.flags)
    }

    pub fn get_reg8(&self, reg: Register) -> Byte {
        match reg {
            Register::AL => self.regs.ax.split_low(),
            Register::AH => self.regs.ax.split_high(),
            Register::BL => self.regs.bx.split_low(),
            Register::BH => self.regs.bx.split_high(),
            Register::CL => self.regs.cx.split_low(),
            Register::CH => self.regs.cx.split_high(),
            Register::DL => self.regs.dx.split_low(),
            Register::DH => self.regs.dx.split_high(),
            _ => panic!("register {:?} is not a byte register.", reg)
        }
    }

    pub fn get_reg16(&self, reg: Register) -> Word {
        match reg {
            Register::AX => self.regs.ax.clone(),
            Register::BX => self.regs.bx.clone(),
            Register::CX => self.regs.cx.clone(),
            Register::DX => self.regs.dx.clone(),
            Register::CS => {
                let mut set = HashSet::new();
                set.insert(self.cs);
                Word::Int(set)
            },
            Register::DS => self.regs.ds.clone(),
            _ => panic!("register {:?} not implemented yet in emulator.", reg)
        }
    }

    pub fn set_reg8(self, reg: Register, value: Byte) -> State {
        State {
            regs: match reg {
                Register::AL => Registers {
                    ax: Word::Bytes(value, self.get_reg8(Register::AH)),
                    .. self.regs
                },
                Register::AH => Registers {
                    ax: Word::Bytes(self.get_reg8(Register::AL), value),
                    .. self.regs
                },
                Register::BL => Registers {
                    bx: Word::Bytes(value, self.get_reg8(Register::BH)),
                    .. self.regs
                },
                Register::BH => Registers {
                    bx: Word::Bytes(self.get_reg8(Register::BL), value),
                    .. self.regs
                },
                Register::CL => Registers {
                    cx: Word::Bytes(value, self.get_reg8(Register::CH)),
                    .. self.regs
                },
                Register::CH => Registers {
                    cx: Word::Bytes(self.get_reg8(Register::CL), value),
                    .. self.regs
                },
                Register::DL => Registers {
                    ax: Word::Bytes(value, self.get_reg8(Register::DH)),
                    .. self.regs
                },
                Register::DH => Registers {
                    dx: Word::Bytes(self.get_reg8(Register::DL), value),
                    .. self.regs
                },
            _ => panic!("can't set byte in register {:?}.", reg)
            },
            .. self
        }
    }

    pub fn set_reg16(self, reg: Register, value: Word) -> State {
        State {
            regs: match reg {
                Register::AX => Registers { ax: value, .. self.regs },
                Register::BX => Registers { bx: value, .. self.regs },
                Register::CX => Registers { cx: value, .. self.regs },
                Register::DX => Registers { dx: value, .. self.regs },
                Register::CS => panic!("You can't set CS directly on x86."),
                Register::SP => Registers { sp: value, .. self.regs },
                Register::SS => Registers { ss: value, .. self.regs },
                Register::BP => Registers { bp: value, .. self.regs },
                Register::SI => Registers { si: value, .. self.regs },
                Register::DI => Registers { di: value, .. self.regs },
                Register::DS => Registers { ds: value, .. self.regs },
                Register::ES => Registers { es: value, .. self.regs },
            _ => panic!("can't set word in register {:?}.", reg)
            },
            .. self
        }
    }
}

impl fmt::Display for State {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let regs = &self.regs;
        let line1 = format!("AX={}  BX={}  CX={}  DX={}  SP={}  BP={}  SI={}  DI={}",
        regs.ax, regs.bx, regs.cx, regs.dx, regs.sp, regs.bp, regs.si, regs.di);
        let line2 = format!("DS={}  ES={}  SS={}  CS={:04x}  IP={:04x}",
            regs.ds, regs.es, regs.ss, self.cs, self.ip);
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
    ds : Word,
    es : Word,
    ss : Word
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
            ds : Word::Undefined,
            es : Word::Undefined,
            ss : Word::Undefined
        }
    }

    pub fn union(self, regs: Registers) -> Registers {
        Registers {
            ax: self.ax.union(regs.ax),
            bx: self.bx.union(regs.bx),
            cx: self.cx.union(regs.cx),
            dx: self.dx.union(regs.dx),
            sp: self.sp.union(regs.sp),
            bp: self.bp.union(regs.bp),
            si: self.si.union(regs.si),
            di: self.di.union(regs.di),
            ds: self.ds.union(regs.ds),
            es: self.es.union(regs.es),
            ss: self.ss.union(regs.ss)
        }
    }

    pub fn is_subset(&self, regs: &Registers) -> bool {
        self.ax.is_subset(&regs.ax) &&
        self.bx.is_subset(&regs.bx) &&
        self.cx.is_subset(&regs.cx) &&
        self.dx.is_subset(&regs.dx) &&
        self.sp.is_subset(&regs.sp) &&
        self.bp.is_subset(&regs.bp) &&
        self.si.is_subset(&regs.si) &&
        self.di.is_subset(&regs.di) &&
        self.ds.is_subset(&regs.ds) &&
        self.es.is_subset(&regs.es) &&
        self.ss.is_subset(&regs.ss)
    }
}

#[derive(Clone, PartialEq)]
pub enum Word {
    Undefined,
    AnyValue,
    Int(HashSet<u16>),
    Bytes(Byte, Byte),
}

impl Word {
    fn new(word: u16) -> Word {
        let mut word_set = HashSet::new();
        word_set.insert(word);
        Word::Int(word_set)
    }

    fn union(self, word: Word) -> Word {
        if let Word::Bytes(byte1, byte2) = word {
            Word::Bytes(byte1.union(self.split_low()),
                byte2.union(self.split_high()))
        } else if let Word::Bytes(byte1, byte2) = self { 
            Word::Bytes(byte1.union(word.split_low()),
                byte2.union(word.split_high()))
        } else {
            match self {
                Word::Undefined => Word::Undefined,
                Word::AnyValue => Word::AnyValue,
                Word::Int(set1) => match word {
                    Word::Undefined => Word::Undefined,
                    Word::AnyValue => Word::AnyValue,
                    Word::Int(set2) => Word::Int(set1.union(&set2).cloned().collect()),
                    _ => panic!("invalid word")
                    },
                _ => panic!("invalid word")
            }
        }
    }

    fn is_subset(&self, word: &Word) -> bool {
        if let Word::Bytes(ref byte1, ref byte2) = *word {
            self.split_low().is_subset(byte1) &&
            self.split_high().is_subset(byte2)
        } else if let Word::Bytes(ref byte1, ref byte2) = *self {
            byte1.is_subset(&word.split_low()) &&
            byte2.is_subset(&word.split_high())
        } else {
            match *self {
                Word::Undefined => match *word {
                    Word::Undefined | Word::AnyValue => true,
                    _ => false
                },
                Word::AnyValue => match *word {
                    Word::AnyValue => true,
                    _ => false
                },
                Word::Int(ref set1) => match *word {
                    Word::Undefined => false,
                    Word::AnyValue => true,
                    Word::Int(ref set2) => set1.is_subset(&set2),
                    _ => panic!("shouldn't be here")
                },
                _ => panic!("shouldn't be here")
            }
        }
    }

    fn split_low(&self) -> Byte {
        match *self {
            Word::Undefined => Byte::Undefined,
            Word::AnyValue => Byte::AnyValue,
            Word::Int(ref words) =>
                Byte::Int({
                    let mut set = HashSet::new();
                    for word in words.iter() {
                        set.insert(*word as u8);
                    };
                    set
                }),
            Word::Bytes(ref byte_low, ref byte_high) => byte_low.clone()
        }
    }

    fn split_high(&self) -> Byte {
        match *self {
            Word::Undefined => Byte::Undefined,
            Word::AnyValue => Byte::AnyValue,
            Word::Int(ref words) =>
                Byte::Int({
                    let mut set = HashSet::new();
                    for word in words {
                        set.insert((*word >> 8) as u8);
                    };
                    set
                }),
            Word::Bytes(ref byte_low, ref byte_high) => byte_high.clone()
        }
    }
}

impl fmt::Display for Word {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match *self {
            Word::Undefined => String::from("????"),
            Word::AnyValue => String::from("****"),
            Word::Int(ref set) =>
                if set.len() == 1 {
                    format!("{:04x}", set.iter().collect::<Vec<&u16>>()[0])
                } else {
                    String::from("{--}")
                },
            Word::Bytes(ref reg1, ref reg2) => format!("{}{}", reg2, reg1)
        })
    }
}

#[derive(Clone, PartialEq)]
pub enum Byte {
    Undefined,
    AnyValue,
    Int(HashSet<u8>),
}

impl Byte {
    fn new(byte: u8) -> Byte {
        let mut byte_set = HashSet::new();
        byte_set.insert(byte);
        Byte::Int(byte_set)
    }

    fn combine(self, byte: Byte) -> Word {
        match self {
            Byte::Undefined => Word::Undefined,
            Byte::AnyValue => Word::AnyValue,
            Byte::Int(set1) => match byte {
                Byte::Undefined => Word::Undefined,
                Byte::AnyValue => Word::AnyValue,
                Byte::Int(set2) => {
                    let mut words = HashSet::new();
                    for bytel in set1 {
                        for byteh in set2.iter() {
                            words.insert(bytel as u16 + ((*byteh as u16) << 8));
                        }
                    }
                    Word::Int(words)
                }
            }
        }
    }

    fn union(self, byte: Byte) -> Byte {
        match self {
            Byte::Undefined => Byte::Undefined,
            Byte::AnyValue => Byte::AnyValue,
            Byte::Int(set1) => match byte {
                Byte::Undefined => Byte::Undefined,
                Byte::AnyValue => Byte::AnyValue,
                Byte::Int(set2) => Byte::Int(set1.union(&set2).cloned().collect())
            }
        }
    }

    fn is_subset(&self, byte: &Byte) -> bool {
        match *self {
            Byte::Undefined => match *byte {
                Byte::Undefined | Byte::AnyValue => true,
                _ => false
            },
            Byte::AnyValue => match *byte {
                Byte::AnyValue => true,
                _ => false
            },
            Byte::Int(ref set1) => match *byte {
                Byte::Undefined => false,
                Byte::AnyValue => true,
                Byte::Int(ref set2) => set1.is_subset(&set2)
            }
        }
    }
}

impl fmt::Display for Byte {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match *self {
            Byte::Undefined => String::from("??"),
            Byte::AnyValue => String::from("**"),
            Byte::Int(ref set) =>
                if set.len() == 1 {
                    format!("{:02x}", set.iter().collect::<Vec<&u8>>()[0])
                } else {
                    String::from("{}")
                },
        })
    }
}

#[derive(Copy, Clone, PartialEq)]
struct Flags {
    carry : Flag,
    parity : Flag,
    adjust : Flag,
    zero : Flag,
    sign : Flag,
    int : Flag,
    dir : Flag,
    overflow : Flag
}

impl Flags {
    fn new() -> Flags {
        Flags {
            carry: Flag::Undefined,
            parity: Flag::Undefined,
            adjust: Flag::Undefined,
            zero: Flag::Undefined,
            sign: Flag::Undefined,
            int: Flag::Undefined,
            dir: Flag::Undefined,
            overflow: Flag::Undefined,
        }
    }

    fn union(self, flags: Flags) -> Flags {
        Flags {
            carry: self.carry.union(flags.carry),
            parity: self.parity.union(flags.parity),
            adjust: self.adjust.union(flags.adjust),
            zero: self.zero.union(flags.zero),
            sign: self.sign.union(flags.sign),
            int: self.int.union(flags.int),
            dir: self.dir.union(flags.dir),
            overflow: self.overflow.union(flags.overflow),
        }
    }

    fn is_subset(&self, flags: Flags) -> bool {
        self.carry.is_subset(flags.carry) &&
        self.parity.is_subset(flags.parity) &&
        self.adjust.is_subset(flags.adjust) &&
        self.zero.is_subset(flags.zero) &&
        self.sign.is_subset(flags.sign) &&
        self.int.is_subset(flags.int) &&
        self.dir.is_subset(flags.dir) &&
        self.overflow.is_subset(flags.overflow)
    }
}

#[derive(Copy, Clone, PartialEq)]
enum Flag {
    Undefined,
    True,
    False,
    TrueAndFalse
}

impl Flag {
    fn union(self, flag: Flag) -> Flag {
        match self {
            Flag::Undefined => flag,
            Flag::True => match flag {
                Flag::Undefined => Flag::True,
                Flag::False => Flag::TrueAndFalse,
                _ => flag
            },
            Flag::False => match flag {
                Flag::Undefined => Flag::False,
                Flag::True => Flag::TrueAndFalse,
                _ => flag
            },
            Flag::TrueAndFalse => Flag::TrueAndFalse
        }
    }

    fn is_subset(&self, flag: Flag) -> bool {
        match *self {
            Flag::Undefined => match flag {
                Flag::Undefined | Flag::TrueAndFalse => true,
                _ => false
            },
            Flag::TrueAndFalse => flag == Flag::TrueAndFalse,
            _ => *self == flag
        }
    }
}

// We assume the PSP is loaded at 0x75a and the program at 0x76a.
// This is what DOSBOX uses as a default.
fn get_initial_state(buffer: &Vec<u8>) -> State {
    if &buffer[0..2] == [0x4d, 0x5a] {
        State {
            cs: get_word(buffer, 0x16).wrapping_add(0x76a),
            ip: get_word(buffer, 0x14),
            regs: Registers {
                ds: Word::new(0x75a),
                es: Word::new(0x75a),
                ss: Word::new(get_word(buffer, 0xe).wrapping_add(0x76a)),
                sp: Word::new(get_word(buffer, 0x10)),
                .. Registers::new()
            },
            .. State::new()
        }
    } else {
        State {
            cs: 0x75a,
            ip: 0x100,
            regs: Registers {
                ds: Word::new(0x75a),
                es: Word::new(0x75a),
                ss: Word::new(0x75a),
                sp: Word::new(0xfffe),
                .. Registers::new()
            },
            .. State::new()
        }
    }
}

pub fn emulate_next(state: State, instructions: &HashMap<usize, Instruction>) -> State {
    let offset = (16*state.cs + state.ip) as usize;
    match instructions.get(&offset) {
        Some(inst) => {
            let new_state = match inst.mnemonic {
                Mnemonic::CALL => emulate_call(state, inst),
                Mnemonic::MOV => emulate_mov(state, inst),
                Mnemonic::INT => emulate_int(state, inst),
                Mnemonic::POP => emulate_pop(state, inst),
                Mnemonic::PUSH => emulate_push(state, inst),
                _ => panic!("unimplemented instruction at {:x}", offset)
            };
            State {
                ip: new_state.ip + inst.length as u16,
                .. new_state
            }
        },
        None => panic!("no instruction at {:x}", offset)
    }
}

fn emulate_call(state: State, inst: &Instruction) -> State {
    state
}

fn emulate_int(state: State, inst: &Instruction) -> State {
    state
}

fn emulate_mov(state: State, inst: &Instruction) -> State {
    let (op1, op2) = (inst.unpack_op1(), inst.unpack_op2());
    match op1 {
        Operand::Register8(_) | Operand::Imm8(_) => {
            let byte = get_byte_op(&state, op2);
            set_byte_op(state, op1, byte)
        }
        _ => {
            let word = get_word_op(&state, op2);
            set_word_op(state, op1, word)
        }
    }
}


fn emulate_pop(state: State, inst: &Instruction) -> State {
    match inst.op1 {
        Some(Operand::Register16(target_reg)) => match state.regs.sp {
            Word::Undefined => state.set_reg16(target_reg, Word::Undefined),
            _ => panic!("stack not yet implemented")
        },
        _ => panic!("Incorrect operand for PUSH.")
    }
}

fn emulate_push(state: State, inst: &Instruction) -> State {
    if state.regs.sp == Word::Undefined {
        return state;
    }
    panic!("stack not yet implemented.")
    /*match inst.op1 {
        Some(Operand::Register8(source_reg) => {
        }*/
}

fn get_word_op(state: &State, operand: Operand) -> Word {
    match operand {
        Operand::Register8(_) | Operand::Imm8(_) =>
            panic!("can't get word from byte source"),
        Operand::Register16(reg) => state.get_reg16(reg),
        Operand::Imm16(imm) => Word::new(imm as u16),
        _ => match state.get_reg16(Register::DS) {
            Word::Undefined => Word::Undefined,
            Word::AnyValue => Word::AnyValue,
            Word::Int(_) => panic!("Getting mem value not yet implemented."),
            _ => panic!("Invalid value for DS.")
        }
    }
}

pub fn read_word_op(state: &State, operand: Operand) -> Word {
    let word = get_word_op(state, operand);
    if let Word::Bytes(bytel, byteh) = word {
        bytel.combine(byteh)
    } else {
        word
    }
}

fn get_byte_op(state: &State, operand: Operand) -> Byte {
    match operand {
        Operand::Register16(_) | Operand::Imm16(_) =>
            panic!("can't get word from byte source"),
        Operand::Register8(reg) => state.get_reg8(reg),
        Operand::Imm8(imm) => Byte::new(imm as u8),
        _ => match state.get_reg16(Register::DS) {
            Word::Undefined => Byte::Undefined,
            Word::AnyValue => Byte::AnyValue,
            Word::Int(_) => panic!("Getting mem value not yet implemented."),
            _ => panic!("Invalid value for DS.")
        }
    }
}

pub fn read_byte_op(state: &State, operand: Operand) -> Byte {
    get_byte_op(state, operand)
}

fn set_word_op(state: State, operand: Operand, word: Word) -> State {
    match operand {
        Operand::Register16(target_reg) =>
            state.set_reg16(target_reg, word),
        Operand::Ptr16(offset) => match state.get_reg16(Register::DS) {
            Word::Undefined =>
                panic!("trying to write to memory with undefined DS."),
            Word::AnyValue =>
                panic!("trying to write to memory with unlimited DS."),
            Word::Int(set) => {
                let mut new_memory = state.memory.clone();
                for ds_value in set.iter() {
                    let location = 16*(*ds_value as usize) + offset as usize;
                    new_memory.insert(location, word.split_low());
                    new_memory.insert(location + 1, word.split_high());
                }
                State {
                    memory: new_memory,
                    .. state
                }
            },
            _ => panic!("invalid DS value.")
        },
        _ => panic!("Unimplemented target for set_word_op")
    }
}

fn set_byte_op(state: State, operand: Operand, byte: Byte) -> State {
    match operand {
        Operand::Register8(target_reg) =>
            state.set_reg8(target_reg, byte),
        Operand::Ptr16(offset) => match state.get_reg16(Register::DS) {
            Word::Undefined =>
                panic!("trying to write to memory with undefined DS."),
            Word::AnyValue =>
                panic!("trying to write to memory with unlimited DS."),
            Word::Int(set) => {
                let mut new_memory = state.memory.clone();
                for ds_value in set.iter() {
                    let location = 16*(*ds_value as usize) + offset as usize;
                    new_memory.insert(location, byte.clone());
                }
                State {
                    memory: new_memory,
                    .. state
                }
            },
            _ => panic!("invalid DS value.")
        },
        _ => panic!("Unimplemented target for set_byte_op")
    }
}
