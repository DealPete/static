use defs::*;
use chip8::arch::*;
use std::fmt;

#[derive(Clone)]
pub struct State<'a> {
    pub pc: u16,
    pub sp: usize,
    pub I: Word,
    pub V: Vec<Byte>,
    pub delay_timer: Byte,
    pub sound_timer: Byte,
    pub stack: Vec<u16>,
    pub memory: Memory<'a>
}

impl<'a> State<'a> {
    pub fn new(initial_memory: &'a [u8]) -> State<'a> {
        State {
            pc: 0x200,
            sp: 0,
            I: Word::Undefined,
            V: {
                let mut new_V = Vec::new();
                for i in 0..16 {
                    new_V.push(Byte::new(0));
                }
                new_V
            },
            delay_timer: Byte::new(0),
            sound_timer: Byte::new(0),
            stack: vec![0; 16],
            memory: Memory::new(initial_memory, 0x200, Endian::Big),
        }
    }

    pub fn get_value(&self, operand: Operand) -> Value {
        match operand {
            Operand::I | Operand::Address(_) =>
                Value::Word(self.get_word(operand)),
            _ => Value::Byte(self.get_byte(operand))
        }
    }

    pub fn get_word(&self, operand: Operand) -> Word {
        match operand {
            Operand::I => self.I.clone(),
            Operand::Address(word) => Word::new(word),
            _ => panic!("unimplemented word type.")
        }
    }

    pub fn get_byte(&self, operand: Operand) -> Byte {
        match operand {
            Operand::V(x) => self.V[x].clone(),
            Operand::Byte(byte) => Byte::new(byte),
            Operand::KeyPress => Byte::from_vec(vec![0x0, 0x1, 0x2, 0x3, 0x4,
                0x5, 0x6, 0x7, 0x8, 0x9, 0xa, 0xb, 0xc, 0xd, 0xe, 0xf]),
            Operand::DelayTimer => Byte::AnyValue,
                //self.delay_timer.clone(),
            Operand::SoundTimer => Byte::AnyValue,
                //self.sound_timer.clone(),
            _ => panic!("unimplemented byte type.")
        }
    }

    pub fn set_value(self, operand: Operand, value: Value) -> State<'a> {
        match value {
            Value::Word(word) => self.set_word(operand, word),
            Value::Byte(byte) => self.set_byte(operand, byte)
        }
    }

    pub fn set_word(self, operand: Operand, word: Word) -> State<'a> {
        match operand {
            Operand::I => State { I: word, .. self },
            _ => panic!("unimplemented target for set_word.")
        }
    }

    pub fn set_byte(self, operand: Operand, byte: Byte) -> State<'a> {
        match operand {
            Operand::V(x) => {
                let mut new_V = self.V.clone();
                new_V[x] = byte;
                State { V: new_V, .. self }
            },
            Operand::DelayTimer => State { delay_timer: byte, .. self },
            Operand::SoundTimer => State { sound_timer: byte, .. self },
            _ => panic!("unimplemented target for set_byte.")
        }
    }
}

impl<'a> StateTrait<State<'a>> for State<'a> {
    fn union(self, state: State<'a>) -> State<'a> {
        if self.pc != state.pc {
            panic!("Can't unify states with different program counters.");
        }
        if self.sp != state.sp {
            panic!("Can't unify states with different stack pointers.");
        }
        State {
            pc: self.pc,
            sp: self.sp,
            I: self.I.union(state.I),
            V: {
                let mut new_V = Vec::new();
                for i in 0..16 {
                    new_V.push(self.V[i].clone().union(state.V[i].clone()))
                }
                new_V
            },
            delay_timer: self.delay_timer.union(state.delay_timer),
            sound_timer: self.sound_timer.union(state.sound_timer),
            stack: self.stack,
            memory: self.memory.union(state.memory)
        }
    }

    fn is_subset(&self, state: &State<'a>) -> bool {
        for i in 0..16 {
            if !self.V[i].is_subset(&state.V[i]) {
                return false;
            }
        }
        for i in 0..16 {
            if self.stack[i] != state.stack[i] {
                return false;
            }
        }
        let ret_val = self.pc == state.pc &&
        self.sp == state.sp &&
        self.I.is_subset(&state.I) &&
        self.delay_timer.is_subset(&state.delay_timer) &&
        self.sound_timer.is_subset(&state.sound_timer);
        ret_val
    }
}

impl<'a> fmt::Display for State<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let line1 = format!("PC={:04x}, SP: {:04x} I={} DT={} ST={}\n",
            self.pc, self.sp, self.I, self.delay_timer, self.sound_timer);
        let line2 = format!("V0={} V1={} V2={} V3={} V4={} V5={} V6={} V7={}\n",
            self.V[0], self.V[1], self.V[2], self.V[3],
            self.V[4], self.V[5], self.V[6], self.V[7]);
        let line3 = format!("V8={} V9={} VA={} VB={} VC={} VD={} VE={} VF={}\n",
            self.V[8], self.V[9], self.V[10], self.V[11],
            self.V[12], self.V[13], self.V[14], self.V[15]);
        let line4 = {
            let mut line = String::from("Stack: ");
            for i in 0..16 {
                line.push_str(format!("{:04x} ", self.stack[i]).as_str());
            }
            line
        };
        let line5 = {
            let mut line = String::new();
            for (address, value) in self.memory.get_deltas() {
                line.push_str(format!("[{:x}] = {}\t", address + 0x200, value).as_str());
            }
            line
        };
        write!(f, "{}{}{}{}{}", line1, line2, line3, line4, line5)
    }
}
