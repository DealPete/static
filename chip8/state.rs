use defs::*;
use chip8::arch::*;
use std::collections::HashMap;
use std::fmt;

#[derive(Clone)]
pub struct State<'a> {
    pub pc: u16,
    pub sp: usize,
    pub I: Word,
    pub V: Vec<Word>,
    pub DelayTimer: Word,
    pub SoundTimer: Word,
    pub stack: Vec<u16>,
    pub memory: HashMap<u16, Word>,
    pub initial_memory: &'a [u8]
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
                    new_V.push(Word::new(0));
                }
                new_V
            },
            DelayTimer: Word::new(0),
            SoundTimer: Word::new(0),
            stack: vec![0; 16],
            memory: HashMap::new(),
            initial_memory: initial_memory
        }
    }

    pub fn get_word(&self, operand: Operand) -> Word {
        match operand {
            Operand::I => self.I.clone(),
            Operand::V(x) => self.V[x].clone(),
            Operand::Address(word) => Word::new(word),
            Operand::Byte(byte) => Word::new(byte as u16),
            _ => panic!("unimplemented word type.")
        }
    }

    pub fn set_word(self, operand: Operand, word: Word) -> State<'a> {
        match operand {
            Operand::I => State { I: word, .. self },
            Operand::V(x) => {
                let mut new_V = self.V.clone();
                new_V[x] = word;
                State { V: new_V, .. self }
            },
            _ => panic!("unimplemented target for set_word.")
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
                println!("self.V1 = {}, state.V1 = {}, new_V = {}", self.V[1], state.V[1], new_V[1]);
                new_V
            },
            DelayTimer: self.DelayTimer.union(state.DelayTimer),
            SoundTimer: self.SoundTimer.union(state.SoundTimer),
            stack: self.stack,
            initial_memory: self.initial_memory,
            memory: {
                let mut new_memory = HashMap::new();
                for (offset, value) in self.memory.iter() {
                    new_memory.insert(*offset, value.clone());
                }
                for (offset, value) in state.memory.iter() {
                    match self.memory.get(offset) {
                        None => new_memory.insert(*offset, value.clone()),
                        Some(memory) =>
                            new_memory.insert(
                                *offset, memory.clone().union(value.clone())
                            )
                    };
                }
                new_memory
            }
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
        self.pc == state.pc &&
        self.sp == state.sp &&
        self.I.is_subset(&state.I) &&
        self.DelayTimer.is_subset(&state.DelayTimer) &&
        self.SoundTimer.is_subset(&state.SoundTimer)
    }
}

impl<'a> fmt::Display for State<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let line1 = format!("PC={:04x}, SP: {:04x} I={} DT={} ST={}\n",
            self.pc, self.sp, self.I, self.DelayTimer, self.SoundTimer);
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
        write!(f, "{}{}{}{}", line1, line2, line3, line4)
    }
}
