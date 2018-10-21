use defs::*;
use chip8::arch::*;
use chip8::state::{State};
use std::collections::HashSet;

pub struct Interpreter {
}

impl<'a> SimulatorTrait<State<'a>, Instruction> for Interpreter {
    fn next_inst_offset(state: &State<'a>) -> usize {
        (state.pc - 0x200) as usize
    }

    fn simulate_system_call(&self, _state: State<'a>, _inst: Instruction) -> Option<State<'a>> {
        panic!("Can't simluate system calls in interpreter");
    }

    fn simulate_next_instruction(&self, mut state: State<'a>, instruction: Instruction) -> SimResult<State<'a>> {
        state.pc += 2;
        match instruction.mnemonic {
            Mnemonic::EXIT => SimResult::End,
            Mnemonic::CLS | Mnemonic::SCD | Mnemonic::SCR | Mnemonic::SCL
            | Mnemonic::LOW | Mnemonic::HIGH =>
                SimResult::State(state),
            Mnemonic::JP | Mnemonic::CALL | Mnemonic::SE | Mnemonic::SNE
            | Mnemonic::SKP | Mnemonic::SKNP | Mnemonic::RET =>
                SimResult::Branch(self.branch(state, instruction)),
            Mnemonic::LD => simulate_ld(state, instruction),
            Mnemonic::ADD | Mnemonic::SUB | Mnemonic::OR | Mnemonic::AND
            | Mnemonic::XOR | Mnemonic::SUBN | Mnemonic::SHL | Mnemonic::SHR =>
                SimResult::State(simulate_binary_operator(state, instruction)),
            Mnemonic::RND => simulate_rnd(state, instruction),
            Mnemonic::DRW => SimResult::State(state.
                set_byte(Operand::V(0xF), Byte::from_vec(vec!(0, 1)))),
            Mnemonic::LDBCD => simulate_ldbcd(state, instruction),
            Mnemonic::LDPTR => simulate_ldptr(state, instruction)
        }
    }

}

impl<'a> Interpreter {
    fn branch(&self, mut state: State<'a>, instruction: Instruction) -> (Vec<State<'a>>, Vec<usize>) {
        let mut new_states = Vec::new();
        let mut new_labels = Vec::new();

        match instruction.mnemonic {
            Mnemonic::CALL => {
                state.stack[state.sp] = state.pc;
                state.sp += 1;
                state.pc = match instruction.unpack_op1() {
                    Operand::Address(word) => word,
                    _ => panic!("CALL operand should be immediate address.")
                };
                new_labels.push(Interpreter::next_inst_offset(&state));
                new_states.push(state);
            },
            Mnemonic::RET => {
                state.sp -= 1;
                state.pc = state.stack[state.sp];
                state.stack[state.sp] = 0;
                new_states.push(state);
            },
            Mnemonic::JP => {
                match instruction.unpack_op1() {
                    Operand::Address(word) => {
                        state.pc = word;
                        new_labels.push(Interpreter::next_inst_offset(&state));
                        new_states.push(state);
                    },
                    Operand::V(0) => match instruction.unpack_op2() {
                        Operand::Address(base) => {
                            match state.V[0] {
                                Byte::Undefined => panic!(
                                    "Can't jump to undefined offset"),
                                Byte::AnyValue => panic!(
                                    "Can't jump to every address"),
                                Byte::Int(ref set) => {
                                    for offset in set {
                                        let mut new_state = state.clone();
                                        new_state.pc = base + *offset as u16;
                                        new_labels.push(Interpreter::
                                            next_inst_offset(&new_state));
                                        new_states.push(new_state);
                                    }
                                }
                            }
                        },
                        _ => panic!("base of indirect jump should be imm16")
                    },
                    _ => panic!("Invalid JP operand.")
                }
            },
            Mnemonic::SKP | Mnemonic::SKNP => {
                let mut new_state = state.clone();
                new_state.pc += 2;
                new_states.push(new_state);
                new_states.push(state);
            },
            Mnemonic::SE | Mnemonic::SNE => {
                let op1 = instruction.unpack_op1();
                let op2 = instruction.unpack_op2();

                let op2byte = &state.get_byte(op2);
                let intersect = state.get_byte(op1).intersect(op2byte);
                let difference = state.get_byte(op1).difference(op2byte);

                match instruction.mnemonic {
                    Mnemonic::SE => {
                        if intersect.len() > 0 {
                            let mut new_state = state.clone();
                            new_state.pc += 2;
                            new_states.push(new_state.set_byte(op1, intersect.clone()));
                        }
                        if difference.len() > 0 {
                            new_states.push(state.set_byte(op1, difference));
                        }
                    },
                    Mnemonic::SNE => {
                        if intersect.len() > 0 {
                            let new_state = state.clone();
                            new_states.push(new_state.set_byte(op1, intersect.clone()));
                        }
                        if difference.len() > 0 {
                            state.pc += 2;
                            new_states.push(state.set_byte(op1, difference));
                        }
                    },
                    _ => panic!("shouldn't be here")
                }
            },
            _ => panic!("unimplemented jump instruction")
        }

        (new_states, new_labels)
    }
}

fn simulate_binary_operator<'a>(state: State<'a>, inst: Instruction) -> State<'a> {
    let (op1, op2) = (inst.unpack_op1(), inst.unpack_op2());
    let (result, vf) = apply_binary_op(state.get_value(op1), state.get_value(op2),
        inst.mnemonic);
    let new_state = state.set_value(op1, result);
    match vf {
        None => new_state,
        Some(byte) => match op2 {
            Operand::Byte(_) => new_state,
            _ => match op1 {
                Operand::I => new_state,
                _ => new_state.set_byte(Operand::V(0xF), byte) 
            }
        }
    }
}

fn simulate_ld<'a>(state: State<'a>, inst: Instruction) -> SimResult<State<'a>> {
    let (op1, op2) = (inst.unpack_op1(), inst.unpack_op2());
    let value = state.get_value(op2);
    SimResult::State(state.set_value(op1, value))
}

fn simulate_rnd<'a>(state: State<'a>, inst: Instruction) -> SimResult<State<'a>> {
    match inst.op2 {
        Some(Operand::Byte(byte)) => {
            let mut set = HashSet::new();
            for i in 0..256u16 {
                set.insert(i as u8 & byte);
            }
            SimResult::State(state.set_byte(inst.unpack_op1(), Byte::Int(set)))
        },
        _ => panic!("RND second operand should be an immediate byte.")
    }
}

fn simulate_ldbcd<'a>(mut state: State<'a>, _inst: Instruction) -> SimResult<State<'a>> {
    match state.I {
        Word::Undefined => panic!("Can't write to undefined memory location."),
        Word::AnyValue => panic!("Can't write to all memory locations."),
        Word::Int(ref set) => {
            for address in set.iter() {
                state.memory.write_string(*address as usize,
                    &vec!(Byte::AnyValue, Byte::AnyValue, Byte::AnyValue));
            }
        },
        Word::Bytes(_, _) => panic!("Index register shouldn't be split.")
    }
    SimResult::State(state)
}

fn simulate_ldptr<'a>(mut state: State<'a>, inst: Instruction) -> SimResult<State<'a>> {
    match inst.unpack_op1() {
        Operand::Pointer => {
            match inst.unpack_op2() {
                Operand::V(x) => {
                    let mut string = Vec::new();
                    for i in 0..(x+1) {
                        string.push(state.V[i].clone());
                    }
                    match state.I {
                        Word::Undefined => panic!("Can't write to undefined memory location."),
                        Word::AnyValue => panic!("Can't write to all memory locations."),
                        Word::Int(ref set) => {
                            for address in set.iter() {
                                state.memory.write_string(*address as usize, &string);
                            }
                        },
                        Word::Bytes(_, _) => panic!("Index register shouldn't be split.")
                    }
                },
                _ => panic!("Only registers can be written to memory.")
            }
        },
        Operand::V(x) => {
            match state.I {
                Word::Undefined => panic!("Can't read from undefined memory location."),
                Word::AnyValue => panic!("Can't read from every memory location."),
                Word::Int(ref set) => {
                    for i in 0..(x+1) {
                        let mut values = Byte::from_vec(Vec::new());
                        for address in set.iter() {
                            let memory_byte = state.memory.get_byte(*address as usize  + i);
                            match memory_byte {
                                Some(byte) => values = values.union(byte),
                                None => return SimResult::Error(state.clone(), String::from(format!("Tried to read from uninitialized memory location {:x}", *address)))
                            }
                        }
                        state.V[i] = values;
                    }
                },
                Word::Bytes(_, _) => panic!("Index register shouldn't be split.")
            }
        },
        _ => panic!("LDPTR not implemented for operands.")
    }

    SimResult::State(state)
}

fn apply_binary_op(op1: Value, op2: Value, op: Mnemonic) -> (Value, Option<Byte>) {
    match (op1, op2) {
        (Value::Byte(byte1), Value::Byte(byte2)) => {
            let (byte, vf) = apply_to_bytes(byte1, byte2, op);
            (Value::Byte(byte), vf)
        },
        (Value::Word(word1), Value::Word(word2)) => {
            let (word, vf) = apply_to_words(word1, word2, op);
            (Value::Word(word), vf)
        },
        (Value::Word(word1), Value::Byte(byte2)) => {
            let (word, vf) = apply_to_words(word1, byte2.to_word(), op);
            (Value::Word(word), vf)
        },
        _ => panic!("Can't apply word source to byte target.")
    }
}

fn apply_to_words(op1: Word, op2: Word, op: Mnemonic) -> (Word, Option<Byte>) {
    match op1 {
        Word::Undefined => (Word::Undefined, Some(Byte::Undefined)),
        Word::AnyValue => match op2 {
            Word::Undefined => (Word::Undefined, Some(Byte::Undefined)),
            _ => (Word::AnyValue, Some(Byte::AnyValue))
        },
        Word::Int(set1) => match op2 {
            Word::Undefined => (Word::Undefined, Some(Byte::Undefined)),
            Word::AnyValue => (Word::AnyValue, Some(Byte::AnyValue)),
            Word::Int(set2) => {
                let mut set = HashSet::new();
                let mut vf = Bit::Undefined;
                for word1 in set1 {
                    for word2 in set2.clone() {
                        let (result, new_vf) =
                            apply_to_u16s(word1, word2, op);
                        set.insert(result);
                        vf = vf.union(new_vf);
                    }
                }
                (Word::Int(set), match vf {
                    Bit::Undefined => None,
                    Bit::True => Some(Byte::new(1)),
                    Bit::False => Some(Byte::new(0)),
                    Bit::TrueAndFalse => Some(Byte::new(0).union(Byte::new(1)))
                })
            },
            Word::Bytes(_, _) => 
                panic!("applying binary operator to split word unimplemented.")
        },
        Word::Bytes(_, _) =>
            panic!("applying binary operator to split word unimplemented.")
    }
}

fn apply_to_bytes(op1: Byte, op2: Byte, op: Mnemonic) -> (Byte, Option<Byte>) {
    match op1 {
        Byte::Undefined => (Byte::Undefined, Some(Byte::Undefined)),
        Byte::AnyValue => match op2 {
            Byte::Undefined => (Byte::Undefined, Some(Byte::Undefined)),
            _ => (Byte::AnyValue, Some(Byte::AnyValue))
        },
        Byte::Int(set1) => match op2 {
            Byte::Undefined => (Byte::Undefined, Some(Byte::Undefined)),
            Byte::AnyValue => (Byte::AnyValue, Some(Byte::AnyValue)),
            Byte::Int(set2) => {
                let mut set = HashSet::new();
                let mut vf = Bit::Undefined;
                for byte1 in set1 {
                    for byte2 in set2.clone() {
                        let (result, new_vf) =
                            apply_to_u8s(byte1, byte2, op);
                        set.insert(result);
                        vf = vf.union(new_vf);
                    }
                }
                (Byte::Int(set), match vf {
                    Bit::Undefined => None,
                    Bit::True => Some(Byte::new(1)),
                    Bit::False => Some(Byte::new(0)),
                    Bit::TrueAndFalse => Some(Byte::new(0).union(Byte::new(1)))
                })
            }
        }
    }
}

fn apply_to_u16s(word1: u16, word2: u16, op: Mnemonic) -> (u16, Bit) {
    match op {
        Mnemonic::ADD => (word1.wrapping_add(word2), Bit::Undefined),
        _ => panic!("unknown binary op for u16s")
    }
}

fn apply_to_u8s(byte1: u8, byte2: u8, op: Mnemonic) -> (u8, Bit) {
    match op {
        Mnemonic::ADD =>
            (byte1.wrapping_add(byte2), if 0xff - byte1 < byte2 {
                Bit::True
            } else {
                Bit::False
            }),
        Mnemonic::SUB =>
            (byte1.wrapping_sub(byte2), if byte1 > byte2 {
                Bit::False
            } else {
                Bit::True
            }),
        Mnemonic::SUBN =>
            (byte2.wrapping_sub(byte1) & 0x00ff, if byte2 > byte1 {
                Bit::False
            } else {
                Bit::True
            }),
        Mnemonic::SHL =>
            (byte2 << 1, match byte2 & 0x80 {
                0x80 => Bit::True,
                _ => Bit::False
            }),
        Mnemonic::SHR =>
            (byte2 >> 1, match byte2 & 0x01 {
                0x01 => Bit::True,
                _ => Bit::False
            }),
        Mnemonic::OR => (byte1 | byte2, Bit::Undefined),
        Mnemonic::AND => (byte1 & byte2, Bit::Undefined),
        Mnemonic::XOR => (byte1 ^ byte2, Bit::Undefined),
        _ => panic!("unknown binary op for u8s")
    }
}
