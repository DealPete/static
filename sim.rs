use state::*;
use defs::*;
use std::collections::HashSet;

pub fn simulate_instruction<'program, C: Context<'program>>(mut state: State<'program>, context: &C, inst: &Instruction) -> State<'program> {
    state.ip = state.ip.wrapping_add(inst.length as u16);
    match inst.mnemonic {
        Mnemonic::ADD | Mnemonic::SUB | Mnemonic::AND
        | Mnemonic::OR | Mnemonic::SHL
            => simulate_binary_operator(state, inst),
        Mnemonic::XOR => simulate_xor(state, inst),
        Mnemonic::CMP => simulate_cmp(state, inst),
        Mnemonic::IN => simulate_in(state, inst),
        Mnemonic::OUT => state,
        Mnemonic::INT => context.simulate_int(state, inst),
        Mnemonic::MOV => simulate_mov(state, inst),
        Mnemonic::POP => simulate_pop(state, inst),
        Mnemonic::PUSH => simulate_push(state, inst),
        Mnemonic::RET => simulate_ret(state),
        _ => panic!("Instruction:\n{}\nunimplemented in simulator.", inst)
    }
}

fn simulate_binary_operator<'program>(state: State<'program>, inst: &Instruction) -> State<'program> {
    let (op1, op2) = (inst.unpack_op1(), inst.unpack_op2());
    let (result, flags) =
        apply(state.get_value(op1), state.get_value(op2), inst.mnemonic);
    state.set_value(op1, result).set_flags(flags)
}

fn simulate_xor<'program>(state: State<'program>, inst: &Instruction) -> State<'program> {
    if inst.op1 == inst.op2 {
        let mut new_flags = state.get_flags();
        new_flags.zero = Flag::True;
        state.clear_value(inst.unpack_op1()).set_flags(new_flags)
    } else {
        simulate_binary_operator(state, inst)
    }
}

fn simulate_cmp<'program>(state: State<'program>, inst: &Instruction) -> State<'program> {
    let (op1, op2) = (inst.unpack_op1(), inst.unpack_op2());
    let (_, flags) =
        apply(state.get_value(op1), state.get_value(op2), Mnemonic::SUB);
    state.set_flags(flags)
}

fn simulate_in<'program>(state: State<'program>, inst: &Instruction) -> State<'program> {
    match inst.unpack_op1() {
        Operand::Register8(Register::AL) =>
            state.set_reg8(Register::AL, Byte::AnyValue),
        Operand::Register16(Register::AX) =>
            state.set_reg16(Register::AX, Word::AnyValue),
        _ => panic!("Wrong destination for IN instruction: {}", inst)
    }
}

fn simulate_mov<'program>(state: State<'program>, inst: &Instruction) -> State<'program> {
    let (op1, op2) = (inst.unpack_op1(), inst.unpack_op2());
    let value = state.get_value(op2);
    state.set_value(op1, value)
}

fn simulate_pop<'program>(state: State<'program>, inst: &Instruction) -> State<'program> {
    let (new_state, word) = pop_word(state);
    new_state.set_word(inst.unpack_op1(), word)
}

fn simulate_push<'program>(state: State<'program>, inst: &Instruction) -> State<'program> {
    let word = state.get_word(inst.unpack_op1());
    push_word(state, word)
}

fn simulate_ret(state: State) -> State {
    let (mut new_state, word) = pop_word(state);
    new_state.ip = match word {
        Word::Int(ref set) => {
            if set.len() == 1 {
                *set.iter().collect::<Vec<&u16>>()[0]
            } else {
                panic!("Multiple return points unimplemented.")
            }
        },
        _ => panic!("Unsupported return type.")
    };
    new_state
}


fn pop_word(state: State) -> (State, Word) {
    let pointer = Operand::SegPtr(Register::SS, Pointer::Reg(Register::SP));
    let word = state.get_word(pointer);
    let (new_sp, _) =
        apply_to_words(state.get_reg16(Register::SP), Word::new(2), Mnemonic::ADD);
    (state.set_reg16(Register::SP, new_sp), word)
}

pub fn push_word(state: State, word: Word) -> State {
    let (new_sp, _) =
        apply_to_words(state.get_reg16(Register::SP), Word::new(2), Mnemonic::SUB);
    let pointer = Operand::SegPtr(Register::SS, Pointer::Reg(Register::SP));
    state.set_reg16(Register::SP, new_sp).set_word(pointer, word)
}

fn apply(op1: Value, op2: Value, op: Mnemonic) -> (Value, Flags) {
    match (op1, op2) {
        (Value::Byte(byte1), Value::Byte(byte2)) => {
            let (byte, flags) = apply_to_bytes(byte1, byte2, op);
            (Value::Byte(byte), flags)
        },
        (Value::Word(word1), Value::Word(word2)) => {
            let (word, flags) = apply_to_words(word1, word2, op);
            (Value::Word(word), flags)
        },
        (Value::Word(word1), Value::Byte(byte2)) => {
            let (word, flags) = apply_to_words(word1, byte2.to_word(), op);
            (Value::Word(word), flags)
        },
        _ => panic!("Can't apply word source to byte target.")
    }
}

pub fn apply_to_words(wordl: Word, wordr: Word, op: Mnemonic) -> (Word, Flags) {
    let word1 = if let Word::Bytes(bytel, byteh) = wordl {
        bytel.combine(byteh)
    } else {
        wordl
    };
    let word2 = if let Word::Bytes(bytel, byteh) = wordr {
        bytel.combine(byteh)
    } else {
        wordr
    };
    let mut flags = Flags::new();
    match word1 {
        Word::Undefined => (Word::Undefined, flags),
        Word::AnyValue =>
            if word2 == Word::Undefined {
                (Word::Undefined, flags)
            } else {
                (Word::AnyValue, Flags {
                    sign: Flag::TrueAndFalse,
                    zero: Flag::TrueAndFalse,
                    overflow: Flag::TrueAndFalse,
                    .. flags
                })
            },
        Word::Int(set1) => match word2 {
            Word::Undefined => (Word::Undefined, flags),
            Word::AnyValue => (Word::AnyValue, Flags {
                sign: Flag::TrueAndFalse,
                zero: Flag::TrueAndFalse,
                overflow: Flag::TrueAndFalse,
                .. flags
            }),
            Word::Int(set2) => {
                let mut set = HashSet::new();
                for word1 in set1 {
                    for word2 in set2.clone() {
                        let (result, new_flags) =
                            apply_to_u16s(word1, word2, op);
                        set.insert(result);
                        flags = flags.union(new_flags);
                        if result == 0 {
                            flags.zero.add_true();
                        } else {
                            flags.zero.add_false();
                        }
                    }
                };
                (Word::Int(set), flags)
            },
            _ => panic!("shouldn't be here")
        },
        _ => panic!("shouldn't be here")
    }
}

fn apply_to_bytes(op1: Byte, op2: Byte, op: Mnemonic) -> (Byte, Flags) {
    let mut flags = Flags::new();
    match op1 {
        Byte::Undefined => (Byte::Undefined, flags),
        Byte::AnyValue =>
            if op2 == Byte::Undefined {
                (Byte::Undefined, flags)
            } else {
                (Byte::AnyValue, Flags {
                    sign: Flag::TrueAndFalse,
                    zero: Flag::TrueAndFalse,
                    overflow: Flag::TrueAndFalse,
                    .. flags
                })
            },
        Byte::Int(set1) => match op2 {
            Byte::Undefined => (Byte::Undefined, flags),
            Byte::AnyValue => (Byte::AnyValue, Flags {
                sign: Flag::TrueAndFalse,
                zero: Flag::TrueAndFalse,
                overflow: Flag::TrueAndFalse,
                .. flags
            }),
            Byte::Int(set2) => {
                let mut set = HashSet::new();
                for byte1 in set1 {
                    for byte2 in set2.clone() {
                        let (result, new_flags) =
                            apply_to_u8s(byte1, byte2, op);
                        flags = flags.union(new_flags);
                        set.insert(result);
                        if result == 0 {
                            flags.zero.add_true();
                        } else {
                            flags.zero.add_false();
                        }
                    }
                };
                (Byte::Int(set), flags)
            }
        }
    }
}

fn apply_to_u16s(word1: u16, word2: u16, op: Mnemonic) -> (u16, Flags) {
    let mut flags = Flags::new();
    match op {
        Mnemonic::ADD => {
            if 0xffff - word1 < word2 {
                flags.overflow.add_true();
                flags.carry.add_true();
            } else {
                flags.overflow.add_false();
                flags.carry.add_false();
            }
            (word1.wrapping_add(word2), flags)
        },
        Mnemonic::SUB => {
            if word1 < word2 {
                flags.overflow.add_true();
                flags.carry.add_true();
            } else {
                flags.overflow.add_false();
                flags.carry.add_false();
            }
            (word1.wrapping_sub(word2), flags)
        },
        Mnemonic::AND => {
            (word1 & word2, flags)
        },
        Mnemonic::OR => {
            (word1 | word2, flags)
        },
        Mnemonic::XOR => {
            (word1 ^ word2, flags)
        },
        Mnemonic::SHL => {
            (word1 << word2, flags)
        },
        _ => panic!("Operation {:?} not implemented for words.", op)
    }
}

fn apply_to_u8s(byte1: u8, byte2: u8, op: Mnemonic) -> (u8, Flags) {
    let mut flags = Flags::new();
    match op {
        Mnemonic::ADD => {
            if 0xff - byte1 < byte2 {
                flags.overflow.add_true();
                flags.carry.add_true();
            } else {
                flags.overflow.add_false();
                flags.carry.add_false();
            }
            (byte1.wrapping_add(byte2), flags)
        },
        Mnemonic::SUB => {
            if byte1 < byte2 {
                flags.overflow.add_true();
                flags.carry.add_true();
            } else {
                flags.overflow.add_false();
                flags.carry.add_false();
            }
            (byte1.wrapping_sub(byte2), flags)
        },
        Mnemonic::AND => {
            (byte1 & byte2, flags)
        },
        Mnemonic::OR => {
            (byte1 | byte2, flags)
        },
        Mnemonic::XOR => {
            (byte1 ^ byte2, flags)
        },
        Mnemonic::SHL => {
            (byte1 << byte2, flags)
        },
        _ => panic!("Operation {:?} not implemented for bytes.", op)
    }
}
