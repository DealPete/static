use defs::*;
use state::*;
use x86::arch::*;
use std::collections::HashSet;

pub enum Result<'a> {
    End,
    State(State<'a>),
    Branch(Vec<State<'a>>)
}

pub fn simulate_next_instruction<'a, C: Context>(mut state: State<'a>, context: &C, instruction: Instruction) -> Result<'a> {
    //println!("{}", state);
    //println!("{}", instruction);
    /*use std::io;
    let mut input = String::new();
    io::stdin().read_line(&mut input).ok();*/
    if instruction.mnemonic.is_branch() {
        Result::Branch(branch(state, instruction))
    } else if instruction.mnemonic == Mnemonic::INT {
        state.ip = state.ip.wrapping_add(instruction.length as u16);
        match context.simulate_int(state, instruction) {
            None => Result::End,
            Some(state) => Result::State(state)
        }
    } else {
        Result::State(simulate_instruction(state, instruction))
    }
}

fn branch<'a>(mut state: State<'a>, instruction: Instruction) -> Vec<State<'a>> {
    let mut new_states = Vec::new();

    if instruction.mnemonic == Mnemonic::RET {
        let (state, word) = pop_word(state);
        match word {
            Word::Undefined => panic!("can't jump to undefined location."),
            Word::AnyValue => panic!("can't jump to unlimited location."),
            Word::Int(ref set) => {
                for offset in set {
                    let mut new_state = state.clone();
                    new_state.ip = *offset;
                    new_states.push(new_state);
                }
            },
            _ => panic!("Unsupported return type.")
        };
        return new_states;
    }

    let offsets: Vec<u16> = if instruction.op1 == None {
        Vec::new()
    } else {
        match state.get_value(instruction.unpack_op1()) {
            Value::Word(Word::Undefined) | Value::Byte(Byte::Undefined) =>
                panic!("can't jump to undefined location."),
            Value::Word(Word::AnyValue) | Value::Byte(Byte::AnyValue) =>
                panic!("can't jump to unlimited location."),
            Value::Word(Word::Int(set)) => {
                let mut vec = Vec::new();
                for word in set {
                    vec.push(word);
                };
                vec
            },
            Value::Byte(Byte::Int(set)) => {
                let mut vec = Vec::new();
                for byte in set {
                    vec.push(byte as i8 as u16);
                };
                vec
            },
            _ => panic!("Unsupport jump offset type.")
        }
    };

    let cont: bool;
    let jump: bool;
    let flag: Option<(Flag, bool)>;

    if instruction.mnemonic == Mnemonic::CALL
        || instruction.mnemonic == Mnemonic::JMP {
            jump = true; cont = false; flag = None;
    } else if instruction.mnemonic == Mnemonic::LOOP {
        let (new_cx, _) =
            apply_to_words(state.get_reg16(Register::CX), Word::new(1), Mnemonic::SUB);
        state = state.set_reg16(Register::CX, new_cx);

        let cx = state.get_reg16(Register::CX);

        let (can, must) = cx.compare(0);
        jump = !must;
        cont = can;
        flag = None;
    } else {
        let (flag_to_check, truth_value) = match instruction.mnemonic {
            Mnemonic::JZ => (Flag::Zero, true),
            Mnemonic::JNZ => (Flag::Zero, false),
            Mnemonic::LOOP => (Flag::Zero, true),
            _ => panic!("jump opeand not yet implemented.")
        };

        let flag_bit = state.get_flag(flag_to_check);

        if flag_bit == Bit::Undefined {
            panic!("flag bit for conditional jump is undefined!");
        };

        jump = flag_bit.has_truth_value(true);
        cont = flag_bit.has_truth_value(false);
        flag = Some((flag_to_check, truth_value));
    }

    if jump {
        for offset in offsets {
            let mut new_state = state.clone();
            new_state.ip = new_state.ip.wrapping_add(instruction.length as u16);
            if instruction.mnemonic == Mnemonic::CALL {
                let return_address = Word::new(new_state.ip);
                new_state = push_word(new_state, return_address);
            }
            new_state.ip = new_state.ip.wrapping_add(offset as u16);
            new_states.push(
                match flag {
                    Some((flag_to_check, truth_value)) =>
                        new_state.set_flag(flag_to_check, Bit::new().set(truth_value)),
                    None => new_state
                }
            );
        }
    }

    if cont {
        if instruction.mnemonic == Mnemonic::LOOP {
            state = state.set_reg16(Register::CS, Word::new(0));
        };
        state.ip = state.ip.wrapping_add(instruction.length as u16);
        new_states.push(
            match flag {
                Some((flag_to_check, truth_value)) =>
                    state.set_flag(flag_to_check, Bit::new().set(!truth_value)),
                None => state
            }
        );
    }

    new_states
}

fn simulate_instruction<'a>(mut state: State<'a>, inst: Instruction) -> State<'a> {
    state.ip = state.ip.wrapping_add(inst.length as u16);
    match inst.mnemonic {
        Mnemonic::DEC | Mnemonic::INC =>
            simulate_unary_operator(state, inst),
        Mnemonic::ADD | Mnemonic::AND | Mnemonic::OR | Mnemonic::SHL =>
            simulate_binary_operator(state, inst),
        Mnemonic::XOR | Mnemonic::SUB =>
            simulate_nullifying_operator(state, inst),
        Mnemonic::CLD => state.clear_flag(Flag::Direction),
        Mnemonic::CMP => simulate_cmp(state, inst),
        Mnemonic::IN => simulate_in(state, inst),
        Mnemonic::OUT => state,
        Mnemonic::MOV => simulate_mov(state, inst),
        Mnemonic::NOP => state,
        Mnemonic::POP => simulate_pop(state, inst),
        Mnemonic::PUSH => simulate_push(state, inst),
        _ => panic!("Instruction:\n{}\nunimplemented in simulator.", inst)
    }
}

fn simulate_unary_operator<'a>(state: State<'a>, inst: Instruction) -> State<'a> {
    let op = inst.unpack_op1();
    let (result, flags) = apply_unary_op(state.get_value(op), inst.mnemonic);
    state.set_value(op, result).set_flags(flags)
}

fn simulate_binary_operator<'a>(state: State<'a>, inst: Instruction) -> State<'a> {
    let (op1, op2) = (inst.unpack_op1(), inst.unpack_op2());
    let (result, flags) =
        apply_binary_op(state.get_value(op1), state.get_value(op2), inst.mnemonic);
    state.set_value(op1, result).set_flags(flags)
}

fn simulate_nullifying_operator<'a>(state: State<'a>, inst: Instruction) -> State<'a> {
    if inst.op1 == inst.op2 {
        state.clear_value(inst.unpack_op1())
            .set_flag(Flag::Zero, Bit::True)
            .set_flag(Flag::Overflow, Bit::False)
            .set_flag(Flag::Carry, Bit::False)
    } else {
        simulate_binary_operator(state, inst)
    }
}

fn simulate_cmp<'a>(state: State<'a>, inst: Instruction) -> State<'a> {
    let (op1, op2) = (inst.unpack_op1(), inst.unpack_op2());
    let (_, flags) =
        apply_binary_op(state.get_value(op1), state.get_value(op2), Mnemonic::SUB);
    state.set_flags(flags)
}

fn simulate_in<'a>(state: State<'a>, inst: Instruction) -> State<'a> {
    match inst.unpack_op1() {
        Operand::Register8(Register::AL) =>
            state.set_reg8(Register::AL, Byte::AnyValue),
        Operand::Register16(Register::AX) =>
            state.set_reg16(Register::AX, Word::AnyValue),
        _ => panic!("Wrong destination for IN instruction: {}", inst)
    }
}

fn simulate_mov<'a>(state: State<'a>, inst: Instruction) -> State<'a> {
    let (op1, op2) = (inst.unpack_op1(), inst.unpack_op2());
    let value = state.get_value(op2);
    state.set_value(op1, value)
}

fn simulate_pop<'a>(state: State<'a>, inst: Instruction) -> State<'a> {
    let (new_state, word) = pop_word(state);
    new_state.set_word(inst.unpack_op1(), word)
}

fn simulate_push<'a>(state: State<'a>, inst: Instruction) -> State<'a> {
    let word = state.get_word(inst.unpack_op1());
    push_word(state, word)
}

pub fn pop_word(state: State) -> (State, Word) {
    let pointer = Operand::Pointer(
        Pointer::new(1, PtrType::Reg(Register::SP)).set_segment(Register::SS));
    let word = state.get_word(pointer);
    let (new_sp, _) =
        apply_to_words(state.get_reg16(Register::SP), Word::new(2), Mnemonic::ADD);
    (state.set_reg16(Register::SP, new_sp), word)
}

pub fn push_word(state: State, word: Word) -> State {
    let (new_sp, _) =
        apply_to_words(state.get_reg16(Register::SP), Word::new(2), Mnemonic::SUB);
    let pointer = Operand::Pointer(
        Pointer::new(1, PtrType::Reg(Register::SP)).set_segment(Register::SS));
    state.set_reg16(Register::SP, new_sp).set_word(pointer, word)
}

fn apply_unary_op(op: Value, mnemonic: Mnemonic) -> (Value, Flags) {
    match op {
        Value::Byte(byte) => {
            let (result, flags) = apply_to_byte(byte, mnemonic);
            (Value::Byte(result), flags)
        },
        Value::Word(word) => {
            let (result, flags) = apply_to_word(word, mnemonic);
            (Value::Word(result), flags)
        }
    }
}

fn apply_binary_op(op1: Value, op2: Value, op: Mnemonic) -> (Value, Flags) {
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

fn apply_to_word(word: Word, op: Mnemonic) -> (Word, Flags) {
    let new_word = if let Word::Bytes(bytel, byteh) = word {
        bytel.combine(byteh)
    } else {
        word
    };
    let mut flags = Flags::new();
    match new_word {
        Word::Undefined => (Word::Undefined, flags),
        Word::AnyValue => (Word::AnyValue, Flags::new()
            .set(Flag::Sign, Bit::TrueAndFalse)
            .set(Flag::Zero, Bit::TrueAndFalse)
            .set(Flag::Overflow, Bit::TrueAndFalse)),
        Word::Int(word_set) => {
            let mut set = HashSet::new();
            let mut zero_flag = Bit::new();
            for word in word_set {
                let (result, new_flags) =
                    apply_to_u16(word, op);
                set.insert(result);
                flags = flags.union(new_flags);
                if result == 0 {
                    zero_flag.add_true();
                } else {
                    zero_flag.add_false();
                }
            };
            (Word::Int(set), flags.set(Flag::Zero, zero_flag))
        },
        _ => panic!("shouldn't be here")
    }
}

fn apply_to_byte(byte: Byte, op: Mnemonic) -> (Byte, Flags) {
    let mut flags = Flags::new();
    match byte {
        Byte::Undefined => (Byte::Undefined, flags),
        Byte::AnyValue => (Byte::AnyValue, Flags::new()
            .set(Flag::Sign, Bit::TrueAndFalse)
            .set(Flag::Zero, Bit::TrueAndFalse)
            .set(Flag::Overflow, Bit::TrueAndFalse)),
        Byte::Int(byte_set) => {
            let mut set = HashSet::new();
            let mut zero_flag = Bit::new();
            for byte in byte_set {
                let (result, new_flags) =
                    apply_to_u8(byte, op);
                set.insert(result);
                flags = flags.union(new_flags);
                if result == 0 {
                    zero_flag.add_true();
                } else {
                    zero_flag.add_false();
                }
            };
            (Byte::Int(set), flags.set(Flag::Zero, zero_flag))
        }
    }
}

fn apply_to_words(wordl: Word, wordr: Word, op: Mnemonic) -> (Word, Flags) {
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
            match word2 {
                Word::Undefined => (Word::Undefined, flags),
                _ => (Word::AnyValue, Flags::new()
                    .set(Flag::Sign, Bit::TrueAndFalse)
                    .set(Flag::Zero, Bit::TrueAndFalse)
                    .set(Flag::Overflow, Bit::TrueAndFalse)
                )
            },
        Word::Int(set1) => match word2 {
            Word::Undefined => (Word::Undefined, flags),
            Word::AnyValue => (Word::AnyValue, Flags::new()
                .set(Flag::Sign, Bit::TrueAndFalse)
                .set(Flag::Zero, Bit::TrueAndFalse)
                .set(Flag::Overflow, Bit::TrueAndFalse)
            ),
            Word::Int(set2) => {
                let mut set = HashSet::new();
                let mut zero_flag = Bit::new();
                for word1 in set1 {
                    for word2 in set2.clone() {
                        let (result, new_flags) =
                            apply_to_u16s(word1, word2, op);
                        set.insert(result);
                        flags = flags.union(new_flags);
                        if result == 0 {
                            zero_flag.add_true();
                        } else {
                            zero_flag.add_false();
                        }
                    }
                };
                (Word::Int(set), flags.set(Flag::Zero, zero_flag))
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
            match op2 {
                Byte::Undefined => (Byte::Undefined, flags),
                _ => (Byte::AnyValue, Flags::new()
                    .set(Flag::Sign, Bit::TrueAndFalse)
                    .set(Flag::Zero, Bit::TrueAndFalse)
                    .set(Flag::Overflow, Bit::TrueAndFalse)
                )
            },
        Byte::Int(set1) => match op2 {
            Byte::Undefined => (Byte::Undefined, flags),
            Byte::AnyValue => (Byte::AnyValue, Flags::new()
                .set(Flag::Sign, Bit::TrueAndFalse)
                .set(Flag::Zero, Bit::TrueAndFalse)
                .set(Flag::Overflow, Bit::TrueAndFalse)
            ),
            Byte::Int(set2) => {
                let mut set = HashSet::new();
                let mut zero_flag = Bit::new();
                for byte1 in set1 {
                    for byte2 in set2.clone() {
                        let (result, new_flags) =
                            apply_to_u8s(byte1, byte2, op);
                        flags = flags.union(new_flags);
                        set.insert(result);
                        if result == 0 {
                            zero_flag.add_true();
                        } else {
                            zero_flag.add_false();
                        }
                    }
                };
                (Byte::Int(set), flags.set(Flag::Zero, zero_flag))
            }
        }
    }
}

fn apply_to_u16(word: u16, op: Mnemonic) -> (u16, Flags) {
    match op {
        Mnemonic::INC => {
            (word + 1, if word == 0xffff {
                Flags::new().set(Flag::Overflow, Bit::True)
                    .set(Flag::Carry, Bit::True)
            } else {
                Flags::new().set(Flag::Overflow, Bit::False)
                    .set(Flag::Carry, Bit::False)
            })
        },
        Mnemonic::DEC => {
            ((word as i16 - 1) as u16, if word == 0x0000 {
                Flags::new().set(Flag::Overflow, Bit::True)
                    .set(Flag::Carry, Bit::True)
            } else {
                Flags::new().set(Flag::Overflow, Bit::False)
                    .set(Flag::Carry, Bit::False)
            })
        },
        _ => panic!("Operation {:?} not implemented for words.", op)
    }
}

fn apply_to_u8(word: u8, op: Mnemonic) -> (u8, Flags) {
    match op {
        Mnemonic::INC => {
            (word + 1, if word == 0xff {
                Flags::new().set(Flag::Overflow, Bit::True)
                    .set(Flag::Carry, Bit::True)
            } else {
                Flags::new().set(Flag::Overflow, Bit::False)
                    .set(Flag::Carry, Bit::False)
            })
        },
        Mnemonic::DEC => {
            ((word as i8 - 1) as u8, if word == 0x0000 {
                Flags::new().set(Flag::Overflow, Bit::True)
                    .set(Flag::Carry, Bit::True)
            } else {
                Flags::new().set(Flag::Overflow, Bit::False)
                    .set(Flag::Carry, Bit::False)
            })
        },
        _ => panic!("Operation {:?} not implemented for words.", op)
    }
}

fn apply_to_u16s(word1: u16, word2: u16, op: Mnemonic) -> (u16, Flags) {
    match op {
        Mnemonic::ADD => {
            (word1.wrapping_add(word2), if 0xffff - word1 < word2 {
                Flags::new().set(Flag::Overflow, Bit::True)
                    .set(Flag::Carry, Bit::True)
            } else {
                Flags::new().set(Flag::Overflow, Bit::False)
                    .set(Flag::Carry, Bit::False)
            })
        },
        Mnemonic::SUB => {
            (word1.wrapping_sub(word2), if word1 < word2 {
                Flags::new().set(Flag::Overflow, Bit::True)
                    .set(Flag::Carry, Bit::True)
            } else {
                Flags::new().set(Flag::Overflow, Bit::False)
                    .set(Flag::Carry, Bit::False)
            })
        },
        Mnemonic::AND => {
            (word1 & word2, Flags::new())
        },
        Mnemonic::OR => {
            (word1 | word2, Flags::new())
        },
        Mnemonic::XOR => {
            (word1 ^ word2, Flags::new())
        },
        Mnemonic::SHL => {
            (word1 << word2, Flags::new())
        },
        _ => panic!("Operation {:?} not implemented for words.", op)
    }
}

fn apply_to_u8s(byte1: u8, byte2: u8, op: Mnemonic) -> (u8, Flags) {
    match op {
        Mnemonic::ADD => {
            (byte1.wrapping_add(byte2), if 0xff - byte1 < byte2 {
                Flags::new().set(Flag::Overflow, Bit::True)
                    .set(Flag::Carry, Bit::True)
            } else {
                Flags::new().set(Flag::Overflow, Bit::False)
                    .set(Flag::Carry, Bit::False)
            })
        },
        Mnemonic::SUB => {
            (byte1.wrapping_sub(byte2), if byte1 < byte2 {
                Flags::new().set(Flag::Overflow, Bit::True)
                    .set(Flag::Carry, Bit::True)
            } else {
                Flags::new().set(Flag::Overflow, Bit::False)
                    .set(Flag::Carry, Bit::False)
            })
        },
        Mnemonic::AND => {
            (byte1 & byte2, Flags::new())
        },
        Mnemonic::OR => {
            (byte1 | byte2, Flags::new())
        },
        Mnemonic::XOR => {
            (byte1 ^ byte2, Flags::new())
        },
        Mnemonic::SHL => {
            (byte1 << byte2, Flags::new())
        },
        _ => panic!("Operation {:?} not implemented for bytes.", op)
    }
}
