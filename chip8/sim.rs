use defs::*;
use chip8::arch::*;
use chip8::state::{State};
use std::collections::HashSet;

pub fn simulate_next_instruction<'a, C: Context<State<'a>, Instruction>>(mut state: State<'a>, context: &C, instruction: Instruction) -> SimResult<State<'a>> {
    state.pc += 2;
    match instruction.mnemonic {
        Mnemonic::CLS => SimResult::State(state),
        Mnemonic::JP | Mnemonic::CALL | Mnemonic::SE | Mnemonic::SNE
        | Mnemonic::SKP | Mnemonic::SKNP | Mnemonic::RET =>
            SimResult::Branch(branch(state, instruction, context)),
        Mnemonic::LD => simulate_ld(state, instruction),
        Mnemonic::SHL | Mnemonic::SHR =>
            SimResult::State(simulate_unary_operator(state, instruction)),
        Mnemonic::ADD | Mnemonic::SUB | Mnemonic::OR | Mnemonic::AND
        | Mnemonic::XOR | Mnemonic::SUBN =>
            SimResult::State(simulate_binary_operator(state, instruction)),
        Mnemonic::RND => simulate_rnd(state, instruction),
        Mnemonic::DRW => SimResult::State(state),
        Mnemonic::LDBCD => simulate_ldbcd(state, instruction),
        Mnemonic::LDPTR => simulate_ldptr(state, instruction)
    }
}

fn branch<'a, C: Context<State<'a>, Instruction>>(mut state: State<'a>, instruction: Instruction, context: &C) -> (Vec<State<'a>>, Vec<usize>) {
    let mut new_states = Vec::new();
    let mut new_labels = Vec::new();

    println!("V1 = {}", state.get_word(Operand::V(1)));

    match instruction.mnemonic {
        Mnemonic::CALL => {
            state.sp += 1;
            state.stack[state.sp] = state.pc;
            state.pc = match instruction.unpack_op1() {
                Operand::Address(word) => word,
                _ => panic!("CALL operand should be immediate address.")
            };
            new_labels.push(context.next_inst_offset(&state));
            new_states.push(state);
        },
        Mnemonic::RET => {
            state.pc = state.stack[state.sp];
            state.sp -= 1;
            new_states.push(state);
        },
        Mnemonic::JP => {
            state.pc = match instruction.unpack_op1() {
                Operand::Address(word) => word,
                _ => panic!("CALL operand should be immediate address.")
            };
            new_labels.push(context.next_inst_offset(&state));
            new_states.push(state);
        },
        Mnemonic::SE | Mnemonic::SNE => {
            let op1 = instruction.unpack_op1();
            let op2 = instruction.unpack_op2();
            let (can, must) = state.get_word(op1).compare(&state.get_word(op2));
            if (can && instruction.mnemonic == Mnemonic::SE)
                || (!must && instruction.mnemonic == Mnemonic::SNE) {
                    let mut new_state = state.clone();
                    new_state.pc += 2;
                    new_states.push(new_state);
            }
            if (can && instruction.mnemonic == Mnemonic::SNE)
                || (!must && instruction.mnemonic == Mnemonic::SE) {
                    new_states.push(state);
            }
        },
        _ => panic!("unimplemented jump instruction")
    }

    (new_states, new_labels)
}

fn simulate_unary_operator<'a>(state: State<'a>, inst: Instruction) -> State<'a> {
    let op1 = inst.unpack_op1();
    let (result, vf) = apply_unary_op(state.get_word(op1), inst.mnemonic);
    state.set_word(op1, result).set_word(Operand::V(0xF), vf)
}

fn simulate_binary_operator<'a>(state: State<'a>, inst: Instruction) -> State<'a> {
    let (op1, op2) = (inst.unpack_op1(), inst.unpack_op2());
    let (result, vf) = apply_binary_op(state.get_word(op1), state.get_word(op2),
        inst.mnemonic);
    let new_state = state.set_word(op1, result);
    match vf {
        None => new_state,
        Some(word) => new_state.set_word(Operand::V(0xF), word) 
    }
}

fn simulate_ld<'a>(state: State<'a>, inst: Instruction) -> SimResult<State<'a>> {
    let (op1, op2) = (inst.unpack_op1(), inst.unpack_op2());
    let word = state.get_word(op2);
    SimResult::State(state.set_word(op1, word))
}

fn simulate_rnd<'a>(state: State<'a>, inst: Instruction) -> SimResult<State<'a>> {
    match inst.op2 {
        Some(Operand::Byte(byte)) => {
            let mut set = HashSet::new();
            for i in 0..256u16 {
                set.insert((i as u8  & byte) as u16);
            }
            SimResult::State(state.set_word(inst.unpack_op1(), Word::Int(set)))
        },
        _ => panic!("RND second operand should be an immediate byte.")
    }
}

fn simulate_ldbcd<'a>(state: State<'a>, inst: Instruction) -> SimResult<State<'a>> {
    panic!("LDBCD unimplemented")
}

fn simulate_ldptr<'a>(state: State<'a>, inst: Instruction) -> SimResult<State<'a>> {
    panic!("LDPTR unimplemented")
}

fn apply_unary_op(op: Word, mnemonic: Mnemonic) -> (Word, Word) {
    match op {
        Word::Undefined => (Word::Undefined, Word::Undefined),
        Word::AnyValue => (Word::AnyValue, Word::AnyValue),
        Word::Int(word_set) => {
            let mut set = HashSet::new();
            let mut new_vf = Word::Int(HashSet::new());
            for word in word_set {
                let (result, vf) = apply_to_u16(word, mnemonic);
                set.insert(result);
                new_vf = new_vf.union(vf);
            }
            (Word::Int(set), new_vf)
        },
        _ => panic!("shouldn't be here")
    }
}

fn apply_binary_op(op1: Word, op2: Word, op: Mnemonic) -> (Word, Option<Word>) {
    match op1 {
        Word::Undefined => (Word::Undefined, Some(Word::Undefined)),
        Word::AnyValue => match op2 {
            Word::Undefined => (Word::Undefined, Some(Word::Undefined)),
            _ => (Word::AnyValue, Some(Word::AnyValue))
        },
        Word::Int(set1) => match op2 {
            Word::Undefined => (Word::Undefined, Some(Word::Undefined)),
            Word::AnyValue => (Word::AnyValue, Some(Word::AnyValue)),
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
                    Bit::True => Some(Word::new(1)),
                    Bit::False => Some(Word::new(0)),
                    Bit::TrueAndFalse => Some(Word::new(0).union(Word::new(1)))
                })
            },
            _ => panic!("shouldn't be here")
        },
        _ => panic!("shouldn't be here")
    }
}

fn apply_to_u16(word: u16, op: Mnemonic) -> (u16, Word) {
    match op {
        Mnemonic::SHL => (word << 1, match word & 0x8000 {
            0x8000 => Word::new(1),
            _ => Word::new(0)
        }),
        Mnemonic::SHR => (word >> 1, match word & 0x0001 {
            0x0001 => Word::new(1),
            _ => Word::new(0)
        }),
        _ => panic!("unknown unary op")
    }
}

fn apply_to_u16s(word1: u16, word2: u16, op: Mnemonic) -> (u16, Bit) {
    match op {
        Mnemonic::ADD =>
            ((word1 + word2) & 0x00ff, if word1 + word2 > 0xff {
                Bit::True
            } else {
                Bit::False
            }),
        Mnemonic::SUB =>
            (word1.wrapping_sub(word2) & 0x00ff, if word1 > word2 {
                Bit::False
            } else {
                Bit::True
            }),
        Mnemonic::SUBN =>
            (word2.wrapping_sub(word1) & 0x00ff, if word2 > word1 {
                Bit::False
            } else {
                Bit::True
            }),
        Mnemonic::OR => (word1 | word2, Bit::Undefined),
        Mnemonic::AND => (word1 & word2, Bit::Undefined),
        Mnemonic::XOR => (word1 ^ word2, Bit::Undefined),
        _ => panic!("unknown binary op")
    }
}
