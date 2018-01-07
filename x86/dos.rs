use defs::*;
use state::*;
use x86::arch::*;

// We assume the PSP is loaded at 0x75a and the program at 0x76a.
// This is what DOSBOX uses as a default.
const PSP_SEGMENT: u16 = 0x75a;
const PROGRAM_SEGMENT: u16 = 0x76a;

pub struct DOS {
}

impl Context for DOS {
    fn simulate_int<'a>(&self, state: State<'a>, inst: Instruction) -> Option<State<'a>> {
        match inst.op1 {
            Some(Operand::Imm8(func)) => {
                match func {
                    0x10 => Some(simulate_int10(state)),
                    0x20 => None,
                    0x21 => simulate_int21(state),
                    _ => Some(state)
                }
            },
            _ => panic!("Expected first operand of INT to be imm8")
        }
    }

}

fn simulate_int10(state: State) -> State {
    let ah = state.unpack_reg8(Register::AH)
        .expect("can't call INT 10 with non-deterministic AH.");
    match ah {
        0x00...0x02 | 0x05...0x07 => state,
        0x03 => state
            .set_reg8(Register::DH, Byte::AnyValue)
            .set_reg8(Register::DL, Byte::AnyValue)
            .set_reg8(Register::CH, Byte::AnyValue)
            .set_reg8(Register::CL, Byte::AnyValue),
        _ => state
    }
}

fn simulate_int21(state: State) -> Option<State> {
    let ah = state.unpack_reg8(Register::AH)
        .expect("can't call INT 21 with non-deterministic AH.");
    match ah {
        0x00 | 0x4c => None,
        0x06 => {
            if state.get_reg8(Register::DL).can_be(0xff) {
                Some(state.set_reg8(Register::AL, Byte::AnyValue)
                    .set_flag(Flag::Zero, Bit::TrueAndFalse))
            } else {
                let dl = state.get_reg8(Register::DL);
                Some(state.set_reg8(Register::AL, dl))
            }
        },
        0x09 => Some(state),
        _ => Some(state)
    }
}

pub fn initial_state<'a>(file_buffer: Vec<u8>, load_module: &'a LoadModule) -> State<'a> {
    let mut state = State::new(load_module)
        .set_reg16(Register::DS, Word::new(PSP_SEGMENT))
        .set_reg16(Register::ES, Word::new(PSP_SEGMENT));

    if file_buffer[0..2] == [0x4d, 0x5a] {
        state.cs = get_word_le(&file_buffer, 0x16).wrapping_add(PROGRAM_SEGMENT);
        state.ip = get_word_le(&file_buffer, 0x14);
        state.set_reg16(Register::SS,
            Word::new(get_word_le(&file_buffer, 0xe).wrapping_add(PROGRAM_SEGMENT)))
            .set_reg16(Register::SP, Word::new(get_word_le(&file_buffer, 0x10)))
    } else {
        state.cs = PSP_SEGMENT;
        state.ip = 0x100;
        state.set_reg16(Register::SS, Word::new(PSP_SEGMENT))
            .set_reg16(Register::SP, Word::new(0xfffe))
    }
}

pub fn load_module(file_buffer: &[u8]) -> LoadModule {
    let mut buffer = Vec::new();
    let header_size = if file_buffer[0..2] == [0x4d, 0x5a] {
        16*get_word_le(&file_buffer, 0x08) as usize
    } else {
        0
    };
    for i in header_size..file_buffer.len() {
        buffer.push(file_buffer[i]);
    }
    LoadModule {
        file_offset: header_size,
        memory_segment: PROGRAM_SEGMENT,
        buffer: buffer
    }
}

